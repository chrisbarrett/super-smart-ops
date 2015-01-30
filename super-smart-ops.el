;;; super-smart-ops.el --- Like smart operators, but better.  -*- lexical-binding: t; -*-

;; Copyright (C) 2014 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 0.1
;; Package-Requires: ((s "1.9.0") (dash "2.5.0") (cl-lib "0.3"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Summary
;; =======
;;
;; Like smart operators, but better.
;;
;; Provides enhanced editing support for operators in programming languages.
;;
;; - Automatically inserts padding around operators
;;
;; - Deleting backwards deletes the padding and last character of the operator
;;
;; - Customisable for different languages
;;
;; - Safe to use with Yasnippet.
;;
;;
;; Configuration
;; =============
;;
;; To add support for smart operators to a language mode, call the
;; `super-smart-ops-configure-for-mode' function.
;;
;;     (super-smart-ops-configure-for-mode 'asm-mode)
;;
;; Common arithmetic operators are enable by default. Use the `:rem` keyword
;; argument to disable certain operators.
;;
;;     (super-smart-ops-configure-for-mode 'asm-mode
;;       :rem '("%" "-"))
;;
;; Add support for more operators using the `:add` keyword argument.
;;
;;     (super-smart-ops-configure-for-mode 'haskell-mode
;;       :add '("$"))
;;
;; You can also specify your own custom commands to perform the insertion and
;; formatting. `<backspace>` will still work as expected for these operators. Use
;; the `:custom` keyword argument to configure these commands.
;;
;;     (super-smart-ops-configure-for-mode 'haskell-mode
;;       ; ...
;;       :custom
;;       '(("." . cb-hs:smart-dot)
;;         ("," . cb-hs:smart-comma)
;;         ("|" . cb-hs:smart-pipe)
;;         ("#" . cb-hs:smart-hash)
;;         (":" . cb-hs:smart-colon)))
;;
;;
;; Examples
;; ========
;;
;; The examples below use a pipe character ('|') to represent the cursor position.
;;
;;
;; Example 1
;; ---------
;;
;; Padding is managed automatically when inserting operators.
;;
;;     1|
;;
;; Typing '+' inserts the operator and adds padding:
;;
;;     1 + |
;;
;; Typing '=' adjusts the padding as you'd expect:
;;
;;     1 += |
;;
;; Deleting backwards (with <backspace>) deletes any trailing padding and the
;; last operator:
;;
;;     1 +|
;;
;; Deleting backwards again deletes the last operator and its leading padding:
;;
;;     1|
;;
;;
;; Example 2
;; =========
;;
;; It's common to mistype an operator, then want to correct it immediately.
;;
;;     1|
;;
;; Typing '+' inserts the operator and adds padding:
;;
;;     1 + |
;;
;; Deleting backwards (with <backspace>) deletes the operator and its padding,
;; ready for your correction.
;;
;;     1|
;;
;;
;; Example 3
;; =========
;;
;; Some languages support operator sections. Inserting operators in parens
;; behave as you'd want for these cases.
;;
;;     (|
;;
;; Typing '+' gives:
;;
;;     (+|
;;
;; In Haskell for instance, this means that typing </> should be padded in a
;; bare code context...
;;
;;     x </> |
;;
;; ...but will not be padded in parens.
;;
;;     f (</>|
;;
;;

;;; Code:

(require 'cl-lib)
(require 's)
(require 'dash)
(require 'thingatpt)
(autoload 'yas--field-end "yasnippet")
(autoload 'yas--field-start "yasnippet")

(defgroup super-smart-ops nil
  "Automatically format operators in programming languages."
  :group 'languages
  :prefix "super-smart-ops-")

(defcustom super-smart-ops-text-inserted-functions nil
  "Abnormal hook functions called whenever text is inserted by smart operators.
Each function takes the number of characters inserted as an argument."
  :group 'super-smart-ops
  :type 'hook)

(defcustom super-smart-ops-text-removed-functions nil
  "Abnormal hook functions called whenever text is removed by smart operators.
Each function takes the number of characters removed as an argument."
  :group 'super-smart-ops
  :type 'hook)

;;; Internal

(defvar-local super-smart-ops-list
  '("=" "<" ">" "%" "+" "-" "*" "/" "&" "|" "!" ":")
  "A list of strings to treat as operators.")

(defun super-smart-ops--in-string-or-comment? ()
  "Non-nil if point is in a string or comment."
  (nth 8 (syntax-ppss)))

(defun super-smart-ops--prev-non-space-char ()
  "Return the previous non-whitespace character on this line, as a string."
  (save-excursion
    (when (search-backward-regexp (rx (not space))
                                  (line-beginning-position) t)
      (thing-at-point 'char))))

(defun super-smart-ops--delete-horizontal-space-non-readonly ()
  "Delete horizontal space around point that is not read-only."
  (while (and (not (eolp))
              (s-matches? (rx space) (char-to-string (char-after)))
              (not (get-char-property (point) 'read-only)))
    (forward-char 1))

  (while (and (not (bolp))
              (s-matches? (rx space) (char-to-string (char-before)))
              (not (get-char-property (1- (point)) 'read-only)))
    (delete-char -1)))

(defun super-smart-ops--maybe-just-one-space-after-operator ()
  "Insert a trailing space unless:
- the next char is an operator
- we are in a parenthesised operator."
  (unless (or
           ;; next char is a smart op
           (and (not (eolp)) (-contains? super-smart-ops-list (char-to-string (char-after))))
           ;; at end of parens
           (thing-at-point-looking-at
            (eval `(rx "(" (+ (or ,@super-smart-ops-list)) ")"))))
    (just-one-space)))

(defmacro super-smart-ops--run-with-modification-hooks (&rest body)
  "Execute BODY forms, then call hooks to notify of insertions and deletions.

After BODY is run, call `super-smart-ops-text-inserted-functions' if the
buffer has grown in length.  If the overall buffer length is
shorter, call `super-smart-ops-text-removed-functions'."
  (let ((size-before (cl-gensym))
        (size-after  (cl-gensym))
        (result      (cl-gensym))
        (difference  (cl-gensym)))
    `(let* ((,size-before (buffer-size))
            (,result      (progn ,@body))
            (,size-after  (buffer-size))
            (,difference (- ,size-after ,size-before)))
       (cond
        ((zerop ,difference))
        ((cl-plusp ,difference)
         (run-hook-with-args 'super-smart-ops-text-inserted-functions ,difference))
        ((cl-minusp ,difference)
         (run-hook-with-args 'super-smart-ops-text-removed-functions (abs ,difference))))

       ,result)))

(defun super-smart-ops--yas-current-field ()
  "Return the current active yasnippet field."
  (and (boundp 'yas--active-field-overlay)
       yas--active-field-overlay
       (overlay-buffer yas--active-field-overlay)
       (overlay-get yas--active-field-overlay 'yas--field)))

(defun super-smart-ops--add-smart-ops (ops custom)
  "Apply custom operators in the current buffer.

OPS are the smart operators for this mode.

CUSTOM are custom operator implementations."
  (let ((custom-ops (-map 'car custom)))
    (setq-local super-smart-ops-list (-union ops custom-ops))
    (dolist (o ops)
      (local-set-key (kbd o) (eval `(super-smart-ops-make-smart-op ,o))))

    (dolist (c custom)
      (cl-destructuring-bind (op . fn) c
        ;; Decorate custom operators to run modification hooks.
        (local-set-key (kbd op) (lambda ()
                                  (interactive "*")
                                  (super-smart-ops--run-with-modification-hooks
                                   (call-interactively fn))))))))

;;; Public

;;;###autoload
(defun super-smart-ops-make-smart-op (str)
  "Return a function that will insert smart operator STR.
Useful for setting up keymaps manually.

As a side-effect, this procedure will define a named function
that will insert the smart op."
  (cl-assert (not (s-matches? (rx space) str)))
  (let ((fname (intern (format "super-smart-op-insert/%s" str))))
    (unless (fboundp fname)
      (eval `(defun ,fname (&optional no-padding)
               "Auto-generated command.  Inserts a smart operator.
If called with a prefix arg, do not insert padding."
               (interactive "*P")
               (if no-padding
                   (insert ,str)
                 (super-smart-ops-insert ,str)))))
    fname))

;;;###autoload
(defun super-smart-ops-delete-last-op ()
  "Delete the last smart-operator that was inserted."
  (unless (or (derived-mode-p 'text-mode) (super-smart-ops--in-string-or-comment?))
    (super-smart-ops--run-with-modification-hooks
     (save-restriction
       (narrow-to-region (line-beginning-position) (point))

       (when (s-matches? (concat (regexp-opt super-smart-ops-list) " *$")
                         (buffer-substring (line-beginning-position) (point)))
         ;; Delete op
         (let ((op-pos
                (save-excursion
                  (search-backward-regexp (regexp-opt super-smart-ops-list)))))
           (while (and (/= (point) op-pos)
                       (not (get-char-property (point) 'read-only)))
             (delete-char -1)))

         ;; Delete preceding spaces.
         (super-smart-ops--delete-horizontal-space-non-readonly)
         t)))))

;;;###autoload
(defun super-smart-ops-insert (op)
  "Insert a smart operator OP, unless we're in a string or comment."

  (super-smart-ops--run-with-modification-hooks

   ;; If point is just before a closing bracket with padding, narrow to
   ;; current position to preserve that padding.
   (save-restriction
     (when (s-matches? (rx bol (+ space) (or "]" ")" "}"))
                       (buffer-substring (point) (line-end-position)))
       (narrow-to-region (line-beginning-position) (point)))

     ;; Perform insertion.
     (cond
      ((or (super-smart-ops--in-string-or-comment?)
           ;; Looking at quotation mark?
           (-contains? '(?\" ?\') (char-after)))
       (insert op))

      ((-contains? (cl-list* "(" super-smart-ops-list) (super-smart-ops--prev-non-space-char))
       (super-smart-ops--delete-horizontal-space-non-readonly)
       (insert op)
       (super-smart-ops--maybe-just-one-space-after-operator))

      (t
       (unless (s-matches? (rx bol (* space) eol)
                           (buffer-substring (line-beginning-position) (point)))
         (just-one-space))

       (insert op)
       (super-smart-ops--maybe-just-one-space-after-operator))))))

;;;###autoload
(cl-defun super-smart-ops-configure-for-mode (mode &key add rem custom)
  "Define the smart operators for the given mode.

- MODE is the mode to add the smart ops for.

- ADD is a list of smart operators to add to the defaults.

- REM is a list of smart operators to remove from the defaults.

- CUSTOM is a list of special operator insertion commands to use
  instead of the defaults. It is an alist of (OP . FUNCTION),
  where OP is a string and FUNCTION is a symbol."
  (declare (indent 1))
  (cl-assert (symbolp mode))
  (cl-assert (null (-intersection add rem)))
  (cl-assert (null (-intersection add (-map 'car custom))))
  (cl-assert (null (-intersection rem (-map 'car custom))))

  (let ((hook (intern (concat (symbol-name mode) "-hook")))
        (ops (-union (-map 'car custom)
                     (-difference (-union super-smart-ops-list add) rem))))

    ;; Set smart ops list for buffers that already exist.
    (dolist (b (buffer-list))
      (with-current-buffer b
        (when (derived-mode-p mode)
          (super-smart-ops--add-smart-ops ops custom))))

    ;; Set smart ops in mode's hook.
    (add-hook hook (lambda ()
                     (super-smart-ops--add-smart-ops ops custom)))

    (list :mode mode :ops ops)))

;;; Compatibility

(eval-after-load 'smartparens
  '(defadvice sp-backward-delete-char (around delete-smart-op activate)
     "Delete the smart operator that was just inserted, including padding."
     (super-smart-ops--run-with-modification-hooks
      (or (super-smart-ops-delete-last-op) ad-do-it))))

(eval-after-load 'evil
  '(defadvice super-smart-ops-insert (around restrict-to-insert-state activate)
     "If evil mode is active, only insert in insert state."
     (cond
      ((and (featurep 'evil-mode) (evil-insert-state-p))
       ad-do-it)
      ((featurep 'evil-mode))
      (t
       ad-do-it))))

(provide 'super-smart-ops)

;;; super-smart-ops.el ends here
