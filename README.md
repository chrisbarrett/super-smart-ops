# super-smart-ops

## Summary

Like `smart operators`, but better.

Provides enhanced editing support for operators in programming languages.

- Automatically inserts padding around operators

- Deleting backwards deletes the padding and last character of the operator

- Customisable for different languages

- Safe to use with Yasnippet.

## Installing

You will need Emacs 24+, `make` and [Cask](https://github.com/cask/cask) to
build the project.

    cd super-smart-ops
    make && make install

## Configuration

To add support for smart operators to a language mode, call the
`declare-smart-ops` function.

```lisp
(super-smart-ops-configure-for-mode 'asm-mode)
```

Common arithmetic operators are enable by default. Use the `:rem` keyword
argument to disable certain operators.

```lisp
(super-smart-ops-configure-for-mode 'asm-mode
  :rem '("%" "-"))
```

Add support for more operators using the `:add` keyword argument.

```lisp
(super-smart-ops-configure-for-mode 'haskell-mode
  :add '("$"))
```

You can also specify your own custom commands to perform the insertion and
formatting. `<backspace>` will still work as expected for these operators. Use
the `:custom` keyword argument to configure these commands.


```lisp
(super-smart-ops-configure-for-mode 'haskell-mode
  ; ...
  :custom
  '(("." . cb-hs:smart-dot)
    ("," . cb-hs:smart-comma)
    ("|" . cb-hs:smart-pipe)
    ("#" . cb-hs:smart-hash)
    (":" . cb-hs:smart-colon)))
```


## Examples

The examples below use a pipe character ('|') to represent the cursor position.


### Example 1

Padding is managed automatically when inserting operators.

    1|

Typing '+' inserts the operator and adds padding:

    1 + |

Typing '=' adjusts the padding as you'd expect:

    1 += |

Deleting backwards (with `<backspace>`) deletes any trailing padding and the
last operator:

    1 +|

Deleting backwards again deletes the last operator and its leading padding:

    1|


### Example 2

It's common to mistype an operator, then want to correct it immediately.

    1|

Typing '+' inserts the operator and adds padding:

    1 + |

Deleting backwards (with `<backspace>`) deletes the operator and its padding,
ready for your correction.

    1|


### Example 3

Some languages support operator sections. Inserting operators in parens
behave as you'd want for these cases.

    (|

Typing '+' gives:

    (+|

In Haskell for instance, this means that typing </> should be padded in a
bare code context...

    x </> |

...but will not be padded in parens.

    f (</>|


## Contributing

Yes, please do! See CONTRIBUTING.md for guidelines.

## License

See COPYING. Copyright (c) 2014 Chris Barrett.
