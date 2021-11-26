# rpnhs
A [reverse Polish notation](https://en.wikipedia.org/wiki/Reverse_Polish_notation) calculator in Haskell.

Inspired by [rpnpy](https://github.com/terrycojones/rpnpy).

For convenience, you may want to alias rpnhs in your shell.\
`alias hc='rpnhs'` (short for Haskell calculator)

### Feature overview
* General mathematical operators (see [operator list](#operators))
* Bitwise operations on integers
* Accepts values as integers (`3`), fractions (`1/4`), and decimals (`3.14`) (see [values](#values))
* Use arbitrary bases for both input and output of integers, supporting radix complement (see [bases](#bases))
* Load and store variables to and from the stack (see [variables](#variables))
* Uses a fractional representation where possible to avoid floating point inaccuracies (`1/2 + 1/2 = 1`)
* Define macros for constants or common patterns (e.g. `triple` -> `3 *`) (see [macros](#macros))

### Commandline options
```
-p,--auto-print          Auto print if no output is produced from inline mode
--prompt PROMPT          Set the interactive mode prompt
-m,--macro-file FILE     Load a given macro file on startup
-h,--help                Show this help text
```

### Operation modes
rpnhs provides 2 modes of operation:
* Inline - process commandline arguments and print to stdout, for scripts or quick and simple calculations.
* Interactive - run a REPL-like environment, for more involved calculations and inspecting the stack.

#### Inline
To run the inline mode pass in instructions to the calculator as commandline arguments.\
`rpnhs 4 5 / 1 + p` will print `9/5` to stdout.\
`rpnhs '1 p 3 * p 3 * p 3 * p'` will print the first 4 powers of 3 to stdout, on separate lines.

Note the single quotes used in the second example. This is to prevent the shell expanding the `*` as a glob pattern.\
An alternative solution would be to use `mul` instead which is an alias for `*`. See [operators](#operators) for a complete list of operator aliases.

If execution fails, no output will be made to stdout, and an error message will be printed to stderr.\
`rpnhs 1 2 + p hi` produces only `parse error: unrecognised token (hi)`

#### Interactive
To run the interactive mode, run rpnhs with no arguments.\
`rpnhs`

You are then presented with a prompt to input any number of instructions. After pressing enter, the instructions will be executed, and the state of the calculator modified.
```
> 1
> 2
> +
> p
 3
> 3 + p
 6
```
To get a feel for things try using the `stack` command to inspect the stack after each step.

If execution fails, the calculator's state will revert to before that line was run.
```
> 1
> 2 0 /
error: operator failed
> p
 1
```

To exit the calculator, enter `q`.

##### Metacommands
Metacommands begin with `:` and can only be used in interactive mode. They must occupy the entire line when used.
| Metacommand        | Description                                            |
| :----------------- | :----------------------------------------------------- |
| `:q`               | Exit the calculator (same as `q`)                      |
| `:u[n]`            | Undo the last `n` successful lines (default 1)         |
| `:def [macro def]` | Define a new macro (see [macros](#macros) for details) |


### Commands
Commands are instructions for interacting with the calculator in ways other than pushing values and applying operators.
| Command          | Description                                                                                        |
| :--------------- | :------------------------------------------------------------------------------------------------- |
| `print`, `p`     | Print the value at the top of the stack (see [bases](#bases) for printing in other bases)          |
| `stack`, `f`     | Print all values in the stack, with the top value at the rightmost position                        |
| `pop[n]`, `r`    | Pop the top `n` (default 1) values from the stack (without printing)                               |
| `clear`, `c`     | Empty the stack                                                                                    |
| `dup[n]`, `d[n]` | Duplicate the top value of the stack `n` times (default 1)                                         |
| `pull[n]`        | Pull the `n`th value on the stack to the top                                                       |
| `swap`, `s`      | Swap the top 2 values on the stack (alias for `pull2`)                                             |
| `depth`, `z`     | Push the current depth of the stack                                                                |
| `s[name]`        | Pop the top value on the stack and store it in the variable `name`, overwriting any previous value |
| `l[name]`        | Load the value in `name` and push it to the stack (fails if undefined)                             |
| `view`, `v`      | View the values of all variables                                                                   |
| `v[name]`        | View the value in `name`                                                                           |

### Operators
Operators pop 1-2 values from the top of the stack, and push back a single result.

#### General
| Operator            | Notes                               | Arity |
| :------------------ | :---------------------------------- | :---- |
| `abs`               |                                     | 1     |
| `neg`, `negate`     |                                     | 1     |
| `recip`             |                                     | 1     |
| `sqrt`, `root`      |                                     | 1     |
| `exp`, `e^`         |                                     | 1     |
| `ln`                |                                     | 1     |
| `log2`              |                                     | 1     |
| `fact`, `!`         |                                     | 1     |
| `add`, `plus`, `+`  |                                     | 2     |
| `sub`, `minus`, `-` |                                     | 2     |
| `mul`, `times`, `*` |                                     | 2     |
| `div`, `/`          | true division                       | 2     |
| `idiv`, `i/`        | integer division                    | 2     |
| `mod`, `%`          | truncated towards negative infinity | 2     |
| `pow`, `^`          |                                     | 2     |
| `log`               | `a b log` -> logb(a)                | 2     |

#### Trigonometry
| Operator           | Arity |
| :----------------- | :---- |
| `sin`              | 1     |
| `cos`              | 1     |
| `tan`              | 1     |
| `sinh`             | 1     |
| `cosh`             | 1     |
| `tanh`             | 1     |
| `asin`, `arcsin`   | 1     |
| `acos`, `arccos`   | 1     |
| `atan`, `arctan`   | 1     |
| `asinh`, `arcsinh` | 1     |
| `acosh`, `arccosh` | 1     |
| `atanh`, `arctanh` | 1     |

The above operators work with radians.

To convert between radians and degrees the below operators are provided.
| Operator | Notes              | Arity |
| :------- | :----------------- | :---- |
| `deg`    | radians -> degrees | 1     |
| `rad`    | degrees -> radians | 1     |


#### Bitwise
| Operator      | Arity |
| :------------ | :---- |
| `not`, `~`    | 1     |
| `and`, `&`    | 2     |
| `or`, `\|`    | 2     |
| `nand`, `~&`  | 2     |
| `nor`, `~\|`  | 2     |
| `xor`         | 2     |

Note that bitwise operators work on the two's complement representation of values.\
`0 ~ p` prints `-1`

#### Binary shifts
| Operator       | Arity |
| :------------- | :---- |
| `lshift`, `<<` | 2     |
| `rshift`, `>>` | 2     |

`0b10010 2 << pb` prints `1001000`

Right shifts are sign extended, so behaviour with negative values should be intuitive.\
`-40 2 >> p` prints `-10`

When provided a negative shift amount, the shift will be made in the opposite direction.\
`n <<` will act the same as `-n >>`

#### Misc
| Operator          | Notes                                                                              | Arity |
| :---------------- | :--------------------------------------------------------------------------------- | :---- |
| `rnd`, `round`    | round to the nearest integer                                                       | 1     |
| `floor`           | round to the greatest integer <= the value                                         | 1     |
| `ceil`, `ceiling` | round to the least integer >= the value                                            | 1     |
| `fl`, `float`     | convert a value to a float (useful if a fractional representation in inconvenient) | 1     |

#### Folding operators
For some binary operators a folding variant is available.\
These operators consume the entire stack, leaving the final result in its place.
```
> 1 2 3 4 5 f
 1 2 3 4 5
> ++ f
 15
```

| Folding variant | Binary operator |
| :-------------- | :-------------- |
| `++`            | `+`             |
| `**`            | `*`             |
| `&&`            | `&`             |
| `\|\|`          | `\|`            |

Some care should be taken when using these operators. If there are values in the stack leftover from previous calculations, these will be included in the fold. The `c` command may be useful before setting up a fold.

### Values
Pushing values to the stack can be done in different forms.
|     |     |
| --- | --- |
| Integer  | `3`    |
| Fraction | `1/4`  |
| Decimal  | `3.14` |

A prefix minus can be used with any valid value.\
`-5 -3/10 * p` prints `3/2`

A few constants are also available.
* `pi`
* `e`
* `g` (as 9.81)

`pi e - p` prints `0.423310825130748`

See [bases](#bases) for declaring integers in different bases.

### Bases
Base representations use digits 0-9, followed by characters a-z (lowercase). The maximum base supported is 36.\
There is no support for inputting or printing non-integer values in other bases.

#### Input
|     |     |
| --- | --- |
| Binary   | `0b(~)...` |
| Octal    | `0o(~)...` |
| Hex      | `0x(~)...` |
| Base `n` | `n(~)'...` |

For binary, octal, and hex, the leading `0` is optional.

`0b1001 p` prints `9`\
`0x5a p` prints `90`\
`3'12021 p` prints `142`

When `~` is given in the position indicated above, the input is interpreted as a radix complement representation (In binary, this is two's complement).

`0bc01001 p` prints `9`\
`0b~1001 p` prints `-7`\
`10~'856 p` prints `-144`

#### Printing
The `p` command can be extended with base information.
|     |     |
| --- | --- |
| Binary   | `pb(~)`  |
| Octal    | `po(~)`  |
| Hex      | `px(~)`  |
| Base `n` | `p'n(~)` |

`9 pb` prints `1001`\
`90 px` prints `5a`\
`142 p'3` prints `12021`\
`-9 pb` prints `-1001`

When `~` is given in the position indicated above, the value is printed in a radix complement representation (In binary, this is two's complement).

`9 pb~` prints `01001`\
`-7 pb~` prints `1001`\
`-144 p'10~` prints `856`

### Variables
Loading and storing variables is done with the `l` and `s` [commands](#commands).\
The below example uses the variables `x` and `y`.
```
> 1 sx 2 sy
> p
error: empty stack
> lx ly + p
 3
```

Here's an example of the quadratic formula in use.
```
> 1 sa -2 sb -15 sc
> lb neg lb 2 ^ 4 la * lc * - sqrt + 2 la * / p
 5.0
> lb neg lb 2 ^ 4 la * lc * - sqrt - 2 la * / p
 -3.0
```

### Macros

Macros are single words that are expanded into a list of instructions.\
They can be used to define new constants, avoid repetition of common patterns, or define new operators in terms of existing ones.

To define a macro in interactive mode, use the `:def` metacommand.
```
> :def triple 3 *
```
The first word is the name of the macro, and the remaining line will be its expansion.

Using macros is simple.
```
> 4 triple p
 12
> 9 triple p
 27
```

Here are a few examples of macros you may find useful.
```
nroot  recip pow
avg    z scnt ++ lcnt /
ncr    sr sn' ln' ! lr ! ln' lr - ! * i/
npr    sr sn' ln' ! ln' lr - ! i/
quad   sc sb sa lb neg lb 2 ^ 4 la * lc * - sqrt + 2 la * / lb neg lb 2 ^ 4 la * lc * - sqrt - 2 la * /
```

Macro definitions can contain other macros themselves.\
```
> :def triple 3 *
> :def inc 1 +
> :def tripinc triple inc
> 4 tripinc p
 13
```

Macros cannot contain themselves. This includes an indirect reference through other macros.\
```
> :def a b
> :def b a
```
Attempting to use such a macro will produce the following error:\
```
> a
parse error: unrecognised token (a)
```

#### Saved macros
To avoid redefining the same macro between sessions, you can save them.\
Saved macros are loaded from a macro file on start up, and are always available (including from inline mode).

The following files are tried in order:
*  A file supplied to the `--macro-file` option
*  `$RPNHS_MACRO_FILE`
*  `~/.rpnhs_macros`
*  `~/.config/rpnhs/macros`
*  `$XDG_CONFIG_HOME/rpnhs/macros`

The macro file should be a plain text file containing 1 macro definition per line.\
Empty lines and lines beginning with `#` are ignored.

For example:
```
~/.rpnhs_macros
----------------------
# nth root
# 64 3 nroot -> 4
nroot  recip pow

# mean
# c 8.2 4.3 10 5/2 avg -> 6.25
avg  z scnt ++ lcnt /
```

