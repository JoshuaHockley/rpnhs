# rpnhs
A [reverse Polish notation](https://en.wikipedia.org/wiki/Reverse_Polish_notation) calculator in Haskell.

Inspired by [rpnpy](https://github.com/terrycojones/rpnpy), [dc](https://www.gnu.org/software/bc/manual/dc-1.05/html_mono/dc.html), and [Forth](https://en.wikipedia.org/wiki/Forth_(programming_language)).

For convenience, you may want to alias rpnhs in your shell.\
`alias hc='rpnhs'` (short for Haskell calculator)

### Feature overview
* General mathematical operators (see [operator list](#operators))
* Bitwise operations on integers
* Accepts values as integers (`3`), fractions (`1/4`), and decimals (`3.14`) (see [values](#values))
* Uses a fractional representation where possible to avoid floating point inaccuracies (`1/2 + 1/2 = 1`)
* Use arbitrary bases for both input and output of integers, supporting radix complement (see [bases](#bases))
* Load and store variables to and from the stack (see [variables](#variables))
* Define subroutines for common patterns (e.g. `triple` -> `3 *`) (see [subroutines](#subroutines))
* Map and fold over the stack (see [map and fold](#map-and-fold))
* Basic imperative programming (see [programming](#programming))

### Commandline options
```
-P,--prompt PROMPT       Set the interactive mode prompt
-p,--auto-print          Auto print if no output is produced from inline mode
-i,--eprint-prog         Print the program marking the current instruction
                         when an error occurs
-s,--eprint-stack        Print the stack when an error occurs
-d,--def-file FILE       Load a given definition file on startup
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

Note the single quotes used in the second example. This is to prevent the shell expanding `*` as a glob pattern.\
An alternative solution would be to use `mul` instead which is an alias for `*`. See [operators](#operators) for a complete list of operator aliases.

If execution fails, no output will be made to stdout, and an error message will be printed to stderr.\
`rpnhs 1 2 + p hi` produces only a parse error.

#### Interactive
To start the interactive mode, run rpnhs with no arguments.\
`rpnhs`

You are then presented with a prompt to input any number of instructions. After pressing enter, the instructions will be executed, and the state of the calculator will be updated.
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
| Metacommand              | Description                                                           |
| :----------------------- | :-------------------------------------------------------------------- |
| `:q`                     | Exit the calculator (same as `q`)                                     |
| `:u(n)`                  | Undo the last `n` successful lines (default 1)                        |
| `:def (subroutine body)` | Define a new subroutine (see [subroutines](#subroutines) for details) |


### Commands
Commands are instructions for interacting with the calculator in ways other than pushing values and applying operators.
| Command                  | Description                                                                                        |
| :----------------------  | :------------------------------------------------------------------------------------------------- |
| `print`, `p`             | Print the value at the top of the stack (see [bases](#bases) for printing in other bases)          |
| `stack`, `f`             | Print all values in the stack, with the top value at the rightmost position                        |
| `pop[n]`, `r[n]`         | Pop the top `n` (default 1) values from the stack (without printing)                               |
| `clear`, `c`             | Empty the stack                                                                                    |
| `dup[n]`, `d[n]`         | Duplicate the top value of the stack `n` times (default 1)                                         |
| `pull[n]`, `pl[n]`       | Pull the `n`th value on the stack to the top                                                       |
| `push[n]`, `ps[n]`       | Push the top of the stack down `n` elements                                                        |
| `swap`, `s`              | Swap the top 2 values on the stack (alias for `push1`)                                             |
| `depth`, `z`             | Push the current depth of the stack                                                                |
| `store[name]`, `s[name]` | Pop the top value on the stack and store it in the variable `name`, overwriting any previous value |
| `load[name]`, `l[name]`  | Load the value of `name` and push it to the stack (fails if undefined)                             |
| `view[name]`, `v[name]`  | View the value of `name`                                                                           |
| `view`, `v`              | View the values of all variables                                                                   |

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

### Operators
Operators pop 1-2 values from the top of the stack, and push back a single result.

#### General
| Operator            | Notes                                                  | Arity |
| :------------------ | :----------------------------------------------------- | :---- |
| `abs`, `\|\|`       |                                                        | 1     |
| `neg`, `negate`     |                                                        | 1     |
| `recip`             |                                                        | 1     |
| `sqrt`, `root`      |                                                        | 1     |
| `exp`, `e^`         |                                                        | 1     |
| `ln`                |                                                        | 1     |
| `log2`              |                                                        | 1     |
| `fact`, `!`         |                                                        | 1     |
| `add`, `plus`, `+`  |                                                        | 2     |
| `sub`, `minus`, `-` |                                                        | 2     |
| `mul`, `times`, `*` |                                                        | 2     |
| `div`, `/`          | true division                                          | 2     |
| `idiv`, `i/`        | integer division                                       | 2     |
| `mod`, `%`          | truncated towards negative infinity                    | 2     |
| `pow`, `^`          |                                                        | 2     |
| `log`               | `a b log -> logb(a)`                                   | 2     |
| `ffact`, `f!`       | falling factorial: `m n f! -> m! / (m - n)!`           | 2     |
| `rfact`, `r!`       | rising factorial: `m n r! -> (m + n - 1)! / (m - 1)!`  | 2     |

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

#### Comparison operators
| Operator        | Arity |
| :-------------- | :---- |
| `lt`, `<`       | 2     |
| `lte`, `<=`     | 2     |
| `eq`, `==`, `=` | 2     |
| `neq`, `!=`     | 2     |
| `gte`, `>=`     | 2     |
| `gt`, `>`       | 2     |

These operators push `1` to the stack if the result is true, or `0` if the result is false.\
`3 4 < p` prints `1`

They are useful in combination with `If` and `While` (see [programming](#programming)).

#### Misc
| Operator          | Notes                                                                              | Arity |
| :---------------- | :--------------------------------------------------------------------------------- | :---- |
| `rnd`, `round`    | round to the nearest integer                                                       | 1     |
| `floor`           | round to the greatest integer <= the value                                         | 1     |
| `ceil`, `ceiling` | round to the least integer >= the value                                            | 1     |
| `fl`, `float`     | convert a value to a float (useful if a fractional representation in inconvenient) | 1     |

### Bases
Base representations use digits 0-9, followed by characters a-z (lowercase). The maximum base supported is 36.\
There is no support for inputting or printing non-integer values bases other than decimal.

#### Input
|     |     |
| --- | --- |
| Binary   | `0b(~)...`  |
| Octal    | `0o(~)...`  |
| Hex      | `0x(~)...`  |
| Base `n` | `[n(~)]...` |

For binary, octal, and hex, the leading `0` is optional.

`0b1001 p` prints `9`\
`0x5a p` prints `90`\
`[3]12021 p` prints `142`

When `~` is given in the position indicated above, the input is interpreted as a radix complement representation (In binary, this is two's complement).

`0b~01001 p` prints `9`\
`0b~1001 p` prints `-7`\
`[10~]856 p` prints `-144`

#### Printing
The `p` (or `print`) command can be extended with base information.
|     |     |
| --- | --- |
| Binary   | `pb(~)`   |
| Octal    | `po(~)`   |
| Hex      | `px(~)`   |
| Base `n` | `p[n(~)]` |

`9 pb` prints `1001`\
`90 px` prints `5a`\
`142 p[3]` prints `12021`\
`-9 pb` prints `-1001`

When `~` is given in the position indicated above, the value is printed in a radix complement representation (In binary, this is two's complement).

`9 pb~` prints `01001`\
`-7 pb~` prints `1001`\
`-144 p[10~]` prints `856`

### Variables
Loading and storing variables is done with the `l` (or `load`) and `s` (or `store`) [commands](#commands).\
The below example uses the variables `x` and `y`.
```
> 1 s[x] 2 s[y]
> p
error: empty stack
> l[x] l[y] + p
 3
```

Here's an example of the quadratic formula in use.
```
> 1 s[a] -2 s[b] -15 s[c]
> l[b] neg l[b] 2 ^ 4 l[a] * l[c] * - sqrt + 2 l[a] * / p
 5.0
> l[b] neg l[b] 2 ^ 4 l[a] * l[c] * - sqrt - 2 l[a] * / p
 -3.0
```

### Subroutines

Subroutines are single words that represent a list of instructions.\
They can be used to define new constants, avoid repetition of common patterns, or define new operators in terms of existing ones.

To define a subroutine in interactive mode, use the `:def` metacommand.
```
> :def triple 3 *
```
The first word is the name of the subroutine, and the remaining line is its definition.

Using subroutines is simple.
```
> 4 triple p
 12
> 9 triple p
 27
```

You can think of each call to `triple` as being replaced by `3 *`.

Here are a few examples of subroutines you may find useful.
```
nroot  recip ^
++     Fold (+)
avg    z s[z] ++ l[z] /
npr    f!
ncr    d ps2 f! s ! i/
quad   s[c] s[b] s[a] l[b] neg l[b] 2 ^ 4 l[a] * l[c] * - sqrt + 2 l[a] * / l[b] neg l[b] 2 ^ 4 l[a] * l[c] * - sqrt - 2 l[a] * /
```

Subroutine definitions can contain calls to other subroutines themselves, so long as they have been defined first.
```
> :def triple 3 *
> :def inc 1 +
> :def tripinc triple inc
> 4 tripinc p
 13
```

#### Variables in subroutines
Subroutine calls don't have the same effect on variables as the same instructions inline would.
Instead, they behave somewhat like new scopes in a programming language: existing variables can be read, but new variables (or updates to existing variables) do not persist after the call.

This subroutine loads the value of `x` onto the stack.
```
> :def getx l[x]
```

No surprises here.
```
> 4 s[x] f
> getx f
 4
```

This subroutine adds the top values in the stack, and stores the result into `sum`.
```
> :def sum + s[sum]
> 1 2 + s[sum] v
 sum = 3
> 3 4 sum v
 sum = 3
```
The call to `sum` did store 7 to the variable, but after the call completes the scope is exited, and the value of `sum` reverts to its state before the call (in this case, 3).

The benefit of this behaviour is that you can call subroutines without variables being overwritten.\
For example, if the subroutine `quad` internally stores to `a`, `b`, and `c`, but you have an important result stored in `a`, you can still safely call the subroutine.
```
> v
 a = 234789
> 1 -2 -15 quad f v
 5.0 -3.0
 a = 234789
```

#### Saving definitions
To avoid redefining the same subroutines between sessions, you can save them.\
Saved definitions are loaded from a file on start up, and are always available (including from inline mode).

The following files are tried in order:
*  A file supplied to the `--def-file` option
*  `$RPNHS_DEFS`
*  `~/.rpnhs_defs`
*  `~/.config/rpnhs/defs`
*  `$XDG_CONFIG_HOME/rpnhs/defs`

The def file should be a plain text file containing 1 definition per line.\
Empty lines and lines beginning with `#` are ignored.

For example:
```
~/.rpnhs_defs
----------------------
# nth root
# 64 3 nroot -> 4
nroot  recip ^

...
```

### Map and Fold
The special commands `Map` and `Fold` can be used to manipulate the entire stack at once.\
They both take a 'block' as an argument to describe what should be done.

#### Blocks
Blocks are a list of instructions enclosed by parenthesis.\
For example: `(1)`, `(1 +)`, `(l[m] - 2 ^)`\
You can think of blocks as rpnhs' version of lambdas.\
Note that blocks cannot go on the stack like values. They can only be used directly after a special commands like `Map` or `Fold`.

#### Map
The `Map` special command allows you to transform the entire stack by running a block on each value in turn.\
For example, to 'add 1' to each value in the stack:
```
> 1 2 3 f
 1 2 3
> Map (1 +) f
 2 3 4
```

Under the hood this looks like...
```
[1 2 3]
1 1 + -> 2
2 1 + -> 3
3 1 + -> 4
[2 3 4]
```

Here's a more practical example.\
This subroutine uses a map and a [fold](#fold) to calculate the bias corrected sample variance of a list of numbers, given the mean is stored in `m`.
```
> :def svar z s[z] Map (l[m] - 2 ^) Fold (+) l[z] 1 - /
> f v
 59 55 33 16 23 55 24 45 23 57
 m = 39
> svar p
 286
```

So far `Map` has been passed blocks that transform 1 value into another, like `(1 +)`, but what happens when this isn't the case?
```
> 1 2 3 f
 1 2 3
> Map (0) f
 1 0 2 0 3 0
```

Under the hood this looks like...\
```
[1 2 3]
1 0 -> [1 0]
2 0 -> [2 0]
3 0 -> [3 0]
[1 0 2 0 3 0]
```
This example reveals the exact behaviour of `Map`.\
The block is run with each value, and then the results of each are joined together.\
You might recognise this behaviour as 'concat map' or 'flat map'.

One interesting case of is when we use `r` in the block to discard the value we are acting on.
```
> 1 2 3 f
 1 2 3
> Map (r) p
error: empty stack
```
This map seems to have emptied the stack.
We'll exploit this a bit later to filter the stack on arbitrary rules.

Note: Any changes to variables made within the block are discarded when the block finishes.

#### Fold
The `Fold` special command repeats the provided block until the stack contains at most 1 value.\
Perhaps the most useful fold is 'sum the stack':
```
> 1 2 3 4 Fold (+) f
 10
```

You can imagine that the fold will be replaced by the block the exact number of times needed to reach a stack of at most 1 value.
```
> 1 2 3 4 + + + f
 10
```

Note that there is no restriction on what the block does. If the stack never becomes small enough, the fold will not terminate.
The below fold will continually add 1 to the top of the stack, never shrinking it.
Use `ctrl-C` to force quit the calculator.
```
> 1 2 3 Fold (1 +) f
^C
```

### Programming
Basic programming is achievable with the `If` and `While` special commands.
Both take 2 blocks as arguments.

#### If
The `If` special command takes a 'then' and 'else' branch in the form of blocks.\
The top value is popped from the stack. If it is non-zero, the 'then' block is executed. If it is zero, the 'else' block is executed.\
If the stack is empty an error occurs.
```
> 1 If (2) (3) f
 2
> 0 If (2) (3) p
 3
```

To set up the condition you will likely want to use a [comparison operator](#comparison-operators).

This subroutine sets negative numbers to zero, while leaving positive numbers unchanged.
```
> :def clamp d 0 >= If () (r 0)
> 5 clamp p
 5
> -4 clamp p
 0
> 0 clamp p
 0
```

This is equivalent to:
```
def clamp(x):
    if x >= 0 then
        return x
    else
        return 0
```

Note the use of `d` at the start of the definition of `clamp`.\
Comparison operators are still operators (they consume their arguments), so if you still need a value after comparing it with something, either store it to a variable or 'back it up' on the stack with `d`.

Lastly, here's an example of how `If` can be used with `Map` to filter the stack.
```
> :def filter If () (r)
> :def pred s[x] l[x] 30 > l[x] 70 < &
> 59 55 33 16 23 55 24 45 23 57
> Map (d pred filter) f
 59 55 33 55 45 57
```

#### While
The `While` special command takes 2 blocks: a 'head' and a 'body'.\
First the head is executed.
Then the top of the stack is popped.
If the value is zero, the command finishes and execution continues.
If the value is non-zero, we continue, executing the body and the head before checking the condition again.

The 'head' and 'body' mimic the 'condition' and 'body' components of the typical while loop.\
Use the head block to set up the condition on the top of the stack, and the body for the actual operation.

The following program prints out the numbers 1-5.
```
> 1 While (d 5 <=) (p 1 +)
 1
 2
 3
 4
 5
```

This is equivalent to:
```
x = 1
while (x <= 5):
    print x
    x = x + 1
```

#### Example: The Collatz conjecture

Here's a more complex example of programming in rpnhs.\
The `collatz` subroutine computes the [Collatz conjecture](https://en.wikipedia.org/wiki/Collatz_conjecture) sequence for the number at the top of the stack. Each step in the sequence is printed, and the total number of steps taken is pushed onto the stack at the end.
```
> :def odd 2 %
> :def collatzStep d odd If (3 * 1 +) (2 i/)
> :def collatz 0 s[cnt] While (d 1 !=) (collatzStep p l[cnt] 1 + s[cnt]) r l[cnt]
> 10 collatz
 5
 16
 8
 4
 2
 1
> p
 6
```

#### Example: Godel numberings
To wrap up, here's a practical example where the programming features come in handy.

These subroutines facilitate translating between pairs/lists of naturals and a [godel number](https://en.wikipedia.org/wiki/G%C3%B6del_numbering) representation.\
The `<<>>` subroutines use the numbering `<<x, y>> = 2^x(2y + 1)` and map between `(N, N)` and `N+`.\
The `<>` subroutines use the numbering ` <x, y> = <<x, y>> - 1` and map between `(N, N)` and `N`.\
The `<[]>` subroutines use the numbering `[x1, x2, x3, ... xn] = <<x1, <<x2, <<x3, ... <<xn, 0>>...>>>>` and map between `[N]` and `N`.

```
# godel encode (N, N) -> N+
<<e>>  2 * 1 + s 2 s ^ *
# godel decode N+ -> (N, N)
<<d>>  0 s While (d 2 % 0 =) (1 >> s 1 + s) 1 >>
# godel encode (N, N) -> N
<e>    <<e>> 1 -
# godel decode N  -> (N, N)
<d>    1 + <<d>>
# godel encode [N] ->  N
<[e]>  0 Fold (<<e>>)
# godel decode  N  -> [N]
<[d]>  While (d 0 !=) (<<d>>) r
```

Here's a demonstration of encoding and then decoding a list of naturals.
```
> 1 2 3 4 <[e]> p pb
 8466
 10000100010010
> <[d]> f
 1 2 3 4
```

