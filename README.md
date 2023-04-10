# betty: my little programming language

## What is betty?

betty is a programming language that I wrote for fun and to learn. It has been designed to be a simple language with a limited but essential set of features, such as control flow, loops, functions and error handling among others. It is defined as _dynamical_, _functional_ and _high level_.
It is a simple yet complete language, featuring:
- Different datatypes, such as `Int`, `Float`, `String`, `Bool`, `Vector` and others.
- Control flow using `if`, `else`, `match`.
- Loop is done via `while`, `for`, `foreach`.  
- First class functions, anonymous and builtin functions.
- Error handling with the `try ... catch ... else` block. Custom errors definition is available as well.
- Import external betty files with the `using` statement.
- Comments start with the pipe symbol.
- . . . and more.

## Compile betty.exe
In order to compile the interpreter, run the following command after downloading the repo:

```
cargo b --release
```

## Interpreter options
<ins>Usage:</ins> `betty.exe [OPTIONS] [PATH] [ARGS]...`
<ins>Arguments:</ins>
- `[PATH]` Path of the betty file. If not provided, run repl console
- `[ARGS]...` Additional args provided to the program

<ins>Options:</ins>
- `-t --time` Time the program execution
- `--stack-size <STACK_SIZE>`  Allocate memory for the stack (in bytes) [default: `2 * 1024 * 1024`]
- `-h --help` Print help
- `-V --version` Print version

## The REPL
To open the repl, run the executable without arguments.

```
.\betty.exe
```

After doing so, the console will open and an input symbol (`>>>`) will pop up, waiting for you to write. You can enter expressions and statements, but you are only allowed to write a single statement at a time. If the expression returns `nothing`, the repl will not print anything.

```betty
betty 0.1.0
-----------
>>> 1 + 2
3
>>> assert(1 + 2 = 3)
>>> for i => 3 do println(i) end
0
1
2
>>>
```

To gracefully quit the repl, use Ctrl+Z. To forcefully exit the program, use Ctrl+C.

## Availability
At the moment, betty is only available for the Windows operating system.

## Examples

### Operations

```betty
>>> 1 + 2 + 3  | Add
6
>>> 1 - 2 - 3  | Subtract
-5
>>> 4 * 4      | Multiply
16
>>> 5 / 2      | Divide
2.500000000000000
>>> 2 ^ 5      | Exponentiation
32
>>> 10 % 3     | Remainder
1
>>> --3        | Unary minus
3
>>> 3 + +3     | Unary plus
6
>>> 1 in [1, 2, 3]    | Contains with Vector
true
>>> "ab" in "abcd"    | Contains with String
true
>>> true and false    | Logical and
true
>>> true or false     | Logical or
true
>>> not false         | Logical not
true
```

### Variables

```betty
| Assignment
age :: 22
name :: "root"

| Reassignment
age :+ 1  | Now 23

| Conditional assignment
warning :: age >= 18 ? "Drink carefully" else "Don't drink"
```

### Control flow

```betty
| if, else if, else
age :: 22

if age < 18 do
    println("You are not an adult yet!")
else if age = 18 do
    println("You are a young adult!")
else do
    println("You are not an adult!")
end

| match statement
name :: "betty"
match name do
    "betty" => println("Hey betty!")
    "karl" => println("Hey karl!")
    else => println("neither betty nor karl")
end
```

### Loop

```betty
| while loop
i :: 0
while i < 5 do
    println(i)  | 0 1 2 3 4
    i :+ 1
end

| for loop (infer start and step)
for i => 5 do
    println(i)  | 0 1 2 3 4
end

| for loop (infer step)
for i :: 4 => 6 do
    println(i)  | 4 5
end

| for loop (infer start)
for i => -10, -2 do
    println(i)  | 0 -2 -4 -6 -8
end

| foreach loop (on Vector)
foreach item in [1, 2, 3] do
    println(item)  | 1, 2, 3
end

| foreach loop (on String)
foreach char in "hello" do
    println(char)  | h e l l o
end
```

### Functions

```betty
| Declare a function
fun add(x, y) do
    return x + y
end

| Declare an anonymous function
add :: fun(x, y) => x + y

| Declare an anonymous function with multiple statements
pythagorean_theorem :: fun (a, b) do
    a :* 2
    b :* 2
    c :: (a + b) ^ 0.5
    return c
end
```

### Error handling

```betty
| throw an error
throw ValueError

| throw an error with a custom message
throw ValueError("My custom message")

| Catch a ValueError and do not bind it to any variable
try do
    do_something()
catch ValueError do
    println("A ValueError was caught")
end

| Catch a ValueError and bind it to 'err'
try do
    do_something()
catch err :: ValueError do
    println("A ValueError was caught:\n", err)
end

| Multiple error caught on the same catch block
try do
    do_something()
catch err :: ValueError, DivisionByZeroError do
    println("Caught one of two errors:\n", err)
end

| Multiple catches with else block
try do
    do_something()
catch err :: ValueError do
    println("Here's the ValueError:\n", err)
catch err :: DivisionByZeroError do
    println("Here's the DivisionByZeroError:\n", err)
else do
    println("No error was thrown!")
end

| Define custom errors, which we can throw
newerror MyCustomError
throw MyCustomError("My custom message...")
```

### Imports

```betty
| Import a module named 'mod.betty'
| If the extension of the file is '.betty', you can omit it
using "mod"

| Relative import and aliases
using foo, bar as br in "mod"
```
