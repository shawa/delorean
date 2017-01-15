![](http://shawa.netsoc.ie/i/OXtfSA.png)

Delorean
========

(A **Time Travelling** Interpreter)

**Delorean** is a toy interpreter for a toy statement language that
supports stepping forward and back through a program's execution, with
various inspection tools. It also does rudimentary static analysis.

Usage
-----

The wonderful Docopt is used to define and parse the CLI. To run on a
particular program:

    $ stack build
    $ stack exec delorean-exe -- <infile.txt>

Intepreter
----------

Delorean will launch into the debug prompt. The current statement to be
executed can be displayed with the `list` command (or, as with all of
the commands, its initial `l`)

    ___  __ \_______  /________________________ _______
    __  / / /  _ \_  / _  __ \_  ___/  _ \  __ `/_  __ \
    _  /_/ //  __/  /__/ /_/ /  /   /  __/ /_/ /_  / / /
    /_____/ \___//_____|____//_/    \___/\__,_/ /_/ /_/
    The Amazing Time-Travelling Interpreter!
    type `help` or `h` for a list of commands

    delorean> l
    a <- 50: Int
    b <- 10: Int
    b <- 30: Int
    c <- a + b
    print c
    if b > a then
      print b
    else
      print a
    fi
    delorean>

There is some amount of `show` trickery to get basic pretty printing of
source listings.

To execute lines of the program, use `step`, or `s` for short:

    delorean> l
    a <- 50: Int
    b <- 10: Int
    b <- 30: Int
    c <- a + b
    print c
    if b > a then
      print b
    else
      print a
    fi
    delorean> step
    delorean> s
    delorean> i a
    50: Int
    delorean>

Variable History and Inspection
-------------------------------

The value of a particular variable can be inspected with the
`inspect <varname>` command. This will print out a list of all of the
values which `<varname>` has taken on up until the current line of
execution, most recent at the top.

    ...
    delorean> i a
    50: Int
    delorean> i b
    30: Int
    10: Int
    delorean>

As well as inspection of a single variable, `changes` shows all of the
different states that all of the variables have been in up until the
current point:


    delorean> c
    a -> 50: Int, b -> 30: Int   -- `a` is of type Int and mapped to the value 50
    a -> 50: Int, b -> 10: Int
    a -> 50: Int

Stepping Backwards (88)
-----------------------

As well as being able to step forward, Delorean can also step back
through the history of the program. At each execution of a statement,
the state of the program along with that statement is pushed onto the
'life' stack.

When stepping back, a (Statement, Environment) pair is popped off, and
the Statment Prompted for execution, the effect being that `step` now
executes the *last-popped* statement, before executing the statement the
program was on before `back` was issued:

    delorean> list
    a <- 50: Int
    delorean> step
    delorean> list
    b <- 10: Int
    delorean> step
    delorean> l
    b <- 30: Int
    delorean> back
    delorean> l
    b <- 10: Int
    delorean> b
    delorean> l
    a <- 50: Int

Carefull not to step back to before the start of the program; in that
case the interpreter will, unfortunately, crash.

Other Commands
--------------

`dump` will dump out the (Statement, Environment) stack used when
stepping back. This is mainly useful for debugging the interpreter
itself, but sure whats the harm in leaving it in:

    delorean> d
    [(b <- 30: Int,fromList [("a",50: Int),("b",30: Int)]),(b <- 10: Int,
    fromList [("a",50: Int),("b",10: Int)]),(a <- 50: Int,fromList [("a",
    50: Int)]),(pass,fromList [])]

`help` lists all available commands:

    h help    : Print this message
    s step    : Execute one statement of the program
    b back    : Undo one statement of the program
    l list    : List the current statement
    c changes : List all different states of the program
    d dump    : (debug) Dump out all runtime data
    i inspect <variable name>:
         Inspect given variable's content

Static Analysis
---------------

Originally the plan was to use GADTs to grant static types to the
Expression language, but that ended up being a lot more troublesome than
initially expected.

What's much easier to do is to check for declared, but unused variables!

The simple approach is that
`{unused variables} = {declared variables} \ {used variables}`. We can
easily define a *declared variable* as any variable appearing in an
`(Assign <name> <expr>)` statement, and a *used* variable as any
variable appearing in any Expression anywhere in the program.

Upon opening delorean on a program with unused variables, a basic
message is printed out:

    ___  __ \_______  /________________________ _______
    __  / / /  _ \_  / _  __ \_  ___/  _ \  __ `/_  __ \
    _  /_/ //  __/  /__/ /_/ /  /   /  __/ /_/ /_  / / /
    /_____/ \___//_____|____//_/    \___/\__,_/ /_/ /_/
    The Amazing Time-Travelling Interpreter!
    type `help` or `h` for a list of commands

    Yikes, variable b is defined but never used!
    delorean>

Or for a larger example:

    Great Scott! The variables b, c, d, e are defined, but never used!
    delorean> list
    a <- 50: Int
    b <- 10: Int
    c <- 10: Int
    d <- 10: Int
    e <- 10: Int
    print a

