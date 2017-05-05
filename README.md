# Written Test 1

## Logistics

All work on this test is to be done alone.  If you look things up on the Web,
cite them.  You can't get help from classmates, tutors, friends, or mentors on
answering the content of these questions.  If you're stuck on some technical
issue (like one of the provided compilers doesn't run, or you aren't sure about
some OCaml syntax), ask the staff.  Don't post public questions about this
assignment on Piazza.

You'll submit all your answers in plain text in a file in this directory, to
each (sub)question's corresponding text file, and push by the deadline. For
example, the answer of the question 1.4 should go to `q1_4.txt`. Please wrap the
text at some reasonable amount; 70-90 characters per line is useful.

The deadline is 11:59PM on Thursday, May 11.

## Rationale

No matter how you continue in the field of computer science—whether  you're a
software engineer, academic researcher, web developer, systems architect,
technical writer, etc—you will be confronted with numerous _design decisions_.
That is, you or someone on your team will have an idea for how to improve the
status quo, or how to get off the ground on a new project that you're not sure
how to start, and so on.  You will need to develop skills in effectively
judging the merit of these ideas, and communicating it to your team.

These skills are what these assignments seek to simulate (along with evaluating
your understanding of compilers).  Imagine that you are writing a technical
email to your peers or colleagues, and need to explain the reasons for
different decisions and how they are made.  These communications should be:

- Unambiguous, so that everyone knows what you mean (whether you are right or
  wrong).
- Descriptive, so that readers have examples or other concrete aspects of the 
  answer to respond to.
- Technically accurate, so that any assumptions made are explicit, and the the
  resulting chain of reasoning is clear
- Concise, so that people don't have to waste time wading through extra words


# Question 1

After working hard on a compiler similar to Anaconda for several days, your
teammate claims they have a great idea for improving the compiler.  The key
idea is that they add one new datatype, and another argument to the
`compile_expr` function:

```
type target
  | TMem of int * reg
  | TReg of reg

...

let rec compile_expr (e : expr) (si : int) (env : ((string * int) list)) (t : target) =
  ...
```

The goal, they say, is to avoid some extraneous `mov` instructions.  The
`target` datatype represents the location (register or memory) in which the
current expression should be stored, rather than always storing the result in
`EAX`.

This repository has two directories, `binops/` and `altered/`. `binops/`
contains the original compiler (similar to Anaconda), and `altered/` contains
the version with these modifications. Answer the following:

1.  Give an example of a program that has the _same_ instructions output on
both compilers.
2.  Give an example of a program that has _different_ instructions output when
compiled with the altered compiler.
3.  Will the altered compiler ever compile a program into an assembly program
with _more_ instructions than the original compiler?  Why or why not?
4.  Will the altered compiler ever compile a program into an assembly program
that gives a _different final answer_ (e.g. different value in `EAX`) than the
original?  Why or why not?
5.  Is the altered compiler a strict improvement over the original?  Why or why
not?


# Question 2

Frustrated with the lack of “normal” features in the snake languages, your
project manager demands that variable update and while loops be added.  This
would require some new syntax:

```
expr := while <expr>: <expr>
      | <name> := <expr>
      | begin: <expr>; ... end
```

And new expression forms:

```
type expr =
  | EWhile of expr * expr
  | EAssign of string * expr
  | EBegin of expr list
```

For example, this program should evaluate to 10:

```
let x = 0 in
let total = 0 in
while x < 5:
  begin:
    total := total + x;
    x := x + 1;
  end
total
```

This program should evaluate to 20:

```
let x = 5 in
let z = 8 in
let y = (x := 6) in
y + x + z
```

That is, an assignment expression evaluates to the value on the right-hand
side, and changes to the variable are reflected in expressions that evaluate
_after_ the assignment.

This program should evaluate to 10:

```
let x = 0 in
while x < 10:
  x := x + 1
```

That is, a  `while` loop evaluates to the value of its body on the last
iteration.


1. Either as psuedocode, OCaml code, or in clear prose, describe an
implementation of these three new features.  Take the starter code in the
released Cobra assignment as a starting point if you want to consider a
concrete compiler to add this to.  Consider handling the following:

    - What does a `while` expression evaluate to if the loop body runs zero times?
    - What new well-formedness errors are possible?
    - What instructions will do the work for each new expression?

2. Does this give us any new capabilities beyond what Cobra can calculate?  For
example, could we write some arithmetic algorithm with these features that is
simply impossible to write with the functions provided by Cobra?  Why or why not?


# Question 3

In the calling convention we presented in lecture, and the extended version
used for Cobra, we use a word on the stack to store the old value of `ESP`.  I
argue that this isn't strictly necessary.  That is, instead of looking up the
old value of `ESP` on the stack, we could avoid storing it entirely, and do
some other operation to “undo” the change to the stack pointer that happened
before the call.  Discuss this claim – if you think the value of `ESP` _must_
be stored, explain why (perhaps with a concrete example or two).  If you think
it's possible to omit the `ESP` storage, describe how the altered compiler
would work.

