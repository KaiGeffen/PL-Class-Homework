README
GENERAL
--------
Small functional programming language - Homework responses to Arjun Guha's (UMass) Fall 2018 Programming Languages class (631) done as exercise.
https://people.cs.umass.edu/~arjun/courses/compsci631-fall2018/home/
Answers by Kai Geffen


# Verifier

Verify that the user-specified assertions and loop-invariants are correct.
Takes a filepath to a program of the form 

`requires bexp; ensures bexp; cmd;`

Verify that if the program meets the requires conditions, it will meet the ensures conditions after running the command. It must also satisfy the loop invariant conditions of any while loops in the program.

We accomplish this by calculating the *weakest precondition*, which is the least constraining statement which always (In all worlds) results in the post condition being satisfied. If a precondition P satisfies `{P}c{Q}`then `P => wp`. Using that, we check that the given precondition implies our calculated weakest precondition  is *valid*.

Seperately, we verify that all of the loop invariants are upheld (On exit from the loop, the invariant implies the post-condition, and after each iteration of the loop the invariant is reestablished).
