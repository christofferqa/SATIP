Static Analysis for TIP
=======================
An implementation of a bunch of different static analyses presented by [Anders Møller](http://cs.au.dk/~amoeller/) during the [Static Analysis](http://cs.au.dk/SA) course at University of Aarhus for TIP (Tiny Imperative Language).

Contributors:
* Christoffer Quist Adamsen, <christofferqa@gmail.com>

* Troels Leth Jensen, <troelslethjensen@gmail.com>


RUN
===
Use `make compile` to compile the files, and e.g. `make run FILE=tests/sign_interprocedural_normalized.tip` to run the analyses on sign_interprocedural_normalized.tip.


TIP
===
Programs:
    P ::= F
        | F P

Functions:
    F ::= id(id, ..., id) { S return E; }

Statements:
    S ::= var id, ..., id;
        | id = E;
        | *id = E;
        | output E;
        | S S
        | if (E) { S }
        | if (E) { S } else { S }
        | while (E) { S }

Expressions:
    E ::= intconst
        | id
        | E+E | E-E | E*E | E/E | E>E | E==E
        | (E)
        | input
        | id(E, ..., E)
        | (E)(E, ..., E)
        | &id
        | malloc
        | *E
        | null


ANALYSES
========
* Type Analysis

* Control Flow Analysis/Closure Analysis

* Intraprocedural Liveness Analysis

* Intraprocedural Available Expressions Analysis

* Intraprocedural Constant Propagation Analysis

* Intraprocedural Initialized Variables Analysis

* Intraprocedural Reaching Definitions Analysis

* Intraprocedural Very Busy Expressions Analysis

* Intraprocedural, Context Insensitive Interprocedural, and Context Sensitive Interprocedural Sign Analysis

* Interprocedural Pointer Analysis using Andersen's Analysis


TODO
====
* Currently working on making a separate normalized AST. As a consequence Andersen's Analysis is currently commented out.

* The interprocedural analyses assumes that variable names are unique across functions.