#ifndef __FUNCTIONAL__
#define __FUNCTIONAL__

#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

/*

This is a project for me to sit down with a computer
and make a functional programming language. In order
to do so I'll need to make some constructions.

The grammar:
<exp> ::= <sym> | <app> | (<exp>) 
<app> ::= <exp> <exp>

f g <-> "calling f on g"

All symbols are strings, however that does not need
to stop the definer of the reductions to utilize data
in much different ways. We are in C, after all.

*/

typedef struct exp E; // <exp> ~ "expression"

typedef struct app A; // <app> ~ "application"

typedef struct sym S; // <sym> ~ "symbol"

typedef enum { 
    application=1,
    symbol=2
} exp_t; // to discriminate the union below

struct app {
    E *f;
    E *x;
};

struct sym {
    char* dum;
};

struct exp {
    exp_t type; // A or S
    union {
        A a;
        S s;
    };
};

/*

If you'd like to declare some constants or something of the sort,
above is the place to do it. For instance, something I'd do
is add a constant True or False.
-------------------------------------------------------------------
This is the interface for parsing expressions from some source of
text

*/

E parse(char* program, int length);

/*
Now, that above was the structure. It is really important
because it is going to heavily influence the way in which
you program the reductions and to understand the parser. 

The next thing is the interface for computation:
------------------------------------------------------------------
E reduce(E);
------------------------------------------------------------------

Ta da! This brings this expression, by whatever reduction you specify,
to some final form, which you also specify. This is basically just a base
case.

No, but really, this is where you would define all of the semantics of the
language. Other than that, it's really just a shell, devoid of universality
and quite shallow indeed.
*/

E reduce(E);

#endif
