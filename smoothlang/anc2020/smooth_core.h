
#ifndef _SMOOTH__H
#define _SMOOTH__H

/* Include the definitions being exported by RTS for use in modules */
#include "smoothlang/anc2020/smooth.h"

/* Include some basic definitions found in C for the compiled output to use */
#include <stdlib.h> /* EXIT_SUCCESS, NULL etc. */

#include <stdint.h> /* uint8_t */

/* Useful just for within the generated code */
#define LOCAL           local
#define SELF            self
#define SET(a, b)       a = b
#define REPEAT(c, n)    for (i = (n); i > 0; --i) { c; }

#define VARIABLE_LOOKUP(n) ((n) < numlocals ? locals[n] : CLOSURE_LOOKUP(self, (n) - numlocals))

#define LAMBDA(x)   smooth_lambda(x)
#define UNLAMBDA(x) smooth_unlambda(x)


/* These are not used at all when using native stack */
#ifdef NATIVE_STACK
#  define PUSH(x) return (x)
#else
#  define CALL(x)  smooth_call((smooth) x)
#  define PUSH(x) smooth_stack_push((smooth) x)
#  define POP()   smooth_stack_pop()
#endif

#define CORE_INIT(p) smooth_core_init(p)


/*
 * These allow us to have static code implementations for the generated code
 * whilst leaving an external signature for smooth.h
 */
#ifdef RTS_STATIC
#define CLOSURE_CREATE(x,y,z,a,b) closure_create(x,y,z,a,b)
#define CLOSURE_LOOKUP(x,y)       closure_lookup(x,y)
#define APPLY(f,l,n)              apply(f,l,n)
#define BOX(x)                    box(x)
#define UNBOX(x)                  unbox(x)
#else
#define CLOSURE_CREATE(x,y,z,a,b) smooth_closure_create(x,y,z,a,b)
#define CLOSURE_LOOKUP(x,y)       smooth_closure_lookup(x,y)
#define APPLY(f,l,n)              smooth_apply(f,l,n)
#define BOX(x)                    smooth_box(x)
#define UNBOX(x)                  smooth_unbox(x)
#endif


/**********************************/

typedef uint8_t byte;

/**********************************/

/*
 * If we are using RTS_STATIC, closure_create will be used as a static procedure
 * else the macro will point to the normal smooth_closure_create procedure
 */
#ifdef RTS_STATIC

/* SOMEHOW INCLUDE THE SOURCE HERE (pass it in as a constant on command line?) */

RTS_STATIC

#else

#  ifdef __cplusplus
extern "C" {
#  endif

  smooth smooth_lambda   (smooth x);
  smooth smooth_unlambda (smooth x);

  void smooth_call (smooth x);

  void smooth_stack_push (smooth x);
  smooth smooth_stack_pop (void);

  void smooth_core_init (const smooth_size* lambda_sizes);

#  ifdef __cplusplus
}
#  endif

#endif /* !RTS_STATIC */

#endif /* _SMOOTH__H */

