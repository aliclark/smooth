#ifndef _SMOOTH__H
#define _SMOOTH__H

/* Include the definitions being exported by RTS for use in modules */
#include "smoothlang/anc2020/smooth.h"

#ifndef SMOOTH_NO_LIBS
/* Include some basic definitions found in C for the compiled output to use */
#include <stdlib.h> /* EXIT_SUCCESS, NULL etc. */

#include <stdint.h> /* uint8_t */
#endif

#ifndef EXIT_SUCCESS
#  define EXIT_SUCCESS 0
#endif

#ifndef EXIT_FAILURE
#  define EXIT_FAILURE 1
#endif

#ifndef NULL
#  define NULL 0
#endif

/* Useful just for within the generated code */
#define LOCAL           local
#define SELF            self
#define SET(a, b)       a = b
#define REPEAT(c, n)    for (i = (n); i > 0; --i) { c; }

#define NOTP(a, b, c)   (((a) == (b)) ? (c) : (b))
#define LOCALSP_GET(x, y, z, w) smooth_localsp_get(x, y, z, w)

#ifdef SMOOTH_INLINE_VARIABLE_LOOKUP
#define VARIABLE_LOOKUP(n) (numlocals > n ? args[numlocals - (n + 1)] : CLOSURE_LOOKUP(self, ((closure*) self)->body->numlocals - (numlocals + n + 1)))
#else
#define VARIABLE_LOOKUP(n) smooth_variable_lookup(self, args, numlocals, n)
#endif

#ifdef SMOOTH_INLINE_LAMBDA_CAST
#define LAMBDA(x)   ((smooth) ((x) + smooth_lambda_sizes))
#define UNLAMBDA(x) ((smooth) (((smooth_size*) (x)) - smooth_lambda_sizes))
#else
#define LAMBDA(x)   smooth_lambda(x)
#define UNLAMBDA(x) smooth_unlambda(x)
#endif

#define SCOPE_DOWN(x) smooth_gc_decref_scope(self, args, numlocals, x)
#define SCOPE_REMOVE() smooth_gc_remove_scope(self, args, numlocals)


/* These are not used at all when using native stack */
#ifdef SMOOTH_NATIVE_STACK
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
#ifdef SMOOTH_RTS_STATIC
#define CLOSURE_CREATE(x,y,z,a,b) closure_create(x, y, z, a, b)
#define CLOSURE_LOOKUP(x,y)       closure_lookup(x, y)
#define APPLY(f,l,n)              apply(f, l, n)
#define BOX(x)                    box(x)
#define UNBOX(x)                  unbox(x)
#else
#define CLOSURE_CREATE(x,y,z,a,b) smooth_closure_create(x, y, z, a, b)
#define CLOSURE_LOOKUP(x,y)       smooth_closure_lookup(x, y)
#define APPLY(f,l,n)              smooth_apply(f, l, n)
#define BOX(x)                    smooth_box(x)
#define UNBOX(x)                  smooth_unbox(x)
#endif


/* This must be a macro since it uses goto */
#define TAIL_APPLY(f, l, n)                                             \
  do {                                                                  \
    if (smooth_is_lambda(f) && (n == (*((smooth_size*) f)))) {          \
      pc        = UNLAMBDA(f);                                          \
      self      = (smooth) NULL;                                        \
      args      = l;                                                    \
      numlocals = n;                                                    \
      goto jump;                                                        \
    } else if (smooth_is_closure(f) &&                                  \
               (n == (((closure*) f)->body->numlocals - ((closure*) f)->curpos))) { \
      pc        = UNLAMBDA(((closure*) f)->code);                       \
      self      = f;                                                    \
      args      = l;                                                    \
      numlocals = n;                                                    \
      goto jump;                                                        \
    } else {                                                            \
      PUSH(APPLY(f, l, n));                                             \
    }                                                                   \
  } while (0)


#define INCREF(x) smooth_gc_incref(x)
#define DECREF(x) smooth_gc_decref(x)


/**********************************/

#ifndef SMOOTH_NO_LIBS
typedef uint8_t byte;
#else
typedef char byte;
#endif

#ifndef __cplusplus
typedef byte bool;
#endif

typedef struct closure_body {
  smooth* locals;
  int     numlocals;
  int     curpos;
} closure_body;

typedef struct closure {
  smooth code;
  int    curpos;
  closure_body* body;
  struct closure* parent; /* Allows access to more closed variables. */
} closure;





typedef closure_scope_prevals {
  smooth* values;
  bool*   isset;
  /* Once refcount is 0 we can delete this memory and remove the pointer from scope list */
  int     refcount;
} closure_scope_prevals;

typedef struct closure_scope_list {
  /* No size necessary because we already know that.
   * It may be possible at some point to use the actual pointer space as the value
   * if there is only one value needed
   * (this may however introduce the need for a count or boolean "more than one" flag.
   */
  smooth*                values;
  closure_scope_prevals* prevals;
  struct closure_scope_list* next;
  /* Once refcount is 0 we can delete 'values' and decref the prevals memory (but we stay in the chain) */
  int  refcount;
} closure_scope_list;


typedef struct closure_args_fill_body {
  smooth* locals;
  int     numlocals;
  int     curpos;
} closure_args_fill_body;

typedef struct closure_args_fill {
  int curpos;
  closure_args_fill_body* body;
} closure_args_fill;


/* Either of these pointers may be null, but not both
 * (if both were null we could have just used the lambda or native address on its own)
 */
typedef struct closure {
  closure_args_fill*  args;
  closure_scope_list* scope;
  smooth code;
} closure;



/**********************************/

/*
 * If we are using SMOOTH_RTS_STATIC, closure_create will be used as a static procedure
 * else the macro will point to the normal smooth_closure_create procedure
 */
#ifdef SMOOTH_RTS_STATIC

/* SOMEHOW INCLUDE THE SOURCE HERE (pass it in as a constant on command line?) */

#include "smooth_core.c"

#else

#  ifdef __cplusplus
extern "C" {
#  endif

  smooth smooth_lambda   (smooth x);
  smooth smooth_unlambda (smooth x);

  void   smooth_call (smooth x);

  void   smooth_stack_push (smooth x);
  smooth smooth_stack_pop (void);

  void   smooth_core_init (const smooth_size* lambda_sizes);

  smooth smooth_gc_decref_scope (smooth closure, smooth* locals, smooth_size numlocals, smooth rv);
  void   smooth_gc_remove_scope (smooth closure, smooth* locals, smooth_size numlocals);

  bool   smooth_is_lambda  (smooth x);
  bool   smooth_is_closure (smooth x);

  smooth* smooth_localsp_get (smooth f, smooth_size n, smooth* locals, smooth* jmps);

#ifndef SMOOTH_INLINE_VARIABLE_LOOKUP
  smooth smooth_variable_lookup (smooth self, smooth* args, smooth_size numlocals, smooth_size n);
#endif

#  ifdef __cplusplus
}
#  endif

#endif /* !SMOOTH_RTS_STATIC */

#endif /* _SMOOTH__H */

