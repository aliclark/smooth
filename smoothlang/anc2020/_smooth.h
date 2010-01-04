
#ifndef _SMOOTH__H
#define _SMOOTH__H

#include "smoothlang/anc2020/smooth.h"

#ifndef SMOOTH_FIXED_STACK
#include "smoothlang/anc2020/linkedarray/ali_linked_array.h"
#endif


#define EXIT_SUCCESS 0
#define NULL         0


#ifndef __attribute__
#  define __attribute__(x) /*0*/
#endif /* __attribute__ */



#ifndef SMOOTH_STACK_SIZE
#  ifdef SMOOTH_FIXED_STACK
#    define SMOOTH_STACK_SIZE SMOOTH_FIXED_STACK
#  else
#    define SMOOTH_STACK_SIZE 256
#  endif
#endif


#define SMOOTH_SET(a, b)       a = b
#define SMOOTH_CLOSURE_CAST(a) ((smooth_closure_t*) a)

#define SMOOTH_CALL(x) smooth_call((smooth_t) x)

/*
 * Something like this will be used sometime in the future,
 * but cannot be used conditionally.
 * Perhaps when I detect recursion I can use this instead?
 */
#define SMOOTH__CALL(x) \
  do { \
    if (SMOOTH_LAMBDA_P(x)) { \
      SMOOTH_PUSH(NULL); \
      smooth_pc = x; \
      goto jump; \
    } else { \
      smooth__call((smooth_t) x); \
    } \
  } while (0)

#define SMOOTH_REPEAT(c, n)    for (i = n; i > 0; --i) { c; }


#ifdef SMOOTH_FIXED_STACK
#  define SMOOTH__PUSH(x) *smooth_sp++ = ((smooth_t) (x))
#  define SMOOTH__POP()   (*--smooth_sp)
#  define SMOOTH_SPOFF(x) smooth_sp[x]
#else
#  define SMOOTH__PUSH(x) smooth_push(x)
#  define SMOOTH__POP()   *smooth__linked_array_get(smooth_stack, --smooth_sp)
#  define SMOOTH_SPOFF(x) *smooth__linked_array_get(smooth_stack, smooth_sp + (x))
#endif /* SMOOTH_FIXED_STACK */

#define SMOOTH_TOS()   SMOOTH_SPOFF(-1)
#define SMOOTH_SPINC() ++smooth_sp
#define SMOOTH_SPDEC() --smooth_sp


#define SMOOTH_CLOSURE_MEM  128

/*
When you use SMOOTH_*_P, exactly one of the predicates will be true, no matter what you pass.
In other words, don't use SMOOTH_*_P unless you know you have some kind of function/procedure.
*/

#define SMOOTH_LAMBDA_P(x) (((smooth_t) (x)) < 128)

/*
 * Take an address and see if it is within our closure memory space.
 * This makes the assumption that the `smooth_closures` address is a much higher number
 * than any of the jump location numbers in execute.
 * For that reason, I would be much happier to just always assume that a pointer returned
 * is one of these, than to test with SMOOTH_CLOSP.
 */
#define SMOOTH_CLOSURE_P(x)                                             \
  ((((smooth_t) x) >= ((smooth_t) smooth_closures)) &&                  \
   (((smooth_t) x) < ((smooth_t) (smooth_closures + SMOOTH_CLOSURE_MEM))))

#define SMOOTH_PRIMITIVE_P(x) (!(SMOOTH_CLOSURE_P(x) || SMOOTH_LAMBDA_P(x)))


#define SMOOTH__CLOSURE_CODE(x)   ((smooth_closure_t*) x)->lambda
#define SMOOTH__CLOSURE_LOCAL(x)  ((smooth_closure_t*) x)->local
#define SMOOTH__CLOSURE_PARENT(x) ((smooth_closure_t*) x)->parent


/**********************************/


struct smooth_closure {
  smooth_t lambda;               /* where to run code from. Could be native or on host. */
  smooth_t local;                /* The local variable for this closure. */
  struct smooth_closure* parent; /* Allows access to more closed variables. */
};

typedef smooth_t smooth_th_t;


/**********************************/

extern smooth_t smooth_pc;

#ifdef SMOOTH_FIXED_STACK
extern smooth_t* __restrict__ smooth_sp;
#else
extern smooth_linked_array_t* __restrict__ smooth_stack;
extern smooth_t  smooth_sp;
#endif

extern smooth_closure_t smooth_closures[SMOOTH_CLOSURE_MEM];

void smooth_call  (smooth_t x);
void smooth__call (smooth_t x);

#endif /* _SMOOTH__H */
