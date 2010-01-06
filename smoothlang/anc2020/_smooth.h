
#ifndef _SMOOTH__H
#define _SMOOTH__H

#include "smoothlang/anc2020/smooth.h"


#ifndef SMOOTH_FIXED_STACK

/*
 * Please do not allocate one of these yourself;
 * use linked_array_allocate if you want one.
 */
typedef struct smooth_linked_array {
	smooth_t* array; /* The block of allocated memory for array contents. */
	smooth_t max;      /* The max capacity of the array memory for this head. */
	smooth_t length;   /* Holds the value (max*2), saving on calculation time. */
	struct smooth_linked_array* rest; /* The other half of the linked_array. */
} smooth_linked_array_t;


#endif


#define EXIT_SUCCESS 0
#define NULL         0


#ifndef SMOOTH_STACK_SIZE
#  ifdef SMOOTH_FIXED_STACK
#    define SMOOTH_STACK_SIZE SMOOTH_FIXED_STACK
#  else
#    define SMOOTH_STACK_SIZE 256
#  endif
#endif

#define LOCAL                  local
#define SELF                   self
#define SMOOTH_SET(a, b)       a = b
#define SMOOTH_CLOSURE_CAST(a) ((smooth_closure_t*) a)
#define SMOOTH_REPEAT(c, n)    for (i = n; i > 0; --i) { c; }

#define SMOOTH_CALL(x) smooth_call((smooth_t) x)

#ifdef SMOOTH_FIXED_STACK
#  define SMOOTH_PUSH(x)  *smooth_sp++ = ((smooth_t) (x))
#  define SMOOTH_POP()    (*--smooth_sp)
#  define SMOOTH_SPOFF(x) smooth_sp[x]
#else
#  define SMOOTH_PUSH(x)  smooth_la_push(x)
#  define SMOOTH_POP()    smooth_la_spoff(--smooth_sp)
#  define SMOOTH_SPOFF(x) smooth_la_spoff(smooth_sp + (x))
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

#ifdef __cplusplus
extern "C" {
#endif

  extern smooth_t smooth_pc;
  
#ifdef SMOOTH_FIXED_STACK
  extern smooth_t* __restrict__ smooth_sp;
#else
  extern smooth_linked_array_t* __restrict__ smooth_stack;
  extern smooth_t smooth_sp;
#endif

  extern smooth_closure_t smooth_closures[SMOOTH_CLOSURE_MEM];

  void smooth_call (smooth_t x);

#ifndef SMOOTH_FIXED_STACK
  void smooth_la_push (smooth_t x);
  smooth_t smooth_la_pop (void);
  smooth_linked_array_t *smooth__linked_array_allocate (smooth_t length);
#endif

#ifdef __cplusplus
}
#endif

#endif /* _SMOOTH__H */
