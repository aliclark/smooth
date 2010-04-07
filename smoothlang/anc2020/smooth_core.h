
#ifndef _SMOOTH__H
#define _SMOOTH__H

#include "smoothlang/anc2020/smooth.h"


#define EXIT_SUCCESS 0
#define NULL         0

/*
 * Whilst all memory is growable on demand, the start size can be changed to suit the application.
 * Whilst closures and gc_table are one each per process, stack is one per thread,
 * so if you want thousands of threads, consider making stack_size smaller.
 *
 * All sizes _must_ be specified as a power of two and greater than 0.
 * Also, these are no. of items, so its more like 12 bytes per closure and
 * 4 bytes each for stack element and gc table entry
 *
 * Default settings of 1 item per structure are only reasonable for applications
 * which want to be able to use a very tiny amount of bss where possible.
 * Most applications do not care about that and should increase these values to
 * probably at least 1024,1024,1024,16
 */
#ifndef STACK_SIZE
#  define STACK_SIZE 1
#endif
#ifndef CLOSURES_SIZE
#  define CLOSURES_SIZE 1
#endif
#ifndef GC_TABLE_SIZE
#  define GC_TABLE_SIZE 1
#endif
#ifndef CALL_REGISTER_SIZE
#  define CALL_REGISTER_SIZE 1
#endif

#define LOCAL           local
#define SELF            self
#define SET(a, b)       a = b
#define CLOSURE_CAST(a) ((smooth_closure_t*) a)
#define REPEAT(c, n)    for (i = n; i > 0; --i) { c; }

#define CALL(x)  smooth_call((smooth_t) x)

#define PUSH(x)  smooth_push((smooth_t) x)
#define POP()    smooth_pop()
#define TOS()    smooth_down()
#define SPINC()  smooth_up()
#define SPDEC()  smooth_down()
#if 0
#define SPOFF(x) smooth_sp[x]
#endif

#define CORE_INIT() smooth_core_init()
#define LAMBDA(x)   ((smooth_t) (x + ((smooth_t) smooth_lambdas_start)))
#define UNLAMBDA(x) ((smooth_t) (x - ((smooth_t) smooth_lambdas_start)))

#define SMOOTH_CLOSURE_MEM  256

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
 *
 * NEW: We will need to do a "log n" traversal of closures mem (since it is now growable)
 * on each smooth_call to find out if the given address is in that space.
 * This won't be much slower if we start with a high base on our log n.
 *
 */
#define SMOOTH_CLOSURE_P(x) smooth_closure_p(x)

#define SMOOTH_PRIMITIVE_P(x) (!(SMOOTH_CLOSURE_P(x) || SMOOTH_LAMBDA_P(x)))


#define CLOSURE_CREATE(x,y,z) SMOOTH_CLOSURE_CREATE(x,y,z)
#define CLOSURE_CODE(x)   ((smooth_closure_t*) x)->lambda
#define CLOSURE_LOCAL(x)  ((smooth_closure_t*) x)->local
#define CLOSURE_PARENT(x) ((smooth_closure_t*) x)->parent


/**********************************/


typedef char byte;
typedef char bool;

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

  extern smooth_t* __restrict__ smooth_sp;

  void smooth_call (smooth_t x);
  void smooth_push (smooth_t x);

  smooth_t smooth_pop (void);

  void smooth_core_init (void);

  bool smooth_closure_p (smooth_t x);

#ifdef __cplusplus
}
#endif

#endif /* _SMOOTH__H */
