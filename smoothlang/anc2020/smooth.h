
/*
 * Copyright (c) 2009, Ali Clark <emailaliclark@gmail.com>
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#ifndef _SMOOTH_H
#define _SMOOTH_H

#ifndef SMOOTH_FIXED_STACK
#include "smoothlang/anc2020/linkedarray/ali_linked_array.h"
#endif

#ifndef _SMOOTH_T_TYPE
#define _SMOOTH_T_TYPE
typedef unsigned long int smooth_t;
#endif

#ifndef __attribute__
#  define __attribute__(x) /*0*/
#endif /* __attribute__ */


/*
When you use SMOOTH_*_P, exactly one of the predicates will be true, no matter what you pass.
In other words, don't use SMOOTH_*_P unless you know you have some kind of function/procedure.
*/


#define SMOOTH_LAMBDA_P(x) (((smooth_t) (x)) < 128)


#define SMOOTH_CLOSURE_MEM  128

#define SMOOTH_CLOSURE_CREATE(c, l, p) smooth_closure_create((smooth_t) c, (smooth_t) l, (smooth_closure_t*) p)

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

#define SMOOTH_CLOSURE_CODE(x)   ((smooth_closure_t*) x)->lambda
#define SMOOTH_CLOSURE_LOCAL(x)  ((smooth_closure_t*) x)->local
#define SMOOTH_CLOSURE_PARENT(x) ((smooth_closure_t*) x)->parent


#define SMOOTH_PRIMITIVE_P(x) (!(SMOOTH_CLOSURE_P(x) || SMOOTH_LAMBDA_P(x)))


/*
 * Stack operations
 * The default is correctness - we have a growable stack and check if growth is needed.
 * If the user knows what they are doing they can choose a fixed stack, with no limits checking.
 * The obvious advantage there is in speed, but the disadvantage is segfault on too much recursion.
 */
#ifdef SMOOTH_FIXED_STACK

#define SMOOTH_PUSH(x)  *smooth_sp++ = (smooth_t) x
#define SMOOTH_POP()    *--smooth_sp
#define SMOOTH_SPOFF(x) smooth_sp[x]
#define SMOOTH_TOS()    SMOOTH_SPOFF(-1)

#else

#define SMOOTH_PUSH(x)  smooth_push((smooth_t) x)
#define SMOOTH_POP()    *linked_array_get(smooth_stack, --smooth_sp)
#define SMOOTH_SPOFF(x) *linked_array_get(smooth_stack, smooth_sp + (x))
#define SMOOTH_TOS()    SMOOTH_SPOFF(-1)

#endif /* SMOOTH_FIXED_STACK */

#define SMOOTH_SPINC() ++smooth_sp
#define SMOOTH_SPDEC() --smooth_sp


#define SMOOTH_CALL(x) smooth_call(x)


#ifdef __cplusplus
extern "C" {
#endif


  typedef struct smooth_closure {
    smooth_t lambda;              /* where to run code from. Could be native or on host. */
    smooth_t local;               /* The local variable for this closure. */
    struct smooth_closure* parent; /* Allows access to more closed variables. */

  } smooth_closure_t;

  /* The space for closures */
  smooth_closure_t  smooth_closures[SMOOTH_CLOSURE_MEM];

  smooth_t smooth_closure_create (smooth_t lambda, smooth_t local, smooth_closure_t* parent);


#ifdef SMOOTH_FIXED_STACK
  smooth_t* smooth_sp;

#else
  struct linked_array* smooth_stack;
  smooth_t smooth_sp;
  void smooth_push (smooth_t x);

#endif


  void smooth_call (smooth_t x);


#ifdef __cplusplus
}
#endif

#endif /* _SMOOTH_H */

