
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

/*
 * The macros don't do much except add casts on the arguments,
 * but they're use is recommended anyway for providing a slightly more abstract interface
 * (or if you do use the declared names, it is favourable not to assume they are function pointers)
 */

#define SMOOTH_CLOSURE_CREATE(c, l, p) \
                                 smooth_closure_create((smooth_t) c, (smooth_t) l, (smooth_closure_t*) p)
#define SMOOTH_CLOSURE_LOCAL(x)  smooth_closure_local((smooth_closure_t*) x)
#define SMOOTH_CLOSURE_PARENT(x) smooth_closure_parent((smooth_closure_t*) x)

/* Smaller names because SMOOTH_CLOSURE_* is quite verbose for day-to-day coding */
#define SMOOTH_C_CREATE(c, l, p) SMOOTH_CLOSURE_CREATE(c, l, p)
#define SMOOTH_C_LOCAL(x)        SMOOTH_CLOSURE_LOCAL(x)
#define SMOOTH_C_PARENT(x)       SMOOTH_CLOSURE_PARENT(x)


#define SMOOTH_PUSH(x)  smooth_push((smooth_t) x)
#define SMOOTH_POP()    smooth_pop()
#define SMOOTH_CALL(x)  smooth_call((smooth_t) x)


#ifndef _SMOOTH_T_TYPE
#define _SMOOTH_T_TYPE
typedef unsigned long int smooth_t;
#endif

struct smooth_closure;
typedef struct smooth_closure smooth_closure_t;


#ifdef __cplusplus
extern "C" {
#endif


  smooth_t          smooth_closure_create (smooth_t lambda, smooth_t local, smooth_closure_t* parent);
  smooth_t          smooth_closure_local  (smooth_closure_t* c);
  smooth_closure_t* smooth_closure_parent (smooth_closure_t* c);


  smooth_t smooth_pop  (void);
  void     smooth_push (smooth_t x);
  void     smooth_call (smooth_t x);


#ifdef __cplusplus
}
#endif

#endif /* _SMOOTH_H */

