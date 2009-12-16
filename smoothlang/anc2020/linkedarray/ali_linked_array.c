/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */

/*
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

/*
 *  ali_linked_array.c
 *
 *  Copyright (C) 2009  Ali Clark
 */

#include "smoothlang/anc2020/linkedarray/ali_linked_array.h"
#include "smoothlang/anc2020/linkedarray/ali_memory.h"

#define EXIT_FAILURE 1

void exit (int s);
void perror (const char* s);

/*
 * Initialises the members on the head of the structure.
 * size is the size of each element, and max is the number
 * of items we want to allocate for this head.
 */
static __inline__ void linked_array_initialise (struct linked_array *list,
						smooth_t max);

/*
 * Frees the top half of the link_array array memory
 * and the structure of the linked_array head.
 */
static __inline__ void linked_array_free_head (struct linked_array *list);

/*---------------------------------------------------------------------------*/

static __inline__ void linked_array_initialise (struct linked_array *list,
						smooth_t max)
{
	list->array  = MALLOCATE(sizeof(smooth_t) * max);
	list->max    = max;
	list->length = max * 2;
	list->rest   = NIL;
}

/*---------------------------------------------------------------------------*/

static __inline__ void linked_array_free_head (struct linked_array *list)
{
	FREE(list->array);
	FREE(list);
}

/*---------------------------------------------------------------------------*/

struct linked_array *linked_array_allocate (smooth_t length)
{
	smooth_t max;
	STALLOCATED( struct linked_array, a);
	STALLOCATED( struct linked_array, b);

	length = (smooth_t) ceil_base2( length);
	max    = (smooth_t) ((length == 1) ? 1 : (length / 2));

	linked_array_initialise( a, max);
	linked_array_initialise( b, max);

	b->rest = a;
	return b;
}

/*---------------------------------------------------------------------------*/

void linked_array_free (struct linked_array *list)
{
	if (list->rest) {
		linked_array_free(list->rest);
	}
	linked_array_free_head(list);
}

/*---------------------------------------------------------------------------*/

/* This is the "1" in our O(1). It must be zippidy zip! */
smooth_t* linked_array_get (struct linked_array *list, smooth_t index)
{
	while (index < list->max) {
		if (list->rest) {
			list = list->rest;
		} else {
			return list->array + index;
		}
	}
	return list->array + (index - list->max);
}

/*---------------------------------------------------------------------------*/

struct linked_array *linked_array_grow (struct linked_array *list)
{
	STALLOCATED(struct linked_array, a);
	linked_array_initialise(a, list->length);
	a->rest = list;
	return a;
}

/*---------------------------------------------------------------------------*/

struct linked_array *linked_array_shrink (struct linked_array *list)
{
	struct linked_array *rest = list->rest;

	if (!rest || !rest->rest) {
		perror( "Error: Tried to shrink linked_array "
			"beyond initial size.");
		exit( EXIT_FAILURE);
	}
	linked_array_free_head( list);
	return rest;
}

/*---------------------------------------------------------------------------*/

