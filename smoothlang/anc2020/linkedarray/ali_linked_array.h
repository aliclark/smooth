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
 *  ali_linked_array.h
 *
 *  Copyright (C) 2009  Ali Clark
 */

/*
 * Very important note. Due to the automatic enlarging in the code on invalid accesses,
 * You must not use more than one pointer to a linked array.
 * Or if you do, just be very careful not to do invalid index lookups.
 * To get around this, you may want to use struct linked_array** instead.
 */

#ifndef _ALI_LINKED_ARRAY_H
#define _ALI_LINKED_ARRAY_H

#ifndef _SMOOTH_T_TYPE
#define _SMOOTH_T_TYPE
typedef unsigned long int smooth_t;
#endif

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Please do not allocate one of these yourself;
 * use linked_array_allocate if you want one.
 */
struct linked_array {
	smooth_t* array; /* The block of allocated memory for array contents. */
	smooth_t max;      /* The max capacity of the array memory for this head. */
	smooth_t length;   /* Holds the value (max*2), saving on calculation time. */
	struct linked_array* rest; /* The other half of the linked_array. */
};

/*
 * Will return a pointer to the index of the linked_list in O(log n) time.
 * This pointer can then be used with a cast to read or write its contents.
 */
smooth_t* linked_array_get (struct linked_array* list, smooth_t index);

/*
 * Allocates another head for the linked_array with twice the capacity of
 * the head of the supplied linked_array. The new head has elements
 * each of size "size". Returns the newly allocated linked_array.
 */
struct linked_array* linked_array_grow (struct linked_array* list);

/*
 * If you have dynamically grown the linked_array using linked_array_grow,
 * you can use this procedure to free the top half of the linked_array again,
 * effectively reversing the last growth.
 * This will halve the size of your linked_array.
 * The return value is the rest of the linked_array.
 */
struct linked_array* linked_array_shrink (struct linked_array* list);

/*
 * Create a new linked_array with "start" being the
 * number of elements in the array, and "size" being the number of bytes
 * used to store each element.
 * You must use this procedure if you want a new linked_array, you cannot
 * simply allocate one from the struct.
 */
struct linked_array* linked_array_allocate (smooth_t start);

/*
 * If you were using the linked_array to hold pointers to objects
 * you allocated yourself, you must free them yourself;
 * linked_array_free will only free the memory it has allocated itself.
 */
void linked_array_free (struct linked_array* list);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* _ALI_LINKED_ARRAY_H */

