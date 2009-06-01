/*
 *  KERL - Kent Erlang Robotics Library
 *  Copyright (C) 2009
 *     Thomas Lorentsen, Sten Gruener
 *
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */
/**
 * \file ei_extended.cpp
 * \brief An Extention for Erlang Interface
 * \author Thomas Lorentsen, Sten Gruener
 *
 * For things that need to be done multiple times in kerl are defined here.
 * This is mostly used for returning messages back to erlang, clean up or list building.
 */

#include "erl_driver.h"
#include "ei.h"

#include "common_kerl.h"
#include "kerl.h"


/**
 * \fn freeMemory(KerlData*)
 * \param state ei struct
 *
 * Local function to free allocated memory.
 * Any memory allocated must be freed before returning anything back to erlang.
 */		
void freeMemory(KerlData* state){

	driver_free(state->fn);
	driver_free(state->robotID);
	driver_free(state->pid);
	driver_free(state->buff);

}


/**
 * \fn returnOK(KerlData*)
 * \param state ei struct
 * Returns ok back to erlang as an atom
 */
void returnOK(KerlData* state){
	ErlDrvTermData spec[] = {
		ERL_DRV_PORT, driver_mk_port(state->port),
		ERL_DRV_ATOM, driver_mk_atom(state->pid),
		ERL_DRV_ATOM, driver_mk_atom("ok"),
		ERL_DRV_TUPLE, 3,
	};
	driver_output_term (state->port, spec, sizeof (spec) / sizeof (spec[0]));
	freeMemory(state);
}


/**
 * \fn returnErrorMessage(KerlData*, char*)
 * \param state ei struct
 * \param message The message tp pass back to erlang.  This is converted into an atom.
 * Returns a tuple to notify erlang of an error.
 * Messages must not contain underscores, commas, brackets, etc as this may confuse erlang a bit.
 * Use the function provided in common to convert error values into string representation.
 * This would be in the form of {error, message}
 */
void returnErrorMessage(KerlData* state, char* message){
	ErlDrvTermData spec[] = {
		ERL_DRV_PORT, driver_mk_port(state->port),
		ERL_DRV_ATOM, driver_mk_atom(state->pid),
		ERL_DRV_ATOM, driver_mk_atom("error"),
		ERL_DRV_ATOM, driver_mk_atom(message),
		ERL_DRV_TUPLE, 2,
		ERL_DRV_TUPLE, 3,
	};
	driver_output_term (state->port, spec, sizeof (spec) / sizeof (spec[0]));
	freeMemory(state);
}

/**
 * \fn pushOntoStack(ErlDrvTermData*, int*, ErlDrvTermData)
 * \param spec return struct
 * \param ptr integer pointer to the to pof the struct
 * \param data data to put on the stack
 * add data to a stack for list building
 */
void pushOntoStack(ErlDrvTermData* spec, int* ptr, ErlDrvTermData data){
	spec[*ptr] = data;
	*ptr=*ptr+1;
}
/**
 * \fn pushOntoStack(ErlDrvTermData*, int*, ErlDrvTermData, ErlDrvTermData)
 * \param spec return struct
 * \param ptr integer pointer to the to pof the struct
 * \param dataA data to put on the stack
 * \param dataB data to put on the stack
 * adds 2 peices of data onto stack.
 * to build lists where pairs of data is required.
 */
void pushOntoStack(ErlDrvTermData* spec, int* ptr, ErlDrvTermData dataA, ErlDrvTermData dataB){
	pushOntoStack(spec, ptr, dataA);
	pushOntoStack(spec, ptr, dataB);
}


/**
 * \fn pushDoubleListOntoStack(ErlDrvTermData*, int*, double*, int)
 * \param spec return stuct
 * \param ptr pointer to the position in the spec
 * \param array the data to stick into the spec
 * \param size the size of the array
 * Pushes a list of doubles into the spec.
 * This also upscales the doubles so they can be returned as a whole number
 * they will need to be down scaled to get the original values back
 */
void pushDoubleListOntoStack(ErlDrvTermData* spec, int* ptr, double* array, int size) {
	// Copy paste of Sten`s code so it should work
	int i;

	// This is what this tuple will contain (used by driver.erl to do the downscaling)
	pushOntoStack(spec, ptr, ERL_DRV_ATOM, driver_mk_atom("list_float"));

	// Here we stick the values we want in the list
	for (i = 0; i < size; ++i){
		pushOntoStack(spec, ptr, ERL_DRV_INT, (int)(array[i]*1000000));
	}
	// End the list
	pushOntoStack(spec, ptr, ERL_DRV_NIL);
	pushOntoStack(spec, ptr, ERL_DRV_LIST);
	pushOntoStack(spec, ptr, size+1);
}
