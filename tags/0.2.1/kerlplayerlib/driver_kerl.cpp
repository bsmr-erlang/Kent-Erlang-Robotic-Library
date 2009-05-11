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
 * \file driver_kerl.cpp
 * \brief An Erlang Port Driver
 * \author Thomas Lorentsen, Sten Gruener
 *
 * This contains code required to load a driver into Erlang allowing
 * calls to be made to Player.
 *
 * Erlang will initialize the driver allowing it to pass calls using Erlang Interface.
 * This driver will wrap up the varibless passed from Erlang and pass it onto
 * kerl.cpp which will allow robot control in player.
 *
 * This driver runs concurrently.
 *
 * Driver is explained: http://erlang.org/doc/man/driver_entry.html
 */
#include <stdio.h>
#include <string.h> 
#include <unistd.h>


#include "erl_driver.h"
#include "ei.h"

#include "common_kerl.h"
#include "kerl.h"
#include "ei_extended.h"



/**
 * \fn kerlDriverStart(ErlDrvPort, char*)
 * \param port is the port
 * \param buff any commands passed
 * \return the port on success
 *
 * This is called when the driver is instantiated, when open_port/2 is called.
 */
static ErlDrvData kerlDriverStart(ErlDrvPort port, char *buff){
	KerlData* d = (KerlData*)driver_alloc(sizeof(KerlData));
	d->port = port;
	DBUG("Debugging On\n\r");
	return (ErlDrvData)d;
}

/**
 * \fn kerlDriverStop(ErlDrvData)
 * \param handle data
 * This is called when the port is closed
 */
static void kerlDriverStop(ErlDrvData handle){
	DBUG("stopping driver\n\r");
	driver_free((char*)handle);
}

/**
 * \fn doFree(void*)
 * \param state is the data passed into the driver
 * Not currently used but called at the end of a function call
 */
static void doFree(void *state) { return;}

/**
 * \fn kerlDriverMain(void*)
 * \param asyncData is the kerldata passed into the driver 
 * Contains the kerl main functions passed from the async driver.
 * Decoding of the atoms passed from erlang is done here and decides what kerl function is needed to call.
 * If no function can be called then an error is returned.
 */
static void kerlDriverMain(void *asyncData) {
	KerlData *state = reinterpret_cast<KerlData*>(asyncData);
	int tempTupleArtiy;
	
	DBUG("entered driver\n\r");
	DBUG("Configuring state\n\r");
	state->termIndex = 0;
	state->version = 0;
	//warning: the length here is the maximum length of the function name
	state->fn=(char *) driver_alloc(FNLENGTH);
	//warning: the length here is the maximum length of the robots name
	state->robotID=(char *) driver_alloc(IDLENGTH);
	//the pid is needed to be passed up to driver.erl in order to know the calling process
	state->pid=(char *) driver_alloc(20);
	state->tupleArity=0;
	

	//decode version
	ei_decode_version(state->buff, &state->termIndex, &state->version);
	//open the first tuple, arity should be always 2 ;)
	ei_decode_tuple_header(state->buff, &state->termIndex, &tempTupleArtiy);
	//extract the robots id attached to the driver
	ei_decode_string(state->buff, &state->termIndex, state->robotID);
	//decode the pid
	ei_decode_string(state->buff, &state->termIndex, state->pid);
	DBUG("Caller PID: %s -> %p\n\r", state->pid, state->pid);
	//decode header, should be the size of the inner payload-packet
	ei_decode_tuple_header(state->buff, &state->termIndex, &state->tupleArity);
	//decode first atom that contains the command
	ei_decode_atom(state->buff, &state->termIndex, state->fn);
	//if statements match atom in fn to call a function in kerl
	// This needs to be stuck into a nice simple ifdef. dont forget the return at the end.
	// No need for else ifs as there is nothing else to do after the function is called
	// Call with  driver:callPort(S, create). 

	DBUG("Caller PID: %s\n\r", state->pid);

	// Find which function to call from the state->fn atom
	// If a function is called it will return from this function
	CALL("create", 6, kerlCreateRobot)
	CALL("rotate", 6, kerlRotate)
	CALL("get_position", 12, kerlGetPosition)
	CALL("update", 6, kerlUpdate)
	CALL("move", 4, kerlMove)
	CALL("destroy", 7, kerlDestroyRobot)
	CALL("results", 7, kerlResults)
	CALL("set_option", 11, kerlSetOption)
	CALL("info", 4, query)
	
	//if nothing is matched return a error
	returnErrorMessage(state, errorToString(UNDEFINEDFUNCTION));
}



/**
 * \fn kerlDriverOutput(ErlDrvData, char*, int)
 * \param handle pointer to the passed port information (todo: not sure ;))
 * \param buff erlang port input string
 * \param bufflen length of the input string
 * Function calls to the driver go here before they run asynchronously.
 * The memory has to be copied to avoid curruption and then a thread is spawned at the end.
 */
static void kerlDriverOutput(ErlDrvData handle, char *buff, int bufflen){
	KerlData* dataTmp = new KerlData();
	KerlData* state = (KerlData*) handle;

	// Alloc memory
	char* tbuff = (char *) driver_alloc(bufflen*sizeof(char));

	DBUG("Entered aysnc driver\n\r");
		
	//Copy the state passed from erlang into a new state as the old one gets currupted
	memcpy(dataTmp, state, sizeof(KerlData));
	// Copy the buffer into the tempory buffer space
	// This is needed as buffer gets currupted once we are in the async driver
	DBUG("1 copying buffer at %p size %d\n\r", buff, bufflen);
	memcpy(tbuff, buff, bufflen*sizeof(char));
	dataTmp->buff = tbuff;
	
	DBUG("1 dataTmp %p\n\r", dataTmp);
	DBUG("1 buff %p %s\n\r", tbuff, tbuff);
	// This will now run asynchronously
	driver_async(dataTmp->port, NULL, kerlDriverMain, dataTmp, doFree);

}

	

/**
 * \fn kerlDriverReadyAsync(ErlDrvData, ErlDrvThreadData)
 * \param drvData the driver data passed
 * \param asyncData the async data passed
 * This function is called when a async thread has completed
 * This function is called after an asynchronous call has completed.
 * The asynchronous call is started with driver_async.
 */
static void kerlDriverReadyAsync(ErlDrvData drvData, ErlDrvThreadData asyncData) {
	DBUG("KerlDriverReadyAsync\n\r");
}

/**
 * \var kerlDriverEntry
 * This is the driver entry definition and helps erlang to know how to talk
 * to the driver.
 * The variables change in here depending if it is async of not
 */
ErlDrvEntry kerlDriverEntry = {
	NULL,			    /* F_PTR init, N/A */
	kerlDriverStart,		/* L_PTR start, called when port is opened */
	kerlDriverStop,		/* F_PTR stop, called when port is closed */
	kerlDriverOutput,	/* F_PTR output, called when erlang has sent
							data to the port */
	NULL,			    /* F_PTR ready_input, 
							called when input descriptor ready to read*/
	NULL,			    /* F_PTR ready_output, 
							called when output descriptor ready to write */
	"kerl",			    /* char *driver_name, the argument to open_port */
	NULL,			    /* F_PTR finish, called when unloaded */
	NULL,			    /* F_PTR control, port_command callback */
	NULL,			    /* F_PTR timeout, reserved */
	NULL,			    /* F_PTR outputv, reserved */
	NULL,
	kerlDriverReadyAsync, /* io ready function */
	NULL,
	NULL,
	NULL
};

extern "C" {
	/**
	 * \fn DRIVER_INIT(kerl)
	 * Initialise erlang driver.
	 */
	DRIVER_INIT(kerl) /* must match name in driver_entry */
	{
		return &kerlDriverEntry;
	}
}

