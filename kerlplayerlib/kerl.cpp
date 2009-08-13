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
 * \file kerl.cpp
 * \brief Functions for robot control from Erlang
 * \author Thomas Lorentsen
 *
 * In order to control our robot from Erlang we need some functions that will interpret
 * erlang function calls and pass them to player functions.
 * They will then pass back to erlang a response.
 *
 * These function require no return value as all data and error codes are passed onto erlang.
 */
#include "erl_driver.h"

#include <string.h> 
#include <libplayerc/playerc.h>
#include "common_kerl.h"
#include "player_kerl.h"

#include "kerl.h"
#include "ei_extended.h"

#include "erl_driver.h"
#include "ei.h"

/**
 * \fn kerlCreateRobot(KerlData*)
 * \param state ei struct
 * Creates a Player connection
 * Pass in a unique name to identify the robot,
 * hostname, ip and a index number to connect to player
 * {create, &lt;&lt;hostname>>, ip, index, &lt;&lt;robotID>>}
 */
void kerlCreateRobot(KerlData *state){
	int res;
	
	//decode function-specific arguments
	char* address = (char *) driver_alloc(DOMAINLENGTH);
	ei_decode_string (state->buff, &state->termIndex, address);
	
	double port = 6665;
	ei_decode_double (state->buff, &state->termIndex, &port);
	
	double index = 0;
	ei_decode_double (state->buff, &state->termIndex, &index);

	//not really needed, state has got it already
	char* tempRobotID = (char *) driver_alloc(50);
	ei_decode_string (state->buff, &state->termIndex, tempRobotID);
	//flush temp
	driver_free(tempRobotID);
	
	//call function
	res = createRobot(address, (int)port, (int)index, state->robotID);
	//cleanup
	driver_free(address);
	
	//return
	if(res==SUCCESS){
		returnOK(state);
	}else{
		returnErrorMessage(state, errorToString(res));
	}
}

/**
 * \fn kerlDestroyRobot(KerlData*)
 * \param state ei struct
 * Destroys the robot who has the robot id name
 * {destroy}
 */
void kerlDestroyRobot(KerlData *state){
	int res;
	//return
	if((res = destroyRobot(state->robotID))==SUCCESS){
		returnOK(state);
	}else{
		returnErrorMessage(state, errorToString(res));
	}
}

/**
 * \fn kerlMove(KerlData*)
 * \param state ei struct
 * Choose to move at a speed or move a certain distance
 * for example {move, speed, 1.0}, {move, distance, 15.0}, {move, position, 0, 0, 3.14}, {move, full, 1, 1}
 */
void kerlMove(KerlData *state){
	int res;
	
	//decode function-specific arguments
	char* type = (char *) driver_alloc(8);
	ei_decode_atom(state->buff, &state->termIndex, type);

	double value=0;
	ei_decode_double(state->buff, &state->termIndex, &value);
	
	if (!strncmp(type, "distance", 8)) {	
		//call function
		res = travelDistance(state->robotID, value);
	}else if(!strncmp(type, "speed", 4)){
		res = move(state->robotID, value, 0);
	}else if(!strncmp(type, "position", 8)){
		//position
		double x=value;
		double y=0;
		ei_decode_double(state->buff, &state->termIndex, &y);	
		double a=0;
		ei_decode_double(state->buff, &state->termIndex, &a);	

		res = moveCoordinate(state->robotID, x, y, a);
	}else{
		//full movement
		double rotation =0;
		ei_decode_double(state->buff, &state->termIndex, &rotation);
		res = move(state->robotID, value, rotation);	
	}

	//free memory
	driver_free(type);

	//return
	if(res==SUCCESS){
		returnOK(state);
	} else {
		returnErrorMessage(state, errorToString(res));
	}
}

/**
 * \fn kerlRotate(KerlData*)
 * \param state ei struct
 * Choose to rotate either by amount of degrees or set the speed
 * for example: {rotate, degrees, 180}, {rotate, speed, 20}
 * This function will need to decide which function to call in player_kerl
 * depending on the tuple passed.
 */
void kerlRotate(KerlData *state){
	int res;
	
	//decode function-specific arguments
	char* type = (char *) driver_alloc(8);
	ei_decode_atom(state->buff, &state->termIndex, type);

	double value=0;
	ei_decode_double (state->buff, &state->termIndex, &value);

	if (!strncmp(type, "degrees", 7)) {	
		res = rotateDegrees(state->robotID, value);
	}else{
		res = move(state->robotID, 0, value);
	}

	//free memory
	driver_free(type);

	//return
	if(res==SUCCESS){
		returnOK(state);
	}else{
		returnErrorMessage(state, errorToString(res));
	}
}

/**
 * \fn kerlGetPosition(KerlData*)
 * \param state ei struct
 * Return the robot current position back to erlang
 * {Port, tuple_float_3, {x, y, a}}
 */
void kerlGetPosition(KerlData *state){
	int res;
	double x, y, a;
	res = getPosition(state->robotID, &x, &y, &a);
	if(!res){
		int ptr=0;
		ErlDrvTermData spec[16];

		pushOntoStack(spec, &ptr, ERL_DRV_PORT, driver_mk_port(state->port));
		pushOntoStack(spec, &ptr, ERL_DRV_ATOM, driver_mk_atom(state->pid));
		pushOntoStack(spec, &ptr, ERL_DRV_ATOM, driver_mk_atom("tuple_float_3"));
		pushOntoStack(spec, &ptr, ERL_DRV_INT, (int) (x*1000000));
		pushOntoStack(spec, &ptr, ERL_DRV_INT, (int) (y*1000000));
		pushOntoStack(spec, &ptr, ERL_DRV_INT, (int) (a*1000000));
		pushOntoStack(spec, &ptr, ERL_DRV_TUPLE, 3);
		pushOntoStack(spec, &ptr, ERL_DRV_TUPLE, 4);

		driver_output_term (state->port, spec, sizeof (spec) / sizeof (spec[0]));
		freeMemory(state);
	}else{
		returnErrorMessage(state, errorToString(res));
	}
}

/**
 * \fn kerlUpdate(KerlData*)
 * \param state ei struct
 * Force player to update its data
 * This will allow us to grab results for devices
 * {update}
 */
void kerlUpdate(KerlData *state){
	int res = update(state->robotID);

	if(!res){
		returnOK(state);
	}else{
		returnErrorMessage(state, errorToString(PLAYERFAIL));
	}
}

/**
 * \fn kerlResults(KerlData*)
 * \param state ei struct
 * Grab results from a device, you need to select the device from the tuple you pass from erlang
 * This will pass the a list back to erlang
 * For example you pass {results, lasers}	
 * This function should return something like {[1, 2, 3], [1, 2, 3]}
 * This function can also return an empty lists but this is usually rare and only happens on the first
 * few reads.
 * where lasers is the name of the device, a list of list of angles and a list of laser results
 */
void kerlResults(KerlData *state){
	int res;
	int ptr;
	// Grab the next atom and put it in the struct
	ei_decode_atom(state->buff, &state->termIndex, state->fn);

	if (!strncmp(state->fn, "lasers", 6)) {	

		//only lasers implemented yet
		double *angles;
		double *results;
		
		
		int size = 0;

		// get the number of results and return on error
		if ((res = laserResultsSize(state->robotID, &size))!=SUCCESS) {
			DBUG("return error message\n\r");
			returnErrorMessage(state, errorToString(res));
			return;
		}
		
		DBUG("Number of Results %i %d\n\r", size, size);

		// If there is nothing to read dont allocate any memory
		// Just let it continue and it will return an empty list
		if (size>0) {
			// Remember to use driver_alloc when dealing with memory in erlang
			results=(double*) driver_alloc(sizeof(double)*size);
			angles=(double*) driver_alloc(sizeof(double)*size);
		}

		DBUG("Reading results \n\r");
		//fill the arrays
		res=readLaserResults(state->robotID, results, angles, &size);

		//success?
		if(res!=SUCCESS){
			DBUG("Nothing read (memory init: %d %d)\n\r", (results==NULL), (angles==NULL));
			// deallocate arrays if they have been created
			if (!results) driver_free(results);
			if (!angles ) driver_free(angles);
			DBUG("return error message\n\r");
			returnErrorMessage(state, errorToString(res));
			return;
		}
		DBUG("Building erlang data to return \n\r");
		//OUTPUT PART
		// we need 14 fields (it is working information like lists declaration)
		// + 2 fields for each value
		// => size of spec
		int spec_len = (size*2*2) + 16;

		// Allocate space for the spec
		ErlDrvTermData spec[spec_len];

		// Think like a stack, push data on then stick either list or tuple with the size
		// Some header structure
		// Will look like {Port, list_float, list [List...,Nil], list_float, list [List...,Nil]}

		ptr=0;
		pushOntoStack(spec, &ptr, ERL_DRV_PORT, driver_mk_port(state->port));
		pushOntoStack(spec, &ptr, ERL_DRV_ATOM, driver_mk_atom(state->pid));
		//push 2 lists with accoring headers
		pushDoubleListOntoStack(spec, &ptr, angles, size);

		pushDoubleListOntoStack(spec, &ptr, results, size);
		
		// End the tuple
		pushOntoStack(spec, &ptr, ERL_DRV_TUPLE, 6);

		// free memory if needed
		if (!results) driver_free(results);
		if (!angles ) driver_free(angles);

		driver_output_term(state->port, spec, spec_len);
		freeMemory(state);
		DBUG("Allocated: %i Used : %i\n", spec_len, ptr);
#ifdef KERLFIDUCUAL
	} else if (!strncmp(state->fn, "fiducial", 8)) {
		DBUG("Fiducial device reader\n\r");
		//result size
		int size = 0;
		

		// memory to fill with fiducial data
		int *fid_id;

		// get the number of results and return on error
		if ((res = fiducialResultsSize(state->robotID, &size))!=SUCCESS) {
			DBUG("return error message\n\r");
			returnErrorMessage(state, errorToString(res));
			return;
		}
		DBUG("There are %d beacons found\n\r");


		//Allocate memory if there are beacons found
		if (size>0) {
			DBUG("Allocating memory\n\r");
			fid_id=(int*) driver_alloc(sizeof(int)*size);
		}
		DBUG("Reading results \n\r");

		res=readFiducialResults(state->robotID, fid_id, &size);
		//success?
		if(res!=SUCCESS){
			DBUG("Nothing read (memory init: %d )\n\r", (fid_id==NULL));
			// deallocate arrays if they have been created
			if (!fid_id) driver_free(fid_id);
			DBUG("return error message\n\r");
			returnErrorMessage(state, errorToString(res));
			return;
		}

		DBUG("Building erlang data to return \n\r");
		// See comments in laser reading for more help
		// [{id},{id}]

		int spec_len = (size*2) + 9;


		// Allocate space for the spec
		ErlDrvTermData spec[spec_len];

		ptr=0;
		pushOntoStack(spec, &ptr, ERL_DRV_PORT, driver_mk_port(state->port));
		pushOntoStack(spec, &ptr, ERL_DRV_ATOM, driver_mk_atom(state->pid));

		//TODO Clean following list creating up
		int i;
		for (i = 0; i!=size; i++) {
			
			pushOntoStack(spec, &ptr, ERL_DRV_INT, fid_id[i]);
		}

		// End the list
		pushOntoStack(spec, &ptr, ERL_DRV_NIL);
		pushOntoStack(spec, &ptr, ERL_DRV_LIST);
		pushOntoStack(spec, &ptr, size+1);


		// End the tuple
		pushOntoStack(spec, &ptr, ERL_DRV_TUPLE, 3);


		// free memory if needed
		if (!fid_id) driver_free(fid_id);

		driver_output_term(state->port, spec, spec_len);
		freeMemory(state);
		DBUG("Allocated: %i Used : %i\n", spec_len, ptr);
#endif
	}else{
		returnErrorMessage(state, errorToString(NOSUCHDEVICE));
	}
	// Nothing after here
}

/**
 * \fn kerlSetOption(KerlData*)
 * \param state ei struct
 * Allows to set driver`s options from erlang
 * 
 */
void kerlSetOption(KerlData *state){

	// Grab the next atom and put it in the struct
	ei_decode_atom(state->buff, &state->termIndex, state->fn);

	//no such option
	returnErrorMessage(state, errorToString(UNDEFINEDOPTION));
}

/**
 * \fn query(KerlData*)
 * Runs a query on a robot and returns a tuple containing useful information about it.
 */
void query(KerlData *state) {
	int res;
	ClientQuery* query =  new ClientQuery;
	DBUG("querying robot\n\r");
	// get the robot information
	query->robotID = state->robotID;
	res = queryRobot(state->robotID, query);
	
	if(!res){
		DBUG("preparing to send back query\n\r");
		// Erlangfy the returned query
		int ptr=0;
		int tdevices;
		int listcounter = 0;
		
		// counts the number of devices initialised
		DBUG("devices: %d\n\r", query->devices);
		for(tdevices = query->devices; tdevices > 0; tdevices = tdevices >> 1)
			if (tdevices & 1)
				listcounter++;

		// Size of list + number of devices
		ErlDrvTermData spec[19+(listcounter<<1)];
		
		
		pushOntoStack(spec, &ptr, ERL_DRV_PORT, driver_mk_port(state->port));
		pushOntoStack(spec, &ptr, ERL_DRV_ATOM, driver_mk_atom(state->pid));
		pushOntoStack(spec, &ptr, ERL_DRV_ATOM, driver_mk_atom(query->robotID));
		pushOntoStack(spec, &ptr, ERL_DRV_ATOM, driver_mk_atom(query->address));
		pushOntoStack(spec, &ptr, ERL_DRV_INT, query->port);
		pushOntoStack(spec, &ptr, ERL_DRV_INT, query->index);
		// Device listing here
		if (query->devices & SUPPOSITION) 
			pushOntoStack(spec, &ptr, ERL_DRV_ATOM, driver_mk_atom(PLAYER_POSITION2D_STRING));
		if (query->devices & SUPLASERS) 
			pushOntoStack(spec, &ptr, ERL_DRV_ATOM, driver_mk_atom(PLAYER_LASER_STRING));
		if (query->devices & SUPSONAR) 
			pushOntoStack(spec, &ptr, ERL_DRV_ATOM, driver_mk_atom(PLAYER_SONAR_STRING));
		if (query->devices & SUPFIDUCIAL) 
			pushOntoStack(spec, &ptr, ERL_DRV_ATOM, driver_mk_atom(PLAYER_FIDUCIAL_STRING));
		
		
		pushOntoStack(spec, &ptr, ERL_DRV_NIL);
		pushOntoStack(spec, &ptr, ERL_DRV_LIST);
		pushOntoStack(spec, &ptr, listcounter+1);
		//End device listing
		pushOntoStack(spec, &ptr, ERL_DRV_TUPLE, 5);
		pushOntoStack(spec, &ptr, ERL_DRV_TUPLE, 3);
	
		DBUG("returning query\n\r");
		driver_output_term (state->port, spec, sizeof (spec) / sizeof (spec[0]));

		//free memory and return
		DBUG("freeing memory\n\r");
	
		freeMemory(state);
	}else{
		returnErrorMessage(state, errorToString(res));
	}
	delete query;
}
