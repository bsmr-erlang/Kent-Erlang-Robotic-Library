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
 * \file player_kerl.cpp
 * \brief Functions to control a robot in Player
 * \author Thomas Lorentsen, Sten Gruener
 *
 * This holds the player side of the code.
 * Provides functions to control a player robot
 * This should not contain any erlang related code
 *
 * Each robot will be contained in a map where there key is its name or robot identification.
 * Using a name provides a more user friendly way of 
 * identifiying robots as a user could choose a name they wish to use.
 */

#include <stdlib.h>
#include <unistd.h>
#include <math.h>
#include <string.h>

#include <libplayerc/playerc.h>
#include "common_kerl.h"
#include "player_kerl.h"

#include <string>
#include <sstream>
#include <iostream>
#include <map>

using namespace std;

/// A map containing all our bots identifing them by their name
map<string, PlayerClient*> clients;

/**
 * \fn createRobot(char*, int, int, char*)
 * \param address the address player is listening on
 * \param port the port player is listening on
 * \param index is the internal playerstage id
 * \param robotID an identification for the robot
 * \return 0 on success
 * Use this to create a new robot connection.
 * Once the robot has been created it can then be controlled with any other function.
 * A robotID identifies the robot created so it can be controlled.
 * It must be a unique id otherwise this function will return a none zero.
 * Function will need to allocate memory needed and add the robot to a map of robots.
 *
 * If needed this function will also initate devices so that they can be used
 */
int createRobot(char* address, int port, int index, char* robotID) {
	PlayerClient *client;
	pair<map<string,PlayerClient*>::iterator,bool> ret;
	int i, valid;
	string idmapped(robotID); // This string is needed for the map

	DBUG("Checking if clients is working\r\n");
	if (clients.empty()) DBUG("- This is the first robot\r\n");
	
	DBUG("Checking if robotid exists: %s %p\r\n", robotID, robotID);
	if(clients.find(idmapped)!=clients.end()) return NAMEINUSE;
	
	DBUG("allocating memory\n");
	client = new PlayerClient;
	//save index and other useful info inside of the struct
	client->index = index;
	
	client->address = (char *) malloc((strlen(address)+1)*sizeof(char));
	memcpy(client->address, address, (strlen(address)+1)*sizeof(char));

	client->port = port;

	// connect to client
	DBUG("Connecting\n");
	
	client->client = playerc_client_create(NULL, address, port);
	if (playerc_client_connect(client->client)) return ERRORCONNECTING;

	DBUG("AUTOINIT: Does robot exist \r\n");
	if (playerc_client_get_devlist(client->client)) return ERRORQUERYINGDEVICES;
	valid=0;
	for (i = 0; i != client->client->devinfo_count; i++) {
		if (client->client->devinfos[i].addr.index == index) {
			valid=1;
			break;
		}
	}
	if (!valid) return INVALIDINDEX;

	/*
	 * This code will automatically initiallise devices found on the robot
	 */
	DBUG(" AUTOINIT Getting list of devices\r\n");

	if (playerc_client_get_devlist(client->client)) return ERRORQUERYINGDEVICES;

	for (i = 0; i != client->client->devinfo_count; i++) {
		
		switch (client->client->devinfos[i].addr.interf) {
		case PLAYER_LASER_CODE:
			if (client->client->devinfos[i].addr.index == index) {
				DBUG(" AUTOINIT Found device: laser at %d \r\n", client->client->devinfos[i].addr.index);
				// Initialise the lasers as it gives a visual output on stage
				client->laser = playerc_laser_create(client->client, client->index);
				if (playerc_laser_subscribe(client->laser, PLAYER_OPEN_MODE)) return DEVICENOTINITIALISED;
			}
			break;
		case PLAYER_POSITION2D_CODE:
			if (client->client->devinfos[i].addr.index == index) {
				DBUG(" AUTOINIT Found device: position2d at %d \r\n", client->client->devinfos[i].addr.index);
				DBUG("creating positioning device\n\r");
				DBUG(" binit pos: %p ", client->position2d);
				client->position2d = playerc_position2d_create(client->client, client->index);
	
				if (playerc_position2d_subscribe(client->position2d, PLAYER_OPEN_MODE)) return DEVICENOTINITIALISED;
				playerc_position2d_enable(client->position2d, 1);
				DBUG(" ainit pos: %p ", client->position2d);
			}
			break;
		default:
			if (client->client->devinfos[i].addr.index == index) {
				DBUG(" AUTOINIT Found device: unknown at %d \r\n", client->client->devinfos[i].addr.index);
			}
		}

		//client->client->devinfo
	}
	
	// insert the client into the map using its robotID as the key
	DBUG("Inserting into clients map\n\r");
	ret=clients.insert(pair<string,PlayerClient*>(idmapped, client));
	// Check if it was inserted
	DBUG("check if client was stored\n\r");
	if (ret.second==false) return NOTSTORED;

	DBUG("returning successfully\n\r");
	return SUCCESS;
}


/**
 * \fn destroyRobot(char*)
 * \param robotID an identification for the robot
 * \return 0 on success
 * This will destroy the robot so it can no longer be controlled until it is recreated.
 * Function will need to reallocate the memory used by the robot after it has been stopped.
 */
int destroyRobot(char* robotID){
	PlayerClient *client;
	if ((client = _getClient(robotID))==NULL) return NOSUCHCLIENT;
	//test if socket is valid	
	TESTSOCKET(client)

	// Do some clean up

	// Disconnect the client
	if (client->client != NULL) {
		playerc_client_disconnect(client->client);
		playerc_client_destroy(client->client);
	}
	// erase the robot from the array and free memory
	clients.erase(clients.find(robotID));
	free(client);

	return SUCCESS;
}


/**
 * \fn move(char*, double, double)
 * \param robotID an identification for the robot
 * \param speed The speed the robot will move in m/s
 * \param rotation in deg/s 
 * \return 0 on success
 * Tells the robot to move forward at a certain speed.
 * To stop the robot moving forward pass 0 as the speed.
 */
int move(char* robotID, double speed, double rotation){
	PlayerClient *client;
	if ((client = _getClient(robotID))==NULL) return NOSUCHCLIENT;
	//test if socket is valid	
	TESTSOCKET(client)

	if (playerc_position2d_set_cmd_vel(client->position2d, speed, 0, DTOR(rotation), 1)) return DEVICENOTINITIALISED; else return SUCCESS;
}

/**
 * \fn rotateDegrees(char*, double)
 * \param robotID an identification for the robot
 * \param degrees the amount of degrees to rotate by
 * Rotates the robot by this amount of degrees.
 * This is relative to robot current position
 * This function is none blocking so you will have to detect when the robot has stopped rotating.
 */
int rotateDegrees(char* robotID, double degrees) {
	PlayerClient *client;
	if ((client = _getClient(robotID))==NULL) return NOSUCHCLIENT;
	
	// Have to update to get the latest position
	UPDATE(robotID)
	// Rotates from the current direction
	if (playerc_position2d_set_cmd_pose(client->position2d, client->position2d->px, client->position2d->py, 
										client->position2d->pa+DTOR(degrees), 1)) return DEVICENOTINITIALISED;

	return SUCCESS;
}


/**
 * \fn update(char*)
 * \param robotID an identification for the robot
 * \return 0 on success
 * Notifies Player to update devices.
 * This function is none blocking so you may not get updated results as soon as you read a device.
 */
int update(char* robotID) {
	PlayerClient *client;
	if ((client = _getClient(robotID))==NULL) return NOSUCHCLIENT;
	// update was called by accident and is not needed so return success
	if (client->laser == NULL && client->position2d == NULL){
		DBUG("Not updating, no devices initialised\n\r");
		return SUCCESS;
	}
	DBUG("updating\n");

	//test if socket is valid
	TESTSOCKET(client)

	// poke it 3 times to make it work
	playerc_client_read(client->client);
	playerc_client_read(client->client);
	playerc_client_read(client->client);

	return SUCCESS;
}


/**
 * \fn getPosition(char*, double*, double*, double*)
 * \param robotID an identification for the robot
 * \param px the x position read
 * \param py the y position read
 * \param pa the angle read
 * \return 0 if successfully set values
 * Sets the values pass to the positions read from the odometer
 */
int getPosition(char* robotID, double *px, double *py, double *pa) {
	PlayerClient *client;
	if ((client = _getClient(robotID))==NULL) return NOSUCHCLIENT;
	
	// need to update to get current position
	UPDATE(robotID)
	if (client->position2d == NULL) return DEVICENOTINITIALISED;
	*px = client->position2d->px;
	*py = client->position2d->py;
	*pa = client->position2d->pa;

	return SUCCESS;
}


/**
 * \fn laserResultsSize(char*, int*)
 * \param robotID an identification for the robot
 * \param numberOfResults sets this value to the number of lasers
 * Simply sets the amount of lasers there are to read.
 * This will need to be called before calling readLaserResults so you can allocate memory for lasers
 */
int laserResultsSize(char* robotID, int *numberOfResults) {
	PlayerClient *client;
	if ((client = _getClient(robotID))==NULL) return NOSUCHCLIENT;
	// need to update to get current position
	UPDATE(robotID)
	if (client->laser == NULL) return DEVICENOTINITIALISED;
	*numberOfResults=client->laser->scan_count;
	return SUCCESS;
}


/**
 * \fn readLaserResults(char*, double*, double*, int*)
 * \param robotID an identification for the robot
 * \param laserResults the array to put laser results into
 * \param laserBearings the bearings of each laser
 * \param size the number of laser results to read, use laserResultsSize to find this value
 * Sets both the laser result and the bearing
 * Remember to allocate sizeof(double)*size in laserResults and laserBearings before passing them through
 */
int readLaserResults(char* robotID, double *laserResults, double *laserBearings, int *size) {
	PlayerClient *client;
	int i;
	if ((client = _getClient(robotID))==NULL) return NOSUCHCLIENT;
	// read laser results
	for(i = 0; i < *size;i++) {
		laserResults[i] = client->laser->scan[i][0];
		laserBearings[i] = client->laser->scan[i][1];
	}

	return SUCCESS;
}


/**
 * \fn moveCoordinate(char*, double, double, double)
 * \param robotID an identification for the robot
 * \param px the wished x position
 * \param py the wished y position
 * \param pa the wished angle in radiants
 * 
 * Moves robot to wished coordinates.
 */
int moveCoordinate(char* robotID, double px, double py, double pa) {
	PlayerClient *client;
	if ((client = _getClient(robotID))==NULL) return NOSUCHCLIENT;
	//test if socket is valid	
	TESTSOCKET(client)
	// set the position
	if (playerc_position2d_set_cmd_pose(client->position2d, px, py, pa, 1)) return DEVICENOTINITIALISED;

	return SUCCESS;
}


/**
 * \fn travelDistance(char*, double)
 * \param robotID an identification for the robot
 * \param distance distance to go
 * 
 * Lets robot move in current direction for given distance.
 */
int travelDistance(char* robotID, double distance) {
	PlayerClient *client;
	if ((client = _getClient(robotID))==NULL) return NOSUCHCLIENT;
	
	UPDATE(robotID)
	// Move the robot
	if (playerc_position2d_set_cmd_pose(client->position2d, 
		cos(client->position2d->pa)*distance + client->position2d->px, 
		sin(client->position2d->pa)*distance + client->position2d->py, 
		client->position2d->pa, 1)) return DEVICENOTINITIALISED;

	return SUCCESS;
}


/**
 * \fn queryRobot(char*, ClientQuery*)
 * Runs a query on a robot and returns a tuple containing useful information about it.
 */
int queryRobot(char* robotID, ClientQuery* query) {
	PlayerClient *client;
	if ((client = _getClient(robotID))==NULL) return NOSUCHCLIENT;
	
	//Fill up the struct
	query->index=client->index;
	query->address=client->address;
	query->port=client->port;
	
	if (client->position2d)
		query->devices = query->devices | SUPPOSITION;
	if (client->laser)
		query->devices = query->devices | SUPLASERS;
		
	return SUCCESS;
}


/***************************** Private Functions ************************************/
/************************************************************************************/

/**
 * \fn _getClient(char*)
 * \param robotID an identification for the robot
 * returns a client with this name
 * returns null if client does not exist
 */
PlayerClient* _getClient(char* robotID) {
	// find the client using the name as a id.
	string idmapped(robotID);
	if(clients.find(idmapped)==clients.end()) return NULL;
	return clients.find(idmapped)->second;
}

