/**
 * \file player_kerl.h
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

/**
 * \def GETROBOT
 * Return the robot, otherwise return NOSUCHCLIENT
 */
#define GETROBOT client = _getClient(robotID);\
if(client == NULL) return NOSUCHCLIENT;	

/**
 * \def UPDATE(robotId)
 * Update function
 */
#define UPDATE(robotId) int re = update(robotId);\
if(re != SUCCESS)return re;

/**
 * \def TESTSOCKET(cli)
 * Client socket validity test
 */
#define TESTSOCKET(cli) if(cli->client->sock < 0) return NOSOCKET;

/**
 * \struct PlayerClient
 * This struct contains pointers to the client and devices
 * to control the robot.
 *
 * It is currently missing a few other devices.
 * As we add more devices append them to this struct
 */
typedef struct {
	/*! Internal stage id of the robot */
	int index;
	/*! Player listen address */
	char* address;
	/*! Player listen port */
	int port;
	/*! Player client */
	playerc_client_t* client;
	/*! Player position data */
	playerc_position2d_t* position2d;
	/*! Player laser data */
	playerc_laser_t* laser;
#ifdef KERLFIDUCUAL
	/*! Player fiducial data */
	playerc_fiducial_t* fiducial;
#endif
} PlayerClient;

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
int createRobot(char* address, int port, int index, char* robotID);

/**
 * \fn destroyRobot(char*)
 * \param robotID an identification for the robot
 * \return 0 on success
 * This will destroy the robot so it can no longer be controlled until it is recreated.
 * Function will need to reallocate the memory used by the robot after it has been stopped.
 */
int destroyRobot(char* robotID);


/**
 * \fn move(char*, double, double)
 * \param robotID an identification for the robot
 * \param speed The speed the robot will move in m/s
 * \param rotation in rad/s 
 * \return 0 on success
 * Tells the robot to move forward at a certain speed.
 * To stop the robot moving forward pass 0 as the speed.
 */
int move(char* robotID, double speed, double rotation);

/**
 * \fn moveCoordinate(char*, double, double)
 * \param robotID an identification for the robot
 * \param x The x coordinate to move to
 * \param y the y coordinate to move to
 * Move to this position on the map.
 * This is relative to robot current position
 */
int moveCoordinate(char* robotID, double x, double y);


/**
 * \fn rotateDegrees(char*, double)
 * \param robotID an identification for the robot
 * \param degrees the amount of degrees to rotate by
 * Rotates the robot by this amount of degrees.
 * This is relative to robot current position
 * This function is none blocking so you will have to detect when the robot has stopped rotating.
 */
int rotateDegrees(char* robotID, double degrees);

/**
 * \fn update(char*)
 * \param robotID an identification for the robot
 * \return 0 on success
 * Notifies Player to update devices.
 * This function is none blocking so you may not get updated results as soon as you read a device.
 */
int update(char* robotID);

/**
 * \fn getPosition(char*, double*, double*, double*)
 * \param robotID an identification for the robot
 * \param px the x position read
 * \param py the y position read
 * \param pa the angle read
 * \return 0 if successfully set values
 * Sets the values pass to the positions read from the odometer
 */
int getPosition(char* robotID, double *px, double *py, double *pa);

/**
 * \fn laserResultsSize(char*, int*)
 * \param robotID an identification for the robot
 * \param numberOfResults sets this value to the number of lasers
 * Simply sets the amount of lasers there are to read.
 * This will need to be called before calling readLaserResults so you can allocate memory for lasers
 */
int laserResultsSize(char* robotID, int *numberOfResults);

/**
 * \fn readLaserResults(char*, double*, double*, int*)
 * \param robotID an identification for the robot
 * \param laserResults the array to put laser results into
 * \param laserBearings the bearings of each laser
 * \param size the number of laser results to read, use laserResultsSize to find this value
 * Sets both the laser result and the bearing
 * Remember to allocate sizeof(double)*size in laserResults and laserBearings before passing them through
 */
int readLaserResults(char* robotID, double *laserResults, double *laserBearings, int *size);

/**
 * \fn moveCoordinate(char*, double, double, double)
 * \param robotID an identification for the robot
 * \param px the wished x position
 * \param py the wished y position
 * \param pa the wished angle in radiants
 * 
 * Moves robot to wished coordinates.
 */
int moveCoordinate(char* robotID, double px, double py, double pa);

/**
 * \fn travelDistance(char*, double)
 * \param robotID an identification for the robot
 * \param distance distance to go
 * 
 * Lets robot move in current direction for given distance.
 */
int travelDistance(char* robotID, double distance);

/**
 * \fn queryRobot(char*, ClientQuery*)
 * \param robotID Robot identifier
 * \param query Query to run
 * Runs a query on a robot and returns a tuple containing useful information about it.
 */
int queryRobot(char* robotID, ClientQuery* query);

/**
 * \fn _getClient(char*)
 * \param robotID an identification for the robot
 * returns a client with this name
 */
PlayerClient* _getClient(char* robotID);

