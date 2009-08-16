/**
 * \file kerl.h
 * \brief Functions for robot control from Erlang
 * \author Thomas Lorentsen, Sten Gruener
 *
 * In order to control our robot from Erlang we need some functions that will interpret
 * erlang function calls and pass them to player functions.
 * They will then pass back to erlang a response.
 *
 * These function require no return value as all data and error codes are passed onto erlang.
 */

/**
 * \def CALL(command, length, function)
 * Defines a call from erlang atom where command is the atom value, size of atom as a string
 * and the function to call. Then it will return out of the function it is in.
 */
#define CALL(command, length, function) if(!strncmp(state->fn, command, length) ){ function(state); return; }



/**
 * \struct KerlData
 * This struct contains contains data for the Erlang Driver
 * Pass this around to allow communication to and from erlang
 */
typedef struct {
	/*! Erlang driver port handle */
	ErlDrvPort port;
	char *pid;
	char *buff;
	int bufflen;
	int termIndex;
	char *fn;	
	int tupleArity;
	int version;
	/*! Robot identifier */
	char *robotID;
} KerlData;

/**
 * \fn kerlCreateRobot(KerlData*)
 * \param state ei struct
 * Creates a Player connection
 * Pass in a unique name to identify the robot,
 * hostname, ip and a index number to connect to player
 * {create, &lt;&lt;rid>>, &lt;&lt;hostname>>, ip, index}
 */
void kerlCreateRobot(KerlData *state);

/**
 * \fn kerlDestroyRobot(KerlData*)
 * \param state ei struct
 * Destroys the robot who has the robot id name
 * {destroy, &lt;&lt;rid>>}
 */
void kerlDestroyRobot(KerlData *state);

/**
 * \fn kerlMove(KerlData*)
 * \param state ei struct
 * Choose to move at a speed or move a certain distance
 * for example {move, &lt;&lt;rid>>, speed, 1.0}, {move, &lt;&lt;rid>>, distance, 15.0}
 */
void kerlMove(KerlData *state);

/**
 * \fn kerlRotate(KerlData*)
 * \param state ei struct
 * Choose to rotate either by amount of degrees or set the speed
 * for example: {rotate, &lt;&lt;rid>>, degrees, 180}, {rotate, &lt;&lt;rid>>, speed, 20}
 * This function will need to decide which function to call in player_kerl
 * depending on the tuple passed.
 */
void kerlRotate(KerlData *state);

/**
 * \fn kerlGetPosition(KerlData*)
 * \param state ei struct
 * Return the robot current position back to erlang
 * {getpos, &lt;&lt;rid>>}
 */
void kerlGetPosition(KerlData *state);

/**
 * \fn kerlUpdate(KerlData*)
 * \param state ei struct
 * Force player to update its data
 * This will allow us to grab results for devices
 * {update, &lt;&lt;rid>>}
 */
void kerlUpdate(KerlData *state);

/**
 * \fn kerlResults(KerlData*)
 * \param state ei struct
 * Grab results from a device, you need to select the device from the tuple you pass from erlang
 * This will pass the a list back to erlang
 * For example you pass {results, &lt;&lt;rid>>, lasers}
 * This function should return something like {lasers, [1, 2, 3], [1, 2, 3]}
 * where lasers is the name of the device, a list of laser results and a list of angles
 */
void kerlResults(KerlData *state);

/**
 * \fn kerlSetOption(KerlData*)
 * \param state ei struct
 * Allows to set driver`s options from erlang
 * List of supported options: update_timeout (default: 0.00)
 */
void kerlSetOption(KerlData *state);


/**
 * \fn query(KerlData*)
 * \param state ei struct
 * Returns a tuple containing useful information about the robot
 */
void query(KerlData *state);
