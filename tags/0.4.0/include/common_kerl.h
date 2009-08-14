/**
 * \file common_kerl.h
 * \brief Common definitions
 * \author Thomas Lorentsen, Sten Gruener
 *
 * This file contains common definitions and functions needed for kerl.
 *
 * Error numbers start from severl ranges depending on where 
 * they were caused, this makes debugging easier as we can
 * tell if it is a player problem, ei problem or a kerl problem.
 *
 * Errors caused in kerl code start from -64
 * Errors returned from the player server start from -128
 * Errors returned from ei start from -192
 */

/**
 * \def DBUG(x, args...)
 * Issue a debug message if debugging is enabled.
 */
#ifdef DEBUG
#define DBUG(x, args...) fprintf(stderr, x, ##args)
#else
#define DBUG(x, args...)
#endif

/**
 * \def BENCH(x, args...)
 * Issue a benchmark message is benchmarking is enabled.
 */
#ifdef BENCHMARKING
#define BENCH(x, args...) fprintf(stderr, x, ##args)
#else
#define BENCH(x, args...)
#endif

/**
 * \struct ClientQuery
 * Player client query
 */
typedef struct {
	/*! Robot identifier */
	char *robotID;
	/*! Internal stage identifier */
	int index;
	/*! Connection address */
	char* address;
	/*! Connection port */
	int port;
	/*! Devices to query */
	int devices;
} ClientQuery;

/**
 * \def ECASE
 * Error case definition - on error return message.
 */
#define ECASE(error, message) case error : return message

// Configuration
/**
 * \def FNLENGTH
 * Maximum length of a funciton name in driver_kerl
 */
#define FNLENGTH 20
/**
 * \def IDLENGTH
 * Maximum length of the robot id
 */
#define IDLENGTH 50
/**
 * \def DOMAINLENGTH
 * Maximum length of the domainname of player
 */
#define DOMAINLENGTH 50

// Append error messages

// general return values
/**
 * \def SUCCESS
 * Success return value
 */
#define SUCCESS 0
/**
 * \def GENERICFAIL
 * Generic failure return value
 */
#define GENERICFAIL -1
/**
 * \def BROKENCODE
 * Broken code - see code for details
 */
#define BROKENCODE -2

// general player kerl errors start from -64
/**
 * \def NAMEINUSE 
 * Error: Name in use
 */
#define NAMEINUSE -64
/**
 * \def NOSUCHCLIENT
 * Error: No such client
 */
#define NOSUCHCLIENT -65
/**
 * \def NOTSTORED
 * Error: Not stored
 */
#define NOTSTORED -66
/**
 * \def DEVICENOTINITIALISED 
 * Error: Device not initialised
 */
#define DEVICENOTINITIALISED -67
/**
 * \def NOSUCHDEVICE 
 * Error: No such device
 */
#define NOSUCHDEVICE -68
/**
 * \def ERRORQUERYINGDEVICES 
 * Error: Error querying devices
 */
#define ERRORQUERYINGDEVICES -69
/**
 * \def INVALIDINDEX
 * Error: Invalid index specified
 */
#define INVALIDINDEX -70

// errors from player start from -128
/**
 * \def ERRORCONNECTING 
 * Player error: Error connecting
 */
#define ERRORCONNECTING -128
/**
 * \def PLAYERFAIL 
 * Player error: Player failure
 */
#define PLAYERFAIL -127
/**
 * \def NOSOCKET 
 * Player error: No socket
 */
#define NOSOCKET -126

// errors from erlang interface start from -192
/**
 * \def UNDEFINEDFUNCTION 
 * Erlang interface error: Undefined function
 */
#define UNDEFINEDFUNCTION -192
/**
 * \def UNDEFINEDOPTION 
 * Erlang interface error: Undefined option
 */
#define UNDEFINEDOPTION -193


// This is a list of devices we can support
/**
 * \def SUPPOSITION 
 * Supported device: Position sensor
 */
#define SUPPOSITION 0x1
/**
 * \def SUPLASERS 
 * Supported device: Laser range finder
 */
#define SUPLASERS 0x2
/**
 * \def SUPSONAR 
 * Supported device: Sonar range finder
 */
#define SUPSONAR 0x4
/**
 * \def SUPFIDUCIAL 
 * Supported device: Fiducial sensors
 */
#define SUPFIDUCIAL 0x8

//add to this list must be in power of twos
//for example, gps might be 0x16 and then camera might be 0x32
//This allows device to be represented as a binary on or off

/**
 * \fn errorToString(int)
 * \param errorno the error value passed
 * \return the string representation of the error
 * Converts a error defined in common_kerl.h 
 * into a human readable form
 * All spaces must be replaced with underscores to be
 * passed as erlang atoms
 * If the error is not found it should return the number 
 * as a string.
 * You can also pass SUCCESS to get a string representation
 * that will work in Erlang, just dont pass it back as
 * an error {error, ok} otherwise it will seem rather silly
 */
char* errorToString(int errorno);
