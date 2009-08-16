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
 * \file common_kerl.cpp
 * \brief Common definitions
 * \author Thomas Lorentsen, Sten Gruener
 *
 * Provides some common functions we will want to call
 * to do useful things.
 * Provides error handling.
 */
#include "common_kerl.h"

/**
 * \fn errorToString(int)
 * \param errorno the error value passed
 * \return the string representation of the error
 * Converts a error defined in common_kerl.h 
 * into a human readable form.
 * All spaces must be replaced with underscores to be
 * passed as erlang atoms.
 * If the error is not found it will return undefined_error.
 * This shouldn't happen though.
 * You can also pass SUCCESS to get a string representation
 * that will work in Erlang, just dont pass it back as
 * an error {error, ok} otherwise it will seem rather silly
 */
char* errorToString(int errorno) {
	switch (errorno) {
		ECASE(SUCCESS,"ok"); // This should produce lols when we get {error, ok} being sent to erlang
		ECASE(GENERICFAIL,"unimplimented_function");
		ECASE(BROKENCODE, "set_as_broken"); // see code
		ECASE(NAMEINUSE,"name_in_use");
		ECASE(NOSUCHCLIENT, "no_such_client");
		ECASE(NOTSTORED, "client_not_stored");
		ECASE(DEVICENOTINITIALISED, "device_not_initialized");
		ECASE(NOSUCHDEVICE, "no_such_device");
		ECASE(ERRORQUERYINGDEVICES, "device_query_failed");
		ECASE(INVALIDINDEX, "robot_not_found");
		ECASE(ERRORCONNECTING, "failed_to_connect");
		ECASE(PLAYERFAIL, "player_failed");
		ECASE(NOSOCKET, "player_no_socket");
		ECASE(UNDEFINEDFUNCTION, "undefined_function");
		ECASE(UNDEFINEDOPTION, "undefined_option");
		default: return "undefined_error"; // should convert the errorno to a string here
	}
}
