/**
 * \file ei_extended.h
 * \brief An Extention for Erlang Interface
 * \author Thomas Lorentsen, Sten Gruener
 *
 * For things that need to be done multiple times in kerl are defined here.
 */


/**
 * \fn freeMemory(KerlData*)
 * \param state ei struct
 *
 * Local function to free allocated memory
 */		
void freeMemory(KerlData* state);

/**
 * \fn returnOK(KerlData*)
 * \param state ei struct
 * Returns ok back to erlang as an atom
 */
void returnOK(KerlData* state);

/**
 * \fn returnErrorMessage(KerlData*, char*)
 * \param state ei struct
 * \param message The message tp pass back to erlang.  This is converted into an atom.
 * Returns an tuple to notify erlang of an error.
 * Messages must not contain underscores, commas, brackets, etc as this may confuse erlang a bit.
 * Use the function provided in common to convert error values into string representation.
 * This would be in the form of {error, message}
 */
void returnErrorMessage(KerlData* state, char* message);

/**
 * \fn pushOntoStack(ErlDrvTermData*, int*, ErlDrvTermData)
 * \param spec return struct
 * \param ptr integer pointer to the to pof the struct
 * \param data to put on the stack
 */

void pushOntoStack(ErlDrvTermData* spec, int* ptr, ErlDrvTermData data);

/**
 * \fn pushOntoStack(ErlDrvTermData*, int*, ErlDrvTermData, ErlDrvTermData)
 * \param spec return struct
 * \param ptr integer pointer to the to pof the struct
 * \param dataA data to put on the stack
 * \param dataB data to put on the stack
 */
void pushOntoStack(ErlDrvTermData* spec, int* ptr, ErlDrvTermData dataA, ErlDrvTermData dataB);

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
void pushDoubleListOntoStack(ErlDrvTermData* spec, int* ptr, double* array, int size);
