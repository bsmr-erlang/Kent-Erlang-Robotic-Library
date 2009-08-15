README
======

KERL is a library and driver to allow robot control using Player from Erlang functional Language.  Please refer to INSTALL for detailed guidelines.

PRE INSTALLATION
================

KERL requires the following to libraries function
	- libplayerc
	- ei


This should simply mean installing Player and Erlang.
You may have to build player from source to get the required header files.

BUILDING
========

To configure and install KERL modules and driver:

run

./confugure
make


INSTALLATION
============


Installation is not currently supported yet.
Please refer to ./INSTALL.txt
Also refer to INSTALL.* for guides on a particular distributon

You can run as root

$ make install

to install kerl to 

/usr/lib/erlang/lib/kerl/



DOCUMENTATION
=============

You can generate the edoc and doxygen documentation by

# cd ./doc/
# make

This will produce html files with the documentation for KERL's source files.


TESTING
=======

You must first build KERL before you can test it.

Run 'make test' to test all code.
Rhis can also be done in any folder inside testing.

Try the examples found in the example folder by running
player with the worlds provided in worlds folder.


DIRECTORY STRUCTURE
===================

doc				- Source Documentation
ebin			- Erlang compiled modules
examples	  	- Example erlang programs
include 	  	- Include files
kerlplayerlib 		- Player driver library
module 			- Module source code
testing			- Unit testing
stage			- Stage world configurations for testing

USAGE & EXAMPLE
===============
Reffer to INSTALL.txt for more information

AUTHORS
=======

Thomas Lorentsen
Sten Gruner
