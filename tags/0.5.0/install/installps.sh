#!/bin/sh
# Authors: Sten Gruener, Thomas Lorentsen
# Version: 2008-11-02
#I implement http://www.itoc.usma.edu/webbot/pmwiki.php/Main/Player-Stage with some improvements
#
# Script now supports Gentoo ~tom
# - You need to install dependancies first; if you find a required package please add to the
#   packages install section of this script ~tom
# - Please check script for any mistakes before you bork your computers ( still in debug stage )
# - Confirmed working on Toms Gentoo and Ubuntu Box ~tom
#
# This allows Stage to find player and install the libraries required for player
export PKG_CONFIG_PATH="$(pwd)/player-2.1.2/libplayercore/"

if [ $USER != "root" ]; then
        echo Run this as root
        exit 1
fi

#init fancy colors
esc=""

blackf="${esc}[30m";   redf="${esc}[31m";    greenf="${esc}[32m"
yellowf="${esc}[33m"   bluef="${esc}[34m";   purplef="${esc}[35m"
cyanf="${esc}[36m";    whitef="${esc}[37m"
  
blackb="${esc}[40m";   redb="${esc}[41m";    greenb="${esc}[42m"
yellowb="${esc}[43m"   blueb="${esc}[44m";   purpleb="${esc}[45m"
cyanb="${esc}[46m";    whiteb="${esc}[47m"

boldon="${esc}[1m";    boldoff="${esc}[22m"
italicson="${esc}[3m"; italicsoff="${esc}[23m"
ulon="${esc}[4m";      uloff="${esc}[24m"
invon="${esc}[7m";     invoff="${esc}[27m"

reset="${esc}[0m"
#end colors

#test funtion
try(){
	if eval $1; then 
		echo "${greenf}${boldon}done${boldoff}${reset}"; 
	else 
		echo "${redf}${boldon}fail${boldoff}${reset}";
		exit 0;
	fi
}

echo "${boldon}${ulon}Welcome to Player/Stage installer${uloff}"
#update env variables
#These should already be set
#echo "Updating enviroment file...${boldoff}"
#echo "LIBRARY_PATH=\"$LIBRARY_PATH/usr/local/lib:/usr/lib:\"" >> /etc/environment
#echo "LD_LIBRARY_PATH=\"$LD_LIBRARY_PATH/usr/local/lib:/usr/lib:/lib:\"" >> /etc/environment 
#echo "PYTHONPATH=\"$PYTHONPATH/usr/local/lib/python2.5/site-packages:\"" >> /etc/environment  

#fetch packets
echo "${boldon}Checking packets...${boldoff}"

OSD="none"
# if OS is ubuntu use apt-get
grep -io ubuntu /etc/*-release > /dev/null
if [ $? -eq 0 ]
then
OSD="debian based"
echo "${boldon}Ubuntu Detected${boldoff}"
sleep 3
#try "apt-get -y update"
#try "apt-get -y upgrade"
try "apt-get -y install gcc"
try "apt-get -y install g++"
try "apt-get -y install bjam"
try "apt-get -y install libboost-dev"
try "apt-get -y install libboost-date-time-dev"
try "apt-get -y install libboost-filesystem-dev"
try "apt-get -y install libboost-graph-dev"
try "apt-get -y install libboost-iostreams-dev"
try "apt-get -y install libboost-program-options-dev"
try "apt-get -y install libboost-python-dev"
try "apt-get -y install libboost-regex-dev"
try "apt-get -y install libboost-serialization-dev"
try "apt-get -y install libboost-signals-dev"
try "apt-get -y install libboost-test-dev"
try "apt-get -y install libboost-thread-dev"
try "apt-get -y install libboost-wave-dev "
try "apt-get -y install build-essential"
try "apt-get -y install cmake" #cmake for stage
try "apt-get -y install libcv-dev" #(Open CV - to support the drivers requiring the Computer Vision library)
try "apt-get -y install libfltk1.1-dev" #(Required for Stage install)
#(Mesa's implementation of OpenGL, required for Stage)
try "apt-get -y install libgl1-mesa-dev libgl1-mesa-glx mesa-common-dev gle-doc"
#try "apt-get -y install libglu1-mesa libglu1-mesa-dev"
#try "apt-get -y install freeglut3 freeglut3-dev freeglut3-dbg libglut3  libglut3-dev glut-doc glutg3-dev"


try "apt-get -y install libglib2.0-dev" #(for Stage)
try "apt-get -y install libgsl0-dev" #(GNU Scientific Library, required for the amcl--adaptive monte carlo driver)
try "apt-get -y install libltdl3-dev"
try "apt-get -y install libpng12-dev" #(for Stage)
try "apt-get -y install libstdc++6-4.2-dev" #todo - check if it works cuz its a virtual package
try "apt-get -y install python2.5-dev"
try "apt-get -y install swig" 
#last one
try "apt-get -y install glutg3-dev"
#erlang
try "apt-get -y install erlang"
try "apt-get -y install erlang-dev"
fi
# If os is gentoo, use emerge
grep -io gentoo /etc/*-release > /dev/null
if [ $? -eq 0 ]
then
OSD="Gentoo"
echo "${boldon}Gentoo Detected${boldoff}"
# An ebuild would be nice one day
# stage configuration will fail without the masked version of cmake
try "USE='opengel' emerge =dev-util/cmake-2.4.8 fltk"
try "emerge erlang"

fi

# Ask if you want to continue even if os was not detected
# 
if [ $OSD == "none" ]
then
read -p "OS Not detected, would you like to continue? [N|y] "
#if [ ! "$(echo $REPLY | grep -o ^y)" =="y"  ]
if [ ! $REPLY == "y" ] 
then
    exit 0
fi
fi


#install Player first
echo "${boldon}Installing Player...${boldoff}"


	echo "Downloading player..."
	#fetching the source
	try "wget -c http://mesh.dl.sourceforge.net/sourceforge/playerstage/player-2.1.2.tar.gz"
	echo "Extracting..."
	try "tar xfz player-2.1.2.tar.gz"
	rm player-2.1.2.tar.gz
	cd player-2.1.2
	#clear for configure
#fi

#configuring player
echo "Configuring..."
try "./configure --prefix=/usr/local/share/player"
echo "Compiling..."
try "make"
echo "Installing..."
try "make install"


#time to install Stage
echo "${boldon}Installing Stage...${boldoff}"

	cd ..
	echo "Downloading stage..."
	#fetching the source
	try "wget -c http://mesh.dl.sourceforge.net/sourceforge/playerstage/Stage-3.0.1-Source.tar.gz"
	echo "Extracting..."
	try "tar xfz Stage-3.0.1-Source.tar.gz"
	rm Stage-3.0.1-Source.tar.gz
	cd Stage-3.0.1-Source
	#clear for configure
#fi

#configuring stage
echo "Configuring stage..."
try "cmake -DCMAKE_INSTALL_PREFIX=/usr/local/share/stage ."
echo "Compiling..."
try "make"
echo "Installing stage..."
try "make install"


#update path
echo "${boldon}Updating PATH...${boldoff}"
echo "PATH=\"$PATH:/usr/local/share/player/bin:/usr/local/share/stage/bin:\"" >> ~/.bashrc

#update Lib path for stage and player
echo "export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/share/stage/lib:/usr/local/share/player/lib:" >> ~/.bashrc

#register erl`s alias to run it with multithread support
echo "alias erl='erl +A24'" >> ~/.bashrc

#clean
echo "${boldon}Cleaning up...${boldoff}"
try "apt-get clean"
try "apt-get autoclean"

#done
echo " "
echo "${boldon}Success${boldoff}"
echo "Player is installed into /usr/local/share/player"
echo "Stage is installed into /usr/local/share/stage"
echo "${boldon}Please restart terminal${boldoff}"

#update pkg-config with some useful paths that may be needed later
echo "export PKG_CONFIG_PATH=\"$PKG_CONFIG_PATH:$(pwd)/player-2.1.2/libplayercore/:\"" >> ~/.bashrc

exit 0
