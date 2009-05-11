#!/bin/bash
# Authors: Thomas Lorentsen
# Version: 2008-10-23

APT="aptitude install -y"
VMWARE="VMwareTools-*.tar.gz"

if [ $USER != "root" ]; then
	echo Run this as root
	exit 1
fi

## Updates ubuntu
if [ ! -f updated ] 
then
aptitude update && touch updated
fi

$APT xorg && \
$APT xfce4 && \
$APT thunar && \
$APT synaptic && \
$APT firefox && \
$APT gdm 

if [ ! $? -eq 0 ]; then
	echo "failed to install"
	exit 1
fi

# Some optional apps
$APT emacs

## Check if vmware tools is already installed
if [ ! -f /usr/lib/vmware-tools/bin32/vmware-toolbox ]
then
## Install Vmware tools
$APT autoconf automake binutils make cpp gcc linux-headers-$(uname -r)

mount /media/cdrom0 && cp /media/cdrom0/$VMWARE .
if [ -f $VMWARE ]
then
	tar xf $VMWARE -C /tmp/
	/tmp/vmware-tools-distrib/vmware-install.pl
	echo "You may wish to reboot now"
	echo "You can rerun this script to continue the installation"
	exit 0
else
	echo "Not installing vmware tools.  You must do this manually"
	echo "You can rerun the script to continue installation"
fi
fi
## Install player stage dependancies
PLAYERDEP="autotools-dev build-essential cpp libboost-dev libboost-thread-dev libboost-thread1.34.1 
libboost-signals-dev libboost-signals1.34.1 libltdl3 libltdl3-dev libgnomecanvas2-0 libgsl0-dev libgtk2.0-dev 
libjpeg62-dev libtool swig"

$APT $PLAYERDEP

if [ ! $? -eq 0 ]; then
        echo "Failed to install required packages for player stage"
	echo $APT $PLAYERDEP
        exit 1
fi


mkdir -p /usr/X11R6/lib/X11/
ln -s /etc/X11/rgb.txt /usr/X11R6/lib/X11/rgb.txt


