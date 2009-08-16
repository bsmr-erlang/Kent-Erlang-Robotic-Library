#!/bin/bash
## first draft player-stage world generator script
## accelerator for multiple robot configs...
## Author - Matthew Champ
## Example - /world.sh StenGruener filename 55 position2d laser speech
if [ -z $1 ]; then
	echo "Usage: Provide your name, the name you want your configs to have and the number of robots. All further parameters will then be used as device names!"
	exit
fi
USER=$1
WORLD=$2
ROBCOUNT=$3
OF="$2.world"
OF2="$2.cfg"
ARGCOUNT=$#
let "REMARGS=$ARGCOUNT-3"
shift;shift;shift
DEVICES=$@
COUNTING=0
echo "Remaining arguments read as device requests: $DEVICES"
for d in $DEVICES
do
DEVICE[$COUNTING]=$d
COUNTING=$(($COUNTING + 1))
done

echo "DEBUG: Devices are now set as the following:"
for elements in $(seq 0 $((${#DEVICE[@]}-1)))
do
echo "${DEVICE[$elements]}"
done
echo "DEBUG: COUNTING is now $COUNTING"

## begin .world writing section
echo "# $WORLD" > $OF
echo "# Author - $1" >> $OF
echo "" >> $OF
echo "include \"pioneer.inc\"" >> $OF
echo "include \"map.inc\"" >> $OF
echo "include \"sick.inc\"" >> $OF
echo "" >> $OF
echo "interval_sim 100" >> $OF
echo "interval_real 1" >> $OF
echo "" >> $OF
echo "paused 0" >> $OF
echo "" >> $OF
echo "window" >> $OF
echo "(" >> $OF
echo " size [ 700.000 700.000 ]" >> $OF
echo " scale 37" >> $OF
echo " show_data 1" >> $OF
echo ")" >> $OF
echo "" >> $OF
echo "floorplan" >> $OF
echo "(" >> $OF
echo " name \"cave\"" >> $OF
echo " size [16 16 0.6]" >> $OF
echo " bitmap \"bitmaps/rink.png\"" >> $OF
echo ")" >> $OF
echo "" >> $OF

## create the robots section, find some way of setting positions without a majillion parameters?
for i in `seq 1 $ROBCOUNT`;
do
	echo "pioneer2dx" >> $OF
	echo "(" >> $OF
	echo " name \"r$i\"" >> $OF
	echo "" >> $OF
	echo " pose [ 0 0 0 0 ]" >> $OF
	echo " sicklaser()" >> $OF
	echo ")" >> $OF
done
## end .world writing section

## begin .cfg writing section
echo "# Author - $1" > $OF2
echo "" >> $OF2
echo "#load Stage plugin simulation driver" >> $OF2
echo "driver" >> $OF2
echo "(" >> $OF2
echo " name \"stage\"" >> $OF2
echo " provides [ \"simulation:0\" ]" >> $OF2
echo " plugin \"libstageplugin\"" >> $OF2
echo "" >> $OF2
echo " # load named worldfile into sim" >> $OF2
echo " worldfile \"$OF\"" >> $OF2
echo ")" >> $OF2
echo "" >> $OF2

## create stage drivers for the robots
for i in `seq 1 $ROBCOUNT`;
do
	let "TEMP=$i-1"
	let "TEMP2=$COUNTING-1"
	DEVICESTRING=''

	for device in $DEVICES
	do
	DEVICESTRING="$DEVICESTRING \"$device:$TEMP\""
	done

	echo "driver" >> $OF2
	echo "(" >> $OF2
	echo " name \"stage\"" >> $OF2
	echo " provides [ $DEVICESTRING ]" >> $OF2
##	echo " provides [ \"position2d:$TEMP\" \"laser:$TEMP\" \"speech:$TEMP\" ]" >> $OF2
	echo " model \"r$i\"" >> $OF2
	echo ")" >> $OF2
	echo "" >> $OF2
done
