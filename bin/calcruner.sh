#!/bin/sh

#read date
#read deep
#read interval
#read objects
#read ist
#read position

#echo "--------params-----------------"
#printf "$date $deep $interval $objects $ist $position \n"
#echo "--------params-----------------"

#printf "$date\n$deep\n$interval\n$objects\n$ist\n$position\n" | ./regular 1>/dev/null

printf "2020 10 10 0 0 0\n1\n3600\n50 50\n" | ./info 1>/dev/null