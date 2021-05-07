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

printf "$1\n1\n$2\n1 2 3 4 5 6 7\n190\n37 55\n" | ./event 1>/dev/null