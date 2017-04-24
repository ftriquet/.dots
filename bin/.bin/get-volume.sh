#!/bin/bash
echo -n "Vol: "
onoff=$(amixer get Master | rg 'Front Left:' | cut -d '[' -f3 | cut -d ']' -f1)
if [[ $onoff = 'on' ]]; then
	amixer get Master | rg 'Front Left:' | cut -d '[' -f2 | cut -d ']' -f1
else
	echo "Muted"
fi
