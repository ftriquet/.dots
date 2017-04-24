#!/bin/bash
[[ $(playerctl status 2>&-) ]] || exit

status=$(playerctl status)
artist=$(playerctl metadata artist)
song=$(playerctl metadata title)
if [[ $status = 'Paused' ]]; then
	echo "$artist / $song / Paused"
else
	echo "$artist / $song / Playing"
fi
