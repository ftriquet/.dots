#!/bin/bash
IMAGE=/tmp/i3lock.png
SCREENSHOT="scrot $IMAGE" # 0.46s

# Alternate screenshot method with imagemagick. NOTE: it is much slower
# SCREENSHOT="import -window root $IMAGE" # 1.35s

# Here are some imagemagick blur types
# Uncomment one to use, if you have multiple, the last one will be used

# All options are here: http://www.imagemagick.org/Usage/blur/#blur_args
# BLURTYPE="0x5" # 7.52s
#BLURTYPE="0x2" # 4.39s
BLURTYPE="5x2" # 3.80s
#BLURTYPE="2x8" # 2.90s
# BLURTYPE="2x3" # 2.92s

# Get the screenshot, add the blur and lock the screen with it
$SCREENSHOT
# convert $IMAGE -rotate 180 -noise $BLURTYPE $IMAGE
convert $IMAGE -blur "$BLURTYPE" -swirl 270 -noise "$BLURTYPE" $IMAGE
# convert $IMAGE /home/francois/morty.png -gravity north -composite -matte $IMAGE
# convert $IMAGE /home/francois/rick.png -gravity south -composite -matte $IMAGE

#IMAGE='/home/francois/Pictures/one_plus_3.png'
i3lock -u -i $IMAGE
rm $IMAGE

#i3lock-fancy -f ~/.fonts/overpass/overpass-regular.otf
