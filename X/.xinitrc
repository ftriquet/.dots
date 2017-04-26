[ -f $HOME/.Xresources ] && xrdb -merge ~/.Xresources
[ -z $(xrandr | grep HDMI1 | cut -d ' ' -f2 | grep -i disconnected) ] && xrandr --output HDMI1 --auto --right-of eDP1
xset r rate 200 25
setxkbmap -option compose:ralt
setxkbmap us dvp
numlockx on
synclient TapButton1=1
synclient TapButton2=3
xautolock -time 5 -locker '/home/francois/.bin/lock.sh' &
nm-applet &
dunst &
compton &

exec xmonad
