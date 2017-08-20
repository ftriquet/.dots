xset r rate 200 25
[ -f $HOME/.Xresources ] && xrdb -merge ~/.Xresources
[ -z $(xrandr | grep HDMI1 | cut -d ' ' -f2 | grep -i disconnected) ] && xrandr --output HDMI1 --auto --above eDP1
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
#exec cinnamon-session
