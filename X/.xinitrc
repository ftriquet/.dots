xset r rate 200 25
setxkbmap -option compose:ralt
[ -f $HOME/.Xresources ] && xrdb -merge ~/.Xresources
[ -z $(xrandr | grep HDMI1 | cut -d ' ' -f2 | grep -i disconnected) ] && xrandr --output HDMI1 --auto --above eDP1
setxkbmap us dvp
numlockx on
synclient TapButton1=1
synclient TapButton2=3
xautolock -time 5 -notify 20 -notifier 'notify-send LOCK "Screen will be locked soon"' -locker '/home/francois/.bin/lock.sh' &
nm-applet &
dunst &
compton &

rm "${HOME}/.xmonad/xmonad.state"
exec xmonad
#exec cinnamon-session
