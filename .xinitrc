/usr/bin/VBoxClient-all &
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources
#xmonad
#gnome-session
#i3
#exec startkde

##!/bin/sh
#
# ~/.xinitrc
#
## Executed by startx (run your window manager from here)
#Monitor settings
#sh randr.sh &

#Services
#xscreensaver -no-splash &
#urxvtd &
#numlockx &
#udiskie &
#compton &
#sleep 2 && nitrogen --restore &
#trayer --edge top --align right --widthtype request --expand true --SetDockType true --SetPartialStrut true --transparent true --alpha 0 --tint 0x1A1918 --heighttype pixel --height 14 --padding 0 --distance 09 --distancefrom right &
#exec trayer --edge top --align right --widthtype pixel --width 100 --SetDockType true --SetPartialStrut true --transparent true --alpha 0 --tint 0x000000 --expand true --heighttype pixel --height 16 &


#Cursor settings
xsetroot -cursor_name left_ptr
#xinput --set-prop 9 "Device Accel Adaptive Deceleration" 1
#xinput --set-prop 9 "Device Accel Constant Deceleration" 1
#xinput --set-prop 9 "Device Accel Velocity Scaling" 1

#Modmap
if [ -f $HOME/.Xmodmap ]; then
    /usr/bin/xmodmap $HOME/.Xmodmap
fi

#Fonts
xset +fp /usr/share/fonts/local
xset fp rehash
while true; do
   xsetroot -name "$( date +"%F %R" )"
      sleep 1m    # Update time every minute
      done &

#Environments
#exec startkde
#export DE=kde
exec xmonad
#exec awesome
#exec gnome-session
#exec ck-launch-session startxfce4
##exec ck-launch-session openbox-session


