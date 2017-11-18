# start vbox stuffs
VBoxClient-all &

# change bg img
feh --bg-fill /home/sarai/pics/colorful_flowers_dark_background-wide.jpg &

# start key handler + bspwm
sxhkd &
panel &
exec bspwm
