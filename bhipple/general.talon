-
tag(): user.i3wm
tag(): user.git

################################################################################
# General
launch: user.system_command("dmenu_run")

run alphabet: user.system_command("alphabet")
run anki: user.system_command("anki")
run bar: user.system_command("dmenu_run")
run bluetooth connect: user.system_command("bt-connect")
run chromium: user.system_command("chromium")
run emacsclient: user.system_command("emacsclient -c")
run poweroff: user.system_command("poweroff")
run reboot: user.system_command("reboot")
run terminal: key(super-enter)

################################################################################
# Dumpers
dump emacs: user.system_command("notify-send \"$(cat ~/.talon/user/knausj_talon/bhipple/emacs.talon)\"")
dump terminal: user.system_command("notify-send \"$(cat ~/.talon/user/knausj_talon/bhipple/terminal.talon)\"")