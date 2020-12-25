app: Anki
-

################################################################################
# General
sync: y

################################################################################
# Studying
again: 1
hard: 2
good: 3
easy: 4

flip: key(space)

# Prevent false positives
0: ""
5: ""
6: ""
7: ""
8: ""
9: ""

################################################################################
# Adding cards
add card: key(ctrl-enter)

# add frontside <user.text>:

add backside:
  key(ctrl-v)
  sleep(100ms)
  key(enter f3)