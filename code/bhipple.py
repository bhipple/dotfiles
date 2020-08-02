import subprocess
from talon import Module


mod = Module()


@mod.action_class
class Actions:
    def launch():
        """Startup dmenu to launch a program"""
        subprocess.check_call(['dmenu_run'])

    def alphabet():
        """Show the alphabet"""
        subprocess.check_call(['alphabet'])

    def emacsclient():
        """Show the alphabet"""
        subprocess.check_call(['emacsclient', '-c'])
