import subprocess
from talon import Module


mod = Module()


@mod.action_class
class Actions:
    def launch():
        """Startup dmenu to launch a program"""
        subprocess.check_call(['dmenu_run'])
