import subprocess
from talon import Module


mod = Module()


@mod.action_class
class Actions:
    def foo():
        """Really?"""
        print("Alive")
        subprocess.check_call(['touch', '/tmp/alive'])
        return "ran"

    def launch():
        """Startup dmenu to launch a program"""
        subprocess.check_call(['dmenu_run'])
