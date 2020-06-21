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
