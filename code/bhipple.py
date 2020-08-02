import subprocess
from talon import Module


mod = Module()


@mod.action_class
class Actions:
    def run(cmd: str):
        """Helper to exec something"""
        subprocess.check_call(cmd.split())
