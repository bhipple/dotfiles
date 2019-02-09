#!/usr/bin/env python3
# Script to pre-compute and pre-fetch a large number of dictionary of name:verison packages off of pypi.
# Things not yet supported:
# - wheel formats
# - formats other than tar.gz
import subprocess

# Example syntax
packages = {
    "chardet":           "3.0.4",
    "requests":          "2.20.1",
    "requests-kerberos": "0.12.0",
    "yapf":              "0.25.0",
}

def prefetch(name, version):
    letter = name[0]
    sha256 = subprocess.check_output(
        'nix-prefetch-url https://files.pythonhosted.org/packages/source/{0}/{1}/{1}-{2}.tar.gz 2>/dev/null'.format(letter, name, version),
        shell=True).decode('utf-8').strip()
    print("{:<20} {:<10} {}".format(name, version, sha256))


if __name__ == '__main__':
    [prefetch(k, v) for k, v in packages.items()]
