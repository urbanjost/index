#!/bin/bash
python3 -m venv venv
source venv/bin/activate
pip install -U pip
pip install -r requirements.txt

#To validate registry.toml, run:

cat > /tmp/load_registry.py <<\EOF
import sys, toml

filename = 'fpm.toml'
with open(filename, 'r') as myfile:
            data=myfile.read()

p = toml.loads(data)
print(p)
EOF
/usr/bin/python3 /tmp/load_registy.py

#toml.loads() will fail with an exception in the TOML is invalid.
#For you own reference, here's a stand-alone script to verify TOML in Python:
