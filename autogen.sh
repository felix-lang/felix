#!/bin/bash
echo "DEFAULT FELIX BUILD"
./configure $*
./mk extract
./mk
