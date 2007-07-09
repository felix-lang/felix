#!/bin/bash
echo "DEFAULT FELIX BUILD"
`dirname $0`/configure $* && ./mk extract && ./mk
