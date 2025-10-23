#!/bin/bash

# Set for your installation
BINDIR=../../linux/bin
LIBDIR=../../linux/lib64

export LD_LIBRARY_PATH=$LIBDIR:$LD_LIBRARY_PATH

inpFile=$1

# Run sciDOSPost
${BINDIR}/sciDOSpost -I:${BINDIR}/scipuff.ini -i $inpFile
