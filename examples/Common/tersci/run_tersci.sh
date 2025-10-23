#!/bin/bash

BINDIR=../../../linux/bin

# Remove old output files
rm -f terrain.utm.ter tersci.utm.DOMDETAIL.OUT tersci.utm.MAPDETAIL.OUT tersci.utm.MAPPARAMS.OUT tersci.utm.out

# Run SCICHEM 
${BINDIR}/tersci -i tersci.utm.inp
