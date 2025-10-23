#!/bin/bash

BINDIR=../../../linux/bin

# Remove old output files
rm -f Discarded_ISHD_Records.dat KBOI2005.MSG KBOI2005.PRF KBOI2005.RPT KBOI2005.SFC

# Run SCICHEM 
${BINDIR}/metsci KBOI.2005.INP
