#!/bin/bash

# Set for your installation
BINDIR=../../linux/bin
LIBDIR=../../linux/lib64

export LD_LIBRARY_PATH=$LIBDIR:$LD_LIBRARY_PATH

PRJNAME=$1

# Remove old output files
rm -f ${PRJNAME}.dep ${PRJNAME}.dos ${PRJNAME}.err ${PRJNAME}.log ${PRJNAME}.prj ${PRJNAME}.qlog ${PRJNAME}.rst ${PRJNAME}.smp ${PRJNAME}.asmp ${PRJNAME}.ados 

# Run SCICHEM 
${BINDIR}/runsci -I:${BINDIR}/scipuff.ini -P:${PRJNAME}
