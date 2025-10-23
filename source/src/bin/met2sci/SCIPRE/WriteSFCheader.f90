INTEGER FUNCTION WriteSFCheader( elev,lat,lon ,landuse) RESULT( irv )

!------ Write header based on ISHD surface files

USE met2sci_fi

IMPLICIT NONE

REAL ,        INTENT( IN ) :: elev, lat, lon
CHARACTER(*), INTENT( IN ) :: landuse

INTEGER nt, i

irv = FAILURE

nt = 13
IF( elev /= -9999. )nt = nt + 1

carg(1)  = 'ID'      ; carg(nt+1)  = ' '
carg(2)  = 'YYMMDD'  ; carg(nt+2)  = ' '
carg(3)  = 'HOUR'    ; carg(nt+3)  = 'HRS'
carg(4)  = 'LAT'     ; carg(nt+4)  = 'N'
carg(5)  = 'LON'     ; carg(nt+5)  = 'E'
carg(6)  = 'Z'       ; carg(nt+6)  = 'M'
carg(7)  = 'P'       ; carg(nt+7)  = 'MB'
carg(8)  = 'CC'      ; carg(nt+8)  = ' '
carg(9)  = 'PRATE'   ; carg(nt+9)  = 'MM/HR'
carg(10) = 'T'       ; carg(nt+10) = 'C'
carg(11) = 'HUMID'   ; carg(nt+11) = '%'
carg(12) = 'WDIR'    ; carg(nt+12) = 'DEG'
carg(13) = 'WSPD'    ; carg(nt+13) = 'M/S'
IF( elev /= -9999. )THEN
 carg(nt) = 'ELEV'; carg(2*nt) = 'M'
END IF


WRITE(lunOutSFC,FMT='(A)',     IOSTAT=ios,ERR=9999) '# Generated with MET2SCI'
WRITE(lunOutSFC,FMT='(A)',     IOSTAT=ios,ERR=9999) '# TYPE: OBSERVATION' 
WRITE(lunOutSFC,FMT='(A)',     IOSTAT=ios,ERR=9999) '# LANDUSE: '//TRIM(landuse)
WRITE(lunOutSFC,FMT='(A)',     IOSTAT=ios,ERR=9999) '# TIMEREFERENCE: UTC  '
WRITE(lunOutSFC,FMT='(A,F8.3)',IOSTAT=ios,ERR=9999) '# LATITUDE:  ',lat
WRITE(lunOutSFC,FMT='(A,F8.3)',IOSTAT=ios,ERR=9999) '# LONGITUDE: ',lon
WRITE(lunOutSFC,FMT='(A)',     IOSTAT=ios,ERR=9999) '#'
WRITE(lunOutSFC,FMT='(A)',     IOSTAT=ios,ERR=9999) 'SURFACE'
WRITE(lunOutSFC,FMT='(I3)',    IOSTAT=ios,ERR=9999) nt
WRITE(lunOutSFC,FMT='(30A8)',  IOSTAT=ios,ERR=9999) (carg(i   ),i=1,nt)
WRITE(lunOutSFC,FMT='(30A8)',  IOSTAT=ios,ERR=9999) (carg(i+nt),i=1,nt)
WRITE(lunOutSFC,FMT='(A)',     IOSTAT=ios,ERR=9999) MISSING_C

irv = SUCCESS

9999 CONTINUE

RETURN
END


