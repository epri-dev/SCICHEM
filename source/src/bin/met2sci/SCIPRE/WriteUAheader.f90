INTEGER FUNCTION WriteUAheader() RESULT( irv )

!------ Write header based on FSL upper air

USE met2sci_fi

IMPLICIT NONE

INTEGER nt, nv1, nv2, i

irv = FAILURE

nv1 = 5; nv2 = 6
nt  = nv1 + nv2

carg(1) = 'ID'     ; carg(nt+1) = ' '
carg(2) = 'LAT'    ; carg(nt+2) = 'N'
carg(3) = 'LON'    ; carg(nt+3) = 'E'
carg(4) = 'YYMMDD' ; carg(nt+4) = ' '
carg(5) = 'HOUR'   ; carg(nt+5) = 'HRS'

carg(nv1+1) = 'Z'     ; carg(nt+nv1+1) = 'M       '
carg(nv1+2) = 'P'     ; carg(nt+nv1+2) = 'MB      '
carg(nv1+3) = 'WDIR'  ; carg(nt+nv1+3) = 'DEG     '
carg(nv1+4) = 'WSPD'  ; carg(nt+nv1+4) = 'M/S     '
carg(nv1+5) = 'T'     ; carg(nt+nv1+5) = 'C       '
carg(nv1+6) = 'HUMID' ; carg(nt+nv1+6) = '%       '

WRITE(lunOutUA,FMT='(A)',   IOSTAT=ios,ERR=9999) '# Generated with MET2SCI'
WRITE(lunOutUA,FMT='(A)',   IOSTAT=ios,ERR=9999) '# TYPE: OBSERVATION' 
WRITE(lunOutUA,FMT='(A)',   IOSTAT=ios,ERR=9999) '# TIMEREFERENCE: UTC'
WRITE(lunOutUA,FMT='(A)',   IOSTAT=ios,ERR=9999) '#'
WRITE(lunOutUA,FMT='(A)',   IOSTAT=ios,ERR=9999) 'PROFILE'
WRITE(lunOutUA,FMT='(2I3)', IOSTAT=ios,ERR=9999) nv1, nv2
WRITE(lunOutUA,FMT='(30A8)',IOSTAT=ios,ERR=9999) (carg(i   ),i=1,nv1)
WRITE(lunOutUA,FMT='(30A8)',IOSTAT=ios,ERR=9999) (carg(i+nt),i=1,nv1)
WRITE(lunOutUA,FMT='(30A8)',IOSTAT=ios,ERR=9999) (carg(i   ),i=1+nv1,nv2+nv1)
WRITE(lunOutUA,FMT='(30A8)',IOSTAT=ios,ERR=9999) (carg(i+nt),i=1+nv1,nv2+nv1)
WRITE(lunOutUA,FMT='(A)',   IOSTAT=ios,ERR=9999)  MISSING_C

irv = SUCCESS

9999 CONTINUE

RETURN
END

