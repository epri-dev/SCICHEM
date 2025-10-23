!----------------------------------------------------------------------
SUBROUTINE write_dbg( nunit,filename,max,nvar,npoints,varnames,title, &
                       rdata,iflag,ihead )

IMPLICIT NONE

!...inputs:
! filename: CHARACTER*(*)   the xpp file to be generated.
! max:      integer*4       the first DIMENSION of data.
! nvar:     integer*4       the number of variables.
! varnames: CHARACTER(*)*   array of four CHARACTER variable names.
! title:    CHARACTER*(*)   the plot title.
! npoints: integer*4        the number of points for each variable.
! rdata:    real(max,1)     the data.
!                           (variables,points) is storage order if iflag=0
!                           (points,variables) is storage order if iflag=1
! iflag:    integer*4       see rdata
! ihead:    integer*4       numer of header variables (typically 1(single time) or 2(multiple times)

CHARACTER(*),           INTENT( IN ) :: title, filename
CHARACTER(*),           INTENT( IN ) :: varnames(*)
INTEGER,                INTENT( IN ) :: nunit, max, nvar, npoints, iflag, ihead
REAL, DIMENSION(max,1), INTENT( IN ) :: rdata

INTEGER ios, i, j, k

OPEN(UNIT=nunit,FILE=filename,STATUS='UNKNOWN',IOSTAT=ios)
IF( ios /= 0 )THEN
  WRITE(6,*) 'Unable to open file ',TRIM(filename)
  RETURN
END IF

WRITE(nunit,'(A)',IOSTAT=ios) 'Title: '//ADJUSTL(TRIM(title))
WRITE(nunit,'(A,I3)',IOSTAT=ios) 'Number of output variables:',nvar

IF( iflag == 0 )THEN

  DO j = 1,npoints
    DO i = ihead+1,nvar
      DO k = 1,ihead
        WRITE(nunit,FMT='(A)',ADVANCE='NO',IOSTAT=ios) TRIM(varnames(k))//', '
      END DO
      WRITE(nunit,FMT='(A)',ADVANCE='NO',IOSTAT=ios) varnames(i)(1:4)//':'
      DO k = 1,ihead
        WRITE(nunit,FMT='(ES15.7)',ADVANCE='NO',IOSTAT=ios) rdata(k,j)
      END DO
      WRITE(nunit,FMT='(ES15.7)',ADVANCE='YES',IOSTAT=ios)rdata(i,j)
    END DO
  END DO

ELSE

  DO j = 1,npoints
    DO i = 1,nvar
      DO k = 1,ihead
        WRITE(nunit,FMT='(A)',ADVANCE='NO',IOSTAT=ios) TRIM(varnames(k))//', '
      END DO
      WRITE(nunit,FMT='(A)',ADVANCE='NO',IOSTAT=ios) varnames(i)(1:4)//':'
      DO k = 1,ihead
        WRITE(nunit,FMT='(ES15.7)',ADVANCE='NO',IOSTAT=ios) rdata(j,k)
      END DO
      WRITE(nunit,FMT='(ES15.7)',ADVANCE='YES',IOSTAT=ios)rdata(j,i)
    END DO
  END DO

END IF

CLOSE(UNIT=nunit,IOSTAT=ios)

RETURN
END

!----------------------------------------------------------------------

SUBROUTINE write_csv( nunit,filename,max,nvar,npoints,varnames,title, &
                      rdata,iflag )

IMPLICIT NONE

!...inputs:
! filename: CHARACTER*(*)   the xpp file to be generated.
! max:      integer*4       the first DIMENSION of data.
! nvar:     integer*4       the number of variables.
! varnames: CHARACTER(*)*   array of four CHARACTER variable names.
! title:    CHARACTER*(*)   the plot title.
! npoints: integer*4        the number of points for each variable.
! rdata:    real(max,1)     the data.
!                           (variables,points) is storage order if iflag=0
!                           (points,variables) is storage order if iflag=1
! iflag:    integer*4       see rdata

CHARACTER(*),           INTENT( IN ) :: title, filename
CHARACTER(*),           INTENT( IN ) :: varnames(*)
INTEGER,                INTENT( IN ) :: nunit, max, nvar, npoints, iflag
REAL, DIMENSION(max,1), INTENT( IN ) :: rdata

INTEGER                    :: ios, i,j

CHARACTER(1) q, c

q = CHAR(34)  !Double quote
c = CHAR(44)  !Comma

OPEN(UNIT=nunit,FILE=filename,STATUS='UNKNOWN',IOSTAT=ios)
IF( ios /= 0 )THEN
  WRITE(6,*) 'Unable to open file ',TRIM(filename)
  RETURN
END IF

WRITE(nunit,'(A)',IOSTAT=ios) q//'Title: '//q//c//q//ADJUSTL(TRIM(title))//q
WRITE(nunit,'(A,I3)',IOSTAT=ios) q//'Number of output variables: '//q//c,nvar
DO i = 1,nvar
  WRITE(nunit,FMT='(A)',ADVANCE='NO',IOSTAT=ios) q//TRIM(varnames(i))//q//c
END DO
WRITE(nunit,FMT='()',ADVANCE='YES',IOSTAT=ios) !End of record

IF( iflag == 0 )THEN

  DO j = 1,npoints
    DO i = 1,nvar
      WRITE(nunit,FMT='(ES14.7,A)',ADVANCE='NO',IOSTAT=ios) rdata(i,j),c
    END DO
    WRITE(nunit,'()',ADVANCE='YES')
  END DO

ELSE

  DO j = 1,npoints
    DO i = 1,nvar
      WRITE(nunit,FMT='(ES14.7,A)',ADVANCE='NO',IOSTAT=ios) rdata(j,i),c
    END DO
    WRITE(nunit,'()',ADVANCE='YES')
  END DO

END IF

CLOSE(UNIT=nunit,IOSTAT=ios)

RETURN
END

!----------------------------------------------------------------------

SUBROUTINE write_ascii( nunit,filename,max,nvar,npoints,varnames,title, &
                        rdata,iflag )

IMPLICIT NONE

!...inputs:
! filename: CHARACTER*(*)   the xpp file to be generated.
! max:      integer*4       the first DIMENSION of data.
! nvar:     integer*4       the number of variables.
! varnames: CHARACTER(*)*   array of four CHARACTER variable names.
! title:    CHARACTER*(*)   the plot title.
! npoints: integer*4        the number of points for each variable.
! rdata:    real(max,1)     the data.
!                           (variables,points) is storage order if iflag=0
!                           (points,variables) is storage order if iflag=1
! iflag:    integer*4       see rdata

CHARACTER*(*),          INTENT( IN ) :: title, filename
CHARACTER*(*),          INTENT( IN ) :: varnames(*)
INTEGER,                INTENT( IN ) :: nunit, max, nvar, npoints, iflag
REAL, DIMENSION(max,1), INTENT( IN ) :: rdata

INTEGER ios, i, j

OPEN(UNIT=nunit,FILE=filename,STATUS='UNKNOWN',IOSTAT=ios)
IF( ios /= 0 )THEN
  WRITE(6,*) 'Unable to open file ',TRIM(filename)
  RETURN
END IF

WRITE(nunit,*)nvar
WRITE(nunit,'(A6)',ADVANCE='NO')'' !Some space at the beginning of the header line - IS
DO i = 1,nvar
  WRITE(nunit,'(A15)',ADVANCE='NO')varnames(i) !Changed from A8 to A15 to align with data - IS
END DO
WRITE(nunit,'()',ADVANCE='YES')


WRITE(nunit,*)TRIM(title)

IF( iflag == 0 )THEN

  DO j = 1,npoints
    DO i = 1,nvar
      WRITE(nunit,'(1p5e15.7)',ADVANCE='NO')rdata(i,j)
    END DO
    WRITE(nunit,'()',ADVANCE='YES')
  END DO

ELSE

  DO j = 1,npoints
    DO i = 1,nvar
      WRITE(nunit,'(1p5e15.7)',ADVANCE='NO')rdata(j,i)
    END DO
    WRITE(nunit,'()',ADVANCE='YES')
  END DO

END IF

CLOSE(UNIT=nunit,IOSTAT=ios)

RETURN
END

!----------------------------------------------------------------------

SUBROUTINE write_xpp( nunit,filename,max,nvar,npoints,varnames,title, &
                      rdata,iflag )

IMPLICIT NONE

!...inputs:
! filename: CHARACTER*(*)   the xpp file to be generated.
! max:      integer*4       the first DIMENSION of data.
! nvar:     integer*4       the number of variables.
! varnames: CHARACTER(*)*   array of four CHARACTER variable names.
! title:    CHARACTER*(*)   the plot title.
! nopoints: integer*4       the number of points for each variable.
! rdata:    real(max,1)     the data.
!                           (variables,points) is storage order if iflag=0
!                           (points,variables) is storage order if iflag=1
! iflag:    integer*4       see rdata

CHARACTER(*),           INTENT( IN    ) :: filename
CHARACTER(*),           INTENT( INOUT ) :: title
CHARACTER*(*),          INTENT( IN    ) :: varnames(*)
INTEGER,                INTENT( IN    ) :: nunit, max, nvar, npoints, iflag
REAL, DIMENSION(max,1), INTENT( IN    ) :: rdata

INTEGER       i, j, ierr, nch, nwch
INTEGER       nwvn, nw, nwttl, nskip, nwd
CHARACTER(4)  ctt
CHARACTER(40) vname

INTEGER, DIMENSION(1) :: idum
REAL,    DIMENSION(2) :: pid

nwvn = (LEN(varnames(1))+3)/4
nw   = 1
nwd  = 1

CALL disk_open( pid,TRIM(filename),nunit,nwd,'N',ierr )

IF( ierr /= 0 )THEN
  WRITE(6,*) 'Unable to open file ',TRIM(filename)
  RETURN
END IF

nch = MIN(LEN_TRIM(title),60)

IF( title(nch:nch) /= '<' )THEN
  nch = MIN(nch+1,60)
  title(nch:nch) = '<'
END IF

nwttl = (nch + 3)/4

nwch = MIN((nwttl+nwd-1)*4,60)

IF( nch /= nwch )THEN
  DO i = nch+1,nwch
    title(i:i) = ' '
  END DO
  nch   = nwch
  nwttl = (nch + 3)/4
END IF

nskip = -(nwttl+3)

IF( nwvn == 1 )THEN
  ctt = 'TT  '
ELSE
  WRITE(ctt,'(''#'',I3.3)')nwvn
END IF

idum(1) = nskip
CALL disk_write_i( pid,nw,1,idum )

CALL disk_write_c( pid,nw,1,ctt )

idum(1) = nwttl
CALL disk_write_i( pid,nw,1,idum )

CALL disk_write_c( pid,nw,nwttl,title )

idum(1) = npoints + nvar*65536
CALL disk_write_i( pid,nw,1,idum )

DO i = 1,nvar
  vname = varnames(i)
  CALL disk_write_c( pid,nw,nwvn,vname )
END DO

IF( npoints == 0 )GOTO 60

IF( iflag == 0 )THEN
  DO i = 1,npoints
    CALL disk_write( pid,nw,nvar,rdata(1,i) )
  END DO
ELSE
  DO i = 1,npoints
    DO j = 1,nvar
      CALL disk_write( pid,nw,1,rdata(i,j) )
    END DO
  END DO
END IF

60 CONTINUE
idum(1) = 1
CALL disk_write_i( pid,nw,1,idum )
CALL disk_close( pid )

RETURN
END
!********************************************************!
! - disk_close - wake plot file disk close
!********************************************************!
SUBROUTINE disk_close( filid )

IMPLICIT NONE

REAL, DIMENSION(2), INTENT( IN ) :: filid

INTEGER nf

nf = IFIX(filid(1))

CLOSE(UNIT=nf)

RETURN
END
!********************************************************!
! - disk_open - wake plot file disk OPEN
!********************************************************!
SUBROUTINE disk_open( filid,fname,nf,nwr,nrof,ierr )

IMPLICIT NONE

INTEGER,            INTENT( IN  ) :: nf, nwr
CHARACTER*(*),      INTENT( IN  ) :: fname, nrof
REAL, DIMENSION(2), INTENT( OUT ) :: filid
INTEGER,            INTENT( OUT ) :: ierr

INTEGER, PARAMETER :: irf = 4    ! recl length in bytes

INTEGER nbr

! Release 3.1 and earlier used number of bytes while Release 3.2 uses
! the number of words
!       nbr = 4*nwr
nbr = nwr

IF( nrof == 'R' )THEN
  OPEN(UNIT=nf,FILE=fname,STATUS='OLD',ACCESS='DIRECT', &
        RECL=irf*nbr,ERR=900,IOSTAT=ierr)
ELSE IF( nrof == 'N' )THEN
  OPEN(UNIT=nf,FILE=fname,STATUS='NEW',ACCESS='DIRECT', &
         RECL=irf*nbr,ERR=900,IOSTAT=ierr)
ELSE IF( nrof == 'O' )THEN
  OPEN(UNIT=nf,FILE=fname,STATUS='OLD',ACCESS='DIRECT', &
         RECL=irf*nbr,ERR=900,IOSTAT=ierr)
ELSE
  OPEN(UNIT=nf,FILE=fname,STATUS='UNKNOWN',ACCESS='DIRECT', &
         RECL=irf*nbr,ERR=900,IOSTAT=ierr)
END IF

filid(1) = FLOAT(nf)
filid(2) = FLOAT(nwr)

RETURN

!  OPEN error
900 CONTINUE

RETURN
END
!********************************************************!
! - disk_read - wake plot file disk read
!********************************************************!
SUBROUTINE disk_read( filid,nr,nwd,rdata )

IMPLICIT NONE

INTEGER,                 INTENT( IN    ) :: nwd
INTEGER,                 INTENT( INOUT ) :: nr
CHARACTER(*),            INTENT( INOUT ) :: cccc
INTEGER, DIMENSION(nwd), INTENT( INOUT ) :: idata
REAL, DIMENSION(2),      INTENT( IN    ) :: filid
REAL, DIMENSION(nwd),    INTENT( INOUT ) :: rdata

INTEGER i, is, ie
INTEGER nf, nwr, ns, ne

nf  = IFIX(filid(1))
nwr = IFIX(filid(2))

DO ns = 1,nwd,nwr
  ne = ns + nwr - 1
  READ(nf,REC=nr) (rdata(i),i=ns,ne)
  nr = nr+1
END DO

RETURN

!********************************************************!

ENTRY disk_read_c( filid,nr,nwd,cccc )

nf  = IFIX(filid(1))
nwr = IFIX(filid(2))

DO ns = 1,nwd,nwr
  ne = ns + nwr - 1
  is = (ns-1)*4 + 1
  ie = ne*4
  READ(nf,REC=nr)cccc(is:ie)
  nr = nr + 1
END DO

RETURN

!********************************************************!

ENTRY disk_read_i( filid,nr,nwd,idata )

nf  = IFIX(filid(1))
nwr = IFIX(filid(2))

DO ns = 1,nwd,nwr
  ne = ns + nwr - 1
  READ(nf,REC=nr) (idata(i),i=ns,ne)
  nr = nr + 1
END DO

RETURN

END

!********************************************************!
! - disk_write - wake plot file disk write
!********************************************************!
SUBROUTINE disk_write( filid,nr,nwd,rdata )

IMPLICIT NONE

INTEGER,                 INTENT( IN    ) :: nwd
INTEGER,                 INTENT( INOUT ) :: nr
CHARACTER(*),            INTENT( IN    ) :: cccc
INTEGER, DIMENSION(nwd), INTENT( IN    ) :: idata
REAL, DIMENSION(2),      INTENT( IN    ) :: filid
REAL, DIMENSION(nwd),    INTENT( IN    ) :: rdata

INTEGER i, is, ie
INTEGER nf, nwr, ns, ne

nf  = IFIX(filid(1))
nwr = IFIX(filid(2))

DO ns = 1,nwd,nwr
  ne = ns + nwr-1
  WRITE(nf,REC=nr) (rdata(i),i=ns,ne)
  nr = nr + 1
END DO

RETURN

!********************************************************!

ENTRY disk_write_i( filid,nr,nwd,idata )

nf  = IFIX(filid(1))
nwr = IFIX(filid(2))

DO ns = 1,nwd,nwr
  ne = ns + nwr - 1
  WRITE(nf,REC=nr) (idata(i),i=ns,ne)
  nr = nr + 1
END DO

RETURN

!********************************************************!

ENTRY disk_write_c( filid,nr,nwd,cccc )

nf  = IFIX(filid(1))
nwr = IFIX(filid(2))

DO ns = 1,nwd,nwr
  ne = ns + nwr - 1
  is = (ns-1)*4 + 1
  ie = ne*4
  WRITE(nf,REC=nr) cccc(is:ie)
  nr = nr + 1
END DO

RETURN

END
!********************************************************!
! - disk_readx - wake plot file disk read/WRITE with error
!********************************************************!
SUBROUTINE disk_readx( filid,nr,nwd,rdata,ierr )

IMPLICIT NONE

INTEGER                 :: nr, nwd, ierr
CHARACTER(*)            :: cccc
REAL, DIMENSION(2)      :: filid
REAL, DIMENSION(nwd)    :: rdata

INTEGER                 :: i, is, ie
INTEGER                 :: nf, nwr, ns, ne

nf  = IFIX(filid(1))
nwr = IFIX(filid(2))

DO ns = 1,nwd,nwr
  ne = ns+nwr-1
  READ (nf,rec=nr,err=20) (rdata(i),i=ns,ne)
  nr=nr+1
END DO
ierr = 0

RETURN

20 ierr = 1
RETURN

!********************************************************!
ENTRY disk_readx_c(filid,nr,nwd,cccc,ierr)

nf  = IFIX(filid(1))
nwr = IFIX(filid(2))

DO ns = 1,nwd,nwr
  ne = ns+nwr-1
  is = (ns-1)*4 + 1
  ie = ne*4
  READ (nf,rec=nr,err=50) cccc(is:ie)
  nr=nr+1
END DO

ierr = 0
RETURN

50      ierr = 1
RETURN

END

!********************************************************!

SUBROUTINE disk_writex(filid,nr,nwd,rdata,ierr)

IMPLICIT NONE

INTEGER                 :: nr, nwd, ierr
CHARACTER(*)            :: cccc
REAL, DIMENSION(2)      :: filid
REAL, DIMENSION(nwd)    :: rdata

INTEGER                 :: i, is, ie
INTEGER                 :: nf, nwr, ns, ne

nf  = IFIX(filid(1))
nwr = IFIX(filid(2))
nf  = IFIX(filid(1))
nwr = IFIX(filid(2))

DO ns = 1,nwd,nwr
  ne = ns+nwr-1
  WRITE (nf,rec=nr,err=70) (rdata(i),i=ns,ne)
  nr = nr+1
END DO

ierr = 0
RETURN

70      ierr = 1
RETURN

!********************************************************!
ENTRY disk_writex_c(filid,nr,nwd,cccc,ierr)

nf  = IFIX(filid(1))
nwr = IFIX(filid(2))

DO ns = 1,nwd,nwr
  ne = ns+nwr-1
  is = (ns-1)*4 + 1
  ie = ne*4
  WRITE (nf,rec=nr,err=90) cccc(is:ie)
  nr = nr+1
END DO

ierr = 0
RETURN

90      ierr = 1
RETURN

END
