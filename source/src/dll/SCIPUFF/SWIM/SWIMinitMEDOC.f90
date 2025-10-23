INTEGER FUNCTION SWIMinitMEDOC( Src,NestSrc )

!------ Initialize grid source structure for MEDOC file

USE SWIM_fi
USE SWIMparam_fd
USE Char8Array_fd
USE datums

IMPLICIT NONE

TYPE( GridSrc ),           INTENT( INOUT ) :: Src
CHARACTER(PATH_MAXLENGTH), INTENT( OUT   ) :: NestSrc

REAL, PARAMETER :: epsUTM = 100.E-3  !Tolerance for UTM test: 100 m

TYPE( Char8Array ) Var3d, Var2d

CHARACTER(8)   codename
CHARACTER(PATH_MAXLENGTH) FileName

INTEGER unit, ios, irv, zone
INTEGER nrecH
INTEGER imax, jmax, kmax, ndum, nvar3d, nvar2d
INTEGER nr, nl
REAL    dx, dy, x0, y0, xlat0, xlon0, zbtop_med, xlen
REAL    dz_tol, tMEDOC, x, y
REAL    tAnlys
LOGICAL lformat, lstagger, lend

INTERFACE
  INTEGER FUNCTION MEDOCgridRecord( unit,lformat,z,kmax,dx,dy,x0,y0,xlat0,xlon0,zbtop_med )
    USE SWIM_fi
    INTEGER, INTENT( IN  )      :: unit
    LOGICAL, INTENT( IN  )      :: lformat
    INTEGER, INTENT( IN  )      :: kmax
    REAL, DIMENSION(:), POINTER :: z
    REAL,         INTENT( OUT ) :: dx, dy, x0, y0, xlat0, xlon0, zbtop_med
  END FUNCTION MEDOCgridRecord

END INTERFACE

INTEGER, EXTERNAL :: MEDOCopenFile, MEDOCformatRecord, MEDOCcodenameRecord
INTEGER, EXTERNAL :: MEDOCreadModelHeader
INTEGER, EXTERNAL :: MEDOCclimoTimeOffset, MEDOCtimeRecord
INTEGER, EXTERNAL :: MEDOCdimensionRecord, MEDOCvarnameRecord
INTEGER, EXTERNAL :: skip
INTEGER, EXTERNAL :: getFieldIndex
INTEGER, EXTERNAL :: MEDOCgetNextTime, backup, SkipToBreak
INTEGER, EXTERNAL :: SetMEDOCvar2d, SetMEDOCvar3d

SWIMinitMEDOC = SWIMfailure

unit     = getFieldIndex( Src%unit )
FileName = TRIM(Src%Source(1))
lformat  = .NOT.BTEST(Src%type,GSB_BINARY)

NULLIFY(Var3d%string,Var3d%unit)
NULLIFY(Var2d%string,Var2d%unit)

!------ Open file

irv = MEDOCopenFile( unit,FileName,lformat )
IF( irv /= SWIMsuccess )GOTO 9999

IF( Reverse )THEN
  Src%nBreak = 1  !Set to read first timebreak
ELSE
  Src%nBreak = 0
END IF

!------ Check for numerical model header, i.e., map projection and grid information

irv = MEDOCreadModelHeader( unit,lformat,Src,nrecH )
IF( irv /= SWIMsuccess )GOTO 9999

Src%nrecH = nrecH

!------ Read file type record

irv = MEDOCformatRecord( unit,lformat,lend )
IF( irv /= SWIMsuccess )GOTO 9999

IF( lend )THEN
  error%Number  = UK_ERROR
  error%Routine = 'SWIMinitMEDOC'
  error%Message = 'Error reading first record on MEDOC input file'
  CALL ReportFileName( error%Inform,'File=',FileName )
  GOTO 9999
END IF

!------ Read code name and staggered grid flag

irv = MEDOCcodenameRecord( unit,lformat,codename,lstagger,NestSrc )
IF( irv /= SWIMsuccess )GOTO 9999

IF( INDEX(codename,'WRF') > 0 )Src%type = IBSET(Src%type,GSB_WRFINP)  !Used to set WRF value of earth radius

!------ Read date and time

irv = MEDOCtimeRecord( unit,lformat,Src,tMEDOC,.FALSE. )
IF( irv /= SWIMsuccess )GOTO 9999

Src%tFirst = tMEDOC

irv = MEDOCtimeRecord( unit,lformat,Src,tAnlys,.FALSE. )  !Analysis time
IF( irv /= SWIMsuccess )GOTO 9999

IF( tAnlys == NOT_SET_R .OR. ABS(tAnlys) > 10.*365.*24.*3600. )THEN
  Src%tFcst = tMEDOC
ELSE
  Src%tFcst = tAnlys
END IF

!------ Read grid dimensions and number of variables

irv = MEDOCdimensionRecord( unit,lformat,imax,jmax,kmax,ndum,nvar3d,nvar2d )
IF( irv /= SWIMsuccess )GOTO 9999

Src%nX = imax
Src%nY = jmax
Src%nZ = kmax

Src%nXY = imax*jmax

Src%nVar2d = nvar2d
Src%nVar3d = nvar3d

ALLOCATE( Src%Z(kmax),STAT=ios )
IF( ios /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'SWIMinitMEDOC'
  error%Message = 'Error allocating vertical grid'
  GOTO 9999
END IF

!------ Read vertical grid and horizontal grid spacing and origin

irv = MEDOCgridRecord( unit,lformat,Src%Z,kmax,dx,dy,x0,y0,xlat0,xlon0,zbtop_med )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Set map type if not set by model header input

IF( BTEST(Src%type,GSB_NOMAP) )THEN

  IF( x0 == NOT_SET_R .AND. y0 == NOT_SET_R )THEN  !LL domain if cartesian coordinate not set

    Src%type = IBSET(Src%type,GSB_LATLON)

    xlen = FLOAT(imax-1)*dx
    IF( 360.-xlen > 0.1*dx .AND. 360.-xlen < 1.1*dx )Src%type = IBSET(Src%type,GSB_PERIODIC)

  ELSE !Cartesian coordinates; check for UTM definition

    IF( xlat0 == NOT_SET_R .AND. xlon0 /= NOT_SET_R )THEN !Special UTM input (zone in xlon0)

      zone = NINT(xlon0)
      IF( zone >= 1 .AND. zone <= 60 )THEN
        Src%type    = IBCLR(Src%type,GSB_CARTESIAN)
        Src%type    = IBSET(Src%type,GSB_UTM)
        Src%UTMzone = zone
        irv = UTM2LL( zone,x0,y0,xlat0,xlon0 )
        IF( irv /= 0 )THEN
          CALL SWIMsetUTMerror( 'UTM2LL',irv )
          error%Inform = 'Error converting MEDOC UTM location to LL'
          GOTO 9999
        END IF
        Src%type = IBSET(Src%type,GSB_UTM)
      ELSE
        xlon0    = NOT_SET_R
        Src%type = IBSET(Src%type,GSB_CARTESIAN)
      END IF

    ELSE IF( xlon0 /= NOT_SET_R .AND. xlat0 /= NOT_SET_R )THEN !Call it UTM if transformation of
                                                               !(xlat0,xlon0) matches (x0,y0)
      Src%type = IBSET(Src%type,GSB_CARTESIAN) !Default
      zone = 0
      ios  = 0
      CALL AdjustLongitude( xlon0 ) !Set to +/-180
      DO
        irv = LL2UTM( xlat0,xlon0,zone,x,y )
        IF( irv /= 0 )EXIT
        IF( ABS(x-x0) < epsUTM .AND. ABS(y-y0) < epsUTM )THEN
          Src%UTMzone = zone
          Src%type    = IBSET(Src%type,GSB_UTM)
          Src%type    = IBCLR(Src%type,GSB_CARTESIAN)
          EXIT
        END IF
        ios = ios + 1
        IF( ios == 2 )EXIT
        IF( x > x0 )THEN !Check if neighboring zone works
          zone = zone + 1
          IF( zone == 61 )zone = 1
        ELSE
          zone = zone - 1
          IF( zone == 0  )zone = 60
        END IF
      END DO

    ELSE

      Src%type = IBSET(Src%type,GSB_CARTESIAN)

    END IF

  END IF

!------ Also set default grid stagger

  IF( lstagger )THEN
    Src%nStg3D = 3
    Src%nStg2D = 0
    ALLOCATE( Src%iStg3D(0:3,Src%nStg3D),STAT=ios )
    IF( ios /= 0 )THEN
      error%Inform = 'Error allocating grid stagger arrays for 3d fields'
      GOTO 9999
    END IF
    Src%iStg3D(:,1) = (/ GVP_U,1,0,0 /)
    Src%iStg3D(:,2) = (/ GVP_V,0,1,0 /)
    Src%iStg3D(:,3) = (/ GVP_W,0,0,1 /)
    src%type = IBSET(src%type,GSB_STAGGER)
  END IF

END IF

!----- Adjust reference longitude to best fit project domain

IF( xlon0 /= NOT_SET_R )THEN
  IF( BTEST(Src%type,GSB_LATLON) )THEN
    CALL AdjustLongitudeDom( xlon0,FLOAT(imax-1)*dx ) !Add/subtract 360 (or not) as appropriate
  ELSE
    CALL AdjustLongitudeDom( xlon0,NOT_SET_R )
  END IF
END IF

!------ Set horizontal grid parameters for various map types

IF( BTEST(Src%type,GSB_LATLON) )THEN

  Src%dX   = dx
  Src%dY   = dy
  Src%X0   = xlon0
  Src%Y0   = xlat0
  Src%Lon0 = xlon0
  Src%Lat0 = xlat0

ELSE IF( BTEST(Src%type,GSB_ROTLL) )THEN

  Src%dX   = dx
  Src%dY   = dy
  Src%Lon0 = xlon0
  Src%Lat0 = xlat0
  Src%X0   = x0
  Src%Y0   = y0

ELSE  !All non-Lat/lon coordinates are "Cartesian-like" so we work in kilometers

  Src%dX   = dx * 1.E-3
  Src%dY   = dy * 1.E-3
  Src%Lon0 = xlon0
  Src%Lat0 = xlat0
  Src%X0   = x0
  Src%Y0   = y0

END IF

!------ Check zbtop

IF( BTEST(Src%type,GSB_NOMAP) )THEN

  IF( kmax > 1 )THEN

    IF( zbtop_med <= 0. )zbtop_med = Src%Z(kmax)

    dz_tol = 0.01*(Src%Z(kmax)-Src%Z(kmax-1))

    IF( lstagger )THEN

      Src%type  = IBSET(Src%type,GSB_MCWIF)
      zbtop_med = 0.5*(Src%Z(kmax)+Src%Z(kmax-1))

    ELSE IF( ABS(zbtop_med-Src%Z(kmax)) > dz_tol )THEN

      zbtop_med = Src%Z(kmax)

    END IF

    Src%Ztop = zbtop_med

  ELSE

    Src%Ztop = Src%Z(kmax)

  END IF

  Src%type = IBCLR(Src%type,GSB_NOMAP)
  Src%type = IBSET(Src%type,GSB_SIGMAZ)  !Default vertical coordinate

ELSE IF( BTEST(Src%type,GSB_SIGMAZM) .OR. BTEST(Src%type,GSB_SIGMAZ) )THEN

  IF( zbtop_med == 0. )THEN
    IF( lstagger )THEN
      IF( ASSOCIATED(Src%Zw) )THEN
        Src%Ztop = Src%Zw(kmax-1)
      ELSE
        Src%Ztop = 0.5*(Src%Z(kmax)+Src%Z(kmax-1))
      END IF
    ELSE
      Src%Ztop = Src%Z(kmax)
    END IF
  ELSE
    Src%Ztop = zbtop_med
  END IF

ELSE IF( BTEST(Src%type,GSB_ZMSL) .OR. BTEST(Src%type,GSB_ZAGL) )THEN

  Src%Ztop = 1.   !TBD

END IF

!------ Read variable names and units

Var3d%n = nvar3d
Var2d%n = nvar2d

irv = MEDOCvarnameRecord( unit,lformat,Var3d,Var2d,ndum )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Set 2d & 3d variable indicies

irv = SetMEDOCvar2d( Var2d,Src )
IF( irv /= SWIMsuccess )GOTO 9999

irv = SetMEDOCvar3d( Var3d,Src )
IF( irv /= SWIMsuccess )GOTO 9999

REWIND(unit)

IF( Prj%create )THEN
  SWIMinitMEDOC = SWIMresult
  GOTO 9999
END IF

!------ Set number of records for skipping and backspacing

IF( BTEST(Src%type,GSB_BINARY) )THEN

  IF( ndum > 0 )THEN
    nr = 10
  ELSE
    nr = 9
  END IF
  nr = nr + Src%nVar3d + Src%nVar2d

ELSE

  nr = 7  !Format+codename+2*times+dimension+2*dummy records
  nr = nr + (kmax+11+5)/6
  nr = nr + (ndum+2*Src%nVar3d+2*Src%nVar2d+5)/6
  nr = nr + (3*ndum+5)/6

  nl = (Src%nX*Src%nY*Src%nZ+5)/6
  nr = nr + Src%nVar3d * nl

  nl = (Src%nX*Src%nY+5)/6
  nr = nr + Src%nVar2d * nl

END IF

Src%nSkipBreak = nr

SWIMinitMEDOC = SWIMresult

9999 CONTINUE

IF( ASSOCIATED(Var3d%string) )DEALLOCATE( Var3d%string,STAT=ios )
IF( ASSOCIATED(Var3d%unit  ) )DEALLOCATE( Var3d%unit,  STAT=ios )
IF( ASSOCIATED(Var2d%string) )DEALLOCATE( Var2d%string,STAT=ios )
IF( ASSOCIATED(Var2d%unit)   )DEALLOCATE( Var2d%unit,  STAT=ios )

RETURN
END

!==============================================================================

INTEGER FUNCTION skip( unit,lformat,nrec )

USE SWIMparam_fd

IMPLICIT NONE

INTEGER, INTENT( IN ) :: unit, nrec
LOGICAL, INTENT( IN ) :: lformat

INTEGER i, ios

skip = SWIMfailure

ios = 0
IF( lformat )THEN
  DO i = 1,nrec
    READ(unit,*,IOSTAT=ios)
    IF( ios /= 0 )EXIT
  END DO
ELSE
  DO i = 1,nrec
    READ(unit,IOSTAT=ios)
    IF( ios /= 0 )EXIT
  END DO
END IF

! Check for end of file in all compilers
IF( IS_IOSTAT_END(ios) )ios = -1

IF( ios < 0 )THEN
  skip = SWIMnull
ELSE IF( ios > 0 )THEN
  skip = SWIMfailure
ELSE
  skip = SWIMsuccess
END IF

RETURN
END

!==============================================================================

INTEGER FUNCTION backup( unit,nrec )

USE SWIMparam_fd

IMPLICIT NONE

INTEGER, INTENT( IN ) :: unit, nrec

INTEGER i, ios

ios = 0
DO i = 1,nrec
  BACKSPACE(unit,IOSTAT=ios)
  IF( ios /= 0 )EXIT
END DO

IF( ios == 0 )THEN
  backup = SWIMsuccess
ELSE
  backup = SWIMfailure
END IF

RETURN
END

!==============================================================================

INTEGER FUNCTION SkipToBreak( Src )

!------ Position to read Src%nBreak

USE SWIM_fi
USE SWIMparam_fd
USE constants_fd

IMPLICIT NONE

TYPE( GridSrc ), INTENT( IN  ) :: Src

INTEGER ios, irv, nrecH
INTEGER unit
LOGICAL lformat

TYPE( GridSrc ) :: dumSrc

INTEGER, EXTERNAL :: MEDOCopenFile, MEDOCreadModelHeader, skip
INTEGER, EXTERNAL :: getFieldIndex

SkipToBreak = SWIMfailure

unit = getFieldIndex( Src%unit )
CLOSE(unit,IOSTAT=ios)

lformat = .NOT.BTEST(Src%type,GSB_BINARY)

irv = MEDOCopenFile( unit,TRIM(Src%Source(1)),lformat )
IF( irv /= SWIMsuccess )GOTO 9999

dumSrc%type = IZERO8

irv = MEDOCreadModelHeader( unit,lformat,dumSrc,nrecH )
IF( irv /= SWIMsuccess )GOTO 9999

SkipToBreak = skip( unit,lformat,(Src%nBreak-1)*Src%nSkipBreak )

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION MEDOCgetNextTime( Src,tMEDOC )

!------ Read next time break
!       N.B. File must be positioned at beginning of "current" time break
!       Blank lines are not permissible

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

TYPE( GridSrc ), INTENT( IN  ) :: Src
REAL,            INTENT( OUT ) :: tMEDOC

INTEGER unit, irv
LOGICAL lformat, lend, lstagger

CHARACTER(PATH_MAXLENGTH) NestSrc
CHARACTER(8)   codename

INTEGER, EXTERNAL :: MEDOCformatRecord, MEDOCcodenameRecord, MEDOCtimeRecord
INTEGER, EXTERNAL :: MEDOCskipModelHeader
INTEGER, EXTERNAL :: skip
INTEGER, EXTERNAL :: getFieldIndex

MEDOCgetNextTime = SWIMfailure
tMEDOC           = NOT_SET_R

unit    = getFieldIndex( Src%unit )
lformat = .NOT.BTEST(Src%type,GSB_BINARY)

!------ Skip format and codename records of next time break

irv = MEDOCskipModelHeader( Src )
IF( irv == SWIMnull )THEN
  lend = .TRUE.
  MEDOCgetNextTime = SWIMnull
  GOTO 9999
ELSE IF( irv /= SWIMsuccess )THEN
  GOTO 9999
END IF
irv = MEDOCformatRecord( unit,lformat,lend )
IF( irv /= SWIMsuccess )GOTO 9999
IF( lend )THEN               !Check for end-of-file
  MEDOCgetNextTime = SWIMnull
  GOTO 9999
END IF

irv = MEDOCcodenameRecord( unit,lformat,codename,lstagger,NestSrc )
IF( irv /= SWIMsuccess )GOTO 9999

irv = MEDOCtimeRecord( unit,lformat,Src,tMEDOC,.FALSE. )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Goto end of time break

irv = skip( unit,lformat,Src%nSkipBreak-3 )
IF( irv /= SWIMsuccess )THEN
  error%Number  = RD_ERROR
  error%Routine = 'MEDOCgetNextTime'
  error%Message = 'Error skipping to next time break'
  CALL ReportFileName( error%Inform,'File=',Src%Source(1) )
  GOTO 9999
END IF

MEDOCgetNextTime = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION MEDOCpositionToBreak( Src )

!------ Position to proper time break accounting for REVSERSE (or not) mode

USE SWIM_fi
USE SWIMparam_fd
USE constants_fd

IMPLICIT NONE

TYPE( GridSrc ), INTENT( INOUT ) :: Src

INTEGER irv, ios, unit
INTEGER nr, nrecH
REAL    tMEDOC
LOGICAL lformat
LOGICAL lend

CHARACTER(PATH_MAXLENGTH) FileName

TYPE( GridSrc ) :: dumSrc

INTEGER, EXTERNAL :: MEDOCreadModelHeader, PostProgressMessage
INTEGER, EXTERNAL :: MEDOCgetNextTime, skip, backup, SkipToBreak
INTEGER, EXTERNAL :: MEDOCformatRecord
INTEGER, EXTERNAL :: getFieldIndex

!------ Don't bother during create

IF( Prj%create )THEN
  MEDOCpositionToBreak = SWIMresult
  GOTO 9999
END IF

!------ Setup messaging

message%bString = 'Positioning to read MEDOC time break'

irv = PostProgressMessage( message )

!------ Initialize

MEDOCpositionToBreak = SWIMfailure

unit     = getFieldIndex( Src%unit )
nr       = Src%nSkipBreak
FileName = TRIM(Src%Source(1))
lformat  = .NOT.BTEST(Src%type,GSB_BINARY)

dumSrc%type = IZERO8

REWIND( unit,IOSTAT=ios )

!------ Skip model header section (if it exists)
!       N.B. Use dummy GridSrc

irv = MEDOCreadModelHeader( unit,lformat,dumSrc,nrecH )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Position to proper time break:
!       If Reverse=T, at the bottom of the first break after current time
!       Otherwise,     "  "    top  "   "    "     "   before   "      "

IF( Reverse )THEN
  irv = skip( unit,lformat,nr )
  IF( irv /= SWIMsuccess )THEN
    error%Number  = RD_ERROR
    error%Routine = 'MEDOCpositionToBreak'
    error%Message = 'Error skipping first time break'
    CALL ReportFileName( error%Inform,'File=',FileName )
    GOTO 9999
  END IF
END IF

DO

  irv = MEDOCgetNextTime( Src,tMEDOC ) !Leaves file positioned at bottom of tMEDOC time break

  IF( irv == SWIMfailure )THEN
    GOTO 9999
  ELSE IF( irv == SWIMnull )THEN            !End-of-file
    IF( Reverse )THEN
      irv = backup( unit,1 )
    ELSE
      irv = backup( unit,Src%nSkipBreak+1 )
      irv = MEDOCformatRecord( unit,lformat,lend )
      BACKSPACE(unit,IOSTAT=ios)
      IF( irv /= SWIMsuccess )THEN
        irv = backup( unit,Src%nrecH )
        irv = MEDOCformatRecord( unit,lformat,lend )
        BACKSPACE(unit,IOSTAT=ios)
      END IF
    END IF
    IF( irv == SWIMfailure )irv = SkipToBreak( Src )
    IF( irv /= SWIMsuccess )THEN
      error%Number  = RD_ERROR
      error%Routine = 'MEDOCpositionToBreak'
      error%Inform  = 'Error backspacing MEDOC file'
      CALL ReportFileName( error%Action,'File=',FileName )
      GOTO 9999
    END IF
    EXIT
  ELSE                                 !Success; increment timebreak count
    Src%nBreak = Src%nBreak + 1
  END IF

!------ Go until tMEDOC greater/less than current time, depending on reverse mode

  IF( Reverse )THEN
    IF( tMEDOC > Prj%time )CYCLE
  ELSE
    IF( tMEDOC < Prj%time )CYCLE
  END IF

  IF( .NOT.Reverse )THEN               !Position to read latest break with time <= current time
    Src%nBreak = Src%nBreak - 1        !N.B. Src%nBreak incremented in SWIMreadMEDOC
    irv = backup( unit,Src%nSkipBreak )
    irv = MEDOCformatRecord( unit,lformat,lend )
    BACKSPACE(unit,IOSTAT=ios)
    IF( irv /= SWIMsuccess )THEN
      irv = backup( unit,Src%nrecH )
      irv = MEDOCformatRecord( unit,lformat,lend )
      BACKSPACE(unit,IOSTAT=ios)
    END IF
    IF( irv /= SWIMfailure .AND. Src%nBreak > 0 )THEN
       Src%nBreak = Src%nBreak - 1
       irv = backup( unit,Src%nSkipBreak )
      irv = MEDOCformatRecord( unit,lformat,lend )
      BACKSPACE(unit,IOSTAT=ios)
      IF( irv /= SWIMsuccess )THEN
        irv = backup( unit,Src%nrecH )
        irv = MEDOCformatRecord( unit,lformat,lend )
        BACKSPACE(unit,IOSTAT=ios)
      END IF
    END IF
    IF( irv /= SWIMsuccess )THEN
      error%Number  = RD_ERROR
      error%Routine = 'SWIMinitMEDOC'
      error%Inform  = 'Error MEDOCpositionToBreak MEDOC file'
      CALL ReportFileName( error%Action,'File=',FileName )
      GOTO 9999
    END IF
  END IF

  EXIT

END DO

MEDOCpositionToBreak = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION MEDOCskipModelHeader( Src )

!------ Skip MEDOC model header section

USE SWIM_fi
USE SWIMparam_fd
USE constants_fd

IMPLICIT NONE

TYPE( GridSrc ), INTENT( IN ) :: Src

INTEGER irv, unit, ios
INTEGER nrecH
LOGICAL lformat

CHARACTER(8) string

TYPE( GridSrc ) :: dumSrc

INTEGER, EXTERNAL :: MEDOCreadModelHeader, PostProgressMessage
INTEGER, EXTERNAL :: getFieldIndex

!------ Don't bother during create

IF( Prj%create )THEN
  MEDOCskipModelHeader = SWIMresult
  GOTO 9999
END IF

!------ Setup messaging

message%bString = 'Positioning to read MEDOC time break'

irv = PostProgressMessage( message )

!------ Initialize

MEDOCskipModelHeader = SWIMfailure

unit     = getFieldIndex( Src%unit )
lformat  = .NOT.BTEST(Src%type,GSB_BINARY)

dumSrc%type = IZERO8

!------ Check for EOF

IF( lformat )THEN
  READ(unit,'(A)',IOSTAT=ios) string
ELSE
  READ(unit,IOSTAT=ios) string
END IF

! Check for end of file in all compilers
IF( IS_IOSTAT_END(ios) )ios = -1

IF( ios < 0 )THEN
  MEDOCskipModelHeader = SWIMnull
  GOTO 9999
ELSE IF( ios /= 0 )THEN
  GOTO 9999
END IF
BACKSPACE( unit,IOSTAT=ios )

!------ Skip model header section (if it exists)
!       N.B. Use dummy GridSrc

irv = MEDOCreadModelHeader( unit,lformat,dumSrc,nrecH )
IF( irv /= SWIMsuccess )GOTO 9999

MEDOCskipModelHeader = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION MEDOCopenFile( unit,FileName,lformat )

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

INTEGER,      INTENT( IN  ) :: unit
CHARACTER(*), INTENT( IN  ) :: FileName
LOGICAL,      INTENT( IN  ) :: lformat

INTEGER ios

MEDOCopenFile = SWIMfailure

IF( lformat )THEN
  OPEN(UNIT=unit,FILE=FileName,STATUS='OLD',FORM='FORMATTED', &
                                            ACTION='READ',IOSTAT=ios)
ELSE
  OPEN(UNIT=unit,FILE=FileName,STATUS='OLD',FORM='UNFORMATTED', &
                                            ACTION='READ',IOSTAT=ios)
END IF
! Check for end of file in all compilers
IF( IS_IOSTAT_END(ios) )ios = -1

IF( ios /= 0 )THEN
  error%Number  = OP_ERROR
  error%Routine = 'MEDOCopenFile'
  error%Message = 'Error opening MEDOC met file'
  CALL ReportFileName( error%Inform,'File=',FileName )
  GOTO 9999
END IF

MEDOCopenFile = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION MEDOCreadModelHeader( unit,lformat,Src,nrec )

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

INTEGER,         INTENT( IN    ) :: unit
LOGICAL,         INTENT( IN    ) :: lformat
TYPE( GridSrc ), INTENT( INOUT ) :: Src
INTEGER,         INTENT( OUT   ) :: nrec

INTEGER, PARAMETER :: MAXN = 10  !Max no. of variables in a map header record

INTEGER ios, nch, n_arg, i, j
LOGICAL lerr, lRead

CHARACTER(80) line
CHARACTER(64) kwrd
CHARACTER(32) c_arg(MAXN)
CHARACTER(8)  mapProj

MEDOCreadModelHeader = SWIMfailure

error%Number  = RD_ERROR
error%Routine = 'MEDOCreadModelHeader'
error%Message = 'Error reading MEDOC file'
error%Inform  = 'Attempting to read numerical model header records'

nrec = 0

IF( lformat )THEN

  READ(unit,'(A8)',IOSTAT=ios) mapProj
  IF( ios /= 0 )GOTO 9999
  IF( LEN_TRIM(mapProj) == 0 )THEN    !Blank lines not allowed
    error%Inform = 'Blank lines not allowed in MEDOC file'
    GOTO 9999
  END IF

  CALL cupper( mapProj )

  IF( mapProj(1:1) /= 'F'  )THEN
    error%Inform = 'Formatted file type specified'
    error%Action = 'Expecting first header record to begin with F'
    GOTO 9999
  END IF

ELSE

  READ(unit,IOSTAT=ios) mapProj
  IF( ios /= 0 )GOTO 9999

  IF( mapProj(1:1) /= 'B'  )THEN
    error%Inform = 'Binary file type specified'
    error%Action = 'Expecting first header record to begin with B'
    GOTO 9999
  END IF

END IF

!------ Check for map projection definition

lRead = .TRUE.

SELECT CASE( TRIM(ADJUSTL(mapProj(2:))) )
  CASE( 'LAMBERT' )
    Src%type = IBSET(Src%type,GSB_LAMBERT)

  CASE( 'POLAR' )
    Src%type = IBSET(Src%type,GSB_POLAR)

  CASE( 'RPOLAR','ROTPOLR','ROTPOLE' )
    Src%type = IBSET(Src%type,GSB_RPOLAR)
    lRead = .FALSE.

  CASE( 'ROTLL','RLATLON','LLROT' )
    Src%type = IBSET(Src%type,GSB_ROTLL)
    lRead = .FALSE.

  CASE( 'MERCATR','MERCATO','MERCTR' )
    Src%type = IBSET(Src%type,GSB_MERCATOR)

  CASE( 'UTM' )
    Src%type = IBSET(Src%type,GSB_UTM)

  CASE( 'LATLON','LL' )
    Src%type = IBSET(Src%type,GSB_LATLON)
    lRead = .FALSE.

  CASE( 'CARTESI','CART' )
    Src%type = IBSET(Src%type,GSB_CARTESIAN)
    lRead = .FALSE.

  CASE DEFAULT  !No recognized map projection; assume it's a standard MEDOC file
    Src%type = IBSET(Src%type,GSB_NOMAP)
    BACKSPACE(unit,IOSTAT=ios)
    CALL SWIMclearError()
    MEDOCreadModelHeader = SWIMresult
    GOTO 9999

END SELECT

nrec = 1

!------ Read horizontal project parameters

IF( lformat .AND. lRead )CALL get_next_data( unit,line,nch,kwrd,n_arg,c_arg,MAXN,lerr )

IF( BTEST(Src%type,GSB_LAMBERT) )THEN

  IF( lformat )THEN
    IF( lerr .OR. n_arg < 2 )ios = -1
    IF( ios == 0 )READ(c_arg(1),*,IOSTAT=ios ) Src%Lat1
    IF( ios == 0 )READ(c_arg(2),*,IOSTAT=ios ) Src%Lat2
  ELSE
    READ(unit,IOSTAT=ios) Src%Lat1,Src%Lat2
  END IF
  IF( ios /= 0 )THEN
    error%Inform = 'Error reading Lambert projection parameters'
    GOTO 9999
  END IF
  nrec = nrec + 1

ELSE IF( BTEST(Src%type,GSB_POLAR) )THEN

  IF( lformat )THEN
    IF( lerr .OR. n_arg < 1 )ios = -1
    IF( ios == 0 )READ(c_arg(1),*,IOSTAT=ios ) Src%Lat1
  ELSE
    READ(unit,IOSTAT=ios) Src%Lat1
  END IF
  IF( ios /= 0 )THEN
    error%Inform = 'Error reading Polar Stereographic projection parameters'
    GOTO 9999
  END IF
  nrec = nrec + 1

ELSE IF( BTEST(Src%type,GSB_MERCATOR) )THEN

  IF( lformat )THEN
    IF( lerr .OR. n_arg < 1 )ios = -1
    IF( ios == 0 )READ(c_arg(1),*,IOSTAT=ios ) Src%Lat1
  ELSE
    READ(unit,IOSTAT=ios) Src%Lat1
  END IF
  IF( ios /= 0 )THEN
    error%Inform = 'Error reading Mercator projection parameters'
    GOTO 9999
  END IF
  nrec = nrec + 1

ELSE IF( BTEST(Src%type,GSB_UTM) )THEN

  IF( lformat )THEN
    IF( lerr .OR. n_arg < 1 )ios = -1
    IF( ios == 0 )READ(c_arg(1),*,IOSTAT=ios ) Src%UTMzone
  ELSE
    READ(unit,IOSTAT=ios) Src%UTMzone
  END IF
  IF( ios /= 0 )THEN
    error%Inform = 'Error reading UTM zone'
    GOTO 9999
  END IF
  nrec = nrec + 1

END IF

!------ Determine vertical coordinate

IF( lformat )THEN
  CALL get_next_data( unit,line,nch,kwrd,n_arg,c_arg,MAXN,lerr )
  IF( lerr .OR. n_arg < 1 )ios = -1
ELSE
  READ(unit,IOSTAT=ios) c_arg(1)(1:8)
END IF
IF( ios /= 0 )THEN
  error%Inform = 'Error reading vertical coordinate type'
  GOTO 9999
END IF
nrec = nrec + 1

SELECT CASE( TRIM(ADJUSTL(c_arg(1)(1:8))) )
!  CASE( 'SIGMA' )
!    Src%type = IBSET(Src%type,GSB_SIGMA)   !Hydrostatic; height a function of temperature (time-varying)
!    N.B. not supported

  CASE( 'SIGMAF' )
    Src%type = IBSET(Src%type,GSB_SIGMAF)  !Non-hydrostatic; height fixed in time (for base state)

  CASE( 'Z3D','Z3DMSL','HGHT-MSL' )
    Src%type = IBSET(Src%type,GSB_ZMSL)    !3D, time-varying height MSL

  CASE( 'Z3DAGL','HGHT-AGL','HEIGHT' )
    Src%type = IBSET(Src%type,GSB_ZAGL)    !3D, time-varying height AGL

  CASE( 'SIGMAZ' )
    Src%type = IBSET(Src%type,GSB_SIGMAZ)  !0 corresponds to mean sea level

  CASE( 'SIGMAZM' )
    Src%type = IBSET(Src%type,GSB_SIGMAZM) !0 corresponds to minimum terrain within domain

  CASE DEFAULT
    error%Inform = 'Unrecognized vertical coordinate type'
    GOTO 9999

END SELECT

!------ Read top pressure level for SIGMA coordinate

!IF( BTEST(Src%type,GSB_SIGMA) .OR. BTEST(Src%type,GSB_SIGMAF) )THEN
IF( BTEST(Src%type,GSB_SIGMAF) )THEN
  IF( lformat )THEN
    CALL get_next_data( unit,line,nch,kwrd,n_arg,c_arg,MAXN,lerr )
    IF( lerr .OR. n_arg < 2 )ios = -1
    IF( ios == 0 )READ(c_arg(1),*,IOSTAT=ios ) Src%Ptop
    IF( ios == 0 )READ(c_arg(2),*,IOSTAT=ios ) Src%P00
  ELSE
    READ(unit,IOSTAT=ios) Src%Ptop,Src%P00
  END IF
  IF( ios /= 0 )THEN
    error%Inform = 'Error reading top and sea-level pressure levels for SIGMA coordinate'
    GOTO 9999
  END IF
  nrec = nrec + 1
END IF

!------ Read base state parameters for non-hydrostatic MM5

IF( BTEST(Src%type,GSB_SIGMAF) )THEN
  IF( lformat )THEN
    CALL get_next_data( unit,line,nch,kwrd,n_arg,c_arg,MAXN,lerr )
    IF( lerr .OR. n_arg < 3 )ios = -1
    IF( ios == 0 )READ(c_arg(1),*,IOSTAT=ios ) Src%TsrfRef
    IF( ios == 0 )READ(c_arg(2),*,IOSTAT=ios ) Src%lapseRef
    IF( ios == 0 )READ(c_arg(3),*,IOSTAT=ios ) Src%Tiso
  ELSE
    READ(unit,IOSTAT=ios) Src%TsrfRef,Src%lapseRef,Src%Tiso
  END IF
  IF( ios /= 0 )THEN
    error%Inform = 'Error reading temperature base state parameters'
    GOTO 9999
  END IF
  nrec = nrec + 1
END IF

!------ Setup staggered grids

IF( lformat )THEN
  CALL get_next_data( unit,line,nch,kwrd,n_arg,c_arg,MAXN,lerr )
  IF( lerr .OR. n_arg < 2 )ios = -1
  IF( ios == 0 )READ(c_arg(1),*,IOSTAT=ios ) Src%nStg3D
  IF( ios == 0 )READ(c_arg(2),*,IOSTAT=ios ) Src%nStg2D
ELSE
  READ(unit,IOSTAT=ios) Src%nStg3D,Src%nStg2D
END IF
IF( ios /= 0 )THEN
  error%Inform = 'Error reading number of staggered variables'
  GOTO 9999
END IF
nrec = nrec + 1

IF( Src%nStg3D > 0 )THEN  !3d fields

  ALLOCATE( Src%iStg3D(0:3,Src%nStg3D),STAT=ios )
  IF( ios /= 0 )THEN
    error%Inform = 'Error allocating grid stagger arrays for 3d fields'
    GOTO 9999
  END IF

  DO i = 1,Src%nStg3D

    IF( lformat )THEN
      CALL get_next_data( unit,line,nch,kwrd,n_arg,c_arg,MAXN,lerr )
      IF( lerr .OR. n_arg < 4 )ios = -1
      IF( ios == 0 )THEN
        DO j = 1,3
          READ(c_arg(1+j),*,IOSTAT=ios ) Src%iStg3D(j,i); IF( ios /= 0 )EXIT
        END DO
      END IF
    ELSE
      READ(unit,IOSTAT=ios) c_arg(1)(1:8),(Src%iStg3D(j,i),j=1,3)  !8 characters for variable name
    END IF
    IF( ios /= 0 )THEN
      error%Inform = 'Error reading staggered grid record'
      WRITE(error%Action,'(A,I2)',IOSTAT=ios) '3D variable ',i
      GOTO 9999
    END IF

!------ Match 3d variable name and set appropriate ID

    SELECT CASE( TRIM(ADJUSTL(c_arg(1)(1:8))) )
      CASE( 'U' )
        Src%iStg3d(0,i) = GVP_U
      CASE( 'V' )
        Src%iStg3d(0,i) = GVP_V
      CASE( 'W' )
        Src%iStg3d(0,i) = GVP_W
      CASE( 'T' )
        Src%iStg3d(0,i) = GVP_T
      CASE( 'TA' )
        Src%iStg3d(0,i) = GVP_T
      CASE( 'H','RH' )
        Src%iStg3d(0,i) = GVP_H
      CASE( 'P' )
        Src%iStg3d(0,i) = GVP_P
      CASE( 'Z','HEIGHT' )
        Src%iStg3d(0,i) = GVP_Z
      CASE( 'QCLD','CLD' )
        Src%iStg3d(0,i) = GVP_QCLD
      CASE( 'UUL' )
        Src%iStg3d(0,i) = GVP_UUL
      CASE( 'VVL' )
        Src%iStg3d(0,i) = GVP_VVL
      CASE( 'UVL' )
        Src%iStg3d(0,i) = GVP_UVL
      CASE( 'SHL' )
        Src%iStg3d(0,i) = GVP_SHL
      CASE DEFAULT
        Src%iStg3d(0,i) = GVP_NONE
    END SELECT

    nrec = nrec + 1

  END DO

END IF

IF( Src%nStg2D > 0 )THEN  !2d fields

  ALLOCATE( Src%iStg2D(0:2,Src%nStg2D),STAT=ios )
  IF( ios /= 0 )THEN
    error%Inform = 'Error allocating grid stagger arrays for 2D fields'
    GOTO 9999
  END IF

  DO i = 1,Src%nStg2D
    IF( lformat )THEN
      CALL get_next_data( unit,line,nch,kwrd,n_arg,c_arg,MAXN,lerr )
      IF( lerr .OR. n_arg < 4 )ios = -1
      IF( ios == 0 )THEN
        DO j = 1,2
          READ(c_arg(1+j),*,IOSTAT=ios ) Src%iStg2D(j,i); IF( ios /= 0 )EXIT
        END DO
      END IF
    ELSE
      READ(unit,IOSTAT=ios) c_arg(1)(1:8),(Src%iStg2D(j,i),j=1,2)
    END IF
    IF( ios /= 0 )THEN
      error%Inform = 'Error reading staggered grid record'
      WRITE(error%Inform,'(A,I2)',IOSTAT=ios) '2D variable ',i
      GOTO 9999
    END IF

!------ Match 2d variable name and set appropriate ID

    SELECT CASE( TRIM(ADJUSTL(c_arg(1))) )
      CASE( 'REL','TOPO' )
        Src%iStg2d(0,i) = GVP_TERRAIN
      CASE( 'ZI','PBL_HITE' )
        Src%iStg2d(0,i) = GVP_ZI
      CASE( 'HFLX','SFC_HTFX' )
        Src%iStg2d(0,i) = GVP_HFLX
      CASE( 'USTAR','USTR','UST' )
        Src%iStg2d(0,i) = GVP_UST
      CASE( 'CC' )
        Src%iStg2d(0,i) = GVP_CC
      CASE( 'ZRUF' )
        Src%iStg2d(0,i) = GVP_ZRUF
      CASE( 'ZRUF(T)' )
        Src%iStg2d(0,i) = GVP_ZRUFT
      CASE( 'ALBEDO','ALBED','ALBD' )
        Src%iStg2d(0,i) = GVP_ALBEDO
      CASE( 'BOWEN','BR' )
        Src%iStg2d(0,i) = GVP_BOWEN
      CASE( 'LU','LUSE','LANDUSE','LUC','LC','LCOVER' )
        Src%iStg2d(0,i) = GVP_LANDUSE
      CASE( 'CANOPY','HCANP','HCNP' )
        Src%iStg2d(0,i) = GVP_HCNP
      CASE( 'ALPHA','ALPH','ALPHC' )
        Src%iStg2d(0,i) = GVP_ALPHA
      CASE( 'PRATE','PRECIP' )
        Src%iStg2d(0,i) = GVP_PRATE
      CASE( 'LAI' )
        Src%iStg2d(0,i) = GVP_LAI
      CASE( 'ACCPR','PRECIPTOT','PRECIPACC' )
        Src%iStg2d(0,i) = GVP_ACCPR
      CASE DEFAULT
        Src%iStg2d(0,i) = GVP_NONE
    END SELECT

    nrec = nrec + 1

  END DO

END IF

MEDOCreadModelHeader = SWIMresult

CALL SWIMclearError()

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION MEDOCformatRecord( unit,lformat,lend )

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

INTEGER, INTENT( IN  ) :: unit
LOGICAL, INTENT( IN  ) :: lformat
LOGICAL, INTENT( OUT ) :: lend

INTEGER ios

CHARACTER(8) :: fflag

MEDOCformatRecord = SWIMfailure

error%Number  = RD_ERROR
error%Routine = 'MEDOCformatRecord'
error%Message = 'Error reading MEDOC file'
error%Inform  = 'Attempting to read format record'

lend = .FALSE.

IF( lformat )THEN

  READ(unit,'(A8)',IOSTAT=ios) fflag

! Check for end of file in all compilers
  IF( IS_IOSTAT_END(ios) )ios = -1

  IF( ios < 0 )THEN  !End of file
    lend = .TRUE.
    MEDOCformatRecord = SWIMresult
    CALL SWIMclearError()
    GOTO 9999
  ELSE IF( ios > 0 )THEN
    GOTO 9999  !Error
  END IF
  IF( LEN_TRIM(fflag) == 0 )THEN    !Blank lines not allowed
    error%Number  = IV_ERROR
    error%Routine = 'SWIMinitMEDOC'
    error%Message = 'Blank lines not allowed in MEDOC file'
    GOTO 9999
  END IF

  IF( fflag /= 'FFFFFFFF' )THEN
    error%Number  = IV_ERROR
    error%Routine = 'SWIMinitMEDOC'
    error%Message = 'Formatted file type specified'
    error%Inform  = 'Expecting first header record = FFFFFFFF'
    GOTO 9999
  END IF

ELSE

  READ(unit,IOSTAT=ios) fflag

! Check for end of file in all compilers
  IF( IS_IOSTAT_END(ios) )ios = -1

  IF( ios < 0 )THEN  !End of file
    lend = .TRUE.
    MEDOCformatRecord = SWIMresult
    CALL SWIMclearError()
    GOTO 9999
  ELSE IF( ios > 0 )THEN
    GOTO 9999  !Error
  END IF

  IF( fflag /= 'BBBBBBBB' )THEN
    error%Number  = IV_ERROR
    error%Routine = 'SWIMinitMEDOC'
    error%Message = 'Binary file type specified'
    error%Inform  = 'Expecting first header record = BBBBBBB'
    GOTO 9999
  END IF

END IF

MEDOCformatRecord = SWIMresult

CALL SWIMclearError()

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION MEDOCcodenameRecord( unit,lformat,codename,lstagger,NestSrc )

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

INTEGER,        INTENT( IN  ) :: unit
LOGICAL,        INTENT( IN  ) :: lformat
CHARACTER(8),   INTENT( OUT ) :: codename
LOGICAL,        INTENT( OUT ) :: lstagger
CHARACTER(PATH_MAXLENGTH), &
                INTENT( OUT ) :: NestSrc

INTEGER ios

CHARACTER(8) cdum

MEDOCcodenameRecord = SWIMfailure

error%Number  = RD_ERROR
error%Routine = 'MEDOCcodenameRecord'
error%Message = 'Error reading MEDOC file'
error%Inform  = 'Unable to read CODENAME record'

lstagger = .FALSE.

NestSrc = ''

IF( lformat )THEN
  READ(unit,'(A8,1X,A8,1X,A)',IOSTAT=ios) codename,cdum,NestSrc
  IF( ios /= 0 )GOTO 9999
  IF( cdum(1:1) == 'T' )lstagger = .TRUE.
ELSE
#if !defined(LINUX)
  READ(unit,IOSTAT=ios) codename,lstagger !,NestSrc  30-Nov-2018 DSH: EPRI Windows version having problems with this line (binary only)
#else
  READ(unit,IOSTAT=ios) codename,lstagger,NestSrc
#endif
  IF( ios /= 0 )THEN
    NestSrc = ' '
    BACKSPACE(unit,IOSTAT=ios)
    READ(unit,IOSTAT=ios) codename,lstagger
    IF( ios /= 0 )THEN
      lstagger = .FALSE.
      BACKSPACE(unit,IOSTAT=ios)
      READ(unit,IOSTAT=ios) codename
      IF( ios /= 0 )GOTO 9999
    END IF
  END IF
END IF

MEDOCcodenameRecord = SWIMresult

CALL SWIMclearError()

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION MEDOCclimoTimeOffset( unit,lformat,Src )

!------ Adjust gridded source time offset for MEDOC climatology files
!       Note: reads time record and then backspaces

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

INTEGER,         INTENT( IN    ) :: unit
LOGICAL,         INTENT( IN    ) :: lformat
TYPE( GridSrc ), INTENT( INOUT ) :: Src

INTEGER iday, ios

MEDOCclimoTimeOffset = SWIMfailure

IF( lformat )THEN
  READ(unit,'(I12)',IOSTAT=ios) iday
ELSE
  READ(unit,IOSTAT=ios) iday
END IF
IF( ios /= 0 )THEN
  error%Number  = RD_ERROR
  error%Routine = 'MEDOCclimoTimeOffset'
  error%Message = 'Error reading MEDOC file'
  error%Inform  = 'Attempting to read day for climatology'
  GOTO 9999
END IF

Src%timeOffset = Src%timeOffset + (iday-Prj%dayStart)*86400.

BACKSPACE(unit)

MEDOCclimoTimeOffset = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION MEDOCtimeRecord( unit,lformat,Src,tMEDOC,lWriteLog )

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

INTEGER,         INTENT( IN  ) :: unit
LOGICAL,         INTENT( IN  ) :: lformat
TYPE( GridSrc ), INTENT( IN  ) :: Src
REAL,            INTENT( OUT ) :: tMEDOC
LOGICAL,         INTENT( IN  ) :: lWriteLog

INTEGER irv, ios, iday, imonth, iyear, ihour, imin, isec

CHARACTER(128) :: string

INTEGER, EXTERNAL :: SWIMaddLogMessage
REAL,    EXTERNAL :: GetTimeMet

MEDOCtimeRecord = SWIMfailure

error%Number  = RD_ERROR
error%Routine = 'MEDOCtimeRecord'
error%Message = 'Error reading MEDOC file'
error%Inform  = 'Attempting to read day,month,year,time'

IF( lformat )THEN
  READ(unit,'(6(I12,1X))',IOSTAT=ios) iday,imonth,iyear,ihour,imin,isec
ELSE
  READ(unit,IOSTAT=ios) iday,imonth,iyear,ihour,imin,isec
END IF
IF( ios /= 0 )GOTO 9999

IF( lWriteLog )THEN
  WRITE(string,"('MEDOC time: ',6(I12,1X))",IOSTAT=ios) iday,imonth,iyear,ihour,imin,isec
  irv = SWIMaddLogMessage( string )
  IF( irv /= SWIMsuccess )GOTO 9999
END IF

IF( iday  == NOT_SET_MED_I .OR. &
    ihour == NOT_SET_MED_I .OR. &
    imin  == NOT_SET_MED_I .OR. &
    isec  == NOT_SET_MED_I )THEN
  tMEDOC = NOT_SET_R
  MEDOCtimeRecord = SWIMresult
  GOTO 9999
END IF

IF( Prj%yearStart /= NOT_SET_I .AND. &
           (imonth == NOT_SET_MED_I .OR. iyear == NOT_SET_MED_I) )THEN
  error%Number  = IV_ERROR
  error%Routine = 'MEDOCtimeRecord'
  error%Message = 'MEDOC file does not have year or month'
  error%Inform  = 'Cannot run in year-month-day time format'
  GOTO 9999
END IF

IF( imonth == NOT_SET_MED_I )imonth = NOT_SET_I
IF( iyear  == NOT_SET_MED_I )iyear  = NOT_SET_I

tMEDOC = GetTimeMet( iday,imonth,iyear,ihour,imin,isec,Src%timeOffset )

MEDOCtimeRecord = SWIMresult

CALL SWIMclearError()

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION MEDOCdimensionRecord( unit,lformat, &
                                      imax,jmax,kmax,ndum,nvar3d,nvar2d )

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

INTEGER, INTENT( IN  ) :: unit
LOGICAL, INTENT( IN  ) :: lformat
INTEGER, INTENT( OUT ) :: imax, jmax, kmax, ndum, nvar3d, nvar2d

INTEGER ios

MEDOCdimensionRecord = SWIMfailure

IF( lformat )THEN
  READ(unit,'(6(I12,1X))',IOSTAT=ios) imax,jmax,kmax,ndum,nvar3d,nvar2d
ELSE
  READ(unit,IOSTAT=ios) imax,jmax,kmax,ndum,nvar3d,nvar2d
END IF
IF( ios /= 0 )THEN
  error%Number  = RD_ERROR
  error%Routine = 'MEDOCdimensionRecord'
  error%Message = 'Error reading MEDOC file'
  error%Inform  = 'Attempting to read grid dimensions and no. of fields'
  GOTO 9999
END IF

MEDOCdimensionRecord = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION MEDOCgridRecord( unit,lformat,z,kmax,dx,dy,x0,y0,xlat0,xlon0,zbtop_med )

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

INTEGER, INTENT( IN  )      :: unit
LOGICAL, INTENT( IN  )      :: lformat
REAL, DIMENSION(:), POINTER :: z
INTEGER, INTENT( IN  )      :: kmax
REAL,    INTENT( OUT )      :: dx, dy, x0, y0, xlat0, xlon0, zbtop_med

INTEGER idum, n, k, ios
REAL    dum

MEDOCgridRecord = SWIMfailure

error%Number  = RD_ERROR
error%Routine = 'MEDOCgridRecord'
error%Message = 'Error reading MEDOC file'
error%Inform  = 'Attempting to read vertical grid,dx,dy,origin,etc.'

IF( lformat )THEN
  READ(unit,'(6(I12,1X))',IOSTAT=ios) (idum,n=1,6)
  IF( ios == 0 )READ(unit,'(6(I12,1X))',IOSTAT=ios) (idum,n=1,3)
  IF( ios == 0 )READ(unit,'(6(F12.4,1X))',IOSTAT=ios) (z(k),k=1,kmax),dx,dy, &
                               x0,y0,xlat0,xlon0,(dum,n=1,4),zbtop_med
ELSE
  READ(unit,IOSTAT=ios) (idum,n=1,6)
  IF( ios == 0 )READ(unit,IOSTAT=ios) (idum,n=1,3)
  IF( ios == 0 )READ(unit,IOSTAT=ios) (z(k),k=1,kmax),dx,dy, &
                          x0,y0,xlat0,xlon0,(dum,n=1,4),zbtop_med
END IF
IF( ios /= 0 )GOTO 9999

IF( xlat0 == NOT_SET_MED_R .AND. xlon0 /= NOT_SET_MED_R )THEN
  xlat0 = NOT_SET_R
ELSE IF( xlon0 == NOT_SET_MED_R )THEN
  xlon0 = NOT_SET_R
  xlat0 = NOT_SET_R
END IF

IF( x0 == NOT_SET_MED_R .OR. y0 == NOT_SET_MED_R )THEN
  x0 = NOT_SET_R
  y0 = NOT_SET_R
END IF

MEDOCgridRecord = SWIMresult

CALL SWIMclearError()

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION MEDOCvarnameRecord( unit,lformat,Var3d,Var2d,ndum )

USE SWIM_fi
USE SWIMparam_fd
USE Char8Array_fd

IMPLICIT NONE

INTEGER, INTENT( IN  )              :: unit
LOGICAL, INTENT( IN  )              :: lformat
INTEGER, INTENT( IN  )              :: ndum
TYPE( Char8Array ), INTENT( INOUT ) :: Var3d
TYPE( Char8Array ), INTENT( INOUT ) :: Var2d

CHARACTER(8) cdum

INTEGER nvar3d, nvar2d, n, ios
REAL    dum

MEDOCvarnameRecord = SWIMfailure

error%Number  = RD_ERROR
error%Routine = 'MEDOCvarnameRecord'
error%Message = 'Error reading MEDOC file'
error%Inform  = 'Attempting to read field names'

nvar3d = Var3d%n
nvar2d = Var2d%n

ALLOCATE( Var3d%string(nvar3d),Var3d%unit(nvar3d),STAT=ios )
IF( ios == 0 )ALLOCATE( Var2d%string(nvar2d),Var2d%unit(nvar2d),STAT=ios )
IF( ios /= 0 )THEN
  error%Inform = 'Error allocating variable name and unit arrays'
  GOTO 9999
END IF

IF( lformat )THEN
  IF( ndum > 0 )THEN
    READ(unit,'(6(A8,1X))',IOSTAT=ios) (cdum,n=1,ndum), &
                (Var3d%string(n),n=1,nvar3d),(Var3d%unit(n),n=1,nvar3d), &
                (Var2d%string(n),n=1,nvar2d),(Var2d%unit(n),n=1,nvar2d)
    IF( ios == 0 )READ(unit,'(6(F12.4,1X))',IOSTAT=ios) (dum,dum,dum,n=1,ndum)
  ELSE
    READ(unit,'(6(A8,1X))',IOSTAT=ios) &
                (Var3d%string(n),n=1,nvar3d),(Var3d%unit(n),n=1,nvar3d), &
                (Var2d%string(n),n=1,nvar2d),(Var2d%unit(n),n=1,nvar2d)
  END IF
ELSE
  IF( ndum > 0 )THEN
    READ(unit,IOSTAT=ios) (cdum,n=1,ndum), &
                (Var3d%string(n),n=1,nvar3d),(Var3d%unit(n),n=1,nvar3d), &
                (Var2d%string(n),n=1,nvar2d),(Var2d%unit(n),n=1,nvar2d)
   IF( ios == 0 )READ(unit,IOSTAT=ios) (dum,dum,dum,n=1,ndum)
  ELSE
    READ(unit,IOSTAT=ios) &
                (Var3d%string(n),n=1,nvar3d),(Var3d%unit(n),n=1,nvar3d), &
                (Var2d%string(n),n=1,nvar2d),(Var2d%unit(n),n=1,nvar2d)
  END IF
END IF
IF( ios /= 0 )GOTO 9999

MEDOCvarnameRecord = SWIMresult

CALL SWIMclearError()

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SetMEDOCvar3d( Var3d,Src )

USE SWIM_FI
USE SWIMparam_fd
USE Char8Array_fd

IMPLICIT NONE

TYPE( Char8Array ), INTENT( INOUT ) :: Var3d
TYPE( GridSrc ),    INTENT( INOUT ) :: Src

INTEGER alloc_stat, irv, i, nvar3d
LOGICAL lU, lV

INTEGER, EXTERNAL :: SWIMaddLogMessage, SetVelCnv

SetMEDOCvar3d = SWIMfailure

nvar3d     = Var3d%n
Src%nVar3d = nvar3d
lU         = .FALSE.
lV         = .FALSE.

ALLOCATE( Src%Var3dID(nvar3d),STAT=alloc_stat )
IF( alloc_stat == 0 )ALLOCATE( Src%Conv3d(nvar3d),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'SetMEDOCvar3d'
  error%Message = 'Error allocating variable ID and conversion arrays'
  GOTO 9999
END IF

DO i = 1,nvar3d

  CALL cupper( Var3d%unit(i) )
  IF( LEN_TRIM(Var3d%unit(i)) == 0 )Var3d%unit(i) = 'NONE'

  SELECT CASE( TRIM(Var3d%string(i)) )

    CASE( 'U' )
      Src%Var3dID(i) = GVP_U
      irv = SetVelCnv( Var3d%unit(i),Src%Conv3d(i) )
      IF( irv /= SWIMsuccess )THEN
        IF( .NOT.BTEST(Src%type,GSB_OLDMMIF) )THEN
          error%Number  = IV_ERROR
          error%Routine = 'SetMEDOCvar3d'
          error%Message = 'Invalid units for MEDOC '//TRIM(Var3d%string(i))//' field: '//TRIM(Var3d%unit(i))
          error%Inform  = 'Valid units are M/S, KNOTS, MPH, KPH'
          GOTO 9999
        END IF
        Src%Conv3d(i) = 1. !Set unrecognized units to default (m/s)
        CALL SWIMclearError()
      END IF
      lU = .TRUE.

    CASE( 'V' )
      Src%Var3dID(i) = GVP_V
      irv = SetVelCnv( Var3d%unit(i),Src%Conv3d(i) )
      IF( irv /= SWIMsuccess )THEN
        IF( .NOT.BTEST(Src%type,GSB_OLDMMIF) )THEN
          error%Number  = IV_ERROR
          error%Routine = 'SetMEDOCvar3d'
          error%Message = 'Invalid units for MEDOC '//TRIM(Var3d%string(i))//' field: '//TRIM(Var3d%unit(i))
          error%Inform  = 'Valid units are M/S, KNOTS, MPH, KPH'
          GOTO 9999
        END IF
        Src%Conv3d(i) = 1.
        CALL SWIMclearError()
      END IF
      lV = .TRUE.

    CASE( 'W' )
      Src%Var3dID(i) = GVP_W
      irv = SetVelCnv( Var3d%unit(i),Src%Conv3d(i) )
      IF( irv /= SWIMsuccess )THEN
        IF( .NOT.BTEST(Src%type,GSB_OLDMMIF) )THEN
          error%Number  = IV_ERROR
          error%Routine = 'SetMEDOCvar3d'
          error%Message = 'Invalid units for MEDOC '//TRIM(Var3d%string(i))//' field: '//TRIM(Var3d%unit(i))
          error%Inform  = 'Valid units are M/S, KNOTS, MPH, KPH'
          GOTO 9999
        END IF
        Src%Conv3d(i) = 1.
        CALL SWIMclearError()
      END IF

    CASE( 'T' )
      Src%Var3dID(i) = GVP_T
      Src%Conv3d(i)  = FLOAT(OBP_KELVIN)  !Only degrees Kelvin accepted
      IF( TRIM(Var3d%unit(i)) /= 'K' .AND. .NOT.BTEST(Src%type,GSB_OLDMMIF) )THEN
        error%Number  = IV_ERROR
        error%Routine = 'SetMEDOCvar3d'
        error%Message = 'Invalid units for MEDOC '//TRIM(Var3d%string(i))//' field: '//TRIM(Var3d%unit(i))
        error%Inform  = 'Valid units are K'
        GOTO 9999
      END IF

    CASE( 'TA' )
      Src%Var3dID(i) = GVP_T
      SELECT CASE( TRIM(Var3d%unit(i)) )
        CASE( 'C' )
          Src%Conv3d(i) = FLOAT(OBP_CELSIUS)
        CASE( 'K' )
          Src%Conv3d(i) = FLOAT(OBP_KELVIN)
        CASE( 'F' )
          Src%Conv3d(i) = FLOAT(OBP_FAHRENHEIT)
        CASE DEFAULT
          Src%Conv3d(i) = FLOAT(OBP_KELVIN)
          IF( .NOT.BTEST(Src%type,GSB_OLDMMIF) )THEN
            error%Number  = IV_ERROR
            error%Routine = 'SetMEDOCvar3d'
            error%Message = 'Invalid units for MEDOC '//TRIM(Var3d%string(i))//' field: '//TRIM(Var3d%unit(i))
            error%Inform  = 'Valid units are K, C or F'
            GOTO 9999
          END IF
      END SELECT
      Src%type = IBSET(Src%type,GSB_ABSTEMP)

    CASE( 'H' )
      Src%Var3dID(i) = GVP_H
      Src%type = IBSET(Src%type,GSB_ABSHUMID)
      SELECT CASE( TRIM(Var3d%unit(i)) )
        CASE( 'GM/GM','G/G','KG/KG' )
          Src%Conv3d(i) = 1.
        CASE( 'GM/KG','G/KG' )
          Src%Conv3d(i) = 1.E-3
        CASE( '%','PERCENT' )
          Src%Conv3d(i) = -1.
          Src%type = IBCLR(Src%type,GSB_ABSHUMID)
        CASE DEFAULT
          Src%Conv3d(i) = 1.
          IF( .NOT.BTEST(Src%type,GSB_OLDMMIF) )THEN
            error%Number  = IV_ERROR
            error%Routine = 'SetMEDOCvar3d'
            error%Message = 'Invalid units for MEDOC '//TRIM(Var3d%string(i))//' field: '//TRIM(Var3d%unit(i))
            error%Inform  = 'Valid units are G/G, G/KG or %'
            GOTO 9999
          END IF
      END SELECT

    CASE( 'RH' )
      Src%Var3dID(i) = GVP_H
      SELECT CASE( TRIM(Var3d%unit(i)) )
        CASE( '%','PERCENT' )
          Src%Conv3d(i) = -1.
        CASE DEFAULT
          error%Number  = IV_ERROR
          error%Routine = 'SetMEDOCvar3d'
          error%Message = 'RH field units must be percent'
          GOTO 9999
      END SELECT

    CASE( 'P' )
      Src%Var3dID(i) = GVP_P
      IF( TRIM(Var3d%unit(i)) /= 'MB' .AND. .NOT.BTEST(Src%type,GSB_OLDMMIF) )THEN
        error%Number  = IV_ERROR
        error%Routine = 'SetMEDOCvar3d'
        error%Message = 'Invalid units for MEDOC '//TRIM(Var3d%string(i))//' field: '//TRIM(Var3d%unit(i))
        error%Inform  = 'Valid units are MB'
        GOTO 9999
      END IF

    CASE( 'QCLD','CLD' )
      Src%Var3dID(i) = GVP_QCLD
      SELECT CASE( TRIM(Var3d%unit(i)) )
        CASE( 'G/M3','G/M^3' )
          Src%Conv3d(i) = 1.
        CASE( 'GM/GM','G/G','KG/KG' )
          Src%Conv3d(i) = 1.E+3
          Src%type = IBSET(Src%type,GSB_QCLDMIX)
        CASE( 'GM/KG','G/KG' )
          Src%Conv3d(i) = 1.
          Src%type = IBSET(Src%type,GSB_QCLDMIX)
        CASE DEFAULT
          Src%Conv3d(i) = 1.  ! Assume G/M^3
          IF( BTEST(Src%type,GSB_OLDMMIF) )THEN
            Src%Conv3d(i) = 1.E+3  ! Assume G/G
          ELSE
            error%Number  = IV_ERROR
            error%Routine = 'SetMEDOCvar3d'
            error%Message = 'Invalid units for MEDOC '//TRIM(Var3d%string(i))//' field: '//TRIM(Var3d%unit(i))
            error%Inform  = 'Valid units are G/M3, G/G or G/KG'
            GOTO 9999
          END IF
      END SELECT

    CASE( 'Z','HEIGHT' )
      IF( BTEST(Src%type,GSB_ZMSL) .OR. BTEST(Src%type,GSB_ZAGL) )THEN
        Src%Var3dID(i) = GVP_Z
        SELECT CASE( TRIM(Var3d%unit(i)) )
          CASE( 'KM' )
            Src%Conv3d(i) = 1.E3
          CASE DEFAULT
            Src%Conv3d(i) = 1.
            IF( TRIM(Var3d%unit(i)) /= 'M' .AND. .NOT.BTEST(Src%type,GSB_OLDMMIF) )THEN
              error%Number  = IV_ERROR
              error%Routine = 'SetMEDOCvar3d'
              error%Message = 'Invalid units for MEDOC '//TRIM(Var3d%string(i))//' field: '//TRIM(Var3d%unit(i))
              error%Inform  = 'Valid units are M or KM'
              GOTO 9999
            END IF
        END SELECT
      ELSE
        Src%Var3dID(i) = GVP_NONE
      END IF

    CASE( 'UUL' )
      IF( Prj%LSVType == LVP_MET .OR. Prj%LSVType == LVP_OPER )THEN
        Src%Var3dID(i) = GVP_UUL
        Prj%LSVType    = LVP_MET
        IF( TRIM(Var3d%unit(i)) /= 'M2/S2' .AND. .NOT.BTEST(Src%type,GSB_OLDMMIF) )THEN
          error%Number  = IV_ERROR
          error%Routine = 'SetMEDOCvar3d'
          error%Message = 'Invalid units for MEDOC '//TRIM(Var3d%string(i))//' field: '//TRIM(Var3d%unit(i))
          error%Inform  = 'Valid units are M2/S2'
          GOTO 9999
        END IF
      ELSE
        Src%Var3dID(i)  = GVP_NONE
      END IF

    CASE( 'VVL' )
      IF( Prj%LSVType == LVP_MET .OR. Prj%LSVType == LVP_OPER )THEN
        Src%Var3dID(i) = GVP_VVL
        Prj%LSVType    = LVP_MET
        IF( TRIM(Var3d%unit(i)) /= 'M2/S2' .AND. .NOT.BTEST(Src%type,GSB_OLDMMIF) )THEN
          error%Number  = IV_ERROR
          error%Routine = 'SetMEDOCvar3d'
          error%Message = 'Invalid units for MEDOC '//TRIM(Var3d%string(i))//' field: '//TRIM(Var3d%unit(i))
          error%Inform  = 'Valid units are M2/S2'
          GOTO 9999
        END IF
      ELSE
        Src%Var3dID(i)  = GVP_NONE
      END IF

    CASE( 'UVL' )
      IF( Prj%LSVType == LVP_MET .OR. Prj%LSVType == LVP_OPER )THEN
        Src%Var3dID(i) = GVP_UVL
        Prj%LSVType    = LVP_MET
        IF( TRIM(Var3d%unit(i)) /= 'M2/S2' .AND. .NOT.BTEST(Src%type,GSB_OLDMMIF) )THEN
          error%Number  = IV_ERROR
          error%Routine = 'SetMEDOCvar3d'
          error%Message = 'Invalid units for MEDOC '//TRIM(Var3d%string(i))//' field: '//TRIM(Var3d%unit(i))
          error%Inform  = 'Valid units are M2/S2'
          GOTO 9999
        END IF
      ELSE
        Src%Var3dID(i)  = GVP_NONE
      END IF

    CASE( 'SHL' )
      IF( Prj%LSVType == LVP_MET .OR. Prj%LSVType == LVP_OPER )THEN
        Src%Var3dID(i) = GVP_SHL
        Prj%LSVType    = LVP_MET
        IF( TRIM(Var3d%unit(i)) /= 'M' .AND. .NOT.BTEST(Src%type,GSB_OLDMMIF) )THEN
          error%Number  = IV_ERROR
          error%Routine = 'SetMEDOCvar3d'
          error%Message = 'Invalid units for MEDOC '//TRIM(Var3d%string(i))//' field: '//TRIM(Var3d%unit(i))
          error%Inform  = 'Valid units are M'
          GOTO 9999
        END IF
      ELSE
        Src%Var3dID(i)  = GVP_NONE
      END IF

    CASE DEFAULT
      Src%Var3dID(i) = GVP_NONE

  END SELECT

  IF( Src%Var3dID(i) == GVP_NONE )THEN
    irv = SWIMaddLogMessage( '*** Unused MEDOC 3d variable: '// &
                             TRIM(Var3d%string(i)) )
  ELSE
    irv = SWIMaddLogMessage( 'Recognized MEDOC 3d variable: '// &
                             TRIM(Var3d%string(i)) )
  END IF
  IF( irv /= SWIMsuccess )GOTO 9999

END DO

IF( .NOT.(lU.AND.lV) )THEN
  error%Number  = IV_ERROR
  error%Routine = 'SetMEDOCvar3d'
  error%Message = 'Invalid MEDOC file'
  IF( .NOT.lU )THEN
    error%Inform = 'U velocity component is missing'
    IF( .NOT.lV )error%Action = 'V velocity component is missing'
  ELSE
    error%Inform = 'V velocity component is missing'
  END IF
  GOTO 9999
END IF
SetMEDOCvar3d = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SetMEDOCvar2d( Var2d,Src )

USE SWIM_FI
USE SWIMparam_fd
USE Char8Array_fd
USE constants_fd

IMPLICIT NONE

TYPE( Char8Array ), INTENT( INOUT ) :: Var2d
TYPE( GridSrc ),    INTENT( INOUT ) :: Src

INTEGER alloc_stat, irv, i, nvar2d

INTEGER, EXTERNAL :: SWIMaddLogMessage

SetMEDOCvar2d = SWIMfailure

nvar2d = Var2d%n

Src%nVar2d = nvar2d
ALLOCATE( Src%Var2dID(nvar2d),Src%Conv2d(nvar2d),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'SetMEDOCvar2d'
  error%Message = 'Error allocating variable ID array'
  GOTO 9999
END IF

Src%Conv2d = 0.

DO i = 1,nvar2d

  CALL cupper( Var2d%string(i) )
  IF( LEN_TRIM(Var2d%unit(i)) == 0 )Var2d%unit(i) = 'NONE'

  SELECT CASE( TRIM(Var2d%string(i)) )

    CASE( 'REL','TOPO' )
      Src%Var2dID(i) = GVP_TERRAIN
      IF( TRIM(Var2d%unit(i)) /= 'M' .AND. .NOT.BTEST(Src%type,GSB_OLDMMIF) )THEN
        error%Number  = IV_ERROR
        error%Routine = 'SetMEDOCvar2d'
        error%Message = 'Invalid units for MEDOC '//TRIM(Var2d%string(i))//' field: '//TRIM(Var2d%unit(i))
        error%Inform  = 'Valid units are M'
        GOTO 9999
      END IF

    CASE( 'ZI','PBL_HITE' )
      IF( Prj%BL%type == BLP_OPER .OR. Prj%BL%type == BLP_MET )THEN
        Src%Var2dID(i) = GVP_ZI
        IF( TRIM(Var2d%unit(i)) /= 'M' .AND. .NOT.BTEST(Src%type,GSB_OLDMMIF) )THEN
          error%Number  = IV_ERROR
          error%Routine = 'SetMEDOCvar2d'
          error%Message = 'Invalid units for MEDOC '//TRIM(Var2d%string(i))//' field: '//TRIM(Var2d%unit(i))
          error%Inform  = 'Valid units are M'
          GOTO 9999
        END IF
      ELSE
        Src%Var2dID(i) = GVP_NONE
      END IF

    CASE( 'HFLX','SFC_HTFX' )
      IF( Prj%BL%type == BLP_OPER .OR. Prj%BL%type == BLP_MET )THEN
        Src%Var2dID(i) = GVP_HFLX
        IF( TRIM(Var2d%unit(i)) /= 'W/M2' .AND. .NOT.BTEST(Src%type,GSB_OLDMMIF) )THEN
          error%Number  = IV_ERROR
          error%Routine = 'SetMEDOCvar2d'
          error%Message = 'Invalid units for MEDOC '//TRIM(Var2d%string(i))//' field: '//TRIM(Var2d%unit(i))
          error%Inform  = 'Valid units are W/M2'
          GOTO 9999
        END IF
      ELSE
        Src%Var2dID(i) = GVP_NONE
      END IF

    CASE( 'USTAR','USTR','UST' )
      IF( Prj%BL%type == BLP_OPER .OR. Prj%BL%type == BLP_MET )THEN
        Src%Var2dID(i) = GVP_UST
        IF( TRIM(Var2d%unit(i)) /= 'M/S' .AND. .NOT.BTEST(Src%type,GSB_OLDMMIF) )THEN
          error%Number  = IV_ERROR
          error%Routine = 'SetMEDOCvar2d'
          error%Message = 'Invalid units for MEDOC '//TRIM(Var2d%string(i))//' field: '//TRIM(Var2d%unit(i))
          error%Inform  = 'Valid units are M/S'
          GOTO 9999
        END IF
      ELSE
        Src%Var2dID(i) = GVP_NONE
      END IF

    CASE( 'CC' )
      Src%Var2dID(i) = GVP_CC
        IF( (TRIM(Var2d%unit(i)) /= 'FRACTION' .AND. TRIM(Var2d%unit(i)) /= 'NONE') &
                                 .AND. .NOT.BTEST(Src%type,GSB_OLDMMIF) )THEN
          error%Number  = IV_ERROR
          error%Routine = 'SetMEDOCvar2d'
          error%Message = 'Invalid units for MEDOC '//TRIM(Var2d%string(i))//' field: '//TRIM(Var2d%unit(i))
          error%Inform  = 'Valid units are FRACTION (or blank)'
          GOTO 9999
        END IF

    CASE( 'ZRUF' )
      Src%Var2dID(i) = GVP_ZRUF
      IF( TRIM(Var2d%unit(i)) /= 'M' .AND. .NOT.BTEST(Src%type,GSB_OLDMMIF) )THEN
        error%Number  = IV_ERROR
        error%Routine = 'SetMEDOCvar2d'
        error%Message = 'Invalid units for MEDOC '//TRIM(Var2d%string(i))//' field: '//TRIM(Var2d%unit(i))
        error%Inform  = 'Valid units are M'
        GOTO 9999
      END IF

    CASE( 'ZRUF(T)' )
      Src%Var2dID(i) = GVP_ZRUFT
      IF( TRIM(Var2d%unit(i)) /= 'M' .AND. .NOT.BTEST(Src%type,GSB_OLDMMIF) )THEN
        error%Number  = IV_ERROR
        error%Routine = 'SetMEDOCvar2d'
        error%Message = 'Invalid units for MEDOC '//TRIM(Var2d%string(i))//' field: '//TRIM(Var2d%unit(i))
        error%Inform  = 'Valid units are M'
        GOTO 9999
      END IF

    CASE( 'ALBEDO','ALBED','ALBD' )
      Src%Var2dID(i) = GVP_ALBEDO
      IF( (TRIM(Var2d%unit(i)) /= 'DIMLESS' .AND. TRIM(Var2d%unit(i)) /= 'NONE') &
                                .AND. .NOT.BTEST(Src%type,GSB_OLDMMIF) )THEN
        error%Number  = IV_ERROR
        error%Routine = 'SetMEDOCvar2d'
        error%Message = 'Invalid units for MEDOC '//TRIM(Var2d%string(i))//' field: '//TRIM(Var2d%unit(i))
        error%Inform  = 'Valid units are DIMLESS (or blank)'
        GOTO 9999
      END IF

    CASE( 'BOWEN','BR' )
      Src%Var2dID(i) = GVP_BOWEN
      IF( (TRIM(Var2d%unit(i)) /= 'DIMLESS' .AND. TRIM(Var2d%unit(i)) /= 'NONE') &
                                .AND. .NOT.BTEST(Src%type,GSB_OLDMMIF) )THEN
        error%Number  = IV_ERROR
        error%Routine = 'SetMEDOCvar2d'
        error%Message = 'Invalid units for MEDOC '//TRIM(Var2d%string(i))//' field: '//TRIM(Var2d%unit(i))
        error%Inform  = 'Valid units are DIMLESS (or blank)'
        GOTO 9999
      END IF

    CASE( 'LU','LUSE','LANDUSE','LUC','LC','LCOVER' )
      Src%Var2dID(i) = GVP_LANDUSE
      IF( TRIM(Var2d%unit(i)) /= 'NONE' .AND. .NOT.BTEST(Src%type,GSB_OLDMMIF) )THEN
        error%Number  = IV_ERROR
        error%Routine = 'SetMEDOCvar2d'
        error%Message = 'Invalid units for MEDOC '//TRIM(Var2d%string(i))//' field: '//TRIM(Var2d%unit(i))
        error%Inform  = 'Valid units are DIMLESS (or blank)'
        GOTO 9999
      END IF

    CASE( 'CANOPY','HCANP','HCNP' )
      Src%Var2dID(i) = GVP_HCNP
      IF( TRIM(Var2d%unit(i)) /= 'M' .AND. .NOT.BTEST(Src%type,GSB_OLDMMIF) )THEN
        error%Number  = IV_ERROR
        error%Routine = 'SetMEDOCvar2d'
        error%Message = 'Invalid units for MEDOC '//TRIM(Var2d%string(i))//' field: '//TRIM(Var2d%unit(i))
        error%Inform  = 'Valid units are M'
        GOTO 9999
      END IF

    CASE( 'PRATE','PRECIP' )
      IF( Prj%BL%pr_type == -1. )THEN
        Src%Var2dID(i) = GVP_PRATE
        SELECT CASE( TRIM( Var2d%unit(i)) )
          CASE( 'MM/HR' )
            Src%Conv2d(i) = 1.
          CASE( 'KG/M2S','KG/M2-S','KG/M2/S' )
            Src%Conv2d(i) = 1000./RHO_WATER * 3600.
          CASE( 'KG/M2HR','KG/M2-HR','KG/M2/HR' )
            Src%Conv2d(i) = 1000./RHO_WATER
          CASE DEFAULT
            Src%Conv2d(i) = 1
            IF( .NOT.BTEST(Src%type,GSB_OLDMMIF) )THEN
              error%Number  = IV_ERROR
              error%Routine = 'SetMEDOCvar2d'
              error%Message = 'Invalid units for MEDOC '//TRIM(Var2d%string(i))//' field: '//TRIM(Var2d%unit(i))
              error%Inform  = 'Valid units are MM/HR, KG/M2/S or KG/M2/HR'
              GOTO 9999
            END IF
        END SELECT
      ELSE
        Src%Var2dID(i) = GVP_NONE
      END IF

    CASE( 'ACCPR','PRECIPTOT','PRECIPACC' )  !
      IF( Prj%BL%pr_type == -1. )THEN
        Src%Var2dID(i) = GVP_ACCPR
        SELECT CASE( TRIM( Var2d%unit(i)) )
          CASE( 'MM' )
            Src%Conv3d(i) = 1.
          CASE( 'KG/M2' )
            Src%Conv2d(i) = 1000./RHO_WATER
          CASE DEFAULT
            Src%Conv2d(i) = 1
            IF( .NOT.BTEST(Src%type,GSB_OLDMMIF) )THEN
              error%Number  = IV_ERROR
              error%Routine = 'SetMEDOCvar2d'
              error%Message = 'Invalid units for MEDOC '//TRIM(Var2d%string(i))//' field: '//TRIM(Var2d%unit(i))
              error%Inform  = 'Valid units are MM or KG/M2'
              GOTO 9999
            END IF
        END SELECT
      ELSE
        Src%Var2dID(i) = GVP_NONE
      END IF

    CASE( 'ALPHA','ALPH','ALPHC' )
      IF( (TRIM(Var2d%unit(i)) /= 'DIMLESS' .AND. TRIM(Var2d%unit(i)) /= 'NONE') &
                                .AND. .NOT.BTEST(Src%type,GSB_OLDMMIF) )THEN
        error%Number  = IV_ERROR
        error%Routine = 'SetMEDOCvar2d'
        error%Message = 'Invalid units for MEDOC '//TRIM(Var2d%string(i))//' field: '//TRIM(Var2d%unit(i))
        error%Inform  = 'Valid units are DIMLESS (or blank)'
        GOTO 9999
      END IF
      Src%Var2dID(i) = GVP_ALPHA

    CASE( 'LAI' )
      Src%Var2dID(i) = GVP_LAI
      IF( (TRIM(Var2d%unit(i)) /= 'DIMLESS' .AND. TRIM(Var2d%unit(i)) /= 'NONE') &
                                .AND. .NOT.BTEST(Src%type,GSB_OLDMMIF) )THEN
        error%Number  = IV_ERROR
        error%Routine = 'SetMEDOCvar2d'
        error%Message = 'Invalid units for MEDOC '//TRIM(Var2d%string(i))//' field: '//TRIM(Var2d%unit(i))
        error%Inform  = 'Valid units are DIMLESS (or blank)'
        GOTO 9999
      END IF

    CASE DEFAULT
      Src%Var2dID(i) = GVP_NONE

  END SELECT

  IF( Src%Var2dID(i) == GVP_NONE )THEN
    irv = SWIMaddLogMessage( '*** Unused MEDOC 2d variable: '// &
                             TRIM(Var2d%string(i)) )
  ELSE
    irv = SWIMaddLogMessage( 'Recognized MEDOC 2d variable: '// &
                             TRIM(Var2d%string(i)) )
  END IF
  IF( irv /= SWIMsuccess )GOTO 9999

END DO

SetMEDOCvar2d = SWIMresult

9999 CONTINUE

RETURN
END

