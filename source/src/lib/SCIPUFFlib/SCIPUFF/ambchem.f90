SUBROUTINE InitChemAmb()

! --- Initialize chemistry ambient from input file

USE scipuff_fi
USE chem_fi

IMPLICIT NONE

INTEGER ios

!====   Open ambient input data file

OPEN(UNIT=chem%lunAmb,FILE=TRIM(chem%ambFile),STATUS='OLD',IOSTAT=ios)
IF( ios /= 0 )THEN
  nError   = OP_ERROR
  eRoutine = 'InitChemAmb'
  eMessage = 'Error opening ambient species file'
  CALL ReportFileName( eInform,'File=',chem%ambFile )
  eAction  = 'Make sure file exists'
  GOTO 9999
END IF

CALL ReadAmbHeader()
IF( nError /= NO_ERROR )GOTO 9999

CALL InitAmbFields()
IF( nError /= NO_ERROR )GOTO 9999

9999 CONTINUE

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE ReadAmbHeader()

USE scipuff_fi
USE chem_fi
USE error_fi
USE files_fi
USE default_fd
USE coordinate_fd

IMPLICIT NONE

INTEGER ios, lun, nxa, nya, nza, idum, ndum
INTEGER i, isp, iamb, nvar2d, nvar3d
INTEGER iday, imonth, iyear, ihour, imin, isec
REAL    dxa, dya, x0a, y0a, xlat0, xlon0, dum, zbtop_med, AmbHmin, fac
LOGICAL found, lFormat

CHARACTER(8)   fflag, cdum
CHARACTER(16)  amb_spname
CHARACTER(PATH_MAXLENGTH) errfile

CHARACTER(8), DIMENSION(:), ALLOCATABLE :: Var2d, Var3d

REAL,    EXTERNAL :: SetAmbTime

lun = chem%lunAmb

errfile = chem%ambFile

lFormat = .TRUE.

chem%Ambient%InterpAmb = .TRUE.
lMonthAmb = .FALSE.
MonthDay  = 0

!===  Read header data

READ(lun,'(A8)',IOSTAT=ios) fflag
IF( ios /= 0 )GOTO 9998

chem%Ambient%type = 0

IF( fflag == 'FFFFFFFF' )THEN
  chem%Ambient%type = IBSET(chem%Ambient%type,ATB_MEDOC)
  chem%Ambient%type = IBCLR(chem%Ambient%type,ATB_BINARY)
ELSE
  !====   Reopen ambient input data file in unformatted form
  CLOSE(lun)
  OPEN(UNIT=lun,FILE=TRIM(chem%ambFile),STATUS='OLD',FORM='UNFORMATTED',IOSTAT=ios)
  IF( ios /= 0 )THEN
    nError   = OP_ERROR
    eRoutine = 'ReadAmbHeader'
    eMessage = 'Error opening unformatted ambient species file'
    CALL ReportFileName( eInform,'File=',chem%ambFile )
    eAction  = 'Make sure file exists'
    GOTO 9999
  END IF

  READ(lun,IOSTAT=ios) fflag
  IF( ios /= 0 )GOTO 9998
  IF (fflag == 'BBBBBBBB') THEN
    chem%Ambient%type = IBSET(chem%Ambient%type,ATB_MEDOC)
    chem%Ambient%type = IBSET(chem%Ambient%type,ATB_BINARY)
  ELSE
    nError   = RD_ERROR
    eRoutine = 'ReadAmbHeader'
    eMessage = 'Formatted or unformatted MEDOC file type expected'
    CALL ReportFileName( eInform,'File=',chem%ambFile )
    eAction  = 'First record must be FFFFFFFF or BBBBBBBB'
    GO TO 9999
  END IF
END IF
IF( BTEST(chem%Ambient%type,ATB_BINARY) )lFormat = .FALSE.

IF( BTEST(chem%Ambient%type,ATB_MEDOC) )THEN

IF( lFormat )THEN
  READ(lun,'(A8)',IOSTAT=ios) fflag
ELSE
  READ(lun,IOSTAT=ios) fflag
END IF
IF( ios /= 0 )GOTO 9998

IF( lFormat )THEN
  READ(lun,'(6(I12,1X))',IOSTAT=ios) iday,imonth,iyear,ihour,imin,isec
ELSE
  READ(lun,IOSTAT=ios) iday,imonth,iyear,ihour,imin,isec
END IF
IF( ios /= 0 )GOTO 9998

IF( iday == 99 .AND. iyear /= 9999 )ios = -2
IF( iday /= 99 .AND. iyear == 9999 )ios = -1
IF( ios < 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'ReadAmbHeader'
  IF( ios == -2 )THEN
    eMessage = 'Year in ambient monthly background header not set to 9999'
  ELSEIF( ios == -1 )THEN
    eMessage = 'Day in ambient monthly background header not set to 99'
  END IF
  CALL ReportFileName( eInform,'File=',chem%ambFile )
  eAction  = 'Day and Year must be 99 and 9999 respt. for using monthly ambient concentration format'
  GOTO 9999
ELSEIF( iday == 99 .AND. iyear == 9999 )THEN
  lMonthAmb = .TRUE.
  CALL RdMonthlyAmb( lun,lformat )
  GOTO 9999
END IF

IF( lFormat )THEN
  READ(lun,'(6(I12,1X))',IOSTAT=ios) idum,idum,idum,idum,idum,idum
ELSE
  READ(lun,IOSTAT=ios) idum,idum,idum,idum,idum,idum
END IF
IF( ios /= 0 )GOTO 9998

IF( lFormat )THEN
  READ(lun,'(6(I12,1X))',IOSTAT=ios) nxa,nya,nza,ndum,nvar3d,nvar2d
ELSE
  READ(lun,IOSTAT=ios) nxa,nya,nza,ndum,nvar3d,nvar2d
END IF
IF( ios /= 0 )GOTO 9998

END IF

!=== Set ambient time

chem%Ambient%time1 = SetAmbTime( iday,imonth,iyear,ihour,imin,isec )

!=== Check if sufficient ambient data

IF( (nxa < 1) .OR. (nya < 1) .OR. (nza < 1) )THEN
  nError   = IV_ERROR
  eRoutine = 'ReadAmbHeader'
  eMessage = 'Ambient species data insufficient'
  CALL ReportFileName( eInform,'File=',chem%ambFile )
  GOTO 9999
END IF

ALLOCATE( chem%Ambient%zGrid(nza),STAT=ios )
IF( ios /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'ReadAmbHeader'
  eMessage = 'Insufficient memory to allocate vertical grid'
  WRITE(eInform,*)'Size =',nza
  GOTO 9999
END IF

chem%Ambient%nx = nxa
chem%Ambient%ny = nya
chem%Ambient%nz = nza

IF( BTEST(chem%Ambient%type,ATB_MEDOC) )THEN

IF( lFormat )THEN
  READ(lun,'(6(I12,1X))',IOSTAT=ios) (idum,i=1,6)
ELSE
  READ(lun,IOSTAT=ios) (idum,i=1,6)
END IF
IF( ios /= 0 )GOTO 9998

IF( lFormat )THEN
  READ(lun,'(3(I12,1X))',IOSTAT=ios) (idum,i=1,3)
ELSE
  READ(lun,IOSTAT=ios) (idum,i=1,3)
END IF
IF( ios /= 0 )GOTO 9998

IF( lFormat )THEN
  READ(lun,'(6(F12.4,1X))',IOSTAT=ios) (chem%Ambient%zGrid(i),i=1,nza),dxa,dya, &
                                                    x0a,y0a,xlat0,xlon0,(dum,i=1,4),zbtop_med
ELSE
  READ(lun,IOSTAT=ios) (chem%Ambient%zGrid(i),i=1,nza),dxa,dya, &
                                                    x0a,y0a,xlat0,xlon0,(dum,i=1,4),zbtop_med
END IF
IF( ios /= 0 )GOTO 9998

CALL SWIMinitCoord( chem%Ambient%coord )

IF( x0a == -999999. )THEN
  x0a = xlon0
  y0a = xlat0
  chem%Ambient%coord%type = I_LATLON
ELSE
  dxa = dxa * 1.0E-3               !Convert to Ambient coords (km)
  dya = dya * 1.0E-3
  chem%Ambient%coord%type = I_UTM
END IF

END IF

chem%Ambient%x0 = x0a
chem%Ambient%y0 = y0a

chem%Ambient%dx = dxa
chem%Ambient%dy = dya

chem%Ambient%x1 = x0a + FLOAT(nxa-1)*dxa
chem%Ambient%y1 = y0a + FLOAT(nya-1)*dya

chem%Ambient%n2d = nvar2d
chem%Ambient%n3d = nvar3d

ALLOCATE( Var3d(nvar3d),Var2d(MAX(1,nvar2d)),STAT=ios )
IF( ios /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'ReadAmbHeader'
  eMessage = 'Insufficient memory to allocate variable names'
  WRITE(eInform,*)'Size =',nvar2d,nvar3d
  GOTO 9999
END IF

IF( BTEST(chem%Ambient%type,ATB_MEDOC) )THEN

IF( ndum > 0 )THEN
  IF( lFormat )THEN
    READ(lun,'(6(A8,1X))',IOSTAT=ios) (cdum,i=1,ndum), &
              (Var3d(i),i=1,nvar3d),(cdum,i=1,nvar3d), &
              (Var2d(i),i=1,nvar2d),(cdum,i=1,nvar2d)
  ELSE
    READ(lun,IOSTAT=ios) (cdum,i=1,ndum), &
              (Var3d(i),i=1,nvar3d),(cdum,i=1,nvar3d), &
              (Var2d(i),i=1,nvar2d),(cdum,i=1,nvar2d)
  END IF
ELSE
  IF( lFormat )THEN
    READ(lun,'(6(A8,1X))',IOSTAT=ios) &
              (Var3d(i),i=1,nvar3d),(cdum,i=1,nvar3d), &
              (Var2d(i),i=1,nvar2d),(cdum,i=1,nvar2d)
  ELSE
    READ(lun,IOSTAT=ios) &
              (Var3d(i),i=1,nvar3d),(cdum,i=1,nvar3d), &
              (Var2d(i),i=1,nvar2d),(cdum,i=1,nvar2d)
  END IF
END IF
IF( ios /= 0 )GOTO 9998

END IF

ALLOCATE( chem%Ambient%ID(chem%nSpecies),chem%Ambient%read(nvar3d),STAT=ios )
IF( ios /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'ReadAmbHeader'
  eMessage = 'Insufficient memory to allocate index array'
  WRITE(eInform,*)'Size =',chem%nSpecies
  GOTO 9999
END IF

chem%Ambient%ID   = 0
chem%Ambient%read = .FALSE.

IF( BTEST(chem%Ambient%type,ATB_MEDOC) )THEN
  IF( lFormat )THEN
    chem%Ambient%nskip = 4 + INT((10+nza)/6 + 1) + INT((ndum+2*nvar2d+2*nvar3d-1)/6 + 1)
  ELSE
    chem%Ambient%nskip = 6 !4 + 1 + 1
  END IF
END IF

chem%Ambient%terrain = -1  !Initialize for no terrain

DO i = 1,nvar2d
  SELECT CASE( TRIM(ADJUSTL(var2d(i))) )
    CASE( 'REL','TOPO','TOPT' )
      chem%Ambient%terrain = i  !Terrain must be read from field i (reset to zero later)
      ALLOCATE( chem%Ambient%ht(nxa*nya),chem%Ambient%zMid(nxa*nya,nza),STAT=ios )
      IF( ios /= 0 )THEN
        nError   = RD_ERROR
        eRoutine = 'ReadAmbHeader'
        eMessage = 'Insufficient memory to allocate terrain arrays'
        WRITE(eInform,*)'Size =',nxa*nya,nxa*nya*nza
        GOTO 9999
      END IF
      EXIT
  END SELECT
END DO

iamb = 0

DO i = 1,nvar3d

  amb_spname = ADJUSTL(var3d(i))

!--- Find the ambient species in species list

  found = .FALSE.

  DO isp = 1,chem%nSpecies
    IF( TRIM(chem%species(isp)%name) == TRIM(amb_spname) )THEN
      IF( chem%species(isp)%class /= ID_SPECIES_EQUILIBRIUM )THEN
        iamb = iamb + 1
        chem%Ambient%ID(isp) = iamb
        chem%Ambient%read(i) = .TRUE.
        !WRITE(lun_log,'(A,I4)',IOSTAT=ios)'Recognised 3D ambient species '// &
        !                    TRIM(amb_spname)//' as active species ',isp
      !ELSE
        !WRITE(lun_log,'(A)',IOSTAT=ios)'Recognised 3D ambient species '// &
        !                TRIM(amb_spname)//' as equilibrium species (Not used)'
      END IF
      found = .TRUE.
      EXIT
    END IF
  END DO

  IF( .NOT.found )THEN
    WRITE(lun_log,'(A)',IOSTAT=ios)'3D ambient species '// &
                      TRIM(amb_spname)//' not in species list'
  END IF
END DO

ALLOCATE( chem%Ambient%amb(nxa*nya*nza,iamb,2),STAT=ios )
IF( ios /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'ReadAmbHeader'
  eMessage = 'Insufficient memory to allocate data arrays'
  WRITE(eInform,*)'Size =',nxa,nya,nza,iamb
  GOTO 9999
END IF

chem%Ambient%it1   = 1
chem%Ambient%time2 = NOT_SET_R

CALL ReadAmbFields( 1 )
IF( nError /= NO_ERROR )GOTO 9999

!------ Setup terrain factor

IF( chem%Ambient%terrain == 0 .AND. BTEST(chem%Ambient%type,ATB_MEDOC) )THEN

  AmbHmin = HUGE(0.)
  DO i = 1,nxa*nya
    AmbHmin = MIN(chem%Ambient%ht(i),AmbHmin)
  END DO

  DO i = 1,nxa*nya
    chem%Ambient%ht(i) = chem%Ambient%ht(i) - AmbHmin
  END DO

  IF( zbtop_med == NOT_SET_R .OR. zbtop_med <= 0. )THEN

    zbtop_med = 0.5*(chem%Ambient%zGrid(nza)+chem%Ambient%zGrid(nza-1))

  ELSE

    zbtop_med = zbtop_med - AmbHmin
    fac = 1. - AmbHmin / zbtop_med

    DO i = 1,nza
      chem%Ambient%zGrid(i)  = chem%Ambient%zGrid(i)  * fac
    END DO

  END IF

  DO i = 1,nxa*nya
    fac = 1.0 - chem%Ambient%ht(i)/zbtop_med
    chem%Ambient%zMid(i,:) = chem%Ambient%zGrid * fac
    chem%Ambient%ht(i) = chem%Ambient%ht(i) + AmbHmin
  END DO

END IF

9999 CONTINUE

IF( ALLOCATED(Var3d) )DEALLOCATE( Var3d,STAT=ios )
IF( ALLOCATED(Var2d) )DEALLOCATE( Var2d,STAT=ios )

RETURN

9998 CONTINUE

nError   = RD_ERROR
eRoutine = 'ReadAmbHeader'
eMessage = 'Error reading ambient species input file header'
CALL ReportFileName( eInform,'File=',errfile )
GOTO 9999

END

!------------------------------------------------------------------------------

SUBROUTINE ReadAmbFields( n )

!--- Reads ambient species fields, assumes file is positioned at start of 3d data

USE chem_fi
USE error_fi
USE files_fi
USE default_fd

IMPLICIT NONE

INTEGER, INTENT( IN ) :: n

INTEGER i, j, iamb, lun, ntot, ios
REAL    dum
LOGICAL lFormat

!IF( chem%Ambient%time2 == NOT_SET_R )THEN
!  WRITE(lun_log,'("Reading ambient file ",A," at time ",1PG11.4,"hrs.")',IOSTAT=ios)&
!      TRIM(chem%ambFile),chem%Ambient%time1/3600.
!ELSE
!  WRITE(lun_log,'("Reading ambient file ",A," at time ",1PG11.4,"hrs.")',IOSTAT=ios)&
!      TRIM(chem%ambFile),chem%Ambient%time2/3600.
!END IF
!IF( ios /= 0 ) THEN
!  nError   = WR_ERROR
!  eRoutine = 'ReadAmbFields'
!  eMessage = 'Error writing ambient time to log file'
!  GO TO 9999
!END IF

lun     = chem%lunAmb
ntot    = chem%Ambient%nx * chem%Ambient%ny * chem%Ambient%nz
lFormat = .TRUE.
IF( chem%Ambient%time2 == NOT_SET_R )THEN
  MonthDay(n,1) = chem%Ambient%time1/3600.
ELSE
  MonthDay(n,1) = chem%Ambient%time2/3600.
END IF
MonthDay(n,2) = 0  ! Logical for stepping radical species

IF( BTEST(chem%Ambient%type,ATB_BINARY) )lFormat = .FALSE.

iamb = 0

!--- Read 3d fields into appropriate locations
IF( lFormat )THEN
  DO i = 1,chem%Ambient%n3d
    IF( chem%Ambient%read(i) )THEN
      iamb = iamb + 1
      READ(lun,'(6(F12.4,1X))',IOSTAT=ios) (chem%Ambient%amb(j,iamb,n),j=1,ntot)
    ELSE
      READ(lun,'(6(F12.4,1X))',IOSTAT=ios) (dum,j=1,ntot)
    END IF
  END DO
ELSE
  DO i = 1,chem%Ambient%n3d
    IF( chem%Ambient%read(i) )THEN
      iamb = iamb + 1
      READ(lun,IOSTAT=ios) (chem%Ambient%amb(j,iamb,n),j=1,ntot)
    ELSE
      READ(lun,IOSTAT=ios) (dum,j=1,ntot)
    END IF
  END DO
END IF
IF( ios /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'ReadAmbFields'
  eMessage = 'Error reading ambient species 3d data'
  CALL ReportFileName( eInform,'File=',chem%ambFile )
  GOTO 9999
END IF

!--- Skip 2d fields, except for terrain initially

ntot = chem%Ambient%nx * chem%Ambient%ny

IF( lFormat )THEN
  DO i = 1,chem%Ambient%n2d
    IF( i == chem%Ambient%terrain )THEN
      READ(lun,'(6(F12.0,1X))',IOSTAT=ios) (chem%Ambient%ht(j),j=1,ntot)
      chem%Ambient%terrain = 0
    ELSE
      READ(lun,'(6(F12.0,1X))',IOSTAT=ios) (dum,j=1,ntot)
    END IF
  END DO
ELSE
  DO i = 1,chem%Ambient%n2d
    IF( i == chem%Ambient%terrain )THEN
      READ(lun,IOSTAT=ios) (chem%Ambient%ht(j),j=1,ntot)
      chem%Ambient%terrain = 0
    ELSE
      READ(lun,IOSTAT=ios) (dum,j=1,ntot)
    END IF
  END DO
END IF
IF( ios /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'ReadAmbFields'
  eMessage = 'Error reading ambient species 2d data'
  CALL ReportFileName( eInform,'File=',chem%ambFile )
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE InitAmbFields()

USE chem_fi
USE scipuff_fi
USE default_fd

IMPLICIT NONE

REAL ambTime

IF( chem%Ambient%time1 > t )THEN
  chem%Ambient%time2 = chem%Ambient%time1
  chem%Ambient%it1   = 0    !Flag for only one time (in field-1)
  GOTO 9999
END IF

CALL getNextAmbTime()
IF( nError /= NO_ERROR )GOTO 9999

IF( chem%Ambient%time2 /= NOT_SET_R )THEN
  IF( lMonthAmb )THEN
    ambTime = 1.
    CALL GetMonthDayAmb( 2,ambTime )
  ELSE
  CALL ReadAmbFields( 2 )
  END IF
  IF( nError /= NO_ERROR )GOTO 9999
END IF

CALL UpdateAmbFields()
IF( nError /= NO_ERROR )GOTO 9999

9999 CONTINUE

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE UpdateAmbFields()

USE chem_fi
USE scipuff_fi

IMPLICIT NONE

INTEGER it2, lun
REAL ambTime

IF( chem%Ambient%time2 == NOT_SET_R )GOTO 9999

lun = chem%lunAmb

DO WHILE( chem%Ambient%time2 < t .AND. chem%Ambient%time2 /= NOT_SET_R )

  chem%Ambient%time1 = chem%Ambient%time2

  IF( chem%Ambient%it1 == 0 )THEN
    chem%Ambient%it1 = 1
  ELSE
    chem%Ambient%it1 = 3 - chem%Ambient%it1   !Swap field pointer
  END IF

  CALL getNextAmbTime()
  IF( nError /= NO_ERROR )GOTO 9999

  IF( chem%Ambient%time2 /= NOT_SET_R )THEN

    it2 = 3 - chem%Ambient%it1

    IF( lMonthAmb )THEN
      ambTime = 1
      CALL GetMonthDayAmb( it2,ambTime )
    ELSE
    CALL ReadAmbFields( it2 )
    END IF
    IF( nError /= NO_ERROR )GOTO 9999

  END IF

END DO

9999 CONTINUE

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE getNextAmbTime()

USE chem_fi
USE error_fi
USE default_fd
USE scipuff_fi

IMPLICIT NONE

INTEGER lun, ios, i
INTEGER iday, imonth, iyear, ihour, imin, isec

CHARACTER(8)   fflag

LOGICAL lFormat

REAL, EXTERNAL :: SetAmbTime

lun = chem%lunAmb

lFormat = .TRUE.
IF( BTEST(chem%Ambient%type,ATB_BINARY) )lFormat = .FALSE.

IF( lMonthAmb )THEN
  chem%Ambient%time2 = chem%Ambient%time1 + 3600.
  GOTO 9999
END IF

IF( lFormat )THEN
  READ(lun,'(A8)',IOSTAT=ios) fflag
ELSE
  READ(lun,IOSTAT=ios) fflag
END IF
IF( ios == 0 )THEN
  IF( lFormat )THEN
    READ(lun,'(A8)',IOSTAT=ios) fflag
  ELSE
    READ(lun,IOSTAT=ios) fflag
  END IF
END IF
IF( ios == 0 )THEN
  IF( lFormat )THEN
    READ(lun,'(6(I12,1X))',IOSTAT=ios) iday,imonth,iyear,ihour,imin,isec
  ELSE
    READ(lun,IOSTAT=ios) iday,imonth,iyear,ihour,imin,isec
  END IF
END IF

DO i = 1,chem%Ambient%nskip
  IF( lFormat )THEN
    IF( ios == 0 )READ(lun,*,IOSTAT=ios )
  ELSE
    IF( ios == 0 )READ(lun,IOSTAT=ios )
  END IF
END DO

IF( ios == 0 )THEN

  chem%Ambient%time2 = SetAmbTime( iday,imonth,iyear,ihour,imin,isec )

ELSE IF( ios < 0 )THEN

  chem%Ambient%time2 = NOT_SET_R

ELSE

  nError   = RD_ERROR
  eRoutine = 'getNextAmbTime'
  eMessage = 'Error reading next time from ambient species input file header'
  CALL ReportFileName( eInform,'File=',chem%ambFile )
  GOTO 9999

END IF

9999 CONTINUE

RETURN
END

!------------------------------------------------------------------------------
RECURSIVE SUBROUTINE SetChemAmbient( xIn,yIn,zIn,t,leq,lmet,lplot )


USE chem_fi
USE default_fd
USE error_fi
USE files_fi
USE met_fi
USE AqAer_fi, ONLY: ChemMet_str

IMPLICIT NONE

REAL,    INTENT( IN ) :: xIn, yIn, zIn, t
LOGICAL, INTENT( IN ) :: leq            !Set any equilibrium species ambient
LOGICAL, INTENT( IN ) :: lmet           !Met fields are available
LOGICAL, OPTIONAL, INTENT( IN ) :: lplot !Call from plotchem

INTEGER i, j, k, j0, is, nxy, ios
INTEGER npx, npy, npz, npt, np, iamb, ip
REAL    rx, ry, amb
REAL    x, y, z

LOGICAL             :: lstepAer

INTEGER, DIMENSION(2)     :: it, ix, iy
REAL,    DIMENSION(2)     :: fact, facx, facy
INTEGER, DIMENSION(2,2,2) :: iz
REAL,    DIMENSION(2,2,2) :: facz
INTEGER, DIMENSION(8)     :: ipt
REAL,    DIMENSION(8)     :: fac
INTEGER, DIMENSION(8)     :: ipxy
INTEGER, DIMENSION(8)     :: ipz

REAL,    EXTERNAL :: SetFixedAmb

z = zIn
zlev = zIN

IF( PRESENT(lplot) )THEN
  lstepAer = .NOT. lplot
ELSE
  lstepAer = .TRUE.
END IF

IF( chem%lAmbFile )THEN

    x = xIn; y = yIn

!--- Find spatial indices

  IF( chem%Ambient%nx > 1 )THEN
    IF( x < chem%Ambient%x0 )THEN
      ix(1)   = 1
      facx(1) = 1.0
      npx     = 1
    ELSE IF( x > chem%Ambient%x1 )THEN
      ix(1)   = chem%Ambient%nx
      facx(1) = 1.0
      npx     = 1
    ELSE
      rx      = (x - chem%Ambient%x0)/chem%Ambient%dx
      ix(1)   = INT(rx) + 1
      ix(2)   = ix(1) + 1
      facx(2) = rx - FLOAT(ix(1)-1)
      facx(1) = 1.0 - facx(2)
      npx     = 2
    END IF
  ELSE
    ix(1)   = 1
    facx(1) = 1.0
    npx     = 1
  END IF

  IF( chem%Ambient%ny > 1 )THEN
    IF( y < chem%Ambient%y0 )THEN
      iy(1)   = 1
      facy(1) = 1.0
      npy     = 1
    ELSE IF( y > chem%Ambient%y1 )THEN
      iy(1)   = chem%Ambient%ny
      facy(1) = 1.0
      npy     = 1
    ELSE
      ry      = (y - chem%Ambient%y0)/chem%Ambient%dy
      iy(1)   = INT(ry) + 1
      iy(2)   = iy(1) + 1
      facy(2) = ry - FLOAT(iy(1)-1)
      facy(1) = 1.0 - facy(2)
      npy     = 2
    END IF
  ELSE
    iy(1)   = 1
    facy(1) = 1.0
    npy     = 1
  END IF

  IF( chem%Ambient%terrain < 0 )THEN

    IF( chem%Ambient%nz > 1 )THEN
      IF( z <= chem%Ambient%zGrid(1) )THEN
        iz(1,:,:)   = 1
        facz(1,:,:) = 1.0
        npz         = 1
      ELSE IF( z >= chem%Ambient%zGrid(chem%Ambient%nz) )THEN
        iz(1,:,:)   = chem%Ambient%nz
        facz(1,:,:) = 1.0
        npz         = 1
      ELSE
        k = 2
        DO WHILE( z > chem%Ambient%zGrid(k) )
          k = k + 1
        END DO
        iz(1,:,:)   = k - 1
        iz(2,:,:)   = k
        facz(2,:,:) = (z - chem%Ambient%zGrid(k-1))/ &
                  (chem%Ambient%zGrid(k) - chem%Ambient%zGrid(k-1))
        facz(1,:,:) = 1.0 - facz(2,:,:)
        npz         = 2
      END IF
    ELSE
      iz(1,:,:)   = 1
      facz(1,:,:) = 1.0
      npz         = 1
    END IF

  ELSE

    DO j = 1,npy
      j0 = (iy(j)-1)*chem%Ambient%nx

      DO i = 1,npx
        is = j0 + ix(i)

        IF( chem%Ambient%nz > 1 )THEN
          IF( z <= chem%Ambient%zMid(is,1) )THEN
            iz(:,i,j)   = 1
            facz(1,i,j) = 1.0
            facz(2,i,j) = 0.0
            npz         = 1
          ELSE IF( z >= chem%Ambient%zMid(is,chem%Ambient%nz) )THEN
            iz(:,i,j)   = chem%Ambient%nz
            facz(1,i,j) = 1.0
            facz(2,i,j) = 0.0
            npz         = 1
          ELSE
            k = 2
            DO WHILE( z > chem%Ambient%zMid(is,k) )
              k = k + 1
            END DO
            iz(1,i,j)   = k - 1
            iz(2,i,j)   = k
            facz(2,i,j) = (z - chem%Ambient%zMid(is,k-1))/ &
                          (chem%Ambient%zMid(is,k) - chem%Ambient%zMid(is,k-1))
            facz(1,i,j) = 1.0 - facz(2,i,j)
            npz         = 2
          END IF
        ELSE
          iz(:,i,j)   = 1
          facz(1,i,j) = 1.0
          facz(2,i,j) = 0.0
          npz         = 1
        END IF

      END DO
    END DO

  END IF

  IF( chem%Ambient%InterpAmb )THEN
    IF( chem%Ambient%it1 == 0 .OR. chem%Ambient%time2 == NOT_SET_R )THEN
      it(1)   = 1
      npt     = 1
      fact(1) = 1.0
    ELSE
      it(1)   = chem%Ambient%it1
      it(2)   = 3 - it(1)
      fact(2) = (t-chem%Ambient%time1)/(chem%Ambient%time2-chem%Ambient%time1)
      fact(2) = MIN(MAX(fact(2),0.),1.)
      fact(1) = 1.0 - fact(2)
      npt     = 2
    END IF
  ELSE
    it(1)   = 1
    npt     = 1
    fact(1) = 1.0
  END IF

!---- Set up list of factors and grid points

  nxy = chem%Ambient%nx*chem%Ambient%ny
  np  = 0
  DO j = 1,npy
    j0 = (iy(j)-1)*chem%Ambient%nx
    DO i = 1,npx
      is = j0 + ix(i)
      DO k = 1,npz
        np = np + 1
        ipt(np) = (iz(k,i,j)-1)*nxy + is
        ipxy(np) = is
        ipz(np)  = iz(k,i,j)
        fac(np) = facx(i)*facy(j)*facz(k,i,j)
      END DO
    END DO
  END DO

  IF( chem%Ambient%InterpAmb )THEN
    DO j = 1,2
      IF( MonthDay(j,2) == 0 )THEN
        ! Skip call from PlotChem and GetSampMC
        IF( lstepAer )THEN
          CALL StepRadicals( j )
          IF( nError /= NO_ERROR )THEN
            nError   = IV_ERROR
            eRoutine = 'StepRadicals'
            eMessage = 'Error from StepRadicals module.'
            GO TO 9999
          END IF
        END IF
      END IF
    END DO
!---- Interpolate from ambient fields

    DO i = 1,chem%nSpecies
      IF( chem%Ambient%ID(i) > 0 )THEN
        iamb = chem%Ambient%ID(i)
        amb  = 0.0
        DO j = 1,npt
          DO ip = 1,np
            amb = amb + fact(j)*fac(ip)*chem%Ambient%amb(ipt(ip),iamb,it(j))
          END DO
        END DO
        chem%species(i)%amb = amb
      ELSE
        chem%species(i)%amb = SetFixedAmb( chem%species(i)%ambient,lmet )
        IF( nError /= NO_ERROR )GOTO 9999
      END IF
    END DO

  ELSE IF( chem%lStepAmb )THEN  ! not InterpAmb

    !---- Interpolate from  stepped ambient field

    DO i = 1,chem%nSpecies
      amb  = 0.0
      DO ip = 1,np
        amb = amb + fac(ip)*chem%Ambient%stepAmb(ipxy(ip),ipz(ip),i)
      END DO
      chem%species(i)%amb = amb
    END DO

  END IF  ! lStepAmb

ELSE

  IF( chem%lStepAmb )THEN
     DO i = 1,chem%nSpecies
       chem%species(i)%amb = chem%Ambient%stepAmb(1,1,i)
     END DO
  ELSE
    DO i = 1,chem%nSpecies
      chem%species(i)%amb = SetFixedAmb( chem%species(i)%ambient,lmet )
      IF( nError /= NO_ERROR )GOTO 9999
    END DO
  END IF

END IF

DO i = nspectot+1,chem%nSpecies
  chem%species(i)%conc = 0.0
END DO

nspecies = chem%nFast + chem%nSlow + chem%nParticle

!====   Set ambient equilibrium species concentrations

IF( chem%nEquilibrium > 0 .AND. leq )THEN

  nequil   = chem%nEquilibrium
  nspectot = nspecies + nequil
  nambient = chem%nSpecies - nspectot

  species => chem%species

  equil => chem%equil

  nsolve_eq = chem%nSolveEq
  nlin_eq   = chem%nLinearEq
  ndir_eq   = chem%nDirectEq
  nsubs_eq  = chem%nSubstEq

  indx_eq  => chem%IndexEq
  itype_eq => chem%TypeEq
  irow_eq  => chem%RowEq

  CALL UpdateChemRate( .TRUE. )

  CALL SetChemEquilibrium( .TRUE. )
  IF( nError /= NO_ERROR )THEN
    nError = NO_ERROR
    WRITE(lun_log,'(A)',IOSTAT=ios) 'Error setting equilibrium in SetChemAmbient'
  END IF

END IF

9999 CONTINUE

RETURN
END

!------------------------------------------------------------------------------

REAL FUNCTION SetFixedAmb( amb,lmet )

USE chem_fi
USE met_fi

IMPLICIT NONE

REAL,    INTENT( IN ) :: amb
LOGICAL, INTENT( IN ) :: lmet           !Met fields are available

IF( amb >= 0.0 )THEN  ! Set from imc file

  SetFixedAmb = amb

ELSE IF( lmet )THEN   ! Set from humidity

  SetFixedAmb = 1.0E06*hb*(29./18.)/(1. + hb*(29./18.))
  IF( chem%cUnits == UNIT_MOLECULE )SetFixedAmb = SetFixedAmb*7.33981E15*(pb/1013.2/tb)

ELSE  ! No met available so not used (only used in reactions)

  SetFixedAmb = 0.0

END IF

RETURN
END

!------------------------------------------------------------------------------

REAL FUNCTION SetAmbTime( iday,imonth,iyear,ihour,imin,isec )

USE scipuff_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: iday, imonth, iyear, ihour, imin, isec

INTEGER jul, jyy

INTEGER, EXTERNAL :: julian_day, days_in_year

IF( iyear /= NOT_SET_I .AND. imonth /= NOT_SET_I )THEN

  jul  = julian_day( imonth,iday,iyear )

  IF( iyear > year_start )THEN
    DO jyy = year_start,iyear-1
      jul = jul + days_in_year( jyy )
    END DO
  ELSE IF( iyear < year_start )THEN
    DO jyy = year_start-1,iyear,-1
      jul = jul - days_in_year( jyy )
    END DO
  END IF

ELSE

  jul = iday

END IF

SetAmbTime = FLOAT(jul-jul_start)*86400. + FLOAT(ihour)*3600. + FLOAT(imin)*60. &
                                         + FLOAT(isec) - tstart*3600.


RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE ChemAmbPrjRes( dxPrj,dyPrj )

USE scipuff_fi
USE met_fi
USE chem_fi
USE default_fd

IMPLICIT NONE

REAL, INTENT( OUT ) :: dxPrj, dyPrj

REAL xp, yp, xmap, ymap

dxPrj = NOT_SET_R
dyPrj = NOT_SET_R

IF( chem%lAmbFile )THEN

  IF( chem%Ambient%nx > 1 .OR. chem%Ambient%ny > 1 )THEN

    IF( PrjCoord%type /= chem%Ambient%coord%type )THEN

      xp = chem%Ambient%x0 + 0.5*FLOAT(chem%Ambient%nx-1)*chem%Ambient%dx
      yp = chem%Ambient%y0 + 0.5*FLOAT(chem%Ambient%ny-1)*chem%Ambient%dy
      CALL SWIMmapfac( chem%Ambient%coord,xp,yp,xmap,ymap )
      dxPrj = chem%Ambient%dx / xmap               !Convert to m
      dyPrj = chem%Ambient%dy / ymap

      xp = 0.5*(xmin+xmax)
      yp = 0.5*(ymin+ymax)
      CALL mapfac( xp,yp,xmap,ymap )             !Convert to project coordinates

      dxPrj = dxPrj * xmap
      dyPrj = dyPrj * ymap

    ELSE

      dxPrj = chem%Ambient%dx
      dyPrj = chem%Ambient%dy

    END IF

  ELSE

    dxPrj = xmax - xmin
    dyPrj = ymax - ymin

  END IF

END IF

RETURN
END

!=======================================================================

SUBROUTINE SetChemAmbDosRes( imat )

USE scipuff_fi
USE error_fi
USE chem_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: imat

INTEGER mcID
REAL    dxPrj, dyPrj

mcID = material(imat)%mcID

SELECT CASE( mat_mc%type(mcID) )
CASE( MC_CHEM )
    chem => chemMC(mat_mc%ID(mcID),1)
    CALL ChemAmbPrjRes( dxPrj,dyPrj )
    IF( dxPrj /= NOT_SET_R )THEN
      IF( MIN(dxPrj,dyPrj) < xChemAmbDosRes )THEN
        lChemAmbDosRes = .TRUE.
        xChemAmbDosRes = MIN(xChemAmbDosRes,dxPrj,dyPrj)
      END IF
    END IF
  CASE DEFAULT
    nError   = UK_ERROR
    eRoutine = 'SetChemAmbDosRes'
    eMessage = 'Multicomponent error'
    WRITE(eInform,'(A,I6)') 'Unknown multicomponent type :',mat_mc%type(mcID)
    GOTO 9999
END SELECT

9999 CONTINUE

RETURN
END

!=======================================================================

SUBROUTINE RdMonthlyAmb( lun,lformat )

USE scipuff_fi
USE error_fi
USE chem_fi
USE files_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: lun
LOGICAL, INTENT( IN ) :: lformat

INTEGER i, j, k
INTEGER ios, idum, im, ih
INTEGER iday, imonth, iyear
INTEGER ihour, imin, isec
INTEGER iamb, isp
INTEGER nxa, nya, nza, ndum, namb
INTEGER nvar2d, nvar3d, ntot

LOGICAL found

CHARACTER(8)   fflag, cdum
CHARACTER(16)  amb_spname
CHARACTER(PATH_MAXLENGTH) errfile

CHARACTER(8), DIMENSION(:), ALLOCATABLE :: Var2d, Var3d

REAL dxa, dya, x0a, y0a, xlat0, xlon0, dum, zbtop_med, AmbHmin, fac

IF( lFormat )THEN
  READ(lun,'(6(I12,1X))',IOSTAT=ios) idum,idum,idum,idum,idum,idum
ELSE
  READ(lun,IOSTAT=ios) idum,idum,idum,idum,idum,idum
END IF
IF( ios /= 0 )GOTO 9998

IF( lFormat )THEN
  READ(lun,'(6(I12,1X))',IOSTAT=ios) nxa,nya,nza,ndum,nvar3d,nvar2d
ELSE
  READ(lun,IOSTAT=ios) nxa,nya,nza,ndum,nvar3d,nvar2d
END IF
IF( ios /= 0 )GOTO 9998

!=== Check if sufficient ambient data

IF( (nxa < 1) .OR. (nya < 1) .OR. (nza < 1) )THEN
  nError   = IV_ERROR
  eRoutine = 'RdMonthlyAmb'
  eMessage = 'Ambient species data insufficient'
  CALL ReportFileName( eInform,'File=',chem%ambFile )
  GOTO 9999
END IF

ALLOCATE( Var3d(nvar3d),Var2d(MAX(1,nvar2d)),STAT=ios )
IF( ios /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'RdMonthlyAmb'
  eMessage = 'Insufficient memory to allocate variable names'
  WRITE(eInform,*)'Size =',nvar2d,nvar3d
  GOTO 9999
END IF

REWIND( lun,IOSTAT=ios )
IF( ios /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'RdMonthlyAmb'
  eMessage = 'Error rewinding ambient data file'
  CALL ReportFileName( eInform,'File=',chem%ambFile )
  GOTO 9999
END IF

DO im = 1,12 ! Months

  ALLOCATE( chem%MonthlyAmb(im)%zGrid(nza),STAT=ios )
  IF( ios /= 0 )THEN
    nError   = RD_ERROR
    eRoutine = 'RdMonthlyAmb'
    eMessage = 'Insufficient memory to allocate vertical grid'
    WRITE(eInform,*)'Size =',nza
    GOTO 9999
  END IF

  chem%MonthlyAmb(im)%nx = nxa
  chem%MonthlyAmb(im)%ny = nya
  chem%MonthlyAmb(im)%nz = nza

  ALLOCATE( chem%MonthlyAmb(im)%ID(chem%nSpecies),chem%MonthlyAmb(im)%read(nvar3d),STAT=ios )
  IF( ios /= 0 )THEN
    nError   = RD_ERROR
    eRoutine = 'RdMonthlyAmb'
    eMessage = 'Insufficient memory to allocate index array'
    WRITE(eInform,*)'Size =',chem%nSpecies
    GOTO 9999
  END IF

  IF( im == 1 )THEN
    ALLOCATE( chem%Ambient%zGrid(nza),STAT=ios )
    IF( ios /= 0 )THEN
      nError   = RD_ERROR
      eRoutine = 'RdMonthlyAmb'
      eMessage = 'Insufficient memory to allocate vertical grid'
      WRITE(eInform,*)'Size =',nza
      GOTO 9999
    END IF
    ALLOCATE( chem%Ambient%ID(chem%nSpecies),chem%Ambient%read(nvar3d),STAT=ios )
    IF( ios /= 0 )THEN
      nError   = RD_ERROR
      eRoutine = 'RdMonthlyAmb'
      eMessage = 'Insufficient memory to allocate index array'
      WRITE(eInform,*)'Size =',chem%nSpecies
      GOTO 9999
    END IF
  END IF

  DO ih = 1,24  ! Hours

    IF( lFormat )THEN
      READ(lun,'(A8)',IOSTAT=ios) fflag
    ELSE
      READ(lun,IOSTAT=ios) fflag
    END IF
    IF( ios /= 0 )GOTO 9998

    IF( lFormat )THEN
      READ(lun,'(A8)',IOSTAT=ios) fflag
    ELSE
      READ(lun,IOSTAT=ios) fflag
    END IF
    IF( ios /= 0 )GOTO 9998

    IF( lFormat )THEN
      READ(lun,'(6(I12,1X))',IOSTAT=ios) iday,imonth,iyear,ihour,imin,isec
    ELSE
      READ(lun,IOSTAT=ios) iday,imonth,iyear,ihour,imin,isec
    END IF
    IF( ios /= 0 )GOTO 9998

    IF( imonth /= im .OR. ihour /= ih )THEN
      nError   = RD_ERROR
      eRoutine = 'RdMonthlyAmb'
      eMessage = 'Cannot find correct month or hour'
      WRITE(eInform,*)'Month, Hour =',im,ih
      GOTO 9998
    END IF

    IF( lFormat )THEN
      READ(lun,'(6(I12,1X))',IOSTAT=ios) idum,idum,idum,idum,idum,idum
    ELSE
      READ(lun,IOSTAT=ios) idum,idum,idum,idum,idum,idum
    END IF
    IF( ios /= 0 )GOTO 9998

    IF( lFormat )THEN
      READ(lun,'(6(I12,1X))',IOSTAT=ios) nxa,nya,nza,ndum,nvar3d,nvar2d
    ELSE
      READ(lun,IOSTAT=ios) nxa,nya,nza,ndum,nvar3d,nvar2d
    END IF
    IF( ios /= 0 )GOTO 9998

    IF( lFormat )THEN
      READ(lun,'(6(I12,1X))',IOSTAT=ios) (idum,i=1,6)
    ELSE
      READ(lun,IOSTAT=ios) (idum,i=1,6)
    END IF
    IF( ios /= 0 )GOTO 9998

    IF( lFormat )THEN
      READ(lun,'(3(I12,1X))',IOSTAT=ios) (idum,i=1,3)
    ELSE
      READ(lun,IOSTAT=ios) (idum,i=1,3)
    END IF
    IF( ios /= 0 )GOTO 9998

    IF( lFormat )THEN
      READ(lun,'(6(F12.4,1X))',IOSTAT=ios) (chem%MonthlyAmb(im)%zGrid(i),i=1,nza),dxa,dya, &
                                                        x0a,y0a,xlat0,xlon0,(dum,i=1,4),zbtop_med
    ELSE
      READ(lun,IOSTAT=ios) (chem%MonthlyAmb(im)%zGrid(i),i=1,nza),dxa,dya, &
                                                        x0a,y0a,xlat0,xlon0,(dum,i=1,4),zbtop_med
    END IF
    IF( ios /= 0 )GOTO 9998

    CALL SWIMinitCoord( chem%MonthlyAmb(im)%coord )

    IF( x0a == -999999. )THEN
      x0a = xlon0
      y0a = xlat0
      chem%MonthlyAmb(im)%coord%type = I_LATLON
    ELSE
      dxa = dxa * 1.0E-3               !Convert to Ambient coords (km)
      dya = dya * 1.0E-3
      chem%MonthlyAmb(im)%coord%type = I_UTM
    END IF

    chem%MonthlyAmb(im)%x0 = x0a
    chem%MonthlyAmb(im)%y0 = y0a

    chem%MonthlyAmb(im)%dx = dxa
    chem%MonthlyAmb(im)%dy = dya

    chem%MonthlyAmb(im)%x1 = x0a + FLOAT(nxa-1)*dxa
    chem%MonthlyAmb(im)%y1 = y0a + FLOAT(nya-1)*dya

    chem%MonthlyAmb(im)%n2d = nvar2d
    chem%MonthlyAmb(im)%n3d = nvar3d

    Var3d = 'NOT_SET '
    Var2d = 'NOT_SET '

    IF( ndum > 0 )THEN
      IF( lFormat )THEN
        READ(lun,'(6(A8,1X))',IOSTAT=ios) (cdum,i=1,ndum), &
                  (Var3d(i),i=1,nvar3d),(cdum,i=1,nvar3d), &
                  (Var2d(i),i=1,nvar2d),(cdum,i=1,nvar2d)
      ELSE
        READ(lun,IOSTAT=ios) (cdum,i=1,ndum), &
                  (Var3d(i),i=1,nvar3d),(cdum,i=1,nvar3d), &
                  (Var2d(i),i=1,nvar2d),(cdum,i=1,nvar2d)
      END IF
    ELSE
      IF( lFormat )THEN
        READ(lun,'(6(A8,1X))',IOSTAT=ios) &
                  (Var3d(i),i=1,nvar3d),(cdum,i=1,nvar3d), &
                  (Var2d(i),i=1,nvar2d),(cdum,i=1,nvar2d)
      ELSE
        READ(lun,IOSTAT=ios) &
                  (Var3d(i),i=1,nvar3d),(cdum,i=1,nvar3d), &
                  (Var2d(i),i=1,nvar2d),(cdum,i=1,nvar2d)
      END IF
    END IF
    IF( ios /= 0 )GOTO 9998

    chem%MonthlyAmb(im)%ID   = 0
    chem%MonthlyAmb(im)%read = .FALSE.

    IF( ih == 1 )THEN
      chem%MonthlyAmb(im)%terrain = -1  !Initialize for no terrain
      DO i = 1,nvar2d
        SELECT CASE( TRIM(ADJUSTL(var2d(i))) )
          CASE( 'REL','TOPO','TOPT' )
            chem%MonthlyAmb(im)%terrain = i  !Terrain must be read from field i (reset to zero later)
            ALLOCATE( chem%MonthlyAmb(im)%ht(nxa*nya),chem%MonthlyAmb(im)%zMid(nxa*nya,nza),STAT=ios )
            IF( ios == 0 .AND. im == 1 )&
            ALLOCATE( chem%Ambient%ht(nxa*nya),chem%Ambient%zMid(nxa*nya,nza),STAT=ios )
            IF( ios /= 0 )THEN
              nError   = RD_ERROR
              eRoutine = 'ReadAmbHeader'
              eMessage = 'Insufficient memory to allocate terrain arrays'
              WRITE(eInform,*)'Size =',nxa*nya,nxa*nya*nza
              GOTO 9999
            END IF
            EXIT
        END SELECT
      END DO
    END IF

    namb = 0

    DO i = 1,nvar3d

      amb_spname = ADJUSTL(var3d(i))

    !--- Find the ambient species in species list

      found = .FALSE.

      DO isp = 1,chem%nSpecies
        IF( TRIM(chem%species(isp)%name) == TRIM(amb_spname) )THEN
          IF( chem%species(isp)%class /= ID_SPECIES_EQUILIBRIUM )THEN
            namb = namb + 1
            chem%MonthlyAmb(im)%ID(isp) = namb
            chem%MonthlyAmb(im)%read(i) = .TRUE.
            !IF( ih == 1 .AND. im == 1 )THEN
            !  WRITE(lun_log,'(A,I4)',IOSTAT=ios)'Recognised 3D ambient species '// &
            !        TRIM(amb_spname)//' as active species ',isp
            !END IF
          !ELSE
            !WRITE(lun_log,'(A)',IOSTAT=ios)'Recognised 3D ambient species '// &
            !                TRIM(amb_spname)//' as equilibrium species (Not used)'
          END IF
          found = .TRUE.
          EXIT
        END IF
      END DO

      IF( ih == 1 .AND. .NOT.found )THEN
        WRITE(lun_log,'(A,I2)',IOSTAT=ios)'3D ambient species '// &
                          TRIM(amb_spname)//' not in species list for month ',im
      END IF
    END DO

    ! Hourly ambient (x+y,z,species,hr) data for each month
    IF( ih == 1 )THEN

      ALLOCATE( chem%MonthlyAmb(im)%HrAmb(nxa*nya,nza,namb,24),STAT=ios )
      IF( ios /= 0 )THEN
        nError   = RD_ERROR
        eRoutine = 'ReadAmbHeader'
        eMessage = 'Insufficient memory to allocate Monthly HrAmb'
        WRITE(eInform,*)'Size =',nxa*nya*nza*namb*24
        GOTO 9999
      END IF
      IF( im == 1 )NULLIFY(chem%Ambient%HrAmb)

      !=== Check if sufficient ambient data

      IF( (nxa < 1) .OR. (nya < 1) .OR. (nza < 1) )THEN
        nError   = IV_ERROR
        eRoutine = 'ReadAmbHeader'
        eMessage = 'Ambient species data insufficient'
        CALL ReportFileName( eInform,'File=',chem%ambFile )
        GOTO 9999
      END IF

      chem%Ambient%nx = nxa
      chem%Ambient%ny = nya
      chem%Ambient%nz = nza

    END IF

    !--- Read 3d fields into appropriate locations
    ntot = nxa * nya * nza
    iamb = 0
    IF( lFormat )THEN
      DO i = 1,nvar3d
        IF( chem%MonthlyAmb(im)%read(i) )THEN
          iamb = iamb + 1
          READ(lun,'(6(F12.4,1X))',IOSTAT=ios) ((chem%MonthlyAmb(im)%HrAmb(j,k,iamb,ih),j=1,nxa*nya),k=1,nza)
        ELSE
          READ(lun,'(6(F12.4,1X))',IOSTAT=ios) ((dum,j=1,nxa*nya),k=1,nza)
        END IF
        IF( ios /= 0 )EXIT
      END DO
    ELSE
      DO i = 1,nvar3d
        IF( chem%MonthlyAmb(im)%read(i) )THEN
          iamb = iamb + 1
          READ(lun,IOSTAT=ios) ((chem%MonthlyAmb(im)%HrAmb(j,k,iamb,ih),j=1,nxa*nya),k=1,nza)
        ELSE
          READ(lun,IOSTAT=ios) ((dum,j=1,nxa*nya),k=1,nza)
        END IF
        IF( ios /= 0 )EXIT
      END DO
    END IF
    IF(  ios /= 0 )THEN
      nError   = RD_ERROR
      eRoutine = 'RdMonthlyAmb'
      WRITE(eMessage,'("Error reading ambient species 3d data for month ",I2,", hr ",I2," and ambient species ",I3,", vertical layer ",I3)')im,ih,iamb,k
      eAction  = 'Check format of ambient species data'
      CALL ReportFileName( eInform,'File=',chem%ambFile )
      GOTO 9999
    END IF

    !--- Skip 2d fields, except for terrain initially

    ntot = nxa * nya

    IF( lFormat )THEN
      DO i = 1,nvar2d
        IF( i == chem%MonthlyAmb(im)%terrain )THEN
          READ(lun,'(6(F12.0,1X))',IOSTAT=ios) (chem%MonthlyAmb(im)%ht(j),j=1,ntot)
          chem%MonthlyAmb(im)%terrain = 0
        ELSE
          READ(lun,'(6(F12.0,1X))',IOSTAT=ios) (dum,j=1,ntot)
        END IF
        IF( ios /= 0 )EXIT
      END DO
    ELSE
      DO i = 1,nvar2d
        IF( i == chem%MonthlyAmb(im)%terrain )THEN
          READ(lun,IOSTAT=ios) (chem%MonthlyAmb(im)%ht(j),j=1,ntot)
          chem%MonthlyAmb(im)%terrain = 0
        ELSE
          READ(lun,IOSTAT=ios) (dum,j=1,ntot)
        END IF
        IF( ios /= 0 )EXIT
      END DO
    END IF
    IF( ios /= 0 )THEN
      nError   = RD_ERROR
      eRoutine = 'RdMonthlyAmb'
      eMessage = 'Error reading ambient species 2d data'
      CALL ReportFileName( eInform,'File=',chem%ambFile )
      GOTO 9999
    END IF

    !------ Setup terrain factor

    IF( chem%MonthlyAmb(im)%terrain == 0 .AND. BTEST(chem%Ambient%type,ATB_MEDOC) )THEN

      AmbHmin = HUGE(0.)
      DO i = 1,nxa*nya
        AmbHmin = MIN(chem%MonthlyAmb(im)%ht(i),AmbHmin)
      END DO

      DO i = 1,nxa*nya
        chem%MonthlyAmb(im)%ht(i) = chem%MonthlyAmb(im)%ht(i) - AmbHmin
      END DO

      IF( zbtop_med == NOT_SET_R .OR. zbtop_med <= 0. )THEN

        zbtop_med = 0.5*(chem%MonthlyAmb(im)%zGrid(nza)+chem%MonthlyAmb(im)%zGrid(nza-1))

      ELSE

        zbtop_med = zbtop_med - AmbHmin
        fac = 1. - AmbHmin / zbtop_med

        DO i = 1,nza
          chem%MonthlyAmb(im)%zGrid(i)  = chem%MonthlyAmb(im)%zGrid(i)  * fac
        END DO

      END IF

      DO i = 1,nxa*nya
        fac = 1.0 - chem%MonthlyAmb(im)%ht(i)/zbtop_med
        chem%MonthlyAmb(im)%zMid(i,:) = chem%Ambient%zGrid * fac
        chem%MonthlyAmb(im)%ht(i) = chem%MonthlyAmb(im)%ht(i) + AmbHmin
      END DO

    END IF

  END DO

END DO

! Set Ambient values and time

chem%Ambient%n2d     = chem%MonthlyAmb(1)%n2d
chem%Ambient%n3d     = chem%MonthlyAmb(1)%n3d
chem%Ambient%ID      = chem%MonthlyAmb(1)%ID
chem%Ambient%x0      = chem%MonthlyAmb(1)%x0
chem%Ambient%dx      = chem%MonthlyAmb(1)%dx
chem%Ambient%x1      = chem%MonthlyAmb(1)%x1
chem%Ambient%y0      = chem%MonthlyAmb(1)%y0
chem%Ambient%dy      = chem%MonthlyAmb(1)%dy
chem%Ambient%y1      = chem%MonthlyAmb(1)%y1
chem%Ambient%zGrid   = chem%MonthlyAmb(1)%zGrid
chem%Ambient%terrain = chem%MonthlyAmb(1)%terrain
IF( chem%Ambient%terrain == 0 )THEN
  chem%Ambient%ht    = chem%MonthlyAmb(1)%ht
  chem%Ambient%zmid  = chem%MonthlyAmb(1)%zmid
END IF
chem%Ambient%coord = chem%MonthlyAmb(1)%coord

ALLOCATE( chem%Ambient%amb(nxa*nya*nza,namb,2),STAT=ios )
IF( ios /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'ReadAmbHeader'
  eMessage = 'Insufficient memory to allocate data arrays'
  WRITE(eInform,*)'Size =',nxa,nya,nza,namb
  GOTO 9999
END IF

chem%Ambient%it1   = 1
chem%Ambient%time1 = -1.
CALL GetMonthDayAmb( chem%Ambient%it1,chem%Ambient%time1 )

9999 CONTINUE

IF( ALLOCATED(Var3d) )DEALLOCATE( Var3d,STAT=ios )
IF( ALLOCATED(Var2d) )DEALLOCATE( Var2d,STAT=ios )
CLOSE(lun,IOSTAT=ios)

RETURN

9998 CONTINUE

nError   = RD_ERROR
eRoutine = 'RdMonthlyAmb'
eMessage = 'Error reading ambient species input file header'
CALL ReportFileName( eInform,'File=',errfile )
GOTO 9999

END

!=======================================================================

SUBROUTINE GetMonthDayAmb( it,ambTime )

! This subroutine is equivalent to ReadAmbFields( it ) as it
! sets chem%Ambient%amb(:,:,it) array. If ambtime > 0 then
! it finds the hour at the current time

USE scipuff_fi
USE error_fi
USE chem_fi
USE files_fi

IMPLICIT NONE

INTEGER, INTENT( IN    ) :: it
REAL   , INTENT( INOUT ) :: ambTime

INTEGER j, k, ip
INTEGER jul, days
INTEGER iday, imonth, iyear, ihour
INTEGER nxa, nya, nza, nxya

LOGICAL, EXTERNAL :: leap_year
INTEGER, EXTERNAL :: julian_day, days_in_year

IF( year_start /= NOT_SET_I )THEN
  days  = INT((t + tstart*3600.)/86400.)
  ihour = INT((t + tstart*3600. - days*86400.)/3600.)
  IF( ambTime > 0 )ihour = ihour + ambTime
  IF( ihour == 0 )THEN
    ihour = 24
    days  = days - 1
  END IF
  jul   = jul_start + days
  iyear = year_start
  CALL julian_ymd( jul,iyear,imonth,iday )
ELSE
  nError   = RD_ERROR
  eRoutine = 'GetMonthDayAmb'
  eMessage = 'Error:  Year_start not set'
  eAction  = 'Must set start year to use Monthly ambient data'
  eInform  = ''
  GOTO 9999
END IF

!=== Set ambient time if requested
IF( ambTime < 0 )THEN
  ! Set time1 to nearest hour less than t.
  ambTime = MAX(days*24. + ihour - tstart,0.)*3600.
  WRITE(lun_log,'("Initializing ambient concentration start time to ",F13.5," hrs")')ambTime/3600.
END IF

nxa  = chem%MonthlyAmb(imonth)%nx
nya  = chem%MonthlyAmb(imonth)%ny
nza  = chem%MonthlyAmb(imonth)%nz
nxya =  nxa*nya

DO k = 1,nza
  DO j = 1,nxya
    ip = (k-1)*nxya + j
    chem%Ambient%amb(ip,:,it) = chem%MonthlyAmb(imonth)%HrAmb(j,k,:,ihour)
  END DO
END DO

WRITE(lun_log,'("Setting ambient to concentrations values from month ",I2," and hour ",I2)')imonth, ihour

MonthDay(it,1) = imonth*100 + ihour
MonthDay(it,2) = 0

9999 CONTINUE

RETURN
END

