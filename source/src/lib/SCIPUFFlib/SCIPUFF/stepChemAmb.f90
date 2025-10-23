!------------------------------------------------------------------------------
SUBROUTINE InitChemSfcFlux()

!--- Initialize ambient surface flux from input file

USE chem_fi
USE error_fi
USE files_fi
USE default_fd
USE scipuff_fi, ONLY: restart

IMPLICIT NONE

INTEGER ios

!====   Open ambient surface flux input data file

OPEN(UNIT=chem%lunSfcFlx,FILE=TRIM(chem%sFlxFile),STATUS='OLD',IOSTAT=ios)
IF( ios /= 0 )THEN
  nError   = OP_ERROR
  eRoutine = 'InitChemSfcFlux'
  eMessage = 'Error opening ambient species surface flux input data file'
  CALL ReportFileName( eInform,'File=',chem%sFlxFile )
  eAction  = 'Make sure file exists'
  GOTO 9999
END IF

CALL ReadSFluxHeader()
IF( nError /= NO_ERROR )GOTO 9999

CALL InitSFluxFields()
IF( nError /= NO_ERROR )GOTO 9999

9999 CONTINUE
RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE ReadSFluxHeader()

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

REAL    dxa, dya, x0a, y0a, xlat0, xlon0, dum
LOGICAL found, lFormat

CHARACTER(8)   fflag, cdum
CHARACTER(16)  amb_spname
CHARACTER(PATH_MAXLENGTH) errfile

CHARACTER(8), DIMENSION(:), ALLOCATABLE :: Var2d, Var3d

REAL,    EXTERNAL :: SetAmbTime

lun = chem%lunSfcFlx

errfile = chem%sFlxFile


!===  Read header data

READ(lun,'(A8)',IOSTAT=ios) fflag
IF( ios /= 0 )GOTO 9998

chem%sFlux%type = 0

IF( fflag == 'FFFFFFFF' )THEN
  chem%sFlux%type = IBSET(chem%sFlux%type,ATB_MEDOC)
  chem%sFlux%type = IBCLR(chem%sFlux%type,ATB_BINARY)
ELSE
  !====   Reopen ambient surface flux file in unformatted form
  CLOSE(lun)
  OPEN(UNIT=lun,FILE=TRIM(chem%sFlxFile),STATUS='OLD',FORM='UNFORMATTED',IOSTAT=ios)
  IF( ios /= 0 )THEN
    nError   = OP_ERROR
    eRoutine = 'ReadSFluxHeader'
    eMessage = 'Error opening unformatted ambient species surface flux file'
    CALL ReportFileName( eInform,'File=',chem%sFlxFile )
    eAction  = 'Make sure file exists'
    GOTO 9999
  END IF

  READ(lun,IOSTAT=ios) fflag
  IF( ios /= 0 )GOTO 9998
  IF (fflag == 'BBBBBBBB') THEN
    chem%sFlux%type = IBSET(chem%sFlux%type,ATB_MEDOC)
    chem%sFlux%type = IBSET(chem%sFlux%type,ATB_BINARY)
  ELSE
    nError   = RD_ERROR
    eRoutine = 'ReadSFluxHeader'
    eMessage = 'Formatted or unformatted MEDOC file type expected'
    CALL ReportFileName( eInform,'File=',chem%sFlxFile )
    eAction  = 'First record must be FFFFFFFF or BBBBBBBB'
    GOTO 9999
  END IF
END IF

IF( BTEST(chem%sFlux%type,ATB_BINARY) )THEN
  lFormat = .FALSE.
ELSE
  lFormat = .TRUE.
END IF

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

!=== Set ambient time

chem%sFlux%time1 = SetAmbTime( iday,imonth,iyear,ihour,imin,isec )

!=== Check if sufficient ambient data

IF( (nxa < 1) .OR. (nya < 1) )THEN
  nError   = IV_ERROR
  eRoutine = 'ReadSFluxHeader'
  eMessage = 'Ambient species surface flux data insufficient'
  CALL ReportFileName( eInform,'File=',chem%sFlxFile )
  GOTO 9999
END IF

chem%sFlux%nx = nxa
chem%sFlux%ny = nya
chem%sFlux%nz = nza

IF( lFormat )THEN
  READ(lun,'(6(I12,1X))',IOSTAT=ios) (idum,i=1,6)
ELSE
  READ(lun,IOSTAT=ios) (idum,i=1,6)
ENDIF
IF( ios /= 0 )GOTO 9998

IF( lFormat )THEN
  READ(lun,'(6(I12,1X))',IOSTAT=ios) (idum,i=1,3)
ELSE
  READ(lun,IOSTAT=ios) (idum,i=1,3)
ENDIF
IF( ios /= 0 )GOTO 9998

IF( lFormat )THEN
  READ(lun,'(6(F12.4,1X))',IOSTAT=ios) (dum,i=1,nza),dxa,dya, &
                                       x0a,y0a,xlat0,xlon0,(dum,i=1,4),dum
ELSE
  READ(lun,IOSTAT=ios) (dum,i=1,nza),dxa,dya,x0a,y0a,xlat0,xlon0,(dum,i=1,4),dum
ENDIF
IF( ios /= 0 )GOTO 9998

chem%sFlux%x0 = x0a
chem%sFlux%y0 = y0a

chem%sFlux%dx = dxa
chem%sFlux%dy = dya

chem%sFlux%x1 = x0a + FLOAT(nxa-1)*dxa
chem%sFlux%y1 = y0a + FLOAT(nya-1)*dya

chem%sFlux%n2d = nvar2d
chem%sFlux%n3d = nvar3d

ALLOCATE( Var3d(MAX(1,nvar3d)),Var2d(nvar2d),STAT=ios )
IF( ios /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'ReadSFluxHeader'
  eMessage = 'Insufficient memory to allocate variable names'
  WRITE(eInform,*)'Size =',MAX(1,nvar2d),MAX(1,nvar3d)
  GOTO 9999
END IF

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

ALLOCATE( chem%sFlux%sID(chem%nSpecies),chem%sFlux%sRead(nvar2d),STAT=ios )
IF( ios /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'ReadSFluxHeader'
  eMessage = 'Insufficient memory to allocate index array'
  WRITE(eInform,*)'Size =',chem%nSpecies,nvar2d
  GOTO 9999
END IF

chem%sFlux%sID   = 0
chem%sFlux%sRead = .FALSE.

chem%sFlux%nskip = 4 + INT((10+nza)/6 + 1) + INT((ndum+2*nvar2d+2*nvar3d-1)/6 + 1)

iamb = 0

DO i = 1,nvar2d

  amb_spname = ADJUSTL(var2d(i))

  !--- Find the ambient species in species list

  found = .FALSE.

  DO isp = 1,chem%nSpecies
    IF( TRIM(chem%species(isp)%name) == TRIM(amb_spname) )THEN
      IF( chem%species(isp)%class /= ID_SPECIES_EQUILIBRIUM )THEN
        iamb = iamb + 1
        chem%sFlux%sID(isp) = iamb
        chem%sFlux%sRead(i) = .TRUE.
        WRITE(lun_log,'(A,I4)',IOSTAT=ios)'Recognised ambient surface flux species '// &
                            TRIM(amb_spname)//' as active species ',isp
      ELSE
        WRITE(lun_log,'(A)',IOSTAT=ios)'Recognised ambient surface flux species '// &
                        TRIM(amb_spname)//' as equilibrium species (Not used)'
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

ALLOCATE( chem%sFlux%sflx(nxa*nya,iamb,2),STAT=ios )
IF( ios /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'ReadSFluxHeader'
  eMessage = 'Insufficient memory to allocate data arrays'
  WRITE(eInform,*)'Size =',nxa,nya,iamb
  GOTO 9999
END IF

chem%sFlux%it1   = 1
chem%sFlux%time2 = NOT_SET_R

CALL ReadSFluxFields( 1 )

9999 CONTINUE

IF( ALLOCATED(Var3d) )DEALLOCATE( Var3d,STAT=ios )
IF( ALLOCATED(Var2d) )DEALLOCATE( Var2d,STAT=ios )

RETURN

9998 CONTINUE

nError   = RD_ERROR
eRoutine = 'ReadSFluxHeader'
eMessage = 'Error reading ambient species input file header'
CALL ReportFileName( eInform,'File=',errfile )
GOTO 9999

END

!------------------------------------------------------------------------------

SUBROUTINE ReadSFluxFields( n )

!--- Reads ambient species surface flux fields, assumes file is positioned at start of 3d data

USE chem_fi
USE error_fi
USE files_fi
USE default_fd

IMPLICIT NONE

INTEGER, INTENT( IN ) :: n

INTEGER i, j, iamb, lun, ntot, ios
REAL    dum
LOGICAL lFormat

IF( chem%sFlux%time2 == NOT_SET_R )THEN
  WRITE(lun_log,'("Reading ambient surface flux file ",A," at time ",1PG11.4,"hrs.")',IOSTAT=ios)&
      TRIM(chem%sFlxFile),chem%sFlux%time1/3600.
ELSE
  WRITE(lun_log,'("Reading ambient surface flux file ",A," at time ",1PG11.4,"hrs.")',IOSTAT=ios)&
      TRIM(chem%sFlxFile),chem%sFlux%time2/3600.
ENDIF
IF( ios /= 0 ) THEN
  nError   = WR_ERROR
  eRoutine = 'ReadSFluxFields'
  eMessage = 'Error writing ambient surface flux time to log file'
  GO TO 9999
END IF

lun  = chem%lunSfcFlx

IF( BTEST(chem%sFlux%type,ATB_BINARY) )THEN
  lFormat = .FALSE.
ELSE
  lFormat = .TRUE.
END IF

!--- Skip 3d fields
ntot = chem%sFlux%nx * chem%sFlux%ny * chem%sFlux%nz
IF( lFormat )THEN
  DO i = 1,chem%sFlux%n3d
    READ(lun,'(6(F12.4,1X))',IOSTAT=ios) (dum,j=1,ntot)
    IF( ios /= 0 )EXIT
  END DO
ELSE
  DO i = 1,chem%sFlux%n3d
    READ(lun,IOSTAT=ios) (dum,j=1,ntot)
    IF( ios /= 0 )EXIT
  END DO
END IF
IF( ios /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'ReadSFluxFields'
  eMessage = 'Error reading ambient species surface flux 3d data'
  CALL ReportFileName( eInform,'File=',chem%sFlxFile )
  GOTO 9999
END IF

ntot = chem%sFlux%nx * chem%sFlux%ny
iamb = 0
IF( lFormat )THEN
  DO i = 1,chem%sFlux%n2d
    IF( chem%sFlux%sRead(i) )THEN
      iamb = iamb + 1
      READ(lun,'(6(F12.4,1X))',IOSTAT=ios) (chem%sFlux%sflx(j,iamb,n),j=1,ntot)
    ELSE
      READ(lun,'(6(F12.0,1X))',IOSTAT=ios) (dum,j=1,ntot)
    END IF
    IF( ios /= 0 )EXIT
  END DO
ELSE
  DO i = 1,chem%sFlux%n2d
    IF( chem%sFlux%sRead(i) )THEN
      iamb = iamb + 1
      READ(lun,IOSTAT=ios) (chem%sFlux%sflx(j,iamb,n),j=1,ntot)
    ELSE
      READ(lun,IOSTAT=ios) (dum,j=1,ntot)
    END IF
    IF( ios /= 0 )EXIT
  END DO
END IF
IF( ios /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'ReadSFluxFields'
  eMessage = 'Error reading ambient species surface flux 2d data'
  CALL ReportFileName( eInform,'File=',chem%sFlxFile )
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE InitSFluxFields()

USE chem_fi
USE scipuff_fi
USE default_fd

IMPLICIT NONE

IF( chem%sFlux%time1 > t )THEN
  chem%sFlux%time2 = chem%sFlux%time1
  chem%sFlux%it1   = 0    !Flag for only one time (in field-1)
  GOTO 9999
END IF

CALL getNextSFluxTime()
IF( nError /= NO_ERROR )GOTO 9999

IF( chem%sFlux%time2 /= NOT_SET_R )THEN
  CALL ReadSFluxFields( 2 )
  IF( nError /= NO_ERROR )GOTO 9999
END IF

CALL UpdateSFluxFields()
IF( nError /= NO_ERROR )GOTO 9999

9999 CONTINUE

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE UpdateSFluxFields()

USE chem_fi
USE scipuff_fi

IMPLICIT NONE

INTEGER it2, lun

IF( chem%sFlux%time2 == NOT_SET_R )GOTO 9999

lun = chem%lunAmb

DO WHILE( chem%sFlux%time2 < t .AND. chem%sFlux%time2 /= NOT_SET_R )

  chem%sFlux%time1 = chem%sFlux%time2

  IF( chem%sFlux%it1 == 0 )THEN
    chem%sFlux%it1 = 1
  ELSE
    chem%sFlux%it1 = 3 - chem%sFlux%it1   !Swap field pointer
  END IF

  CALL getNextSFluxTime()
  IF( nError /= NO_ERROR )GOTO 9999

  IF( chem%sFlux%time2 /= NOT_SET_R )THEN

    it2 = 3 - chem%sFlux%it1

    CALL ReadSFluxFields( it2 )
    IF( nError /= NO_ERROR )GOTO 9999

  END IF

END DO

9999 CONTINUE

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE getNextSFluxTime()

USE chem_fi
USE error_fi
USE default_fd

IMPLICIT NONE

INTEGER i, lun, ios
INTEGER iday, imonth, iyear, ihour, imin, isec

CHARACTER(8)   fflag
LOGICAL        lFormat

REAL, EXTERNAL :: SetAmbTime

lun = chem%lunSfcFlx

lFormat = .TRUE.
IF( BTEST(chem%sFlux%type,ATB_BINARY) )lFormat = .FALSE.

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
  ENDIF
END IF
IF( ios == 0 )THEN
  IF( lFormat )THEN
    READ(lun,'(6(I12,1X))',IOSTAT=ios) iday,imonth,iyear,ihour,imin,isec
  ELSE
    READ(lun,IOSTAT=ios) iday,imonth,iyear,ihour,imin,isec
  ENDIF
END IF

IF( lFormat )THEN
  DO i = 1,chem%sFlux%nskip
    READ(lun,*,IOSTAT=ios )
    IF( ios /= 0 )EXIT
  END DO
ELSE
  DO i = 1,chem%sFlux%nskip
    READ(lun,*,IOSTAT=ios )
    IF( ios /= 0 )EXIT
  END DO
END IF
IF( ios == 0 )THEN

  chem%sFlux%time2 = SetAmbTime( iday,imonth,iyear,ihour,imin,isec )

ELSE IF( ios < 0 )THEN

  chem%sFlux%time2 = NOT_SET_R

ELSE

  nError   = RD_ERROR
  eRoutine = 'getNextSFluxTime'
  eMessage = 'Error reading ambient species surface flux file header'
  CALL ReportFileName( eInform,'File=',chem%sFlxFile )
  GOTO 9999

END IF

9999 CONTINUE

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE InitChemStepAmb( lPlot )

!--- Initialize stepped ambient file

USE chem_fi
USE error_fi
USE files_fi
USE default_fd
USE scipuff_fi

IMPLICIT NONE

LOGICAL, INTENT( IN ) :: lPlot
INTEGER ios
INTEGER nxa, nya, nza

IF( chem%lAmbFile )THEN

  CALL InitChemAmb()
  IF( nError /= NO_ERROR )GOTO 9999

  IF( chem%Ambient%time2 == NOT_SET_R )&
    chem%Ambient%InterpAmb = .FALSE.

ELSE

  !--- Set a single point in middle of domain for ambient stepping

  chem%Ambient%nx = 1
  chem%Ambient%ny = 1
  chem%Ambient%nz = 1

  chem%Ambient%x0 = 0.5*(xmin + xmax)
  chem%Ambient%y0 = 0.5*(ymin + ymax)

  ALLOCATE( chem%Ambient%zGrid(1),STAT=ios )
  IF( ios /= 0 )THEN
    nError   = RD_ERROR
    eRoutine = 'InitChemAmb'
    eMessage = 'Error allocating vertical grid'
    WRITE(eInform,*)'Size =',1
    GOTO 9999
  END IF

  chem%Ambient%zGrid(1) =  10.

  chem%Ambient%dx = xmax - xmin
  chem%Ambient%dy = ymax - ymin

  chem%Ambient%x1 = 0.5*(xmin + xmax)
  chem%Ambient%y1 = 0.5*(xmin + xmax)

  ALLOCATE( chem%Ambient%ID(chem%nSpecies),STAT=ios )
  IF( ios /= 0 )THEN
    nError   = RD_ERROR
    eRoutine = 'InitChemAmb'
    eMessage = 'Error allocating ID'
    WRITE(eInform,*)'Size =',chem%nSpecies
    GOTO 9999
  END IF

  chem%Ambient%ID        = -1
  chem%Ambient%terrain   = -1
  chem%Ambient%time1     = NOT_SET_R
  chem%Ambient%time2     = NOT_SET_R
  chem%Ambient%tStepAmb  = t

ENDIF

OPEN(UNIT=lun_amr,FILE=TRIM(file_amr),STATUS='UNKNOWN',FORM='FORMATTED',IOSTAT=ios)
IF( ios /= 0 )THEN
  nError   = OP_ERROR
  eRoutine = 'InitChemStepAmb'
  eMessage = 'Error opening formatted stepped ambient species file'
  CALL ReportFileName( eInform,'File=',TRIM(file_amr) )
  eAction  = 'Make sure file exists'
  GOTO 9999
END IF

nxa = chem%Ambient%nx
nya = chem%Ambient%ny
nza = chem%Ambient%nz

ALLOCATE( chem%Ambient%stepAmb(nxa*nya,nza,chem%nSpecies),STAT=ios )
IF( ios /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'ReadSFluxHeader'
  eMessage = 'Insufficient memory to allocate data arrays'
  WRITE(eInform,*)'Size =',nxa,nya,nza,chem%nSpecies
  GOTO 9999
END IF

IF( create .AND. .NOT. lPlot )THEN

  CALL SetChemStepAmb( .FALSE. )
  IF( nError /= NO_ERROR )GOTO 9999

ELSE

  CALL ReadStepAmbFields( )
  IF( nError /= NO_ERROR )GOTO 9999

END IF

9999 CONTINUE

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE SetChemStepAmb( lmet )

!--- Set stepped ambient concentration from Ambient%amb or species%ambient

USE chem_fi
USE error_fi
USE files_fi
USE default_fd
USE scipuff_fi

IMPLICIT NONE

LOGICAL, INTENT( IN ) :: lmet           !Met fields are available

INTEGER, DIMENSION(2) :: it
REAL,    DIMENSION(2) :: fact

INTEGER j, iamb
INTEGER ix, iy, iz, isp, ixy, ixyz
INTEGER nxa, nya, nza, npt

REAL amb

REAL, EXTERNAL :: SetFixedAmb

nxa = chem%Ambient%nx
nya = chem%Ambient%ny
nza = chem%Ambient%nz

chem%Ambient%tStepAmb = t

IF( chem%Ambient%InterpAmb )THEN

  IF( chem%Ambient%it1 == 0 .OR. chem%Ambient%time2 == NOT_SET_R )THEN
    IF( chem%Ambient%it1 == 0 )THEN
      it(1)  = 1                 ! Use the earliest time available
    ELSE
      it(1)  = chem%Ambient%it1  ! Start stepping from chem%Ambient%time1
      chem%Ambient%InterpAmb = .FALSE.
      chem%Ambient%tStepAmb = chem%Ambient%time1
    END IF
    npt      = 1
    fact(1)  = 1.0
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

!--- Set stepAmb from ambient array
DO isp = 1,chem%nSpecies
  iamb = chem%Ambient%ID(isp)
  IF( iamb > 0 )THEN
    DO iz = 1,nza
      DO iy = 1,nya
        DO ix = 1,nxa
          ixy  = (iy-1)*nxa + ix
          ixyz = (iz-1)*nya*nxa + ixy
          amb  = 0.0
          DO j = 1,npt
            amb = amb + fact(j)*chem%Ambient%amb(ixyz,iamb,it(j))
          END DO
          chem%Ambient%stepAmb(ixy,iz,isp) = amb
        END DO
      END DO
    END DO
  ELSE
    chem%Ambient%stepAmb(:,:,isp) = SetFixedAmb( chem%species(isp)%ambient,lmet )
    IF( nError /= NO_ERROR )GOTO 9999
  END IF
END DO

9999 CONTINUE

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE WriteChemStepAmb()

!--- Reads stepped ambient species fields

USE chem_fi
USE error_fi
USE files_fi
USE default_fd
USE diagnostics_fi

IMPLICIT NONE

INTEGER idum, ios
INTEGER i, iz, isp, ixy
INTEGER iday, imonth, iyear, ihour, imin, isec
INTEGER nxa, nya, nza, nxy, ndum, nvar3d

REAL dum, x0a, y0a, dxa, dya
REAL xlat, xlon, zbtop_amr

LOGICAL lFormat,isOpen

CHARACTER(8)  fflag, cdum, isUNF
CHARACTER(80) cmsg,cmsg2,cmsg3

IF( .NOT. chem%lStepAmb )GOTO 9999

cmsg  = 'Writing stepped ambient file'
cmsg2 = CHAR(0)
cmsg3 = CHAR(0)
CALL write_progress( cmsg,cmsg2,cmsg3 )
IF( nError /= NO_ERROR )GOTO 9999

WRITE(lun_log,'("Writing stepped ambient file ",A," at time ",1PG11.4,"hrs.")',IOSTAT=ios)&
      TRIM(file_amr),chem%Ambient%tStepAmb/3600.
IF( ios /= 0 ) THEN
  nError   = WR_ERROR
  eRoutine = 'WriteChemStepAmb'
  eMessage = 'Error writing ambient time to log file'
  GO TO 9999
END IF

! If interpolating skip writing species names and stepamb
IF( chem%Ambient%InterpAmb )THEN
  nvar3d = 0
ELSE
  nvar3d = chem%nSpecies
END IF

nxa = chem%Ambient%nx
nya = chem%Ambient%ny
nza = chem%Ambient%nz
nxy = nxa * nya

x0a = chem%Ambient%x0
y0a = chem%Ambient%y0

dxa = chem%Ambient%dx
dya = chem%Ambient%dy

idum = -99
ndum = 0
dum  = -99.
xlat = -99.
xlon = -99.
zbtop_amr = 1000.

INQUIRE( UNIT=lun_amr,OPENED=isOpen,IOSTAT=ios,UNFORMATTED=isUNF )
IF( ios /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'WriteChemStepAmb'
  eMessage = 'Error in inquire for stepped ambient restart file'
  CALL ReportFileName( eInform,'File=',file_amr )
  GOTO 9999
END IF

IF( isOpen )THEN
  IF( isUNF == 'YES' )THEN
    lFormat = .FALSE.
    fflag   = 'BBBBBBBB'
  ELSEIF ( isUNF == 'NO' )THEN
    lFormat = .TRUE.
    fflag   = 'FFFFFFFF'
  ELSE
    nError   = RD_ERROR
    eRoutine = 'WriteChemStepAmb'
    eMessage = 'Error in inquire for stepped ambient restart file'
    CALL ReportFileName( eInform,'File=',file_amr )
    GOTO 9999
  ENDIF
ELSE
  nError   = RD_ERROR
  eRoutine = 'WriteChemStepAmb'
  eMessage = 'Error as stepped ambient restart file not open'
  CALL ReportFileName( eInform,'File=',file_amr )
  GOTO 9999
END IF

cdum  = 'DUMMY'

IF( lFormat )THEN
  WRITE(lun_amr,'(A8)',IOSTAT=ios) fflag
ELSE
  WRITE(lun_amr,IOSTAT=ios) fflag
END IF
IF( ios /= 0 )GOTO 9998

IF( lFormat )THEN
  WRITE(lun_amr,'(A8)',IOSTAT=ios) 'SCICHEM'
ELSE
  WRITE(lun_amr,IOSTAT=ios) 'SCICHEM'
END IF
IF( ios /= 0 )GOTO 9998

CALL GetAmbTime( chem%Ambient%tStepAmb, iday,imonth,iyear,ihour,imin,isec )

IF( lFormat )THEN
  WRITE(lun_amr,'(6(I12,1X))',IOSTAT=ios) iday,imonth,iyear,ihour,imin,isec
ELSE
  WRITE(lun_amr,IOSTAT=ios) iday,imonth,iyear,ihour,imin,isec
END IF
IF( ios /= 0 )GOTO 9998

IF( lFormat )THEN
  WRITE(lun_amr,'(6(I12,1X))',IOSTAT=ios) (idum,i=1,6)
ELSE
  WRITE(lun_amr,IOSTAT=ios) (idum,i=1,6)
END IF
IF( ios /= 0 )GOTO 9998

IF( lFormat )THEN
  WRITE(lun_amr,'(6(I12,1X))',IOSTAT=ios) nxa,nya,nza,ndum,nvar3d,0
ELSE
  WRITE(lun_amr,IOSTAT=ios) nxa,nya,nza,ndum,nvar3d,0
END IF
IF( ios /= 0 )GOTO 9998

IF( lFormat )THEN
  WRITE(lun_amr,'(6(I12,1X))',IOSTAT=ios) (idum,i=1,6)
ELSE
  WRITE(lun_amr,IOSTAT=ios) (idum,i=1,6)
END IF
IF( ios /= 0 )GOTO 9998

IF( lFormat )THEN
  WRITE(lun_amr,'(6(I12,1X))',IOSTAT=ios) (idum,i=1,3)
ELSE
  WRITE(lun_amr,IOSTAT=ios) (idum,i=1,3)
END IF
IF( ios /= 0 )GOTO 9998

IF( lFormat )THEN
  WRITE(lun_amr,'(6(1ES12.5,1X))',IOSTAT=ios) (chem%Ambient%zGrid(i),i=1,nza),dxa,dya, &
                                      x0a,y0a,xlat,xlon,(dum,i=1,4),zbtop_amr
ELSE
  WRITE(lun_amr,IOSTAT=ios) (chem%Ambient%zGrid(i),i=1,nza),dxa,dya, &
                                      x0a,y0a,xlat,xlon,(dum,i=1,4),zbtop_amr
END IF
IF( ios /= 0 )GOTO 9998

IF( lFormat )THEN
  WRITE(lun_amr,'(6(A8,1X))',IOSTAT=ios) (chem%species(i)%name,i=1,nvar3d),&
                                       (cdum,i=1,nvar3d)
ELSE
  WRITE(lun_amr,IOSTAT=ios) (chem%species(i)%name,i=1,nvar3d),&
                                       (cdum,i=1,nvar3d)
END IF
IF( ios /= 0 )GOTO 9998

IF( lFormat )THEN
  DO isp = 1,nvar3d
    WRITE(lun_amr,'(6(1ES12.5,1X))',IOSTAT=ios) ((chem%Ambient%stepAmb(ixy,iz,isp),ixy=1,nxy),iz=1,nza)
    IF( ios /= 0 )EXIT
  END DO
ELSE
  DO isp = 1,nvar3d
    WRITE(lun_amr,IOSTAT=ios) ((chem%Ambient%stepAmb(ixy,iz,isp),ixy=1,nxy),iz=1,nza)
    IF( ios /= 0 )EXIT
  END DO
END IF

9998 CONTINUE

IF( ios /= 0 )THEN
  nError   = WR_ERROR
  eRoutine = 'WriteChemStepAmb'
  eMessage = 'Error writing stepped ambient species data'
  CALL ReportFileName( eInform,'File=',file_amr )
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE GetAmbTime( ambTime, iday,imonth,iyear,ihour,imin,isec )

USE scipuff_fi

IMPLICIT NONE

REAL, INTENT( IN ) :: ambTime

INTEGER jul, iday,imonth,iyear,ihour,imin,isec

iyear = year_start

jul = INT((ambTime + tstart*3600.)/86400.) + jul_start

CALL julian_ymd( jul,iyear,imonth,iday )

ihour = INT((ambTime  - FLOAT(jul-jul_start)*86400.)/3600.)
imin  = INT((ambTime  - FLOAT(jul-jul_start)*86400. - FLOAT(ihour)*3600.)/60.)
isec  = INT( ambTime  - FLOAT(jul-jul_start)*86400. - FLOAT(ihour)*3600. - FLOAT(imin)*60.)


RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE ReadStepAmbFields()

!--- Reads ambient species fields

USE chem_fi
USE error_fi
USE files_fi
USE default_fd
USE scipuff_fi
USE diagnostics_fi

IMPLICIT NONE

INTEGER idum, ios, alloc_stat
INTEGER i, iz, isp, ixy
INTEGER iday, imonth, iyear, ihour, imin, isec
INTEGER nxa, nya, nza, nxy, ndum, nvar2d, nvar3d

REAL dum, x0a, y0a, dxa, dya
REAL xlat, xlon, zbtop_amr

LOGICAL lFormat

CHARACTER(8)  fflag, cdum
CHARACTER(80) cmsg,cmsg2,cmsg3
CHARACTER(16), DIMENSION(:), ALLOCATABLE :: spNames

REAL, EXTERNAL :: SetAmbTime

cmsg  = 'Reading stepped ambient file'
cmsg2 = CHAR(0)
cmsg3 = CHAR(0)
CALL write_progress( cmsg,cmsg2,cmsg3 )
IF( nError /= NO_ERROR )GOTO 9999

cdum = 'DUMMY'

lFormat = .TRUE.

READ(lun_amr,'(A8)',IOSTAT=ios) fflag
IF( ios /= 0 )GOTO 9998

IF( fflag /= 'FFFFFFFF' )THEN

  !====   Reopen ambient restart file in unformatted form
  CLOSE(lun_amr)

  OPEN(UNIT=lun_amr,FILE=TRIM(file_amr),STATUS='OLD',FORM='UNFORMATTED',IOSTAT=ios)
  IF( ios /= 0 )THEN
    nError   = OP_ERROR
    eRoutine = 'ReadStepAmbFields'
    eMessage = 'Error opening unformatted ambient species restart file'
    CALL ReportFileName( eInform,'File=',TRIM(file_amr) )
    eAction  = 'Make sure file exists'
    GOTO 9999
  END IF

  READ(lun_amr,IOSTAT=ios) fflag
  IF( ios /= 0 )GOTO 9998

  IF (fflag /= 'BBBBBBBB') THEN
    nError   = RD_ERROR
    eRoutine = 'ReadStepAmbFields'
    eMessage = 'Error in fflag'
    eInform  = 'fflag must be FFFFFFFF'
    GOTO 9999
  ELSE
    lFormat = .TRUE.
  END IF

END IF

REWIND(lun_amr,IOSTAT=ios)

DO

  IF( lFormat )THEN
    READ(lun_amr,'(A8)',IOSTAT=ios) fflag
  ELSE
    READ(lun_amr,IOSTAT=ios) fflag
  END IF
  IF( ios /= 0 )GOTO 9998

  IF( lFormat )THEN
    READ(lun_amr,'(A8)',IOSTAT=ios) cdum
  ELSE
    READ(lun_amr,IOSTAT=ios) cdum
  END IF
  IF( ios /= 0 )GOTO 9998

  IF( lFormat )THEN
    READ(lun_amr,'(6(I12,1X))',IOSTAT=ios) iday,imonth,iyear,ihour,imin,isec
  ELSE
    READ(lun_amr,IOSTAT=ios) iday,imonth,iyear,ihour,imin,isec
  END IF
  IF( ios /= 0 )GOTO 9998

  chem%Ambient%tStepAmb = SetAmbTime( iday,imonth,iyear,ihour,imin,isec )

  WRITE(lun_log,'("Reading stepped ambient file ",A," at time ",1PG11.4,"hrs.")',IOSTAT=ios)&
        TRIM(file_amr),chem%Ambient%tStepAmb/3600.
  IF( ios /= 0 ) THEN
    nError   = WR_ERROR
    eRoutine = 'ReadStepAmbFields'
    eMessage = 'Error writing ambient time to log file'
    GO TO 9999
  END IF

  IF( lFormat )THEN
    READ(lun_amr,'(6(I12,1X))',IOSTAT=ios) (idum,i=1,6)
  ELSE
    READ(lun_amr,IOSTAT=ios) (idum,i=1,6)
  END IF
  IF( ios /= 0 )GOTO 9998

  IF( lFormat )THEN
    READ(lun_amr,'(6(I12,1X))',IOSTAT=ios) nxa,nya,nza,ndum,nvar3d,nvar2d
  ELSE
    READ(lun_amr,IOSTAT=ios) nxa,nya,nza,ndum,nvar3d,nvar2d
  END IF
  IF( ios /= 0 )GOTO 9998

  IF( nxa /= chem%Ambient%nx .OR. nya /= chem%Ambient%ny &
      .OR. nza /= chem%Ambient%nz )THEN
    nError   = RD_ERROR
    eRoutine = 'ReadStepAmbFields'
    eMessage = 'Ambient restart file grid dimension do not match ambient file dimensions'
    WRITE(eInform,*)'Size =',nxa,nya,nza,nvar3d,chem%Ambient%nx,&
                            chem%Ambient%ny ,chem%Ambient%nz,chem%nSpecies
    GOTO 9999
  END IF

  IF( nvar3d == 0 )THEN
    WRITE(lun_log,'("Using interpolated values from ambient file ",A," at time ",1PG11.4,"hrs.")',IOSTAT=ios)&
        TRIM(chem%ambFile),chem%Ambient%tStepAmb/3600.
  ELSE IF( nvar3d /= chem%nSpecies )THEN
    nError   = RD_ERROR
    eRoutine = 'ReadStepAmbFields'
    eMessage = 'Incorrect number of 3d variable in ambient restart file'
    WRITE(eInform,*)'Size =',nvar3d,chem%nSpecies
    GOTO 9999
  END IF

  nxy = nxa * nya

  IF( lFormat )THEN
    READ(lun_amr,'(6(I12,1X))',IOSTAT=ios) (idum,i=1,6)
  ELSE
    READ(lun_amr,IOSTAT=ios) (idum,i=1,6)
  END IF
  IF( ios /= 0 )GOTO 9998

  IF( lFormat )THEN
    READ(lun_amr,'(6(I12,1X))',IOSTAT=ios) (idum,i=1,3)
  ELSE
    READ(lun_amr,IOSTAT=ios) (idum,i=1,3)
  END IF
  IF( ios /= 0 )GOTO 9998

  IF( lFormat )THEN
    READ(lun_amr,'(6(F12.4,1X))',IOSTAT=ios) (chem%Ambient%zGrid(i),i=1,nza),dxa,dya, &
                                              x0a,y0a,xlat,xlon,(dum,i=1,4),zbtop_amr
  ELSE
    READ(lun_amr,IOSTAT=ios) (chem%Ambient%zGrid(i),i=1,nza),dxa,dya, &
                               x0a,y0a,xlat,xlon,(dum,i=1,4),zbtop_amr
  END IF
  IF( ios /= 0 )EXIT

  chem%Ambient%x0 = x0a
  chem%Ambient%y0 = y0a

  chem%Ambient%dx = dxa
  chem%Ambient%dy = dya

  ALLOCATE( spNames(nvar3d),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'ReadStepAmbFields'
    eMessage = 'Error allocating temporary array'
    GOTO 9999
  END IF

  IF( lFormat )THEN
    READ(lun_amr,'(6(A8,1X))',IOSTAT=ios) (spNames(i),i=1,nvar3d),&
                                          (cdum,i=1,nvar3d)
  ELSE
    READ(lun_amr,IOSTAT=ios) (spNames(i),i=1,nvar3d),&
                                          (cdum,i=1,nvar3d)
  END IF
  IF( ios /= 0 )EXIT

  DO i = 1,nvar3d
    IF( TRIM(spNames(i)) /= TRIM(chem%species(i)%name(1:8)) )THEN
      nError   = UK_ERROR
      eRoutine = 'ReadStepAmbFields'
      eMessage = 'Species name mismatch in ambient restart file'
      WRITE(eInform,'("File = ",A," , species no. and name = ",I4,1x,A)')i,TRIM(spNames(i))
      GOTO 9999
    END IF
  END DO

  IF( ALLOCATED(spNames) )DEALLOCATE( spNames,STAT=alloc_stat )

  IF( lFormat )THEN
    DO isp = 1,nvar3d
      READ(lun_amr,'(6(F12.4,1X))',IOSTAT=ios) ((chem%Ambient%stepAmb(ixy,iz,isp),ixy=1,nxy),iz=1,nza)
      IF( ios /= 0 )EXIT
    END DO
  ELSE
    DO isp = 1,nvar3d
      READ(lun_amr,IOSTAT=ios) ((chem%Ambient%stepAmb(ixy,iz,isp),ixy=1,nxy),iz=1,nza)
      IF( ios /= 0 )EXIT
    END DO
  END IF
  IF( ios /= 0 )THEN
    nError   = RD_ERROR
    eRoutine = 'ReadStepAmbFields'
    eMessage = 'Error reading stepped ambient species 3d data'
    CALL ReportFileName( eInform,'File=',file_amr )
    GOTO 9999
  END IF

  IF( ABS(chem%Ambient%tStepAmb - t) < 60. )EXIT

END DO

9998 CONTINUE

IF( ios > 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'ReadStepAmbFields'
  eMessage = 'Error reading stepped ambient species 3d data'
  CALL ReportFileName( eInform,'File=',file_amr )
  GOTO 9999
END IF

IF( ABS(chem%Ambient%tStepAmb - t) >= 60. )THEN
  nError   = RD_ERROR
  eRoutine = 'ReadStepAmbFields'
  WRITE(eMessage,'("Error reading ambient for time = ",1pE13.5," hrs")')t/3600.
  CALL ReportFileName( eInform,'File=',file_amr )
  GOTO 9999
END IF

9999 CONTINUE

IF( ALLOCATED(spNames) )DEALLOCATE( spNames,STAT=alloc_stat )

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE StepChemAmb()

!--- Steps ambient species fields

USE chem_fi
USE AqAer_fi, ONLY: ChemMet_str
USE error_fi
USE files_fi
USE default_fd
USE scipuff_fi
USE diagnostics_fi
USE met_fi


IMPLICIT NONE

INTEGER ios, alloc_stat
INTEGER i, isp
INTEGER ix, iy, iz, ixy, ixyz
INTEGER nxa, nya, nza, nxy
INTEGER nvar3d, npt

REAL x0a, y0a, dxa, dya
REAL xamb, yamb, zamb
REAL t0, t1, dt
REAL spec_mass1,spec_mass2, mass_diff
REAL carea, pscale

LOGICAL leqm_set, lstartAmbStep

INTEGER          , DIMENSION(2) :: it
REAL             , DIMENSION(2) :: fact
REAL, ALLOCATABLE, DIMENSION(:) :: diffz
REAL, ALLOCATABLE, DIMENSION(:) :: fac_dist

CHARACTER(80) cmsg,cmsg2,cmsg3

TYPE( ChemMet_str ) :: chemMet

REAL, EXTERNAL :: SetAmbTime
REAL, EXTERNAL :: SetPrate

lstartAmbStep = .FALSE.

IF( chem%Ambient%InterpAmb )THEN

  CALL UpdateAmbFields()
  IF( nError /= NO_ERROR )GOTO 9999

  IF( chem%Ambient%it1 /= 0 .AND. chem%Ambient%time2 == NOT_SET_R )THEN
    ! Update stepamb from Ambient%amb for stepping
    CALL SetChemStepAmb( .FALSE. )
    lstartAmbStep = .TRUE.
  ELSE
    chem%Ambient%tStepAmb  = t
  END IF

END IF

!--- Step the ambient concentration if not interpolating
IF( .NOT. chem%Ambient%InterpAmb )THEN

  dt = 0.5*delt

  t0 = chem%Ambient%tStepAmb - dt
  t1 = chem%Ambient%tStepAmb + dt

  IF( .NOT. lstartAmbStep .AND. t > t0 .AND. t < t1 )GOTO 9999

  WRITE(cmsg,'("Stepping ambient at ",F13.5," hr")')t/3600.
  cmsg2 = CHAR(0)
  cmsg3 = CHAR(0)
  CALL write_progress( cmsg,cmsg2,cmsg3 )
  IF( nError /= NO_ERROR )GOTO 9999

  nfast     = chem%nFast
  nslow     = chem%nSlow
  ngaseous  = chem%nGaseous
  nequil    = chem%nEquilibrium
  nparticle = chem%nParticle
  nspecies  = nfast + nslow + nparticle
  nspectot  = nspecies + nequil
  nambient  = chem%nSpecies - nspectot

  species => chem%species

  IF( chem%nReactions == 0 )RETURN

  fast    => chem%fast
  slow    => chem%slow
  gaseous => chem%gaseous
  equil   => chem%equil

  nsolve_eq = chem%nSolveEq
  nlin_eq   = chem%nLinearEq
  ndir_eq   = chem%nDirectEq
  nsubs_eq  = chem%nSubstEq

  indx_eq  => chem%IndexEq
  itype_eq => chem%TypeEq
  irow_eq  => chem%RowEq

  rtol = chem%rtol
  volx = 0.

  ResetEq = .FALSE.
  ALLOCATE( pdnrate(nspectot),STAT=alloc_stat )
  IF( nslow > 0 .AND. alloc_stat == 0 )ALLOCATE( ydots(nslow),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    nError   = IV_ERROR
    eRoutine = 'StepChemAmb'
    eMessage = 'Error allocating work arrays'
    GOTO 9999
  END IF

  nza = chem%Ambient%nz

  ! Get ambient surface flux if present
  IF( chem%lSfcFlx .AND.  nza > 1 )THEN

    CALL UpdateSFluxFields()
    IF( nError /= NO_ERROR )GOTO 9999

    IF( chem%sFlux%it1 == 0 .OR. chem%sFlux%time2 == NOT_SET_R )THEN
      it(1)   = 1
      npt     = 1
      fact(1) = 1.0
    ELSE
      it(1)   = chem%sFlux%it1
      it(2)   = 3 - it(1)
      fact(2) = (t1-chem%sFlux%time1)/(chem%sFlux%time2-chem%sFlux%time1)
      fact(2) = MIN(MAX(fact(2),0.),1.)
      fact(1) = 1.0 - fact(2)
      npt     = 2
    END IF

  END IF

  x0a  = chem%Ambient%x0
  y0a  = chem%Ambient%y0

  nvar3d = chem%nSpecies
  nxa    = chem%Ambient%nx
  nya    = chem%Ambient%ny
  nxy    = nxa*nya

  dxa = chem%Ambient%dx
  dya = chem%Ambient%dy

  species => chem%species
  leqm_set = .FALSE.

  ALLOCATE( diffz(nza), STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    nError   = SZ_ERROR
    eRoutine = 'StepChemAmb'
    eMessage = 'Error allocating diffz array'
    WRITE(eInform,'("nza = ",I5)')nza
    GOTO 9999
  END IF

  DO ixy = 1,nxy

    iy   = INT(ixy/nxa-1)
    ix = ixy - (nxa-1)*iy

    xamb = x0a + dxa*(ix-1)
    yamb = y0a + dya*(iy-1)

    diffz = 0.

    DO iz = 1,nza

      zamb = chem%Ambient%zGrid(iz)

      ixyz = nya*nxa*(iz-1) + ixy

      CALL get_met( xamb,yamb,zamb,0.,0.,1 )

      diffz(iz) = difb

      !====   Get stepped ambient concentrations and save initial values

      DO i = 1,nspectot
        species(i)%mass = MAX(0.,chem%Ambient%stepAmb(ixy,iz,i))
        species(i)%msav = species(i)%mass
        species(i)%conc = MAX(0.,chem%Ambient%stepAmb(ixy,iz,i))
        species(i)%csav = species(i)%conc
        species(i)%amb  = 0.
      END DO

      !====   Update rate constants

      zlev = zamb
      CALL UpdateChemRate( .TRUE. )
      IF( nError /= NO_ERROR )GOTO 9999

      IF( chem%lBalance )CALL GetSpeciesMass(spec_mass1)

      !====   Advance gas-phase chemistry for ambient concentration
      CALL StepGasPhase( dt,leqm_set )
      IF( nError /= NO_ERROR )THEN

        WRITE(lun_log,'(A)',IOSTAT=ios) 'Error stepping chemistry for ambient '
        WRITE(lun_log,'(A)',IOSTAT=ios) 'Concentrations at the beginning of stepChemAmb:'
        WRITE(lun_log,'(A)',IOSTAT=ios) 'Species   conc    amb    conc + amb   mass'
        DO i = 1,nspectot
          WRITE(lun_log,'(A8,1P,4E12.4)',IOSTAT=ios)TRIM(species(i)%name),species(i)%csav,species(i)%amb,&
                             species(i)%csav+species(i)%amb,species(i)%msav
        END DO
        WRITE(lun_log,'(A)',IOSTAT=ios) 'Reaction rate constants:'
        DO i = 1,chem%nReactions
          WRITE(lun_log,'(I6,1P,2E12.4)',IOSTAT=ios) i,chem%reaction(i)%k
        END DO

        IF( ios /= 0 )THEN
          nError   = WR_ERROR
          eRoutine = 'StepChem'
          eMessage = 'Error writing SCIPUFF log file'
          CALL ReportFileName( eInform,'File=',file_log )
          GOTO 9999
        END IF

        nError    = NO_ERROR
        nbad_chem = nbad_chem + 1

        CYCLE

      END IF

      IF( nequil > 0 )THEN
        CALL SetChemEquilibrium( .FALSE. )
        IF( nError /= NO_ERROR )THEN
          nError = NO_ERROR
          WRITE(lun_log,'(A)',IOSTAT=ios) 'Error setting equilibrium in StepChem-1 - will continue'
          IF( ios /= 0 )THEN
            nError   = WR_ERROR
            eRoutine = 'StepChemAmb'
            eMessage = 'Error writing SCIPUFF log file'
            CALL ReportFileName( eInform,'File=',file_log )
            GOTO 9999
          END IF
        END IF
      END IF

      carea            = 0.
      pscale           = 0.
      chemMet%tab      = tb                                ! temp(k)
      chemMet%pb       = pb/1013.25                        ! press(atm)
      chemMet%hb       = hb                                ! humidity(g H2O/g dry air)
      chemMet%cldall   = cw                                ! cloud liquid water content(g/m3)
      chemMet%cldbot   = cldbot                            ! cloud bottom (m-agl)
      chemMet%cldtop   = cldtop                            ! cloud top (m-agl)
      chemMet%cldavg   = cw_avg                            ! average cloud liquid water content
      IF( prate == NOT_SET_R )THEN
        chemMet%pratepuf = SetPrate( zamb,1000.,0.,prbl ) ! precip rate (mm/hr)
      ELSE
        chemMet%pratepuf = prate                           ! convert type to precip rate (mm/hr)
      ENDIF
      chemMet%fcc      = cldcvr                            ! cloud cover fraction
      chemMet%fprcpc   = 0.0
      chemMet%xml      = xml
      chemMet%zb1      = 10.
      chemMet%zruf     = zruf
      chemMet%us2      = us2
      chemMet%ws2      = ws2

      CALL SetChemAqaer( )
      IF( nError /= NO_ERROR )GOTO 9999

!pk 10/16/2018 call not needed      CALL StepAerAqEx( dt,nspecies,carea,pscale,chem_aqaer,chemMet,nError )
!      IF( nError /= NO_ERROR )THEN
!        nError   = IV_ERROR
!        eRoutine = 'StepAerAq'
!        eMessage = 'Error from AqAer module. See AqAer log for details'
!        GO TO 9999
!      END IF

      CALL GetChemAqaer( )

      !====   Force species balance
      IF( chem%lBalance )THEN

        CALL GetSpeciesMass(spec_mass2)
        mass_diff = spec_mass2 - spec_mass1

        ALLOCATE(fac_dist(nspecies),STAT = alloc_stat)
        IF( alloc_stat /= 0 )THEN
          nError   = IV_ERROR
          eRoutine = 'StepChem'
          eMessage = 'Error allocating fac_dist arrays'
          GOTO 9999
        END IF

        CALL GetSpeciesConc(fac_dist)

        DO i = 1,nspecies
          species(i)%conc = species(i)%conc - mass_diff*fac_dist(i)
          species(i)%mass = species(i)%mass - mass_diff*fac_dist(i)
        END DO

        DEALLOCATE(fac_dist,STAT = alloc_stat)

      END IF

      !====   set ambient concentrations

      DO i = 1,nspectot
        chem%Ambient%stepAmb(ixy,iz,i) = MAX(0.,species(i)%mass)
      END DO
      DO i = 1,chem%nStar
        isp = chem%star(i)%s%ID
        chem%Ambient%stepAmb(ixy,iz,isp) = MAX(0.,chem%star(i)%s%conc)
      END DO


    END DO  ! iz loop

    IF( nza > 1 )THEN
      CALL DiffChemAmb( dt,nza,ixy,it,diffz,fact,npt )
      IF( nError /= NO_ERROR) goto 9999
    END IF

  END DO  ! ixy loop

  chem%Ambient%tStepAmb = t1

END IF

9999 CONTINUE

IF( ASSOCIATED(pdnrate) )THEN
  DEALLOCATE( pdnrate, STAT=alloc_stat )
  NULLIFY( pdnrate )
END IF
IF( ASSOCIATED(ydots) )THEN
  DEALLOCATE( ydots, STAT=alloc_stat )
  NULLIFY( ydots )
END IF
IF( ALLOCATED(diffz) )DEALLOCATE( diffz, STAT=alloc_stat )

RETURN

END

!------------------------------------------------------------------------------

SUBROUTINE DiffChemAmb(dt,nza,ixy,it,diffz,fact,npt )
!-- Steps the ambient concentration for vertical diffusion

! --- MODULES

USE error_fi
USE chem_fi

IMPLICIT NONE

!Inputs
REAL dt
INTEGER nza, ixy, npt

INTEGER, DIMENSION(2)   :: it
REAL   , DIMENSION(2)   :: fact
REAL   , DIMENSION(nza) :: diffz

!Locals
INTEGER k, isp

REAL tflx
REAL doztk,doztkp1,dzkot,sflx

REAL, DIMENSION(:), POINTER :: za
REAL, DIMENSION(nza) :: aa,bb,cc,rr
REAL, DIMENSION(nza) :: zt,rrsav,aconc

tflx = 0.0

za => chem%Ambient%zGrid

DO  k = 1,nza-1
  zt(k) =  0.5*(za(k)+za(k+1))
END DO
zt(nza) = 1.5*za(nza) - 0.5*za(nza-1)

k = 1
doztkp1 = diffz(k+1)/(zt(k+1)-zt(k))
dzkot = (za(k+1)-za(k))/dt
aa(k) =  0.0
bb(k) =  doztkp1 + dzkot
cc(k) =  -doztkp1
rrsav(k) =  dzkot

DO  k = 2,nza-1
  doztk   = diffz(k)/(zt(k)-zt(k-1))
  doztkp1 = diffz(k+1)/(zt(k+1)-zt(k))
  dzkot   = (za(k+1)-za(k))/dt
  aa(k) =  -doztk
  bb(k) =  doztk + doztkp1 + dzkot
  cc(k) =  -doztkp1
  rrsav(k) =  dzkot
END DO

k = nza
doztk   = diffz(k)/(zt(k)-zt(k-1))
dzkot   = (za(k)-za(k-1))/dt
aa(k) =  -doztk
bb(k) =  doztk + dzkot
cc(k) =  0.0
rrsav(k) =  dzkot

nspecies = chem%nFast + chem%nSlow + chem%nParticle

DO isp = 1,nspecies

  aconc = chem%Ambient%stepAmb(ixy,:,isp)
  sflx  = 0.0

  IF( chem%lSfcFlx )THEN
    IF( chem%sFlux%sID(isp) > 0 )THEN
      DO k = 1,npt
        sflx = sflx + fact(k)* chem%sFlux%sflx(ixy,chem%sFlux%sID(isp),it(k))
      END DO
    END IF
  END IF

  rr(1) = rrsav(1)*aconc(1) + sflx
  DO k = 2,nza-1
    rr(k) = rrsav(k)*aconc(k)
  END DO
  rr(nza) = rrsav(nza)*aconc(nza) - tflx

  CALL TridagChem(aa,bb,cc,rr,aconc,nza)
  IF( nError /= NO_ERROR) GOTO 9999

  chem%Ambient%stepAmb(ixy,:,isp) = aconc

END DO

9999 RETURN
END

! -------------------------------------------------------------

SUBROUTINE TridagChem(a,b,c,r,u,n)
! Solve Tridiagonal matrix Mu = r

USE error_fi
IMPLICIT NONE

INTEGER                        :: n
REAL,DIMENSION(N)              :: a,b,c,r,u

INTEGER                        :: j
REAL                           :: bet
REAL,DIMENSION(N)              :: gam

IF( b(1)==0. )THEN
  nError = IV_ERROR
  eRoutine = 'TridagChem'
  eMessage = 'Zero diagonal element'
  eInform  = 'b(1) = 0'
  eAction  = 'Stopping run'
  GO TO 9999
END IF

bet=b(1)
u(1)=r(1)/bet

DO j=2,n
   gam(j)=c(j-1)/bet
   bet=b(j)-a(j)*gam(j)
   IF( bet == 0. )THEN
      nError = IV_ERROR
      eRoutine = 'TridagChem'
      eMessage = 'Zero diagonal element'
      eInform  = 'bet = 0'
      eAction  = 'Stopping run'
      GO TO 9999
   END IF
   u(j)=(r(j)-a(j)*u(j-1))/bet
END DO

DO j=n-1,1,-1
  u(j)=u(j)-gam(j+1)*u(j+1)
END DO

9999 RETURN
END
