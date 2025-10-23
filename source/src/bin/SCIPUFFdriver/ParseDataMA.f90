INTEGER FUNCTION ParseDataMA()

USE SCIPUFFdriver_fi

IMPLICIT NONE

INTEGER ios, imat

INTEGER, EXTERNAL :: FindMatlName

CHARACTER(80), EXTERNAL :: BuildFileNameAERMOD

ParseDataMA = FAILURE

SELECT CASE( TRIM(carg(ikwrd)) )

  CASE( 'MATCLASS' )

    IF( .NOT.lMApathway )THEN
      nmat = 0                !Ignore initial material definition (N.B. POLLUTID no longer supporsted)
      lMApathway = .TRUE.
    END IF
    nmat = nmat + 1
    IF( nmat > new%input%mtlHead%max  )THEN
      WRITE(*,'(A)') 'Too many materials'  !Could reallocate but this is simpler
      WRITE(*,'(A)') 'Maximum is ', new%input%mtlHead%max
    END IF

    mtlList(nmat)%name = TRIM(carg(ikwrd+1))
    mtlList(nmat)%units = 'kg'
    SELECT CASE( TRIM(carg(ikwrd+2)) )
      CASE( 'GAS' )
        mtlList(nmat)%type = HM_GAS
        gasMatl(nmat) = gasMatl0              !Initialize to default gas material
        gasMatl(nmat)%save = IBSET(gasMatl(nmat)%save,HSB_TOTALDOS) !Initialize to output total dosage
        gasMatl(nmat)%save = IBSET(gasMatl(nmat)%save,HSB_TOTALDEP) !Initialize to output total dep
      CASE( 'PART' )
        mtlList(nmat)%type = HM_PARTICLE
        IF( TRIM(mtlList(nmat)%name) == 'PM2.5' )THEN
          partMatl(nmat) = partMatl2p5
          mtlList(1)%matData = TRANSFER(partMatl2p5,mtlList(1)%matData)
        ELSE IF( TRIM(mtlList(nmat)%name) == 'PM10' )THEN
          partMatl(nmat) = partMatl10
          mtlList(1)%matData = TRANSFER(partMatl10,mtlList(1)%matData)
        ELSE
          WRITE(*,'(A)') 'Unsupported particle material name '//TRIM(carg(ikwrd+2))
          WRITE(*,'(A)') 'Valid material names are PM2.5 and PM10'
          GOTO 9999
        END IF
        partMatl(nmat)%save = IBSET(partMatl(nmat)%save,HSB_TOTALDOS)
        partMatl(nmat)%save = IBSET(partMatl(nmat)%save,HSB_TOTALDEP)
        partMatl(nmat)%save = IBSET(partMatl(nmat)%save,HSB_GROUPDOS)
        partMatl(nmat)%save = IBSET(partMatl(nmat)%save,HSB_GROUPDEP)
      CASE DEFAULT
        WRITE(*,'(A)') 'Unsupported material type: '//TRIM(carg(ikwrd+2))
        GOTO 9999

    END SELECT

    mtlList(nmat)%PuffIndex   = NOT_SET_I
    mtlList(nmat)%iNotUsed(1) = NOT_SET_I
    mtlList(nmat)%iNotUsed(2) = NOT_SET_I
    mtlList(nmat)%File        = ' '
    mtlList(nmat)%Path        = ' '

  CASE( 'DESCRPTN' )

  CASE( 'DENSITY' )

    IF( narg < ikwrd+2 )THEN
      WRITE(*,'(A)') 'Insufficient MA DENSITY input'
      GOTO 9999
    END IF

    imat = FindMatlName(TRIM(carg(ikwrd+1)))
    IF( imat < 1 )GOTO 9999

    IF( mtlList(imat)%type == HM_GAS )THEN
      READ(carg(ikwrd+2),*,IOSTAT=ios) gasMatl(imat)%gasDensity
    ELSE
      READ(carg(ikwrd+2),*,IOSTAT=ios) partMatl(imat)%Density
    END IF
    IF( ios /= 0 )THEN
      WRITE(*,'(A)') 'Error reading MA DENSITY'
      GOTO 9999
    END IF

  CASE( 'CONCMIN' )

    IF( narg < ikwrd+2 )THEN
      WRITE(*,'(A)') 'Insufficient MA CONCMIN input'
      GOTO 9999
    END IF

    imat = FindMatlName(TRIM(carg(ikwrd+1)))
    IF( imat < 1 )GOTO 9999

    IF( mtlList(imat)%type == HM_GAS )THEN
      READ(carg(ikwrd+2),*,IOSTAT=ios) gasMatl(imat)%minConcentration
    ELSE
      READ(carg(ikwrd+2),*,IOSTAT=ios) partMatl(imat)%minConcentration
    END IF
    IF( ios /= 0 )THEN
      WRITE(*,'(A)') 'Error reading MA CONCMIN'
      GOTO 9999
    END IF

    gasMatl(imat)%minConcentration = MAX(gasMatl(imat)%minConcentration,0.)

  CASE( 'GASDEPOS' )

    IF( mtlList(1)%type /= HM_GAS )THEN
      WRITE(*,'(A)') 'Ignoring GASDEPOS for non gas material'
      GOTO 9998
    END IF

    IF( narg < ikwrd+2 )THEN
      WRITE(*,'(A)') 'Insufficient MA GASDEPOS input'
      GOTO 9999
    END IF

    imat = FindMatlName(TRIM(carg(ikwrd+1)))
    IF( imat < 1 )GOTO 9999

    IF( mtlList(imat)%type == HM_GAS )THEN
      READ(carg(ikwrd+2),*,IOSTAT=ios) gasMatl(imat)%gasDeposition
      IF( ios /= 0 )THEN
        WRITE(*,'(A)') 'Error reading MA GASDEPOS'
        GOTO 9999
      END IF
    END IF

  CASE( 'DECAYMIN' )

    IF( narg < ikwrd+2 )THEN
      WRITE(*,'(A)') 'Insufficient MA DECAYMIN input'
      GOTO 9999
    END IF

    imat = FindMatlName(TRIM(carg(ikwrd+1)))
    IF( imat < 1 )GOTO 9999

    IF( mtlList(imat)%type == HM_GAS )THEN
      READ(carg(ikwrd+2),*,IOSTAT=ios) gasMatl(imat)%decayMin
    ELSE
      READ(carg(ikwrd+2),*,IOSTAT=ios) partMatl(imat)%decayMin
    END IF
    IF( ios /= 0 )THEN
      WRITE(*,'(A)') 'Error reading MA DECAYMIN (nighttime) rate'
      GOTO 9999
    END IF

  CASE( 'DECAYAMP' )

    IF( narg < ikwrd+2 )THEN
      WRITE(*,'(A)') 'Insufficient MA DECAYAMP input'
      GOTO 9999
    END IF

    imat = FindMatlName(TRIM(carg(ikwrd+1)))
    IF( imat < 1 )GOTO 9999

   IF( mtlList(imat)%type == HM_GAS )THEN
      READ(carg(ikwrd+2),*,IOSTAT=ios) gasMatl(imat)%DecayAmp
    ELSE
      READ(carg(ikwrd+2),*,IOSTAT=ios) partMatl(imat)%DecayAmp
    END IF
    IF( ios /= 0 )THEN
      WRITE(*,'(A)') 'Error reading MA DECAYMAX (daytime) rate'
      GOTO 9999
    END IF

  CASE( 'DOSAGE' )

    IF( narg < ikwrd+1 )THEN
      WRITE(*,'(A)') 'Insufficient MA DOSAGE input'
      GOTO 9999
    END IF

    imat = FindMatlName(TRIM(carg(ikwrd+1)))
    IF( imat < 1 )GOTO 9999

    IF( narg > ikwrd+1 )THEN
      IF( carg(ikwrd+2)(1:1) == 'T' )THEN
        IF( mtlList(imat)%type == HM_GAS )THEN
          gasMatl(imat)%save = IBSET(gasMatl(imat)%save,HSB_TOTALDOS)
        ELSE
          partMatl(imat)%save = IBSET(partMatl(imat)%save,HSB_TOTALDOS)
        END IF
      ELSE IF( carg(ikwrd+2)(1:1) == 'G' )THEN
        IF( mtlList(imat)%type == HM_GAS )THEN
          gasMatl(imat)%save = IBSET(gasMatl(imat)%save,HSB_GROUPDOS)
        ELSE
          partMatl(imat)%save = IBSET(partMatl(imat)%save,HSB_GROUPDOS)
        END IF
      END IF
    END IF

    IF( narg > ikwrd+2 )THEN
       IF( mtlList(imat)%type == HM_GAS )THEN
          IF( carg(ikwrd+3)(1:1) == 'T' )THEN
            gasMatl(nmat)%save = IBSET(gasMatl(nmat)%save,HSB_TOTALDOS)
          ELSE IF( carg(ikwrd+3)(1:1) == 'G' )THEN
            gasMatl(nmat)%save = IBSET(gasMatl(nmat)%save,HSB_GROUPDOS)
          END IF
       ELSE
          IF( carg(ikwrd+3)(1:1) == 'T' )THEN
            partMatl(nmat)%save = IBSET(partMatl(nmat)%save,HSB_TOTALDOS)
          ELSE IF( carg(ikwrd+3)(1:1) == 'G' )THEN
            partMatl(nmat)%save = IBSET(partMatl(nmat)%save,HSB_GROUPDOS)
          END IF
       END IF
    END IF

  CASE( 'DEPOSIT','DEPOSITN' )

    IF( narg < ikwrd+1 )THEN
      WRITE(*,'(A)') 'Insufficient MA DEPOSIT input'
      GOTO 9999
    END IF

    imat = FindMatlName(TRIM(carg(ikwrd+1)))
    IF( imat < 1 )GOTO 9999

    IF( narg > ikwrd+1 )THEN
      IF( mtlList(imat)%type == HM_GAS )THEN
        IF( carg(ikwrd+2)(1:1) == 'T' )THEN
          gasMatl(nmat)%save = IBSET(gasMatl(nmat)%save,HSB_TOTALDEP)
        ELSE IF( carg(ikwrd+2)(1:1) == 'G' )THEN
          gasMatl(nmat)%save = IBSET(gasMatl(nmat)%save,HSB_GROUPDEP)
        END IF
      ELSE
        IF( carg(ikwrd+3)(1:1) == 'T' )THEN
          partMatl(nmat)%save = IBSET(partMatl(nmat)%save,HSB_TOTALDEP)
        ELSE IF( carg(ikwrd+3)(1:1) == 'G' )THEN
          partMatl(nmat)%save = IBSET(partMatl(nmat)%save,HSB_GROUPDEP)
        END IF
      END IF
    END IF

    IF( narg > ikwrd+2 )THEN
      IF( mtlList(imat)%type == HM_GAS )THEN
        IF( carg(ikwrd+2)(1:1) == 'T' )THEN
          gasMatl(nmat)%save = IBSET(gasMatl(nmat)%save,HSB_TOTALDEP)
        ELSE IF( carg(ikwrd+2)(1:1) == 'G' )THEN
          gasMatl(nmat)%save = IBSET(gasMatl(nmat)%save,HSB_GROUPDEP)
        END IF
      ELSE
        IF( carg(ikwrd+3)(1:1) == 'T' )THEN
          partMatl(nmat)%save = IBSET(partMatl(nmat)%save,HSB_TOTALDEP)
        ELSE IF( carg(ikwrd+3)(1:1) == 'G' )THEN
          partMatl(nmat)%save = IBSET(partMatl(nmat)%save,HSB_GROUPDEP)
        END IF
      END IF
    END IF

  CASE( 'IMCFILE','MCFILE' )

    IF( narg < ikwrd+1 )THEN
      WRITE(*,'(A)') 'Insufficient MA IMCFILE input'
      GOTO 9999
    END IF

    IF( mtlList(1)%type /= HM_GAS )THEN
      WRITE(*,'(A)') 'Ignoring IMCFILE for non gas material'
      GOTO 9998
    END IF

    BACKSPACE(lun,IOSTAT=ios)

#if defined(LINUX)
!------ Re-read line "as is" since case matters for path and file names on Linux
!       and to handle files names with spaces

    CALL get_next_data_NO_CUPPER( lun,line,nch,kwrd,narg,carg,MAXN,lerr )

#else
!------ Re-read line to handle files names with spaces

    CALL get_next_data( lun,line,nch,kwrd,narg,carg,MAXN,lerr )
#endif

    fname = TRIM(BuildFileNameAERMOD(carg(ikwrd+1),line,path_in))
    CALL SplitName( fname,mtlList(nmat)%file,mtlList(nmat)%path ) !IMC file & path

  CASE( 'FINISHED' )

    IF( lMApathway )THEN
      DO imat = 1,nmat
        IF( mtlList(imat)%type == HM_GAS )THEN
          mtlList(imat)%matData = TRANSFER(gasMatl(imat),mtlList(imat)%matData)
        ELSE
          mtlList(imat)%matData = TRANSFER(partMatl(imat),mtlList(imat)%matData)
        END IF
        new%input%mtlHead%number = nmat
      END DO
    END IF

  CASE DEFAULT
    WRITE(*,'(A)') 'Invalid keyword for MA pathway: '//TRIM(carg(ikwrd))
    GOTO 9999

END SELECT

9998 CONTINUE

ParseDataMA = SUCCESS

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION FindMatlName( string ) RESULT( imat )

USE SCIPUFFdriver_fi

IMPLICIT NONE

INTEGER i

CHARACTER(*), INTENT( IN ) :: string

IF( nmat < 1 )THEN
  WRITE(*,'(A)') 'FindMatlName error: no materials defined'
  GOTO 9999
END IF

imat = -1
DO i = 1,nmat
  IF( ADJUSTL(TRIM(string)) == TRIM(mtlList(i)%name) )THEN
    imat = i
    EXIT
  END IF
END DO

IF( imat == -1 )THEN
  WRITE(*,'(A)') 'Material not found: '//ADJUSTL(TRIM(string))
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END


!==============================================================================

REAL FUNCTION GetMatlDensity( matl_name ) RESULT( rho )

USE SCIPUFFdriver_fi

IMPLICIT NONE

INTEGER i, imat

CHARACTER(*), INTENT( IN ) :: matl_name

!------ Initialize to unphysical value

rho = -999.

!------ Find material

IF( nmat < 1 )THEN
  WRITE(*,'(A)') 'GetMatlDensity error: no materials defined'
  GOTO 9999
END IF

imat = -1
DO i = 1,nmat
  IF( ADJUSTL(TRIM(matl_name)) == TRIM(mtlList(i)%name) )THEN
    imat = i
    EXIT
  END IF
END DO

IF( imat == -1 )THEN
  WRITE(*,'(A)') 'Material not found: '//ADJUSTL(TRIM(matl_name))
  GOTO 9999
END IF

!------ Get density

IF( mtlList(imat)%type == HM_GAS )THEN
  gasMatl0 = TRANSFER(mtlList(imat)%matData,gasMatl0)
  rho = gasMatl0%gasDensity

ELSE IF( mtlList(imat)%type == HM_PARTICLE )THEN
  WRITE(*,'(A)') 'Particle materials not supported type'
  GOTO 9999

ELSE
  WRITE(*,'(A)') 'Invalid material type'
  GOTO 9999
ENDIF

9999 CONTINUE

RETURN
END

