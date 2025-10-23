!*******************************************************************************
!            UnloadRelease
!*******************************************************************************
SUBROUTINE UnloadRelease( release )

USE convert_fd
USE release_fd
USE relparam_fd
USE scipuff_fi
USE release_fi

!     Load SCIPUFF commons from an SCIP Release structure

IMPLICIT NONE

TYPE( releaseT ), INTENT( IN ) :: release

CHARACTER(192), EXTERNAL :: StripNull
REAL,           EXTERNAL :: ScaleReal

!==== Initialize check

IF( release%tRel == 1.0E+36 .OR. release%tRel == NOT_SET_R )THEN
  nError = IV_ERROR
  eRoutine = 'UnloadRelease'
  eMessage = 'Invalid release parameter : tRel'
  eInform  = 'Valid release not defined'
  GOTO 9999
END IF

!==== Unload
CALL InitRelease()

!==== Basic parameters

trel = ScaleReal( release%tRel,HCF_HOUR2SEC )

  xrel = release%xRel
  yrel = release%yRel
  zrel = release%zRel


!==== Release type dependent parameters

relName    = TRIM(StripNull(release%relName))
relDisplay = TRIM(StripNull(release%relDisplay))

  relmat    = TRIM(StripNull(release%material))
  relStatus = release%status

SELECT CASE( release%type )
  CASE( HR_INST )
    reltyp = 'I'
    CALL UnloadReleaseInst( release%relData )
    IF( nError /= NO_ERROR )GOTO 9999
  CASE( HR_XINST )
    reltyp = 'IX'
    CALL UnloadReleaseXInst( release%relData )
    IF( nError /= NO_ERROR )GOTO 9999
  CASE( HR_XINST3 )
    reltyp = 'IX3'
    CALL UnloadReleaseXInst3( release%relData )
    IF( nError /= NO_ERROR )GOTO 9999
  CASE( HR_FINST )
    reltyp = 'IA'
    CALL UnloadReleaseInst( release%relData )
    IF( nError /= NO_ERROR )GOTO 9999
  CASE( HR_FILE )
    reltyp = 'IF'
    CALL UnloadReleaseFile( release%relData )
    IF( nError /= NO_ERROR )GOTO 9999
  CASE( HR_PUFF )
    nError   = IV_ERROR
    eRoutine = 'UnloadRelease'
    eMessage = 'Unable to Unload HP_PUFF release type'
    eInform  = 'HR_PUFF is a special release type for urban handoff'
    GOTO 9999
  CASE( HR_CONT )
    reltyp = 'C'
    CALL UnloadReleaseCont( release%relData )
    IF( nError /= NO_ERROR )GOTO 9999
  CASE( HR_MOVE )
    reltyp = 'CM'
    CALL UnloadReleaseMove( release%relData )
    IF( nError /= NO_ERROR )GOTO 9999
  CASE( HR_CONTF )
    reltyp = 'CF'
    CALL UnloadReleaseContFile( release%relData )
    IF( nError /= NO_ERROR )GOTO 9999
  CASE( HR_STACKF )
    umom = DEF_VAL_R
    vmom = DEF_VAL_R
    reltyp = 'CSF'
    CALL UnloadReleaseStackFile( release%relData )
    IF( nError /= NO_ERROR )GOTO 9999
  CASE( HR_STACKFP )
    umom = 0. !DEF_VAL_R
    vmom = 0. !DEF_VAL_R
    reltyp = 'CSFP'
    CALL UnloadReleaseStackFile( release%relData )
    IF( nError /= NO_ERROR )GOTO 9999
  CASE( HR_AREAF )
    umom = DEF_VAL_R
    vmom = DEF_VAL_R
    reltyp = 'CAF'
    CALL UnloadReleaseAreaFile( release%relData )
    IF( nError /= NO_ERROR )GOTO 9999
  CASE( HR_STACK3F )
    reltyp = 'CSF'
    CALL UnloadReleaseStack3File( release%relData )
    IF( nError /= NO_ERROR )GOTO 9999
  CASE( HR_STACK )
    umom = DEF_VAL_R
    vmom = DEF_VAL_R
    reltyp = 'CS'
    CALL UnloadReleaseStack( release%relData )
    IF( nError /= NO_ERROR )GOTO 9999
  CASE( HR_STACK3 )
    reltyp = 'CS'
    CALL UnloadReleaseStack3( release%relData )
    IF( nError /= NO_ERROR )GOTO 9999
  CASE( HR_AREA )
    umom = DEF_VAL_R
    vmom = DEF_VAL_R
    reltyp = 'CA'
    CALL UnloadReleaseArea( release%relData )
    IF( nError /= NO_ERROR )GOTO 9999
  CASE( HR_PRIME )
    umom = 0. !DEF_VAL_R
    vmom = 0. !DEF_VAL_R
    reltyp = 'CSP'
    CALL UnloadReleaseStack( release%relData )
    IF( nError /= NO_ERROR )GOTO 9999
  CASE( HR_POOL )
    reltyp(1:2) = 'CP'
    CALL UnloadReleasePool( release%relData )
    IF( nError /= NO_ERROR )GOTO 9999
  CASE DEFAULT
    nError   = IV_ERROR
    eRoutine = 'UnloadRelease'
    eMessage = 'Invalid release type'
    WRITE(eInform,'(A,I8)')'type =',release%type
    GOTO 9999
END SELECT

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            LoadRelease
!*******************************************************************************
SUBROUTINE LoadRelease( release )

USE convert_fd
USE release_fd
USE relparam_fd
USE scipuff_fi
USE release_fi
USE message_fd

!     Load an SCIP Release structure from SCIPUFF commons

IMPLICIT NONE

TYPE( releaseT ), INTENT( OUT ) :: release

INTEGER irv
TYPE ( messageT ) :: caution

REAL, EXTERNAL :: ScaleReal
INTEGER, EXTERNAL :: PostCautionMessage

!==== Load

IF( trel == 1.0E+36 .OR. trel == NOT_SET_R )THEN
  nError   = IV_ERROR
  eRoutine = 'LoadRelease'
  eMessage = 'Invalid release parameter : trel'
  eInform  = 'Valid release not defined'
  GOTO 9999
END IF

release%padding = NOT_SET_I ! For Lahey Global Check

    !==== Basic parameters

release%tRel = ScaleReal( trel,HCF_SEC2HOUR )

IF( TRIM(reltyp(1:1)) /= 'X' )THEN

  release%xRel = xrel
  release%yRel = yrel
  release%zRel = zrel

  release%notUsedA = NOT_SET_R
  release%notUsedB = NOT_SET_R

  release%notUsed = NOT_SET_R

  release%relData%padding(1:HS_PADRELGEN) = NOT_SET_I

END IF

!==== Release type dependent parameters

IF( TRIM(reltyp(1:1)) /= 'X' )THEN
  IF( tdur == DEF_VAL_R )THEN
    IF( reltyp(1:2) /= 'CP' )THEN
      nError = IV_ERROR
      eRoutine = 'LoadRelease'
      eMessage = 'Invalid release parameter : Duration'
      eInform  = 'Only Pool sources accept duration=DEFAULT'
      GOTO 9999
    END IF
  END IF

  release%material = relmat
  release%status   = relStatus

END IF

release%relName    = relName
release%relDisplay = relDisplay

SELECT CASE( TRIM(reltyp) )
  CASE( 'I' )
    IF( name_rel == ' ' )THEN
      release%type = HR_INST
      CALL LoadReleaseInst( release%relData )
      IF( nError /= NO_ERROR )GOTO 9999
    ELSE
      release%type = HR_FILE
      CALL LoadReleaseFile( release%relData )
      IF( nError /= NO_ERROR )GOTO 9999
    END IF
  CASE( 'IX' )
    release%type = HR_XINST
    CALL LoadReleaseXInst( release%relData )
    IF( nError /= NO_ERROR )GOTO 9999
  CASE( 'IX3' )
    release%type = HR_XINST3
    CALL LoadReleaseXInst3( release%relData )
    IF( nError /= NO_ERROR )GOTO 9999
  CASE( 'IA' )
    release%type = HR_FINST
    CALL LoadReleaseInst( release%relData )
    IF( nError /= NO_ERROR )GOTO 9999
  CASE( 'IF' )
    release%type = HR_FILE
    CALL LoadReleaseFile( release%relData )
    IF( nError /= NO_ERROR )GOTO 9999
  CASE( 'IP' )
    nError   = IV_ERROR
    eRoutine = 'LoadRelease'
    eMessage = 'Unable to Unload IP release type'
    eInform  = 'IP is a special release type for urban handoff'
    GOTO 9999
  CASE( 'C' )
    release%type = HR_CONT
    CALL LoadReleaseCont( release%relData )
    IF( nError /= NO_ERROR )GOTO 9999
    release%status = 1
  CASE( 'CM' )
    release%type = HR_MOVE
    CALL LoadReleaseMove( release%relData )
    IF( nError /= NO_ERROR )GOTO 9999
    release%status = 1
  CASE( 'CF' )
    release%type = HR_CONTF
    CALL LoadReleaseContFile( release%relData )
    IF( nError /= NO_ERROR )GOTO 9999
  CASE( 'CSF' )
    IF( umom == DEF_VAL_R .AND. vmom == DEF_VAL_R )THEN
      release%type = HR_STACKF
      CALL LoadReleaseStackFile( release%relData )
    ELSE
      release%type = HR_STACK3F
      CALL LoadReleaseStack3File( release%relData )
    END IF
    IF( nError /= NO_ERROR )GOTO 9999
  CASE( 'CAF' )
    release%type = HR_AREAF
    CALL LoadReleaseAreaFile( release%relData )
    IF( nError == WN_ERROR )THEN
      caution%iParm = 0; caution%jParm = 0; caution%routine = 'LoadRelease'
      caution%aString = 'Large aspect ratio for area source '//TRIM(release%relName)
      caution%bString = 'Source should be broken-up into'
      caution%cString = 'multiple areas with AR <= 10'
      irv = PostCautionMessage( caution )
      nError = NO_ERROR
    END IF
    IF( nError /= NO_ERROR )GOTO 9999
  CASE( 'CSFP' )
    IF( LEN_TRIM(relDisplay) < 1 )THEN
      nError = IV_ERROR
      eRoutine = 'LoadRelease'
      eMessage = 'Invalid PRIME release'
      eInform  = 'No PRIME file specified'
      GOTO 9999
    END IF
    release%type = HR_STACKFP
    release%padding = -1                    !Flag used to set which data to get in getRelease functions ?????
    CALL LoadReleaseStackFile( release%relData )
  CASE( 'CS' )
    IF( umom == DEF_VAL_R .AND. vmom == DEF_VAL_R )THEN
      release%type = HR_STACK
      CALL LoadReleaseStack( release%relData )
    ELSE
      release%type = HR_STACK3
      CALL LoadReleaseStack3( release%relData )
    END IF
    IF( nError /= NO_ERROR )GOTO 9999
    release%status = 1
  CASE( 'CA' )
    release%type = HR_AREA
    CALL LoadReleaseArea( release%relData )
    IF( nError == WN_ERROR )THEN
      caution%iParm = 0; caution%jParm = 0; caution%routine = 'LoadRelease'
      caution%aString = 'Large aspect ratio for area source '//TRIM(release%relName)
      caution%bString = 'Source should be broken-up into'
      caution%cString = 'multiple areas with AR <= 10'
      irv = PostCautionMessage( caution )
      nError = NO_ERROR
    END IF
    IF( nError /= NO_ERROR )GOTO 9999
    release%status = 1
  CASE( 'CSP' )
    release%type = HR_PRIME
    release%padding = -1                    !Flag used to set which data to get in getRelease functions
    CALL LoadReleaseStack( release%relData )
    IF( nError /= NO_ERROR )GOTO 9999
  CASE( 'CP' )
    release%type = HR_POOL
    CALL LoadReleasePool( release%relData )
    IF( nError /= NO_ERROR )GOTO 9999
  CASE DEFAULT
    nError   = IV_ERROR
    eRoutine = 'LoadRelease'
    eMessage = 'Invalid release type'
    eInform  = 'type ='// TRIM(reltyp)
    GOTO 9999
  END SELECT

9999 CONTINUE

RETURN
END
!*******************************************************************************
!        UnloadReleaseCont
!*******************************************************************************
SUBROUTINE UnloadReleaseCont( relDataIn )

USE convert_fd
USE release_fd
USE relparam_fd
USE scipuff_fi
USE release_fi

!     Load SCIPUFF commons from an SCIP Release structure

IMPLICIT NONE

TYPE( relGenT ), INTENT( IN ) :: relDataIn

TYPE( relContT ) relData

REAL, EXTERNAL :: ScaleReal

!==== Unload

relData = TRANSFER(relDataIn,relData)

subgroup = relData%distribution

cmass = relData%rate
tdur  = ScaleReal( relData%duration,HCF_HOUR2SEC )

sigx     = 0.0
sigy     = relData%sigY
sigz     = relData%sigZ
size_rel = NOT_SET_R

umom = 0.0
vmom = 0.0
IF( relData%momentum /= NOT_SET_R )THEN
  wmom = relData%momentum
END IF

IF( relData%buoyancy /= NOT_SET_R )THEN
  buoy = relData%buoyancy
END IF

IF( relData%distribution == HD_LOGNORM )THEN
  rel_param(REL_MMD_INDX)   = relData%MMD
  rel_param(REL_SIGMA_INDX) = relData%sigma
END IF

rel_param(REL_WMFRAC_INDX) = relData%dryFrac
rel_param(REL_AFRAC_INDX)  = relData%activeFrac
rel_param(REL_NXTUPDT_INDX)  = relData%nextUpdtTime

RETURN
END
!*******************************************************************************
!        LoadReleaseCont
!*******************************************************************************
SUBROUTINE LoadReleaseCont( relDataOut )

USE convert_fd
USE release_fd
USE relparam_fd
USE scipuff_fi
USE release_fi

!     Load an SCIP Release structure from SCIPUFF commons

IMPLICIT NONE

TYPE( relGenT ), INTENT( OUT ) :: relDataOut

TYPE( relContT ) relData

REAL, EXTERNAL :: ScaleReal

!==== Load

relData%distribution = subgroup

relData%rate     = cmass
relData%duration = ScaleReal( tdur,HCF_SEC2HOUR )

IF( size_rel > 0 )THEN
  relData%sigY = size_rel
  relData%sigZ = size_rel
ELSE
  relData%sigY = sigy
  relData%sigZ = sigz
END IF

relData%momentum = wmom
relData%buoyancy = buoy

IF( rel_param(REL_MMD_INDX)/= NOT_SET_R )THEN
  relData%MMD   = rel_param(REL_MMD_INDX)
  relData%sigma = rel_param(REL_SIGMA_INDX)
ELSE
  relData%MMD   = NOT_SET_R
  relData%sigma = NOT_SET_R
END IF

relData%activeFrac   = rel_param(REL_AFRAC_INDX)
relData%nextUpdtTime = rel_param(REL_NXTUPDT_INDX)
relData%dryFrac = rel_param(REL_WMFRAC_INDX)
relData%padding = NOT_SET_I

relDataOut = TRANSFER(relData,relDataOut)

RETURN
END
!*******************************************************************************
!        UnloadReleaseContFile
!*******************************************************************************
SUBROUTINE UnloadReleaseContFile( relDataIn )

USE convert_fd
USE release_fd
USE relparam_fd
USE scipuff_fi
USE release_fi

!     Load SCIPUFF commons from an SCIP Release structure

IMPLICIT NONE

TYPE( relGenT ), INTENT( IN ) :: relDataIn

TYPE( relContFileT ) relData

REAL, EXTERNAL :: ScaleReal

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: StripNull

!==== Unload

relData = TRANSFER(relDataIn,relData)

subgroup = relData%distribution

cmass = relData%rate
tdur  = HUGE(0.) !ScaleReal( relData%duration,HCF_HOUR2SEC )

sigx     = 0.0
sigy     = relData%sigY
sigz     = relData%sigZ
size_rel = NOT_SET_R

umom = 0.0
vmom = 0.0
IF( relData%momentum /= NOT_SET_R )THEN
  wmom = relData%momentum
END IF

IF( relData%buoyancy /= NOT_SET_R )THEN
  buoy = relData%buoyancy
END IF

IF( relData%distribution == HD_LOGNORM )THEN
  rel_param(REL_MMD_INDX)   = relData%MMD
  rel_param(REL_SIGMA_INDX) = relData%sigma
END IF

rel_param(REL_WMFRAC_INDX) = relData%dryFrac
rel_param(REL_AFRAC_INDX)   = relData%activeFrac
rel_param(REL_NXTUPDT_INDX)  = relData%nextUpdtTime

name_rel = TRIM(StripNull(relData%relFile))

RETURN
END
!*******************************************************************************
!        LoadReleaseContFile
!*******************************************************************************
SUBROUTINE LoadReleaseContFile( relDataOut )

USE convert_fd
USE release_fd
USE relparam_fd
USE scipuff_fi
USE release_fi

!     Load an SCIP Release structure from SCIPUFF commons

IMPLICIT NONE

TYPE( relGenT ), INTENT( OUT ) :: relDataOut

TYPE( relContFileT ) relData

REAL, EXTERNAL :: ScaleReal

!==== Load

relData%distribution = subgroup

relData%rate     = cmass
relData%duration = HUGE(0.) !ScaleReal( tdur,HCF_SEC2HOUR )

IF( size_rel > 0 )THEN
  relData%sigY = size_rel
  relData%sigZ = size_rel
ELSE
  relData%sigY = sigy
  relData%sigZ = sigz
END IF

relData%momentum = wmom
relData%buoyancy = buoy

IF( rel_param(REL_MMD_INDX)/= NOT_SET_R )THEN
  relData%MMD   = rel_param(REL_MMD_INDX)
  relData%sigma = rel_param(REL_SIGMA_INDX)
ELSE
  relData%MMD   = NOT_SET_R
  relData%sigma = NOT_SET_R
END IF

relData%activeFrac   = rel_param(REL_AFRAC_INDX)
relData%nextUpdtTime = rel_param(REL_NXTUPDT_INDX)
relData%dryFrac = rel_param(REL_WMFRAC_INDX)
relData%relFile = TRIM(name_rel)

relData%padding = NOT_SET_I

relDataOut = TRANSFER(relData,relDataOut)

RETURN
END
!*******************************************************************************
!        UnloadReleaseInst
!*******************************************************************************
SUBROUTINE UnloadReleaseInst( relDataIn )

USE release_fd
USE relparam_fd
USE scipuff_fi
USE release_fi

!     Load SCIPUFF commons from an SCIP Release structure

IMPLICIT NONE

TYPE( relGenT ), INTENT( IN ) :: relDataIn

TYPE( relInstT ) relData

REAL, EXTERNAL :: Int2Real

!==== Unload

relData = TRANSFER(relDataIn,relData)

subgroup = relData%distribution

cmass = relData%mass

sigx     = relData%sigX
sigy     = relData%sigY
sigz     = relData%sigZ
size_rel = NOT_SET_R

umom = 0.0
vmom = 0.0
IF( relData%momentum /= NOT_SET_R )THEN
  wmom = relData%momentum
END IF

IF( relData%buoyancy /= NOT_SET_R )THEN
  buoy = relData%buoyancy
END IF

IF( relData%distribution == HD_LOGNORM )THEN
  rel_param(REL_MMD_INDX)   = relData%MMD
  rel_param(REL_SIGMA_INDX) = relData%sigma
END IF

IF( relData%nRandom > 0 )THEN
  rel_param(REL_RAND_INDX)   = Int2Real(relData%nRandom)
  rel_param(REL_SEED_INDX)   = Int2Real(relData%ranSeed)
  rel_param(REL_SPREAD_INDX) = relData%ranSpread
ELSE
  rel_param(REL_RAND_INDX)   = NOT_SET_R
  rel_param(REL_SEED_INDX)   = NOT_SET_R
  rel_param(REL_SPREAD_INDX) = NOT_SET_R
END IF

rel_param(REL_SPREADT_INDX) = NOT_SET_R
rel_param(REL_SPREADV_INDX) = NOT_SET_R
rel_param(REL_RANDIR_INDX)  = NOT_SET_R

rel_param(REL_WMFRAC_INDX) = relData%dryFrac
rel_param(REL_AFRAC_INDX)  = relData%activeFrac

RETURN
END
!*******************************************************************************
!        LoadReleaseInst
!*******************************************************************************
SUBROUTINE LoadReleaseInst( relDataOut )

USE release_fd
USE relparam_fd
USE scipuff_fi
USE release_fi

!     Load an SCIP Release structure from SCIPUFF commons

IMPLICIT NONE

TYPE( relGenT ), INTENT( OUT ) :: relDataOut

TYPE( relInstT ) relData

INTEGER, EXTERNAL :: Real2Int

!==== Load

relData%distribution = subgroup

relData%mass = cmass

IF( size_rel > 0 )THEN
  relData%sigX = size_rel
  relData%sigY = size_rel
  relData%sigZ = size_rel
ELSE
  relData%sigX = sigx
  relData%sigY = sigy
  relData%sigZ = sigz
END IF

relData%momentum = wmom
relData%buoyancy = buoy

IF( rel_param(REL_MMD_INDX)/= NOT_SET_R )THEN
  relData%MMD   = rel_param(REL_MMD_INDX)
  relData%sigma = rel_param(REL_SIGMA_INDX)
ELSE
  relData%MMD   = NOT_SET_R
  relData%sigma = NOT_SET_R
END IF

IF( rel_param(REL_RAND_INDX) > 0 )THEN
  relData%nRandom   = Real2Int(rel_param(REL_RAND_INDX))
  relData%ranSeed   = Real2Int(rel_param(REL_SEED_INDX))
  relData%ranSpread = rel_param(REL_SPREAD_INDX)
ELSE
  relData%nRandom   = NOT_SET_I
  relData%ranSeed   = NOT_SET_I
  relData%ranSpread = NOT_SET_R
END IF

relData%activeFrac = rel_param(REL_AFRAC_INDX)
relData%dryFrac = rel_param(REL_WMFRAC_INDX)
relData%padding = NOT_SET_I

relDataOut = TRANSFER(relData,relDataOut)

RETURN
END
!*******************************************************************************
!        UnloadReleaseXInst
!*******************************************************************************
SUBROUTINE UnloadReleaseXInst( relDataIn )

USE release_fd
USE relparam_fd
USE scipuff_fi
USE release_fi

!     Load SCIPUFF commons from an SCIP Release structure

IMPLICIT NONE

TYPE( relGenT ), INTENT( IN ) :: relDataIn

TYPE( relXInstT ) relData

REAL, EXTERNAL :: Int2Real

!==== Unload

relData = TRANSFER(relDataIn,relData)

subgroup = relData%distribution

cmass = relData%mass

sigx     = relData%sigX
sigy     = relData%sigY
sigz     = relData%sigZ
sigRxy   = relData%sigRxy
sigRxz   = relData%sigRxz
sigRyz   = relData%sigRyz
size_rel = NOT_SET_R

umom = 0.0
vmom = 0.0
IF( relData%momentum /= NOT_SET_R )THEN
  wmom = relData%momentum
END IF

IF( relData%buoyancy /= NOT_SET_R )THEN
  buoy = relData%buoyancy
END IF

IF( relData%distribution == HD_LOGNORM )THEN
  rel_param(REL_MMD_INDX)   = relData%MMD
  rel_param(REL_SIGMA_INDX) = relData%sigma
END IF

IF( relData%nRandom > 0 )THEN
  rel_param(REL_RAND_INDX)   = Int2Real(relData%nRandom)
  rel_param(REL_SEED_INDX)   = Int2Real(relData%ranSeed)
  rel_param(REL_SPREAD_INDX) = relData%ranSpread
ELSE
  rel_param(REL_RAND_INDX)   = NOT_SET_R
  rel_param(REL_SEED_INDX)   = NOT_SET_R
  rel_param(REL_SPREAD_INDX) = NOT_SET_R
END IF

rel_param(REL_SPREADT_INDX) = NOT_SET_R
rel_param(REL_SPREADV_INDX) = NOT_SET_R
rel_param(REL_RANDIR_INDX)  = NOT_SET_R

rel_param(REL_WMFRAC_INDX) = relData%dryFrac
rel_param(REL_AFRAC_INDX)  = relData%activeFrac

RETURN
END
!*******************************************************************************
!        LoadReleaseXInst
!*******************************************************************************
SUBROUTINE LoadReleaseXInst( relDataOut )

USE release_fd
USE relparam_fd
USE scipuff_fi
USE release_fi

!     Load an SCIP Release structure from SCIPUFF commons

IMPLICIT NONE

TYPE( relGenT ), INTENT( OUT ) :: relDataOut

TYPE( relXInstT ) relData

INTEGER, EXTERNAL :: Real2Int

!==== Load

relData%distribution = subgroup

relData%mass = cmass

IF( size_rel > 0 )THEN
  relData%sigX = size_rel
  relData%sigY = size_rel
  relData%sigZ = size_rel
ELSE
  relData%sigX = sigx
  relData%sigY = sigy
  relData%sigZ = sigz
END IF

relData%sigRxy = sigRxy
relData%sigRxz = sigRxz
relData%sigRyz = sigRyz

relData%momentum = wmom
relData%buoyancy = buoy

IF( rel_param(REL_MMD_INDX)/= NOT_SET_R )THEN
  relData%MMD   = rel_param(REL_MMD_INDX)
  relData%sigma = rel_param(REL_SIGMA_INDX)
ELSE
  relData%MMD   = NOT_SET_R
  relData%sigma = NOT_SET_R
END IF

IF( rel_param(REL_RAND_INDX) > 0 )THEN
  relData%nRandom   = Real2Int(rel_param(REL_RAND_INDX))
  relData%ranSeed   = Real2Int(rel_param(REL_SEED_INDX))
  relData%ranSpread = rel_param(REL_SPREAD_INDX)
ELSE
  relData%nRandom   = NOT_SET_I
  relData%ranSeed   = NOT_SET_I
  relData%ranSpread = NOT_SET_R
END IF

relData%activeFrac = rel_param(REL_AFRAC_INDX)
relData%dryFrac = rel_param(REL_WMFRAC_INDX)
relData%padding = NOT_SET_I

relDataOut = TRANSFER(relData,relDataOut)

RETURN
END
!*******************************************************************************
!        UnloadReleaseXInst
!*******************************************************************************
SUBROUTINE UnloadReleaseXInst3( relDataIn )

USE release_fd
USE relparam_fd
USE scipuff_fi
USE release_fi

!     Load SCIPUFF commons from an SCIP Release structure

IMPLICIT NONE

TYPE( relGenT ), INTENT( IN ) :: relDataIn

TYPE( relXInst3T ) relData

REAL, EXTERNAL :: Int2Real

!==== Unload

relData = TRANSFER(relDataIn,relData)

subgroup = relData%distribution

cmass = relData%mass

sigx     = relData%sigX
sigy     = relData%sigY
sigz     = relData%sigZ
sigRxy   = relData%sigRxy
sigRxz   = relData%sigRxz
sigRyz   = relData%sigRyz
size_rel = NOT_SET_R

umom = 0.0
vmom = 0.0
IF( relData%momentum /= NOT_SET_R )THEN
  wmom = relData%momentum
END IF

IF( relData%buoyancy /= NOT_SET_R )THEN
  buoy = relData%buoyancy
END IF

IF( relData%distribution == HD_LOGNORM )THEN
  rel_param(REL_MMD_INDX)   = relData%MMD
  rel_param(REL_SIGMA_INDX) = relData%sigma
END IF

IF( relData%nRandom > 0 )THEN
  rel_param(REL_RAND_INDX)    = Int2Real(relData%nRandom)
  rel_param(REL_SEED_INDX)    = Int2Real(relData%ranSeed)
  rel_param(REL_SPREAD_INDX)  = relData%ranSpreadA
  rel_param(REL_SPREADT_INDX) = relData%ranSpreadT
  rel_param(REL_SPREADV_INDX) = relData%ranSpreadV
  rel_param(REL_RANDIR_INDX)  = relData%ranDir
ELSE
  rel_param(REL_RAND_INDX)    = NOT_SET_R
  rel_param(REL_SEED_INDX)    = NOT_SET_R
  rel_param(REL_SPREAD_INDX)  = NOT_SET_R
  rel_param(REL_SPREADT_INDX) = NOT_SET_R
  rel_param(REL_SPREADV_INDX) = NOT_SET_R
  rel_param(REL_RANDIR_INDX)  = NOT_SET_R
END IF

rel_param(REL_WMFRAC_INDX) = relData%dryFrac
rel_param(REL_AFRAC_INDX)  = relData%activeFrac

RETURN
END
!*******************************************************************************
!        LoadReleaseXInst
!*******************************************************************************
SUBROUTINE LoadReleaseXInst3( relDataOut )

USE release_fd
USE relparam_fd
USE scipuff_fi
USE release_fi

!     Load an SCIP Release structure from SCIPUFF commons

IMPLICIT NONE

TYPE( relGenT ), INTENT( OUT ) :: relDataOut

TYPE( relXInst3T ) relData

INTEGER, EXTERNAL :: Real2Int

!==== Load

relData%distribution = subgroup

relData%mass = cmass

IF( size_rel > 0 )THEN
  relData%sigX = size_rel
  relData%sigY = size_rel
  relData%sigZ = size_rel
ELSE
  relData%sigX = sigx
  relData%sigY = sigy
  relData%sigZ = sigz
END IF

relData%sigRxy = sigRxy
relData%sigRxz = sigRxz
relData%sigRyz = sigRyz

relData%momentum = wmom
relData%buoyancy = buoy

IF( rel_param(REL_MMD_INDX)/= NOT_SET_R )THEN
  relData%MMD   = rel_param(REL_MMD_INDX)
  relData%sigma = rel_param(REL_SIGMA_INDX)
ELSE
  relData%MMD   = NOT_SET_R
  relData%sigma = NOT_SET_R
END IF

IF( rel_param(REL_RAND_INDX) > 0 )THEN
  relData%nRandom    = Real2Int(rel_param(REL_RAND_INDX))
  relData%ranSeed    = Real2Int(rel_param(REL_SEED_INDX))
  relData%ranSpreadA = rel_param(REL_SPREAD_INDX)
  relData%ranSpreadT = rel_param(REL_SPREADT_INDX)
  relData%ranSpreadV = rel_param(REL_SPREADV_INDX)
  relData%ranDir     = rel_param(REL_RANDIR_INDX)
ELSE
  relData%nRandom    = NOT_SET_I
  relData%ranSeed    = NOT_SET_I
  relData%ranSpreadA = NOT_SET_R
  relData%ranSpreadT = NOT_SET_R
  relData%ranSpreadV = NOT_SET_R
  relData%ranDir     = NOT_SET_R
END IF

relData%activeFrac = rel_param(REL_AFRAC_INDX)
relData%dryFrac = rel_param(REL_WMFRAC_INDX)
relData%padding = NOT_SET_I

relDataOut = TRANSFER(relData,relDataOut)

RETURN
END
!*******************************************************************************
!        UnloadReleaseMove
!*******************************************************************************
SUBROUTINE UnloadReleaseMove( relDataIn )

USE convert_fd
USE release_fd
USE relparam_fd
USE scipuff_fi
USE release_fi

!     Load SCIPUFF commons from an SCIP Release structure

IMPLICIT NONE

TYPE( relGenT ), INTENT ( IN ) :: relDataIn

TYPE( relMoveT ) relData

REAL, EXTERNAL :: ScaleReal

!==== Unload

relData = TRANSFER(relDataIn,relData)

subgroup = relData%distribution

cmass = relData%rate
tdur  = ScaleReal( relData%duration,HCF_HOUR2SEC )

sigx     = 0.0
sigy     = relData%sigY
sigz     = relData%sigZ
size_rel = NOT_SET_R

urel  = relData%velX
vrel  = relData%velY
wrel  = relData%velZ

umom = 0.0
vmom = 0.0
IF( relData%momentum /= NOT_SET_R )wmom = relData%momentum
IF( relData%buoyancy /= NOT_SET_R )buoy = relData%buoyancy

IF( relData%distribution == HD_LOGNORM )THEN
  rel_param(REL_MMD_INDX)   = relData%MMD
  rel_param(REL_SIGMA_INDX) = relData%sigma
END IF

rel_param(REL_WMFRAC_INDX) = relData%dryFrac
rel_param(REL_AFRAC_INDX)   = relData%activeFrac
rel_param(REL_NXTUPDT_INDX) = relData%nextUpdtTime
RETURN
END
!*******************************************************************************
!        LoadReleaseMove
!*******************************************************************************
SUBROUTINE LoadReleaseMove( relDataOut )

USE convert_fd
USE release_fd
USE relparam_fd
USE scipuff_fi
USE release_fi

!     Load an SCIP Release structure from SCIPUFF commons

IMPLICIT NONE

TYPE( relGenT ), INTENT( OUT ) :: relDataOut

TYPE( relMoveT ) relData

REAL, EXTERNAL :: ScaleReal

!==== Load

relData%distribution = subgroup

relData%rate     = cmass
relData%duration = ScaleReal( tdur,HCF_SEC2HOUR )

IF( size_rel > 0 )THEN
  relData%sigY = size_rel
  relData%sigZ = size_rel
ELSE
  relData%sigY = sigy
  relData%sigZ = sigz
END IF

relData%velX = urel
relData%velY = vrel
relData%velZ = wrel

relData%momentum = wmom
relData%buoyancy = buoy

IF( rel_param(REL_MMD_INDX)/= NOT_SET_R )THEN
  relData%MMD   = rel_param(REL_MMD_INDX)
  relData%sigma = rel_param(REL_SIGMA_INDX)
ELSE
  relData%MMD   = NOT_SET_R
  relData%sigma = NOT_SET_R
END IF

relData%activeFrac = rel_param(REL_AFRAC_INDX)
relData%nextUpdtTime = rel_param(REL_NXTUPDT_INDX)
relData%dryFrac = rel_param(REL_WMFRAC_INDX)
relData%padding = NOT_SET_I

relDataOut = TRANSFER(relData,relDataOut)

RETURN
END
!*******************************************************************************
!        UnloadReleaseStack
!*******************************************************************************
SUBROUTINE UnloadReleaseStack( relDataIn )

USE convert_fd
USE release_fd
USE relparam_fd
USE scipuff_fi
USE release_fi

!     Load SCIPUFF commons from an SCIP Release structure

IMPLICIT NONE

TYPE( relGenT ), INTENT ( IN ) :: relDataIn

TYPE( relStackT ) relData

REAL, EXTERNAL :: ScaleReal

!==== Unload

relData = TRANSFER(relDataIn,relData)

subgroup = relData%distribution

cmass = relData%rate
tdur  = ScaleReal( relData%duration,HCF_HOUR2SEC )

sigx     = NOT_SET_R
sigy     = NOT_SET_R
sigz     = NOT_SET_R
size_rel = relData%diameter

umom = DEF_VAL_R
vmom = DEF_VAL_R
IF( relData%exitVel  /= NOT_SET_R )wmom = relData%exitVel

IF( relData%exitTemp /= NOT_SET_R )buoy = relData%exitTemp

IF( relData%distribution == HD_LOGNORM )THEN
  rel_param(REL_MMD_INDX)   = relData%MMD
  rel_param(REL_SIGMA_INDX) = relData%sigma
END IF

rel_param(REL_WMFRAC_INDX) = relData%dryFrac
rel_param(REL_AFRAC_INDX)   = relData%activeFrac
rel_param(REL_NXTUPDT_INDX) = relData%nextUpdtTime

RETURN
END
!*******************************************************************************
!        UnloadReleaseStack3
!*******************************************************************************
SUBROUTINE UnloadReleaseStack3( relDataIn )

USE convert_fd
USE release_fd
USE relparam_fd
USE scipuff_fi
USE release_fi

!     Load SCIPUFF commons from an SCIP Release structure

IMPLICIT NONE

TYPE( relGenT ), INTENT ( IN ) :: relDataIn

TYPE( relStack3T ) relData

REAL, EXTERNAL :: ScaleReal

!==== Unload

relData = TRANSFER(relDataIn,relData)

subgroup = relData%distribution

cmass = relData%rate
tdur  = ScaleReal( relData%duration,HCF_HOUR2SEC )

sigx     = NOT_SET_R
sigy     = NOT_SET_R
sigz     = NOT_SET_R
size_rel = relData%diameter

IF( relData%exitVel(1) /= NOT_SET_R )umom = relData%exitVel(1)
IF( relData%exitVel(2) /= NOT_SET_R )vmom = relData%exitVel(2)
IF( relData%exitVel(3) /= NOT_SET_R )wmom = relData%exitVel(3)

IF( relData%exitTemp /= NOT_SET_R )buoy = relData%exitTemp

IF( relData%distribution == HD_LOGNORM )THEN
  rel_param(REL_MMD_INDX)   = relData%MMD
  rel_param(REL_SIGMA_INDX) = relData%sigma
END IF

rel_param(REL_WMFRAC_INDX) = relData%dryFrac
rel_param(REL_AFRAC_INDX)   = relData%activeFrac
rel_param(REL_NXTUPDT_INDX) = relData%nextUpdtTime

RETURN
END
!*******************************************************************************
!        UnloadReleaseStackFile
!*******************************************************************************
SUBROUTINE UnloadReleaseStackFile( relDataIn )

USE convert_fd
USE release_fd
USE relparam_fd
USE scipuff_fi
USE release_fi

!     Load SCIPUFF commons from an SCIP Release structure

IMPLICIT NONE

TYPE( relGenT ), INTENT ( IN ) :: relDataIn

TYPE( relStackFileT ) relData

REAL, EXTERNAL :: ScaleReal

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: StripNull

!==== Unload

relData = TRANSFER(relDataIn,relData)

subgroup = relData%distribution

cmass = relData%rate
tdur  = HUGE(0.) !ScaleReal( relData%duration,HCF_HOUR2SEC )

sigx     = NOT_SET_R
sigy     = NOT_SET_R
sigz     = NOT_SET_R
size_rel = relData%diameter

umom = DEF_VAL_R
vmom = DEF_VAL_R
IF( relData%exitVel  /= NOT_SET_R )wmom = relData%exitVel

IF( relData%exitTemp /= NOT_SET_R )buoy = relData%exitTemp

IF( relData%distribution == HD_LOGNORM )THEN
  rel_param(REL_MMD_INDX)   = relData%MMD
  rel_param(REL_SIGMA_INDX) = relData%sigma
END IF

rel_param(REL_WMFRAC_INDX) = relData%dryFrac
rel_param(REL_AFRAC_INDX)  = relData%activeFrac
rel_param(REL_NXTUPDT_INDX) = relData%nextUpdtTime

name_rel = TRIM(StripNull(relData%relFile))

RETURN
END
!*******************************************************************************
!        UnloadReleaseStack3File
!*******************************************************************************
SUBROUTINE  UnloadReleaseStack3File( relDataIn )

USE convert_fd
USE release_fd
USE relparam_fd
USE scipuff_fi
USE release_fi

!     Load SCIPUFF commons from an SCIP Release structure

IMPLICIT NONE

TYPE( relGenT ), INTENT ( IN ) :: relDataIn

TYPE( relStack3FileT ) relData

REAL, EXTERNAL :: ScaleReal

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: StripNull

!==== Unload

relData = TRANSFER(relDataIn,relData)

subgroup = relData%distribution

cmass = relData%rate
tdur  = HUGE(0.) !ScaleReal( relData%duration,HCF_HOUR2SEC )

sigx     = NOT_SET_R
sigy     = NOT_SET_R
sigz     = NOT_SET_R
size_rel = relData%diameter

IF( relData%exitVel(1) /= NOT_SET_R )umom = relData%exitVel(1)
IF( relData%exitVel(2) /= NOT_SET_R )vmom = relData%exitVel(2)
IF( relData%exitVel(3) /= NOT_SET_R )wmom = relData%exitVel(3)

IF( relData%exitTemp /= NOT_SET_R )buoy = relData%exitTemp

IF( relData%distribution == HD_LOGNORM )THEN
  rel_param(REL_MMD_INDX)   = relData%MMD
  rel_param(REL_SIGMA_INDX) = relData%sigma
END IF

rel_param(REL_WMFRAC_INDX) = relData%dryFrac
rel_param(REL_AFRAC_INDX)  = relData%activeFrac
rel_param(REL_NXTUPDT_INDX) = relData%nextUpdtTime

name_rel = TRIM(StripNull(relData%relFile))

RETURN
END
!*******************************************************************************
!        UnloadReleaseArea
!*******************************************************************************
SUBROUTINE UnloadReleaseArea( relDataIn )

USE convert_fd
USE release_fd
USE relparam_fd
USE scipuff_fi
USE release_fi

!     Load SCIPUFF commons from an SCIP Release structure

IMPLICIT NONE

TYPE( relGenT ), INTENT ( IN ) :: relDataIn

TYPE( relAreaT ) relData

REAL, EXTERNAL :: ScaleReal

!==== Unload

relData = TRANSFER(relDataIn,relData)

subgroup = relData%distribution

cmass = relData%rate
tdur  = ScaleReal( relData%duration,HCF_HOUR2SEC )

size_rel = NOT_SET_R

xlenA = relData%xlen
ylenA = relData%ylen
angA  = relData%ang

umom = DEF_VAL_R
vmom = DEF_VAL_R
IF( relData%exitVel  /= NOT_SET_R )wmom = relData%exitVel

IF( relData%exitTemp /= NOT_SET_R )buoy = relData%exitTemp

IF( relData%distribution == HD_LOGNORM )THEN
  rel_param(REL_MMD_INDX)   = relData%MMD
  rel_param(REL_SIGMA_INDX) = relData%sigma
END IF

rel_param(REL_WMFRAC_INDX) = relData%dryFrac
rel_param(REL_AFRAC_INDX)   = relData%activeFrac
rel_param(REL_NXTUPDT_INDX) = relData%nextUpdtTime

RETURN
END
!*******************************************************************************
!        UnloadReleaseAreaFile
!*******************************************************************************
SUBROUTINE UnloadReleaseAreaFile( relDataIn )

USE convert_fd
USE release_fd
USE relparam_fd
USE scipuff_fi
USE release_fi

!     Load SCIPUFF commons from an SCIP Release structure

IMPLICIT NONE

TYPE( relGenT ), INTENT ( IN ) :: relDataIn

TYPE( relAreaFileT ) relData

REAL, EXTERNAL :: ScaleReal

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: StripNull

!==== Unload

relData = TRANSFER(relDataIn,relData)

subgroup = relData%distribution

cmass = relData%rate
tdur  = ScaleReal( relData%duration,HCF_HOUR2SEC )

xlenA = relData%xlen
ylenA = relData%ylen
angA  = relData%ang

umom = DEF_VAL_R
vmom = DEF_VAL_R
IF( relData%exitVel  /= NOT_SET_R )wmom = relData%exitVel

IF( relData%exitTemp /= NOT_SET_R )buoy = relData%exitTemp

IF( relData%distribution == HD_LOGNORM )THEN
  rel_param(REL_MMD_INDX)   = relData%MMD
  rel_param(REL_SIGMA_INDX) = relData%sigma
END IF

rel_param(REL_WMFRAC_INDX) = relData%dryFrac
rel_param(REL_AFRAC_INDX)   = relData%activeFrac
!!DEC$ IF DEFINED (OPVIEW)
!rel_param(REL_NXTUPDT_INDX) = relData%nextUpdtTime
!!DEC$ ENDIF

name_rel = TRIM(StripNull(relData%relFile))

RETURN
END
!*******************************************************************************
!        LoadReleaseStack
!*******************************************************************************
SUBROUTINE LoadReleaseStack( relDataOut )

USE convert_fd
USE release_fd
USE relparam_fd
USE scipuff_fi
USE release_fi

!     Load an SCIP Release structure from SCIPUFF commons

IMPLICIT NONE

TYPE( relGenT ), INTENT( OUT ) :: relDataOut

TYPE( relStackT ) relData

REAL, EXTERNAL :: ScaleReal

!==== Load

relData%distribution = subgroup

relData%rate     = cmass
relData%duration = ScaleReal( tdur,HCF_SEC2HOUR )

relData%diameter = size_rel

relData%exitVel  = wmom

relData%exitTemp = buoy

IF( rel_param(REL_MMD_INDX)/= NOT_SET_R )THEN
  relData%MMD   = rel_param(REL_MMD_INDX)
  relData%sigma = rel_param(REL_SIGMA_INDX)
ELSE
  relData%MMD   = NOT_SET_R
  relData%sigma = NOT_SET_R
END IF

relData%activeFrac = rel_param(REL_AFRAC_INDX)
relData%nextUpdtTime = rel_param(REL_NXTUPDT_INDX)
relData%dryFrac = rel_param(REL_WMFRAC_INDX)
relData%padding = NOT_SET_I

relDataOut = TRANSFER(relData,relDataOut)

RETURN
END
!*******************************************************************************
!        LoadReleaseStack3
!*******************************************************************************
SUBROUTINE LoadReleaseStack3( relDataOut )

USE convert_fd
USE release_fd
USE relparam_fd
USE scipuff_fi
USE release_fi

!     Load an SCIP Release structure from SCIPUFF commons

IMPLICIT NONE

TYPE( relGenT ), INTENT( OUT ) :: relDataOut

TYPE( relStack3T ) relData

REAL, EXTERNAL :: ScaleReal

!==== Load

relData%distribution = subgroup

relData%rate     = cmass
relData%duration = ScaleReal( tdur,HCF_SEC2HOUR )

relData%diameter = size_rel

relData%exitVel(1) = umom
relData%exitVel(2) = vmom
relData%exitVel(3) = wmom

relData%exitTemp = buoy

IF( rel_param(REL_MMD_INDX)/= NOT_SET_R )THEN
  relData%MMD   = rel_param(REL_MMD_INDX)
  relData%sigma = rel_param(REL_SIGMA_INDX)
ELSE
  relData%MMD   = NOT_SET_R
  relData%sigma = NOT_SET_R
END IF

relData%activeFrac = rel_param(REL_AFRAC_INDX)
relData%nextUpdtTime = rel_param(REL_NXTUPDT_INDX)
relData%dryFrac = rel_param(REL_WMFRAC_INDX)
relData%padding = NOT_SET_I

relDataOut = TRANSFER(relData,relDataOut)

RETURN
END
!*******************************************************************************
!        LoadReleaseStackFile
!*******************************************************************************
SUBROUTINE LoadReleaseStackFile( relDataOut )

USE convert_fd
USE release_fd
USE relparam_fd
USE scipuff_fi
USE release_fi

!     Load an SCIP Release structure from SCIPUFF commons

IMPLICIT NONE

TYPE( relGenT ), INTENT( OUT ) :: relDataOut

TYPE( relStackFileT ) relData

REAL, EXTERNAL :: ScaleReal

!==== Load

relData%distribution = subgroup

relData%rate     = cmass
relData%duration = HUGE(0.) !ScaleReal( tdur,HCF_SEC2HOUR )

relData%diameter = size_rel

relData%exitVel  = wmom

relData%exitTemp = buoy

IF( rel_param(REL_MMD_INDX)/= NOT_SET_R )THEN
  relData%MMD   = rel_param(REL_MMD_INDX)
  relData%sigma = rel_param(REL_SIGMA_INDX)
ELSE
  relData%MMD   = NOT_SET_R
  relData%sigma = NOT_SET_R
END IF

relData%activeFrac = rel_param(REL_AFRAC_INDX)
relData%nextUpdtTime = rel_param(REL_NXTUPDT_INDX)
relData%dryFrac = rel_param(REL_WMFRAC_INDX)
relData%relFile = TRIM(name_rel)
relData%padding = NOT_SET_I

relDataOut = TRANSFER(relData,relDataOut)

RETURN
END
!*******************************************************************************
!        LoadReleaseStack3File
!*******************************************************************************
SUBROUTINE LoadReleaseStack3File( relDataOut )

USE convert_fd
USE release_fd
USE relparam_fd
USE scipuff_fi
USE release_fi

!     Load an SCIP Release structure from SCIPUFF commons

IMPLICIT NONE

TYPE( relGenT ), INTENT( OUT ) :: relDataOut

TYPE( relStack3FileT ) relData

REAL, EXTERNAL :: ScaleReal

!==== Load

relData%distribution = subgroup

relData%rate     = cmass
relData%duration = HUGE(0.) !ScaleReal( tdur,HCF_SEC2HOUR )

relData%diameter = size_rel

relData%exitVel(1) = umom
relData%exitVel(2) = vmom
relData%exitVel(3) = wmom

relData%exitTemp = buoy

IF( rel_param(REL_MMD_INDX)/= NOT_SET_R )THEN
  relData%MMD   = rel_param(REL_MMD_INDX)
  relData%sigma = rel_param(REL_SIGMA_INDX)
ELSE
  relData%MMD   = NOT_SET_R
  relData%sigma = NOT_SET_R
END IF

relData%activeFrac = rel_param(REL_AFRAC_INDX)
relData%nextUpdtTime = rel_param(REL_NXTUPDT_INDX)
relData%dryFrac = rel_param(REL_WMFRAC_INDX)
relData%relFile = TRIM(name_rel)
relData%padding = NOT_SET_I

relDataOut = TRANSFER(relData,relDataOut)

RETURN
END
!*******************************************************************************
!        LoadReleaseArea
!*******************************************************************************
SUBROUTINE LoadReleaseArea( relDataOut )

USE convert_fd
USE release_fd
USE relparam_fd
USE scipuff_fi
USE release_fi

!     Load an SCIP Release structure from SCIPUFF commons

IMPLICIT NONE

TYPE( relGenT ), INTENT( OUT ) :: relDataOut

TYPE( relAreaT ) relData

REAL,    EXTERNAL :: ScaleReal

!==== Load

relData%distribution = subgroup

relData%rate     = cmass
relData%duration = ScaleReal( tdur,HCF_SEC2HOUR )

IF( ABS(LOG10(xlenA/ylenA)) > 1. )nError = WN_ERROR  !Caution for large aspect ratio

relData%xlen = xlenA
relData%ylen = ylenA
relData%ang  = angA

relData%exitVel  = wmom

relData%exitTemp = buoy

IF( rel_param(REL_MMD_INDX)/= NOT_SET_R )THEN
  relData%MMD   = rel_param(REL_MMD_INDX)
  relData%sigma = rel_param(REL_SIGMA_INDX)
ELSE
  relData%MMD   = NOT_SET_R
  relData%sigma = NOT_SET_R
END IF

relData%activeFrac = rel_param(REL_AFRAC_INDX)
relData%nextUpdtTime = rel_param(REL_NXTUPDT_INDX)
relData%dryFrac = rel_param(REL_WMFRAC_INDX)
relData%padding = NOT_SET_I

relDataOut = TRANSFER(relData,relDataOut)

9999 CONTINUE

RETURN
END
!*******************************************************************************
!        LoadReleaseAreaFile
!*******************************************************************************
SUBROUTINE LoadReleaseAreaFile( relDataOut )

USE convert_fd
USE release_fd
USE relparam_fd
USE scipuff_fi
USE release_fi

!     Load an SCIP Release structure from SCIPUFF commons

IMPLICIT NONE

TYPE( relGenT ), INTENT( OUT ) :: relDataOut

TYPE( relAreaFileT ) relData

REAL,    EXTERNAL :: ScaleReal

!==== Load

relData%distribution = subgroup

relData%rate     = cmass
relData%duration = ScaleReal( tdur,HCF_SEC2HOUR )

IF( ABS(LOG10(xlenA/ylenA)) > 1. )nError = WN_ERROR

relData%xlen = xlenA
relData%ylen = ylenA
relData%ang  = angA

relData%exitVel  = wmom

relData%exitTemp = buoy

IF( rel_param(REL_MMD_INDX)/= NOT_SET_R )THEN
  relData%MMD   = rel_param(REL_MMD_INDX)
  relData%sigma = rel_param(REL_SIGMA_INDX)
ELSE
  relData%MMD   = NOT_SET_R
  relData%sigma = NOT_SET_R
END IF

relData%activeFrac = rel_param(REL_AFRAC_INDX)
!!DEC$ IF DEFINED (OPVIEW)
!relData%nextUpdtTime = rel_param(REL_NXTUPDT_INDX)
!!DEC$ ENDIF
relData%dryFrac = rel_param(REL_WMFRAC_INDX)
relData%relFile = TRIM(name_rel)
relData%padding = NOT_SET_I

relDataOut = TRANSFER(relData,relDataOut)

9999 CONTINUE

RETURN
END
!*******************************************************************************
!        UnloadReleasePool
!*******************************************************************************
SUBROUTINE UnloadReleasePool( relDataIn )

USE release_fd
USE scipuff_fi
USE relparam_fd
USE release_fi

!     Load SCIPUFF commons from an SCIP Release structure

IMPLICIT NONE

TYPE( relGenT ), INTENT( IN ) :: relDataIn

TYPE( relPoolT ) relData

!==== Unload

relData = TRANSFER(relDataIn,relData)

cmass = relData%mass
tdur  = DEF_VAL_R          !SCIPUFF expects this
sigx  = relData%sizeX
sigy  = relData%sizeY
sigz  = 0.0                !SCIPUFF expects this
size_rel = NOT_SET_R

rel_param(REL_AFRAC_INDX) = relData%activeFrac

RETURN
END
!*******************************************************************************
!        LoadReleasePool
!*******************************************************************************
SUBROUTINE LoadReleasePool( relDataOut )

USE release_fd
USE scipuff_fi
USE release_fi
USE relparam_fd

!     Load an SCIP Release structure from SCIPUFF commons

IMPLICIT NONE

TYPE( relGenT ), INTENT( OUT ) :: relDataOut

TYPE( relPoolT ) relData

!==== Load

relData%mass  = cmass

IF( size_rel > 0 )THEN
  relData%sizeX = size_rel
  relData%sizeY = size_rel
ELSE
  relData%sizeX = sigx
  relData%sizeY = sigy
END IF

relData%padding = NOT_SET_I

relData%activeFrac = rel_param(REL_AFRAC_INDX)


relDataOut = TRANSFER(relData,relDataOut)

RETURN
END
!*******************************************************************************
!        UnloadReleaseFile
!*******************************************************************************
SUBROUTINE UnloadReleaseFile( relDataIn )

USE release_fd
USE relparam_fd
USE scipuff_fi
USE release_fi

!     Load SCIPUFF commons from an SCIP Release structure

IMPLICIT NONE

TYPE( relGenT ), INTENT( IN) :: relDataIn

TYPE( relFileT ) relData

REAL,                      EXTERNAL :: Int2Real
CHARACTER(PATH_MAXLENGTH), EXTERNAL :: StripNull

!==== Unload

relData = TRANSFER(relDataIn,relData)

IF( relData%nRandom > 0 )THEN
  rel_param(REL_RAND_INDX)   = Int2Real( relData%nRandom )
  rel_param(REL_SEED_INDX)   = Int2Real( relData%ranSeed )
  rel_param(REL_SPREAD_INDX) = relData%ranSpread
ELSE
  rel_param(REL_RAND_INDX)   = NOT_SET_R
  rel_param(REL_SEED_INDX)   = NOT_SET_R
  rel_param(REL_SPREAD_INDX) = NOT_SET_R
END IF

rel_param(REL_SPREADT_INDX) = NOT_SET_R
rel_param(REL_SPREADV_INDX) = NOT_SET_R
rel_param(REL_RANDIR_INDX)  = NOT_SET_R

name_rel = TRIM(StripNull(relData%relFile))

RETURN
END
!*******************************************************************************
!        LoadReleaseFile
!*******************************************************************************
SUBROUTINE LoadReleaseFile( relDataOut )

USE release_fd
USE relparam_fd
USE scipuff_fi
USE release_fi

!     Load an SCIP Release structure from SCIPUFF commons

IMPLICIT NONE

TYPE( relGenT ), INTENT( OUT ) :: relDataOut

TYPE( relFileT ) relData

INTEGER, EXTERNAL :: Real2Int

!==== Load

IF( rel_param(REL_RAND_INDX) > 0 )THEN
  relData%nRandom   = Real2Int( rel_param(REL_RAND_INDX) )
  relData%ranSeed   = Real2Int( rel_param(REL_SEED_INDX) )
  relData%ranSpread = rel_param(REL_SPREAD_INDX)
ELSE
  relData%nRandom   = NOT_SET_I
  relData%ranSeed   = NOT_SET_I
  relData%ranSpread = NOT_SET_R
END IF

relData%padding = NOT_SET_I

relData%relFile = TRIM(name_rel)

relDataOut = TRANSFER(relData,relDataOut)

RETURN
END
