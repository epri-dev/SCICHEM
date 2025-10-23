!===============================================================================
!     getReleaseRandomParams
!===============================================================================
SUBROUTINE getReleaseRandomParams( release,nRandom,nSeed,rSpread,rDir )

USE default_fd
USE release_fd

IMPLICIT NONE

TYPE( ReleaseT ),   INTENT(IN ) :: release
INTEGER,            INTENT(OUT) :: nRandom
INTEGER,            INTENT(OUT) :: nSeed
REAL, DIMENSION(3), INTENT(OUT) :: rSpread
REAL,               INTENT(OUT) :: rDir

TYPE( relFileT   ) relDataF
TYPE( relInstT   ) relDataI
TYPE( relXInstT  ) relDataXI
TYPE( relXInst3T ) relDataXI3

nRandom = NOT_SET_I
nSeed = 0
rSpread = NOT_SET_R
rDir = 0.0

SELECT CASE( release%type )
  CASE( HR_INST )
    relDataI = TRANSFER(release%relData,relDataI)
    nRandom = relDataI%nRandom
    nSeed = relDataI%ranSeed
    rSpread(1) = relDataI%ranSpread
  CASE( HR_XINST )
    relDataXI = TRANSFER(release%relData,relDataXI)
    nRandom = relDataXI%nRandom
    nSeed = relDataXI%ranSeed
    rSpread(1) = relDataXI%ranSpread
  CASE( HR_XINST3 )
    relDataXI3 = TRANSFER(release%relData,relDataXI3)
    nRandom = relDataXI3%nRandom
    nSeed = relDataXI3%ranSeed
    rSpread(1) = relDataXI3%ranSpreadA
    rSpread(2) = relDataXI3%ranSpreadT
    rSpread(3) = relDataXI3%ranSpreadV
    rDir = relDataXI3%ranDir
  CASE( HR_FINST )
    relDataI = TRANSFER(release%relData,relDataI)
    nRandom = relDataI%nRandom
    nSeed = relDataI%ranSeed
    rSpread(1) = relDataI%ranSpread
  CASE( HR_FILE )
    relDataF = TRANSFER(release%relData,relDataF)
    nRandom = relDataF%nRandom
    nSeed = relDataF%ranSeed
    rSpread(1) = relDataF%ranSpread
  CASE DEFAULT
END SELECT

RETURN

END
!===============================================================================
!     getReleaseSigmas
!===============================================================================
SUBROUTINE getReleaseSigmas( release,sigx,sigy,sigz,Rxy,Rxz,Ryz )

USE default_fd
USE release_fd
USE scipuff_fi
USE sciprime_fi

IMPLICIT NONE

TYPE( ReleaseT ), INTENT(IN ) :: release
REAL,             INTENT(OUT) :: sigx
REAL,             INTENT(OUT) :: sigy
REAL,             INTENT(OUT) :: sigz
REAL,             INTENT(OUT) :: Rxy
REAL,             INTENT(OUT) :: Rxz
REAL,             INTENT(OUT) :: Ryz

TYPE( relContT       ) relDataC
TYPE( relContFileT   ) relDataCF
TYPE( relStackFileT  ) relDataSF
TYPE( relStack3FileT ) relDataSF3
TYPE( relAreaT       ) relDataA
TYPE( relAreaFileT   ) relDataAF
TYPE( relInstT       ) relDataI
TYPE( relXInstT      ) relDataXI
TYPE( relXInst3T     ) relDataXI3
TYPE( relMoveT       ) relDataM
TYPE( relPoolT       ) relDataP
TYPE( relStackT      ) relDataS
TYPE( relStack3T     ) relDataS3

INTEGER iflag

sigx = 0.0
sigy = 0.0
sigz = 0.0
Rxy = 0.0
Rxz = 0.0
Ryz = 0.0

SELECT CASE( release%type )
  CASE( HR_INST )
    relDataI = TRANSFER(release%relData,relDataI)
    sigx = relDataI%sigX
    sigy = relDataI%sigY
    sigz = relDataI%sigZ
  CASE( HR_XINST )
    relDataXI = TRANSFER(release%relData,relDataXI)
    sigx = relDataXI%sigX
    sigy = relDataXI%sigY
    sigz = relDataXI%sigZ
    Rxy = relDataXI%sigRxy
    Rxz = relDataXI%sigRxz
    Ryz = relDataXI%sigRyz
  CASE( HR_XINST3 )
    relDataXI3 = TRANSFER(release%relData,relDataXI3)
    sigx = relDataXI3%sigX
    sigy = relDataXI3%sigY
    sigz = relDataXI3%sigZ
    Rxy = relDataXI3%sigRxy
    Rxz = relDataXI3%sigRxz
    Ryz = relDataXI3%sigRyz
  CASE( HR_FINST )
    relDataI = TRANSFER(release%relData,relDataI)
    sigx = relDataI%sigX
    sigy = relDataI%sigY
    sigz = relDataI%sigZ
  CASE( HR_CONT )
    relDataC = TRANSFER(release%relData,relDataC)
    sigy = relDataC%sigY
    sigz = relDataC%sigZ
  CASE( HR_MOVE )
    relDataM = TRANSFER(release%relData,relDataM)
    sigy = relDataM%sigY
    sigz = relDataM%sigZ
  CASE( HR_CONTF )
    relDataCF = TRANSFER(release%relData,relDataCF)
    sigy = relDataCF%sigY
    sigz = relDataCF%sigZ
  CASE( HR_STACKF )
    relDataSF = TRANSFER(release%relData,relDataSF)
    sigx = 0.5*relDataSF%diameter
    sigy = 0.5*relDataSF%diameter
  CASE( HR_STACK3F )
    relDataSF3 = TRANSFER(release%relData,relDataSF3)
    sigx = 0.5*relDataSF3%diameter
    sigy = 0.5*relDataSF3%diameter
    IF( relDataSF3%exitvel(1) /= DEF_VAL_R )THEN
      sigz = 0.5*relDataSF3%diameter
    END IF
  CASE( HR_STACK )
    relDataS = TRANSFER(release%relData,relDataS)
    sigx = 0.5*relDataS%diameter
    sigy = 0.5*relDataS%diameter
  CASE( HR_STACK3 )
    relDataS3 = TRANSFER(release%relData,relDataS3)
    sigx = 0.5*relDataS3%diameter
    sigy = 0.5*relDataS3%diameter
    IF( relDataS3%exitvel(1) /= DEF_VAL_R )THEN
      sigz = 0.5*relDataS3%diameter
    END IF
  CASE( HR_STACKFP )
    iflag = release%padding
    IF( iflag == -1 )THEN
      relDataS = TRANSFER(release%relData,relDataS)
      sigx = 0.5*relDataS%diameter
      sigy = 0.5*relDataS%diameter
    ELSE
      IF( wake )THEN
        sigx = sigy_prm(iflag)
        sigy = sigy_prm(iflag)
        sigz = sigz_prm(iflag)
      ELSE
        sigy = sigy_prm(iflag)
        sigz = sigz_prm(iflag)
      END IF
    END IF
  CASE( HR_AREA )
    relDataA = TRANSFER(release%relData,relDataA)
    CALL SetAreaSig( relDataA%xlen,relDataA%ylen,relDataA%ang,sigx,sigy,Rxy )
  CASE( HR_AREAF )
    relDataAF = TRANSFER(release%relData,relDataAF)
    CALL SetAreaSig( relDataAF%xlen,relDataAF%ylen,relDataAF%ang,sigx,sigy,Rxy )
  CASE( HR_PRIME )
    iflag = release%padding
    IF( iflag == -1 )THEN
      relDataS = TRANSFER(release%relData,relDataS)
      sigx = 0.5*relDataS%diameter
      sigy = 0.5*relDataS%diameter
    ELSE
      IF( wake )THEN
        sigx = sigy_prm(iflag)
        sigy = sigy_prm(iflag)
        sigz = sigz_prm(iflag)
      ELSE
        sigy = sigy_prm(iflag)
        sigz = sigz_prm(iflag)
      END IF
    END IF
  CASE( HR_POOL )
    relDataP = TRANSFER(release%relData,relDataP)
    sigx = relDataP%sizeX
    sigy = relDataP%sizeY
  CASE DEFAULT
END SELECT

RETURN
END
!===============================================================================
!     getReleaseActiveFraction
!===============================================================================
SUBROUTINE getReleaseActiveFraction( release,frac )

USE default_fd
USE release_fd

IMPLICIT NONE

TYPE( ReleaseT ), INTENT(IN ) :: release
REAL,             INTENT(OUT) :: frac

TYPE( relContT       ) relDataC
TYPE( relContFileT   ) relDataCF
TYPE( relStackFileT  ) relDataSF
TYPE( relStack3FileT ) relDataSF3
TYPE( relAreaT       ) relDataA
TYPE( relInstT       ) relDataI
TYPE( relXInstT      ) relDataXI
TYPE( relXInst3T     ) relDataXI3
TYPE( relMoveT       ) relDataM
TYPE( relPoolT       ) relDataP
TYPE( relStackT      ) relDataS
TYPE( relStack3T     ) relDataS3

frac = 1.0

SELECT CASE( release%type )
  CASE( HR_INST )
    relDataI = TRANSFER(release%relData,relDataI)
    frac = relDataI%activeFrac
  CASE( HR_XINST )
    relDataXI = TRANSFER(release%relData,relDataXI)
    frac = relDataXI%activeFrac
  CASE( HR_XINST3 )
    relDataXI3 = TRANSFER(release%relData,relDataXI3)
    frac = relDataXI3%activeFrac
  CASE( HR_FINST )
    relDataI = TRANSFER(release%relData,relDataI)
    frac = relDataI%activeFrac
  CASE( HR_CONT )
    relDataC = TRANSFER(release%relData,relDataC)
    frac = relDataC%activeFrac
  CASE( HR_MOVE )
    relDataM = TRANSFER(release%relData,relDataM)
    frac = relDataM%activeFrac
  CASE( HR_CONTF )
    relDataCF = TRANSFER(release%relData,relDataCF)
    frac = relDataCF%activeFrac
  CASE( HR_STACKF,HR_STACKFP )
    relDataSF = TRANSFER(release%relData,relDataSF)
    frac = relDataSF%activeFrac
  CASE( HR_STACK3F )
    relDataSF3 = TRANSFER(release%relData,relDataSF3)
    frac = relDataSF3%activeFrac
CASE( HR_AREA )
    relDataA = TRANSFER(release%relData,relDataA)
    frac = relDataA%activeFrac
CASE( HR_AREAF )
    relDataA = TRANSFER(release%relData,relDataA)
    frac = relDataA%activeFrac
CASE( HR_STACK )
    relDataS = TRANSFER(release%relData,relDataS)
    frac = relDataS%activeFrac
  CASE( HR_STACK3 )
    relDataS3 = TRANSFER(release%relData,relDataS3)
    frac = relDataS3%activeFrac
  CASE( HR_PRIME )
    relDataS = TRANSFER(release%relData,relDataS)
    frac = relDataS%activeFrac
  CASE( HR_POOL )
    relDataP = TRANSFER(release%relData,relDataP)
    frac = relDataP%activeFrac
  CASE DEFAULT
END SELECT

RETURN
END
!===============================================================================
!     getReleaseDryFraction
!===============================================================================
SUBROUTINE getReleaseDryFraction( release,frac )

USE default_fd
USE release_fd

IMPLICIT NONE

TYPE( ReleaseT ), INTENT(IN ) :: release
REAL,             INTENT(OUT) :: frac

TYPE( relContT       ) relDataC
TYPE( relContFileT   ) relDataCF
TYPE( relStackFileT  ) relDataSF
TYPE( relStack3FileT ) relDataSF3
TYPE( relAreaT       ) relDataA
TYPE( relInstT       ) relDataI
TYPE( relXInstT      ) relDataXI
TYPE( relXInst3T     ) relDataXI3
TYPE( relMoveT       ) relDataM
TYPE( relStackT      ) relDataS
TYPE( relStack3T     ) relDataS3

frac = 1.0

SELECT CASE( release%type )
  CASE( HR_INST )
    relDataI = TRANSFER(release%relData,relDataI)
    frac = relDataI%dryFrac
  CASE( HR_XINST )
    relDataXI = TRANSFER(release%relData,relDataXI)
    frac = relDataXI%dryFrac
  CASE( HR_XINST3 )
    relDataXI3 = TRANSFER(release%relData,relDataXI3)
    frac = relDataXI3%dryFrac
  CASE( HR_FINST )
    relDataI = TRANSFER(release%relData,relDataI)
    frac = relDataI%dryFrac
  CASE( HR_CONT )
    relDataC = TRANSFER(release%relData,relDataC)
    frac = relDataC%dryFrac
  CASE( HR_MOVE )
    relDataM = TRANSFER(release%relData,relDataM)
    frac = relDataM%dryFrac
  CASE( HR_CONTF )
    relDataCF = TRANSFER(release%relData,relDataCF)
    frac = relDataCF%dryFrac
  CASE( HR_STACKF,HR_STACKFP )
    relDataSF = TRANSFER(release%relData,relDataSF)
    frac = relDataSF%dryFrac
  CASE( HR_STACK3F )
    relDataSF3 = TRANSFER(release%relData,relDataSF3)
    frac = relDataSF3%dryFrac
CASE( HR_AREA )
    relDataA = TRANSFER(release%relData,relDataA)
    frac = relDataA%dryfrac
CASE( HR_AREAF )
    relDataA = TRANSFER(release%relData,relDataA)
    frac = relDataA%dryfrac
CASE( HR_STACK )
    relDataS = TRANSFER(release%relData,relDataS)
    frac = relDataS%dryfrac
  CASE( HR_STACK3 )
    relDataS3 = TRANSFER(release%relData,relDataS3)
    frac = relDataS3%dryfrac
  CASE( HR_PUFF )
    frac = NOT_SET_R
  CASE( HR_PRIME )
    relDataS = TRANSFER(release%relData,relDataS)
    frac = relDataS%dryFrac
  CASE DEFAULT
END SELECT

RETURN
END
!===============================================================================
!     getReleaseDuration
!===============================================================================
SUBROUTINE getReleaseDuration( release,tdur )

USE default_fd
USE convert_fd
USE release_fd

IMPLICIT NONE

TYPE( ReleaseT ), INTENT(IN ) :: release
REAL,             INTENT(OUT) :: tdur

TYPE( relContT       ) relDataC
TYPE( relMoveT       ) relDataM
TYPE( relStackT      ) relDataS
TYPE( relStack3T     ) relDataS3
TYPE( relAreaT       ) relDataA

REAL, EXTERNAL :: ScaleReal

tdur = 0.0

SELECT CASE( release%type )
  CASE( HR_CONT )
    relDataC = TRANSFER(release%relData,relDataC)
    tdur = ScaleReal( relDataC%duration,HCF_HOUR2SEC )
  CASE( HR_MOVE )
    relDataM = TRANSFER(release%relData,relDataM)
    tdur = ScaleReal( relDataM%duration,HCF_HOUR2SEC )
  CASE( HR_CONTF )
    tdur = HUGE(0.)
  CASE( HR_STACKF,HR_STACKFP )
    tdur =  HUGE(0.)
  CASE( HR_STACK3F )
    tdur =  HUGE(0.)
  CASE( HR_AREAF )
    tdur =  HUGE(0.)
  CASE( HR_AREA )
    relDataA = TRANSFER(release%relData,relDataA)
    tdur = ScaleReal( relDataA%duration,HCF_HOUR2SEC )
  CASE( HR_STACK )
    relDataS = TRANSFER(release%relData,relDataS)
    tdur = ScaleReal( relDataS%duration,HCF_HOUR2SEC )
  CASE( HR_STACK3 )
    relDataS3 = TRANSFER(release%relData,relDataS3)
    tdur = ScaleReal( relDataS3%duration,HCF_HOUR2SEC )
  CASE( HR_POOL )
    tdur = DEF_VAL_R
  CASE( HR_PRIME )
    relDataS = TRANSFER(release%relData,relDataS)
    tdur = ScaleReal( relDataS%duration,HCF_HOUR2SEC )
  CASE DEFAULT
END SELECT

RETURN
END
!===============================================================================
!     getReleaseDynamics
!===============================================================================
SUBROUTINE getReleaseDynamics( release,buoy,mom )

USE default_fd
USE release_fd
USE scipuff_fi
USE sciprime_fi

IMPLICIT NONE

TYPE( ReleaseT ),   INTENT(IN ) :: release
REAL,               INTENT(OUT) :: buoy
REAL, DIMENSION(3), INTENT(OUT) :: mom

TYPE( relContT       ) relDataC
TYPE( relContFileT   ) relDataCF
TYPE( relStackFileT  ) relDataSF
TYPE( relStack3FileT ) relDataSF3
TYPE( relAreaT       ) relDataA
TYPE( relInstT       ) relDataI
TYPE( relXInstT      ) relDataXI
TYPE( relXInst3T     ) relDataXI3
TYPE( relMoveT       ) relDataM
TYPE( relStackT      ) relDataS
TYPE( relStack3T     ) relDataS3

REAL sigx,sigy
INTEGER iflag

buoy = 0.0
mom  = 0.0

SELECT CASE( release%type )
  CASE( HR_INST )
    relDataI = TRANSFER(release%relData,relDataI)
    buoy   = relDataI%buoyancy
    mom(3) = relDataI%momentum
  CASE( HR_XINST )
    relDataXI = TRANSFER(release%relData,relDataXI)
    buoy   = relDataXI%buoyancy
    mom(3) = relDataXI%momentum
  CASE( HR_XINST3 )
    relDataXI3 = TRANSFER(release%relData,relDataXI3)
    buoy   = relDataXI3%buoyancy
    mom(3) = relDataXI3%momentum
  CASE( HR_FINST )
    relDataI = TRANSFER(release%relData,relDataI)
    buoy   = relDataI%buoyancy
    mom(3) = relDataI%momentum
  CASE( HR_CONT )
    relDataC = TRANSFER(release%relData,relDataC)
    buoy   = relDataC%buoyancy
    mom(3) = relDataC%momentum
  CASE( HR_MOVE )
    relDataM = TRANSFER(release%relData,relDataM)
    buoy   = relDataM%buoyancy
    mom(3) = relDataM%momentum
  CASE( HR_CONTF )
    relDataCF = TRANSFER(release%relData,relDataCF)
    buoy   = relDataCF%buoyancy
    mom(3) = relDataCF%momentum
  CASE( HR_STACKF )
    relDataSF = TRANSFER(release%relData,relDataSF)
    buoy = relDataSF%exitTemp
    mom(1) = DEF_VAL_R
    mom(2) = DEF_VAL_R
    mom(3) = relDataSF%exitVel
    sigx = 0.5*relDataSF%diameter
    sigy = 0.5*relDataSF%diameter
    CALL set_stack_params( sigx,sigy,buoy,mom )
  CASE( HR_AREA )
    relDataA = TRANSFER(release%relData,relDataA)
    buoy = DEF_VAL_R  !****relDataA%exitTemp
    mom(1) = DEF_VAL_R
    mom(2) = DEF_VAL_R
    mom(3) = relDataA%exitVel
    CALL set_area_params( relDataA%xlen*relDataA%ylen,buoy,mom )
  CASE( HR_AREAF )
    relDataA = TRANSFER(release%relData,relDataA)
    buoy = DEF_VAL_R  !****relDataA%exitTemp
    mom(1) = DEF_VAL_R
    mom(2) = DEF_VAL_R
    mom(3) = relDataA%exitVel
    CALL set_area_params( relDataA%xlen*relDataA%ylen,buoy,mom )
  CASE( HR_STACKFP )
    iflag = release%padding
    IF( iflag == -1 )THEN
      relDataSF = TRANSFER(release%relData,relDataSF)
      buoy = relDataSF%exitTemp
      mom(3) = relDataSF%exitVel
    ELSE
      IF( wake )THEN
        buoy   = buoy_prm(iflag)
        mom(3) = wmom_prm(iflag)
      ELSE
        buoy   = 0.0
        mom(3) = 0.0
      END IF
    END IF
  CASE( HR_STACK3F )
    relDataSF3 = TRANSFER(release%relData,relDataSF3)
    buoy = relDataSF3%exitTemp
    mom  = relDataSF3%exitVel
    sigx = 0.5*relDataSF3%diameter
    sigy = 0.5*relDataSF3%diameter
    CALL set_stack_params( sigx,sigy,buoy,mom )
  CASE( HR_STACK )
    relDataS = TRANSFER(release%relData,relDataS)
    buoy = relDataS%exitTemp
    mom(1) = DEF_VAL_R
    mom(2) = DEF_VAL_R
    mom(3) = relDataS%exitVel
    sigx = 0.5*relDataS%diameter
    sigy = 0.5*relDataS%diameter
    CALL set_stack_params( sigx,sigy,buoy,mom )
  CASE( HR_STACK3 )
    relDataS3 = TRANSFER(release%relData,relDataS3)
    buoy = relDataS3%exitTemp
    mom  = relDataS3%exitVel
    sigx = 0.5*relDataS3%diameter
    sigy = 0.5*relDataS3%diameter
    CALL set_stack_params( sigx,sigy,buoy,mom )
  CASE( HR_PRIME )
    iflag = release%padding
    IF( iflag == -1 )THEN
      relDataS = TRANSFER(release%relData,relDataS)
      buoy   = relDataS%exitTemp
      mom(3) = relDataS%exitVel
    ELSE
      IF( wake )THEN
        buoy   = buoy_prm(iflag)
        mom(3) = wmom_prm(iflag)
      ELSE
        buoy   = 0.0
        mom(3) = 0.0
      END IF
    END IF
  CASE DEFAULT
END SELECT

RETURN
END
!===============================================================================
!     getReleaseVelocity
!===============================================================================
SUBROUTINE getReleaseVelocity( release,vel )

USE default_fd
USE release_fd

IMPLICIT NONE

TYPE( ReleaseT ),   INTENT(IN ) :: release
REAL, DIMENSION(3), INTENT(OUT) :: vel

TYPE( relMoveT       ) relDataM

vel = 0.0

SELECT CASE( release%type )
  CASE( HR_MOVE )
    relDataM = TRANSFER(release%relData,relDataM)
    vel(1) = relDataM%velX
    vel(2) = relDataM%velY
    vel(3) = relDataM%velZ
  CASE DEFAULT
END SELECT

RETURN
END
!===============================================================================
!     getReleaseFile
!===============================================================================
SUBROUTINE getReleaseFile( release,file )

USE default_fd
USE release_fd

IMPLICIT NONE

TYPE( ReleaseT ),          INTENT(IN ) :: release
CHARACTER(PATH_MAXLENGTH), INTENT(OUT) :: file

TYPE( relFileT       ) relDataF
TYPE( relContFileT   ) relDataCF
TYPE( relStackFileT  ) relDataSF
TYPE( relStack3FileT ) relDataSF3
TYPE( relAreaFileT   ) relDataAF

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: StripNull

file = ' '

SELECT CASE( release%type )
  CASE( HR_FILE )
    relDataF = TRANSFER(release%relData,relDataF)
    file = relDataF%relFile
  CASE( HR_CONTF )
    relDataCF = TRANSFER(release%relData,relDataCF)
    file = relDataCF%relFile
  CASE( HR_STACKF,HR_STACKFP )
    relDataSF = TRANSFER(release%relData,relDataSF)
    file = relDataSF%relFile
  CASE( HR_STACK3F )
    relDataSF3 = TRANSFER(release%relData,relDataSF3)
    file = relDataSF3%relFile
  CASE( HR_AREAF )
    relDataAF = TRANSFER(release%relData,relDataAF)
    file = relDataAF%relFile
  CASE DEFAULT
 END SELECT

file = TRIM(StripNull(file))

RETURN
END
!===============================================================================
!     getReleaseMass
!===============================================================================
SUBROUTINE getReleaseMass( release,mass )

USE default_fd
USE release_fd
USE scipuff_fi
USE sciprime_fi

IMPLICIT NONE

TYPE( ReleaseT ), INTENT(IN ) :: release
REAL,             INTENT(OUT) :: mass

TYPE( relContT       ) relDataC
TYPE( relContFileT   ) relDataCF
TYPE( relStackFileT  ) relDataSF
TYPE( relStack3FileT ) relDataSF3
TYPE( relAreaT       ) relDataA
TYPE( relInstT       ) relDataI
TYPE( relXInstT      ) relDataXI
TYPE( relXInst3T     ) relDataXI3
TYPE( relMoveT       ) relDataM
TYPE( relPoolT       ) relDataP
TYPE( relStackT      ) relDataS
TYPE( relStack3T     ) relDataS3

INTEGER iflag

mass = NOT_SET_R

SELECT CASE( release%type )
  CASE( HR_INST )
    relDataI = TRANSFER(release%relData,relDataI)
    mass = relDataI%mass
  CASE( HR_XINST )
    relDataXI = TRANSFER(release%relData,relDataXI)
    mass = relDataXI%mass
  CASE( HR_XINST3 )
    relDataXI3 = TRANSFER(release%relData,relDataXI3)
    mass = relDataXI3%mass
  CASE( HR_FINST )
    relDataI = TRANSFER(release%relData,relDataI)
    mass = relDataI%mass
  CASE( HR_CONT )
    relDataC = TRANSFER(release%relData,relDataC)
    mass = relDataC%rate
  CASE( HR_MOVE )
    relDataM = TRANSFER(release%relData,relDataM)
    mass = relDataM%rate
  CASE( HR_CONTF )
    relDataCF = TRANSFER(release%relData,relDataCF)
    mass = relDataCF%rate
  CASE( HR_STACKF )
    relDataSF = TRANSFER(release%relData,relDataSF)
    mass = relDataSF%rate
  CASE( HR_STACK3F )
    relDataSF3 = TRANSFER(release%relData,relDataSF3)
    mass = relDataSF3%rate
  CASE( HR_AREA )
    relDataA = TRANSFER(release%relData,relDataA)
    mass = relDataA%rate
  CASE( HR_AREAF )
    relDataA = TRANSFER(release%relData,relDataA)
    mass = relDataA%rate
  CASE( HR_STACKFP )
    iflag = release%padding
    IF( iflag == -1 )THEN
      relDataSF = TRANSFER(release%relData,relDataSF)
      mass = relDataSF%rate
    ELSE
      IF( wake )THEN
        mass = cmass_prm*frac_prm(iflag)
      ELSE
        relDataSF = TRANSFER(release%relData,relDataSF)
        mass = relDataSF%rate
      END IF
    END IF
  CASE( HR_STACK )
    relDataS = TRANSFER(release%relData,relDataS)
    mass = relDataS%rate
  CASE( HR_STACK3 )
    relDataS3 = TRANSFER(release%relData,relDataS3)
    mass = relDataS3%rate
  CASE( HR_PRIME )
    iflag = release%padding
    IF( iflag == -1 )THEN
      relDataS = TRANSFER(release%relData,relDataS)
      mass = relDataS%rate
    ELSE
      IF( wake )THEN
        mass = cmass_prm*frac_prm(iflag)
      ELSE
        relDataS = TRANSFER(release%relData,relDataS)
        mass = relDataS%rate
      END IF
    END IF
  CASE( HR_POOL )
    relDataP = TRANSFER(release%relData,relDataP)
    mass = relDataP%mass
  CASE DEFAULT
END SELECT

RETURN
END
!===============================================================================
!     getReleaseDistribution
!===============================================================================
SUBROUTINE getReleaseDistribution( release,distribution,MMD,sigma )

USE default_fd
USE release_fd

IMPLICIT NONE

TYPE( ReleaseT ), INTENT(IN ) :: release
INTEGER,          INTENT(OUT) :: distribution
REAL,             INTENT(OUT) :: MMD
REAL,             INTENT(OUT) :: sigma

TYPE( relContT       ) relDataC
TYPE( relContFileT   ) relDataCF
TYPE( relStackFileT  ) relDataSF
TYPE( relStack3FileT ) relDataSF3
TYPE( relAreaT       ) relDataA
TYPE( relInstT       ) relDataI
TYPE( relXInstT      ) relDataXI
TYPE( relXInst3T     ) relDataXI3
TYPE( relMoveT       ) relDataM
TYPE( relStackT      ) relDataS
TYPE( relStack3T     ) relDataS3
TYPE( relPuffT       ) relDataP

distribution = NOT_SET_I
MMD = NOT_SET_R
sigma = NOT_SET_R

SELECT CASE( release%type )
  CASE( HR_INST )
    relDataI = TRANSFER(release%relData,relDataI)
    distribution = relDataI%distribution
    MMD = relDataI%MMD
    sigma = relDataI%sigma
  CASE( HR_XINST )
    relDataXI = TRANSFER(release%relData,relDataXI)
    distribution = relDataXI%distribution
    MMD = relDataXI%MMD
    sigma = relDataXI%sigma
  CASE( HR_XINST3 )
    relDataXI3 = TRANSFER(release%relData,relDataXI3)
    distribution = relDataXI3%distribution
    MMD = relDataXI3%MMD
    sigma = relDataXI3%sigma
  CASE( HR_FINST )
    relDataI = TRANSFER(release%relData,relDataI)
    distribution = relDataI%distribution
    MMD = relDataI%MMD
    sigma = relDataI%sigma
  CASE( HR_CONT )
    relDataC = TRANSFER(release%relData,relDataC)
    distribution = relDataC%distribution
    MMD = relDataC%MMD
    sigma = relDataC%sigma
  CASE( HR_MOVE )
    relDataM = TRANSFER(release%relData,relDataM)
    distribution = relDataM%distribution
    MMD = relDataM%MMD
    sigma = relDataM%sigma
  CASE( HR_CONTF )
    relDataCF = TRANSFER(release%relData,relDataCF)
    distribution = relDataCF%distribution
    MMD = relDataCF%MMD
    sigma = relDataCF%sigma
  CASE( HR_STACKF,HR_STACKFP )
    relDataSF = TRANSFER(release%relData,relDataSF)
    distribution = relDataSF%distribution
    MMD = relDataSF%MMD
    sigma = relDataSF%sigma
  CASE( HR_STACK3F )
    relDataSF3 = TRANSFER(release%relData,relDataSF3)
    distribution = relDataSF3%distribution
    MMD = relDataSF3%MMD
    sigma = relDataSF3%sigma
  CASE( HR_AREA )
    relDataA = TRANSFER(release%relData,relDataA)
    distribution = relDataA%distribution
    MMD = relDataA%MMD
    sigma = relDataA%sigma
  CASE( HR_AREAF )
    relDataA = TRANSFER(release%relData,relDataA)
    distribution = relDataA%distribution
    MMD = relDataA%MMD
    sigma = relDataA%sigma
  CASE( HR_POOL )
    distribution = 0
  CASE( HR_STACK )
    relDataS = TRANSFER(release%relData,relDataS)
    distribution = relDataS%distribution
    MMD = relDataS%MMD
    sigma = relDataS%sigma
  CASE( HR_STACK3 )
    relDataS3 = TRANSFER(release%relData,relDataS3)
    distribution = relDataS3%distribution
    MMD = relDataS3%MMD
    sigma = relDataS3%sigma
  CASE( HR_PUFF )
    relDataP = TRANSFER(release%relData,relDataP)
    distribution = relDataP%subgroup
  CASE( HR_PRIME )
    relDataS = TRANSFER(release%relData,relDataS)
    distribution = relDataS%distribution
    MMD = relDataS%MMD
    sigma = relDataS%sigma
  CASE DEFAULT
END SELECT

RETURN
END
!===============================================================================
!     getReleaseLocation
!===============================================================================
SUBROUTINE getReleaseLocation( release,met,xbar,ybar,zbar )

USE default_fd
USE release_fd
USE error_fi
USE cont_rel_fd
USE scipuff_fi
USE sciprime_fi

IMPLICIT NONE

TYPE( ReleaseT ),          INTENT( IN ) :: release
TYPE( cont_release_met  ), INTENT( IN ) :: met     !extra met data
REAL(8),                   INTENT( OUT) :: xbar
REAL(8),                   INTENT( OUT) :: ybar
REAL,                      INTENT( OUT) :: zbar

INTEGER iflag

SELECT CASE( release%type )
  CASE( HR_PRIME,HR_STACKFP )
    iflag = release%padding
    IF( iflag == -1 ) THEN
      xbar = release%xrel
      ybar = release%yrel
      zbar = met%zbar
    ELSE
      IF( wake )THEN
        xbar = xrel_prm(iflag)
        ybar = yrel_prm(iflag)
        zbar = zrel_prm(iflag)
      ELSE
        xbar = release%xrel
        ybar = release%yrel
        zbar = zrel_prm(iflag)
      END IF
    END IF
  CASE DEFAULT
    xbar = release%xrel
    ybar = release%yrel
    zbar = met%zbar
END SELECT

9999 CONTINUE

RETURN
END
!===============================================================================
!     getReleasePrime
!===============================================================================
SUBROUTINE getReleasePrime( release,kyprm,kzprm )

USE default_fd
USE release_fd
USE error_fi
USE cont_rel_fd
USE scipuff_fi
USE sciprime_fi

IMPLICIT NONE

TYPE( ReleaseT ), INTENT( IN  ) :: release
REAL,             INTENT( OUT ) :: kyprm
REAL,             INTENT( OUT ) :: kzprm

INTEGER iflag

kyprm = 0.0
kzprm = 0.0

SELECT CASE( release%type )
  CASE( HR_PRIME,HR_STACKFP )
    iflag = release%padding
    IF( wake .AND. iflag > 0 ) THEN
      kyprm = ky_prm(iflag)
      kzprm = kz_prm(iflag)
    END IF
  CASE DEFAULT
END SELECT

9999 CONTINUE

RETURN
END
!===============================================================================
!        LoadPuffFromPuffRelease
!===============================================================================
SUBROUTINE LoadPuffFromPuffRelease( relSpec,puffRelease )

USE release_fd
USE scipuff_fi

IMPLICIT NONE

TYPE( releaseSpecT ), INTENT( INOUT ) :: relSpec
TYPE( puff_str ),     INTENT( INOUT ) :: puffRelease

TYPE( relPuffT ) relData

IF( relSpec%release%type /= HR_PUFF )THEN
  nError = IV_ERROR
  eRoutine = 'LoadPuffFromPuffRelease'
  eMessage = 'Attempt to load from a release type other than HR_PUFF'
  WRITE(eInform,'(A,I8)')'type =',relSpec%release%type
  GOTO 9999
END IF

!==== Unload

relData = TRANSFER(relSpec%release%relData,relData)

CALL zero_puff( puffRelease )

puffRelease%xbar = relSpec%release%xrel
puffRelease%ybar = relSpec%release%yrel
puffRelease%zbar = relSpec%release%zrel

puffRelease%sxx = relData%sxx
puffRelease%sxy = relData%sxy
puffRelease%sxz = relData%sxz
puffRelease%syy = relData%syy
puffRelease%syz = relData%syz
puffRelease%szz = relData%szz

puffRelease%c  = relData%mass
puffRelease%cc = 1.0 + relData%sigRatio**2

puffRelease%xuc = relData%mass*relData%difhLSVxx
puffRelease%xvc = relData%mass*relData%difhLSVxy
puffRelease%yvc = relData%mass*relData%difhLSVyy

puffRelease%yvsc = relData%mass*relData%difhShear
puffRelease%yvbc = relData%mass*relData%difhBuoy

puffRelease%zwc = relData%mass*relData%difVert

puffRelease%si  = relData%scaleLateral
puffRelease%si2 = relData%scaleStream
puffRelease%sv  = relData%scaleVert

puffRelease%cfo = relData%activeFrac

puffRelease%ityp = relSpec%ityp

9999 CONTINUE

RETURN
END
!===============================================================================
!        LoadPuffAuxFromPuffRelease
!===============================================================================
SUBROUTINE LoadPuffAuxFromPuffRelease( relSpec,puffRelease )

USE release_fd
USE scipuff_fi
USE UtilMtlAux

IMPLICIT NONE

TYPE( releaseSpecT ), INTENT( INOUT ) :: relSpec
TYPE( puff_str ),     INTENT( INOUT ) :: puffRelease

TYPE( relPuffT ) relData

INTEGER ityp,imat

TYPE( puff_material )   matp
TYPE( liquid_material ) matl
TYPE( puff_liquid )     puffRelLiquid
TYPE( puff_dynamics )   pd

LOGICAL, EXTERNAL :: IsGas
LOGICAL, EXTERNAL :: IsLiquid
LOGICAL, EXTERNAL :: IsWetParticle


IF( relSpec%release%type /= HR_PUFF )THEN
  nError = IV_ERROR
  eRoutine = 'LoadPuffAuxFromPuffRelease'
  eMessage = 'Attempt to load from a releas etype other than HR_PUFF'
  WRITE(eInform,'(A,I8)')'type =',relSpec%release%type
  GOTO 9999
END IF

!==== Unload

relData = TRANSFER(relSpec%release%relData,relData)

IF( dynamic )THEN
  IF( IsGas(typeID(puffRelease%ityp)%icls) )THEN
    CALL get_dynamics( puffRelease,pd )
    CALL siginv( puffRelease )
    pd%w = relData%wDynamic*PI3*SQRT(puffRelease%det)
    pd%t = relData%tDynamic*PI3*SQRT(puffRelease%det)
    pd%un = 0.0
    pd%vn = 0.0
    CALL put_dynamics( puffRelease,pd )
  END IF
END IF

IF( IsLiquid(typeID(puffRelease%ityp)%icls) .OR. IsWetParticle(typeID(puffRelease%ityp)%icls) )THEN
  IF( relData%dropDiam /= NOT_SET_R )THEN
    CALL get_liquid( puffRelease,puffRelLiquid )
    puffRelLiquid%d     = relData%dropDiam
    puffRelLiquid%sigd  = relData%dropSigD
    puffRelLiquid%t     = relData%dropTemp
    puffRelLiquid%ccs   = puffRelease%c
    puffRelLiquid%tevap = 0.0

    IF( puffRelLiquid%d <= 0.0 )THEN
      nError = IV_ERROR
      eRoutine = 'LoadPuffAuxFromPuffRelease'
      eMessage = 'Invalid liquid release droplet size'
      eInform  = 'Must be greater than zero'
      GOTO 9999
    END IF

!-----  Set correct size bin

    imat = typeID(puffRelease%ityp)%imat
    ityp = material(imat)%ioffp + GetSubgroups( material(imat),mat_aux ) + 1
    CALL get_puff_material( ityp,matp )
    matl = TRANSFER(matp,matl)
    DO WHILE( puffRelLiquid%d < matl%dmin )
      ityp = ityp - 1
      CALL get_puff_material( ityp,matp )
      matl = TRANSFER(matp,matl)
    END DO

    puffRelease%ityp = ityp

    CALL put_liquid( puffRelease,puffRelLiquid )
  END IF
END IF

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE SetAreaSig( xlen,ylen,ang,sx,sy,rxy )

IMPLICIT NONE

REAL, INTENT( IN  ) :: xlen, ylen, ang
REAL, INTENT( OUT ) :: sx, sy, rxy

REAL sxx, syy, tem, s, c

sx = 0.5*xlen; sy = 0.5*ylen; rxy = 0.

IF( ABS(ang) > 0. )THEN
  sxx = sx*sx; syy = sy*sy
  c = COSD(ang); s = SIND(ang)
  tem = c*c*sxx + s*s*syy
  rxy = c*s*(sxx - syy);
  syy = s*s*sxx + c*c*syy
  sxx = tem
  sx  = SQRT(sxx)
  sy  = SQRT(syy)
  rxy = rxy/(sx*sy)
END IF

RETURN
END

!==============================================================================

SUBROUTINE SetAreaRel( sx,sy,rxy,xlen,ylen,ang )

USE constants_fd

IMPLICIT NONE

REAL, INTENT( IN  ) :: sx, sy, rxy
REAL, INTENT( OUT ) :: xlen, ylen, ang

REAL sxx, syy, sxy, tem, s, c

IF( ABS(rxy) > SMALL )THEN
  sxx = sx**2; syy = sy**2; sxy = rxy*sx*sy
  ang = 0.5*ATAN2(2.*sxy,sxx-syy) / PI180
  c = COSD(-ang); s = SIND(-ang)
  tem = 0.5*(sxx + syy + sxy/(s*c))
  syy = sxx + syy - tem
  sxx = tem
  xlen = 2.*SQRT(sxx)
  ylen = 2.*SQRT(syy)
ELSE
  xlen = 2.*sx; ylen = 2.*sy; ang = 0.
END IF

RETURN
END
