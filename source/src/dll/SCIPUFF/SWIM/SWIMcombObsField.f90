RECURSIVE INTEGER FUNCTION SWIMcombObsField( i,fld,InterpPrf,lProf,lAssm )

!------ Combine previous field with current (weighted) obs profiles

USE SWIM_fi
USE SWIMparam_fd
USE SWIMobsInterp_fd
USE SWIMutilArrayPtr
USE constants_fd

IMPLICIT NONE

INTEGER,           INTENT( IN    ) :: i           !Horizontal grid index
TYPE( MetField  ), INTENT( INOUT ) :: fld
TYPE( ObsInterp ), INTENT( INOUT ) :: InterpPrf
LOGICAL,           INTENT( IN    ) :: lProf
LOGICAL,           INTENT( IN    ) :: lAssm        !Assimilation flag

REAL, PARAMETER :: VEL_EPS = 0.1
REAL, PARAMETER :: WT_FAC  = 0.5

INTEGER alloc_stat, nxy, nz, nzt, ip, k, ii
REAL    t0, h, D, zh, del_t, t_us
LOGICAL DoUpdate

REAL, DIMENSION(:), POINTER :: PrevWt

INTERFACE

  RECURSIVE SUBROUTINE SWIMcombPrf( i,nxy,nz,Intrp,PrevField,PrevWt,Field,UpdateWt )
    USE SWIMobsInterp_fd
    INTEGER,              INTENT( IN ) :: i
    INTEGER,              INTENT( IN ) :: nxy
    INTEGER,              INTENT( IN ) :: nz
    TYPE( ObsInterpPrf ), INTENT( IN ) :: Intrp
    REAL, DIMENSION(:),   POINTER      :: PrevField
    REAL, DIMENSION(:),   POINTER      :: PrevWt
    REAL, DIMENSION(:),   POINTER      :: Field
    LOGICAL, OPTIONAL,    INTENT( IN ) :: UpdateWt
  END SUBROUTINE SWIMcombPrf

  RECURSIVE SUBROUTINE SWIMcombPrfSCM( i,nxy,nz,Intrp,Field,Field0,PrevWt )
    USE SWIMobsInterp_fd
    INTEGER,              INTENT( IN ) :: i
    INTEGER,              INTENT( IN ) :: nxy
    INTEGER,              INTENT( IN ) :: nz
    TYPE( ObsInterpPrf ), INTENT( IN ) :: Intrp
    REAL, DIMENSION(:),   POINTER      :: Field
    REAL, DIMENSION(:),   POINTER      :: Field0
    REAL, DIMENSION(:),   POINTER      :: PrevWt
  END SUBROUTINE SWIMcombPrfSCM

  RECURSIVE SUBROUTINE SWIMcombPrfSCM_U( i,nxy,nz,Intrp,Field,Field0,PrevWt )
    USE SWIMobsInterp_fd
    INTEGER,              INTENT( IN ) :: i
    INTEGER,              INTENT( IN ) :: nxy
    INTEGER,              INTENT( IN ) :: nz
    TYPE( ObsInterpPrf ), INTENT( IN ) :: Intrp
    REAL, DIMENSION(:),   POINTER      :: Field
    REAL, DIMENSION(:),   POINTER      :: Field0
    REAL, DIMENSION(:),   POINTER      :: PrevWt
  END SUBROUTINE SWIMcombPrfSCM_U

END INTERFACE

REAL, EXTERNAL :: StndTpot

SWIMcombObsField = SWIMfailure

nxy = fld%grid%nXY
nz  = fld%grid%nZ

IF( BTEST(fld%grid%type,GTB_STAGGER) )THEN
  ip  = i + nxy
  nzt = nz + 1
ELSE
  ip  = i
  nzt = nz
END IF

!------ Save weights for variances, etc.

IF( BTEST(fld%type,FTB_LSV) .OR. &
    BTEST(fld%type,FTB_UU)  )THEN
  ALLOCATE( PrevWt(nzt*nxy),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Number  = UK_ERROR
    error%Routine = 'SWIMcombObsField'
    error%Message = 'Error allocating old variance field weights'
    GOTO 9999
  END IF
  CALL UpdateArray( 0.,PrevWt,1.0,fld%obsWt%su,fld%grid%nZ*fld%grid%nXY )
ELSE
  NULLIFY( PrevWt )
END IF

DoUpdate = .TRUE.

!------ Update velocity for non-SWIFT fields

IF( lAssm )THEN

  IF( ABS(fld%tNext) < 1. )THEN
    CALL SWIMcombPrfSCM_U( ip,nxy,nz,InterpPrf%U,fld%NextField%U,fld%Field1%U,fld%obsWt%su )
  ELSE
    CALL SWIMcombPrfSCM( ip,nxy,nz,InterpPrf%U,fld%NextField%U,fld%Field1%U,fld%obsWt%su )
  END IF
    CALL SWIMcombPrfSCM( ip,nxy,nz,InterpPrf%V,fld%NextField%V,fld%Field1%V,fld%obsWt%sv )

ELSE

    CALL SWIMcombPrf( ip,nxy,nz,InterpPrf%U,fld%Field%U,fld%obsWt%su,fld%NextField%U )
    DoUpdate = .FALSE.
    CALL SWIMcombPrf( ip,nxy,nz,InterpPrf%V,fld%Field%V,fld%obsWt%sv,fld%NextField%V )

END IF

!------ Large-scale variance

IF( BTEST(fld%type,FTB_LSV) )THEN

  IF( DoUpdate )THEN
    CALL SWIMcombPrf( ip,nxy,nz,InterpPrf%UUL,fld%LSV%UU,fld%obsWt%su,fld%NextLSV%UU )
    DoUpdate = .FALSE.
  ELSE
    CALL SWIMcombPrf( ip,nxy,nz,InterpPrf%UUL,fld%LSV%UU,PrevWt,fld%NextLSV%UU, &
                                                                  UpdateWt=.FALSE. )
  END IF
  CALL SWIMcombPrf( ip,nxy,nz,InterpPrf%VVL,fld%LSV%VV,PrevWt,fld%NextLSV%VV, &
                                                                UpdateWt=.FALSE. )
  CALL SWIMcombPrf( ip,nxy,nz,InterpPrf%UVL,fld%LSV%UV,PrevWt,fld%NextLSV%UV, &
                                                                UpdateWt=.FALSE. )
  IF( BTEST(fld%type,FTB_LSVL) )THEN
    CALL SWIMcombPrf( ip,nxy,nz,InterpPrf%SHL,fld%LSV%SL,PrevWt,fld%NextLSV%SL, &
                                                                    UpdateWt=.FALSE. )
  END IF

END IF

!------ Boundary layer turbulence

IF( BTEST(fld%type,FTB_UU) )THEN

  IF( DoUpdate )THEN
    CALL SWIMcombPrf( ip,nxy,nz,InterpPrf%UU,fld%BLprof%UU,fld%obsWt%su,fld%NextBLprof%UU )
    DoUpdate = .FALSE.
  ELSE
    CALL SWIMcombPrf( ip,nxy,nz,InterpPrf%UU,fld%BLprof%UU,PrevWt,fld%NextBLprof%UU, &
                                                                  UpdateWt=.FALSE. )
  END IF
  CALL SWIMcombPrf( ip,nxy,nz,InterpPrf%VV,fld%BLprof%VV,PrevWt,fld%NextBLprof%VV, &
                                                                  UpdateWt=.FALSE. )
  CALL SWIMcombPrf( ip,nxy,nz,InterpPrf%WW,fld%BLprof%WW,PrevWt,fld%NextBLprof%WW, &
                                                                  UpdateWt=.FALSE. )
  CALL SWIMcombPrf( ip,nxy,nz,InterpPrf%WT,fld%BLprof%WT,PrevWt,fld%NextBLprof%WT, &
                                                                  UpdateWt=.FALSE. )
  CALL SWIMcombPrf( ip,nxy,nz,InterpPrf%SL,fld%BLprof%SL,PrevWt,fld%NextBLprof%SL, &
                                                                  UpdateWt=.FALSE. )
  CALL SWIMcombPrf( ip,nxy,nz,InterpPrf%SZ,fld%BLprof%SZ,PrevWt,fld%NextBLprof%SZ, &
                                                                  UpdateWt=.FALSE. )

END IF

!------ Temperature (extrapolate if no profile data available)

IF( BTEST(fld%type,FTB_T) )THEN
  IF( lAssm )THEN
    IF( ASSOCIATED(InterpPrf%Tpot%obs) ) &
      CALL SWIMcombPrfSCM( ip,nxy,nz,InterpPrf%Tpot,fld%NextField%Tpot,fld%Field1%Tpot,fld%obsWt%st )
  ELSE
    IF( lProf )THEN
      CALL SWIMcombPrf( ip,nxy,nz,InterpPrf%Tpot,fld%Field%Tpot,fld%obsWt%st,fld%NextField%Tpot )
    ELSE
      CALL SWIMcombPrf( ip,nxy,1,InterpPrf%Tpot,fld%Field%Tpot,fld%obsWt%st,fld%NextField%Tpot )
      t0 = fld%NextField%Tpot(ip)
      h  = fld%grid%terrain%H(i) + fld%grid%Hmin
      D  = fld%grid%terrain%D(i)
      zh = h + D*fld%grid%Z(1)
      t_us = StndTpot( zh )
      del_t = t0 - t_us
      ii = ip
      DO k = 2,nz
        ii = ii + nxy
        zh = h + D*fld%grid%Z(k)
        t_us = StndTpot( zh )
        fld%NextField%Tpot(ii) = t_us + del_t
      END DO
    END IF
  END IF
END IF

!------ Pressure

IF( BTEST(fld%type,FTB_P) )THEN
  IF( lAssm )THEN
    IF( ASSOCIATED(InterpPrf%Press%obs) ) &
      CALL SWIMcombPrfSCM( ip,nxy,nz,InterpPrf%Press,fld%NextField%Press,fld%Field1%Press,fld%obsWt%sp )
  ELSE
    CALL SWIMcombPrf( ip,nxy,nz,InterpPrf%Press,fld%Field%Press,fld%obsWt%sp,fld%NextField%Press )
  END IF
END IF

!------ Humidity
!       N.B. Work with relative humidity

IF( BTEST(fld%type,FTB_H) )THEN
  IF( lAssm )THEN
    IF( ASSOCIATED(InterpPrf%Humid%obs) ) &
      CALL SWIMcombPrfSCM( ip,nxy,nz,InterpPrf%Humid,fld%NextField%Humid,fld%Field1%Humid,fld%obsWt%sh )
  ELSE
    CALL SWIMcombPrf( ip,nxy,nz,InterpPrf%Humid,fld%Field%Humid,fld%obsWt%sh,fld%NextField%Humid )
  END IF
END IF

!------ Cloud liquid water
!       N.B. Work g/m^3

IF( BTEST(fld%type,FTB_QCLD) )THEN
  IF( lAssm )THEN
    IF( ASSOCIATED(InterpPrf%Qcloud%obs) ) &
      CALL SWIMcombPrfSCM( ip,nxy,nz,InterpPrf%Qcloud,fld%NextField%Qcloud,fld%Field1%Qcloud,fld%obsWt%sqc )
  ELSE
    CALL SWIMcombPrf( ip,nxy,nz,InterpPrf%Qcloud,fld%Field%Qcloud,fld%obsWt%sqc,fld%NextField%Qcloud )
  END IF
END IF

!------ Non-height-dependent fields

IF( BTEST(fld%type,FTB_ZI) )THEN
  CALL SWIMcombPt( InterpPrf%Zi,fld%BL%Zi(i),fld%obsWt%szi(i),fld%NextBL%Zi(i) )
END IF

IF( BTEST(fld%type,FTB_HFLX) )THEN
  CALL SWIMcombPt( InterpPrf%Hflux,fld%BL%HeatFlux(i),fld%obsWt%shflx(i), &
                                                      fld%NextBL%HeatFlux(i) )
END IF

IF( BTEST(fld%type,FTB_UST) )THEN
  CALL SWIMcombPt( InterpPrf%Ust,fld%BL%ustr(i),fld%obsWt%sustr(i), &
                                                      fld%NextBL%ustr(i) )
END IF

IF( BTEST(fld%type,FTB_MOL) )THEN
  CALL SWIMcombPt( InterpPrf%invL,fld%BL%invMOL(i),fld%obsWt%smol(i), &
                                                      fld%NextBL%invMOL(i) )
END IF

IF( BTEST(fld%type,FTB_PRCP) )THEN
  CALL SWIMcombPrcp( InterpPrf%prcp,fld%BL%prcp(i),fld%obsWt%sprcp(i), &
                                                      fld%NextBL%prcp(i) )
ELSE IF( BTEST(fld%type,FTB_PRATE) )THEN
  CALL SWIMcombPt( InterpPrf%prcp,fld%BL%prcp(i),fld%obsWt%sprcp(i), &
                                                      fld%NextBL%prcp(i) )
END IF

IF( BTEST(fld%type,FTB_CLDCV) )THEN
  CALL SWIMcombPt( InterpPrf%cldcv,fld%BL%cc(i),fld%obsWt%scc(i), &
                                                      fld%NextBL%cc(i) )
END IF

IF( BTEST(fld%type,FTB_LAI) )THEN
  CALL SWIMcombPt( InterpPrf%LAI,fld%BL%LAI(i),fld%obsWt%slai(i), &
                                                      fld%NextBL%LAI(i) )
END IF

SWIMcombObsField = SWIMresult

9999 CONTINUE

IF( ASSOCIATED(PrevWt) )DEALLOCATE( PrevWt,STAT=alloc_stat )

RETURN
END

!==============================================================================

RECURSIVE SUBROUTINE SWIMcombPrf( i,nxy,nz,Intrp,PrevField,PrevWt,Field,UpdateWt )

USE SWIMobsInterp_fd
USE constants_fd

IMPLICIT NONE

INTEGER,              INTENT( IN ) :: i
INTEGER,              INTENT( IN ) :: nxy
INTEGER,              INTENT( IN ) :: nz
TYPE( ObsInterpPrf ), INTENT( IN ) :: Intrp
REAL, DIMENSION(:),   POINTER      :: PrevField
REAL, DIMENSION(:),   POINTER      :: PrevWt
REAL, DIMENSION(:),   POINTER      :: Field
LOGICAL, OPTIONAL,    INTENT( IN ) :: UpdateWt

REAL, PARAMETER :: WT_FAC = 0.5

INTEGER k, ip
REAL    wtsum, vwsum
LOGICAL DoUpdate

IF( PRESENT(UpdateWt) )THEN
  DoUpdate = UpdateWt
ELSE
  DoUpdate = .TRUE.
END IF

DO k = 1,nz
  ip = i + (k-1)*nxy

  wtsum = MAX(0.,PrevWt(ip)-Intrp%wt(k))
  vwsum = PrevField(ip)*wtsum

  wtsum = wtsum + Intrp%wt(k)
  vwsum = vwsum + Intrp%obs(k)

  IF( wtsum > SMALL )THEN
    Field(ip)  = vwsum / wtsum
    IF( DoUpdate )PrevWt(ip) = wtsum * WT_FAC  !'age' weights for next time
  END IF

END DO

RETURN
END

!==============================================================================

RECURSIVE SUBROUTINE SWIMcombPt( Intrp,PrevPt,PrevWt,Pt )

USE SWIMobsInterp_fd
USE constants_fd

IMPLICIT NONE

TYPE( ObsInterpSrf ), INTENT( IN    ) :: Intrp
REAL,                 INTENT( IN    ) :: PrevPt
REAL,                 INTENT( INOUT ) :: PrevWt
REAL,                 INTENT( OUT   ) :: Pt

REAL, PARAMETER :: WT_FAC = 0.5

REAL    wtsum, vwsum

wtsum = MAX(0.,PrevWt-Intrp%wt)
vwsum = PrevPt*wtsum

wtsum = wtsum + Intrp%wt
vwsum = vwsum + Intrp%obs

IF( wtsum > SMALL )THEN
  Pt     = vwsum / wtsum
  PrevWt = wtsum * WT_FAC  !'age' weights for next time
END IF

RETURN
END

!==============================================================================

RECURSIVE SUBROUTINE SWIMcombPrcp( Intrp,PrevPt,PrevWt,Pt )

USE SWIMobsInterp_fd
USE constants_fd

IMPLICIT NONE

TYPE( ObsInterpSrf ), INTENT( IN    ) :: Intrp
REAL,                 INTENT( IN    ) :: PrevPt
REAL,                 INTENT( INOUT ) :: PrevWt
REAL,                 INTENT( OUT   ) :: Pt

IF( Intrp%wt > 0. )THEN
  Pt = Intrp%obs
  PrevWt = 1.0
ELSE IF( PrevWt > 0. )THEN
  Pt = PrevPt
ELSE
  Pt = 0.
END IF

RETURN
END

!==============================================================================

RECURSIVE SUBROUTINE SWIMcombPrfSCM( i,nxy,nz,Intrp,Field,Field0,PrevWt )

USE SWIM_fi, ONLY: E2BO
USE SWIMobsInterp_fd
USE constants_fd

IMPLICIT NONE

INTEGER,              INTENT( IN ) :: i
INTEGER,              INTENT( IN ) :: nxy
INTEGER,              INTENT( IN ) :: nz
TYPE( ObsInterpPrf ), INTENT( IN ) :: Intrp
REAL, DIMENSION(:),   POINTER      :: Field
REAL, DIMENSION(:),   POINTER      :: Field0
REAL, DIMENSION(:),   POINTER      :: PrevWt

REAL, PARAMETER :: WT_FAC = 0.5

INTEGER k, ip
REAL    wtsum, fac

DO k = 1,nz
  ip = i + (k-1)*nxy

  wtsum = MAX(0.,PrevWt(ip)-Intrp%wt(k))
  wtsum = wtsum + Intrp%wt(k)

  IF( Intrp%wt(k) > SMALL )THEN
    fac = 1. / (1. + E2BO*Intrp%wt(k))
    Field(ip)  = Field(ip) + fac * (E2BO*Intrp%obs(k) + Field0(ip) - Field(ip))
    PrevWt(ip) = wtsum * WT_FAC
  END IF

END DO

RETURN
END

!==============================================================================

RECURSIVE SUBROUTINE SWIMcombPrfSCM_U( i,nxy,nz,Intrp,Field,Field0,PrevWt )

USE SWIM_fi, ONLY: E2BO
USE SWIMobsInterp_fd
USE constants_fd

IMPLICIT NONE

INTEGER,              INTENT( IN ) :: i
INTEGER,              INTENT( IN ) :: nxy
INTEGER,              INTENT( IN ) :: nz
TYPE( ObsInterpPrf ), INTENT( IN ) :: Intrp
REAL, DIMENSION(:),   POINTER      :: Field
REAL, DIMENSION(:),   POINTER      :: Field0
REAL, DIMENSION(:),   POINTER      :: PrevWt

REAL, PARAMETER :: WT_FAC = 0.5

INTEGER k, ip
REAL    wtsum, fac            ,fsave

DO k = 1,nz
  ip = i + (k-1)*nxy

  wtsum = MAX(0.,PrevWt(ip)-Intrp%wt(k))
  wtsum = wtsum + Intrp%wt(k)

  IF( Intrp%wt(k) > SMALL )THEN
    fac = 1. / (1. + E2BO*Intrp%wt(k))
    fsave = Field(ip)
    Field(ip)  = Field(ip) + fac * (E2BO*Intrp%obs(k) + Field0(ip) - Field(ip))
    PrevWt(ip) = wtsum * WT_FAC
  END IF

END DO

RETURN
END
