INTEGER FUNCTION SWIMinterpTime( rate,fld )

!------ Interpolate met fields

USE SWIM_fi
USE SWIMparam_fd
USE SWIMutilArrayPtr

IMPLICIT NONE

REAL,            INTENT( IN    ) :: rate
TYPE( MetField), INTENT( INOUT ) :: fld

INTEGER n, i, nzb, ifld, irv
REAL    rm1, fac
LOGICAL lUpdate

INTEGER is, jul, iseason

INTEGER, EXTERNAL :: JulianPrj, GetSeason, SetLandUse
INTEGER, EXTERNAL :: getSmoothIndex, SWIMshearGrad, getFieldIndex

rm1 = 1.0 - rate

!------ 3D arrays

n = SIZE(fld%Field%U)

    CALL UpdateArray( rate,fld%Field%U,rm1,fld%NextField%U,n )
    CALL UpdateArray( rate,fld%Field%V,rm1,fld%NextField%V,n )
    IF( BTEST(fld%type,FTB_W  ) )CALL UpdateArray( rate,fld%Field%W,rm1,fld%NextField%W,n )
    IF( BTEST(fld%type,FTB_DU2) )CALL UpdateArray( rate,fld%Field%dU2,rm1,fld%NextField%dU2,n )


  IF( BTEST(fld%type,FTB_T) )THEN
    CALL UpdateArray( rate,fld%Field%Tpot,rm1,fld%NextField%Tpot,n )
  END IF

  IF( BTEST(fld%type,FTB_H) )THEN
    CALL UpdateArray( rate,fld%Field%Humid,rm1,fld%NextField%Humid,n )
  END IF

  IF( BTEST(fld%type,FTB_P) )THEN
    CALL UpdateArray( rate,fld%Field%Press,rm1,fld%NextField%Press,n )
  END IF

IF( BTEST(fld%type,FTB_QCLD) )THEN
  CALL UpdateArray( rate,fld%Field%Qcloud,rm1,fld%NextField%Qcloud,n )
END IF

IF( BTEST(fld%type,FTB_Z) )THEN

  CALL UpdateArray( rate,fld%Field%Z,rm1,fld%NextField%Z,n )

  IF( BTEST(fld%grid%type,GTB_STAGGERZ) )THEN
    IF( BTEST(fld%grid%type,GTB_Z3DW) )THEN
      fld%grid%sigma%Zw => fld%field%Z
      CALL SetSigmaZ( fld%grid )
    ELSE
      fld%grid%sigma%Z => fld%Field%Z(fld%grid%nXY+1:)
      CALL SetSigmaZw( fld%grid )
    END IF
  ELSE
    fld%grid%sigma%Z => fld%Field%Z
  END IF

  CALL set_kbl( fld%grid )  !No error check since this routine assumes no errors

END IF

IF( ASSOCIATED(fld%NextZ) .AND. .NOT.BTEST(fld%type,FTB_MCWIF) )THEN           !Update sigma-z levels

  nzb = fld%grid%nZ

  lUpdate = .FALSE.
  DO i = 1,nzb
    IF( ABS(fld%grid%Z(i)-fld%NextZ(i)) > 5.E-3 )THEN
      lUpdate = .TRUE.
      EXIT
    END IF
  END DO

  IF( lUpdate )THEN

    CALL UpdateArray( rate,fld%grid%Z,rm1,fld%NextZ,nzb )

    IF( BTEST(fld%grid%type,GTB_STAGGERZ) )THEN
      IF( fld%grid%Zw(1) > 0. )THEN
        fld%grid%Zw(1) = 0.5*fld%grid%Z(1)
      END IF
      fld%grid%Zw(1) = 0.
      DO i = 2,nzb
        fld%grid%Zw(i) = 0.5*(fld%grid%Z(i-1)+fld%grid%Z(i))
      END DO
      fld%grid%Zw(nzb+1) = 1.5*fld%grid%Z(nzb) - 0.5*fld%grid%Z(nzb-1)
    END IF

    ifld = getSmoothIndex( fld%index )     !Update associated smooth fields
    DO WHILE( ifld > 0 )
      CALL CopyArray( field(ifld)%grid%Z,fld%grid%Z,nzb )
      IF( ASSOCIATED(fld%grid%Zw) )CALL CopyArray( field(ifld)%grid%Zw,fld%grid%Zw,nzb+1 )
      ifld = getSmoothIndex( field(ifld)%index )
    END DO

  END IF

END IF

IF( BTEST(fld%type,FTB_LSV) )THEN
  CALL UpdateArray( rate,fld%LSV%UU,rm1,fld%NextLSV%UU,n )
  CALL UpdateArray( rate,fld%LSV%VV,rm1,fld%NextLSV%VV,n )
  CALL UpdateArray( rate,fld%LSV%UV,rm1,fld%NextLSV%UV,n )
  IF( BTEST(fld%type,FTB_LSVL) )CALL UpdateArray( rate,fld%LSV%SL,rm1,fld%NextLSV%SL,n )
END IF

IF( BTEST(fld%type,FTB_UU) )THEN
  CALL UpdateArray( rate,fld%BLprof%UU,rm1,fld%NextBLprof%UU,n )
  CALL UpdateArray( rate,fld%BLprof%VV,rm1,fld%NextBLprof%VV,n )
  CALL UpdateArray( rate,fld%BLprof%WW,rm1,fld%NextBLprof%WW,n )
  CALL UpdateArray( rate,fld%BLprof%WT,rm1,fld%NextBLprof%WT,n )
  CALL UpdateArray( rate,fld%BLprof%SL,rm1,fld%NextBLprof%SL,n )
  CALL UpdateArray( rate,fld%BLprof%SZ,rm1,fld%NextBLprof%SZ,n )
END IF

IF( ASSOCIATED(fld%QLprof%QQ) )THEN
  CALL UpdateArray( rate,fld%QLprof%QQ,rm1,fld%NextQLprof%QQ,n )
  CALL UpdateArray( rate,fld%QLprof%SL,rm1,fld%NextQLprof%SL,n )
  CALL UpdateArray( rate,fld%QLprof%Tpot,rm1,fld%NextQLprof%Tpot,n )
  CALL UpdateArray( rate,fld%QLprof%Press,rm1,fld%NextQLprof%Press,n )
END IF

!------ 2D (horizontal) arrays

n = fld%grid%nXY

IF( BTEST(fld%type,FTB_ZI) )THEN
  CALL UpdateArray( rate,fld%BL%zi,rm1,fld%NextBL%zi,n )
END IF

IF( BTEST(fld%type,FTB_HFLX) )THEN
  CALL UpdateArray( rate,fld%BL%HeatFlux,rm1,fld%NextBL%HeatFlux,n )
END IF

IF( BTEST(fld%type,FTB_UST) )THEN
  CALL UpdateArray( rate,fld%BL%ustr,rm1,fld%NextBL%ustr,n )
END IF

IF( BTEST(fld%type,FTB_MOL) )THEN
  CALL UpdateArray( rate,fld%BL%invMOL,rm1,fld%NextBL%invMOL,n )
END IF

IF( BTEST(fld%type,FTB_CLDCV) )THEN
  CALL UpdateArray( rate,fld%BL%cc,rm1,fld%NextBL%cc,n )
END IF

IF( BTEST(fld%type,FTB_PRCP) )THEN
  fac = FLOAT(NINT(rate))
  CALL UpdateArray( fac,fld%BL%prcp,1.-fac,fld%NextBL%prcp,n )
ELSE IF( BTEST(fld%type,FTB_PRATE) )THEN
  CALL UpdateArray( rate,fld%BL%prcp,rm1,fld%NextBL%prcp,n )
  IF( ASSOCIATED(fld%BL%rainprb) )CALL UpdateArray( rate,fld%BL%rainprb,rm1,fld%NextBL%rainprb,n )
END IF

IF( BTEST(fld%type,FTB_ZRUF) )THEN
  CALL UpdateArray( rate,fld%BL%zruf,rm1,fld%NextBL%zruf,n )
  CALL CopyArray( fld%grid%landcover%roughness,fld%BL%zruf,n )
END IF

IF( BTEST(fld%type,FTB_LAI) )THEN
  CALL UpdateArray( rate,fld%BL%LAI,rm1,fld%NextBL%LAI,n )
END IF

IF( BTEST(fld%grid%type,GTB_LANDUSE) )THEN

  IF( fld%t == NOT_SET_R )THEN
    jul = fld%grid%landcover%julLU
  ELSE
    jul = JulianPrj( fld%t )
  END IF

  IF( jul > fld%grid%landcover%julLU )THEN
    fld%grid%landcover%julLU = jul

    DO is = 1,fld%grid%nXY
      iseason = GetSeason( fld%grid%lat(is),jul )
      IF( iseason <= 0 )EXIT

      irv = SetLandUse( fld%grid%landcover%LandUse(is),iseason,Prj%BL%i_wet, &
                        fld%grid%landcover%roughness(is), &
                        fld%grid%landcover%canopyHt(is),         &
                        fld%grid%landcover%alpha(is), &
                        fld%grid%landcover%albedo(is), &
                        fld%grid%landcover%Bowen(is) )
      IF( irv /= 1 )EXIT

    END DO

  END IF

END IF

SWIMinterpTime = SWIMresult

RETURN
END

!==============================================================================

INTEGER FUNCTION SWIMinterpTimeAssm( rate,fld )

!------ Interpolate special fields used for gridded+obs assimilation
!       Updated fields are in fld%Field1 & fld%BL1
!       N.B. Assumes NO SWIFT, BL profile, 3d Z-field, met unc (file)

USE SWIM_fi
USE SWIMparam_fd
USE SWIMutilArrayPtr

IMPLICIT NONE

REAL,            INTENT( IN    ) :: rate
TYPE( MetField), INTENT( INOUT ) :: fld

INTEGER n
REAL    rm1, fac

rm1 = 1.0 - rate

!------ 3D arrays

n = SIZE(fld%Field1%U)

CALL UpdateArray( rate,fld%Field1%U,rm1,fld%Field2%U,n )
CALL UpdateArray( rate,fld%Field1%V,rm1,fld%Field2%V,n )
IF( BTEST(fld%type,FTB_W  ) )CALL UpdateArray( rate,fld%Field1%W,rm1,fld%Field2%W,n )
IF( BTEST(fld%type,FTB_DU2) )CALL UpdateArray( rate,fld%Field1%dU2,rm1,fld%Field2%dU2,n )

IF( BTEST(fld%type,FTB_T) )THEN
  CALL UpdateArray( rate,fld%Field1%Tpot,rm1,fld%Field2%Tpot,n )
END IF

IF( BTEST(fld%type,FTB_H) )THEN
  CALL UpdateArray( rate,fld%Field1%Humid,rm1,fld%Field2%Humid,n )
END IF

IF( BTEST(fld%type,FTB_P) )THEN
  CALL UpdateArray( rate,fld%Field1%Press,rm1,fld%Field2%Press,n )
END IF

IF( BTEST(fld%type,FTB_QCLD) )THEN
  CALL UpdateArray( rate,fld%Field1%Qcloud,rm1,fld%Field2%Qcloud,n )
END IF

IF( BTEST(fld%type,FTB_Z) )THEN

!ERROR
!  CALL UpdateArray( rate,fld%Field1%Z,rm1,fld%Field2%Z,n )

!  IF( BTEST(fld%grid%type,GTB_STAGGERZ) )THEN
!    fld%grid%sigma%Z => fld%Field1%Z(fld%grid%nXY+1:)
!    CALL SetSigmaZw( fld%grid )
!  ELSE
!    fld%grid%sigma%Z => fld%Field1%Z
!  END IF

END IF

IF( BTEST(fld%type,FTB_LSV) )THEN
!  CALL UpdateArray( rate,fld%LSV%UU,rm1,fld%NextLSV%UU,n )
!  CALL UpdateArray( rate,fld%LSV%VV,rm1,fld%NextLSV%VV,n )
!  CALL UpdateArray( rate,fld%LSV%UV,rm1,fld%NextLSV%UV,n )
!  IF( BTEST(fld%type,FTB_LSVL) )CALL UpdateArray( rate,fld%LSV%SL,rm1,fld%NextLSV%SL,n )
END IF


!------ 2D (horizontal) arrays

n = fld%grid%nXY

IF( BTEST(fld%type,FTB_ZI) )THEN
  CALL UpdateArray( rate,fld%BL1%zi,rm1,fld%BL2%zi,n )
END IF

IF( BTEST(fld%type,FTB_HFLX) )THEN
  CALL UpdateArray( rate,fld%BL1%HeatFlux,rm1,fld%BL2%HeatFlux,n )
END IF

IF( BTEST(fld%type,FTB_MOL) )THEN
  CALL UpdateArray( rate,fld%BL1%invMOL,rm1,fld%BL2%invMOL,n )
END IF

IF( BTEST(fld%type,FTB_CLDCV) )THEN
  CALL UpdateArray( rate,fld%BL1%cc,rm1,fld%BL2%cc,n )
END IF

IF( BTEST(fld%type,FTB_PRCP) )THEN
  fac = FLOAT(NINT(rate))
  CALL UpdateArray( fac,fld%BL1%prcp,1.-fac,fld%BL2%prcp,n )
ELSE IF( BTEST(fld%type,FTB_PRATE) )THEN
  CALL UpdateArray( rate,fld%BL1%prcp,rm1,fld%BL2%prcp,n )
  IF( ASSOCIATED(fld%BL1%rainprb) )CALL UpdateArray( rate,fld%BL1%rainprb,rm1,fld%BL2%rainprb,n )
END IF

IF( BTEST(fld%type,FTB_ZRUF) )THEN
  CALL UpdateArray( rate,fld%BL1%zruf,rm1,fld%BL2%zruf,n )
END IF

IF( BTEST(fld%type,FTB_LAI) )THEN
  CALL UpdateArray( rate,fld%BL1%LAI,rm1,fld%BL2%LAI,n )
END IF

SWIMinterpTimeAssm = SWIMresult

RETURN
END

