!***********************************************************************
!               GUI_SCIP_met
!***********************************************************************
SUBROUTINE GUI_SCIP_met(prjdlg,metdlg,tool)

USE resource_fd
USE mettype_fd
USE tooluser_fd
USE pcscipuf_fi
USE files_fi
USE dialog_fi

IMPLICIT NONE

TYPE( pweatherT ) tool
TYPE( METDEF_DLG ) metdlg
TYPE( ProjectStructure ) prjdlg

INTEGER i

!==== Project id member

tool%project = prjdlg%ID

!==== Weather LSV member

SELECT CASE (metdlg%lsv)
  CASE (LSV_INPUT)
    tool%weather%lsv%type = HL_LSVINP
  CASE (LSV_MODEL)
    tool%weather%lsv%type = HL_LSVMOD
  CASE (LSV_OBS)
    tool%weather%lsv%type = HL_LSVOBS
  CASE (LSV_OPERATIONAL)
    tool%weather%lsv%type = HL_LSVOPER
  CASE (LSV_OPER2_0)
    tool%weather%lsv%type = HL_LSVOPER_20
  CASE DEFAULT
    tool%weather%lsv%type = HL_NONE
END SELECT
tool%weather%lsv%uu   = metdlg%uub
tool%weather%lsv%sl   = metdlg%slb

!==== Weather BL member

SELECT CASE (metdlg%bl)
  CASE (BL_CALC)
    tool%weather%bl%type = HB_BLCALC
  CASE (BL_OBS)
    tool%weather%bl%type = HB_BLOBS
  CASE (BL_SIMPLE)
    tool%weather%bl%type = HB_BLSIMPLE
  CASE (BL_PROFILE)
    tool%weather%bl%type = HB_BLPRF
  CASE (BL_OPERATIONAL)
    tool%weather%bl%type = HB_BLOPER
  CASE (BL_MEDOC)
    tool%weather%bl%type = HB_BLMED
  CASE DEFAULT
    tool%weather%bl%type = HB_NONE
END SELECT

tool%weather%bl%wetness    = metdlg%wetness
tool%weather%bl%ziDay      = metdlg%zimax
tool%weather%bl%ziNight    = metdlg%zimin
tool%weather%bl%hfluxDay   = metdlg%hdiur
tool%weather%bl%hfluxNight = metdlg%hconst
tool%weather%bl%albedo     = metdlg%albedo
tool%weather%bl%bowen      = metdlg%bowen
tool%weather%bl%cloud      = metdlg%cloud
IF( metdlg%lcanopy )THEN
  tool%weather%bl%canopy     = metdlg%canopy
  tool%weather%bl%roughness  = NOT_SET_R
  tool%weather%bl%canopyParam= metdlg%canopyParam
ELSE
  tool%weather%bl%canopy     = NOT_SET_R
  tool%weather%bl%roughness  = metdlg%rough
  tool%weather%bl%canopyParam= 2.0
END IF

!==== Weather Precip member

SELECT CASE (metdlg%precip)
  CASE (PC_CLEAR)
    tool%weather%precip%type  = HP_NONE
    tool%weather%precip%class = SCIPnull
  CASE (PC_LIGHT_RAIN)
    tool%weather%precip%type  = HP_PRCPINP
    tool%weather%precip%class = HP_RAIN + HP_LIGHT
  CASE (PC_MODERATE_RAIN)
    tool%weather%precip%type  = HP_PRCPINP
    tool%weather%precip%class = HP_RAIN + HP_MODERATE
  CASE (PC_HEAVY_RAIN)
    tool%weather%precip%type  = HP_PRCPINP
    tool%weather%precip%class = HP_RAIN + HP_HEAVY
  CASE (PC_LIGHT_SNOW)
    tool%weather%precip%type  = HP_PRCPINP
    tool%weather%precip%class = HP_SNOW + HP_LIGHT
  CASE (PC_MODERATE_SNOW)
    tool%weather%precip%type  = HP_PRCPINP
    tool%weather%precip%class = HP_SNOW + HP_MODERATE
  CASE (PC_HEAVY_SNOW)
    tool%weather%precip%type  = HP_PRCPINP
    tool%weather%precip%class = HP_SNOW + HP_HEAVY
  CASE (PC_MET)
    tool%weather%precip%type  = HP_PRCPMET
    tool%weather%precip%class = SCIPnull
  CASE DEFAULT
    tool%weather%precip%type  = HP_NONE
    tool%weather%precip%class = SCIPnull
END SELECT

!==== Weather met member

SELECT CASE (metdlg%met)
  CASE (MET_MEDOC)
    tool%weather%met%type = HW_METMED
  CASE (MET_WRF)
    tool%weather%met%type = HW_METWRF
  CASE (MET_ASSIM)
    tool%weather%met%type = HW_METASSIM
  CASE (MET_MEDLIS)
    tool%weather%met%type = HW_METMEDLS
  CASE (MET_MRF)
    tool%weather%met%type = HW_METMRF
  CASE (MET_OBS)
    tool%weather%met%type = HW_METSRF + HW_METPRF
  CASE (MET_SRF)
    tool%weather%met%type = HW_METSRF
  CASE (MET_UAIR)
    tool%weather%met%type = HW_METPRF
  CASE (MET_FIXED)
    tool%weather%met%type = HW_METFIX
  CASE DEFAULT
    tool%weather%met%type  = HW_METNONE
END SELECT

IF( BTEST(tool%weather%met%type,HWB_METOPER) )THEN
  tool%weather%met%nnPrf     = DEF_VAL_I
  tool%weather%met%nnSfc     = DEF_VAL_I
ELSE
  tool%weather%met%nnPrf     = metdlg%nobs
  tool%weather%met%nnSfc     = metdlg%nsfc
END IF
tool%weather%met%timeBin   = metdlg%tbin
tool%weather%met%unitSpd   = metdlg%unit_spd
tool%weather%met%unitDir   = metdlg%unit_dir
tool%weather%met%speed     = metdlg%speed
tool%weather%met%direction = metdlg%direction

i = 0
IF( BTEST(tool%weather%met%type,HWB_METMED) )THEN
  i = i + 1
  tool%weather%met%input(i) = metdlg%medfile
END IF
IF( BTEST(tool%weather%met%type,HWB_METWRF) )THEN
  i = i + 1
  tool%weather%met%input(i) = metdlg%medfile
END IF
IF( BTEST(tool%weather%met%type,HWB_METMEDLS) )THEN
  i = i + 1
  tool%weather%met%input(i) = metdlg%medfile
END IF
IF( BTEST(tool%weather%met%type,HWB_METASSIM) )THEN
  i = i + 1
  tool%weather%met%input(i) = metdlg%asmfile
END IF
IF( BTEST(tool%weather%met%type,HWB_METMRF) )THEN
  i = i + 1
  tool%weather%met%input(i) = metdlg%medfile
END IF
IF( BTEST(tool%weather%met%type,HWB_METPRF) )THEN
  i = i + 1
  tool%weather%met%input(i) = metdlg%profile
END IF
IF( BTEST(tool%weather%met%type,HWB_METSRF) )THEN
  i = i + 1
  tool%weather%met%input(i) = metdlg%surfile
END IF

!==== Weather terrain member

IF( metdlg%lmc  )THEN
  tool%weather%flags%doMC   = SCIPon
  tool%weather%terrain%type = HT_SCIPUFF
  tool%weather%terrain%file = TRIM(metdlg%terfile)
ELSE
  tool%weather%flags%doMC   = SCIPoff
  tool%weather%terrain%type = HT_NONE
  tool%weather%terrain%file = ' '
END IF

!==== Terrain mass-consistency member

tool%weather%terrain%mc%maxIter(1) = metdlg%nprm
tool%weather%terrain%mc%maxIter(2) = metdlg%nfft
tool%weather%terrain%mc%eps(1)     = metdlg%epsprm
tool%weather%terrain%mc%eps(2)     = metdlg%epsfft
tool%weather%terrain%mc%alpha(1)   = metdlg%alpmin
tool%weather%terrain%mc%alpha(2)   = metdlg%alpmax
tool%weather%terrain%mc%nz         = metdlg%nz
DO i = 1,metdlg%nz
  tool%weather%terrain%mc%z(i) = metdlg%z(i)
END DO

IF( metdlg%lavailter  ) &
  tool%weather%terrain%type = IBSET(tool%weather%terrain%type,HTB_AVAIL_T)
IF( metdlg%luseter    ) &
  tool%weather%terrain%type = IBSET(tool%weather%terrain%type,HTB_USE_T)
IF( metdlg%lavaillc   ) &
  tool%weather%terrain%type = IBSET(tool%weather%terrain%type,HTB_AVAIL_L)
IF( metdlg%luselc     ) &
  tool%weather%terrain%type = IBSET(tool%weather%terrain%type,HTB_USE_L)
IF( metdlg%llccategory) &
  tool%weather%terrain%type = IBSET(tool%weather%terrain%type,HTB_CATEGORY)

!==== Weather flags member

IF( metdlg%local )THEN
  tool%weather%flags%reference = HT_LOCAL
ELSE
  tool%weather%flags%reference = HT_UTC
END IF

i = 0
tool%weather%flags%tOutput = NOT_SET_R
IF( metdlg%lmcout )THEN
 i = IBSET(i,HOB_OUTPUT)
 IF( metdlg%tout /= NOT_SET_R .AND. metdlg%tout /= DEF_VAL_R)THEN
   i = IBSET(i,HOB_OUTMET)
   tool%weather%flags%tOutput = metdlg%tout
 ELSE
   tool%weather%flags%tOutput = DEF_VAL_R
 END IF
END IF
IF( metdlg%lmcformat )THEN
  i = IBSET(i,HOB_ASCII)
END IF
IF( metdlg%l2Dsave )THEN
  i = IBSET(i,HOB_2D)
END IF
IF( metdlg%l3Dsave )THEN
  i = IBSET(i,HOB_3D)
END IF
tool%weather%flags%doOutput  = i

tool%weather%flags%notUsed   = 0
tool%weather%flags%slHazard  = metdlg%slhazard

RETURN
END
!***********************************************************************
!               SCIP_GUI_met
!***********************************************************************
SUBROUTINE SCIP_GUI_met(metdlg,tool)

USE resource_fd
USE mettype_fd
USE tooluser_fd
USE pcscipuf_fi
USE files_fi
USE dialog_fi
USE param_fd

IMPLICIT NONE

TYPE( pweatherT ) tool
TYPE( METDEF_DLG ) metdlg

INTEGER i,ii,idum

LOGICAL lAssim
LOGICAL luse,lcat,ldum
CHARACTER(16) cdum

LOGICAL, EXTERNAL :: hasError

!==== Clear

metdlg = metdef(DEFAULT_LEVEL)

ii = 1

!==== Weather LSV member

SELECT CASE (tool%weather%lsv%type)
  CASE (HL_LSVINP)
    metdlg%lsv = LSV_INPUT
  CASE (HL_LSVMOD)
    metdlg%lsv = LSV_MODEL
  CASE (HL_LSVOBS)
    metdlg%lsv = LSV_OBS
  CASE (HL_LSVOPER)
    metdlg%lsv = LSV_OPERATIONAL
  CASE (HL_LSVOPER_20)
    metdlg%lsv = LSV_OPER2_0
  CASE DEFAULT
    metdlg%lsv = LSV_NONE
END SELECT
metdlg%uub = tool%weather%lsv%uu
metdlg%slb = tool%weather%lsv%sl

!==== Weather BL member

SELECT CASE (tool%weather%bl%type)
  CASE (HB_BLCALC)
    metdlg%bl = BL_CALC
  CASE (HB_BLOBS)
    metdlg%bl = BL_OBS
  CASE (HB_BLSIMPLE)
    metdlg%bl = BL_SIMPLE
  CASE (HB_BLPRF)
    metdlg%bl = BL_PROFILE
  CASE (HB_BLOPER)
    metdlg%bl = BL_OPERATIONAL
  CASE (HB_BLMED)
    metdlg%bl = BL_MEDOC
  CASE DEFAULT
    metdlg%bl = BL_NONE
END SELECT

metdlg%wetness    = tool%weather%bl%wetness
metdlg%zimax      = tool%weather%bl%ziDay
metdlg%zimin      = tool%weather%bl%ziNight
metdlg%hdiur      = tool%weather%bl%hfluxDay
metdlg%hconst     = tool%weather%bl%hfluxNight
metdlg%albedo     = tool%weather%bl%albedo
metdlg%bowen      = tool%weather%bl%bowen
metdlg%cloud      = tool%weather%bl%cloud
metdlg%lcanopy    = tool%weather%bl%canopy /= NOT_SET_R
metdlg%canopy     = tool%weather%bl%canopy
metdlg%rough      = tool%weather%bl%roughness
metdlg%canopyParam= tool%weather%bl%canopyParam

!==== Weather Precip member

SELECT CASE (tool%weather%precip%type)
  CASE (HP_PRCPINP)
    SELECT CASE (tool%weather%precip%class)
      CASE (HP_RAIN + HP_LIGHT)
        metdlg%precip = PC_LIGHT_RAIN
      CASE (HP_RAIN + HP_MODERATE)
        metdlg%precip = PC_MODERATE_RAIN
      CASE (HP_RAIN + HP_HEAVY)
        metdlg%precip = PC_HEAVY_RAIN
      CASE (HP_SNOW + HP_LIGHT)
        metdlg%precip = PC_LIGHT_SNOW
      CASE (HP_SNOW + HP_MODERATE)
        metdlg%precip = PC_MODERATE_SNOW
      CASE (HP_SNOW + HP_HEAVY)
        metdlg%precip = PC_HEAVY_SNOW
      CASE DEFAULT
        metdlg%precip = PC_CLEAR
    END SELECT
  CASE (HP_PRCPMET)
    metdlg%precip = PC_MET
  CASE DEFAULT
    metdlg%precip = PC_CLEAR
END SELECT

!==== Weather met member

SELECT CASE (tool%weather%met%type)
  CASE (HW_METNONE)
    metdlg%met = NOT_SET_I
  CASE (HW_METMED)
    metdlg%met = MET_MEDOC
    metdlg%medfile = tool%weather%met%input(1)
  CASE (HW_METWRF)
    metdlg%met = MET_WRF
    metdlg%medfile = tool%weather%met%input(1)
  CASE (HW_METASSIM)
    metdlg%met = MET_ASSIM
    metdlg%asmfile = tool%weather%met%input(1)
    metdlg%tbin    = tool%weather%met%timeBin
  CASE (HW_METMEDLS)
    metdlg%met = MET_MEDLIS
    metdlg%medfile = tool%weather%met%input(1)
  CASE (HW_METMRF)
    metdlg%met = MET_MRF
    metdlg%medfile = tool%weather%met%input(1)
  CASE (HW_METSRF + HW_METPRF)
    metdlg%met = MET_OBS
    metdlg%nobs = tool%weather%met%nnPrf
    metdlg%nsfc = tool%weather%met%nnSfc
    metdlg%tbin = tool%weather%met%timeBin
    metdlg%profile = tool%weather%met%input(1)
    metdlg%surfile = tool%weather%met%input(2)
  CASE (HW_METSRF)
    metdlg%met = MET_SRF
    metdlg%nsfc = tool%weather%met%nnSfc
    metdlg%tbin = tool%weather%met%timeBin
    metdlg%surfile = tool%weather%met%input(1)
  CASE (HW_METPRF)
    metdlg%met = MET_UAIR
    metdlg%nobs = tool%weather%met%nnPrf
    metdlg%tbin = tool%weather%met%timeBin
    metdlg%profile = tool%weather%met%input(1)
  CASE (HW_METFIX)
    metdlg%met = MET_FIXED
    metdlg%unit_spd  = tool%weather%met%unitSpd
    metdlg%unit_dir  = tool%weather%met%unitDir
    metdlg%speed     = tool%weather%met%speed
    metdlg%direction = tool%weather%met%direction
  CASE DEFAULT !Assume if not one of the above it must be operational
    metdlg%met = NOT_SET_I
END SELECT
lAssim = metdlg%met == MET_ASSIM

!==== Weather terrain member

metdlg%lmc = tool%weather%flags%doMC == SCIPon
IF( metdlg%lmc )THEN
  metdlg%terfile = tool%weather%terrain%file
  CALL check_ter_header(I_LATLON,metdlg%terfile,ldum,cdum,idum,luse,lcat)
  IF( hasError() )THEN
    metdlg%lmc = .FALSE.
    CALL AddErrorAction('Use of terrain file turned off')
    CALL ShowErrorMessage( NULL_POINTER )
  ELSE
    tool%weather%terrain%type = IBSET(tool%weather%terrain%type,HTB_AVAIL_T)
    IF( luse )tool%weather%terrain%type = IBSET(tool%weather%terrain%type,HTB_AVAIL_L)
    IF( lcat )tool%weather%terrain%type = IBSET(tool%weather%terrain%type,HTB_CATEGORY)
  END IF
ELSE
  metdlg%terfile = ' '
END IF

!==== Terrain mass-consistency member

IF( metdlg%lmc .OR. lAssim )THEN
  metdlg%nprm      = tool%weather%terrain%mc%maxIter(1)
  metdlg%nfft      = tool%weather%terrain%mc%maxIter(2)
  metdlg%epsprm    = tool%weather%terrain%mc%eps(1)
  metdlg%epsfft    = tool%weather%terrain%mc%eps(2)
  metdlg%alpmin    = tool%weather%terrain%mc%alpha(1)
  metdlg%alpmax    = tool%weather%terrain%mc%alpha(2)
  metdlg%nz        = tool%weather%terrain%mc%nz
  DO i = 1,metdlg%nz
    metdlg%z(i) = tool%weather%terrain%mc%z(i)
  END DO
END IF

metdlg%lavailter   = BTEST(tool%weather%terrain%type,HTB_AVAIL_T)
metdlg%luseter     = BTEST(tool%weather%terrain%type,HTB_USE_T)
metdlg%lavaillc    = BTEST(tool%weather%terrain%type,HTB_AVAIL_L)
metdlg%luselc      = BTEST(tool%weather%terrain%type,HTB_USE_L)
metdlg%llccategory = BTEST(tool%weather%terrain%type,HTB_CATEGORY)

!==== Weather flags member

metdlg%local = tool%weather%flags%reference == HT_LOCAL

!      if(tool.weather.flags.doMC .eq. SCIPon)then
  metdlg%lmcout    = BTEST(tool%weather%flags%doOutput,HOB_OUTPUT)
  metdlg%lmcformat = BTEST(tool%weather%flags%doOutput,HOB_ASCII)
  metdlg%l2Dsave   = BTEST(tool%weather%flags%doOutput,HOB_2D)
  metdlg%l3Dsave   = BTEST(tool%weather%flags%doOutput,HOB_3D)
!      end if

if( BTEST(tool%weather%flags%doOutput,HOB_OUTMET) )then
  metdlg%tout = tool%weather%flags%tOutput
else
  if( metdlg%lmcout )then
    metdlg%tout = DEF_VAL_R
  else
    metdlg%tout = NOT_SET_R
  end if
end if

metdlg%slhazard = tool%weather%flags%slHazard

RETURN
END
