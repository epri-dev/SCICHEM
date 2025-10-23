MODULE environment_fd

!==== parameters ==============================================================

  INTEGER, PARAMETER :: HS_MAXENVIRO = 250

!==== enviroT = envsample_str =================================================

  TYPE  enviroT
    SEQUENCE
    REAL z             !z
    REAL pressure      !mb
    REAL potentialTemp !Potential Temp DegK
    REAL humidity      !gm/gm
    REAL windUComp     !m/sec E-W
    REAL windVComp     !m/sec N-S
    REAL windWComp     !m/sec
  END TYPE  enviroT

!==== environmentT = enviroment_str ===========================================

  TYPE  environmentT
    SEQUENCE
    REAL             sfcElevation          !m
    REAL             sfcPressure           !mb
    REAL             mixingLayerHeight     !m
    TYPE ( enviroT ) samples(HS_MAXENVIRO)
    INTEGER          nsamp
  END TYPE  environmentT

!==== enviroBLT ==============================================================

  TYPE  enviroBLT
    SEQUENCE
    REAL roughness         !Roughness Height (m)
    REAL canopy            !Canopy Height (m) : <0 implies no canopy
    REAL alpha             !Canopy Flow index
    REAL L                 !Monin-Obukov Length (m)
    REAL surfaceLayer      !Surface Layer height (m)
    REAL mixingLayerHeight !Mixing Layer Height (m)
    REAL wt                !Surface heat flux (W/m2)
    REAL uStar             !Velocity Scale (Friction velocity) (m/s)
    REAL wStar             !Vertical velocity scale (m/s)
    REAL dTdz              !Surface temperature gradient (K/m)
  END TYPE  enviroBLT

END MODULE environment_fd
