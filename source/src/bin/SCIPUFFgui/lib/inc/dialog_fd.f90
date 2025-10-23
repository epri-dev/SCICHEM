MODULE dialog_fd

  USE metparam_fd
  USE tooluser_fd
  USE DefSize_fd

  INTEGER, PARAMETER :: MAXZBG = 1000 !eventually obsolete

  TYPE  TIME_DLG
    SEQUENCE
    TYPE( temporalT ) time
    LOGICAL           default_save
    CHARACTER(24)     startString
    CHARACTER(24)     endString
  END TYPE  TIME_DLG

  TYPE  DOMAIN_DLG
    SEQUENCE
    TYPE( spatialT ) spatial
    LOGICAL          hasReference
  END TYPE  DOMAIN_DLG

  TYPE  METDEF_DLG
    SEQUENCE
    UNION
      MAP
        INTEGER nobs
        INTEGER nsfc
        INTEGER met
        INTEGER bl
        INTEGER lsv
        INTEGER nfft
        INTEGER nprm
        INTEGER nz
        INTEGER unit_spd
        INTEGER unit_dir
        INTEGER, DIMENSION(2) :: iNotUsedA
        INTEGER iNotUsedB
        INTEGER precip
        INTEGER iNotUsedC
        INTEGER wetness
      END MAP
      MAP
        INTEGER, DIMENSION(16) :: dbint
      END MAP
    END UNION
    UNION
      MAP
        REAL rough
        REAL canopy
        REAL zimin
        REAL zimax
        REAL hconst
        REAL hdiur
        REAL bowen
        REAL albedo
        REAL cloud
        REAL slb
        REAL uub
        REAL speed
        REAL direction
        REAL tbin
        REAL slhazard
        REAL alpmin
        REAL alpmax
        REAL epsfft
        REAL epsprm
        REAL rNotUsedA
        REAL tout
        REAL rNotUsedB
        REAL canopyParam
      END MAP
      MAP
        REAL, DIMENSION(23) :: dbreal
      END MAP
    END UNION

    REAL, DIMENSION(MAXZBG) :: z

    LOGICAL lmc
    LOGICAL lmcout
    LOGICAL lmcformat
    LOGICAL local
    LOGICAL lcanopy
    LOGICAL luseter
    LOGICAL luselc
    LOGICAL lavailter
    LOGICAL lavaillc
    LOGICAL llccategory
    LOGICAL l2Dsave
    LOGICAL l3Dsave
    CHARACTER(PATH_MAXLENGTH) profile
    CHARACTER(PATH_MAXLENGTH) surfile
    CHARACTER(PATH_MAXLENGTH) medfile
    CHARACTER(PATH_MAXLENGTH) terfile
    CHARACTER(PATH_MAXLENGTH) asmfile
  END TYPE  METDEF_DLG

  TYPE RunAnimateT
    SEQUENCE
    LOGICAL on
    INTEGER update
    CHARACTER(PATH_MAXLENGTH) optionsFile
  END TYPE RunAnimateT

END MODULE dialog_fd
