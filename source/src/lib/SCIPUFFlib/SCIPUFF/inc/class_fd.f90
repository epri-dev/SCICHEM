MODULE class_fd

!------ Material class Names

  CHARACTER(4), PARAMETER :: MAT_GAS  = 'GAS'  !Gas materials
  CHARACTER(4), PARAMETER :: MAT_PRT  = 'PART' !Particle materials
  CHARACTER(4), PARAMETER :: MAT_LIQ  = 'LIQ'  !Liquid materials
  CHARACTER(4), PARAMETER :: MAT_WET  = 'WETP' !Wet particle materials
  CHARACTER(4), PARAMETER :: MAT_AER  = 'AERO' !Aersosol materials


  CHARACTER(4), PARAMETER :: MAT_NULL = 'NULL' !Null sensor
  CHARACTER(4), PARAMETER :: MAT_SSAT = 'SSAT' !"Saturated" sensor

!------ Material class IDs and flags

  INTEGER, PARAMETER :: MATID_GAS     = 0 !Gas materials
  INTEGER, PARAMETER :: MATID_PRT     = 1 !Particle materials
  INTEGER, PARAMETER :: MATID_LIQ     = 2 !Liquid materials

  INTEGER, PARAMETER :: MATID_EVAP    = 4 !Secondary Evaporation (Liquid)
  INTEGER, PARAMETER :: MATID_AEROSOL = 6 !Aerosol group for Liquids
  INTEGER, PARAMETER :: MATID_MULTI   = 8 !Multicomponent
  INTEGER, PARAMETER :: MATID_WETP    = 9 !Wet particle
  INTEGER, PARAMETER :: MATID_MULTI_DEP = 10 !Multicomponent deposition
  INTEGER, PARAMETER :: MATID_MULTI_DOS = 11 !Multicomponent deposition
  INTEGER, PARAMETER :: MATID_NULL_SENSOR = 19 !Null sensor
  INTEGER, PARAMETER :: MATID_SAT_SENSOR  = 20 !"Saturated" sensor

END MODULE class_fd
