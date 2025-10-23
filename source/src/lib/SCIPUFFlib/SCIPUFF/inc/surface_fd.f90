MODULE surface_fd

  TYPE  sfield_puff  !Surface puff structure
    SEQUENCE
    INTEGER nblocks                        ! No. of blocks influenced
    INTEGER, DIMENSION(:), POINTER :: iblk ! Block pointers
    INTEGER icld                           ! Gamma cloudshine field
  END TYPE  sfield_puff

  TYPE  sfield_block  !Surface block structure
    SEQUENCE
    INTEGER type                           ! Block type
    INTEGER field                          ! First field location
    INTEGER id                             ! Material/effects ID
    INTEGER flags                          ! Tot/Haz flags
    INTEGER iaux                           ! Aux structure ID
    CHARACTER(64) name                     ! Block name
  END TYPE  sfield_block

  TYPE  sfield_effect  !Surface effects block structure
    SEQUENCE
    INTEGER type
    INTEGER id
  END TYPE  sfield_effect

  TYPE mask_fd
    REAL xmin, ymin
    REAL xmax, ymax
  END TYPE mask_fd

END MODULE surface_fd

MODULE surface_dose_fd

  INTERFACE

    SUBROUTINE surface_dose( p,sdat,srfI,zsrf,sblk,spuff,stype,mask )
      USE struct_fd
      USE surface_fd
      USE srfparam_fd
      TYPE( puff_str ),                   INTENT( IN ) :: p
      INTEGER,                            INTENT( IN ) :: srfI
      INTEGER, DIMENSION(*),              INTENT( IN ) :: stype
      REAL,                               INTENT( IN ) :: zsrf
      REAL, DIMENSION(ISRF_TOT),          INTENT( IN ) :: sdat
      TYPE( sfield_block ), DIMENSION(*), INTENT( IN ) :: sblk
      TYPE( sfield_puff ),  DIMENSION(*), INTENT( IN ) :: spuff
      TYPE( mask_fd),       OPTIONAL,     INTENT( IN ) :: mask
    END SUBROUTINE surface_dose

  END INTERFACE

END MODULE surface_dose_fd


