MODULE spcstruct_fd
  USE prjstruct_fd    !Basic project structures
  USE domain_fd       !Reference structure

! Bits for GetProjectPufff flag

  INTEGER, PARAMETER :: PPB_TRANS = 0  !Translate puff centroid to LLA
  INTEGER, PARAMETER :: PPB_MSL   = 1  !Zbar, Zc, Zinc above MSL (AGL otherwise)
  INTEGER, PARAMETER :: PPB_SLOPE = 2  !Save terrain slopes in puff axx & ayy

!==== terrainHeadT ============================================================

  TYPE  terrainHeadT
    SEQUENCE
    INTEGER status
    INTEGER max
    INTEGER nx
    INTEGER ny
    REAL    x0
    REAL    y0
    REAL    dx
    REAL    dy
    REAL    hmin
  END TYPE  terrainHeadT

!==== pterrainHeadT ===========================================================

  TYPE  pterrainHeadT
    SEQUENCE
    TYPE ( projectIDT   ) project
    TYPE ( terrainHeadT ) terrain
  END TYPE  pterrainHeadT

!==== puffCoordinateT ===========================================================

  TYPE puffCoordinateT
    SEQUENCE
    INTEGER             mode            !HD_LATLON,HD_UTM,HD_CARTESIAN
    INTEGER             UTMZone         !UTM refernce zone if Mode=HD_UTM
    TYPE ( referenceT ) reference       !Cartesian Refernce point if Mode=HD_CARTESIAN
  END TYPE puffCoordinateT

!==== puffHeadT ============================================================

  TYPE  puffHeadT
    SEQUENCE
    INTEGER maxPuff
    INTEGER maxAux
    INTEGER maxType
    INTEGER nPuff
    INTEGER nAux
    INTEGER nType
    REAL    time
    TYPE ( puffCoordinateT ) coordinate
    INTEGER dynamic        !project dynamics flag
    INTEGER dense          !project dense gas dynamics flag
    INTEGER buoy           !project buoyant gas flag
    INTEGER nMCtype
  END TYPE  puffHeadT

!==== ppuffHeadT ===========================================================

  TYPE  ppuffHeadT
    SEQUENCE
    TYPE ( projectIDT ) project
    TYPE ( puffHeadT  ) puff
  END TYPE  ppuffHeadT

!==== puffT ===========================================================

  TYPE  puffT !Basic puff structure - currenty identical to puff_str w/o aux pointer
    SEQUENCE
    REAL(8) xbar,ybar
    REAL    zbar
    REAL    sxx,sxy,sxz,syy,syz,szz
    REAL    axx,axy,axz,ayy,ayz,azz,det
    REAL    c,cc,xuc,xvc,yvc,yvsc,yvbc,zwc,wc,ccb
    REAL    si,si2,sv
    REAL    sr,cfo
    REAL    zi,zc
    REAL    uo,vo,wo
    INTEGER ityp,inxt,iprv,ipgd,idtl,idtn,iaux
  END TYPE  puffT

!==== puffCopy ===========================================================

  TYPE  puffCopy !Basic puff structure - currenty identical to puff_str
    SEQUENCE
    TYPE( puffT )               :: puff
    REAL, DIMENSION(:), POINTER :: aux
  END TYPE  puffCopy

!==== puffTypeT ===========================================================

  TYPE  puffTypeT
    SEQUENCE
    INTEGER imat            !material ID
    INTEGER igrp            !SubGroup ID
    INTEGER npaux           !No.Puff Auxiliary data
    INTEGER ipmc            !Pointer for Multi-Component aux data
    INTEGER mcID            !Multi-Component ID
    INTEGER icls            !Puff Class
    INTEGER ltot            !Total variance flag
    CHARACTER(16) material  !Material Name
  END TYPE  puffTypeT

END MODULE spcstruct_fd
