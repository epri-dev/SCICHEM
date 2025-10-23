!==============================================================================
!  Module for SWIM met field interpolation
!==============================================================================

MODULE SWIMcurrentMet_fi

  USE SWIMinterp_fd

  SAVE

  REAL hx, hy, hp, dp, gxp, gyp  !Local terrain parameters
  REAL zRef                      !Height above reference level (Hmin)
  REAL zh                        !Height AGL
  REAL zm                        !Terrain-following coordinate
  REAL zsl                       !Surface-layer depth

  REAL sigh                      !Horizontal puff scale
  REAL sigv                      !Vertical puff scale

  REAL xbar                      !Puff centroid
  REAL ybar                      !Puff latitude (perhaps)

  LOGICAL lzinv, lsl, lter, lstagger

  TYPE( met1dh ) mx, mxu, my, myv
  TYPE( meth   ) mxy
  TYPE( metv   ) mz, mzw

  TYPE CurrentMetZ
    REAL, DIMENSION(:), POINTER :: zb, zbw
  END TYPE CurrentMetZ

  INTEGER klev0, klev0w

  LOGICAL :: lSaveCoord

  INTEGER, PARAMETER  :: MAXFLD = 99
  REAL, DIMENSION(MAXFLD) :: xFld, yFld

  INTEGER thisThread

  COMMON / CurrentMet /  hx, hy, hp, dp, gxp, gyp  !Local terrain parameters
  COMMON / CurrentMet /  zRef                      !Height above reference level (Hmin)
  COMMON / CurrentMet /  zh                        !Height AGL
  COMMON / CurrentMet /  zm                        !Terrain-following coordinate
  COMMON / CurrentMet /  zsl                       !Surface-layer depth

  COMMON / CurrentMet /  sigh                      !Horizontal puff scale
  COMMON / CurrentMet /  sigv                      !Vertical puff scale

  COMMON / CurrentMet /  xbar                      !Puff centroid
  COMMON / CurrentMet /  ybar                      !Puff latitude (perhaps)

  COMMON / CurrentMet /  lzinv, lsl, lter, lstagger

  COMMON / CurrentMet /  mx, mxu, my, myv
  COMMON / CurrentMet /  mxy
  COMMON / CurrentMet /  mz, mzw

  COMMON / CurrentMet /  klev0, klev0w

  COMMON / CurrentMet /  lSaveCoord
  COMMON / CurrentMet /  xFld, yFld
  COMMON / CurrentMet /  thisThread
!$OMP THREADPRIVATE(/CurrentMet/)


END MODULE SWIMcurrentMet_fi
