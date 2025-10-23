MODULE AreaMode_fd

  USE poparea_fd

  INTEGER, PARAMETER :: HP_BON        = POP_BON
  INTEGER, PARAMETER :: HP_BAREA      = POP_BAREA
  INTEGER, PARAMETER :: HP_BEXPECT    = POP_BEXPECT
  INTEGER, PARAMETER :: HP_OFF        = POP_OFF
  INTEGER, PARAMETER :: HP_ON         = 2**HP_BON      ! (1)
  INTEGER, PARAMETER :: HP_AREA       = 2**HP_BAREA    ! (2)
  INTEGER, PARAMETER :: HP_EXPECT     = 2**HP_BEXPECT  ! (4)

! areaMode = 1 : HP_ON                       -> Population within contour
!            3 : HP_ON + HP_AREA             -> Area of contour
!            5 : HP_ON +           HP_EXPECT -> Expected population (Only available if type=HP_MEAN)
!            7 : HP_ON + HP_AREA + HP_EXPECT -> Expected area       (Only available if type=HP_MEAN)

END MODULE AreaMode_fd

MODULE type_fd

  INTEGER, PARAMETER :: HP_NUMTYP  = 4
  INTEGER, PARAMETER :: HP_MEAN    = 1
  INTEGER, PARAMETER :: HP_PROB    = 2
  INTEGER, PARAMETER :: HP_EXCEED  = 3
  INTEGER, PARAMETER :: HP_VARIANCE= 4

  CHARACTER(32), DIMENSION(HP_NUMTYP), PARAMETER :: TYPE_STRING = (/&
         'Mean Value  ( M )      '     ,& !HP_MEAN
         'Probability ( P[v>E] ) '     ,& !HP_PROB
         'Exceedance  ( v[Pc>P] )'     ,& !HP_EXCEED
         'Variance    ( V )      '      & !HP_VARIANCE
  /)

END MODULE type_fd
