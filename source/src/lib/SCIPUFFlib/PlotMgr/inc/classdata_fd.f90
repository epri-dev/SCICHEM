MODULE classData_fd

  INTEGER, PARAMETER :: CD_NUM_VSLICE = 7
  INTEGER, PARAMETER :: CD_NUM_HSLICE = 5
  INTEGER, PARAMETER :: CD_NUM_SSLICE = 4
  INTEGER, PARAMETER :: CD_NUM_VINT   = 4
  INTEGER, PARAMETER :: CD_NUM_TABLE  = 3
  INTEGER, PARAMETER :: CD_NUM_SURF   = 0

  INTEGER, PARAMETER :: CD_XMIN   = 1  !vslice,hslice,vint,sslice
  INTEGER, PARAMETER :: CD_XMAX   = 2  !vslice,hslice,vint,sslice
  INTEGER, PARAMETER :: CD_YMIN   = 3  !vslice,hslice,vint,sslice
  INTEGER, PARAMETER :: CD_YMAX   = 4  !vslice,hslice,vint,sslice
  INTEGER, PARAMETER :: CD_ZMIN   = 5  !vslice,hslice
  INTEGER, PARAMETER :: CD_ZMAX   = 6  !vslice
  INTEGER, PARAMETER :: CD_VRES   = 7  !vslice
  INTEGER, PARAMETER :: CD_RISK   = 1  !table
  INTEGER, PARAMETER :: CD_ROUND  = 2  !table
  INTEGER, PARAMETER :: CD_CUTOFF = 3  !table

  INTEGER, PARAMETER :: ROUND_DEFAULT     = -999 !Default rounding of table results (data=0.0)
  INTEGER, PARAMETER :: ROUND_NONE        =    0 !Round to nearest 100s             (data=1.0)
  INTEGER, PARAMETER :: ROUND_HUNDRED     =    2 !Round to nearest 100s             (data=2.0)
  INTEGER, PARAMETER :: ROUND_THOUSAND    =    3 !Round to neatest 1000s            (data=3.0)
  INTEGER, PARAMETER :: ROUND_TWO_DIGIT   =   -2 !Round to two significant digits   (data=4.0)
  INTEGER, PARAMETER :: ROUND_THREE_DIGIT =   -3 !Rounds to three significant digits(data=5.0)

!----- ClassData locations for additional source search inputs

  INTEGER, PARAMETER :: CD_ADJ_XMASK_MIN = 1
  INTEGER, PARAMETER :: CD_ADJ_XMASK_MAX = 2
  INTEGER, PARAMETER :: CD_ADJ_YMASK_MIN = 3
  INTEGER, PARAMETER :: CD_ADJ_YMASK_MAX = 4
  INTEGER, PARAMETER :: CD_ADJ_TMASK_MIN = 5
  INTEGER, PARAMETER :: CD_ADJ_TMASK_MAX = 6

  INTEGER, PARAMETER :: CD_NUM_ADJ_DOMAIN = 4
  INTEGER, PARAMETER :: CD_NUM_ADJ_TIME   = 2

END MODULE classData_fd

