MODULE ipgrd_fi

  SAVE

  INTEGER, PARAMETER :: MAXG  = 4000  !Maximum Number of Horiz. grids
  INTEGER, PARAMETER :: MAXGZ = 200   !Maximum Number of Vertical grids

  INTEGER, ALLOCATABLE, DIMENSION(:,:,:) :: ipgrd
  INTEGER, ALLOCATABLE, DIMENSION(:)     :: npgrd, mxlev_grd

  INTEGER mxgrd

END MODULE ipgrd_fi
