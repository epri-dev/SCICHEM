MODULE interp2d_fi

  SAVE

  INTEGER irate

  REAL,    DIMENSION(:), ALLOCATABLE :: ratex, ratey
  INTEGER, DIMENSION(:), ALLOCATABLE :: indxx, indxy

END MODULE interp2d_fi

!------------------------------------------------------------------------------

SUBROUTINE interp_2d( xi,nxi,yi,nyi,vi,mxi,xo,nxo,yo,nyo,vo,mxo,iflag )

!------ 2-d linear interpolation routine - no extrapolation

USE interp2d_fi

IMPLICIT NONE

INTEGER,                INTENT( IN    ) :: nxi, nyi !Input  grid dimensions
INTEGER,                INTENT( IN    ) :: nxo, nyo !Output grid dimension
INTEGER,                INTENT( IN    ) :: mxi, mxo !Max inner dimensions for input & output arrays
REAL, DIMENSION(*),     INTENT( IN    ) :: xi, yi   !Input  grid arrays
REAL, DIMENSION(*),     INTENT( IN    ) :: xo, yo   !Output grid arrays
REAL, DIMENSION(mxi,*), INTENT( IN    ) :: vi       !Input 2-d array
REAL, DIMENSION(mxo,*), INTENT( OUT   ) :: vo       !Output 2d-arry (interpolation result)
INTEGER,                INTENT( INOUT ) :: iflag    !Control flag: -1 = error
                                                    !               0 = interpolate only
                                                    !               1 = set rates & interpolate
                                                    !               2 = set rates only
INTEGER i, j, ii, ip, im, jp, jm, alloc_stat
REAL    rxm, rxp, rym, ryp, val

IF( iflag < 0 )RETURN  !negative iflag implies previous bad result

!-------- Set rates if iflag > 0

IF( iflag > 0 )THEN

  IF( ALLOCATED(ratex) )DEALLOCATE( ratex,STAT=alloc_stat )
  IF( ALLOCATED(ratey) )DEALLOCATE( ratey,STAT=alloc_stat )
  IF( ALLOCATED(indxx) )DEALLOCATE( indxx,STAT=alloc_stat )
  IF( ALLOCATED(indxy) )DEALLOCATE( indxy,STAT=alloc_stat )

  ALLOCATE( ratex(nxo),indxx(nxo),ratey(nyo),indxy(nyo),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    iflag = -1
    RETURN
  END IF

!-------- x direction

  DO i = 1,nxo

    ii = 1
    DO WHILE( xo(i) >= xi(ii) .AND. ii <= nxi )
      ii = ii + 1
    END DO

    IF( ii == 1 )THEN
      ratex(i) = 0.0
      indxx(i) = 2
    ELSE IF( ii > nxi )THEN
      ratex(i) = 1.0
      indxx(i) = nxi
    ELSE
      ratex(i) = (xo(i)-xi(ii-1))/(xi(ii)-xi(ii-1))
      indxx(i) = ii
    END IF

  END DO

!-------- y direction

  DO i = 1,nyo

    ii = 1
    DO WHILE( yo(i) >= yi(ii) .AND. ii <= nyi )
      ii = ii + 1
    END DO

    IF( ii == 1 )THEN
      ratey(i) = 0.0
      indxy(i) = 2
    ELSE IF( ii > nyi )THEN
      ratey(i) = 1.0
      indxy(i) = nyi
    ELSE
      ratey(i) = (yo(i)-yi(ii-1))/(yi(ii)-yi(ii-1))
      indxy(i) = ii
    END IF

  END DO

  irate = 1234

  IF( iflag /= 2 )THEN
    iflag = 0
  END IF

END IF

IF( irate /= 1234 )THEN !Check if rates have been set

  iflag = -1
  RETURN

END IF

!-------- Check if computing rates only

IF( iflag == 2 )RETURN

!-------- Interpolate

DO j = 1,nyo
  jp = MIN(indxy(j),nyi)
  jm = MAX(1,jp - 1)
  ryp = ratey(j)
  rym = 1. - ryp
  DO i = 1,nxo
    ip = MIN(indxx(i),nxi)
    im = MAX(1,ip - 1)
    rxp = ratex(i)
    rxm = 1. - rxp
    val = rxp*ryp*vi(ip,jp) + rxp*rym*vi(ip,jm) + &
          rxm*ryp*vi(im,jp) + rxm*rym*vi(im,jm)
    vo(i,j) = val
  END DO
END DO

RETURN
END

!==============================================================================

SUBROUTINE interp_2dp( xi,nxi,yi,nyi,vi,xo,nxo,yo,nyo,vo,iflag )

!------ 2-d linear interpolation routine (packed 2D arrays) - no extrapolation

USE interp2d_fi

IMPLICIT NONE

INTEGER,            INTENT( IN    ) :: nxi, nyi !Input  grid dimensions
INTEGER,            INTENT( IN    ) :: nxo, nyo !Output grid dimension
REAL, DIMENSION(*), INTENT( IN    ) :: xi, yi   !Input  grid arrays
REAL, DIMENSION(*), INTENT( IN    ) :: xo, yo   !Output grid arrays
REAL, DIMENSION(*), INTENT( IN    ) :: vi       !Input 2-d array
REAL, DIMENSION(*), INTENT( OUT   ) :: vo       !Output 2d-arry (interpolation result)
INTEGER,            INTENT( INOUT ) :: iflag    !Control flag: -1 = error
                                                !               0 = interpolate only
                                                !               1 = set rates & interpolate
                                                !               2 = set rates only
INTEGER i, j, ii, ip, im, jp, jm, i0, i0p, i0m, alloc_stat
REAL    rxm, rxp, rym, ryp

IF( iflag < 0 )RETURN  !negative iflag implies previous bad result

!-------- Set rates if iflag > 0

IF( iflag > 0 )THEN

  IF( ALLOCATED(ratex) )DEALLOCATE( ratex,STAT=alloc_stat )
  IF( ALLOCATED(ratey) )DEALLOCATE( ratey,STAT=alloc_stat )
  IF( ALLOCATED(indxx) )DEALLOCATE( indxx,STAT=alloc_stat )
  IF( ALLOCATED(indxy) )DEALLOCATE( indxy,STAT=alloc_stat )

  ALLOCATE( ratex(nxo),indxx(nxo),ratey(nyo),indxy(nyo),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    iflag = -1
    RETURN
  END IF

!-------- x direction

  DO i = 1,nxo

    ii = 1
    DO WHILE( xo(i) >= xi(ii) .AND. ii <= nxi )
      ii = ii + 1
    END DO

    IF( ii == 1 )THEN
      ratex(i) = 0.0
      indxx(i) = 2
    ELSE IF( ii > nxi )THEN
      ratex(i) = 1.0
      indxx(i) = nxi
    ELSE
      ratex(i) = (xo(i)-xi(ii-1))/(xi(ii)-xi(ii-1))
      indxx(i) = ii
    END IF

  END DO

!-------- y direction

  DO i = 1,nyo

    ii = 1
    DO WHILE( yo(i) >= yi(ii) .AND. ii <= nyi )
      ii = ii + 1
    END DO

    IF( ii == 1 )THEN
      ratey(i) = 0.0
      indxy(i) = 2
    ELSE IF( ii > nyi )THEN
      ratey(i) = 1.0
      indxy(i) = nyi
    ELSE
      ratey(i) = (yo(i)-yi(ii-1))/(yi(ii)-yi(ii-1))
      indxy(i) = ii
    END IF

  END DO

  irate = 1234

  IF( iflag /= 2 )THEN
    iflag = 0
  END IF

END IF

IF( irate /= 1234 )THEN

  iflag = -1
  RETURN

END IF

!-------- Check if computing rates only

IF( iflag == 2 )RETURN

!-------- Interpolate

DO j = 1,nyo
  jp = MIN(indxy(j),nyi)
  jm = MAX(1,jp - 1)
  ryp = ratey(j)
  rym = 1. - ryp
  i0  = (j-1)*nxo
  i0p = (jp-1)*nxi
  i0m = (jm-1)*nxi
  DO i = 1,nxo
    ip = MIN(indxx(i),nxi)
    im = MAX(1,ip - 1)
    rxp = ratex(i)
    rxm = 1. - rxp
    vo(i0+i) = rxp*ryp*vi(i0p+ip) + rxp*rym*vi(i0m+ip) + &
               rxm*ryp*vi(i0p+im) + rxm*rym*vi(i0m+im)
  END DO
END DO

RETURN
END

!==============================================================================

SUBROUTINE init_interp_2d()
USE interp2d_fi

INTEGER alloc_stat

irate = 0

IF( ALLOCATED(ratex) )DEALLOCATE( ratex,STAT=alloc_stat )
IF( ALLOCATED(ratey) )DEALLOCATE( ratey,STAT=alloc_stat )
IF( ALLOCATED(indxx) )DEALLOCATE( indxx,STAT=alloc_stat )
IF( ALLOCATED(indxy) )DEALLOCATE( indxy,STAT=alloc_stat )

RETURN
END

!==============================================================================

SUBROUTINE exit_interp_2d()

CALL init_interp_2d()

RETURN
END
