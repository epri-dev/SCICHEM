SUBROUTINE rebin( bin,ni,bout,no,bfac,nbin,nbins,nbinl )

! This routine sets up rebinning factors to rebin from bin to bout
! bin,bout = list of dust diameter bin boundaries
! ni ,no   = number of bins
! bfac     = array of bin factors
! nbin     = array of number of output bins for each input bin
! nbins    = array of number of lowest output bins for each input bin
! nbinl    = array of pointers to first bfac for each input bin

IMPLICIT NONE

INTEGER,                  INTENT( IN  ) :: ni, no
REAL,    DIMENSION(ni+1), INTENT( IN  ) :: bin
REAL,    DIMENSION(no+1), INTENT( IN  ) :: bout
INTEGER, DIMENSION(ni),   INTENT( OUT ) :: nbin, nbins, nbinl
REAL,    DIMENSION(*),    INTENT( OUT ) :: bfac

REAL    dlx, dux, frac
INTEGER iout, i, j

!------ set rebin factors

iout = 0

DO i = 1,ni

  nbin(i) = 0

  IF( bin(i) < bout(1) )THEN
    nbin(i) = 1
    iout = iout + 1
    IF( bin(i+1) < bout(1) )THEN
      frac = 1.0
    ELSE
      frac = LOG(bout(1)/bin(i))/LOG(bin(i+1)/bin(i))
    END IF
    bfac(iout) = frac
    nbins(i) = 1
    nbinl(i) = iout
  END IF

  DO j = 1,no

    IF( bout(j+1) > bin(i) .AND. bout(j) < bin(i+1) )THEN

      dlx  = MAX(bout(j),bin(i))
      dux  = MIN(bout(j+1),bin(i+1))
      frac = LOG(dux/dlx)/LOG(bin(i+1)/bin(i))

      IF( j == 1 .AND. nbin(i) == 1 )THEN
        bfac(iout) = bfac(iout) + frac
      ELSE
        nbin(i) = nbin(i) + 1
        iout    = iout    + 1
        bfac(iout) = frac
        IF( nbin(i) == 1 )THEN
          nbins(i) = j
          nbinl(i) = iout
        END IF
      END IF

    END IF

  END DO

  IF( bin(i+1) > bout(no+1) )THEN
    IF( bin(i) > bout(no+1) )THEN
      nbin(i) = nbin(i) + 1
      iout    = iout    + 1
      nbins(i) = no
      nbinl(i) = iout
      bfac(iout) = 1.0
    ELSE
      frac = LOG(bin(i+1)/bout(no+1))/LOG(bin(i+1)/bin(i))
      bfac(iout) = bfac(iout) + frac
    END IF
  END IF

END DO

RETURN
END

!===============================================================================

SUBROUTINE rebin_s( bin,ni,bout,no,bfac,nbin,nbins,nbinl )

! This routine sets up rebinning factors to rebin from bin to bout
! bin      = list of input dust diameter sizes
! bout     = list of output dust diameter bin boundaries
! ni ,no   = number of bins
! bfac     = array of bin factors
! nbin     = array of number of output bins for each input bin
! nbins    = array of number of lowest output bins for each input bin
! nbinl    = array of pointers to first bfac for each input bin

IMPLICIT NONE

INTEGER,                  INTENT( IN  ) :: ni, no
REAL,    DIMENSION(ni),   INTENT( IN  ) :: bin
REAL,    DIMENSION(no+1), INTENT( IN  ) :: bout
INTEGER, DIMENSION(ni),   INTENT( OUT ) :: nbin, nbins, nbinl
REAL,    DIMENSION(*),    INTENT( OUT ) :: bfac

INTEGER i, j

!------ set rebin factors

InputBins : DO i = 1,ni

  nbin(i)  = 1
  bfac(i)  = 1.
  nbinl(i) = i

  DO j = 1,no
    IF( bout(j+1) > bin(i) )THEN
      nbins(i) = j
      CYCLE InputBins
    END IF
  END DO

  nbins(i) = no

END DO InputBins

RETURN
END

!===============================================================================

SUBROUTINE check_bounds( bin,ni,bout,no,icls )

!  Adjust bin sizes to remove small mismatches

IMPLICIT NONE

REAL, PARAMETER :: EPS = 1.E-4

INTEGER,               INTENT( IN    ) :: ni, no, icls
REAL, DIMENSION(ni+1), INTENT( INOUT ) :: bin
REAL, DIMENSION(no+1), INTENT( INOUT ) :: bout

INTEGER i, j, js
REAL    btest
LOGICAL ltest

LOGICAL, EXTERNAL :: IsLiquid

IF( IsLiquid(icls) )THEN
  IF( bout(1) > 0. )THEN
    bin(1) = MIN(1.E-8,0.5*bin(2),bout(1))
  ELSE
    bin(1) = MIN(1.E-8,0.5*bin(2),0.5*bout(2))
  END IF
  bout(1) = bin(1)
END IF

!------ Try to align output bins with input bins

js = 1

OutBinLoop : DO i = 1,no

  DO j = js,ni

    btest = EPS*(bin(j+1)-bin(j))

    IF( ABS(bout(i)-bin(j)) < btest )THEN

      IF( bout(i) > bin(j) )THEN
        IF( i == 1 )THEN
          ltest = .TRUE.
        ELSE
          ltest = bout(i)-bin(j) < 0.1*(bout(i)-bout(i-1))
        END IF
      ELSE
        ltest = bin(j)-bout(i) < 0.1*(bout(i+1)-bout(i))
      END IF

      IF( ltest )THEN
        bout(i) = bin(j)
        js = j + 1
      END IF

      CYCLE OutBinLoop

    END IF

    IF( bout(i) < bin(j) )CYCLE OutBinLoop

  END DO

END DO OutBinLoop

RETURN
END
