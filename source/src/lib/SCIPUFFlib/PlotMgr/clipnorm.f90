MODULE ClipNorm_fi

! Module for the clipnorm routines used for building evaluating PDF's from a set
! of one or more clipped normal PDFs

  SAVE

!=== Parameters ===============================================================

  INTEGER, PARAMETER :: N_RANSET = 100               ! Number of random numbers in a set
  INTEGER, PARAMETER :: MAX_SET  = 10                ! Max. number of random number sets
  INTEGER, PARAMETER :: N_TAIL   = 5                 ! Number of points in tail to use analytic value
  INTEGER, PARAMETER :: N_MAIN   = N_RANSET - N_TAIL ! Last point not in the tail

  REAL,    PARAMETER :: RT2R = 0.7071068             ! Reciprocal of square root of 2.0
  REAL,    PARAMETER :: SQR2 = 1.4142136             ! Square root of 2.0

  REAL,    PARAMETER :: MIN_COEFF = 1.E-06           ! Minimum value of combination term coefficient

!=== Fixed dimension arrays ===================================================

  REAL,    DIMENSION(N_RANSET) :: cdf                ! Interpolation factors

!=== Allocatable arrays =======================================================

  INTEGER, DIMENSION((MAX_SET+1))          :: indx        ! Combination term index ( 2**(i-1) + 1 = 2,3,5,9,17... )

  LOGICAL, DIMENSION(2**(MAX_SET+1))       :: termInit    ! Combination terms have been computed

  REAL, DIMENSION(N_RANSET,2**(MAX_SET+1)) :: rndmCN      ! Clipped normal random set
  REAL, DIMENSION(N_RANSET,2**(MAX_SET+1)) :: rndmCDF     ! Clipped normal random set
  REAL, DIMENSION(N_RANSET,(MAX_SET))      :: rndmset     ! Uniform random sets
  REAL, DIMENSION((MAX_SET+1))             :: gauss_mean  ! Gaussian mean of the individual pdf
  REAL, DIMENSION((MAX_SET+1))             :: gauss_sig   ! Gaussian sigma of the individual pdf
  REAL, DIMENSION((MAX_SET+1))             :: gauss_gam   ! Gamma of the individual pdf
  REAL, DIMENSION((MAX_SET+1))             :: gauss_gamI  ! 1-Gamma of the individual pdf
  REAL, DIMENSION((MAX_SET+1))             :: full_gam    ! Total gamma of the individual pdf (includes PA)
  REAL, DIMENSION((MAX_SET+1))             :: full_gamI   ! Total 1-gamma of the individual pdf (includes PA)
  REAL, DIMENSION(2**(MAX_SET+1))          :: term_mean   ! Combination term mean
  REAL, DIMENSION(2**(MAX_SET+1))          :: term_sig    ! Combination term sigma
  REAL, DIMENSION(2**(MAX_SET+1))          :: term_gam    ! Combination term gamma
  REAL, DIMENSION(2**(MAX_SET+1))          :: term_coeff  ! Combination term coefficient
  REAL, DIMENSION(2**(MAX_SET+1))          :: tail_wt     ! Combination term tail weight
  REAL, DIMENSION((MAX_SET+1))             :: set_mean    ! clipped normal mean of the individual pdf
  REAL, DIMENSION((MAX_SET+1))             :: set_sig     ! clipped normal sigma of the individual pdf
  REAL, DIMENSION((MAX_SET+1))             :: set_qq      ! PA of the individual pdf


!=== Pseudo-Parameters ========================================================
!    Parameters that can't be set as such because the Linux compiler doesn't
!    seem to allow FLOAT() statements in parameter statements

  REAL    :: TAIL_FAC  ! Weighting factor
  REAL    :: MAIN_FAC  ! Weighting factor
  REAL    :: RAN_FAC   ! Averaging factor

!=== Scalars ==================================================================

  LOGICAL :: rndmInit  ! rndmCN has been built
  LOGICAL :: clipInit  ! clipnormInit has been called

  REAL :: meanT        ! Overall mean : Sum mean*PA
  REAL :: gamT         ! Overall gamma
  REAL :: meanS        ! Overall conditional mean
  REAL :: sigS         ! Overall sigma
  REAL :: gamS         !
  REAL :: coeffS       ! First term

  INTEGER :: nSets     ! Number of individual clipped normals
  INTEGER :: nTerms    ! Number of combination terms (2**nSets)

!=== Initialization ===========================================================

  COMMON /ClipNorm_fi_cmn/ rndmInit, clipInit, meanT, gamT, meanS, sigS, gamS, coeffS, nSets, nTerms
  COMMON /ClipNorm_fi_cmn/ termInit, rndmCN, rndmCDF, gauss_mean, gauss_sig, gauss_gam, gauss_gamI
  COMMON /ClipNorm_fi_cmn/ full_gam, full_gamI, term_mean, term_sig, term_gam, term_coeff
  COMMON /ClipNorm_fi_cmn/ tail_wt, set_mean, set_sig, set_qq
!$OMP THREADPRIVATE(/ClipNorm_fi_cmn/)

END MODULE ClipNorm_fi

!==============================================================================

MODULE ClipNorm

  INTERFACE
    REAL FUNCTION ProbExceedInvert( prb,v0,p0,v1,p1,pp )
      REAL, INTENT( IN  ) :: prb       ! Probability for C > val
      REAL, OPTIONAL      :: v0        ! lower bound guess
      REAL, OPTIONAL      :: v1        ! upper bound guess
      REAL, OPTIONAL      :: p0        ! lower bound guess
      REAL, OPTIONAL      :: p1        ! upper bound guess
      REAL, OPTIONAL      :: pp        ! actual result
    END FUNCTION

    SUBROUTINE initClipNormSet( nb,xmean,xsig,qq,mean0,gam0,maxSets )
      INTEGER,             INTENT( IN  ) :: nb             ! Number of independent pdfs
      REAL, DIMENSION(nb), INTENT( IN  ) :: xmean,xsig,qq  ! mean, sigma, gamma and probability of the individual pdf
      REAL,                INTENT( OUT ) :: mean0,gam0
      INTEGER,             OPTIONAL      :: maxSets        ! Maximum number of sets to consider
    END SUBROUTINE

  END INTERFACE

END MODULE ClipNorm
!==============================================================================
!==============================================================================
!    Global initialization/cleanup routines
!==============================================================================
!==============================================================================
SUBROUTINE initClipNorm()

!-- Initialize uniform random number sets

USE ClipNorm_fi
USE error_fi

IMPLICIT NONE

!=== Local variables ==========================================================

INTEGER ios, i

INTEGER, DIMENSION(:), ALLOCATABLE :: nseed

REAL, DIMENSION(:), ALLOCATABLE    :: wrkspace    ! Temporary work array

!==============================================================================
!=== Initialize pseudo-parameters

TAIL_FAC = FLOAT(N_RANSET)/FLOAT(N_TAIL) ! Weighting factor
MAIN_FAC = 1.0 - FLOAT(N_MAIN)/FLOAT(N_RANSET) ! Weighting factor
RAN_FAC  = 1.0/FLOAT(N_RANSET)           ! Averaging factor

!=== Initialize random number generation

CALL RANDOM_SEED( SIZE=i )  ! I is set to the size of  the seed array
i = MAX(i,2)
ALLOCATE( nseed(i),STAT=ios )
IF( ios /= 0 )THEN
  nError   = IV_ERROR
  eRoutine = 'init_cn'
  eMessage = 'Error allocating work arrays (nseed)'
  GOTO 9999
END IF
nseed(1) = 12324567
nseed(2) = 987654321
DO i = 3,SIZE(nseed)
  nseed(i) = 2*nseed(i-1) + 1
END DO

CALL RANDOM_SEED( PUT=nseed )

indx = 0

clipInit = .FALSE.

!=== Allocate temporary space

ALLOCATE( wrkspace(MAX_SET),STAT=ios )
IF( ios /= 0 )THEN
  nError   = IV_ERROR
  eRoutine = 'initClipNorm'
  eMessage = 'Error allocating work arrays (wrkspace)'
  GOTO 9999
END IF

!=== Generate random sets

DO i = 1,N_RANSET
  CALL RANDOM_NUMBER( HARVEST=wrkspace )
  rndmset(i,:) = wrkspace
END DO

!=== Generate interpolation terms

DO i = 1,N_RANSET
  cdf(i)= FLOAT(i)/FLOAT(N_RANSET)
END DO

!=== Generate term pointers

DO i = 1,MAX_SET+1
  indx(i) = 2**(i-1) + 1
END DO

!=== Clean up temporary memory and return

DEALLOCATE( wrkspace,STAT=ios )

RETURN

!=== Error handing

9999 CONTINUE

CALL exitClipNorm()
IF( ALLOCATED(wrkspace) )DEALLOCATE( wrkspace,STAT=ios )

RETURN
END
!==============================================================================
!==============================================================================
SUBROUTINE exitClipNorm()

!-- Clean up random number sets

USE ClipNorm_fi

IMPLICIT NONE

!==============================================================================
!=== Make sure memory for a set of clipped-normals has been cleaned up

CALL exitClipNormSet()

RETURN
END
!==============================================================================
!==============================================================================
!    Data set initialization/cleanup routines
!==============================================================================
!==============================================================================
SUBROUTINE initClipNormSet( nb,xmean,xsig,qq,mean0,gam0,maxSets )

!-- Initialize the PDF generation for a set of clipped normals

USE ClipNorm_fi
USE error_fi

IMPLICIT NONE

!=== Parameters ===============================================================

REAL,    PARAMETER :: EFFECTIVE_ZERO = 0.0         ! Effective zero mean - may need to make this non-zero
INTEGER, PARAMETER :: EFFECTIVE_MAX  = MAX_SET + 1 ! Maximum number of sets - composite if above

!=== Inputs and Outputs =======================================================

INTEGER,             INTENT( IN  ) :: nb             ! Number of independent pdfs
REAL, DIMENSION(nb), INTENT( IN  ) :: xmean,xsig,qq  ! mean, sigma, gamma and probability of the individual pdf
REAL,                INTENT( OUT ) :: mean0,gam0     ! Overall mean and gamma
INTEGER,             OPTIONAL      :: maxSets        ! Maximum number of sets to consider

!=== Local variables ==========================================================

REAL soc
REAL gbar
REAL sigg

REAL,    DIMENSION(MAX_SET) :: wrkM
REAL,    DIMENSION(MAX_SET) :: wrkS
REAL,    DIMENSION(MAX_SET) :: wrkQ
INTEGER, DIMENSION(MAX_SET) :: ID

INTEGER nValid
INTEGER i,j,k,jf,iterm
INTEGER nMax

REAL, EXTERNAL :: CNexceed

!=== Initialize return values

mean0 = 0.
gam0  = 0.

clipInit = .FALSE.

IF( PRESENT(maxSets) )THEN
  nMax = maxSets
ELSE
  nMax = EFFECTIVE_MAX
END IF

!=== Size check

IF( nb > MAX_SET )THEN
  nError   = IV_ERROR
  eRoutine = 'initClipNormSet'
  eMessage = 'Too many component distributions'
  WRITE(eInform,'(A,2I5)')'Request, Max :',nb,MAX_SET
  GOTO 9999
END IF

!=== Set sizes - check for zero mean

nValid  = 0
DO i = 1,nb
  IF( xmean(i) > EFFECTIVE_ZERO .AND. xsig(i) > EFFECTIVE_ZERO )THEN
    nValid = nValid + 1
    wrkM(nValid) = xmean(i)
    ID(nValid)   = i
  END IF
END DO
IF( nValid == 0 )THEN
  nError   = IV_ERROR
  eRoutine = 'initClipNormSet'
  eMessage = 'All sets have zero mean'
  GOTO 9999
END IF

nSets = MIN(nValid,nMax)
nTerms = 2**nSets

!=== Save set values

IF( nSets == nb )THEN
  set_mean = xmean
  set_sig  = xsig
  set_qq   = qq
ELSE
  IF( nValid > nMax )THEN
    CALL vsorti( wrkM,ID,nValid )
    j = nValid - nMax+1
    DO i = 1,j
      wrkS(i) = xsig(ID(i))
      wrkQ(i) = qq(ID(i))
    END DO
    CALL composit( wrkM,wrkS,wrkQ,j,set_mean(1),set_sig(1),set_qq(1) )
    DO i = 2,nMax
      set_mean(i) = xmean(ID(i+j-1))
      set_sig(i)  = xsig(ID(i+j-1))
      set_qq(i)   = qq(ID(i+j-1))
    END DO
  ELSE
    DO i = 1,nValid
      set_mean(i) = xmean(ID(i))
      set_sig(i)  = xsig(ID(i))
      set_qq(i)   = qq(ID(i))
    END DO
  END IF
END IF

!=== Set Gaussian values

DO i = 1,nSets
  soc = set_sig(i)/set_mean(i)
  CALL clnpar( soc,gbar,sigg )
  gauss_sig(i)  = set_mean(i)*sigg
  gauss_mean(i) = set_mean(i)*gbar
  gauss_gamI(i) = CNexceed( gauss_mean(i),gauss_sig(i),0.0 ) !0.5*erfc(gauss_mean(i)*RT2R/gauss_sig(i))
END DO
gauss_gam = 1.0 - gauss_gamI
full_gamI = set_qq*gauss_gamI
full_gam  = 1.0 - full_gamI

!=== Compute overall values

meanT = 0.
gamT  = 1.
sigS  = 0.
gamS  = 1.

DO j = 1,nSets
  meanT = meanT + set_qq(j)*set_mean(j)
  gamT  = gamT*full_gam(j)
  sigS  = sigS  + set_qq(j)*(set_sig(j))**2  &
                + set_qq(j)*(1.0-set_qq(j))*set_mean(j)**2
  gamS  = gamS*(1.0 - set_qq(j))
END DO

gamT  = 1.0 - gamT
gamS  = 1.0 - gamS
meanS = meanT/gamS
sigS  = SQRT( (sigS - gamS*(1.0-gamS)*meanS**2)/gamS )
soc   = sigS/meanS
CALL clnpar( soc,gbar,sigg )
sigS  = meanS*sigg
meanS = meanS*gbar

!=== Set up for multiple sets - combination terms

rndmInit = .FALSE.

!=== Build combination terms

iterm  = 1

TermLoop: DO k = 2,nTerms

  IF( k == indx(iterm) )THEN    ! Term is individual pdf
    jf    = iterm
    iterm = iterm + 1
  ELSE                          ! Term is sum of individual pdfs
    jf = 0
  END IF

  term_coeff(k) = 1.0
  DO j = 1,nSets
    IF( BTEST(k-1,j-1) )THEN
      IF( jf == 0 )term_coeff(k) = term_coeff(k)*full_gamI(j)
    ELSE
      term_coeff(k) = term_coeff(k)*full_gam(j)
    END IF
  END DO

  termInit(k) = .FALSE.

END DO TermLoop

!=== First term

coeffS = 1.0
DO j = 1,nSets
  coeffS = coeffS*full_gam(j)
END DO

!=== Return values

mean0 = meanT
gam0  = gamT

!=== Exit

clipInit = .TRUE.

9999 CONTINUE

RETURN

END
!==============================================================================
!==============================================================================
SUBROUTINE composit( mean,sig,qq,n,cm,cs,cq )

!composite a set of clipped normals (with PA) into a single clipped normal (with PA)

IMPLICIT NONE

INTEGER,          INTENT( IN  ) :: n
REAL,DIMENSION(n),INTENT( IN  ) :: mean
REAL,DIMENSION(n),INTENT( IN  ) :: sig
REAL,DIMENSION(n),INTENT( IN  ) :: qq
REAL,             INTENT( OUT ) :: cm
REAL,             INTENT( OUT ) :: cs
REAL,             INTENT( OUT ) :: cq

INTEGER i,j
REAL    c

!=== Set composite PA
cq = 1.0
DO i = 1,n
  cq = cq*(1.0-qq(i))
END DO
cq = 1.0 - cq

!=== Set composite mean,sig
cm = 0.0
cs = 0.0
DO i = 1,n
  c  = qq(i)*mean(i)
  cm = cm + c
  cs = cs + qq(i)*(sig(i)**2+mean(i)**2)
  DO j = i+1,n
    cs = cs + 2.*qq(j)*mean(j)*c
  END DO
END DO
cm = cm/cq
cs = SQRT( cs/cq - cm**2 )

RETURN
END
!==============================================================================
!==============================================================================
SUBROUTINE exitClipNormSet( )

!clean up PDF generation for a set of clipped normals

USE ClipNorm_fi

IMPLICIT NONE

!=== Reset initialization flags

clipInit = .FALSE.
rndmInit = .FALSE.

RETURN
END
!==============================================================================
!==============================================================================
SUBROUTINE buildRndm()

!Initialze PDF generation for a set of clipped normals
!Build 2**n random sets

USE ClipNorm_fi
USE error_fi

IMPLICIT NONE

!=== Local variables ==========================================================

REAL, EXTERNAL :: erfcinv

INTEGER i,j,k,ios

CHARACTER(16) errorString

REAL, DIMENSION(:), ALLOCATABLE    :: wrkspace    ! Temporary work array

!==============================================================================
!===== check initialization

IF( .NOT.clipInit )THEN
  nError   = IV_ERROR
  eRoutine = 'buildRndm'
  eMessage = 'Attempt to build random sets without first calling initClipNormSet'
  GOTO 9999
END IF

IF( rndmInit )THEN
  nError   = IV_ERROR
  eRoutine = 'buildRndm'
  eMessage = 'Attempt to build random sets a second time'
  GOTO 9999
END IF

!=== set up for multiple sets - combination terms

IF( nSets > 1 )THEN

!===== allocate space

  errorString = 'wrkspace'
  IF( ALLOCATED(wrkspace) )THEN
    GOTO 9998
  ELSE
    ALLOCATE( wrkspace(nSets),STAT=ios )
    IF( ios /= 0 )GOTO 9997
  END IF

!===== Build clipped normal random sets

  rndmCN = 0.0
  rndmCN(:,1) = 1.0

  DO i = 1,N_RANSET

    wrkspace = rndmset(i,1:nSets)

!--- Find a clipped normal distributed random number, with xmean and xsig
!    mean and variance, from a uniform random number,(Ref. Pg.54 Record book)
!    for the nb individual pdf at (2**(j-1)+1)'th terms.

    DO j=1,nSets
      rndmCN(i,indx(j)) = gauss_mean(j) + SQR2*gauss_sig(j)*erfcinv( 2.*gauss_gamI(j)*wrkspace(j) )
    END DO

    DO k = 2,nTerms
      DO j = 1,nSets
!--- Skip the contribution of (j-1)th bit to the (2**(j-1)+1)'th  term
!    (it is the only contribution) to avoid changing the f1, f2, ... terms.
        IF( indx(j) /= k )THEN
          IF( BTEST(k-1,j-1) )THEN  ! Add individual random numbers to generate new random numbers
            rndmCN(i,k) = rndmCN(i,k) + rndmCN(i,indx(j))
          END IF
        END IF
      END DO
    END DO

  END DO

END IF

rndmInit = .TRUE.

!=== Exit

9999 CONTINUE

!=== Clean up work space

IF( ALLOCATED(wrkspace) )DEALLOCATE( wrkspace,STAT=ios )

RETURN

!=== Error handling

9998 CONTINUE
nError   = IV_ERROR
eRoutine = 'buildRndm'
eMessage = 'Attempt to allocate previously allocated array'
eInform  = 'Array = '//TRIM(errorString)
GOTO 9999

9997 CONTINUE
nError   = IV_ERROR
eRoutine = 'buildRndm'
eMessage = 'Allocation failure'
eInform  = 'Array = '//TRIM(errorString)
GOTO 9999

END
!==============================================================================
!==============================================================================
SUBROUTINE setRndm( k )

!Initialze PDF generation for a set of clipped normals
!Build the terms for the kth contribution

USE ClipNorm_fi
USE error_fi

IMPLICIT NONE

!=== Inputs and Outputs =======================================================

INTEGER, INTENT( IN ) :: k             ! termID

!=== Local variables ==========================================================

REAL    soc, gbar, sigg, ysum
REAL    prb, prbc
INTEGER i, j

REAL, EXTERNAL :: CNexceed

!==============================================================================
!===== check initialization

IF( .NOT.clipInit )THEN
  nError   = IV_ERROR
  eRoutine = 'setRndm'
  eMessage = 'Attempt to build combination terms without first calling initClipNormSet'
  GOTO 9999
END IF

IF( .NOT.rndmInit )THEN
  CALL buildRndm()
  IF( nError /= NO_ERROR )GOTO 9999
END IF

!==== sort and build terms if necessary

IF( .NOT.termInit(k) )THEN

!=== Build combination terms

!=== Sort the distribution

  CALL usort( rndmCN(1,k),N_RANSET )

!=== Scale the distribution to give the conditional mean

  gbar = 0.0
  DO j = 1,nSets
    IF( BTEST(k-1,j-1) )THEN ! Add the analytical individual conditional mean contributions
      gbar = gbar + set_mean(j)/gauss_gamI(j)
    END IF
  END DO
  ysum = rndmCN(1,k)
  DO i = 2,N_RANSET
    ysum = ysum + rndmCN(i,k)
  END DO
  ysum = ysum*RAN_FAC

  rndmCN(1:N_RANSET,k)  = rndmCN(1:N_RANSET,k)*gbar/ysum
  rndmCDF(1:N_RANSET,k) = cdf(1:N_RANSET)*term_coeff(k)

!=== Build terms

  term_mean(k) = 0.0
  term_sig(k)  = 0.0
  DO j = 1,nSets
    IF( BTEST(k-1,j-1) )THEN ! Add the mean and sigma individual pdf contributions (M1n = q1*M1 + q2*M2 + .. + qn*Mn)
      term_mean(k) = term_mean(k) + set_qq(j)*set_mean(j)
      term_sig(k)  = term_sig(k)  + set_qq(j)*(set_sig(j))**2 &
                                  + set_qq(j)*(1.0-set_qq(j))*set_mean(j)**2
    END IF
  END DO

  term_sig(k) = SQRT(term_sig(k))
  soc = term_sig(k)/term_mean(k)
  CALL clnpar( soc,gbar,sigg )
  term_sig(k)  = term_mean(k)*sigg
  term_mean(k) = term_mean(k)*gbar
  term_gam(k)  = 1.0 - CNexceed( term_mean(k),term_sig(k),0.0 ) !0.5*erfc( term_mean(k)*RT2R/term_sig(k) )

  prb = CNexceed( term_mean(k),term_sig(k),rndmCN(N_MAIN,k) )
  prbc = prb/(1.0 - term_gam(k))
  tail_wt(k) = term_coeff(k)* &
               ( ( CNexceed( term_mean(k),term_sig(k),rndmCN(N_MAIN,k) ) )/ &
               (1.0 - term_gam(k)) - MAIN_FAC)

  termInit(k) = .TRUE.

END IF

!=== Exit

9999 CONTINUE

!=== Clean up work space

RETURN

END
!==============================================================================
!==============================================================================
!    PDF at a point generation routines - previous initialization
!==============================================================================
!==============================================================================
REAL FUNCTION ProbExceed( val )

!Find probabaility of exceeding a value

USE ClipNorm_fi
USE error_fi

IMPLICIT NONE

!=== Parameters ===============================================================

REAL, PARAMETER :: ARG_MAX = 3.2 ! erfc(ARG_MAX) < 6.0E-6

!=== Inputs and Outputs =======================================================

REAL, INTENT( IN ) :: val        ! Probability for C > val

!=== Locals ===================================================================

INTEGER j, k, jf, iterm
REAL    xtmp
REAL    wt, arg, prob

REAL, EXTERNAL :: CNexceed,erfc
REAL, EXTERNAL :: InterpVal

!==============================================================================
!=== Initialize return value

prob = 0.0

!=== check initialization

IF( .NOT.clipInit )THEN
  nError   = IV_ERROR
  eRoutine = 'ProbExceed'
  eMessage = 'Attempt to evaluate PDF without first calling initClipNormSet'
  GOTO 9999
END IF

!=== Check if any PDF is large - if not use overall clipped normal

DO j = 1,nSets
  arg = (val/FLOAT(nSets)-gauss_mean(j))*RT2R/gauss_sig(j)
  IF( arg < ARG_MAX )EXIT
END DO

!=== No - Use overall clipped normal value and return

IF( arg >= ARG_MAX )THEN
  prob = gamS*CNexceed( meanS,sigS,val )  !(0.5*erfc( (val-meanS)*RT2R/sigS ))
  GOTO 9999
END IF

!=== Yes - Initialize for PDF generation

prob   = 0.0
iterm  = 1   !Index to first individual PDF term

!=== Loop over combination terms

TermLoop: DO k = 2,nTerms

!=== Check to see if term is individual pdf term or sum term

  IF( k == indx(iterm) )THEN    ! Term is individual pdf
    jf    = iterm              !   Point to individual pdf
    iterm = iterm + 1          !   Increment index
  ELSE                          ! Term is sum of individual pdfs
    jf = 0                      !   Sum term flag
  END IF

!=== Term comes from sum of individual pdfs

  IF( jf == 0 )THEN

!==== Check to see if term coefficient is large enough to consider

    IF( term_coeff(k) < MIN_COEFF )CYCLE TermLoop

    CALL setRndm( k )
    IF( nError /= NO_ERROR )GOTO 9999

!==== Check for tail
!==== Not in last N_TAIL  points
!     Locate probability xtmp at val from cdf (numerical sum of individual pdfs)

    IF( val < rndmCN(N_MAIN,k) )THEN

      xtmp = term_coeff(k) - InterpVal( N_RANSET,rndmCN(1,k),val,rndmCDF(1,k) )

!==== Last N_TAIL points
!     Use weighted average with analytic result

    ELSE

      xtmp = term_coeff(k)*(CNexceed( term_mean(k),term_sig(k),val ))/(1.0 - term_gam(k))

!==== Weighted Average

      IF( val < rndmCN(N_RANSET,k) )THEN
        wt = (rndmCN(N_RANSET,k)-val)/(rndmCN(N_RANSET,k)-rndmCN(N_MAIN,k))
        xtmp = xtmp - wt*tail_wt(k)
      END IF

    END IF

!=== Term comes from individual pdfs

  ELSE
                                   !0.5*erfc((val-gauss_mean(jf))*RT2R/gauss_sig(jf)))
    xtmp = term_coeff(k)*set_qq(jf)*CNexceed( gauss_mean(jf),gauss_sig(jf),val )

  END IF

!=== Apply term

  prob = prob + xtmp

END DO TermLoop

!=== Apply first term

prob = MIN(1.,MAX(0.,prob))

!=== Set return value and exit

9999 CONTINUE

ProbExceed = prob

RETURN
END
!==============================================================================
!==============================================================================
REAL FUNCTION ProbExceedInvert( prbIn,v0,p0,v1,p1,pp )

!Find value which has a given probabaility of exceeding

USE ClipNorm_fi
USE error_fi

IMPLICIT NONE

!=== Parameters ===============================================================

REAL, PARAMETER   :: MIN_PRB         = 1.0E-8
REAL, PARAMETER   :: ABSERR          = 1.0E-20
REAL, PARAMETER   :: RELERR          = 1.0E-5
REAL, PARAMETER   :: TOLERANCE       = 1.0E-3
INTEGER,PARAMETER :: MAX_EVALUATIONS = 500
INTEGER,PARAMETER :: MAX_TRY         =  25

!=== Inputs and Outputs =======================================================

REAL, INTENT( IN  ) :: prbIn     ! Probability for C > val
REAL, OPTIONAL      :: v0        ! lower bound guess
REAL, OPTIONAL      :: v1        ! upper bound guess
REAL, OPTIONAL      :: p0        ! lower bound guess
REAL, OPTIONAL      :: p1        ! upper bound guess
REAL, OPTIONAL      :: pp        ! actual result

!=== Locals ===================================================================

INTEGER count, neval, ntry
REAL    xmin, xmax, pmin, pmax
REAL    xa, xb, xc, fa, fb, fc
REAL    range0, range, tol, delx, fac, delf
REAL    val, pval, ptol, prb

REAL, EXTERNAL :: ProbExceed

!=== Initilaize return value

prb  = MAX( prbIn,MIN_PRB )
val  = 0.0
pval = prb

IF ( prb > gamT )GOTO 9999

!=== check initialization

IF( .NOT.clipInit )THEN
  nError   = IV_ERROR
  eRoutine = 'ProbExceedInvert'
  eMessage = 'Attempt to evaluate PDF without first calling initClipNormSet'
  GOTO 9999
END IF

!=== Set starting guesses - gernate if not provided

xmin = -999.0
xmax = -999.0

IF( PRESENT(v0) )THEN
  xmin = v0
  IF( PRESENT(p0) )THEN
    pmin = p0
  ELSE
    pmin = ProbExceed( xmin )
  END IF
ELSE
  xmin = meanT
  pmin = ProbExceed( xmin )
  ntry = MAX_TRY
  DO WHILE( pmin <= prb .AND. ntry > 1 )
    xmax = xmin
    pmax = pmin
    ntry = ntry - 1
    xmin = 0.1*xmin
    pmin = ProbExceed( xmin )
  END DO
END IF

IF( PRESENT(v1) )THEN
  xmax = v1
  IF( PRESENT(p1) )THEN
    pmax = p1
  ELSE
    pmax = ProbExceed( xmax )
  END IF
ELSE IF( xmax == -999.0)THEN
  xmax = 10.0*xmin
  pmax = ProbExceed( xmax )
  ntry = MAX_TRY
  DO WHILE( pmax > prb .AND. ntry > 1 )
    ntry = ntry - 1
    xmax = 10.0*xmax
    pmax = ProbExceed( xmax )
  END DO
END IF

IF( PRESENT(pp) )THEN
  ptol = TOLERANCE
ELSE
  ptol = 0.01*TOLERANCE
END IF

!=== Initialize iteration - modified secant rule
!    xb,xc = end points of interval with xb the better solution
!    xb,xa = last two iterations
!    Use secant rule unless change is too small or range isn't decreasing fast enough

count = 0

xb = xmin
xc = xmax
xa = xc

fb = pmin - prb
fc = pmax - prb
fa = fc

range0 = xc - xb

IF( SIGN(1.0,fb) == SIGN(1.0,fc) )THEN
  nError   = IV_ERROR
  eRoutine = 'ProbExceedInvert'
  eMessage = 'Initial range does not bracket requested value'
  GOTO 9999
END IF

!=== Iterate

neval = 1
DO WHILE( neval < MAX_EVALUATIONS )

!===== Make xb the better solution
  IF( ABS(fc) < ABS(fb) )THEN
    xa = xb
    fa = fb
    xb = xc
    fb = fc
    xc = xa
    fc = fa
  END IF

!===== Check to see if xb is "close enough"
  IF( ABS(fb) < ptol*prb )EXIT

!=====Check range to see if its "small enough"
  delx  = 0.5*(xc-xb)
  range = ABS(delx)
  tol   = RELERR*xb + ABSERR
  IF( range <= tol )EXIT

!===== Compute secant parameters
  fac = (xb-xa)*fb
  IF( fac >= 0.0 )THEN
    delf = fa - fb
  ELSE
    fac  = -fac
    delf = fb - fa
  END IF

!===== Update last iterate
  xa = xb
  fa = fb

!===== Determine best method to compute new iterate
  count = count + 1
  IF( count > 4 )THEN
    IF( 8.0*range >= range0 )THEN
      xb = 0.5*(xb+xc)                   !Bisection
    ELSE
      count = 0
      range0 = range
    END IF
  END IF

  IF( count <= 4 )THEN
    IF( fac <= ABS(delf)*tol )THEN
      xb = xb + SIGN(tol,delx)             !Minimum change
    ELSE IF( fac >= delf*delx )THEN
      xb = 0.5*(xb+xc)                     !Bisection
    ELSE
      xb = xb + fac/delf                   !Secant
    END IF
  END IF

  fb = ProbExceed( xb ) - prb
  neval = neval + 1

!===== reset range - make sure to bracket solution
  IF( SIGN(1.0,fb) == SIGN(1.0,fc) )THEN
    xc = xa
    fc = fa
  END IF

END DO

val  = xb
pval = fb + prb

!=== Set return value and exit

9999 CONTINUE

ProbExceedInvert = val
IF( PRESENT(pp) )pp = pval

RETURN
END
!==============================================================================
!==============================================================================
!    PDF at a point generation routines - no previous initialization
!==============================================================================
!==============================================================================
SUBROUTINE ptClipNormSet( nb,xmean,xsig,qq,invert,conditional,maxSets,val,prb )

!PDF generation for a set of clipped normals

USE ClipNorm
USE error_fi

IMPLICIT NONE

!=== Parameters ===============================================================

REAL, PARAMETER :: GAMMA_MIN = 1.E-4

!=== Inputs and Outputs =======================================================

INTEGER             ,INTENT( IN    ) :: nb             ! Number of independent pdfs
REAL, DIMENSION(nb) ,INTENT( IN    ) :: xmean,xsig,qq  ! mean, sigma, gamma and probability of the individual pdf
LOGICAL             ,INTENT( IN    ) :: invert         ! TRUE->prb=IN val=OUT ; FALSE->val=IN, prb=OUT
LOGICAL             ,INTENT( IN    ) :: conditional    ! TRUE->prb=conditional; FALSE->prb=mean
INTEGER             ,INTENT( IN    ) :: maxSets        ! Maximum number of independent pdfs - composite if nb > maxSets
REAL                ,INTENT( INOUT ) :: val,prb

!=== Locals ===================================================================

REAL xm, po, prob

REAL, EXTERNAL :: ProbExceed

!==============================================================================

!=== Initialize ClipNorm

CALL initClipNormSet( nb,xmean,xsig,qq,xm,po,maxSets=maxSets )
IF( nError /= NO_ERROR )GOTO 9999

IF( invert )THEN

  IF( conditional )THEN
    prob = prb*MAX(GAMMA_MIN,po)
  ELSE
    prob = prb
  END IF
  IF( prob > po )THEN
    val = 0.0
  ELSE
    val = ProbExceedInvert( prob )
  END IF

ELSE

  prb = ProbExceed( val )
  IF( conditional )THEN
    prb = prb/MAX(GAMMA_MIN,po)
  END IF

END IF

!=== Exit ClipNorm

9999 CONTINUE

CALL exitClipNormSet()

RETURN
END
!==============================================================================
!==============================================================================
!    Full PDF generation routines - no previous initialization
!==============================================================================
!==============================================================================
SUBROUTINE pdfClipNormSet( nb,xmean,xsig,qq,cmin,cmax,rat,delta,maxn,mode, &
                           maxSets,npts,xdf,cval,debugFlag )

! Generate a PDF from a set of clipped-normal pdfs and associated PAs

USE ClipNorm_fi
USE ClipNorm
USE error_fi

IMPLICIT NONE

!=== Parameters ===============================================================

REAL, PARAMETER :: PRB_MIN   = 1.0E-5
REAL, PARAMETER :: ABSERR    = 1.0E-20
REAL, PARAMETER :: RELERR    = 1.0E-5
REAL, PARAMETER :: TOLERANCE = 1.0E-3
REAL, PARAMETER :: LAST_FAC= 1.1

INTEGER,PARAMETER :: MAX_EVALUATIONS = 500

INTEGER,PARAMETER :: ITER_MODE = 0
INTEGER,PARAMETER :: HALF_MODE = 1

!=== Inputs and Outputs =======================================================

INTEGER,             INTENT( IN  ) :: nb        ! Number of independant pdfs
REAL, DIMENSION(nb), INTENT( IN  ) :: xmean     ! Clipped normal mean for independent pdfs
REAL, DIMENSION(nb), INTENT( IN  ) :: xsig      ! Clipped normal sigma for independent pdfs
REAL, DIMENSION(nb), INTENT( IN  ) :: qq        ! PA for independent pdfs
REAL ,               INTENT( IN  ) :: cmin      ! Minimum value of interest
REAL ,               INTENT( IN  ) :: cmax      ! Maximum value of interest
REAL ,               INTENT( IN  ) :: rat       ! Neighboring probabality reduction factor
REAL ,               INTENT( IN  ) :: delta     ! Neighboring probabality delta
INTEGER,             INTENT( IN  ) :: maxn      ! Max number of points in pdf
INTEGER,             INTENT( IN  ) :: mode      ! Generation mode
INTEGER,             INTENT( IN  ) :: maxSets   ! Maximum number independant pdfs - composite if nb > maxSets
INTEGER,             INTENT( OUT ) :: npts      ! Actual number of points in pdf
REAL, DIMENSION(*),  INTENT( OUT ) :: xdf       ! Numerical distribution - probability
REAL, DIMENSION(*),  INTENT( OUT ) :: cval      ! Numerical distribution - values
LOGICAL,OPTIONAL                   :: debugFlag ! Debugging flag

!=== Locals ===================================================================

REAL    xmin, xmax, xm, xx
REAL    pp, po, prb, pmin, pmax
REAL    xr, xl, dr, dl
REAL    del, fac
LOGICAL debugIt

REAL, EXTERNAL :: ProbExceed

!==============================================================================

IF( PRESENT( debugFlag ) )THEN
  debugIt = debugFlag
ELSE
  debugIt = .FALSE.
END IF

!=== Initialize the clipnorm function

CALL initClipNormSet( nb,xmean,xsig,qq,xm,po,maxSets=maxSets )
IF( nError /= NO_ERROR )GOTO 9999

!=== Set starting point - P > 0.0

xdf(1)  = po
cval(1) = 0.0

del = delta*xdf(1)

!=== Get P > cmin (mean if cmin=0.0)

IF( cmin > 0.0 )THEN
  xmin = cmin
ELSE
  xmin = xm
END IF

pmin = ProbExceed( xmin )
IF( nError /= NO_ERROR )GOTO 9999

!=== Check for cmin value - Exit if small else prepare to build PDF

prb = MAX(rat*xdf(1),xdf(1)-del)

IF( pmin < prb .AND. cmin > 0.0 )THEN
  xdf(2)  = pmin
  cval(2) = cmin
  npts    = 2
  IF( pmin < PRB_MIN )THEN
    xdf(2) = 0.0
    GOTO 9999
  END IF
ELSE
  npts = 1
  IF( cmin == 0.0 )THEN
    DO WHILE( pmin < prb )
      IF( ABS(pmin+po-1.) < PRB_MIN )EXIT
      xmin = xmin*0.5
      pmin = ProbExceed( xmin )
      IF( nError /= NO_ERROR )GOTO 9999
    END DO
  END IF
  IF( mode == HALF_MODE )THEN
    xdf(2)  = pmin
    cval(2) = cmin
    npts    = 2
  END IF
END IF

!=== Loop to build numerical XDF

IF( mode == HALF_MODE )THEN

  pp = xdf(2)
  xx = cval(2)
  DO WHILE( pp > PRB_MIN .AND. xx < cmax )
    xx = MIN(3.33333*xx,cmax)
    pp = ProbExceed( xx )
  END DO

  fac = 0.2*(xx-cmin)
  dr = cval(npts)
  xr = xdf(npts)
  DO WHILE( npts < maxn )

!=== Next point

    dl = dr
    xl = xr
    dr = dl + fac
    xr = ProbExceed( dr )
    npts = npts + 1
    cval(npts) = dr
    xdf(npts) = xr

!=== Check range

    CALL checkrange( dl,xl,dr,xr,rat,del,maxn,PRB_MIN,npts,xdf,cval )
    IF( xr <= PRB_MIN )EXIT
    IF( dr >= cmax )EXIT

  END DO

!=== Sort

  CALL vsortr( cval,xdf,npts )

ELSE

  DO

!=== Next point

    prb = MAX(rat*xdf(npts),xdf(npts)-del)

!=== Exit if reached the end - P < Pmin

    IF( prb < PRB_MIN )EXIT

!=== Locate first guess within a factor of 2

    xmax = xmin
    pmax = pmin
    fac  = 2.0
    DO WHILE( pmax >= prb )
      xmin = xmax
      pmin = pmax
      xmax = xmax*fac
      pmax = ProbExceed( xmax )
      fac   = 10.0
      IF( nError /= NO_ERROR )GOTO 9999
    END DO

!=== Set point

    xmin = ProbExceedInvert( prb,v0=xmin,p0=pmin,v1=xmax,p1=pmax,pp=pmin )
    npts = npts + 1
    xdf(npts)  = pmin
    cval(npts) = xmin

!=== Determine if need to continiue

    IF( cval(npts) > cmax .OR. npts == maxn )EXIT

  END DO

END IF

!=== Set last point

IF( npts == maxn )THEN
  pp = xdf(npts)
  xx = cval(npts)
  DO WHILE( pp > PRB_MIN )
    xx = xx*2.0
    pp = ProbExceed( xx )
  END DO
  cval(npts) = xx
  xdf(npts)  = 0.0
ELSE
  npts = npts + 1
  xdf(npts)  = 0.0
  cval(npts) = MAX(cmin,LAST_FAC*cval(npts-1))
END IF

!=== Exit

9999 CONTINUE

!=== Cleanup clipnorm function

term_coeff(1) = coeffS
xx = SUM(term_coeff)

CALL exitClipNormSet()

RETURN
END
!==============================================================================
!==============================================================================
RECURSIVE SUBROUTINE checkrange( dl,xl,dr,xr,rat,del,maxn,pmin,npts,xdf,cval )

IMPLICIT NONE

!=== Inputs and Outputs

REAL,               INTENT( IN    ) :: dl
REAL,               INTENT( IN    ) :: xl
REAL,               INTENT( IN    ) :: dr
REAL,               INTENT( IN    ) :: xr
REAL,               INTENT( IN    ) :: rat
REAL,               INTENT( IN    ) :: del
INTEGER,            INTENT( IN    ) :: maxn
REAL,               INTENT( IN    ) :: pmin
INTEGER,            INTENT( INOUT ) :: npts      ! Actual number of points in pdf
REAL, DIMENSION(*), INTENT( INOUT ) :: xdf       ! Numerical distribution - probability
REAL, DIMENSION(*), INTENT( INOUT ) :: cval      ! Numerical distribution - values

REAL    dm,xm,delta
LOGICAL addpt

REAL, EXTERNAL :: ProbExceed

delta = MIN( (1.0-rat)*xl,del )
addpt = npts < maxn .AND. ABS(xl-xr) > delta .AND. xl > pmin
IF( addpt )THEN
  dm = 0.5*(dl+dr)
  xm = ProbExceed( dm )
  DO WHILE( xm <= 0.0 )
    dm = 0.5*(dl+dm)
    xm = ProbExceed( dm )
  END DO
  npts = npts + 1
  cval(npts) = dm
  xdf(npts) = xm
  CALL checkrange( dl,xl,dm,xm,rat,del,maxn,pmin,npts,xdf,cval )
  CALL checkrange( dm,xm,dr,xr,rat,del,maxn,pmin,npts,xdf,cval )
END IF

RETURN
END
