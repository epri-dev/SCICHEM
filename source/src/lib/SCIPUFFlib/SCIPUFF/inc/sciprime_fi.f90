MODULE sciprime_fi

USE param_fd

SAVE

INTEGER, PARAMETER :: ISTRG = 132  !Length of Runstream Image Record
INTEGER, PARAMETER :: IFMAX = 40   !Max Number of Fields Per Runstream Record
INTEGER, PARAMETER :: IPN   = 2    !Number of Pathway IDs (Includes '**')
INTEGER, PARAMETER :: IKN   = 6    !Number of keywords

INTEGER, PARAMETER :: NSEC  = 36   !Number of Sectors for Building Dimensions
INTEGER, PARAMETER :: NST   = 6    !Number of stability classes
INTEGER, PARAMETER :: MXSRC = 200  !Max Number of Sources

REAL,    PARAMETER :: Umin = 0.01  ! Set minimum u to 0.01m/s to avoid divide by zero

LOGICAL  bline,infld,rural,urban
LOGICAL  ldbhr, primeprof
LOGICAL  capped, horiz, linwake

INTEGER  iline,ifc,ipnum,ippnum,kst,numwake,ierr

REAL     cmass_prm, WDSIN, WDCOS
REAL     dsbh,dsbw,dsbl,xadj,yadj
REAL     hstack,dstack,tstack,wstack,hseff,reff
REAL     ustack,ubldg,afv,dtemdz,pvel,uref
REAL     zi_rel, capfact
REAL     xplm,uplm,wplm,tplm,rplm,fbini
REAL     xplmold,uplmold,wplmold,tplmold,rplmold

CHARACTER      :: runst(istrg)
CHARACTER(2)   :: path,ppath
CHARACTER(8)   :: pkeywd,keywrd,keywd(IKN)
CHARACTER(40)  :: field(IFMAX)
CHARACTER(132) :: runst1

INTEGER, DIMENSION(IFMAX)      :: locb, loce
INTEGER, DIMENSION(IKN)        :: isstat

CHARACTER(12), DIMENSION(:),   ALLOCATABLE  :: srcid   !MXSRC
INTEGER,       DIMENSION(:,:), ALLOCATABLE  :: iwrk2   !MXSRC,IKN
REAL,          DIMENSION(:,:), ALLOCATABLE  :: adsbh,adsbw,adsbl,adsxadj,adsyadj !NSEC,MXSRC
LOGICAL,       DIMENSION(:),   ALLOCATABLE  :: isrural !MXSRC

REAL, DIMENSION(2) :: xrel_prm,yrel_prm,zrel_prm,frac_prm,sigy_prm,sigz_prm
REAL, DIMENSION(2) :: ky_prm,kz_prm,wmom_prm,buoy_prm

REAL, DIMENSION(:), ALLOCATABLE :: concPrime

END MODULE
