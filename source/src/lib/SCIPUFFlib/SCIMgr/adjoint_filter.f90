INTEGER FUNCTION AdjointReleaseFilterF( userID,nRel,relList,nMat,matList )

USE state_fd
USE release_fd
USE material_fd
USE SCIMgrState

IMPLICIT NONE

INTEGER,                                INTENT( IN    ) :: userID
INTEGER,                                INTENT( INOUT ) :: nRel
TYPE( releaseT  ),DIMENSION(*), TARGET, INTENT( INOUT ) :: relList
INTEGER,                        TARGET, INTENT( INOUT ) :: nMat
TYPE( materialT ),DIMENSION(*),         INTENT( INOUT ) :: matList

INTEGER currentState, irv

INTEGER, EXTERNAL  :: AdjointReleaseFilter

!==== Initialize

IF( SCIMgrCheckState( HS_IDLEBUSY ) )THEN !Not available during callbacks
  currentState = SCIMgrSetState(HS_BUSY)
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

AdjointReleaseFilterF = AdjointReleaseFilter( nRel,relList,nMat,matList )

irv = SCIMgrSetState( currentState )

CALL reset_messaging()


RETURN
END
!==============================================================================
!==============================================================================
!==============================================================================
INTEGER FUNCTION AdjointReleaseFilter( nRel,relList,nMat,matList )

!--- Filter adjoint releases - discarded releases have zero mass

USE release_fd
USE material_fd
USE SCIPresults_fd
USE SCIMgr_fi
USE error_fi
USE AdjointFilter_fi

IMPLICIT NONE

INTEGER,                        INTENT( INOUT ) :: nRel
TYPE( releaseT  ),DIMENSION(*), INTENT( INOUT ) :: relList
INTEGER,                        INTENT( INOUT ) :: nMat
TYPE( materialT ),DIMENSION(*), INTENT( INOUT ) :: matList

TYPE( sloc_str ), DIMENSION(:), ALLOCATABLE :: smp

TYPE( dat_ptr ), DIMENSION(:,:), ALLOCATABLE :: wrkh   ! pointers to hits
TYPE( dat_ptr ), DIMENSION(:,:), ALLOCATABLE :: wrkn   ! pointers to nulls
TYPE( dat_ptr ), DIMENSION(:,:), ALLOCATABLE :: rjth   ! pointers to hits which are outliers
TYPE( dat_ptr ), DIMENSION(:,:), ALLOCATABLE :: rjtn   ! pointers to nulls which are outliers
TYPE( dat_ptr ), DIMENSION(:,:), ALLOCATABLE :: rdch   ! pointers to reduced set of hits
TYPE( dat_ptr ), DIMENSION(:,:), ALLOCATABLE :: rdcn   ! pointers to reduced set of nulls

INTEGER ios, is, i, j, k, it, ir, ie, nhit, nthit, n95
INTEGER nbt, nbx, nby, nbc, nrt, nnull
REAL    tmin, tmax, xmin, xmax, ymin, ymax, cmin, cmax
REAL    xmean, ymean, xsig, ysig, ccut, tmean, cmean
REAL    dt, dc, csig, tsig, fac, bt, bx, by, bc
REAL    zt, zx, zy, zc, rat, rskip, ratNull
LOGICAL lHit, lNull

REAL, EXTERNAL :: Kernel

AdjointReleaseFilter = SCIPfailure

!--- Build list of release ID's

ALLOCATE( relp(nRel),STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eMessage = 'Error allocating relp release array'
  WRITE(eInform,*) 'Requested dimension = ',nRel
  eRoutine = 'AdjointReleaseFilter'
  GOTO 9999
END IF

CALL BuildRelID( nRel,relList,nMat,matList )

!--- Set up working arrays for data reduction

ALLOCATE( smp(nsmp),STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eMessage = 'Error allocating sampler array'
  WRITE(eInform,*) 'Requested dimension = ',nsmp
  eRoutine = 'AdjointReleaseFilter'
  GOTO 9999
END IF

DO is = 1,nsmp
  NULLIFY( smp(is)%nid )
  NULLIFY( smp(is)%edge )
END DO

xmin =  HUGE(1.0)
xmax = -HUGE(1.0)
ymin =  HUGE(1.0)
ymax = -HUGE(1.0)

DO is = 1,nsmp

  j = 1
  DO WHILE( indr(j,is) < 0 )
    j = j + 1
  END DO

  i = indr(j,is)

  smp(is)%x = SNGL(relList(i)%xRel)
  smp(is)%y = SNGL(relList(i)%yRel)

  xmin = MIN(xmin,smp(is)%x)
  xmax = MAX(xmax,smp(is)%x)
  ymin = MIN(ymin,smp(is)%y)
  ymax = MAX(ymax,smp(is)%y)

  smp(is)%id    = is
  smp(is)%nbr   = 0
  smp(is)%nedge = nsmp

  ALLOCATE( smp(is)%nid(nsmp),smp(is)%edge(nsmp),STAT=ios )
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eMessage = 'Error allocating smp(is)%nid array'
    WRITE(eInform,*) 'Requested dimension = ',nsmp
    eRoutine = 'AdjointReleaseFilter'
    GOTO 9999
  END IF

END DO

xmean = 0.
ymean = 0.
DO is = 1,nsmp
  xmean = xmean + smp(is)%x
  ymean = ymean + smp(is)%y
END DO

dx = xmax - xmin
IF( dx == 0.0 )THEN
  IF( xmax > 0.0 )THEN
    dx = 1.E-2*xmax
  ELSE
    dx = 1.E-2
  END IF
END IF

dy = ymax - ymin
IF( dy == 0.0 )THEN
  IF( ymax > 0.0 )THEN
    dy = 1.E-2*ymax
  ELSE
    dy = 1.E-2
  END IF
END IF

xmean = (xmean/FLOAT(nsmp) - xmin)/dx
ymean = (ymean/FLOAT(nsmp) - ymin)/dy

xsig = 0.0
ysig = 0.0
DO is = 1,nsmp
  smp(is)%xd = (smp(is)%x - xmin)/dx
  smp(is)%yd = (smp(is)%y - ymin)/dy
  xsig = xsig + (smp(is)%xd - xmean)**2
  ysig = ysig + (smp(is)%yd - ymean)**2
END DO

xsig = MAX(1.E-20,xsig/FLOAT(nsmp))
ysig = MAX(1.E-20,ysig/FLOAT(nsmp))

IF( nsmp >= 3 )THEN
  CALL triangulate( smp )
  IF( nError /= NO_ERROR )GOTO 9999
END IF

tmin = tim(1)%t
tmax = tim(nt)%t

ALLOCATE( sall(nt,nsmp),STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eMessage = 'Error allocating all cnc arrays'
  WRITE(eInform,*) 'Requested dimensions = ',nt,nsmp
  eRoutine = 'AdjointReleaseFilter'
  GOTO 9999
END IF

cmax = -9999.
cmin = HUGE(cmin)
DO it = 1,nt
  DO is = 1,nsmp
    sall(it,is)%it = it
    sall(it,is)%id = smp(is)%id
    sall(it,is)%c  = 0.
    j = indr(it,is)
    IF( j > 0 )THEN
      IF( relp(j)%icls /= 0 )THEN
        sall(it,is)%c = 1./TRANSFER(relList(j)%relData%padding(2),cmax)
        IF( sall(it,is)%c > cmax )cmax = sall(it,is)%c
        IF( sall(it,is)%c < cmin )cmin = sall(it,is)%c
      END IF
    END IF
  END DO
END DO

ccut = cmin/cmax

ALLOCATE( wrkh(nt,0:nsmp),wrkn(nt,0:nsmp),wrka(nt,nsmp),STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eMessage = 'Error allocating wrk arrays'
  WRITE(eInform,*) 'Requested dimensions = ',nt,nsmp
  eRoutine = 'AdjointReleaseFilter'
  GOTO 9999
END IF

ALLOCATE( rjth(nt,0:nsmp),rjtn(nt,0:nsmp),STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eMessage = 'Error allocating rjt arrays'
  WRITE(eInform,*) 'Requested dimensions = ',nt,nsmp
  eRoutine = 'AdjointReleaseFilter'
  GOTO 9999
END IF

ALLOCATE( rdch(nt,0:nsmp),rdcn(nt,0:nsmp),STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eMessage = 'Error allocating rdc arrays'
  WRITE(eInform,*) 'Requested dimensions = ',nt,nsmp
  eRoutine = 'AdjointReleaseFilter'
  GOTO 9999
END IF

DO it = 1,nt
  NULLIFY( wrkh(it,0)%p )
  NULLIFY( rjth(it,0)%p )
  NULLIFY( wrkn(it,0)%p )
  NULLIFY( rjtn(it,0)%p )
  NULLIFY( rdch(it,0)%p )
  NULLIFY( rdcn(it,0)%p )
  DO is = 1,nsmp
    NULLIFY( wrka(it,is)%ker )
  END DO
END DO

nhit  = 0
itend = nt
itsta = 1

DO it = 1,nt

  ALLOCATE( wrkh(it,0)%p,rjth(it,0)%p,STAT=ios )
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eMessage = 'Error allocating wrk array'
    WRITE(eInform,*) 'Requested dimensions = ',it
    eRoutine = 'AdjointReleaseFilter'
    GOTO 9999
  END IF

  wrkh(it,0)%p%id = 0   ! Num. of hits at it
  rjth(it,0)%p%id = 0   ! Num. of rejected hits at it

  ALLOCATE( wrkn(it,0)%p,rjtn(it,0)%p,STAT=ios )
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eMessage = 'Error allocating wrk array'
    WRITE(eInform,*) 'Requested dimensions = ',it
    eRoutine = 'AdjointReleaseFilter'
    GOTO 9999
  END IF

  wrkn(it,0)%p%id = 0   ! Num. of nulls at it
  rjtn(it,0)%p%id = 0   ! Num. of rejected nulls at it

  DO is = 1,nsmp
    wrka(it,is)%it = it
    wrka(it,is)%id = is
    wrka(it,is)%c  = LOG(MAX(ccut,sall(it,is)%c/cmax))
    i = indr(it,is)
    IF( i > 0 )THEN
      IF( relp(i)%icls > 0 )THEN
        k = wrkh(it,0)%p%id + 1
        wrkh(it,0)%p%id = k
        wrkh(it,k)%p    => wrka(it,is)
        wrkh(it,k)%p%iu = uHIT
      ELSE
        wrka(it,is)%iu = uNULL
      END IF
    END IF
  END DO
  nhit = nhit + wrkh(it,0)%p%id
END DO

DO it = 1,nt
  IF( wrkh(it,0)%p%id > 0 )THEN
    itsta = MAX(1,it-1) ! Last time with all nulls before any hit unless it is 1
    EXIT
  END IF
END DO

DO it = nt,1,-1
  IF( wrkh(it,0)%p%id > 0 )THEN
    itend = MIN(nt,it+1) ! First time with all nulls after any hit unless it is nt
    EXIT
  END IF
END DO

!--- Process the sensors, if required

IF( nhit_target == 9999 )THEN
  ! No filtering
  AdjointReleaseFilter = SCIPsuccess
  RETURN
END IF

nsrc  = nhit_target  !--- target no. of hits

lHit  = nhit > nsrc
lNull = nRel > 2*nsrc .OR. lHit

nthit = itend - itsta + 1
ntsmp = nthit*nsmp

tmin  = tim(itsta)%t
tmax  = tim(itend)%t
tmean = 0.
cmin  = LOG(cmin/cmax)
cmax  = 0.
cmean = 0.

nedge = ntsmp*(ntsmp+1)/2

DO it = itsta,itend
  tmean = tmean + tim(it)%t
  DO is = 1,nsmp
    cmean = cmean + wrka(it,is)%c
  END DO
END DO

dt    = tmax - tmin
tmean = (tmean/FLOAT(nthit) - tmin)/dt
dc    = cmax - cmin
cmean = (cmean/FLOAT(ntsmp) - cmin)/dc

tsig = 0.0
csig = 0.0

DO it = itsta,itend
  tim(it)%td = (tim(it)%t - tmin)/dt
  tsig = tsig + (tim(it)%td - tmean)**2
  DO is = 1,nsmp
    wrka(it,is)%cd = (wrka(it,is)%c - cmin)/dc
    csig  = csig + (wrka(it,is)%cd - cmean)**2
  END DO
  tim(it)%dt(:) = 0.0
END DO

tsig = MAX(1.E-20,tsig/FLOAT(nthit))
csig = MAX(1.E-20,csig/FLOAT(ntsmp))

DO it = itsta,itend-1
  DO is = it+1,itend
    tim(it)%dt(is) = ABS(tim(it)%td - tim(is)%td)
    tim(is)%dt(it) = tim(it)%dt(is)
  END DO
END DO

nrd = MAX(2,INT(nsrc*0.3))
n95 = MAX(nrd,INT(nhit*0.95))
fac = SQRT(5.) * FLOAT(n95)**(-1./7.)
bt  = SQRT(tsig)*fac
bx  = SQRT(xsig)*fac
by  = SQRT(ysig)*fac
bc  = SQRT(csig)*fac

ALLOCATE( fca(ntsmp),fch(ntsmp),fs(ntsmp),STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eMessage = 'Error allocating fs array'
  WRITE(eInform,*) 'Requested dimensions = ',ntsmp
  eRoutine = 'AdjointReleaseFilter'
  GOTO 9999
END IF

nbt = INT(1./bt) + 1
nbx = INT(1./bx) + 1
nby = INT(1./by) + 1
nbc = INT(1./bc) + 1
dt  = 1./FLOAT(nbt)
dx  = 1./FLOAT(nbx)
dy  = 1./FLOAT(nby)
dc  = 1./FLOAT(nbc)

fac = Kernel( 0.,0.,0.,bt,bx,by,0.,bc )

ALLOCATE( ker(nedge),STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eMessage = 'Error allocating ker array'
  WRITE(eInform,*) 'Requested dimensions = ',nedge
  eRoutine = 'AdjointReleaseFilter'
  GOTO 9999
END IF

DO i = 1,ntsmp
  it = (i-1)/nsmp + itsta
  is = i - (it-itsta)*nsmp
  ALLOCATE( wrka(it,is)%ker(itsta:itend,nsmp),STAT=ios )
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eMessage = 'Error allocating wrka(it,is)%ker array'
    WRITE(eInform,*) 'Requested dimensions = ',it,is
    eRoutine = 'AdjointReleaseFilter'
    GOTO 9999
  END IF
END DO

DO i = 1,ntsmp-1

  it = (i-1)/nsmp + itsta
  is = i - (it-itsta)*nsmp

  DO j = i+1,ntsmp
    ir = (j-1)/nsmp + itsta
    k  = j - (ir-itsta)*nsmp
    zt = tim(it)%dt(ir)
    zx = smp(is)%edge(k)%dx
    zy = smp(is)%edge(k)%dy
    zc = wrka(it,is)%c - wrka(ir,k)%c
    p1 = MIN(i,j)
    p2 = MAX(i,j)
    ie = (p1-1)*(2*ntsmp-p1)/2 + p2
    wrka(it,is)%ker(ir,k)%p => ker(ie)
    wrka(ir,k)%ker(it,is)%p => ker(ie)
    wrka(it,is)%ker(ir,k)%p%dc = zc
    wrka(it,is)%ker(ir,k)%p%ks = Kernel(zt,zx,zy,bt,bx,by,0.,bc)
    wrka(it,is)%ker(ir,k)%p%kc = Kernel(zt,zx,zy,bt,bx,by,zc,bc)
  END DO

  ie = (i-1)*(2*ntsmp-i)/2 + i
  wrka(it,is)%ker(it,is)%p => ker(ie)
  wrka(it,is)%ker(it,is)%p%ks = fac
  wrka(it,is)%ker(it,is)%p%kc = fac
  wrka(it,is)%ker(it,is)%p%dc = 0.

END DO

wrka(itend,nsmp)%ker(itend,nsmp)%p => ker(ie)
wrka(itend,nsmp)%ker(itend,nsmp)%p%ks = fac
wrka(itend,nsmp)%ker(itend,nsmp)%p%kc = fac
wrka(itend,nsmp)%ker(itend,nsmp)%p%dc = 0.

fs(:)  = 0.
fca(:) = 0.
fch(:) = 0.

DO i = 1,ntsmp

  it = (i-1)/nsmp + itsta
  is = i - (it-itsta)*nsmp

  DO j = 1,ntsmp
    ir = (j-1)/nsmp + itsta
    k  = j - (ir-itsta)*nsmp
    fca(i) = fca(i) + wrka(it,is)%ker(ir,k)%p%ks
    IF( wrka(ir,k)%iu == uHIT )fch(i) = fch(i) + wrka(it,is)%ker(ir,k)%p%ks
  END DO

  fch(i) = fch(i)/fca(i)

END DO

!--- Remove outliers from set of Hits

IF( lHit )THEN

  UseMax = .FALSE.

  CALL RemoveOut( wrkh,rjth,nt,nsmp )
  IF( nError /= NO_ERROR )GOTO 9999

  rat  = MIN(1.,FLOAT(nsrc)/FLOAT(nhit))
  nsrc = INT(MAX(1.,rat*nhit))

  IF( nthit > 3 )THEN
    nrt = 3 + INT(FLOAT(nthit-3)*rat)
    nrt = MIN(nthit,nrt)
  ELSE
    nrt = MAX(1,nthit)
  END IF

ELSE

  rat = 1.

END IF

!--- Find immediate neighbours for all valid hits

DO it = itsta,itend

  DO is = 1,wrkh(it,0)%p%id
    p1 = wrkh(it,is)%p%id
    DO i = 1,smp(p1)%nbr
      p2 = smp(p1)%nid(i)
      IF( wrka(it,p2)%iu == uNull )wrka(it,p2)%iu = uNBR
    END DO
  END DO

  wrkn(it,0)%p%id = 0   ! Num. of null neighbours at it

  DO is = 1,nsmp
    IF( wrka(it,is)%iu == uNBR )THEN
      k = wrkn(it,0)%p%id + 1
      wrkn(it,0)%p%id = k
      wrkn(it,k)%p    => wrka(it,is)
    END IF
  END DO

END DO

ALLOCATE( irt(nthit),STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eMessage = 'Error allocating irt array'
  WRITE(eInform,*) 'Requested dimensions = ',nthit
  eRoutine = 'AdjointReleaseFilter'
  GOTO 9999
END IF

IF( lHit )THEN

  IF( wrkh(itsta,0)%p%id > 0 )THEN     ! Adjust irt(1) to be the first time with hits
    irt(1) = itsta
  ELSE
    irt(1) = itsta + 1
  END IF

  IF( nrt > 1 )THEN

    IF( wrkh(itend,0)%p%id > 0 )THEN   ! Adjust irt(2) to be the last time with hits
      irt(2) = itend
    ELSE
      irt(2) = itend - 1
    END IF

    AddRt:DO

      rskip = FLOAT(irt(2)-irt(1))/FLOAT(nrt-1)
      ir = 2
      is = irt(1)
      ie = irt(2)
      j  = 1
      IF( nrt > 2 )THEN
        irt(3:nrt) = 0
      ELSE
        EXIT
      END IF

      DO

        k = MAX(1,NINT(rskip*FLOAT(j)))

!--- Add to reduced time list from itsta

        is = irt(1) + k
        IF( is >= ie )THEN
          IF( k == 1 )THEN
            EXIT AddRt
          ELSE
            EXIT
          END IF
        END IF
        i = 0
        DO
          i = i + 1
          IF( i > ir )EXIT
          IF( irt(i) == is )EXIT
        END DO
        IF( i > ir )THEN
          ir = ir + 1
          irt(ir) = is
        END IF

!--- Add to reduced time list from itend

        ie = irt(2) - k
        IF( ie <= is )THEN
          IF( k == 1 )THEN
            EXIT AddRt
          ELSE
            EXIT
          END IF
        END IF

        i = 0
        DO
          i = i + 1
          IF( i > ir )EXIT
          IF( irt(i) == ie )EXIT
        END DO
        IF( i > ir )THEN
          ir = ir + 1
          irt(ir) = ie
        END IF
        j = j + 1
      END DO

      n95 = 0                ! Approx. number of reduced sources (local)
      DO i = 1,ir
        it  = irt(i)
        IF( wrkh(it,0)%p%id > 3 )THEN
          n95 = n95 + 3 + INT((wrkh(it,0)%p%id-3)*rat)
        ELSE IF( wrkh(it,0)%p%id > 0 )THEN
          n95 = n95 + wrkh(it,0)%p%id
        END IF
      END DO

      IF( n95 >= nsrc .OR. ir >= nthit )THEN
        EXIT
      ELSE
        nrt = nrt + 1    ! Nsrc or nthit not reached. Reduce rskip
      END IF

      IF( nrt > irt(2)-irt(1)+1 )EXIT

    END DO AddRt

    IF( ir < nrt )nrt = ir

  END IF

ELSE

  DO it = itsta,itend
    irt(it-itsta+1) = it
  END DO

  nrt = itend - itsta + 1

END IF

!--- Remove outliers from set of null neighbour

IF( lNull )THEN

  UseMax = .TRUE.

  CALL RemoveOut( wrkn,rjtn,nt,nsmp )
  IF( nError /= NO_ERROR )GOTO 9999

END IF

IF( lHit )THEN

  DO it = 1,nt
    ALLOCATE( rdch(it,0)%p,STAT=ios )
    IF( ios /= 0 )THEN
      nError   = UK_ERROR
      eMessage = 'Error allocating rdch array'
      WRITE(eInform,*) 'Requested dimensions = ',it
      eRoutine = 'AdjointReleaseFilter'
      GOTO 9999
    END IF
    rdch(it,0)%p%id = 0   ! Num. of reduced hits at it
  END DO

  DO it = itsta,itend
    DO is = 1,nsmp
      i = indr(it,is)
      IF( i < 1 )CYCLE
      k  = wrka(it,is)%id
      ir = (it-itsta)*nsmp + k
      IF( it < itsta .OR. it > itend )THEN
        ccut = 0.
      ELSE
        ccut = fch(ir)
      END IF
    END DO
  END DO

!--- Find reduced set of hits based on rat

  UseMax = .TRUE.

  CALL ReduceWrk( nrt,rat,wrkh,rdch,nt,nsmp )
  IF( nError /= NO_ERROR )GOTO 9999

ELSE

  DO it = 1,nt
    rdch(it,0)%p => wrkh(it,0)%p
    DO j = 1,wrkh(it,0)%p%id
      rdch(it,j)%p => wrkh(it,j)%p
    END DO
  END DO

END IF

IF( lNull )THEN

  DO it = 1,nt
    ALLOCATE( rdcn(it,0)%p,STAT=ios )
    IF( ios /= 0 )THEN
      nError   = UK_ERROR
      eMessage = 'Error allocating rdcn array'
      eRoutine = 'AdjointReleaseFilter'
      GOTO 9999
    END IF
    rdcn(it,0)%p%id = 0   ! Num. of reduced hits at it
  END DO

!--- Find reduced set of nulls based on rat

  UseMax  = .TRUE.
  ratNull = -rat*(3.0 - 0.01*rat*FLOAT(nhit))

  CALL ReduceWrk( nrt,ratNull,wrkn,rdcn,nt,nsmp )
  IF( nError /= NO_ERROR )GOTO 9999

ELSE

  DO it = 1,nt
    rdcn(it,0)%p => wrkn(it,0)%p
    DO j = 1,wrkn(it,0)%p%id
      rdcn(it,j)%p => wrkn(it,j)%p
    END DO
  END DO

END IF

nhit  = 0
nnull = 0
DO it = itsta,itend
  nhit  = nhit  + rdch(it,0)%p%id
  nnull = nnull + rdcn(it,0)%p%id
END DO

!--- Add all hits and nulls in reduced set as nulls in the first and last time step

DO it = itsta,itend

  DO is = 1,rdch(it,0)%p%id
    k = rdch(it,is)%p%id
    j = indr(itsta,k)
    IF( j > 0 )CALL add2relr( relp(j),nhit,nnull )
    j = indr(itend,k)
    IF( j > 0 )CALL add2relr( relp(j),nhit,nnull )
    IF( it > itsta )THEN
      j = indr(it-1,k)
      IF( j > 0 )CALL add2relr( relp(j),nhit,nnull )
    END IF
    IF( it < itend )THEN
      j = indr(it+1,k)
      IF( j > 0 )CALL add2relr( relp(j),nhit,nnull )
    END IF
  END DO

  DO is = 1,rdcn(it,0)%p%id
    k = rdcn(it,is)%p%id
    j = indr(itsta,k)
    IF( j > 0 )CALL add2relr( relp(j),nhit,nnull )
    j = indr(itend,k)
    IF( j > 0 )CALL add2relr( relp(j),nhit,nnull )
    IF( it > itsta )THEN
      j = indr(it-1,k)
      IF( j > 0 )CALL add2relr( relp(j),nhit,nnull )
    END IF
    IF( it < itend )THEN
      j = indr(it+1,k)
      IF( j > 0 )CALL add2relr( relp(j),nhit,nnull )
    END IF
  END DO

END DO

!--- Reset reduced lists

i = 0
DO j = 1,nRel
  IF( relp(j)%linc )THEN
    i = i  + 1
    relList(i) = relList(j)
    relp(i)    = relp(j)
  END IF
END DO

nRel = i

i = 0
MLoop: DO j = 1,nMat
  DO k = 1,nRel
    IF( relp(k)%imat == j )THEN
      i = i + 1
      matList(i) = matList(j)
      CYCLE MLoop
    END IF
  END DO
END DO Mloop

nMat = i

AdjointReleaseFilter = SCIPsuccess

9999 CONTINUE

IF( ALLOCATED(sall) )DEALLOCATE( sall,STAT=ios )
IF( ALLOCATED(indr) )DEALLOCATE( indr,STAT=ios )
IF( ALLOCATED(relp) )DEALLOCATE( relp,STAT=ios )

IF( ALLOCATED(smp) )THEN
  DO is = 1,nsmp
    IF( ASSOCIATED(smp(is)%nid ) )DEALLOCATE( smp(is)%nid, STAT=ios )
    IF( ASSOCIATED(smp(is)%edge) )DEALLOCATE( smp(is)%edge,STAT=ios )
  END DO
  DEALLOCATE( smp,STAT=ios )
END IF

IF( ALLOCATED(tim) )THEN
  DO it = 1,nt
    IF( ASSOCIATED(tim(it)%dt) )DEALLOCATE( tim(it)%dt,STAT=ios )
  END DO
  DEALLOCATE( tim,STAT=ios )
END IF

IF( ALLOCATED(wrka) )THEN
  DO i = 1,ntsmp
    it = (i-1)/nsmp + itsta
    is = i - (it-itsta)*nsmp
    IF( ASSOCIATED(wrka(it,is)%ker) )DEALLOCATE( wrka(it,is)%ker,STAT=ios )
  END DO
  DEALLOCATE( wrka,STAT=ios )
END IF

IF( ALLOCATED(wrkh) )THEN
  DO it = 1,nt
    IF( ASSOCIATED(wrkh(it,0)%p) )DEALLOCATE( wrkh(it,0)%p,STAT=ios )
  END DO
  DEALLOCATE( wrkh,STAT=ios )
END IF

IF( ALLOCATED(wrkn) )THEN
  DO it = 1,nt
    IF( ASSOCIATED(wrkn(it,0)%p) )DEALLOCATE( wrkn(it,0)%p,STAT=ios )
  END DO
  DEALLOCATE( wrkn,STAT=ios )
END IF

IF( ALLOCATED(rjtn) )THEN
  DO it = 1,nt
    IF( ASSOCIATED(rjtn(it,0)%p) )DEALLOCATE( rjtn(it,0)%p,STAT=ios )
  END DO
  DEALLOCATE( rjtn,STAT=ios )
END IF

IF( ALLOCATED(rjth) )THEN
  DO it = 1,nt
    IF( ASSOCIATED(rjth(it,0)%p) )DEALLOCATE( rjth(it,0)%p,STAT=ios )
  END DO
  DEALLOCATE( rjth,STAT=ios )
END IF

IF( ALLOCATED(rdch) )THEN
  IF( lHit )THEN
    DO it = 1,nt
      IF( ASSOCIATED(rdch(it,0)%p) )DEALLOCATE( rdch(it,0)%p,STAT=ios )
    END DO
  END IF
  DEALLOCATE( rdch,STAT=ios )
END IF

IF( ALLOCATED(rdcn) )THEN
  IF( lNull )THEN
    DO it = 1,nt
      IF( ASSOCIATED(rdcn(it,0)%p) )DEALLOCATE( rdcn(it,0)%p,STAT=ios )
    END DO
  END IF
  DEALLOCATE( rdcn,STAT=ios )
END IF

IF( ALLOCATED(fca) )DEALLOCATE( fca,STAT=ios )
IF( ALLOCATED(fch) )DEALLOCATE( fch,STAT=ios )
IF( ALLOCATED(fs)  )DEALLOCATE( fs, STAT=ios )
IF( ALLOCATED(irt) )DEALLOCATE( irt,STAT=ios )
IF( ALLOCATED(ker) )DEALLOCATE( ker,STAT=ios )

RETURN
END

!==============================================================================

SUBROUTINE add2relr( rel,nhit,nnull )

USE AdjointFilter_fi

IMPLICIT NONE

INTEGER          :: nhit, nnull
TYPE( releaseP ) :: rel

IF( .NOT.rel%linc )THEN     ! Not in reduced list
  rel%linc = .TRUE.
  IF( rel%icls == 0 )THEN
    nnull = nnull + 1
  ELSE
    nhit  = nhit  + 1
  END IF
END IF

RETURN
END

!==============================================================================

SUBROUTINE BuildRelID( nRel,relList,nMat,matList )
!DIR$ OPTIMIZE:0

USE SCIMgr_fd
USE error_fi
USE AdjointFilter_fi

IMPLICIT NONE

INTEGER,                        INTENT( IN ) :: nRel
TYPE( releaseT  ),DIMENSION(*), INTENT( IN ) :: relList
INTEGER,                        INTENT( IN ) :: nMat
TYPE( materialT ),DIMENSION(*), INTENT( IN ) :: matList

INTEGER i, j, ios
LOGICAL newID

!---- Count time list first

nt = 1

DO i = 2,nRel
  IF( relList(i)%tRel /= relList(i-1)%tRel )nt = nt + 1
END DO

!---- Allocate time list

ALLOCATE( tim(nt),STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eMessage = 'Error allocating time list array'
  WRITE(eInform,*) 'Requested dimension = ',nt
  eRoutine = 'BuildRelID'
  GOTO 9999
END IF

DO i = 1,nt
  NULLIFY( tim(i)%dt )
END DO

DO i = 1,nt
  ALLOCATE( tim(i)%dt(nt),STAT=ios )
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eMessage = 'Error allocating time list array'
    WRITE(eInform,*) 'Requested dimension = ',nt
    eRoutine = 'BuildRelID'
    GOTO 9999
  END IF
END DO

!---- Fill time list

nt         = 1
tim(1)%t   = relList(1)%tRel*3600.
relp(1)%it = 1

DO i = 2,nRel
  IF( relList(i)%tRel /= relList(i-1)%tRel )THEN
    nt = nt + 1
    tim(nt)%t = relList(i)%tRel*3600.
  END IF
  relp(i)%it = nt
END DO

!--- Set location ID's

nsmp = 1
relp(1)%is = 1

DO i = 2,nRel
  newID = .TRUE.
  DO j = 1,i-1
    IF( relList(i)%xRel == relList(j)%xRel .AND. relList(i)%yRel == relList(j)%yRel )THEN
      relp(i)%is = relp(j)%is
      newID = .FALSE.
      EXIT
    END IF
  END DO
  IF( newID )THEN
    nsmp = nsmp + 1
    relp(i)%is = nsmp
  END IF
END DO

!--- Set index array

ALLOCATE( indr(nt,nsmp),STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eMessage = 'Error allocating index array'
  WRITE(eInform,*) 'Requested dimension = ',nt,nsmp
  eRoutine = 'BuildRelID'
  GOTO 9999
END IF

DO i = 1,nt
  DO j = 1,nsmp
    indr(i,j) = -999
  END DO
END DO

DO i = 1,nRel
  indr(relp(i)%it,relp(i)%is) = i
  DO j = 1,nMat
    IF( TRIM(relList(i)%material) == TRIM(matList(j)%name) )THEN
      relp(i)%imat = j
      IF( BTEST(matList(j)%type,HMB_NULLSENSOR) )THEN
        relp(i)%icls = 0
      ELSE IF( BTEST(matList(j)%type,HMB_SATSENSOR) )THEN
        relp(i)%icls = 2
      ELSE
        relp(i)%icls = 1
      END IF
    END IF
  END DO
  relp(i)%linc = .FALSE.
END DO

9999 CONTINUE

RETURN
END

!========================================================================

SUBROUTINE triangulate( smp )

! Triangulation subroutine
! Takes as input nsmp vertices in smp structure
! Returned is a list of NTRI triangular faces in the arrays
! V(). These triangles are arranged in clockwise order.

USE AdjointFilter_fi
USE error_fi

IMPLICIT NONE

TYPE( sloc_str ), DIMENSION(nsmp), INTENT( INOUT ) :: smp

INTEGER, DIMENSION(:), ALLOCATABLE :: lend
INTEGER, DIMENSION(:), ALLOCATABLE :: next
REAL,    DIMENSION(:), ALLOCATABLE :: xs, ys

INTEGER indf, indl
INTEGER i, j, k, ios

nedge = smp(1)%nedge

DO i = 1,nedge

  IF( ASSOCIATED(smp(i)%edge) )DEALLOCATE( smp(i)%edge,STAT=ios )
  IF( ASSOCIATED(smp(i)%nid)  )DEALLOCATE( smp(i)%nid, STAT=ios )
  ALLOCATE( smp(i)%edge(nedge),smp(i)%nid(nedge),STAT=ios )
  IF( ios /= 0 )THEN
    nError = UK_ERROR
    eMessage = 'Error allocating smp(i)%edge'
    WRITE(eInform,*) 'Requested dimension = ',nedge
    eRoutine = 'triangulate'
    GOTO 9999
  END IF
  DO k = 1,nedge
    smp(i)%edge(k)%dx  = 1.E20
    smp(i)%edge(k)%dy  = 1.E20
    smp(i)%edge(k)%con = 0
  END DO

  smp(i)%edge(i)%con = 1
  smp(i)%edge(i)%dx  = 0.
  smp(i)%edge(i)%dy  = 0.

END DO

IF( nedge >= 3 )THEN

  ALLOCATE( xs(nedge),ys(nedge),STAT=ios )
  IF( ios /= 0 )THEN
    nError = UK_ERROR
    eMessage = 'Error allocating xs'
    WRITE(eInform,*) 'Requested dimension = ',nedge
    eRoutine = 'triangulate'
    GOTO 9999
  END IF

  DO i = 1,nedge
    xs(i) = smp(i)%x
    ys(i) = smp(i)%y
  END DO

  ntri = 6*nedge - 9

  ALLOCATE( lend(nedge),next(ntri),STAT=ios )
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eMessage = 'Error allocating edge list'
    WRITE(eInform,*) 'Requested dimension = ',nedge
    eRoutine = 'triangulate'
    GOTO 9999
  END IF

  CALL trmesh( nedge,xs,ys,next,lend,ios )
  IF( ios == 2 )THEN ! COLINEAR
    IF ( ABS(xs(1)-xs(2)) < dx*1.E-6 .AND. ABS(xs(1)-xs(3)) < dx*1.E-6  )THEN
      xs(1) = xs(1) + dx*1.E-3
    ELSE
      ys(1) = ys(1) + dy*1.E-3
    END IF
    CALL trmesh( nedge,xs,ys,next,lend,ios )
  END IF
  IF( ios /= 0 )THEN
    WRITE(eMessage,*)'Error in trmesh with ier =',ios
    nError   = UK_ERROR
    eMessage = 'Error in trmesh'
    WRITE(eInform,*) 'Error = ',ios
    eRoutine = 'triangulate'
    GOTO 9999
  END IF

  indf = 1
  DO i = 1,nedge
    indl = lend(i)
    DO j = indf,indl
      k  = next(j)
      IF( k /= 0 )smp(i)%edge(k)%con  = 1
    END DO
    indf = indl + 1
    smp(i)%nbr = 0
  END DO

  DO i = 1,nedge-1
    DO k = i+1,nedge
      smp(i)%edge(k)%dx = ABS(smp(i)%xd-smp(k)%xd)
      smp(i)%edge(k)%dy = ABS(smp(i)%yd-smp(k)%yd)
      IF( smp(i)%edge(k)%con == 1 )THEN
        smp(i)%nbr = smp(i)%nbr + 1
        smp(i)%nid(smp(i)%nbr) = smp(k)%id
        smp(k)%nbr = smp(k)%nbr + 1
        smp(k)%nid(smp(k)%nbr) = smp(i)%id
      END IF
      smp(k)%edge(i) = smp(i)%edge(k)
    END DO
  END DO

END IF

9999 CONTINUE

IF( ALLOCATED(xs)   )DEALLOCATE( xs,  STAT=ios )
IF( ALLOCATED(ys)   )DEALLOCATE( ys,  STAT=ios )
IF( ALLOCATED(lend) )DEALLOCATE( lend,STAT=ios )
IF( ALLOCATED(next) )DEALLOCATE( next,STAT=ios )

RETURN
END

!========================================================================

REAL FUNCTION Kernel( t,x,y,bt,bx,by,c,bc )

IMPLICIT NONE

REAL, INTENT( IN ) :: t, x, y, bt, bx, by
REAL, INTENT( IN ) :: c, bc

INTEGER, PARAMETER :: ND = 4

REAL rt, rx, ry, rc

rt = t/bt
rx = x/bx
ry = y/by
rc = c/bc

IF( ABS(rt) > 1. .OR. ABS(rx) > 1. .OR. ABS(ry) > 1. .OR. ABS(rc) > 1. )THEN
  Kernel = 0.
ELSE
  Kernel = (0.75**ND)*(1.- rt*rt)*(1.- rx*rx)*(1.- ry*ry)*(1.- rc*rc)
END IF

RETURN
END

!========================================================================

SUBROUTINE RemoveOut( wrk,rjt,n1,n2 )

! Remove outliers from wrk(working) and add to rjt(reject) list

USE AdjointFilter_fi
USE error_fi

IMPLICIT NONE

INTEGER,                              INTENT( IN )    :: n1, n2
TYPE ( dat_ptr ), DIMENSION(n1,0:n2), INTENT( INOUT ) :: wrk   ! pointers to hits/nulls
TYPE ( dat_ptr ), DIMENSION(n1,0:n2), INTENT( INOUT ) :: rjt   ! pointers to hits/nulls which are outliers

REAL, DIMENSION(:), ALLOCATABLE :: fchm, fchs

REAL    rsign, fsigl, fsigh
INTEGER ios, it, is, ir, i, j, k

IF( UseMax )THEN
  rsign = 1.5     ! Remove points with kernel density >(mean+0.7*sigma) or < (mean-1.7*sigma)
ELSE
  rsign = 1.0     ! Remove points with kernel density <(mean+1*sigma)
END IF

ALLOCATE( fchm(nt),fchs(nt),STAT=ios )
IF( ios /= 0 )THEN
  nError = UK_ERROR
  eMessage = 'Error allocating fchm array'
  WRITE(eInform,*)'Requested size = ',nt
  eRoutine = 'RemoveOut'
  GOTO 9999
END IF

fchm(:) = 0.
fchs(:) = 0.

DO it = itsta,itend
  IF( wrk(it,0)%p%id == 0 )CYCLE
  DO is = 1,wrk(it,0)%p%id
    ir       = (it-itsta)*nsmp + wrk(it,is)%p%id
    fchm(it) = fchm(it) + fch(ir)
  END DO
  fchm(it) = fchm(it)/FLOAT(wrk(it,0)%p%id)
  DO is = 1,wrk(it,0)%p%id
    ir       = (it-itsta)*nsmp + wrk(it,is)%p%id
    fchs(it) = fchs(it) + (fch(ir)-fchm(it))**2
  END DO
  fchs(it) = SQRT(fchs(it)/FLOAT(wrk(it,0)%p%id))
END DO

DO it = itsta,itend
  IF( wrk(it,0)%p%id == 0 )CYCLE
  fsigh = fchm(it)+ (rsign-1.)*fchs(it)
  fsigl = fchm(it)- rsign*fchs(it)
  DO is = 1,wrk(it,0)%p%id
    i = wrk(it,is)%p%id + (it-itsta)*nsmp
    IF( UseMax )THEN
      IF( fch(i) > fsigh .OR. fch(i) < fsigl )wrk(it,is)%p%iu = -wrk(it,is)%p%iu
    ELSE
      IF( fch(i) < fsigl )wrk(it,is)%p%iu = -wrk(it,is)%p%iu
    END IF
  END DO
END DO

DO it = itsta,itend

  rjt(it,0)%p%id = 0
  IF( wrk(it,0)%p%id == 0 )CYCLE

  nrm = 0
  IF( ALLOCATED(indx) )DEALLOCATE( indx,STAT=ios )
  IF( ios == 0 )ALLOCATE( indx(wrk(it,0)%p%id),STAT=ios )
  IF( ios /= 0 )THEN
    nError = UK_ERROR
    eMessage = 'Error allocating indx array'
    WRITE(eInform,*)'Requested size = ',wrk(it,0)%p%id
    eRoutine = 'RemoveOut'
    GOTO 9999
  END IF

  DO is = 1,wrk(it,0)%p%id
    p1 = wrk(it,is)%p%id
    i  = p1 + (it-itsta)*nsmp
    IF( wrk(it,is)%p%iu < 0 )THEN
!---- Add to remove list
      nrm = nrm + 1
      indx(nrm) = is
      rjt(it,0)%p%id = rjt(it,0)%p%id + 1
      rjt(it,rjt(it,0)%p%id)%p => wrk(it,is)%p
      DO j = 1,ntsmp
        ir = (j-1)/nsmp + itsta
        k  = j - (ir-itsta)*nsmp
        IF( wrka(ir,k)%iu < 0 )CYCLE
!---- Remove inv distance more than one sigma
        IF( wrka(it,p1)%ker(ir,k)%p%ks > 1.e-20 .AND. wrka(it,p1)%iu == -uHIT )THEN
           fch(j) = fch(j) - wrka(it,p1)%ker(ir,k)%p%ks/fca(j)
        END IF
      END DO
    END IF
  END DO

  DO j = nrm,1,-1
    DO is = indx(j),wrk(it,0)%p%id-1
      wrk(it,is)%p => wrk(it,is+1)%p
    END DO
    NULLIFY( wrk(it,wrk(it,0)%p%id)%p )
    wrk(it,0)%p%id = wrk(it,0)%p%id-1
  END DO

END DO

9999 CONTINUE

DEALLOCATE( fchm,fchs,STAT=ios )

RETURN
END

!========================================================================

SUBROUTINE ReduceWrk( nrt,rat,wrk,rdc,n1,n2 )

!--- Find the reduced set of hits/nulls

USE AdjointFilter_fi
USE error_fi

IMPLICIT NONE

INTEGER,                              INTENT( IN    ) :: nrt
INTEGER,                              INTENT( IN    ) :: n1, n2
REAL,                                 INTENT( INOUT ) :: rat
TYPE ( dat_ptr ), DIMENSION(n1,0:n2), INTENT( INOUT ) :: wrk   ! pointers to hits/nulls
TYPE ( dat_ptr ), DIMENSION(n1,0:n2), INTENT( INOUT ) :: rdc   ! pointers to hits/nulls which are selected

INTEGER imin, imax, nMin, i, j, k, it, is, ir, ios
REAL    fmin, fmax, frat, maxks

IF( rat < 0.0 )THEN   !---- Working on null set
  rat  = ABS(rat)
  nMin = 5  ! Minimum of 5 nulls for every irt
ELSE
  nMin = 3  ! Minimum of 3 hits for every irt
END IF

DO i = 1,nrt

  it = irt(i)

  IF( wrk(it,0)%p%id == 0 )CYCLE

  IF( wrk(it,0)%p%id > nMin )THEN
    nsrc = nMin + INT((wrk(it,0)%p%id-nMin)*rat)
  ELSE IF( wrk(it,0)%p%id > 0 )THEN
    nsrc = wrk(it,0)%p%id
  END IF

  nrd = 0
  DO WHILE( nrd <= nsrc )

    fmax = -1.E30
    fmin =  1.E30
    imin = 1
    imax = 1
    DO is = 1,wrk(it,0)%p%id
      ir   = (it-itsta)*nsmp + wrk(it,is)%p%id
      IF( fmin > fch(ir) )THEN
        imin = ir
        fmin = fch(ir)
      END IF
      IF( fmax < fch(ir) )THEN
        imax = ir
        fmax = fch(ir)
      END IF
    END DO

    IF( UseMax )THEN
      is = imax - (it-itsta)*nsmp
    ELSE
      is = imin - (it-itsta)*nsmp
    END IF

!--- Add sampler with min/max density for hit/null to filter list

    nrd = nrd + 1
    p1  = rdc(it,0)%p%id + 1
    rdc(it,0)%p%id = p1             ! Number if rdc at it
    rdc(it,p1)%p => wrka(it,is)

    IF( p1 == 1 .AND. wrk(it,0)%p%id > 1 )THEN
      DO is = 1,wrk(it,0)%p%id
        p2 = wrk(it,is)%p%id
        j  = (it-itsta)*nsmp + p2
        fch(j) = 0.6 + FLOAT(is - 1)*0.4/FLOAT(wrk(it,0)%p%id - 1)
      END DO
    END IF

    nrm = 0

    IF( ALLOCATED(indx) )DEALLOCATE( indx,STAT=ios )
    IF( ios == 0 )ALLOCATE( indx(wrk(it,0)%p%id),STAT=ios )
    IF( ios /= 0 )THEN
      nError = UK_ERROR
      eMessage = 'Error allocating indx array'
      WRITE(eInform,*)'Requested size = ',wrk(it,0)%p%id
      eRoutine = 'ReduceWrk'
      GOTO 9999
    END IF

    DO is = 1,wrk(it,0)%p%id

      IF( rdc(it,p1)%p%id /= wrk(it,is)%p%id )CYCLE

      maxks = wrk(it,is)%p%ker(it,is)%p%ks/fca(imax)

!--- Find maximum ks contribution to fch of the selected hit/null

      DO ir = itsta,itend
        DO k = 1,wrk(ir,0)%p%id
          p2 = wrk(ir,k)%p%id
          j   = (ir-itsta)*nsmp + p2
          IF( wrk(it,is)%p%ker(ir,p2)%p%ks > 1.E-20 .AND. wrk(ir,k)%p%iu == wrk(it,is)%p%iu )THEN
              maxks = MAX(maxks,wrk(it,is)%p%ker(ir,p2)%p%ks/fca(j))
          END IF
        END DO
      END DO

!--- Adjust the densities for hits/nulls by adding/subtracting the
!--- contribution of the selected hit/null

      DO ir = itsta,itend
        DO k = 1,wrk(ir,0)%p%id
          p2 = wrk(ir,k)%p%id
          j  = (ir-itsta)*nsmp + p2
          IF( wrk(it,is)%p%ker(ir,p2)%p%ks > 1.E-20 .AND. wrk(ir,k)%p%iu == wrk(it,is)%p%iu )THEN
            frat = 2.**(wrk(it,is)%p%ker(ir,p2)%p%ks/(maxks*fca(j)))
            IF( UseMax )THEN
              fch(j) = fch(j)/frat
            ELSE
              fch(j) = fch(j)*frat
            END IF
          END IF
        END DO
      END DO

!--- Add to remove list

      nrm = nrm + 1
      indx(nrm) = is

    END DO

!--- Move the pointers up

    DO j = nrm,1,-1
      DO is = indx(j),wrk(it,0)%p%id-1
        wrk(it,is)%p => wrk(it,is+1)%p
      END DO
      NULLIFY( wrk(it,wrk(it,0)%p%id)%p )
      wrk(it,0)%p%id = wrk(it,0)%p%id-1
    END DO
    IF( wrk(it,0)%p%id == 0 )EXIT
  END DO

END DO

9999 CONTINUE

RETURN
END

