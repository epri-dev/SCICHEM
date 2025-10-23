SUBROUTINE set_stack_rel_prime( release,frac )
!*******************************************************************************
!
!----- Set SCICHEM release parameters for STACK release Using PRIME
!      (AERMOD version dated 12345)
!
!      Call AERMOD/PRIME subroutines
!         NUMPR1
!         PRIME1
!         WAKE_INI
!         NUMRISE  (slightly modified in SCICHEM prime.f)
!
!      CALL isc_prime subroutines
!         SRCQA
!         SINDEX
!
!      Subroutine GET_WAKEDAT based on AERMOD/PRIME subroutine WAKE_XSIG in
!      PRIME.F (12345 lines 4444 to 4522)
!      Additional code to compute streamwise gradients.
!
!      Subroutine ADDPRIMECONC based on subroutine PRMCALC in AERMOD file
!      CALC1.F (mainly code starting at 12345 line 1237; PRM_PLUME called
!      directly instead of from PRM_PCHI)
!
!*******************************************************************************

! --- MODULES

USE sciprime_fi
USE scipuff_fi
USE met_fi
USE files_fi
USE error_fi
USE constants_fd
USE sampler_fi

USE MAIN1, ONLY: MXGLVL,GRIDHT,ZBASE
USE PRIME_WAKEDAT, ONLY: MXNTR,FQCAV,YBADJ

IMPLICIT NONE

! --- ARGUMENTS

TYPE( releaseT ), INTENT( IN  ) :: release
REAL,             INTENT( OUT ) :: frac    !Mass fraction in primary plume

! --- LOCALS

CHARACTER(20)             :: rdfrm
REAL,PARAMETER            :: gravi = 9.807

REAL                      :: xmlinv
REAL                      :: ddz, thv1, thv2
REAL                      :: xmap, ymap    ! - map factors

INTEGER                   :: ios, alloc_stat
INTEGER                   :: i,k,ifvsec,isrc
INTEGER                   :: ntr
REAL,DIMENSION(nst)       :: dturb,dtrur,purb,prur
REAL,DIMENSION(MXGLVL)    :: uamb,ramb,tamb
REAL,DIMENSION(MXGLVL)    :: svamb,swamb
REAL,DIMENSION(MXGLVL)    :: dedz
REAL(8),DIMENSION(mxntr)  :: xtr,ytr,ztr,rtr

REAL                      :: xr,yr,zr,rr,sz,sy,szc,syc
REAL                      :: bldfac,xfac
REAL                      :: xr0,zr0,sz0,sy0,wmom0,buoy0,syc0,szc0
REAL                      :: dszdx,dsydx,dszcdx,dsycdx
REAL                      :: dszdx0,dsydx0
REAL                      :: h,hx,hy,tamb0,ramb0,hsav

REAL                      :: sigx,sigy,sigz,sigRxy,sigRxz,sigRyz,buoy,cmass      ,fac
REAL, DIMENSION(3)        :: mom

!      DSBH - real    - Effective building height (m)
!      DSBW - real    - Effective building width (m) across flow
!      DSBL - real    - Effective building length (m) along flow
!      XADJ - real    - Distance (m) from source to upwind face of bldg along flow
!      YADJ - real    - Distance (m) from source to center of upwind face of bldg across flow

LOGICAL                   :: nopath, nokey
LOGICAL                   :: lfound

!***********************************************************************
!     Initialize Default Wind Profile Exponents and DTHETADZ
!***********************************************************************

!STAB.CLASS  A    B     C     D      E      F
!           ***  ***   ***   ***    ***    ***
DATA dturb  /0.,  0.,   0.,   0.,   0.02, 0.035/, &
     dtrur  /0.,  0.,   0.,   0.,   0.02, 0.035/, &
     purb  /0.15, 0.15, 0.20, 0.25, 0.30, 0.30/,  &
     prur  /0.07, 0.07, 0.10, 0.15, 0.35, 0.55/

IF( nsrc_prime == 0 )THEN

  !-- Initialise variables
  CALL init_stack_rel_prime()

  iline = 0
  WRITE(rdfrm,'("(A",I3.3,",T1,",I3.3,"A1)")') istrg, istrg
  OPEN(lun_tmp,FILE=TRIM(release%relDisplay),STATUS='OLD',ACTION='READ',IOSTAT=ios)
  IF( ios /= 0 )THEN
    nError   = IV_ERROR
    eRoutine ='set_stack_rel_prime'
    eMessage = 'Missing Prime input File: '//TRIM(release%relDisplay)
    GOTO 9999
  END IF
  DO
    ! Increment the Line Counter
    iline = iline + 1
    ! READ Record to Buffers, as A80 and 80A1 for ISTRG = 80.
    ! Length of ISTRG is Set in PARAMETER Statement in MAIN1.INC
    READ(lun_tmp,rdfrm,IOSTAT=ios) runst1, (runst(i), i = 1, istrg)
    IF( ios == 0 )THEN
      ! Convert Lower Case to Upper Case Letters           ---   CALL LWRUPR
      CALL lwrupr
      ! Define Fields on Card                              ---   CALL DEFINE
      CALL define
      ! Get the Contents of the Fields                     ---   CALL GETFLD
      CALL getfld
      !  If Blank Line, Then CYCLE to Next Card
      IF (bline) CYCLE
      ! Extract Pathway ID From Field 1                    ---   CALL EXPATH
      CALL expath(field(1),nopath)
      ! For Invalid Pathway and Comment Lines Skip to Next Record
      IF (nopath .or. path == '**') CYCLE
      ! Extract Keyword From Field 2                       ---   CALL EXKEY
      CALL exkey(field(2),nokey)
      ! When Keyword Is Wrong, Save Keyword and Skip To The Next Record
      IF (nokey) THEN
        pkeywd = keywrd
        CYCLE
      END IF
      ! Process Input Card Based on Pathway
      IF (path == 'SO') THEN
        ! Process SOurce Pathway Cards                    ---   CALL SOCARD
        CALL socard
      END IF
      ! Store the Current Keyword as the Previous Keyword
      pkeywd = keywrd
    ELSE
      EXIT
    END IF
  END DO
  CLOSE(lun_tmp,IOSTAT=ios)
  ! Check Source Array Limits for Too Few Values
  CALL srcqa
  IF (nError /= NO_ERROR) GO TO 9999
  ! Check for missing keywords
  DO i = 1,ikn
    IF (isstat(i) == 0) THEN
      nError = WN_ERROR
      eRoutine='set_stack_rel_prime'
      WRITE(eInform,*)'Using default value of 0.0'
      SELECT CASE(i)
        CASE(1)
          eMessage= 'Missing rural keyword'
          WRITE(eInform,*)'Using default/previously set value of ',rural
        CASE(2)
          eMessage= 'Missing BUILDHGT keyword'
        CASE(3)
          eMessage= 'Missing BUILDWID keyword'
        CASE(4)
          eMessage= 'Missing BUILDLEN keyword'
        CASE(5)
          eMessage= 'Missing XBADJ keyword'
        CASE(6)
          eMessage= 'Missing YBADJ keyword'
       END SELECT
       eAction  = CHAR(0)
       CALL WarningMessage(.true.)
       IF (nError /= NO_ERROR) GO TO 9999
    END IF
  END DO

  IF( nsmp > 0 )THEN
    ALLOCATE( concPrime(nsmp),STAT=alloc_stat )
    IF( alloc_stat /= 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'set_stack_rel_prime'
      eMessage = 'Error allocating PRIME receptor array'
      GOTO 9999
    END IF
    concPrime = 0.
  END IF

END IF

CALL numpr1()
CALL prime1()

CALL getReleaseSigmas( release,sigx,sigy,sigz,sigRxy,sigRxz,sigRyz )
CALL getReleaseDynamics( release,buoy,mom )
CALL getReleaseMass( release,cmass )

hstack    = release%zrel                  ! stack height (m)
dstack    = 2.*sigx                       ! stack diameter (m)
tstack    = buoy + 273.15                 ! exhaust temperature (deg K)
wstack    = mom(3)                        ! exhaust velocity (m/s)
zbase     = hmin

! KST - integer - PG stability class  *** no longer used ***
IF (xml /= 0.0) THEN
   xmlinv = 1./xml
   IF (xmlinv < -0.14) THEN
      kst = 1
   ELSE IF (xmlinv < -0.05) THEN
      kst = 2
   ELSE IF (xmlinv < -0.01) THEN
      kst = 3
   ELSE IF (xmlinv < 0.01) THEN
      kst = 4
   ELSE IF (xmlinv < 0.06) THEN
      kst = 5
   ELSE
      kst = 6
   END IF
ELSE
   kst = 4
END IF

CALL sindex(srcid,nsrc_prime,release%relName,isrc,lfound)
IF( .NOT.lfound )THEN
  eRoutine = 'set_stack_rel_prime'
  nError   = IV_ERROR
  eMessage = 'PRIME source not found'
  eInform  = 'Release name = '//TRIM(release%relName)
  GO TO 9999
END IF

IF (isrural(isrc)) THEN
    rural = .true.
    urban = .false.
ELSE
    urban = .true.
    rural = .false.
END IF

! Power law coefficient and temperature gradient
IF (rural) THEN
    pvel   = prur(kst)
    dtemdz = dtrur(kst)
ELSE
    pvel   = purb(kst)
    dtemdz = dturb(kst)
END IF

! ---- Check if buildings affect plume
CALL get_met(SNGL(release%xrel),SNGL(release%yrel),release%zrel,0.,0.,1 )
CALL get_sector(ub,vb,uref,ifvsec,afv)
dsbh  = adsbh(ifvsec,isrc)
dsbw  = adsbw(ifvsec,isrc)
dsbl  = adsbl(ifvsec,isrc)
xadj  = adsxadj(ifvsec,isrc)
yadj  = adsyadj(ifvsec,isrc)

CALL wakflg(hstack,dsbh,dsbw,wake)

!IF( wake )THEN

! --- Set meteorological arrays for plume rise calculation

  tamb0 = tb
  ramb0 = 1.2
  CALL set_bl_prime()

  CALL get_met(SNGL(release%xrel),SNGL(release%yrel),SNGL(GRIDHT(1)),0.,0.,1)
  uamb(1)  = SQRT(ub*ub + vb*vb)
  tamb(1)  = tb
  dedz(1)  = (1. + 0.608*hb)*dtdz
  swamb(1) = SQRT(wwbl)
  svamb(1) = SQRT(MAX(uubl,vvbl))

  CALL get_met(SNGL(release%xrel),SNGL(release%yrel),0.5*SNGL(GRIDHT(1)+GRIDHT(2)),0.,0.,1)
  thv2 = (1. + 0.608*hb)*thb

  DO k = 2,MXGLVL

    CALL get_met(SNGL(release%xrel),SNGL(release%yrel),SNGL(GRIDHT(k)),0.,0.,1)
    uamb(k)  = SQRT(ub*ub + vb*vb)
    tamb(k)  = tb
    swamb(k) = SQRT(wwbl)
    svamb(k) = SQRT(MAX(uubl,vvbl))

    IF( k < MXGLVL )THEN
      thv1 = thv2
      CALL get_met(SNGL(release%xrel),SNGL(release%yrel),0.5*SNGL(GRIDHT(k)+GRIDHT(k+1)),0.,0.,1)
      thv2 = (1. + 0.608*hb)*thb
      ddz  = 2./SNGL(GRIDHT(k+1)-GRIDHT(k-1))
      dedz(k) = (thv2-thv1)*ddz
    ELSE
      dedz(k) = (1. + 0.608*hb)*dtdz
    END IF

  END DO

  IF( wts >= 0. )THEN  !Set zero temperature gradient within ustable or neutral BL
    IF( lter )THEN
      CALL get_topogPrj( SNGL(release%xrel),SNGL(release%yrel),h,hx,hy )
    ELSE
      h = 0.
    END IF
   DO k = 1,MXGLVL
      IF( GRIDHT(k) < zinv-h )THEN
        dedz(k) = 0.
      ELSE
        EXIT
      END IF
    END DO
  END IF

!----- Check for low wind speed near release (D. Henn 10/2020)

  DO k = 2,MXGLVL
    IF( GRIDHT(k) > MAX(1.2*DBLE(release%yrel),1.5*dsbh) )EXIT
    IF( uamb(k) < 1.0 )THEN
      wake = .false.
      EXIT
!      GO TO 9999
    END IF
  END DO

  ! Create ambient density profile on grid center
  ! (Code section from nummet of PRIME.FOR
  DO i = 1,MXGLVL
    thv1 = 0.5*(tamb(i)+tamb0) ! Compute average temperature in layer
    ramb(i)=ramb0*(tamb0/tamb(i))*EXP(-gravi*SNGL(GRIDHT(i))/(RGAS*thv1))
  END DO

  ! Get values without bld effects

  IF( wake )THEN
    bldfac = 1.e-3
  ELSE             !If no building downwash, create very small building
    bldfac = 1.    !Otherwise get_wakedat results in zero values for plume
    dsbh   = 0.1
    dsbw   = 1.
    dsbl   = 1.
    xadj   = 0.
  END IF

  CALL set_ambient(MXGLVL,uamb,ramb,dedz,tamb,svamb,swamb,hstack,dsbh*bldfac,dsbw )

  CALL get_met(SNGL(release%xrel),SNGL(release%yrel),hstack,0.,0.,1)
  ustack = MAX(uMin,SQRT(ub*ub+vb*vb)) ! Wind speed (m/s) at release height

  CALL get_met(SNGL(release%xrel),SNGL(release%yrel),dsbh*bldfac,0.,0.,1)
  ubldg  = MAX(uMin,SQRT(ub*ub+vb*vb))  ! Wind speed (m/s) at top of building

  CALL wake_ini(ldbhr,rural,DBLE(dsbh)*bldfac,DBLE(dsbw),DBLE(dsbl),DBLE(xadj),DBLE(yadj),DBLE(ubldg),DBLE(ustack))
  hseff = hstack
  reff  = 0.5*dstack
  ntr = mxntr
  capped = .FALSE.
  horiz  = .FALSE.
  capfact = 0.

  CALL numrise( ldbhr,DBLE(hseff),DBLE(reff),DBLE(tstack),DBLE(wstack),ntr,capped,horiz,DBLE(capfact), &
                xtr,ytr,ztr,rtr,linwake,numwake,ierr)
  IF( ierr /= 0 )THEN
    eRoutine= 'numrise'
    nError  = SZ_ERROR
    eMessage= 'Array size error'
    WRITE(eInform,*)'PRIME Model not implemented'
    eAction  = 'Using Plume rise algorithm'
    !CALL WarningMessage(.true.)
    wake = .false.
    GO TO 9999
  END IF

  CALL get_wakedat( xr,yr,zr,rr,sy,sz,dsydx,dszdx,syc,szc, &
                    dsycdx,dszcdx,ntr,xtr,ytr,ztr, &
                    rtr,mom(3),buoy )
  IF( ierr /= 0 )THEN
    ! Ignore PRIME values without building effects
    xr0    = 0.
    zr0    = 0.
    sy0    = 0.
    syc0   = 0.
    sz0    = 0.
    szc0   = 0.
    wmom0  = 0.
    buoy0  = 0.
    dsydx0 = 0.
    dszdx0 = 0.
    xfac   = -2.  !Use building plume
  ELSE
    xr0    = xr
    zr0    = zr
    sy0    = sy
    syc0   = syc
    sz0    = sz
    szc0   = szc
    wmom0  = mom(3)
    buoy0  = buoy
    dsydx0 = dsydx
    dszdx0 = dszdx
    IF( wake )THEN
      xfac   = 0.
    ELSE
      xfac   = -1. !Use no-building plume
    END IF
  END IF

  ! Get values with bld effects

  IF( wake )THEN  !Check for downwash; otherwise plume is defined by "no building" plume

    CALL set_ambient(MXGLVL,uamb,ramb,dedz,tamb,svamb,swamb,hstack,dsbh,dsbw )

    CALL get_met(SNGL(release%xrel),SNGL(release%yrel),hstack,0.,0.,1)
    ustack = MAX(uMin,SQRT(ub*ub+vb*vb)) ! Wind speed (m/s) at release height

    CALL get_met(SNGL(release%xrel),SNGL(release%yrel),dsbh,0.,0.,1)
    ubldg  = MAX(uMin,SQRT(ub*ub+vb*vb))  ! Wind speed (m/s) at top of building

    CALL wake_ini(ldbhr,rural,DBLE(dsbh),DBLE(dsbw),DBLE(dsbl),DBLE(xadj),DBLE(yadj),DBLE(ubldg),DBLE(ustack))
    hseff = hstack
    reff  = 0.5*dstack
    ntr = mxntr
    capped = .FALSE.
    horiz  = .FALSE.
    capfact = 0.

    CALL numrise( ldbhr,DBLE(hseff),DBLE(reff),DBLE(tstack),DBLE(wstack),ntr,capped,horiz,DBLE(capfact), &
                  xtr,ytr,ztr,rtr,linwake,numwake,ierr)
    IF( ierr /= 0 )THEN
      nError  = UK_ERROR
!      eRoutine= 'numrise'
      !nError  = SZ_ERROR
      !eMessage= 'Array size error'
      !WRITE(eInform,*)'PRIME Model not implemented'
      !eAction  = 'Using Plume rise algorithm'
      !CALL WarningMessage(.true.)
      wake = .false.
      GO TO 9999
    END IF

    CALL get_wakedat( xr,yr,zr,rr,sy,sz,dsydx,dszdx,syc,szc, &
                      dsycdx,dszcdx,ntr,xtr,ytr,ztr, &
                      rtr,mom(3),buoy )
    IF( ierr /= 0 )THEN
      nError  = UK_ERROR
      !eRoutine= 'get_wakedat'
      !eMessage= 'xr Point lies outside range of tabulated values '
      !WRITE(eInform,*)'PRIME Model not implemented'
      !eAction  = 'Using Plume rise algorithm'
      !CALL WarningMessage(.true.)
      !write(lun_log,*)TRIM(eMessage)
      !write(lun_log,*)TRIM(eInform)
      wake = .false.
      GO TO 9999
    END IF

  ELSE

    fqcav = 0.   !wake=.FALSE. - No building downwash

  END IF

  ! Reset release location
  CALL mapfac( SNGL(release%xrel) , SNGL(release%yrel) , xmap , ymap )
  wdsin = SIN(afv)
  wdcos = COS(afv)
  IF( wake )THEN
    xrel_prm(1) = SNGL(release%xrel) + xr*wdsin*xmap - yr*wdcos*xmap  ! afv = -pi to +pi from North.
    xrel_prm(2) = SNGL(release%xrel) + xr*wdsin*xmap - (yr+SNGL(ybadj))*wdcos*xmap
    yrel_prm(1) = SNGL(release%yrel) + xr*wdcos*ymap + yr*wdsin*xmap
    yrel_prm(2) = SNGL(release%yrel) + xr*wdcos*ymap + (yr+SNGL(ybadj))*wdsin*xmap
  ELSE
    xrel_prm(1) = SNGL(release%xrel) + xr0*wdsin*xmap
    xrel_prm(2) = SNGL(release%xrel) + xr0*wdsin*xmap
    yrel_prm(1) = SNGL(release%yrel) + xr0*wdcos*ymap
    yrel_prm(2) = SNGL(release%yrel) + xr0*wdcos*ymap
  END IF

! set scichem release parameters for the two source
  if (lter) then
    call get_topog(xrel_prm(1),yrel_prm(1),h,hx,hy)
    if (nError /= NO_ERROR) go to 9999
  else
    h = 0.0
  end if
  hsav = h

  if( xfac == -2. )then
    xfac = 0.  !No-building plume error; use building plume but set wake=F because only one plume defined
    wake = .FALSE.
  elseif( xfac < 0. )then
    xfac = 1. !Use "no building" plume
  else
    xfac = MAX(0.,MIN(1.,(xtr(ntr)-xr)/(xtr(ntr)-xr0)))
  end if

  zr = (1.-xfac)*zr + xfac*zr0
  zrel_prm(1)  = zr  + h

  if (lter) then
    call get_topog(xrel_prm(2),yrel_prm(2),h,hx,hy)
    if (nError /= NO_ERROR) go to 9999
  endif

  sy = (1.-xfac)*sy + xfac*sy0
  sz = (1.-xfac)*sz + xfac*sz0
  dsydx = (1.-xfac)*dsydx + xfac*dsydx0
  dszdx = (1.-xfac)*dszdx + xfac*dszdx0

  zrel_prm(2)  = h + 0.5*dsbh
  sigy_prm(1)  = sy
  sigy_prm(2)  = MAX(syc,0.5*dsbw)
  sigz_prm(1)  = sz
  sigz_prm(2)  = MAX(szc,0.5*dsbh)
  frac_prm(1)  = 1.0 - SNGL(fqcav)
  frac_prm(2)  = SNGL(fqcav)
  cmass_prm    = cmass
  wmom_prm(1)  = (1.-xfac)*mom(3) + xfac*wmom0
  wmom_prm(2)  = 0.0
  buoy_prm(1)  = (1.-xfac)*buoy + xfac*buoy0
  buoy_prm(2)  = 0.0
  frac         = frac_prm(1)
  CALL get_met(xrel_prm(2),yrel_prm(2),zrel_prm(2),0.,0.,0)
  ustack    = MAX(uMin,SQRT(ub*ub+vb*vb)) ! Wind speed (m/s) at secondary src release location
  ky_prm(2) = syc*dsycdx*ustack
  kz_prm(2) = MIN(difb,szc*dszcdx*ustack)

  CALL get_met(xrel_prm(1),yrel_prm(1),zrel_prm(1),0.,0.,0)
  ustack    = MAX(uMin,SQRT(ub*ub+vb*vb)) ! Wind speed (m/s) at primary src release height ** saved for receptors **
  ky_prm(1) = sy*dsydx*ustack
  kz_prm(1) = MIN(difb,sz*dszdx*ustack)

  zi_rel = zinv - hsav

!  if (fqcav > 0.0) then
!    write(lun_log,'("Prime calculation: Source 1(xrel,yrel,zrel,1-fqcav,cmass_prm): ")',ADVANCE='NO')
!    write(lun_log,'(3(1pE10.3,1x),0pF4.2,1x,1pE10.3)')xrel_prm(1), yrel_prm(1), zrel_prm(1),frac_prm(1),cmass_prm
!    write(lun_log,'("Prime calculation: Source 2(xrel,yrel,zrel,fqcav): ")',ADVANCE='NO')
!    write(lun_log,'(3(1pE10.3,1x),0pF4.2)')xrel_prm(2),yrel_prm(2), zrel_prm(2),frac_prm(2)
!    write(299,'("Prime calculation: Source 1(xrel,yrel,zrel,1-fqcav,cmass_prm): ")',ADVANCE='NO')
!    write(299,'(3(1pE10.3,1x),0pF4.2,1x,1pE10.3)')xrel_prm(1), yrel_prm(1), zrel_prm(1),frac_prm(1),cmass_prm
!    write(299,'("Prime calculation: Source 2(xrel,yrel,zrel,fqcav): ")',ADVANCE='NO')
!    write(299,'(3(1pE10.3,1x),0pF4.2)')xrel_prm(2),yrel_prm(2), zrel_prm(2),frac_prm(2)
!  else
!    write(lun_log,'("Prime calculation (xrel,yrel,zrel,cmass_prm): ")',ADVANCE='NO')
!    write(lun_log,'(3(1pE10.3,1x),1pE10.3)')xrel_prm(1), yrel_prm(1), zrel_prm(1),cmass_prm
!    write(299,'("Prime calculation (xrel,yrel,zrel,cmass_prm): ")',ADVANCE='NO')
!    write(299,'(3(1pE10.3,1x),1pE10.3)')xrel_prm(1), yrel_prm(1), zrel_prm(1),cmass_prm
!  end if

  ! Skip adding PRIME concentration to sampler in B3 to be consistent with dosage file
  DO i = 1,nsmp
   CALL AddPRIMEconc( i,xr,sy,xmap,ymap,ntr,xtr,ztr,release%xrel,release%yrel,release%zrel )
  END DO

!END IF

9999 CONTINUE

RETURN
END

SUBROUTINE wakflg(hs,dsbh,dsbw,wake)
!***********************************************************************
!                 WAKFLG Module of ISC2 Short Term Model - ISCST2
! ----------------------------------------------------------------------
! ---    ISC-PRIME     Version 1.0    Level 970812              Modified
! ---        D. Strimaitis
! ---        Earth Tech, Inc.
!            Prepared for EPRI under contract WO3527-01
! ----------------------------------------------------------------------

!        PURPOSE: To Set Wake Flags for Building Downwash Algorithms

!        PROGRAMMER: Roger Brode, Jeff Wang

!        DATE:    March 2, 1992

!        INPUTS:  Building Dimensions
!                 Source Parameters
!                 Meteorological Variables for One Hour

!        OUTPUTS: Logical Flags for Wake Switche, WAKE
!***********************************************************************
IMPLICIT NONE
REAL            :: dsbh,dsbw,hs
LOGICAL         :: wake

!     Set Initial Wake Switches Based on Building Dimensions
IF (dsbh==0.0 .or. dsbw==0.0 .or. &
    hs >= (dsbh + 1.5*AMIN1(dsbh,dsbw))) THEN
    wake   = .false.
ELSE
    wake   = .true.
END IF

RETURN
END

SUBROUTINE wsadj(us,hs,uref,zref,p)
!***********************************************************************
!                 WSADJ Module of ISC2 Short Term Model - ISCST2
!
!        PURPOSE: Adjusts Wind Speed from Anemometer Height to Stack Height
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        INPUTS:  Arrays of Source Parameters
!                 Meteorological Variables for One Hour
!                 Wind Speed Profile Exponents (Default or User-defined)
!
!        OUTPUTS: Stack Top Wind Speed, US
!
!***********************************************************************
IMPLICIT NONE
REAL         :: us,hs,uref,zref,p

!     Adjust Wind Speed -- Assume Wind Speed Constant Below 10 meters
IF (hs >= 10.0) THEN
    us = uref * (hs/zref)**p
ELSE IF (zref > 10.0) THEN
    us = uref * (10.0/zref)**p
ELSE
    us = uref
END IF

!     Do Not Allow Stack Height Wind Speed < 1.0 m/s
IF (us < 1.0) THEN
    us = 1.0
END IF

RETURN
END

SUBROUTINE get_sector(u,v,s,ifvsec,afv)
  IMPLICIT NONE
  ! Input u,v; Output s,ifvsec,afv
  REAL    :: u,v,s,beta,deg2rad,afv
  INTEGER :: ifvsec

  deg2rad = ATAN(1.0)/45.0
  s = SQRT(u*u+v*v)
  IF( u == 0. .AND. v == 0. )THEN
    afv = 0.
  ELSE
    afv = ATAN2(u,v)    ! -pi to +pi from North.
  END IF
  beta = afv/deg2rad
  IF (beta < 0.0) beta = beta + 360.0
  ifvsec = INT(beta*0.1 + 0.4999)
  IF (ifvsec == 0) ifvsec = 36

RETURN
END
!==============================================================================

SUBROUTINE init_stack_rel_prime()

USE scipuff_fi
USE sciprime_fi
USE MAIN1, ONLY: L_EFFSIGY, L_VECTORWS, L_AdjUstar,L_LowWind1, L_LowWind2,&
                 L_UserSVmin, L_UserWSmin, L_UserFRANmax, URBSTAB

IMPLICIT NONE

INTEGER alloc_stat

! Allocate prime variables

ALLOCATE( srcid(mxsrc),iwrk2(mxsrc,ikn),isrural(mxsrc),STAT=alloc_stat )
ALLOCATE( adsbh(nsec,mxsrc),adsbw(nsec,mxsrc),adsbl(nsec,mxsrc),STAT=alloc_stat )
ALLOCATE( adsxadj(nsec,mxsrc),adsyadj(nsec,mxsrc),STAT=alloc_stat )

! Initialise variables

! LDBHR - Debug output written when .TRUE.
ldbhr   = .FALSE.
iwrk2   = 0
isstat  = 0
wake    = .FALSE.
adsbh   = 0.0
adsbw   = 0.0
adsbl   = 0.0
adsxadj = 0.0
adsyadj = 0.0
isrural = .FALSE.
rural   = .FALSE.
urban   = .TRUE.
srcid   = ' '
L_EFFSIGY     = .FALSE.
L_VECTORWS    = .FALSE.
L_AdjUstar    = .FALSE.
L_LowWind1    = .FALSE.
L_LowWind2    = .FALSE.
L_UserSVmin   = .FALSE.
L_UserWSmin   = .FALSE.
L_UserFRANmax = .FALSE.
URBSTAB       = .FALSE.

! Call NUMMET to set up wind, temp profiles when .TRUE. else use GETMET
primeprof = .false.

keywd(1:ikn) = (/'RURAL   ','BUILDHGT','BUILDWID',&
                  'BUILDLEN','XBADJ   ','YBADJ   '/)

CALL set_prime_const()

RETURN
END

!==============================================================================

SUBROUTINE set_prime_const()

!------ Extracted from Prime file setup.f, subroutine VARINI

USE MAIN1

IMPLICIT NONE

! --- Initialize double precision constants based on PI
      PI      = 4.0D0*DATAN(1.0D0)
      TWOPI   = 2.0D0*PI
      RTOFPI  = DSQRT(PI)
      SRT2PI  = DSQRT(TWOPI)
      RTOF2   = DSQRT(2.0D0)
      RTPIBY2 = DSQRT(PI/2.0D0)
      DTORAD  = PI/180.0D0
      RTODEG  = 180.0D0/PI

! --- Initialize constant for 1/3 used as exponent
      THIRD = 1.0D0/3.0D0

RETURN
END

!==============================================================================

SUBROUTINE set_stack_rel_prime_null()

USE scipuff_fi
USE sciprime_fi

IMPLICIT NONE

cmass_prm = 0.

frac_prm(1) = 1.
frac_prm(2) = 0.

sigy_prm(1:2) = 1.
sigy_prm(1:2) = 1.
sigz_prm(1:2) = 1.
xrel_prm(1:2) = 0.
yrel_prm(1:2) = 0.
zrel_prm(1:2) = 0.
ky_prm(1:2)   = 1.
kz_prm(1:2)   = 1.
wmom_prm(1:2) = 0.
buoy_prm(1:2) = 0.

RETURN
END

!==============================================================================

SUBROUTINE AddPRIMEconc( ismp,xSCI,sySCI,xmap,ymap,ntr,xtr,ztr,xrel,yrel,zrel )

USE scipuff_fi
USE sampler_fi
USE sciprime_fi
USE met_fi
USE MAIN1, WDSIN_AER=>WDSIN, WDCOS_AER=>WDCOS, XADJ_AER=>XADJ, YADJ_AER=>YADJ

IMPLICIT NONE

INTEGER, INTENT( IN ) :: ismp
REAL,    INTENT( IN ) :: xSCI, sySCI
REAL,    INTENT( IN ) :: xmap, ymap
INTEGER, INTENT( IN ) :: ntr
REAL(8), DIMENSION(ntr), INTENT( IN  ) :: xtr, ztr
REAL(8), INTENT( IN ) :: xrel
REAL(8), INTENT( IN ) :: yrel
REAL,    INTENT( IN ) :: zrel

INTEGER ipositn, is, n1, n2
REAL    xsr, ysr, xbrec, ybrec
REAL    xfac
REAL(8) szout, syout, szcav, sycav, cPRM, ctot
REAL(8) FYOUT

REAL(8) :: q2(3),y2(3),sy2(3),z2(3),h2(3),sz2(3),qc2(3), &
           qtksav,ppfsav,fqcav !,qtk,ppf

xsr = (smp(ismp)%x-SNGL(xrel))/xmap  !Distance from receptor to source in project coordinates
ysr = (smp(ismp)%y-SNGL(yrel))/ymap

!     Calculate Downwind (X) and Crosswind (Y) Distances
x = DBLE( xsr*wdsin + ysr*wdcos)
y = DBLE(-xsr*wdcos + ysr*wdsin)

IF( x >= xSCI )GOTO 9999

!     Calculate Source-Receptor (Radial) Distance, DISTR
distr = SQRT( x*x + y*y )

xbrec = SNGL(x)-xadj
ybrec = SNGL(y)-yadj

zflag = DBLE(smp(ismp)%zh)
zrt   = DBLE(smp(ismp)%zh)
us    = DBLE(ustack)
SRT2PI = SQRT(8.D0*DATAN(1.0D0))

ctot = 0.D0

!************ Based on calc1.f subroutine PRMCALC
! --- PRIME ---------------------------------------------------------
! --- Calculate where receptor is relative to near-wake cavity
!     and building (IPOSITN=1 for within bldg; 2=within
!     near-wake, 3=within far wake; 4=outside)
! --- Note:  xbrec is downwind dist. of receptor from upwind
!     bldg face; ybrec is crosswind dist. of receptor from
!     center of upwind bldg. face                  ---  CALL POSITION
!      call POSITION(xbrec,ybrec,zflag,ipositn)
      CALL position( DBLE(xbrec),DBLE(ybrec),DBLE(zflag),ipositn )

      if(ipositn.EQ.4 .AND. X.LE.0.0D0) then
! ---    Receptor is upwind of sources and is not within
! ---    a building wake - use AERMOD calculation
! ---    Set PRMVAL(NUMTYP) array = AERVAL(NUMTYP) array
!         PRMVAL = AERVAL *** NO CONTRIBUTION ***

      elseif(ipositn.NE.2 .AND. distr .LT. 0.99D0) then
! ---    Receptor Too Close to Source for Calculation and is not
! ---    within a building near-wake (cavity) - use AERMOD calculation
! ---    Set PRMVAL(NUMTYP) array = AERVAL(NUMTYP) array
!         PRMVAL = AERVAL *** NO CONTRIBUTION ***
! -------------------------------------------------------------

!      ELSE IF (DISTR .GT. MAXDIST) THEN  *** TEST NOT RELEVANT ***
!        Maximum distance from source; assigned
!        value of 80km for obsolescent TOXICS option
!        or new FASTALL option; otherwise "unlimited" (1.0D20)
! ---    Receptor is beyond MAXDIST - use AERMOD calculation
! ---    Set PRMVAL(NUMTYP) array = AERVAL(NUMTYP) array
!         PRMVAL = AERVAL

!      ELSE IF (.NOT. WAKE) THEN *** TEST NOT RELEVANT: WAKE MUST BE TRUE ***
! ---    No wake effects for this source for this hour - use AERMOD calculation
! ---    Set PRMVAL(NUMTYP) array = AERVAL(NUMTYP) array
!         PRMVAL = AERVAL

      ELSE

        IF( wts < 0. .OR. zrel_prm(1) <= zi_rel )THEN

          CALL numgrad( x,xtr,ztr,ntr,zr )

! ---    Calculate sigmas
!         dhpout = dhp
!         call WAKE_XSIG(x,hs,dhpout,nobid,szout,syout,
!     &                  szcav,sycav)
         CALL WAKE_XSIG(DBLE(x),DBLE(zrel),zr-DBLE(zrel),.FALSE.,szout,syout,szcav,sycav)
         sy = syout
         sz = szout

         qtk=DBLE(cmass_prm)
         ppf=0.D0  !*** plume penetration factor ***

! ---    PRIME ---------------------------------------------------
! ---    When there is a building wake, consider treatment of mass in
! ---    cavity as additional sources, or as only source
         qtksav = qtk
         ppfsav = ppf
! ---    Place selected plume data into transfer arrays (first element)
         q2(1)  = qtk
         y2(1)  = DBLE(y)
         sy2(1) = sy
         z2(1)  = zflag
         h2(1)  = DBLE(zrel) !he
         sz2(1) = DBLE(sz)
         n1 = 1
         n2 = 1
!        if(WAKE) then *** TRUE if in this routine ***
! ---       Define cavity source                              ---   CALL CAV_SRC
            call CAV_SRC(DBLE(x),DBLE(y),DBLE(zflag),fqcav,qc2,h2,y2,z2,sz2,sy2,n1,n2)
            if(fqcav.GT.0.0D0) then
! ---          Set source strengths
               q2(1)=qtk*(1.0D0-fqcav)
               q2(2)=qtk*fqcav*qc2(2)
               q2(3)=qtk*fqcav*qc2(3)
            end if
!        end if

! ---    Initialize PRMVAL(NUMTYP) output array values to zero, because contributions
! ---    due to more than one source are summed here (or do loop may
! ---    not execute if neither source contributes)
         cPRM = 0.0D0 !PRMVAL = 0.0D0

! ---    Loop over 3 possible sources (is=1 for primary source,
! ---    is=2 for "outside" cavity source, and is=3 for "inside" cavity source)
         do is = n1, n2

! ---       Cycle to next source if emission rate is 0.0
            if (q2(is) .eq. 0.0D0) cycle

! ---       Transfer data for current source
            qtk = q2(is)
            y   = SNGL(y2(is))
            sy  = sy2(is)
            sz  = sz2(is)
            he  = h2(is)
            zflag = z2(is)

! -------------------------------------------------------------
!           Calculate the 'y-term' contribution to
!           dispersion, FSUBY                              ---   CALL FYPLM
            CALL FYPLM(SY,FYOUT)
            FSUBY  = FYOUT

            IF( FSUBY.EQ.0.0D0 )THEN
! ---          Lateral term is 0.0, set PRMVAL array (1:NUMTYP) to 0.0.
               !cPRM = 0.0D0 !PRMVAL = 0.0D0

            ELSE

! ---          Set FOPT = 0.5 for PRIME calculation since wake is "near neutral"
               FOPT = 0.5D0

! *** ignore deposition
!                ADJ = 1.0D0

!                CALL PRM_PCHI( ADJ, 0., 0 )
                CALL PRM_PLUME( zrt,cPRM )
                ctot = ctot + cPRM

            END IF

         END DO

! ---    Restore original plume data
         QTK = QTKSAV
         PPF = PPFSAV
         y   = DBLE(y2(1))
         sy  = sy2(1)
         sz  = sz2(1)
         he  = h2(1)
         zflag = z2(1)

         xfac = ( 1. + MIN( 2.*(uubl+vvbl)/(MAX(uu_calm,ub**2+vb**2)),1.) ) * sySCI
         xfac = 1. - EXP(-0.5*((x-xSCI)/(xfac))**2)
         concPrime(ismp) = SNGL(cPRM) * xfac

        END IF
      END IF

9999 CONTINUE

RETURN
END

!==============================================================================
!----------------------------------------------------------------------
!      subroutine get_prime_grid(nz,mxz,zg,zf,r0)
      subroutine get_prime_grid( nz,zg,nza )
!----------------------------------------------------------------------

! --- purpose:  get the grid setup in numpr1

!----------------------------------------------------------------------

      USE sciprime_fi

      USE MAIN1

      INTEGER,             INTENT( IN  ) :: nz
      REAL, DIMENSION(nz), INTENT( OUT ) :: zg
      INTEGER,             INTENT( OUT ) :: nza

      INTEGER i

      nza = MIN(nz,MXGLVL)

      DO i = 1,nza
        zg(i) = GRIDHT(i)
      END DO

      RETURN
      END

!----------------------------------------------------------------------
      subroutine set_bl_prime()
!----------------------------------------------------------------------

! --- prime      Non-standard                 set_bl_prime
!                d. s. henn, r. i. sykes, sage management

! --- purpose:  set boundary layer heat flux and depth

! --- inputs:
!          wts - real    - sfc heat flux (C-m/s)
!          zi  - real    - bl depth (m)


! --- output: none

! --- set_bl_prime called by:  set_stack_rel_prime
! --- set_bl_prime calls:      none

  USE PRIME_params
  USE PRIME_ambient
  USE MAIN1, ONLY: STABLE, UNSTAB, ZI, SFCHF, USTAR, WSTAR, OBULEN, ZIMECH
  USE met_fi, ONLY: wts, zinv, us2, ws2, xml

  IMPLICIT NONE

	wts_pri = wts
	zi_pri  = zinv

  ZI     = DBLE(zinv)
  ZIMECH = ZI                   !Only used for stable conditions
  SFCHF  = DBLE(wts) * 1200.D0  !Approximate conversion - since not used?
  USTAR  = DBLE(SQRT(us2))
  WSTAR  = DBLE(SQRT(ws2))
  OBULEN = DBLE(xml)

  STABLE = wts < 0.
  UNSTAB = .NOT.STABLE

	RETURN
	END

!----------------------------------------------------------------------
      subroutine set_ambient( nz,ua,ra,dtmdz,tamb,sv,sw, &
                              HEFF,DSBHSCI,DSBWSCI )
!----------------------------------------------------------------------

! --- purpose:  set the variables in common ambient data.

!----------------------------------------------------------------------

      USE MAIN1
      USE scipuff_fi, ONLY: wwtrop
      USE met_fi,     ONLY: uu_calm

      IMPLICIT NONE

      INTEGER,             INTENT( IN ) :: nz
      REAL, DIMENSION(nz), INTENT( IN ) :: ua,ra,dtmdz,tamb,sv,sw
      REAL,                INTENT( IN ) :: HEFF,DSBHSCI,DSBWSCI

      INTEGER i, NDXBHI, NDXALO, NDXBLO
      REAL(8) ZHI, ZLO

! --- Set background profiles

      DO i = 1,nz
        GRIDWS(i)  = ua(i)
        GRIDPT(i)  = tamb(i)
        GRIDTG(i)  = dtmdz(i)
        GRIDRHO(i) = ra(i)
        GRIDSV(i)  = sv(i)
        GRIDSW(i)  = sw(i)
      END DO

     THIRD = 1.D0/3.D0

     DSBH = DSBHSCI
     DSBW = DSBWSCI
     B_SUBS = MIN( DSBH, DSBW )
     B_SUBL = MAX( DSBH, DSBW )
     B_SUBL = MIN( B_SUBL, 8.0D0*B_SUBS )
     RSCALE = B_SUBS**(2.0D0*THIRD) * B_SUBL**THIRD

     ZHI = 1.2D0*RSCALE * (15.0D0 + &
                   (DSBH/(1.2D0*RSCALE))**3)**THIRD
     IF (UNSTAB) THEN
        ZHI = MIN( ZHI, ZI )
     END IF
     ZLO = 0.0D0

     HE = HEFF

     CALL LOCATE(GRIDHT, 1, MXGLVL, ZHI, NDXBHI)
     CALL LOCATE(GRIDHT, 1, MXGLVL, ZLO, NDXBLO)
     NDXALO = NDXBLO + 1
     CALL ANYAVG ( MXGLVL, GRIDHT, GRIDWS, ZLO,NDXALO, &
       ZHI,NDXBHI,UEFF )
     CALL ANYAVG ( MXGLVL, GRIDHT, GRIDSV, ZLO,NDXALO, &
       ZHI,NDXBHI,SVEFF )
     CALL ANYAVG ( MXGLVL, GRIDHT, GRIDSW, ZLO,NDXALO, &
       ZHI,NDXBHI,SWEFF )
     CALL ANYAVG ( MXGLVL, GRIDHT, GRIDTG, ZLO,NDXALO, &
       ZHI,NDXBHI,TGEFF )
! ---    Save original SVEFF before applying minimum value, for use
!        with LOWWIND2 Beta option
     SIGVEFF = SVEFF

     SVMIN = MAX(SQRT(uu_calm),0.1)
     WSMIN = MAX(SQRT(wwtrop),0.01)

     RETURN
     END

!----------------------------------------------------------------------

      SUBROUTINE InitSCICHEM_PRIME( fb )

! --- scichem and prime interface file
! --- purpose:  initialize SCICHEM variables for use in numrise

      USE sciprime_fi
      USE PRIME_WAKEDAT, ONLY: xLb, xLR, xbadj

      IMPLICIT NONE

      REAL, INTENT( IN ) :: fb

      xplm     = SNGL( xLb + 1.15D0*xLR + MAX(xbadj,0.0D0) )
!      xplm     = SNGL( 0.5*xLb + MAX(xbadj,0.0D0) )
      fbini    = fb
      uplm     = 0.0
      wplm     = 0.0
      tplm     = 0.0
      rplm     = 0.0

      RETURN
      END

!----------------------------------------------------------------------

      SUBROUTINE UpdateSCICHEM_PRIME()

! --- scichem and prime interface file
! --- purpose:  update SCICHEM variables for use in numrise

      USE sciprime_fi
      USE PRIME_PLU

      IMPLICIT NONE

      xplmold = x
      uplmold = u
      wplmold = w
      tplmold = tp
      rplmold = r

      RETURN
      END

!---------------------------------------------------------------------

      SUBROUTINE ChkFinalSCICHEM_PRIME()

! --- scichem and prime interface file
! --- purpose:  set final SCICHEM variables from numrise if past xplm

      USE sciprime_fi
      USE PRIME_PLU

      IMPLICIT NONE

      REAL facx

      IF( (xplm <= x).AND. (xplm >= xplmold) )THEN
        facx  = (xplm-xplmold)/(SNGL(x)-xplmold)
        uplm  = uplmold + facx*(SNGL(u)-uplmold)
        wplm  = wplmold + facx*(SNGL(w)-wplmold)
        tplm  = tplmold + facx*(SNGL(tp)-tplmold)
        rplm  = rplmold + facx*(SNGL(r)-rplmold)
      END IF

      RETURN
      END

!----------------------------------------------------------------------
      subroutine get_wakedat( xro,yro,zro,rro,syo,szo,dsydxo,dszdxo, &
                              syco,szco,dsycdxo,dszcdxo,ntr,xtr,ytr,ztr, &
                              rtr,wmom,buoy)

! --- scichem and prime interface file
! --- purpose:  get wakedat variables in common

! --- Based on AERMOD subroutine WAKE_XSIG

!----------------------------------------------------------------------
      USE PRIME_params
      USE PRIME_ambient
      USE PRIME_WAKEDAT
      USE sciprime_fi
      USE constants_fd

      IMPLICIT NONE

      REAL,                    INTENT( OUT ) :: xro,yro,zro,rro,syo,szo,syco,szco
      REAL,                    INTENT( OUT ) :: dsydxo,dszdxo,dsycdxo,dszcdxo
      INTEGER,                 INTENT( IN  ) :: ntr
      REAL(8), DIMENSION(ntr), INTENT( IN  ) :: xtr, ytr, ztr, rtr
      REAL,                    INTENT( OUT ) :: wmom, buoy

      INTEGER nm1, i, ip1, nwkm1, ncvm1
      REAL(8) xr,yr,zr,rr,sy,sz,syc,szc,x
      REAL(8) xp,syp,szp,zp,zx
      REAL(8) dsydx,dszdx,dsycdx,dszcdx
      REAL(8) hstk, rise, bidsq, fac, dxi
      REAL(8) ua, ra,ta, dudz, dpdz
      REAL    flux
      LOGICAL nobid

!---- Initialize

      nobid  = .false.
      dszdx  = 0.0
      dsydx  = 0.0
      dszcdx = 0.0
      dsycdx = 0.0
      ierr   = 0

      rro = 0.; syo = 0.; szo = 0.; syco = 0.; szco = 0.
      dsydxo = 0.; dszdxo = 0.; dsycdxo = 0.; dszcdxo = 0.

!     Transition from near wake to far wake region
      xr = DBLE(xplm) ! set to xlb + 1.15*xlr + MAX(xbadj,0.0) in NUMRISE
!     Interpolate plume centerline and height
      if (xr >= xtr(ntr)) then
         zr = ztr(ntr)
         yr = ytr(ntr)
         rr = rtr(ntr)
         zx = 0.
      else
         nm1 = ntr - 1
         zr = ztr(1)
         yr = ytr(1)
         rr = rtr(1)
         zx = (ztr(2)-ztr(1))/(xtr(2)-xtr(1))
         if( (xtr(2) - xtr(1)) < 1.e-6 )then
           ierr = -1 !****
           return
         end if
         do i=nm1,1,-1
            if (xr >= xtr(i))then
               ip1 = i + 1
               if( (xtr(ip1) - xtr(i)) < 1.e-6 )then
                 ierr = -1 !****
                 return
               end if
               fac = (xtr(ip1)-xr)/(xtr(ip1)-xtr(i))
               yr  = ytr(ip1)-(ytr(ip1)-ytr(i))*fac
               zr  = ztr(ip1)-(ztr(ip1)-ztr(i))*fac
               rr  = rtr(ip1)-(rtr(ip1)-rtr(i))*fac
               zx  = (ztr(ip1)-ztr(i))/(xtr(ip1)-xtr(i))
               exit
            endif
         end do
      endif
      CALL numgrad( xr,xtr,ztr,ntr,zr ) !*** temporary check***

! --- Single precision output

      xro = SNGL(xr); yro = SNGL(yr); zro = SNGL(zr); rro = SNGL(rr)

! --- Use "hstk+rise" for consistency w/ WAKE_XSIG
      hstk = DBLE(hstack)
      rise = zr-hstk
      x    = xr

! --- Setup parameters for plume sigmas
!     Taken from

!      HE = zr
!      CALL HEFF ( xr )
!      CALL IBLVAL ( xr )


!*****************************************************************************
! --- Following based on WAKE_XSIG with additional code to compute d(sz,sy)/dx
!*****************************************************************************

! --- Primary source:
! -------------------
      if(x.LE.0.0D0) then
! ---    Report null values (these should never get used!)
         sz=0.0D0
         sy=0.0D0
         ierr = -1 !****
         return
      elseif(nwak.LE.1 .OR. x.lt.xwak(1))then  !Combined cases from WAKE_XSIG
! ---    Plume never altered by wake turbulence OR
! ---    Point lies upwind of wake region; use HOST curves
         call SIGZPR(x,hstk+rise,sz)
         call SIGYPR(x,hstk+rise,sy)
         if(.not.NOBID) then
            bidsq=(rise/3.5D0)**2
            sz=DSQRT(sz**2+bidsq)
            sy=DSQRT(sy**2+bidsq)
         endif
! *** Compute gradients *******************************************************
         dxi = DBLE(xtr(2)-xtr(1)); xp = x+dxi; zp = hstk+rise+zx*dxi
         if( dxi < 1.e-6 )then
           ierr = -1 !****
           return
         end if
         call SIGZPR(xp,zp,szp)
         call SIGYPR(xp,zp,syp)
         if(.not.NOBID) then
!            bidsq=(rise/3.5D0)**2 !*** Use previous value
            szp=DSQRT(szp**2+bidsq)
            syp=DSQRT(syp**2+bidsq)
         endif
         dszdx = (szp-sz)/dxi
         dsydx = (syp-sy)/dxi
!******************************************************************************

      elseif(x.gt.xwak(nwak)) then
! ---    Point lies downwind of transition to ambient growth; use
! ---    HOST curves with virtual source term
         call SIGZPR(x,hstk+rise,sz)
         call SIGYPR(x,hstk+rise,sy)
         sz = DSQRT(sz*sz + vsigz*vsigz )
         sy = DSQRT(sy*sy + vsigy*vsigy )
! *** Compute gradients *******************************************************
         dxi = DBLE(xwak(nwak)-xwak(nwak-1)); xp = x+dxi; zp = hstk+rise+zx*dxi
         if( dxi < 1.e-6 )then
           ierr = -1 !****
           return
         end if
         call SIGZPR(xp,zp,szp)
         call SIGYPR(xp,zp,syp)
         szp = DSQRT(szp*szp + vsigz*vsigz )
         syp = DSQRT(syp*syp + vsigy*vsigy )
         dszdx = (szp-sz)/dxi
         dsydx = (syp-sy)/dxi
!******************************************************************************

      else
! ---    Point lies within range of tabulated values
         nwkm1=nwak-1
         sz=szwak(1)
         sy=sywak(1)
         do i=nwkm1,1,-1
            if(x.ge.xwak(i))then
               ip1=i+1
               if( (xtr(ip1) - xtr(i)) < 1.e-6 )then
                 ierr = -1 !****
                 return
               end if
               fac=(xwak(ip1)-x)/(xwak(ip1)-xwak(i))
               sz=szwak(ip1)-(szwak(ip1)-szwak(i))*fac
               sy=sywak(ip1)-(sywak(ip1)-sywak(i))*fac
! *** Compute gradients *******************************************************
               dxi = 1.D0/(xwak(ip1)-xwak(i))
               dszdx = (szwak(ip1)-szwak(i))*dxi
               dsydx = (sywak(ip1)-sywak(i))*dxi
!******************************************************************************
               goto 50
            endif
         enddo
      endif

! --- Cavity source:
! -------------------
50    if(ncav.LE.1) then
! ---    No contribution from cavity source (report initial values)
         szc=szcav(1)
         syc=sycav(1)
      elseif(x.lt.xcav(1)) then
! ---    Point lies upwind of cavity region (report initial values)
         szc=szcav(1)
         syc=sycav(1)
      elseif(x.gt.xcav(ncav)) then
! ---    Point lies downwind of transition to ambient growth; use
! ---    HOST curves with virtual source term
         call SIGZPR(x,0.0D0,szc)
         call SIGYPR(x,0.0D0,syc)
         szc = DSQRT(szc*szc + vsigzc*vsigzc)
         syc = DSQRT(syc*syc + vsigyc*vsigyc)
      else
! ---    Point lies within range of tabulated values
         ncvm1=ncav-1
         szc=szcav(1)
         syc=sycav(1)
         do i=ncvm1,1,-1
            if(x.ge.xcav(i))then
               ip1=i+1
               if( (xcav(ip1) - xcav(i)) < 1.e-6 )then
                 EXIT
               end if
               fac=(xcav(ip1)-x)/(xcav(ip1)-xcav(i))
               szc=szcav(ip1)-(szcav(ip1)-szcav(i))*fac
               syc=sycav(ip1)-(sycav(ip1)-sycav(i))*fac
! *** Compute gradients *******************************************************
               dxi = 1.D0/(xcav(ip1)-xcav(i))
               dszcdx = (szcav(ip1)-szcav(i))*dxi
               dsycdx = (sycav(ip1)-sycav(i))*dxi
!******************************************************************************
               EXIT
            endif
         enddo
      endif
!*****************************************************************************
! --- Bottom of section based on WAKE_XSIG
!*****************************************************************************

! --- Copy into single-precision output

      xro = SNGL(xr); yro = SNGL(yr); zro = SNGL(zr)
      syo = SNGL(sy); szo = SNGL(sz); dsydxo = SNGL(dsydx); dszdxo = SNGL(dszdx)
      syco = SNGL(syc); szco = SNGL(szc); dsycdxo = SNGL(dsycdx); dszcdxo = SNGL(dszcdx)
!      if( ANY((/xro<=0.,yro<=0.,zro<0.,syo<=0.,szo<=0.,dsydxo<=0.,dszdxo<=0./)) )THEN !yro may be zero if wind direction does not change with height
      if( ANY((/xro<=0.,zro<0.,syo<=0.,szo<=0.,dsydxo<=0.,dszdxo<=0./)) )THEN
         ierr = -1 !****
         return
       end if

! --- Set mass fraction and fluxes

      flux = PI*rplm*rplm*SQRT(uplm*uplm + wplm*wplm)
      wmom = flux*wplm
      call zmet(zr,ua,ra,ta,dudz,dpdz)
      buoy = flux*(tplm-SNGL(ta))
      buoy = MAX(buoy,0.0)

      RETURN
      END

!==============================================================================

SUBROUTINE PSIMPL( xr,yr,zflagr,hes,szs,sys,hrval )

!USE MAIN1_INC
!USE MAIN2_INC
!USE MAIN3_INC
!***********************************************************************
!               PSIMPL Module of ISC2 Short Term Model - ISCST2
! ----------------------------------------------------------------------
! ---    ISC-PRIME     Version 1.0    Level 970812              Modified
! ---        D. Strimaitis
! ---        Earth Tech, Inc.
!            Prepared for EPRI under contract WO3527-01
! ----------------------------------------------------------------------
!
!        PURPOSE: Calculates Hourly Concentration or Deposition
!                 value for POINT Sources
!                 Using Gaussian Plume Equation for Simple Terrain
!
!                 (Replaces PCHI and PDEP)
!
!           NOTE: Particle settling is treated as a "tilted plume"
!                 until the centerline reaches the surface.  Thereafter
!                 the centroid height of the plume continues to be
!                 modified by gravity.  This process is simulated by
!                 altering the sigma-z for each particle-size.  Hence,
!                 sigma-z is now a function of particle-size.
!
!        PROGRAMMER: D. Strimaitis, SRC
!
!        DATE:    December 15, 1993
!
!        MODIFIED:   To call PDEP for call to SUB. DEPCOR; to use
!                    modified SUB. VERT.  R.W. Brode, PES, Inc. - 9/30/94
!
!        INPUTS:
!
!        OUTPUTS: HRVAL, Concentration or Deposition for Particular
!                 Source/Receptor Combination
!
!        CALLED FROM:   PCALC
!***********************************************************************

USE met_fi
USE sciprime_fi
USE constants_fd

IMPLICIT NONE

!     Variable Declarations

REAL, INTENT( IN  ) :: xr, yr, zflagr, hes, szs, sys
REAL, INTENT( OUT ) :: hrval

REAL, PARAMETER :: EXPLIM = -50.0

! --- Declare local PRIME arrays for "3-source" data
REAL q2(3)
REAL(8) y2(3), sy2(3), z2(3), h2(3), sz2(3), qc2(3), fqcav !REAL(8) to match CAV_SRC

INTEGER is, n1, n2

REAL qtk, yterm, a0, v, vterm, emifac
REAL y, sy, sz, he, us, zflag

! --- PRIME ---------------------------------------------------
! --- When there is a building wake, consider treatment of mass in
! --- cavity as additional sources, or as only source
! --- Place selected plume data into transfer arrays (first element)

qtk=cmass_prm

q2(1)=qtk
y2(1)=DBLE(yr)
sy2(1)=DBLE(sys)
z2(1)= DBLE(zflagr)
h2(1)=DBLE(hes)
sz2(1)=DBLE(szs)
n1=1
n2=1

! ---    Define cavity source                              ---   CALL CAV_SRC
call CAV_SRC(DBLE(xr),DBLE(yr),DBLE(zflagr),fqcav,qc2,h2,y2,z2,sz2,sy2,n1,n2)
IF( fqcav>0.0 )THEN
! ---       Set source strengths
  q2(1)=qtk*(1.0-SNGL(fqcav))
  q2(2)=qtk*SNGL(fqcav*qc2(2))
  q2(3)=qtk*SNGL(fqcav*qc2(3))
END IF

hrval = 0.0

! --- Loop over 3 possible sources
DO is=n1,n2

! --- Transfer data for current source
qtk=q2(is)
y=SNGL(y2(is))
sy=SNGL(sy2(is))
zflag=SNGL(z2(is))
he=SNGL(h2(is))
sz=SNGL(sz2(is))
! ----------------------------------------------------------------------

emifac = 1. !or 3600?

us = ustack !Saved from above

YTERM = -0.5*(Y*Y)/(SY*SY)
IF (YTERM > EXPLIM) THEN

      V = 0.
!           Calculate the Vertical Term, V, for gases
!                 Calculate Concentration Form of V         ---   CALL VERT
      A0 = -0.5/(SZ*SZ)
      CALL VERT(HE,SZ,A0,ZFLAG,V)
!           Include SZ in the denomenator of V
      V = V/SZ


!           Complete VTERM (SZ already in denomenator of V)
      VTERM = V/(PI2*US*SY)

!           Check for Possible Underflow Condition
      IF (VTERM>0.0 .AND. (LOG(VTERM)+YTERM)>EXPLIM) THEN
         HRVAL = hrval + &
                 QTK * emifac * VTERM * EXP(YTERM)
      END IF

END IF

END DO

 999  RETURN
END

!==============================================================================

SUBROUTINE VERT(HEARG,SZARG,A0,ZARG,VOUT)

!USE MAIN1_INC
!USE MAIN2_INC
!USE MAIN3_INC
!***********************************************************************
!                 VERT Module of ISC2 Short Term Model - ISCST2
!
!        PURPOSE: Calculates Vertical Term for Use in Gaussian Plume Equation
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        MODIFIED BY R.W. Brode, PES, Inc. to use calling arguments - 9/30/94
!
!        MODIFIED BY D. Strimaitis, SRC (for Wet REMOVAL of Gases)
!
!        DATE:    November 8, 1993
!
!
!        INPUTS:  Plume Height
!                 Vertical Dispersion Parameter
!                 Stability Class
!                 Mixing Height
!                 Receptor Height Above Ground
!
!        OUTPUTS: Vertical Term, VOUT
!
!        CALLED FROM:   PSIMPL, PCOMPL, ASIMPL
!***********************************************************************

USE met_fi
USE sciprime_fi
USE constants_fd

IMPLICIT NONE

REAL, INTENT( IN  ) :: HEARG,SZARG,A0,ZARG
REAL, INTENT( OUT ) :: VOUT

REAL, PARAMETER :: EXPLIM = -50.0

!     Variable Declarations
INTEGER i
REAL    a1, a2, a3, a4, a5, a6, sum, twoizi, t, SRT2PI

!     Variable Initializations

VOUT = 0.0

SRT2PI = SQRT(PI2)

IF (ZARG == 0.0) THEN
!        Vertical Term for Case With No Flagpole Receptor
!   IF (STABLE .OR. ZI>=10000.) THEN
   IF (wts < 0. .OR. zi_rel >= 10000.) THEN
      A1 = A0 * HEARG * HEARG
      IF (A1 > EXPLIM)  VOUT = 2.*EXP(A1)
   ELSE IF ((SZARG/zi_rel) >= 1.6) THEN
      VOUT  = SRT2PI*(SZARG/zi_rel)
   ELSE
      A1 = A0 * HEARG * HEARG
      IF (A1 > EXPLIM)  VOUT = EXP(A1)
      SUM = 0.0
      DO 100 I = 1, 100
         T  = 0.0
         TWOIZI = 2.*FLOAT(I)*zi_rel
         A2 = A0 * (TWOIZI-HEARG) * (TWOIZI-HEARG)
         A3 = A0 * (TWOIZI+HEARG) * (TWOIZI+HEARG)
         IF (A2 > EXPLIM)  T = EXP(A2)
         IF (A3 > EXPLIM)  T = T + EXP(A3)
         SUM = SUM + T
         IF (ABS(T) <= 5.0E-9) THEN
!                 Exit Loop
            GOTO 200
         END IF
 100        CONTINUE
!           Calculate Total Vert. Term - (2.*) was Removed for Optimization
 200        VOUT  = 2.*(VOUT + SUM)
   END IF
ELSE
!        Vertical Term for Case of ZARG .NE. 0.0
   IF (wts < 0. .OR. zi_rel >= 10000.) THEN
      A1 = A0 * (ZARG-HEARG) * (ZARG-HEARG)
      A2 = A0 * (ZARG+HEARG) * (ZARG+HEARG)
      IF (A1 > EXPLIM)  VOUT = EXP(A1)
      IF (A2 > EXPLIM)  VOUT = VOUT + EXP(A2)
   ELSE IF (SZARG/zi_rel >= 1.6) THEN
      VOUT  = SRT2PI*(SZARG/zi_rel)
   ELSE
      A1 = A0 * (ZARG-HEARG) * (ZARG-HEARG)
      A2 = A0 * (ZARG+HEARG) * (ZARG+HEARG)
      IF (A1 > EXPLIM)  VOUT = EXP(A1)
      IF (A2 > EXPLIM)  VOUT = VOUT + EXP(A2)
      SUM = 0.0
      DO 300 I = 1, 100
         T  = 0.0
         TWOIZI = 2.*FLOAT(I)*zi_rel
         A3 = A0 * (ZARG-(TWOIZI-HEARG)) * (ZARG-(TWOIZI-HEARG))
         A4 = A0 * (ZARG+(TWOIZI-HEARG)) * (ZARG+(TWOIZI-HEARG))
         A5 = A0 * (ZARG-(TWOIZI+HEARG)) * (ZARG-(TWOIZI+HEARG))
         A6 = A0 * (ZARG+(TWOIZI+HEARG)) * (ZARG+(TWOIZI+HEARG))
         IF (A3 > EXPLIM)  T = T + EXP(A3)
         IF (A4 > EXPLIM)  T = T + EXP(A4)
         IF (A5 > EXPLIM)  T = T + EXP(A5)
         IF (A6 > EXPLIM)  T = T + EXP(A6)
         SUM = SUM + T
         IF (ABS(T) <= 1.0E-8) THEN
!                 Exit Loop
            GOTO 400
         END IF
 300        CONTINUE
 400        VOUT  = VOUT + SUM
   END IF
END IF
RETURN
END
