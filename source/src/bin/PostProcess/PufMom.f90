SUBROUTINE PufMom( ToolUserID )

!------ Compute overall centroid and moments
!       N.B. Includes all puffs

USE scipuff_fi
USE SCIPtool
USE localpuf

IMPLICIT NONE

INTEGER                      :: ToolUserID

INTEGER, PARAMETER           :: MAXN=20
CHARACTER(PATH_MAXLENGTH)    :: ifile, ofile
CHARACTER(1)                 :: answ
CHARACTER(4),DIMENSION(MAXN) :: namex

REAL, DIMENSION(3) :: xxrel, xbar, ubar, xfac, normal
REAL, DIMENSION(6) :: xmom

REAL(8), DIMENSION(3,3) :: amat, amat_t

INTEGER nvar, ncht, nchi, ncho, itxt, ios
INTEGER i, it, j, k, m, irv, nt, flag
REAL    sz, hp, hx, hy, fac, xmap, ymap
REAL    sumc, zrfl, ztop
REAL    frac, delz, delx, dely, sxx, sxy, sxz, syy, syz, szz
LOGICAL lerror, lexist, lrfl_top, lrfl_bot, lrotate

REAL, DIMENSION(:,:) , ALLOCATABLE :: rdata

TYPE( ppuffHeadT ) :: puffHead

INTEGER, EXTERNAL :: GetProjectPuffs
INTEGER, EXTERNAL :: GetProjectPlotTimes
LOGICAL, EXTERNAL :: check_slope, IsHazard

ToolUserID = 8642

itxt = -1
NPUF = 1

nvar = 13
nt   = 0
namex(1)  = 'time'
namex(2)  = 'x'
namex(3)  = 'y'
namex(4)  = 'z'
namex(5)  = 'sxx'
namex(6)  = 'sxy'
namex(7)  = 'sxz'
namex(8)  = 'syy'
namex(9)  = 'syz'
namex(10) = 'szz'
namex(11) = 'u'
namex(12) = 'v'
namex(13) = 'w'

title = 'Puff Moments'
ncht  = LEN_TRIM(title)

fac_rfl = 3.

flag = 0
flag = IBSET(flag,PPB_MSL)
flag = IBSET(flag,PPB_SLOPE)

WRITE(6,60)
60 FORMAT('Enter offsets for x, y, and z : ',$)
READ(5,*) xxrel(1),xxrel(2),xxrel(3)

WRITE(6,61)
61 FORMAT('Enter xmap and ymap for project (prj/meters) : ',$)
READ(5,*) xmap, ymap

xfac(1) = 1./xmap
xfac(2) = 1./ymap
xfac(3) = 1.

DO
  CALL get_file( 'Puff ',ifile,nchi )
  IF( nchi == 0 )GOTO 9999

  ifile = ADJUSTL(ifile)
  nchi  = LEN_TRIM(ifile)
  INQUIRE( file=TRIM(ifile),EXIST=lexist )
  IF( lexist )THEN
    EXIT
  ELSE
    WRITE(6,*)'Cannot find ',TRIM(ifile)
    CYCLE
  END IF

END DO

DO
  WRITE(6,'(/,"Use TXT:filename for text output; or use CSV extension",/)')
  CALL get_file( 'Output ',ofile,ncho )
  IF( ncho == 0 )GOTO 9999
  ofile = ADJUSTL(ofile)
  IF (ofile(1:4) == 'TXT:') THEN
    itxt = 1
    ofile = ofile(5:LEN_TRIM(ofile))
  ELSE IF( INDEX(ofile,'.CSV') > 0 .OR. INDEX(ofile,'.csv') > 0 ) THEN
    itxt = 2
  ELSE
    itxt = 0
  END IF
  INQUIRE( file=TRIM(ofile),EXIST=lexist )
  IF(lexist)THEN
    WRITE(6,'(" ",A," already exists",/," Delete file ? ",$)')TRIM(ofile)
    READ(5,'(A)',err=9999,END=9999) answ
    CALL cupper( answ )
    IF( answ(1:1) == 'Y' )THEN
      OPEN( 1,FILE=TRIM(ofile) )
      CLOSE( 1,STATUS='DELETE' )
      INQUIRE( file=TRIM(ofile),exist=lerror )
    ELSE
      CYCLE
    END IF
  ELSE
    lerror = .FALSE.
  END IF
  IF( lerror )THEN
    WRITE(6,*)'Cannot delete ',TRIM(ofile)
    CYCLE
  ELSE
    EXIT
  END IF

END DO

puffHead%project%name    = ifile(1:nchi-4)
puffHead%project%path    = ''
puffHead%Project%ID      = 0
puffHead%Project%version = 0

irv = GetProjectPlotTimes( puffHead%project )
IF( irv /= SCIPSuccess )THEN
  WRITE(6,*)' Error reading puff times'
  GOTO 9999
ELSEIF( nTimePuff == 0 )THEN
  WRITE(6,*)' Cannot find any valid puff times'
  GOTO 9999
END IF

xfac(1) = 1./xmap
xfac(2) = 1./ymap
xfac(3) = 1.

ALLOCATE( rdata(MAXN,nTimePuff),STAT=ios )

!----- Loop over times

DO it = 1,nTimePuff

  irv = GetProjectPuffs( ToolUserID,puffHead,it,.TRUE.,.FALSE.,flag )
  IF( irv /= SCIPSuccess )THEN
    WRITE(6,*)' Error reading puff at time break ',it
    GOTO 9999
  END IF

  IF( npuf > 0 )THEN

    sumc = 0.
    DO i = 1,3
      xbar(i) = 0.
      ubar(i) = 0.
    END DO
    DO i = 1,6
      xmom(i)  = 0.
    END DO

!----- Loop over puffs

    DO i = 1,npuf

      CALL subr_xx( puff(i),xxrel ) !Subtract reference location from puff centroids

      sz = SQRT(puff(i)%szz)
      lrfl_bot = .FALSE.
      lrfl_top = .FALSE.
      hx = 0.; hy = 0.

      hp = puff(i)%sr                     !Ground elevation (MSL)
      hx = puff(i)%axx; hy = puff(i)%ayy  !Terrain slopes

!----- Check for ground proximity
!      If so, check if puff needs to be rotated

      IF( puff(i)%zbar-hp < fac_rfl*sz)THEN

        lrfl_bot = .TRUE.

        IF( check_slope(hx,hy) )THEN
          fac = 1.0/SQRT(1.0+hx*hx+hy*hy)
          normal(1) = -fac*hx
          normal(2) = -fac*hy
          normal(3) =  fac
          CALL set_rot_norm( normal,amat ) !Rotate into coord. aligned with terrain
          amat_t = TRANSPOSE( amat )
          CALL apply_rot_norm( puff(i),amat,amat_t )
          sz = SQRT(puff(i)%szz)
          lrotate = .TRUE.
        ELSE
          lrotate = .FALSE.
        END IF

      END IF

      CALL siginv( puff(i) )

!----- Check for inversion reflection

      ztop = puff(i)%zbar + 10.*sz; zrfl = 0.
      IF( puff(i)%zc > 0. )THEN
        IF( puff(i)%zc - puff(i)%zbar < fac_rfl*sz)THEN
          lrfl_top = .TRUE.
          ztop = puff(i)%zc
          zrfl = ztop
        END IF
      END IF

!----- Re-define centroid and moments accounting for reflections

      IF( lrfl_bot .OR. lrfl_top )THEN

        CALL chop_puff( puff(i),hp,ztop,zrfl,hp,lrfl_top,lrfl_bot,frac, &
                    delz,delx,dely,sxx,sxy,sxz,syy,syz,szz )
        puff(i)%sxx = sxx
        puff(i)%sxy = sxy
        puff(i)%sxz = sxz
        puff(i)%syy = syy
        puff(i)%syz = syz
        puff(i)%szz = szz

        IF( lrotate )THEN
          CALL RotateDel( delx,dely,delz,amat_t )
          CALL apply_rot_norm( puff(i),amat_t,amat )
        END IF

        puff(i)%xbar = puff(i)%xbar + DBLE(delx*xmap)
        puff(i)%ybar = puff(i)%ybar + DBLE(dely*ymap)
        puff(i)%zbar = puff(i)%zbar + DBLE(delz)

      END IF

!----- Add puff contribution

      CALL get_puff_mom( puff(i),puff(i)%c,xbar,xmom,xfac )

      sumc = sumc + puff(i)%c
      ubar(1) = ubar(1) + puff(i)%c*puff(i)%uo
      ubar(2) = ubar(2) + puff(i)%c*puff(i)%vo
      ubar(3) = ubar(3) + puff(i)%c*puff(i)%wo

    END DO

!----- Centroid

    DO j = 1,3
      xbar(j) = xbar(j)/sumc
      ubar(j) = ubar(j)/sumc
    END DO

!----- 2nd moments

    m = 0
    DO j = 1,3
      DO k = j,3
        m = m + 1
        xmom(m) = xmom(m)/sumc - xbar(j)*xfac(j)*xbar(k)*xfac(k)
      END DO
    END DO

!----- Output

    nt = nt + 1
    rdata(1,nt) = t
    DO i = 1,3
      rdata(i+1,nt) = xbar(i) + xxrel(i)
    END DO
    DO i = 1,6
      rdata(i+4,nt) = xmom(i)
    END DO
    DO i = 1,3
      rdata(i+10,nt) = ubar(i)
    END DO

  END IF

END DO

IF( itxt == 1 )THEN               !ASCII file
  CALL write_ascii( 3,TRIM(ofile),MAXN,nvar,nt,namex,title,rdata,0 )
ELSE IF ( itxt == 2 )THEN         !CSV
  CALL write_csv( 3,TRIM(ofile),MAXN,nvar,nt,namex,title,rdata,0 )
ELSE                              !XPP
  CALL write_xpp( 3,TRIM(ofile),MAXN,nvar,nt,namex,title,rdata,0 )
END IF

9999  CONTINUE

CALL ClearProjectPuffs()

RETURN
END

!----------------------------------------------------------------------

SUBROUTINE get_puff_mom( p,mass,xbar,xmom,xfac )

USE struct_fd

IMPLICIT NONE

TYPE (puff_str) :: p
REAL :: mass
REAL, DIMENSION(3) :: xbar, xfac, xcent
REAL, DIMENSION(6) :: xmom, sig
INTEGER :: j, k, m

xcent(1) = SNGL(p%xbar)
xcent(2) = SNGL(p%ybar)
xcent(3) = p%zbar
sig(1)   = p%sxx
sig(2)   = p%sxy
sig(3)   = p%sxz
sig(4)   = p%syy
sig(5)   = p%syz
sig(6)   = p%szz

m = 0
DO j = 1,3
  xbar(j) = xbar(j) + mass*xcent(j)
  DO k = j,3
    m = m + 1
    xmom(m) = xmom(m) + mass * &
              ( xcent(j)*xfac(j)*xcent(k)*xfac(k)+ sig(m) )
  END DO
END DO

RETURN
END

!----------------------------------------------------------------------

SUBROUTINE subr_xx( p,xxrel )

USE struct_fd

IMPLICIT NONE

TYPE( puff_str ),   INTENT( INOUT ) ::  p
REAL, DIMENSION(3), INTENT( IN    ) ::  xxrel

p%xbar = p%xbar - DBLE(xxrel(1))
p%ybar = p%ybar - DBLE(xxrel(2))
p%zbar = p%zbar - xxrel(3)

RETURN
END

! -----------------------------------------------------------------------

SUBROUTINE getz_refl( zc,zinv,szz,zrfl )

IMPLICIT NONE

REAL :: zc, zinv, szz, zrfl, sz
REAL :: rt2pi, arg1, arg2, arg3, arg4
REAL, EXTERNAL :: erfc

sz = SQRT(szz)
if (zinv == 0.) zinv = zc+10.*sz
arg1 = zc*zc/(2.*szz)
arg2 = (zinv - zc)*(zinv - zc)/(2.*szz)
arg3 = SQRT(arg1)
arg4 = SQRT(arg2)
rt2pi = SQRT(2.*3.14159265)

zrfl = 2.*szz*(EXP(-arg1) - EXP(-arg2)) &
             + zc*rt2pi*sz*(1. - erfc(arg4) - erfc(arg3)) &
             + zinv*rt2pi*sz*erfc(arg4)

zrfl = zrfl/(rt2pi*sz)

RETURN
END

