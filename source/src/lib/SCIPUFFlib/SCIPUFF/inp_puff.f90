SUBROUTINE input_puff( initRel,nun,iflag,npo )

USE scipuff_fi
USE files_fi
USE UtilMtlAux

! This routine reads a CLOUDTRANS puff file, rebins subgroups
! as necessary and creates a set of puffs to be released by SCIPUFF
!
! nun   : Input Unit number
! iflag : 0 -> Instantaneous -> Looking for MASS
!         1 -> Continuous    -> Looking for RATE = MASS/SEC
! npo   : Number of puffs

IMPLICIT NONE

TYPE( ReleaseT ), INTENT ( IN ) :: initRel
INTEGER, INTENT( IN  ) :: nun, iflag
INTEGER, INTENT( OUT ) :: npo

REAL, PARAMETER :: EPS = 1.E-3

CHARACTER(16), DIMENSION(NP_ALL) :: dfnam, dfunit
CHARACTER(16), DIMENSION(ntypm)  :: cnami
CHARACTER(80)  line
CHARACTER(80)  kwrd

TYPE( puff_str        ) :: pin
TYPE( puff_dynamics   ) :: pd
TYPE( liquid_material ) :: pmatl
TYPE( puff_liquid     ) :: pq
TYPE( part_material   ) :: pmatpart
TYPE( puff_material   ) :: pmatlIn

INTEGER mpuf, naux
INTEGER i, j, npi, ndp, ntypi, nh, in, ip, isg
INTEGER imas, irat, ix, iy, iz, isig, imxx, imxy, imxz
INTEGER imyy, imyz, imzz, isiv, isih, isi2, icfo
INTEGER isigx, isigy, isigz, idry, imom, ibuoy
INTEGER imomx, imomy
INTEGER ivar
INTEGER ic, isiz, iloc, ityp, i0, j0
INTEGER ntot, nr,  nrd, ncol, ir, nch, icls, nsg
INTEGER narg, ios, ID
REAL    xfac, yfac, fac, rho
REAL    rhod, tamb, psat, hvd, difd

INTEGER, DIMENSION(ntypm)        :: nsgi, imat
INTEGER, DIMENSION(MAXSGP,ntypm) :: nbins, nbin, nbinl

REAL, DIMENSION(MAXSGP+1,ntypm)  :: sgb
REAL, DIMENSION(MAXSGP+1)        :: pbounds
REAL, DIMENSION(ntypm)           :: dens !Array for dust density
REAL, DIMENSION(MAXSGP*10,ntypm) :: bfac
REAL, DIMENSION(NP_ALL)          :: dati

LOGICAL lmom, lbound, lend, lset_sv, lset_si, lset_si2, lerr

REAL,    EXTERNAL :: material_density
LOGICAL, EXTERNAL :: IsParticle, PuffRealizable
LOGICAL, EXTERNAL :: IsLiquid, IsWetParticle
INTEGER, EXTERNAl :: allocatePuffAux
INTEGER, EXTERNAl :: deallocatePuffAux

INTERFACE

  SUBROUTINE surface_dose( p,sdat,srfI,zsrf,sblk,spuff,stype,mask )
    USE struct_fd
    USE surface_fd
    USE srfparam_fd
    TYPE( puff_str ),                   INTENT( IN ) :: p
    INTEGER,                            INTENT( IN ) :: srfI
    INTEGER, DIMENSION(*),              INTENT( IN ) :: stype
    REAL,                               INTENT( IN ) :: zsrf
    REAL, DIMENSION(ISRF_TOT),          INTENT( IN ) :: sdat
    TYPE( sfield_block ), DIMENSION(*), INTENT( IN ) :: sblk
    TYPE( sfield_puff ),  DIMENSION(*), INTENT( IN ) :: spuff
    TYPE( mask_fd),       OPTIONAL,     INTENT( IN ) :: mask
  END SUBROUTINE surface_dose

END INTERFACE

!------ Initialize

NULLIFY(pin%aux)

!------ Skip over header

READ(nun,101,ERR=1000,END=1000) nh
DO i = 1,nh-1
  READ(nun,100,ERR=1000,END=1000)
END DO

!------ Read initial data

READ(nun,101,ERR=1000,END=1000) npi !No. Input puffs
IF( npi+npuf > MAXPUF )THEN
  nError   = SZ_ERROR
  eRoutine = 'input_puff'
  eMessage = 'Insufficient space in puff array to create puffs from CLOUDTRANS file'
  CALL ReportFileName( eInform,'File=',file_tmp )
  WRITE(eAction,'(A,I9)',IOSTAT=ios)'Try increasing the Maximum Puff Limit. Current limit=',MAXPUF
  GOTO 9999
END IF

READ(nun,101,ERR=1000,END=1000) ndp !Data fields/puff
IF( ndp > NP_ALL )THEN
  nError   = SZ_ERROR
  eRoutine = 'input_puff'
  eMessage = 'Too many data fields/puff in CLOUDTRANS data'
  WRITE(eInform,'(A,I5)',IOSTAT=ios) 'Maximum number is ',NP_ALL
  CALL ReportFileName( eAction,'File=',file_tmp )
  GOTO 9999
END IF

READ(nun,103,ERR=1000,END=1000) (dfnam(i),i=1,ndp)  !Data field names
READ(nun,103,ERR=1000,END=1000) (dfunit(i),i=1,ndp) !Data units
READ(nun,101,ERR=1000,END=1000) ntypi               !No. Input material types
IF( ntypi > ntypm )THEN
  nError   = SZ_ERROR
  eRoutine = 'input_puff'
  eMessage = 'Too many material types in CLOUDTRANS data'
  WRITE(eInform,'(A,I5)')'Maximum number is ',ntypm
  CALL ReportFileName( eAction,'File=',file_tmp )
  GOTO 9999
END IF

READ(nun,101,ERR=1000,END=1000) (nsgi(i),i=1,ntypi) !No. Subgroups/type

i = 0
DO
  READ(nun,'(A)',ERR=1000,END=1000) line               !Material names
  CALL get_next_data( 0,line(24:),nch,kwrd,narg,cnami(i+1),ntypi-i,lerr )
  IF( lerr )THEN
    eMessage = 'Error reading material names on Cloudtrans file'
    GOTO 9998
  END IF
  i = i + narg
  IF( i == ntypi )THEN
    EXIT
  ELSE IF( i > ntypi )THEN
    eMessage = 'Too many material names on Cloudtrans file'
    GOTO 9998
  END IF
END DO

READ(nun,104,ERR=1000,END=1000) (dens(i),i=1,ntypi) !Material densities

!------ Check initial data

DO i = 1,ntypi    !Loop over input materials
  CALL cupper( cnami(i) )
  imat(i) = 0
  DO j = 1,ntypm
    IF( cnami(i) == material(j)%cmat )imat(i) = j
  END DO
  IF( imat(i) <= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'input_puff'
    eMessage = 'Unknown material in CLOUDTRANS data: '//TRIM(cnami(i))
    CALL ReportFileName( eInform,'File=',file_tmp )
    eAction = 'Material in CLOUDTRANS file must match one of the project materials'
    GOTO 9999
  END IF
END DO

DO i = 1,ntypi    !Loop over input materials

  ityp = imat(i)
  icls = material(ityp)%icls
  CALL CheckSubgroups( nsgi(i),material(ityp) )
  IF( nError /= NO_ERROR )THEN
    eRoutine = 'input_puff'
    CALL ReportFileName( eInform,'CLOUDTRANS file=',file_tmp )
    GOTO 9999
  END IF

!------ All this to read bin boundaries!

  READ(nun,100,ERR=1000,END=1000) !Subgroup header
  ntot = nsgi(i)+1       !No. of bin boundaries to be read
  nr   = (ntot+4)/5      !No. of records to read (assuming boundaries)
  j0   = 0

  DO ir = 1,nr

    READ(nun,100,ERR=1000,END=1000) line
    CALL cupper( line )
    IF( line(1:4) == 'PUFF' .OR. line(1:4) == 'SUBG' )THEN !Check for end of
      lend = .TRUE.                                        !bin boundary data
    ELSE
      lend = .FALSE.
    END IF
    nch = LEN_TRIM(line)
    IF( ir == nr )THEN
      nrd  = ntot - (nr-1)*5
      ncol = (nrd-1)*16 + 1
      IF( nch < ncol-16 .OR. (lend .AND. nrd > 1) )THEN
        nError = RD_ERROR
        eRoutine = 'input_puff'
        eMessage = 'Error reading subgroup boundaries in '// &
                   'CLOUDTRANS data'
        eInform  = 'Insufficient boundary values read'
        CALL ReportFileName( eAction,'File=',file_tmp )
        GOTO 9999
      ELSE IF( nch < ncol .OR. (lend .AND. nrd == 1) )THEN
        lbound = .FALSE.     !File contains bin centroids
        nrd    = nrd - 1
      ELSE
        lbound = .TRUE.      !File contains bin boundaries
      END IF
    ELSE
      nrd = 5
      ncol = (nrd-1)*16 + 1
      IF( nch < ncol .OR. lend )THEN
        nError = RD_ERROR
        eRoutine = 'input_puff'
        eMessage = 'Error reading subgroup boundaries in '// &
                   'CLOUDTRANS data'
        eInform  = 'Insufficient boundary values read'
        CALL ReportFileName( eAction,'File=',file_tmp )
        GOTO 9999
      END IF
    END IF
    IF( nrd > 0 )THEN
      READ(line,105,ERR=1000,END=1000) (sgb(j,i),j=j0+1,j0+nrd)
    END IF
    j0 = j0 + 5
  END DO

  IF( lend )BACKSPACE( nun )

  IF( lbound )THEN
    DO j = 2,ntot
      IF( sgb(j,i) <= sgb(j-1,i) )THEN
        nError = RD_ERROR
        eRoutine = 'input_puff'
        eMessage = 'Subgroup boundaries must increase monotonically'// &
                   ' in CLOUDTRANS data'
        CALL ReportFileName( eAction,'File=',file_tmp )
        GOTO 9999
      END IF
    END DO
  END IF

END DO

100 FORMAT(A)
101 FORMAT((23X,5(I6,10X)))
103 FORMAT((23X,4(2X,A8)))
104 FORMAT((23X,4(E16.0)))
105 FORMAT(5E16.0)

!------ Check Data fields

imas = 0
irat = 0
ix   = 0
iy   = 0
iz   = 0
isig = 0
imxx = 0
imxy = 0
imxz = 0
imyy = 0
imyz = 0
imzz = 0
isiv = 0
isih = 0
isi2 = 0
icfo = 0
isigx= 0
isigy= 0
isigz= 0
idry = 0
imom = 0
ibuoy= 0
imomx= 0
imomy= 0

ivar = 0

lset_sv  = .FALSE.
lset_si  = .FALSE.
lset_si2 = .FALSE.

DO i = 1,ndp

  CALL cupper( dfnam(i) )

  SELECT CASE( TRIM(dfnam(i)) )
    CASE( '<C>', 'MASS' )
      imas = i
    CASE( 'RATE' )
      irat = i
    CASE( 'X' )
      ix = i
    CASE( 'Y' )
      iy = i
    CASE( 'Z' )
      iz = i
    CASE( 'SIG' )
      isig = i
    CASE( 'MXX','SXX' )
      imxx = i
    CASE( 'MXY','SXY' )
      imxy = i
    CASE( 'MXZ','SXZ' )
      imxz = i
    CASE( 'MYY','SYY' )
      imyy = i
    CASE( 'MYZ','SYZ' )
      imyz = i
    CASE( 'MZZ','SZZ' )
      imzz = i
    CASE( 'SIGMA-X' )
      isigx = i
    CASE( 'SIGMA-Y' )
      isigy = i
    CASE( 'SIGMA-Z' )
      isigz = i
    CASE( 'SLV' )
      isiv = i
    CASE( 'SLH' )
      isih = i
    CASE( 'SL2' )
      isi2 = i
    CASE( 'CFO' )
      icfo = i
    CASE( 'DRYFRAC' )
      idry = i
    CASE( 'WMOM' )
      imom = i
    CASE( 'BUOY' )
      ibuoy = i
    CASE( 'UMOM' )
      imomx = i
    CASE( 'VMOM' )
      imomy = i
    CASE( '<C2>' )
      ivar = i
  END SELECT

END DO

IF( iflag == 0 )THEN
  IF( imas <= 0 )THEN
    eMessage = 'No mass field data in CLOUDTRANS data'
    GOTO 9998
  END IF
  ic = imas
ELSE
  IF( irat <= 0 )THEN
    eMessage = 'No release rate data in CLOUDTRANS data'
    GOTO 9998
  END IF
  ic = irat
END IF

isiz = imxx*imyy*imzz + isigx*isigy*isigz
IF( isig <= 0 .AND. isiz <= 0 )THEN
  eMessage = 'No moment field data in CLOUDTRANS data'
  GOTO 9998
END IF

iloc = ix*iy*iz
IF( iloc <= 0 )THEN
  eMessage = 'No location data in CLOUDTRANS data'
  GOTO 9998
END IF

lset_sv = isiv <= 0
isiz    = isiv+isih+isi2
IF( isiz <= 0 .AND. isig > 0 )THEN
  isiv = isig
  isih = isig
  isi2 = isig
ELSE IF( isiz > 0 )THEN
  IF( isi2 <= 0 )THEN
    IF( isih /= 0 )THEN
      isi2 = isih
    ELSE
      isi2 = isiv
      lset_si2 = lset_sv
    END IF
  END IF
  IF( isih <= 0 )THEN
    IF( isi2 /= 0 )THEN
      isih = isi2
    ELSE
      isih = isiv
      lset_si = lset_sv
    END IF
  END IF
  IF( isiv <= 0 )THEN
    IF( isih /= 0 )THEN
      isiv = isih
    ELSE
      isiv = isi2
    END IF
  END IF
END IF

lmom = isig <= 0 .AND. isigx <= 0

!------ Rebin Particle groups

DO i = 1,ntypi

  ityp = imat(i)
  icls = material(ityp)%icls
  rho  = material_density( material(ityp) )
  IF( ABS(rho-dens(i)) > EPS*rho )THEN
    nError   = WN_ERROR
    eRoutine = 'input_puff'
    eMessage = 'Material Densities Differ : MatDef / Release'
    WRITE(eInform,'(A,1P,2E15.5)')'Densities : ',rho,dens(i)
    eAction = 'First value (MatDef) will be used'
    CALL WarningMessage( .TRUE. )
    IF( nError /= NO_ERROR )GOTO 9999
  END IF

  IF( IsParticle(icls) .OR. IsLiquid(icls) .OR. IsWetParticle(icls) )THEN !Groups (Liq,Part)

    CALL get_bounds( material(ityp),nsg,pbounds )

    IF( lbound )THEN
      CALL check_bounds( pbounds,nsg,sgb(1,i),nsgi(i),icls )
      CALL rebin( sgb(1,i),nsgi(i),pbounds,nsg, &
                  bfac(1,i),nbin(1,i),nbins(1,i),nbinl(1,i) )
    ELSE
      CALL rebin_s( sgb(1,i),nsgi(i),pbounds,nsg, &
                    bfac(1,i),nbin(1,i),nbins(1,i),nbinl(1,i) )
    END IF

!------ check if output bins cover input bins

    IF( lbound )THEN
      IF( (pbounds(1)      -sgb(1,i))       > 0.01 * (sgb(2        ,i)-sgb(1      ,i)) .OR. &
          (sgb(nsgi(i)+1,i)-pbounds(nsg+1)) > 0.01 * (sgb(nsgi(i)+1,i)-sgb(nsgi(i),i)) )THEN
        nError   = WN_ERROR
        eRoutine = 'input_puff'
        eMessage = 'Bins for '//TRIM(material(ityp)%cmat)//' do not cover range on CLOUDTRANS file'
        WRITE(eInform,FMT='("Matl range:",2ES9.2," Cloudtrans range:",2ES9.2)',IOSTAT=ios ) &
                                  pbounds(1),pbounds(nsg+1),sgb(1,i),sgb(nsgi(i)+1,i)
        CALL WarningMessage( .TRUE. )
        IF( nError /= NO_ERROR )GOTO 9999
      END IF
    ELSE
      IF( (pbounds(1)    -sgb(1,i))       > 0.01 * (pbounds(2)    -pbounds(1)) .OR. &
          (sgb(nsgi(i),i)-pbounds(nsg+1)) > 0.01 * (pbounds(nsg+1)-pbounds(nsg)) )THEN
        nError   = WN_ERROR
        eRoutine = 'input_puff'
        eMessage = 'Bins for '//TRIM(material(ityp)%cmat)//' do not cover range on CLOUDTRANS file'
        WRITE(eInform,FMT='("Matl range:",2ES9.2," Cloudtrans range:",2ES9.2)',IOSTAT=ios ) &
                                  pbounds(1),pbounds(nsg+1),sgb(1,i),sgb(nsgi(i),i)
        CALL WarningMessage( .TRUE. )
        IF( nError /= NO_ERROR )GOTO 9999
      END IF
    END IF


  ELSE !No groups (Gases)

    DO in = 1,nsgi(i)
      nbin(in,i) = 1
    END DO

  END IF

END DO

!------ Read puff data and initialize

mpuf = npuf

READ(nun,100,ERR=1000,END=1000) !Puff data header

DO ip = 1,npi

  READ(nun,*,ERR=1000,END=1000) i,ityp,isg,(dati(i),i=1,ndp)

!-------- Set puff class

  icls = material(imat(ityp))%icls

!-------- Zero out temporary puff

  CALL zero_puff( pin )

!-------- Mass

  pin%c = dati(ic)
  IF( pin%c <= 0. )THEN
    nError   = IV_ERROR
    eRoutine = 'input_puff'
    eMessage = 'Invalid puff mass in CLOUDTRANS file'
    eInform  = 'Mass must be greater than zero'
    CALL ReportFileName( eAction,'File=',file_tmp )
    GOTO 9999
  END IF

!-------- Centroid

  xfac = 1.0
  yfac = 1.0
  CALL cupper( dfunit(ix) )
  CALL cupper( dfunit(iy) )
  IF( dfunit(ix) /= 'M' .OR. dfunit(iy) /= 'M' )THEN
    nError   = UK_ERROR
    eRoutine = 'input_puff'
    eMessage = 'Puff centroids in CLOUDTRANS file must be in meters'
    eInform  = 'Centroids are relative to location in scenario file'
    CALL ReportFileName( eAction,'File=',file_tmp )
    GOTO 9999
  END IF

  pin%xbar = DBLE(dati(ix)*xfac)
  pin%ybar = DBLE(dati(iy)*yfac)
  pin%zbar = dati(iz)

  IF( icfo <= 0 )THEN
    pin%cfo = 1.
  ELSE
    pin%cfo = dati(icfo)
  END IF

!-------- Moments

  IF( lmom )THEN
    pin%sxx = dati(imxx)
    pin%syy = dati(imyy)
    pin%szz = dati(imzz)
    IF( imxy > 0 )pin%sxy = dati(imxy)
    IF( imxz > 0 )pin%sxz = dati(imxz)
    IF( imyz > 0 )pin%syz = dati(imyz)
  ELSE
    IF( isigx > 0 )THEN
      pin%sxx = dati(isigx)**2
      pin%syy = dati(isigy)**2
      pin%szz = dati(isigz)**2
    ELSE
      pin%sxx = dati(isig)**2
      pin%syy = dati(isig)**2
      pin%szz = dati(isig)**2
    END IF
  END IF

  IF( .NOT.PuffRealizable(pin%sxx,pin%sxy,pin%sxz, &
                          pin%syy,pin%syz,pin%szz) )THEN
    nError   = IV_ERROR
    eRoutine = 'input_puff'
    eMessage = 'Invalid puff diagonal moments in CLOUDTRANS file'
    eInform  = 'non-realizable moments'
    CALL ReportFileName( eAction,'File=',file_tmp )
    CALL dump_puff( ip,pin )
    GOTO 9999
  END IF

  CALL siginv( pin )

!------- Scales

  IF( isih > 0 )THEN
    pin%si = dati(isih)
  ELSE
    pin%si = SQRT(MIN(pin%sxx,pin%syy))
  END IF
  IF( isi2 > 0 )THEN
    pin%si2 = dati(isi2)
  ELSE
    pin%si2 = SQRT(MAX(pin%sxx,pin%syy))
  END IF
  IF( isiv > 0 )THEN
    pin%sv = dati(isiv)
  ELSE
    pin%sv = SQRT(pin%szz)
  END IF
  IF( pin%si <= 0. .OR. pin%si2 <= 0. .OR. pin%sv  <= 0. )THEN
    nError   = IV_ERROR
    eRoutine = 'input_puff'
    eMessage = 'Invalid puff internal scales in CLOUDTRANS file'
    eInform  = 'Internal scales must be greater than zero'
    CALL ReportFileName( eAction,'File=',file_tmp )
    GOTO 9999
  END IF

!------ PARTICLE, WET-PARTICLE - Rebin

  IF( IsParticle(icls) .OR. IsWetParticle(icls)  )THEN
    IF( IsWetParticle(icls) .AND. idry /= 0 )THEN
      IF( dati(idry) == 1.0 )THEN
        nsg = 0
      ELSE IF( dati(idry) > 0. .AND. dati(idry) < 1.0 )THEN
        nsg  = MAX(GetSubgroups(material(imat(ityp)),mat_aux),1)
      ELSE
        nError   = IV_ERROR
        eRoutine = 'input_puff'
        eMessage = 'Invalid dry mass fraction in CLOUDTRANS file'
        eInform  = 'Mass fraction must be > 0. and <= 1.'
        CALL ReportFileName( eAction,'File=',file_tmp )
        GOTO 9999
      END IF
    ELSE
      nsg = 0
    END IF
    naux = typeID(material(imat(ityp))%ioffp + 1 + nsg)%npaux

    IF( isg < 1 .OR. isg > nsgi(ityp) )THEN
      nError   = UK_ERROR
      eRoutine = 'input_puff'
      WRITE(eInform,'(I0)')ip
      eMessage = 'Puff subgroup out of bounds for puff '//TRIM(ADJUSTL(eInform))//'.'
      CALL formatSubGroupError(eInform,1,nsgi(ityp),isg)
      CALL ReportFileName( eAction,'File=',file_tmp )
      GOTO 9999
    END IF

    IF( nbin(isg,ityp) > 0 )THEN
      i0 = nbinl(isg,ityp)
      j0 = nbins(isg,ityp)
      DO j = 1,nbin(isg,ityp)
        CALL check_newpuff()
        IF( nError /= NO_ERROR )GOTO 9997
        npuf = npuf + 1
        puff(npuf) = pin                             !pin has no aux array
        puff(npuf)%naux = naux
        ios = allocatePuffAux( puff(npuf) )          !now add aux array
        IF( ios /= 0 )GOTO 9995
        fac = bfac(i0+j-1,ityp)
        CALL scale_puff( puff(npuf),fac )
        puff(npuf)%ityp = material(imat(ityp))%ioffp + j0 + j - 1 + nsg
        IF( nsg > 0 )THEN
          CALL get_liquid( puff(npuf),pq )
          CALL get_puff_material( puff(npuf)%ityp-nsg,pmatlIn )
          pmatpart = TRANSFER(pmatlIn,pmatpart)
          ID = material(imat(ityp))%AuxMatID
          IF( ID /= NOT_SET_I )THEN
            tamb = 300.
            CALL lqd( WetPartLiquidMat(ID),tamb,rhod,psat,hvd,difd )
          ELSE
            rhod = RHO_WATER
          END IF
          fac = pmatpart%rho/rhod
          fac = (1.0+fac*(1.0/dati(idry)-1.0))**0.3333333
          pq%d    = pmatpart%dbar * fac
          pq%sigd = (pmatpart%dmax-pmatpart%dmin)/(2.*SQRT3) * fac
          pq%tevap = 0.0
          pq%t    = 0.0
          pq%ccs  = puff(npuf)%c
          CALL put_liquid( puff(npuf),pq )
        END IF

      END DO
    END IF

!-------- LIQUIDS - Rebin

  ELSE IF( IsLiquid(icls) )THEN

    IF( isg < 0 .OR. isg > nsgi(ityp) )THEN
      nError   = UK_ERROR
      eRoutine = 'input_puff'
      WRITE(eInform,'(I0)')ip
      eMessage = 'Puff subgroup out of bounds for puff '//TRIM(ADJUSTL(eInform))//'.'
      CALL formatSubGroupError(eInform,0,nsgi(ityp),isg)
      CALL ReportFileName( eAction,'File=',file_tmp )
      GOTO 9999
    END IF

    IF( isg == 0 )THEN !Vapor puff
      naux = typeID(material(imat(ityp))%ioffp + 1)%npaux
      CALL check_newpuff()
      IF( nError /= NO_ERROR )GOTO 9997
      npuf = npuf + 1
      puff(npuf) = pin                                  !pin has no aux array
      puff(npuf)%naux = naux
      ios = allocatePuffAux( puff(npuf) )              !now add aux array
      IF( ios /= 0 )GOTO 9995
      puff(npuf)%ityp = material(imat(ityp))%ioffp + 1
      IF( dynamic )THEN
        IF( imom /= 0 .OR. imomx /= 0 .OR. imomy /= 0 .OR. ibuoy /= 0 )THEN
          CALL get_dynamics( puff(npuf),pd )
          IF( imom  /= 0 )pd%w  = dati(imom)
          IF( imomx /= 0 )pd%un = dati(imomx)
          IF( imomy /= 0 )pd%vn = dati(imomy)
          IF( ibuoy /= 0 )pd%t  = dati(ibuoy)
          CALL put_dynamics( puff(npuf),pd )
        END IF
      END IF

    ELSE !Liquid puff group
      naux = typeID(material(imat(ityp))%ioffp + 2)%npaux
      IF( nbin(isg,ityp) > 0 )THEN
        i0 = nbinl(isg,ityp)
        j0 = nbins(isg,ityp)
        DO j = 1,nbin(isg,ityp)
          CALL check_newpuff()
          IF( nError /= NO_ERROR )GOTO 9997
          npuf = npuf + 1
          puff(npuf) = pin                              !pin has no aux array
          puff(npuf)%naux = naux
          ios = allocatePuffAux( puff(npuf) )           !now add aux array
          fac = bfac(i0+j-1,ityp)
          CALL scale_puff( puff(npuf),fac )
          IF( ios /= 0 )GOTO 9995
          puff(npuf)%ityp = material(imat(ityp))%ioffp + j0 + j
          CALL get_puff_material( puff(npuf)%ityp,pmatlIn )
          pmatl = TRANSFER(pmatlIn,pmatl)
          pq%d    = pmatl%dbar
          pq%sigd = (pmatl%dmax-pmatl%dmin)/(2.*SQRT3)
          pq%tevap = 0.0
          pq%t    = 0.0
          pq%ccs  = 0.0
          CALL put_liquid( puff(npuf),pq )
        END DO
      END IF
    END IF

!-------- GAS

  ELSE
    naux = typeID(material(imat(ityp))%ioffp + 1)%npaux !Gas
    CALL check_newpuff()
    IF( nError /= NO_ERROR )GOTO 9997
    npuf = npuf + 1
    puff(npuf) = pin                               !pin has no aux array
    puff(npuf)%naux = naux
    ios = allocatePuffAux( puff(npuf) )            !now add aux array
    IF( ios /= 0 )GOTO 9995
    puff(npuf)%ityp = material(imat(ityp))%ioffp + 1
    IF( dynamic )THEN
      IF( imom /= 0 .OR. imomx /= 0 .OR. imomy /= 0 .OR. ibuoy /= 0 )THEN
        CALL get_dynamics( puff(npuf),pd )
        IF( imom  /= 0 )pd%w  = dati(imom)
        IF( imomx /= 0 )pd%un = dati(imomx)
        IF( imomy /= 0 )pd%vn = dati(imomy)
        IF( ibuoy /= 0 )pd%t  = dati(ibuoy)
        CALL put_dynamics( puff(npuf),pd )
      END IF
    END IF

  END IF

END DO

npo = npuf - mpuf

IF( lset_sv )THEN
  CALL reset_sv( npo,puff(mpuf+1:npuf),lset_si,lset_si2 )
  IF( nError /= NO_ERROR )GOTO 9996
  WRITE(lun_log,'(A)',IOSTAT=ios)'CLOUDTRANS reader - Reset sv based on cloud moments'
  IF( ios /= 0 )THEN
    nError   = WR_ERROR
    eRoutine = 'input_puff'
    eMessage = 'Error writing SCIPUFF log file'
    CALL ReportFileName( eInform,'File=',file_log )
    GOTO 9999
  END IF
 END IF

9999 CONTINUE

RETURN

!------ Set read errors and return

1000 eMessage = 'Error reading CLOUDTRANS data'
9998 nError   = RD_ERROR
9996 CALL ReportFileName( eInform,'File=',file_tmp )
9997 eRoutine = 'input_puff'
GOTO 9999

9995 eRoutine = 'input_puff'
     nError = SZ_ERROR
     eMessage = 'Error allocating puff aux array'
     WRITE(eInform,'(A,I0,A,I0)')'IOS =',ios,' :size =',naux
GOTO 9999

9994 eRoutine = 'input_puff'
     nError = SZ_ERROR
     eMessage = 'Error deallocating puff aux array after puff accepted on nest'
     WRITE(eInform,'(A,I0)')'IOS =',ios
GOTO 9999
END

!===============================================================================

SUBROUTINE formatSubGroupError(OutString, imin, imax, ivalue)

IMPLICIT NONE

CHARACTER(*), INTENT( OUT ) :: OutString
INTEGER,      INTENT(  IN ) :: imin
INTEGER,      INTENT(  IN ) :: imax
INTEGER,      INTENT(  IN ) :: ivalue

CHARACTER(16) smin, smax, svalue

WRITE(smin,'(I0)')imin
smin = ADJUSTL(smin)

WRITE(smax,'(I0)')imax
smax = ADJUSTL(smax)

WRITE(svalue,'(I0)')ivalue
svalue = ADJUSTL(svalue)

OutString = 'Valid subgroup range is [ '//TRIM(smin)//','//TRIM(smax)//' ].  Encountered subgroup value '//TRIM(svalue)//'.'

RETURN
END

!===============================================================================

SUBROUTINE reset_sv( np,p,lset_si,lset_si2 )

USE scipuff_fi

IMPLICIT NONE

INTEGER,                         INTENT( IN    ) :: np
TYPE( puff_str ), DIMENSION(np), INTENT( INOUT ) :: p
LOGICAL,                         INTENT( IN    ) :: lset_si, lset_si2

TYPE( puff_str ) :: ptmp

INTEGER i,j
REAL    pmass,xx,yy,zz

REAL(8), DIMENSION(ntypp)   :: xbar, ybar
REAL,    DIMENSION(ntypp)   :: ctot, zbar, sv
REAL,    DIMENSION(6,ntypp) :: sig

IF( np <= 0 )RETURN

DO j = 1,ntypp

  ctot(j) = 0.

  xbar(j) = 0.D0
  ybar(j) = 0.D0
  zbar(j) = 0.

  sig(1,j) = 0.
  sig(4,j) = 0.
  sig(6,j) = 0.
  sig(2,j) = 0.
  sig(3,j) = 0.
  sig(5,j) = 0.

  sv(j) = 0.

END DO

DO i = 1,np

  j = p(i)%ityp
  pmass = p(i)%c

  ctot(j) = ctot(j) + pmass

  xbar(j) = xbar(j) + DBLE(pmass)*p(i)%xbar
  ybar(j) = ybar(j) + DBLE(pmass)*p(i)%ybar
  zbar(j) = zbar(j) + pmass*p(i)%zbar

END DO

DO j = 1,ntypp

  pmass = ctot(j)
  IF( pmass > 0. )THEN
    xbar(j) = xbar(j)/DBLE(pmass)
    ybar(j) = ybar(j)/DBLE(pmass)
    zbar(j) = zbar(j)/pmass
  END IF

END DO

DO i = 1,np

  j = p(i)%ityp
  pmass = p(i)%c

  xx = SNGL(p(i)%xbar - xbar(j))
  yy = SNGL(p(i)%ybar - ybar(j))
  zz =      p(i)%zbar - zbar(j)

  sig(1,j) = sig(1,j) + pmass*(xx**2 + p(i)%sxx)
  sig(4,j) = sig(4,j) + pmass*(yy**2 + p(i)%syy)
  sig(6,j) = sig(6,j) + pmass*(zz**2 + p(i)%szz)
  sig(2,j) = sig(2,j) + pmass*(xx*yy + p(i)%sxy)
  sig(3,j) = sig(3,j) + pmass*(xx*zz + p(i)%sxz)
  sig(5,j) = sig(5,j) + pmass*(zz*yy + p(i)%syz)

END DO

DO j = 1,ntypp

  pmass = ctot(j)
  IF( pmass > 0. )THEN

    sig(1,j) = sig(1,j)/pmass
    sig(4,j) = sig(4,j)/pmass
    sig(6,j) = sig(6,j)/pmass
    sig(2,j) = sig(2,j)/pmass
    sig(3,j) = sig(3,j)/pmass
    sig(5,j) = sig(5,j)/pmass

    ptmp%sxx = sig(1,j)
    ptmp%sxy = sig(2,j)
    ptmp%sxz = sig(3,j)
    ptmp%syy = sig(4,j)
    ptmp%syz = sig(5,j)
    ptmp%szz = sig(6,j)

    CALL siginv( ptmp )

    sv(j) = SQRT(0.5/ptmp%azz)
  END IF

END DO

DO i = 1,np

  j = p(i)%ityp
  IF( sv(j) <= 0.0 )THEN
    eMessage = 'Encountered zero moment while resetting scales'
    GOTO 9999
  END IF

  p(i)%sv = sv(j)
  IF( lset_si  )p(i)%si  = sv(j)
  IF( lset_si2 )p(i)%si2 = sv(j)

END DO

9999 CONTINUE

RETURN
END
