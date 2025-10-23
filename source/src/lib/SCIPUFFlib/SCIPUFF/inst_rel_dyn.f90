SUBROUTINE i_release( relSpec,LogFlag )

USE scipuff_fi
USE files_fi
USE relparam_fd
USE convert_fd

IMPLICIT NONE

TYPE( releaseSpecT ), INTENT( INOUT ) :: relSpec
LOGICAL, INTENT( IN ) :: LogFlag    ! Write to log file

REAL    cmass_liq, cmass_vap
INTEGER ityp_vap, naux_vap
INTEGER ios, nun, nch, iflg, mpuf, imat, ityp
INTEGER i, nsg, nb_rel, naux, jtyp, ifld
LOGICAL l2phase
LOGICAL lGroundRel
INTEGER jfld
INTEGER subgroup
INTEGER nSeed

REAL(8) xbar, ybar
REAL    zbar, h, xmap, ymap, hx, hy, xx, yy, rxx, ryy, shh
REAL cmass, mmd, sigma, dryFrac
REAL sigx,sigy,sigz,sigRxy,sigRxz,sigRyz
REAL, DIMENSION(3) :: rSpread
REAL rDir

REAL, DIMENSION(MAXSGP)   :: weight
REAL, DIMENSION(MAXSGP+1) :: pbounds

CHARACTER(12)           :: cPuff
CHARACTER(12), EXTERNAL :: FormatPuffs

LOGICAL, EXTERNAL :: chkgrd
LOGICAL, EXTERNAL :: IsWetParticle, IsLiquid, IsAerosol
LOGICAL, EXTERNAL :: IsMulti
INTEGER, EXTERNAL :: num_puff_types
REAL,    EXTERNAL :: ScaleReal

CHARACTER(PATH_MAXLENGTH) :: file

!-----  Check release time, and save if necessary

IF( ScaleReal(relSpec%release%trel,HCF_HOUR2SEC) > t + 0.01*delt )THEN
  CALL StoreInstRel(relSpec)
  GOTO 9999
END IF

!---- Initialize eRoutine for check_newpuff calls

eRoutine = 'i_release'

!-----  releases puffs from NAME_REL[.rel] file

mpuf = npuf   !Save old number of puffs

lGroundRel = .FALSE.

CALL getReleaseFile( relSpec%release,file )
IF( file /= ' ' )THEN

  nch = LEN_TRIM(file)
  nun = lun_tmp
  CALL add_ext( file_tmp,file(1:nch),'rel' )

  OPEN(UNIT=nun,FILE=file_tmp,STATUS='OLD',ACTION='READ',IOSTAT=ios)
  IF( ios /= 0 )THEN
    nError   = OP_ERROR
    eMessage = 'Error opening source release input file'
    CALL ReportFileName( eInform,'File=',file_tmp )
    eAction  = 'Check scenario file for proper specification'
    GOTO 9999
  END IF

  iflg  = 0

  CALL input_puff( relSpec%release,nun,iflg,nb_rel )
  IF( nError /= NO_ERROR )GOTO 9999

  IF( nun == lun_tmp )CLOSE(lun_tmp,IOSTAT=ios)

  IF( nb_rel == 0 )GOTO 9999

  CALL mapfac( SNGL(relSpec%release%xrel),SNGL(relSpec%release%yrel),xmap,ymap )

  npuf = mpuf

  DO i = mpuf+1,mpuf+nb_rel

    xbar = relSpec%release%xrel + puff(i)%xbar*DBLE(xmap)
    ybar = relSpec%release%yrel + puff(i)%ybar*DBLE(ymap)

!------ Make sure xrel inside domain if global longitude

    IF( global_lon )CALL SetGlobalLonD( xbar )

    xx   = (SNGL(xbar)-xmin)/dxg
    yy   = (SNGL(ybar)-ymin)/dyg
    rxx  = puff(i)%sxx*xmap*xmap/(dxg*dxg)
    ryy  = puff(i)%syy*ymap*ymap/(dyg*dyg)

    IF( chkgrd(xx,yy,rxx,ryy) )THEN

      shh = 0.5*(puff(i)%sxx+puff(i)%syy)

      CALL get_topogOut( SNGL(xbar),SNGL(ybar),h,hx,hy,shh,ifld )

      CALL setPuffifld( puff(i),ifld )
      CALL setPuffirel( puff(i),0 )

      zbar = relSpec%release%zrel + puff(i)%zbar + h
      npuf = npuf + 1

      IF( i /= npuf )THEN
        CALL copy_puff( puff(i),puff(npuf) )
        IF( nError /= NO_ERROR )GOTO 9999
      END IF

      puff(npuf)%xbar = xbar
      puff(npuf)%ybar = ybar
      puff(npuf)%zbar = zbar

      IF( IsMulti(typeID(puff(npuf)%ityp)%icls) )THEN
        CALL InitMCinst( relSpec,puff(npuf) )
        IF( nError /= NO_ERROR )GOTO 9999
      END IF

    END IF

  END DO

!------ set puff for simple puff release (usually from nested model)

ELSE IF( relSpec%release%type == HR_PUFF )THEN

  !Kind of violates the idea of using passed in structure but its UnloadReleasePuff that fills
  !in the puff structure referenced here. Leave for now
 ! CALL UnloadRelease(relSpec%release)

  ityp = relSpec%ityp !puffRelease%ityp

! --- Check for wet particle release

  CALL getReleaseDryFraction( relSpec%release,dryFrac )
  CALL check_wet_reltyp( ityp,dryFrac )
  IF( nError /= NO_ERROR )GOTO 9999

  naux = typeID(ityp)%npaux
  CALL check_newpuff()
  IF( nError /= NO_ERROR )GOTO 9999

  npuf = npuf + 1


  CALL LoadPuffFromPuffRelease( relSpec,puff(npuf) )

  shh = 0.5*(puff(npuf)%sxx+puff(npuf)%syy)

  CALL get_topogOut( SNGL(puff(npuf)%xbar),SNGL(puff(npuf)%ybar),h,hx,hy,shh,ifld )

  puff(npuf)%zbar = puff(npuf)%zbar + h

  CALL setPuffifld( puff(npuf),ifld )
  CALL setPuffirel( puff(npuf),0 )

  !Just to get Aux allocated?
  CALL set_aux_rel( relSpec,puff(npuf),naux )

  CALL LoadPuffAuxFromPuffRelease( relSpec,puff(npuf) )

  !------ Set puff for simple instantaneous release

ELSE

  CALL getReleaseSigmas( relSpec%release,sigx,sigy,sigz,sigRxy,sigRxz,sigRyz )

  lGroundRel = lter .AND. relSpec%release%zrel==0.0
  IF( BTEST(relSpec%release%type,HRB_OFFDIAG) .AND. lGroundRel )lGroundRel = sigRxz==0.0 .AND. sigRyz==0.0

  CALL mapfac( SNGL(relSpec%release%xrel),SNGL(relSpec%release%yrel),xmap,ymap )

  xbar = SNGL(relSpec%release%xrel)
  ybar = SNGL(relSpec%release%yrel)
  xx   = (xbar-xmin)/dxg
  yy   = (ybar-ymin)/dyg
  rxx  = (sigx*xmap/dxg)**2
  ryy  = (sigy*ymap/dyg)**2

  IF( chkgrd(xx,yy,rxx,ryy) )THEN

    shh = 0.5*(sigx**2+sigy**2)

    CALL get_topogOut( SNGL(relSpec%release%xrel),SNGL(relSpec%release%yrel),h,hx,hy,shh,ifld )

    zbar = relSpec%release%zrel + h

    ityp = relSpec%ityp

    CALL getReleaseDistribution( relSpec%release,subgroup,mmd,sigma )
    CALL getReleaseMass( relSpec%release,cmass )
    CALL getReleaseDryFraction( relSpec%release,dryFrac )
! --- Check 2-phase liquid release

    CALL check_liquid_reltyp( ityp,relSpec%release%xrel,relSpec%release%yrel,zbar,relSpec%distrib,mmd,dryFrac,l2phase )
    IF( nError /= NO_ERROR )GOTO 9999

! --- Check for wet particle release

    CALL check_wet_reltyp( ityp,dryFrac )
    IF( nError /= NO_ERROR )GOTO 9999

    naux = typeID(ityp)%npaux
    imat = typeID(ityp)%imat

    IF( relSpec%distrib <= 0 )THEN   !--- Single size bin release

      IF( l2phase )THEN

        cmass_liq = cmass*dryFrac

        CALL set_puff_inst( relSpec,zbar,cmass_liq,ityp,naux,ifld )
        IF( nError /= NO_ERROR )GOTO 9999

        ityp_vap  = material(imat)%ioffp + 1
        cmass_vap = cmass - cmass_liq
        naux_vap  = typeID(ityp_vap)%npaux

        CALL set_puff_inst( relSpec,zbar,cmass_vap,ityp_vap,naux_vap,ifld )
        IF( nError /= NO_ERROR )GOTO 9999

      ELSE

        CALL set_puff_inst( relSpec,zbar,cmass,ityp,naux,ifld )
        IF( nError /= NO_ERROR )GOTO 9999

      END IF

    ELSE     !--- Lognormal distribution

      CALL get_bounds( material(imat),nsg,pbounds )
      IF( nsg < 2 )THEN
        eMessage = 'Request for a lognormal distribution of release mass but'
        eInform  = TRIM(ADJUSTL(material(imat)%cmat))//' has only a single bin'
        eAction  = 'It is more efficient to use a single bin release'
        CALL CautionMessage()
      END IF
      IF( IsWetParticle(typeID(ityp)%icls) )CALL set_wetbin( pbounds,nsg,imat,dryFrac )

		  CALL check_logn( pbounds(1),pbounds(nsg+1),mmd,sigma,rxx )
      IF( rxx > 0.05 )THEN
        nError = WN_ERROR
        WRITE(eMessage,*) NINT(rxx*100.0)
        eMessage = TRIM(ADJUSTL(eMessage))//'% of the mass is outside the bin range'
		    eInform  = 'This mass will be lumped into the first and last bins'
        CALL WarningMessage( .TRUE. )
        IF( nError /= NO_ERROR )THEN
          eMessage = 'More than 5% of the mass is outside the bin range'
          eInform  = 'Please adjust the MMD and/or sigma'
          eAction  = '(or redefine the material size bins)'
          GOTO 9999
        END IF
      END IF

!---- Set relative weights for size bins

      CALL logn_bin( pbounds,nsg,mmd,sigma,weight )

!---- Check for 2-phase liquid release; reset weights and release vapor puff

      IF( l2phase )THEN

        DO i = 1,nsg
          weight(i) = weight(i)*dryFrac
        END DO

        cmass_vap = cmass*(1.0-dryFrac)
        ityp_vap  = material(imat)%ioffp + 1
        naux_vap  = typeID(ityp_vap)%npaux

        CALL set_puff_inst( relSpec,zbar,cmass_vap,ityp_vap,naux_vap,ifld )
        IF( nError /= NO_ERROR )GOTO 9999

      END IF

!---- Release a puff in each size bin

      DO i = 1,nsg
        jtyp = ityp + i - 1
        CALL set_puff_inst( relSpec,zbar,cmass*weight(i),jtyp,naux,ifld )
        IF( nError /= NO_ERROR )GOTO 9999
      END DO

    END IF

  END IF

END IF

IF( npuf > mpuf )THEN

! --- Write to log file

  IF( LogFlag ) THEN
    cPuff   = FormatPuffs(npuf)

    IF( file /= ' ' )THEN
      WRITE(lun_log,114,IOSTAT=ios)'Instantaneous release  at t =', &
                          t/3600.,'hrs. from file ',TRIM(file), &
                          ' with NPUFF = '//cPuff
    ELSE
      WRITE(lun_log,115,IOSTAT=ios)'Instantaneous release  at t =', &
               t/3600.,'hrs. from input scenario file with NPUFF = '//cPuff
    END IF
114 FORMAT(A,F7.2,A,A,A)
115 FORMAT(A,F7.2,A)

    IF( ios /= 0 )THEN
      nError   = WR_ERROR
      eMessage = 'Error writing SCIPUFF log file'
      CALL ReportFileName( eInform,'File=',file_log )
      GOTO 9999
    END IF

  END IF

!-------- Generate random realizations, if required

  CALL getReleaseRandomParams( relSpec%release,nRandom,nSeed,rSpread,rDir )
  IF( nRandom > 1 )THEN
    CALL random_puff_loc( mpuf, rSpread )
  END IF

!-------- Rotate ground releases

  IF( lGroundRel )THEN
    WRITE(lun_log,'(A)')'  Rotating ground puffs from release '//TRIM(relSpec%release%relDisplay)//&
                        &' of '//TRIM(relSpec%release%material)
    DO i = mpuf+1,npuf
      WRITE(lun_log,'(A,1P6G13.6)')'  Before rotation',puff(i)%sxx,puff(i)%sxy,puff(i)%sxz,puff(i)%syy,puff(i)%syz,puff(i)%szz
      shh = 0.5*(puff(i)%sxx+puff(i)%syy)
      jfld = 0
      ifld = 1
      DO WHILE( jfld < ifld )
        jfld = ifld
        CALL get_metFieldID( SNGL(puff(i)%xbar),SNGL(puff(i)%ybar),0.0,shh,ifld )
        IF( nError /= NO_ERROR )GOTO 9999
      END DO
      CALL get_topogIn( SNGL(puff(i)%xbar),SNGL(puff(i)%ybar),h,hx,hy,ifld )
      CALL dense_rot_norm( 0.,0.,hx,hy,puff(i) )
      WRITE(lun_log,'(A,1P6G13.6)')'  After  rotation',puff(i)%sxx,puff(i)%sxy,puff(i)%sxz,puff(i)%syy,puff(i)%syz,puff(i)%szz
    END DO
  END IF

ELSE

  nError   = IV_ERROR
  WRITE(eMessage,'(A,F7.2,A)',IOSTAT=ios)'Instantaneous release  at t =', &
                              t/3600.,'hrs. outside domain'
  WRITE(eInform,'(A,F10.4,1X,F10.4)',IOSTAT=ios)'Location: ',relSpec%release%xrel,relSpec%release%yrel
  GOTO 9999

END IF

!---- Reset eRoutine if no errors

eRoutine = CHAR(0)

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE StoreInstRel(relSpec)

USE scipuff_fi

IMPLICIT NONE

TYPE( releaseSpecT ), INTENT( INOUT ) :: relSpec

INTEGER alloc_stat

TYPE( StoreReleaseT ), POINTER :: lastRel, nextRel

!--- Save instantaneous release definition for release during large timestep

IF( InstReleaseList%relSpec%release%trel == NOT_SET_R )THEN

  CALL copyReleaseSpec(relSpec,InstReleaseList%relSpec)

  mxtlev = MAX(mxtlev,7)

ELSE

  lastRel => InstReleaseList
  nextRel => lastRel%NextRelease

  DO WHILE( ASSOCIATED(nextRel) )
    lastRel => nextRel
    nextRel => lastRel%NextRelease
  END DO

  ALLOCATE( nextRel,STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    nError   = SZ_ERROR
    eMessage = 'Error allocating instantaneous release list'
    GOTO 9999
  END IF
  CALL InitReleaseSpec( nextRel%relSpec )
  lastRel%NextRelease => nextRel

  CALL copyReleaseSpec(relSpec,nextRel%relSpec)
  NULLIFY( nextRel%NextRelease )

END IF

lastRel => InstReleaseList
nextRel => lastRel%NextRelease

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE CheckInstRelease( lev1,lev2 )

USE scipuff_fi

IMPLICIT NONE

INTEGER, INTENT( IN    ) :: lev1       ! current time level
INTEGER, INTENT( INOUT ) :: lev2       ! max time level

REAL    dts
INTEGER alloc_stat, mpuf, i
INTEGER n1, n2

CHARACTER(128) cmsg,cmsg2,cmsg3
CHARACTER(12), EXTERNAL :: FormatPuffs

TYPE( StoreReleaseT ), POINTER :: nextRel

IF( InstReleaseList%relSpec%release%trel == NOT_SET_R )RETURN

lev2 = MAX(lev2,7)
dts  = delt * (0.5**lev2)

IF( 3600.*InstReleaseList%relSpec%release%trel < t+dts )THEN

  mpuf = npuf

  DO WHILE( 3600.*InstReleaseList%relSpec%release%trel < t+dts )


    CALL valid_scn( InstReleaseList%relSpec )
    IF( nError /= NO_ERROR )GOTO 9999

    !----- initialize for random locations

    CALL init_random_loc( InstReleaseList%relSpec )
    IF( nError /= NO_ERROR )GOTO 9999

    CALL i_release( InstReleaseList%relSpec,.TRUE. )
    IF( nError /= NO_ERROR )GOTO 9999

    !----- clear random locations

    CALL clear_random_loc()
    IF( nError /= NO_ERROR )GOTO 9999

    nextRel => InstReleaseList%NextRelease

    IF( ASSOCIATED(nextRel) )THEN
      CALL copyReleaseSpec( nextRel%relSpec,InstReleaseList%relSpec )
      IF( ASSOCIATED(nextRel%NextRelease) )THEN
        InstReleaseList%NextRelease => nextRel%NextRelease
      ELSE
        CALL ClearReleaseSpec( InstReleaseList%NextRelease%relSpec )
        NULLIFY( InstReleaseList%NextRelease )
      END IF
      CALL ClearReleaseSpec( nextRel%relSpec )
      DEALLOCATE( nextRel,STAT=alloc_stat )
    ELSE
      CALL ClearReleaseSpec(InstReleaseList%relSpec)
    END IF

    IF( InstReleaseList%relSpec%release%trel == NOT_SET_R )EXIT

  END DO

!------ Set interactions for new releases

  IF( npuf > mpuf )THEN

    cmsg  = CHAR(0)
    cmsg2 = CHAR(0)
    cmsg3 = 'Initializing '//TRIM(FormatPuffs(npuf-mpuf))//' new puffs'
    CALL write_progress( cmsg,cmsg2,cmsg3 )

    n1 = mpuf + 1
    n2 = npuf
    CALL set_rel( n1,n2,lev1 )
    IF( nError /= NO_ERROR )GOTO 9999

    DO i = mpuf+1,npuf
      lev2 = MAX(lev2,puff(i)%idtl)
    END DO

  END IF

END IF

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE add_ext( file,name,ext )

USE DefSize_fd

IMPLICIT NONE

CHARACTER(*), INTENT( OUT ) :: file
CHARACTER(*), INTENT( IN  ) :: name, ext

CHARACTER(PATH_MAXLENGTH) ftmp, path

INTEGER i

CALL SplitName( name,ftmp,path )

i = INDEX(ftmp,'.')

IF( i == 0 )THEN
  file = name//'.'//ext
ELSE
  file = name
END IF

RETURN
END

!============================================================================

SUBROUTINE set_puff_inst( relSpec,zbar,xmass,ityp,naux,ifld )

USE scipuff_fi

IMPLICIT NONE

TYPE( releaseSpecT ), INTENT( INOUT ) :: relSpec
REAL,    INTENT( IN ) :: xmass, zbar
INTEGER, INTENT( IN ) :: ityp, naux, ifld

IF( xmass <= 0.0 )RETURN

CALL check_newpuff()
IF( nError /= NO_ERROR )GOTO 9999

npuf = npuf + 1

CALL set_puff_rel( relSpec,puff(npuf),xmass,zbar,ityp,naux,ifld )

RETURN

9999 CONTINUE

eRoutine = 'set_puff_inst'

RETURN
END

!============================================================================

SUBROUTINE random_puff_loc( mpuf, rSpread )

USE scipuff_fi
USE relparam_fd

IMPLICIT NONE

INTEGER,            INTENT( IN ) :: mpuf
REAL, DIMENSION(3), INTENT( IN ) :: rSpread

INTEGER ireal, npufo, nb_rel, i, ipuf, naux, ifld
REAL    shh, zz, h, hx, hy

nb_rel = npuf - mpuf
npufo  = npuf

!---- Set release data structure

CALL SetDataRelID( nRandom,rSpread(1),puff(npuf) )

!-------- Generate nRandom copies

DO ireal = 2,nRandom
  DO i = mpuf+1,npufo
    naux = typeID(puff(i)%ityp)%npaux
    CALL check_newpuff()
    IF( nError /= NO_ERROR )THEN
      eRoutine = 'random_puff_loc'
      GOTO 9999
    END IF

    npuf = npuf + 1

    CALL copy_puff( puff(i),puff(npuf) )
    IF( nError /= NO_ERROR )GOTO 9999

  END DO
END DO

!------ Determine height AGL

IF( lter )THEN
  shh = 0.5*(puff(npuf)%sxx+puff(npuf)%syy)
  CALL get_topogOut( SNGL(puff(npuf)%xbar),SNGL(puff(npuf)%ybar),h,hx,hy,shh,ifld )
  zz = MAX(puff(npuf)%zbar-h,0.01*SQRT(puff(npuf)%szz))
END IF

!-----  Randomize puff locations

DO ireal = 1,nRandom
  DO i = mpuf+1,npufo
    ipuf = (ireal-1)*nb_rel + i
    puff(ipuf)%xbar = puff(ipuf)%xbar + DBLE(xRandom(ireal))
    puff(ipuf)%ybar = puff(ipuf)%ybar + DBLE(yRandom(ireal))
    IF( lter )THEN
      CALL get_topogIn( SNGL(puff(ipuf)%xbar),SNGL(puff(ipuf)%ybar),h,hx,hy,ifld )
      puff(ipuf)%zbar = h + MAX(0.,zz + zRandom(ireal))   !Set to constant height AGL + random displacement
    ELSE
      puff(ipuf)%zbar = MAX(0.,puff(ipuf)%zbar + zRandom(ireal))   !Set to constant height AGL + random displacement
    END IF
  END DO
END DO

DO i = mpuf+1,npuf
  CALL setPuffirel( puff(i),numRelID )
END DO

9999 CONTINUE

RETURN
END
