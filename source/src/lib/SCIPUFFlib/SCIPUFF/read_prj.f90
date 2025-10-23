SUBROUTINE read_prj()

USE scipuff_fi
USE files_fi
USE SWIMparam_fd

IMPLICIT NONE

INTEGER, PARAMETER :: IVERSION_READ = 200

INTEGER ios, maxPath, i, newaux

CHARACTER(16) auxTitle

LOGICAL, EXTERNAL :: IsGas
LOGICAL, EXTERNAL :: IsLiquid

OPEN( UNIT=lun_prj,FILE=file_prj,STATUS='OLD',FORM='UNFORMATTED', &
      IOSTAT=ios )
IF( ios /= 0 )THEN
  nError   = OP_ERROR
  eRoutine = 'read_prj'
  eMessage = 'Error opening SCIPUFF project file'
  CALL ReportFileName( eInform,'File=',file_prj )
  eAction  = 'Make sure file exists and is a valid project file'
  GOTO 9999
END IF

!------ read version number of project file

READ(lun_prj,IOSTAT=ios) iversion

IF( ios /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'read_prj'
  eMessage = 'Error reading version number on project file'
  CALL ReportFileName( eInform,'File=',file_prj )
  eAction  = 'Make sure file is a valid project file'
  GOTO 9999
END IF

REWIND(lun_prj,IOSTAT=ios)

!---- read versions 0.Vx where 0.2 < 0.Vx < current revision level

IF( iversion > iversion_code )THEN

  nError   = UK_ERROR
  eRoutine = 'read_prj'
  eMessage = 'Project version number is newer than code version'
  WRITE(eInform,*)'Project Version = ',iversion,' Code Version =',iversion_code
  eAction  = 'Use a newer version of the code to read this project'
  GOTO 9999

END IF

IF( (iversion/100 >= IVERSION_READ/100)  .AND. &
     (iversion/100 <= iversion_code/100) )THEN

  CALL read_prj_hdr( maxPath )
  IF( nError /= NO_ERROR )GOTO 9999

  CALL read_prj_const()
  IF( nError /= NO_ERROR )GOTO 9999

  CALL read_prj_turb()
  IF( nError /= NO_ERROR )GOTO 9999

  CALL read_prj_pprm()
  IF( nError /= NO_ERROR )GOTO 9999

  CALL read_prj_pgrd()
  IF( nError /= NO_ERROR )GOTO 9999

  CALL read_prj_pmtl()
  IF( nError /= NO_ERROR )GOTO 9999

  CALL read_prj_smtl()
  IF( nError /= NO_ERROR )GOTO 9999

  CALL read_prj_cntrl()
  IF( nError /= NO_ERROR )GOTO 9999

  CALL read_prj_ext( maxPath )
  IF( nError /= NO_ERROR )GOTO 9999

  CALL ClearMClist()

  IF( multicomp )THEN
    CALL read_prj_mc()
    IF( nError /= NO_ERROR )GOTO 9999
  ELSE
    CALL ClearMCtype()    !For compatibility with old projects
  END IF

  CALL read_prj_met()
  IF( nError /= NO_ERROR )GOTO 9999

  IF( BTEST(run_mode,REVERSE_MODE) )THEN
    CALL read_prj_adjoint()
    IF( nError /= NO_ERROR )GOTO 9999
  END IF

!------ Read any auxiliary data

  ios = 0
  nWetPartLiq = 0
  DO WHILE( ios == 0 )

    READ(lun_prj,IOSTAT=ios) auxTitle
    IF( ios /= 0 )EXIT

    SELECT CASE( TRIM(auxTitle) )
      CASE( 'WETP_LIQUID' )
        CALL read_prj_wetpLiquid()
        IF( nError /= NO_ERROR )GOTO 9999

      CASE DEFAULT
        nError   = UK_ERROR
        eRoutine = 'read_prj'
        eMessage = 'Unrecognized auxiliary data block'
        WRITE(eInform,*)'Type = ',TRIM(auxTitle)
        eAction  = 'Use the appropriate code version to read this project'
        GOTO 9999

    END SELECT

  END DO

END IF

!------ Check for global calculation (longitude) & polar cap

IF( lmap == I_LATLON .AND. (xmax-xmin) >= 359.9 )THEN
  global_lon = .TRUE.
  xmin = -180.0
  xmax =  180.0
  IF( ymax >= POLARCAP_LAT-0.1 )THEN
    ymax = POLARCAP_LAT
    polarcap_n = .TRUE.
  ELSE
    polarcap_n = .FALSE.
  END IF
  IF( ymin <= -POLARCAP_LAT+0.1 )THEN
    ymin = -POLARCAP_LAT
    polarcap_s = .TRUE.
  ELSE
    polarcap_s = .FALSE.
  END IF
ELSE
  global_lon = .FALSE.
  polarcap_n = .FALSE.
  polarcap_s = .FALSE.
END IF

!------ set buoyant gas flags, ( .false. for all old projects)

buoy_gas = .FALSE.

IF( iversion >= 340 )CALL set_buoy_flags()

!------ set puff%aux skips

CALL set_auxskp()

!------ Adjust for pre-2800 projects as reading of puffs will adjust auxs (Horizontal momentum)

IF( iversion < 2800 )THEN
  IF( dynamic )THEN
    DO i = 1,ntypp
      IF( IsGas(typeID(i)%icls) )THEN
        newaux = NAUX_DYNAMICS_GAS - NAUX_DYNAMICS_GAS_OLD
      ELSE
        newaux = NAUX_DYNAMICS_PART - NAUX_DYNAMICS_PART_OLD
      END IF
      typeID(i)%npaux = typeID(i)%npaux + newaux
      IF( typeID(i)%ipmc > 0 )typeID(i)%ipmc = typeID(i)%ipmc + newaux
    END DO
  END IF
END IF

!------ Adjust for pre-2500 projects as reading of puffs will adjust auxs (Liquid t and tEvap)

IF( iversion < 2500 )THEN
  DO i = 1,ntypp
    IF( IsLiquid(typeID(i)%icls) )THEN
      typeID(i)%npaux = typeID(i)%npaux + 1
      IF( typeID(i)%ipmc > 0 )typeID(i)%ipmc = typeID(i)%ipmc + 1
    END IF
  END DO
END IF

!---- set Error flag if 0.Vx < current revision level - unable to continue run
!     because of possible file incompatibility upon write

IF( iversion/100 /= iversion_code/100 )THEN
  nError   = VN_ERROR
  eRoutine = 'read_prj'
  eMessage = 'Incompatible project version'
  GOTO 9999

END IF

9999 CONTINUE

CLOSE( UNIT=lun_prj,IOSTAT=ios )

RETURN
END

!===============================================================================

SUBROUTINE read_prj_hdr( maxPath )

USE scipuff_fi
USE files_fi

!------ read common / run_header /

IMPLICIT NONE

INTEGER, INTENT( OUT ) :: maxPath

INTEGER, PARAMETER :: NP_ALLV02  = 46

INTEGER       ios, i
CHARACTER(4)  dum
CHARACTER(40) old_name

IF( iversion >= 2700 )THEN
  READ(lun_prj,IOSTAT=ios) iversion,name,maxPath,names,title &
                          ,audit_class &
                          ,audit_analyst &
                          ,audit_date &
                          ,audit_version &
                          ,audit_space
ELSE IF( iversion >= 2600 )THEN
  READ(lun_prj,IOSTAT=ios) iversion,name,maxPath,names,title &
                          ,audit_class &
                          ,audit_analyst &
                          ,audit_date &
                          ,audit_version &
                          ,audit_space
ELSE IF( iversion >= 800 )THEN
  READ(lun_prj,IOSTAT=ios) iversion,old_name,maxPath,names,title &
                          ,audit_class &
                          ,audit_analyst &
                          ,audit_date &
                          ,audit_version &
                          ,audit_space
ELSE IF( iversion >= 300 )THEN
  READ(lun_prj,IOSTAT=ios) iversion,old_name,maxPath, &
                          (names(i),i=1,21),(names(i),i=24,NP_ALL),title &
                          ,audit_class &
                          ,audit_analyst &
                          ,audit_date &
                          ,audit_version &
                          ,audit_space
ELSE IF( iversion >= 200 )THEN
  READ(lun_prj,IOSTAT=ios) iversion,old_name,maxPath, &
                          (dum,i=1,NP_ALLV02),title &
                          ,audit_class &
                          ,audit_analyst &
                          ,audit_date &
                          ,audit_version &
                          ,audit_space
ELSE
  ios = 0
  nError   = RD_ERROR
  eRoutine = 'read_prj_hdr'
  eMessage = 'Version error reading project file'
  CALL ReportFileName( eInform,'File=',file_prj )
END IF

IF( ios /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'read_prj_hdr'
  eMessage = 'Error reading project file'
  CALL ReportFileName( eInform,'File=',file_prj )
END IF

IF( iversion < 2700 )maxPath = 0
IF( iversion < 2600 )name = TRIM(old_name)

RETURN
END

!===============================================================================

SUBROUTINE read_prj_const()

USE basic_fi
USE error_fi
USE files_fi

!------ read common / const /

IMPLICIT NONE

INTEGER ios
REAL    pi, pi2, pi3, pi180, rhocp, sphfac, sphfacr !Locals

READ(lun_prj,IOSTAT=ios) pi,pi2,pi3,pi180,sphfac,sphfacr &
                        ,g,gt,f0,rhoair,rmuair,rnu,rhocp
IF( ios /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'read_prj_const'
  eMessage = 'Error reading project file'
  CALL ReportFileName( eInform,'File=',file_prj )
END IF

RETURN
END

!===============================================================================

SUBROUTINE read_prj_turb()

USE scipuff_fi
USE met_fi
USE files_fi

!------ read common / turb /

IMPLICIT NONE

INTEGER ios
REAL    La,Lb,Lbs,Lvonk,Lcvrtx,Lcqb,Lcsi1,Lcsi2 !To avoid conflict with parameter names

READ(lun_prj,IOSTAT=ios) La,Lb,Lbs,Lvonk,Lcvrtx,Lcqb,Lcsi1,Lcsi2 &
                        ,wwtrop,sltrop,epstrop,sle_fac &
                        ,uu_calm,sl_calm

IF( ios /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'read_prj_turb'
  eMessage = 'Error reading project file'
  CALL ReportFileName( eInform,'File=',file_prj )
END IF

RETURN
END

!===============================================================================

SUBROUTINE read_prj_pprm()

USE scipuff_fi
USE files_fi

!------ read common / puff_param /

IMPLICIT NONE

INTEGER ios

READ(lun_prj,IOSTAT=ios) rrmrge,simrge,cmin,delmin,asplt,asplt2 &
                        ,aspltc,dxsplt,dzsplt,delx2,delz2,fac_rfl

IF( ios /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'read_prj_pprm'
  eMessage = 'Error reading project file'
  CALL ReportFileName( eInform,'File=',file_prj )
END IF

RETURN
END

!===============================================================================

SUBROUTINE read_prj_pgrd()

USE scipuff_fi
USE files_fi

!------ read common / puff_grd /

IMPLICIT NONE

INTEGER ios

READ(lun_prj,IOSTAT=ios) xmin,xmax,ymin,ymax,zmax,mgrd &
                        ,hres,vres,dxg,dyg,dzg,nx,ny,nz &
                        ,lon0,lat0,xref,yref

IF( ios /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'read_prj_pgrd'
  eMessage = 'Error reading project file'
  CALL ReportFileName( eInform,'File=',file_prj )
END IF

RETURN
END

!===============================================================================

SUBROUTINE read_prj_pmtl()

USE scipuff_fi
USE structv04_fd
USE structv13_fd
USE files_fi
USE UtilMtlAux

!------ read common / puff_matl /

IMPLICIT NONE

TYPE( material_V04  ), DIMENSION(:), ALLOCATABLE :: V04_material
TYPE( material_V13  ), DIMENSION(:), ALLOCATABLE :: V13_material
TYPE( puff_material )   pmat
TYPE( gas_material )    pmatgas
TYPE( liquid_material ) pmatliq

INTEGER ios, i, j, nsg, i0, idum, alloc_stat, matl_type

LOGICAL, EXTERNAL :: IsGas
LOGICAL, EXTERNAL :: IsLiquid
INTEGER, EXTERNAL :: SetClass

READ(lun_prj,IOSTAT=ios) nclass,idum,idum,ntypm,ntypp,mxsgp,nmaux

IF( ios /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'read_prj_pmtl'
  eMessage = 'Error reading project file'
  CALL ReportFileName( eInform,'File=',file_prj )
  GOTO 9999
END IF

IF( nclass > MAXCLS )THEN
  nError   = IV_ERROR
  eRoutine = 'read_prj_pmtl'
  eMessage = 'Incorrect number of project material classes'
  WRITE(eInform,'(A,I2,A,I2)')'Project number is ',nclass, &
                              'Code number is ',MAXCLS
  GOTO 9999
END IF

IF( ALLOCATED(material) )DEALLOCATE( material,STAT=alloc_stat )
ALLOCATE( material(ntypm),STAT=alloc_stat )
IF( alloc_stat /= 0 )GOTO 9998

IF( ALLOCATED(typeID) )DEALLOCATE( typeID,STAT=alloc_stat )
ALLOCATE( typeID(ntypp),STAT=alloc_stat )
IF( alloc_stat /= 0 )GOTO 9998

IF( ASSOCIATED(mat_aux) )DEALLOCATE( mat_aux,STAT=alloc_stat )
ALLOCATE( mat_aux(nmaux),STAT=alloc_stat )
IF( alloc_stat /= 0 )GOTO 9998

BACKSPACE( lun_prj,IOSTAT=ios )

!------ read MATERIAL description data

matl_type = NOT_SET_I

IF( iversion >= 1400 )THEN
  READ(lun_prj,IOSTAT=ios) nclass,idum,idum,ntypm,ntypp,mxsgp &
                          ,nmaux &
                          ,(material(i),i=1,ntypm) &
                          ,(mat_aux(i),i=1,nmaux) &
                          ,(typeID(i),i=1,ntypp) &
                          ,(namec(i),i=1,MAXCLS)
  IF( ios /= 0 )THEN
    nError   = RD_ERROR
    eRoutine = 'read_prj_pmtl'
    eMessage = 'Error reading project file'
    CALL ReportFileName( eInform,'File=',file_prj )
    GOTO 9999
  END IF
ELSE IF( iversion >= 1040 )THEN
  matl_type = 1300
  ALLOCATE( V13_material(ntypm),STAT=alloc_stat )
  IF( alloc_stat /= 0 )GOTO 9998
  READ(lun_prj,IOSTAT=ios) nclass,idum,idum,ntypm,ntypp,mxsgp &
                          ,nmaux &
                          ,(V13_material(i),i=1,ntypm) &
                          ,(mat_aux(i),i=1,nmaux) &
                          ,(typeID(i),i=1,ntypp) &
                          ,(namec(i),i=1,MAXCLS)
  IF( ios /= 0 )THEN
    nError   = RD_ERROR
    eRoutine = 'read_prj_pmtl'
    eMessage = 'Error reading project file'
    CALL ReportFileName( eInform,'File=',file_prj )
    GOTO 9999
  END IF
ELSE IF( iversion >= 510 )THEN
  matl_type = 1300
  ALLOCATE( V13_material(ntypm),STAT=alloc_stat )
  IF( alloc_stat /= 0 )GOTO 9998
  READ(lun_prj,IOSTAT=ios) nclass,idum,idum,ntypm,ntypp,mxsgp &
                          ,nmaux &
                          ,(V13_material(i),i=1,ntypm) &
                          ,(mat_aux(i),i=1,nmaux) &
                          ,(typeID(i),i=1,ntypp) &
                          ,(namec(i),i=1,MAXCLS)
  IF( ios /= 0 )THEN
    nError   = RD_ERROR
    eRoutine = 'read_prj_pmtl'
    eMessage = 'Error reading project file'
    CALL ReportFileName( eInform,'File=',file_prj )
    GOTO 9999
  END IF
ELSE IF( iversion >= 300 )THEN !Material characters size change
  matl_type = 0400
  ALLOCATE( V04_material(ntypm),STAT=alloc_stat )
  IF( alloc_stat /= 0 )GOTO 9998
  READ(lun_prj,IOSTAT=ios) nclass,idum,idum,ntypm,ntypp,mxsgp &
                          ,nmaux &
                          ,(V04_material(i),i=1,ntypm) &
                          ,(mat_aux(i),i=1,nmaux) &
                          ,(typeID(i),i=1,ntypp) &
                          ,(namec(i),i=1,MAXCLS)
  IF( ios /= 0 )THEN
    nError   = RD_ERROR
    eRoutine = 'read_prj_pmtl'
    eMessage = 'Error reading project file'
    CALL ReportFileName( eInform,'File=',file_prj )
    GOTO 9999
  END IF
ELSE
  nError   = RD_ERROR
  eRoutine = 'read_prj_pmtl'
  eMessage = 'Version error reading project file'
  eAction  = 'Version is no longer supported'
  CALL ReportFileName( eInform,'File=',file_prj )
  GOTO 9999
END IF

!------ convert materials

SELECT CASE( matl_type )
  CASE( 400 )
    DO i = 1,ntypm
      material(i)%icls     = V04_material(i)%icls !Class index
      material(i)%iaux     = V04_material(i)%iaux !Matl Auxiliary pointer
      material(i)%ioffp    = V04_material(i)%ioffp !Puff ityp Offset
      material(i)%ioffs    = V04_material(i)%ioffs !Srf Deposition Offset
      material(i)%ioffd    = V04_material(i)%ioffd !Srf Dose Offset

      material(i)%lsrfg    = V04_material(i)%lsrfg !Srf Deposition by group
      material(i)%lsrft    = V04_material(i)%lsrft !Srf Deposition total
      material(i)%ldosg    = V04_material(i)%ldosg !Srf Dose by group
      material(i)%ldost    = V04_material(i)%ldost !Srf Dose total

      material(i)%prop(1)  = V04_material(i)%prop(1) !decay parameters
      material(i)%prop(2)  = V04_material(i)%prop(2) !decay parameters
      DO j = 3,10
        material(i)%prop(j)= 0.0
      END DO

      material(i)%cmat     = V04_material(i)%cmat !Material Name - NEW SIZE
      material(i)%ccls     = V04_material(i)%ccls !Class Name    - NEW SIZE
      material(i)%unit     = V04_material(i)%unit !Mass units    - NEW SIZE
      material(i)%file     = V04_material(i)%file !Material file name
      material(i)%path     = V04_material(i)%path !Material file path

      material(i)%effClass = 0
      material(i)%effAvail = 0
    END DO
    DEALLOCATE( V04_material,STAT=alloc_stat )
    IF( iversion < 310 )THEN
      DO i = 1,ntypp
        typeID(i)%icls = SetClass( material(typeID(i)%imat)%ccls )
      END DO
      DO i = 1,ntypm
        material(i)%icls = SetClass( material(i)%ccls )
      END DO
    END IF

  CASE( 1300 )
    DO i = 1,ntypm
      material(i)%icls     = V13_material(i)%icls !Class index
      material(i)%iaux     = V13_material(i)%iaux !Matl Auxiliary pointer
      material(i)%ioffp    = V13_material(i)%ioffp !Puff ityp Offset
      material(i)%ioffs    = V13_material(i)%ioffs !Srf Deposition Offset
      material(i)%ioffd    = V13_material(i)%ioffd !Srf Dose Offset

      material(i)%lsrfg    = V13_material(i)%lsrfg !Srf Deposition by group
      material(i)%lsrft    = V13_material(i)%lsrft !Srf Deposition total
      material(i)%ldosg    = V13_material(i)%ldosg !Srf Dose by group
      material(i)%ldost    = V13_material(i)%ldost !Srf Dose total

      material(i)%prop     = V13_material(i)%prop

      material(i)%cmat     = V13_material(i)%cmat !Material Name
      material(i)%ccls     = V13_material(i)%ccls !Class Name
      material(i)%unit     = V13_material(i)%unit !Mass units
      material(i)%file     = V13_material(i)%file !Material file name
      material(i)%path     = V13_material(i)%path !Material file path

      SELECT CASE( TRIM(material(i)%cmat) )
        CASE( 'VX' )
          material(i)%effClass = 1
          material(i)%effAvail = 1
        CASE( 'GB' )
          material(i)%effClass = 1
          material(i)%effAvail = 1
        CASE( 'HD' )
          material(i)%effClass = 1
          material(i)%effAvail = 1
        CASE( 'SEB' )
          material(i)%effClass = 2
          material(i)%effAvail = 1
        CASE( 'TUL' )
          material(i)%effClass = 2
          material(i)%effAvail = 1
        CASE( 'QFV' )
          material(i)%effClass = 2
          material(i)%effAvail = 1
        CASE DEFAULT
          material(i)%effClass = 0
          material(i)%effAvail = 0
      END SELECT
    END DO
    DEALLOCATE( V13_material,STAT=alloc_stat )

  CASE DEFAULT

END SELECT

IF( iversion < 2400 )THEN     !Need to modify old liquid materials to add specific heats

  DO i = 1,ntypm
    IF( IsLiquid(material(i)%icls) )THEN
      CALL GetLiquidParam( pmatliq,material(i)%iaux,2,mat_aux )
      pmatliq%cpL = NOT_SET_R
      pmatliq%cpV = NOT_SET_R
      CALL PutLiquidParam( pmatliq,material(i)%iaux,2,mat_aux )
    END IF
  END DO

END IF

!------ check number of subgroups

DO i = 1,ntypm
  nsg = GetSubgroups( material(i),mat_aux )
  IF( nsg > MAXSGP )THEN
    nError   = RD_ERROR
    eRoutine = 'read_prj_pmtl'
    eMessage = 'Too many particle size groups on project file'
    WRITE(eInform,'(A,I2,A,I2)')'Project number is ',nsg, &
                                'Code max is ',MAXSGP
    CALL ReportFileName( eInform,'File=',file_prj )
    GOTO 9999
  END IF
!====== Version prior to v0.400 did not have dense gas effects and ignored gas density
  IF( iversion < 400 )THEN
    i0 = material(i)%ioffp + 1
    IF( IsGas(material(i)%icls) )THEN
      CALL get_puff_material( i0,pmat )
      pmatgas = TRANSFER(pmat,pmatgas)
      pmatgas%rho = rhoair
      pmat = TRANSFER(pmatgas,pmat)
      CALL put_puff_material( i0,pmat )
    ELSE IF( IsLiquid(material(i)%icls) )THEN
      CALL get_puff_material( i0,pmat )
      pmatliq = TRANSFER(pmat,pmatliq)
      pmatliq%rho = rhoair
      pmat = TRANSFER(pmatliq,pmat)
      CALL put_puff_material( i0,pmat )
    END IF
  END IF
END DO

9999 CONTINUE

IF( ALLOCATED(V04_material) )DEALLOCATE( V04_material,STAT=alloc_stat )
IF( ALLOCATED(V13_material) )DEALLOCATE( V13_material,STAT=alloc_stat )

RETURN

9998 CONTINUE

nError = SZ_ERROR
eRoutine = 'read_prj'
eMessage = 'Error allocating space for project data'
GOTO 9999

END

!===============================================================================

SUBROUTINE read_prj_smtl()

USE scipuff_fi
USE files_fi

!------ read common / surf_matl /

IMPLICIT NONE

INTEGER ios

READ(lun_prj,IOSTAT=ios,ERR=9998,END=9998) ntyps,ntypd

9999 CONTINUE

RETURN

9998 CONTINUE
nError   = RD_ERROR
eRoutine = 'read_prj_smtl'
eMessage = 'Error reading project file'
CALL ReportFileName( eInform,'File=',file_prj )
GOTO 9999

END

!===============================================================================

SUBROUTINE read_prj_cntrl()

USE scipuff_fi
USE met_fi
USE files_fi

!------ read common / cntrl /

IMPLICIT NONE

INTEGER ios

LOGICAL lRadPrj    !for compatibility with NFAC versions

READ(lun_prj,IOSTAT=ios) t_avg,lRadPrj,lter,lmap,local,lsplitz &
                        ,lzi_prj,lymd,dose,surface,tzone,jul_start &
                        ,year_start,month_start,day_start &
                        ,year_end,month_end,day_end &
                        ,tstart,tend,tend_hr &
                        ,delt,t_save,dt_save,t_old_r
IF( ios /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'read_prj_cntrl'
  eMessage = 'Error reading project file'
  CALL ReportFileName( eInform,'File=',file_prj )
END IF

RETURN
END

!===============================================================================

SUBROUTINE read_prj_ext( maxPath )

USE scipuff_fi
USE met_fi
USE files_fi
USE substrate_fi

!------ read extensions for newer versions

IMPLICIT NONE

INTEGER, INTENT( IN ) :: maxPath

INTEGER ios, i, idum, n, nc, alloc_stat
INTEGER hazard        !For compatibility with HAZARD versions
INTEGER nmcaux     !For compatibility with MULTI-COMPONENT versions
INTEGER numNWPN    !For compatibility with NWPN versions
REAL    dum, grdmin

CHARACTER(196) smpOld
CHARACTER(PATH_MAXLENGTH), DIMENSION(:), ALLOCATABLE :: smpTemp

INTEGER, EXTERNAL :: NumMaterialAux

nzbl      = 11
nmcaux    = 0
dynamic   = .FALSE.
dense_gas = .FALSE.
z_dosage  = 0.0
smpfile   = ' '
utm_zone  = NOT_SET_I
static    = .FALSE.
run_mode  = 0
multicomp = .FALSE.
hazard    = 0
dt_smp    = DEF_VAL_R

prjEffects = 0

substrate_type = 0
porosity       = NOT_SET_R
tortuosity     = NOT_SET_R
grain_size     = NOT_SET_R
numNWPN        = 0
numRelID       = 0
smpOld         = ' '

IF( iversion >= 2700 )THEN

  IF( maxPath <= PATH_MAXLENGTH )THEN
    READ(lun_prj,IOSTAT=ios) nzbl,dynamic,dense_gas &
                            ,z_dosage,smpfile(1:maxPath),utm_zone,static &
                            ,multicomp,hazard,run_mode,dt_smp,prjEffects &
                            ,substrate_type,porosity,tortuosity,grain_size &
                            ,numNWPN,numRelID

    IF( ios /= 0 )GOTO 9999
  ELSE
    n = (maxPath-1)/PATH_MAXLENGTH + 1
    ALLOCATE( smpTemp(n),STAT=alloc_stat )
    IF( alloc_stat /= 0 )THEN
      nError   = RD_ERROR
      eRoutine = 'read_prj'
      WRITE(eMessage,'(A,I5)')'Incompatible project path length size, current limit: ',PATH_MAXLENGTH
      WRITE(eInform,'(A,I5)') 'Attempt to read project with size limit: ',maxPath
      GOTO 9998
    END IF
    nc = maxPath - (n-1)*PATH_MAXLENGTH
    READ(lun_prj,IOSTAT=ios) nzbl,dynamic,dense_gas &
                            ,z_dosage,(smpTemp(i),i=1,n-1),smpTemp(n)(1:nc) &
                            ,utm_zone,static &
                            ,multicomp,hazard,run_mode,dt_smp,prjEffects &
                            ,substrate_type,porosity,tortuosity,grain_size &
                            ,numNWPN,numRelID

    IF( ios /= 0 )GOTO 9999

    IF( LEN_TRIM(smpTemp(2)) /= 0 )THEN
      nError   = RD_ERROR
      eRoutine = 'read_prj'
      WRITE(eMessage,'(A,I5)')'Incompatible sampler path length size, current limit: ',PATH_MAXLENGTH
      WRITE(eInform,'(A,I5)') 'Attempt to read project with size limit: ',maxPath
      GOTO 9998
    ELSE
      smpFile = smpTemp(1)
    END IF
  END IF

ELSE IF( iversion >= 2100 )THEN

  READ(lun_prj,IOSTAT=ios) nzbl,dynamic,dense_gas &
                          ,z_dosage, smpOld, utm_zone, static &
                          ,multicomp,hazard,run_mode,dt_smp,prjEffects &
                          ,substrate_type,porosity,tortuosity,grain_size &
                          ,numNWPN,numRelID

  IF( ios /= 0 )GOTO 9999

ELSE IF( iversion >= 2000 )THEN

  READ(lun_prj,IOSTAT=ios) nzbl,dynamic,dense_gas &
                          ,z_dosage, smpOld, utm_zone, static &
                          ,multicomp,hazard,run_mode,dt_smp,prjEffects &
                          ,substrate_type,porosity,tortuosity,grain_size &
                          ,numNWPN

  IF( ios /= 0 )GOTO 9999

ELSE IF( iversion >= 1600 )THEN

  READ(lun_prj,IOSTAT=ios) nzbl,dynamic,dense_gas &
                          ,z_dosage, smpOld, utm_zone, static &
                          ,multicomp,hazard,run_mode,dt_smp,prjEffects &
                          ,substrate_type,porosity,tortuosity,grain_size

  IF( ios /= 0 )GOTO 9999

ELSE IF( iversion >= 1500 )THEN

  READ(lun_prj,IOSTAT=ios) nzbl,dynamic,dense_gas &
                          ,z_dosage, smpOld(1:80), utm_zone, static &
                          ,multicomp,hazard,run_mode,dt_smp,prjEffects &
                          ,substrate_type,porosity,tortuosity,grain_size

  IF( ios /= 0 )GOTO 9999

ELSE IF( iversion >= 1400 )THEN

  READ(lun_prj,IOSTAT=ios) nzbl,dynamic,dense_gas,grdmin &
                          ,z_dosage, smpOld(1:80), utm_zone, static &
                          ,multicomp,hazard,run_mode,dt_smp,prjEffects

  IF( ios /= 0 )GOTO 9999

ELSE IF( iversion >= 1300 )THEN

  READ(lun_prj,IOSTAT=ios) nzbl,dynamic,dense_gas,grdmin &
                          ,z_dosage, smpOld(1:80), utm_zone, static &
                          ,multicomp,hazard,run_mode,dt_smp

  IF( ios /= 0 )GOTO 9999

ELSE IF( iversion >= 1204 )THEN

  READ(lun_prj,IOSTAT=ios) nzbl,dynamic,dense_gas,grdmin &
                          ,z_dosage, smpOld(1:80), utm_zone, static &
                          ,multicomp,hazard,run_mode

  IF( ios /= 0 )GOTO 9999

ELSE IF( iversion >= 1203 )THEN

  READ(lun_prj,IOSTAT=ios) nzbl,dynamic,dense_gas,grdmin &
                          ,z_dosage, smpOld(1:80), utm_zone, static &
                          ,multicomp,hazard,run_mode,dt_smp

  IF( ios /= 0 )GOTO 9999

ELSE IF( iversion >= 1100 )THEN

  READ(lun_prj,IOSTAT=ios) nzbl,dynamic,dense_gas,grdmin &
                          ,z_dosage, smpOld(1:80), utm_zone, static &
                          ,multicomp,hazard,run_mode

  IF( ios /= 0 )GOTO 9999

ELSE IF( iversion >= 900 )THEN

  READ(lun_prj,IOSTAT=ios) nzbl,dynamic,dense_gas,grdmin &
                          ,z_dosage, smpOld(1:80), utm_zone, static &
                          ,multicomp,hazard

  IF( ios /= 0 )GOTO 9999

ELSE IF( iversion >= 700 )THEN

  READ(lun_prj,IOSTAT=ios) nzbl,dynamic,dense_gas,grdmin &
                          ,idum,(dum,i=1,idum) &
                          ,z_dosage, smpOld(1:80), utm_zone, static &
                          ,multicomp

  IF( ios /= 0 )GOTO 9999

ELSE IF( iversion >= 651 )THEN

  READ(lun_prj,IOSTAT=ios) nzbl,dynamic,dense_gas,grdmin &
                          ,nmcaux,(dum,i=1,nmcaux) &
                          ,idum,(dum,i=1,idum) &
                          ,z_dosage, smpOld(1:80), utm_zone, static &
                          ,multicomp

  IF( ios /= 0 )GOTO 9999

ELSE IF( iversion >= 650 )THEN

  READ(lun_prj,IOSTAT=ios) nzbl,dynamic,dense_gas,grdmin &
                          ,nmcaux,(dum,i=1,nmcaux) &
                          ,idum,(dum,i=1,idum) &
                          ,z_dosage, smpOld(1:80), utm_zone, static

  multicomp = nmcaux > 0

  IF( ios /= 0 )GOTO 9999

ELSE IF( iversion >= 420 )THEN

  READ(lun_prj,IOSTAT=ios) nzbl,dynamic,dense_gas,grdmin &
                          ,nmcaux,(dum,i=1,nmcaux) &
                          ,idum,(dum,i=1,idum) &
                          ,z_dosage, smpOld(1:80), utm_zone

  multicomp = nmcaux > 0

  IF( ios /= 0 )GOTO 9999

ELSE IF( iversion >= 400 )THEN

  READ(lun_prj,IOSTAT=ios) nzbl,dynamic,dense_gas,grdmin &
                          ,nmcaux,(dum,i=1,nmcaux) &
                          ,idum,(dum,i=1,idum) &
                          ,z_dosage, smpOld(1:80)

  multicomp = nmcaux > 0

  IF( ios /= 0 )GOTO 9999

ELSE IF( iversion >= 340 )THEN

  READ(lun_prj,IOSTAT=ios) nzbl,dynamic,dense_gas,grdmin &
                          ,nmcaux,(dum,i=1,nmcaux) &
                          ,idum,(dum,i=1,idum) &
                          ,z_dosage

  multicomp = nmcaux > 0

  IF( ios /= 0 )GOTO 9999

ELSE IF( iversion >= 310 )THEN

  READ(lun_prj,IOSTAT=ios) nzbl,dynamic,dense_gas,grdmin &
                          ,nmcaux,(dum,i=1,nmcaux) &
                          ,idum,(dum,i=1,idum)

  multicomp = nmcaux > 0

  IF( ios /= 0 )GOTO 9999

ELSE IF( iversion >= 300 )THEN

  READ(lun_prj,IOSTAT=ios) nzbl,dynamic,dense_gas &
                          ,nmcaux,(dum,i=1,nmcaux)

  multicomp = nmcaux > 0

  IF( ios /= 0 )GOTO 9999

ELSE IF( iversion >= 230 )THEN

  READ(lun_prj,IOSTAT=ios) nzbl

  IF( ios /= 0 )GOTO 9999

  DO i = 1,ntypp
    typeID(i)%npaux = NumMaterialAux( typeID(i)%icls, &
                                      typeID(i)%igrp, &
                                      typeID(i)%ltot )
  END DO

ELSE IF( iversion >= 200 )THEN

  DO i = 1,ntypp
    typeID(i)%npaux = NumMaterialAux( typeID(i)%icls, &
                                      typeID(i)%igrp, &
                                      typeID(i)%ltot )
  END DO

ELSE

  GOTO 9999

END IF

IF( .NOT.multicomp )THEN   !Zero ID's if not multicomponent
  DO i = 1,ntypm
    material(i)%mcID = 0
  END DO
END IF

IF( ALLOCATED(releaseID) )DEALLOCATE( releaseID,STAT=ios )

IF( numRelID > 0 )THEN

  ALLOCATE( releaseID(numRelID),STAT=ios )
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'read_prj_ext'
    eMessage = 'Error allocating release ID array'
    CALL ReportFileName( eInform,'File=',file_prj )
    GOTO 9998
  END IF
  READ(lun_prj,IOSTAT=ios) (releaseID(i),i=1,numRelID)
  IF( ios /= 0 )GOTO 9999

END IF

IF( iversion < 2700 )THEN
  IF( LEN_TRIM(smpOld) <= PATH_MAXLENGTH )THEN
    smpfile = TRIM(smpOld)
  ELSE
    nError   = RD_ERROR
    eRoutine = 'read_prj'
    WRITE(eMessage,'(A,I5)')'Incompatible project path length size, current limit: ',PATH_MAXLENGTH
    WRITE(eInform,'(A,I5)') 'Attempt to read project with size limit: ',LEN_TRIM(smpOld)
    GOTO 9998
  END IF
END IF

9998 CONTINUE

RETURN

9999 CONTINUE

nError   = RD_ERROR
eRoutine = 'read_prj_ext'
eMessage = 'Error reading project file'
CALL ReportFileName( eInform,'File=',file_prj )
GOTO 9998

END

!===============================================================================

SUBROUTINE read_prj_mc()

USE scipuff_fi
USE files_fi
USE  chem_fi

!------ read extensions for newer versions

IMPLICIT NONE

INTEGER ios, i

IF( iversion >= 3301 )THEN
  READ(lun_prj,IOSTAT=ios) DoChemistry
  IF( ios /= 0 )GOTO 9998
END IF
READ(lun_prj,IOSTAT=ios) mat_mc%nMCtype
IF( ios /= 0 )GOTO 9998

ALLOCATE( mat_mc%type(mat_mc%nMCtype),mat_mc%ID(mat_mc%nMCtype),STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'read_prj_mc'
  eMessage = 'Error allocating material multicomponent list'
  GOTO 9999
END IF

READ(lun_prj,IOSTAT=ios) mat_mc%type,mat_mc%ID
IF( ios /= 0 )GOTO 9998

! ---- Check that multicomponent types are all recognized

DO i = 1,mat_mc%nMCtype

  SELECT CASE( mat_mc%type(i) )
    CASE( MC_CHEM )

    CASE DEFAULT
      nError   = UK_ERROR
      eMessage = 'Multicomponent error'
      eRoutine = 'read_prj_mc'
      WRITE(eInform,'(A,I6)') 'Unknown multicomponent type :',mat_mc%type(i)
      GOTO 9999
  END SELECT

END DO

CALL ReadChemMC()
IF( nError /= NO_ERROR )GOTO 9999

9999 CONTINUE

RETURN

9998 CONTINUE
nError   = RD_ERROR
eRoutine = 'read_prj_mc'
eMessage = 'Error reading project file'
CALL ReportFileName( eInform,'File=',file_prj )
GOTO 9999

END

!==============================================================================

SUBROUTINE RstChemMC()

!------ Skip section for "special" restart
!        N.B. Assumes materials are identical to those in current project
!        N.B. Old project file assumed opened on unit LUN_TMP

USE files_fi
USE scipuff_fi
USE chem_fi

IMPLICIT NONE

INTEGER i, ii, n, ios

DO ii = 1,mat_mc%nMCtype

  IF( mat_mc%type(ii) == MC_CHEM )THEN

    n = mat_mc%ID(ii)

    chem => chemMC(n,1)

    READ(lun_tmp,IOSTAT=ios)  !chem%flag,chem%nSpecies,chem%nReactions, &
                              !chem%nZenith,chem%rtol,chem%ambFile,chem%FileNotUsed, &
                              !chem%cUnits,chem%eUnits,chem%kUnits, &
                              !chem%tConv,chem%kConv,chem%eConv,chem%eConvType
    IF( ios /= 0 )GOTO 9998

    READ(lun_tmp,IOSTAT=ios) !(chem%zenith(i),i=1,chem%nZenith)
    IF( ios /= 0 )GOTO 9998

    !-- read the particle types and particle emission units
    IF( chem%pTypeN > 0 )THEN
      READ(lun_tmp,IOSTAT=ios) !(chem%pTypes(i),chem%pUnits(i),i=1,chem%pTypeN)
      IF( ios /= 0 )GOTO 9998
    END IF

    DO i = 1,chem%nSpecies
      READ(lun_tmp,IOSTAT=ios) !chem%species(i)%class,chem%species(i)%classAux,chem%species(i)%name, &
                               !chem%species(i)%ambient,chem%species(i)%tol,&
                               !chem%species(i)%ldos,chem%species(i)%ldep, &
                               !chem%species(i)%scav,chem%species(i)%vdep,&
                               !chem%species(i)%mw,chem%species(i)%nit,chem%species(i)%lim
      IF( ios /= 0 )GOTO 9998
    END DO

    DO i = 1,chem%nReactions
      READ(lun_tmp,IOSTAT=ios) !chem%reaction(i)%class,chem%reaction(i)%type, &
                               !chem%reaction(i)%A,chem%reaction(i)%B,&
                               !chem%reaction(i)%fB, &
                               !np,(idum,dum,j=1,np),&
                               !ndat,(dum,j=1,ndat)
      IF( ios /= 0 )GOTO 9998
    END DO
  END IF
END DO

9999 CONTINUE

RETURN

9998 CONTINUE
nError   = UK_ERROR
eMessage = 'Error skipping chem arrays for restart'
eRoutine = 'RstChemMC'

RETURN
END

!===============================================================================

SUBROUTINE rst_prj_mc()

!------- Skip MC section for "special" restart
!        N.B. Assumes materials are identical to those in current project
!        N.B. Old project file assumed opened on unit LUN_TMP

USE scipuff_fi
USE files_fi

IMPLICIT NONE

INTEGER ios

READ(lun_tmp,IOSTAT=ios)               !mat_mc%nMCtype
IF( ios == 0 )READ(lun_tmp,IOSTAT=ios) !mat_mc%type,mat_mc%ID
IF( ios /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'rst_prj_mc'
  eMessage = 'Error reading old project file'
  eInform  = 'Attempting to skip multicomponent section'
  GOTO 9999
END IF

CALL RstChemMC()
IF( nError /= NO_ERROR )GOTO 9999

9999 CONTINUE

RETURN
END

!-------------------------------------------------------------------------

SUBROUTINE ReadChemMC()

USE files_fi
USE scipuff_fi
USE chem_fi
USE mpi_fi, ONLY: AqAerInp

IMPLICIT NONE

INTEGER i, ii, j, n, ndat, np, ios, alloc_stat, idum
REAL    dum
CHARACTER(PATH_MAXLENGTH) prjName

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: StripExtension
DOUBLE PRECISION ddum
REAL, DIMENSION(:), ALLOCATABLE :: rData

n = 0
DO i = 1,mat_mc%nMCtype
  IF( mat_mc%type(i) == MC_CHEM )THEN
    n = n + 1
    IF( mat_mc%ID(i) /= n )THEN
      nError   = UK_ERROR
      eMessage = 'Invalid CHEM multicomponent ID'
      eRoutine = 'ReadChemMC'
    END IF
  END IF
END DO

CALL ExitChemMC()

ALLOCATE( chemMC(n,nthreads),STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eMessage = 'Error allocating multicomponent arrays'
  eRoutine = 'ReadChemMC'
END IF

DO ii = 1,mat_mc%nMCtype

  IF( mat_mc%type(ii) == MC_CHEM )THEN

    n = mat_mc%ID(ii)

    chem => chemMC(n,1)

    CALL ClearChemMC()

    IF( iversion < 2402 )THEN
      chem%FileNotUsed = ' '
      READ(lun_prj,IOSTAT=ios)chem%flag,chem%nSpecies,chem%nReactions, &
                              chem%nZenith,chem%rtol,chem%ambFile, &
                              chem%cUnits,chem%eUnits,chem%kUnits, &
                              chem%tConv,chem%kConv,chem%eConv,chem%eConvType
    ELSE
      IF( iversion < 2802 )THEN
        READ(lun_prj,IOSTAT=ios)chem%flag,chem%nSpecies,chem%nReactions,chem%nZenith, &
                              chem%rtol,chem%ambFile,chem%FileNotUsed,chem%sFlxFile, &
                              chem%lBalance,chem%lStepAmb,chem%lStage, &
                              chem%pTypeN,chem%cUnits,chem%eUnits,chem%kUnits, &
                              chem%tConv,chem%kConv,chem%eConv,chem%eConvType
        chem%oUnits = UNIT_PPM
      ELSE
        READ(lun_prj,IOSTAT=ios)chem%flag,chem%nSpecies,chem%nReactions,chem%nZenith, &
                              chem%rtol,chem%ambFile,chem%FileNotUsed,chem%sFlxFile, &
                              chem%lBalance,chem%lStepAmb,chem%lStage, &
                              chem%pTypeN,chem%cUnits,chem%oUnits,chem%eUnits,chem%kUnits, &
                              chem%tConv,chem%kConv,chem%eConv,chem%eConvType
      END IF
    END IF
    IF( ios /= 0 )GOTO 9998

    chem%lAmbFile = chem%ambFile /= ' '
    IF( chem%lAmbFile )THEN
      chem%lunAmb = 300 + n
    ELSE
      chem%lunAmb = 0
    END IF

    chem%lAddAmb = .TRUE. ! Default set to False for SCICHEM 3.0
    chem%lSfcFlx = chem%sFlxFile /= ' '
    IF( chem%lSfcFlx )THEN
      chem%lunSfcFlx = 301 + n
    ELSE
      chem%lunSfcFlx = 0
    END IF

    ALLOCATE( chem%zenith(chem%nZenith),chem%species(chem%nSpecies), &
              chem%reaction(chem%nReactions),STAT=alloc_stat )
    IF( alloc_stat /= 0 )THEN
      nError   = UK_ERROR
      eMessage = 'Error allocating multicomponent arrays'
      eRoutine = 'ReadChemMC'
      GOTO 9999
    END IF

    READ(lun_prj,IOSTAT=ios) (chem%zenith(i),i=1,chem%nZenith)
    IF( ios /= 0 )GOTO 9998

    !-- read the particle types and particle emission units
    IF( chem%pTypeN > 0 )THEN
      ALLOCATE( chem%pTypes(chem%pTypeN),chem%pUnits(chem%pTypeN),STAT=alloc_stat )
      IF( alloc_stat /= 0 )THEN
        nError   = UK_ERROR
        eMessage = 'Error allocating multicomponent arrays'
        eRoutine = 'ReadChemMC'
        GOTO 9999
      END IF
      READ(lun_prj,IOSTAT=ios) (chem%pTypes(i),chem%pUnits(i),i=1,chem%pTypeN)
      IF( ios /= 0 )GOTO 9998
    END IF

    DO i = 1,chem%nSpecies
      IF( iversion > 3100 )THEN
        READ(lun_prj,IOSTAT=ios) chem%species(i)%class,chem%species(i)%classAux,chem%species(i)%name, &
                               chem%species(i)%ambient,chem%species(i)%tol,&
                               chem%species(i)%ldos,chem%species(i)%ldep, &
                               chem%species(i)%scav,chem%species(i)%vdep,&
                               chem%species(i)%Henry0,chem%species(i)%TpFac,&
                               chem%species(i)%RxFac,chem%species(i)%SrfRFac,chem%species(i)%ldrydep,&
                               chem%species(i)%mw,chem%species(i)%nit,chem%species(i)%lim
      ELSE
      READ(lun_prj,IOSTAT=ios) chem%species(i)%class,chem%species(i)%classAux,chem%species(i)%name, &
                               chem%species(i)%ambient,chem%species(i)%tol,&
                               chem%species(i)%ldos,chem%species(i)%ldep, &
                               chem%species(i)%scav,chem%species(i)%vdep,&
                               chem%species(i)%mw,chem%species(i)%nit,chem%species(i)%lim
          chem%species(i)%Henry0  = NOT_SET_R
          chem%species(i)%TpFac   = NOT_SET_R
          chem%species(i)%RxFac   = NOT_SET_R
          chem%species(i)%SrfRFac = NOT_SET_R
          chem%species(i)%ldrydep = .FALSE.
      ENDIF
      IF( ios /= 0 )GOTO 9998
    END DO

    chem%thermal = .FALSE.

    DO i = 1,chem%nReactions
      IF( iversion < 2802 )THEN
      READ(lun_prj,IOSTAT=ios) chem%reaction(i)%class,chem%reaction(i)%type, &
                               chem%reaction(i)%A,chem%reaction(i)%B,&
                               chem%reaction(i)%fB, &
                               np,(idum,dum,j=1,np),&
                               ndat,(dum,j=1,ndat)
      ELSE
      READ(lun_prj,IOSTAT=ios) chem%reaction(i)%class,chem%reaction(i)%type, &
                               chem%reaction(i)%A,chem%reaction(i)%B,&
                               chem%reaction(i)%fB, &
                               np,(idum,dum,j=1,np),&
                               ndat,(ddum,j=1,ndat)
      END IF
      IF( ios /= 0 )GOTO 9998
      IF( np > 0 )THEN
        ALLOCATE( chem%reaction(i)%P(np),chem%reaction(i)%fP(np), &
                  STAT=alloc_stat )
        IF( alloc_stat /= 0 )THEN
          nError   = UK_ERROR
          eMessage = 'Error allocating multicomponent reaction product arrays'
          eRoutine = 'ReadChemMC'
          GOTO 9999
        END IF
      ELSE
        NULLIFY( chem%reaction(i)%P )
        NULLIFY( chem%reaction(i)%fP )
      END IF

      IF( ndat > 0 )THEN
        ALLOCATE( chem%reaction(i)%data(ndat),STAT=alloc_stat )
        IF( alloc_stat /= 0 )THEN
          nError   = UK_ERROR
          eMessage = 'Error allocating multicomponent reaction data arrays'
          eRoutine = 'ReadChemMC'
          GOTO 9999
        END IF
        IF( iversion < 2802 )THEN
          IF( ALLOCATED(rData) )DEALLOCATE( rData,STAT=alloc_stat )
          ALLOCATE( rData(ndat),STAT=alloc_stat )
          IF( alloc_stat /= 0 )THEN
            nError   = UK_ERROR
            eMessage = 'Error allocating multicomponent reaction data arrays'
            eRoutine = 'ReadChemMC'
            GOTO 9999
          END IF
        END IF
      ELSE
        NULLIFY( chem%reaction(i)%data )
      END IF

      BACKSPACE( lun_prj,IOSTAT=ios )
      IF( ios /= 0 )GOTO 9998
      IF( iversion > 2801 )THEN
      READ(lun_prj,IOSTAT=ios) chem%reaction(i)%class,chem%reaction(i)%type, &
                               chem%reaction(i)%A,chem%reaction(i)%B,&
                               chem%reaction(i)%fB,chem%reaction(i)%nP, &
                    (chem%reaction(i)%P(j),chem%reaction(i)%fP(j),j=1,np),&
                               ndat,(chem%reaction(i)%data(j),j=1,ndat)
      ELSE
      READ(lun_prj,IOSTAT=ios) chem%reaction(i)%class,chem%reaction(i)%type, &
                               chem%reaction(i)%A,chem%reaction(i)%B,&
                               chem%reaction(i)%fB,chem%reaction(i)%nP, &
                     (chem%reaction(i)%P(j),chem%reaction(i)%fP(j),j=1,np),&
                               ndat,(rdata(j),j=1,ndat)
      IF( ios /= 0 )GOTO 9998
      DO j = 1,ndat
        chem%reaction(i)%data(j) = DBLE( rdata(j) )
      END DO
      END IF
      IF( ios /= 0 )GOTO 9998

      IF( BTEST(chem%reaction(i)%class,ID_REACT_THERMAL) )THEN
        chem%reaction(i)%H = chem%reaction(i)%data(ndat)
        chem%thermal = .TRUE.
      END IF

    END DO

!------ Read group output

    IF( iversion >= 2801 )THEN
      READ(lun_prj,IOSTAT=ios) chem%nOutGroup
      IF( ios /= 0 )GOTO 9998
      IF( chem%nOutGroup > 0 )THEN
        ALLOCATE( chem%OutGroup(chem%nOutGroup),STAT=ios )
        IF( ios /= 0 )THEN
          nError   = UK_ERROR
          eRoutine = 'ReadChemMC'
          eMessage = 'Error allocating Multi-component output group array'
          GOTO 9999
        END IF
        DO i = 1,chem%nOutGroup
          NULLIFY( chem%OutGroup(i)%iComp,chem%OutGroup(i)%fComp )
          READ(lun_prj,IOSTAT=ios) chem%OutGroup(i)%Name,np
          ALLOCATE( chem%OutGroup(i)%iComp(np),chem%OutGroup(i)%fComp(np),STAT=ios )
          IF( ios /= 0 )THEN
            nError   = UK_ERROR
            eRoutine = 'ReadChemMC'
            eMessage = 'Error allocating Multi-component output group component arrays'
            GOTO 9999
          END IF
          BACKSPACE(lun_prj,IOSTAT=ios)
          IF( ios /= 0 )GOTO 9998
          READ(lun_prj,IOSTAT=ios) chem%OutGroup(i)%Name,chem%OutGroup(i)%nComp, &
                                  (chem%OutGroup(i)%iComp(j),chem%OutGroup(i)%fComp(j),j=1,np)
          IF( ios /= 0 )GOTO 9998
        END DO
      END IF
    ELSE
      chem%nOutGroup = 0
    END IF

    CALL SetChemPointers()
    IF( nError /= NO_ERROR )GOTO 9999

    H2O = NOT_SET_I

    DO i = 1,chem%nSpecies
      chem%species(i)%ID    = i
      chem%species(i)%lstar = .FALSE.
      IF( TRIM(chem%species(i)%name) /= 'H2O' .AND. &
          TRIM(chem%species(i)%name) /= 'WATER' )THEN
        IF( chem%species(i)%ambient < 0.0 )THEN
          nError   = IV_ERROR
          eRoutine = 'ReadChemMC'
          eMessage = 'Negative ambient specified for species '//TRIM(chem%species(i)%name)
          eInform  = 'Negative value only allowed for H2O/WATER'
          GOTO 9999
        END IF
      ELSE
        IF( chem%species(i)%ambient >= 0.0 ) H2O = i
      END IF
    END DO

    !---- Set base project name
    prjName = StripExtension( file_prj )

    CALL ReadChemDiagnostics()

    !====== AqAqer Mode

    AqAerInp%nError  = nError
    AqAerInp%prjName = prjName
    AqAerInp%mcFile  = TRIM(material(n)%file)
    CALL ReadAqAerEx( AqAerInp )
    IF( nError /= NO_ERROR )THEN
      nError   = IV_ERROR
      eRoutine = 'ReadAqAer'
      eMessage = 'Error from AqAer module. See project qlog for details'
      GO TO 9999
    END IF

    CALL SetStarSpecies( n )
    IF( nError /= NO_ERROR )GOTO 9999

    IF( .NOT. create )THEN

      CALL SetChemAqaer( )
      IF( nError /= NO_ERROR )GOTO 9999

      chem_aqaer%prjName = prjName
      CALL InitAerAqEx( n,chem_aqaer,ID_SPECIES_PARTICLE,nError )
      IF( nError /= NO_ERROR )THEN
        eRoutine = 'InitAerAq'
        eMessage = 'Error in InitAerAq. Check project qlog for details'
        GOTO 9999
      END IF

      CALL GetChemAqaer( )

    END IF

    IF( nthreads > 1 )THEN
      DO i = 2,nthreads
        CALL CopyChemMC( n,i )
        IF( nError /= NO_ERROR )GOTO 9999
      END DO
    END IF

  END IF

END DO

9999 CONTINUE

RETURN

9998 CONTINUE
nError   = UK_ERROR
eMessage = 'Error reading multicomponent arrays'
eRoutine = 'ReadChemMC'
IF( ALLOCATED(rData) )DEALLOCATE( rData,STAT=alloc_stat)
GO TO 9999

RETURN
END

!===============================================================================

SUBROUTINE read_prj_met()

USE scipuff_fi
USE met_fi
USE files_fi
USE SWIMparam_fd

IMPLICIT NONE

INTEGER ios, irv, alloc_stat, ifld, i, nxy
REAL    dum

INTEGER(KIND=1) :: AuxCoord(SIZE_MapCoordPrjTransfer)

TYPE( SWIMgridStr ), POINTER :: grd

INTEGER, EXTERNAL :: InitLambert, InitPolar, InitMercator, InitRotPolar, InitRotLL, SWIMcnvCoord

CALL ClearMetGrid()

hmin = 0.0

IF( iversion >= 1700 )THEN

  IF( iversion >= 1800 )THEN
    READ(lun_prj,IOSTAT=ios) numMet,mcTypePrj
  ELSE
    READ(lun_prj,IOSTAT=ios) numMet
  END IF

  IF( numMet == 0 )GOTO 9999

  ALLOCATE( MetGrid(numMet),lMetGridComplete(numMet),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'read_prj_met'
    eMessage = 'Error allocating met grids'
    GOTO 9999
  END IF

  lMetGridComplete = .FALSE.

  DO ifld = 1,numMet
    grd => MetGrid(ifld)
    grd%lter = .FALSE.
    NULLIFY( grd%zi,grd%H,grd%Hx,grd%Hy )
  END DO

  nBaseMet = 0
  lter = .FALSE.

  metFields : DO ifld = 1,numMet

    grd => MetGrid(ifld)

    IF( iversion >= 2300 )THEN

      READ(lun_prj,IOSTAT=ios) grd%nx,grd%ny,grd%dx,grd%dy,grd%xmin,grd%ymin, &
                               grd%xminPrj,grd%xmaxPrj,grd%yminPrj,grd%ymaxPrj, &
                               grd%lter,(AuxCoord(i),i=1,SIZE_MapCoordPrjTransfer)
     IF( ios /= 0 )THEN
        nError   = RD_ERROR
        eRoutine = 'read_prj_met'
        eMessage = 'Error reading met grid definitions'
        CALL ReportFileName( eInform,'File=',file_prj )
        GOTO 9999
      END IF

      grd%coord = TRANSFER(AuxCoord,grd%coord)

      SELECT CASE( grd%coord%type )
        CASE( I_LAMBERT )
          irv = InitLambert( grd%coord%Lat0,grd%coord%Lat1,grd%coord%Lat2,grd%coord%Rearth, &
                             grd%coord%n,grd%coord%f,grd%coord%m0,grd%coord%y0 )

        CASE( I_POLAR )
          irv = InitPolar( grd%coord%Lat0,grd%coord%Lat1,grd%coord%Rearth, &
                           grd%coord%n,grd%coord%f,grd%coord%m0,grd%coord%y0 )

        CASE( I_RPOLAR )
          irv = InitRotPolar( grd%coord%Lat0,grd%coord%Lon0, &
                              grd%coord%sp0,grd%coord%cp0,grd%coord%sl0,grd%coord%cl0, &
                              grd%coord%cc0,grd%coord%sc0,grd%coord%cs0,grd%coord%ss0  )

        CASE( I_ROTLL )
          irv = InitRotLL( grd%coord%Lat0,grd%coord%sp0,grd%coord%cp0 )

        CASE( I_MERCATOR )
          irv = InitMercator( grd%coord%Lat1,Rearth*1.E-3,grd%coord%f,grd%coord%m0 )

        CASE DEFAULT
          irv = 1

      END SELECT
      IF( irv /= 1 )THEN
        nError   = IV_ERROR
        eRoutine = 'read_prj_met'
        eMessage = 'Error initializing map coordinate transformations'
        GOTO 9999
      END IF

    ELSE

      READ(lun_prj,IOSTAT=ios) grd%nx,grd%ny,grd%dx,grd%dy,grd%xmin,grd%ymin, &
                               grd%lter
      IF( ios /= 0 )THEN
        nError   = RD_ERROR
        eRoutine = 'read_prj_met'
        eMessage = 'Error reading met grid definitions'
        CALL ReportFileName( eInform,'File=',file_prj )
        GOTO 9999
      END IF

      grd%coord%type          = lmap
      grd%coord%zone          = utm_zone
      grd%coord%reference%x   = xref
      grd%coord%reference%y   = yref
      grd%coord%reference%lat = lat0
      grd%coord%reference%lon = lon0

      grd%xminPrj = grd%xmin
      grd%yminPrj = grd%ymin
      grd%xmaxPrj = grd%xmin + FLOAT(grd%nx-1)*grd%dx
      grd%ymaxPrj = grd%ymin + FLOAT(grd%ny-1)*grd%dy

    END IF

    IF( grd%nx > 0 )THEN
      nBaseMet  = nBaseMet + 1
      grd%basic = 0
    ELSE
      grd%nx    = -grd%nx
      grd%basic = ISHFT(grd%ny,-16)
      grd%ny    = IAND(grd%ny,2**16-1)
    END IF

    CALL AllocMetGridArrays( MetGrid(ifld) )
    IF( nError /= NO_ERROR )GOTO 9999

    IF( grd%lter )THEN

      nxy = grd%nX * grd%nY

      READ(lun_prj,IOSTAT=ios) (grd%H(i),i=1,nxy),(grd%Hx(i),i=1,nxy), &
                               (grd%Hy(i),i=1,nxy),grd%Hmin

      IF( ios /= 0 )THEN
        nError   = RD_ERROR
        eRoutine = 'read_prj_met'
        eMessage = 'Error reading met terrain fields'
        CALL ReportFileName( eInform,'File=',file_prj )
        GOTO 9999
      END IF

      IF( .NOT.lter )THEN
        hmin = grd%Hmin
        lter = .TRUE.
      ELSE
        hmin = MIN( hmin,grd%Hmin )
      END IF

    END IF

    grd%delx2 = delx2  !Will be reset later
    lMetGridComplete(ifld) = .TRUE.


  END DO metFields

ELSE

  numMet   = 1
  nBaseMet = 1

  ALLOCATE( MetGrid(numMet),lMetGridComplete(numMet),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'read_prj_met'
    eMessage = 'Error allocating met grids'
    GOTO 9999
  END IF

  lMetGridComplete(1) = .FALSE.

  grd => MetGrid(1)

  IF( lter )THEN

    READ(lun_prj,IOSTAT=ios) grd%nx,grd%ny,grd%dx,grd%dy,grd%xmin, &
                             (dum,i=1,grd%nx-1),grd%ymin,(dum,i=1,grd%ny-1), &
                             grd%lter
    IF( ios /= 0 )THEN
      nError   = RD_ERROR
      eRoutine = 'read_prj_met'
      eMessage = 'Error reading met grid definitions'
      CALL ReportFileName( eInform,'File=',file_prj )
    END IF

    CALL AllocMetGridArrays( MetGrid(1) )
    IF( nError /= NO_ERROR )GOTO 9999

    IF( grd%lter )THEN

      nxy = grd%nX * grd%nY

      IF( iversion >= 411 )THEN
        READ(lun_prj,IOSTAT=ios) (grd%H(i),i=1,nxy),(grd%Hx(i),i=1,nxy), &
                                 (grd%Hy(i),i=1,nxy),grd%Hmin
      ELSE
        READ(lun_prj,IOSTAT=ios) (grd%H(i),i=1,nxy),(grd%Hx(i),i=1,nxy), &
                                 (grd%Hy(i),i=1,nxy)
        grd%Hmin = 0.
      END IF
      IF( ios /= 0 )THEN
        nError   = RD_ERROR
        eRoutine = 'read_prj_met'
        eMessage = 'Error reading met terrain fields'
        CALL ReportFileName( eInform,'File=',file_prj )
        GOTO 9999
      END IF

    END IF

    hmin = grd%Hmin

  ELSE

    NULLIFY( grd%zi,grd%H,grd%Hx,grd%Hy )
    grd%lter = .FALSE.

  END IF

  grd%coord%type          = lmap
  grd%coord%zone          = utm_zone
  grd%coord%reference%x   = xref
  grd%coord%reference%y   = yref
  grd%coord%reference%lat = lat0
  grd%coord%reference%lon = lon0

  grd%xminPrj = grd%xmin
  grd%yminPrj = grd%ymin
  grd%xmaxPrj = grd%xmin + FLOAT(grd%nx-1)*grd%dx
  grd%ymaxPrj = grd%ymin + FLOAT(grd%ny-1)*grd%dy

  grd%delx2 = delx2

  lMetGridComplete(1) = .TRUE.

END IF

numMetMax = numMet

CALL SetPrjCoord()

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE read_prj_adjoint()

USE scipuff_fi
USE files_fi
USE adjoint_fi

!------ read extensions for newer versions

IMPLICIT NONE

INTEGER ios, i

IF( ALLOCATED(AdjMat) )DEALLOCATE( AdjMat,STAT=ios )

ALLOCATE( AdjMat(ntypm),STAT=ios )
IF( ios /= 0 )THEN
  nError = UK_ERROR
  eRoutine = 'read_prj_adjoint'
  eMessage = 'Failed to allocate adjoint material storage'
  GOTO 9999
END IF

READ(lun_prj,IOSTAT=ios) (AdjMat(i),i=1,ntypm)
IF( ios /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'read_prj_adjoint'
  eMessage = 'Error reading project file'
  CALL ReportFileName( eInform,'File=',file_prj )
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE read_prj_wetpLiquid()

USE scipuff_fi
USE files_fi

!------ write auxiliary liquid data for wet particles

IMPLICIT NONE

INTEGER ios, i

IF( ALLOCATED(WetPartLiquidMat) )DEALLOCATE( WetPartLiquidMat,STAT=ios )

READ(lun_prj,IOSTAT=ios) nWetPartLiq

ALLOCATE( WetPartLiquidMat(nWetPartLiq),STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'read_prj_adjoint'
  eMessage = 'Error allocating auxiliary liquid storage'
  CALL ReportFileName( eInform,'File=',file_prj )
  GOTO 9999
END IF

BACKSPACE( lun_prj,IOSTAT=ios )

READ(lun_prj,IOSTAT=ios) nWetPartLiq,(WetPartLiquidMat(i),i=1,nWetPartLiq)
IF( ios /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'read_prj_wetpLiquid'
  eMessage = 'Error reading project file'
  CALL ReportFileName( eInform,'File=',file_prj )
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE set_buoy_flags()

USE scipuff_fi

!  Sets flags for buoyant gas materials (non-neutral densities)

IMPLICIT NONE

TYPE( puff_material ) pmat
TYPE( gas_material )  pmatgas
TYPE( aerosol_material ) pmataer
TYPE( liquid_material  ) pmatliq

INTEGER i, alloc_stat
REAL    rhog

LOGICAL, EXTERNAL :: IsGas
LOGICAL, EXTERNAL :: IsLiquid, IsAerosol

!------ loop over puff types and check densities

IF( .NOT.dynamic )RETURN

IF( ALLOCATED(buoy_flag) )DEALLOCATE( buoy_flag,STAT=alloc_stat )
ALLOCATE( buoy_flag(ntypp),STAT=alloc_stat )

IF( ALLOCATED(buoy_fac) )DEALLOCATE( buoy_fac,STAT=alloc_stat )
ALLOCATE( buoy_fac(ntypp),STAT=alloc_stat )

DO i = 1,ntypp

  buoy_flag(i) = IsLiquid(typeID(i)%icls)

  IF( IsGas(typeID(i)%icls) )THEN
    IF( IsAerosol(typeID(i)%icls) )THEN
      IF( IsAerosol(material(typeID(i)%imat)%icls) )THEN
        CALL get_puff_material( i,pmat )
        pmataer = TRANSFER(pmat,pmataer)
        rhog = rhoair*pmataer%w/MWAIR
      ELSE
        CALL get_puff_material( i-1,pmat )
        pmatliq = TRANSFER(pmat,pmatliq)
        rhog = rhoair*pmatliq%w/MWAIR
      END IF
    ELSE
      CALL get_puff_material( i,pmat )
      pmatgas = TRANSFER(pmat,pmatgas)
      rhog = pmatgas%rho
    END IF
    IF( rhog /= rhoair )THEN
      buoy_gas     = .TRUE.
      buoy_flag(i) = .TRUE.
    END IF
    buoy_fac(i) = (rhog-rhoair)/(rhog*rhoair)
  END IF

END DO

RETURN
END

!===============================================================================

SUBROUTINE deallocate_read_prj()

!------ Deallocates space allocated by read_prj

USE scipuff_fi
USE adjoint_fi

IMPLICIT NONE

INTEGER alloc_stat

IF( ALLOCATED(material)  )DEALLOCATE( material,STAT=alloc_stat )
IF( ASSOCIATED(mat_aux)  )DEALLOCATE( mat_aux,STAT=alloc_stat ); NULLIFY( mat_aux )
IF( ALLOCATED(typeID)    )DEALLOCATE( typeID,STAT=alloc_stat )
IF( ALLOCATED(buoy_flag) )DEALLOCATE( buoy_flag,STAT=alloc_stat )
IF( ALLOCATED(buoy_fac)  )DEALLOCATE( buoy_fac,STAT=alloc_stat )
IF( ALLOCATED(releaseID) )DEALLOCATE( releaseID,STAT=alloc_stat )
IF( ALLOCATED(AdjMat) )DEALLOCATE( AdjMat,STAT=alloc_stat )
IF( ALLOCATED(WetPartLiquidMat) )DEALLOCATE( WetPartLiquidMat,STAT=alloc_stat )

CALL ClearMetGrid()

CALL ExitChemMC()
CALL ClearMClist()



RETURN
END

!===============================================================================

SUBROUTINE ClearMClist()

USE scipuff_fi

IMPLICIT NONE

INTEGER ios

mat_mc%nMCtype = 0

IF( ASSOCIATED(mat_mc%type) )DEALLOCATE( mat_mc%type,STAT=ios )
IF( ASSOCIATED(mat_mc%ID  ) )DEALLOCATE( mat_mc%ID  ,STAT=ios )

RETURN
END

!===============================================================================

SUBROUTINE ClearMCtype()

USE scipuff_fi

IMPLICIT NONE

INTEGER i

DO i=1,ntypp
  typeID(i)%mcID = 0
  typeID(i)%ipmc = 0
END DO

DO i=1,ntypm
  material(i)%mcID = 0
  material(i)%nmc  = 0
END DO

RETURN
END
