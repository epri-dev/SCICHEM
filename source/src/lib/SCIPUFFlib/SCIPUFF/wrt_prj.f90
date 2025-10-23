SUBROUTINE write_prj()

USE scipuff_fi
USE files_fi

!------ output project file

IMPLICIT NONE

INTEGER ios

LOGICAL lopen

!------ open file

INQUIRE( FILE=file_prj,OPENED=lopen )
IF ( lopen )THEN
  REWIND( UNIT=lun_prj,IOSTAT=ios)
  IF( ios /= 0 )THEN
    nError   = OP_ERROR
    eRoutine = 'write_prj'
    eMessage = 'Error rewinding SCIPUFF project file'
    CALL ReportFileName( eInform,'File=',file_prj )
    WRITE(eAction,*)'IOSTAT=',ios
    GOTO 9999
  END IF
ELSE
  OPEN(UNIT=lun_prj,FILE=file_prj,STATUS='UNKNOWN',FORM='UNFORMATTED', &
       IOSTAT=ios)
  IF( ios /= 0 )THEN
    nError   = OP_ERROR
    eRoutine = 'write_prj'
    eMessage = 'Error opening SCIPUFF project file'
    CALL ReportFileName( eInform,'File=',file_prj )
    WRITE(eAction,*)'IOSTAT=',ios
    GOTO 9999
  END IF
END IF

!------ write records

CALL write_prj_hdr()
IF( nError /= NO_ERROR )GOTO 9999

CALL write_prj_const()
IF( nError /= NO_ERROR )GOTO 9999

CALL write_prj_turb()
IF( nError /= NO_ERROR )GOTO 9999

CALL write_prj_pprm()
IF( nError /= NO_ERROR )GOTO 9999

CALL write_prj_pgrd()
IF( nError /= NO_ERROR )GOTO 9999

CALL write_prj_pmtl()
IF( nError /= NO_ERROR )GOTO 9999

CALL write_prj_smtl()
IF( nError /= NO_ERROR )GOTO 9999

CALL write_prj_cntrl()
IF( nError /= NO_ERROR )GOTO 9999

CALL write_prj_ext()
IF( nError /= NO_ERROR )GOTO 9999

IF( multicomp )THEN
  CALL write_prj_mc()
  IF( nError /= NO_ERROR )GOTO 9999
END IF

CALL write_prj_met()
IF( nError /= NO_ERROR )GOTO 9999

IF( BTEST(run_mode,REVERSE_MODE) )THEN
  CALL write_prj_adjoint()
  IF( nError /= NO_ERROR )GOTO 9999
END IF

IF( nWetPartLiq > 0 )CALL write_prj_wetpLiquid()

CALL FLUSH(lun_prj)

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE write_prj_hdr()

USE scipuff_fi
USE files_fi

!------ write common / run_header /

IMPLICIT NONE

INTEGER ios

WRITE(lun_prj,IOSTAT=ios) iversion,name,PATH_MAXLENGTH,names,title &
                         ,audit_class &
                         ,audit_analyst &
                         ,audit_date &
                         ,audit_version &
                         ,audit_space

IF( ios /= 0 )THEN
  nError   = WR_ERROR
  eRoutine = 'write_prj_hdr'
  eMessage = 'Error writing project file'
  CALL ReportFileName( eInform,'File=',file_prj )
END IF

RETURN
END

!===============================================================================

SUBROUTINE write_prj_const()

USE scipuff_fi
USE files_fi

!------ write common / const /

IMPLICIT NONE

INTEGER ios

WRITE(lun_prj,IOSTAT=ios) PI,PI2,PI3,PI180,SPHFAC,SPHFACR &
                         ,g,gt,f0,rhoair,rmuair,rnu,RHOCP

IF( ios /= 0 )THEN
  nError   = WR_ERROR
  eRoutine = 'write_prj_const'
  eMessage = 'Error writing project file'
  CALL ReportFileName( eInform,'File=',file_prj )
END IF

RETURN

END

!===============================================================================

SUBROUTINE write_prj_turb()

USE scipuff_fi
USE met_fi
USE files_fi

!------ write common / turb /

IMPLICIT NONE

INTEGER ios

WRITE(lun_prj,IOSTAT=ios) A,B,BS,VONK,CVRTX,CQB &
                         ,CSI1,CSI2,wwtrop,sltrop,epstrop,sle_fac &
                         ,uu_calm,sl_calm

IF( ios /= 0 )THEN
  nError   = WR_ERROR
  eRoutine = 'write_prj_turb'
  eMessage = 'Error writing project file'
  CALL ReportFileName( eInform,'File=',file_prj )
END IF

RETURN
END

!===============================================================================

SUBROUTINE write_prj_pprm()

USE scipuff_fi
USE files_fi

!------ write common / puff_param /

IMPLICIT NONE

INTEGER ios

WRITE(lun_prj,IOSTAT=ios) rrmrge,simrge,cmin,delmin,asplt,asplt2 &
                         ,aspltc,dxsplt,dzsplt,delx2,delz2,fac_rfl

IF( ios /= 0 )THEN
  nError   = WR_ERROR
  eRoutine = 'write_prj_pprm'
  eMessage = 'Error writing project file'
  CALL ReportFileName( eInform,'File=',file_prj )
END IF

RETURN
END

!===============================================================================

SUBROUTINE write_prj_pgrd()

USE scipuff_fi
USE files_fi

!------ write common / puff_grd /

IMPLICIT NONE

INTEGER ios

WRITE(lun_prj,IOSTAT=ios) xmin,xmax,ymin,ymax,zmax,mgrd &
                         ,hres,vres,dxg,dyg,dzg,nx,ny,nz &
                         ,lon0,lat0,xref,yref

IF( ios /= 0 )THEN
  nError   = WR_ERROR
  eRoutine = 'write_prj_pgrd'
  eMessage = 'Error writing project file'
  CALL ReportFileName( eInform,'File=',file_prj )
END IF

RETURN
END

!===============================================================================

SUBROUTINE write_prj_pmtl()

USE scipuff_fi
USE files_fi

!------ write common / puff_matl /

IMPLICIT NONE

INTEGER ios, i, igas, iprt

nclass  = MAXCLS
igas = -999
iprt = -999
mxsgp   = MAXSGP
WRITE(lun_prj,IOSTAT=ios) nclass,igas,iprt,ntypm,ntypp,mxsgp &
                         ,nmaux &
                         ,(material(i),i=1,ntypm) &
                         ,(mat_aux(i),i=1,nmaux) &
                         ,(typeID(i),i=1,ntypp) &
                         ,(namec(i),i=1,MAXCLS)
IF( ios /= 0 )THEN
  nError   = WR_ERROR
  eRoutine = 'write_prj_pmtl'
  eMessage = 'Error writing project file'
  CALL ReportFileName( eInform,'File=',file_prj )
END IF

RETURN
END

!===============================================================================

SUBROUTINE write_prj_smtl()

USE scipuff_fi
USE files_fi

!------ write surface parameters

IMPLICIT NONE

INTEGER ios

WRITE(lun_prj,IOSTAT=ios,ERR=9998) ntyps, ntypd

9999 CONTINUE

RETURN

9998    CONTINUE
nError   = WR_ERROR
eRoutine = 'write_prj_smtl'
eMessage = 'Error writing project file'
CALL ReportFileName( eInform,'File=',file_prj )
GOTO 9999

END

!===============================================================================

SUBROUTINE write_prj_cntrl()

USE scipuff_fi
USE met_fi
USE files_fi

!------ write common / cntrl /

IMPLICIT NONE

INTEGER ios

LOGICAL lRadPrj   !for compatibility with NFAC versions

lRadPrj = .FALSE.
WRITE(lun_prj,IOSTAT=ios) t_avg,lRadPrj,lter,lmap,local,lsplitz &
                         ,lzi_prj,lymd,dose,surface,tzone,jul_start &
                         ,year_start,month_start,day_start &
                         ,year_end,month_end,day_end &
                         ,tstart,tend,tend_hr &
                         ,delt,t_save,dt_save,t_old_r

IF( ios /= 0 )THEN
  nError   = WR_ERROR
  eRoutine = 'write_prj_cntrl'
  eMessage = 'Error writing project file'
  CALL ReportFileName( eInform,'File=',file_prj )
END IF

RETURN
END

!===============================================================================

SUBROUTINE write_prj_ext()

USE scipuff_fi
USE met_fi
USE files_fi
USE substrate_fi

!------ write extensions for new versions

IMPLICIT NONE

INTEGER ios, i

INTEGER hazard        !For compatibility with HAZARD versions
INTEGER numNWPN      !For compatibility with NWPN versions

hazard = 0
numNWPN = 0

WRITE(lun_prj,IOSTAT=ios) nzbl,dynamic,dense_gas &
                         ,z_dosage, smpfile, utm_zone, static &
                         ,multicomp,hazard,run_mode,dt_smp,prjEffects &
                         ,substrate_type,porosity,tortuosity,grain_size &
                         ,numNWPN,numRelID
IF( ios /= 0 )THEN
  nError   = WR_ERROR
  eRoutine ='write_prj_ext'
  eMessage ='Error writing project file'
  CALL ReportFileName( eInform,'File=',file_prj )
END IF

!------ Write releaseID data

IF( numRelID > 0 )THEN

  WRITE(lun_prj,IOSTAT=ios) (releaseID(i),i=1,numRelID)
  IF( ios /= 0 )THEN
    nError   = WR_ERROR
    eRoutine = 'write_prj_ext'
    eMessage = 'Error writing project file'
    CALL ReportFileName( eInform,'File=',file_prj )
  END IF

END IF

RETURN
END

!===============================================================================

SUBROUTINE write_prj_mc()

USE scipuff_fi
USE files_fi
USE chem_fi

!------ write multicomponent stuff for new versions

IMPLICIT NONE

INTEGER ios

WRITE(lun_prj,IOSTAT=ios) DoChemistry
IF( ios /= 0 )GOTO 9998

WRITE(lun_prj,IOSTAT=ios) mat_mc%nMCtype
IF( ios /= 0 )GOTO 9998

WRITE(lun_prj,IOSTAT=ios) mat_mc%type,mat_mc%ID
IF( ios /= 0 )GOTO 9998

CALL WriteChemMC()
IF( nError /= NO_ERROR )GOTO 9999

9999 CONTINUE

RETURN

9998 CONTINUE
nError   = WR_ERROR
eRoutine = 'write_prj_mc'
eMessage = 'Error writing project file'
CALL ReportFileName( eInform,'File=',file_prj )
GOTO 9999

END

!===============================================================================

SUBROUTINE write_prj_met()

USE scipuff_fi
USE met_fi
USE files_fi

IMPLICIT NONE

INTEGER ios, i, ifld, nxy

INTEGER(KIND=1) :: AuxCoord(SIZE_MapCoordPrjTransfer)

TYPE( SWIMgridStr ), POINTER :: grd

WRITE(lun_prj,IOSTAT=ios) numMet, mcTypePrj

metFields : DO ifld = 1,numMet

  grd => MetGrid(ifld)

  AuxCoord = TRANSFER(grd%coord,AuxCoord,SIZE_MapCoordPrjTransfer)

  IF( grd%basic == 0 )THEN
    WRITE(lun_prj,IOSTAT=ios) grd%nx,grd%ny,grd%dx,grd%dy,grd%xmin,grd%ymin, &
                              grd%xminPrj,grd%xmaxPrj,grd%yminPrj,grd%ymaxPrj, &
                              grd%lter,(AuxCoord(i),i=1,SIZE_MapCoordPrjTransfer)
  ELSE
    i = ISHFT(grd%basic,16)
    i = IOR(i,grd%ny)
    WRITE(lun_prj,IOSTAT=ios) -grd%nx,i,grd%dx,grd%dy,grd%xmin,grd%ymin, &
                               grd%xminPrj,grd%xmaxPrj,grd%yminPrj,grd%ymaxPrj, &
                               grd%lter,(AuxCoord(i),i=1,SIZE_MapCoordPrjTransfer)
  END IF
  IF( ios /= 0 )THEN
    nError   = WR_ERROR
    eRoutine = 'write_prj_met'
    eMessage = 'Error writing met grid definitions'
    CALL ReportFileName( eInform,'File=',file_prj )
  END IF

  IF( grd%lter )THEN

    nxy = grd%nX * grd%nY

    WRITE(lun_prj,IOSTAT=ios) (grd%H(i),i=1,nxy),(grd%Hx(i),i=1,nxy), &
                              (grd%Hy(i),i=1,nxy),grd%Hmin

    IF( ios /= 0 )THEN
      nError   = WR_ERROR
      eRoutine = 'write_prj_met'
      eMessage = 'Error writing met terrain fields'
      CALL ReportFileName( eInform,'File=',file_prj )
    END IF

  END IF

END DO metFields

RETURN
END

!===============================================================================

SUBROUTINE write_prj_adjoint()

USE scipuff_fi
USE files_fi
USE adjoint_fi

!------ write multicomponent stuff for new versions

IMPLICIT NONE

INTEGER ios, i

WRITE(lun_prj,IOSTAT=ios) (AdjMat(i),i=1,ntypm)
IF( ios /= 0 )THEN
  nError   = WR_ERROR
  eRoutine = 'write_prj_adjoint'
  eMessage = 'Error writing project file'
  CALL ReportFileName( eInform,'File=',file_prj )
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE write_prj_wetpLiquid()

USE scipuff_fi
USE files_fi

!------ write auxiliary liquid data for wet particles

IMPLICIT NONE

INTEGER ios, i

CHARACTER(16) auxTitle

auxTitle = 'WETP_LIQUID'

WRITE(lun_prj,IOSTAT=ios) auxTitle
IF( ios == 0 )WRITE(lun_prj,IOSTAT=ios) nWetPartLiq, &
                                       (WetPartLiquidMat(i),i=1,nWetPartLiq)
IF( ios /= 0 )THEN
  nError   = WR_ERROR
  eRoutine = 'write_prj_wetpLiquid'
  eMessage = 'Error writing project file'
  CALL ReportFileName( eInform,'File=',file_prj )
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END

