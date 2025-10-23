SUBROUTINE old_start()

USE scipuff_fi
USE surface_fi
USE files_fi
USE cont_rel_fi
USE PtrGrdStrItf
USE sagdef_fd
USE PtrGrdStrItf

IMPLICIT NONE

REAL    delt_old
INTEGER i, j, ilev
INTEGER is, maxlev
INTEGER ndos_blocks, irv

CHARACTER(PATH_MAXLENGTH) :: file_dosAdj

INTEGER, EXTERNAL :: SAG_CloseID
INTEGER, EXTERNAL :: SAG_ClearGridID

!-----  open output file & read first records

CALL read_output()
IF( nError /= NO_ERROR )GOTO 9999

!-----  open/read surface dose file

IF( surface )THEN

  CALL read_surface( lun_dep,file_dep,srfdep )
  IF( nError /= NO_ERROR )GOTO 9999

!----- Get pointer to dep structure

  Psrfdep => SAG_PtrGrdStr( srfdep ) ! Associate "local" grid structure pointer

!----- Get pointer to dep aux structure

  Pauxdep => SAG_PtrAuxStr( srfdep ) ! Associate "local" grid structure pointer

  IF( BTEST(run_mode,DINCRMNT) )THEN
    IF( t > 0. )THEN
      Psrfdep => SAG_PtrGrdStr( srfdep )
      maxlev = Psrfdep%maxlev
      i = SAG_ClearGridID( srfdep )
      IF( i /= SAG_OK )THEN
        nError = WR_ERROR
        eRoutine = 'old_start'
        eMessage = 'Error clearing surface dep grid'
        GOTO 9999
      END IF
      tLastDep = t
      Psrfdep%maxlev = maxlev
    END IF
  END IF

END IF

IF( dose )THEN

  IF( BTEST(run_mode,REVERSE_MODE) )THEN
    ndos_blocks = ntypd/3
    ALLOCATE( srfdosAdj(ndos_blocks),STAT=irv )

    DO i = 1,ndos_blocks
      WRITE(file_dosAdj,'(A,I3.3)') TRIM(file_dos),i

      CALL read_surface( lun_dos,file_dosAdj,srfdosAdj(i) )
      IF( nError /= NO_ERROR )GOTO 9999

      irv = SAG_CloseID( srfdosAdj(i) )
      IF( irv /= SAG_OK )THEN
        nError = UK_ERROR
        eRoutine = 'create_surface'
        eMessage = 'Error closing surface dose field'
        CALL ReportFileName( eInform,'File=',file_dosAdj )
        GOTO 9999
      END IF
    END DO

  ELSE

  CALL read_surface( lun_dos,file_dos,srfdos )
  IF( nError /= NO_ERROR )GOTO 9999

!----- Get pointer to dos structure

  Psrfdos => SAG_PtrGrdStr( srfdos ) ! Associate "local" grid structure pointer

!----- Get pointer to dep aux structure

  Pauxdos => SAG_PtrAuxStr( srfdos ) ! Associate "local" grid structure pointer

  IF( multicomp )THEN
    CALL read_surface( lun_ados,file_ados,srfados )
    IF( nError /= NO_ERROR )GOTO 9999
  !----- Get pointer to ambient dos structure
    Psrfados => SAG_PtrGrdStr( srfados ) ! Associate "local" grid structure pointer
  !----- Get pointer to ambient dos aux structure
    Pauxados => SAG_PtrAuxStr( srfados ) ! Associate "local" grid structure pointer
  END IF


  END IF

  tLastDos = t
  IF( BTEST(run_mode,DINCRMNT) )THEN
    IF( t > 0. )THEN
      Psrfdos => SAG_PtrGrdStr( srfdos )
      maxlev = Psrfdos%maxlev
      i = SAG_ClearGridID( srfdos )
      IF( i /= SAG_OK )THEN
        nError = WR_ERROR
        eRoutine = 'old_start'
        eMessage = 'Error clearing surface dose grid'
        GOTO 9999
      END IF
      Psrfdos%maxlev = maxlev
      IF( multicomp )THEN
        Psrfados => SAG_PtrGrdStr( srfados )
        maxlev = Psrfados%maxlev
        i = SAG_ClearGridID( srfados )
        IF( i /= SAG_OK )THEN
          nError = WR_ERROR
          eRoutine = 'old_start'
          eMessage = 'Error clearing surface ambient dose grid'
          GOTO 9999
        END IF
        Psrfados%maxlev = maxlev
      END IF
    END IF
  END IF

END IF

!------ Zero ipgrd

CALL allocate_ip( nz )
IF( nError /= NO_ERROR )GOTO 9999

CALL zero_ip()

!------ Set ipgrd

CALL set_ip( 1,npuf )
IF( nError /= NO_ERROR )GOTO 9999

!------ Read new time input

delt_old = delt
IF( static )THEN
  delt_static  = delt_old
  t_rst_static = t
ELSE
  delt_static  = NOT_SET_R
  t_rst_static = NOT_SET_R
END IF

CALL init_time2()

CALL ReadNamelistTime2( lun_inp )
IF( nError /= NO_ERROR )THEN
  eRoutine = 'old_start'
  CALL ReportFileName( eInform,'File=',file_inp )
  GOTO 9999
END IF

!-----  Check for change in time step

IF( delt_old < delt )THEN
  ilev = 0
  DO WHILE( delt_old < delt .AND. ilev < MAXTLV )
    ilev = ilev + 1
    delt_old = 2.0*delt_old
  END DO
  DO i = 1,npuf
    IF( puff(i)%idtl >= 0. )THEN
      puff(i)%idtl = puff(i)%idtl + ilev
      IF( puff(i)%idtl > MAXTLV )GOTO 9998
    END IF
  END DO
  DO i = 1,numDefinition
    IF( cDefinition(i)%state == CR_EMPTY )CYCLE
    IF( cDefinition(i)%rSet%nrel > 0 )THEN
      DO j = 1,cDefinition(i)%rSet%nrel
        cDefinition(i)%rSet%rels(j)%basePuff%idtl = cDefinition(i)%rSet%rels(j)%basePuff%idtl + ilev
        IF( cDefinition(i)%rSet%rels(j)%basePuff%idtl > MAXTLV )GOTO 9998
      END DO
    END IF
  END DO
END IF

!-----  Set puff and release time step lists

CALL set_tlev(.TRUE.)

9999 CONTINUE

RETURN

9998 CONTINUE
nError   = SZ_ERROR
eRoutine = 'old_start'
eMessage = 'Too many time-step refinement levels'
WRITE(eInform,'(A,I5)')'Maximum number is ',MAXTLV
eAction  = 'Use a smaller maximum time step'
GOTO 9999

END

!=======================================================================

SUBROUTINE read_output()

USE scipuff_fi
USE met_fi
USE files_fi
USE cont_rel_fi

IMPLICIT NONE

INTEGER ios

!------ read project file

CALL read_prj()
IF( nError /= NO_ERROR )GOTO 9999

!------ read puff file

CALL read_puff()
IF( nError /= NO_ERROR )GOTO 9999

IF( multicomp )THEN
  CALL RestartMC()
  IF( nError /= NO_ERROR )GOTO 9999
END IF

!------ Clear all static puffs for restart

CALL restart_static_puffs()

9999 CONTINUE

CLOSE(UNIT=lun_prj,IOSTAT=ios)

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE RestartMC()

USE error_fi

IMPLICIT NONE

CALL RestartChemMC()
IF( nError /= NO_ERROR )GOTO 9999

9999 CONTINUE

RETURN
END
