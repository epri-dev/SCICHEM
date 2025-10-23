!===============================================================================
!     ReadNamelistMatdef
!===============================================================================
SUBROUTINE ReadNamelistMatdef( iunit )

USE default_fd
USE error_fi
USE matdef_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: iunit

REAL psize(10*MAXSGP)
REAL pbounds(10*MAXSGP+1)

INTEGER ios
INTEGER i
REAL    NWPN_decay                 !Compatibility with NWPN
INTEGER effectClass,effectAvail    !Compatibility with FXPLOT

INTEGER, EXTERNAL :: FindNML

NAMELIST / matdef / class,mname,units,file_name,file_path, &
                    group_deposition,group_dose, &
                    total_deposition,total_dose,multi_comp, &
                    conc_min,decay_amp,decay_min, &
                    NWPN_decay,effectClass,effectAvail, &
                    density,gas_deposition, &
                    antoine,liquid_density,mweight,spread_factor, &
                    surf_tension,evap_min, &
                    specific_heat_liq,specific_heat_vap,viscosity, &
                    nsg,psize,pbounds

!==== Initialize

CALL InitMatdef( psize,pbounds )

!==== Read

ios = FindNML( iunit,'matdef' )

IF( ios == 0 )READ(UNIT=iunit,NML=matdef,IOSTAT=ios)

! Check for end of file in all compilers
IF( IS_IOSTAT_END(ios) )ios = -1

IF( ios > 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'ReadNamelistMatdef'
  eMessage = 'Error reading MATDEF namelist'
  GOTO 9999
ELSE IF( ios < 0 )THEN
  nError   = EOF_ERROR
  eRoutine = 'ReadNamelistMatdef'
  eMessage = 'EOF reading MATDEF namelist'
  GOTO 9999
END IF

!==== Reset

IF( conc_min == NOT_SET_R )THEN
  conc_min = evap_min
  IF( conc_min == NOT_SET_R )conc_min = 0.0
END IF

CALL cupper( mname )
CALL cupper( class )

DO i = 1,nsg
  binSize(i)   = psize(i)
  binBounds(i) = pbounds(i)
END DO
binBounds(nsg+1) = pbounds(nsg+1)

9999 CONTINUE

RETURN
END
!===============================================================================
!     ReadNamelistMatdef_v02
!===============================================================================
SUBROUTINE ReadNamelistMatdef_v02( iunit )

USE error_fi
USE matdef_fi, gas_deposition_v02 => gas_deposition

IMPLICIT NONE

INTEGER, INTENT( IN ) :: iunit

!===== NOTE
!     We change the name used for
!     gas_deposition because in v02 gas_deposition was an array.
!===== NOTE

INTEGER ios
INTEGER i

INTEGER, EXTERNAL :: FindNML

INTEGER  nsubgroup
REAL     gas_deposition(10*MAXSGP)
REAL     particle_size(10*MAXSGP)
REAL     particle_bounds(10*MAXSGP+1)

NAMELIST / matdef / class,mname,units, &
                    group_deposition,group_dose, &
                    total_deposition,total_dose, &
                    decay_amp,decay_min, &
                    density,gas_deposition, &
                    nsubgroup,particle_size,particle_bounds

!==== Initialize

CALL InitMatdef( particle_size,particle_bounds )

nsubgroup = nsg
DO i = 1,MAXSGP
  gas_deposition(i) = gas_deposition_v02
END DO

!==== Read

ios = FindNML( iunit,'matdef' )

IF( ios == 0 )READ(UNIT=iunit,NML=matdef,IOSTAT=ios)

! Check for end of file in all compilers
IF( IS_IOSTAT_END(ios) )ios = -1

IF( ios > 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'ReadNamelistMatdef_v02'
  eMessage = 'Error reading MATDEF namelist'
  GOTO 9999
ELSE IF( ios < 0 )THEN
  nError   = EOF_ERROR
  eRoutine = 'ReadNamelistMatdef_v02'
  eMessage = 'EOF reading MATDEF namelist'
  GOTO 9999
END IF

!==== Reset

gas_deposition_v02 = gas_deposition(1)

nsg = nsubgroup
DO i = 1,nsg
  binSize(i)   = particle_size(i)
  binBounds(i) = particle_bounds(i)
END DO
binBounds(nsg+1) = particle_bounds(nsg+1)

CALL cupper( mname )
CALL cupper( class )

9999 CONTINUE

RETURN
END
!===============================================================================
!     WriteNamelistMatdef
!===============================================================================
SUBROUTINE WriteNamelistMatdef( iunit )

USE class_fd
USE error_fi
USE matdef_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: iunit

SELECT CASE( TRIM(class) )
  CASE( MAT_GAS, MAT_NULL, MAT_SSAT )
    CALL WriteNamelistMatdefGas( iunit )
  CASE( MAT_LIQ )
    CALL WriteNamelistMatdefLiquid( iunit )
  CASE( MAT_AER )
    CALL WriteNamelistMatdefAerosol( iunit )
  CASE( MAT_WET )
    CALL WriteNamelistMatdefParticle( iunit )
  CASE( MAT_PRT )
    CALL WriteNamelistMatdefParticle( iunit )
  CASE DEFAULT
    nError = IV_ERROR
    eRoutine = 'WriteNamelistMatdef'
    eMessage = 'Invalid material class designation'
    eInform  = 'Class = '//TRIM(class)
END SELECT

RETURN
END
!===============================================================================
!     WriteNamelistMatdefGas
!===============================================================================
SUBROUTINE WriteNamelistMatdefGas( iunit )

USE error_fi
USE matdef_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: iunit

INTEGER ios

NAMELIST / matdef / class,mname,units,file_name,file_path, &
                    group_deposition,group_dose, &
                    multi_comp, &
                    conc_min,decay_amp,decay_min, &
                    density,gas_deposition

WRITE(UNIT=iunit,NML=matdef,IOSTAT=ios)
IF( ios /= 0 )THEN
  nError   = WR_ERROR
  eRoutine = 'WriteNamelistMatdefGas'
  eMessage = 'Error writing gas MATDEF namelist'
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END
!===============================================================================
!     WriteNamelistMatdefLiquid
!===============================================================================
SUBROUTINE WriteNamelistMatdefLiquid( iunit )

USE error_fi
USE matdef_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: iunit

INTEGER ios
INTEGER i

REAL psize(MAXSGP)
REAL pbounds(MAXSGP+1)

NAMELIST / matdef / class,mname,units,file_name,file_path, &
                    group_deposition,group_dose, &
                    total_deposition,total_dose,multi_comp, &
                    conc_min,decay_amp,decay_min, &
                    density,gas_deposition, &
                    antoine,liquid_density,mweight,spread_factor, &
                    surf_tension,viscosity, &
                    nsg,psize,pbounds, &
                    specific_heat_liq,specific_heat_vap

DO i = 1,MAXSGP
  psize(i)   = binSize(i)
  pbounds(i) = binBounds(i)
END DO
pbounds(MAXSGP+1) = binBounds(MAXSGP+1)

WRITE(UNIT=iunit,NML=matdef,IOSTAT=ios)
IF( ios /= 0 )THEN
  nError   = WR_ERROR
  eRoutine = 'WriteNamelistMatdefLiquid'
  eMessage = 'Error writing liquid MATDEF namelist'
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END
!===============================================================================
!     WriteNamelistMatdefParticle
!===============================================================================
SUBROUTINE WriteNamelistMatdefParticle( iunit )

USE error_fi
USE matdef_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: iunit

INTEGER ios
INTEGER i

REAL psize(MAXSGP)
REAL pbounds(MAXSGP+1)

NAMELIST / matdef / class,mname,units,file_name,file_path, &
                    group_deposition,group_dose, &
                    total_deposition,total_dose,multi_comp, &
                    conc_min,decay_amp,decay_min, &
                    density, &
                    nsg,psize,pbounds


DO i = 1,MAXSGP
  psize(i)   = binSize(i)
  pbounds(i) = binBounds(i)
END DO
pbounds(MAXSGP+1) = binBounds(MAXSGP+1)

WRITE(UNIT=iunit,NML=matdef,IOSTAT=ios)
IF( ios /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'WriteNamelistMatdefParticle'
  eMessage = 'Error writing particle MATDEF namelist'
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END
!===============================================================================
!     WriteNamelistMatdefAerosol
!===============================================================================
SUBROUTINE WriteNamelistMatdefAerosol( iunit )

USE error_fi
USE matdef_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: iunit

INTEGER ios

INTEGER effectClass,effectAvail    !Compatibility with FXPLOT

NAMELIST / matdef / class,mname,units,file_name,file_path, &
                    group_deposition,group_dose, &
                    total_deposition,total_dose,multi_comp, &
                    conc_min,decay_amp,decay_min, &
                    gas_deposition,effectClass,effectAvail, &
                    antoine,liquid_density,mweight, &
                    specific_heat_liq,specific_heat_vap

WRITE(UNIT=iunit,NML=matdef,IOSTAT=ios)
IF( ios /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'WriteNamelistMatdefAerosol'
  eMessage = 'Error writing aerosol MATDEF namelist'
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END
