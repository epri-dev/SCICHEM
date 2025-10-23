!===============================================================================
!     ReadMaterial
!===============================================================================
SUBROUTINE ReadMaterial( file,lunit,searchName )

USE scipuff_fi
USE matdef_fi

!     Reads the SCIPUFF MATDEF namelists from the input file (*.INP)

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: file
INTEGER,      INTENT( IN ) :: lunit
CHARACTER(*), INTENT( IN ) :: searchName

INTEGER i, ios, nmatStart
LOGICAL lseek, read_v02

!==== Initialize

lseek = searchName /= ''//CHAR(0) .AND. searchName /= ' ' &
                                  .AND. searchName /= 'ALL'

!==== If searching for specific name - check to see if already have it

IF( ALLOCATED(material) )THEN
  IF( lseek )THEN
    DO i = 1,ntypm
      IF( TRIM(material(i)%cmat) == TRIM(searchName) )GOTO 1000
    END DO
  END IF
END IF

!==== Open the file

read_v02 = .FALSE.

2000 CONTINUE
OPEN( UNIT=lunit,FILE=file,STATUS='OLD',ACTION="READ",IOSTAT=ios,DELIM='APOSTROPHE' )
IF( ios /= 0 )THEN
  nError   = OP_ERROR
  eRoutine = 'ReadMaterial'
  eMessage = 'Error opening SCIPUFF input file'
  GOTO 9998
END IF

!==== Read the namelists
!       if lseek - read until find desired material
!       else     - read until encounter end-of-file

nmatStart = ntypm

DO WHILE( nError == NO_ERROR )
  IF( read_v02 )THEN
    CALL ReadNamelistMatdef_v02( lunit )
  ELSE
    CALL ReadNamelistMatdef( lunit )
    IF( nError == RD_ERROR )THEN
      IF( ntypm == nmatStart )THEN
        read_v02 = .TRUE.
        CALL init_error()
        CLOSE( UNIT=lunit,IOSTAT=ios )
        GOTO 2000
      END IF
    END IF
  END IF
  IF( nError == NO_ERROR )THEN
    IF( lseek )THEN
      IF( TRIM(mname) == TRIM(searchName) )THEN
        CALL PutMaterial()
        IF( nError /= NO_ERROR )GOTO 9999
        EXIT
      END IF
    ELSE
      CALL PutMaterial()
      IF( nError /= NO_ERROR )GOTO 9999
    END IF
  END IF
END DO

!==== Do some error checking

1000 CONTINUE
IF( nError == EOF_ERROR )THEN
  IF( nmatStart == ntypm )THEN
    eRoutine = 'ReadMaterial'
    eMessage = 'EOF encountered reading MATDEF namelists'
    IF( lseek )THEN
      eInform = 'Requested material not found : '//TRIM(searchName)
    ELSE
      eInform = 'No valid material definitions found'
    END IF
  ELSE
    CALL init_error()
  END IF
END IF

IF( nError /= NO_ERROR )GOTO 9999

!==== Close the file and return

9999 CONTINUE

CLOSE( UNIT=lunit,IOSTAT=ios )

RETURN

9998 CONTINUE

CALL ReportFileName( eInform,'File=',file )
GOTO 9999

END
!===============================================================================
!     WriteMaterial
!===============================================================================
SUBROUTINE WriteMaterial( file,lunit,searchName )

USE scipuff_fi

!     Writes the SCIPUFF MATDEF namelists to the input file (*.INP)

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: file
INTEGER     , INTENT( IN ) :: lunit
CHARACTER(*), INTENT( IN ) :: searchName

INTEGER ios
INTEGER i
LOGICAL lseek

!==== Initialize

lseek = searchName /= ''//CHAR(0) .AND. searchName /= ' ' &
                                  .AND. searchName /= 'ALL'

!==== Open the file

OPEN( UNIT=lunit,FILE=file,STATUS='UNKNOWN',POSITION='APPEND',IOSTAT=ios,DELIM='APOSTROPHE' )
IF( ios /= 0 )THEN
  nError   = OP_ERROR
  eRoutine = 'WriteMaterial'
  eMessage = 'Error opening SCIPUFF input file'
  GOTO 9998
END IF

!==== write the namelist

DO i = 1,ntypm
  IF( lseek )THEN
    IF( TRIM(material(i)%cmat) == TRIM(searchName) )THEN
      CALL GetMaterial( i )
      IF( nError /= NO_ERROR )GOTO 9999
      CALL WriteNamelistMatdef( lunit )
      IF( nError /= NO_ERROR )GOTO 9999
      EXIT
    END IF
  ELSE
    CALL GetMaterial( i )
    IF( nError /= NO_ERROR )GOTO 9999
    CALL WriteNamelistMatdef( lunit )
    IF( nError /= NO_ERROR )GOTO 9999
  END IF
END DO

!==== Close the file and return

9999 CONTINUE

CLOSE( UNIT=lunit,IOSTAT=ios )

RETURN

9998 CONTINUE

CALL ReportFileName( eInform,'File=',file )
GOTO 9999

END
!===============================================================================

SUBROUTINE ReadWetPartLiquid( file,lunit,LiquidMat,LiqName )

USE scipuff_fi
USE matdef_fi
USE class_fd
USE files_fi

CHARACTER(*),            INTENT( IN  ) :: file
INTEGER,                 INTENT( IN  ) :: lunit
TYPE( liquid_material ), INTENT( OUT ) :: LiquidMat
CHARACTER(*),            INTENT( OUT ) :: LiqName

CALL ReadNamelistMatdef( lunit )
IF( nError /= NO_ERROR )THEN
  eRoutine = 'ReadWetPartLiquid'
  eMessage = 'Error reading wet particle auxiliary file'
  CALL ReportFileName( eInform,'File=',file )
  eAction  = 'Must contain liquid material MATDEF namelist'
  GOTO 9999
END IF

IF( class /= MAT_LIQ )THEN
  nError   = RD_ERROR
  eRoutine = 'ReadWetPartLiquid'
  eMessage = 'Auxiliary material must be a liquid type'
  CALL ReportFileName( eInform,'File=',file )
  GOTO 9999
END IF

!==== Check the liquid density

IF( liquid_density(1) <= 0.0 )THEN
  nError   = RD_ERROR
  eRoutine = 'ReadWetPartLiquid'
  eMessage = 'Auxiliary liquid density must be positive'
  CALL ReportFileName( eInform,'File=',file )
  GOTO 9999
END IF

!==== Check the Molecular Weight

IF( mweight <= 0.0 )THEN
  nError   = RD_ERROR
  eRoutine = 'ReadWetPartLiquid'
  eMessage = 'Auxiliary liquid molecular weight must be positive'
  CALL ReportFileName( eInform,'File=',file )
  GOTO 9999
END IF

!==== Check the surface tension

IF( surf_tension <= 0.0 )THEN
  nError   = RD_ERROR
  eRoutine = 'ReadWetPartLiquid'
  eMessage = 'Auxiliary liquid surface tension must be positive'
  CALL ReportFileName( eInform,'File=',file )
  GOTO 9999
END IF

!---- Initialize data structure

LiquidMat%nsg       = 0                       !No. of subgroups
LiquidMat%sf        = NOT_SET_R               !Spread factor (Secondary Evaporation flag)
LiquidMat%viscosity = NOT_SET_R               !Viscosity (for sfc absorption)
LiquidMat%dum       = NOT_SET_R               !Extra space for expansion
LiquidMat%dmin      = NOT_SET_R               !Min diameter
LiquidMat%dbar      = NOT_SET_R               !Mean diameter
LiquidMat%sigvd     = NOT_SET_R               !Sigma speed factor
LiquidMat%diff      = NOT_SET_R               !Brownian diffusion
LiquidMat%dmax      = NOT_SET_R               !Max diameter

LiquidMat%rho  = liquid_density(1)            !Density
LiquidMat%rhob = liquid_density(2)            !Density Temperature coefficient
LiquidMat%a    = antoine(1)                   !Antoine Coefficient a
LiquidMat%b    = antoine(2)                   !Antoine Coefficient b
LiquidMat%c    = antoine(3)                   !Antoine Coefficient c
LiquidMat%w    = mweight                      !Molecular Weight
LiquidMat%st   = surf_tension                 !Surface Tension

LiqName = TRIM(mname)

9999 CONTINUE

RETURN
END
