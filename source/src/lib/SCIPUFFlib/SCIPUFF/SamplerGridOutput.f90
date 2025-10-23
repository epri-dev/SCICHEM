!==============================================================================
!------ Routines for writing gridded sensor ouptut to XML file
!==============================================================================
MODULE SamplerGridOuput

USE sampler_fi
USE error_fi
USE files_fi

IMPLICIT NONE

CHARACTER(1), PARAMETER :: QQ  = CHAR(34)  !Double quote
CHARACTER(1), PARAMETER :: SL  = CHAR(47)  !Forward slash
CHARACTER(1), PARAMETER :: OP  = CHAR(60)  !Open angle bracket
CHARACTER(1), PARAMETER :: CL  = CHAR(62)  !Close angle bracket

CHARACTER(1), PARAMETER :: TabStr1 = CHAR(9)
CHARACTER(2), PARAMETER :: TabStr2 = TabStr1//CHAR(9)
CHARACTER(3), PARAMETER :: TabStr3 = TabStr2//CHAR(9)
CHARACTER(4), PARAMETER :: TabStr4 = TabStr3//CHAR(9)
CHARACTER(5), PARAMETER :: TabStr5 = TabStr4//CHAR(9)

CONTAINS

!==============================================================================

SUBROUTINE SamplerGridHeader()

INTEGER ios

CHARACTER(16) coord

!------ XML version

WRITE(lun_smp,FMT='(A)',IOSTAT=ios) '<?xml version="1.0" encoding="UTF-8"?>'
IF( ios /= 0 )THEN
  nError   = WR_ERROR
  eRoutine = 'SamplerGridHeader'
  eMessage = 'Error writing XML version'
  GOTO 9999
END IF

!------ GridOutput

WRITE(lun_smp,FMT='(A)',IOSTAT=ios)  &
  OP//'GridOutput xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="GridOutput_L3.xsd"'//CL
IF( ios /= 0 )THEN
  nError   = WR_ERROR
  eRoutine = 'SamplerGridHeader'
  eMessage = 'Error writing GridOutput record'
  GOTO 9999
END IF

!------ Descriptor: title and coordinate type

SELECT CASE( sampMap )
  CASE( I_LATLON )
    coord = 'LATLON'
  CASE( I_UTM )
    WRITE(coord,FMT='(A,I2)',IOSTAT=ios) 'UTM ',sampZone
  CASE DEFAULT
    coord = 'Local Cartesian'
END SELECT

IF( LEN_TRIM(SampTitle) > 0 )THEN
  WRITE(lun_smp,FMT='(A)',IOSTAT=ios) TabStr1// &
    OP//'Descriptor Title='//QQ//TRIM(SampTitle)//QQ//' CoordType='//QQ//TRIM(coord)//QQ//CL
ELSE
  WRITE(lun_smp,FMT='(A)',IOSTAT=ios) TabStr1// &
    OP//'Descriptor CoordType='//QQ//TRIM(coord)//QQ//CL
END IF
IF( ios /= 0 )THEN
  nError   = WR_ERROR
  eRoutine = 'SamplerGridHeader'
  eMessage = 'Error writing Descriptor record'
  GOTO 9999
END IF

WRITE(lun_smp,FMT='(A)',IOSTAT=ios) TabStr1//OP//SL//'Descriptor'//CL
IF( ios /= 0 )THEN
  nError   = WR_ERROR
  eRoutine = 'SamplerGridHeader'
  eMessage = 'Error closing Descriptor record'
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END SUBROUTINE SamplerGridHeader

!==============================================================================

SUBROUTINE SamplerGridCloseHeader()

INTEGER ios

!------ GridOutput

WRITE(lun_smp,FMT='(A)',IOSTAT=ios)  OP//SL//'GridOutput'//CL
IF( ios /= 0 )THEN
  nError   = WR_ERROR
  eRoutine = 'SamplerGridCloseHeader'
  eMessage = 'Error closing GridOutput record'
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END SUBROUTINE SamplerGridCloseHeader

!==============================================================================

SUBROUTINE SamplerGridFieldDescriptors()

USE sampler_fi

INTEGER alloc_stat, ios, is, nv, nv0, iv, i, j, jj
LOGICAL lScale, lCycle
REAL    dmin, dmax

CHARACTER(64) BinSizeStr

TYPE( sensor ), POINTER :: ss

!------ Loop over all samplers

iv = 0

DO is = 1,nsmp

  ss => smp(is)

  nv = ss%nvar

!------ Allocate string arrays

  ALLOCATE( ss%FieldName(nv),ss%FieldUnits(nv),STAT=alloc_stat )
  IF( alloc_stat == 0 .AND. BTEST(ss%stype,STB_PART) )THEN
    ALLOCATE( ss%SizeBin(nv),STAT=alloc_stat )
  END IF
  IF( alloc_stat /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'SamplerGridFieldDescriptors'
    eMessage = 'Error allocating sensor descriptor arrays'
    GOTO 9999
  END IF

!------ Set default field names

  DO i = 1,nv
    ss%FieldName(i)  = TRIM(smp_vname(iv+i))
    ss%FieldUnits(i) = TRIM(smp_units(iv+i))
  END DO

!------ Check for "special" names if appropriate (but skip "non-standard" sensors)

  lCycle = BTEST(ss%stype,STB_MET)
  lCycle = lCycle .OR. BTEST(ss%stype,STB_CORR)
  lCycle = lCycle .OR. BTEST(ss%stype,STB_MULT)

  IF( lCycle )THEN
    iv = iv + nv
    CYCLE
  END IF

!------ Check for time-scale based on smp_vname (a bit of a kludge)

  IF( nv >= 3 )THEN
    lScale = smp_vname(iv+3)(1:1) == 'T'
  ELSE
    lScale = .FALSE.
  END IF


!------ Check for "by size"

  IF( BTEST(ss%stype,STB_PART) )THEN

    IF( lScale )THEN
      nv0 = 4
    ELSE
      nv0 = 3
    END IF

!------ Loop over size bins

    DO i = 1,ss%nbin
      j = (i-1)*nv0
      ss%FieldName(j+1) = 'Mean'
      ss%FieldName(j+2) = 'Variance'
      IF( lScale )ss%FieldName(j+3) = 'TimeScale'
      ss%FieldName(j+nv0) = 'Number Density'

      dmin = ss%bin(i)   * 1.E6
      dmax = ss%bin(i+1) * 1.E6

      BinSizeStr = SetBinSizeStr( dmin,dmax )

      DO jj = j+1,j+nv0
        ss%SizeBin(jj) = TRIM(BinSizeStr)
      END DO

    END DO

!------ Check for deposition or dosage sensor

  ELSE IF( BTEST(ss%stype,STB_DEP) .OR. BTEST(ss%stype,STB_INTEGRATE) )THEN

    ss%FieldName(1) = 'Mean'
    ss%FieldName(2) = 'Variance'

    IF( BTEST(ss%stype,STB_DEP) )THEN
      IF( nv > 2 )ss%FieldName(3) = 'Surface' !Residual??
    END IF

    ios = DepSensorBinSize( smp(is)%var,dmin,dmax )  !Check if for particle size group
    IF( ios == 1 )THEN
      ALLOCATE( ss%SizeBin(nv),STAT=alloc_stat )
      IF( alloc_stat /= 0 )THEN
        nError   = UK_ERROR
        eRoutine = 'SamplerGridFieldDescriptors'
        eMessage = 'Error allocating sensor BinSize string array'
        GOTO 9999
      END IF
      BinSizeStr = SetBinSizeStr( dmin,dmax )
      DO i = 1,nv
        ss%SizeBin(i) = TRIM(BinSizeStr)
      END DO
    END IF

!------ Good old "Standard" sensor

  ELSE

    ss%FieldName(1) = 'Mean'
    ss%FieldName(2) = 'Variance'
    IF( lScale )ss%FieldName(3) = 'TimeScale'

  END IF

  iv = iv + nv

END DO

9999 CONTINUE

RETURN
END SUBROUTINE SamplerGridFieldDescriptors

!==============================================================================

INTEGER FUNCTION DepSensorBinSize( sVar,dmin,dmax ) RESULT( irv )

USE scipuff_fi
USE UtilMtlAux

CHARACTER(64), INTENT( IN  ) :: sVar
REAL,          INTENT( OUT ) :: dmin, dmax

INTEGER ios, i, j, imat, icls, nch

CHARACTER(16) matstring

LOGICAL, EXTERNAL :: IsParticle
LOGICAL, EXTERNAL :: IsWetParticle

TYPE( part_material ) :: pmatp

irv  = 0  !Initialization indicates no string
dmin = -999.
dmax = -999.

i = INDEX(sVar,':')

IF( i == 0 )GOTO 9999

matstring = ADJUSTL(TRIM(sVar(1:i-1)))

imat = -1
DO j = 1,ntypm
  IF( TRIM(material(j)%cmat) == TRIM(matstring) )THEN
    imat = j; EXIT
  END IF
END DO

IF( imat == -1 )GOTO 9999

icls = material(imat)%icls

IF( .NOT.(IsParticle(icls) .OR. IsWetParticle(icls)) )GOTO 9999  !Only get size for particle
nch = LEN_TRIM(sVar)
READ(sVar(i+1:nch),FMT=*,IOSTAT=ios) isg
IF( ios /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'DepSensorBinSize'
  eMessage = 'Error reading material subgroup number'
  WRITE(eInform,'(A)')'Material name is '//TRIM(matstring)
  GOTO  9999
END IF

IF( isg <= 0 )GOTO 9999

CALL GetParticleParam( pmatp,material(imat)%iaux,isg,mat_aux )

dmin = pmatp%dmin * 1.E6
dmax = pmatp%dmax * 1.E6


irv = 1

9999 CONTINUE

RETURN
END FUNCTION DepSensorBinSize

!==============================================================================

CHARACTER(64) FUNCTION SetBinSizeStr( d1,d2 )

REAL, INTENT( IN ) :: d1, d2

INTEGER ios

CHARACTER(64) s1, s2

SetBinSizeStr = 'Not Set'

WRITE( s1,FMT='(G10.4)',IOSTAT=ios) d1
IF( ios /= 0 )GOTO 9999

WRITE( s2,FMT='(G10.4)',IOSTAT=ios) d2
IF( ios /= 0 )GOTO 9999

s1 = TRIM(ADJUSTL(s1))
s2 = TRIM(ADJUSTL(s2))

SetBinSizeStr = TRIM(s1)//'-'//TRIM(s2)//' microns'

9999 CONTINUE

RETURN
END FUNCTION SetBinSizeStr

!==============================================================================

SUBROUTINE SamplerGridTime( tOut )

!------ Output time record

REAL, INTENT( IN ) :: tOut

INTEGER ios

CHARACTER(64) string

!------ Check that last record is "<\GridOutput>"

BACKSPACE(lun_smp,IOSTAT=ios)
READ(lun_smp,FMT='(A)',IOSTAT=ios) string
IF( ios /= 0 )GOTO 9999
IF( TRIM(ADJUSTL(string)) /= OP//SL//'GridOutput'//CL )THEN
  ios = -1; GOTO 9999
END IF

!------ Backspace (again) to overwrite last record <\GridOutput>

BACKSPACE(lun_smp,IOSTAT=ios)

!------ Write time string

WRITE(string,FMT=*,IOSTAT=ios) tOut

IF( ios == 0 )THEN
  WRITE(lun_smp,FMT='(A)',IOSTAT=ios ) TabStr1// &
    OP//'OutputTime RunTime='//QQ//TRIM(ADJUSTL(string))//QQ//CL
END IF

IF( ios /= 0 )THEN
  nError   = WR_ERROR
  eRoutine = 'SamplerGridTime'
  eMessage = 'Error writing output time'
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END SUBROUTINE SamplerGridTime

!==============================================================================

SUBROUTINE SamplerGridCloseTime()

!------ Close time record

INTEGER ios

WRITE(lun_smp,FMT='(A)',IOSTAT=ios ) TabStr1//OP//SL//'OutputTime'//CL

IF( ios /= 0 )THEN
  nError   = WR_ERROR
  eRoutine = 'SamplerGridCloseTime'
  eMessage = 'Error closing time record'
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END SUBROUTINE SamplerGridCloseTime

!==============================================================================

SUBROUTINE SamplerGridSensor( ss )

!------ Output grid description

TYPE( sensor ),INTENT( IN ) :: ss

INTEGER ios

CHARACTER(64)  SensorStr, emptyStr, nxStr, nyStr, dxStr, dyStr, x0Str, y0Str, z0Str, anStr, munits

munits = 'meters'

!------ Check for no output

IF( ss%nx == 0 )THEN

  emptyStr = 'true'
  WRITE(nxStr,FMT='(I4)',    IOSTAT=ios) 0
  WRITE(nyStr,FMT='(I4)',    IOSTAT=ios) 0
  WRITE(dxStr,FMT='(F8.2)',  IOSTAT=ios) 0.
  WRITE(dyStr,FMT='(F8.2)',  IOSTAT=ios) 0.
  WRITE(x0Str,FMT='(ES15.6)',IOSTAT=ios) 0.
  WRITE(y0Str,FMT='(ES15.6)',IOSTAT=ios) 0.
  WRITE(z0Str,FMT='(ES15.6)',IOSTAT=ios) 0.
  WRITE(anStr,FMT='(F8.2)',  IOSTAT=ios) 0.

ELSE   !Setup grid parameter strings

  emptyStr = 'false'
  WRITE(nxStr,FMT='(I4)',    IOSTAT=ios) ss%nx
  WRITE(nyStr,FMT='(I4)',    IOSTAT=ios) ss%ny
  WRITE(dxStr,FMT='(F8.2)',  IOSTAT=ios) ss%dx
  WRITE(dyStr,FMT='(F8.2)',  IOSTAT=ios) ss%dy
  WRITE(x0Str,FMT='(ES15.6)',IOSTAT=ios) ss%x0
  WRITE(y0Str,FMT='(ES15.6)',IOSTAT=ios) ss%y0
  WRITE(z0Str,FMT='(ES15.6)',IOSTAT=ios) ss%zh
  WRITE(anStr,FMT='(F8.2)',  IOSTAT=ios) ss%rot

END IF

SensorStr = TRIM(ADJUSTL(ss%type))
IF( BTEST(ss%stype,STB_INTEGRATE) )SensorStr = TRIM(SensorStr)//':INT'

WRITE(lun_smp,FMT='(A)',IOSTAT=ios ) TabStr2// &
OP//'Sensor Type='//QQ//TRIM(ADJUSTL(SensorStr))//QQ// &
         ' Agent='//QQ//TRIM(ADJUSTL(ss%var))//QQ// &
    ' bEmptyGrid='//QQ//TRIM(ADJUSTL(emptyStr))//QQ// &
    ' numXPoints='//QQ//TRIM(ADJUSTL(nxStr))//QQ// &
    ' numYPoints='//QQ//TRIM(ADJUSTL(nyStr))//QQ// &
            ' dX='//QQ//TRIM(ADJUSTL(dxStr))//QQ// &
            ' dY='//QQ//TRIM(ADJUSTL(dyStr))//QQ// &
       ' originX='//QQ//TRIM(ADJUSTL(x0Str))//QQ// &
       ' originY='//QQ//TRIM(ADJUSTL(y0Str))//QQ// &
     ' heightAGL='//QQ//TRIM(ADJUSTL(z0Str))//QQ// &
' orientationAngle='//QQ//TRIM(ADJUSTL(anStr))//QQ// &
         ' Units='//QQ//TRIM(ADJUSTL(munits))//QQ//CL

IF( ss%nx == 0 )THEN
  WRITE(lun_smp,FMT='(A)',IOSTAT=ios ) TabStr2//OP//SL//'Sensor'//CL
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END SUBROUTINE SamplerGridSensor

!==============================================================================

SUBROUTINE SamplerGridSensorOutput( ss )

!------ Output all sensor fields

TYPE( sensor ),INTENT( IN ) :: ss

INTEGER ios, iv, i, j

CHARACTER(256) string

IF( lBinOut )GOTO 9999

!------ Check for no output

IF( ss%nx == 0 )GOTO 9999

!------ Output data

DO iv = 1,ss%nvar

  string = TabStr3//OP//'SensorData FieldName='//QQ//TRIM(ss%FieldName(iv))//QQ &
           //' FieldUnits='//QQ//TRIM(ss%FieldUnits(iv))//QQ

  IF( ASSOCIATED(ss%SizeBin) )string = TRIM(string)//' SizeBin='//QQ//TRIM(ss%SizeBin(iv))//QQ

  string = TRIM(string)//CL

  WRITE(lun_smp,FMT='(A)',IOSTAT=ios ) TRIM(string)

  WRITE(lun_smp,FMT='(A)',IOSTAT=ios ) TabStr4//OP//'PointValues'//CL
  IF( ios == 0 )THEN
    IF( BTEST(ss%stype,STB_FXGRID) .OR. BTEST(ss%stype,STB_AUTOGRID) )THEN
      WRITE(lun_smp,FMT='(10ES15.7)',IOSTAT=ios) ((ss%gsmp(i,j,iv),i=1,ss%nx),j=1,ss%ny)
    ELSE
      WRITE(lun_smp,FMT='(10ES15.7)',IOSTAT=ios) ss%dsmp(iv)
    END IF
  END IF
  IF( ios /= 0 )THEN
    nError   = WR_ERROR
    eRoutine = 'SamplerGridSensorOutput'
    eMessage = 'Error writing output time'
    GOTO 9999
  END IF
  WRITE(lun_smp,FMT='(A)',IOSTAT=ios ) TabStr4//OP//SL//'PointValues'//CL

  WRITE(lun_smp,FMT='(A)',IOSTAT=ios ) TabStr3//OP//SL//'SensorData'//CL

END DO

WRITE(lun_smp,FMT='(A)',IOSTAT=ios ) TabStr2//OP//SL//'Sensor'//CL  !Close Sensor element


9999 CONTINUE

RETURN
END SUBROUTINE SamplerGridSensorOutput

!==============================================================================

SUBROUTINE CheckGridOutSamp()

INTEGER is

!------ Initialize error

nError = IV_ERROR
eRoutine = 'CheckGridOutSamp'
eMessage = 'Invalid sampler input with gridded ouput'

!------ Time-averaging not allowed

IF( lAvg )THEN
  eInform = 'Time-averaging not allowed with gridded output'
  GOTO 9999
END IF

!------ Specific output times required

IF( .NOT.lSmpOutList )THEN
  eInform = 'Specific output times required with gridded output'
  GOTO 9999
END IF

!------ Loop over samplers

DO is = 1,nsmp

  IF( BTEST(smp(is)%stype,STB_INTEGRATE) )THEN

    IF( .NOT.(BTEST(smp(is)%stype,STB_FXGRID) .OR. BTEST(smp(is)%stype,STB_AUTOGRID)) )THEN
      eInform = 'Integrated single-point sensors not allowed with gridded output'
      CALL SensorNumStr( is,eAction )
      GOTO 9999
    END IF
    IF( BTEST(smp(is)%stype,STB_PART) )THEN
      eInform = 'Integrated "by size" sensors not allowed with gridded output'
      CALL SensorNumStr( is,eAction )
      GOTO 9999
    END IF

  ELSE IF( BTEST(smp(is)%stype,STB_MULT) )THEN
    IF( BTEST(smp(is)%stype,STB_AUTOGRID) )THEN
      eInform = 'Multi-component sensors not allowed with adaptive gridded output'
      CALL SensorNumStr( is,eAction )
      GOTO 9999
    END IF

  ELSE IF( BTEST(smp(is)%stype,STB_CORR) )THEN
    eInform = 'Correlation sensors not allowed with gridded output'
    CALL SensorNumStr( is,eAction )
    GOTO 9999

  ELSE IF( BTEST(smp(is)%stype,STB_MET) )THEN
    IF( BTEST(smp(is)%stype,STB_AUTOGRID) )THEN
      eInform = 'Auto gridded met sensors not allowed'
      CALL SensorNumStr( is,eAction )
      GOTO 9999
    END IF

  END IF

END DO

CALL init_error()

9999 CONTINUE

RETURN
END SUBROUTINE CheckGridOutSamp

!==============================================================================

END MODULE SamplerGridOuput

