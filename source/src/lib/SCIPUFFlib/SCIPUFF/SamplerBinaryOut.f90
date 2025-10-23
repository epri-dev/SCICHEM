SUBROUTINE out_smpBinaryInit()

USE scipuff_fi
USE met_fi
USE sampler_fi
USE files_fi
USE error_fi

IMPLICIT NONE

REAL(8), PARAMETER :: DAY2SEC = 3600.D0 * 24.D0

INTEGER ios, is, idy, ihr, imn
INTEGER thisYear, thisMonth, thisDay
REAL    thisTimeHours

REAL(8) :: t8, x8

INTEGER, EXTERNAL :: WriteBinaryInt, WriteBinaryInt8

CALL init_binary_sampler()
IF( nError /= NO_ERROR )GOTO 9999

nError   = WR_ERROR
eRoutine = 'out_smp'
eMessage = 'Error writing output time to binary sampler file'

CALL year_month_day( t/3600.+tstart,year_start,month_start,day_start, &
                     thisTimeHours,thisYear,thisMonth,thisDay )
ios = WriteBinaryInt( lun_sps,isps,thisYear  ); IF( ios /= 0 )GOTO 9999
ios = WriteBinaryInt( lun_sps,isps,thisMonth ); IF( ios /= 0 )GOTO 9999
ios = WriteBinaryInt( lun_sps,isps,thisDay   ); IF( ios /= 0 )GOTO 9999

t8  = DBLE(t) + DBLE(tstart)*3600.D0
idy = INT(DBLE(t8)/DAY2SEC)
t8  = DBLE(t8) - DAY2SEC*DFLOAT(idy)
IF( t8 < 0. )THEN
  idy = idy - 1
  t8 = t8 + DAY2SEC
END IF

ihr = INT(t8/3600.D0)
x8  = t8-DFLOAT(ihr)*3600.D0
imn = INT(x8/60.D0)
is  = INT(x8-DFLOAT(imn)*60.D0)
ios = WriteBinaryInt( lun_sps,isps,ihr ); IF( ios /= 0 )GOTO 9999
ios = WriteBinaryInt( lun_sps,isps,imn ); IF( ios /= 0 )GOTO 9999
ios = WriteBinaryInt( lun_sps,isps,is  ); IF( ios /= 0 )GOTO 9999

intSPSDataLength = nsmpT * nbinOut * 4
ios = WriteBinaryInt8( lun_sps,isps,intSPSDataLength ); IF( ios /= 0 )GOTO 9999

CALL init_error()

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE init_binary_sampler()

USE scipuff_fi
USE met_fi
USE sampler_fi
USE files_fi
USE SamplerGridOuput

IMPLICIT NONE

INTEGER ios, alloc_stat
INTEGER i, j
INTEGER ni, nj
INTEGER mcID, iv

TYPE( SampClassT ), POINTER :: SampClass

INTEGER(KIND=4) :: is, intSPS
REAL(KIND=4)    :: sngSPS

LOGICAL :: lKmToM, lSouthern

CHARACTER(256) strSPSString, units
CHARACTER(256) class, strSave
CHARACTER(256) specieName
CHARACTER(256), DIMENSION(:), ALLOCATABLE :: unitsStr, classStr

INTEGER, EXTERNAL :: GetVersionString
INTEGER, EXTERNAL :: WriteBinaryString, WriteBinaryInt, WriteBinaryInt8, WriteBinaryReal

!------ If binary sampler file not begun, create header here

IF( .NOT.lIsSPSOpened )THEN

  nError   = WR_ERROR
  eRoutine = 'init_binary_sampler'
  eMessage = 'Error writing binary sampler file'

  lIsSPSOpened = .TRUE.

  IF( lLOSSmp )THEN
    DO is = 1,nsmp
      IF( .NOT.BTEST(smp(is)%stype,STB_LOS) )THEN
        nError   = IV_ERROR
        eMessage = 'Mixing LOS and non-LOS sensorS with binary output'
        eInform  = 'Must be all LOS or none'
        eAction  = 'Try ASCII output'
        GOTO 9999
      END IF
    END DO
  END IF

! **************************************
! BINARY SAMPLER OUTPUT FILE MAIN HEADER
! **************************************

  isps = 1  !Record count

  IF( lLOSSmp )THEN
    strSPSString = "SCIPUFF Concentration LOS Samplers"
  ELSE
    strSPSString = "SCIPUFF Samplers"
  END IF

  ios = WriteBinaryString( lun_sps,isps,strSPSString ); IF( ios /= 0 )GOTO 9999

  sngSPS = 1.4     !SPS file version
  ios = WriteBinaryReal( lun_sps,isps,sngSPS ); IF( ios /= 0 )GOTO 9999

  intSPSTimeCountFilePosition = HUGE( intSPSTimeCountFilePosition )  !Dummy integer(8) value; will be position of time-varying output
  ios = WriteBinaryInt8( lun_sps,isps,intSPSTimeCountFilePosition ); IF( ios /= 0 )GOTO 9999
  intSPSDataLength = isps-2                                          !Save record for write below

!------ Coordinate system specs

  strSPSString = "LATLON"
  SELECT CASE( lmap )
    CASE( I_UTM )
      strSPSString = "UTM"
    CASE( I_CARTESIAN )
      strSPSString = "CARTESIAN"
    CASE( I_METERS )
      strSPSString = "METERS"
  END SELECT
  ios = WriteBinaryString( lun_sps,isps,strSPSString ); IF( ios /= 0 )GOTO 9999

  strSPSString = "WGS84"
  ios = WriteBinaryString( lun_sps,isps,strSPSString ); IF( ios /= 0 )GOTO 9999  !Datum

  lKmToM = .FALSE.
  strSPSString = "DEGREES"
  SELECT CASE( lmap )
    CASE( I_UTM )
      strSPSString = "METERS"
      lKmToM = .TRUE.
    CASE( I_CARTESIAN )
      strSPSString = "METERS"
      lKmToM = .TRUE.
    CASE( I_METERS )
      strSPSString = "METERS"
  END SELECT
  ios = WriteBinaryString( lun_sps,isps,strSPSString ); IF( ios /= 0 )GOTO 9999  !Horiz units

  strSPSString = "METERS"
  ios = WriteBinaryString( lun_sps,isps,strSPSString ); IF( ios /= 0 )GOTO 9999  !Vert units

  ios = WriteBinaryInt( lun_sps,isps,utm_zone ); IF( ios /= 0 )GOTO 9999  !UTM zone

  strSPSString = "N"
  lSouthern = .FALSE.
  IF( lmap == I_UTM )CALL is_southern_hemisphere( lSouthern )
  IF( lSouthern )strSPSString = "S"
  ios = WriteBinaryString( lun_sps,isps,strSPSString ); IF( ios /= 0 )GOTO 9999  !Hemisphere

  ios = WriteBinaryReal( lun_sps,isps,lon0 ); IF( ios /= 0 )GOTO 9999  !Origin lon
  ios = WriteBinaryReal( lun_sps,isps,lat0 ); IF( ios /= 0 )GOTO 9999  !Origin lon

  sngSPS = 0.0
  ios = WriteBinaryReal( lun_sps,isps,sngSPS ); IF( ios /= 0 )GOTO 9999  !Std parallel 1
  ios = WriteBinaryReal( lun_sps,isps,sngSPS ); IF( ios /= 0 )GOTO 9999  !Std parallel 1
  ios = WriteBinaryReal( lun_sps,isps,sngSPS ); IF( ios /= 0 )GOTO 9999  !Azimuth

  sngSPS = 1.0
  ios = WriteBinaryReal( lun_sps,isps,sngSPS ); IF( ios /= 0 )GOTO 9999  !Scale factor

  sngSPS = 0.0
  ios = WriteBinaryReal( lun_sps,isps,sngSPS ); IF( ios /= 0 )GOTO 9999  !False east
  ios = WriteBinaryReal( lun_sps,isps,sngSPS ); IF( ios /= 0 )GOTO 9999  !False north

!------ Time convention

  intSPS = INT(tzone)
  ios = WriteBinaryInt( lun_sps,isps,intSPS ); IF( ios /= 0 )GOTO 9999  !Time zone

  intSPS = 0; IF( local )intSPS = 1
  ios = WriteBinaryInt( lun_sps,isps,intSPS ); IF( ios /= 0 )GOTO 9999  !Local time flag

! **************************************
! SAMPLER DEFINITIONS
! **************************************

! get the number of unique materials for these samplers
  intSPSMaterialCount = nSampClass  !"Material" is legacy of original EPA code

  intSPS = 0        !Get number of locations per class; must check for fixed grids
  DO is = 1,nsmp,nSampClass
    IF( BTEST(smp(is)%stype,STB_FXGRID) )THEN
      intSPS = intSPS + smp(is)%nx*smp(is)%ny
    ELSE
      intSPS = intSPS + 1
    END IF
  END DO

  ios = WriteBinaryInt( lun_sps,isps,intSPS ); IF( ios /= 0 )GOTO 9999

  nsmpT = intSPS

  DO is = 1,nsmp,nSampClass

    ni = smp(is)%nx
    nj = smp(is)%ny

    DO j = 1,nj
      DO i = 1,ni

        IF( BTEST(smp(is)%stype,STB_FXGRID) )THEN
          sngSPS = smp(is)%x + FLOAT(i-1)*smp(is)%dxs
        ELSE
          sngSPS = smp(is)%x
        END IF
        IF( lKmToM )sngSPS = sngSPS * 1000.
        ios = WriteBinaryReal( lun_sps,isps,sngSPS ); IF( ios /= 0 )GOTO 9999

        IF( BTEST(smp(is)%stype,STB_FXGRID) )THEN
          sngSPS = smp(is)%y + FLOAT(j-1)*smp(is)%dys
        ELSE
          sngSPS = smp(is)%y
        END IF
        IF( lSouthern )sngSPS = sngSPS + 10000  !Only for UTM - assumed in KM
        IF( lKmToM )sngSPS = sngSPS * 1000.
        ios = WriteBinaryReal( lun_sps,isps,sngSPS ); IF( ios /= 0 )GOTO 9999
        sngSPS = smp(is)%z + hmin
        ios = WriteBinaryReal( lun_sps,isps,sngSPS ); IF( ios /= 0 )GOTO 9999

        IF( lLOSSmp )THEN
          sngSPS = smp(is)%az
          ios = WriteBinaryReal( lun_sps,isps,sngSPS ); IF( ios /= 0 )GOTO 9999
          sngSPS = smp(is)%el
          ios = WriteBinaryReal( lun_sps,isps,sngSPS ); IF( ios /= 0 )GOTO 9999
          sngSPS = smp(is)%dist
          ios = WriteBinaryReal( lun_sps,isps,sngSPS ); IF( ios /= 0 )GOTO 9999
        END IF

      END DO
    END  DO
  END DO

  lKgToUg = .FALSE.

  ALLOCATE( unitsStr(nSampClass), classStr(nSampClass),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'init_binary_sampler'
    eMessage = 'Error allocating units and class string arrays'
    GOTO 9999
  END IF

  DO is = 1,nSampClass     !Set units based on first nSampClass samplers (at first location); set class string too
        IF( BTEST(smp(is)%stype,STB_DEP) .OR. BTEST(smp(is)%stype,STB_INTGRID) )THEN
          intSPSMaterial = smp(is)%npts
        ELSE
          intSPSMaterial = typeID(smp(is)%is)%imat
        END IF
        units = TRIM(material(intSPSMaterial)%unit) !Mass units
        class = TRIM(material(intSPSMaterial)%ccls) !Material class
        IF( BTEST(smp(is)%stype,STB_INTEGRATE) )units = TRIM(units)//'-sec'
        IF( BTEST(smp(is)%stype,STB_DEP) )THEN
          units = TRIM(units)//'/m^2'
        ELSE IF( lLOSSmp )THEN
          units = TRIM(units)//'/m^2'
        ELSE
          units = TRIM(units)//'/m^3'
        END IF
    unitsStr(is) = TRIM(units)
    classStr(is) = TRIM(class)
  END DO

!------ Write the "materials"
!       N.B. Assumes only certain sensors are defined, e.g., CONC, DEP, ...

  intSPS = 0
  DO is = 1,nSampClass
    intSPS = intSPS + 1
    IF( lOutputVariance )intSPS = intSPS + 1
    IF( BTEST(smp(is)%stype,STB_DEP) .AND. smp(is)%ie == -1 )THEN
      intSPS = intSPS + 1
      IF( lOutputVariance )intSPS = intSPS + 1
    END IF
  END DO
  ios = WriteBinaryInt( lun_sps,isps,intSPS ); IF( ios /= 0 )GOTO 9999

  nbinOut = intSPS
  iv = 0

  DO intSPSLoop = 1,intSPSMaterialCount
    IF( intSPSLoop == 1 )THEN
      SampClass => FirstSampClass
    ELSE
      SampClass => SampClass%next
    END IF
    strSPSString = TRIM(SampClass%type)//' '//TRIM(SampClass%var)
    IF( BTEST(smp(intSPSLoop)%stype,STB_MULT) )THEN
      i = INDEX(strSPSString,':',BACK=.TRUE.)
      IF( i > 1 )strSPSString = TRIM(strSPSString(1:i-1))
    END IF
    units = TRIM(unitsStr(intSPSLoop))
    ios = WriteBinaryString( lun_sps,isps,strSPSString ); IF( ios /= 0 )GOTO 9999
    strSave = strSPSString
    strSPSString = TRIM(classStr(intSPSLoop))
    ios = WriteBinaryString( lun_sps,isps,strSPSString ); IF( ios /= 0 )GOTO 9999
    ios = WriteBinaryString( lun_sps,isps,units ); IF( ios /= 0 )GOTO 9999
    IF( lOutputVariance )THEN
      strSPSString = TRIM(strSave)//" variance"
      ios = WriteBinaryString( lun_sps,isps,strSPSString ); IF( ios /= 0 )GOTO 9999
      strSPSString = TRIM(classStr(intSPSLoop))
      ios = WriteBinaryString( lun_sps,isps,strSPSString ); IF( ios /= 0 )GOTO 9999
      units = '('//TRIM(units)//')^2'
      ios = WriteBinaryString( lun_sps,isps,units ); IF( ios /= 0 )GOTO 9999
    END IF

    IF( BTEST(smp(intSPSLoop)%stype,STB_DEP) .AND. smp(intSPSLoop)%ie == -1 )THEN
      units = TRIM(unitsStr(intSPSLoop))
      strSPSString = TRIM(strSave)//" surface"
      ios = WriteBinaryString( lun_sps,isps,strSPSString ); IF( ios /= 0 )GOTO 9999
      strSPSString = TRIM(classStr(intSPSLoop))
      ios = WriteBinaryString( lun_sps,isps,strSPSString ); IF( ios /= 0 )GOTO 9999
      ios = WriteBinaryString( lun_sps,isps,units ); IF( ios /= 0 )GOTO 9999
      IF( lOutputVariance )THEN
        strSPSString = TRIM(strSave)//" surface variance"
        ios = WriteBinaryString( lun_sps,isps,strSPSString ); IF( ios /= 0 )GOTO 9999
        strSPSString = TRIM(classStr(intSPSLoop))
        ios = WriteBinaryString( lun_sps,isps,strSPSString ); IF( ios /= 0 )GOTO 9999
        units = '('//TRIM(units)//')^2'
        ios = WriteBinaryString( lun_sps,isps,units ); IF( ios /= 0 )GOTO 9999
      END IF
    END IF


  END DO

!------ Save current file position so we can come back to write number of times in the file

  intSPSTimeCountFilePosition = (isps-1)*4           !Save current record (zero-based bytes for post-processor)
  isps = intSPSDataLength                            !Prepare to overwrite (record saved above)

  ios = WriteBinaryInt8( lun_sps,isps,intSPSTimeCountFilePosition ); IF( ios /= 0 )GOTO 9999  !

  isps = intSPSTimeCountFilePosition/4 + 1           !Put back current record

  intSPSDumpCount = 123456                           !Write unique number for check when finalizing
  ios = WriteBinaryInt( lun_sps,isps,intSPSDumpCount ); IF( ios /= 0 )GOTO 9999

  intSPSDumpCount = 0                                !Zero count

  CALL init_error()

END IF

9999 CONTINUE

IF( ALLOCATED(unitsStr) )DEALLOCATE( unitsStr,STAT=alloc_stat)
IF( ALLOCATED(classStr) )DEALLOCATE( classStr,STAT=alloc_stat)

RETURN
END

!==============================================================================

INTEGER FUNCTION WriteBinaryString( lun,irec,string ) RESULT( ios )

IMPLICIT NONE

INTEGER,      INTENT( IN    ) :: lun
INTEGER(8),   INTENT( INOUT ) :: irec
CHARACTER(*), INTENT( IN    ) :: string

INTEGER i, j, ilen

CHARACTER(3) :: pad = '   '

ilen = LEN_TRIM(string)
i    = (ilen + 3)/4

WRITE(lun,REC=irec,IOSTAT=ios) i*4
IF( ios /= 0 )RETURN
irec = irec + 1

DO i = 1,ilen,4
  j = i+3
  IF( j > ilen )THEN
    WRITE(lun,REC=irec,IOSTAT=ios) string(i:ilen)//pad(1:j-ilen)
  ELSE
    WRITE(lun,REC=irec,IOSTAT=ios) string(i:j)
  END IF
  IF( ios /= 0 )EXIT
  irec = irec + 1
END DO

RETURN
END

!==============================================================================

INTEGER FUNCTION ReadBinaryString( lun,irec,string ) RESULT( ios )

IMPLICIT NONE

INTEGER,      INTENT( IN    ) :: lun
INTEGER(8),   INTENT( INOUT ) :: irec
CHARACTER(*), INTENT( OUT   ) :: string

INTEGER i, j, ilen

string = ''

READ(lun,REC=irec,IOSTAT=ios) ilen
IF( ios /= 0 )RETURN
irec = irec + 1

DO i = 1,ilen,4
  j = i+3
  READ(lun,REC=irec,IOSTAT=ios) string(i:j)
  IF( ios /= 0 )EXIT
  irec = irec + 1
END DO

RETURN
END

!==============================================================================

INTEGER FUNCTION WriteBinaryInt( lun,irec,i4 ) RESULT( ios )

IMPLICIT NONE

INTEGER,    INTENT( IN    ) :: lun
INTEGER(8), INTENT( INOUT ) :: irec
INTEGER,    INTENT( IN    ) :: i4

WRITE(lun,REC=irec,IOSTAT=ios) i4
IF( ios == 0 )irec = irec + 1

RETURN
END

!==============================================================================

INTEGER FUNCTION ReadBinaryInt( lun,irec,i4 ) RESULT( ios )

IMPLICIT NONE

INTEGER,    INTENT( IN    ) :: lun
INTEGER(8), INTENT( INOUT ) :: irec
INTEGER,    INTENT( OUT   ) :: i4

READ(lun,REC=irec,IOSTAT=ios) i4
IF( ios == 0 )irec = irec + 1

RETURN
END

!==============================================================================

INTEGER FUNCTION WriteBinaryReal( lun,irec,r4 ) RESULT( ios )

IMPLICIT NONE

INTEGER,    INTENT( IN    ) :: lun
INTEGER(8), INTENT( INOUT ) :: irec
REAL,       INTENT( IN    ) :: r4

WRITE(lun,REC=irec,IOSTAT=ios) r4
IF( ios == 0 )irec = irec + 1

RETURN
END

!==============================================================================

INTEGER FUNCTION ReadBinaryReal( lun,irec,r4 ) RESULT( ios )

IMPLICIT NONE

INTEGER,    INTENT( IN    ) :: lun
INTEGER(8), INTENT( INOUT ) :: irec
REAL,       INTENT( OUT   ) :: r4

READ(lun,REC=irec,IOSTAT=ios) r4
IF( ios == 0 )irec = irec + 1

RETURN
END

!==============================================================================

INTEGER FUNCTION WriteBinaryInt8( lun,irec,i8 ) RESULT( ios )

IMPLICIT NONE

INTEGER,    INTENT( IN    ) :: lun
INTEGER(8), INTENT( INOUT ) :: irec
INTEGER(8), INTENT( IN    ) :: i8

INTEGER i

INTEGER(4), DIMENSION(2) :: intT

intT = TRANSFER(i8,intT)

DO i = 1,2
  WRITE(lun,rec=irec,IOSTAT=ios) intT(i)
  IF( ios /= 0 )EXIT
  irec = irec + 1
END DO

RETURN
END

!==============================================================================

INTEGER FUNCTION ReadBinaryInt8( lun,irec,i8 ) RESULT( ios )

IMPLICIT NONE

INTEGER,    INTENT( IN    ) :: lun
INTEGER(8), INTENT( INOUT ) :: irec
INTEGER(8), INTENT( OUT   ) :: i8

INTEGER i

INTEGER(4), DIMENSION(2) :: intT

DO i = 1,2
  READ(lun,rec=irec,IOSTAT=ios) intT(i)
  IF( ios /= 0 )EXIT
  irec = irec + 1
END DO

i8 = TRANSFER(intT,i8)

RETURN
END

!==============================================================================

SUBROUTINE is_southern_hemisphere( lSouthern )

USE sampler_fi

IMPLICIT NONE

LOGICAL, INTENT( OUT ) :: lSouthern

INTEGER ismp
REAL    ycoord

lSouthern = .TRUE.
DO ismp = 1,nsmp
  ycoord = smp(ismp)%y
  IF( ycoord >= 0.0 )THEN
    lSouthern = .FALSE.
    EXIT
  END IF
END DO

RETURN
END

!==============================================================================

SUBROUTINE out_smpBinarySingle()

USE scipuff_fi
USE met_fi
USE sampler_fi
USE files_fi
USE error_fi

IMPLICIT NONE

INTEGER ios, iFirstSmp, is
INTEGER(KIND=8) irec
REAL    sngSPS, fac

INTEGER, EXTERNAL :: WriteBinaryReal, WriteBinaryInt

nError   = WR_ERROR
eRoutine = 'out_smp'
eMessage = 'Error writing to binary sampler file'

IF( lKgToUg )THEN
  fac = 1.0E9
ELSE
  fac = 1.
END IF

iFirstSmp = 0

DO intSPSLoop = 1,intSPSMaterialCount
  iFirstSmp = iFirstSmp + 1

  DO is = iFirstSmp,nsmp,intSPSMaterialCount             !First variable (mean)
    sngSPS = smp(is)%dsmp(1)*fac
    ios = WriteBinaryReal( lun_sps,isps,sngSPS ); IF( ios /= 0 )GOTO 9999
  END DO

  IF( lOutputVariance )THEN                              !Second variable (variance)
    DO is = iFirstSmp,nsmp,intSPSMaterialCount
      sngSPS = smp(is)%dsmp(2)*fac**2
      ios = WriteBinaryReal( lun_sps,isps,sngSPS ); IF( ios /= 0 )GOTO 9999
    END DO
  END IF

  IF( lDepS )THEN

    DO is = iFirstSmp,nsmp,intSPSMaterialCount
      IF( BTEST(smp(is)%stype,STB_DEP) .AND. smp(is)%ie == -1 )THEN  !Deposition remaining on surface
        sngSPS = smp(is)%dsmp(3)*fac
        ios = WriteBinaryReal( lun_sps,isps,sngSPS ); IF( ios /= 0 )GOTO 9999
      END IF
    END DO

    IF( lOutputVariance )THEN                                    !Scale variance
      DO is = iFirstSmp,nsmp,intSPSMaterialCount
        IF( BTEST(smp(is)%stype,STB_DEP) .AND. smp(is)%ie == -1 )THEN  !Deposition remaining on surface
          IF( smp(is)%dsmp(1) > 0. .AND. smp(is)%dsmp(3) > 0. )THEN
            sngSPS = smp(is)%dsmp(2)*fac**2 * smp(is)%dsmp(3)/smp(is)%dsmp(1)
          ELSE
            sngSPS = 0.
          END IF
          ios = WriteBinaryReal( lun_sps,isps,sngSPS ); IF( ios /= 0 )GOTO 9999
        END IF
      END DO
    END IF

  END IF
END DO

CALL init_error()

intSPSDumpCount = intSPSDumpCount + 1

irec = intSPSTimeCountFilePosition/4 + 1
ios  = WriteBinaryInt( lun_sps,irec,intSPSDumpCount ); IF( ios /= 0 )GOTO 9999

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE finalize_binary_sampler()

!------ Write final number of output times to binary sampler file

USE scipuff_fi
USE files_fi
USE sampler_fi

IMPLICIT NONE

INTEGER    ios, idum
INTEGER(8) irec

INTEGER, EXTERNAL :: WriteBinaryInt

IF( lIsSPSOpened )THEN

  eRoutine = 'finalize_binary_sampler'

  irec = intSPSTimeCountFilePosition/4 + 1
  READ(lun_sps,REC=irec,IOSTAT=ios) idum
  IF( ios /= 0 )THEN
    nError  = RD_ERROR
    eInform = 'Error reading number of times on binary sampler file'
    GOTO 9999
  END IF

  nError  = WR_ERROR
  eInform = 'Error writing number of times on binary sampler file'

  ios = WriteBinaryInt( lun_sps,irec,intSPSDumpCount ); IF( ios /= 0 )GOTO 9999

  CALL init_error()

END IF

9999 CONTINUE

CLOSE(lun_sps,IOSTAT=ios)

RETURN
END

!==============================================================================

SUBROUTINE rst_smpBinaryInit()

!------ Find last sampler time; also set data about file, e.g., intSPSMaterialCount

USE scipuff_fi
USE files_fi
USE sampler_fi

IMPLICIT NONE

INTEGER ios, irec
INTEGER thisYear, thisMonth, thisDay, ihr, imn, is
INTEGER jul, iy

CHARACTER(256) string

INTEGER, EXTERNAL :: ReadBinaryString, ReadBinaryInt, ReadBinaryInt8
INTEGER, EXTERNAL :: julian_day, days_in_year

eRoutine = 'rst_smpBinaryInit'
nError   = RD_ERROR

!------- Skip header string and version; read position for no. of output times

isps = 1
READ(lun_sps,REC=isps,IOSTAT=ios) irec !No. of records in string
IF( ios /= 0 )THEN
  eInform = 'Error reading header string on binary sampler file'
  GOTO 9999
END IF

isps = isps + irec/4 + 2 !Skip records

!READ(lun_sps,REC=isps,IOSTAT=ios) intSPSTimeCountFilePosition
!isps = isps + 2
ios = ReadBinaryInt8( lun_sps,isps,intSPSTimeCountFilePosition ); IF( ios /= 0 )GOTO 9999

!------ Determine records of one output time

ios = ReadBinaryString( lun_sps,isps,string ); IF( ios /= 0 )GOTO 9999 !Coordinate system specs
ios = ReadBinaryString( lun_sps,isps,string ); IF( ios /= 0 )GOTO 9999
ios = ReadBinaryString( lun_sps,isps,string ); IF( ios /= 0 )GOTO 9999
ios = ReadBinaryString( lun_sps,isps,string ); IF( ios /= 0 )GOTO 9999
isps = isps + 1  !Skip UTM zone
ios = ReadBinaryString( lun_sps,isps,string ); IF( ios /= 0 )GOTO 9999
isps = isps + 10 !Skip 10 real/integer values

ios = ReadBinaryInt( lun_sps,isps,nsmpT ); IF( ios /= 0 )GOTO 9999 !No. of sampler locations

isps = isps + nsmpT*3 !Skip locations
IF( lLOSSmp )isps = isps + nsmpT*3   !Skip LOS parameters
ios = ReadBinaryInt( lun_sps,isps,nbinOut ); IF( ios /= 0 )GOTO 9999 !No. of variables per location

intSPSMaterialCount = nSampClass

!------ Read number of output times

isps = intSPSTimeCountFilePosition/4 + 1
ios  = ReadBinaryInt( lun_sps,isps,intSPSDumpCount ); IF( ios /= 0 )GOTO 9999

irec0SPS = isps
nrecSPST = 6 + 2 + nsmpT * nbinOut !Year/time + data length + data

!------ Read last time

isps = irec0SPS + (intSPSDumpCount-1)*nrecSPST

ios = ReadBinaryInt( lun_sps,isps,thisYear  ); IF( ios /= 0 )GOTO 9999
ios = ReadBinaryInt( lun_sps,isps,thisMonth ); IF( ios /= 0 )GOTO 9999
ios = ReadBinaryInt( lun_sps,isps,thisDay   ); IF( ios /= 0 )GOTO 9999
ios = ReadBinaryInt( lun_sps,isps,ihr ); IF( ios /= 0 )GOTO 9999
ios = ReadBinaryInt( lun_sps,isps,imn ); IF( ios /= 0 )GOTO 9999
ios = ReadBinaryInt( lun_sps,isps,is  ); IF( ios /= 0 )GOTO 9999

!READ(lun_sps,REC=isps,IOSTAT=ios) intSPSDataLength !Should equal 4*nsmpT*nbinOut
!isps = isps + 2 !Set to read sampler data

!------ Find run time of last break

jul = julian_day( thisMonth,thisday,thisYear )
IF( thisYear > year_start )THEN
  DO iy = year_start+1,thisYear
    jul = jul + days_in_year( iy )
  END DO
END IF

t_smp = FLOAT(jul-jul_start)*24.*3600. + FLOAT(ihr)*3600. + FLOAT(imn)*60. + FLOAT(is) - tstart

lIsSPSOpened = .TRUE.

CALL init_error()

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE position_binSmp( tol )

!------ Position to sampler time <= t (restart time)

USE scipuff_fi
USE files_fi
USE sampler_fi

IMPLICIT NONE

REAL, INTENT( IN ) :: tol

INTEGER ios, i
INTEGER thisYear, thisMonth, thisDay, ihr, imn, is
INTEGER jul, iy

INTEGER, EXTERNAL :: ReadBinaryString, ReadBinaryInt
INTEGER, EXTERNAL :: julian_day, days_in_year

eRoutine = 'position_binSmp'
nError   = RD_ERROR

IF( t >= t_smp-tol )THEN !Restart time is at/after last sampler ltime

  i = intSPSDumpCount

ELSE                     !Restart time is earlier then last sampler time

  DO i = intSPSDumpCount-1,1,-1

    isps = irec0SPS + (i-1)*nrecSPST

    ios = ReadBinaryInt( lun_sps,isps,thisYear  ); IF( ios /= 0 )GOTO 9999
    ios = ReadBinaryInt( lun_sps,isps,thisMonth ); IF( ios /= 0 )GOTO 9999
    ios = ReadBinaryInt( lun_sps,isps,thisDay   ); IF( ios /= 0 )GOTO 9999
    ios = ReadBinaryInt( lun_sps,isps,ihr ); IF( ios /= 0 )GOTO 9999
    ios = ReadBinaryInt( lun_sps,isps,imn ); IF( ios /= 0 )GOTO 9999
    ios = ReadBinaryInt( lun_sps,isps,is  ); IF( ios /= 0 )GOTO 9999

    jul = julian_day( thisMonth,thisday,thisYear )
    IF( thisYear > year_start )THEN
      DO iy = year_start+1,thisYear
        jul = jul + days_in_year( iy )
      END DO
    END IF

    t_smp = FLOAT(jul-jul_start)*24.*3600. + FLOAT(ihr)*3600. + FLOAT(imn)*60. + FLOAT(is) - tstart

    IF( t >= t_smp-tol )EXIT

  END DO

END IF

!------ Position to write next time

isps = irec0SPS + i*nrecSPST

CALL init_error()

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE readPrev_binSmp()

!------ Read data for time before current t
!       N.B. Used only for restarting with time-integrated sensors
!       Do not do with incremental outoput if last output time was a proper sampler output

USE scipuff_fi
USE files_fi
USE sampler_fi

IMPLICIT NONE

INTEGER ios, iFirstSmp, is
INTEGER(KIND=8) isps_save
INTEGER iv, iv1
REAL    sngSPS, fac, tol, ts

INTEGER, EXTERNAL :: ReadBinaryReal

isps_save = isps
isps = isps - nrecSPST + 8 !Skip date/time and data length records
IF( isps < 0 )THEN
  isps = isps_save
  GOTO 9999
END IF

eRoutine = 'position_binSmp'
nError   = RD_ERROR

IF( lKgToUg )THEN
  fac = 1.0E9
ELSE
  fac = 1.
END IF

iFirstSmp = 0

DO intSPSLoop = 1,intSPSMaterialCount
  iFirstSmp = iFirstSmp + 1

  DO is = iFirstSmp,nsmp,intSPSMaterialCount             !First variable (mean)
    ios = ReadBinaryReal( lun_sps,isps,sngSPS ); IF( ios /= 0 )GOTO 9999
    smp(is)%dsmp(1) = sngSPS/fac
  END DO

  IF( lOutputVariance )THEN                              !Second variable (variance)
    DO is = iFirstSmp,nsmp,intSPSMaterialCount
      ios = ReadBinaryReal( lun_sps,isps,sngSPS ); IF( ios /= 0 )GOTO 9999
      smp(is)%dsmp(2) = sngSPS/fac**2
    END DO
  END IF

  IF( lDepS )THEN

    DO is = iFirstSmp,nsmp,intSPSMaterialCount
      IF( BTEST(smp(is)%stype,STB_DEP) .AND. smp(is)%ie == -1 )THEN  !Deposition remaining on surface
        ios = ReadBinaryReal( lun_sps,isps,sngSPS ); IF( ios /= 0 )GOTO 9999
        smp(is)%dsmp(3) = sngSPS/fac
      END IF
    END DO

    IF( lOutputVariance )THEN                                    !Scale variance
      DO is = iFirstSmp,nsmp,intSPSMaterialCount
        ios = ReadBinaryReal( lun_sps,isps,sngSPS ); IF( ios /= 0 )GOTO 9999
        IF( BTEST(smp(is)%stype,STB_DEP) .AND. smp(is)%ie == -1 )THEN  !Deposition remaining on surface
          IF( smp(is)%dsmp(1) > 0. .AND. smp(is)%dsmp(3) > 0. )THEN
            smp(is)%dsmp(3) = sngSPS*smp(is)%dsmp(1)/(smp(is)%dsmp(2)*fac**2)
          ELSE
            smp(is)%dsmp(3) = 0.
          END IF
        END IF
      END DO
    END IF

  END IF
END DO

isps = isps_save !Ready to write next output time

CALL init_error()

9999 CONTINUE

RETURN
END

