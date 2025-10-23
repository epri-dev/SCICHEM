MODULE landuse

!------ Land use parameters

USE landuse_fd
USE charT_fd
USE message_fd
USE errorParam_fd

IMPLICIT NONE

SAVE

INTEGER, PARAMETER :: LAND_USE_VERSION = 200

INTEGER num_cat, num_sup, n_tot

REAL, ALLOCATABLE ::   zruf_lu(:,:)
REAL, ALLOCATABLE ::   hcnp_lu(:,:)
REAL, ALLOCATABLE :: alphac_lu(:,:)
REAL, ALLOCATABLE :: albedo_lu(:,:)
REAL, ALLOCATABLE ::  bowen_lu(:,:,:)

TYPE( char64T ), DIMENSION(:), ALLOCATABLE :: string_lu

INTEGER, ALLOCATABLE :: sup_class(:)
INTEGER, ALLOCATABLE :: lu_count(:,:)
INTEGER, ALLOCATABLE :: su_count(:,:)
INTEGER, ALLOCATABLE :: tot_count(:)

TYPE( messageT ) errMsg

END MODULE landuse

!-----------------------------------------------------------------------

INTEGER FUNCTION InitLandUseCat( n )

!DEC$ ATTRIBUTES DLLEXPORT :: InitLandUseCat

!------ Initialize land use categories

USE landuse

IMPLICIT NONE

INTEGER, INTENT( IN ) :: n

INTEGER alloc_stat

!------ Initialize error structure

CALL InitLandUseError()

!------ allocate arrays for n output grid cells

InitLandUseCat = 0

n_tot = n

IF( ALLOCATED(lu_count) )DEALLOCATE(lu_count,STAT=alloc_stat)
IF( ALLOCATED(su_count) )DEALLOCATE(su_count,STAT=alloc_stat)
IF( ALLOCATED(tot_count))DEALLOCATE(tot_count,STAT=alloc_stat)

ALLOCATE( lu_count(num_cat+num_sup,n_tot),STAT=alloc_stat )
IF( alloc_stat == 0 )ALLOCATE( su_count(num_sup,n_tot),STAT=alloc_stat )
IF( alloc_stat == 0 )ALLOCATE( tot_count(n_tot),STAT=alloc_stat )

!------ if error, deallocate and return

IF( alloc_stat /= 0 )THEN
  IF( ALLOCATED(lu_count) )DEALLOCATE( lu_count,STAT=alloc_stat )
  IF( ALLOCATED(su_count) )DEALLOCATE( su_count,STAT=alloc_stat )
  IF( ALLOCATED(tot_count))DEALLOCATE( tot_count,STAT=alloc_stat )
  errMsg%iParm   = UK_ERROR
  errMsg%jParm   = alloc_stat
  errMsg%routine = 'InitLandUseCat'
  errMsg%aString = 'Error allocating grid cell arrays'
  WRITE(errMsg%bString,'(A,I0)',IOSTAT=alloc_stat) 'Array size = ',n
  RETURN
END IF

!------ initialize arrays

lu_count  = 0
su_count  = 0
tot_count = 0

InitLandUseCat = 1

RETURN
END

!===============================================================================

INTEGER FUNCTION AddLandUseCat( i,cat )

!DEC$ ATTRIBUTES DLLEXPORT :: AddLandUseCat

!------ add category counts for cell i

USE landuse

IMPLICIT NONE

INTEGER, INTENT( IN ) :: i, cat

INTEGER ic, istat

!------ check for i out of range

AddLandUseCat = 0

IF( i > n_tot .OR. i < 1 )THEN
  errMsg%iParm   = IV_ERROR
  errMsg%routine = 'AddLandUseCat'
  errMsg%aString = 'Cell index out of bound'
  WRITE(errMsg%bString,'(A,I0)',IOSTAT=istat) 'Index = ',i
  WRITE(errMsg%cString,'(A,I0)',IOSTAT=istat) 'Maximum = ',n_tot
  RETURN
END IF

!------ add to sums

ic = cat

IF( ic > 1000 )ic = ic - 1000 + num_cat

IF( ic > num_cat+num_sup .OR. ic < 1 )THEN
  errMsg%iParm   = IV_ERROR
  errMsg%routine = 'AddLandUseCat'
  errMsg%aString = 'Category index out of bound'
  WRITE(errMsg%bString,'(A,I0)',IOSTAT=istat) 'Index = ',ic
  WRITE(errMsg%cString,'(A,I0)',IOSTAT=istat) 'Maximum = ',num_cat+num_sup
  RETURN
END IF

lu_count(ic,i)            = lu_count(ic,i) + 1
su_count(sup_class(ic),i) = su_count(sup_class(ic),i) + 1
tot_count(i)              = tot_count(i) + 1

AddLandUseCat = 1

RETURN
END

!===============================================================================

INTEGER FUNCTION OutLandUseCat( i )

!DEC$ ATTRIBUTES DLLEXPORT :: OutLandUseCat

!------ output amalgamated land use category for cell i

USE landuse

IMPLICIT NONE

INTEGER, INTENT( IN ) :: i

INTEGER j, max, cat, istat

REAL fac

!------ check for i out of range

OutLandUseCat = 0

IF( i > n_tot .OR. i < 1 )THEN
  errMsg%iParm   = IV_ERROR
  errMsg%routine = 'OutLandUseCat'
  errMsg%aString = 'Cell index out of bound'
  WRITE(errMsg%bString,'(A,I0)',IOSTAT=istat) 'Index = ',i
  WRITE(errMsg%cString,'(A,I0)',IOSTAT=istat) 'Maximum = ',n_tot
  RETURN
END IF

!------ check for zero count

IF( tot_count(i) == 0 )THEN
  errMsg%iParm   = IV_ERROR
  errMsg%routine = 'OutLandUseCat'
  errMsg%aString = 'No categories set for this cell'
  WRITE(errMsg%bString,'(A,I0)',IOSTAT=istat) 'Cell index = ',i
  RETURN
END IF

!------ look for a category that accounts for more than 50%

fac = 1./FLOAT(tot_count(i))

cat = 1
DO WHILE( FLOAT(lu_count(cat,i))*fac <= 0.5 .AND. cat <= num_cat )
  cat = cat + 1
END DO

IF( cat > num_cat )THEN

!------ look for most prevalant super category

  cat = 1 + 1000
  max = su_count(1,i)

  DO j = 2,num_sup
    IF( su_count(j,i) > max )THEN
      cat = j + 1000
      max = su_count(j,i)
    END IF
  END DO

END IF

OutLandUseCat = cat

RETURN
END

!===============================================================================

INTEGER FUNCTION ClearLandUseCat()

!DEC$ ATTRIBUTES DLLEXPORT :: ClearLandUseCat

USE landuse

IMPLICIT NONE

INTEGER alloc_stat

IF( ALLOCATED(lu_count) )DEALLOCATE( lu_count,STAT=alloc_stat )
IF( ALLOCATED(su_count) )DEALLOCATE( su_count,STAT=alloc_stat )
IF( ALLOCATED(tot_count))DEALLOCATE( tot_count,STAT=alloc_stat )

!------ Set status based on succesful deallocation

IF( ALLOCATED(lu_count) .OR. ALLOCATED(su_count) .OR. ALLOCATED(tot_count) )THEN
  ClearLandUseCat = 0
  errMsg%iParm    = IV_ERROR
  errMsg%routine  = 'ClearLandUseCat'
  errMsg%aString  = 'Arrays not deallocated'
ELSE
  ClearLandUseCat = 1
END IF

RETURN
END

!===============================================================================

INTEGER FUNCTION InitLandUse( file_str )

!DEC$ ATTRIBUTES DLLEXPORT :: InitLandUse

USE landuse

!------ read table of land use parameters

IMPLICIT NONE

TYPE( landuse_init), INTENT( IN ) :: file_str

INTEGER, PARAMETER :: MAXN = 50

CHARACTER(16)  c_arg(MAXN)
CHARACTER(400) line
CHARACTER(64)  kwrd

LOGICAL lerr

INTEGER ios, i, j, k, nch, n_arg, ngot
INTEGER lun, alloc_stat

! Already in memory for EPA so skip routine
IF( ALLOCATED(sup_class) )THEN
  InitLandUse = 1
  RETURN
END IF

InitLandUse = 0

IF( file_str%lun <= 0 )THEN
  lun = 99
ELSE
  lun = file_str%lun
END IF
line = ' '

errMsg%routine = 'InitLandUse'

OPEN(lun,FILE=file_str%file,STATUS='OLD',ACTION='READ',IOSTAT=ios)
IF( ios /= 0 )THEN
  errMsg%iParm   = OP_ERROR
  errMsg%jParm   = ios
  errMsg%aString = 'Error opening landuse data file'
  errMsg%bString = 'File = '//TRIM(file_str%file)
  errMsg%cString = 'Check that path and file exist'
  GOTO 9999
END IF

!------ Initialiaze for read errors

errMsg%iParm   = RD_ERROR
errMsg%aString = 'Error reading landuse data file'

!------ Read number of categories and super categories (for averaging)

CALL get_next_data( lun,line,nch,kwrd,n_arg,c_arg,MAXN,lerr )
CALL cupper( kwrd )
IF( lerr .OR. kwrd /= 'NUM_CAT' )THEN
  errMsg%bString = 'Error looking for NUM_CAT keyword'
  GOTO 9999
END IF

READ(c_arg(1),*,IOSTAT=ios) num_cat
IF( ios /= 0 )THEN
  errMsg%jParm   = ios
  errMsg%bString = 'Error reading NUM_CAT (no. of categories)'
  GOTO 9999
END IF

CALL get_next_data( lun,line,nch,kwrd,n_arg,c_arg,MAXN,lerr )
CALL cupper( kwrd )
IF( lerr .OR. kwrd /= 'NUM_SUP' )THEN
  errMsg%bString = 'Error looking for NUM_SUP keyword'
  GOTO 9999
END IF

READ(c_arg(1),*,iostat=ios) num_sup
IF( ios /= 0 )THEN
  errMsg%jParm   = ios
  errMsg%bString = 'Error reading NUM_SUP (no. of super categories)'
  GOTO 9999
END IF

CALL get_next_data( lun,line,nch,kwrd,n_arg,c_arg,MAXN,lerr )
CALL cupper( kwrd )
IF( lerr .OR. kwrd /= 'SUPER_CLASS' )THEN
  errMsg%bString = 'Error looking for SUPER_CLASS keyword'
  GOTO 9999
END IF

n_tot = num_cat + num_sup

!------ Allocate landuse arrays

ALLOCATE( sup_class(n_tot),zruf_lu(4,n_tot),hcnp_lu(4,n_tot),alphac_lu(4,n_tot), &
          albedo_lu(4,n_tot),bowen_lu(4,n_tot,3),string_lu(n_tot),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  errMsg%iParm   = UK_ERROR
  errMsg%aString = 'Error allocating category'
  WRITE(errMsg%bString,'(A,I0)',IOSTAT=alloc_stat) 'Array size = ',n_tot
  GOTO 9999
END IF

DO i = 1,n_arg
  READ(c_arg(i),*,ERR=9999,END=9999) sup_class(i)
  IF( ios /= 0 )THEN
    errMsg%jParm   = ios
    errMsg%bString = 'Error reading category / super class mapping'
    GOTO 9999
  END IF
END DO

ngot = n_arg
DO WHILE( ngot < num_cat )
  CALL get_next_data( lun,line,nch,kwrd,n_arg,c_arg,MAXN,lerr )
  IF( lerr )THEN
    errMsg%bString = 'Error reading category / super class mapping'
    GOTO 9999
  END IF
  DO i = 1,n_arg
    READ(c_arg(i),*,IOSTAT=ios) sup_class(ngot+i)
    IF( ios /= 0 )THEN
      errMsg%jParm   = ios
      errMsg%bString = 'Error reading category / super class mapping'
      GOTO 9999
    END IF
  END DO
  ngot = ngot + n_arg
END DO

DO i = 1,num_sup
  sup_class(num_cat+i) = i
END DO

!------ Roughness

READ(lun,*,IOSTAT=ios)
IF( ios /= 0 )THEN
  errMsg%jParm   = ios
  errMsg%bString = 'Error reading Roughness section header'
  GOTO 9999
END IF

DO i = 1,n_tot
  READ(lun,FMT='(A)',IOSTAT=ios) line
  IF( ios == 0 )READ(line,*,IOSTAT=ios) (zruf_lu(j,i),j=1,4)
  IF( ios /= 0 )THEN
    errMsg%jParm   = ios
    errMsg%bString = 'Error reading Roughness values'
    GOTO 9999
  END IF
  j = INDEX(line,'=')
  IF( j > 0 )THEN
    string_lu(i)%string = TRIM(line(j+1:))
    CALL cupper(string_lu(i)%string)
  END IF
END DO

!------ Canopy height

READ(lun,*,IOSTAT=ios)
IF( ios /= 0 )THEN
  errMsg%jParm   = ios
  errMsg%bString = 'Error reading Canopy Height section header'
  GOTO 9999
END IF
DO i = 1,n_tot
  READ(lun,*,IOSTAT=ios) (hcnp_lu(j,i),j=1,4)
  IF( ios /= 0 )THEN
    errMsg%jParm   = ios
    errMsg%bString = 'Error reading Canopy Height values'
    GOTO 9999
  END IF
END DO

!------ Canopy velocity profile parameter

READ(lun,*,IOSTAT=ios)
IF( ios /= 0 )THEN
  errMsg%jParm   = ios
  errMsg%bString = 'Error reading Canopy Velocity Profile Parameter section header'
  GOTO 9999
END IF
DO i = 1,n_tot
  READ(lun,*,IOSTAT=ios) (alphac_lu(j,i),j=1,4)
  IF( ios /= 0 )THEN
    errMsg%jParm   = ios
    errMsg%bString = 'Error reading Canopy Velocity Profile Parameter values'
    GOTO 9999
  END IF
END DO

!------ Albedo

READ(lun,*,IOSTAT=ios)
IF( ios /= 0 )THEN
  errMsg%jParm   = ios
  errMsg%bString = 'Error reading Albedo section header'
  GOTO 9999
END IF
DO i = 1,n_tot
  READ(lun,*,IOSTAT=ios) (albedo_lu(j,i),j=1,4)
  IF( ios /= 0 )THEN
    errMsg%jParm   = ios
    errMsg%bString = 'Error reading Albedo values'
    GOTO 9999
  END IF
END DO

!------ Bowen ratio (function of wetness)

DO k = 1,3
  READ(lun,*,IOSTAT=ios)
  IF( ios /= 0 )THEN
    errMsg%jParm   = ios
    SELECT CASE( k )
      CASE( 1 )
        errMsg%bString = 'Error reading Bowen Ratio - Dry section header'
      CASE( 2 )
        errMsg%bString = 'Error reading Bowen Ratio - Normal section header'
      CASE( 3 )
        errMsg%bString = 'Error reading Bowen Ratio - Wet section header'
    END SELECT
    GOTO 9999
  END IF
  DO i = 1,n_tot
    READ(lun,*,ERR=9999,END=9999) (bowen_lu(j,i,k),j=1,4)
    IF( ios /= 0 )THEN
      errMsg%jParm   = ios
      SELECT CASE( k )
        CASE( 1 )
          errMsg%bString = 'Error reading Bowen Ratio - Dry values'
        CASE( 2 )
          errMsg%bString = 'Error reading Bowen Ratio - Normal values'
        CASE( 3 )
          errMsg%bString = 'Error reading Bowen Ratio - Wet values'
      END SELECT
      GOTO 9999
    END IF
  END DO
END DO

CLOSE(UNIT=lun,IOSTAT=ios)
IF( ios /= 0 )THEN
  errMsg%iParm   = UK_ERROR
  errMsg%jParm   = ios
  errMsg%aString = 'Error closing landuse data file'
  GOTO 9999
END IF

!------ Success: no error condition

InitLandUse = 1

CALL InitLandUseError()

9999 CONTINUE

!------ Deallocate arrays if an error occured

IF( InitLandUse == 0 )THEN
  IF( ALLOCATED(sup_class) )DEALLOCATE( sup_class,STAT=alloc_stat )
  IF( ALLOCATED(zruf_lu)   )DEALLOCATE( zruf_lu,  STAT=alloc_stat )
  IF( ALLOCATED(hcnp_lu)   )DEALLOCATE( hcnp_lu,  STAT=alloc_stat )
  IF( ALLOCATED(alphac_lu) )DEALLOCATE( alphac_lu,STAT=alloc_stat )
  IF( ALLOCATED(albedo_lu) )DEALLOCATE( albedo_lu,STAT=alloc_stat )
  IF( ALLOCATED(bowen_lu)  )DEALLOCATE( bowen_lu, STAT=alloc_stat )
  IF( ALLOCATED(string_lu) )DEALLOCATE( string_lu,STAT=alloc_stat )
  num_cat = 0
END IF

RETURN
END

!===============================================================================

INTEGER FUNCTION NumLandUseCat()

!DEC$ ATTRIBUTES DLLEXPORT :: NumLandUseCat

!------ Output number of basic land use categories
!       N.B. Does not include superclasses

USE landuse

IMPLICIT NONE

NumLandUseCat = num_cat

RETURN
END

!===============================================================================

INTEGER FUNCTION LandUseCatStrings( ncat,catStrings )

!DEC$ ATTRIBUTES DLLEXPORT :: LandUseCatStrings

!------ Output land use category names as read from roughness section of landuse.dat

USE landuse

IMPLICIT NONE

INTEGER,                       INTENT( IN  ) :: ncat
TYPE( char64T ), DIMENSION(*), INTENT( OUT ) :: catStrings

INTEGER i

LandUseCatStrings = 0

IF( num_cat == 0 .OR. &
    ncat    <  1 .OR. &
    .NOT.ALLOCATED(string_lu) )THEN
  RETURN
END IF

DO i = 1,MIN(num_cat,ncat)
  catStrings(i)%string = TRIM(string_lu(i)%string)
END DO

LandUseCatStrings = 1

RETURN
END

!===============================================================================

INTEGER FUNCTION LandUseCatFromString( catString ) RESULT( icat )

!DEC$ ATTRIBUTES DLLEXPORT :: LandUseCatFromString

!------ Output land use category index based on an input string
!       First look for exact match with categories in SCIPUFF landuse.dat
!       Then look for keywords to set superclasses
!       Default is Grassland superclass

USE landuse
USE charT_fd

IMPLICIT NONE

TYPE( char64T ), INTENT( IN ) :: catString

INTEGER i

CHARACTER(64) string
CHARACTER(6)  str

LOGICAL, EXTERNAL :: matchstr

string = TRIM(catString%string)
CALL cupper( string )

icat = 0  !Default output (implies no recognized category)

!------ Search for exact matches with landuse.dat categories

DO i = 1,n_tot
  IF( TRIM(string) == TRIM(string_lu(i)%string) )THEN
    icat = i
    EXIT
  END IF
END DO

!----- If no match, look for certain keywords in string; set superclasses if appropriate

IF( icat == 0 )THEN

  IF( matchstr(string,'URBAN DEVELOPED CITY TOWN BUILTUP SUBURBAN') )THEN
    str = 'URBAN'
  ELSE IF( matchstr(string,'GRASS GRASSLAND FIELD FIELDS CROP FARM CULTIVATED  &
                            &TUNDRA WETLAND STEPPE PRAIRIE MEADOW') )THEN
    str = 'GRASS'
  ELSE IF( matchstr(string,'FOREST WOOD WOODS WOODED TREE TREES') )THEN
    str = 'FOREST'
  ELSE IF( matchstr(string,'DESERT BARE BARREN DRY') )THEN
    str = 'DESERT'
  ELSE IF( matchstr(string,'WATER OCEAN SEA BAY SOUND LAKE RIVER') )THEN
    str = 'WATER'
  ELSE
    str = 'NONE'
  END IF

  IF( TRIM(str) /= 'NONE' )THEN
    DO i = 1,num_sup
      IF( INDEX(string_lu(i+num_cat)%string,TRIM(str)) > 0 )THEN
        icat = 1000+i
        EXIT
      END IF
    END DO
  END IF

END IF

RETURN
END

!===============================================================================

INTEGER FUNCTION ExitLandUse()

!DEC$ ATTRIBUTES DLLEXPORT :: ExitLandUse

USE landuse

IMPLICIT NONE

INTEGER alloc_stat

INTEGER, EXTERNAL :: ClearLandUseCat

num_cat = 0

ExitLandUse = ClearLandUseCat()

IF( ExitLandUse <= 0 )RETURN

IF( ALLOCATED(sup_class) )DEALLOCATE( sup_class,STAT=alloc_stat )
IF( ALLOCATED(zruf_lu)   )DEALLOCATE( zruf_lu,  STAT=alloc_stat )
IF( ALLOCATED(hcnp_lu)   )DEALLOCATE( hcnp_lu,  STAT=alloc_stat )
IF( ALLOCATED(alphac_lu) )DEALLOCATE( alphac_lu,STAT=alloc_stat )
IF( ALLOCATED(albedo_lu) )DEALLOCATE( albedo_lu,STAT=alloc_stat )
IF( ALLOCATED(bowen_lu)  )DEALLOCATE( bowen_lu, STAT=alloc_stat )
IF( ALLOCATED(string_lu) )DEALLOCATE( string_lu,STAT=alloc_stat )

!------ set status based on succesful deallocation

IF( ALLOCATED(sup_class) .OR. ALLOCATED(zruf_lu)   .OR. ALLOCATED(hcnp_lu)  .OR. &
    ALLOCATED(alphac_lu) .OR. ALLOCATED(albedo_lu) .OR. ALLOCATED(bowen_lu) .OR. &
    ALLOCATED(string_lu) )THEN
  ExitLandUse = 0
  errMsg%iParm   = IV_ERROR
  errMsg%routine = 'ExitLandUse'
  errMsg%aString = 'Arrays not deallocated'
ELSE
  ExitLandUse = 1
END IF

RETURN
END

!===============================================================================

INTEGER FUNCTION GetLastLandUseError( error )

!DEC$ ATTRIBUTES DLLEXPORT :: GetLastLandUseError

USE landuse
USE message_fd

IMPLICIT NONE

TYPE( messageT ), INTENT( OUT ) :: error

!------ Load message structure

error = errMsg

!------ Clear error and return

CALL InitLandUseError()

GetLastLandUseError = 1

RETURN
END

!===============================================================================

SUBROUTINE InitLandUseError()

USE landuse

errMsg%iParm   = 0
errMsg%jParm   = 0
errMsg%aString = CHAR(0)
errMsg%bString = CHAR(0)
errMsg%cString = CHAR(0)
errMsg%routine = CHAR(0)

RETURN
END

!===============================================================================

INTEGER FUNCTION GetSeason( lat,jul )

!DEC$ ATTRIBUTES DLLEXPORT :: GetSeason

!------ Set season based on latitude and julian day
!       1=spring, 2=summer, 3=autumn, 4=winter

IMPLICIT NONE

REAL,    INTENT( IN ) :: lat
INTEGER, INTENT( IN ) :: jul

REAL, PARAMETER :: S = 0.06

INTEGER is
REAL    jday, sday

IF( jul <= 0 .OR. jul > 366 )THEN

   GetSeason = -1
   RETURN

ELSE IF( lat < -90. .OR. lat > 90. )THEN

   GetSeason = -2
   RETURN

ELSE

  IF( ABS(lat) > 70. )THEN
    is = 4
  ELSE IF( ABS(lat) < 20. )THEN
    is = 2
  ELSE
    jday = FLOAT(MOD(jul+345,365))/365.
    IF( lat > 0. )THEN
      sday = 0.5*(lat-20.)/50.
      IF( jday < sday-S )THEN
        is = 4
      ELSE IF( jday < sday+S )THEN
        is = 1
      ELSE IF( jday < 1.-sday-S )THEN
        is = 2
      ELSE IF( jday < 1.-sday+S )THEN
        is = 3
      ELSE
        is = 4
      END IF
    ELSE
      sday = 0.5*(70.+lat)/50.
      IF( jday < sday-S )THEN
        is = 2
      ELSE IF( jday < sday+S )THEN
        is = 3
      ELSE IF( jday < 1.-sday-S )THEN
        is = 4
      ELSE IF( jday < 1.-sday+S )THEN
        is = 1
      ELSE
        is = 2
      END IF
    END IF
  END IF

  GetSeason = is

END IF

RETURN
END

!===========================================================================

INTEGER FUNCTION SetLandUse( index,season,i_wet,zruf,hc,alphac,albedo,bowen )

!DEC$ ATTRIBUTES DLLEXPORT :: SetLandUse

USE landuse

IMPLICIT NONE

INTEGER, INTENT( IN  ) :: index, season, i_wet
REAL,    INTENT( OUT ) :: zruf, hc, alphac, albedo, bowen

INTEGER id

SetLandUse = 0

id = index

IF( id > 1000 )id = id - 1000 + num_cat

IF( id < 1 .OR. id > num_cat+num_sup )RETURN

zruf   =   zruf_lu(season,id)
hc     =   hcnp_lu(season,id)
alphac = alphac_lu(season,id)
albedo = albedo_lu(season,id)
bowen  =  bowen_lu(season,id,i_wet)

SetLandUse = 1

RETURN
END

!===============================================================================

INTEGER FUNCTION GetLandUsePathLength()

!DEC$ ATTRIBUTES DLLEXPORT :: GetLandUsePathLength

!------ Initialize land use categories

USE param_fd

IMPLICIT NONE

GetLandUsePathLength = PATH_MAXLENGTH

RETURN
END

!===============================================================================

INTEGER FUNCTION GetLandUseVersion()

!DEC$ ATTRIBUTES DLLEXPORT :: GetLandUseVersion

!------ Initialize land use categories

USE landuse

IMPLICIT NONE

GetLandUseVersion = LAND_USE_VERSION

RETURN
END

!===============================================================================

INTEGER FUNCTION GetNumSubstrates( file_str,nsub )

USE landuse

!DEC$ ATTRIBUTES DLLEXPORT :: GetNumSubstrates

!------ Get number of substrate categories for secondary evaporation

IMPLICIT NONE

TYPE( landuse_init), INTENT( IN  ) :: file_str
INTEGER,             INTENT( OUT ) :: nsub

INTEGER lun, ios

INTEGER, EXTERNAL :: SkipToSubstrate

GetNumSubstrates = 0

ios = SkipToSubstrate( file_str,lun,nsub )
IF( ios /= 0 )GOTO 9999

GetNumSubstrates = 1

9999 CONTINUE

CLOSE(UNIT=lun,IOSTAT=ios)

RETURN
END

!===============================================================================

INTEGER FUNCTION GetSubstrates( file_str,nsub,substrate )

USE landuse

!DEC$ ATTRIBUTES DLLEXPORT :: GetSubstrates

!------ Initialize substrate category names for secondary evaporation

IMPLICIT NONE

TYPE( landuse_init ),           INTENT( IN  ) :: file_str
INTEGER,                        INTENT( IN  ) :: nsub
CHARACTER(16), DIMENSION(nsub), INTENT( OUT ) :: substrate

INTEGER lun, ios, i, j, nn

CHARACTER(128) line

INTEGER, EXTERNAL :: SkipToSubstrate

GetSubstrates = 0

ios = SkipToSubstrate( file_str,lun,nn )
IF( ios /= 0 .OR. nn /= nsub )GOTO 9999

READ(lun,*,ERR=9999,END=9999)   ! Skip header

DO i = 1,nsub
  READ(lun,'(A)',ERR=9999,END=9999) line
  j = INDEX(line,' ')
  IF( j <= 0 .OR. j > 17 )GOTO 9999
  substrate(i) = line(1:j-1)
END DO

GetSubstrates = 1

9999 CONTINUE

CLOSE(UNIT=lun,IOSTAT=ios)

RETURN
END

!===============================================================================

INTEGER FUNCTION GetSubstrateParams( file_str,substrate_type,porosity, &
                                                 tortuosity,grain_size )

USE landuse

!DEC$ ATTRIBUTES DLLEXPORT :: GetSubstrateParams

!------ Get substrate category parameters for secondary evaporation

IMPLICIT NONE

TYPE( landuse_init), INTENT( IN  ) :: file_str
INTEGER,             INTENT( IN  ) :: substrate_type
REAL,                INTENT( OUT ) :: porosity,tortuosity,grain_size

INTEGER lun, ios, i, j, nn

CHARACTER(128) line

INTEGER, EXTERNAL :: SkipToSubstrate

GetSubstrateParams = 0

ios = SkipToSubstrate( file_str,lun,nn )
IF( ios /= 0 .OR. nn < substrate_type )GOTO 9999

READ(lun,*,ERR=9999,END=9999)   ! Skip header

DO i = 1,substrate_type
  READ(lun,'(A)',ERR=9999,END=9999) line
END DO

j = INDEX(line,' ')
IF( j <= 0 .OR. j > 17 )GOTO 9999

READ(line(j:),*,ERR=9999,END=9999) porosity,tortuosity,grain_size

GetSubstrateParams = 1

9999 CONTINUE

CLOSE(UNIT=lun,IOSTAT=ios)

RETURN
END

!===============================================================================

INTEGER FUNCTION SkipToSubstrate( file_str,lun,nn )

!------ Skip landuse data; return number of substrate categories

USE landuse

IMPLICIT NONE

TYPE( landuse_init), INTENT( IN  ) :: file_str
INTEGER,             INTENT( OUT ) :: lun, nn

INTEGER ios, i

CHARACTER(128) line

errMsg%routine = 'SkipToSubstrate'
errMsg%bString = 'File = '//TRIM(file_str%file)

IF( file_str%lun <= 0 )THEN
  lun = 99
ELSE
  lun = file_str%lun
END IF

OPEN(UNIT=lun,FILE=TRIM(file_str%file),STATUS='OLD',ACTION='READ',IOSTAT=ios)
IF( ios /= 0 )THEN
  errMsg%iParm   = OP_ERROR
  errMsg%jParm   = ios
  errMsg%aString = 'Error opening landuse data file'
  errMsg%cString = 'Check that path and file exist'
  GOTO 9999
END IF

!------ Read number of categories and super categories (for averaging)

DO
  READ(lun,'(A)',IOSTAT=ios) line
  IF( ios /= 0 )THEN
    errMsg%iParm   = RD_ERROR
    errMsg%jParm   = ios
    errMsg%aString = 'Error seaching for substrate categories'
    GOTO 9999
  END IF
  CALL cupper( line )
  i = INDEX(line,'SUBSTRATE CATEGORIES')
  IF( i /= 0 )EXIT
END DO

READ(line(i+20:),*,IOSTAT=ios) nn
IF( ios /= 0 )THEN
  errMsg%iParm   = RD_ERROR
  errMsg%jParm   = ios
  errMsg%aString = 'Error reading number of substrates'
  GOTO 9999
END IF

errMsg%routine = CHAR(0)
errMsg%bString = CHAR(0)

9999 CONTINUE

SkipToSubstrate = ios

RETURN
END

!===============================================================================

INTEGER FUNCTION GetNumAlternateSubstrates( file_str,nsub )

USE landuse

!DEC$ ATTRIBUTES DLLEXPORT :: GetNumAlternateSubstrates

!------ Get number of substrate categories for secondary evaporation

IMPLICIT NONE

TYPE( landuse_init), INTENT( IN  ) :: file_str
INTEGER,             INTENT( OUT ) :: nsub

INTEGER lun, ios
CHARACTER(128) line

INTEGER, EXTERNAL :: SkipToAlternateSubstrate

GetNumAlternateSubstrates = 0

ios = SkipToAlternateSubstrate( file_str,lun )
IF( ios /= 0 )GOTO 9999

nsub = 0
DO
  READ(lun,'(A)',IOSTAT=ios) line
  IF( ios /= 0 )EXIT
  IF( line(1:2) == " '" )nsub = nsub + 1
END DO
IF ( ios > 0 )GOTO 9999

GetNumAlternateSubstrates = 1

9999 CONTINUE

CLOSE(UNIT=lun,IOSTAT=ios)

RETURN
END

!===============================================================================

INTEGER FUNCTION GetAlternateSubstrates( file_str,nsub,substrate )

USE landuse

!DEC$ ATTRIBUTES DLLEXPORT :: GetAlternateSubstrates

!------ Initialize substrate category names for secondary evaporation

IMPLICIT NONE

TYPE( landuse_init),            INTENT( IN  ) :: file_str
INTEGER,                        INTENT( IN  ) :: nsub
CHARACTER(16), DIMENSION(nsub), INTENT( OUT ) :: substrate

INTEGER lun, ios, nn

CHARACTER(128) line

INTEGER, EXTERNAL :: SkipToAlternateSubstrate

GetAlternateSubstrates = 0

ios = SkipToAlternateSubstrate( file_str,lun )
IF( ios /= 0 )GOTO 9999

nn = 0
DO
  READ(lun,'(A)',IOSTAT=ios) line
  IF( ios /= 0 )EXIT
  IF( line(1:2) == " '" )THEN
    nn = nn + 1
    IF( nn > nsub )GOTO 9999
    substrate(nn) = line(3:15)
  END IF
END DO
IF ( ios > 0 )GOTO 9999
IF ( nn /= nsub )GOTO 9999

GetAlternateSubstrates = 1

9999 CONTINUE

CLOSE(UNIT=lun,IOSTAT=ios)

RETURN
END

!===============================================================================

INTEGER FUNCTION SkipToAlternateSubstrate( file_str,lun )

USE landuse

IMPLICIT NONE

TYPE( landuse_init), INTENT( IN  ) :: file_str
INTEGER,             INTENT( OUT ) :: lun

INTEGER ios, i

CHARACTER(128) line

IF( file_str%lun <= 0 )THEN
  lun = 99
ELSE
  lun = file_str%lun
END IF

OPEN(UNIT=lun,FILE=TRIM(file_str%file),STATUS='OLD',ACTION='READ',IOSTAT=ios)
IF( ios /= 0 )GOTO 9999

!------ Read to start of alternate substartes

DO
  READ(lun,'(A)',IOSTAT=ios) line
  IF( ios /= 0 )GOTO 9999
  CALL cupper( line )
  i = INDEX(line,'VLSSFC.PAR:')
  IF( i /= 0 )EXIT
END DO

9999 CONTINUE

SkipToAlternateSubstrate = ios

RETURN
END
