INTEGER FUNCTION SWIMaddPolarField( ifld,iPole )

!------ Add polar met field
!       Note: currently always a single location at the pole

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

INTEGER, INTENT( IN ) :: ifld, iPole

TYPE( MetField), POINTER :: fld, fldp

INTEGER irv, alloc_stat, n

INTEGER, EXTERNAL :: SWIMreallocMetField, InitLSV, GridLogMsg
INTEGER, EXTERNAL :: SWIMallocTerrain, SWIMalloc3dField, SWIMallocBLParam
INTEGER, EXTERNAL :: SWIMallocBLaux, SWIMallocVariance, SetGridLL

SWIMaddPolarField = SWIMfailure

!------ Add a met field

irv = SWIMreallocMetField( 1 )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Point to polar met field and 'parent'

fld  => field(numField)
fldp => field(ifld)

fld%index = 0
CALL setFieldIndex( fld%index,numField )

!------ Set times as 'not set'

fld%t     = NOT_SET_R
fld%tNext = NOT_SET_R
fld%tBL   = NOT_SET_R

!------ Set initial status

fld%status = 0
fld%status = IBSET(fld%status,FSB_DOINIT)
fld%status = IBSET(fld%status,FSB_UPDATE)

!------ Set field type

IF( iPole == NORTHPOLE )THEN
  fld%type = IBSET(0,FTB_NPOLE)
ELSE
  fld%type = IBSET(0,FTB_SPOLE)
END IF

IF( BTEST(fldp%type,FTB_T) )fld%type = IBSET(fld%type,FTB_T)
IF( BTEST(fldp%type,FTB_P) )fld%type = IBSET(fld%type,FTB_P)
IF( BTEST(fldp%type,FTB_H) )fld%type = IBSET(fld%type,FTB_H)
IF( BTEST(fldp%type,FTB_QCLD) )fld%type = IBSET(fld%type,FTB_QCLD)

!------ Set LSV & uncertainty types

IF( BTEST(fldp%type,FTB_LSV ) )fld%type = IBSET(fld%type,FTB_LSV)
IF( BTEST(fldp%type,FTB_LSVL) )fld%type = IBSET(fld%type,FTB_LSVL)

!------ Set BL type

SELECT CASE( fldp%BLtype )

  CASE( BLP_NONE )
    fld%BLtype = fldp%BLtype

  CASE DEFAULT
    fld%BLtype = BLP_NEST

END SELECT

!------ Set grid source type and save parent field number

fld%gridSource%type = IBSET(fld%gridSource%type,GSB_NEST)
fld%gridSource%unit = ifld

!------ Set polar grid parameters

fld%grid%type = IBSET(0,GTB_LATLON)

IF( iPole == NORTHPOLE )THEN
  fld%grid%type = IBSET(fld%grid%type,GTB_NPOLE)
ELSE
  fld%grid%type = IBSET(fld%grid%type,GTB_SPOLE)
END IF

!------ Single location (at the pole) for now

fld%grid%Xmin = -180.
fld%grid%Xmax =  180.
fld%grid%dX   =  360.
fld%grid%nX   = 1

IF( iPole == NORTHPOLE )THEN
  fld%grid%Ymin = fldp%grid%Ymax
  fld%grid%Ymax = 90.
ELSE
  fld%grid%Ymin = -90.
  fld%grid%Ymax = fldp%grid%Ymin
END IF
fld%grid%dY = fld%grid%Ymax - fld%grid%Ymin
fld%grid%nY = 1

fld%grid%nXY = fld%grid%nX*fld%grid%nY

!------ Copy parent vertical grid

fld%grid%nZ = fldp%grid%nZ

ALLOCATE( fld%grid%Z(fldp%grid%nZ),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'SWIMaddPolarField'
  error%Message = 'Error allocating vertical grid arrays'
  GOTO 9999
END IF

fld%grid%Z  = fldp%grid%Z

fld%grid%Ztop = fldp%grid%Ztop

!------ Set terrain & landuse

irv = SWIMallocTerrain( fld%grid )
IF( irv /= SWIMsuccess )GOTO 9999

fld%grid%terrain%H  = 0.
fld%grid%terrain%D  = 1.
fld%grid%terrain%Du = 1.; fld%grid%terrain%Dv = 1.

fld%grid%Hmin = 0.

fld%grid%landcover%roughness = 2.E-4  !Between tundra and snow/ice
fld%grid%landcover%canopyHt  = 0.
fld%grid%landcover%alpha     = 0.
fld%grid%landcover%Bowen     = 1.0
fld%grid%landcover%albedo    = 0.5

fld%grid%landcover%kbl = 1

!------ Set Lat/Lon grid

fld%grid%coord%type = I_LATLON

irv = SetGridLL( fld%grid )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Allocate fields based on parent

n = fld%grid%nXY * fld%grid%nZ

irv = SWIMalloc3dField( fld%type,n,fld%Field )
IF( irv /= SWIMsuccess )GOTO 9999

irv = SWIMalloc3dField( fld%type,n,fld%NextField )
IF( irv /= SWIMsuccess )GOTO 9999

n = fld%grid%nXY

irv = SWIMallocBLParam( fld%type,n,fld%BL,fld%NextBL )
IF( irv /= SWIMsuccess )GOTO 9999

fld%BLaux%nz = Prj%BL%nzbl

irv = SWIMallocBLaux( n,fld%BLaux )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Initialize large-scale variability

fld%LSVtype = fldp%LSVtype
irv = InitLSV( field(numField) )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Initialize wind uncertainty


!------ Log messages

irv = GridLogMsg( field(numField) )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Another SWIMming success

SWIMaddPolarField = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SWIMcheckPolar( ifld )

!------ Check if polar field(s) should be added:
!       Project latitude must extend at least up/down to +/- 86 det;
!       Parent field must be gridded, Lat/Lon, 360 deg. in Longitude
!       and extend at least up/down to +/- 86 deg.

USE SWIM_fi
USE SWIMparam_fd

INTEGER, INTENT( IN ) :: ifld

INTEGER irv
REAL    PolarLimit
LOGICAL PrjNorth, PrjSouth

INTEGER, EXTERNAL :: SWIMaddPolarField

SWIMcheckPolar = SWIMfailure

PolarLimit = POLARCAP_LAT-0.1

PrjNorth = (Prj%Ymax >=  PolarLimit)
PrjSouth = (Prj%Ymin <= -PolarLimit)

LatCheck : IF(  PrjNorth .OR. PrjSouth )THEN

  MetType : IF( BTEST(field(ifld)%type,FTB_MEDOC ) .OR. &
                BTEST(field(ifld)%type,FTB_SCIP  ) )THEN

    LLgrid : IF( BTEST(field(ifld)%grid%type,GTB_LATLON) )THEN

      GlobalLon : IF( field(ifld)%grid%Xmax-field(ifld)%grid%Xmin >= &
                                       360.-field(ifld)%grid%dX )THEN

        AboveNorthLat : IF( field(ifld)%grid%Ymax+0.5*field(ifld)%grid%dY >= PolarLimit .AND. &
                                                                                 PrjNorth )THEN
          irv = SWIMaddPolarField( ifld,NORTHPOLE )
          IF( irv /= SWIMsuccess )GOTO 9999
        END IF AboveNorthLat

        BelowSouthLat : IF( field(ifld)%grid%Ymin-0.5*field(ifld)%grid%dY <= -PolarLimit .AND. &
                                                          PrjSouth )THEN
          irv = SWIMaddPolarField( ifld,SOUTHPOLE )
          IF( irv /= SWIMsuccess )GOTO 9999
        END IF BelowSouthLat

      END IF GlobalLon

    END IF LLgrid

  END IF MetType

END IF LatCheck

SWIMcheckPolar = SWIMresult

9999 CONTINUE

RETURN
END

