INTEGER FUNCTION SWIMinitObsAssim( fld )

!------ Setup obs structures for use in assimilation into existing met field

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

TYPE( MetField ), INTENT( INOUT ) :: fld

INTEGER irv, alloc_stat, i, jObs, nxy, nxyz, nzt
LOGICAL lRifl

INTEGER, EXTERNAL :: SWIMallocObsWt, SWIMalloc3dField, SWIMallocBLParam
INTEGER, EXTERNAL :: MEDOCexpand, SetTerrainGrad

SWIMinitObsAssim = SWIMfailure

!------ Check for consistency of time reference

DO i = 1,fld%nObsSource
  jObs = fld%iObsSource(i)
  IF( ObsSrc(jObs)%local .NEQV. Prj%localMet )THEN
    error%Number  = IV_ERROR
    error%Routine = 'SWIMinitObsAssim'
    error%Message = 'Inconsistent time references (Local or UTC) in obs files'
    GOTO 9999
  END IF
END DO

!------ Modify MEDOC grids and fields if on unstaggered grid
!       N.B. Non-MEDOC input is invalid

IF( .NOT.(BTEST(fld%grid%type,GTB_STAGGER) .AND. BTEST(fld%grid%type,GTB_STAGGERZ)) )THEN

  IF( .NOT.BTEST(fld%type,FTB_MEDOC) )THEN  !Only MEDOC input can be shifted from un- to staggered grid
    error%Number  = IV_ERROR
    error%Message = 'Non-MEDOC gridded fields must be on C (staggered) grid for assimilation'
    error%Inform  = '(MEDOC fields are averaged onto staggered locations)'
    GOTO 9999
  END IF

  IF( .NOT.BTEST(fld%type,FTB_W) )THEN  !Add W if not on file
    fld%type = IBSET(fld%type,FTB_W)
    nxyz = fld%grid%nXY*fld%grid%nZ  !Additional level will be added in MEDOCexpand
    ALLOCATE( fld%NextField%W(nxyz),fld%Field%W(nxyz),STAT=alloc_stat )
    IF( alloc_stat /= 0 )THEN
      error%Number  = UK_ERROR
      error%Routine = 'SWIMinitObsAssim'
      error%Message = 'Error allocating W field for assimilation into MEDOC fields'
    END IF
    DO i = 1,nxyz
      fld%Field%W(i)     = 0.
      fld%NextField%W(i) = 0.
    END DO
    irv = SetTerrainGrad( fld%grid )
    IF( irv /= SWIMsuccess )GOTO 9999
  END IF

  nzt = fld%grid%NZ + 1
  ALLOCATE( fld%grid%Zw(nzt),STAT=alloc_stat )  !Need vertical grid for W
  IF( alloc_stat /= 0 )THEN
    error%Number  = UK_ERROR
    error%Routine = 'SWIMinitObsAssim'
    error%Message = 'Error allocating Zw field for assimilation into MEDOC fields'
  END IF

  fld%grid%Zw(1) = 0.
  DO i = 2,nzt-1
    fld%grid%Zw(i) = 0.5*(fld%grid%Z(i-1)+fld%grid%Z(i))
  END DO
  fld%grid%Zw(nzt) = 1.5*fld%grid%Z(nzt-1) - 0.5*fld%grid%Z(nzt-2)

  fld%grid%Ztop = fld%grid%Zw(nzt-1)

  irv = MEDOCexpand( fld,0 ) !Add extra 2d slice for vertical staggered grid

END IF

!------ Check for use of influence radius

lRifl = .TRUE.
DO i = 1,fld%nObsSource
  jObs = fld%iObsSource(i)
  IF( .NOT.BTEST(ObsSrc(jObs)%type,OTB_RIFL) )lRifl = .FALSE.
END DO

IF(.NOT.lRifl )THEN
  DO i = 1,fld%nObsSource
    jObs = fld%iObsSource(i)
    ObsSrc(jObs)%type = IBCLR(ObsSrc(jObs)%type,OTB_RIFL)
  END DO
END IF

!------ Allocate gridded fields that will be saved for temporal interpolation

nxy  = fld%grid%nXY
nxyz = nxy*(fld%grid%nZ+1)  !Vertical grid must be staggered for McWif

irv = SWIMalloc3dField( fld%type,nxyz,fld%Field1 )
IF( irv /= SWIMsuccess )GOTO 9999

irv = SWIMalloc3dField( fld%type,nxyz,fld%Field2 )
IF( irv /= SWIMsuccess )GOTO 9999

irv = SWIMallocBLParam( fld%type,nxy,fld%BL1,fld%BL2 )
IF( irv /= SWIMsuccess )GOTO 9999

!----- Allocate for obs weights
!      N.B. this is based on fld%type, not necessarily obs

irv = SWIMallocObsWt( fld%type,nxy,nxyz,fld%obsWt )
IF( irv /= SWIMsuccess )GOTO 9999

!----- Set 'first' status

fld%status = IBSET(fld%status,FSB_FIRSTASSM)

SWIMinitObsAssim = SWIMresult

9999 CONTINUE

RETURN
END


