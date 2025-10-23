INTEGER FUNCTION InitSCIPUFFProject( UserID )

USE SCIPUFFdriver_fi
USE SCIPtool

IMPLICIT NONE

INTEGER, INTENT( IN ) :: UserID

INTEGER i,j,irv

TYPE( pinputT )    :: tmpInp
TYPE( pweatherT )  :: tmpMet
TYPE( pmaterialT ) :: tmpMat

REAL, DIMENSION(21) :: tmpArray

InitSCIPUFFProject = FAILURE

!------ Set default values

tmpInp%project = new%project

tmpInp%input%mtlHead%max    = 2  !Maximum number of materials
tmpInp%input%mtlHead%number = 1

ALLOCATE( mtlList(tmpInp%input%mtlHead%max),STAT=irv )
IF( irv /= 0 )THEN
  WRITE(*,'(A)') 'Error allocating material list'
  GOTO 9999
END IF

DO i = 1,tmpInp%input%mtlHead%max
  mtlList(i)%type          = NOT_SET_I
  mtlList(i)%puffIndex     = NOT_SET_I
  mtlList(i)%iNotUsed(1)   = NOT_SET_I
  mtlList(i)%iNotUsed(2)   = NOT_SET_I
  DO j = 1,HS_PADMTLGEN
    mtlList(i)%matData%padding(j) = NOT_SET_I
  END DO
  mtlList(i)%name          = ' '
  mtlList(i)%units         = ' '
  mtlList(i)%file          = ' '
  mtlList(i)%path          = ' '
END DO

irv = SCIPDefaultInpF( UserID,tmpInp,mtlList )
IF( irv == SCIPfailure )THEN
  WRITE(*,'(A)') 'Failed to load default project input'
  GOTO 9999
END IF

tmpMat%mtlHead%max    = tmpInp%input%mtlHead%max
tmpMat%mtlHead%number = 2

mtlList(1)%type = HM_GAS
mtlList(2)%type = HM_PARTICLE

irv = SCIPDefaultMaterialF( UserID,tmpMat,mtlList )
IF( irv == SCIPfailure )THEN
  WRITE(*,'(A)') 'Failed to load default material input'
  GOTO 9999
END IF

irv = SCIPDefaultWeatherF( UserID,tmpMet )
IF( irv == SCIPfailure )THEN
  WRITE(*,'(A)') 'Failed to load default weather input'
  GOTO 9999
END IF

!------ Copy from local structure

new%input%time   = tmpInp%input%time
new%input%flags  = tmpInp%input%flags
new%input%domain = tmpInp%input%domain
new%input%option = tmpInp%input%option

new%weather = tmpMet%weather

new%weather%lsv%type        = HL_LSVOPER  !HL_NONE for OFF
new%weather%flags%reference = HT_LOCAL    !Default met time reference to local

new%weather%bl%roughness = NOT_SET_R
new%weather%bl%landuse   = 'NOT SET'

new%input%time%start%time%reference = HT_LOCAL  !Default local time
new%input%time%end%time%reference   = HT_LOCAL
new%input%time%start%zone           = NOT_SET_R !0. if default is UTC

new%input%time%end%step%max    = 900.          !Default timestep 15 minutes
new%input%time%end%step%output = 1.            !Default hourly output interval

new%input%option%timeAvg = 3600. !Default 1-hour time-averaging

!------ Set dynamic mode

new%input%flags%method = IBSET(new%input%flags%method,HFB_DYNAMIC)

new%input%option%lOutputVariance = .FALSE.

!------ Allow 10 releases initially. (Can be incremented if needed.)
!       N.B. These may be further defined with hourly emission rates

new%scnHead%max    = 10
new%scnHead%number = 0

ALLOCATE( relList(new%scnHead%max),lRelPrime(new%scnHead%max),SrcFlag(new%scnHead%max),STAT=irv )
IF( irv /= 0 )THEN
  WRITE(*,'(A)') 'Error allocating release list'
  GOTO 9999
END IF

DO i = 1,new%scnHead%max
  CALL InitRelList( relList(i) )
END DO

lRelPrime      = .FALSE.
lIntrpEmission = .TRUE.
SrcFlag        = 0

!------ Default material

new%input%mtlHead%max    = tmpInp%input%mtlHead%max
new%input%mtlHead%number = 1

mtlList(1)%name  = 'TRACER' !Default
mtlList(1)%type  = HM_GAS
mtlList(1)%units = 'KG'

gasMatl0 = TRANSFER(mtlList(1)%matData,gasMatl0)

gasMatl0%minConcentration = 0.
gasMatl0%gasDensity       = 1.2
gasMatl0%gasDeposition    = 0.
gasMatl0%save              = IBSET(gasMatl0%save,HSB_TOTALDOS)
gasMatl0%save              = IBSET(gasMatl0%save,HSB_TOTALDEP)

mtlList(1)%matData = TRANSFER(gasMatl0,mtlList(1)%matData)
gasMatl0 = TRANSFER(mtlList(1)%matData,gasMatl0)  !debug

ALLOCATE( gasMatl(new%input%mtlHead%max),STAT=irv )
IF( irv /= 0 )THEN
  WRITE(*,'(A)') 'Error allocating gas material array'
  GOTO 9999
END IF

gasMatl(1) = gasMatl0

!----- PM2.5 and PM10 materials

mtlList(2)%name  = 'PART'
mtlList(2)%type  = HM_PARTICLE
mtlList(2)%units = 'KG'

partMatl2p5 = TRANSFER(mtlList(2)%matData,partMatl2p5)

partMatl2p5%minConcentration = 0.
partMatl2p5%save             = 0

partMatl10 = partMatl2p5

partMatl2p5%nSizeBins = 14
partMatl10%nSizeBins  = 20

tmpArray = (/ &
   1.0000002E-07,   1.2589263E-07,   1.5848934E-07,   1.9952635E-07,&
   2.5118865E-07,   3.1622793E-07,   3.9810718E-07,   5.0118746E-07,&
   6.3095729E-07,   7.9432851E-07,   1.0000008E-06,   1.2589257E-06,&
   1.5848927E-06,   1.9952627E-06,   2.5118879E-06,   3.1622778E-06,&
   3.9810739E-06,   5.0118724E-06,   6.3095758E-06,   7.9432893E-06,&
   1.0000012E-05 /)

partMatl10%binBounds(1:21)  = tmpArray(1:21)
partMatl2p5%binBounds(1:15) = tmpArray(1:15)

tmpArray = (/ &
   1.1441114E-07,   1.4403508E-07,   1.8132944E-07,   2.2828024E-07,&
   2.8738779E-07,  3.6179981E-07,   4.5547895E-07,   5.7341413E-07, &
   7.2188561E-07,   9.0880002E-07,   1.1441116E-06,   1.4403511E-06, &
   1.8132946E-06,   2.2828031E-06,   2.8738789E-06,   3.6179983E-06, &
   4.5547908E-06,   5.7341426E-06,  7.2188564E-06,   9.0880021E-06, &
   0.0 /)

partMatl10%binSize(1:20)  = tmpArray(1:20)
partMatl2p5%binSize(1:14) = tmpArray(1:14)

ALLOCATE( partMatl(new%input%mtlHead%max),STAT=irv )
IF( irv /= 0 )THEN
  WRITE(*,'(A)') 'Error allocating particle material array'
  GOTO 9999
END IF

partMatl(1) = partMatl2p5

!------ Initialize for met list

nMet = 0
isrf = 1
iprf = 1

!------ Default to run (instead of setup only)

lSetupOnly = .FALSE.

!------ Default array limits

limit%met1D        = HUGE(1)
limit%puffs        = 20000
limit%surfaceGrid  = 25000

InitSCIPUFFProject = SUCCESS

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE InitRelList( relListi )

USE tooluser_fd
USE default_fd

TYPE( releaseT ), INTENT( OUT ) :: relListi

INTEGER j

relListi%padding   = NOT_SET_I
relListi%type      = NOT_SET_I
relListi%status    = HS_VALID
relListi%tRel      = NOT_SET_R
relListi%xRel      = NOT_SET_R
relListi%yRel      = NOT_SET_R
relListi%zRel      = NOT_SET_R
relListi%notUsedA  = NOT_SET_R
relListi%notUsedB  = NOT_SET_R
relListi%notUsed   = NOT_SET_R
DO j = 1,HS_PADRELGEN
  relListi%relData%padding(j) = NOT_SET_I
END DO
relListi%material   = ' '
relListi%relName    = ' '
relListi%relDisplay = ' '

RETURN
END

