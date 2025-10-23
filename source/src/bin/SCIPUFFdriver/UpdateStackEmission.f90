
INTEGER FUNCTION UpdateStackEmission( relMC,update,updateMC,nMCup  )

!------ Update either standard or multicomponent stacks, depending on input arguments

USE SCIPUFFdriver_fi
USE update_fd
USE constants_fd

IMPLICIT NONE

TYPE( releaseMCT ),   DIMENSION(:), POINTER         :: relMC
TYPE( updateRelT ),   OPTIONAL,     INTENT( INOUT ) :: update
TYPE( updateRelMCT ), OPTIONAL,     INTENT( INOUT ) :: updateMC
INTEGER,              OPTIONAL,     INTENT( IN    ) :: nMCup

INTEGER irv, i, j, nMCx, releaseType
REAL    t, rate, ratm, r1, r2, r3, tdur, tx
REAL    currentTime, nextUpdate
LOGICAL lMC

CHARACTER(32) relName

REAL, DIMENSION(:), ALLOCATABLE :: rMC

INTEGER, EXTERNAL :: ReadNextEmission, FindRelName

UpdateStackEmission = FAILURE

!IF( .NOT.lEmissionFile )THEN  !*** This routine is called for PRIME sources even w/o emissions file
!  UpdateStackEmission = SUCCESS
!  GOTO 9999
!END IF

lMC = PRESENT(updateMC)  !Check for multicomponent release

IF( lMC )THEN

  IF( PRESENT(update) )GOTO 9999
  IF( nMCup /= nMC    )GOTO 9999
  relName = TRIM(updateMC%release%relName)
  releaseType = updateMC%release%type
  currentTime = updateMC%currentTime
  nextUpdate  = updateMC%nextUpdate
  nMCx = nMC

ELSE

  IF( .NOT.PRESENT(update) )GOTO 9999
  relName = TRIM(update%release%relName)
  releaseType = update%release%type
  currentTime = update%currentTime
  nextUpdate  = update%nextUpdate
  nMCx = 0

END IF

IF( .NOT.lEmissionFile .AND. releaseType /= HR_PRIME )THEN
  UpdateStackEmission = SUCCESS
  GOTO 9999
END IF

i = INDEX(relName,':')
IF( i > 0 )relName = TRIM(relName(1:i-1))

i = FindRelName( relName )

IF( i == 0 )GOTO 9999


IF( nMCx > 0 )THEN
  ALLOCATE( rMC(nMCx),STAT=irv )
  IF( irv /= 0 )GOTO 9999
END IF

IF( lIntrpEmission )THEN

  t = 0.5*(currentTime+nextUpdate)

  IF( t > tNextEmi .AND. lReadEmission )THEN
    irv = ReadNextEmission( t )
    IF( irv /= SUCCESS )GOTO 9999
  END IF

  IF( lReadEmission )THEN
    rate = (t-tEmi)/(tNextEmi-tEmi)
    rate = MAX(MIN(rate,1.),0.)
    ratm = 1.-rate
    r1 = ratm*emiRate1(i) + rate*emiRate2(i)
    r2 = ratm*emiTemp1(i) + rate*emiTemp2(i)
    r3 = ratm*emiVel1(i)  + rate*emiVel2(i)
    IF( nMCx > 0 )THEN
      rMC = ratm*emiMC1(:,i) + rate*emiMC2(:,i)
    END IF
  ELSE
    r1 = emiRate2(i)
    r2 = emiTemp2(i)
    r3 = emiVel2(i)
    IF( nMCx > 0 )THEN
      rMC = emiMC2(:,i)
    END IF
  END IF

ELSE

  t = currentTime

  IF( t >= tNextEmi .AND. lReadEmission )THEN
    irv = ReadNextEmission( t )
    IF( irv /= SUCCESS )GOTO 9999
  END IF

  IF( lReadEmission )THEN
    tdur = 1.            !Emissions persist for 1 hour
  ELSE
    tdur = 365.*24.*10.  !Only here for PRIME source
  END IF

  IF( t < tEmi + tdur )THEN
    r1 = emiRate1(i)
    r2 = emiTemp1(i)
    r3 = emiVel1(i)
    IF( nMCx > 0 )THEN
      rMC = emiMC1(:,i)
    END IF
  ELSE
    r1 = 0.
    r2 = 300.  !Set small but physical velocity and temperature
    r3 = 0.01
    IF( nMCx > 0 )THEN
      rMC = 0.
    END IF
  END IF

END IF

IF( nextUpdate > tNextEmi )THEN !relList%tNext
  tx =  tNextEmi !relList%tNext  !Only set tNext if next SCIPUFF update time is beyond next list time
ELSE
  tx = DEF_VAL_R
END IF

SELECT CASE(releaseType )
  CASE( HR_STACK,HR_PRIME,HR_STACKF,HR_STACKFP )
    IF( lMC )THEN
      relStackData = TRANSFER(updateMC%release%relData,relStackData)
    ELSE
      relStackData = TRANSFER(update%release%relData,relStackData)
    END IF
    relStackData%rate    = r1
    relStackData%exitVel = r3
    IF( iAreaSrc(i,1) > 0 )THEN
      relStackData%exitTemp = DEF_VAL_R
    ELSE
      relStackData%exitTemp = r2 + ABSZERO
    END IF
    relStackData%nextUpdtTime = tx
    IF( lMC )THEN
      updateMC%release%relData = TRANSFER(relStackData,updateMC%release%relData)
    ELSE
      update%release%relData = TRANSFER(relStackData,update%release%relData)
    END IF
  CASE( HR_AREAF )
    IF( lMC )THEN
      relAreaData = TRANSFER(updateMC%release%relData,relAreaData)
    ELSE
      relAreaData = TRANSFER(update%release%relData,relAreaData)
    END IF
    relAreaData%rate      = r1
    relStackData%exitTemp = DEF_VAL_R
    relStackData%nextUpdtTime = tx
    IF( lMC )THEN
      updateMC%release%relData = TRANSFER(relAreaData,updateMC%release%relData)
    ELSE
      update%release%relData = TRANSFER(relAreaData,update%release%relData)
    END IF
  CASE( HR_CONT,HR_CONTF )
    IF( lMC )THEN
      relContData  = TRANSFER(updateMC%release%relData,relContData)
    ELSE
      relContData  = TRANSFER(update%release%relData,relContData)
    END IF
    relContData%rate     = r1
    relContData%buoyancy = r2
    relContData%momentum = r3
    relContData%nextUpdtTime = tx
    IF( lMC )THEN
      updateMC%release%relData = TRANSFER(relContData,updateMC%release%relData)
    ELSE
      update%release%relData = TRANSFER(relContData,update%release%relData)
    END IF
END SELECT

IF( nMCx > 0 )THEN
  DO j = 1,nMCx
    relMC(j)%relID  = 1 !Always 1 release only
    relMC(j)%MCname = MCname(j)
    relMC(j)%MCmass = rMC(j)
  END DO
END IF

UpdateStackEmission = SUCCESS

9999 CONTINUE

IF( ALLOCATED(rMC) )DEALLOCATE( rMC,STAT=irv )

RETURN
END
