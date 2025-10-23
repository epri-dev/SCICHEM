SUBROUTINE RunAERMODInp( ToolUserID,restart,error )

USE SCIPUFFdriver_fi
USE SCIPtool
#if !defined(LINUX)
USE swinAPI
#endif
USE sampler_fi, ONLY: MAXSMP

IMPLICIT NONE

INTEGER irv, alloc_stat
INTEGER ToolUserID

LOGICAL restart

TYPE( messageT )   error

INTEGER, EXTERNAL :: InitAERMOD, InitSCIPUFFProject, ReadAERMODInput
INTEGER, EXTERNAL :: GenSCIPUFFWeather, GenSCIPUFFSensor, GetTimeFromWeather
INTEGER, EXTERNAL :: GenSCIPUFFAreaSource, SetSCIPUFFDomain, SetEmissions
INTEGER, EXTERNAL :: SetMCrelease

CHARACTER(128), EXTERNAL :: AddExtension
CHARACTER(128), EXTERNAL :: StripNull,AddNull
INTEGER,        EXTERNAL :: SYSDELETEFILE, SYSMOVEFILE

!------ Initialize SCIPUFF structures

new%project%ID      = 0
new%project%version = 0
new%project%name    = TRIM(prjname)
new%project%path    = ''

irv = InitSCIPUFFProject( ToolUserID )
IF( irv /= SUCCESS )GOTO 9999

!------ Initialize special AERMOD data

irv = InitAERMOD()
IF( irv /= SUCCESS )GOTO 9999

!------ Read AERMOD input files; setup SCIPUFF structures

irv = ReadAERMODInput()
IF( irv /= SUCCESS )GOTO 9999

!------ Setup multicomponent releases

irv = SetMCrelease()
IF( irv /= SUCCESS )GOTO 9999

!------ Generate SCIPUFF area sources (if any)

irv = GenSCIPUFFAreaSource()
IF( irv /= SUCCESS )GOTO 9999

!------ Generate SCIPUFF weather input

irv = GenSCIPUFFWeather()
IF( irv /= SUCCESS )GOTO 9999

IF( .NOT.lTime )THEN
  irv = GetTimeFromWeather()
  IF( irv /= SUCCESS )GOTO 9999
END IF

!------ Generate SCIPUFF sensors from AERMOD receptors

irv = GenSCIPUFFSensor()
IF( irv /= SUCCESS )GOTO 9999

IF( nsamp > MAXSMP )THEN
  error%iParm   = 999
  error%routine = 'RunAERMODInp'
  WRITE(error%aString,'("Number of samplers exceed the maximum allowed value of ",I3)')MAXSMP
  error%bString = "Use postprocessor to sample the integrated concentration output field"
  GOTO  9999
END IF

!------ Set SCIPUFF domain to cover area of receptors

irv = SetSCIPUFFDomain()
IF( irv /= SUCCESS )GOTO 9999

!------ Setup for hourly emissions if appropriate

IF( LEN_TRIM(emiFile) > 1 .OR. LEN_TRIM(primeFile) > 1 )THEN
  irv = SetEmissions()
  IF( irv /= SUCCESS )GOTO 9999
ELSE
  lEmissionFile = .FALSE.
  lReadEmission = .FALSE.
END IF

!**************************************************************
new%input%flags%mode = IBSET(new%input%flags%mode,HFB_DINCRMNT)
!**************************************************************

!------ Create SCIPUFF project

IF( .NOT.restart )THEN

  CALL ComputeDurationAERMOD( new%input%time%start%time,new%input%time%end%time )

  IF( new%input%time%start%zone == NOT_SET_R )CALL SetTimeZone()

  CALL ReportDefaults() !Report default values used to defined project

  IF( nMCrel > 0 )THEN
    irv = SCIPNewProjectMC( ToolUserID,new,mtlList,relList,nMCrel,relMCList )
  ELSE
    irv = SCIPNewProject( ToolUserID,new,mtlList,relList )
  END IF
  IF( irv == SCIPfailure )THEN
    irv = SCIPGetLastError( error )
    IF( irv == SCIPfailure )THEN
      error%iParm   = 999
      error%routine = 'SCIPNewProject'
      error%aString = 'Failed to create Project'
      error%bString = 'Unknown error - SCIPGetLastError failure'
    ELSE
      WRITE(*,*) TRIM(error%routine)
      WRITE(*,*) TRIM(error%aString)
      WRITE(*,*) TRIM(error%bString)
      WRITE(*,*) TRIM(error%cString)
    END IF
   GOTO 9999
  END IF

  IF( lSetupOnly )GOTO 9999

END IF

!----- Run project

run%project = new%project
run%end     = new%input%time%end

WRITE(6,*)
WRITE(6,*)'Running '//TRIM(prjname)

irv = SCIPRunProject( ToolUserID,run )
IF( irv == SCIPfailure )THEN
  irv = SCIPGetLastError( error )
  IF( irv == SCIPfailure )THEN
    error%iParm   = 999
    error%routine = 'SCIPRunProject'
    error%aString = 'Failed to run Project'
    error%bString = 'Unknown error - SCIPGetLastError failure'
    error%cString = ' '
  ELSE
    WRITE(*,'(A)') TRIM(error%routine)
    WRITE(*,'(A)') TRIM(error%aString)
    WRITE(*,'(A)') TRIM(error%bString)
    WRITE(*,'(A)') TRIM(error%cString)
  END IF
  GOTO 9999
END IF

9999 CONTINUE

IF( ALLOCATED(gasMatl)  )DEALLOCATE(gasMatl, STAT=alloc_stat)
IF( ALLOCATED(partMatl) )DEALLOCATE(partMatl, STAT=alloc_stat)
IF( ALLOCATED(emiRate1) )DEALLOCATE(emiRate1,STAT=alloc_stat)
IF( ALLOCATED(emiTemp1) )DEALLOCATE(emiTemp1,STAT=alloc_stat)
IF( ALLOCATED(emiVel1)  )DEALLOCATE(emiVel1, STAT=alloc_stat)
IF( ALLOCATED(emiRate2) )DEALLOCATE(emiRate2,STAT=alloc_stat)
IF( ALLOCATED(emiTemp2) )DEALLOCATE(emiTemp2,STAT=alloc_stat)
IF( ALLOCATED(emiVel2)  )DEALLOCATE(emiVel2, STAT=alloc_stat)
IF( ALLOCATED(iAreaSrc) )DEALLOCATE(iAreaSrc,STAT=alloc_stat)
IF( ALLOCATED(MCname)   )DEALLOCATE(MCname,  STAT=alloc_stat)
IF( ALLOCATED(MCrate)   )DEALLOCATE(MCrate,  STAT=alloc_stat)
IF( ALLOCATED(emiMC1)   )DEALLOCATE(emiMC1,  STAT=alloc_stat)
IF( ALLOCATED(emiMC2)   )DEALLOCATE(emiMC2,  STAT=alloc_stat)

RETURN
END

!==============================================================================

SUBROUTINE ReportDefaults()

USE SCIPUFFdriver_fi
USE SCIPtool

INTEGER ios

WRITE(*,'(/,A)',IOSTAT=ios) '********** Report Default Settings **************'

IF( .NOT.lSetTzone )THEN
  WRITE(*,'(A)',IOSTAT=ios) 'Time zone not explicitly set'
  IF( new%input%time%start%zone /= NOT_SET_R )THEN
    WRITE(*,'(A,F6.2)',IOSTAT=ios) 'Assumed value from weather input or longitude'
  END IF
END IF

IF( .NOT.lSetPrjTcnv )THEN
  IF( new%input%time%start%time%reference == HT_LOCAL )THEN
    WRITE(*,'(A)',IOSTAT=ios) 'Time convention = LOCAL'
  ELSE
    WRITE(*,'(A)',IOSTAT=ios) 'Time convention = UTC'
  END IF
  WRITE(*,'(A,F6.2)',IOSTAT=ios) 'Time zone = ',new%input%time%start%zone
END IF

IF( .NOT.lSetMetTcnv )THEN
  IF( new%weather%flags%reference == HT_LOCAL )THEN
    WRITE(*,'(A)',IOSTAT=ios) 'Weather input time convention = LOCAL'
  ELSE
    WRITE(*,'(A)',IOSTAT=ios) 'Weather input convention = UTC'
  END IF
END IF

IF( .NOT.lSetDT )THEN
  WRITE(*,'(A,F8.2)',IOSTAT=ios) 'Time step (s) = ',new%input%time%end%step%max
END IF

IF( .NOT.lSetDTout )THEN
  WRITE(*,'(A,F8.2)',IOSTAT=ios) 'Output Interval (h) = ',new%input%time%end%step%output
END IF

IF( .NOT.lSetTimeAvg )THEN
  WRITE(*,'(A,F8.2)',IOSTAT=ios) 'Time-averaging (s) = ',new%input%option%timeAvg
END IF

IF( .NOT.lSetZmax )THEN
  WRITE(*,'(A,F8.2)',IOSTAT=ios) 'Domain top (m) = ',new%input%domain%domain%zMax
END IF

IF( .NOT.lSetLSV )THEN
  SELECT CASE( new%weather%lsv%type )
    CASE( HL_LSVOPER )
      WRITE(*,'(A)',IOSTAT=ios) 'Large-scale variability = OPERATIONAL'
    CASE( HL_LSVMOD )
      WRITE(*,'(A)',IOSTAT=ios) 'Large-scale variability = MODEL'
    CASE( HL_LSVINP )
      WRITE(*,'(A)',IOSTAT=ios) 'Large-scale variability = INPUT'
    CASE( HL_NONE )
      WRITE(*,'(A)',IOSTAT=ios) 'Large-scale variability = OFF'
  END SELECT
END IF

IF( .NOT.lSetTimeBin )THEN
  WRITE(*,'(A,F8.2)',IOSTAT=ios) 'Weather observation time bin (s) = ', new%weather%met%timeBin
END IF

IF( .NOT.lSetUUcalm )THEN
  WRITE(*,'(A,F8.2)',IOSTAT=ios) 'Calm wind variance (m^2/s^2) = ', new%input%option%UUcalm
END IF

IF( .NOT.lSetSLcalm )THEN
  WRITE(*,'(A,F8.2)',IOSTAT=ios) 'Calm wind length scale (m) = ', new%input%option%slCalm
END IF

WRITE(*,'(A)',IOSTAT=ios) '********** End Default Settings *****************'

RETURN
END
