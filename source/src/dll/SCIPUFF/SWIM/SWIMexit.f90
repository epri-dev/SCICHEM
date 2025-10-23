INTEGER FUNCTION SWIMexitRun()

!DEC$ ATTRIBUTES DLLEXPORT :: SWIMexitRun

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

INTEGER i, alloc_stat, irv, ios

TYPE ( messageT ) :: caution

INTERFACE

  SUBROUTINE SWIMclearObsList( Obs )
    USE SWIMobs_fd
    TYPE( ObsMet ), POINTER :: Obs
  END SUBROUTINE SWIMclearObsList

  SUBROUTINE SWIMclearGridList( First )
    USE SWIMobs_fd
    TYPE( FirstObsGridList ), POINTER :: First
  END SUBROUTINE SWIMclearGridList

END INTERFACE

INTEGER, EXTERNAL :: SWIMdealloc3dField
INTEGER, EXTERNAL :: SWIMdeallocBLParam, SWIMdeallocBLaux
INTEGER, EXTERNAL :: SWIMdeallocObsWt, SWIMdeallocVariance
INTEGER, EXTERNAL :: SWIMdeallocGrid, SWIMdeallocGridSrc
INTEGER, EXTERNAL :: SWIMclearLogMessage, ClearStndAtmos
INTEGER, EXTERNAL :: SWIMdeallocQLprof, SWIMdeallocIntrpFac
INTEGER, EXTERNAL :: ExitLandUse
INTEGER, EXTERNAL :: PostCautionMessage

!------ Clear Prj mass-consistent array

IF( ASSOCIATED(Prj%MC%Z) )DEALLOCATE(Prj%MC%Z,STAT=irv)

!------ Deallocate Obs lists and close files

IF( ALLOCATED(ObsSrc) )THEN
  DO i = 1,numObsSrc
    IF( ObsSrc(i)%lASOS1 )THEN
      IF( ObsSrc(i)%nASOS1repeat > 0 )THEN
        caution%iParm = 0; caution%jParm = 0; caution%routine = ''
        WRITE(caution%bString,'(I8)',IOSTAT=ios) ObsSrc(i)%nASOS1repeat
        caution%bString = TRIM(ADJUSTL(caution%bString) )//' duplicate lines skipped on ASOS 1 minute data file '
        CALL ReportFileName( caution%aString,TRIM(caution%bString),ObsSrc(i)%Source )
        caution%bString = 'First occurence: '//TRIM(ADJUSTL(ObsSrc(i)%errorLine1))
        caution%cString = 'Last  occurence: '//TRIM(ADJUSTL(ObsSrc(i)%errorLine2))
        irv = PostCautionMessage( caution )
      END IF
    END IF
    IF( ASSOCIATED(ObsSrc(i)%PrevObs)      )CALL SWIMclearObsList(  ObsSrc(i)%PrevObs )
    IF( ASSOCIATED(ObsSrc(i)%Obs)          )CALL SWIMclearObsList(  ObsSrc(i)%Obs )
    IF( ASSOCIATED(ObsSrc(i)%PrevGridList) )CALL SWIMclearGridList( ObsSrc(i)%PrevGridList )
    IF( ASSOCIATED(ObsSrc(i)%GridList)     )CALL SWIMclearGridList( ObsSrc(i)%GridList )
    IF( ASSOCIATED(ObsSrc(i)%ObsArray)     )DEALLOCATE(ObsSrc(i)%ObsArray,STAT=irv)
    IF( ASSOCIATED(ObsSrc(i)%VarID)        )DEALLOCATE(ObsSrc(i)%VarID,   STAT=irv)
    IF( ASSOCIATED(ObsSrc(i)%Conv)         )DEALLOCATE(ObsSrc(i)%Conv,    STAT=irv)
    IF( ASSOCIATED(ObsSrc(i)%z)            )DEALLOCATE(ObsSrc(i)%z,       STAT=irv)
    CLOSE(ObsSrc(i)%unit,IOSTAT=irv)
  END DO
  DEALLOCATE( ObsSrc,STAT=alloc_stat )
  numObsSrc = 0
END IF

!------ Clear standard atmosphere arrays

irv = ClearStndAtmos()

!------ Deallocate fields and associated arrays

IF( ALLOCATED(field) )THEN
  DO i = 1,numField
    irv = SWIMdealloc3dField( field(i)%Field )
    irv = SWIMdealloc3dField( field(i)%NextField )
    irv = SWIMdeallocBLParam( field(i)%BL )
    irv = SWIMdeallocBLParam( field(i)%NextBL )
    irv = SWIMdeallocBLaux( field(i)%BLaux )
    irv = SWIMdeallocObsWt( field(i)%obsWt )
    irv = SWIMdeallocGrid( field(i)%grid )
    irv = SWIMdeallocGridSrc( field(i)%gridSource )
    irv = SWIMdeallocVariance( field(i)%LSV )
    irv = SWIMdeallocVariance( field(i)%NextLSV )
    irv = SWIMdealloc3dField( field(i)%Field1 )
    irv = SWIMdealloc3dField( field(i)%Field2 )
    irv = SWIMdeallocBLParam( field(i)%BL1 )
    irv = SWIMdeallocBLParam( field(i)%BL2 )
    irv = SWIMdeallocVariance( field(i)%LSV1 )
    irv = SWIMdeallocVariance( field(i)%LSV2 )
    IF( Prj%lOut2D .OR. Prj%lOut3D )THEN
      IF( field(i)%unitOut > 0 )CLOSE(field(i)%unitOut,IOSTAT=ios)
    END IF
    IF( BTEST(field(i)%grid%type,GTB_LANDUSE) .OR. &
       (Prj%BL%i_cat /= NOT_SET_I .AND. Prj%BL%i_cat /= 0) )irv = ExitLandUse()
  END DO
  DEALLOCATE( field,STAT=alloc_stat )
  numField    = 0
  numFieldMax = 0
END IF

!------ Clear error string

CALL SWIMclearError()

CALL SWIMResetMessaging()

SWIMexitRun = SWIMresult

RETURN
END

!==============================================================================

INTEGER FUNCTION SWIMexit()

!DEC$ ATTRIBUTES DLLEXPORT :: SWIMexit

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE


SWIMexit = SWIMresult

RETURN
END
