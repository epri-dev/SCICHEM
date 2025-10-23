!==============================================================================
!  SWIM halt functions
!==============================================================================

INTEGER FUNCTION setSWIMhalt( mode ) RESULT( handledStop )

!DEC$ ATTRIBUTES DLLEXPORT :: setSWIMhalt

USE SWIM_fi
USE SWIMparam_fd
USE basic_fd

IMPLICIT NONE

INTEGER, INTENT( IN ) :: mode


handledStop = FALSE

!==== Check to see if this is for SWIFT to handle

!IF( AvailSWIFT .AND. setSWIFThalt( mode ) == TRUE )THEN
!  GOTO 9999

!==== Check to see if this is for UWM to handle

IF( SWIMinProgress )THEN

!==== Check to see if this is for SWIM to handle

!==== Check mode
!     Abort - Set error

  IF( mode < 0 )THEN

    error%Number  = AB_ERROR
    error%Routine = 'SWIMhalt'
    error%Message = 'User requested abort detected by SWIM'
    error%Inform  = 'SWIM aborted'
    SWIMresult    = SWIMfailure
    handledStop = TRUE

!     Stop - Save for later checking

  ELSE

    IF( SWIMiteration )THEN
      StopMode = mode
      CALL enableSWIMhalt( StopMode )
      handledStop = TRUE
    END IF

  END IF

END IF

9999 CONTINUE

RETURN
END
!***********************************************************************
!                enableSWIMhalt
!***********************************************************************
SUBROUTINE enableSWIMhalt( iflag )

USE basic_fd
USE SCIPresults_fd
USE message_fd
USE SWIMparam_fd

!     GUI Callback to enable buttons in Progress Box

IMPLICIT NONE

INTEGER iflag

INTEGER irv
INTEGER ButtonState

INTEGER, EXTERNAL :: PostButtonStateMessage
INTEGER, EXTERNAL :: PostButtonLabelMessage
INTEGER, EXTERNAL :: PostWaitMessage

TYPE( messageT ) message

!==== Set Button STATIC labels

!==== Button identifier : (bit 0 (Left), 1 (Middle), 2 (Right))

message%iParm = 5 !Bits 0 and 2 -> Left and Right

!==== Tag label :  FALSE->Buttons , TRUE->Static labels

message%jParm = TRUE

!==== Labels
!       iflag < 0 -> Mass-consistent iterations
!       iflag > 0 -> SCIPUFF step

message%routine  = CHAR(0)
message%aString  = 'Next Iteration'
message%bString  =  CHAR(0)
message%cString  = 'Calculation'

!==== Set Button State
!       Show(set)/Hide(clear)      : (bit 0 (Left), 1 (Middle), 2 (Right))
!       Enable(set)/Disable(clear) : (bit 3 (Left), 4 (Middle), 5 (Right))
!                                         9        18          36
SELECT CASE( iflag )
  CASE( SWIM_ENABLE )
    ButtonState = 45 !Enable/Show Left and Right
  CASE( SWIM_HALT )
    ButtonState = 36 !Enable/Show Right
  CASE DEFAULT
    ButtonState = 0 !None
END SELECT

!==== Make Callback

irv =  PostButtonStateMessage( ButtonState )

!==== Make Callback

irv =  PostButtonLabelMessage( message )


!==== If showing any button - make a HM_RELEASEWAIT call

IF( ButtonState /= 0 )THEN

!==== Make Callback

  irv =  PostWaitMessage( FALSE )

END IF

RETURN
END
!===============================================================================

SUBROUTINE SWIMtimeMessage( cmsg,tx )

USE SWIM_fi

IMPLICIT NONE

REAL,         INTENT( IN  ) :: tx
CHARACTER(*), INTENT( OUT ) :: cmsg

CHARACTER(32) ctem

LOGICAL lymd_out

INTEGER hour, min, sec, year, month, day
INTEGER nch, nch1

lymd_out  = Prj%julStart /= 0 .AND. Prj%julStart /= NOT_SET_I
CALL TimeConvert( tx,Prj%localMet,lymd_out,hour,min,sec,year,month,day,ctem )

cmsg = ctem
nch1 = LEN_TRIM(cmsg)

IF( tx == 0. .OR. ABS(tx) > 1800. )THEN
  CALL c_format( tx/3600.,nch,ctem )
  WRITE(cmsg,'(A)') cmsg(1:nch1)//' ('//ctem(1:nch)//' hr)'
ELSE IF( ABS(tx) > 60. )THEN
  CALL c_format( tx/60.,nch,ctem )
  WRITE(cmsg,'(A)') cmsg(1:nch1)//' ('//ctem(1:nch)//' min)'
ELSE
  CALL c_format( tx,nch,ctem )
  WRITE(cmsg,'(A)') cmsg(1:nch1)//' ('//ctem(1:nch)//' sec)'
END IF

RETURN
END
