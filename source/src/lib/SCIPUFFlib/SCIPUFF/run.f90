SUBROUTINE run( lcontinue,istep )

USE scipuff_fi
USE met_fi
USE SWIMparam_fd
USE SCIPresults_fd
USE cont_rel_functions

!   Performs one complete timestep for SCIPUFF

IMPLICIT NONE

LOGICAL, INTENT( OUT   ) :: lcontinue
INTEGER, INTENT( INOUT ) :: istep

INTEGER irv
LOGICAL do_output

CHARACTER(80) cmsg,cmsg2,cmsg3

INTEGER, EXTERNAL :: SWIMupdateMet, SWIMoutput
INTEGER, EXTERNAL :: SetMetGrid

CALL write_progress_bar( 0 )

CALL check_progress()
IF( nError /= NO_ERROR )GOTO 9999

!------ Update meteorological fields if required

irv = SWIMupdateMet( t,SWIMstatus )
CALL enableSCIPUFFhalt( istop )      !Reset buttons
IF( irv /= SWIMsuccess )THEN
  CALL setSWIMerror( 'SWIMupdateMet' )
  GOTO 9999
END IF

CALL WriteSWIMlog()

IF( BTEST(SWIMstatus,SSB_EXPAND) )THEN
  irv = SetMetGrid()
  IF( irv /= SCIPsuccess )GOTO 9999
END IF

!------ synchronize time stepping with calling program

CALL synchronize( t,t+delt )
IF( nError /= NO_ERROR )GOTO 9999

!------ output met fields

IF( lout_met )THEN

  IF( lout_mc )THEN
    do_output =  BTEST(SWIMstatus,SSB_NEWMET)
  ELSE
    do_output = MOD(INT(t/delt+0.4999),nout_met) == 0
  END IF
  IF( do_output )THEN
    IF( t > timeOutMet )THEN
      cmsg  = 'Outputting wind field'
      cmsg2 = ' '
      cmsg3 = ' '
      CALL write_progress( cmsg,cmsg2,cmsg3 )
      IF( nError /= NO_ERROR )GOTO 9999

      irv = SWIMoutput()
      IF( irv /= SWIMsuccess )THEN
        CALL setSWIMerror( 'SWIMoutput' )
        GOTO 9999
      END IF
      timeOutMet = t

      CALL WriteSWIMlog()

      IF( nError /= NO_ERROR )GOTO 9999
      IF( lout_mc )nout_met = -1
    END IF
  END IF

END IF

CALL check_progress()
IF( nError /= NO_ERROR )GOTO 9999

!------ Delete existing static puffs for rebuilding with new met

IF( static )CALL reset_static_puffs()

!------ Release puffs

CALL puff_release()
IF( nError /= NO_ERROR )GOTO 9999

!------ decay surface fields

IF( ldecay .AND. surface )THEN
  CALL srf_decay()
  IF( nError /= NO_ERROR )GOTO 9999
END IF

!------ Step puffs

CALL step()
IF( nError /= NO_ERROR )GOTO 9999

istep = istep + 1

!------ Output any SWIM log messages

CALL WriteSWIMlog()

CALL check_progress()
IF( nError /= NO_ERROR )GOTO 9999

!------ Set flag for continue run

lcontinue = (npuf < MAXPUF) .AND. ((istop == 0).OR.(istop == 2))
IF( lcontinue )THEN
  lcontinue = (npuf>0) .OR. ActiveSource .OR. countDefinitions()>0
END IF

9999 CONTINUE

RETURN
END
