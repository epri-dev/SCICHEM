SUBROUTINE do_plot( lextra,lrmrk,lskip )

USE contri_fi
USE files_fi
USE pltchoice_fi
USE SCIPtool
USE GUIparam_fd

IMPLICIT NONE

LOGICAL lextra,lrmrk,lskip

LOGICAL cancel_print
LOGICAL lwnd,lno_plot,lfirst
INTEGER UserID

EXTERNAL CheckFill,CheckDraw

lno_plot = .FALSE.

UserID = 2468

IF( cancel_print(lprint) )RETURN

!------ Set plot window

CALL set_plot( lwnd )

!------ Load Palette

CALL loadpal()
CALL setpal( lprint )
CALL ClearBrush()
CALL ClearPen()
CALL gsln( 0 )
CALL gsplci( 1 )
IF( lno_plot )GOTO 9998

IF( cancel_print(lprint) )RETURN

!---- Use SCIPtool to generate plot

lfirst = .TRUE.

IF( .NOT.lskip )CALL draw_plot()

IF( lavs_cts )GOTO 9000

9998	CONTINUE

IF( lbck )THEN
  CALL draw_back( lno_plot )
  IF( cancel_print(lprint) )RETURN
END IF

!------ Draw map

IF( lmapon .AND. lextra )THEN
  CALL draw_map()
  IF( cancel_print(lprint) )RETURN
END IF

!------ Draw points

IF( lpts .AND. lextra )THEN
  CALL draw_source( PlotDef(BASE_LEVEL)%Field%userTime, lpts_txt )
  IF( cancel_print(lprint) )RETURN
END IF

!------ Draw Weather

IF( lwxs .AND. lextra )THEN
  CALL draw_weather( tima )
  IF( cancel_print(lprint) )RETURN
END IF

!------ Draw overlay

IF( lovly .AND. lextra )CALL draw_overlay()

!------ Draw registration marks for bare plots

IF( lrmrk .AND. lextra )CALL draw_registration()

9000	CONTINUE

RETURN

5999	FORMAT(' ',72('='))
7000	FORMAT(' ',A)
6000	FORMAT(' Project    : ',A)
6003	FORMAT(' Path       : ',A)
6001	FORMAT(' Time       : ',1PE15.4)
6002	FORMAT(' No. Cells  : ',I6)
6100	FORMAT(' Variable   : ',A)
6101	FORMAT(' Aux. Var   : ',A)
6200	FORMAT(' Function   : ',A)
6300	FORMAT(' Data Range : ',1P2E15.5)
6301	FORMAT(' Aux. Range : ',1P2E15.5)
6302	FORMAT(' Func Range : ',1P2E15.5)
6400	FORMAT(' Max Levels : ',2I5)
6500	FORMAT(' Data Domain: ',1P4E15.5)
6600	FORMAT(' Plot Domain: ',1P4E15.5)
6700	FORMAT(' Viewport   : ',4F15.4)
6800	FORMAT(' Data Limits: ',1P2E15.5)
6900	FORMAT(' Contours   : ',1P2E15.5,I4)

END
