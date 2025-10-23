!*******************************************************************************
!                draw_plot
!*******************************************************************************
SUBROUTINE draw_plot()

USE contri_fi
USE pltchoice_fi
USE tooluser_fd
USE SCIPtool
USE GUIparam_fd
USE files_fi
USE GUIerror_fi
USE contour_fd

IMPLICIT NONE

!==============================================================================
! Local variables
!==============================================================================
INTEGER                        :: i
INTEGER                        :: irv
INTEGER                        :: ncom
INTEGER                        :: UserID
INTEGER                        :: mode
TYPE( char128T ),DIMENSION(5)  :: comment

!TYPE( SCIPLineT ), DIMENSION(:), ALLOCATABLE :: Lines
!TYPE( SCIPPointT ), DIMENSION(:), ALLOCATABLE :: Points
!INTEGER cMode,nLines,nPoints,ios,nn
!REAL, DIMENSION(:), ALLOCATABLE :: x,xp,y,yp
!INTEGER, EXTERNAL :: PlotTransform

!TYPE( ppuffHeadT )phead
!TYPE( puffHeadT )head
!TYPE( puffT ), DIMENSION(:), ALLOCATABLE :: puffs
!TYPE( puffTypeT ), DIMENSION(:), ALLOCATABLE :: types
!REAL, DIMENSION(:), ALLOCATABLE :: aux
!INTEGER ios

!==============================================================================
! Function calls
!==============================================================================
LOGICAL  cancel_print
INTEGER, EXTERNAL :: CheckFill
INTEGER, EXTERNAL :: CheckDraw

!==============================================================================
! Check for user CANCEL print
!==============================================================================
IF( cancel_print(lprint) )GOTO 9999

UserID = 9876

!==============================================================================
! Draw Field
!==============================================================================
IF( .NOT.lavs_cts )THEN

  ipen = 1
  irv = SCIPDrawField( UserID,FieldID(FLD_INDEX),PlotDef(BASE_LEVEL)%Field,DrawType, &
                       ContourPlot%ListHdr,ContourPlot%ListPtr,PlotDraw,CheckFill,CheckDraw )
  IF( irv /= SCIPsuccess )THEN
    irv = SCIPGetLastError(error)
    WRITE(11,*)'*****************************'
    WRITE(11,*)'*****************************'
    WRITE(11,*)'Error in SCIPDrawField'
    WRITE(11,*) TRIM(error%routine)
    WRITE(11,*) TRIM(error%aString)
    WRITE(11,*) TRIM(error%bString)
    WRITE(11,*) TRIM(error%cString)
    WRITE(11,*)'*****************************'
    WRITE(11,*)'*****************************'
    GOTO 9999
  END IF

  !cMode = CLOSE_CONTOUR
  !irv = SCIPContourCount( UserID,FieldID(FLD_INDEX),PlotDef(BASE_LEVEL)%Field,DrawType, &
  !                        ContourPlot%ListHdr,ContourPlot%ListPtr,cMode,nLines,nPoints )
  !IF( nLines > 0 .AND. nPoints > 0 .AND. irv == SCIPsuccess )THEN
  !  ALLOCATE( Lines(nLines),Points(nPoints),STAT=ios )
  !  IF( ios == 0 )THEN
  !    irv = SCIPContourField( UserID,FieldID(FLD_INDEX),PlotDef(BASE_LEVEL)%Field,DrawType, &
  !                            ContourPlot%ListHdr,ContourPlot%ListPtr,cMode,Lines,Points )
  !    DO irv = 1,nLines
  !      nn = Lines(irv)%Number
  !      ALLOCATE( x(nn),xp(nn),y(nn),yp(nn) )
  !      DO i = 1,nn
  !        x(i) = Points(i+Lines(irv)%Start-1)%x
  !        y(i) = Points(i+Lines(irv)%Start-1)%y
  !      END DO
  !      ios = PlotTransform( nn,x,y,xp,yp )
  !      CALL gsplci(1)
  !      CALL gpl(nn,xp,yp)
  !      SELECT CASE( lines(irv)%Mode )
  !        CASE( HP_RIGHTHAND )
  !          CALL plchhq( xp(1),yp(1),CHAR(irv+48)//'R1',0.6*fsz,0.,0. )
  !          CALL plchhq( xp(nn/3),yp(nn/3),'R2',0.6*fsz,0.,0. )
  !        CASE( HP_LEFTHAND )
  !          CALL plchhq( xp(1),yp(1),'L1',0.6*fsz,0.,0. )
  !          CALL plchhq( xp(nn/3),yp(nn/3),'L2',0.6*fsz,0.,0. )
  !        CASE DEFAULT
  !          call plchhq( xp(1),yp(1),'X',0.6*fsz,0.,0. )
  !      END SELECT
  !      DEALLOCATE( x,xp,y,yp )
  !    END DO
  !    DEALLOCATE( Lines,Points,STAT=ios )
  !  END IF
  !END IF

 ! CALL SplitName( file_inp,phead%project%name,phead%project%path )
 ! CALL RemoveExtension( phead%project%name )
 ! irv = SCIPGetProjectPuffHeader(UserID,phead,10)
 ! head = phead%puff
 ! IF( head%nPuff > 0 .AND. head%nType > 0 .AND. head%nAux > 0 )THEN
 !   ALLOCATE( puffs(head%nPuff),STAT=ios )
 !   ALLOCATE( types(head%nType),STAT=ios )
 !   ALLOCATE( aux(head%nAux),STAT=ios )
 !   head%maxPuff = head%nPuff
 !   head%maxType = head%nType
 !   head%maxAux  = head%nAux
 !   phead%puff = head
 !   irv = SCIPGetProjectPuff(UserID,phead,10,SCIPtrue,puffs,aux,types)
 !   DEALLOCATE( puffs,STAT=ios )
 !  DEALLOCATE( types,STAT=ios )
 !  DEALLOCATE( aux,STAT=ios )
 !END IF

!==============================================================================
! Check for user CANCEL print
!==============================================================================
  IF( cancel_print(lprint) )GOTO 9999

!==============================================================================
! Draw Computational grid
!==============================================================================
  IF( lcel )THEN

    mode = 1 !Grid
!    mode = 2 !Triangles
!    mode = 3 !Both
    ipen = ipalln+10
!    ipen = ipalln+2
    irv = SCIPDrawGrid( UserID,FieldID(FLD_INDEX),CheckDraw,mode )
!    mode = 2 !Triangles
!    ipen = ipalln+3
!    irv = SCIPDrawGrid( UserID,FieldID(FLD_INDEX),CheckDraw,mode )
    IF( irv /= SCIPsuccess )THEN
      irv = SCIPGetLastError( error )
      WRITE(11,*)'*****************************'
      WRITE(11,*)'*****************************'
      WRITE(11,*)'Error in SCIPDrawGrid'
      WRITE(11,*) TRIM(error%routine)
      WRITE(11,*) TRIM(error%aString)
      WRITE(11,*) TRIM(error%bString)
      WRITE(11,*) TRIM(error%cString)
      WRITE(11,*)'*****************************'
      WRITE(11,*)'*****************************'
      GOTO 9999
    END IF

  END IF

!==============================================================================
! Uniform Population density - convert from Area to Population
!==============================================================================
  IF( popden > 0 .AND. popden /= default )THEN
    DO i = 1,ContourPlot%ListHdr%Number
      ContourPlot%ListPtr(i)%population = popden*ContourPlot%ListPtr(i)%area
      ContourPlot%ListPtr(i)%area = 0.0
    END DO
  END IF

!==============================================================================
! Export Field
!==============================================================================
ELSE

!==============================================================================
! Build header list - Plot title strings plus hazard area if applicable
!==============================================================================
  ncom = 0
  IF( ltt3 .AND. LEN_TRIM(ctt3) > 0 )THEN
    ncom = ncom + 1
    comment(ncom)%string = 'Field   Comment  : '//TRIM(ctt3)
  END IF
  IF( lttu .AND. LEN_TRIM(cttu) > 0 )THEN
    ncom = ncom + 1
    comment(ncom)%string = 'Field   Comment  : '//TRIM(cttu)
  END IF
  IF( lttl .AND. LEN_TRIM(cttl) > 0 )THEN
    ncom = ncom + 1
    comment(ncom)%string = 'Field   Comment  : '//TRIM(cttl)
  END IF
  IF( DrawType%Type > HP_NUMTYP )THEN
    ncom = ncom + 1
    comment(ncom)%string = 'Field   Comment  : Hazard Area(s)'
  END IF

!==============================================================================
! Write field to file
!==============================================================================
  irv = SCIPWriteField( UserID,FieldID(FLD_INDEX),PlotDef(BASE_LEVEL)%Field,DrawType, &
                        ContourPlot%ListHdr,ContourPlot%ListPtr,PlotWrite,ncom,comment )

  IF( irv /= SCIPsuccess )THEN
    irv = SCIPGetLastError( error )
    WRITE(11,*)'*****************************'
    WRITE(11,*)'*****************************'
    WRITE(11,*)'Error in SCIPWriteField'
    WRITE(11,*) TRIM(error%routine)
    WRITE(11,*) TRIM(error%aString)
    WRITE(11,*) TRIM(error%bString)
    WRITE(11,*) TRIM(error%cString)
    WRITE(11,*)'*****************************'
    WRITE(11,*)'*****************************'
    GOTO 9999
  END IF
END IF

9999 CONTINUE

!==============================================================================
! Reset Pen to black
!==============================================================================
CALL gsplci( 1 )

RETURN
END
!*******************************************************************************
!*******************************************************************************
!*******************************************************************************
!                CheckFill
!*******************************************************************************
INTEGER FUNCTION CheckFill( np,xp,yp,nlev )

USE sagdef_fd
USE sagerr_fd
USE ToolUser_fd
USE contri_fi

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
INTEGER,               INTENT( IN    ) :: np     !number of points
REAL,   DIMENSION(np), INTENT( IN    ) :: xp     !Input  X array
REAL,   DIMENSION(np), INTENT( IN    ) :: yp     !Output Y array
INTEGER,               INTENT( INOUT ) :: nlev   !Current level (pen color)

!==============================================================================
! Local variables
!==============================================================================
INTEGER                       :: nn
REAL,DIMENSION(:),ALLOCATABLE :: x
REAL,DIMENSION(:),ALLOCATABLE :: y
INTEGER                       :: ncol
INTEGER                       :: ios
INTEGER                       :: irv
REAL                          :: delx,dely
REAL AR, Lx, Ly, dg, mg0
REAL, DIMENSION(2) :: yy
REAL, DIMENSION(3) :: xg, yg

!==============================================================================
! Function calls
!==============================================================================
INTEGER, EXTERNAL :: index_contour
INTEGER, EXTERNAL :: PlotTransform

!==============================================================================
! Initialize
!==============================================================================
CheckFill = SAG_ERROR

!==============================================================================
! Allocate work space
!==============================================================================
nn = np
ALLOCATE( x(nn),y(nn),STAT=ios )
IF( ios /= 0 )GOTO 9999

!==============================================================================
! Transform to Plot coordinates
!==============================================================================
IF( nn == 2 )THEN
  yy = yp(1)
  irv = PlotTransform( np,xp,yy,x,y )
ELSE
  irv = PlotTransform( np,xp,yp,x,y )
END IF
IF( irv /= SCIPsuccess )GOTO 9999

!==============================================================================
! Check for visibility
!==============================================================================
IF( nn > 2 )CALL checkpoly( nn,x,y )

!==============================================================================
! Finished if call is only to check visibility (nlev<0)
!==============================================================================
IF( nlev < 0 )THEN

  nlev = nn

ELSE IF( nn == 2 )THEN

  IF( nlev > 0 )THEN
    ncol = index_contour(nlev)      !Set Pen color
  ELSE
    ncol = 1
  END IF
  CALL gsplci( ncol )

  AR = (pymx-pymn) / (pxmx-pxmn)
  Lx = xmxd - xmnd
  Ly = ymxd - ymnd
  y(2) = y(1) + yp(2)*Ly/Lx*AR

  CALL gpl( 2,x,y )

  xg(1) = (x(1)-xmnd)/Lx; yg(1) = (y(1)-ymnd)/Ly
  xg(2) = (x(2)-xmnd)/Lx; yg(2) = (y(2)-ymnd)/Ly

  xg(3) = 0.75*xg(2) + 0.25*xg(1)
  yg(3) = 0.75*yg(2) + 0.25*yg(1)

  dg    = 0.1*SQRT((xg(2)-xg(1))**2 + (yg(2)-yg(1))**2)
  delx  = xg(2)-xg(1)
  dely  = yg(2)-yg(1)
  IF( ABS(delx) > 1.E-6 )THEN
    mg0  = dely / delx
    dely = dg / SQRT(1.+mg0**2)
    delx = mg0 * dely
  ELSE
    dely = 0.
    delx = dg
  END IF

  xg(3) = xg(3) + delx
  yg(3) = yg(3) - dely /AR
  x(1)  = xmnd + xg(3)*Lx
  y(1)  = ymnd + yg(3)*Ly
  CALL gpl( 2,x,y )

  xg(3) = xg(3) - 2.*delx
  yg(3) = yg(3) + 2.*dely /AR
  x(1)  = xmnd + xg(3)*Lx
  y(1)  = ymnd + yg(3)*Ly
  CALL gpl( 2,x,y )

ELSE

!==============================================================================
! Draw if visible
!==============================================================================
  IF( nn > 0 )THEN
    ncol = index_contour(nlev)      !Set Pen color
    CALL gsplci( ncol )             !Set line pen
    CALL gsfaci( ncol )             !Set area fill pen
    CALL gpl( np,x,y )              !Draw (outline) area
    CALL gfa( np,x,y )              !Fill area
  END IF

END If

!==============================================================================
! Set Return Value
!==============================================================================
CheckFill = SAG_OK

!==============================================================================
! Deallocate Work space
!==============================================================================
9999 CONTINUE

IF( ALLOCATED(x) )DEALLOCATE( x,STAT=ios )
IF( ALLOCATED(y) )DEALLOCATE( y,STAT=ios )

RETURN
END
!*******************************************************************************
!*******************************************************************************
!*******************************************************************************
!                CheckDraw
!*******************************************************************************
INTEGER FUNCTION CheckDraw( np,xp,yp,nlev )

USE contri_fi
USE sagerr_fd
USE sagdef_fd
USE PlotTrans_fi
USE ToolUser_fd

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
INTEGER,               INTENT( IN    ) :: np     !number of points
REAL,   DIMENSION(np), INTENT( IN    ) :: xp     !Input  X array
REAL,   DIMENSION(np), INTENT( IN    ) :: yp     !Output Y array
INTEGER,               INTENT( INOUT ) :: nlev   !Current level (pen color)

!==============================================================================
! Local variables
!==============================================================================
INTEGER                       :: nn
REAL,DIMENSION(:),ALLOCATABLE :: x
REAL,DIMENSION(:),ALLOCATABLE :: y
INTEGER                       :: ios
INTEGER                       :: irv
INTEGER                       :: i

!==============================================================================
! Function calls
!==============================================================================
INTEGER, EXTERNAL :: index_contour
INTEGER, EXTERNAL :: PlotTransform

!==============================================================================
! Initialize
!==============================================================================
CheckDraw = SAG_ERROR

!==============================================================================
! Allocate work space
!==============================================================================
nn = np
ALLOCATE( x(nn),y(nn),STAT=ios )
IF( ios /= 0 )GOTO 9999

!==============================================================================
! Transform to Plot coordinates
!==============================================================================
irv = PlotTransform( np,xp,yp,x,y )
IF( irv /= SCIPsuccess )GOTO 9999

!==============================================================================
! Set Pen color
!==============================================================================
i = index_contour(nlev)    !Set Pen color - if multicolored else just use ipen
CALL gsplci( ipen )

!==============================================================================
! Loop over pairs of points - draw if visible
!==============================================================================
DO i = 1,np-1
  nn = 2
  CALL checkpoly( nn,x(i),y(i) )
  IF( nn > 0 )CALL gpl( nn,x(i),y(i) )
END DO

!==============================================================================
! Set Return Value
!==============================================================================
CheckDraw = SAG_OK

!==============================================================================
! Deallocate Work space
!==============================================================================
9999 CONTINUE

IF( ALLOCATED(x) )DEALLOCATE( x,STAT=ios )
IF( ALLOCATED(y) )DEALLOCATE( y,STAT=ios )

RETURN
END
!*******************************************************************************
!*******************************************************************************
!*******************************************************************************
!                index_contour
!*******************************************************************************
INTEGER FUNCTION index_contour( ival )

USE contri_fi
USE pltchoice_fi

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
INTEGER, INTENT( IN ) :: ival     !Input value (contour level)

!==============================================================================
! Local variables
!==============================================================================
INTEGER kval

!==============================================================================
! Modulo with number of contour colors
!==============================================================================
kval = MOD(ival-1,npalcn) + 1

!==============================================================================
! Set color index
!==============================================================================
index_contour = ipalcn + kval

RETURN
END
!*******************************************************************************
!*******************************************************************************
!*******************************************************************************
!                PlotTransform
!*******************************************************************************
INTEGER FUNCTION PlotTransform( np,xp,yp,x,y )

USE contri_fi
USE PlotTrans_fi
USE ToolUser_fd

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
INTEGER,               INTENT( IN  ) :: np     !number of points
REAL,   DIMENSION(np), INTENT( IN  ) :: xp     !Input  X array - Data coord
REAL,   DIMENSION(np), INTENT( IN  ) :: yp     !Input  Y array
REAL,   DIMENSION(np), INTENT( OUT ) :: x      !Output X array - Plot coord
REAL,   DIMENSION(np), INTENT( OUT ) :: y      !Output Y array

!==============================================================================
! Local variables
!==============================================================================
INTEGER i
INTEGER irv

!==============================================================================
! Function calls
!==============================================================================
INTEGER, EXTERNAL :: SCIPTransform

!==============================================================================
! Initialize
!==============================================================================
PlotTransform = SCIPfailure

!==============================================================================
! Copy Original data
!==============================================================================
DO i = 1,np
  x(i) = xp(i)
  y(i) = yp(i)
END DO

!==============================================================================
! Use SCIPTool to transform
!==============================================================================
irv = SCIPTransform( DataCoordinate,PlotCoordinate,np,x,y )
IF( irv /= SCIPsuccess  )GOTO 9999

!==============================================================================
! Add Plot Scaling
!==============================================================================
DO i = 1,np
  x(i) = x(i)*scx + shx
  y(i) = y(i)*scy + shy
END DO

!==============================================================================
! Set Return value
!==============================================================================
PlotTransform = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!*******************************************************************************
!*******************************************************************************
!                DataTransform
!*******************************************************************************
INTEGER FUNCTION DataTransform( np,xp,yp,x,y )

USE contri_fi
USE PlotTrans_fi
USE ToolUser_fd

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
INTEGER,               INTENT( IN  ) :: np     !number of points
REAL,   DIMENSION(np), INTENT( IN  ) :: xp     !Input  X array - Plot coord
REAL,   DIMENSION(np), INTENT( IN  ) :: yp     !Input  Y array
REAL,   DIMENSION(np), INTENT( OUT ) :: x      !Output X array - Data coord
REAL,   DIMENSION(np), INTENT( OUT ) :: y      !Output Y array

!==============================================================================
! Local variables
!==============================================================================
INTEGER i
INTEGER irv

!==============================================================================
! Function calls
!==============================================================================
INTEGER, EXTERNAL :: SCIPTransform

!==============================================================================
! Initialize
!==============================================================================
DataTransform = SCIPfailure

!==============================================================================
! Copy Original data
!==============================================================================
DO i = 1,np
  x(i) = xp(i)
  y(i) = yp(i)
END DO

!==============================================================================
! Remove Plot Scaling
!==============================================================================
DO i = 1,np
  x(i) = (x(i) - shx)/scx
  y(i) = (y(i) - shy)/scy
END DO

!==============================================================================
! Use SCIPTool to transform
!==============================================================================
irv = SCIPTransform(PlotCoordinate,DataCoordinate,np,x,y)
IF( irv /= SCIPsuccess  )GOTO 9999

!==============================================================================
! Set Return value
!==============================================================================
DataTransform = SCIPsuccess

9999 CONTINUE

RETURN
END
