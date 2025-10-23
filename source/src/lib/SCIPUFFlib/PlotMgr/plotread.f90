SUBROUTINE ReadPuffTime( start )

!------ Build list of puff times

USE files_fi
USE error_fi
USE plotlist_fi
USE SCIPresults_fd

IMPLICIT NONE

TYPE( startT ), INTENT( IN ) :: start

INTEGER ios

INTEGER, EXTERNAL :: NumberPuffTimes

!------ Count the number of times

nPuffTime = NumberPuffTimes( .FALSE. )
IF( nError /= NO_ERROR )GOTO 9999

!------ Allocate PuffTime array

ALLOCATE( PuffTime(nPuffTime),STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'ReadPuffTime'
  eMessage = 'Error allocating puff time list'
  GOTO 9999
END IF

CALL GetPuffTimes( start,nPuffTime,PuffTime )
IF( nError /= NO_ERROR )GOTO 9999

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE ReadSrfTime( lun,file,start )

USE sagstr_fd
USE sagdef_fd
USE sagerr_fd
USE plotlist_fi
USE error_fi
USE SCIPresults_fd
USE PtrGrdStrItf

IMPLICIT NONE

INTEGER,        INTENT( IN ) :: lun
CHARACTER(*),   INTENT( IN ) :: file
TYPE( startT ), INTENT( IN ) :: start

TYPE( SAGgrid_str ), POINTER :: grd

INTEGER irv, ios, i, grdI
INTEGER iout
REAL tout

INTEGER, EXTERNAL :: SAG_NewGrdStr, SAG_InitGridID, SAG_OpenID, SAG_ReadHeaderID
INTEGER, EXTERNAL :: SAG_CloseID, SAG_RmvGrdStr
INTEGER, EXTERNAL :: ComputeTime, TimeToString

!------ Initialize a local SAG grid structure just to read header

irv = SAG_NewGrdStr( grdI )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'ReadSrfTime'
  eMessage = 'Error creating surface grid'
  GOTO 9999
END IF

grd => SAG_PtrGrdStr( grdI )

irv = SAG_InitGridID( file,lun,SAG_GRID_BOTH,0,0,0,grdI )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'ReadSrfTime'
  eMessage = 'Error initializing SAG surface grid'
  CALL ReportFileName( eInform,'File=',file )
  GOTO 9999
END IF

irv = SAG_OpenID( grdI )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'ReadSrfTime'
  eMessage = 'Error opening SAG file'
  CALL ReportFileName( eInform,'File=',file )
  GOTO 9999
END IF

!------ Read header until EOF

nSrfTime = 0

DO
  irv = SAG_ReadHeaderID( grdI )
  IF( irv /= SAG_OK )THEN
    IF( grd%status == SAG_EOF_REC )EXIT
    nError   = UK_ERROR
    eRoutine = 'ReadSrfTime'
    eMessage = 'Error reading SAG times'
    CALL ReportFileName( eInform,'File=',file )
    GOTO 9999
  END IF
  nSrfTime = nSrfTime + 1
END DO

irv = SAG_CloseID( grdI ) ! close so can read again

!------ Allocate array for plot times

ALLOCATE( SrfTime(nSrfTime),STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'ReadSrfTime'
  eMessage = 'Error allocating surface time list'
  GOTO 9999
END IF

!------ Read times from header

DO i = 1,nSrfTime
  irv = SAG_ReadHeaderID( grdI )
  SrfTime(i)%nItems = grd%ncells
  ios = ComputeTime( start,grd%time,SrfTime(i)%time )
  IF( ios /= SCIPsuccess )THEN
    nError   = UK_ERROR
    eRoutine = 'ReadSrfTime'
    eMessage = 'Error converting surface time list'
    GOTO 9999
  END IF
  IF( grd%time > 1000. .AND. i > 1000 )THEN
    !--- Adjust time to nearest multiple of output time step
    tout = SrfTime(2)%time%hour - SrfTime(1)%time%hour
    iout = NINT(SrfTime(i)%time%hour/tout)
    SrfTime(i)%time%hour = FLOAT(iout)*tout
  END IF
  ios = TimeToString( SrfTime(i)%time,SrfTime(i)%string )
  IF( ios /= SCIPsuccess )THEN
    nError   = UK_ERROR
    eRoutine = 'ReadSrfTime'
    eMessage = 'Error creating surface time list string'
    GOTO 9999
  END IF
END DO

9999 CONTINUE

!------ Close file and deallocate grid structure

irv = SAG_CloseID( grdI )
irv = SAG_RmvGrdStr( grdI )

RETURN
END

!==============================================================================

SUBROUTINE ReadSrfField( Field,lun,file,grdI )

USE scipuff_fi
USE sagstr_fd
USE sagdef_fd
USE plotlist_fi
USE error_fi
USE field_fd
USE PtrGrdStrItf
USE SCIPresults_fd

IMPLICIT NONE

TYPE( SCIPPlotFieldT ), INTENT( INOUT ) :: Field
INTEGER,                INTENT( IN    ) :: lun
CHARACTER(*),           INTENT( IN    ) :: file
INTEGER,                INTENT( OUT   ) :: grdI

TYPE( SAGgrid_str ), POINTER :: grd

CHARACTER(64) bname  ! block name
CHARACTER(70) vname  ! block name:field name
CHARACTER(PATH_MAXLENGTH) :: ftemp

INTEGER irv, ios, i, ivar, imat, ikind, nsg, ioffp

REAL    xbar, ybar, xmap, ymap
INTEGER mcID, imult
REAL    rat

INTEGER, EXTERNAL :: SAG_NewGrdStr, SAG_InitGridID, SAG_OpenID, SAG_CloseID
INTEGER, EXTERNAL :: SAG_ReadHeaderID, SAG_ReadGridID, SAG_FindVariableID
INTEGER, EXTERNAL :: SAG_ReadDataID
INTEGER, EXTERNAL :: output_groups

!------ Get new SAG grid structure

irv = SAG_NewGrdStr( grdI )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'ReadSrfField'
  eMessage = 'Error creating surface grid'
  GOTO 9999
END IF

grd => SAG_PtrGrdStr( grdI )    ! Associate "local" grid structure pointer

!------ Initialize SAG structure

IF( BTEST(run_mode,REVERSE_MODE) )THEN
  WRITE(ftemp,'(A,I3.3)') TRIM(file),Field%choice
  irv = SAG_InitGridID( ftemp,lun,SAG_GRID_BOTH,0,0,0,grdI )
ELSE
  irv = SAG_InitGridID( file,lun,SAG_GRID_BOTH,0,0,0,grdI )
END IF
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'ReadSrfField'
  eMessage = 'Error initializing SAG surface grid'
  CALL ReportFileName( eInform,'File=',file )
  GOTO 9999
END IF

!------ Open surface file; read nvart for block version

irv = SAG_OpenID( grdI )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'ReadSrfField'
  eMessage = 'Error opening SAG file'
  CALL ReportFileName( eInform,'File=',file )
  GOTO 9999
END IF

grd%mxnam = grd%nvart

!------ Position to selected time break

DO i = 1,Field%timeID
  irv = SAG_ReadHeaderID( grdI )
  IF( irv == SAG_ERROR )THEN
    nError   = UK_ERROR
    eRoutine = 'ReadSrfField'
    eMessage = 'Error reading SAG grid'
    CALL ReportFileName( eInform,'File=',file )
    GOTO 9999
  END IF
END DO

!------ Allocate grid and data fields for ncells

grd%mxgrd = grd%ncells
grd%mxfld = 3          ! mean & variance + plot field
grd%naux  = 0          ! turn off any auxiliary fields

ALLOCATE( grd%ipgrd(grd%mxgrd),STAT=ios )
IF( ios == 0 )ALLOCATE( grd%ipdat(grd%mxgrd*grd%mxfld),STAT=ios )

IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'ReadSrfField'
  eMessage = 'Error allocating surface grid arrays'
  GOTO 9999
END IF

!------ Read grid

irv = SAG_ReadGridID( grdI )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'ReadSrfField'
  eMessage = 'Error reading surface grid'
  GOTO 9999
END IF

!------ Build block and variable names

IF( Field%choice > ntypm )THEN
  i = Field%choice - nPchoice + nPchoiceMC
  IF( i < 1 .OR. i > nPchoiceMC )THEN
    nError   = IV_ERROR
    eRoutine = 'ReadSrfField'
    eMessage = 'Invalid field Choice for surface plot'
    GOTO 9999
  END IF
  imat = ChoiceMCID(i)
  IF( output_groups(material(imat)) > 1 )THEN
    ikind = 0
  ELSE
    ikind = 1
  END IF

  CALL set_block_name( material(imat),ikind,bname )

  vname = TRIM(bname)//':'//'Mean'

ELSE

imat = Field%choice

IF( ClassChoiceComb(Field%class,Field%choice)%kind == SCIPtrue )THEN

  ikind = Field%kind - is_kind(imat) + 1
  nsg   = output_groups( material(imat) )

ELSE

  ikind = 1
  nsg   = 1

END IF

IF( grd%ftype > 0 )THEN

  IF( ClassChoiceComb(Field%class,Field%choice)%kind == SCIPtrue )THEN

    IF( ikind <= nsg )THEN
      CALL set_block_name( material(imat),ikind,bname )
    ELSE
      CALL set_block_name( material(imat),0,bname )
    END IF

  ELSE

    CALL set_block_name( material(imat),1,bname )

  END IF

  SELECT CASE( ClassID(Field%class) )
    CASE( HP_CMAX )
      i = LEN_TRIM(ClassString(Field%class))
      vname = TRIM(bname)//' '//TRIM(ClassString(Field%class)(1:i-5))//':Mean'

    CASE DEFAULT
      vname = TRIM(bname)//':Mean'
  END SELECT

ELSE

  bname = 'MT'
  ioffp = 0

  IF( ClassChoiceComb(Field%class,Field%choice)%kind == SCIPtrue )THEN

    IF( ikind <= nsg )THEN
      WRITE(vname,'(A1,I3.3)')bname(1:1),material(imat)%ioffp + ikind + ioffp
    ELSE
      WRITE(vname,'(A1,I2.2,A1)')bname(1:1),imat,bname(2:2)
    END IF

  ELSE

    IF( nsg <= 1 )THEN
      WRITE(vname,'(A1,I3.3)')bname(1:1),material(imat)%ioffp + ikind + ioffp
    ELSE
      WRITE(vname,'(A1,I2.2,A1)')bname(1:1),imat,bname(2:2)
    END IF

  END IF

END IF

END IF

Field%units = TRIM(material(imat)%unit)

irv = SAG_FindVariableID( grdI,vname,ivar )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'ReadSrfField'
  eMessage = 'Error reading field name '//TRIM(vname)
  GOTO 9999
END IF

IF( Field%choice > ntypm )THEN
  i = Field%choice - nPchoice + nPchoiceMC
  SELECT CASE( ClassID(Field%class) )
    CASE( HP_DEP )
      ikind = Field%kind - is_kindMC(3,i) + 1
    CASE( HP_DOS )
      ikind = Field%kind - is_kindMC(4,i) + 1
    CASE DEFAULT
      nError   = UK_ERROR
      eRoutine = 'ReadSrfField'
      eMessage = 'Invalid Class type'
      GOTO 9999
  END SELECT

  imat = ChoiceMCID(i)
  mcID = material(imat)%mcID
  SELECT CASE( mat_mc%type(mcID) )
    CASE( MC_CHEM )
      imult = ivar + 2

  END SELECT

END IF

!------ Read specified fields

irv = SAG_ReadDataID( grdI,ivar,1 ) ! mean
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'ReadSrfField'
  eMessage = 'Error reading surface grid'
  GOTO 9999
END IF

SELECT CASE( ClassID(Field%class) )
  CASE( HP_EMAX )

  CASE DEFAULT
    irv = SAG_ReadDataID( grdI,ivar+1,2 ) ! variance
    IF( irv /= SAG_OK )THEN
      nError   = UK_ERROR
      eRoutine = 'ReadSrfField'
      eMessage = 'Error reading surface grid'
      GOTO 9999
    END IF

END SELECT

!---- Read multi-component mean into field-3

IF( Field%choice > ntypm )THEN
  irv = SAG_ReadDataID( grdI,imult+ikind,3 )
  IF( irv /= SAG_OK )THEN
    nError   = UK_ERROR
    eRoutine = 'ReadSrfField'
    eMessage = 'Error reading surface grid'
    GOTO 9999
  END IF

!---- Reset field values

  DO i = 1,grd%ncells
    IF( grd%ipdat(i) > 0.0 )THEN
      rat = SQRT(grd%ipdat(i+grd%mxgrd))/grd%ipdat(i)
    ELSE
      rat = 0.0
    END IF
    grd%ipdat(i+grd%mxgrd)   = (rat*grd%ipdat(i+2*grd%mxgrd))**2
    grd%ipdat(i)             = grd%ipdat(i+2*grd%mxgrd)
    grd%ipdat(i+2*grd%mxgrd) = 0.0
  END DO
END IF

!------ Set delmin

IF( grd%maxlev < 0 )THEN
  grd%delmin = 0.
ELSE
  xbar = grd%xmin + 0.5*(grd%nx-1)*grd%dx
  ybar = grd%ymin + 0.5*(grd%ny-1)*grd%dy
  CALL mapfac( xbar,ybar,xmap,ymap )
  grd%delmin = MIN(grd%dx/xmap,grd%dy/ymap)*(0.5**grd%maxlev)
END IF
9999 CONTINUE

!------ Close file

irv = SAG_CloseID( grdI )

RETURN
END

!==============================================================================

SUBROUTINE ReadSrfAdjField( timeID,lun,file,imat,grdI )

USE scipuff_fi
USE files_fi
USE sagstr_fd
USE sagdef_fd
USE plotlist_fi
USE error_fi
USE PtrGrdStrItf

IMPLICIT NONE

INTEGER,      INTENT( IN  ) :: timeID
INTEGER,      INTENT( IN  ) :: lun
CHARACTER(*), INTENT( IN  ) :: file
INTEGER,      INTENT( IN  ) :: imat
INTEGER,      INTENT( OUT ) :: grdI

TYPE( SAGgrid_str ), POINTER :: grd

CHARACTER(64) bname  ! block name
CHARACTER(70) vname  ! block name:field name

INTEGER irv, ios, i, ivar, ifld
REAL    xbar, ybar, xmap, ymap

CHARACTER(PATH_MAXLENGTH) :: file_dosAdj

INTEGER, EXTERNAL :: SAG_NewGrdStr, SAG_InitGridID, SAG_OpenID, SAG_CloseID
INTEGER, EXTERNAL :: SAG_ReadHeaderID, SAG_ReadGridID, SAG_FindVariableID
INTEGER, EXTERNAL :: SAG_ReadDataID
INTEGER, EXTERNAL :: output_groups

!------ Get new SAG grid structure

irv = SAG_NewGrdStr( grdI )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'ReadSrfAdjField'
  eMessage = 'Error creating surface grid'
  GOTO 9999
END IF

grd => SAG_PtrGrdStr( grdI )    ! Associate "local" grid structure pointer

!------ Initialize SAG structure

WRITE(file_dosAdj,'(A,I3.3)') TRIM(file),imat

irv = SAG_InitGridID( file_dosAdj,lun,SAG_GRID_BOTH,0,0,0,grdI )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'ReadSrfAdjField'
  eMessage = 'Error initializing SAG surface grid'
  CALL ReportFileName( eInform,'File=',file_dosAdj )
  GOTO 9999
END IF

!------ Open surface file; read nvart for block version

irv = SAG_OpenID( grdI )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'ReadSrfAdjField'
  eMessage = 'Error opening SAG file'
  CALL ReportFileName( eInform,'File=',file_dosAdj )
  GOTO 9999
END IF

grd%mxnam = grd%nvart

!------ Position to selected time break

DO i = 1,timeID
  irv = SAG_ReadHeaderID( grdI )
  IF( irv == SAG_ERROR )THEN
    nError   = UK_ERROR
    eRoutine = 'ReadSrfField'
    eMessage = 'Error reading SAG grid'
    CALL ReportFileName( eInform,'File=',file_dosAdj )
    GOTO 9999
  END IF
END DO

!------ Allocate grid and data fields for ncells

grd%mxgrd = grd%ncells
grd%mxfld = grd%nvart
grd%naux  = 0          ! turn off any auxiliary fields

ALLOCATE( grd%ipgrd(grd%mxgrd),STAT=ios )
IF( ios == 0 )ALLOCATE( grd%ipdat(grd%mxgrd*grd%mxfld),STAT=ios )

IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'ReadSrfAdjField'
  eMessage = 'Error allocating surface grid arrays'
  GOTO 9999
END IF

!------ Read grid

irv = SAG_ReadGridID( grdI )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'ReadSrfAdjField'
  eMessage = 'Error reading surface grid'
  CALL ReportFileName( eInform,'File=',file_dosAdj )
  GOTO 9999
END IF

ifld = 0

CALL set_block_name( material(imat),1,bname )
vname = TRIM(bname)//':'//'Mean'

irv = SAG_FindVariableID( grdI,vname,ivar )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'ReadSrfAdjField'
  eMessage = 'Error reading field name '//TRIM(vname)
  CALL ReportFileName( eInform,'File=',file_dosAdj )
  GOTO 9999
END IF

!------ Read specified fields

ifld = ifld + 1
irv = SAG_ReadDataID( grdI,ivar,ifld ) ! mean
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'ReadSrfAdjField'
  eMessage = 'Error reading surface grid'
  CALL ReportFileName( eInform,'File=',file_dosAdj )
  GOTO 9999
END IF

ifld = ifld + 1
irv = SAG_ReadDataID( grdI,ivar+1,ifld ) ! variance
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'ReadSrfAdjField'
  eMessage = 'Error reading surface grid'
  CALL ReportFileName( eInform,'File=',file_dosAdj )
  GOTO 9999
END IF

ifld = ifld + 1
irv = SAG_ReadDataID( grdI,ivar+2,ifld ) ! scale
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'ReadSrfAdjField'
  eMessage = 'Error reading surface grid'
  CALL ReportFileName( eInform,'File=',file_dosAdj )
  GOTO 9999
END IF

!------ Set delmin

IF( grd%maxlev < 0 )THEN
  grd%delmin = 0.
ELSE
  xbar = grd%xmin + 0.5*(grd%nx-1)*grd%dx
  ybar = grd%ymin + 0.5*(grd%ny-1)*grd%dy
  CALL mapfac( xbar,ybar,xmap,ymap )
  grd%delmin = MIN(grd%dx/xmap,grd%dy/ymap)*(0.5**grd%maxlev)
END IF

9999 CONTINUE

!------ Close file

irv = SAG_CloseID( grdI )

RETURN
END

!==============================================================================

SUBROUTINE SubtractSrfAdjField( grdI1,grdI2 )

USE scipuff_fi
USE sagstr_fd
USE sagdef_fd
USE error_fi
USE PtrGrdStrItf
USE srfparam_fd

IMPLICIT NONE

INTEGER, INTENT( IN  ) :: grdI1
INTEGER, INTENT( IN  ) :: grdI2

INTEGER i, i1, i2, lev1, lev2, irv
LOGICAL lzero
REAL    val

INTEGER, DIMENSION(3) :: srftypes

TYPE( SAGgrid_str ), POINTER :: grd1, grd2

INTEGER, EXTERNAL :: SAG_FindMaxLevID, SAG_Dezone, DezoneCell

INTERFACE
  RECURSIVE SUBROUTINE SubtractSrfAdjCell( i1,i2,grd1,grd2,lzero,val )
    USE sagstr_fd
    INTEGER,       INTENT( IN  ) :: i1, i2
    TYPE( SAGgrid_str ), POINTER :: grd1, grd2
    LOGICAL,       INTENT( OUT ) :: lzero
    REAL,          INTENT( IN  ) :: val
  END SUBROUTINE SubtractSrfAdjCell
END INTERFACE

grd1 => SAG_PtrGrdStr( grdI1 )
grd2 => SAG_PtrGrdStr( grdI2 )

lev1 = SAG_FindMaxLevID( grdI1  )
lev2 = SAG_FindMaxLevID( grdI2  )
IF( grd2%maxlev > 0 .AND. grd2%maxlev < lev1 )THEN
  srftypes(1) = DEZONE_MEAN
  srftypes(2) = DEZONE_MEAN
  srftypes(3) = DEZONE_MEAN
  CALL init_dezone( grd1%nvart,srftypes )
  IF( nError /= NO_ERROR )GOTO 9999
  lev1 = lev1 - grd2%maxlev
  DO i = 1,lev1
    irv = SAG_Dezone( grdI1,0,DezoneCell )
    IF( irv /= SAG_OK )THEN
      CALL exit_dezone()
      nError   = UK_ERROR
      eRoutine = 'SubtractSrfAdjField'
      eMessage = 'Error dezoning sfc grid'
      GOTO 9999
    END IF
  END DO
  CALL exit_dezone()
END IF

DO i = 1,grd1%nx*grd1%ny
  i1 = i
  i2 = i
  val = 0.0
  CALL SubtractSrfAdjCell( i1,i2,grd1,grd2,lzero,val )
END DO

9999 CONTINUE

RETURN
END

!==============================================================================

RECURSIVE SUBROUTINE SubtractSrfAdjCell( i1,i2,grd1,grd2,lzero,val )

USE sagstr_fd

INTEGER,       INTENT( IN  ) :: i1, i2
TYPE( SAGgrid_str ), POINTER :: grd1, grd2
LOGICAL,       INTENT( OUT ) :: lzero
REAL,          INTENT( IN  ) :: val

INTEGER i1p, i2p, i
LOGICAL lzerop
REAL    valp

DO i = 1,grd1%nvart
  i1p = i1 + (i-1)*grd1%mxgrd
  i2p = i2 + (i-1)*grd2%mxgrd
  grd2%ipdat(i2p) = grd2%ipdat(i2p) - grd1%ipdat(i1p)
END DO

valp = val + grd2%ipdat(i2)

lzero = .TRUE.

IF( grd2%ipgrd(i2) /= 0 )THEN
  IF( grd1%ipgrd(i1) /= 0 )THEN
    i1p = grd1%ipgrd(i1)
    i2p = grd2%ipgrd(i2)
    DO i = 0,3
      CALL SubtractSrfAdjCell( i1p+i,i2p+i,grd1,grd2,lzerop,valp )
      lzero = lzero .AND. lzerop
    END DO
    IF( lzero )THEN
      grd2%ipgrd(i2) = 0
    END IF
  END IF
END IF

lzero = lzero .AND. grd2%ipdat(i2) <= 0.01*val

RETURN
END

!==============================================================================

SUBROUTINE ReadMetTime( start )

!------ Build list of met times

USE plotlist_fi
USE files_fi
USE error_fi
USE time_fd
USE SCIPresults_fd

IMPLICIT NONE

TYPE(startT), INTENT( IN ) :: start

REAL    tx
INTEGER ios, i
LOGICAL lexist

CHARACTER(256) line, string

INTEGER, EXTERNAL :: ComputeTime, TimeToString

nMetTime = 0

INQUIRE( FILE=TRIM(file_mcw),EXIST=lexist,IOSTAT=ios )
IF( lexist )THEN
  OPEN(FILE=TRIM(file_log),UNIT=lun_log,STATUS='OLD',ACTION='READ',IOSTAT=ios)
  DO
    READ(lun_log,'(A)',IOSTAT=ios) line

    ! Check for end of file in all compilers
    IF( IS_IOSTAT_END(ios) )ios = -1

    IF( ios > 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'ReadMetTime'
      eMessage = 'Error reading met times from log file'
    ELSE IF( ios < 0 )THEN
      EXIT
    ELSE
      i = INDEX( line,'Outputting met field' )
      IF( i > 0 )THEN
        i = INDEX( line,' 1 at' )
        IF( i > 0 )nMetTime = nMetTime + 1
      END IF
    END IF
  END DO
END IF

!------ Allocate array for met times

IF( nMetTime > 0 )THEN

  ALLOCATE( MetTime(nMetTime),STAT=ios )
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'ReadMetTime'
    eMessage = 'Error allocating met time list'
    GOTO 9999
  END IF

!------ Read times from log file

  nMetTime = 0
  REWIND( UNIT=lun_log,IOSTAT=ios)

  DO
    READ(lun_log,'(A)',IOSTAT=ios) line

    ! Check for end of file in all compilers
    IF( IS_IOSTAT_END(ios) )ios = -1

    IF( ios > 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'ReadMetTime'
      eMessage = 'Error reading met times from log file'
    ELSE IF( ios < 0 )THEN
      EXIT
    ELSE
      i = INDEX( line,'Outputting met field' )
      IF( i > 0 )THEN
        i = INDEX( line,' 1 at' )
        IF( i > 0 )THEN
          nMetTime = nMetTime + 1
          READ(line(i+6:),'(A)',IOSTAT=ios) string
          i = INDEX( string,'(' )
          READ(string(i+1:),*) tx
          i = INDEX( string,')' )
          IF( string(i-3:i-1) == 'min' )THEN
            tx = tx/60.
          ELSE IF( string(i-3:i-1) == 'sec' )THEN
            tx = tx/3600.
          END IF
          MetTime(nMetTime)%nItems = 1
          ios = ComputeTime( start,tx,MetTime(nMetTime)%time )
          IF( ios /= SCIPsuccess )THEN
            nError   = UK_ERROR
            eRoutine = 'ReadMetTime'
            eMessage = 'Error converting met time list'
            GOTO 9999
          END IF
          ios = TimeToString( MetTime(nMetTime)%time,MetTime(nMetTime)%string )
          IF( ios /= SCIPsuccess )THEN
            nError   = UK_ERROR
            eRoutine = 'ReadMetTime'
            eMessage = 'Error creating met time list string'
            GOTO 9999
          END IF
        END IF
      END IF
    END IF
  END DO

END IF

9999 CONTINUE

CLOSE( UNIT=lun_log,IOSTAT=ios )

RETURN
END

!==============================================================================

SUBROUTINE CheckPlotReq( Field )

USE field_fd
USE plotlist_fi
USE error_fi
USE SCIPresults_fd

IMPLICIT NONE

TYPE( SCIPPlotFieldT ), INTENT( IN ) :: Field

INTEGER icat, iclass, ivar, ikind

!------ Pull out plot indices

icat   = Field%category
iclass = Field%class
ivar   = Field%choice
ikind  = Field%kind

!------ Check if request indices are within range

IF( icat   < 1 .OR. icat   > HP_NUMCAT )GOTO 9998
IF( iclass < 1 .OR. iclass > nPclassT  )GOTO 9998
IF( ivar   < 1 .OR. ivar   > nPchoiceT )GOTO 9998

!------ Check availability

IF( .NOT.(CatClassComb(icat,iclass)%available    == SCIPtrue) )GOTO 9998
IF( .NOT.BTEST(ClassChoiceComb(iclass,ivar)%available,HPB_AVAILABLE) )GOTO 9998

!------ Check kind index

IF( ClassChoiceComb(iclass,ivar)%kind == SCIPtrue )THEN
  ikind = ikind - ClassChoiceComb(iclass,ivar)%ikind + 1
  IF( ikind < 1 .OR. &
      ikind > ClassChoiceComb(iclass,ivar)%nkind )GOTO 9998
END IF

!------ Check time

!not done yet

9999 CONTINUE
RETURN

9998 CONTINUE
nError   = IV_ERROR
eRoutine = 'CheckPlotReq'
eMessage = 'Invalid plot request'
GOTO 9999

END

!==============================================================================

SUBROUTINE CreateMetPlot( Field,ClassData,grdI )

USE scipuff_fi
USE met_fi
USE sagstr_fd
USE sagdef_fd
USE plotlist_fi
USE field_fd
USE PtrGrdStrItf
USE classdata_fd

IMPLICIT NONE

TYPE( SCIPPlotFieldT ), INTENT( INOUT ) :: Field
REAL, DIMENSION(*),     INTENT( IN    ) :: ClassData    !Additional Class data
INTEGER,                INTENT( OUT   ) :: grdI

TYPE( SAGgrid_str ), POINTER :: grd

LOGICAL terrain
INTEGER irv, mxgrd, mxfld, mxvar, lun
INTEGER i, alloc_stat, ifld
INTEGER nbase
INTEGER j, i0
REAL    x, y, h, hx, hy

CHARACTER(8) file

REAL, DIMENSION(:), ALLOCATABLE :: wrk

INTEGER, EXTERNAL :: SAG_NewGrdStr, SAG_InitGridID

INTERFACE
  INTEGER FUNCTION SAG_FillGridStrField( grd,ifld,Fdata )
    USE sagstr_fd
    TYPE ( SAGgrid_str ),  POINTER      :: grd
    INTEGER,               INTENT( IN ) :: ifld
    REAL,    DIMENSION(*), INTENT( IN ) :: Fdata
  END FUNCTION SAG_FillGridStrField
END INTERFACE

!------ Check choice

nbase = ntypm
IF( BTEST(run_mode,REVERSE_MODE) .AND. ntypm > 1 )nbase = nbase + HP_NUMADJ

IF( Field%choice <= nbase .OR. Field%choice > nPchoice )THEN
  nError   = IV_ERROR
  eRoutine = 'CreateMetPlot'
  eMessage = 'Invalid Met plot variable'
  GOTO 9999
END IF

!------ Get new SAG grid structure

irv = SAG_NewGrdStr( grdI )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'CreateMetPlot'
  eMessage = 'Error creating surface grid'
  GOTO 9999
END IF

grd => SAG_PtrGrdStr( grdI )    ! Associate "local" grid structure pointer

!------ Set field and variable number (for nested grids)

ifld = 0
terrain = .FALSE.
DO i = 1,nBaseMet
  IF( MetGrid(i)%lter )ifld = ifld + 1
  IF( ifld+nbase == Field%choice )THEN
    ifld = i
    terrain = .TRUE.
    EXIT
  END IF
END DO

!------ Initialize SAG structure

file = ' '; lun = 0

grd%nx = MetGrid(ifld)%nx; grd%ny = MetGrid(ifld)%ny

mxgrd = grd%nx*grd%ny
mxfld = 3                       !Plot always expects three
mxvar = 1

!------ Convert to project units if necessary

IF( MetGrid(ifld)%coord%type /= lmap )THEN

  grd%dx = (MetGrid(ifld)%xmaxPrj - MetGrid(ifld)%xminPrj)/FLOAT(MetGrid(ifld)%nx-1)
  grd%dy = (MetGrid(ifld)%ymaxPrj - MetGrid(ifld)%yminPrj)/FLOAT(MetGrid(ifld)%ny-1)

  grd%xmin = MetGrid(ifld)%xminPrj - 0.5*grd%dx
  grd%ymin = MetGrid(ifld)%yminPrj - 0.5*grd%dy

ELSE

  grd%dx   = MetGrid(ifld)%dx; grd%dy = MetGrid(ifld)%dy
  grd%xmin = MetGrid(ifld)%xmin - 0.5*grd%dx
  grd%ymin = MetGrid(ifld)%ymin - 0.5*grd%dy

END IF

irv = SAG_InitGridID( file,lun,SAG_GRID_NONE,mxgrd,mxfld,mxvar,grdI )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'CreateMetPlot'
  eMessage = 'Error initializing SAG surface grid'
  GOTO 9999
END IF

grd%ncells = mxgrd
grd%maxlev = -99
grd%delmin = 0.

!------ Fill data field with terrain

ALLOCATE( wrk(mxgrd),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'CreateMetPlot'
  eMessage = 'Error allocating height grid'
  GOTO 9999
END IF

IF( terrain )THEN

  IF( MetGrid(ifld)%coord%type /= lmap )THEN

    DO j = 1,grd%ny
      i0 = (j-1)*grd%nx
      y = (FLOAT(j)-0.5)*grd%dy + grd%ymin
      DO i = 1,grd%nx
        x = (FLOAT(i)-0.5)*grd%dx + grd%xmin
        CALL get_topogIn( x,y,h,hx,hy,ifld )
        wrk(i0+i) = h + hmin
      END DO
    END DO

  ELSE

    DO i = 1,mxgrd
      wrk(i) = MetGrid(ifld)%H(i) + MetGrid(ifld)%Hmin
    END DO

  END IF

  Field%units = 'm'
  Field%coordinate%horzSlice%height  = 0.0
  Field%coordinate%horzSlice%mode    = 1

  grd%nvart = 1

END IF

irv = SAG_FillGridStrField( grd,1,wrk )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'CreateMetPlot'
  eMessage = 'Error putting met/terrain in SAG field'
  GOTO 9999
END IF

9999 CONTINUE

IF( ALLOCATED(wrk) )DEALLOCATE( wrk,STAT=alloc_stat )

RETURN
END

!==============================================================================

SUBROUTINE ClearTimeLists()

USE plotlist_fi

IMPLICIT NONE

INTEGER alloc_stat

IF( ALLOCATED(SrfTime ) )DEALLOCATE( SrfTime, STAT=alloc_stat )
IF( ALLOCATED(PuffTime) )DEALLOCATE( PuffTime,STAT=alloc_stat )
IF( ALLOCATED(MetTime ) )DEALLOCATE( MetTime, STAT=alloc_stat )

nPuffTime = 0
nSrfTime  = 0
nMetTime  = 0

RETURN
END
