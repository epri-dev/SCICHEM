!*******************************************************************************
!                SAG_LastError
!*******************************************************************************
INTEGER FUNCTION SAG_LastError( line1,line2 )

USE DefSize_fd
USE sagerr_fd
USE sagerr_fi

!     SAG Last Error reporting

IMPLICIT NONE

CHARACTER(*),INTENT( OUT ) :: line1
CHARACTER(*),INTENT( OUT ) :: line2

CHARACTER(12)  parm
CHARACTER(24)  extra

SAG_LastError = LastError

line1 =''//CHAR(0)
line2 =''//CHAR(0)

WRITE(parm,*)LastParm

SELECT CASE( LastError )
  CASE( SAG_ERR_NULL )
    line1 ='No error'
  CASE( SAG_ERR_UNKN )
    line1 ='Unknown error'
  CASE( SAG_ERR_OPENED )
    line1 ='Error opening SAG file'
    IF( LastParm > 0 )THEN
      line2 ='Unit '//TRIM(ADJUSTL(parm))//' already in use'
    ELSE
      line2 ='File is already opened on another unit'
    END IF
  CASE( SAG_ERR_OPEN )
    line1 ='Error opening SAG file'
    line2 ='IOSTAT = '//TRIM(ADJUSTL(parm))
  CASE( SAG_ERR_TEMP )
    line1 ='File access attempted for SAG grid with no associated file'
  CASE( SAG_ERR_IPDAT )
    line1 ='SAG field data pointer error'
  CASE( SAG_ERR_IPGRD )
    line1 ='SAG grid data pointer error'
  CASE( SAG_ERR_IPNAM )
    line1 ='SAG name data pointer error'
  CASE( SAG_ERR_READ )
    line1 ='Error reading SAG file'
    line2 ='IOSTAT = '//TRIM(ADJUSTL(parm))
  CASE( SAG_ERR_WRITE )
    line1 ='Error writing SAG file'
    line2 ='IOSTAT = '//TRIM(ADJUSTL(parm))
  CASE( SAG_ERR_MXGRD )
    line1 ='Too many cells on SAG file'
    line2 ='Attempted to read '//TRIM(ADJUSTL(parm))//' cells'
  CASE( SAG_ERR_MXNAM )
    line1 ='Too many names on SAG file header'
    line2 ='Attempted to read '//TRIM(ADJUSTL(parm))//' names'
  CASE( SAG_ERR_MXFLD )
    line1 ='MXFLD error'
  CASE( SAG_ERR_INVALID )
    line1 ='SAG argument has an invalid value'
  CASE( SAG_ERR_IVAR )
    line1 ='SAG variable pointer outside range'
    line2 ='Variable pointer = '//TRIM(ADJUSTL(parm))
  CASE( SAG_ERR_IFLD )
    line1 ='SAG field pointer outside range'
    line2 ='Field pointer = '//TRIM(ADJUSTL(parm))
  CASE( SAG_ERR_NOTFOUND )
    line1 ='Unable to locate requested variable in SAG variable list'
  CASE( SAG_ERR_STATUS )
    line1 ='Invalid SAG status'
    line2 ='Status = '//TRIM(ADJUSTL(parm))
  CASE( SAG_ERR_TYPE )
    line1 ='Invalid SAG type'
    line2 ='Type = '//TRIM(ADJUSTL(parm))
  CASE( SAG_ERR_SKIP )
    line1 ='Error attempting to skip records in SAG file'
    line2 ='Record type = '//TRIM(ADJUSTL(parm))
  CASE( SAG_ERR_FIND )
    line1 ='Error attempting to find records in SAG file'
    line2 ='Record type = '//TRIM(ADJUSTL(parm))
  CASE( SAG_ERR_MALLOC )
    line1 ='Error attempting allocate memory (MALLOC)'
    line2 ='Memory request = '//TRIM(ADJUSTL(parm))
  CASE( SAG_ERR_LUN )
    line1 ='Invalid logical unit number'
    line2 ='Unit = '//TRIM(ADJUSTL(parm))
  CASE( SAG_ERR_COPY )
    line1 ='Error attempting to copy grid'
    IF( LastParm > 0 )THEN
      extra ='Copy'
    ELSE
      extra ='Receiving'
    END IF
    SELECT CASE( ABS(LastParm) )
      CASE( 1 )
        line2 = TRIM(extra)//'-grid has no allocated grid space'
      CASE( 2 )
        line2 = TRIM(extra)//'-grid has no allocated data space'
      CASE( 3 )
        line2 ='Recieving-grid grid space is too small'
      CASE( 4 )
        line2 ='Recieving-grid data space is too small'
      CASE( 5 )
        line2 = TRIM(extra)//'grid missing block data structure'
      CASE DEFAULT
        line2 ='Unknown Error'
    END SELECT
  CASE( SAG_ERR_BACK )
    IF( LastParm < 0 )THEN
      line1 ='Error restoring data fields'
      line2 ='Field pointers lost'
    ELSE IF( LastParm == 0 )THEN
      line1 ='Error backing up data fields'
      line2 ='BackUp fields already contain data'
    ELSE
      line1 ='Error restoring data fields'
      line2 ='No. fields not restored = '//TRIM(ADJUSTL(parm))
    END IF
  CASE( SAG_ERR_INCOMP )
    line1 ='Error combining grids'
    SELECT CASE( LastParm )
      CASE( 1 )
        line2 ='Basic grid X points mismatch'
      CASE( 2 )
        line2 ='Basic grid Y points mismatch'
      CASE( 3 )
        line2 ='Basic grid type mismatch'
      CASE( 4 )
        line2 ='Grid union results in too many grid cells'
      CASE( 5 )
        line2 ='Resulting grids of unequal size'
      CASE DEFAULT
        line2 ='Unknown Error'
    END SELECT
  CASE( SAG_ERR_IPDATA )
    line1 ='SAG node data pointer error'
  CASE( SAG_ERR_IPNODE )
    line1 ='SAG node pointer error'
  CASE( SAG_ERR_MXDATA )
    line1 ='MXDATA node error'
  CASE( SAG_ERR_MXNODE )
    line1 ='MXNODE node error'
  CASE( SAG_ERR_MXTRI )
    line1 ='MXTRI triangle error'
  CASE( SAG_ERR_IPTRI )
    line1 ='SAG triangle pointer error'
  CASE( SAG_ERR_IPEDGE )
    line1 ='SAG triangle edge data pointer error'
  CASE( SAG_ERR_IPCELL )
    line1 ='SAG triangle cell data pointer error'
  CASE( SAG_ERR_NONODE )
    line1 ='SAG BottonCount - No nodes'
  CASE( SAG_ERR_NOEDGE )
    line1 ='SAG EdgeCount - No edges'
  CASE( SAG_ERR_IPCONN )
    line1 ='SAG triangle connection data pointer error'
  CASE( SAG_ERR_NOTRI )
    line1 ='SAG BuildConnections - No triangles'
  CASE( SAG_ERR_IPENOD )
    line1 ='SAG triangle edge node data pointer error'
  CASE( SAG_ERR_MIBE )
    line1 ='SAG triangle - Multiple independent boundary edge'
  CASE( SAG_ERR_NODATA )
    line1 ='SAG BuildContour - No data'
  CASE( SAG_ERR_NOCNTR )
    line1 ='SAG BuildContour - Contour outside data range'
  CASE( SAG_ERR_IPPTS )
    line1 ='SAG contour points pointer error'
  CASE( SAG_ERR_IPLNS )
    line1 ='SAG contour lines pointer error'
  CASE( SAG_ERR_MXPTS )
    line1 ='MXPTS contour error'
  CASE( SAG_ERR_MXLNS )
    line1 ='MXLNS contour error'
  CASE( SAG_ERR_IPFIL )
    line1 ='SAG fill pointer error'
  CASE( SAG_ERR_IPLEV )
    line1 ='SAG fill level data pointer error'
  CASE( SAG_ERR_USER )
    line1 ='SAG User Function pointer error'
  CASE( SAG_ERR_NOLEV )
    line1 ='SAG FillTriangle - No fill levels specified'
  CASE( SAG_ERR_IPBLK )
    line1 ='SAG block data pointer error'
  CASE( SAG_ERR_FTYPE )
    line1 ='Invalid SAG file type'
    line2 ='Type = '//TRIM(ADJUSTL(parm))
  CASE( SAG_ERR_BLKNAM )
    line1 ='Invalid SAG block/variable name combination'
  CASE( SAG_ERR_WRTPOS )
    line1 ='Position error attempting to write records in SAG file'
    line2 ='Record type = '//TRIM(ADJUSTL(parm))
  CASE( SAG_ERR_CLOSED )
    line1 ='Error writing SAG file'
    IF( LastParm > 0 )THEN
      line2 ='File opened on another unit: Unit='//TRIM(ADJUSTL(parm))
    ELSE
      line2 ='File is not opened'
    END IF
  CASE( SAG_ERR_CALLBACK )
    line1 ='Error detected from CallBack Routine'
    line2 ='CallBack error = '//TRIM(ADJUSTL(parm))
  CASE( SAG_ERR_FSIZE )
    WRITE(extra,*)PATH_MAXLENGTH
    line1 ='Filename too long. Maximum filename length = '//TRIM(ADJUSTL(extra))
    line2 ='Filename length = '//TRIM(ADJUSTL(parm))
  CASE( SAG_ERR_NYI )
    line1 ='SAG function not yet implemented'
  CASE( SAG_ERR_INT4 )
    line1 ='Unable to allocate SAG data array. Size exceeds I4 limit.'
    WRITE(line2,'(A,I11)')'Maximum available size for this grid is',(MAX_ALLOWED_I4/lastParm)
  CASE DEFAULT
    line1 ='Unknown error condition'
END SELECT

RETURN
END
!*******************************************************************************
!                SAG_InitError
!*******************************************************************************
INTEGER FUNCTION SAG_InitError()

USE sagdef_fd
USE sagerr_fd
USE sagerr_fi

!     SAG Last Error reporting

IMPLICIT NONE

SAG_InitError = SAG_OK

LastError = SAG_ERR_NULL
LastParm  = 0

RETURN
END
