!***********************************************************************
!               CreateCompatibleDC
!***********************************************************************
SUBROUTINE init_DC( iwnd,jdc )

USE resource_fd
USE default_fd
USE errorParam_fd
USE SwinAPI
USE pcscipuf_fi, ONLY: NULL_POINTER

! This routine creates a Device Context compatible with a window

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN  ) :: iwnd !Window handle
INTEGER(POINTER_LEN), INTENT( OUT ) :: jdc  !DC handle

INTEGER irv
INTEGER(POINTER_LEN) idc

INTEGER nError
CHARACTER(128) eMessage,eInform,eAction,eRoutine

!---- Initialize

idc = 0
jdc = 0

!---- Get handle to window DC

idc = GetDC( iwnd )
IF( idc == NULL_POINTER )THEN
  eMessage = 'GetDC failure'
  WRITE(eInform,*)'Input = ',iwnd
  GOTO 9999
END IF

!---- Create Compatible DC

jdc = CreateCompatibleDC( idc )
IF( jdc == NULL_POINTER )THEN
  eMessage = 'CreateCompatibleDC failure'
  WRITE(eInform,*)'Input = ',idc
  jdc = NOT_SET_I
  GOTO 9999
END IF

!---- Release window DC

9000 CONTINUE
irv = ReleaseDC( iwnd,idc )
IF( irv <= 0 )THEN
  eMessage = 'ReleaseDC failure'
  WRITE(eInform,*)'Input = ',iwnd,idc
  GOTO 9999
END IF

9100 CONTINUE

RETURN

9999 CONTINUE
nError   = API_ERROR
eRoutine = 'InitDC'
eAction  = 'Call for Technical assistance'
CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
IF( jdc < 0 )THEN
  jdc = 0
  GOTO 9000
ELSE
  GOTO 9100
END IF

END
!***********************************************************************
!               DeteleDC
!***********************************************************************
SUBROUTINE dele_DC( jdc )

USE resource_fd
USE errorParam_fd
USE SwinAPI
!
!     This routine deletes a compatible DC
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: jdc !DC handle

INTEGER irv

CHARACTER(128) eInform

!---- Delete Compatible DC

irv = DeleteDC( jdc )
IF( irv == FALSE )THEN
  WRITE(eInform,*)'Input = ',jdc
  CALL SetError( API_ERROR, &
                'DeleteDC failure', &
                 eInform, &
                'Call for Technical assistance', &
                'DeleDC' )
END IF

RETURN
END
!***********************************************************************
!               CopyBitmap
!***********************************************************************
SUBROUTINE copy_bmp( iwnd,ihbmp,jwid,jhgt,iflg )

USE resource_fd
USE default_fd
USE errorParam_fd
USE SwinAPI
USE pcscipuf_fi, ONLY: NULL_POINTER

! This routine copies the window image to a bitmap

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN    ) :: iwnd  !Window handle
INTEGER(POINTER_LEN), INTENT( INOUT ) :: ihbmp !Bitmap handle
INTEGER,              INTENT( IN    ) :: jwid  !Bitmap width if > 0
INTEGER,              INTENT( IN    ) :: jhgt  !Bitmap height if > 0
INTEGER,              INTENT( IN    ) :: iflg  !Delete old Bitmap flag >0 -> delete

INTEGER(POINTER_LEN) idc,jdc,iold,jold
INTEGER iwid,ihgt,kwid,khgt,irv

TYPE( T_RECT ) Box

INTEGER nError
CHARACTER(128) eMessage,eInform,eAction,eRoutine

LOGICAL, EXTERNAL :: HasError

nError = NO_ERROR

!---- Delete Old Bitmap

IF( iflg /= 0 .AND. ihbmp /= 0 )THEN
  irv = GetObjectType( ihbmp )
  IF( irv /= OBJ_BITMAP )THEN
    irv = GetLastError()
    IF( irv == ERROR_CALL_NOT_IMPLEMENTED )irv = OBJ_BITMAP
  END IF
  IF( irv == OBJ_BITMAP )THEN
    irv = DeleteObject( ihbmp )
    IF( irv == FALSE )THEN
      eMessage = 'DeleteObject failure'
      WRITE(eInform,*)'Input = ',ihbmp
      GOTO 9999
    END IF
  ELSE
    eMessage = 'Invalid ObjectType'
    WRITE(eInform,*)'Input = ',ihbmp,'  Output = ',irv
    GOTO 9999
  END IF
END IF

!---- Initialize

idc   = 0
jdc   = 0
iwid  = 0
ihgt  = 0
ihbmp = 0

!---- Determine Bitmap size

irv = GetClientRect( iwnd,Box )
IF( irv == FALSE )THEN
  eMessage = 'GetClientRect failure'
  iold = GetLastError()
  WRITE(eInform,*)'Input = ',iwnd,'  Error = ',iold
  GOTO 9999
END IF

iwid = Box%right  - Box%left
ihgt = Box%bottom - Box%top

IF( jwid <= 0 )THEN
  kwid = iwid
ELSE
  kwid = jwid
END IF

IF( jhgt <= 0 )THEN
  khgt = ihgt
ELSE
  khgt = jhgt
END IF

!---- Create Compatible DC

CALL init_DC( iwnd,jdc )
IF( hasError() )GOTO 9999

!---- Get handle to window DC

idc = GetDC( iwnd )
IF( idc == NULL_POINTER )THEN
  eMessage = 'GetDC failure'
  WRITE(eInform,*)'Input = ',iwnd
  GOTO 9999
END IF

!---- Create Compatible bitmap

ihbmp = CreateCompatibleBitmap( idc,kwid,khgt )
IF( ihbmp == NULL_POINTER )THEN
  eMessage = 'CreateCompatibleBitmap failure'
  WRITE(eInform,*)'Input = ',jdc,kwid,khgt
  GOTO 9999
END IF

!---- Select bitmap into compatible DC

iold = SelectObject( jdc,ihbmp )
IF( iold == NULL_POINTER )THEN
  eMessage = 'SelectObject failure'
  WRITE(eInform,*)'Input = ',jdc,ihbmp
  GOTO 9999
END IF

!---- Copy bits from window DC to compatible DC

irv = StretchBlt( jdc,0,0,kwid,khgt,idc,0,0,iwid,ihgt,SRCCOPY )
IF( irv == NULL )THEN
  eMessage = 'StretchBlt failure'
  WRITE(eInform,*)'Input = ',jdc,idc
  GOTO 9999
END IF

!---- DeSelect bitmap from compatible DC

jold = SelectObject( jdc,iold )
IF( jold == NULL_POINTER )THEN
  eMessage = '(De)SelectObject failure'
  WRITE(eInform,*)'Input = ',jdc,iold
  GOTO 9999
END IF

!---- Delete compatible DC

CALL dele_DC( jdc )
IF( nError /= NO_ERROR )GOTO 9999

!---- Release window DC

9000 CONTINUE

irv = ReleaseDC( iwnd,idc )
IF( irv <= 0 )THEN
  eMessage ='ReleaseDC failure'
  WRITE(eInform,*)'Input = ',iwnd,idc
  GOTO 9999
END IF

9100 CONTINUE

RETURN

9999 CONTINUE

nError   = API_ERROR
eRoutine = 'CopyBmp'
eAction  = 'Call for Technical assistance'
CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
IF ( idc /= 0 )THEN
  GOTO 9000
ELSE
  GOTO 9100
END IF

END
!***********************************************************************
!               CopyBitmapClipboard
!***********************************************************************
SUBROUTINE bmp_clipboard( iwnd,ihbmp )

USE resource_fd
USE errorParam_fd
USE SwinAPI
!
!     This routine copies a bitmap image to the clipboard
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd  !Window handle
INTEGER(POINTER_LEN), INTENT( IN ) :: ihbmp !Bitmap handle

INTEGER irv
INTEGER(POINTER_LEN) jrv

CHARACTER(128) cttl,cmsg

INTEGER nError
CHARACTER(128) eMessage,eInform,eAction,eRoutine

irv = OpenClipboard( iwnd )
IF( irv == TRUE )THEN
  irv = EmptyClipboard()
  IF( irv == FALSE )THEN
    eInform = 'Unable to empty clipboard'
    GOTO 9999
  END IF
  jrv = SetClipboardData( CF_BITMAP,ihbmp )
  IF( jrv == FALSE )THEN
    eInform = 'Unable to put bitmap data on clipboard'
    GOTO 9999
  END IF
  irv = CloseClipboard()
  IF( irv == FALSE )THEN
    eInform = 'Unable to close clipboard'
    GOTO 9999
  END IF
ELSE
  eInform = 'Unable to open Clipboard'
  GOTO 9999
END IF

cttl = 'Clipboard Copy'C
cmsg = 'Current plot copied to the Clipboard'C
irv   = MessageBox( iwnd,cmsg,cttl,MB_OK )

9998 RETURN

9999 CONTINUE

irv = GetLastError()
WRITE(eMessage,*)'API Error : ',irv
eRoutine = 'ClipboardBitmap'
nError = API_ERROR
CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
GOTO 9998

END
!***********************************************************************
!               CopyBitmapFile
!***********************************************************************
SUBROUTINE save_bmp( ihbmp,filename )

USE resource_fd
USE errorParam_fd
USE dib
USE SwinAPI
USE DefSize_fd
USE pcscipuf_fi, ONLY: NULL_POINTER

! This routine saves a copy of the window bitmap to a file

IMPLICIT NONE

INTEGER(POINTER_LEN),      INTENT( IN ) :: ihbmp    !Bitmap handle
CHARACTER(*),              INTENT( IN ) :: filename !File to save Bitmap

INTEGER(POINTER_LEN) ihpal,ihDIB,addrFile,jrv
INTEGER irv,bpp

CHARACTER(PATH_MAXLENGTH) file

CHARACTER(PATH_MAXLENGTH), EXTERNAL ::AddNull

INTEGER nError
CHARACTER(128) eMessage,eInform,eAction,eRoutine

eMessage = ' '
eInform  = ' '
eAction  = ' '

CALL GetNCARPalette( ihpal )

bpp = 8     !Bits/Pixel 8->256 colors

ihDIB = DibFromBitmap( ihbmp,BI_RGB,bpp,ihpal )
IF( ihDIB == NULL_POINTER )THEN
  eMessage = 'Failed to get DIB handle'
  GOTO 9999
END IF

file = AddNull( filename )
addrFile = ADDRESSOF(file)
irv = WriteDIB( addrFile,ihDIB )
IF( irv == FALSE )THEN
  eMessage = 'Failed to write DIB to file'
  CALL ReportFileName( eInform,'File=',filename )
  GOTO 9998
END IF

9998 CONTINUE

jrv = GlobalFree( ihDIB )
IF( jrv /= NULL_POINTER )THEN
  eMessage = 'Failed delete DIB from memory'
  WRITE(eInform,*)'DIB handle = ',ihDIB
  GOTO 9999
END IF

ihDIB = NULL_POINTER

9999 CONTINUE

IF( LEN_TRIM(eMessage) > 0 )THEN
  eRoutine = 'SaveBitmap'
  nError   = API_ERROR
  CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
END IF

RETURN
END
!***********************************************************************
!               RefreshIt
!***********************************************************************
SUBROUTINE RefreshIt( iwnd,ihbmp )

USE resource_fd
USE errorParam_fd
USE GUIerror_fi
USE pcscipuf_fi
USE animate_fi
USE SwinAPI
!
!     This routine copies the bitmap image back to the window
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd  !Window handle
INTEGER(POINTER_LEN), INTENT( IN ) :: ihbmp !Bitmap handle

INTEGER(POINTER_LEN) idc,jdc,jrv

LOGICAL lok
INTEGER iwid,ihgt,irv

TYPE( T_PAINTSTRUCT)     PS
TYPE( T_RECT ) Box

CHARACTER(128) eMessage,eInform

IF( .NOT.lanimate)CALL MousePause()

idc = BeginPaint( iwnd,PS )
IF( idc == NULL_POINTER )THEN
  eInform = 'Unable to get window DC'
  GOTO 9999
END IF

CALL ResetPalette( idc )

jdc = CreateCompatibleDC( idc )
IF( jdc == NULL_POINTER )THEN
  eInform = 'Unable to create memory DC'
  GOTO 9999
END IF

lok = GetClientRect( iwnd,Box )
IF( .NOT.lok )THEN
  eInform ='Unable to get client rectangle'
  GOTO 9999
END IF

iwid = Box%right  - Box%left
ihgt = Box%bottom - Box%top

CALL ResetPalette( jdc )

jrv = SelectObject( jdc,ihbmp )
IF( jrv == NULL_POINTER )THEN
  eInform = 'Unable to select object into DC'
  GOTO 9999
END IF

irv = StretchBlt( idc,0,0,iwid,ihgt,jdc,0,0,iwid,ihgt,SRCCOPY )
IF( irv == NULL )THEN
  eInform = 'Unable to copy bitmap'
  GOTO 9999
END IF

irv = EndPaint( iwnd,PS )

irv = DeleteDC( jdc )
IF( irv == NULL )THEN
  eInform = 'Unable to delete memory DC'
  GOTO 9999
END IF

IF( .NOT.lanimate )CALL MouseResume()

RETURN

9999 CONTINUE

irv = GetLastError()
WRITE(eMessage,*)'API Error : ',irv
CALL SetError( API_ERROR,eMessage,eInform,' ','RefreshIt' )
CALL ShowErrorMessage( NULL_POINTER )

RETURN
END
!***********************************************************************
!               EraseIt
!***********************************************************************
SUBROUTINE EraseIt( iwnd )

USE resource_fd
USE errorParam_fd
USE pcscipuf_fi
USE animate_fi
USE SwinAPI
!
!     This routine copies the bitmap image back to the window
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd !Window handle

INTEGER(POINTER_LEN) idc
INTEGER irv

TYPE( T_PAINTSTRUCT)     PS

CHARACTER(128) eMessage,eInform

IF( .NOT.lanimate )CALL MousePause()

idc = BeginPaint( iwnd,PS )
IF( idc == NULL_POINTER )THEN
  eInform = 'Unable to get window DC'
  GOTO 9999
END IF

CALL ResetPalette( idc )

irv = EndPaint( iwnd,PS )

IF( .NOT.lanimate )CALL MouseResume()

RETURN

9999 CONTINUE

irv = GetLastError()
WRITE(eMessage,*)'API Error : ',irv
CALL SetError( API_ERROR,eMessage,eInform,' ','EraseIt' )
CALL ShowErrorMessage( NULL_POINTER )

RETURN
END
!***********************************************************************
!               LoadBitmapFile
!***********************************************************************
SUBROUTINE load_bmp( iwnd,ihbmp,filename )

USE resource_fd
USE errorParam_fd
USE GUIerror_fi
USE SwinAPI
USE pcscipuf_fi, ONLY: NULL_POINTER
!
!     This routine reads a bitmap from a file
!
IMPLICIT NONE

INTEGER(4),PARAMETER :: MAXWRITE = 5120

INTEGER(POINTER_LEN), INTENT( IN  ) :: iwnd     !Window handle
INTEGER(POINTER_LEN), INTENT( OUT ) :: ihbmp    !Bitmap handle
CHARACTER(*),         INTENT( IN  ) :: filename !File to save Bitmap

LOGICAL lok
INTEGER iwid,ihgt,irv,n,iClrBits
INTEGER(POINTER_LEN) idc,ifile,ibit,jbit,jrv,size
INTEGER isizbmh
INTEGER,DIMENSION(MAXWRITE/4) :: ird

TYPE( T_BITMAPINFO          ) bmpi
TYPE( T_BITMAPFILEHEADER    ) hdr
TYPE( T_SECURITY_ATTRIBUTES ), POINTER :: fsa
TYPE( T_OVERLAPPED          ), POINTER :: fol

CHARACTER(128) eMessage, eInform

LOGICAL, EXTERNAL :: HasError

!---- Open the file

eInform = ' '

NULLIFY ( fsa )

irv = GENERIC_READ
ifile = CreateFile( filename,irv,0,fsa,OPEN_EXISTING, &
                    FILE_ATTRIBUTE_NORMAL,NULL_POINTER )
IF( ifile == INVALID_HANDLE_VALUE )THEN
  eInform = 'Unable to open file '//TRIM(filename)
  GOTO 9998
END IF

!---- Read file header

iwid = 0 !To eliminate warning from compiler
NULLIFY ( fol )
irv = SIZEOF(hdr)
lok = ReadFile( ifile,ADDRESSOF(hdr),irv,ADDRESSOF(iwid),fol )
IF( .NOT.lok )THEN
  eInform ='Unable to read file header'
  GOTO 9998
END IF

iClrBits = hdr%bfOffBits - SIZEOF(hdr) - SIZEOF(bmpi%bmiHeader)

!---- Read the header

isizbmh = SIZEOF(bmpi%bmiHeader) + iClrBits

lok = ReadFile( ifile,ADDRESSOF(bmpi),isizbmh,ADDRESSOF(iwid),fol )
IF( .NOT.lok )THEN
  eInform ='Unable to read bitmap header'
  GOTO 9998
END IF

bmpi%bmiHeader%biClrUsed      = 256
bmpi%bmiHeader%biClrImportant = 256

!---- Allocate memory to retrieve actual bits

irv = GMEM_FIXED .OR. GMEM_ZEROINIT

size = bmpi%bmiHeader%biSizeImage
ibit = GlobalAlloc( irv,size )
IF( ibit == NULL_POINTER )THEN
  eInform = 'Unable to allocate memory for bitmap bits'
  GOTO 9997
END IF

!---- Get Bitmap bits - first lock Bitmap bits memory to have pointer rather than handle

jbit = GlobalLock( ibit )
IF( jbit == NULL_POINTER )THEN
  eInform = 'Unable to lock bitmap bits memory'
  GOTO 9996
END IF

!---- Read bitmap bits

irv  = bmpi%bmiHeader%biSizeImage
n    = 0
ihgt = jbit

DO WHILE( irv > MAXWRITE )
  lok = ReadFile( ifile,ADDRESSOF(ird),MAXWRITE,ADDRESSOF(iwid),fol )
  IF( .NOT.lok )THEN
    eInform = 'Unable to Read bitmap bits'
    GOTO 9996
  END IF
  ihgt = ihgt + MAXWRITE
  irv  = irv - MAXWRITE
  n    = n + iwid
END DO

IF( irv > 0 )THEN
  lok = ReadFile( ifile,ADDRESSOF(ird),irv,ADDRESSOF(iwid),fol )
  IF( .NOT.lok )THEN
    eInform = 'Unable to Read bitmap bits'
    GOTO 9996
  END IF
  n = n + iwid
END IF

!---- Create Compatible Bitmap

CALL init_DC( iwnd,idc )
IF( hasError() )GOTO 9999
CALL ResetPalette( idc )

ihbmp = CreateDIBitmap( idc,bmpi%bmiHeader,CBM_INIT,jbit,bmpi,&
                        DIB_RGB_COLORS )
IF( ihbmp == NULL_POINTER )THEN
  eInform = 'Unable to set Bitmap bits'
  GOTO 9995
END IF

!---- Clean up

9995 CONTINUE
CALL dele_DC( idc )

9996 lok = GlobalUnlock( ibit )

9997 jrv = GlobalFree( ibit )

9998 irv = CloseHandle( ifile )

9999 CONTINUE
IF( LEN_TRIM(eInform) > 0 )THEN
  irv = GetLastError()
  WRITE(eMessage,*)'API Error : ',irv
  CALL SetError( API_ERROR,eMessage,eInform,' ','LoadBitmap' )
END IF

RETURN
END
!***********************************************************************
!               LoadPCX
!***********************************************************************
SUBROUTINE load_pcx( iwnd,ihbmp,filename )

USE resource_fd
USE errorParam_fd
USE SwinAPI
USE Pcx
!
!     This routine reads a bitmap from a file
!
IMPLICIT NONE

INTEGER(POINTER_LEN),      INTENT( IN  ) :: iwnd     !Window handle
INTEGER(POINTER_LEN),      INTENT( OUT ) :: ihbmp    !Bitmap handle
CHARACTER(*),              INTENT( IN  ) :: filename !File to save Bitmap

INTEGER iwid,ihgt,irv
INTEGER(POINTER_LEN) idc
INTEGER(POINTER_LEN) jdc

CHARACTER(128) eMessage, eInform

LOGICAL, EXTERNAL :: HasError

!---- Get bmp size

eInform = ' '

irv = pcxSize( filename,iwid,ihgt )
IF( irv == FALSE )THEN
  eInform = 'Failed to get PCX file bitmap size'
  GOTO 9999
END IF

!---- Create Compatible Bitmap

CALL init_DC( iwnd,idc )
IF( hasError() )GOTO 9999

jdc = GetDC( iwnd )

ihbmp = CreateCompatibleBitmap( jdc,iwid,ihgt )

CALL ResetPalette( jdc )
CALL ResetPalette( idc )

irv = pcxLoad( filename,idc,ihbmp )
IF( irv == FALSE )THEN
  eInform = 'Failed to load PCX file bitmap'
  GOTO 9995
END IF

!---- Clean up

9995 CONTINUE
CALL dele_DC( idc )
irv = ReleaseDC( iwnd,jdc )

9999 CONTINUE
IF( LEN_TRIM(eInform) > 0 )THEN
  irv = GetLastError()
  WRITE(eMessage,*)'API Error : ',irv
  CALL SetError( API_ERROR,eMessage,eInform,' ','LoadPCX' )
END IF

RETURN
END
!***********************************************************************
!               GetAnimateType
!***********************************************************************
SUBROUTINE get_animate_type( filename,btyp )

USE resource_fd
USE errorParam_fd
USE default_fd
USE dib
USE SwinAPI
USE Pcx
USE pcscipuf_fi, ONLY: NULL_POINTER

! This routine check a bitmap type from a file

IMPLICIT NONE

CHARACTER(*), INTENT( IN  ) :: filename !File to save Bitmap
INTEGER,      INTENT( OUT ) :: btyp

INTEGER iwid,ihgt,irv
INTEGER(POINTER_LEN) ig

!---- Get bmp type

btyp  = NOT_SET_I
iwid  = 0
ihgt  = 0

irv = pcxSize( filename,iwid,ihgt )
IF( irv /= FALSE )THEN
  btyp = BTYPE_PCX
  GOTO 9999
END IF

ig = OpenDIB( filename )
IF( ig /= NULL_POINTER )THEN
  btyp  = BTYPE_BMP
  iwid  = GlobalFree( ig )
  GOTO 9999
END IF

CALL SetError( OP_ERROR, &
               'Unable to determine bitmap file type', &
               'Select only Windows BMP or PCX files', &
               ' ','GetAnimationType' )

9999 CONTINUE

RETURN
END
