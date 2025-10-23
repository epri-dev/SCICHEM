!*******************************************************************************
!            Set string for real values
!*******************************************************************************
SUBROUTINE set_real_string( value,label,iflg )

USE default_fd
USE files_fi

IMPLICIT NONE

REAL,         INTENT( IN  ) :: value
CHARACTER(*), INTENT( OUT ) :: label
INTEGER,      INTENT( OUT ) :: iflg

CHARACTER(32)  number,string,exp
CHARACTER(1)   sign

INTEGER n,i

IF( value == DEF_VAL_R )THEN
  label = 'default'
  iflg  = -1

ELSE IF( value == NOT_SET_R )THEN
  label = ' '
  iflg  = 0

ELSE IF( value == DEFERRED_R )THEN
  label = 'deferred'
  iflg  = -2

ELSE
  WRITE(string,'(1PG13.6E2)')value
  IF( INDEX(string(1:13),'E') > 1 )WRITE(string,'(1PG11.4E2)')value
  string = ADJUSTL(string)

  IF( string(1:1) /= '-' )THEN
    sign = ' '
    n = 1
  ELSE
    sign = '-'
    n = 2
  END IF

  i = INDEX(string,'E')
  IF( i > 1 )THEN
    exp    = string(i:)
    number = string(n:i-1)
  ELSE
    exp    = ' '
    number = TRIM(string(n:))
  END IF

  string = sign//TRIM(number)//TRIM(exp)

  n = LEN_TRIM(number)
  i = INDEX(number,'.')
  DO WHILE( n > i+1 .AND. number(n:n) == '0' )
    n = n - 1
  END DO
  number(n+2:) = ' '

  IF( number(1:1) == '.' )THEN
    string = sign//'0'//TRIM(number)//TRIM(exp)
  ELSE
    string = sign//TRIM(number)//TRIM(exp)
  END IF

  label = TRIM(ADJUSTL(string))
  iflg = 1

END IF

RETURN
END
!*******************************************************************************
!            Set string for real values
!*******************************************************************************
SUBROUTINE set_real8_string( value,label,iflg )

USE default_fd
USE files_fi

IMPLICIT NONE

REAL(8),      INTENT( IN  ) :: value
CHARACTER(*), INTENT( OUT ) :: label
INTEGER,      INTENT( OUT ) :: iflg

CHARACTER(32)  number,string,exp
CHARACTER(1)   sign

INTEGER n,i

IF( value == DEF_VAL_D )THEN
  label = 'default'
  iflg  = -1

ELSE IF( value == NOT_SET_D )THEN
  label = ' '
  iflg  = 0

ELSE IF( value == DEFERRED_D )THEN
  label = 'deferred'
  iflg  = -2

ELSE
  WRITE(string,'(1PG18.11E2)')value
  IF( INDEX(string(1:18),'E') > 1 )WRITE(string,'(1PG16.9E2)')value
  string = ADJUSTL(string)

  IF( string(1:1) /= '-' )THEN
    sign = ' '
    n = 1
  ELSE
    sign = '-'
    n = 2
  END IF

  i = INDEX(string,'E')
  IF( i > 1 )THEN
    exp    = string(i:)
    number = string(n:i-1)
  ELSE
    exp    = ' '
    number = TRIM(string(n:))
  END IF

  string = sign//TRIM(number)//TRIM(exp)

  n = LEN_TRIM(number)
  i = INDEX(number,'.')
  DO WHILE( n > i+1 .AND. number(n:n) == '0' )
    n = n - 1
  END DO
  number(n+2:) = ' '

  IF( number(1:1) == '.' )THEN
    string = sign//'0'//TRIM(number)//TRIM(exp)
  ELSE
    string = sign//TRIM(number)//TRIM(exp)
  END IF

  label = TRIM(ADJUSTL(string))
  iflg = 1

END IF

RETURN
END
!*******************************************************************************
!            Set string for integer values
!*******************************************************************************
SUBROUTINE set_int_string( value,label,iflg )

USE default_fd

IMPLICIT NONE

INTEGER,      INTENT( IN  ) :: value
CHARACTER(*), INTENT( OUT ) :: label
INTEGER,      INTENT( OUT ) :: iflg

IF( value == DEF_VAL_I )THEN
  label = 'default'
  iflg  = -1
ELSE IF( value == NOT_SET_I )THEN
  label = ' '
  iflg  = 0
ELSE
  WRITE(label,*)value
  label = TRIM(ADJUSTL(label))
  iflg  = 1
END IF

RETURN
END
!*******************************************************************************
!            Test Error - Sets error to test error checking
!*******************************************************************************
SUBROUTINE TestError()

USE errorParam_fd

IMPLICIT NONE

CALL SetError( UK_ERROR,'Error Test',' ',' ','TestError' )

RETURN
END
!*******************************************************************************
!               Enable Control
!*******************************************************************************
SUBROUTINE EnableControl( iwnd,ID,iflag )

USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd    !Dialog Handle
INTEGER,              INTENT( IN ) :: ID      !Control ID
INTEGER,              INTENT( IN ) :: iflag   !Enable Flag

INTEGER irv
INTEGER(POINTER_LEN)ictrl

ictrl = GetDlgItem( iwnd,ID )
irv   = EnableWindow( ictrl,iflag )

RETURN
END
!*******************************************************************************
!               Enable Control (Logical)
!*******************************************************************************
SUBROUTINE EnableControlL( iwnd,ID,lflag )

USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd    !Dialog Handle
INTEGER,              INTENT( IN ) :: ID      !Control ID
LOGICAL,              INTENT( IN ) :: lflag   !Enable Flag

INTEGER irv, iflag
INTEGER(POINTER_LEN) ictrl

IF( lflag )THEN
  iflag = TRUE
ELSE
  iflag = FALSE
END IF

ictrl = GetDlgItem( iwnd,ID )
irv   = EnableWindow( ictrl,iflag )

RETURN
END
!*******************************************************************************
!               Show Control
!*******************************************************************************
SUBROUTINE ShowControl( iwnd,ID,iflag )

USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd    !Dialog Handle
INTEGER,              INTENT( IN ) :: ID      !Control ID
INTEGER,              INTENT( IN ) :: iflag   !Show Flag

INTEGER irv
INTEGER(POINTER_LEN) ictrl

ictrl = GetDlgItem( iwnd,ID )
irv   = ShowWindow( ictrl,iflag )

RETURN
END
!*******************************************************************************
!               Set Focus Control
!*******************************************************************************
SUBROUTINE SetFocusControl( iwnd,ID )

USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd    !Dialog Handle
INTEGER,              INTENT( IN ) :: ID      !Control ID

INTEGER irv
INTEGER(POINTER_LEN) ictrl

ictrl = GetDlgItem( iwnd,ID )
irv   = SetFocus( ictrl )

RETURN
END
!*******************************************************************************
!               Is Control Visible
!*******************************************************************************
SUBROUTINE IsControlVisible( iwnd,ID,lflag )

USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN  ) :: iwnd    !Dialog Handle
INTEGER,              INTENT( IN  ) :: ID      !Control ID
LOGICAL,              INTENT( OUT ) :: lflag   !Show Flag

INTEGER(POINTER_LEN) ictrl

ictrl = GetDlgItem( iwnd,ID )
lflag = IsWindowVisible( ictrl ) /= FALSE

RETURN
END
!*******************************************************************************
!               Is Control Enabled
!*******************************************************************************
SUBROUTINE IsControlEnabled( iwnd,ID,lflag )

USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN  ) :: iwnd    !Dialog Handle
INTEGER,              INTENT( IN  ) :: ID      !Control ID
LOGICAL,              INTENT( OUT ) :: lflag   !Show Flag

INTEGER(POINTER_LEN) ictrl

lflag = .FALSE.

IF( IsWindow(iwnd) /= FALSE )THEN
  ictrl = GetDlgItem( iwnd,ID )
  lflag = IsWindowEnabled( ictrl ) /= FALSE
END IF

RETURN
END
!*******************************************************************************
!               Set Control Text
!*******************************************************************************
SUBROUTINE SetControlText( iwnd,ID,string )

USE SwinAPI
USE DefSize_fd

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd    !Dialog Handle
INTEGER,              INTENT( IN ) :: ID      !Control ID
CHARACTER(*),         INTENT( IN ) :: string  !Text

CHARACTER(PATH_MAXLENGTH) text
INTEGER irv

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddNull

text = AddNull( TRIM(string) )
irv  = SendDlgItemMessage( iwnd,ID,WM_SETTEXT,0,ADDRESSOF(text) )

RETURN
END
!*******************************************************************************
!               Get Control Text
!*******************************************************************************
SUBROUTINE GetControlText( iwnd,ID,string )

USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN  ) :: iwnd    !Dialog Handle
INTEGER,              INTENT( IN  ) :: ID      !Control ID
CHARACTER(*),         INTENT( OUT ) :: string  !Text

CHARACTER(128)      text
INTEGER             irv
INTEGER(POINTER_LEN)sz

CHARACTER(128), EXTERNAL :: StripNull

sz = LEN(text)
irv = SendDlgItemMessage( iwnd,ID,WM_GETTEXT,sz,ADDRESSOF(text) )
string = StripNull( TRIM(text) )

RETURN
END
!*******************************************************************************
!               Set Control Font
!*******************************************************************************
SUBROUTINE SetControlFont( iwnd,ID,ifon )

USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd    !Dialog Handle
INTEGER,              INTENT( IN ) :: ID      !Control ID
INTEGER(POINTER_LEN), INTENT( IN ) :: ifon    !Font Handle

INTEGER irv

irv = SendDlgItemMessage( iwnd,ID,WM_SETFONT,ifon,0 )

RETURN
END
!*******************************************************************************
!               Set Control Icon
!*******************************************************************************
SUBROUTINE SetControlIcon( iwnd,ID,icon )

USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd    !Dialog Handle
INTEGER,              INTENT( IN ) :: ID      !Control ID
INTEGER(POINTER_LEN), INTENT( IN ) :: icon    !Icon Handle

INTEGER irv

irv = SendDlgItemMessage( iwnd,ID,STM_SETICON,icon,0 )

RETURN
END
!*******************************************************************************
!               Set Control Image
!*******************************************************************************
SUBROUTINE SetControlImage( iwnd,ID,ibmp )

USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd    !Dialog Handle
INTEGER,              INTENT( IN ) :: ID      !Control ID
INTEGER(POINTER_LEN), INTENT( IN ) :: ibmp    !Image Handle

INTEGER irv

irv = SendDlgItemMessage( iwnd,ID,STM_SETIMAGE,0,ibmp )

RETURN
END
!*******************************************************************************
!               Delete Control Image
!*******************************************************************************
SUBROUTINE DeleteControlImage( iwnd,ID )

USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd    !Dialog Handle
INTEGER,              INTENT( IN ) :: ID      !Control ID

INTEGER(POINTER_LEN) ibmp
INTEGER irv

ibmp = SendDlgItemMessage( iwnd,ID,STM_GETIMAGE,0,0 )

IF( ibmp /= 0 )irv = DeleteObject( ibmp )

RETURN
END
!*******************************************************************************
!               Enable Buttons
!*******************************************************************************
SUBROUTINE EnableButtons( iwnd,iflag,ns,nb )

USE resource_fd
USE winAPI_fd, ONLY: POINTER_LEN

IMPLICIT NONE

INTEGER(POINTER_LEN),   INTENT( IN ) :: iwnd    !Dialog Handle
INTEGER,                INTENT( IN ) :: nb      !No. of Buttons
INTEGER, DIMENSION(nb), INTENT( IN ) :: iflag   !Enable Flags
INTEGER,                INTENT( IN ) :: ns      !Start Button

INTEGER ID, i

ID = BUTTON_BASE + ns - 1
DO i = 1,nb
  ID = ID + 1
  CALL EnableControl( iwnd,ID,iflag(i) )
END DO

RETURN
END
!*******************************************************************************
!               Set Checks
!*******************************************************************************
SUBROUTINE SetChecks( iwnd,iflag,ns,nb )

USE resource_fd
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN),   INTENT( IN ) :: iwnd    !Dialog Handle
INTEGER,                INTENT( IN ) :: nb      !No. of Buttons
INTEGER, DIMENSION(nb), INTENT( IN ) :: iflag   !Enable Flags
INTEGER,                INTENT( IN ) :: ns      !Start Button

INTEGER ID, i, irv

ID = CHECK_BASE + ns - 1
DO i = 1,nb
  ID = ID + 1
  irv = CheckDlgButton( iwnd,ID,iflag(i) )
END DO

RETURN
END
!*******************************************************************************
!               Set Radio
!*******************************************************************************
SUBROUTINE SetRadios( iwnd,iflag,nf,ns,nb )

USE resource_fd
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN),   INTENT( IN ) :: iwnd    !Dialog Handle
INTEGER,                INTENT( IN ) :: nb      !No. of Buttons
INTEGER, DIMENSION(nb), INTENT( IN ) :: iflag   !Enable Flags
INTEGER, DIMENSION(nb), INTENT( IN ) :: nf      !Number of Choices
INTEGER,                INTENT( IN ) :: ns      !Start Button

INTEGER i, is, ie, ix, irv

is = RADIO_BASE+RADIO_GROUP*(ns-2) + 1
DO i = 1,nb
  is = is + RADIO_GROUP
  ie = is + nf(i) - 1
  IF( iflag(i) <= 0 )THEN !Turn them all off
    DO ix = is,ie
      irv = CheckDlgButton( iwnd,ix,0 )
    END DO
  ELSE !Normal course
    ix = is + iflag(i) - 1
    ix = MAX(ix,is)
    ix = MIN(ix,ie)
    irv = CheckRadioButton( iwnd,is,ie,ix )
  END IF
END DO

RETURN
END
!*******************************************************************************
!               Set Edits - Real
!*******************************************************************************
SUBROUTINE SetEditRs( iwnd,r,ns,nb )

USE resource_fd
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN),INTENT( IN ) :: iwnd    !Dialog Handle
INTEGER,             INTENT( IN ) :: nb      !No. of Buttons
REAL, DIMENSION(nb), INTENT( IN ) :: r       !Values
INTEGER,             INTENT( IN ) :: ns      !Start Button

INTEGER ID, i, irv
CHARACTER(128) ctmp

ID = REAL_BASE + ns - 1
DO i = 1,nb
  ID = ID + 1
  CALL set_real_string( r(i),ctmp,irv )
  ctmp = TRIM(ctmp)//CHAR(0)
  irv = SetDlgItemText( iwnd,ID,ctmp )
END DO

RETURN
END
!*******************************************************************************
!               Set Edits - Real
!*******************************************************************************
SUBROUTINE SetEditR8s( iwnd,r,ns,nb )

USE resource_fd
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN),   INTENT( IN ) :: iwnd    !Dialog Handle
INTEGER,                INTENT( IN ) :: nb      !No. of Buttons
REAL(8), DIMENSION(nb), INTENT( IN ) :: r       !Values
INTEGER,                INTENT( IN ) :: ns      !Start Button

INTEGER ID, i, irv
CHARACTER(128) ctmp

ID = DOUBLE_BASE + ns - 1
DO i = 1,nb
  ID = ID + 1
  CALL set_real8_string( r(i),ctmp,irv )
  ctmp = TRIM(ctmp)//CHAR(0)
  irv = SetDlgItemText( iwnd,ID,ctmp )
END DO

RETURN
END
!*******************************************************************************
!               Clear Edits - Real
!*******************************************************************************
SUBROUTINE ClearEditRs( iwnd,ns,nb )

USE resource_fd
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd    !Dialog Handle
INTEGER,              INTENT( IN ) :: ns      !Start Button
INTEGER,              INTENT( IN ) :: nb      !No. of Buttons

INTEGER ID, i, irv
CHARACTER(1),PARAMETER :: CNULL = ' '

ID = REAL_BASE + ns - 1
DO i = 1,nb
  ID = ID + 1
  irv = SetDlgItemText( iwnd,ID,CNULL )
END DO

RETURN
END
!*******************************************************************************
!               Clear Edits - Real
!*******************************************************************************
SUBROUTINE ClearEditR8s( iwnd,ns,nb )

USE resource_fd
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd    !Dialog Handle
INTEGER,              INTENT( IN ) :: ns      !Start Button
INTEGER,              INTENT( IN ) :: nb      !No. of Buttons

INTEGER ID, i, irv
CHARACTER(1),PARAMETER :: CNULL = ' '

ID = DOUBLE_BASE + ns - 1
DO i = 1,nb
  ID = ID + 1
  irv = SetDlgItemText( iwnd,ID,CNULL )
END DO

RETURN
END
!*******************************************************************************
!               Set Edits - Integer
!*******************************************************************************
SUBROUTINE SetEditIs( iwnd,ival,ns,nb )

USE resource_fd
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN),   INTENT( IN ) :: iwnd    !Dialog Handle
INTEGER,                INTENT( IN ) :: nb      !No. of Buttons
INTEGER, DIMENSION(nb), INTENT( IN ) :: ival    !Values
INTEGER,                INTENT( IN ) :: ns      !Start Button

INTEGER ID, i, irv
CHARACTER(128) c1, c2

CHARACTER(128), EXTERNAL :: AddNull

ID = INT_BASE + ns - 1
DO i = 1,nb
  ID = ID + 1
  SELECT CASE( ival(i) )
    CASE( NOT_SET_I )
      c2 = ' '
    CASE( DEF_VAL_I )
      c2 = 'default'
    CASE( DEFERRED_I )
      c2 = 'deferred'
    CASE DEFAULT
      WRITE(c1,*)ival(i)
      c2 = ADJUSTL(TRIM(c1)) !  Strip leading blanks
  END SELECT
  c1 = AddNull( TRIM(c2) ) !Add Trailing NULL Char
  irv = SetDlgItemText( iwnd,ID,c1 )
END DO

RETURN
END
!*******************************************************************************
!               Set Edits - Formatted Integer
!*******************************************************************************
SUBROUTINE FormatEditIs( iwnd,form,ival,ns,nb )

USE resource_fd
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN),   INTENT( IN ) :: iwnd    !Dialog Handle
CHARACTER(*),           INTENT( IN ) :: form    !FORTRAN Format statement
INTEGER,                INTENT( IN ) :: nb      !No. of Buttons
INTEGER, DIMENSION(nb), INTENT( IN ) :: ival    !Values
INTEGER,                INTENT( IN ) :: ns      !Start Button

INTEGER ID, i, irv
CHARACTER(128) c1, c2

CHARACTER(128), EXTERNAL :: AddNull

ID = INT_BASE + ns - 1
DO i = 1,nb
  ID = ID + 1
  SELECT CASE( ival(i) )
    CASE( NOT_SET_I )
      c2 = ' ' !
    CASE( DEF_VAL_I )
      c2 = 'default'
    CASE( DEFERRED_I )
      c2 = 'deferred'
    CASE DEFAULT
      WRITE(c1,form)ival(i)
      c2 = ADJUSTL(TRIM(c1))
  END SELECT
  c1 = AddNull( TRIM(c2) )
  irv = SetDlgItemText( iwnd,ID,c1 )
END DO

RETURN
END
!*******************************************************************************
!               Set Edits - Text
!*******************************************************************************
SUBROUTINE SetEditTs( iwnd,string,ns,nb )

USE resource_fd
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN),        INTENT( IN ) :: iwnd    !Dialog Handle
INTEGER,                     INTENT( IN ) :: nb      !No. of Buttons
CHARACTER(*), DIMENSION(nb), INTENT( IN ) :: string  !Strings
INTEGER,                     INTENT( IN ) :: ns      !Start Button

INTEGER ID, i, irv

ID = EDIT_BASE + ns - 1
DO i = 1,nb
  ID = ID + 1
  irv = SetDlgItemText( iwnd,ID,string(i) )
END DO

RETURN
END
!*******************************************************************************
!                ClearList
!*******************************************************************************
SUBROUTINE ClearList( iwnd,ilst )

USE resource_fd
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd !Dialog Handle
INTEGER,              INTENT( IN ) :: ilst !List ID

INTEGER imsg, irv
INTEGER(POINTER_LEN) ictrl

!---- COMBO or LIST BOX

IF( ilst-COMBO_BASE < 100 )THEN
  imsg = CB_RESETCONTENT
ELSE
  imsg = LB_RESETCONTENT
END IF

!---- Get Control handle

ictrl = GetDlgItem( iwnd,ilst )

!---- Send Message

irv = SendMessage( ictrl,imsg,0,0 )

RETURN
END
!*******************************************************************************
!                AddList
!*******************************************************************************
SUBROUTINE AddList( iwnd,ilst,indx,string,iret )

USE resource_fd
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN  ) :: iwnd   !Dialog Handle
INTEGER,              INTENT( IN  ) :: ilst   !List ID
INTEGER,              INTENT( IN  ) :: indx   !Add index
CHARACTER(*),         INTENT( IN  ) :: string !Add string
INTEGER,              INTENT( OUT ) :: iret   !Return Value

INTEGER imsg, irv
INTEGER(POINTER_LEN) ictrl, jndx
CHARACTER(128) msg

CHARACTER(128), EXTERNAL :: AddNull

!---- COMBO or LIST BOX, ADD or INSERT

irv = ilst - COMBO_BASE
IF( indx >= -1 )THEN !Insert (-1 -> end)
  jndx = indx
  IF( irv < 100 )THEN
    imsg = CB_INSERTSTRING
  ELSE
    imsg = LB_INSERTSTRING
  END IF
ELSE !Add - Sorted list
  jndx = 0
  IF( irv < 100 )THEN
    imsg = CB_ADDSTRING
  ELSE
    imsg = LB_ADDSTRING
  END IF
END IF

!---- Build String

msg = AddNull( TRIM(string) )

!---- Get Control handle

ictrl = GetDlgItem( iwnd,ilst )

!---- Send Message

iret = SendMessage( ictrl,imsg,jndx,ADDRESSOF(msg) )

IF( iret < 0 )CALL ListError( iwnd,imsg,iret )

RETURN
END
!*******************************************************************************
!                GetListData
!*******************************************************************************
SUBROUTINE GetListData( iwnd,ilst,indx,idata,iret )

USE resource_fd
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN  ) :: iwnd   !Dialog Handle
INTEGER,              INTENT( IN  ) :: ilst   !List ID
INTEGER,              INTENT( IN  ) :: indx   !Add index
INTEGER,              INTENT( OUT ) :: idata  !String data
INTEGER,              INTENT( OUT ) :: iret   !Return Value

INTEGER imsg, irv
INTEGER(POINTER_LEN) ictrl,iindx

!---- COMBO or LIST BOX, ADD or INSERT

irv = ilst - COMBO_BASE
IF( irv < 100 )THEN
  imsg = CB_GETITEMDATA
ELSE
  imsg = LB_GETITEMDATA
END IF

!---- Get Control handle

ictrl = GetDlgItem( iwnd,ilst )

!---- Send Message

iindx = indx
iret = SendMessage( ictrl,imsg,iindx,0 )

IF( iret < 0 )THEN
  CALL ListError( iwnd,imsg,iret )
  idata = 0
ELSE
  idata = iret
  iret  = 0
END IF

RETURN
END
!*******************************************************************************
!                SetListData
!*******************************************************************************
SUBROUTINE SetListData( iwnd,ilst,indx,idata,iret )

USE resource_fd
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN  ) :: iwnd   !Dialog Handle
INTEGER,              INTENT( IN  ) :: ilst   !List ID
INTEGER,              INTENT( IN  ) :: indx   !Add index
INTEGER,              INTENT( IN  ) :: idata  !String data
INTEGER,              INTENT( OUT ) :: iret   !Return Value

INTEGER imsg, irv
INTEGER(POINTER_LEN) ictrl,iindx,iidata

!---- COMBO or LIST BOX, ADD or INSERT

irv = ilst - COMBO_BASE
IF( irv < 100 )THEN
  imsg = CB_SETITEMDATA
ELSE
  imsg = LB_SETITEMDATA
END IF

!---- Get Control handle

ictrl = GetDlgItem( iwnd,ilst )

!---- Send Message

iindx = indx
iidata = idata
iret = SendMessage( ictrl,imsg,iindx,iidata )

IF( iret < 0 )CALL ListError( iwnd,imsg,iret )

RETURN
END
!*******************************************************************************
!                ListError
!*******************************************************************************
SUBROUTINE ListError( iwnd,imsg,ierr )

USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd   !Dialog Handle
INTEGER,              INTENT( IN ) :: imsg   !Message ID
INTEGER,              INTENT( IN ) :: ierr   !Error ID

INTEGER irv

CHARACTER(128) msg, ctmp

CHARACTER(128), EXTERNAL :: AddNull

IF( ierr == LB_ERR )THEN
  WRITE(ctmp,*)'LIST/COMBO Error : Message=',imsg
  msg = AddNull( TRIM(ctmp) )
ELSE
  WRITE(ctmp,*)'LIST/COMBO Storage Space Error : Message=',imsg
  msg = AddNull( TRIM(ctmp) )
END IF

ctmp = AddNull( 'LIST/COMBO Utility Routines' )

irv = MessageBox( iwnd,msg,ctmp,0 )

RETURN
END
!*******************************************************************************
!                DeleteList
!*******************************************************************************
SUBROUTINE DeleteList( iwnd,ilst,indx,iret )

USE resource_fd
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN  ) :: iwnd   !Dialog Handle
INTEGER,              INTENT( IN  ) :: ilst   !List ID
INTEGER,              INTENT( IN  ) :: indx   !Delete index
INTEGER,              INTENT( OUT ) :: iret   !Return Value

INTEGER imsg
INTEGER(POINTER_LEN) ictrl,iindx

!---- COMBO or LIST BOX

IF( ilst - COMBO_BASE < 100 )THEN
  imsg = CB_DELETESTRING
ELSE
  imsg = LB_DELETESTRING
END IF

!---- Get Control handle

ictrl = GetDlgItem( iwnd,ilst )

!---- Send Message

iindx = indx
iret = SendMessage( ictrl,imsg,iindx,0 )

IF( iret < 0 )CALL ListError( iwnd,imsg,iret )

RETURN
END
!*******************************************************************************
!                ListCount
!*******************************************************************************
SUBROUTINE ListCount( iwnd,ilst,iret )

USE resource_fd
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN  ) :: iwnd   !Dialog Handle
INTEGER,              INTENT( IN  ) :: ilst   !List ID
INTEGER,              INTENT( OUT ) :: iret   !Return Value

INTEGER imsg
INTEGER(POINTER_LEN) ictrl

!---- COMBO or LIST BOX

IF( ilst - COMBO_BASE < 100 )THEN
  imsg = CB_GETCOUNT
ELSE
  imsg = LB_GETCOUNT
END IF

!---- Get Control handle

ictrl = GetDlgItem( iwnd,ilst )

!---- Send Message

iret = SendMessage( ictrl,imsg,0,0 )

IF( iret < 0 )THEN
  CALL ListError( iwnd,imsg,iret )
  iret = 0
END IF

RETURN
END
!*******************************************************************************
!                ListSelCount
!*******************************************************************************
SUBROUTINE ListSelCount( iwnd,ilst,iret )

USE resource_fd
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN  ) :: iwnd   !Dialog Handle
INTEGER,              INTENT( IN  ) :: ilst   !List ID
INTEGER,              INTENT( OUT ) :: iret   !Return Value

INTEGER imsg, jlst
INTEGER(POINTER_LEN) ictrl
LOGICAL lcombo

!---- COMBO or LIST BOX

lcombo = ilst - COMBO_BASE < 100
IF( lcombo )THEN
  imsg = CB_GETCURSEL
ELSE
  imsg = LB_GETSELCOUNT
END IF

jlst = ABS(ilst)

!---- Get Control handle

ictrl = GetDlgItem( iwnd,jlst )

!---- Send Message

iret = SendMessage( ictrl,imsg,0,0 )

!---- Check Return
!       Combo boxes only have a single selected item so checked to see
!       what was selected. If successful the 1 item selected else 0 items

IF( lcombo )THEN
  IF( iret < 0 )THEN
    iret = 0
  ELSE
    iret = 1
  END IF
ELSE
  IF( iret < 0 )THEN
    CALL ListError( iwnd,imsg,iret )
    iret = 0
  END IF
END IF

RETURN
END
!*******************************************************************************
!                GetList
!*******************************************************************************
SUBROUTINE GetList( iwnd,ilst,max,string,iret )

USE resource_fd
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN),         INTENT( IN  ) :: iwnd   !Dialog Handle
INTEGER,                      INTENT( IN  ) :: ilst   !List ID
INTEGER,                      INTENT( IN  ) :: max    !Size of String Array
CHARACTER(*), DIMENSION(max), INTENT( IN  ) :: string !Array of Strings
INTEGER,                      INTENT( OUT ) :: iret   !Return Value - Number in List

INTEGER imsg, nlst, i, jmsg, smax
INTEGER(POINTER_LEN) ictrl, ii, addrString

!---- COMBO or LIST BOX

IF( ilst - COMBO_BASE < 100 )THEN
  imsg = CB_GETLBTEXT
  jmsg = CB_GETLBTEXTLEN
ELSE
  imsg = LB_GETTEXT
  jmsg = LB_GETTEXTLEN
END IF

!---- Get LIST Count

CALL ListCount( iwnd,ilst,nlst )
IF( nlst > max )THEN
  iret = LB_ERRSPACE
  GOTO 9999
END IF

!---- Get Control handle

ictrl = GetDlgItem( iwnd,ilst )

!---- Send Message to get Text

DO i = 1,nlst

  smax = LEN(string(i))
  addrString = ADDRESSOF(string(i))

  ii = i-1
  iret = SendMessage( ictrl,jmsg,ii,0 )
  IF( iret < 0 )THEN
    GOTO 9999
  ELSE IF( iret > smax )THEN
    iret = LB_ERRSPACE
    GOTO 9999
  END IF

  iret = SendMessage( ictrl,imsg,ii,addrString )
  IF( iret < 0 )GOTO 9999

END DO

9999 CONTINUE

IF( iret < 0 )THEN
  CALL ListError( iwnd,imsg,iret )
  iret = 0
ELSE
  iret = nlst
END IF

RETURN
END
!*******************************************************************************
!                GetListSel
!*******************************************************************************
SUBROUTINE GetListSel( iwnd,ilst,maxlst,indx,iret )

USE resource_fd
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN),      INTENT( IN  ) :: iwnd   !Dialog Handle
INTEGER,                   INTENT( IN  ) :: ilst   !List ID
INTEGER,                   INTENT( IN  ) :: maxlst !Size of index Array
INTEGER,DIMENSION(maxlst), INTENT( OUT ) :: indx   !Array of indices
INTEGER,                   INTENT( OUT ) :: iret   !Return Value - Number in List

INTEGER imsg, nlst, jlst
INTEGER(POINTER_LEN) ictrl,imax
LOGICAL lcombo

!---- COMBO or LIST BOX

lcombo = ilst - COMBO_BASE < 100
IF( lcombo )THEN
  imsg = CB_GETCURSEL
ELSE
  imsg = LB_GETSELITEMS
END IF

jlst = ABS(ilst)

!---- Get LIST Count

CALL ListSelCount( iwnd,ilst,nlst )
IF( nlst > maxlst )THEN
  iret = LB_ERRSPACE
  GOTO 9999
ELSE IF( nlst <= 0 )THEN
  iret = 0
  GOTO 9999
END IF

!---- Get Control handle

ictrl = GetDlgItem( iwnd,jlst )

!---- Send Message to get indices

IF( lcombo )THEN
  iret = SendMessage( ictrl,imsg,0,0 )
  IF( iret >= 0 )indx(1) = iret
ELSE
  imax = maxlst
  iret = SendMessage( ictrl,imsg,imax,ADDRESSOF(indx) )
  IF( iret /= nlst )iret = LB_ERR
END IF

9999 CONTINUE

IF( iret < 0 )THEN
  CALL ListError( iwnd,imsg,iret )
  iret = 0
ELSE
  iret = MAX(nlst,0)
END IF

RETURN
END
!*******************************************************************************
!                GetListCurSel
!*******************************************************************************
SUBROUTINE GetListCurSel( iwnd,ilst,indx )

USE resource_fd
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN  ) :: iwnd   !Dialog Handle
INTEGER,              INTENT( IN  ) :: ilst   !List ID
INTEGER,              INTENT( OUT ) :: indx   !Index

INTEGER imsg, iret
INTEGER(POINTER_LEN) ictrl
LOGICAL lcombo

!---- COMBO or LIST BOX

lcombo = ilst - COMBO_BASE < 100
IF( lcombo )THEN
  imsg = CB_GETCURSEL
ELSE
  imsg = LB_GETCURSEL
END IF

!---- Get Control handle

ictrl = GetDlgItem( iwnd,ilst )

!---- Send Message to get indices

iret = SendMessage( ictrl,imsg,0,0 )
IF( iret >= 0 )THEN
  indx = iret
ELSE
  indx = -1
END IF

RETURN
END
!*******************************************************************************
!                SetListCurSel
!*******************************************************************************
SUBROUTINE SetListCurSel( iwnd,ilst,indx )

USE resource_fd
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd   !Dialog Handle
INTEGER,              INTENT( IN ) :: ilst   !List ID
INTEGER,              INTENT( IN ) :: indx   !Index


INTEGER imsg, iret
INTEGER(POINTER_LEN) ictrl, iindx
LOGICAL lcombo

!---- COMBO or LIST BOX

lcombo = ilst - COMBO_BASE < 100
IF( lcombo )THEN
  imsg = CB_SETCURSEL
ELSE
  imsg = LB_SETCURSEL
END IF

!---- Get Control handle

ictrl = GetDlgItem( iwnd,ilst )

!---- Send Message to get indices

iindx = indx
iret = SendMessage( ictrl,imsg,iindx,0 )
IF( iret < 0 .AND. indx >= 0 )CALL ListError( iwnd,imsg,iret )

RETURN
END
!*******************************************************************************
!                GetListItem
!*******************************************************************************
SUBROUTINE GetListItem( iwnd,ilst,indx,string,iret )

USE resource_fd
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN  ) :: iwnd   !Dialog Handle
INTEGER,              INTENT( IN  ) :: ilst   !List ID
INTEGER,              INTENT( IN  ) :: indx   !Index
CHARACTER(*),         INTENT( OUT ) :: string !Strings
INTEGER,              INTENT( OUT ) :: iret   !Return Value

INTEGER imsg, max, jmsg, jlst
INTEGER(POINTER_LEN) ictrl, iindx

!---- COMBO or LIST BOX

IF( ilst - COMBO_BASE < 100 )THEN
  imsg = CB_GETLBTEXT
  jmsg = CB_GETLBTEXTLEN
ELSE
  imsg = LB_GETTEXT
  jmsg = LB_GETTEXTLEN
END IF

jlst = ABS(ilst)

!---- Get Control handle

ictrl = GetDlgItem( iwnd,jlst )

!---- Send Message to get Text

max = LEN(string)
string(1:max) = ' '

iindx = indx
iret = SendMessage( ictrl,jmsg,iindx,0 )

IF( iret < 0 )THEN
  GOTO 9999
ELSE IF( iret > max )THEN
  iret = LB_ERRSPACE
  GOTO 9999
END IF

iret = SendMessage( ictrl,imsg,iindx,ADDRESSOF(string) )

9999 CONTINUE

IF( iret < 0 )THEN
  CALL ListError( iwnd,imsg,iret )
  iret = 0
END IF

RETURN
END
!*******************************************************************************
!                SetListSel
!*******************************************************************************
SUBROUTINE SetListSel( iwnd,ilst,indx,iret )

USE resource_fd
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN),  INTENT( IN  ) :: iwnd   !Dialog Handle
INTEGER,               INTENT( IN  ) :: ilst   !List ID
INTEGER, DIMENSION(2), INTENT( IN  ) :: indx   !Index range
INTEGER,               INTENT( OUT ) :: iret   !Return Value

INTEGER imsg
INTEGER(POINTER_LEN) ictrl, i1, i2

!---- COMBO or LIST BOX

IF( ilst - COMBO_BASE < 100 )THEN
  imsg = CB_SETCURSEL
  i1   = indx(1)
  i2   = 0
ELSE
  imsg = LB_SELITEMRANGE
  i1   = TRUE
  CALL make_param( i2,indx(2),indx(1) )
END IF

!---- Get Control handle

ictrl = GetDlgItem( iwnd,ilst )

!---- Send Message

iret = SendMessage( ictrl,imsg,i1,i2 )

9999 CONTINUE

IF( iret < 0 )THEN
  IF( i1 >= 0 )CALL ListError( iwnd,imsg,iret )
  iret = 0
END IF

RETURN
END
!*******************************************************************************
!                SetListSelString
!*******************************************************************************
SUBROUTINE SetListSelString( iwnd,ilst,string,iret )

USE resource_fd
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN  ) :: iwnd   !Dialog Handle
INTEGER,              INTENT( IN  ) :: ilst   !List ID
CHARACTER(*),         INTENT( IN  ) :: string !String to select
INTEGER,              INTENT( OUT ) :: iret   !Return Value

INTEGER imsg, jlst
INTEGER(POINTER_LEN) ictrl
CHARACTER(128) selectString

CHARACTER(128), EXTERNAL :: AddNull

!---- COMBO or LIST BOX

IF( ilst - COMBO_BASE < 100 .OR. ilst < 0 )THEN
  imsg = CB_SELECTSTRING
ELSE
  imsg = LB_SELECTSTRING
END IF

jlst = ABS(ilst)

!---- Get Control handle

ictrl = GetDlgItem( iwnd,jlst )

!---- Build Search string

selectString = AddNull( TRIM(string) )

!---- Send Message

iret = SendMessage( ictrl,imsg,-1,ADDRESSOF(selectString) )

9999 CONTINUE

IF( iret < 0 )THEN
  CALL ListError( iwnd,imsg,iret )
  iret = 0
ELSE
  iret = iret + 1
END IF

RETURN
END
!*******************************************************************************
!                FindListStringEx
!*******************************************************************************
SUBROUTINE FindListStringEx( iwnd,ilst,string,iret )

USE resource_fd
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN),  INTENT( IN  ) :: iwnd   !Dialog Handle
INTEGER,               INTENT( IN  ) :: ilst   !List ID
CHARACTER(*),          INTENT( IN  ) :: string !String to select
INTEGER,               INTENT( OUT ) :: iret   !Return Value - item selected

INTEGER imsg, jlst
INTEGER(POINTER_LEN) ictrl
CHARACTER(128) selectString

CHARACTER(128), EXTERNAL :: AddNull

!---- COMBO or LIST BOX

IF( ilst - COMBO_BASE < 100 .OR. ilst < 0 )THEN
  imsg = CB_FINDSTRINGEXACT
ELSE
  imsg = LB_FINDSTRINGEXACT
END IF

jlst = ABS(ilst)

!---- Get Control handle

ictrl = GetDlgItem( iwnd,jlst )

!---- Build Search string

selectString = AddNull( TRIM(string) )

!---- Send Message

iret = SendMessage( ictrl,imsg,-1,ADDRESSOF(selectString) )

9999 CONTINUE

IF( iret < 0 )THEN
  iret = 0
ELSE
  iret = iret + 1
END IF

RETURN
END
!*******************************************************************************
!                CallButton
!*******************************************************************************
SUBROUTINE CallButton( iwnd,ibtn,iret )

USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN  ) :: iwnd   !Dialog Handle
INTEGER,              INTENT( IN  ) :: ibtn   !Button ID
INTEGER,              INTENT( OUT ) :: iret   !Return Value

INTEGER(POINTER_LEN) iibtn

!---- Send Message

iibtn = ibtn
iret = SendMessage( iwnd,WM_COMMAND,iibtn,0 )

RETURN
END
!*******************************************************************************
!                PushButton
!*******************************************************************************
SUBROUTINE PushButton( iwnd,ictrl,ibtn,iret )

USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN  ) :: iwnd   !Dialog Handle
INTEGER             , INTENT( IN  ) :: ictrl  !Caller ID
INTEGER,              INTENT( IN  ) :: ibtn   !Button ID
INTEGER,              INTENT( OUT ) :: iret   !Return Value

INTEGER(POINTER_LEN) jbtn,iibtn

!---- Get handle of Calling Control

IF( ictrl <= 0 )THEN
  jbtn = 0
ELSE
  jbtn = GetDlgItem( iwnd,ictrl )
END IF

!---- Send Message

iibtn = ibtn
iret = SendMessage( iwnd,WM_COMMAND,iibtn,jbtn )

RETURN

END
!*******************************************************************************
!                SetFileString
!*******************************************************************************
SUBROUTINE SetFileString( nmax,infile,outfile )

USE DefSize_fd

IMPLICIT NONE

INTEGER,      INTENT( IN  ) :: nmax
CHARACTER(*), INTENT( IN  ) :: infile
CHARACTER(*), INTENT( OUT ) :: outfile

INTEGER nch, i, j, head, tail, middle

CHARACTER(1)   backslash, colon
CHARACTER(PATH_MAXLENGTH) device, path, file, string

backslash = '\'
colon     = ':'

IF( nmax <= 3 )THEN
  DO i = 1,nmax
    outfile(i:i) = '.'
  END DO
  RETURN
END IF

i = LEN_TRIM(infile)

IF( i <= nmax )THEN
  outfile = infile
  RETURN
END IF

CALL SplitName( infile,file,path )

head = INDEX(path,colon)
IF( head <= 0 )THEN
  head = INDEX(path,backslash)
  IF( head <= 0 )THEN
    head = 3
    device = ' '
  END IF
END IF

device = path(1:head)
string = path(head+1:)
path   = string
tail   = LEN_TRIM(file)
middle = LEN_TRIM(path)

IF( tail > nmax )THEN
  nch = nmax - 3
  i   = nch/2
  j   = tail - (nch - i) - 1
  outfile = file(1:i)//'...'//file(j:)
  RETURN
END IF

IF( tail+head+4 >= nmax .AND. head > 0 )THEN
  nch = nmax - head - 7
  i   = nch/2
  j   = tail - (nch - i) - 1
  outfile = device(1:head)//'...\'//file(1:i)//'...'//file(j:)
  RETURN
END IF

nch = nmax - head - tail - 4
IF( head > 0 )THEN
  outfile = device(1:head)//path(1:nch)//'...\'//file(1:tail)
ELSE
  outfile = path(1:nch)//'...\'//file(1:tail)
END IF

RETURN
END
!*******************************************************************************
!                MyUpdateWindow
!*******************************************************************************
SUBROUTINE MyUpdateWindow( windowHandle,eraseBackground )

USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: windowHandle
LOGICAL,              INTENT( IN ) :: eraseBackground

TYPE( T_RECT) windowRect

INTEGER RETURNValue

RETURNValue = GetClientRect( windowHandle,windowRect )

IF( eraseBackground )THEN
  RETURNValue = InvalidateRect( windowHandle,windowRect,TRUE )
ELSE
  RETURNValue = InvalidateRect( windowHandle,windowRect,FALSE )
END IF

RETURNValue = UpdateWindow( windowHandle )

RETURN
END
