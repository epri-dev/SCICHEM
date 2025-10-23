!*******************************************************************************
!                     OpenFileDlg
!*******************************************************************************
SUBROUTINE OpenFileDlg( ccust,cbuff,name,path,cext,ctitle,hdlg,ihinst,ltest )

USE resource_fd
USE SwinAPI
USE guiAPI
USE DefSize_fd
USE pcscipuf_fi, ONLY: NULL_POINTER

!---- Driver for Open File Common Dialog Box

IMPLICIT NONE

CHARACTER(*)             ccust !Custom File Types
CHARACTER(*)             cbuff !Standard File Types
CHARACTER(*)             name !Filename buffer
CHARACTER(*)             path !Directory buffer
CHARACTER(*)             cext !Default Extension
CHARACTER(*)             ctitle !Dialog Box Title
INTEGER(POINTER_LEN)     ihinst !Instance handle
INTEGER(POINTER_LEN)     hdlg !calling dialog Window handle
LOGICAL                  ltest !Return flag

CHARACTER(PATH_MAXLENGTH)  coutb,title,template
CHARACTER(20)   ext
CHARACTER(1)    cnull
CHARACTER(PATH_MAXLENGTH)  filename, dirname
CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddNull, StripNull

TYPE( T_OPENFILENAME ) ofn

INTEGER nn,i,mm,iflags,nchf !c3.232,hwnd_focus

!---- Initialize

cnull = CHAR(0)
!3.232      hwnd_focus = GetFocus()

!---- Find zero based offsets to file name and extension

filename = StripNull(name) !Remove NULL if there is one
nchf     = LEN(TRIM(filename)) !Set length of String
nn       = 0 !Set Filename offset
i        = nchf + 1 !Start at end of string
DO WHILE( nn == 0 .AND. i > 1 ) !Search backward for : or \
  i = i - 1 !  decrement counter
  IF( name(i:i) == '\')nn = i !  check for backslash
  IF( name(i:i) == ':')nn = i !  check for :
END DO !

mm = nchf !Set extension offset
DO WHILE( mm > nn .AND. name(mm:mm) /= '.') !Search Backward for .
  mm = mm - 1 !  decrement counter
END DO !
mm = mm + 1 !

!---- Initialize Structure

filename = AddNull(name(nn+1:nchf)) !Initial filename
title    = 'Choose a '//ctitle//' File'//cnull !Title
ext      = AddNull(TRIM(cext)) !Default extension
coutb    = ccust !Custom Filter
i = LEN(TRIM(path)) !Remove trailing backslash for Windows3.1
IF( path(i:i) == '\' )THEN
  template = path(1:i-1)
  dirname = AddNull(TRIM(template))
ELSE
  dirname  = AddNull(TRIM(path)) !Initial directory
END IF
template = 'FILEOPEN'//cnull !Dialog Template
iflags   = IOR(OFN_HIDEREADONLY,IOR(OFN_PATHMUSTEXIST,IOR(  &
     OFN_ENABLETEMPLATE,OFN_ENABLEHOOK)))
!
ofn%lStructSize       = SIZEOF(ofn)
!3.232      ofn.hwndOwner         = hparent
ofn%hwndOwner         = hdlg
ofn%hInstance         = ihinst
ofn%lpstrFilter       = ADDRESSOF( cbuff )
ofn%lpstrCustomFilter = ADDRESSOF( coutb )
ofn%nMaxCustFilter    = LEN( coutb )
ofn%nFilterIndex      = 0
ofn%lpstrFile         = ADDRESSOF( filename )
ofn%nMaxFile          = LEN( filename )
ofn%lpstrFileTitle    = 0
ofn%nMaxFileTitle     = 0
ofn%lpstrInitialDir   = ADDRESSOF( dirname )
ofn%lpstrTitle        = ADDRESSOF( title )
ofn%Flags             = iflags
ofn%nFileOffset       = nn
ofn%nFileExtension    = mm
ofn%lpstrDefExt       = ADDRESSOF( ext )
ofn%lCustData         = 0
ofn%lpfnHook          = ADDRESSOF(OpenHook)
ofn%lpTemplateName    = ADDRESSOF( template )
ofn%pvReserved        = NULL_POINTER
ofn%dwReserved        = 0
ofn%FlagsEx           = 0

!---- Open File Common Dialog Box

ltest = GetOpenFileName( ofn )

!---- Sucessful return

IF( ltest )THEN
  dirname = StripNull(filename)
  CALL SplitName(dirname,name,path)
END IF
!

!3.232      iflags = SetFocus(VAL(hwnd_focus))

RETURN
END
