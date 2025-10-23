!*******************************************************************************
!                     GetFile
!*******************************************************************************
SUBROUTINE GetFile( id,ib,il,lok,iwnd_db,filename )

USE resource_fd
USE reldef_fd
USE mettype_fd
USE tooluser_fd
USE files_fi
USE errorParam_fd
USE pcscipuf_fi
USE create_fi
USE dialog_fi
USE restart_fi
USE SwinAPI

!     This routine uses the OpenFile Common Dialog to input various input
!     filenames and paths

IMPLICIT NONE

INTEGER              id !Dialog ID
INTEGER              ib !Button ID
INTEGER              il !Dialog Data level
LOGICAL              lok !Return STATUS flag
INTEGER(POINTER_LEN) iwnd_db !Dialog Handle
CHARACTER(*)         filename

CHARACTER(PATH_MAXLENGTH) ctmpf, ctmpd, ccust, inname
CHARACTER(PATH_MAXLENGTH) cbuff
CHARACTER(128) label
CHARACTER(4)   cext

INTEGER indx,irv
LOGICAL saveflag, lanim, ltmp

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: StripNull

!---- Use Dialog ID amd Button ID to determine file type
!       ccust - Custom file filter
!       cext  - Default extension
!       cbuff - File filters - Available file types
!       label - Common Dialog Box title - currently not displayed

!---- Initialize for Error

lok = .FALSE.

saveflag = .FALSE.

enableRestart = .FALSE.

lanim = .FALSE.
ltmp  = .FALSE.

inname = filename

cbuff = cnull//cnull
indx  = 0

!---- Select File type

SELECT CASE( id )

  CASE( IDB_SCIPUF ) !RUN

    SELECT CASE( ib ) !  Load a File
      CASE( 4 ) !    Save Options File
        ccust  = 'Options File (*.opt)'//cnull//'*.opt'//cnull//cnull
        cbuff  = 'All Files (*.*)'//cnull//'*.*'//cnull//cnull
        cext   = 'opt'
        label  = 'Options'
        lok    = .TRUE.
        lanim  = .FALSE.

      CASE DEFAULT

    END SELECT

  CASE( IDB_PLOT ) !PLOT

    SELECT CASE( ib ) !  Load a File
      CASE( 12 ) !    Animate File
        ccust  = 'Animation File (*.001)'//cnull//'*.0*'//cnull//cnull
        cext   = '001'
        label  = 'Animate'
        lok    = .TRUE.
        lanim  = .TRUE.
        saveflag = .NOT.Play .AND. .NOT.AnimDel

      CASE( 13 ) !    Save Options File
        ccust  = 'Options File (*.opt)'//cnull//'*.opt'//cnull//cnull
        cext   = 'opt'
        label  = 'Options'
        lok    = .TRUE.
        lanim  = .FALSE.
        saveflag = .TRUE.
        ltmp   = .TRUE.

      CASE( 14 ) !    Load Options File
        ccust  = 'Options File (*.opt)'//cnull//'*.opt'//cnull//cnull
        cbuff  = 'All Files (*.*)'//cnull//'*.*'//cnull//cnull
        cext   = 'opt'
        label  = 'Options'
        lok    = .TRUE.
        lanim  = .FALSE.

    END SELECT

  CASE( IDB_CONTOUR ) !PLOT - CONTOUR

    SELECT CASE( ib ) !  Load a File
      CASE( 9 ) !    Size Bin File
        ccust  = 'Contour File (*.clv)'//cnull//'*.clv'//cnull//cnull
        cbuff  = 'All Files (*.*)'//cnull//'*.*'//cnull//cnull
        cext   = 'clv'
        label  = 'Contour Definitions'
        lok    = .TRUE.

      CASE( 10 ) !    Size Bin File
        ccust  = 'Contour File (*.clv)'//cnull//'*.clv'//cnull//cnull
        cext   = 'clv'
        label  = 'Contour Definitions'
        lok    = .TRUE.
        saveflag = .TRUE.

    END SELECT

  CASE( IDB_TIME,IDB_DOMAIN,IDB_PRJDEF,IDB_SETUP ) !Setup Input

    SELECT CASE( ib ) !  Load a File
      CASE( 14 ) !    Input file
        ccust  = 'Input File (*.inp)'//cnull//'*.inp'//cnull//cnull
        cbuff  = 'All Files (*.*)'//cnull//'*.*'//cnull//cnull
        cext   = 'inp'
        label  = 'Input Files'
        lok    = .TRUE.

      CASE DEFAULT

    END SELECT

  CASE( IDB_OPTIONS ) !Setup Input

    SELECT CASE( ib ) !  Load a File
      CASE( 2 ) !    Input file
        ccust  = 'Sampler File (*.sam)'//cnull//'*.sam'//cnull//cnull
        cbuff  = 'All Files (*.*)'//cnull//'*.*'//cnull//cnull
        cext   = 'sam'
        label  = 'Sampler Input Files'
        lok    = .TRUE.
        IF( inname == ' ' )inname = loadfile(16)

      CASE( 14 ) !    Input file
        ccust  = 'Input File (*.inp)'//cnull//'*.inp'//cnull//cnull
        cbuff  = 'All Files (*.*)'//cnull//'*.*'//cnull//cnull
        cext   = 'inp'
        label  = 'Input Files'
        lok    = .TRUE.

      CASE DEFAULT

    END SELECT

  CASE( IDB_PLTOPT ) !Setup Input

    SELECT CASE( ib ) !  Load a File
      CASE( 13 ) !    Input file
        ccust  = 'Overlay File (*.ovl)'//cnull//'*.ovl'//cnull//cnull
        cbuff  = 'All Files (*.*)'//cnull//'*.*'//cnull//cnull
        cext   = 'ovl'
        label  = 'SCIPUFF Overlay Files'
        lok    = .TRUE.

      CASE DEFAULT

    END SELECT

  CASE( IDB_MATDEF ) !MATDEF  - Setup Materials

    SELECT CASE( ib ) !  Load a File
      CASE (9 ) !    Size Bin File
        ccust  = 'Size bin File (*.bin)'//cnull//'*.bin'//cnull//cnull
        cbuff  = 'All Files (*.*)'//cnull//'*.*'//cnull//cnull
        cext   = 'bin'
        label  = 'Size bin Definitions'
        lok    = .TRUE.

      CASE( 10 ) !    Size Bin File
        ccust  = 'Size bin File (*.bin)'//cnull//'*.bin'//cnull//cnull
        cext   = 'bin'
        label  = 'Size bin Definitions'
        lok    = .TRUE.
        saveflag = .TRUE.

      CASE( 14 ) !    Material File
        indx = INDEX(filename,'.inp')
        IF( indx > 0 )THEN
          ccust  = 'Input File (*.inp)'//cnull//'*.inp'//cnull//cnull
          cbuff  = 'Material File (*.mtl)'//cnull//'*.mtl'//cnull// &
                   'All Files (*.*)'//cnull//'*.*'//cnull//cnull
        ELSE
          ccust  = 'Material File (*.mtl)'//cnull//'*.mtl'//cnull//cnull
          cbuff  = 'Input File (*.inp)'//cnull//'*.inp'//cnull// &
                   'All Files (*.*)'//cnull//'*.*'//cnull//cnull
        END IF
        cext   = 'mtl'
        label  = 'Material Definitions'
        lok    = .TRUE.

      CASE( 17 ) !    Multicomponent file
        ccust  = 'Description File (*.imc)'//cnull//'*.imc'//cnull//cnull
        cext   = 'imc'
        cbuff  = 'All Files (*.*)'//cnull//'*.*'//cnull//cnull
        label  = 'Multicomponent Description Files'
        lok    = .TRUE.

      CASE DEFAULT !    UNKNOWN

    END SELECT

  CASE( IDB_RELDEF ) !RELDEF - Setup Release

    SELECT CASE( ib ) !  Load a File
      CASE( 4 ) !    CLOUDTRANS
        ccust  = 'CLOUDTRANS (*.rel)'//cnull//'*.rel'//cnull//cnull
        cbuff  = 'All Files (*.*)'//cnull//'*.*'//cnull//cnull
        cext   = 'rel'
        label  = 'CLOUDTRANS Release File'
        lok    = .TRUE.
        IF( inname == ' ' )inname = loadfile(15)

      CASE( 14 ) !    Release File
        ccust  = 'Release Scenario (*.scn)'//cnull//'*.scn'//cnull//cnull
        cbuff  = 'All Files (*.*)'//cnull//'*.*'//cnull//cnull
        cext   = 'scn'
        label  = 'Release Scenario'
        lok    = .TRUE.

      CASE DEFAULT !    UNKNOWN

    END SELECT

  CASE( IDB_EDTPRJ ) !EDTPRJ - Setup New Project

    SELECT CASE( ib ) !  Load a File
      CASE( 10 ) !    Project File
        ccust  = 'Project (*.prj)'//cnull//'*.prj'//cnull//cnull
        cbuff  = 'All Files (*.*)'//cnull//'*.*'//cnull//cnull
        cext   = 'prj'
        label  = 'New Project'
        lok    = .TRUE.
        enableRestart = .TRUE.

      CASE DEFAULT !    UNKNOWN

    END SELECT

  CASE( IDB_XYTABLE ) !XYTABLE

    SELECT CASE( ib ) !  Load a File
      CASE( 9 ) !    Table File
        ccust  = 'Table (*.tab)'//cnull//'*.tab'//cnull//cnull
        cbuff  = 'All Files (*.*)'//cnull//'*.*'//cnull//cnull
        cext   = 'tab'
        label  = 'Table Output'
        lok    = .TRUE.

      CASE DEFAULT !    UNKNOWN

    END SELECT

  CASE( IDB_METDEF ) !METDEF  - Meteorology Input

    SELECT CASE( ib ) !  Specify a file
      CASE( 6,16 ) !    GRIDDED
        IF( idbcmbo(1,il)+met_offset == MET_MEDOC )THEN
          ccust  = 'MEDOC (*.fmt)'//cnull//'*.fmt'//cnull//cnull
          cbuff  = 'SCIP Gridded (*.grd)'//cnull//'*.grd'//cnull// &
                   'Mass-consistent Winds (*.mcw)'//cnull//'*.mcw'//cnull// &
                   'MRF Winds (mrf*.*)'//cnull//'mrf*.*'//cnull// &
                   'Data Files (*.dat)'//cnull//'*.dat'//cnull// &
                   'All Files (*.*)'//cnull//'*.*'//cnull//cnull
          cext   = 'fmt'
          label  = 'MEDOC Files'
          IF( inname == ' ' )inname = loadfile(18)
        ELSE IF( idbcmbo(1,il)+met_offset == MET_WRF )THEN
          ccust  = 'WRF list (*.lis)'//cnull//'*.lis'//cnull//cnull
          cbuff  = 'SCIP Gridded (*.grd)'//cnull//'*.grd'//cnull// &
                   'Mass-consistent Winds (*.mcw)'//cnull//'*.mcw'//cnull// &
                   'MRF Winds (mrf*.*)'//cnull//'mrf*.*'//cnull// &
                   'Data Files (*.dat)'//cnull//'*.dat'//cnull// &
                   'All Files (*.*)'//cnull//'*.*'//cnull//cnull
          cext   = 'lis'
          label  = 'WRF List Files'
          IF( inname == ' ' )inname = loadfile(21)
        ELSE IF( idbcmbo(1,il)+met_offset == MET_MEDLIS )THEN
          ccust  = 'MEDOC list (*.lis)'//cnull//'*.lis'//cnull//cnull
          cbuff  = 'SCIP Gridded (*.grd)'//cnull//'*.grd'//cnull// &
                   'Mass-consistent Winds (*.mcw)'//cnull//'*.mcw'//cnull// &
                   'MRF Winds (mrf*.*)'//cnull//'mrf*.*'//cnull// &
                   'Data Files (*.dat)'//cnull//'*.dat'//cnull// &
                   'All Files (*.*)'//cnull//'*.*'//cnull//cnull
          cext   = 'lis'
          label  = 'MEDOC List Files'
          IF( inname == ' ' )inname = loadfile(21)
        ELSE
          ccust  = 'SCIP Gridded (*.grd)'//cnull//'*.grd'//cnull//cnull
          cbuff  = 'MEDOC (*.fmt)'//cnull//'*.fmt'//cnull// &
                   'Mass-consistent Winds (*.mcw)'//cnull//'*.mcw'//cnull// &
                   'MRF Winds (mrf*.*)'//cnull//'mrf*.*'//cnull// &
                   'Data Files (*.dat)'//cnull//'*.dat'//cnull// &
                   'All Files (*.*)'//cnull//'*.*'//cnull//cnull
          cext   = 'grd'
          label  = 'SCIP gridded Files'
          IF( inname == ' ' )inname = loadfile(13)
        END IF
        lok    = .TRUE.

      CASE( 11 ) !    ASSIM
        ccust  = 'MetList (*.mlf)'//cnull//'*.fmt'//cnull//cnull
        cbuff  = 'Data Files (*.dat)'//cnull//'*.dat'//cnull// &
                 'All Files (*.*)'//cnull//'*.*'//cnull//cnull
        cext   = 'mlf'
        label  = 'MetList Files'
        IF( inname == ' ' )inname = loadfile(20)
        lok    = .TRUE.

      CASE( 4,17,19 ) !    OBS - Profile
        ccust  = 'Profile Obs (*.prf)'//cnull//'*.prf'//cnull//cnull
        cbuff  = 'Observations (*.obs)'//cnull//'*.obs'//cnull// &
                 'All Files (*.*)'//cnull//'*.*'//cnull//cnull
        cext   = 'prf'
        label  = 'OBS Profile Files'
        lok    = .TRUE.
        IF( inname == ' ' )inname = loadfile(12)

      CASE( 5,18 ) !    OBS - Surface
        ccust  = 'Surface Obs (*.sfc)'//cnull//'*.sfc'//cnull//cnull
        cbuff  = 'Observations (*.obs)'//cnull//'*.obs'//cnull// &
                 'All Files (*.*)'//cnull//'*.*'//cnull//cnull
        cext   = 'sfc'
        label  = 'OBS Surface Files'
        lok    = .TRUE.
        IF( inname == ' ' )inname = loadfile(17)

      CASE( 14 ) !    Meterorology File
        ccust  = 'Met Scenario (*.msc)'//cnull//'*.msc'//cnull//cnull
        cbuff  = 'All Files (*.*)'//cnull//'*.*'//cnull//cnull
        cext   = 'msc'
        label  = 'Meteorology Scenario'
        lok    = .TRUE.

      CASE DEFAULT !    UNKNOWN

    END SELECT

  CASE( IDB_TERPARM ) !PLOT - CONTOUR

    SELECT CASE( ib ) !  Load a File
      CASE( 2 ) !    OBS - Surface
        ccust  = 'Terrain (*.ter)'//cnull//'*.ter'//cnull//cnull
        cbuff  = 'All Files (*.*)'//cnull//'*.*'//cnull//cnull
        cext   = 'ter'
        label  = 'Terrain Files'
        lok    = .TRUE.
        IF( inname == ' ' )inname = loadfile(14)

      CASE( 9 ) !    Size Bin File
        ccust  = 'Grid File (*.grd)'//cnull//'*.grd'//cnull//cnull
        cbuff  = 'All Files (*.*)'//cnull//'*.*'//cnull//cnull
        cext   = 'grd'
        label  = 'Grid Definitions'
        lok    = .TRUE.

      CASE(10 ) !    Size Bin File
        ccust  = 'Grid File (*.grd)'//cnull//'*.grd'//cnull//cnull
        cext   = 'grd'
        label  = 'Grid Definitions'
        lok    = .TRUE.
        saveflag = .TRUE.

    END SELECT

  CASE DEFAULT

END SELECT

!---- Initialize File/Path with Current values
!       ctmpf - Initial file name
!       ctmpd - Initial directory

IF( lok )THEN
  CALL SplitName( inname,ctmpf,ctmpd )
  IF( LEN_TRIM(ctmpf) == 0 )ctmpf = '*.'//TRIM(cext)
  IF( LEN_TRIM(ctmpd) == 0 )ctmpd = TRIM(path_tmp)
ELSE
  WRITE(string1,*)'Dialog/Button ID=',id,ib
  CALL SetError( UK_ERROR,'Unable to determine necessary file type',&
                 string1,' ','GetFile' )
  CALL ShowErrorMessage( iwnd_db )
  GOTO 9999
END IF

!---- OpenFile Common Dialog Driver

irv = EnableWindow( hwnd_pw,FALSE )
IF( saveflag )THEN
  CALL SaveFileDlg( ccust,cbuff,ctmpf,ctmpd, &
                    label,iwnd_db,MyAppInst,lok,.FALSE.,.FALSE.,ltmp,lanim,cext,indx )
ELSE
  CALL OpenFileDlg( ccust,cbuff,ctmpf,ctmpd, &
                    cext,label,iwnd_db,MyAppInst,lok )
END IF
irv = EnableWindow( hwnd_pw,TRUE )

!---- SUCCESS - Get New File/Path

IF( lok )THEN
  filename = StripNull( ctmpf )
  ccust    = StripNull( ctmpd )
  CALL AddPath( filename,ccust )
END IF

9999 CONTINUE

RETURN
END
!*******************************************************************************
!                     GetPrjFile
!*******************************************************************************
SUBROUTINE GetPrjFile( iwnd_db )

USE resource_fd
USE reldef_fd
USE mettype_fd
USE tooluser_fd
USE files_fi
USE errorParam_fd
USE pcscipuf_fi
USE dialog_fi
USE GUImatl_fi
USE create_fi
USE GUItool_fi
USE script_fi
USE pltchoice_fi
USE restart_fi

!     This routine uses the OpenFile Common Dialog to get a Project name
!     and open the project

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog Handle

LOGICAL lok
CHARACTER(PATH_MAXLENGTH) cbuff,ctmpf,ctmpd,ccust,label,AddNull
CHARACTER(4)   cext

INTEGER ios

IF( BTEST(pcscipuf_mode,IPMB_INTERACTIVE) )THEN

!---- Initialize Strings for OpenFile Driver
!       ccust - Custom file filter
!       cext  - Default extension
!       cbuff - File filters - Available file types
!       label - Common Dialog Box title - currently not displayed
!       ctmpf - Initial file name
!       ctmpd - Initial directory

  enableRestart = .FALSE.

  ccust  = 'Project File (*.prj)'//cnull//'*.prj'//cnull//cnull
  cbuff  = cnull//cnull
  cext   = 'prj'
  label  = 'Project'
  string1 = TRIM(project(BASE_LEVEL)%ID%name)//'.'//TRIM(cext)
  ctmpf  = AddNull (string1 ) !Filename
  ctmpd  = TRIM(project(BASE_LEVEL)%ID%path) !Initial Directory

!---- OpenFile Common Dialog Driver

  CALL OpenFileDlg( ccust,cbuff,ctmpf,ctmpd, &
                   'prj','Project',iwnd_db,MyAppInst,lok )

ELSE

  lok = .TRUE.
  CALL SplitName( script_input,ctmpf,ctmpd )

END IF
!---- SUCCESS

IF( lok )THEN
!------ Initialize Project/File names

  LastMap = project(BASE_LEVEL)%MapCoord == I_LATLON
  project(BASE_LEVEL) = project(EDIT_LEVEL_1)
  dlgDomain(BASE_LEVEL)  = dlgDomain(BASE_LEVEL)
  metdef(BASE_LEVEL)  = metdef(DEFAULT_LEVEL)
  dlgOptions(BASE_LEVEL)  = dlgOptions(DEFAULT_LEVEL)
  dlgTime(BASE_LEVEL)  = dlgTime(DEFAULT_LEVEL)
  CALL CopyMaterial( materials(DEFAULT_LEVEL),materials(BASE_LEVEL) )
  CALL CopyScenario( scenario(DEFAULT_LEVEL),scenario(BASE_LEVEL) )
  nTimePuff = 0
  nTimeSrf  = 0
  IF( ALLOCATED(timePuff) )THEN
    DEALLOCATE( timePuff,STAT=ios )
	  IF( ios /= 0 )THEN
	    WRITE(string1,*)ios
	    CALL SetError( UK_ERROR, &
	                  'Failed to DEALLOCATE puff time array', &
	                  'DEALLOCATE error = '//TRIM(ADJUSTL(string1)),' ', &
	                  'SCIPtoGUI_ctrl' )
	    GOTO 9999
	  END IF
  END IF
  IF( ALLOCATED(timeSrf) )THEN
    DEALLOCATE( timeSrf,STAT=ios )
	  IF( ios /= 0 )THEN
	    WRITE(string1,*)ios
	    CALL SetError( UK_ERROR, &
	                  'Failed to DEALLOCATE surface time array', &
	                  'DEALLOCATE error = '//TRIM(ADJUSTL(string1)),' ', &
	                  'SCIPtoGUI_ctrl' )
	    GOTO 9999
	  END IF
  END IF

!------ Set Flags
  lplotOK  = .FALSE. !PLOT SUCCESS Flag OFF
  lprintOK = .FALSE. !PRINT Flag OFF
  project(BASE_LEVEL)%OK      = .TRUE. !PROJECT Flag ON
  project(BASE_LEVEL)%ID%name = TRIM(ctmpf) !New Project Filename
  string1 = project(BASE_LEVEL)%ID%name
  CALL cupper( string1 )
  IF( INDEX(string1,'.PRJ') > 0 )THEN
    CALL RemoveExtension( project(BASE_LEVEL)%ID%name )
  END IF
  CALL clower( project(BASE_LEVEL)%ID%name ) !  Convert to Lower case for consistency
  project(BASE_LEVEL)%ID%path = TRIM(ctmpd) !New Path
  CALL clower(project( BASE_LEVEL)%ID%name ) !  Convert to lower case
  CALL SetFileNames( project(BASE_LEVEL)%ID%name,project(BASE_LEVEL)%ID%path ) !Set Standard Filenames
  CALL OpenProject( iwnd_db ) !OPEN Project
  IF( project(BASE_LEVEL)%OK )THEN !  SUCCESS
    string1 = TRIM(project(BASE_LEVEL)%ID%name)
    IF( project(BASE_LEVEL)%Edit )THEN
      ccust = TRIM('&Edit '//TRIM(string1)) !    Build TOOLBAR Message
    ELSE
      ccust = TRIM('Re&view '//TRIM(string1)) !    Build TOOLBAR Message
    END IF
    CALL SetControlText( hwnd_tb,IDB_VIEWPRJ,ccust ) !    Post TOOLBAR Message
  ELSE !  FAILURE
    ccust = ' ' !    Build TOOLBAR Message
    CALL SetControlText( hwnd_tb,IDB_VIEWPRJ,ccust ) !    Post TOOLBAR Message
  END IF
END IF

9999 CONTINUE

RETURN
END
