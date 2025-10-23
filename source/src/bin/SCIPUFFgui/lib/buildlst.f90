!***********************************************************************
!               BuildList
!***********************************************************************
!
!     List Routines
!
!        subroutine build_list          Build Project Input display lists
!
!        subroutine build_tim           Build Time list
!
!        subroutine build_dom           Build Domain list
!
!        subroutine build_opt           Build Options list
!
!        subroutine build_mat           Build Material list
!
!        subroutine build_rel           Build Release list
!
!        subroutine build_met           Build Meteorology list
!
!        subroutine build_list_numeric  Build a list from numeric values
!                                         and set DialogBox arrays
!        subroutine clear_list_numeric  Clear a list of numeric values
!                                         and set DialogBox arrays
!        subroutine add_list_numeric    Add to a list of numeric values
!                                         and set DialogBox arrays
!        subroutine file_list_numeric   real a file and Add to a list of numeric values
!                                         and set DialogBox arrays
!
!**** INCLUDES *********************************************************
!
!      defW32.inc         ASSIGNMENT - ABSOFT/API definitions
!      resource.inc       ASSIGNMENT - PC_SCIPUFF parameters
!      files.inc          COMMON     - File names and logical units
!      error.inc          COMMON     - Error handling
!      pcscipuf.inc       COMMON     - PC_SCIPUFF main common
!
!**** HISTORY **********************************************************
!
!     31-Jan-95 (SFP) Initial comments and writing of routines
!
!***********************************************************************
!               BuildList
!***********************************************************************
SUBROUTINE build_list( iwnd,ilst )

USE resource_fd
USE mettype_fd
USE reldef_fd
USE tooluser_fd
USE basic_fd
USE winAPI_fd, ONLY: POINTER_LEN

IMPLICIT NONE

INTEGER, PARAMETER :: MAXLST=101

INTEGER(POINTER_LEN) iwnd
INTEGER              ilst

CHARACTER(128), DIMENSION(MAXLST) :: list

INTEGER i, n, ictrl, irv, indx
LOGICAL lenable

n = 0
indx = -1

SELECT CASE( ilst )
  CASE( 1 )
    CALL build_tim( list,MAXLST,n )
  CASE( 2 )
    CALL build_dom( list,MAXLST,n )
  CASE( 3 )
    CALL build_opt( list,MAXLST,n )
  CASE( 4 )
    CALL build_mat( list,MAXLST,n )
    indx = -999
  CASE( 5 )
    CALL build_rel( list,MAXLST,n )
    indx = -999
  CASE( 6 )
    CALL build_met( list,MAXLST,n )
  CASE( 7 )
    CALL build_prj( list,MAXLST,n )
  CASE DEFAULT
    n = 0
END SELECT

ictrl = LIST_BASE + ilst

CALL IsControlEnabled( iwnd,ictrl,lenable )
IF( .NOT.lenable )CALL EnableControl( iwnd,ictrl,TRUE )

CALL ClearList( iwnd,ictrl )
IF( n > 0 )THEN
  DO i = 1,n
    CALL AddList( iwnd,ictrl,indx,list(i),irv )
  END DO
END IF

IF( .NOT.lenable )CALL EnableControl( iwnd,ictrl,FALSE )

RETURN
END
!***********************************************************************
!               BuildTim
!***********************************************************************
SUBROUTINE build_tim( list,mxlst,n )

USE resource_fd
USE mettype_fd
USE reldef_fd
USE tooluser_fd
USE dialog_fi
USE pcscipuf_fi
USE default_fd
USE pltchoice_fi

IMPLICIT NONE

INTEGER                       :: mxlst
CHARACTER(*),DIMENSION(mxlst) :: list
INTEGER                       :: n

INTEGER i,nch

i = 1
IF( i > mxlst  )GOTO 9999

list(i) = 'Start='//TRIM(dlgTime(EDIT_LEVEL_1)%startString)
i = i + 1
IF( i > mxlst  )GOTO 9999

list(i) = 'Stop ='//TRIM(dlgTime(EDIT_LEVEL_1)%endString)
i = i + 1
IF( i > mxlst  )GOTO 9999

IF( project(EDIT_LEVEL_1)%Restart )THEN
  CALL SplitName( project(EDIT_LEVEL_1)%RestartFile,string1,string2 )
  list(i) = 'Restart='//TRIM(string1)
  i = i + 1
  IF( i > mxlst  )GOTO 9999
  CALL format_time( timeRestart(project(EDIT_LEVEL_1)%RestartTimeIndx)%time%runTime,string1,0 )
  list(i) = '  Time ='//TRIM(string1)
  i = i + 1
  IF( i > mxlst  )GOTO 9999
END IF

IF( nTimePuff > 0 .AND. .NOT.project(EDIT_LEVEL_1)%Edit )THEN
  list(i) = 'Run  ='//TRIM(TimePuff(nTimePuff)%string)
  i = i + 1
  IF( i > mxlst  )GOTO 9999
END IF

IF( dlgTime(EDIT_LEVEL_1)%time%start%time%reference == HT_LOCAL )THEN
  list(i) = 'Times  = LOCAL'
ELSE
  list(i) = 'Times  = UTC'
END IF
i = i + 1
IF( i > mxlst  )GOTO 9999

IF( dlgTime(EDIT_LEVEL_1)%time%start%zone == DEF_VAL_R )THEN
  list(i) = '00:00Z set from domain'
ELSE IF( dlgTime(EDIT_LEVEL_1)%time%start%zone == NOT_SET_R )THEN
  list(i) = '00:00Z invalid'
ELSE IF( dlgTime(EDIT_LEVEL_1)%time%start%zone == DEFERRED_R )THEN
  list(i) = '00:00Z deferred'
ELSE
  list(i) = '00:00Z=00:00 Local'
  WRITE(list(i)(8:9),'(I2.2)')INT(dlgTime(EDIT_LEVEL_1)%time%start%zone)
  WRITE(list(i)(11:12),'(I2.2)')NINT(60.*(dlgTime(EDIT_LEVEL_1)%time%start%zone- &
                          FLOAT(INT(dlgTime(EDIT_LEVEL_1)%time%start%zone))))
END IF
i = i + 1
IF( i > mxlst  )GOTO 9999

CALL c_format( dlgTime(EDIT_LEVEL_1)%time%end%time%runTime,nch,string1 )
list(i) = 'Duration = '//string1(1:nch)
i = i + 1
IF( i > mxlst  )GOTO 9999

IF( nTimePuff > 0 .AND. .NOT.project(EDIT_LEVEL_1)%Edit )THEN
  CALL c_format( TimePuff(nTimePuff)%time%runTime,nch,string1 )
  list(i) = 'Current  = '//string1(1:nch)
  i = i + 1
  IF( i > mxlst  )GOTO 9999
END IF

CALL c_format( dlgTime(EDIT_LEVEL_1)%time%end%step%max,nch,string1 )
list(i) = 'Max. timestep = '//string1(1:nch)
i = i + 1
IF( i > mxlst  )GOTO 9999

CALL c_format( dlgTime(EDIT_LEVEL_1)%time%end%step%output,nch,string1 )
list(i) = 'Output interval = '//string1(1:nch)
i = i + 1
IF( i > mxlst  )GOTO 9999

9999 CONTINUE

n = i - 1

RETURN
END
!***********************************************************************
!               TimeString
!***********************************************************************
SUBROUTINE time_string( tim,string )

USE resource_fd
USE mettype_fd
USE reldef_fd
USE tooluser_fd
USE dialog_fd
USE default_fd

IMPLICIT NONE

TYPE( timeT ) tim

CHARACTER(*) string

INTEGER ih,im,is,iyx,i,j

CHARACTER(36),PARAMETER :: MONTHS = 'JanFebMarAprMayJunJulAugSepOctNovDec'
CHARACTER(1)  tail

LOGICAL check_YMD

IF( tim%hour == NOT_SET_R )THEN
  string = 'Unspecified'
  RETURN
END IF

IF( tim%reference == HT_LOCAL )THEN
  tail = 'L'
ELSE
  tail = 'Z'
END IF

ih = INT(tim%hour)
IF( tim%hour > 1.0 )THEN
  im = NINT( 60.*(tim%hour - FLOAT(ih)) )
  is = -999
  IF( im >= 60 )THEN
    ih = ih + 1
    im = im - 60
  END IF
ELSE
  im = INT( 60.*(tim%hour - FLOAT(ih)) )
  is = NINT( 60.*(60.*(tim%hour - FLOAT(ih)) - FLOAT(im)) )
  IF( is >= 60 )THEN
    im = im + 1
    is = is - 60
  END IF
  IF( im >= 60 )THEN
    ih = ih + 1
    im = im - 60
  END IF
END IF

IF( check_YMD(tim) )THEN
  iyx = tim%year - 100*(tim%year/100)
  i = (tim%month-1)*3 + 1
  j = i + 2
  IF( is >= 0 )THEN
    WRITE(string,'(I2.2,''-'',A,''-'',I2.2,'' '',I2.2,2('':'',I2.2),A)') &
                  tim%day,MONTHS(i:j),iyx,ih,im,is,tail
  ELSE
    WRITE(string,'(I2.2,''-'',A,''-'',I2.2,'' '',I2.2,('':'',I2.2),A)') &
                  tim%day,MONTHS(i:j),iyx,ih,im,tail
  END IF
ELSE
  IF( tim%day >= 0 )THEN
    iyx = tim%day
  ELSE
    iyx = 0
  END IF
  DO WHILE( ih >= 24)
    iyx = iyx + 1
    ih = ih - 24
  END DO
  IF( is >= 0 )THEN
    WRITE(string,'(''Day '',I2.2,'' '',I2.2,2('':'',I2.2))')iyx,ih,im,is
  ELSE
    WRITE(string,'(''Day '',I2.2,'' '',I2.2,('':'',I2.2))')iyx,ih,im
  END IF
END IF

RETURN
END
!***********************************************************************
!               format_time
!***********************************************************************
SUBROUTINE format_time( timt,string,iflag )

USE resource_fd
USE mettype_fd
USE reldef_fd
USE tooluser_fd
USE default_fd

IMPLICIT NONE

REAL timt           !In hours
CHARACTER(*) string !Out string
INTEGER iflag       !In units flag

REAL          fac
CHARACTER(8)  timu
CHARACTER(24) stim
INTEGER       nn

IF( timt == NOT_SET_R )THEN
  string = 'Not set'
ELSE IF( timt == DEF_VAL_R )THEN
  string = 'default'
ELSE
  IF( timt >= 59.95 )THEN
    IF( iflag == 0 )THEN
      timu = ' days   '
    ELSE
      timu = 'd       '
    END IF
    fac  = 1./24.
  ELSE IF( timt >= 0.999 )THEN
    IF( iflag == 0 )THEN
      timu = ' hrs    '
    ELSE
      timu = 'h       '
    END IF
    fac = 1.0
  ELSE IF( timt >= 0.01665 )THEN
    IF( iflag == 0 )THEN
      timu = ' min    '
    ELSE
      timu = 'm       '
    END IF
    fac  = 60.
  ELSE
    IF( iflag == 0 )THEN
      timu = ' sec    '
    ELSE
      timu = 's       '
    END IF
    fac  = 3600.
  END IF

  SELECT CASE( ADJUSTL(TRIM(timu)) )
    CASE( 'days','d' )
      WRITE(stim,'(F7.2)') timt*fac
      nn = LEN_TRIM(stim)
    CASE DEFAULT
      CALL c_format( timt*fac,nn,stim )
  END SELECT
  string = stim(1:nn)//timu

END IF

RETURN
END
!***********************************************************************
!               BuildDom
!***********************************************************************
SUBROUTINE build_dom( list,mxlst,n )

USE resource_fd
USE mettype_fd
USE reldef_fd
USE tooluser_fd
USE param_fd
USE dialog_fi
USE default_fd
USE pcscipuf_fi

IMPLICIT NONE

INTEGER                       :: mxlst
CHARACTER(*),DIMENSION(mxlst) :: list
INTEGER                       :: n

INTEGER i,irv
CHARACTER(12) tailx,taily,tailr

i = 1
IF( i > mxlst  )GOTO 9999

IF( dlgDomain(EDIT_LEVEL_1)%spatial%domain%coord == I_LATLON )THEN
  list(i) = 'Longitude/Latitude'
  tailx   = ' E  '
  taily   = ' N  '
  tailr   = ' deg'
ELSE IF( dlgDomain(EDIT_LEVEL_1)%spatial%domain%coord == I_UTM )THEN
  IF( dlgDomain(EDIT_LEVEL_1)%spatial%domain%zoneUTM == NOT_SET_I )THEN
    list(i) = 'UTM - Unspecified zone'
  ELSE IF( dlgDomain(EDIT_LEVEL_1)%spatial%domain%zoneUTM == DEF_VAL_I )THEN
    list(i) = 'UTM - Default zone'
  ELSE
    WRITE(string2,*)dlgDomain(EDIT_LEVEL_1)%spatial%domain%zoneUTM
    string2 = ADJUSTL(string2)
    list(i) = 'UTM - zone : '//TRIM(string2)
  END IF
  tailx   = ' Easting'
  taily   = ' Northing'
  tailr   = ' km '
ELSE
  list(i) = 'Cartesian (X,Y)'
  tailx   = ' km '
  taily   = ' km '
  tailr   = ' km '
END IF
i = i + 1
IF( i > mxlst  )GOTO 9999

CALL set_real_string( dlgDomain(EDIT_LEVEL_1)%spatial%domain%xMin,string2,irv )
IF( TRIM(string1) == 'default' )THEN
  list(i) = 'X min ='//TRIM(string2)
ELSE
  list(i) = 'X min ='//TRIM(string2)//TRIM(tailx)
END IF
i = i + 1
IF( i > mxlst  )GOTO 9999

CALL set_real_string( dlgDomain(EDIT_LEVEL_1)%spatial%domain%xMax,string2,irv )
IF( TRIM(string1) == 'default' )THEN
  list(i) = 'X max ='//TRIM(string2)
ELSE
  list(i) = 'X max ='//TRIM(string2)//TRIM(tailx)
END IF
i = i + 1
IF( i > mxlst  )GOTO 9999

CALL set_real_string( dlgDomain(EDIT_LEVEL_1)%spatial%domain%yMin,string2,irv )
IF( TRIM(string1) == 'default' )THEN
  list(i) = 'Y min ='//TRIM(string2)
ELSE
  list(i) = 'Y min ='//TRIM(string2)//TRIM(taily)
END IF
i = i + 1
IF( i > mxlst  )GOTO 9999

CALL set_real_string( dlgDomain(EDIT_LEVEL_1)%spatial%domain%yMax,string2,irv )
IF( TRIM(string1) == 'default' )THEN
  list(i) = 'Y max ='//TRIM(string2)
ELSE
  list(i) = 'Y max ='//TRIM(string2)//TRIM(taily)
END IF
i = i + 1
IF( i > mxlst  )GOTO 9999

CALL set_real_string( dlgDomain(EDIT_LEVEL_1)%spatial%domain%hRes,string2,irv )
IF( TRIM(string1) == 'default' )THEN
  list(i) = 'Hor. Res.='//TRIM(string2)
ELSE
  list(i) = 'Hor. Res.='//TRIM(string2)//TRIM(tailr)
END IF
i = i + 1
IF( i > mxlst  )GOTO 9999

CALL set_real_string( dlgDomain(EDIT_LEVEL_1)%spatial%domain%zMax,string2,irv )
list(i) = 'Z max ='//TRIM(string2)//' m'
i = i + 1
IF( i > mxlst  )GOTO 9999

CALL set_real_string( dlgDomain(EDIT_LEVEL_1)%spatial%domain%vRes,string2,irv )
list(i) = 'Vert. res. ='//TRIM(string2)//' m'
i = i + 1
IF( i > mxlst  )GOTO 9999

9999 CONTINUE

n = i - 1

RETURN
END
!***********************************************************************
!               BuildOpt
!***********************************************************************
SUBROUTINE build_opt( list,mxlst,n )

USE resource_fd
USE mettype_fd
USE reldef_fd
USE tooluser_fd
USE dialog_fi
USE pcscipuf_fi
USE SCIPtool

IMPLICIT NONE

INTEGER                       :: mxlst
CHARACTER(*),DIMENSION(mxlst) :: list
INTEGER                       :: n

INTEGER i,irv,nsub,ios,mode

TYPE( char16T ), DIMENSION(:), ALLOCATABLE :: substrate

CHARACTER(PATH_MAXLENGTH) :: samfile

i = 0

IF( dlgOptions(EDIT_LEVEL_1)%mGrd /= dlgOptions(DEFAULT_LEVEL)%mGrd )THEN
  i = i + 1
  IF( i > mxlst  )GOTO 9999
  WRITE(string1,*)dlgOptions(EDIT_LEVEL_1)%mGrd
  string1 = ADJUSTL(string1)
  list(i) = 'Grid res='//TRIM(string1)
END IF

IF( dlgOptions(EDIT_LEVEL_1)%substrate /= dlgOptions(DEFAULT_LEVEL)%substrate )THEN
  i = i + 1
  IF( i > mxlst  )GOTO 9999
  mode = 0
  nsub = SCIPNumSubstrates( mode )
  ALLOCATE( substrate(nsub),STAT=ios )
  IF( ios == 0 )THEN
    irv = SCIPGetSubstrates( mode,substrate )
    IF( irv == SCIPsuccess )THEN
      IF( dlgOptions(EDIT_LEVEL_1)%substrate+1 > nsub )THEN
        WRITE(list(i),'(A,I2.2)')'Substrate=Substrate ',dlgOptions(EDIT_LEVEL_1)%substrate+1
      ELSE
        list(i) = 'Substrate='//TRIM(substrate(dlgOptions(EDIT_LEVEL_1)%substrate+1)%string)
      END IF
    ELSE
      list(i) ='Substrate=ERROR(2)'
    END IF
  ELSE
    list(i) = 'Substrate=ERROR(1)'
  END IF
  IF( ALLOCATED(substrate) )DEALLOCATE( substrate,STAT=ios )
END IF

IF( dlgOptions(EDIT_LEVEL_1)%delMin /= dlgOptions(DEFAULT_LEVEL)%delMin )THEN
  i = i + 1
  IF( i > mxlst  )GOTO 9999
  CALL set_real_string( dlgOptions(EDIT_LEVEL_1)%delMin,string1,irv )
  list(i) = 'Srf res='//TRIM(string1)
END IF

IF( dlgOptions(EDIT_LEVEL_1)%nzBL /= dlgOptions(DEFAULT_LEVEL)%nzBL )THEN
  i = i + 1
  IF( i > mxlst  )GOTO 9999
  WRITE(string1,*)dlgOptions(EDIT_LEVEL_1)%nzBL
  string1 = ADJUSTL(string1)
  list(i) = 'BL res='//TRIM(string1)
END IF

IF( dlgOptions(EDIT_LEVEL_1)%wwTrop /= dlgOptions(DEFAULT_LEVEL)%wwTrop )THEN
  i = i + 1
  IF( i > mxlst  )GOTO 9999
  CALL set_real_string( dlgOptions(EDIT_LEVEL_1)%wwTrop,string1,irv )
  list(i) = 'Trop turb='//TRIM(string1)
END IF

IF( dlgOptions(EDIT_LEVEL_1)%slTrop /= dlgOptions(DEFAULT_LEVEL)%slTrop )THEN
  i = i + 1
  IF( i > mxlst  )GOTO 9999
  CALL set_real_string( dlgOptions(EDIT_LEVEL_1)%slTrop,string1,irv )
  list(i) = 'Trop scale='//TRIM(string1)
END IF

IF( dlgOptions(EDIT_LEVEL_1)%epsTrop /= dlgOptions(DEFAULT_LEVEL)%epsTrop )THEN
  i = i + 1
  IF( i > mxlst  )GOTO 9999
  CALL set_real_string( dlgOptions(EDIT_LEVEL_1)%epsTrop,string1,irv )
  list(i) = 'Trop diss='//TRIM(string1)
END IF

IF( dlgOptions(EDIT_LEVEL_1)%zDosage /= dlgOptions(DEFAULT_LEVEL)%zDosage )THEN
  i = i + 1
  IF( i > mxlst  )GOTO 9999
  CALL set_real_string( dlgOptions(EDIT_LEVEL_1)%zDosage,string1,irv )
  list(i) = 'Dosage hgt='//TRIM(string1)
END IF

IF( dlgOptions(EDIT_LEVEL_1)%massMin /= dlgOptions(DEFAULT_LEVEL)%massMin )THEN
  i = i + 1
  IF( i > mxlst  )GOTO 9999
  CALL set_real_string( dlgOptions(EDIT_LEVEL_1)%massMin,string1,irv )
  list(i) = 'Min mass='//TRIM(string1)
END IF

IF( dlgOptions(EDIT_LEVEL_1)%timeAvg /= dlgOptions(DEFAULT_LEVEL)%timeAvg )THEN
  i = i + 1
  IF( i > mxlst  )GOTO 9999
  CALL set_real_string( dlgOptions(EDIT_LEVEL_1)%timeAvg,string1,irv )
  list(i) = 'Avg. time='//TRIM(string1)
END IF

IF( dlgOptions(EDIT_LEVEL_1)%uuCalm /= dlgOptions(DEFAULT_LEVEL)%uuCalm )THEN
  i = i + 1
  IF( i > mxlst  )GOTO 9999
  CALL set_real_string( dlgOptions(EDIT_LEVEL_1)%uuCalm,string1,irv )
  list(i) = 'Calm turb='//TRIM(string1)
END IF

IF( dlgOptions(EDIT_LEVEL_1)%slCalm /= dlgOptions(DEFAULT_LEVEL)%slCalm )THEN
  i = i + 1
  IF( i > mxlst  )GOTO 9999
  CALL set_real_string( dlgOptions(EDIT_LEVEL_1)%slCalm,string1,irv )
  list(i) = 'Calm scale='//TRIM(string1)
END IF

samfile = TRIM(dlgOptions(EDIT_LEVEL_1)%samplerFile)
IF( samfile /= ' ' )THEN
  i = i + 1
  IF( i > mxlst  )GOTO 9999
  CALL ReportFileName( list(i),'Sampler=',samfile )
  IF( dlgOptions(EDIT_LEVEL_1)%dtSampler /= dlgOptions(DEFAULT_LEVEL)%dtSampler )THEN
    i = i + 1
    IF( i > mxlst  )GOTO 9999
    CALL set_real_string( dlgOptions(EDIT_LEVEL_1)%dtSampler,string1,irv )
    list(i) = ' Output='//TRIM(string1)//' sec'
  END IF
END IF
i = i + 1

9999 CONTINUE

n = i - 1

RETURN
END
!***********************************************************************
!               BuildMat
!***********************************************************************
SUBROUTINE build_mat( list,mxlst,n )

USE resource_fd
USE mettype_fd
USE reldef_fd
USE GUImatl_fi
USE pcscipuf_fi
USE UtilMtlAux

IMPLICIT NONE

INTEGER                       :: mxlst
CHARACTER(*),DIMENSION(mxlst) :: list
INTEGER                       :: n

INTEGER        i,nn,iaux,nsg
CHARACTER(32)  number
REAL           dat

LOGICAL, EXTERNAL :: IsGas,IsParticle
LOGICAL, EXTERNAL :: IsAerosol
LOGICAL, EXTERNAL :: IsWetParticle

n = MIN(mxlst,materials(EDIT_LEVEL_1)%nmatl)
DO i = 1,n
  IF( IsGas(materials(EDIT_LEVEL_1)%material(i)%icls) )THEN
    iaux = materials(EDIT_LEVEL_1)%material(i)%iaux
    dat = 1.e2*materials(EDIT_LEVEL_1)%mat_aux(iaux+1)
    CALL c_format( dat,nn,number )
    string1 = ADJUSTL(number)
  ELSE IF( IsAerosol(materials(EDIT_LEVEL_1)%material(i)%icls) )THEN
    iaux = materials(EDIT_LEVEL_1)%material(i)%iaux
    dat = 1.E2*materials(EDIT_LEVEL_1)%mat_aux(iaux+1)
    CALL c_format( dat,nn,number )
    string1 = ADJUSTL(number)
ELSE IF( IsParticle(materials(EDIT_LEVEL_1)%material(i)%icls) &
           .OR.IsWetParticle(materials(EDIT_LEVEL_1)%material(i)%icls) )THEN
   iaux = materials(EDIT_LEVEL_1)%material(i)%iaux
    nsg = GetSubgroups( materials(EDIT_LEVEL_1)%material(i),materials(EDIT_LEVEL_1)%mat_aux )
    WRITE(number,*)nsg
    IF( nsg < 9 )THEN
      string1 =' '//ADJUSTL(number)
    ELSE
      string1 = ADJUSTL(number)
    END IF
    dat = 1.E6*materials(EDIT_LEVEL_1)%mat_aux(iaux+PMAUX_BOUNDS)
    CALL c_format( dat,nn,number )
    string2 = TRIM(string1)//' ('//ADJUSTL(number)
    dat = 1.E6*materials(EDIT_LEVEL_1)%mat_aux(iaux+nsg*MAXPMAUX+PMAUX_BOUNDS)
    CALL c_format( dat,nn,number )
    string1 = TRIM(string2)//'-'//TRIM(ADJUSTL(number))//')'
  ELSE
    iaux = materials(EDIT_LEVEL_1)%material(i)%iaux
    iaux = iaux + MAXGMAUX + MAXLMAUXP
    nsg = GetSubgroups( materials(EDIT_LEVEL_1)%material(i),materials(EDIT_LEVEL_1)%mat_aux )
    WRITE(number,*)nsg
    IF( nsg < 9 )THEN
      string1 = ' '//ADJUSTL(number)
    ELSE
      string1 = ADJUSTL(number)
    END IF
    dat = 1.E6*materials(EDIT_LEVEL_1)%mat_aux(iaux+LMAUX_BOUNDS)
    CALL c_format( dat,nn,number )
    string2 = TRIM(string1)//' ('//ADJUSTL(number)
    dat = 1.E6*materials(EDIT_LEVEL_1)%mat_aux(iaux+nsg*MAXLMAUX+LMAUX_BOUNDS)
    CALL c_format( dat,nn,number )
    string1 = TRIM(string2)//'-'//TRIM(ADJUSTL(number))//')'
  END IF
  WRITE(list(i),100)materials(EDIT_LEVEL_1)%material(i)%cmat(1:8), &
                  materials(EDIT_LEVEL_1)%material(i)%ccls(1:1),TRIM(string1)
100 FORMAT(A,T10,A,T14,A)
END DO

RETURN
END
!***********************************************************************
!               BuildRel
!***********************************************************************
SUBROUTINE build_rel( list,mxlst,n )

USE resource_fd
USE mettype_fd
USE reldef_fd
USE tooluser_fd
USE create_fi
USE GUIparam_fd

IMPLICIT NONE

INTEGER                       :: mxlst
CHARACTER(*),DIMENSION(mxlst) :: list
INTEGER                       :: n

INTEGER i

n = MIN(mxlst,scenario(EDIT_LEVEL_1)%nrel)
DO i = 1,n
  WRITE(scenario(EDIT_LEVEL_1)%release(i)%string(2:5),'(I4.4)')i-1
  list(i) = TRIM(scenario(EDIT_LEVEL_1)%release(i)%string)
END DO

RETURN
END
!***********************************************************************
!               BuildMet
!***********************************************************************
SUBROUTINE build_met( list,mxlst,n )

USE resource_fd
USE mettype_fd
USE reldef_fd
USE tooluser_fd
USE dialog_fi
USE pcscipuf_fi
USE default_fd

IMPLICIT NONE

INTEGER                       :: mxlst
CHARACTER(*),DIMENSION(mxlst) :: list
INTEGER                       :: n

INTEGER i,nch

LOGICAL check_terrain,check_hazard,check_tbin


check_terrain = .FALSE.
check_hazard  = .FALSE.
check_tbin    = .FALSE.
SELECT CASE( metdef(EDIT_LEVEL_1)%met )
  CASE( MET_MEDOC )
    CALL SplitName( metdef(EDIT_LEVEL_1)%medfile,string1,string2 )
    list(1) ='Type = Gridded (MEDOC)'
    CALL ReportFileName( list(2),'Data=',string1 )
    i = 3
    check_terrain = .TRUE.
  CASE( MET_WRF )
    CALL SplitName( metdef(EDIT_LEVEL_1)%medfile,string1,string2 )
    list(1) ='Type = Gridded (WRF)'
    CALL ReportFileName( list(2),'Data=',string1 )
    i = 3
    check_terrain = .FALSE.
  CASE( MET_ASSIM )
    CALL SplitName( metdef(EDIT_LEVEL_1)%asmfile,string1,string2 )
    list(1) ='Type = Multiple (Assim)'
    CALL ReportFileName( list(2),'Data=',string1 )
    i = 3
    check_terrain = .FALSE.
  CASE( MET_MEDLIS )
    CALL SplitName( metdef(EDIT_LEVEL_1)%medfile,string1,string2 )
    list(1) ='Type = Gridded (MEDOC list)'
    CALL ReportFileName( list(2),'Data=',string1 )
    i = 3
    check_terrain = .FALSE.
  CASE( MET_MRF )
    CALL SplitName( metdef(EDIT_LEVEL_1)%medfile,string1,string2 )
    list(1) ='Type = Gridded (SCIP)'
    CALL ReportFileName( list(2),'Data=',string1 )
    i = 3
    check_terrain = .TRUE.
  CASE( MET_OBS )
    CALL SplitName( metdef(EDIT_LEVEL_1)%profile,string1,string3 )
    CALL SplitName( metdef(EDIT_LEVEL_1)%surfile,string2,string3 )
    list(1) = 'Type = Observations'
    CALL ReportFileName( list(2),'UA =',string1 )
    CALL ReportFileName( list(3),'SRF=',string2 )
    i = 4
    check_terrain = .TRUE.
    check_tbin    = metdef(EDIT_LEVEL_1)%tbin /= DEF_VAL_R
  CASE( MET_SRF )
    CALL SplitName( metdef(EDIT_LEVEL_1)%surfile,string2,string3 )
    list(1) = 'Type = Surface Observations'
    CALL ReportFileName( list(2),'SRF=',string2 )
    i = 3
    check_terrain = .TRUE.
    check_tbin    = metdef(EDIT_LEVEL_1)%tbin /= DEF_VAL_R
  CASE( MET_UAIR )
    CALL SplitName( metdef(EDIT_LEVEL_1)%profile,string1,string3 )
    list(1) = 'Type = Upper Air Observations'
    CALL ReportFileName( list(2),'UA =',string1 )
    i = 3
    check_terrain = .TRUE.
    check_tbin    = metdef(EDIT_LEVEL_1)%tbin /= DEF_VAL_R
  CASE(MET_FIXED)
    list(1) = 'Type = Fixed winds'
    CALL c_format( metdef(EDIT_LEVEL_1)%speed,nch,string1 )
    list(2) = '  Wind Speed = '//string1(1:nch)
    CALL c_format( metdef(EDIT_LEVEL_1)%direction,nch,string1 )
    list(3) = '  Direction = '//string1(1:nch)
    i = 4
    check_terrain = .TRUE.
  CASE DEFAULT
    list(1) = 'Data type = ?'
    i = 2
END SELECT
IF( i > mxlst  )GOTO 9999

IF( check_terrain .AND. metdef(EDIT_LEVEL_1)%lmc )THEN
  string1 = TRIM(list(1))//' with Terrain'
  list(1) = TRIM(string1)
  CALL SplitName(metdef(EDIT_LEVEL_1)%terfile,string1,string3)
  CALL ReportFileName( list(i),'    TER=',string1 )
  i = i + 1
  IF( metdef(EDIT_LEVEL_1)%luseter .AND. metdef(EDIT_LEVEL_1)%lavailter )THEN
    list(i) = '    Terrain    = On'
    i = i + 1
  END IF
  IF( metdef(EDIT_LEVEL_1)%luselc .AND. metdef(EDIT_LEVEL_1)%lavaillc )THEN
    list(i) = '    Land cover = On'
    i = i + 1
    IF( metdef(EDIT_LEVEL_1)%llccategory )THEN
      SELECT CASE( metdef(EDIT_LEVEL_1)%wetness )
        CASE( MST_DRY )
          string1 = 'Dry'
        CASE( MST_NORMAL )
          string1 = 'Normal'
        CASE( MST_WET )
          string1 = 'Wet'
        CASE DEFAULT
          string1 = 'Unknown'
      END SELECT
      list(i) = '    Surface moisture = '//TRIM(string1)
      i = i + 1
    END IF
  END IF
END IF
IF( i > mxlst  )GOTO 9999

IF( check_tbin )THEN
  IF( metdef(EDIT_LEVEL_1)%tbin >= 3600. )THEN
    CALL c_format( metdef(EDIT_LEVEL_1)%tbin/3600.,nch,string1 )
    string2 = 'hr'
  ELSE IF( metdef(EDIT_LEVEL_1)%tbin >= 60. )THEN
    CALL c_format( metdef(EDIT_LEVEL_1)%tbin/60.,nch,string1 )
    string2 = 'min'
  ELSE
    CALL c_format( metdef(EDIT_LEVEL_1)%tbin,nch,string1 )
    string2 = 'sec'
  END IF
  string3 = TRIM(string1)//TRIM(string2)
  list(i) = '  Time binning = '//TRIM(string3)
  i = i + 1
END IF
IF( i > mxlst  )GOTO 9999

SELECT CASE( metdef(EDIT_LEVEL_1)%bl )
  CASE( BL_OPERATIONAL )
    list(i) = 'BL = Operational (Met File/Calculated)'
    i = i + 1
    CALL c_format( metdef(EDIT_LEVEL_1)%bowen,nch,string1 )
    list(i) = '  Bowen ratio = '//string1(1:nch)
    i = i + 1
    CALL c_format( metdef(EDIT_LEVEL_1)%albedo,nch,string1 )
    list(i) ='  Albedo = '//string1(1:nch)
    i = i + 1
    CALL c_format( metdef(EDIT_LEVEL_1)%cloud,nch,string1 )
    list(i) = '  Cloud cover = '//string1(1:nch)
    i = i + 1
  CASE( BL_CALC )
    list(i) = 'BL = Calculated'
    i = i + 1
    CALL c_format( metdef(EDIT_LEVEL_1)%bowen,nch,string1 )
    list(i) = '  Bowen ratio = '//string1(1:nch)
    i = i + 1
    CALL c_format( metdef(EDIT_LEVEL_1)%albedo,nch,string1 )
    list(i) = '  Albedo = '//string1(1:nch)
    i = i + 1
    CALL c_format( metdef(EDIT_LEVEL_1)%cloud,nch,string1 )
    list(i) = '  Cloud cover = '//string1(1:nch)
    i = i + 1
  CASE( BL_NONE )
    list(i) = 'BL = None'
    i = i + 1
  CASE( BL_OBS )
    list(i) = 'BL = Observations'
    i = i + 1
  CASE( BL_MEDOC )
    list(i) = 'BL = Gridded file'
    i = i + 1
  CASE( BL_PROFILE )
    list(i) = 'BL = Profile'
    i = i + 1
  CASE( BL_SIMPLE )
    list(i) = 'BL = Simple'
    i = i + 1
    CALL c_format( metdef(EDIT_LEVEL_1)%zimin,nch,string1 )
    list(i) = '  Min Zi = '//string1(1:nch)
    i = i + 1
    CALL c_format( metdef(EDIT_LEVEL_1)%zimax,nch,string1 )
    list(i) = '  Max Zi = '//string1(1:nch)
    i = i + 1
    CALL c_format( metdef(EDIT_LEVEL_1)%hconst,nch,string1 )
    list(i) = '  Min H0 = '//string1(1:nch)
    i = i + 1
    CALL c_format( metdef(EDIT_LEVEL_1)%hdiur,nch,string1 )
    list(i) = '  Max H0 = '//string1(1:nch)
    i = i + 1
  CASE DEFAULT
    list(i) = 'BL = ?'
    i = i + 1
END SELECT
IF( i > mxlst )GOTO 9999

SELECT CASE( metdef(EDIT_LEVEL_1)%precip )
  CASE( PC_CLEAR )
    list(i) = 'Precip = None'
    i = i + 1
  CASE( PC_LIGHT_RAIN )
    list(i) = 'Precip = Light rain'
    i = i + 1
  CASE( PC_MODERATE_RAIN )
    list(i) = 'Precip = Moderate rain'
    i = i + 1
  CASE( PC_HEAVY_RAIN )
    list(i) = 'Precip = Heavy rain'
    i = i + 1
  CASE( PC_LIGHT_SNOW )
    list(i) = 'Precip = Light snow'
    i = i + 1
  CASE( PC_MODERATE_SNOW )
    list(i) = 'Precip = Moderate snow'
    i = i + 1
  CASE( PC_HEAVY_SNOW )
    list(i) = 'Precip = Heavy snow'
    i = i + 1
  CASE( PC_MET )
    list(i) = 'Precip = Read from Weather files'
    i = i + 1
  CASE DEFAULT
    list(i) = 'Precip = ?'
    i = i + 1
END SELECT
IF( i > mxlst  )GOTO 9999

SELECT CASE( metdef(EDIT_LEVEL_1)%lsv )
  CASE( LSV_INPUT )
    list(i) = 'LSV = Input'
    i = i + 1
    CALL c_format( metdef(EDIT_LEVEL_1)%slb,nch,string1 )
    list(i) = '  Scale = '//string1(1:nch)
    i = i + 1
    CALL c_format( metdef(EDIT_LEVEL_1)%uub,nch,string1 )
    list(i) = '  Variance = '//string1(1:nch)
    i = i + 1
  CASE( LSV_MODEL )
    list(i) = 'LSV = Model'
    i = i + 1
  CASE( LSV_NONE )
    list(i) = 'LSV = None'
    i = i + 1
  CASE( LSV_OBS )
    list(i) = 'LSV = Observations'
    i = i + 1
    CALL c_format( metdef(EDIT_LEVEL_1)%slb,nch,string1 )
    list(i) = '  Scale = '//string1(1:nch)
    i = i + 1
  CASE( LSV_OPERATIONAL )
    list(i) = 'LSV = Operational'
    i = i + 1
  CASE DEFAULT
    list(i) = 'LSV = ?'
    i = i + 1
END SELECT
IF( i > mxlst )GOTO 9999

IF( metdef(EDIT_LEVEL_1)%local )THEN
  list(i) = 'Time = Local'
ELSE
  list(i) = 'Time = UTC'
END IF
i = i + 1
IF( i > mxlst )GOTO 9999

IF( metdef(EDIT_LEVEL_1)%lcanopy )THEN
  CALL c_format( metdef(EDIT_LEVEL_1)%canopy,nch,string1 )
  list(i) = 'Canopy height = '//string1(1:nch)
  i = i + 1
  IF( i > mxlst  )GOTO 9999
  CALL c_format( metdef(EDIT_LEVEL_1)%canopyParam,nch,string1 )
  list(i) = 'Canopy param  = '//string1(1:nch)
ELSE
  CALL c_format( metdef(EDIT_LEVEL_1)%rough,nch,string1 )
  list(i) = 'Roughness = '//string1(1:nch)
END IF
i = i + 1
IF( i > mxlst )GOTO 9999

IF( check_hazard )THEN
  CALL c_format( metdef(EDIT_LEVEL_1)%slhazard,nch,string1 )
  list(i) = 'Hazard length scale = '//string1(1:nch)
  i = i + 1
END IF

9999 CONTINUE

n = i - 1

RETURN
END
!***********************************************************************
!               BuildPrj
!***********************************************************************
SUBROUTINE build_prj( list,mxlst,n )

USE resource_fd
USE mettype_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE param_fd

IMPLICIT NONE

INTEGER                       :: mxlst
CHARACTER(*),DIMENSION(mxlst) :: list
INTEGER                       :: n

INTEGER i

i = 1
IF( i > mxlst )GOTO 9999

CALL ReportFileName( list(i),'',project(EDIT_LEVEL_1)%ID%name )
i = i + 1
IF( i > mxlst )GOTO 9999

SELECT CASE( project(EDIT_LEVEL_1)%Title )
  CASE( 'C',' ' )
    list(i) = 'Untitled'

  CASE DEFAULT
    list(i) = TRIM(project(EDIT_LEVEL_1)%Title)

END SELECT
i = i + 1
IF( i > mxlst )GOTO 9999

SELECT CASE( project(EDIT_LEVEL_1)%audit%Analyst )
  CASE( 'C',' ' )
    list(i) = 'Analyst =Unspecified'

  CASE DEFAULT
    list(i) = 'Analyst ='//TRIM(project(EDIT_LEVEL_1)%audit%Analyst)

END SELECT
i = i + 1
IF( i > mxlst )GOTO 9999

SELECT CASE( project(EDIT_LEVEL_1)%audit%CreateDate )
  CASE( 'C' )
    list(i) = 'New Project'

  CASE DEFAULT
    list(i) = TRIM(project(EDIT_LEVEL_1)%audit%CreateDate)

END SELECT
i = i + 1
IF( i > mxlst )GOTO 9999

list(i) = 'Version ='//TRIM(project(EDIT_LEVEL_1)%audit%Version)
i = i + 1
IF( i > mxlst )GOTO 9999

list(i) ='Type    =SCIPUFF'
i = i + 1
IF( i > mxlst )GOTO 9999

CALL ReportFileName( list(i),'Path    =',project(EDIT_LEVEL_1)%ID%path )
i = i + 1
IF( i > mxlst )GOTO 9999

IF( BTEST(project(EDIT_LEVEL_1)%Mode,FAST_MODE) )THEN
  list(i) = 'Mode    =Fast'
ELSE
  list(i) = 'Mode    =Standard'
END IF
IF( BTEST(project(EDIT_LEVEL_1)%Mode,REVERSE_MODE) )THEN
  IF( BTEST(project(EDIT_LEVEL_1)%Mode,FAST_MODE) )THEN
    list(i) = 'Mode    =Reverse Fast'
  ELSE
    list(i) = 'Mode    =Reverse Standard'
  END IF
END IF
i = i + 1
IF( i > mxlst )GOTO 9999

IF( project(EDIT_LEVEL_1)%Dynamic.AND.project(EDIT_LEVEL_1)%DenseGas )THEN
  list(i) = 'Dynamics=Dense Gas'
ELSE IF( project(EDIT_LEVEL_1)%Dynamic )THEN
  list(i) = 'Dynamics=On'
ELSE
  list(i) = 'Dynamics=Off'
END IF
i = i + 1
IF( i > mxlst )GOTO 9999

IF( project(EDIT_LEVEL_1)%StaticPuffs )THEN
  list(i) = 'Statics =On'
ELSE
  list(i) = 'Statics =Off'
END IF
i = i + 1
IF( i > mxlst )GOTO 9999

IF( project(EDIT_LEVEL_1)%SourceNests )THEN
  list(i) = 'Src Nest=On'
ELSE
  list(i) = 'Src Nest=Off'
END IF
i = i + 1

9999 CONTINUE

n = i - 1

RETURN
END
!***********************************************************************
!               BuildListNumeric
!***********************************************************************
SUBROUTINE build_list_numeric( iwnd,ilst,ilev,n,dat,factor )

USE resource_fd
USE mettype_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd
INTEGER              ilst
INTEGER              ilev
INTEGER              n
REAL,DIMENSION(n) :: dat
REAL                 factor

CHARACTER(32) number
INTEGER       i,irv

CALL ClearList( iwnd,ilst )

IF( n > 0 )THEN
  nlst(ilev) = n
  DO i = 1,n
    dblst(ilev,i) = dat(i)*factor
    WRITE(number,*)dblst(ilev,i)
    number = ADJUSTL(number)
    CALL AddList( iwnd,ilst,-999,number,irv )
  END DO
END IF

RETURN
END
!***********************************************************************
!               BuildListNumericX
!***********************************************************************
SUBROUTINE build_list_numericX( iwnd,ilst,ilev,n,jlev,factor )

USE resource_fd
USE mettype_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd
INTEGER              ilst
INTEGER              ilev
INTEGER              n
INTEGER              jlev
REAL                 factor

CHARACTER(32) number
INTEGER    i,irv

CALL ClearList( iwnd,ilst )

IF( n > 0 )THEN
  nlst(ilev) = n
  DO i = 1,n
    dblst(ilev,i) = dblst(jlev,i)*factor
    WRITE(number,*)dblst(ilev,i)
    number = ADJUSTL(number)
    CALL AddList( iwnd,ilst,-999,number,irv )
  END DO
END IF

RETURN
END
!***********************************************************************
!               BuildListContour
!***********************************************************************
SUBROUTINE build_list_contour( iwnd,ilst,ilev,n,dat,factor )

USE resource_fd
USE mettype_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE field_fd

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd
INTEGER              ilst
INTEGER              ilev
INTEGER              n
TYPE(SCIPContourElementT), DIMENSION(n) :: dat
REAL                factor

CHARACTER(32) number
INTEGER       i,irv

CALL ClearList( iwnd,ilst )

IF( n > 0 )THEN
  nlst(ilev) = n
  DO i = 1,n
    dblst(ilev,i) = dat(i)%Contour*factor
    WRITE(number,*)dblst(ilev,i)
    number = ADJUSTL(number)
    CALL AddList( iwnd,ilst,-999,number,irv )
  END DO
END IF

RETURN
END
!***********************************************************************
!               ClearListNumeric
!***********************************************************************
SUBROUTINE clear_list_numeric( iwnd,ilst,ilev )

USE resource_fd
USE mettype_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd
INTEGER              ilst
INTEGER              ilev

CALL ClearList( iwnd,ilst )
nlst(ilev) = 0

RETURN
END
!***********************************************************************
!               FileListNumeric
!***********************************************************************
SUBROUTINE file_list_numeric( iwnd,id,ilst,ilev,filename,factor )

USE resource_fd
USE mettype_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE files_fi
USE errorParam_fd

IMPLICIT NONE

INTEGER,PARAMETER :: MAXLST=101

INTEGER(POINTER_LEN) iwnd
INTEGER              ilst
INTEGER              id
INTEGER              ilev
CHARACTER(*) filename
REAL         factor

CHARACTER(32) number
INTEGER    i,irv,nn
REAL, DIMENSION(MAXLST) :: tmp
LOGICAL lbad

CHARACTER(128) eMessage,eInform
INTEGER        nError

LOGICAL, EXTERNAL :: hasError
INTEGER, EXTERNAL :: lastError

OPEN(UNIT=lun_tmp,FILE=filename,STATUS='OLD',IOSTAT=irv)
IF( irv /= 0 )THEN
  nError = OP_ERROR
  eMessage = 'Unable to open file'
  CALL ReportFileName( eInform,'File=',filename )
  GOTO 9999
END IF

CALL read_list_header( lun_tmp,iwnd,id,ilev,nn )
IF( hasError() )THEN
  nError   = lastError()
  eMessage = 'Error reading file header'
  CALL ReportFileName( eInform,'File=',filename )
  GOTO 9999
END IF

IF( nn > MAXLST )THEN
  nError = SZ_ERROR
  eMessage = 'Unable to read file : Too many items in file'
  WRITE(number,*)MAXLST
  number = ADJUSTL(number)
  eInform = 'Maximum allowed is '//TRIM(number)
  GOTO 9999
END IF

READ(lun_tmp,*,IOSTAT=irv)nn,(tmp(i),i=1,nn)
IF( irv /= 0 )THEN
  nError = RD_ERROR
  eMessage = 'Error reading file'
  CALL ReportFileName( eInform,'File=',filename )
  GOTO 9999
END IF

lbad = .FALSE.
DO i = 1,nn
  CALL add_list_numeric( iwnd,ilst,ilev,factor*tmp(i),lbad )
  IF( lbad )GOTO 2000
END DO

2000 CONTINUE
CLOSE(UNIT=lun_tmp,IOSTAT=irv)
RETURN

9999 CONTINUE
CALL SetError( nError,eMessage,eInform,' ','ReadFileListNumeric' )
CALL ShowErrorMessage( iwnd )
GOTO 2000

END
!***********************************************************************
!               ComputeListNumeric
!***********************************************************************
SUBROUTINE compute_list_numeric( iwnd,ilst,ilev,bmin,bmax,nb,logb,factor )

USE resource_fd
USE errorParam_fd
USE mettype_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi

IMPLICIT NONE

INTEGER,PARAMETER :: MAXLST=101

INTEGER(POINTER_LEN) iwnd
INTEGER              ilst
INTEGER              ilev
REAL                 bmin
REAL                 bmax
INTEGER              nb
LOGICAL              logb
REAL                 factor

INTEGER i
LOGICAL lbad

REAL pinc,pmin,pmax,p

CHARACTER(128) eMessage,eInform
INTEGER        nError

IF( nb <= 1 )RETURN

nError = NO_ERROR

IF( logb )THEN
  pmin = LOG10(MAX(bmin,1.E-30))
  pmax = LOG10(MAX(bmax,1.E-30))
ELSE
  pmin = bmin
  pmax = bmax
END IF
pinc = (pmax-pmin)/FLOAT(nb-1)

lbad = .FALSE.
DO i = 1,nb-1
  p = pmin + FLOAT(i-1)*pinc
  IF( logb )p = 10.0**p
  CALL add_list_numeric( iwnd,ilst,ilev,factor*p,lbad )
  IF( lbad )nError = SZ_ERROR
END DO
p = bmax
CALL add_list_numeric( iwnd,ilst,ilev,factor*p,lbad )
IF( lbad )nError = SZ_ERROR

2000  CONTINUE
IF( nError /= NO_ERROR )THEN
  eMessage = 'Too many list items'
  WRITE(eInform,*)'Maximum = ',MAXLST
  CALL SetError( nError,eMessage,eInform,'List truncated','ComputeList' )
  CALL ShowErrorMessage( iwnd )
END IF

RETURN
END
!***********************************************************************
!               AddListNumeric
!***********************************************************************
SUBROUTINE add_list_numeric( iwnd,ilist,ilev,dat,lbad )

USE resource_fd
USE mettype_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi

IMPLICIT NONE

INTEGER,PARAMETER :: MAXLST=101

INTEGER(POINTER_LEN) iwnd
INTEGER              ilist
INTEGER              ilev
REAL                 dat
LOGICAL              lbad

CHARACTER(32) number
INTEGER    i,irv,ilst,jlst

!---- Prcess List setting LIST Position (ilst) and DATA array position (jlst)

IF( nlst(ilev) >= MAXLST )THEN
  lbad = .TRUE.
  RETURN
END IF

!---- New List (nd=0)

IF( nlst(ilev) == 0 )THEN
  ilst = -1
  jlst = 1

!---- New Value is less than first value in list - Move data values up

ELSE IF( dat < dblst(ilev,1) )THEN
  jlst = 1
  DO i = nlst(ilev),jlst,-1
    dblst(ilev,i+1) = dblst(ilev,i)
  END DO
  ilst = jlst - 1

!---- New Value is greater than last value in list - Leave data alone

ELSE IF( dat > dblst(ilev,nlst(ilev)) )THEN
  ilst = -1
  jlst = nlst(ilev)+1

!---- New Value is in between - Find its rank in the list and move data

ELSE
  jlst = 1
  DO WHILE( dat > dblst(ilev,jlst) .AND. jlst<nlst(ilev) )
    jlst = jlst + 1
  END DO
  IF( dat == dblst(ilev,jlst) )THEN !Value already exists
    GOTO 2000 !  Dont do anything
  END IF
  DO i = nlst(ilev),jlst,-1
    dblst(ilev,i+1) = dblst(ilev,i)
  END DO
  ilst = jlst - 1
END IF

!---- Increment list counter

nlst(ilev) = nlst(ilev) + 1

!---- Put new vlaue into dat array

dblst(ilev,jlst) = dat

!---- Add string to LIST

WRITE(number,*)dblst(ilev,jlst)
number = ADJUSTL(number)
CALL AddList( iwnd,ilist,ilst,number,irv )

2000 CONTINUE

RETURN
END
!***********************************************************************
!               SaveListNumeric
!***********************************************************************
SUBROUTINE save_list_numeric( iwnd,id,ilev,filename,factor )

USE resource_fd
USE mettype_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE files_fi
USE errorParam_fd

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd
INTEGER              id
INTEGER              ilev
REAL                 factor

INTEGER i,irv

CHARACTER(*) filename

CHARACTER(128) eMessage,eInform
INTEGER        nError

LOGICAL, EXTERNAL :: hasError
INTEGER, EXTERNAL :: lastError

OPEN(UNIT=lun_tmp,FILE=filename,STATUS='UNKNOWN',IOSTAT=irv)
IF( irv /= 0 )THEN
  nError = OP_ERROR
  eMessage = 'Unable to open file'
  CALL ReportFileName( eInform,'File=',filename )
  GOTO 9999
END IF

CALL save_list_header( lun_tmp,id,ilev )
IF( hasError() )THEN
  nError = lastError()
  eMessage = 'Error writing file header'
  CALL ReportFileName( eInform,'File=',filename )
  GOTO 9999
END IF

WRITE(lun_tmp,*,IOSTAT=irv)nlst(ilev)
IF( irv /= 0 )THEN
  nError = WR_ERROR
  eMessage = 'Error writing file'
  CALL ReportFileName( eInform,'File=',filename )
  GOTO 9999
END IF

WRITE(lun_tmp,*,IOSTAT=irv)(dblst(ilev,i)/factor,i=1,nlst(ilev))
IF( irv /= 0 )THEN
  nError = WR_ERROR
  eMessage = 'Error writing file'
  CALL ReportFileName( eInform,'File=',filename )
  GOTO 9999
END IF

2000 CONTINUE
CLOSE(UNIT=lun_tmp,IOSTAT=irv)
RETURN

9999 CONTINUE
CALL SetError( nError,eMessage,eInform,' ','SaveFileListNumeric' )
9998 CONTINUE
CALL ShowErrorMessage( iwnd )
GOTO 2000

END
!*******************************************************************************
!                     delete_list_numeric
!*******************************************************************************
SUBROUTINE delete_list_numeric( iwnd,ilist,ilev )

USE resource_fd
USE mettype_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi

!     Delete a value from the list

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd  !Window Handle
INTEGER              ilist !List ID
INTEGER              ilev  !Data level ID

INTEGER idel,j,ii

!---- Get Selected Items from LIST

CALL GetListCurSel( iwnd,ilist,idel )

!---- SUCCESS

IF( idel >= 0 )THEN

  CALL DeleteList( iwnd,ilist,idel,ii )    !Delete from LIST
  DO j = idel+1,nlst(ilev)-1               !Delete from DATA
    dblst(ilev,j) = dblst(ilev,j+1)
  END DO !
  nlst(ilev) = nlst(ilev) - 1            !Decrement list counter
  idel = MIN(idel,nlst(ilev)-1)
  IF( idel >= 0 )CALL SetListCurSel( iwnd,ilist,idel )

END IF

9999 CONTINUE

RETURN
END
!***********************************************************************
!               ReadListHeader
!***********************************************************************
SUBROUTINE read_list_header( lun,iwnd,id,ilev,nn )

USE resource_fd
USE errorParam_fd
USE mettype_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE default_fd

IMPLICIT NONE

INTEGER                  lun
INTEGER(POINTER_LEN)     iwnd
INTEGER                  id
INTEGER                  ilev
INTEGER                  nn

CHARACTER(128) line
CHARACTER(16)  key

INTEGER ncom,irv,i,j,nError
REAL    rdat

CHARACTER(128), EXTERNAL :: AddNull

ncom = 0
nn   = 0
line = '#'

nError = NO_ERROR

DO WHILE( line(1:1) == '#')
  READ(lun,'(A)',IOSTAT=irv)line
  IF( irv /= 0 )THEN
    nError = RD_ERROR
    GOTO 9999
  END IF
  IF( line(1:1) == '#' )THEN
    ncom = ncom + 1
    SELECT CASE( id )
      CASE( IDB_CONTOUR )
        j = INDEX(line,':')
        IF( j > 0 )THEN
          key  = line(2:j-1)
          CALL cupper( key )

          SELECT CASE( key )
            CASE( 'SCALE' )
              READ(line(j+1:),*,IOSTAT=irv)rdat
              IF( irv > 0 )THEN
                nError = RD_ERROR
                GOTO 9999
              ELSE IF( irv < 0 )THEN
                rdat = NOT_SET_R
              END IF
              dbreal(4,ilev) = rdat
              CALL SetEditRs( iwnd,dbreal(4,ilev),4,1 )

            CASE( 'UNITS' )
              IF( LEN(TRIM(line(j+1:))) > 0 )THEN
                dbtext(1,ilev) = AddNull( ADJUSTL(TRIM(line(j+1:))) )
              ELSE
                dbtext(1,ilev) = AddNull( 'Unknown' )
              END IF
              CALL SetEditTs( iwnd,dbtext(1,ilev),1,1 )

            CASE DEFAULT
          END SELECT

        END IF

      CASE DEFAULT
    END SELECT
  ELSE
    READ(line,*,IOSTAT=irv)nn
    IF( irv /= 0 )THEN
      nError = RD_ERROR
      GOTO 9999
    END IF
  END IF
END DO

REWIND(UNIT=lun,IOSTAT=irv)
IF( irv /= 0 )THEN
  nError = RD_ERROR
  GOTO 9999
END IF

DO i = 1,ncom
  READ(lun,*,IOSTAT=irv)
  IF( irv /= 0 )THEN
    nError = RD_ERROR
    GOTO 9999
  END IF
END DO

9999 CONTINUE
IF( nError /= NO_ERROR )CALL SetError( nError,' ',' ',' ',' ' )

RETURN
END
!***********************************************************************
!               SaveListHeader
!***********************************************************************
SUBROUTINE save_list_header( lun,id,ilev )

USE resource_fd
USE mettype_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE errorParam_fd

IMPLICIT NONE

INTEGER     lun
INTEGER     id
INTEGER     ilev

INTEGER irv,nError

CHARACTER(128), EXTERNAL :: StripNull

nError = NO_ERROR

SELECT CASE (id)
  CASE (IDB_CONTOUR)
    WRITE(lun,'(A,1PE12.5)',IOSTAT=irv)'#SCALE:',dbreal(4,ilev)
    IF( irv /= 0 )THEN
      nError = RD_ERROR
      GOTO 9999
    END IF
    WRITE(lun,'(A,A)',IOSTAT=irv)'#UNITS:',TRIM(StripNull(dbtext(1,ilev)))
    IF( irv /= 0 )THEN
      nError = RD_ERROR
      GOTO 9999
    END IF

  CASE DEFAULT
END SELECT

9999 CONTINUE
IF( nError /= NO_ERROR )CALL SetError( nError,' ',' ',' ',' ' )

RETURN
END

