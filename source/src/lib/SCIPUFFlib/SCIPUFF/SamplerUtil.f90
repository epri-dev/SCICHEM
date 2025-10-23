!==============================================================================
!  Sampler utilities
!==============================================================================

MODULE SamplerUtil

USE scipuff_fi
USE sampler_fi
USE files_fi
USE UtilMtlAux
USE SamplerGridOuput
USE met_fi, ONLY: hmin

IMPLICIT NONE

CONTAINS

!==============================================================================

SUBROUTINE create_smp( stitle )

!------ Create sampler output file

CHARACTER(*), INTENT( IN ) :: stitle

INTEGER, PARAMETER :: NUMB_OFFSET = 48
INTEGER, PARAMETER :: ALPH_OFFSET = 55

CHARACTER(3) stmp
CHARACTER(3) sbgrp
INTEGER      is, ngrp, nvar
INTEGER      ios, i1, i, j, iv
INTEGER      base,base2,nb(3)
INTEGER      i1a, iva
INTEGER      hour, min, sec, year, month, day

INTEGER, PARAMETER :: MAXC = 500
CHARACTER(32)   ctem, atem, xtem, ytem, utem, clat, clon
CHARACTER(124)  cmap
CHARACTER(MAXC) string, btem

CHARACTER(8), DIMENSION(:), ALLOCATABLE :: xnam
CHARACTER(8), DIMENSION(:), ALLOCATABLE :: anam

!------ Open sampler output file

OPEN( UNIT=lun_smp,FILE=file_smp,STATUS='NEW',IOSTAT=ios )
IF( ios /= 0 )THEN
  nError   = OP_ERROR
  eRoutine = 'create_smp'
  eMessage = 'Error opening sampler output file'
  CALL ReportFileName( eInform,'File=',file_smp )
  GOTO 9999
END IF
IF( multicomp .AND. nmc0 > 0 )THEN
  !------ Open sampler ambient output file
  OPEN( UNIT=lun_asmp,FILE=file_asmp,STATUS='NEW',IOSTAT=ios )
  IF( ios /= 0 )THEN
    nError   = OP_ERROR
    eRoutine = 'create_smp'
    eMessage = 'Error opening sampler ambient output file'
    CALL ReportFileName( eInform,'File=',file_asmp )
    GOTO 9999
  END IF
END IF

IF( lGridOut )THEN

!------ Open file

  CALL SamplerGridHeader()
  IF( nError /= NO_ERROR )GOTO 9999

  CALL SamplerGridCloseHeader()
  IF( nError /= NO_ERROR )GOTO 9999

ELSE !- Single-point Sensors

!------ Allocate name array

  IF( lWrap )THEN
    i = 10
  ELSE
    i = SIZE(smp_vname)+1
  END IF
  ALLOCATE( xnam(i),STAT=ios )
  IF( ios /= 0 )THEN
    nError   = OP_ERROR
    eRoutine = 'create_smp'
    eMessage = 'Error allocating variable name array'
    CALL ReportFileName( eInform,'File=',file_smp )
    GOTO 9999
  END IF

!------ Write EPA sampler file header section

  WRITE(lun_smp,'(A,F6.1)',IOSTAT=ios) '# Version: ',FLOAT(SamplerFileVersion)/10.
  IF( multicomp .AND. ios == 0 .AND. nmc0 > 0 )WRITE(lun_asmp,'(A,F6.1)',IOSTAT=ios) '# Version: ',FLOAT(SamplerFileVersion)/10.
  IF( ios /= 0 )THEN
    nError   = WR_ERROR
    eRoutine = 'create_smp'
    eMessage = 'Error writing sampler version number'
    CALL ReportFileName( eInform,'File=',file_smp )
    GOTO 9999
  END IF

  CALL time_cnv_sampler( 0.,.FALSE.,lymd,hour,min,sec,year,month,day,ctem )

!------ Write averaging time and output interval
!       N.B. zero averaging time for instantaneous output; -9999. for irregular avg/output times

  IF( lAvg )THEN
    IF( lSmpOutList )THEN
      atem = '-9999. -9999.'
    ELSE
      WRITE(atem,'(F10.1,1X,F10.1)',IOSTAT=ios) dtSmpOut,dtSmpOut
    END IF
  ELSE IF( dtSmpOut > 0. )THEN
    WRITE(atem,'(F10.1,1X,F10.1)',IOSTAT=ios) 0.,dtSmpOut
  ELSE IF( mxlev_smp == 0 )THEN
    WRITE(atem,'(F10.1,1X,F10.1)',IOSTAT=ios) 0.,delt
  ELSE
   atem = '0. -9999.'
  END IF

  SELECT CASE( lmap )
    CASE( I_LATLON )
      cmap = 'LATLON'
      ios  = 0

    CASE( I_UTM )
      cmap = 'UTM'
      WRITE(cmap,'(A3,1X,I4)',IOSTAT=ios) 'UTM', utm_zone

    CASE( I_CARTESIAN,I_METERS )
      WRITE(xtem,'(F11.3)') xref; WRITE(ytem,'(F11.3)') yref
      WRITE(clat,'(F11.4)') lat0; WRITE(clon,'(F11.4)') lon0
      IF( lmap == I_CARTESIAN )THEN
        cmap = 'CARTESIAN'
      ELSE
        cmap = 'METERS'
      END IF
      WRITE(cmap,'(A)',IOSTAT=ios) TRIM(ADJUSTL(cmap))// ' ('//TRIM(ADJUSTL(xtem))//','//TRIM(ADJUSTL(ytem))//')=&
                                                           ('//TRIM(ADJUSTL(clon))//','//TRIM(ADJUSTL(clat))//')'

    CASE DEFAULT
      cmap = 'UNKNOWN'
      ios  = 0

  END SELECT

  IF( ios /= 0 )THEN
    nError   = WR_ERROR
    eRoutine = 'create_smp'
    eMessage = 'Error writing coordinate description to sampler output file'
    CALL ReportFileName( eInform,'File=',file_smp )
    GOTO 9999
  END IF

  WRITE(lun_smp,'(A1,1X,I6,1X,A,1X,A,1X,A)',IOSTAT=ios) '#',nsmp,TRIM(ADJUSTL(ctem)),TRIM(ADJUSTL(atem)),TRIM(ADJUSTL(cmap))
  IF( multicomp .AND. ios == 0 .AND. nmc0 > 0 )WRITE(lun_asmp,'(A1,1X,I6,1X,A,1X,A,1X,A)',IOSTAT=ios) '#',nsmp,TRIM(ADJUSTL(ctem)),TRIM(ADJUSTL(atem)),TRIM(ADJUSTL(cmap))
  IF( ios /= 0 )THEN
    nError   = WR_ERROR
    eRoutine = 'create_smp'
    eMessage = 'Error writing sampler descriptive record'
    CALL ReportFileName( eInform,'File=',file_smp )
    GOTO 9999
  END IF

  DO i = 1,nsmp !Loop through number of receptors
    ctem = TRIM(smp(i)%type)
    IF( BTEST(smp(i)%stype,STB_INTEGRATE) )THEN
      ctem = TRIM(ctem)//':INT'
    END IF
    btem = TRIM(smp(i)%var)
    IF( smp(i)%is /= NOT_SET_I )THEN
      utem = TRIM(material(typeID(smp(i)%is)%imat)%Unit)//'/m3'
      IF( BTEST(smp(i)%stype,STB_INTEGRATE) )utem = TRIM(utem)//'-s'
    END IF
    IF( BTEST(smp(i)%stype,STB_MULT) )THEN
      IF( INDEX(TRIM(btem),'(') == 0 )THEN
        CALL BuildMCspecieList( smp(i),MAXC,string )
        IF( string(1:5) == 'ERROR' )THEN
          nError = WR_ERROR
          eRoutine = 'create_smp'
          eMessage = 'Error creating string with multi-component names'
          eInform  = 'Too many names to put in string'
          GOTO 9999
        END IF
        btem = TRIM(btem)//':'//TRIM(string)
      END IF
      CALL GetMCspecieUnits( smp(i),string )
      utem = TRIM(utem)//' '//TRIM(string)
      IF( BTEST(smp(i)%stype,STB_INTEGRATE) )utem = TRIM(utem)//'-s'
    END IF
    WRITE(lun_smp,'(A1,2(F13.4),2(F8.2),1x,A,1x,A,1x,A)',IOSTAT=ios) '#',smp(i)%x,smp(i)%y,smp(i)%h+hmin,smp(i)%zh, &
                                                                     TRIM(ctem),TRIM(btem),TRIM(utem)
    IF( multicomp .AND. ios == 0 .AND. nmc0 > 0 )WRITE(lun_asmp,'(A1,2(F13.4),2(F8.2),1x,A,1x,A,1x,A)',IOSTAT=ios) '#',smp(i)%x,smp(i)%y,&
                                                                     smp(i)%h+hmin,smp(i)%zh,TRIM(ctem),TRIM(btem),TRIM(utem)
    IF( ios /= 0 )THEN
      nError   = WR_ERROR
      eRoutine = 'create_smp'
      eMessage = 'Error writing sampler output file'
      CALL ReportFileName( eInform,'File=',file_smp )
      GOTO 9999
    END IF
  END DO

  IF( multicomp )THEN
    IF( lWrap )THEN
      i = 10
    ELSE
      i = SIZE(asmp_vname)+1
    END IF
    ALLOCATE( anam(i),STAT=ios )
    IF( ios /= 0 )THEN
      nError   = OP_ERROR
      eRoutine = 'create_smp'
      eMessage = 'Error allocating ambient variable name array'
      CALL ReportFileName( eInform,'File=',file_smp )
      GOTO 9999
    END IF
    anam(1) = 'T'
    i1a = 1
    iva = 0
  END IF

!------ Write sampler output file header

  IF( lWrap )THEN
    WRITE(lun_smp,*,IOSTAT=ios) nvarsmp+1
    IF( ios /= 0 )THEN
      nError   = WR_ERROR
      eRoutine = 'create_smp'
      eMessage = 'Error writing sampler output file'
      CALL ReportFileName( eInform,'File=',file_smp )
      GOTO 9999
    END IF
  END IF

  IF( nsmp < 10**3 )THEN      !Decimal numbering
    base = 10
  ELSE IF( nsmp < 16**3 )THEN !Hexidecimal numbering
    base = 16
  ELSE                        !AlphaNumeric numbering
    base = 36
  END IF

  base2 = base*base
  xnam(1) = 'T'
  i1 = 1
  iv = 0

  DO i = 1,nsmp

    nb(1) = i/base2
    nb(2) = (i-nb(1)*base2)/base
    nb(3) = (i-nb(1)*base2-nb(2)*base)
    DO j = 1,3
      IF( nb(j) < 10 )THEN
        stmp(j:j) = CHAR(nb(j)+NUMB_OFFSET)
      ELSE
        stmp(j:j) = CHAR(nb(j)+ALPH_OFFSET)
      END IF
    END DO

    IF( BTEST(smp(i)%stype,STB_OUTLOC) )THEN
      DO j = 1,3
        iv = iv + 1
        i1 = i1 + 1
        xnam(i1) = TRIM(smp_vname(iv))//stmp
        IF( i1 == 10 .AND. lWrap )CALL write_smp_name( i1,xnam )
      END DO
      IF( BTEST(smp(i)%stype,STB_LOS) )THEN
        DO j = 1,3
          iv = iv + 1
          i1 = i1 + 1
          xnam(i1) = TRIM(smp_vname(iv))//stmp
          IF( i1 == 10 .AND. lWrap )CALL write_smp_name( i1,xnam )
        END DO
      END IF
    END IF

    IF( BTEST(smp(i)%stype,STB_PART) )THEN
      IF( BTEST(smp(i)%stype,STB_INTEGRATE) .OR. BTEST(smp(i)%stype,STB_LOS) )THEN
        nvar = 3
      ELSE IF( lAvg )THEN
        nvar = 3
      ELSE
        nvar = 4
      END IF
      ngrp = smp(i)%nvar / nvar
      DO is = 1,ngrp
        WRITE(sbgrp,'(I3.3)',IOSTAT=ios) is
        IF( ios /= 0 )THEN
          nError   = WR_ERROR
          eRoutine = 'create_smp'
          eMessage = 'Error writing subgroup number for particle sensor'
          CALL ReportFileName( eInform,'File=',file_smp )
          GOTO 9999
        END IF
        DO j = 1,nvar
          iv = iv + 1
          i1 = i1 + 1
          xnam(i1) = TRIM(smp_vname(iv))//stmp//'_'//sbgrp
          IF( i1 == 10 .AND. lWrap )CALL write_smp_name( i1,xnam )
        END DO
      END DO

    ELSE IF( multicomp )THEN
      DO j = 1,smp(i)%nvar
        iv = iv + 1
        i1 = i1 + 1
        xnam(i1) = TRIM(smp_vname(iv))//stmp
        IF( i1 == 10 .AND. lWrap )CALL write_smp_name( i1,xnam )
      END DO
      IF( smp(i)%nmc > 0 )THEN
        DO j = 1,3+smp(i)%nmc
          iva = iva + 1
          i1a = i1a + 1
          anam(i1a) = TRIM(asmp_vname(iva))//stmp
          IF( i1a == 10 .AND. lWrap )THEN
            CALL write_asmp_name( i1a,anam )
          END IF
        END DO
      END IF
    ELSE
      DO j = 1,smp(i)%nvar
        iv = iv + 1
        i1 = i1 + 1
        xnam(i1) = TRIM(smp_vname(iv))//stmp
        IF( i1 == 10 .AND. lWrap )CALL write_smp_name( i1,xnam )
      END DO
    END IF

  END DO  !i=1,nsmp

  IF( i1 > 0 .AND. lWrap )CALL write_smp_name( i1,xnam )

  IF( lWrap )THEN
    WRITE(lun_smp,'(A)',IOSTAT=ios) TRIM(stitle)
    IF( ios /= 0 )THEN
      nError   = WR_ERROR
      eRoutine = 'create_smp'
      eMessage = 'Error writing sampler output file'
      CALL ReportFileName( eInform,'File=',file_smp )
      GOTO 9999
    END IF
  ELSE IF( multicomp )THEN
    CALL write_smp_name( i1,xnam )
    IF( nError /= NO_ERROR )GOTO 9999
    CALL write_asmp_name( i1a,anam )
    IF( nError /= NO_ERROR )GOTO 9999
  ELSE
    CALL write_smp_name( i1,xnam )
    IF( nError /= NO_ERROR )GOTO 9999
  END IF

END IF

!------ Zero sampler arrays

DO i = 1,nsmp
  DO j = 1,smp(i)%nvar
    smp(i)%dsmp(j) = 0.
  END DO
  smp(i)%csum = 0.
END DO

t_smp = t

CLOSE(lun_smp,IOSTAT=ios)
IF( multicomp )CLOSE(lun_asmp,IOSTAT=ios)
9999 CONTINUE

IF( ALLOCATED(xnam) )DEALLOCATE( xnam,STAT=ios )
IF( multicomp )THEN
  IF( ALLOCATED(anam) )DEALLOCATE( anam,STAT=ios )
END IF

RETURN
END SUBROUTINE create_smp

!==============================================================================

SUBROUTINE write_smp_name( n,xnam )

INTEGER,                    INTENT( INOUT ) :: n
CHARACTER(8), DIMENSION(n), INTENT( IN    ) :: xnam

INTEGER i, ios

IF( lWrap )THEN

  WRITE(lun_smp,'(10A8)',IOSTAT=ios) (xnam(i),i=1,n)
  IF( ios /= 0 )THEN
    nError   = WR_ERROR
    eRoutine = 'write_smp_name'
    eMessage = 'Error writing sampler names'
    CALL ReportFileName( eInform,'File=',file_smp )
    GOTO 9999
  END IF

  n = 0

ELSE

  DO i = 1,n
    IF( i == 1 .OR. xnam(i)(1:1) == 'X' .OR. xnam(i)(1:1) == 'Y' )THEN
      WRITE(lun_smp,FMT='(A15)',ADVANCE='NO',IOSTAT=ios) ADJUSTL(TRIM(xnam(i)))
    ELSE
      WRITE(lun_smp,FMT='(A12)',ADVANCE='NO',IOSTAT=ios) ADJUSTL(TRIM(xnam(i)))
    END IF
    IF( ios /= 0 )THEN
      nError   = WR_ERROR
      eRoutine = 'write_smp_name'
      eMessage = 'Error writing sampler names'
      CALL ReportFileName( eInform,'File=',file_smp )
      GOTO 9999
    END IF
  END DO

  WRITE(lun_smp,FMT='()',ADVANCE='YES',IOSTAT=ios) !End of record

END IF

9999 CONTINUE

RETURN
END SUBROUTINE write_smp_name

!==============================================================================

SUBROUTINE write_asmp_name( n,anam )

INTEGER,                    INTENT( INOUT ) :: n
CHARACTER(8), DIMENSION(n), INTENT( IN    ) :: anam

INTEGER lsmp

lsmp    = lun_smp
lun_smp = lun_asmp

! Change lun_smp to point to lun_asmp temporarily
CALL write_smp_name( n,anam )

lun_smp = lsmp

RETURN
END SUBROUTINE write_asmp_name
!==============================================================================

SUBROUTINE reallocate_smp_vname( inc )

USE sampler_fi
USE error_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: inc

INTEGER alloc_stat

CHARACTER(5),  DIMENSION(:), ALLOCATABLE :: tmp_vname
CHARACTER(16), DIMENSION(:), ALLOCATABLE :: tmp_units

IF( nvarAlloc == 0 )THEN
  nvarAlloc = inc
  ALLOCATE( smp_vname(inc),smp_units(inc),STAT=alloc_stat )
  smp_units(1:inc) = ' ' !Default is no units
ELSE
  IF( nvarsmp+inc > nvarAlloc )THEN
  ALLOCATE( tmp_vname(nvarsmp),tmp_units(nvarsmp),STAT=alloc_stat )
  IF( alloc_stat == 0 )THEN
    tmp_vname = smp_vname(1:nvarsmp); tmp_units = smp_units(1:nvarsmp)
    DEALLOCATE( smp_vname,smp_units,STAT=alloc_stat )
    nvarAlloc = nvarAlloc + inc
    ALLOCATE( smp_vname(nvarAlloc),smp_units(nvarAlloc),STAT=alloc_stat )
    IF( alloc_stat == 0 )THEN
      smp_vname(1:nvarsmp) = tmp_vname; smp_units(1:nvarsmp) = tmp_units
      DEALLOCATE( tmp_vname,tmp_units,STAT=alloc_stat )
    END IF
    smp_units(nvarsmp+1:nvarAlloc) = ' '
  END IF
  ELSE
    alloc_stat = 0
  END IF
END IF

IF( alloc_stat /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'reallocate_smp_vname'
  eMessage = 'Error allocating sensor name array'
END IF

RETURN
END SUBROUTINE reallocate_smp_vname

!==============================================================================

SUBROUTINE reallocate_compname( inc )

USE sampler_fi
USE error_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: inc

INTEGER alloc_stat, n

CHARACTER(64), DIMENSION(:), ALLOCATABLE :: tmp_name

IF( .NOT.ALLOCATED(compName) )THEN
  ALLOCATE( compName(inc),STAT=alloc_stat )
ELSE
  n = SIZE(compName)
  ALLOCATE( tmp_name(n),STAT=alloc_stat )
  IF( alloc_stat == 0 )THEN
    tmp_name = compName
    DEALLOCATE( compName,STAT=alloc_stat )
    ALLOCATE( compName(n+inc),STAT=alloc_stat )
    IF( alloc_stat == 0 )THEN
      compName(1:n) = tmp_name
      DEALLOCATE( tmp_name,STAT=alloc_stat )
    END IF
  END IF
END IF

IF( alloc_stat /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'reallocate_compname'
  eMessage = 'Error allocating sensor multicomponent name array'
END IF

RETURN
END SUBROUTINE reallocate_compname

!==============================================================================

SUBROUTINE SkipSamplerHeader()

!------ Skip sampler header records

INTEGER ios, is, i

CHARACTER(256) string, str

IF( lBinOut )RETURN

REWIND( UNIT=lun_smp,IOSTAT=ios )
IF( multicomp .AND. nmc0 > 0 )REWIND( UNIT=lun_asmp,IOSTAT=ios )
IF( lGridOut )THEN   !Read until <\Descriptor>; follow with terminal record for gridded output

  str = '</Descriptor>'
  DO
    READ(UNIT=lun_smp,FMT='(A)',IOSTAT=ios,ERR=9999) string
    IF( ios /= 0 )GOTO 9999
    CALL untabify( string )
    string = TRIM(ADJUSTL(string))
    IF( TRIM(str) == TRIM(string) )THEN
      CALL SamplerGridCloseHeader() !Last record on gridded output file
      IF( nError /= 0 )GOTO 9999
      EXIT
    END IF
  END DO

ELSE

  DO i = 1,nsmp+2
    READ(UNIT=lun_smp, FMT=*,IOSTAT=ios,ERR=9999)  !Skip header section
    IF( multicomp .AND. nmc0 > 0 )READ(UNIT=lun_asmp,FMT=*,IOSTAT=ios,ERR=9999)  !Skip header section
  END DO
  IF( lWrap )THEN                    !Skip nvar + wrapped lines with variable names + title
    is = 2 + INT((1+nvarsmp+9)/10)
    DO i = 1,is
      READ(UNIT=lun_smp,FMT=*,IOSTAT=ios,ERR=9999)
    END DO
  ELSE
    READ(UNIT=lun_smp,FMT=*,IOSTAT=ios,ERR=9999)  !Skip single line with variable names
    IF( multicomp .AND. nmc0 > 0 )READ(UNIT=lun_asmp,FMT=*,IOSTAT=ios,ERR=9999)  !Skip single line with variable names
  END IF
#if !defined(LINUX)
  ENDFILE( UNIT=lun_smp,IOSTAT=ios )
#endif
END IF

9999 CONTINUE
IF( ios /= 0 )THEN
  nError = RD_ERROR
  eRoutine = 'SkipSamplerHeader'
  eMessage = 'Error reading sampler file header'
END IF

RETURN
END SUBROUTINE SkipSamplerHeader

!==============================================================================

SUBROUTINE ReadLastSamplerTime( nback )

!------ Read last time on sampler file

INTEGER, INTENT( OUT ) :: nback

INTEGER ios, is, i, n
REAL    ts

CHARACTER(256) string, strLast

IF( lBinOut )THEN
  CALL rst_smpBinaryInit() !N.B. This does not read last sampler time
  IF( nError /= NO_ERROR )GOTO 9999
  nback = 0
  RETURN
END IF

IF( lGridOut )THEN

  REWIND(UNIT=lun_smp,IOSTAT=ios)
  IF( ios /= 0 )THEN
    nError   = RD_ERROR
    eRoutine = 'ReadLastSamplerTime'
    eMessage = 'Error rewinding gridded sampler file'
    CALL ReportFileName( eInform,'File=',file_smp )
    GOTO 9999
  END IF

  t_smp = -1.E+36

  strLast = '</GridOutput>'

  DO

    READ(UNIT=lun_smp,FMT='(A)',IOSTAT=ios) string
    IF( ios /= 0 )EXIT

    CALL untabify( string )

    string = TRIM(ADJUSTL(string))
    IF( TRIM(string) == TRIM(strLast) )Exit !Leave file at end of last record

    ts = ParseTimeString( string )
    IF( nError /= NO_ERROR )GOTO 9999
    IF( ts /= NOT_SET_R )t_smp = ts

  END DO

ELSE IF( lWrap )THEN

  nback = 0
  ReadLastRecord : DO  !Backspace until all output at last time can be read successfully
    nback = nback + 1
    DO i = 1,nback
      BACKSPACE( UNIT=lun_smp,IOSTAT=ios )
      IF( ios /= 0 )EXIT
    END DO
    IF( ios /= 0 )THEN
      nError   = RD_ERROR
      eRoutine = 'ReadLastSamplerTime'
      eMessage = 'Error attempting to read last sampler output time'
      CALL ReportFileName( eInform,'File=',file_smp )
      GOTO 9999
    END IF
    READ(lun_smp,FMT=*,IOSTAT=ios) t_smp, &
                   ((smp(is)%dsmp(i),i=1,smp(is)%nvar),is=1,nsmp)
    IF( ios == 0 )EXIT
  END DO ReadLastRecord
  nback = nback - 1

ELSE

  BACKSPACE( UNIT=lun_smp,IOSTAT=ios )
  IF( ios /= 0 )THEN  !File maybe too large for BACKSPACE; approach from beginning
    REWIND(lun_smp,IOSTAT=ios)
    READ(lun_smp,*,IOSTAT=ios) !Skip header
    n = 0
    DO
      READ(lun_smp,*,IOSTAT=ios) t_smp
      IF( ios > 0 )THEN
        nError   = RD_ERROR
        eRoutine = 'ReadLastSamplerTime'
        eMessage = 'Error positioning sampler file to read last time'
        CALL ReportFileName( eInform,'File=',file_smp )
        GOTO 9999
      ELSE IF( ios < 0 )THEN
        EXIT
      END IF
      n = n + 1
    END DO
    REWIND(lun_smp,IOSTAT=ios)
    READ(lun_smp,*,IOSTAT=ios) !Skip header
    DO i = 1,n-1
      READ(lun_smp,*,IOSTAT=ios) t_smp
      IF( ios /= 0 )THEN
      END IF
    END DO

  END IF
  READ(lun_smp,*,IOSTAT=ios) t_smp
  IF( ios /= 0 )THEN
    nError   = RD_ERROR
    eRoutine = 'ReadLastSamplerTime'
    eMessage = 'Error attempting to read last sampler output time'
    CALL ReportFileName( eInform,'File=',file_smp )
    GOTO 9999
  END IF
  nback = 1
  IF( multicomp .AND. nmc0 > 0 )THEN
    BACKSPACE( UNIT=lun_asmp,IOSTAT=ios )
    IF( ios /= 0 )THEN
      nError   = RD_ERROR
      eRoutine = 'ReadLastSamplerTime'
      eMessage = 'Error backspacing to read last ambient sampler output time'
      CALL ReportFileName( eInform,'File=',file_asmp )
      eRoutine = 'Line may be too long. Try re-running with wrapped output'
      GOTO 9999
    END IF
    READ(lun_asmp,*,IOSTAT=ios) t_smp
    IF( ios /= 0 )THEN
      nError   = RD_ERROR
      eRoutine = 'ReadLastSamplerTime'
      eMessage = 'Error attempting to read last ambient sampler output time'
      CALL ReportFileName( eInform,'File=',file_asmp )
      GOTO 9999
    END IF
  END IF
END IF

9999 CONTINUE

RETURN
END SUBROUTINE ReadLastSamplerTime

!==============================================================================

REAL FUNCTION ParseTimeString( string)  RESULT( ts )

CHARACTER(*), INTENT( IN  ) :: string

INTEGER ios, iq1, iq2, nch

CHARACTER(64) str

ts  = NOT_SET_R !Default value

str = '<OutputTime'; nch = LEN_TRIM(str)

IF( string(1:nch) == TRIM(str) )THEN

  iq1 = INDEX(string,QQ)
  iq2 = INDEX(string,QQ,.TRUE.)
  IF( iq1 == 0 .OR. iq2-iq1 < 2 )THEN
    nError   = IV_ERROR
    eRoutine = 'ParseTimeString'
    eMessage = 'Invalid OutputTime string on gridded sampler file'
    eInform  = TRIM(string)
    eAction  = 'Missing value in double quotes'
    GOTO 9999
  END IF

  READ(string(iq1+1:iq2-1),FMT=*,IOSTAT=ios) ts
  IF( ios /= 0 )THEN
    nError  = IV_ERROR
    eRoutine = 'ParseTimeString'
    eMessage = 'Error reading OutputTime on gridded sampler file'
    eInform  = 'Bad string: '//TRIM(string)
    GOTO 9999
  END IF

END IF

9999 CONTINUE

RETURN
END FUNCTION ParseTimeString

!==============================================================================

SUBROUTINE SetSamplerListTime()

!------ Find appropriate start times & interval from list, based on current sampler time

IF( .NOT.lSmpOutList )THEN

  dtSmpOut   = 0.
  tStartSamp = 0.

ELSE

  SampTimeList => FirstSampTimeList
  tNextSamp   = HUGE(0.)
  DO WHILE( ASSOCIATED(SampTimeList%next) )
    tNextSamp = SampTimeList%next%tStart
    IF( t_smp < tNextSamp )EXIT
    SampTimeList => SampTimeList%next
  END DO
  tStartSamp = SampTimeList%tStart
  dtSmpOut   = SampTimeList%dtSamp

END IF

RETURN
END SUBROUTINE SetSamplerListTime

!==============================================================================

SUBROUTINE PositionSampler( nback,tol )

!------ Position sampler file to current project time

INTEGER, INTENT( IN ) :: nback
REAL,    INTENT( IN ) :: tol

INTEGER ios
REAL    ts

CHARACTER(64) string

IF( t > tEndSamp )RETURN

IF( lBinOut )THEN
  CALL position_binSmp( tol )
  GOTO 9999
END IF

IF( lGridOut )THEN

  IF( t_smp-t > SPACING(t) )THEN !Rewind and search down (again)

    REWIND(lun_smp)

    t_smp = NOT_SET_R

    DO  !Read until sampler time is greater than project time

      READ(UNIT=lun_smp,FMT='(A)',IOSTAT=ios) string

      ! Check for end of file in all compilers
      IF( IS_IOSTAT_END(ios) )ios = -1

      IF( ios < 0 )THEN
        BACKSPACE(lun_smp); GOTO 9999
      ELSE IF( ios > 0 )THEN
        GOTO 9999
      END IF

      CALL untabify( string )

      string = TRIM(ADJUSTL(string))

      ts = ParseTimeString( string )
      IF( nError /= NO_ERROR )GOTO 9999
      IF( ts == NOT_SET_R )CYCLE
      IF( ts > t+SPACING(t) )THEN
        BACKSPACE(lun_smp)
        CALL SamplerGridCloseHeader()  !End-of-file string
        IF( nError /= 0 )GOTO 9999
        EXIT
      ELSE
        t_smp = ts
      END IF

    END DO

  END IF

ELSE

  DO WHILE( ABS(t-t_smp) > tol ) !Backspace until t_smp = t

    CALL BackSpaceSampler( nback,t_smp,ios )
    IF( ios /= 0 .OR. t_smp < t )THEN
      nError   = RD_ERROR
      eRoutine = 'PositionSampler'
      eMessage = 'Error backspacing through sampler output file'
      eInform  = 'Sampler time mismatch with puff output'
      eAction  = 'Project cannot be restarted'
      GOTO 9999
    END IF

  END DO

#if !defined(LINUX)
  ENDFILE( UNIT=lun_smp,IOSTAT=ios )
  BACKSPACE( UNIT=lun_smp,IOSTAT=ios )
#endif

END IF

9999 CONTINUE

RETURN
END SUBROUTINE PositionSampler

!==============================================================================

SUBROUTINE BackSpaceSampler( nback,tx,ios )

INTEGER, INTENT( IN  ) :: nback
REAL,    INTENT( OUT ) :: tx
INTEGER, INTENT( OUT ) :: ios

INTEGER i, is
REAL    dum

IF( lWrap )THEN
  DO i = 1,nback * 2
    BACKSPACE( UNIT=lun_smp,IOSTAT=ios )
    IF( ios /= 0 )EXIT
  END DO
  IF( ios == 0 )READ(lun_smp,FMT=*,IOSTAT=ios) tx, &
                 ((dum,i=1,smp(is)%nvar),is=1,nsmp)
ELSE
  BACKSPACE( UNIT=lun_smp,IOSTAT=ios ); IF( ios == 0 ) &
  BACKSPACE( UNIT=lun_smp,IOSTAT=ios ); IF( ios == 0 ) &
  READ(lun_smp,FMT=*,IOSTAT=ios) tx
  IF( multicomp .AND. nmc0 > 0 )THEN
    BACKSPACE( UNIT=lun_asmp,IOSTAT=ios ); IF( ios == 0 ) &
    BACKSPACE( UNIT=lun_asmp,IOSTAT=ios ); IF( ios == 0 ) &
    READ(lun_asmp,FMT=*,IOSTAT=ios) tx
  END IF
END IF

RETURN
END SUBROUTINE BackSpaceSampler

!==============================================================================

SUBROUTINE ReadSamplerSum( nback )

!------ Read sums for integrated output
!       N.B. Not designed for gridded output

INTEGER, INTENT( IN ) :: nback

INTEGER ios, i, is
REAL    x, y, z, az, el, dist

IF( lGridOut )THEN
  nError   = IV_ERROR
  eRoutine = 'ReadSamplerSum'
  eMessage = 'Invalid usage with gridded outpout'
  CALL ReportFileName( eInform,'File=',file_smp )
  GOTO 9999
END IF

IF( lBinOut )THEN

  CALL readPrev_binSmp()
  GOTO 9999

END IF

IF( lWrap )THEN
  DO i = 1,nback
    BACKSPACE( UNIT=lun_smp,IOSTAT=ios )
    IF( ios /= 0 )EXIT
  END DO
ELSE
  BACKSPACE( UNIT=lun_smp,IOSTAT=ios )
END IF
IF( ios /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'ReadSamplerSum'
  eMessage = 'Error reading sampler output file'
  CALL ReportFileName( eInform,'File=',file_smp )
  GOTO 9999
END IF

!------ Read last record
!       N.B. Formats (width only) must agree with output in subroutine write_smp

IF( lWrap )THEN

  READ(lun_smp,*,IOSTAT=ios) t_smp,((smp(is)%dsmp(i),i=1,smp(is)%nvar),is=1,nsmp)
  IF( ios /= 0 )GOTO 9998

ELSE

  READ(lun_smp,FMT='(ES15.0)',ADVANCE='NO',IOSTAT=ios,ERR=9998) t_smp
  IF( ios /= 0 )GOTO 9998

  DO is = 1,nsmp

    IF( BTEST(smp(is)%stype,STB_OUTLOC) )READ(lun_smp,FMT='(2ES15.0,ES12.0)',ADVANCE='NO',IOSTAT=ios) x,y,z
    IF( ios /= 0 )GOTO 9998

    IF( BTEST(smp(is)%stype,STB_LOS) )READ(lun_smp,FMT='(3ES12.0)',ADVANCE='NO',IOSTAT=ios) az,el,dist
    IF( ios /= 0 )GOTO 9998

    DO i = 1,smp(is)%nvar
      READ(lun_smp,FMT='(ES12.0)',ADVANCE='NO',IOSTAT=ios) smp(is)%dsmp(i)
      IF( ios /= 0 )GOTO 9998
    END DO

  END DO

END IF

9999 CONTINUE
RETURN

9998 CONTINUE
nError   = RD_ERROR
eRoutine = 'ReadSamplerSum'
eMessage = 'Error reading last record on output file for restart'
CALL ReportFileName( eInform,'File=',file_smp )
GOTO 9999

END SUBROUTINE ReadSamplerSum

!==============================================================================

SUBROUTINE UnProcessSensorX( ss,dsmp )  !X until I can merge with simpler UnProcessSensor ***

TYPE( sensor), INTENT( INOUT ) :: ss   !Sensor structure
REAL, DIMENSION(:), POINTER    :: dsmp !Sensor output array

INTEGER nv, is, j

IF( BTEST(ss%stype,STB_PART) )THEN
  IF( lAvg )THEN
    nv = 3      !Mean,Variance,Number density
  ELSE
    nv = 4      !Mean,Variance,Timescale,Number density
  END IF
  DO is = 1,ss%nbin
    j = (is-1)*nv
    dsmp(j+1) = dsmp(j+1) / ss%conv
    dsmp(j+2) = dsmp(j+2) / ss%conv**2
  END DO
ELSE IF( .NOT.BTEST(ss%stype,STB_MET) )THEN
  dsmp(1) = dsmp(1) / ss%conv
  dsmp(2) = dsmp(2) / ss%conv**2
END IF

IF( lAvg )THEN
  IF( .NOT.BTEST(ss%stype,STB_INTEGRATE) )THEN
    dsmp = dsmp * dtSmpOut
    IF( BTEST(ss%stype,STB_PART) )THEN
      DO is = 1,ss%nbin
        j = (is-1)*nv
        dsmp(j+2) = dsmp(j+2) * dtSmpOut  !Variance
      END DO
    ELSE IF( .NOT.BTEST(ss%stype,STB_INTEGRATE) )THEN
      dsmp(2) = dsmp(2) * dtSmpOut  !Variance
    END IF
  END IF
END IF

RETURN
END SUBROUTINE UnProcessSensorX

END MODULE SamplerUtil

!==============================================================================

SUBROUTINE AllocateSensor()

!------ Allocate sensor array

USE sampler_fi
USE default_fd
USE error_fi

IMPLICIT NONE

INTEGER i, alloc_stat

IF( nsmp == 0 )THEN
  nError = UK_ERROR
  eRoutine = 'AllocateSensor'
  eMessage = 'No valid sensor found'
  GOTO 9999
END IF

IF( nsmp > MAXSMP )THEN
  nError   = IV_ERROR
  eRoutine = 'AllocateSensor'
  WRITE(eMessage,'("Number of samplers ",I0," exceed the maximum allowed value of ",I0)')nsmp,MAXSMP
  eInform = "Use postprocessor to sample the integrated concentration output field"
  GOTO  9999
END IF

ALLOCATE( smp(nsmp),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError = UK_ERROR
  eRoutine = 'AllocateSensor'
  eMessage = 'Error allocating sensor array'
  GOTO 9999
END IF

!------ Set default values

DO i = 1,nsmp
  smp(i)%time = 0.
  NULLIFY( smp(i)%nextpoint )
  NULLIFY( smp(i)%dsmp )
  NULLIFY( smp(i)%gsmp )
  NULLIFY( smp(i)%mcID )
  smp(i)%nmc      = 0
  NULLIFY( smp(i)%asmp )
  NULLIFY( smp(i)%bin )
  smp(i)%nbin     = 0
  NULLIFY( smp(i)%FieldName )
  NULLIFY( smp(i)%FieldUnits )
  NULLIFY( smp(i)%SizeBin )
  smp(i)%name     = 'Not Set'
  smp(i)%is       = NOT_SET_I
  smp(i)%ie       = NOT_SET_I
  smp(i)%stype    = 0
  smp(i)%conv     = 1.0
  smp(i)%nx       = 1
  smp(i)%ny       = 1
  smp(i)%npts     = 1
  smp(i)%dx       = 0.
  smp(i)%dy       = 0.
  smp(i)%dxs      = 0.
  smp(i)%dys      = 0.
  smp(i)%autoLev  = NOT_SET_R
  smp(i)%rot      = 0.
  smp(i)%mvlos%x  = 0.
  smp(i)%mvlos%y  = 0.
  smp(i)%mvlos%z  = 0.
  smp(i)%mvlos%lx = 0.
  smp(i)%mvlos%ly = 0.
  smp(i)%mvlos%lz = 0.
  smp(i)%mvlos%r  = 0.
  smp(i)%speed    = 0.
  smp(i)%tprev    = 0.
END DO

9999 CONTINUE

RETURN
END

!==============================================================================

REAL FUNCTION norm_ang( ang )

IMPLICIT NONE

REAL, INTENT( IN ) :: ang

REAL a360

norm_ang = ang
a360     = -SIGN(360.,ang)

DO WHILE( norm_ang >= 360. .OR. norm_ang < 0. )
  norm_ang = norm_ang + a360
END DO

RETURN
END

!==============================================================================

SUBROUTINE set_smp_los( ss )

USE sampler_fi

IMPLICIT NONE

TYPE( sensor ), INTENT( INOUT ) :: ss         !Sensor structure

CALL set_dir_cosine( ss%az,ss%el,ss%lx,ss%ly,ss%lz )

RETURN
END

!==============================================================================

SUBROUTINE set_dir_cosine( az,el,lx,ly,lz )

IMPLICIT NONE

REAL, INTENT( IN  ) :: az, el
REAL, INTENT( OUT ) :: lx, ly, lz

REAL, EXTERNAL :: sind, cosd

lx = sind(az)*cosd(el)
ly = cosd(az)*cosd(el)
lz = sind(el)

RETURN
END

!==============================================================================

SUBROUTINE set_los_str( ss,xs,ys,los )

USE sampler_fi
USE los_fd

IMPLICIT NONE

TYPE( sensor  ), INTENT( IN  ) :: ss         !Sensor structure
REAL,            INTENT( IN  ) :: xs, ys     !Horizontal location
TYPE( los_str ), INTENT( OUT ) :: los        !LOS structure

los%x  = xs;    los%y  = ys;    los%z  = ss%z
los%lx = ss%lx; los%ly = ss%ly; los%lz = ss%lz
los%r  = ss%dist

RETURN
END

!=======================================================================

SUBROUTINE update_sensor_location( ss,ts )

!------ Update moving sensor location

USE sampler_fi
USE default_fd
USE scipuff_fi, ONLY : lter, lmap, I_LATLON
USE constants_fd

IMPLICIT NONE

TYPE( sensor ), INTENT( INOUT ) :: ss         !Sensor structure
REAL,           INTENT( IN    ) :: ts         !Current time (hrs.)

TYPE( WayPoint ), POINTER :: point, prev
TYPE( los_str ) :: los

REAL    fac, daz, hx, hy, xmap, ymap, ssdist
LOGICAL lNullify

REAL, EXTERNAL :: norm_ang

!------ Save previous location for integrated sensors
!       or zero LOS if no more way points

IF( BTEST(ss%stype,STB_INTEGRATE) .OR. lAvg )THEN
  IF( .NOT.ASSOCIATED(ss%nextpoint) )THEN
    ss%stype = IBCLR(ss%stype,STB_MOVING)
    ss%speed = 0.; ss%mvlos%r = 0.; ss%mvlos%lx = 1.; ss%mvlos%ly = 0.; ss%mvlos%lz = 0.
    GOTO 9999
  ELSE
    ss%mvlos%x = ss%x; ss%mvlos%y = ss%y; ss%mvlos%z = ss%z  !Save previous position
    ss%tprev   = ss%time
  END IF
END IF

lNullify = .FALSE.

!------ Check that current time is less than time of next way point

DO WHILE( ts >= ss%nextpoint%time )
  ss%time =  ss%nextpoint%time
  ss%x    =  ss%nextpoint%x
  ss%y    =  ss%nextpoint%y
  IF( lter )THEN
    CALL get_topogPrj( ss%x,ss%y,ss%h,hx,hy )
  END IF
  IF( BTEST(ss%stype,STB_AGL) )THEN
    ss%zh = ss%nextpoint%z
    ss%z  = ss%zh + ss%h
  ELSE
    ss%z  = MAX(ss%nextpoint%z,ss%h)
    ss%zh = ss%z - ss%h
  END IF
  ss%az   =  ss%nextpoint%az
  ss%el   =  ss%nextpoint%el
  ssdist  =  ss%dist
  ss%dist =  ss%nextpoint%dist
  point   => ss%nextpoint%next
  IF( ASSOCIATED(point) )THEN
    ss%nextpoint => point
  ELSE
    ss%dist = ssdist
    IF( BTEST(ss%stype,STB_INTEGRATE) .OR. lAvg )THEN
      lNullify = .TRUE.
      EXIT
    ELSE
      ss%stype = IBCLR(ss%stype,STB_MOVING)
      GOTO 9999
    END IF
  END IF
END DO

prev => ss%nextpoint%prev
fac = ss%nextpoint%time - prev%time
IF( fac > 0. )THEN
  fac = MIN((ts-prev%time)/fac,1.0)
ELSE
  fac = 1.
END IF

IF( lmap == I_LATLON )THEN
  CALL NextGreatCirclePoint( fac,prev%y,prev%x,ss%nextpoint%y,ss%nextpoint%x,ss%y,ss%x )
ELSE
  ss%x = prev%x + fac*(ss%nextpoint%x - prev%x)
  ss%y = prev%y + fac*(ss%nextpoint%y - prev%y)
END IF

IF( lter )CALL get_topogPrj( ss%x,ss%y,ss%h,hx,hy )

IF( BTEST(smp(nsmp)%stype,STB_AGL) )THEN
  ss%zh = prev%z + fac*(ss%nextpoint%z - prev%z)
  ss%z  = ss%zh + ss%h
ELSE
  ss%z  = MAX(prev%z + fac*(ss%nextpoint%z - prev%z),ss%h)
  ss%zh = ss%z - ss%h
END IF

!----- Vector from previous point to current point
!      N.B. Only for integrated sensors

IF( BTEST(ss%stype,STB_INTEGRATE) .OR. lAvg )THEN

  CALL mapfac( ss%x,ss%y,xmap,ymap )

  ss%mvlos%lx = (ss%x - ss%mvlos%x)/xmap
  ss%mvlos%ly = (ss%y - ss%mvlos%y)/ymap
  ss%mvlos%lz =  ss%z - ss%mvlos%z

  ss%mvlos%r = SQRT(ss%mvlos%lx**2 + ss%mvlos%ly**2 + ss%mvlos%lz**2)

  IF( ss%mvlos%r > SMALL )THEN
    ss%mvlos%lx = ss%mvlos%lx/ss%mvlos%r
    ss%mvlos%ly = ss%mvlos%ly/ss%mvlos%r
    ss%mvlos%lz = ss%mvlos%lz/ss%mvlos%r
    ss%speed    = ss%mvlos%r / (ts - ss%tprev) / 3600.
  ELSE
    ss%speed = 0.; ss%mvlos%r = 0.; ss%mvlos%lx = 1.; ss%mvlos%ly = 0.; ss%mvlos%lz = 0.
  END IF

END IF

IF( ss%nextpoint%az /= NOT_SET_R )THEN
  daz = ss%nextpoint%az - prev%az
  IF( ABS(daz) > 180. )daz = daz - SIGN(360.,daz)
  ss%az = prev%az + fac*daz; ss%az = norm_ang( ss%az )
END IF
IF( ss%nextpoint%el /= NOT_SET_R )THEN
  ss%el = prev%el + fac*(ss%nextpoint%el - prev%el)
END IF

IF( BTEST(smp(nsmp)%stype,STB_LOS) )THEN
  CALL set_smp_los( ss )
  CALL set_los_str( ss,ss%x,ss%y,los )
  CALL grnd_intersect( los,ssdist )
  IF( ss%nextpoint%dist < 0. )THEN
    ss%dist = ssdist
  ELSE
    ss%dist = prev%dist + fac*(ss%nextpoint%dist - prev%dist)
    IF( ssdist > 0. )ss%dist = MIN(ss%dist,ssdist)
  END IF
END IF

ss%time = ts

IF( lNullify )NULLIFY(ss%nextpoint)

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE ConvertSampCoord( outMap,inpMap,zone,x,y )

USE met_fi
USE coordinate_fd
USE sampler_fi

IMPLICIT NONE

INTEGER, INTENT( IN    ) :: outMap   !Output map type
INTEGER, INTENT( IN    ) :: inpMap   !Input  map type
INTEGER, INTENT( IN    ) :: zone     !UTM zone
REAL,    INTENT( INOUT ) :: x, y     !Horizontal coordinates (in/out)

INTEGER irv
REAL    xs, ys

TYPE( MapCoord ) :: coordI, coordO

INTEGER, EXTERNAL :: SWIMcnvCoord

IF( outMap == inpMap )RETURN

coordI = PrjCoord  !Set reference location and UTM zone (if applicable)
coordO = PrjCoord

IF( PrjCoord%type /= I_UTM )THEN  !Use input zone if project is not UTM
  coordI%zone = zone
  coordO%zone = zone
END IF

coordO%type = outMap
coordI%type = inpMap

IF( lCartOrig )THEN
  IF( inpMap == I_CARTESIAN )THEN
    coordI%reference%x = 0.
    coordI%reference%y = 0.
    coordI%reference%lat = cartLat0
    coordI%reference%lon = cartLon0
  ELSE IF( outMap == I_CARTESIAN )THEN
    coordO%reference%x = 0.
    coordO%reference%y = 0.
    coordO%reference%lat = cartLat0
    coordO%reference%lon = cartLon0
  END IF
END IF

xs = x; ys = y

irv = SWIMcnvCoord( xs,ys,coordI,x,y,coordO )

RETURN
END

!==============================================================================

SUBROUTINE NextGreatCirclePoint( fac,lat1,lon1,lat2,lon2,lat,lon )

!------ Find next point moving a fraction along great circle route
!       Note that equations are for positive longitude West, but
!       input is positive East

USE constants_fd, ONLY : PI180

IMPLICIT NONE

REAL, INTENT( IN  ) :: fac
REAL, INTENT( IN  ) :: lat1, lon1
REAL, INTENT( IN  ) :: lat2, lon2
REAL, INTENT( OUT ) :: lat,  lon

REAL rlat1, rlat2, rlon1, rlon2
REAL d, sd, a, b, x, y, z, c1, c2

REAL, EXTERNAL :: DistFunc

rlat1 = lat1*PI180; rlon1= -lon1*PI180
rlat2 = lat2*PI180; rlon2= -lon2*PI180

d = DistFunc( rlat1,rlon1,rlat2,rlon2 )

IF( d < 0.01 )THEN
  a = 1.-fac
  b = fac
ELSE
  sd = SIN(d)
  a  = SIN((1.-fac)*d)/sd
  b  = SIN(fac*d)/sd
END IF

c1 = COS(rlat1); c2 = COS(rlat2)

x = a*c1*COS(rlon1) + b*c2*COS(rlon2)
y = a*c1*SIN(rlon1) + b*c2*SIN(rlon2)
z = a*SIN(rlat1)    + b*SIN(rlat2)

lat = ATAN2(z,SQRT(x**2+y**2))/PI180
lon = -ATAN2(y,x)/PI180

RETURN
END

!==============================================================================

REAL FUNCTION DistFunc( rlat1,rlon1,rlat2,rlon2 ) RESULT( d )

IMPLICIT NONE

REAL rlat1, rlon1, rlat2, rlon2

d = 2.*ASIN(SQRT((SIN(0.5*(rlat1-rlat2)))**2 +  &
                    COS(rlat1)*COS(rlat2)*(SIN(0.5*(rlon1-rlon2)))**2))

RETURN
END


!===============================================================================

REAL FUNCTION ParseSensorTime( stringT,lRelTime ) RESULT( t )

USE default_fd

IMPLICIT NONE

CHARACTER(*), INTENT( IN  ) :: stringT
LOGICAL,      INTENT( OUT ) :: lRelTime    !TRUE for relative time; FALSE for [YYMMDD:]HH:MM:SS format

CHARACTER(256) string

INTEGER i, ios, ymd, ihr,imin
REAL    sec
LOGICAL lerr

REAL, EXTERNAL :: GetTime

lRelTime = .FALSE.

lerr = LEN_TRIM(stringT) < 7

IF( .NOT.lerr )THEN

  i = INDEX(stringT,':')
  IF( i == 9 .OR. i == 7 )THEN
    READ(stringT(1:i-1),*,IOSTAT=ios) ymd
    lerr = ios /= 0
    string = stringT(i+1:)
  ELSE IF( i == 3 )THEN
    ymd = NOT_SET_I
    string = stringT
  ELSE
    lerr = .TRUE.
  END IF

  IF( .NOT.lerr )THEN
    READ(string,FMT='(I2,1X,I2)',IOSTAT=ios) ihr,imin
    lerr = ios /= 0
    IF( .NOT.lerr )THEN
      READ(string(7:),*,IOSTAT=ios) sec
      lerr = ios /= 0
    END IF
  END IF

END IF

IF( lerr )THEN
  READ(stringT,*,IOSTAT=ios) t  !Try to read as relative time (simple real number)
  IF( ios == 0 )THEN
    lerr     = .FALSE.
    lRelTime = .TRUE.
  ELSE
    t = NOT_SET_R
  END IF
ELSE
  t = GetTime( ymd,ihr,imin,sec )
END IF

RETURN
END

!===============================================================================

REAL FUNCTION ParseTimeUnits( string,lValidUnits ) RESULT( s )

!------ Define scale factor converting time units to seconds
!       N.B. Only checks for upper case characters

IMPLICIT NONE

CHARACTER(*), INTENT( IN  ) :: string
LOGICAL,      INTENT( OUT ) :: lValidUnits

CHARACTER(512 ) stringU

lValidUnits = .TRUE.

stringU = TRIM(string)
CALL cupper( stringU )

SELECT CASE( TRIM(stringU) )
  CASE( 'S','SEC','SECOND','SECONDS' )
    s = 1.0
  CASE( 'M','MIN','MINUTE','MINUTES' )
    s = 60.
  CASE( 'H','HR','HRS','HOUR','HOURS' )
    s = 3600.
  CASE DEFAULT
    s = 1.0
    lValidUnits = .FALSE.
END SELECT

RETURN
END

!===============================================================================

REAL FUNCTION GetTime( iymd,ihour,imin,sec ) RESULT( tprj )

!------ Convert to SCIPUFF time

USE scipuff_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: iymd, ihour, imin
REAL,    INTENT( IN ) :: sec

INTEGER iyy, idd, imm, jul, jyy

INTEGER, EXTERNAL :: julian_day, days_in_year

IF( lymd )THEN

  IF( iymd == NOT_SET_I )THEN

    iyy = year_start
    imm = month_start
    idd = day_start

  ELSE

    iyy  = iymd/10000
    imm  = (iymd-10000*iyy)/100
    idd  = iymd-10000*iyy-100*imm

    IF( iyy < 50 )THEN
      iyy = iyy + 2000
    ELSE IF( iyy < 100 )THEN
      iyy = iyy + 1900
    END IF

  END IF

  jul  = julian_day( imm,idd,iyy )

  IF( iyy > year_start )THEN
    DO jyy = year_start,iyy-1
      jul = jul + days_in_year( jyy )
    END DO
  ELSE IF( iyy < year_start )THEN
    DO jyy = year_start-1,iyy,-1
      jul = jul - days_in_year( jyy )
    END DO
  END IF

  tprj = (jul - jul_start)*86400.

ELSE

  tprj = 0.

END IF

tprj = tprj + ihour*3600. + imin*60. + sec - tstart*3600.

RETURN
END

!==============================================================================

SUBROUTINE limint2( x,d,i1,i2,nx )

IMPLICIT NONE

INTEGER, PARAMETER :: N = 4

REAL,    INTENT( IN )  :: x, d
INTEGER, INTENT( OUT ) :: i1, i2
INTEGER, INTENT( IN )  :: nx

INTEGER m

m = 0
DO WHILE( x-FLOAT(m)*d >= 0. .AND. m <= N )
  i1 = -m
  m  = m + 1
END DO

m = 0
DO WHILE (x+FLOAT(m)*d <= FLOAT(nx) .AND. m <= N )
  i2 = m
  m  = m + 1
END DO

RETURN
END

!==============================================================================

SUBROUTINE limintx( x,d,x1,x2,i1,i2,nx )

IMPLICIT NONE

REAL, PARAMETER :: N = 4  !Should match value in limint2

REAL,    INTENT( IN )  :: x, d
REAL,    INTENT( IN )  :: x1, x2
INTEGER, INTENT( OUT ) :: i1, i2
INTEGER, INTENT( IN )  :: nx

INTEGER m

m = 0
DO WHILE( x-FLOAT(m)*d >= 0. .AND. m <= N )
  i1 = -m
  m  = m + 1
END DO

DO WHILE( x+FLOAT(i1)*d > MAX(x1,0.) )
  i1 = i1 - 1
END DO

m = 0
DO WHILE (x+FLOAT(m)*d <= FLOAT(nx) .AND. m <= N )
  i2 = m
  m  = m + 1
END DO

DO WHILE (x+FLOAT(i2)*d < MIN(x2,FLOAT(nx)) )
  i2 = i2 + 1
END DO

RETURN
END

!==============================================================================

SUBROUTINE SensorNumStr( ismp,str )

IMPLICIT NONE

INTEGER,      INTENT( IN  ) :: ismp
CHARACTER(*), INTENT( OUT ) :: str

INTEGER nc

CALL i_format( ismp,nc,str )
str = 'Sensor no. '//TRIM(str)

RETURN
END

!==============================================================================

SUBROUTINE BuildMCspecieList( ss,nc,string )

USE scipuff_fi
USE sampler_fd
USE chem_fi

TYPE( sensor ), INTENT( IN  ) :: ss       !Sensor structure
INTEGER,        INTENT( IN  ) :: nc       !Length of string
CHARACTER(nc),  INTENT( OUT ) :: string

INTEGER i, ic, mcID, ID, nComp

CHARACTER(1) c

string = ''
ic     = 0

mcID = typeID(ss%is)%mcID

IF( mat_mc%type(mcID) == MC_CHEM )THEN

  string(1:1) = '('

  ID = mat_mc%ID(mcID)
  chem   => chemMC(ID,1)

  nComp = chem%nFast + chem%nSlow + chem%nParticle + chem%nEquilibrium

  DO i = 1,nComp
    IF( i == nComp )THEN
      c = ')'
    ELSE
      c = ','
    END IF
    ic = ic + LEN_TRIM(chem%species(i)%name) + 1
    IF( ic > nc )THEN
      string = 'ERROR: string not long enough'
      EXIT
    END IF
    string = TRIM(string)//TRIM(chem%species(i)%name)//c
  END DO

END IF

RETURN
END

!==============================================================================

SUBROUTINE GetMCspecieUnits( ss,string )

USE scipuff_fi
USE sampler_fd
USE chem_fi

TYPE( sensor ), INTENT( IN  ) :: ss       !Sensor structure
CHARACTER(*),   INTENT( OUT ) :: string

INTEGER mcID

CHARACTER(256) specieName, units

string = 'ppm'  !Default

mcID = typeID(ss%is)%mcID

IF( mat_mc%type(mcID) == MC_CHEM )THEN
  CALL GetChemCompName( mat_mc%ID(mcID),ss%mcID(1),specieName,units )  !Assumes all species have the same units
  string = TRIM(units)
END IF

RETURN
END
