SUBROUTINE draw_overlay()

USE errorParam_fd
USE files_fi
USE contri_fi
USE PlotTrans_fi
USE default_fd
USE Tooluser_fd

IMPLICIT NONE

INTEGER, PARAMETER :: MAXDATA = 100

INTEGER, PARAMETER :: OLD_VERSION_OVL = -999
INTEGER, PARAMETER :: JAVA_VERSION_40 = 4000


CHARACTER(128) line

INTEGER ios,ival,isym,izone,ires,i
INTEGER icoord,icommand,nskp,c_decode,np,irv,nstp,file_version
REAL    x,y,size,center,sx,sy,ex,ey
REAL    xp(MAXDATA),yp(MAXDATA)

TYPE( SCIPFieldCoordinateT ) :: FileCoordinate

INTEGER, EXTERNAL :: SCIPTransform

INTEGER nError
CHARACTER(128) eMessage,eInform,eAction,eRoutine

nError = NO_ERROR
eAction = ' '

FileCoordinate = PlotCoordinate

CALL gsplci( 1 )
size   =  1.0
CALL gslwsc( size )
icoord =  0
izone  =  0
center = -1.
isym   =  1
ires   =  0
file_version = OLD_VERSION_OVL

OPEN(UNIT=lun_tmp,FILE=covly,STATUS='OLD',IOSTAT=ios)
IF( ios /= 0 )THEN
  nError   = OV_ERROR
  eRoutine = 'DrawOverlay'
  eMessage = 'Unable to open Overlay data file'
  eInform  = TRIM(covly)
  GOTO 9999
END IF

DO

  READ(lun_tmp,'(A)',IOSTAT=ios)line
  IF( ios > 0 )THEN
    nError   = OV_ERROR
    eRoutine = 'DrawOverlay'
    eMessage = 'Unable to read Overlay data'
    eInform  = TRIM(covly)
    GOTO 9999
  ELSE IF( ios < 0 )THEN
    GOTO 9999
  END IF

  CommandLoop: DO

    icommand = c_decode( line,nskp )

    SELECT CASE( icommand )
      CASE( 1 ) !Comment

      CASE( 2 ) !Area

      CASE( 3 ) !Line
        np = 0

        ReadLoop: DO

          READ(lun_tmp,'(A)',IOSTAT=ios)line
          IF( ios < 0 )EXIT ReadLoop

          ival = c_decode( line,nskp )
          IF( ival > 0 )EXIT ReadLoop

          IF( file_version == OLD_VERSION_OVL )THEN
            READ(line,*,IOSTAT=ios)x,y
          ELSE IF( file_version >= JAVA_VERSION_40 )THEN
            READ(line,*,IOSTAT=ios)x,y
          ELSE
            IF( FileCoordinate%Mode == HD_LATLON )THEN
              READ(line,*,IOSTAT=ios)y,x
            ELSE
              READ(line,*,IOSTAT=ios)x,y
            END IF
          END IF
          IF( ios == 0 )THEN
            np = np + 1
            xp(np) = x
            yp(np) = y
          END IF
          IF( np < MAXDATA )CYCLE ReadLoop

          x = xp(np)
          y = yp(np)
          irv = SCIPTransform( FileCoordinate,PlotCoordinate,np,xp,yp )
          IF( irv /= SCIPsuccess )GOTO 9998

          DO i = 1,np
            xp(i) = xp(i)*scx + shx
            yp(i) = yp(i)*scy + shy
          END DO
          CALL gpl( np,xp,yp )
          xp(1) = x
          yp(1) = y
          np = 1

        END DO ReadLoop

        irv = SCIPTransform( FileCoordinate,PlotCoordinate,np,xp,yp )
        IF( irv /= SCIPsuccess )GOTO 9998
        DO i = 1,np
          xp(i) = xp(i)*scx + shx
          yp(i) = yp(i)*scy + shy
        END DO
        CALL gpl( np,xp,yp )
        CYCLE CommandLoop

      CASE( 4 ) !Point

      CASE( 5 ) !Text

      CASE( 6 ) !Size
        IF( nskp > 0 )THEN
          READ(line(nskp:),*,IOSTAT=ios)size
          IF( ios == 0 )CALL gslwsc( size )
        END IF

      CASE( 7 ) !Color
        IF( nskp > 0 )THEN
          READ(line(nskp:),*,IOSTAT=ios)ival
          IF( ios == 0 )THEN
            CALL gsplci( ival )
            CALL gsfaci( ival )
          END IF
        END IF

      CASE( 8 ) !Center
        IF( nskp > 0 )THEN
          READ(line(nskp:),*,IOSTAT=ios)center
        END IF

      CASE( 9 ) !Fill
        IF( nskp > 0 )THEN
          READ(line(nskp:),*,IOSTAT=ios)ival
          IF( ios == 0 )CALL gsfais( ival )
        END IF

      CASE( 10 ) !Symbol
        IF( nskp > 0 )THEN
          READ(line(nskp:),*,IOSTAT=ios)isym
        END IF

      CASE( 11 ) !Coord
        IF( nskp > 0 )THEN
          CALL cupper( line(nskp:) )
          IF( line(nskp:nskp+5) == 'LATLON' )THEN
            FileCoordinate%Mode          = HD_LATLON
            FileCoordinate%UTMZone       = NOT_SET_I
            FileCoordinate%Reference%x   = NOT_SET_R
            FileCoordinate%Reference%y   = NOT_SET_R
            FileCoordinate%Reference%lat = NOT_SET_R
            FileCoordinate%Reference%lon = NOT_SET_R
          ELSE IF( line(nskp:nskp+2) == 'UTM' )THEN
            FileCoordinate%Mode = HD_UTM
            ival = INDEX(line,'ZONE=')
            IF( ival > 0 )THEN
              READ(line(ival+5:),*,IOSTAT=ios)np
              IF( ios == 0 )THEN
                FileCoordinate%UTMZone       = np
                FileCoordinate%Reference%x   = NOT_SET_R
                FileCoordinate%Reference%y   = NOT_SET_R
                FileCoordinate%Reference%lat = NOT_SET_R
                FileCoordinate%Reference%lon = NOT_SET_R
              END IF
            END IF
          ELSE
            FileCoordinate%Mode = HD_CARTESIAN
            IF( nskp > 0 )then
              nskp = INDEX(line,'(')+1
              nstp = INDEX(line,',')-1
              IF( nstp > nskp )THEN
                READ(line(nskp:nstp),*,IOSTAT=ios)sx
                IF( ios == 0 )THEN
                  nskp = nstp + 2
                  nstp = INDEX(line,') => (')-1
                  READ(line(nskp:nstp),*,IOSTAT=ios)sy
                  IF( ios == 0 )THEN
                    nskp = nstp + 7
                    nstp = nskp + INDEX(line(nskp:),'N,')-1
                    READ(line(nskp:nstp),*,IOSTAT=ios)ey
                    IF( ios == 0 )THEN
                      nskp = nstp + 3
                      nstp = INDEX(line,'E)')-1
                      READ(line(nskp:nstp),*,IOSTAT=ios)ex
                      IF( ios == 0 )THEN
                        FileCoordinate%Mode          = HD_CARTESIAN
                        FileCoordinate%UTMZone       = NOT_SET_I
                        FileCoordinate%Reference%x   = sx
                        FileCoordinate%Reference%y   = sy
                        FileCoordinate%Reference%lat = ey
                        FileCoordinate%Reference%lon = ex
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
              END IF
            END IF
          END IF
        END IF

      CASE( 12 ) !Vertical slice, Horizontal resolution
        IF( nskp > 0 )THEN
          nstp = INDEX(line,'(')-1
          READ(line(nskp:nstp),*,IOSTAT=ios)ires
          IF( ios == 0 )THEN
            nskp = nstp + 2
            nstp = INDEX(line,',')-1
            READ(line(nskp:nstp),*,IOSTAT=ios)sx
            IF( ios == 0 )THEN
              nskp = nstp + 2
              nstp = INDEX(line,') (')-1
              READ(line(nskp:nstp),*,IOSTAT=ios)sy
              IF( ios == 0 )THEN
                nskp = nstp + 4
                nstp = nskp + INDEX(line(nskp:),',')-2
                READ(line(nskp:nstp),*,IOSTAT=ios)ex
                IF( ios == 0 )THEN
                  nskp = nstp + 2
                  nstp = nskp + INDEX(line(nskp:),')')-2
                  READ(line(nskp:nstp),*,IOSTAT=ios)ey
                  IF(ios == 0)THEN
                    FileCoordinate%Mode                 = -FileCoordinate%Mode
                    FileCoordinate%VertSlice%Resolution = ires
                    FileCoordinate%VertSlice%StartPt%x  = sx
                    FileCoordinate%VertSlice%StartPt%y  = sy
                    FileCoordinate%VertSlice%EndPt%x    = ex
                    FileCoordinate%VertSlice%EndPt%y    = ey
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          END IF
        END IF

      CASE( 13 ) !FileVersion
        IF( nskp > 0 )THEN
          READ(line(nskp:),*,IOSTAT=ios)ival
          IF( ios == 0 )file_version = ival
        END IF

      CASE DEFAULT

    END SELECT

    EXIT CommandLoop

  END DO CommandLoop

END DO

9999 CONTINUE

CLOSE(lun_tmp,IOSTAT=ios)
CALL gslwsc( 0.36 )

IF( nError /= NO_ERROR )THEN
  CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
END IF

RETURN

9998 CONTINUE
nError =   OV_ERROR
eRoutine = 'DrawOverlay'
eMessage = 'Unable to transform overlay coordinates to plot coordinates'
eAction  = 'Check Overlay file'
CALL ReportFileName( eInform,'File=',covly )
GOTO 9999

END

!===============================================================================

INTEGER FUNCTION c_decode( line,nk )
!
!     this integer function examines LINE to determine
!     which option it represents.  C_DECODE returns
!     the option number (.le. 0 ->error), and NK is the
!     offset in LINE at which any parameters begin.
!     If there are no parameters, NK is returned as 0.
!
IMPLICIT NONE

INTEGER, PARAMETER :: NSPECS = 13

CHARACTER(*) line
INTEGER     nk

CHARACTER(9)   commands(NSPECS)
INTEGER        find_option


DATA commands( 1)/'#'/
DATA commands( 2)/'!AREA'/
DATA commands( 3)/'!LINE'/
DATA commands( 4)/'!POINT'/
DATA commands( 5)/'!TEXT'/
DATA commands( 6)/'!SIZE'/
DATA commands( 7)/'!COLOR'/
DATA commands( 8)/'!CENTER'/
DATA commands( 9)/'!FILL'/
DATA commands(10)/'!SYMBOL'/
DATA commands(11)/'!COORD'/
DATA commands(12)/'!RESOL'/
DATA commands(13)/'!FVERS'/

c_decode = find_option( line,commands,NSPECS,nk )

RETURN
END

!===============================================================================

INTEGER FUNCTION find_option( line,commands,nspecs,np )

IMPLICIT NONE
!
!     this logical function examines line to determine
!     which option it represents.  find_option returns
!     true unless line contains an invalid option.
!     on return, n is the option number, and np is the
!     offset in line at which any parameters begin.
!     if there are no parameters, np is returned as 0.
!     commands contains the list of options to look for.
!     nspecs is the number of options in the list.
!
INTEGER      nspecs,np
CHARACTER(*) line
CHARACTER(*) commands(nspecs)

LOGICAL hit
INTEGER n,nb,i,lc,j
!
!     skip over initial blanks
!
nb = 1

10 CONTINUE

IF( line(nb:nb) /= ' ' )GOTO 20
nb = nb + 1
IF( LEN(line) < nb )GOTO 58 ! bad value
GOTO 10
20 CONTINUE
DO 25 i = nb,LEN(line)
   IF( line(i:i) == ' ' )GOTO 28
25 CONTINUE
i = LEN(line)+1
28 CONTINUE

lc = i - nb

!     now we need to find the rest of the line

np = INDEX(line(nb:),' ')
IF( np == 0 )GOTO 80
np = np + nb - 1
70  CONTINUE
IF( line(np:np) /= ' ' )GOTO 80
np = np + 1
IF( LEN(line) >= np )GOTO 70
np = 0
80 CONTINUE

!     now that we know where the parameters start, we can convert
!     the command part to upper case without any trouble!

IF( np > 1 )CALL cupper( line(1:np-1) )

!     check whether a valid response was given

DO 40 i = 1,LEN(commands(1))

!     first we check for 1 character matches, then 2s, up to the option length.
!     the checking is terminated when we find only one match
!     at a given level.

   hit = .FALSE.

!        initialize to no matches yet at this level

   IF( lc < i )GOTO 55 !  ambiguous at previous level

!        make sure we don't exceed LINE length

   DO 30 j = 1,nspecs
      IF( line(nb:nb+i-1) == commands(j)(1:i) )THEN

!           we found a match

         IF( hit )GOTO 40

!              if we already had one at this level, then we
!              need to look at more levels to resolve it

         hit = .TRUE.

!              otherwise, we retain this command's number

         n = j
      END IF
30 CONTINUE
   IF( hit )THEN

!           we found a unique match

      GOTO 60
   ELSE

!           we found no matches. abort the search.

      GOTO 50 ! bad value
   END IF
40 CONTINUE

!     not a valid option

50    find_option = -2
RETURN
55    find_option = -1
RETURN
58    find_option = -3
RETURN
60    CONTINUE
find_option = n
RETURN
END



