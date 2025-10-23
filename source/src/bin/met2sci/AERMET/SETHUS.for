      SUBROUTINE SETHUS( ISTAT )
C***********************************************************************
C     SETHUS Module of the MPRM Meteorological Pre-processor

C     PURPOSE:    To construct a format statement to read data
C                 files produced from the HUSWO CD-ROM

C     PROGRAMMER: Desmond Bailey

C     DATE:       April 28, 1998
C
C     Revision History:
C        05/29/08: MACTEC Federal Programs
C                -  added string length to format when reading character
C                   variable ALINE and writing part of a format statement

C     INPUTS:     HUSWO data file - only the first record, which
c                 identifies the variables, is processed.

C     OUTPUTS:    HUSFMT - HUSWO file format        (in COMMON block)

C                 NVARS  - number of variables in the HUSWO data file

c                 IDVAR  - Array of HUSWO variable numbers from the
c                          first record of the data file - dimension
c                          set in sf1.inc.

c     INTERNAL:   VFMT   - Array of HUSWO variable formats, ordered
c                          from variable #3 to variable #20.

C     CALLED FROM: SFEXT
C***********************************************************************

C     Variable Declarations


      IMPLICIT NONE
      
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'

      INTEGER       NCOL, IFC, IPOS, I, IMIT, ISTAT, IVAR
      REAL          FNUM
      CHARACTER*40  FIELD(20)
      CHARACTER*256 ALINE
      CHARACTER*9   VFMT(20)

      INCLUDE 'WORK1.INC'

c     The first two fields in a HUSWO data record contain the station
c     id and date and should always be present.  The content of the
c     remaining fields (up to 18) will vary depending on the extract.

      DATA VFMT /'       A5','     ,A11', ',   1x,A4',',   1x,A4',
     &           ',   1x,A2',',      A2', ',1x,A5,1X',',   1x,A5',
     &           ',   1x,A3',',1x,A4,1X', ',   1x,A3',',   1x,A4',
     &           ',   1x,A6',',   1x,A5', ',   1x,A8',',   1x,A5',
     &           ',   1x,A5',',1x,A5,1X', ',   1X,A4',',   1x,A3'/

      PATH = 'SURFACE   '
      LOC  = 'SETHUS'
      NCOL = 256

C --- Since HUSWO data does not include station elevation, issue
C     warning message if user did not specify elevation on the 
C     SURFACE LOCATION keyword.  Station elevation can improve
C     representativeness of estimated station pressure when 
C     pressure is missing (performed in subroutine SUBST during
C     Stage 3).
      IF( .NOT.GOTPWELEV(3) )THEN
C ---    No user specified station elevation on the LOCATION keyword,
C        and no elevation from file, issue warning message
         MESS =  BLNK80
         WRITE (MESS, 6260)
 6260    FORMAT(' NO SURFACE elevation specified on LOCATION keyword ',
     &           'or from HUSWO file, default of 0m assumed;')
         CALL ERRHDL(1,PATH,'W46',LOC,MESS)
         MESS =  BLNK80
         WRITE (MESS, 6300)
 6300    FORMAT('  Recommend specifying station elevation.')
         CALL ERRHDL(2,PATH,'W46',LOC,MESS)
      ENDIF

c     Read extracted-variables header record
      READ (DEV20,'(A256)') ALINE

c     parse the line to determine locations and number of fields
      CALL DEF256 (NCOL,ALINE,IFC)

c     extract the contents of each field - save as a character variable
      CALL GETFLD( IFC,ALINE,FIELD )

      NVARS = IFC

c     Begin construction of the format to read a HUSWO record - first take care
c     of the station ID, and date/time (fields 1 & 2).  These first two fields
c     are always present.

      WRITE (HUSFMT(1:8),'(A8)') "(A5, A11"
      IPOS = 9

      DO I = 3, NVARS

c     extract the variable id from each field - skip the first two fields

         CALL STONUM(FIELD(I),40,FNUM,IMIT)
         IF (IMIT .NE. 1) THEN
            MESS = BLNK40
            WRITE (MESS, 6500) FIELD(I)(1:5)
            CALL ERRHDL(0,PATH,'E47',LOC,MESS)
            ISTAT = 1
            GO TO 999
         ELSE

            IDVAR(I) = INT(FNUM)

c   determine the index of the format chatacter string for this variable

            IVAR = IDVAR(I)

         ENDIF

c   append the format character string for this variable to HUSFMT
         WRITE (HUSFMT(IPOS:IPOS+8),'(A9)') VFMT(IVAR)
         IPOS = IPOS + 9
         IF (I .eq. NVARS) THEN
            WRITE (HUSFMT(IPOS:IPOS),'(A1)') ")"
         ENDIF

      ENDDO

  999 CONTINUE

      RETURN

 6500 FORMAT(' Error converting HUSWO variable ID: ', A5)
      END

