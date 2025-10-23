      SUBROUTINE RDLREC (DEVICE, IOCODE, RECDSZ, RECSTR,
     1                   BUFFER, MAXBLK, BLKTYP)
C=====================================================================**
C        RDLREC Module of the AERMET Meteorological Preprocessor
C
C     Purpose: To return the next logical record, which is stored
C              in the beginning of BUFFER.  Variable block records
C              contain the total record length in the first 4-bytes
C              of each record; fixed block records do not.  Thus,
C              the size of the record (RECDSZ) is either read here,
C              or supplied by the call routine.
C
C         RECSTR - start position in the block of this logical record
C         MAXBLK - max length of block
C         BLKTYP - FB or VB
C         NMOVE -  Number of bytes to move so that the logical record
C                  is in the beginning of BUFFER
C         OFFSET - Accounts for first 4-bytes if present (VB)
C
C     Initial Release: December 1992
C
C     Programmed by: Pacific Environmental Services, Inc. (PES)
C                    Research Triangle Park, NC
C
C-----------------------------------------------------------------------
C     Local declarations

      IMPLICIT NONE
      
      INTEGER I, J, DEVICE, IOCODE, RECDSZ, RECSTR, MAXBLK, OFFSET
      INTEGER NMOVE
      CHARACTER*(*) BUFFER
      CHARACTER BLKTYP*2,RECFMT*8
C
      IOCODE = 0

130   CONTINUE

      IF (RECDSZ.EQ.0 .OR. RECSTR.GE. MAXBLK-2) THEN
         WRITE(RECFMT,22) MAXBLK
22       FORMAT( '(A', I5, ')' )
         READ(DEVICE,FMT=RECFMT,END=77,ERR=320,IOSTAT=IOCODE)BUFFER
         IF (BLKTYP.EQ.'VB') THEN
            RECDSZ = 0
            RECSTR = 1
         ELSE
            RECSTR = 1 - RECDSZ
         ENDIF
      ENDIF
C
      RECSTR = RECSTR + RECDSZ
C
      IF (BLKTYP.EQ.'FB') THEN
         OFFSET= 0
         NMOVE = RECDSZ
         IF(RECSTR.GE.MAXBLK-2) GOTO 130
      ELSE
         IF(RECSTR.GE.MAXBLK-2) GOTO 130
         READ (BUFFER(RECSTR:RECSTR+3), 200, ERR=320) RECDSZ
200      FORMAT(I4)
         OFFSET = 4
         NMOVE = RECDSZ - 4
         IF (RECDSZ.EQ.0) GOTO 130
      ENDIF
C
      DO I=1,NMOVE
         J = RECSTR + I + OFFSET - 1
         BUFFER(I:I) = BUFFER(J:J)
      ENDDO
      RETURN
C             END OF FILE
77    IOCODE = -1
      RETURN
C             ERROR
320   RETURN
      END

