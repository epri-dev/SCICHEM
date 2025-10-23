      SUBROUTINE SetSFC( lunSFC,lunScratch,lunTmp,tAdj,
     $                   yr1,mo1,day1,
     $                   yr2,mo2,day2,
     $                   ID,clat,clon,elev )

      IMPLICIT NONE
      
      INCLUDE '../AERMET/MAIN1.INC'
      INCLUDE '../AERMET/MAIN2.INC'
      INCLUDE '../AERMET/SF1.INC'
      INCLUDE '../AERMET/SF2.INC'
      INCLUDE '../AERMET/WORK1.INC'

      INTEGER lunSFC, lunScratch, lunTmp, ios
      INTEGER tAdj
      INTEGER yr1, mo1, day1
      INTEGER yr2, mo2, day2
      REAL    elev
      LOGICAL lOPen

      CHARACTER(*) ID, clat, clon

      INTEGER SFARG, ISTAT, K

      INTEGER, EXTERNAL :: JULIAN

      DEV20  = lunSFC     !ISHD file
      DEV21  = lunScratch !Main (scratch) output
      DEV61  = lunTmp     !Internal temporary file

      SFLST  = 0.  !tAdj  ** UTC **
      SFYR1  = yr1
      SFGMO1 = mo1
      SFGDY1 = day1
      SFYR2  = yr2
      SFGMO2 = mo2
      SFGDY2 = day2

      SFLOC1 = TRIM(ID)

      IF( elev /= -9999. )THEN
        GOTPWELEV(3) = .TRUE.
        PWELEV(3)    = elev
      ELSE
        GOTPWELEV(3) = .FALSE.
      END IF

      SFARG = JULIAN(SFYR1,SFGMO1,SFGDY1)
      CALL CHROND('SURFACE   ',SFYR1,SFARG,SFDAY1)

      SFARG = JULIAN(SFYR2,SFGMO2,SFGDY2)
      CALL CHROND('SURFACE   ',SFYR2,SFARG,SFDAY2)

C---- If there was a problem determining either chronological day,
C     return to the calling program

      IF( SFDAY1.LT.0  .OR.  SFDAY2.LT.0 )THEN
         ISTAT = 1
         RETURN
      ENDIF

c     Initialize the concatenated variables

      SFQA1(34,1) = INT(9999/100)                          !  34 TSKC   ! ! dtb120 02064
      SFQA1(34,2) = 9999 - SFQA1(34,1)*100                              ! ! dtb120 02064
      SFOBS(1,34) = 9999                                                ! ! dtb120 02064

c     Variables 35 through 40 are ASOS sky condition and layer height     ! dtb120 02064
c     for six layers ALC1 through ALC6.  We initialize the first layer    ! dtb120 02064
c     to missing.  The remaining layers are meaningful only if the first  ! dtb120 02064
c     layer is non-missing; consequently, these layers (variables 36-40)  ! dtb120 02064
c     are initialized to clear with an unlimited height.                  ! dtb120 02064


      SFQA1(35,1) = INT(9999/1000)                         !  35 ALC1     ! dtb120 02064
      SFQA1(35,2) = 9999 - SFQA1(35,1)*1000                             ! ! dtb120 02064
      SFOBS(1,35) = 9999                                                ! ! dtb120 02064

      DO K = 36, 40                                                     ! ! dtb120 02064
         SFQA1(K,1) = 09                         !                        ! dtb120 02064
         SFQA1(K,2) = 999                        !                        ! dtb120 02064
         SFOBS(1,K) = 00300                                             ! ! dtb120 02064
      ENDDO                                                             ! ! dtb120 02064

      SFLOC = TRIM(ID)
      SFLON = TRIM(clon)
      SFLAT = TRIM(clat)

C---- Clear the work buffers of previous values

      CALL FLWRK1
      CALL FLWRK2
      CALL FLIWK1
      CALL FLIWK2

      STATUS(1,2) = 2 !For error messages
      STATUS(1,1) = 2 !For report

      INQUIRE(UNIT=DEV70,OPENED=lOpen)
      IF( .NOT.lOpen )THEN
        OPEN(UNIT=DEV70,STATUS='SCRATCH',ACTION='READWRITE',IOSTAT=ios)
      END IF

C      SFSAUD(I-29)
      SFSAUD(1)  = 1 !Precip
      SFSAUD(3)  = 1 !Press
      SFSAUD(17) = 1 !Temp
      SFSAUD(20) = 1 !RH
      SFSAUD(21) = 1 !Wdir
      SFSAUD(22) = 1 !Wspeed
 
      RETURN
      END

!==============================================================================

      SUBROUTINE SetSFQA( lunSFQ )

      INCLUDE '../AERMET/MAIN1.INC'
      INCLUDE '../AERMET/MAIN2.INC'
      INCLUDE '../AERMET/UA1.INC'
      INCLUDE '../AERMET/UA2.INC'
      INCLUDE '../AERMET/SF1.INC'
      INCLUDE '../AERMET/SF2.INC'
      INCLUDE '../AERMET/OS1.INC'
      INCLUDE '../AERMET/OS2.INC'
      INCLUDE '../AERMET/MP1.INC'
      INCLUDE '../AERMET/MP2.INC'
!      INCLUDE '../AERMET/BLOCK1.INC'
!      INCLUDE '../AERMET/BLOCK2.INC'

      INTEGER lunSFQ

      lunSFQ = DEV22
      SFSTAT = 3

      RETURN
      END

!==============================================================================

      SUBROUTINE GetSFCloc( istat,xlat,xlon,ID )

      IMPLICIT NONE
      
      INCLUDE '../AERMET/MAIN1.INC'
      INCLUDE '../AERMET/MAIN2.INC'
      INCLUDE '../AERMET/SF1.INC'
      INCLUDE '../AERMET/SF2.INC'
      INCLUDE '../AERMET/WORK1.INC'

      INTEGER istat
      REAL    xlat, xlon
      CHARACTER(*) ID

      INTEGER ID_WBAN, ID_USAF

      CHARACTER CONTROL*60
     
      REWIND(DEV20,IOSTAT=istat)

!---- Assumes this is ISHD type

      READ(DEV20,'(A60)',IOSTAT=istat,ERR=9999) CONTROL

      READ(CONTROL(5:10),  '(I6)',IOSTAT=istat,ERR=9999) ID_USAF
      READ(CONTROL(11:15), '(I5)',IOSTAT=istat,ERR=9999) ID_WBAN
!      READ(CONTROL(16:23), '(I8)',IOSTAT=istat,ERR=999) IDATE
!      READ(CONTROL(28:28), '(A1)',IOSTAT=istat,ERR=999) cDataSource
      READ(CONTROL(29:34), '(F6.3)') xlat
      READ(CONTROL(35:41), '(F7.3)') xlon
      READ(CONTROL(47:51), '(F5.0)') zmsl2use  !Not used currently

      IF( ID_WBAN /= 99999 )THEN
        WRITE(ID,'(I5)',IOSTAT=istat,ERR=9999) ID_WBAN
      ELSEIF( ID_USAF /= 999999 )THEN
        WRITE(ID,'(I6)',IOSTAT=istat,ERR=9999) ID_USAF
      END IF

9999  CONTINUE

      RETURN
      END

!==============================================================================

      SUBROUTINE GetSFCelev( istat,elev )

      IMPLICIT NONE
      
      INCLUDE '../AERMET/MAIN1.INC'
      INCLUDE '../AERMET/MAIN2.INC'
      INCLUDE '../AERMET/SF1.INC'
      INCLUDE '../AERMET/SF2.INC'
      INCLUDE '../AERMET/WORK1.INC'

      INTEGER istat
      REAL    elev

      INTEGER ID_WBAN, ID_USAF

      CHARACTER CONTROL*60
     
      REWIND(DEV20,IOSTAT=istat)

!---- Assumes this is ISHD type

      READ(DEV20,'(A60)',IOSTAT=istat,ERR=9999) CONTROL

      READ(CONTROL(47:51), '(F5.0)') zmsl2use

      IF( zmsl2use < 9000. )elev = zmsl2use  !Use if elevation is only a bit higher than Mt. Everest

9999  CONTINUE

      RETURN
      END

!==============================================================================

      SUBROUTINE GetAnemHgt( zref )

      IMPLICIT NONE
      
      INCLUDE '../AERMET/MAIN1.INC'
      INCLUDE '../AERMET/MAIN2.INC'
      INCLUDE '../AERMET/SF1.INC'
      INCLUDE '../AERMET/SF2.INC'
      INCLUDE '../AERMET/WORK1.INC'

      REAL zref

      IF( GotCommDate )THEN
        zref = MAX(ASOS_ANEM_HGT,2.0)
      ELSE
        zref = 10.  !Default 10m
      END IF

      RETURN
      END 
