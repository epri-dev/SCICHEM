    SUBROUTINE SRCCNV
!***********************************************************************
!*                SRCCNV Module of AERMAP Terrain Preprocessor
!*
!*       PURPOSE: To Determine Source X,Y Coordinates in Lat/Long
!*                and write to a Temporary File
!*
!*       PROGRAMMER: Jayant Hardikar, Roger Brode
!*
!*       DATE:    September 29, 1995
!*
!*       Revision History:
!*
!*       MODIFIED: February 9, 2009
!*
!*                Modified code structure to avoid use of undefined
!*                variables for applications with NADA = 0, i.e.,
!*                no NAD conversion.  Addressed issues with some
!*                cross UTM zone applications with NAD conversions.
!*                Modified for use of standard convention of
!*                negative for West longitude.
!*                Roger W. Brode, U.S. EPA, OAQPS, AQMG
!*
!*       MODIFIED: December 7, 2006
!*
!*                Corrected several problems related to NAD conversion
!*                process, procedure for optimizing critical hill height
!*                calculations for neighboring DEM files, and other issues.
!*                See header comments in AERMAP.FOR source file and
!*                AERMAP MCB#1 for more details.
!*                Roger W. Brode, U.S. EPA, OAQPS, AQMG
!*
!*       INPUTS:  Source coordinates in cartesian coords
!*
!*       OUTPUTS: Source Arrays in Lat/Long
!*
!*       CALLED FROM:   MAIN
!***********************************************************************

!*    Variable Declarations
    USE TER_MAIN

    IMPLICIT NONE

    SAVE

    CHARACTER(12) :: MODNAM

    real (kind=8) :: XARG, YARG, ARGE, ARGN
    real (kind=8) :: XPTIN, YPTIN, XPTOUT, YPTOUT

    INTEGER :: ISPHERE, IZNEW, KEY, ISRC

!*    Variable Initializations
    MODNAM = 'SRCCNV'

    IF (SRCDBG) THEN
        OPEN(UNIT=SRCK, FILE=SRCDET_FILE, STATUS='REPLACE')
    !*       Write Version Date Header to SRCDETAIL Debug File
        WRITE(SRCK,9011) trim(VERSN), RUNDAT, RUNTIM
        9011 FORMAT('TERSCI Version ',A,1x,A8,1x,A8/)

        WRITE(SRCK,*) ''
        WRITE(SRCK,*) 'Processing Source Location Data in SRCCNV:'
        WRITE(SRCK,*) '  Determining North American Datum (NAD) shift', &
        ' values in arc-seconds.'
        WRITE(SRCK,*) '  Total shift (meters) includes both datum shi', &
        'ft and projection shift'
        WRITE(SRCK,*) '  for Lat/Lon to UTM coordinates.'
        WRITE(SRCK,*) ''

        IF (NADA == 0) THEN
            WRITE(SRCK,*) '  User-specified NADA = 0:   No NAD shifts ', &
            'will be calculated.'
            WRITE(SRCK,*) ''
        END IF
    END IF

!*    Select reference ellipsoid for UTM-to-GEO conversions, based on
!*    user-specified reference datum (NADA) for ANCHORXY location.
!*    If user specifies NADA = 0, then assume Clarke 1866 ellipsoid if
!*    7.5-minute/UTM DEMs are used, and assume GRS80 ellipsoid if
!*    1-degree/LAT-LON DEMs are used.

    SELECT CASE (NADA)
    CASE (0)
    IF (TYPDEM == 'UTM') THEN
        ISPHERE = 0  ! UTM/LL conversions based on Clarke 1866 ellipsoid
    ELSE IF (TYPDEM == 'LAT') THEN
        ISPHERE = 4  ! UTM/LAT-LONG conversions based on GRS80 ellipsoid
    END IF
    CASE (2:4)
    ISPHERE = 4     ! UTM/LAT-LONG conversions based on GRS80 ellipsoid
    CASE (1,5:6)
    ISPHERE = 0     ! UTM/LL conversions based on Clarke 1866 ellipsoid
    CASE DEFAULT
    ISPHERE = 4     ! DEFAULT CASE shouldn't occur
    END SELECT

!*    Loop Over All Sources
    DO ISRC = 1, NUMSRC

    !*       First Convert Source Location to UTM coords
        XSRCU(ISRC) = AXS(ISRC) - XAUSER + XATERR
        YSRCU(ISRC) = AYS(ISRC) - YAUSER + YATERR

    !        Convert UTMs to Lat/Long relative to NADA ellipsoid
        XARG = XSRCU(ISRC)
        YARG = YSRCU(ISRC)
        IZONS(ISRC) = ZATERR
        IZDUM = 0
        ARGE  = 0.0D0
        ARGN  = 0.0D0
        CALL UTMGEO (333,IZONS(ISRC),IZDUM,XARG,YARG,ARGE,ARGN,ISPHERE)

        SLON(ISRC) = ARGE
        SLAT(ISRC) = ARGN

    !        Convert back to UTM to check for zone change
        XARG = 0.0D0
        YARG = 0.0D0
        CALL UTMGEO (555,0,IZNEW,XARG,YARG,ARGE,ARGN,ISPHERE)

        IF (IZNEW /= IZONS(ISRC)) THEN
            SLON(ISRC) = ARGE
            SLAT(ISRC) = ARGN
        !*          Save new UTM coordinates and zone of source
            XSRCU(ISRC) = XARG
            YSRCU(ISRC) = YARG
            IZONS(ISRC) = IZNEW
        END IF

    !        Determine NAD shift:
    !        KEY = +1 indicates a transformation of NAD 27 to NAD 83 coords
    !        KEY = -1 indicates a psuedo-transformation of NAD 83 to NAD 27 coords
        KEY = 1
        IF (NADA >= 2 .AND. NADA <= 4) KEY = -1

        XPT27 = SLON(ISRC)/3600.0D0
        YPT27 = SLAT(ISRC)/3600.0D0

        XPTIN = XPT27
        YPTIN = YPT27

        IF (NADA == 0 .OR. .NOT. L_NeedNADCON) THEN
        !*          Skip datum conversion, assign 0.0's to NAD shifts.
            KEY = 0
            XSDIFS(ISRC) = 0.0D0
            YSDIFS(ISRC) = 0.0D0
            XSDIFM(ISRC) = 0.0D0
            YSDIFM(ISRC) = 0.0D0

            DATUMSHFTS(ISRC) = 0.0D0

            AMAG = 0.0D0

        !*          Write message to debug file
            IF (ISRC == 1 .AND. SRCDBG) THEN
                WRITE(SRCK,100) NADA
                100 FORMAT(3X,'No NAD conversions required. All files ', &
                'are consistent with NADA = ',I3/)
            END IF

        ELSE
        !*          Call NADCON 2.1 code to compute DATUM shifts and convert
        !*          geographic coordinates between NAD27 and NAD83, and vice versa.
        !*          XPTIN  & YPTIN  parameters are LON/LAT for NAD27;
        !*          XPTOUT & YPTOUT parameters are LON/LAT for NAD83.

            CALL NADCON(XPTIN,YPTIN,XPTOUT,YPTOUT,DLOS,DLAS, &
            DLOM,DLAM,KEY)
        !           Save shifts in geographic coordinates in arc-seconds.
        !           Full UTM shifts are computed below in corporating shifts
        !           due to ellipsoid.
            XSDIFS(ISRC) = DLOS * DBLE(key)
            YSDIFS(ISRC) = DLAS * DBLE(key)

            IF (KEY == 1) THEN
            !              Use _PTOUT variables from NADCON, which represent
            !              NAD83 converted FROM NAD27.  Get adjusted UTMs for
            !              NAD83 Datum using GRS80 ellipsoid
            !              Calculate adjustment relative to UTM Zone for source
            !              prior to NAD adjustment.

                ARGE = XPTOUT * 3600.0D0
                ARGN = YPTOUT * 3600.0D0
                IZDUM = 0
                XARG  = 0.0D0
                YARG  = 0.0D0
                CALL UTMGEO (555,IZONS(ISRC),IZDUM,XARG,YARG,ARGE,ARGN,4)

            ELSE IF (KEY == -1) THEN
            !              Use _PTIN variables from NADCON, which represent
            !              NAD27 converted FROM NAD83.  Get adjusted UTMs for
            !              NAD27 Datum using Clarke 1866 ellipsoid.
            !              Calculate adjustment relative to UTM Zone for source
            !              prior to NAD adjustment.

            !              Convert to arc-seconds for input to UTMGEO
                ARGE = XPTIN * 3600.0D0
                ARGN = YPTIN * 3600.0D0
                IZDUM = 0
                XARG  = 0.0D0
                YARG  = 0.0D0
                CALL UTMGEO (555,IZONS(ISRC),IZDUM,XARG,YARG,ARGE,ARGN,0)

            END IF

        !           Now calculate full UTM shift including ellipsoid and datum shifts
            XSDIFM(ISRC) = XARG-XSRCU(ISRC)
            YSDIFM(ISRC) = YARG-YSRCU(ISRC)

        !           Calculate total Datum shift in arc-seconds
            DATUMSHFTS(ISRC) = DSQRT(DLOS*DLOS + DLAS*DLAS)

        !           Calculate total shift in meters (includes shift due to ellipsoid)
            AMAG = DSQRT(XSDIFM(ISRC)**2 + YSDIFM(ISRC)**2)

            IF (KEY == 1) THEN
            !*             Assign adjusted coordinates for NAD27 to NAD83 conversion
                XPT27 = XPTIN
                YPT27 = YPTIN
                XPT83 = XPTOUT
                YPT83 = YPTOUT

                IF (SRCDBG) THEN
                    WRITE(SRCK,10) ISRC, XPT27, YPT27, &
                    XSDIFS(ISRC), YSDIFS(ISRC), &
                    XSDIFM(ISRC), YSDIFM(ISRC), &
                    XPT83, YPT83
                    10 FORMAT(' SRC# ',I6,'  X-,YPT27:', 2F20.8 &
                    /13X,' SHFT SECS', 2F20.8 &
                    /13X,' SHFT MTRS', 2F20.8 &
                    /13X,' X-,YPT83:', 2F20.8)

                    WRITE (SRCK,122) DATUMSHFTS(ISRC)
                    122 FORMAT(/13X,' DATUM SHIFT = ',F12.2,' SECONDS')
                    WRITE (SRCK,12) AMAG
                    12 FORMAT (13X,' TOTAL SHIFT = ',F12.2,' METERS'/)

                END IF

            ELSE IF (KEY == -1) THEN
            !*             Assign adjusted coordinates for NAD83 to NAD27 conversion
                XPT27 = XPTIN
                YPT27 = YPTIN
                XPT83 = XPTOUT
                YPT83 = YPTOUT

                IF (SRCDBG) THEN
                    WRITE(SRCK,11) ISRC, XPT83, YPT83, &
                    XSDIFS(ISRC), YSDIFS(ISRC), &
                    XSDIFM(ISRC), YSDIFM(ISRC), &
                    XPT27, YPT27
                    11 FORMAT(' SRC# ',I6,'  X-,YPT83:', 2F20.8 &
                    /13X,' SHFT SECS', 2F20.8 &
                    /13X,' SHFT MTRS', 2F20.8 &
                    /13X,' X-,YPT27:', 2F20.8)

                    WRITE (SRCK,122) DATUMSHFTS(ISRC)
                    WRITE (SRCK,12) AMAG

                END IF

            END IF

        END IF

    !*    End Loop Over Sources
    END DO

    WRITE(iounit,*) 'Exiting SRCCNV'
    WRITE(*,*) 'Exiting SRCCNV'

    RETURN
    end subroutine
