    SUBROUTINE CALCHC(IREC)
!***********************************************************************
!*               CALCHC Module of AERMAP Terrain Preprocessor
!*
!*       PURPOSE: Determine Height Scales for Each Receptor
!*
!*       PROGRAMMER: Jayant Hardikar, Roger Brode
!*
!*       DATE:    September 29, 1995
!*
!*       Revision History:
!*
!*       MODIFIED: February 9, 2009
!*
!*       MODIFIED BY: Roger W. Brode, U.S. EPA, OAQPS, AQMG
!*
!*                Corrected problems associated with cross UTM zone
!*                applications associated with NAD datum conversions.
!*                Modified procedure for determining which elevation files
!*                can be skipped in hill height scale calculation to
!*                accommodate Alaska DEMs and "mixed" DEM file applications.
!*                Incorporated use of more refined routine for calculating
!*                distances based on geographic coordinates, based on
!*                NGS INVERSE utility program. Incorporated more robust
!*                optimization of height scale calculations within 'LAT'
!*                data files based on more refined distance calculation.
!*
!*       MODIFIED: December 7, 2006
!*
!*       MODIFIED BY: Roger W. Brode, U.S. EPA, OAQPS, AQMG
!*
!*                Corrected several problems related to NAD conversion
!*                process, procedure for optimizing critical hill height
!*                calculations for neighboring DEM files, and other issues.
!*                See header comments in AERMAP.FOR source file and
!*                AERMAP MCB#1 for more details.
!*
!*       MODIFIED: January 25, 2002
!*
!*       MODIFIED BY: Peter Eckhoff
!*
!*                Added a routine that assumes the maximum elevation is at the
!*                closet point on a DEM file to the receptor.  An HFDEMX is
!*                calculated and compared to HFMAX to see if HFDEMX might exceed
!*                HFMAX.  If so all the points are analyzed, if not the DEM file
!*                is skipped thus saving time.
!*
!*                An algorithm was also added whereby the calculations for HFMAX are
!*                skipped if the slope from the receptor to an elevation point is
!*                less than 10%.
!*
!*       INPUTS:  Receptor Coordinates, DEM Data, Maximum elevation for a DEM file
!*
!*       OUTPUTS: Receptor Height Scale
!*
!*       CALLED FROM:   MAIN
!***********************************************************************

!*    Variable Declarations
    USE TER_MAIN

    IMPLICIT NONE

    SAVE

    CHARACTER(12) :: MODNAM

    real (kind=8) :: HEFMAX, HFDEMX
    real (kind=8) :: RLONSHFT, RLATSHFT
    real (kind=8) :: XARG, YARG, ARGE, ARGN
    real (kind=8) :: PLAT(2), PLON(2), RMIN(NDEM)
    real (kind=8) :: DISTLL, BP
    real (kind=8) :: XRSHFT, YRSHFT, NADFLG

    INTEGER :: JRECRD, IPROF, INOD, ISPHERE, IREC
    INTEGER :: JDEM, IIDEM, IFTM
    INTEGER :: IZONREC
    INTEGER :: INDX_DEM

    LOGICAL :: L_Opened

    real (kind=8) :: HEFF, GETDEM, ZNODE

!*    Initialize Hefmax, HEFF, IDAT, IDON, and RDIST
!*    The lowest point on the Earth is in the Dead Sea area
!*      at slightly greater than 1300 feet below MSL.

    MODNAM = 'CALCHC      '
    L_Opened = .FALSE.
    HEFMAX = -1000.00D0
    HEFF   = 0.0D0
    RDIST  = 0.0D0
    BP     = 0.0D0

    IF (HILLDBG .AND. IREC == 1) THEN
        IF (IREC == 1) THEN
        !*          Determine whether CALCHC debug file is already opened
            INQUIRE(UNIT=HDBG, OPENED=L_Opened)
            IF ( .NOT. L_Opened) THEN
                OPEN(UNIT=HDBG,FILE=CALCHC_FILE,ERR=99,STATUS='REPLACE')
            !*             Write Version Date Header to CALCHC Debug File
                WRITE(HDBG,9011) trim(VERSN), RUNDAT, RUNTIM
9011            FORMAT('TERSCI Version ',A,1x,A8,1x,A8/)
            END IF
        END IF
        WRITE(HDBG,*) 'Entering CALCHC for Receptor: ',IREC
        IF (TYPDAT == 'DEM') THEN
            WRITE(HDBG,*) 'Starting with Local DEM File: ',JDM(IREC)
        ELSE IF (TYPDAT == 'NED') THEN
            WRITE(HDBG,*) 'Starting with Local NED File: ',JDM(IREC)
        END IF
    END IF

!*    If the Receptor Elevation Was Not Found, Set ZHILL=-9999.
    IF (AZELEV(IREC) < -9998.999D0) THEN
        AZHILL(IREC) = -9999.0D0
        IF (HILLDBG) THEN
        !*          No hill height scale set for this receptor
            WRITE(HDBG,200) IREC
            200 FORMAT(/'Elevation missing (-9999.0) for receptor: ',I8, &
            ' ; no Hill Height Scale calculation made!'/)
        END IF
        GO TO 999
    END IF

!     Retrieve the DEM file number covering the receptor, JDEM
    JDEM = JDM(IREC)

    IZONREC = IZONR(IREC)

!*    Loop Over All DEM Files; When IIDEM = 0, HEFMAX is calculated first
!*    for the receptor's resident DEM file.

    DEMLOOP: DO IIDEM = 0, NUMDEM

    !*      reinitialize max HF for each DEM file, and IFTM flag for debug output
        HFDEMX = -1000.0D0
        IFTM = 0

    !*      Assign DEM file index, IDEM.
    !*      Always start with DEM file containing the receptor (IIDEM = 0)
        IF (IIDEM == 0) THEN
        !*          First pass through loop, assign DEM file index for receptor (JDEM) to IDEM
            IDEM = JDEM
        ELSE IF (IIDEM == JDEM) THEN
            CYCLE DEMLOOP
        ELSE
            IDEM = IIDEM
        END IF

    !*      Check for empty direct access elevation, identified by NUMPRF(IDEM) = 0
        IF (NUMPRF(IDEM) == 0) CYCLE DEMLOOP

    !       Determine whether to apply NAD shift
        IF (NADA == 0 .OR. NADA == NADD(IDEM)) THEN

            NADFLG = 0.0D0            ! No NAD shift needed

        ELSE IF( (NADA == 1    .OR.  NADA >= 5) .AND. &
            (NADD(IDEM) >= 2 .AND. NADD(IDEM) <= 4) )THEN

            NADFLG = 1.0D0            ! Include NAD shift

        ELSE IF( (NADA >= 2    .AND. NADA <= 4) .AND. &
            (NADD(IDEM) == 1 .OR.  NADD(IDEM) >= 5) )THEN

            NADFLG = 1.0D0            ! Include NAD shift

        ELSE

            NADFLG = 0.0D0            ! No NAD shift needed

        END IF

        RLONSHFT = RLON(IREC) + (XRDIFS(IREC) * NADFLG)
        RLATSHFT = RLAT(IREC) + (YRDIFS(IREC) * NADFLG)

    !*      Convert receptor Lat/Lon with NAD shift to UTMs, based on DEM file NADD and DEM Zone
        SELECT CASE (NADD(IDEM))   ! Use datum for DEM file
        CASE (0)
        IF (IPLAN(IDEM) == 0) THEN
            ISPHERE = 4  ! UTM/LAT-LONG conversions based on GRS80 ellipsoid
        ELSE IF (IPLAN(IDEM) == 1) THEN
            ISPHERE = 0  ! UTM/LL conversions based on Clarke 1866 ellipsoid
        END IF
        CASE (2:4)
        ISPHERE = 4     ! UTM/LAT-LONG conversions based on GRS80 ellipsoid
        CASE (1,5:6)
        ISPHERE = 0     ! UTM/LL conversions based on Clarke 1866 ellipsoid
        CASE DEFAULT
        ISPHERE = 4     ! DEFAULT CASE shouldn't occur
        END SELECT

        ARGE = RLONSHFT
        ARGN = RLATSHFT
        XARG = 0.0D0
        YARG = 0.0D0
    !*      Pass DEM file zone (IZOND(IDEM)) to UTMGEO to get new receptor
    !*      coordinates (XARG, YARG) referenced to IZOND(IDEM)).
        CALL UTMGEO (555,IZOND(IDEM),IZONREC,XARG,YARG,ARGE,ARGN, &
        ISPHERE)
    !*      Assign new receptor UTMs to XRSHFT and YRSHFT
        XRSHFT = XARG
        YRSHFT = YARG

    !       Convert Latitude and Longitude of Receptor to Radians
        PLAT(1) = (RLATSHFT / 3600.0D0) * dtorad
        PLON(1) = (RLONSHFT / 3600.0D0) * dtorad

    !*      Reinitialize RDIST (distance) for new host DEM file
        RDIST = 0.0D0

        IF (HILLDBG .AND. (IDEM == JDEM)) THEN

            WRITE(HDBG, 300) TYPDAT, IDEM, FLN(IDEM), IREC, &
            RLATSHFT/3600.0D0, RLONSHFT/3600.0D0, &
            SELAT_DEGS(IDEM), SELON_DEGS(IDEM), &
            RDIST, &
            ELEVMX(IDEM), AZELEV(IREC)

            300 FORMAT(//' Checking ',A3,' File ',I5,' : ',A40, &
            ';  for Receptor No.: ',I7, &
            /90X,'DEM', &
            /6X,'  RECLAT      RECLON    ', &
            ' DEM_SELAT   DEM_SELON  ',23X, &
            '  DIST(m)  MAXELEV  RECELEV' &
            /2X, 2F12.3, 2X, 2F12.3, 23X, F11.2, 2F9.2)

            WRITE(HDBG,301) TYPDAT, IREC, NADA, IDEM, NADD(IDEM), &
            MAPNAME(IDEM)(1:24), &
            RLATSHFT, RLONSHFT, &
            YRSHFT, XRSHFT, IZONREC

            301 FORMAT(/'    REC NADA ',A3,' NADD        Map Name', &
            '               Latitude   Longitude', &
            '      Northing     Easting  Zone' &
            /I7, I5, I4, I5, 2X, A24, 1X, 2F12.2, 2X, &
            &                  2F12.2, 2X, I4/)

        END IF

    !       For adjacent DEM files:
    !       Calculate whether to do a HEFMAX calculation for the whole DEM file.

        IF (IDEM /= JDEM) THEN
        !*        Check whether we need to process this DEM file for HILL

            IF (ELEVMX(IDEM) > HEFMAX) THEN
            !*           DEM max elevation is > current height scale. Next calculate closest
            !*           distance from receptor to DEM file and assume max elev occurs at
            !*           that distance.  PLAT(2) & PLON(2) are coordinates for closest
            !*           point on DEM quadrangle in radians.

            !*           Assign max elevation from DEM file to temporary HEFMAX
                HFDEMX = ELEVMX(IDEM)

            !*           Assign INDX_DEM to track location of DEM file relative to
            !*           the "host" DEM for receptor:
            !*
            !*           INDX_DEM:     7   8   9
            !*                         4   5   6    where 5 = host DEM
            !*                         1   2   3

                IF (SELAT_DEGS(IDEM)*DTORAD >= PLAT(1)) THEN
                !*              DEM file North of receptor
                    PLAT(2) = SELAT_DEGS(IDEM) * DTORAD
                    IF (SELON_DEGS(IDEM)*DTORAD <= PLON(1)) THEN
                    !*                 DEM file is Northwest of receptor
                        PLON(2) = SELON_DEGS(IDEM) * DTORAD
                        INDX_DEM = 7
                    ELSE IF (SWLON_DEGS(IDEM)*DTORAD >= PLON(1)) THEN
                    !*                 DEM file is Northeast of receptor
                        PLON(2) = SWLON_DEGS(IDEM) * DTORAD
                        INDX_DEM = 9
                    ELSE
                    !*                 DEM file is due North of receptor
                        PLON(2) = PLON(1)
                        INDX_DEM = 8
                    END IF
                ELSE IF (NELAT_DEGS(IDEM)*DTORAD <= PLAT(1)) THEN
                !*              DEM file is South of receptor
                    PLAT(2) = NELAT_DEGS(IDEM) * DTORAD
                    IF (NELON_DEGS(IDEM)*DTORAD <= PLON(1)) THEN
                    !*                 DEM file is Southwest of receptor
                        PLON(2) = NELON_DEGS(IDEM) * DTORAD
                        INDX_DEM = 1
                    ELSE IF (NWLON_DEGS(IDEM)*DTORAD >= PLON(1)) THEN
                    !*                 DEM file is Southeast of receptor
                        PLON(2) = NWLON_DEGS(IDEM) * DTORAD
                        INDX_DEM = 3
                    ELSE
                    !*                 DEM file is due South of receptor
                        PLON(2) = PLON(1)
                        INDX_DEM = 2
                    END IF
                ELSE
                !*              DEM file is East or West of receptor
                    PLAT(2) = PLAT(1)
                    IF (SELON_DEGS(IDEM)*DTORAD <= PLON(1)) THEN
                    !*                 DEM file is West of receptor
                        PLON(2) = SELON_DEGS(IDEM) * DTORAD
                        INDX_DEM = 4
                    ELSE IF (SWLON_DEGS(IDEM)*DTORAD >= PLON(1)) THEN
                    !*                 DEM file is East of receptor
                        INDX_DEM = 6
                        PLON(2) = SWLON_DEGS(IDEM) * DTORAD
                    ELSE
                    ! ---              This case shouldn't happen unless receptor is in a DEM file
                    !                  that overlaps the initial "local" DEM file for the receptor.
                    !                  Process this file the same as DEM "assigned" to receptor.
                    !                  Assign max elevation within file to HFDEMX and skip DISTLL
                    !                  calculation.
                        INDX_DEM = 5
                        PLON(2) = PLON(1)
                        HFDEMX = ELEVMX(IDEM)
                        RMIN(IDEM) = 0.0D0
                        IF (HILLDBG) THEN
                            WRITE(HDBG, 300) TYPDAT, IDEM, FLN(IDEM), IREC, &
                            RLATSHFT/3600.0D0,RLONSHFT/3600.0D0, &
                            SELAT_DEGS(IDEM), SELON_DEGS(IDEM), &
                            RMIN(IDEM), ELEVMX(IDEM),AZELEV(IREC)

                            WRITE(HDBG,301) TYPDAT,IREC,NADA,IDEM, NADD(IDEM), &
                            MAPNAME(IDEM)(1:24), &
                            RLATSHFT, RLONSHFT, &
                            YRSHFT, XRSHFT, IZONREC
                        END IF

                        IF (HFDEMX <= HEFMAX) THEN
                        !*                    This file does not contain any potential critical
                        !*                    hill heights; issue message if needed and cycle DEM loop
                            IF (HILLDBG) THEN
                                WRITE (HDBG, 305) TYPDAT, FLN(IDEM)
                                305 FORMAT(' There were NO potential critical ', &
                                'hill heights found in ',A3,' file:  ',A)
                            END IF

                            CYCLE DEMLOOP
                        END IF
                    !*                 Skip distance calculation and start processing file
                        GO TO 500

                    END IF
                END IF

            !*           Calculate closest distance from receptor to DEM file (RMIN)
                RMIN(IDEM) = DISTLL(PLAT,PLON)

            !*           Compare max elev from DEM file to 10:1 slope based on closest distance
                BP = RMIN(IDEM) * 0.1D0

                IF (HILLDBG) THEN
                    WRITE(HDBG, 300) TYPDAT, IDEM, FLN(IDEM), IREC, &
                    RLATSHFT/3600.0D0,RLONSHFT/3600.0D0, &
                    SELAT_DEGS(IDEM), SELON_DEGS(IDEM), &
                    RMIN(IDEM), &
                    ELEVMX(IDEM), AZELEV(IREC)

                    WRITE(HDBG,301) TYPDAT, IREC, NADA, IDEM, NADD(IDEM), &
                    MAPNAME(IDEM)(1:24), &
                    RLATSHFT, RLONSHFT, &
                    YRSHFT, XRSHFT, IZONREC

                END IF

                IF (BP <= (ELEVMX(IDEM) - AZELEV(IREC))) THEN
                !*              DEM file MAY contain critical hill heights.  Assign temporary
                !*              hill height (HFDEMX) for later use.

                    HFDEMX = ELEVMX(IDEM)

                    IF (HFDEMX <= HEFMAX) THEN
                    !*                 DEM file will not contain any critical hill heights based on
                    !*                 max elevation in file and current controlling hill height
                        IF (HILLDBG) THEN
                            WRITE (HDBG, 305) TYPDAT, FLN(IDEM)
                        END IF

                        CYCLE DEMLOOP
                    END IF

                ELSE
                !*              DEM file will not contain critical hill heights based on
                !*              receptor elevation and closest distance to DEM file
                    IF (HILLDBG) THEN
                        WRITE (HDBG, 305) TYPDAT, FLN(IDEM)
                    END IF

                    CYCLE DEMLOOP
                END IF

            ELSE
            !*           DEM file max elevation (ELEVMX) is less than current hill height scale (HEFMAX);
            !*           skip this DEM file without distance calculation

                IF (HILLDBG) THEN
                    WRITE(HDBG, 310) TYPDAT, IDEM, FLN(IDEM), IREC, &
                    RLATSHFT/3600.0D0,RLONSHFT/3600.0D0, &
                    SELAT_DEGS(IDEM), SELON_DEGS(IDEM), &
                    ELEVMX(IDEM), AZELEV(IREC)

                    310 FORMAT(//' Checking ',A3,' File ',I5,' : ',A40, &
                    ';  for Receptor No.: ',I7, &
                    /90X,'DEM', &
                    /6X,'  RECLAT      RECLON    ', &
                    ' DEM_SELAT   DEM_SELON  ',23X, &
                    '  DIST(m)  MAXELEV  RECELEV' &
                    /2X, 2F12.3, 2X, 2F12.3, &
                    &                 28X, '----- ', 2F9.2)

                    WRITE(HDBG,301) TYPDAT, IREC, NADA, IDEM, NADD(IDEM), &
                    MAPNAME(IDEM)(1:24), &
                    RLATSHFT, RLONSHFT, &
                    YRSHFT, XRSHFT, IZONREC

                    WRITE (HDBG, 305) TYPDAT, FLN(IDEM)

                END IF

                CYCLE DEMLOOP

            END IF   ! End of ELEVMX > AZELEV IF-block

        END IF    ! End of DEM File "precheck" IF-block

        500 CONTINUE

    !       Check for either the first time through to calculate an HEFMAX of the DEM file
    !       assigned to the receptor or if another DEM file might produce a higher HEFMAX:

        IF (IDEM == JDEM .OR. HFDEMX > HEFMAX) THEN
            IFTM = 0
        !*        Reopen the Index & Direct Access Files
            OPEN (IDXUNT(IDEM),FILE=IDXFIL(IDEM),STATUS='OLD')
            REWIND (IDXUNT(IDEM))

            OPEN (IDRUNT(IDEM),FILE=DIRFIL(IDEM), &
            ACCESS='DIRECT',RECL=LREC_DIR,STATUS='OLD')

        !*        Initialize record counter for direct access DEM file
            JRECRD = 0

        !*        Loop Over All Profiles in this File
            PROFILE_LOOP: DO IPROF = 1,NUMPRF(IDEM)

            !*           Read the Index File for this DEM File
                READ (IDXUNT(IDEM),IDXFRM) XBASE(IPROF), YBASE(IPROF), &
                NODES(IPROF), IZONP(IPROF), MAXEL(IPROF)

            !***         See if we can skip this profile based on max elev for profile.
            !            Different tests needed for host DEM vs other DEMs.

                IF (IDEM == JDEM) THEN           ! DEM file contains receptor

                    IF (IPLAN(IDEM) == 0) THEN
                    !*                 Use closest distance from receptor to profile
                    !*                 For 1-degree and other Lat/Lon DEMs calculate
                    !*                 distance based on Lat/Lon
                        PLAT(2) = PLAT(1)
                        PLON(2) = (XBASE(IPROF) / 3600.0D0) * dtorad

                        RDIST = DISTLL(PLAT,PLON)                       ! ---   CALL DISTLL
                        BP = RDIST * 0.10D0

                        IF (BP > (MAXEL(IPROF) - AZELEV(IREC))) THEN
                        !*                    We can skip this profile; Increment JRECRD and CYCLE
                            JRECRD = JRECRD + NODES(IPROF)

                            CYCLE PROFILE_LOOP
                        END IF

                    ELSE IF (IPLAN(IDEM) == 1) THEN
                    !*                 Use closest distance from receptor to profile,
                    !*                 based on difference in Easting (X) values

                        BP = DABS(XRSHFT - XBASE(IPROF)) * 0.1D0

                        IF (BP > (MAXEL(IPROF) - AZELEV(IREC))) THEN
                        !*                    We can skip this profile; Increment JRECRD and CYCLE
                            JRECRD = JRECRD + NODES(IPROF)

                            CYCLE PROFILE_LOOP
                        END IF

                    END IF

                ELSE                               ! Non-local DEM file

                    IF (IPLAN(IDEM) == 0) THEN
                    !*                 Use closest distance from receptor to profile
                    !*                 For 1-degree and other Lat/Lon DEMs calculate
                    !*                 distance based on Lat/Lon;
                    !*                 Use base longitude of profile (XBASE) with latitude
                    !*                 of closest node.
                        IF (INDX_DEM >= 1 .AND. INDX_DEM <= 3) THEN
                        !*                    DEM file is South of host DEM; use top node of profile
                            PLAT(2) = ((YBASE(IPROF) + &
                            NODES(IPROF)*DYM(IDEM))/3600.0D0) * DTORAD
                            PLON(2) = (XBASE(IPROF) / 3600.0D0) * DTORAD

                        ELSE IF (INDX_DEM >= 7 .AND. INDX_DEM <= 9) THEN
                        !*                    DEM file is North of host DEM; use bottom node of profile
                            PLAT(2) = (YBASE(IPROF) / 3600.0D0) * DTORAD
                            PLON(2) = (XBASE(IPROF) / 3600.0D0) * DTORAD

                        ELSE
                        !*                    DEM file is East or West of host DEM; use rec lat
                            PLAT(2) = PLAT(1)
                            PLON(2) = (XBASE(IPROF) / 3600.0D0) * DTORAD

                        END IF

                        RDIST = DISTLL(PLAT,PLON)                          ! ---   CALL DISTLL
                        BP = RDIST * 0.10D0

                        IF (BP > (MAXEL(IPROF) - AZELEV(IREC))) THEN
                        !*                    We can skip this profile; Increment JRECRD and CYCLE
                            JRECRD = JRECRD + NODES(IPROF)

                            CYCLE PROFILE_LOOP
                        END IF

                    ELSE IF (IPLAN(IDEM) == 1) THEN
                    !*                 Use closest distance from receptor to profile
                        IF (INDX_DEM >= 1 .AND. INDX_DEM <= 3) THEN
                        !*                    DEM file is South of host DEM; use top node of profile
                            XARG = XBASE(IPROF)
                            YARG = YBASE(IPROF) + NODES(IPROF)*DYM(IDEM)

                        ELSE IF (INDX_DEM >= 7 .AND. INDX_DEM <= 9) THEN
                        !*                    DEM file is North of host DEM; use bottom node of profile
                            XARG = XBASE(IPROF)
                            YARG = YBASE(IPROF)

                        ELSE
                        !*                    DEM file is East or West of host DEM; use rec lat
                            XARG = XBASE(IPROF)
                            YARG = YRSHFT

                        END IF

                        RDIST = DSQRT((XRSHFT-XARG)**2 + (YRSHFT-YARG)**2)
                        BP = RDIST * 0.10D0

                        IF (BP > (MAXEL(IPROF) - AZELEV(IREC))) THEN
                        !*                    We can skip this profile; Increment JRECRD and CYCLE
                            JRECRD = JRECRD + NODES(IPROF)

                            CYCLE PROFILE_LOOP
                        END IF

                    END IF

                END IF


            !*           Loop Over All Nodes in This Profile
                NODE_LOOP: DO INOD = 1, NODES(IPROF)

                !*              Determine the Coordinates of this Point (L/L)        ---  CALL GETNOD
                    CALL GETNOD(IPROF,INOD)

                !*              Read the Terrain Elevation for this Point from the Direct
                !*              Access File                                          ---   CALL GETDEM
                    JRECRD = JRECRD + 1
                    ZNODE = GETDEM(IDEM,JRECRD)

                !*              Determine the Lateral Distance (R) Between
                !*              Receptor and that of this Point

                    IF (IPLAN(IDEM) == 0) THEN
                        PLAT(2) = (YNODE / 3600.0D0) * dtorad
                        PLON(2) = (XNODE / 3600.0D0) * dtorad

                    !*                 For 1-degree and other Lat/Lon DEMs calculate
                    !*                 distance based on Lat/Lon

                        RDIST = DISTLL(PLAT,PLON)                       ! ---   CALL DISTLL

                    ELSE IF (IPLAN(IDEM) == 1) THEN
                    !*                 Adjacent DEM file is in UTM coordinates; convert DEM
                    !*                 node coordinates to Lat/Lon for distance calculation

                        XARG = XNODE
                        YARG = YNODE

                    !*                 For 7.5-minute DEMs calculate distance based on UTM
                    !*                 Receptor coordinates (XRSHFT and YRSHFT) aleady include
                    !*                 NAD shift and UTM zone adjustment, if necessary.

                        RDIST = DSQRT((XRSHFT-XARG)**2 + (YRSHFT-YARG)**2)

                    END IF

                !*              Calculate 10% slope for this node distance

                    BP = RDIST * 0.1D0

                !*              If a node's elevation penetrates a 10% slope from the receptor
                !*                 use the node's elevation as an effective terrain height

                    IF (ZNODE - AZELEV(IREC) >= BP) THEN
                    !*                 Assign node elevation to the Effective Terrain Height (Heff)
                        HEFF = ZNODE

                        IF (HEFF > HEFMAX) THEN
                        !*                    Update Hefmax and HC
                            HEFMAX = HEFF
                            AZHILL(IREC) = HEFF
                            IF (HILLDBG) THEN
                                IF (IFTM == 0) THEN
                                !*                          First time HEFF is updated for this DEM/REC
                                !*                          Write column headings
                                    IF (IPLAN(IDEM) == 0) THEN
                                        WRITE(HDBG,311)
                                        311 FORMAT(12X,'DEM    PROF   NODE      ', &
                                        'NODELAT     NODELON      DIST     ', &
                                        'ZNODE     RELEV    HEFMAX')
                                    ELSE IF (IPLAN(IDEM) == 1) THEN
                                        WRITE(HDBG,312)
                                        312 FORMAT(12X,'DEM    PROF   NODE      ', &
                                        ' NODE-Y      NODE-X      DIST     ', &
                                        'ZNODE     RELEV    HEFMAX')
                                    END IF
                                !*                          Reset "first time" flag to 1
                                    IFTM = 1
                                END IF

                                WRITE(HDBG, 303) IDEM, IPROF, INOD, &
                                YNODE, XNODE, RDIST, ZNODE, &
                                AZELEV(IREC), HEFMAX
                                303 FORMAT(6X,I9,I8,I7,1X,2F12.2,4F10.2)
                            END IF
                        END IF

                    END IF

                END DO NODE_LOOP
            !*           End Loop on Nodes

            END DO PROFILE_LOOP
        !*        End Loop on Profiles

            CLOSE (IDXUNT(IDEM))
            CLOSE (IDRUNT(IDEM))

            IF (HILLDBG) THEN
            !*          No hill height scales set for this DEM file; issue message to debug file
                IF (IFTM == 0) THEN
                    WRITE (HDBG, 305) TYPDAT, FLN(IDEM)
                END IF
            END IF

        END IF

    END DO DEMLOOP
!*    End Loop on DEM Files


!*    Do Not Allow the Hill Height Scale to be Less
!*    Than the Receptor Elevation
    IF (HEFMAX < AZELEV(IREC) .OR. &
    AZHILL(IREC) < AZELEV(IREC)) THEN
        AZHILL(IREC) = AZELEV(IREC)
    END IF

    IF (HILLDBG) THEN
    !*       Write final critical hill height value to debug file
        WRITE(HDBG,306) IREC, AZHILL(IREC)
        306 FORMAT(/'The Critical Hill Height for Receptor:',I6, &
        ' is ',F8.2,' meters.'//'---------')
    END IF

    GO TO 999

    99 CONTINUE
!     Error opening HILLDBG file
    CALL ERRHDL(PATH,MODNAM,'E','500','DEBUGHIL')
    RUNERR = .TRUE.

    999 CONTINUE

    RETURN

    end subroutine


    real (kind=8) FUNCTION DISTLL(PLAT,PLON)
!***********************************************************************
!*               DISTLL Module of AERMAP Terrain Preprocessor
!*
!*       PURPOSE: Determine the Linear Distance Between 2 Lat/Lon Pairs
!*
!*       PROGRAMMER: Jayant Hardikar, Roger Brode
!*
!*       DATE:    September 29, 1995
!*
!*       REVISION HISTORY:
!*
!*                Modified to use National Geodetic Survey program,
!*                INVERSE, to calculate geodesic distance between
!*                latitude/longitude pairs based on the GRS80/WGS84
!*                ellipsoid.  This code provides a more accurate
!*                calculation than previous version based on a
!*                spherical distance.
!*                Roger W. Brode, U.S. EPA, OAQPS, AQMG
!*                September 5, 2008
!*
!*       MODIFIED BY: Roger W. Brode, U.S. EPA, OAQPS, AQMG
!*
!*                    Incorporated upper bound on argument for DACOS
!*                    function to avoid potential runtime errors for
!*                    receptors collocated with DEM nodes.
!*                    December 7, 2006
!*
!*       MODIFIED:   Trap for negative argument to DACOS function,
!*                   which may occur due to rounding if the two
!*                   points are coincident.  R.W. Brode, PES, 1/22/98
!*
!*       INPUTS:  Lat/Long Pair
!*
!*       OUTPUTS: Linear Distance Between them
!*
!*       CALLED FROM:   MAIN
!***********************************************************************

!*    Variable Declarations
    USE TER_MAIN

    IMPLICIT NONE

    SAVE

!---- Parameters for distance calculation (based on GRS80/WGS84/NAD83):
!     A = semi-major axis of reference ellipsoid
!     F = flattening
!     ESQ = Eccentricity squared
    real (kind=8), PARAMETER :: A = 6378137.0D0, &
    F = 1.0D0/298.257223563D0, &
    ESQ = F*(2.0D0-F)

!---- PLAT/PLON = Latitude/Longitude Pairs (radians)
    real (kind=8) :: PLAT(2), PLON(2)

    CHARACTER(12) :: MODNAM

    MODNAM = 'DISTLL'

!---- Call routine to calculate geodesic distance between lat/lon pairs,
!     based on INVERSE program available from National Geodetic Survey.
    CALL GPNHRI(A,F,ESQ,PI,PLAT(1),PLON(1),PLAT(2),PLON(2),DISTLL)

    RETURN
    end function

!b::gpnhri

! wb  SUBROUTINE GPNHRI extracted from INVERSE program
! wb  available from the National Geodetic Survey website:
! wb  http://www.ngs.noaa.gov/PC_PROD/Inv_Fwd/

! name:      inverse
! version:   200208.19
! author:    stephen j. frakes
! purpose:   to compute a geodetic inverse
!            and then display output information

! wb      subroutine gpnhri (a,f,esq,pi,p1,e1,p2,e2,az1,az2,s)
    subroutine gpnhri (a,f,esq,pi,p1,e1,p2,e2,s)

!********1*********2*********3*********4*********5*********6*********7*

! name:        gpnhri
! version:     200208.09
! written by:  robert (sid) safford
! purpose:     subroutine to compute helmert rainsford inverse problem

!     solution of the geodetic inverse problem after t. vincenty
!     modified rainsford's method with helmert's elliptical terms
!     effective in any azimuth and at any distance short of antipocal
!     from/to stations must not be the geographic pole.
!     parameter a is the semi-major axis of the reference ellipsoid
!     finv=1/f is the inverse flattening of the reference ellipsoid
!     latitudes and longitudes in radians positive north and west
!     forward and back azimuths returned in radians clockwise from south
!     geodesic distance s returned in units of semi-major axis a
!     programmed for ibm 360-195   09/23/75

!     note - note - note -
!     1. do not use for meridional arcs and be careful on the equator.
!     2. azimuths are from north(+) clockwise and
!     3. longitudes are positive east(+)

! input parameters:
! -----------------
! a            semi-major axis of reference ellipsoid      meters
! f            flattening (0.0033528...)
! esq          eccentricity squared
! pi           3.14159...
! p1           lat station 1                               radians
! e1           lon station 1                               radians
! p2           lat station 2                               radians
! e2           lon station 2                               radians

! output parameters:
! ------------------
! az1          azi at sta 1 -> sta 2                       radians
! az2          azi at sta 2 -> sta 1                       radians
! s            geodetic dist between sta(s) 1 & 2          meters

! local variables and constants:
! ------------------------------
! aa               constant from subroutine gpnloa
! alimit           equatorial arc distance along the equator   (radians)
! arc              meridional arc distance latitude p1 to p2 (in meters)
! az1              azimuth forward                          (in radians)
! az2              azimuth back                             (in radians)
! bb               constant from subroutine gpnloa
! dlon             temporary value for difference in longitude (radians)
! equ              equatorial distance                       (in meters)
! r1,r2            temporary variables
! s                ellipsoid distance                        (in meters)
! sms              equatorial - geodesic distance (S - s) "Sms"
! ss               temporary variable
! tol0             tolerance for checking computation value
! tol1             tolerance for checking a real zero value
! tol2             tolerance for close to zero value
! twopi            two times constant pi

! global variables and constants:
! -------------------------------

!    module called by:    general

!    this module calls:   gpnarc, gpnloa
!       llibfore/ dsin,   dcos,   dsqrt,  dabs,  datan2, write

!    include files used:
!    common blocks used:

!    references: microsoft fortran 4.10 optimizing compiler, 1988
!                ms-dos operating system
!    comments:
!********1*********2*********3*********4*********5*********6*********7*
!::modification history
!::197507.05, rws, ver 00 tencol released for field use
!::198311.20, rws, ver 01 mten   released to field
!::198411.26, rws, ver 07 mten2  released to field
!::198506.10, rws, wrk    enhancements released to field
!::198507.22, rws, code   modified for mten3
!::198509.01, rws, ver 11 mten3  released to field
!::198708.10, rws, code   modified to use new mten4 gpn record format
!::199112.31, rws, ver 20 mten4 released to field
!::200001.13, rws, ver 21 mten4 released to field
!::200005.26, rws, code   restructured & documentation added
!::200012.31, rws, ver 23 mten5 released
!::200104.09, rws, code   added to calblin program
!::200208.09, rws, code   added subroutines gpnarc & gpnloa
!********1*********2*********3*********4*********5*********6*********7*
!e::gpnhri
!  -------------------------------
!     m t e n  (version 3)
!              (version 4.22)
!              (version 5.23)
!  -------------------------------

    implicit real (kind=8) (a-h,o-z)

    data tol0 /5.0d-15/
    data tol1 /5.0d-14/
    data tol2 /7.0d-03/

    twopi = 2.0d0*pi

!     test the longitude difference with tol1
!     tol1 is approximately 0.000000001 arc seconds

    ss = e2-e1
    if( dabs(ss) < tol1 )then
        e2 = e2+tol1
    ! wb        write(*,*) ' longitudal difference is near zero '

        r2 = p2
        r1 = p1
        call gpnarc ( a, f, esq, pi, r1, r2, arc )
        s  = dabs( arc )

    ! wb  az1 & az2 calculations commented out; not needed for AERMAP
    ! wb        if( p2.gt.p1 )then
    ! wb          az1 = 0.0d0
    ! wb          az2 = pi
    ! wb        else
    ! wb          az1 = pi
    ! wb          az2 = 0.0d0
    ! wb        endif
        return
    endif

!     test for longitude over 180 degrees

    dlon = e2-e1

    if( dlon >= 0.0d0 )then
        if( pi <= dlon .AND. dlon < twopi )then
            dlon = dlon-twopi
        endif
    else
        ss = dabs(dlon)
        if( pi <= ss .AND. ss < twopi )then
            dlon = dlon+twopi
        endif
    endif

    ss = dabs( dlon )
    if( ss > pi )then
    !::     write(*,*) '  '
    !::     write(*,*) ' Longitude difference over 180 degrees  '
    !::     write(*,*) ' Turn it around '
        ss = twopi-ss
    endif

!     compute the limit in longitude (alimit), it is equal
!     to twice the distance from the equator to the pole,
!     as measured along the equator (east/ewst)

    alimit = pi*(1.0d0-f)

!     test for anti-nodal difference

    if( ss >= alimit )then
        r1 = dabs(p1)
        r2 = dabs(p2)

    !       latitudes r1 & r2 are not near the equator

        if( r1 > tol2 .AND. r2 > tol2 )then
            goto 60
        endif

    !       longitude difference is greater than lift-off point
    !       now check to see if  "both"  r1 & r2 are on equator

        if( r1 < tol1 .AND. r2 > tol2 )then
            goto 60
        endif
        if( r2 < tol1 .AND. r1 > tol2 )then
            goto 60
        endif

    !       check for either r1 or r2 just off the equator but < tol2

        if( r1 > tol1 .OR. r2 > tol1 )then
            az1 = 0.0d0
            az2 = 0.0d0
            s   = 0.0d0
            return
        endif

    !       compute the azimuth to anti-nodal point

    !::     write(*,*) '  '
    !::     write(*,*) ' Longitude difference beyond lift-off point '
    !::     write(*,*) '  '

        call gpnloa (a,f,esq,pi,dlon,az1,az2,aa,bb,sms)

    !       compute the equatorial distance & geodetic

        equ = a*dabs(dlon)
        s   = equ-sms
        return
    endif

    60 continue

    f0   = (1.0d0-f)
    b    = a*f0
    epsq = esq/(1.0d0-esq)
    f2   = f*f
    f3   = f*f2
    f4   = f*f3

!     the longitude difference

    dlon  = e2-e1
    ab    = dlon
    kount = 0

!     the reduced latitudes

    u1    = f0*dsin(p1)/dcos(p1)
    u2    = f0*dsin(p2)/dcos(p2)

    u1    = datan(u1)
    u2    = datan(u2)

    su1   = dsin(u1)
    cu1   = dcos(u1)

    su2   = dsin(u2)
    cu2   = dcos(u2)

!     counter for the iteration operation

    1 kount = kount+1

    clon  = dcos(ab)
    slon  = dsin(ab)

    csig  = su1*su2+cu1*cu2*clon
    ssig  = dsqrt((slon*cu2)**2+(su2*cu1-su1*cu2*clon)**2)

    sig   = datan2(ssig,csig)
    sinalf=cu1*cu2*slon/ssig

    w   = (1.0d0-sinalf*sinalf)
    t4  = w*w
    t6  = w*t4

!     the coefficients of type a

    ao  = f-f2*(1.0d0+f+f2)*w/4.0d0+3.0d0*f3*(1.0d0+ &
    &         9.0d0*f/4.0d0)*t4/16.0d0-25.0d0*f4*t6/128.0d0
    a2  = f2*(1.0d0+f+f2)*w/4.0d0-f3*(1.0d0+9.0d0*f/4.0d0)*t4/4.0d0+ &
    &         75.0d0*f4*t6/256.0d0
    a4  = f3*(1.0d0+9.0d0*f/4.0d0)*t4/32.0d0-15.0d0*f4*t6/256.0d0
    a6  = 5.0d0*f4*t6/768.0d0

!     the multiple angle functions

    qo  = 0.0d0
    if( w > tol0 )then
        qo = -2.0d0*su1*su2/w
    endif

    q2  = csig+qo
    q4  = 2.0d0*q2*q2-1.0d0
    q6  = q2*(4.0d0*q2*q2-3.0d0)
    r2  = 2.0d0*ssig*csig
    r3  = ssig*(3.0d0-4.0d0*ssig*ssig)

!     the longitude difference

    s   = sinalf*(ao*sig+a2*ssig*q2+a4*r2*q4+a6*r3*q6)
    xz  = dlon+s

    xy  = dabs(xz-ab)
    ab  = dlon+s

    if( xy < 0.5d-13 )then
        goto 4
    endif

    if( kount <= 7 )then
        goto 1
    endif

!     the coefficients of type b

    4 z   = epsq*w

    bo  = 1.0d0+z*(1.0d0/4.0d0+z*(-3.0d0/64.0d0+z*(5.0d0/256.0d0- &
    z*175.0d0/16384.0d0)))
    b2  = z*(-1.0d0/4.0d0+z*(1.0d0/16.0d0+z*(-15.0d0/512.0d0+ &
    z*35.0d0/2048.0d0)))
    b4  = z*z*(-1.0d0/128.0d0+z*(3.0d0/512.0d0-z*35.0d0/8192.0d0))
    b6  = z*z*z*(-1.0d0/1536.0d0+z*5.0d0/6144.0d0)

!     the distance in meters

    s   = b*(bo*sig+b2*ssig*q2+b4*r2*q4+b6*r3*q6)

!     first compute the az1 & az2 for along the equator

! wb  az1 & az2 calculations commented out; not needed for AERMAP
! wb      if( dlon.gt.pi )then
! wb        dlon = (dlon-2.0d0*pi)
! wb      endif
! wbc
! wb      if( dabs(dlon).gt.pi )then
! wb        dlon = (dlon+2.0d0*pi)
! wb      endif
! wbc
! wb      az1 = pi/2.0d0
! wb      if( dlon.lt.0.0d0 )then
! wb        az1 = 3.0d0*az1
! wb      endif
! wbc
! wb      az2 = az1+pi
! wb      if( az2.gt.2.0d0*pi )then
! wb        az2 = az2-2.0d0*pi
! wb      endif
! wbc
! wbc     now compute the az1 & az2 for latitudes not on the equator
! wbc
! wb      if( .not.(dabs(su1).lt.tol0 .and. dabs(su2).lt.tol0) )then
! wb        tana1 =  slon*cu2/(su2*cu1-clon*su1*cu2)
! wb        tana2 =  slon*cu1/(su1*cu2-clon*su2*cu1)
! wb        sina1 =  sinalf/cu1
! wb        sina2 = -sinalf/cu2
! wbc
! wbc       azimuths from north,longitudes positive east
! wbc
! wb        az1   = datan2(sina1,sina1/tana1)
! wb        az2   = pi-datan2(sina2,sina2/tana2)
! wb      endif
! wbc
! wb      if( az1.lt.0.0d0 )then
! wb        az1 = az1+2.0d0*pi
! wb      endif
! wbc
! wb      if( az2.lt.0.0d0 )then
! wb        az2 = az2+2.0d0*pi
! wb      endif

    return
    end subroutine gpnhri

!B::GPNARC

    SUBROUTINE GPNARC (AMAX,FLAT,ESQ,PI,P1,P2,ARC)

!********1*********2*********3*********4*********5*********6*********7*

! NAME:        GPNARC
! VERSION:     200005.26
! WRITTEN BY:  ROBERT (Sid) SAFFORD
! PURPOSE:     SUBROUTINE TO COMPUTE THE LENGTH OF A MERIDIONAL ARC
!              BETWEEN TWO LATITUDES

! INPUT PARAMETERS:
! -----------------
! AMAX         SEMI-MAJOR AXIS OF REFERENCE ELLIPSOID
! FLAT         FLATTENING (0.0033528 ... )
! ESQ          ECCENTRICITY SQUARED FOR REFERENCE ELLIPSOID
! PI           3.14159...
! P1           LAT STATION 1
! P2           LAT STATION 2

! OUTPUT PARAMETERS:
! ------------------
! ARC          GEODETIC DISTANCE

! LOCAL VARIABLES AND CONSTANTS:
! ------------------------------
! GLOBAL VARIABLES AND CONSTANTS:
! -------------------------------

!    MODULE CALLED BY:    GENERAL

!    THIS MODULE CALLS:
!       LLIBFORE/ OPEN,   CLOSE,  READ,   WRITE,  INQUIRE
!                 DABS,   DBLE,   FLOAT,  IABS,   CHAR,   ICHAR

!    INCLUDE FILES USED:
!    COMMON BLOCKS USED:

!    REFERENCES: Microsoft FORTRAN 4.10 Optimizing Compiler, 1988
!                MS-DOS Operating System
!    COMMENTS:
!********1*********2*********3*********4*********5*********6*********7*
!::MODIFICATION HISTORY
!::197507.05, RWS, VER 00 TENCOL RELEASED FOR FIELD USE
!::198311.20, RWS, VER 01 MTEN   RELEASED TO FIELD
!::198411.26, RWS, VER 07 MTEN2  RELEASED TO FIELD
!::1985xx.xx, RWS, CODE   CREATED
!::198506.10, RWS, WRK    ENHANCEMENTS RELEASED TO FIELD
!::198509.01, RWS, VER 11 MTEN3  RELEASED TO FIELD
!::198512.18, RWS, CODE   MODIFIED FOR MTEN3
!::198708.10, RWS, CODE   MODIFIED TO USE NEW MTEN4 GPN RECORD FORMAT
!::199112.31, RWS, VER 20 MTEN4 RELEASED TO FIELD
!::200001.13, RWS, VER 21 MTEN4 RELEASED TO FIELD
!::200005.26, RWS, CODE   RESTRUCTURED & DOCUMENTATION ADDED
!::200012.31, RWS, VER 23 MTEN5 RELEASED
!********1*********2*********3*********4*********5*********6*********7*
!E::GPNARC
! ---------------------------
!     M T E N  (VERSION 3)
!     M T E N  (VERSION 5.23)
! ---------------------------

    IMPLICIT real (kind=8) (A-H,O-Z)

    LOGICAL ::  FLAG

    DATA TT/5.0D-15/

!     CHECK FOR A 90 DEGREE LOOKUP

    FLAG = .FALSE.

    S1 = DABS(P1)
    S2 = DABS(P2)

    IF( (PI/2.0D0-TT) < S2 .AND. S2 < (PI/2.0D0+TT) )THEN
        FLAG = .TRUE.
    ENDIF

    IF( S1 > TT )THEN
        FLAG = .FALSE.
    ENDIF

    DA = (P2-P1)
    S1 = 0.0D0
    S2 = 0.0D0

!     COMPUTE THE LENGTH OF A MERIDIONAL ARC BETWEEN TWO LATITUDES

    E2 = ESQ
    E4 = E2*E2
    E6 = E4*E2
    E8 = E6*E2
    EX = E8*E2

    T1 = E2*(003.0D0/4.0D0)
    T2 = E4*(015.0D0/64.0D0)
    T3 = E6*(035.0D0/512.0D0)
    T4 = E8*(315.0D0/16384.0D0)
    T5 = EX*(693.0D0/131072.0D0)

    A  = 1.0D0+T1+3.0D0*T2+10.0D0*T3+35.0D0*T4+126.0D0*T5

    IF( FLAG )THEN
        GOTO 1
    ENDIF

    B  = T1+4.0D0*T2+15.0D0*T3+56.0D0*T4+210.0D0*T5
    C  = T2+06.0D0*T3+28.0D0*T4+120.0D0*T5
    D  = T3+08.0D0*T4+045.0D0*T5
    E  = T4+010.0D0*T5
    F  = T5

    DB = DSIN(P2*2.0D0)-DSIN(P1*2.0D0)
    DC = DSIN(P2*4.0D0)-DSIN(P1*4.0D0)
    DD = DSIN(P2*6.0D0)-DSIN(P1*6.0D0)
    DE = DSIN(P2*8.0D0)-DSIN(P1*8.0D0)
    DF = DSIN(P2*10.0D0)-DSIN(P1*10.0D0)

!     COMPUTE THE S2 PART OF THE SERIES EXPANSION

    S2 = -DB*B/2.0D0+DC*C/4.0D0-DD*D/6.0D0+DE*E/8.0D0-DF*F/10.0D0

!     COMPUTE THE S1 PART OF THE SERIES EXPANSION

    1 S1 = DA*A

!     COMPUTE THE ARC LENGTH

    ARC = AMAX*(1.0D0-ESQ)*(S1+S2)

    RETURN
    END SUBROUTINE GPNARC
!b::gpnhri

!B::GPNLOA

    SUBROUTINE GPNLOA (AMAX,FLAT,ESQ,PI,DL,AZ1,AZ2,AO,BO,SMS)

!********1*********2*********3*********4*********5*********6*********7*

! NAME:        GPNLOA
! VERSION:     200005.26
! WRITTEN BY:  ROBERT (Sid) SAFFORD
! PURPOSE:     SUBROUTINE TO COMPUTE THE LIFF-OFF-AZIMUTH CONSTANTS

! INPUT PARAMETERS:
! -----------------
! AMAX         SEMI-MAJOR AXIS OF REFERENCE ELLIPSOID
! FLAT         FLATTENING (0.0033528 ... )
! ESQ          ECCENTRICITY SQUARED FOR REFERENCE ELLIPSOID
! PI           3.14159...
! DL           LON DIFFERENCE
! AZ1          AZI AT STA 1 -> STA 2

! OUTPUT PARAMETERS:
! ------------------
! AZ2          AZ2 AT STA 2 -> STA 1
! AO           CONST
! BO           CONST
! SMS          DISTANCE ... EQUATORIAL - GEODESIC  (S - s)   "SMS"

! LOCAL VARIABLES AND CONSTANTS:
! ------------------------------
! GLOBAL VARIABLES AND CONSTANTS:
! -------------------------------

!    MODULE CALLED BY:    GENERAL

!    THIS MODULE CALLS:
!       LLIBFORE/ DSIN,   DCOS,   DABS,   DASIN

!    INCLUDE FILES USED:
!    COMMON BLOCKS USED:

!    REFERENCES: Microsoft FORTRAN 4.10 Optimizing Compiler, 1988
!                MS-DOS Operating System
!    COMMENTS:
!********1*********2*********3*********4*********5*********6*********7*
!::MODIFICATION HISTORY
!::1985xx.xx, RWS, CODE   CREATED
!::198506.10, RWS, WRK    ENHANCEMENTS RELEASED TO FIELD
!::198509.01, RWS, VER 11 MTEN3  RELEASED TO FIELD
!::198512.18, RWS, CODE   MODIFIED FOR MTEN3
!::198708.10, RWS, CODE   MODIFIED TO USE NEW MTEN4 GPN RECORD FORMAT
!::199112.31, RWS, VER 20 MTEN4 RELEASED TO FIELD
!::200001.13, RWS, VER 21 MTEN4 RELEASED TO FIELD
!::200005.26, RWS, CODE   RESTRUCTURED & DOCUMENTATION ADDED
!::200012.31, RWS, VER 23 MTEN5 RELEASED
!********1*********2*********3*********4*********5*********6*********7*
!E::GPNLOA
! ---------------------------
!     M T E N  (VERSION 3)
!              (VERSION 4.22)
!              (VERSION 5.23)
! ---------------------------

    IMPLICIT real (kind=8) (A-H,O-Z)

    DATA TT/5.0D-13/

    DLON = DABS(DL)
    CONS = (PI-DLON)/(PI*FLAT)
    F    = FLAT

!     COMPUTE AN APPROXIMATE AZ

    AZ   = DASIN(CONS)

    T1   =    1.0D0
    T2   =  (-1.0D0/4.0D0)*F*(1.0D0+F+F*F)
    T4   =    3.0D0/16.0D0*F*F*(1.0D0+(9.0D0/4.0D0)*F)
    T6   = (-25.0D0/128.0D0)*F*F*F

    ITER = 0
    1 ITER = ITER+1
    S    = DCOS(AZ)
    C2   = S*S

!     COMPUTE NEW AO

    AO   = T1 + T2*C2 + T4*C2*C2 + T6*C2*C2*C2
    CS   = CONS/AO
    S    = DASIN(CS)
    IF( DABS(S-AZ) < TT )THEN
        GOTO 2
    ENDIF

    AZ   = S
    IF( ITER <= 6 )THEN
        GOTO 1
    ENDIF

    2 AZ1  = S
    IF( DL < 0.0D0 )THEN
        AZ1 = 2.0D0*PI-AZ1
    ENDIF

    AZ2  = 2.0D0*PI-AZ1

!     EQUATORIAL - GEODESIC  (S - s)   "SMS"

    ESQP = ESQ/(1.0D0-ESQ)
    S    = DCOS(AZ1)

    U2   = ESQP*S*S
    U4   = U2*U2
    U6   = U4*U2
    U8   = U6*U2

    T1   =     1.0D0
    T2   =    (1.0D0/4.0D0)*U2
    T4   =   (-3.0D0/64.0D0)*U4
    T6   =    (5.0D0/256.0D0)*U6
    T8   = (-175.0D0/16384.0D0)*U8

    BO   = T1 + T2 + T4 + T6 + T8
    S    = DSIN(AZ1)
    SMS  = AMAX*PI*(1.0D0 - FLAT*DABS(S)*AO - BO*(1.0D0-FLAT))

    RETURN
    END SUBROUTINE GPNLOA
