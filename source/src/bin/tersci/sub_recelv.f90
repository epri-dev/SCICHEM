    SUBROUTINE RECELV(IREC,JDEM)
!***********************************************************************
!*               RECELV Module of AERMAP Terrain Preprocessor
!*
!*       PURPOSE: Determine Receptor Elevations by a 2-D Interpolation
!*                Between the 4 Closest Raw Terrain Data Points
!*
!*
!*       PROGRAMMER: Jayant Hardikar, Roger Brode
!*
!*       DATE:    September 29, 1995
!*
!*       Revision History:
!*
!*       MODIFIED: February 9, 2009
!*
!*                Corrected problems associated with cross UTM zone
!*                applications associated with NAD datum conversions.
!*                Modified output format for UTM Zone to I3 to
!*                accommodate Southern Hemisphere applications.
!*                Corrected problems associated with handling of
!*                missing elevations from DEM or NED files. Any
!*                elevation less than or equal to -9000.0m is
!*                treated as missing.  Modified for use of standard
!*                convention of negative for West longitude.
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
!*       INPUTS:
!*
!*       OUTPUTS:
!*
!*       CALLED FROM:   MAIN
!***********************************************************************

!*    Variable Declarations
    USE TER_MAIN

    IMPLICIT NONE

    SAVE

    CHARACTER(12) :: MODNAM

    real (kind=8) :: XDM(4), YDM(4)
    real (kind=8) :: XRR, YRR, XRRM, YRRM
    real (kind=8) :: ARGE, ARGN, XARG, YARG, RLATS, RLONS
    real (kind=8) :: NADFLG
    INTEGER :: ISPHERE, JDEM, IREC, JRC(4)
    INTEGER :: IP, IPROF, IERR, JRECRD, JRECMSG, NUMZ
    INTEGER :: IZONREC

    LOGICAL :: L_Opened

    real (kind=8) :: READZ  ! Function to read elevation
! from direct access file

    real (kind=8) :: ZQUADZ(4), Z(4), AVEZ, SUMZ

    real (kind=8) :: ZREC

!*    Initializations
    MODNAM   = 'RECELV'
    L_Opened = .FALSE.
    ZQUADZ   = 0.0D0
    Z        = 0.0D0
    AVEZ     = 0.0D0
    SUMZ     = 0.0D0
    ZREC     = 0.0D0

    IF (RECDBG) THEN
        IF (IREC == 1) THEN
        !*          Determine whether RECELV debug file is already opened
            INQUIRE(UNIT=RELVK, OPENED=L_Opened)
            IF ( .NOT. L_Opened) THEN
                OPEN (UNIT=RELVK, FILE= RECELV_FILE, STATUS='REPLACE')
            !*             Write Version Date Header to RECELV Debug File
                WRITE(RELVK,9011) trim(VERSN), RUNDAT, RUNTIM
9011            FORMAT('TERSCI Version ',A,1x,A8,1x,A8/)
                WRITE(RELVK,100) NUMREC
                100 FORMAT( &
                'From RECELV: ', &
                /'     Receptor elevation calculations for ',I7,' receptors', &
                /'     IERR =  0: normal (non-edge) case', &
                /'     IERR = -1: edge receptor, no y-adjustment', &
                /'     IERR = -2: edge receptor, w/ y-adjustment', &
                /'     IERR = -3: receptor beyond range of data;', &
                /'                possible gap within data file;', &
                /'                flagged with warning message 400', &
                /)
            END IF
        END IF
    END IF

    IZONREC = IZONR(IREC)

!     Output header to debug file
    IF (RECDBG) THEN
        WRITE(RELVK,101) IREC
        101 FORMAT('RECEPTOR ELEVATION CALCS FOR REC#',I7)
        WRITE(RELVK,102) TYPDAT, JDEM, &
        DEMFIL(JDEM)(1:LEN_TRIM(DEMFIL(JDEM)))
        102 FORMAT(/'LOCATED IN ',A3,' FILE: ',I5,' ;  ',A)
        WRITE(RELVK,103) TYPDAT, NADD(JDEM), NADA
        103 FORMAT(/A3,' FILE NAD:  ',I3' ;  ANCHOR POINT NAD: ',I3/)
    END IF

!     Determine whether to apply NAD shift
    IF (NADA == 0 .OR. NADA == NADD(JDEM)) THEN

        NADFLG = 0.0D0                ! No NAD shift needed

    ELSE IF( (NADA == 1    .OR.  NADA >= 5) .AND. &
        (NADD(JDEM) >= 2 .AND. NADD(JDEM) <= 4) )THEN

        NADFLG = 1.0D0                ! Include NAD shift

    ELSE IF( (NADA >= 2    .AND. NADA <= 4) .AND. &
        (NADD(JDEM) == 1 .OR.  NADD(JDEM) >= 5) )THEN

        NADFLG = 1.0D0                ! Include NAD shift

    ELSE

        NADFLG = 0.0D0                ! No NAD shift needed

    END IF

!*    Apply NAD shift to receptor coordinates; longitude shift is
!*    assumed to be positive to the East
    RLONS = RLON(IREC) + (XRDIFS(IREC) * NADFLG)
    RLATS = RLAT(IREC) + (YRDIFS(IREC) * NADFLG)

    XRRM = XRECU(IREC) + (XRDIFM(IREC) * NADFLG)
    YRRM = YRECU(IREC) + (YRDIFM(IREC) * NADFLG)

    IF (IPLAN(JDEM) == 1 .AND. IZONR(IREC) /= IZOND(JDEM)) THEN
    !*       DEM file based on UTM projection, and receptor zone doesn't match DEM zone;
    !*       Convert receptor Lat/Lon to UTMs, based on DEM file NADD and DEM Zone

        SELECT CASE (NADD(JDEM))   ! Use datum for DEM file
        CASE (0)
        IF (IPLAN(JDEM) == 0) THEN
            ISPHERE = 4  ! UTM/LAT-LONG conversions based on GRS80 ellipsoid
        ELSE IF (IPLAN(JDEM) == 1) THEN
            ISPHERE = 0  ! UTM/LL conversions based on Clarke 1866 ellipsoid
        END IF
        CASE (2:4)
        ISPHERE = 4     ! UTM/LAT-LONG conversions based on GRS80 ellipsoid
        CASE (1,5:6)
        ISPHERE = 0     ! UTM/LL conversions based on Clarke 1866 ellipsoid
        CASE DEFAULT
        ISPHERE = 4     ! DEFAULT CASE shouldn't occur
        END SELECT

        ARGE = RLONS
        ARGN = RLATS
        XARG = 0.0D0
        YARG = 0.0D0
    !*       Pass DEM file zone (IZOND(IDEM)) to UTMGEO to get new receptor
    !*       coordinates (XARG, YARG) referenced to IZOND(IDEM)).
        CALL UTMGEO (555,IZOND(JDEM),IZONREC,XARG,YARG,ARGE,ARGN, &
        ISPHERE)
        XRRM = XARG
        YRRM = YARG
    END IF

!     Select the receptor coords for FIND4 based on coord units of DEM file
    IF (IPLAN(JDEM) == 0) THEN
        YRR = RLATS
        XRR = RLONS
    ELSE IF (IPLAN(JDEM) == 1) THEN
        YRR = YRRM
        XRR = XRRM
    END IF

    IF (RECDBG) THEN
        WRITE(RELVK, 301) TYPDAT, IZOND(JDEM), RLONS, RLATS, &
        RLONS/3600.0D0, RLATS/3600.0D0, &
        XRRM, YRRM, IZONREC
        301 FORMAT (A3,' FILE ZONE: ',I3,' ;  RECEPTOR LOCATION ', &
        '(Lon, Lat, arc-seconds): ',2F15.4, &
        /40X,'(Lon, Lat, dec-degrees): ',2F15.6, &
        /45X,'(UTMx, UTMy, Zone): ',2F15.2,I4/)
    END IF

!*    Find the 4 Data Points Closest to the Receptor
!*    First initialize record numbers for nodes
    JREC = 0

!*    Reopen the Index & Direct Access Files
    OPEN (IDXUNT(JDEM),FILE=IDXFIL(JDEM),STATUS='OLD', &
    ERR=999)
    REWIND (IDXUNT(JDEM))

    OPEN (IDRUNT(JDEM),FILE=DIRFIL(JDEM), &
    ACCESS='DIRECT',RECL=LREC_DIR,ERR=999,STATUS='OLD')


!*    Read the Index File for this Terrain File
    IF (NUMPRF(JDEM) > 0) THEN
        DO IPROF = 1, NUMPRF(JDEM)
            READ (IDXUNT(JDEM),IDXFRM,ERR=98,END=98) XBASE(IPROF), &
            YBASE(IPROF), NODES(IPROF), IZONP(IPROF), MAXEL(IPROF)
        END DO

        GO TO 900

    ELSE

        WRITE(IOUNIT,*) 'Direct Access File Empty, IDEM = ',jdem
        WRITE(*,*) 'Direct Access File Empty, IDEM = ',jdem
    !*       Error Reading Direct Access File
        WRITE(DUMMY,'("IDR",I5.5)') MIN(IDRUNT(JDEM),99999)
        CALL ERRHDL(PATH,MODNAM,'E','510',DUMMY)
        RUNERR = .TRUE.

        RETURN

    END IF

    98 CONTINUE

!*    Error Reading Index File
    WRITE(DUMMY,'("IDX",I5.5)') MIN(IDXUNT(JDEM),99999)
    CALL ERRHDL(PATH,MODNAM,'E','510',DUMMY)
    RUNERR = .TRUE.

    RETURN

    900 CONTINUE

    IF (RECDBG) THEN
        WRITE(RELVK,303) IREC, JDEM
        303 FORMAT ('      CALLING FIND4 w/ IREC, JDEM: ', I7, I4)
    END IF

!*    Find 4 DEM nodes closest to source location
!*
    CALL FIND4(JDEM,IREC,IERR,XRR,YRR)

!*    For the 4 Points, Call Subroutine to Calculate the Record#
!*    to Read the DEM Elevation Data From the Direct Access File

    CALL RECNUM

!*    Check for missing node elevation, based on missing indicator of -9999.
    NUMZ = 0
    SUMZ = 0.0D0
    JRECMSG = 0
    DO IP = 1, 4
    !*       Read the elevation for this node from the direct access file
        JRECRD = JREC(IP)
        Z(IP) = READZ(JDEM,JRECRD)
        IF (Z(IP) <= -9000.0D0 .AND. JRECMSG == 0) THEN
        !*          Elevation is missing for this node; Warning issued below
        !*          Save record number for missing node elevation
            JRECMSG = JRECRD
        ELSE IF (Z(IP) <= -9000.0D0 .AND. JRECMSG /= 0) THEN
        !*          Elevation is missing for multiple nodes
            CONTINUE
        ELSE
        !*          Valid node elevation; calculate sum and num of valid elevations
            SUMZ = SUMZ + Z(IP)
            NUMZ = NUMZ + 1
        END IF
    END DO

    IF (NUMZ > 0 .AND. NUMZ < 4) THEN
    !*       Elevation is missing for at least one node, but not all four
    !*       Write Warning Message:
        WRITE(DUMMY,'(I8)') IREC
        CALL ERRHDL(PATH,MODNAM,'W','415',DUMMY)

    !*       Calculate average elevation for non-missing nodes for substitution
        AVEZ = SUMZ/DBLE(NUMZ)
        DO IP = 1, 4
            IF (Z(IP) <= -9000.0D0) THEN
            !*             Assign average value based on adjacent non-missing nodes
                Z(IP) = AVEZ
            END IF
        END DO

    ELSE IF (NUMZ == 0) THEN
    !*       All four elevation nodes are missing
    !*       Write Warning Message:
        IERR = -3
        WRITE(DUMMY,'(I8)') IREC
        CALL ERRHDL(PATH,MODNAM,'W','416',DUMMY)

    !*       Assign -9999.0 missing code to all nodes
        DO IP = 1, 4
            Z(IP) = -9999.0D0
        END DO

    END IF

    IF (RECDBG) THEN
        WRITE(RELVK,50) IERR
        50 FORMAT(7X,'DEM RECNUM:',/11X,'IERR =',I3, &
        /12X,'IP    JREC   NX   NY')
        WRITE (RELVK,51)  JREC(1), NX(1), NY1(1)
        WRITE (RELVK,52)  JREC(2), NX(2), NY1(2)
        WRITE (RELVK,53)  JREC(3), NX(2), NY2(2)
        WRITE (RELVK,54)  JREC(4), NX(1), NY2(1)
        51 FORMAT (12X, ' 1', I8, 2I5)
        52 FORMAT (12X, ' 2', I8, 2I5)
        53 FORMAT (12X, ' 3', I8, 2I5)
        54 FORMAT (12X, ' 4', I8, 2I5)
        WRITE(RELVK,304)
        304 FORMAT(/11X, 'Based on the DEM Coding of:'//, &
        &              11X, '  IP= 2----3'/, &
        &              11X, '      |    |'/, &
        &              11X, '  IP= 1----4'/)
    END IF

!     Subroutine Find4 vs DEM node numbering system:

!      4----3              2----3
!      |  x |              |  x |
!      |    |              |    |
!      1----2              1----4

!     Convert Find4 to DEM numbering system:

    XDM(1) = X(1)
    XDM(2) = X(1)
    XDM(3) = X(2)
    XDM(4) = X(2)

    YDM(1) = Y(1)
    YDM(2) = Y(4)
    YDM(3) = Y(3)
    YDM(4) = Y(2)

    ZQUADZ(1) = Z(1)
    ZQUADZ(2) = Z(4)
    ZQUADZ(3) = Z(3)
    ZQUADZ(4) = Z(2)

    JRC(1) = JREC(1)
    JRC(2) = JREC(4)
    JRC(3) = JREC(3)
    JRC(4) = JREC(2)

    IF (RECDBG) THEN
        WRITE(RELVK,305) XRR, YRR
        305 FORMAT(7X,'RECELV:              X-Rec       Y-Rec'/, &
        &             24X,2F12.2,// &
        &         9X,'IP    JREC  DEM      XDM         YDM         ELEV')
        DO IP = 1, 4
            WRITE(RELVK,306) IP, JRC(IP), JDEM, &
            XDM(IP), YDM(IP), ZQUADZ(IP)
            306 FORMAT(9X,I2,I8,I5, 2F12.2, F10.2)
        END DO
    END IF

    CLOSE (IDXUNT(JDEM))
    CLOSE (IDRUNT(JDEM))

!*    Call Subroutine to Interpolate Z value beween 4 points
    CALL INT2D(Z,X,Y,XRR,YRR,ZREC)

! --- Check for internal gap receptors and assign flag in IRIN array
    IF (IERR == -3 .AND. IRIN(IREC,JDEM) /= 2) THEN
    !*       Internal gap receptor; increment counter and set flag
        NRGAP3 = NRGAP3 + 1
        IRIN(IREC,JDEM) = 3
    END IF

!*    Check for IERR = -3 condition or ZREC = -9999.0
!*    set AZELEV = -9999.0 unless FILLGAPS option was specified
    IF (( .NOT. FILLGAPS .AND. IERR == -3) .OR. &
    ZREC == -9999.0D0) THEN
    !*       Receptor is located within a gap or nodes elevations are missing;
    !*       FILLGAPS option not selected, set receptor elevation missing
        AZELEV(IREC) = -9999.0D0
        IF (RECDBG) THEN
            IF (IRIN(IREC,JDEM) == 2) THEN
                WRITE(RELVK,3072) IREC, TYPDAT, JDM(IREC), TYPDAT, &
                IREC, AZELEV(IREC)
                3072 FORMAT(/'RECEPTOR ',I7,' IS LOCATED WITHIN A NAD GAP', &
                ' ADJACENT TO ',A3,' FILE ',I7,';', &
                /17X,'ELEVATION ASSIGNED MISSING VALUE = -9999.0 (M)', &
                /17X,'SUBSEQUENT ',A3,' FILES MAY PROVIDE ', &
                /17X,'A NON-MISSING VALUE.', &
                //'ELEVATION (M) FOR IREC ',I7,' = ',F12.2, &
                //'---------'/)
            ELSE
                WRITE(RELVK,3073) IREC, TYPDAT, JDM(IREC), TYPDAT, &
                IREC, AZELEV(IREC)
                3073 FORMAT(/'RECEPTOR ',I7,' IS LOCATED WITHIN A GAP', &
                ' INSIDE ',A3,' FILE ',I7,';', &
                /17X,'ELEVATION ASSIGNED MISSING VALUE = -9999.0 (M)', &
                /17X,'SUBSEQUENT ',A3,' FILES MAY PROVIDE ', &
                /17X,'A NON-MISSING VALUE.', &
                //'ELEVATION (M) FOR IREC ',I7,' = ',F12.2, &
                //'---------'/)
            END IF
        END IF

    ELSE IF (FILLGAPS .AND. IERR == -3 .AND. &
        ZREC /= -9999.0D0) THEN
    !*       Receptor is located within a gap or nodes elevations are missing;
    !*       FILLGAPS option selected, use elevation from closest nodes
        LRec_FILLED(IREC,JDM(IREC)) = .TRUE.
        AZELEV(IREC) = ZREC
        IF (RECDBG) THEN
            IF (IRIN(IREC,JDEM) == 2) THEN
                WRITE(RELVK,3082) IREC, TYPDAT, JDM(IREC), TYPDAT, IREC, &
                ZREC
                3082 FORMAT(/'RECEPTOR ',I7,' IS LOCATED WITHIN A NAD GAP', &
                ' ADJACENT TO ',A3,' FILE ',I7,';', &
                /17X,'FILLGAPS OPTION HAS BEEN SELECTED AND A PRELIMINARY', &
                /17X,'ELEVATION HAS BEEN ASSIGNED BASED ON CLOSEST NODES.', &
                /17X,'SUBSEQUENT ',A3,' FILES MAY PROVIDE A NON-GAP VALUE.', &
                //'PRELIMINARY ELEVATION (M) FOR IREC ',I7,' = ',F12.2, &
                //,'---------'/)
            ELSE
                WRITE(RELVK,3083) IREC, TYPDAT, JDM(IREC), TYPDAT, IREC, &
                ZREC
                3083 FORMAT(/'RECEPTOR ',I7,' IS LOCATED WITHIN A GAP', &
                ' INSIDE ',A3,' FILE ',I7,';', &
                /17X,'FILLGAPS OPTION HAS BEEN SELECTED AND A PRELIMINARY', &
                /17X,'ELEVATION HAS BEEN ASSIGNED BASED ON CLOSEST NODES.', &
                /17X,'SUBSEQUENT ',A3,' FILES MAY PROVIDE A NON-GAP VALUE.', &
                //'PRELIMINARY ELEVATION (M) FOR IREC ',I7,' = ',F12.2, &
                //,'---------'/)
            END IF
        END IF

    ELSE IF (IRIN(IREC,JDEM) == 2 .AND. IERR < 0) THEN
    !*       Receptor located in NAD-shift gap, but close enough to be
    !*       treated as edge receptor.
    !*       Assign interpolated value to receptor elevation array
        AZELEV(IREC) = ZREC
        IF (RECDBG) THEN
        !*          Write elevation to debug file
            WRITE(RELVK,3092) IREC, TYPDAT, JDM(IREC), IREC, ZREC
            3092 FORMAT(/'RECEPTOR ',I7,' IS LOCATED WITHIN A NAD GAP', &
            ' ADJACENT TO ',A3,' FILE ',I7,';', &
            /17X,'HOWEVER, RECEPTOR IS WITHIN ONE "GRID" SPACING OF', &
            /17X,'CLOSEST NODE AND IS TREATED AS AN EDGE RECEPTOR.', &
            //'ELEVATION (M) FOR IREC ',I7,' = ',F12.2, &
            //,'---------'/)
        END IF

    ELSE
    !*       Assign interpolated value to receptor elevation array
        AZELEV(IREC) = ZREC
        IF (RECDBG) THEN
        !*          Write elevation to debug file
            WRITE(RELVK,309) IREC, ZREC
            309 FORMAT(/'ELEVATION (M) FOR IREC ',I7,' = ',F12.2, &
            //,'---------'/)

        END IF

    END IF

    RETURN

    999 CONTINUE
! --- Error opening direct access file; this shouldn't happen
    WRITE(DUMMY,'("DIR:",I4)') MIN(IDXUNT(JDEM),9999)
    CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)
    RUNERR = .TRUE.

    RETURN

    end subroutine


    SUBROUTINE FIND4(IDM,IR,IERR, XRR, YRR)
!***********************************************************************
!*    PURPOSE:  THIS SUBROUTINE WILL LOOP OVER THE DEM ROWS AND COLUMNS
!*              OF DATA WITHIN THE DOMAIN AND FIND 4 POINTS AND THEIR
!*              INDEXES WHICH SURROUND A GIVEN RECEPTOR POINT
!*
!*                4------3
!*                |  X   |
!*                |      |
!*                1------2
!*
!*    PROGRAMMER: Roger Brode, Jayant Hardikar
!*
!*    DATE:       April 14, 1997
!*
!*    MODIFIED: June 30, 2008
!*
!*             Corrected problems with handling edge receptors for
!*             some situations, including receptors beyond range of
!*             nodes vertically.  These problems may occur with
!*             with some DEM files converted from other formats.
!*             Roger W. Brode, U.S. EPA, OAQPS, AQMG
!*
!*    MODIFIED: December 7, 2006
!*
!*             Modified procedure for selecting nodes for edge receptors
!*             to account for proximity in both Northing and Easting
!*             direction for 7.5-minute DEM files.
!*             Roger W. Brode, U.S. EPA, OAQPS, AQMG
!*
!*    INPUTS:     DEM file index, IDM
!*                Receptor index, IR
!*
!*    OUTPUTS:    Locations and indices for four points surrounding
!*                receptor location
!*
!*    CALLED FROM:   RECELV
!***********************************************************************

!*----DECLARE VARIABLES-------------------------------------------------
    USE TER_MAIN

    IMPLICIT NONE

    SAVE

    CHARACTER(12) :: MODNAM

    real (kind=8) :: XCOMP1,XCOMP2,YCOMP1,YCOMP2,YY1,YY2
    real (kind=8) :: XRR, YRR

    INTEGER :: IERR, IBEGIN, IDM, IR, IENDS, JX, IP, NXNODE
    INTEGER :: IN

!*    Initialize Variables
    MODNAM = 'FIND4'

!*    Initialize IERR flag to 0, normal case
    IERR = 0

    IF (RECDBG) THEN
        WRITE(RELVK, 100) IDM, IR, XRR, YRR
        100 FORMAT(/7X,'FIND4:'/ &
        '        DEM  REC        XRR             YRR', &
        /8X,I3,I5, 2F16.4/)

        WRITE(RELVK,376) MAPNAME(IDM)
        376 FORMAT(11X,A40)

        WRITE(RELVK,377) SWE_MTRS(IDM), NWE_MTRS(IDM), &
        NEE_MTRS(IDM), SEE_MTRS(IDM), &
        SWN_MTRS(IDM), NWN_MTRS(IDM), &
        NEN_MTRS(IDM), SEN_MTRS(IDM)
        377 FORMAT(25X,'  SW Corner   ', &
        '  NW Corner   ', &
        '  NE Corner   ', &
        '  SE Corner', &
        /13X,'Easting: ',4F14.3/13X,'Northing:',4F14.3)

        WRITE(RELVK, 412) &
        SWLAT_DEGS(IDM),NWLAT_DEGS(IDM), &
        NELAT_DEGS(IDM),SELAT_DEGS(IDM), &
        SWLON_DEGS(IDM),NWLON_DEGS(IDM), &
        NELON_DEGS(IDM),SELON_DEGS(IDM)
        412 FORMAT(13X,'Latitude: ',2x, 4(F11.4,3x)/ &
        &           13X,'Longitude:',2x, 4(F11.4,3x)/)

    END IF

!*    Initialize X and Y values and indices
    X   = 0.0D0
    NX  = 0
    NY1 = 0
    NY2 = 0

    YY1 = 0.0D0
    YY2 = 0.0D0

!*    LOOP OVER ALL X-VALUES OF PROFILES

    IF (IPLAN(IDM) == 1) THEN
    !*       For 7.5-minute files compute index for profile just west of the
    !*       receptor, but make sure it is west of the SE corner of the DEM
    !*       to handle edge receptors along the east side of the DEM file.
        IBEGIN = MAX(1,IDINT((MIN(XRR,SEE_MTRS(IDM)) &
        -XBASE(1))/DXM(IDM)))
        IENDS  = NUMPRF(IDM)
    ELSE IF (IPLAN(IDM) == 0) THEN
        IBEGIN = MAX(1,IDINT((MIN(XRR,SELON_ARCS(IDM)) &
        -XBASE(1))/DXM(IDM)))
        IENDS  = NUMPRF(IDM)
    END IF

!*    Adjust loop bounds if IBEGIN > IENDS
    IF (IBEGIN > IENDS) IBEGIN = IENDS

!*--- Loop over profiles
    DO JX = IBEGIN, IENDS

    !*       Loop through profiles in X-direction looking for profiles that
    !*       bracket the receptor location, or profile that is collocated with
    !*       receptor.

        XCOMP1 = XBASE(JX)
        XCOMP2 = XRR

        IF ((XCOMP1 > XCOMP2) .AND. &
        (XCOMP1 <= XCOMP2+DXM(IDM))) THEN   ! Profile-X has passed Receptor-X

        !*          Check to avoid using distant node in the Northing direction
        !*          along edge profiles for 7.5-minute DEM files.
        !*          IERR =  0 means normal (non-edge) case
        !*          IERR = -1 means edge receptor, but no adjustment made for y-offset
        !*          IERR = -2 means edge receptor, with adjustment made for y-offset
        !*          IERR = -3 means edge receptor, beyond range of profiles or nodes;
        !*                    warning message 400 generated if IERR = -3

            IF ((YBASE(JX) <= (YRR+DYM(IDM))) .AND. &
            ((YBASE(JX)+DBLE((NODES(JX)-1)*DYM(IDM))) >= &
            (YRR-DYM(IDM)))) THEN
            !*             Receptor is within range of nodes within profile

                IF (NX(1) <= 0) THEN
                !*                Receptor location is outside E-W range of profiles, use closest
                    IERR  = MIN(IERR, -1)  ! Edge receptor, no y-offset
                    NX(1) = JX
                    NX(2) = JX
                    X(1)  = XCOMP1
                    X(2)  = XCOMP1
                    EXIT                   ! Exit loop
                ELSE
                    NX(2) = JX
                    X(2)  = XCOMP1
                    EXIT                   ! Normal (non-edge) case; Exit loop
                END IF

            ELSE IF ( (XCOMP1 > SEE_MTRS(IDM) .AND. &
                XCOMP1 <= NEE_MTRS(IDM) .AND. &
                YBASE(JX) > (YRR+DYM(IDM))) .OR. &

                (XCOMP1 > NEE_MTRS(IDM) .AND. &
                XCOMP1 <= SEE_MTRS(IDM) .AND. &
                ((YBASE(JX)+DBLE((NODES(JX)-1)*DYM(IDM))) < &
                (YRR-DYM(IDM))))) THEN
            !*             Receptor is beyond range of nodes within profile

                IF ((JX-1 >= IBEGIN) .AND. &
                ((YBASE(JX-1) > (YRR+DYM(IDM))) .OR. &
                ((YBASE(JX-1)+DBLE((NODES(JX-1)-1)*DYM(IDM))) < &
                (YRR-DYM(IDM)))) ) THEN
                !*                Receptor is well beyond range of nodes within profile;
                !*                skip to next profile for closest nodes
                    NX(1) = MAX(JX-2,IBEGIN)
                    NX(2) = MAX(JX-2,IBEGIN)
                    X(1)  = XCOMP1-2.0D0*DXM(IDM)
                    X(2)  = XCOMP1-2.0D0*DXM(IDM)
                !*                Receptor location is outside range of profiles, use closest
                    IF (IERR > -3) THEN
                        WRITE(DUMMY,'(I8)') IR
                        CALL ERRHDL(PATH,MODNAM,'W','400',DUMMY)
                    END IF
                    IERR  = -3               ! West edge of DEM
                    EXIT
                ELSE
                !*                Receptor is slightly beyond range of nodes within profile
                !*                use closest profile
                    IERR  = MIN(IERR, -2)    ! East edge of DEM, use previous profile
                    NX(1) = MAX(JX-1,IBEGIN)
                    NX(2) = MAX(JX-1,IBEGIN)
                    X(1)  = XCOMP1-DXM(IDM)
                    X(2)  = XCOMP1-DXM(IDM)
                    EXIT                     ! Exit loop
                END IF

            ELSE IF (NX(1) <= 0 .AND. JX < IENDS) THEN
            !*             Receptor location is outside E-W range of profiles, use closest

                IF ((YBASE(JX) > (YRR+DYM(IDM))) .OR. &
                ((YBASE(JX)+DBLE((NODES(JX)-1)*DYM(IDM))) < &
                (YRR-DYM(IDM)))) THEN
                !*                Receptor is well beyond range of nodes within profile;
                !*                skip to next profile for closest nodes
                    NX(1) = MIN(JX+1,IENDS)
                    NX(2) = MIN(JX+1,IENDS)
                    X(1)  = XCOMP1+DXM(IDM)
                    X(2)  = XCOMP1+DXM(IDM)
                ELSE
                !*                Receptor is slightly beyond range of nodes within profile
                !*                use closest profile
                    NX(1) = JX
                    NX(2) = JX
                    X(1)  = XCOMP1
                    X(2)  = XCOMP1
                END IF
            !*             Receptor location is outside range of profiles, use closest
                IF (IERR > -3) THEN
                    WRITE(DUMMY,'(I8)') IR
                    CALL ERRHDL(PATH,MODNAM,'W','400',DUMMY)
                END IF
                IERR  = -3               ! West edge of DEM
                EXIT                     ! Exit loop

            ELSE
                NX(2) = JX               ! File may be bad, appears to
                X(2)  = XCOMP1           ! have data gap. Issue warning.
            !*             Receptor location is outside range of profiles, use closest
                IF (IERR > -3) THEN
                    WRITE(DUMMY,'(I8)') IR
                    CALL ERRHDL(PATH,MODNAM,'W','400',DUMMY)
                END IF
                IERR  = -3
                EXIT                     ! Exit loop

            END IF

        ELSE IF (XCOMP1 > XCOMP2+DXM(IDM)) THEN   ! Profile-X has passed Receptor-X

            IF (NX(1) <= 0 .AND. JX < IENDS) THEN
            !*             Receptor location is outside range of profiles, use closest
                IF ((YBASE(JX) > (YRR+DYM(IDM))) .OR. &
                ((YBASE(JX)+DBLE((NODES(JX)-1)*DYM(IDM))) < &
                (YRR-DYM(IDM)))) THEN
                !*                Receptor is well beyond range of nodes within profile;
                !*                skip to next profile for closest nodes
                    NX(1) = MIN(JX+1,IENDS)
                    NX(2) = MIN(JX+1,IENDS)
                    X(1)  = XCOMP1+DXM(IDM)
                    X(2)  = XCOMP1+DXM(IDM)
                ELSE
                !*                Receptor is slightly beyond range of nodes within profile
                !*                use closest profile
                    NX(1) = JX
                    NX(2) = JX
                    X(1)  = XCOMP1
                    X(2)  = XCOMP1
                END IF
            !*             Set flag and issue warning message for "gap" receptor
                IF (IERR > -3) THEN
                    WRITE(DUMMY,'(I8)') IR
                    CALL ERRHDL(PATH,MODNAM,'W','400',DUMMY)
                END IF
                IERR  = -3               ! West edge of DEM
                EXIT                     ! Exit loop

            ELSE
                NX(2) = JX               ! File may be bad, appears to have gap
                X(2)  = XCOMP1           ! along North boundary. Issue warning.
            !*             Receptor location is outside range of profiles, use closest
                IF (IERR > -3) THEN
                    WRITE(DUMMY,'(I8)') IR
                    CALL ERRHDL(PATH,MODNAM,'W','400',DUMMY)
                END IF
                IERR  = -3               ! West edge of DEM
                EXIT                     ! Exit loop

            END IF

        ELSE IF (XCOMP1 == XCOMP2) THEN   ! Profile-X and Receptor-X match
        !*          Check Northing coordinates for edge receptors
            IF ((YBASE(JX) <= (YRR+DYM(IDM))) .AND. &
            ((YBASE(JX)+DBLE((NODES(JX)-1)*DYM(IDM))) >= &
            (YRR-DYM(IDM)))) THEN
            !*             Receptor is within range of nodes within profile

                NX(1) = JX
                NX(2) = JX
                X(1)  = XCOMP1
                X(2)  = XCOMP1
                EXIT                     ! Normal (non-edge) case; Exit loop

            ELSE IF ( (XCOMP1 > NWE_MTRS(IDM) .AND. &
                XCOMP1 <= SWE_MTRS(IDM) .AND. &
                YBASE(JX) > (YRR+DYM(IDM))) .OR. &

                (XCOMP1 > SWE_MTRS(IDM) .AND. &
                XCOMP1 <= NWE_MTRS(IDM) .AND. &
                ((YBASE(JX)+DBLE((NODES(JX)-1)*DYM(IDM))) < &
                (YRR-DYM(IDM))))) THEN

                IF ((JX+1 <= IENDS) .AND. &
                ((YBASE(JX+1) > (YRR+DYM(IDM))) .OR. &
                ((YBASE(JX+1)+DBLE((NODES(JX+1)-1)*DYM(IDM))) < &
                (YRR-DYM(IDM)))) ) THEN
                !*                Receptor is well beyond range of nodes within profile;
                !*                skip to next profile for closest nodes
                    NX(1) = MIN(JX+2,IENDS)
                    NX(2) = MIN(JX+2,IENDS)
                    X(1)  = XCOMP1+2.0D0*DXM(IDM)
                    X(2)  = XCOMP1+2.0D0*DXM(IDM)
                    IF (IERR > -3) THEN
                        WRITE(DUMMY,'(I8)') IR
                        CALL ERRHDL(PATH,MODNAM,'W','400',DUMMY)
                    END IF
                    IERR  = -3               ! West edge of DEM
                    EXIT                     ! Exit loop
                ELSE
                !*                Receptor is slightly beyond range of nodes within profile
                !*                use closest profile
                    IERR  = MIN(IERR, -2)    ! West edge of DEM, use next profile
                    NX(1) = MIN(JX+1,IENDS)
                    NX(2) = MIN(JX+1,IENDS)
                    X(1)  = XCOMP1+DXM(IDM)
                    X(2)  = XCOMP1+DXM(IDM)
                    EXIT                     ! Exit loop
                END IF

            ELSE IF ( (XCOMP1 > SEE_MTRS(IDM) .AND. &
                XCOMP1 <= NEE_MTRS(IDM) .AND. &
                YBASE(JX) > (YRR+DYM(IDM))) .OR. &

                (XCOMP1 > NEE_MTRS(IDM) .AND. &
                XCOMP1 <= SEE_MTRS(IDM) .AND. &
                ((YBASE(JX)+DBLE((NODES(JX)-1)*DYM(IDM))) < &
                (YRR-DYM(IDM))))) THEN
            !*             Receptor is beyond range of nodes within profile

                IF ((JX-1 >= IBEGIN) .AND. &
                ((YBASE(JX-1) > (YRR+DYM(IDM))) .OR. &
                ((YBASE(JX-1)+DBLE((NODES(JX-1)-1)*DYM(IDM))) < &
                (YRR-DYM(IDM)))) ) THEN
                !*                Receptor is well beyond range of nodes within profile;
                !*                skip to next profile for closest nodes
                    NX(1) = MAX(JX-2,IBEGIN)
                    NX(2) = MAX(JX-2,IBEGIN)
                    X(1)  = XCOMP1-2.0D0*DXM(IDM)
                    X(2)  = XCOMP1-2.0D0*DXM(IDM)
                !*                Receptor location is outside range of profiles, use closest
                    IF (IERR > -3) THEN
                        WRITE(DUMMY,'(I8)') IR
                        CALL ERRHDL(PATH,MODNAM,'W','400',DUMMY)
                    END IF
                    IERR  = -3               ! West edge of DEM
                    EXIT
                ELSE
                !*                Receptor is slightly beyond range of nodes within profile
                !*                use closest profile
                    IERR  = MIN(IERR, -2)    ! East edge of DEM, use previous profile
                    NX(1) = MAX(JX-1,IBEGIN)
                    NX(2) = MAX(JX-1,IBEGIN)
                    X(1)  = XCOMP1-DXM(IDM)
                    X(2)  = XCOMP1-DXM(IDM)
                    EXIT                     ! Exit loop
                END IF

            ELSE                        ! West edge of DEM, continue processing

            !*             Receptor location is outside N-S range of profile, but within DYM
                IERR  = MIN(IERR, -1)  ! Edge receptor, no y-offset
                NX(1) = JX
                NX(2) = JX
                X(1)  = XCOMP1
                X(2)  = XCOMP1
                EXIT                   ! Exit loop

            END IF

        ELSE IF (XCOMP1 < XCOMP2) THEN   ! Profile-X hasn't reached Receptor-X
        !*          Check Northing coordinates for edge receptors
            IF ((YBASE(JX) <= (YRR+DYM(IDM))) .AND. &
            ((YBASE(JX)+DBLE((NODES(JX)-1)*DYM(IDM))) >= &
            (YRR-DYM(IDM)))) THEN
                NX(1) = JX
                X(1)  = XCOMP1
                IF (JX == IENDS) THEN  ! Already at last profile
                    IF (XCOMP1+DXM(IDM) < XCOMP2) THEN
                        IF (IERR > -3) THEN
                            WRITE(DUMMY,'(I8)') IR
                            CALL ERRHDL(PATH,MODNAM,'W','400',DUMMY)
                        END IF
                        IERR = -3
                    ELSE
                        IERR = -1
                    END IF
                    NX(2) = JX
                    X(2)  = XCOMP1
                    EXIT                  ! Exit loop
                ELSE
                    CYCLE
                END IF

            ELSE IF ( (XCOMP1+DXM(IDM) > XCOMP2) .AND. &
                (XCOMP1 > NWE_MTRS(IDM) .AND. &
                XCOMP1 <= SWE_MTRS(IDM) .AND. &
                YBASE(JX) > (YRR+DYM(IDM))) .OR. &

                (XCOMP1+DXM(IDM) > XCOMP2) .AND. &
                (XCOMP1 > SWE_MTRS(IDM) .AND. &
                XCOMP1 <= NWE_MTRS(IDM) .AND. &
                ((YBASE(JX)+DBLE((NODES(JX)-1)*DYM(IDM))) < &
                (YRR-DYM(IDM)))) ) THEN

                IF (JX+1 <= IENDS .AND. &
                ((YBASE(JX+1) > (YRR+DYM(IDM))) .OR. &
                ((YBASE(JX+1)+DBLE((NODES(JX+1)-1)*DYM(IDM))) < &
                (YRR-DYM(IDM)))) ) THEN
                !*                Receptor is well beyond range of nodes within profile;
                !*                skip to next profile for closest nodes
                    NX(1) = MIN(JX+2,IENDS)
                    NX(2) = MIN(JX+2,IENDS)
                    X(1)  = XCOMP1+2.0D0*DXM(IDM)
                    X(2)  = XCOMP1+2.0D0*DXM(IDM)
                    IF (IERR > -3) THEN
                        WRITE(DUMMY,'(I8)') IR
                        CALL ERRHDL(PATH,MODNAM,'W','400',DUMMY)
                    END IF
                    IERR  = -3               ! West edge of DEM
                    EXIT                     ! Exit loop
                ELSE
                !*                Receptor is slightly beyond range of nodes within profile
                !*                use closest profile
                    IERR  = MIN(IERR, -2)    ! West edge of DEM, use next profile
                    NX(1) = MIN(JX+1,IENDS)
                    NX(2) = MIN(JX+1,IENDS)
                    X(1)  = XCOMP1+DXM(IDM)
                    X(2)  = XCOMP1+DXM(IDM)
                    EXIT                     ! Exit loop
                END IF

            ELSE IF (XCOMP1+DXM(IDM) > XCOMP2) THEN
                IF (IERR > -3) THEN
                    WRITE(DUMMY,'(I8)') IR
                    CALL ERRHDL(PATH,MODNAM,'W','400',DUMMY)
                END IF
                IERR = -3
                NX(1) = JX
                X(1)  = XCOMP1
                IF (JX == IENDS) THEN  ! Already at last profile
                    NX(2) = JX
                    X(2)  = XCOMP1
                    EXIT                  ! Exit loop
                ELSE
                    CYCLE
                END IF

            ELSE IF (JX == IENDS) THEN
                IF (IERR > -3) THEN
                    WRITE(DUMMY,'(I8)') IR
                    CALL ERRHDL(PATH,MODNAM,'W','400',DUMMY)
                END IF
                IERR = -3

                IF ((YBASE(JX) <= (YRR+DYM(IDM))) .AND. &
                ((YBASE(JX)+DBLE((NODES(JX)-1)*DYM(IDM))) >= &
                (YRR-DYM(IDM)))) THEN
                    NX(1) = JX
                    NX(2) = JX
                    X(1)  = XCOMP1
                    X(2)  = XCOMP1
                ELSE
                    NX(1) = JX-1
                    NX(2) = JX-1
                    X(1)  = XCOMP1-DXM(IDM)
                    X(2)  = XCOMP1-DXM(IDM)
                END IF

                EXIT

            ELSE                        ! West edge of DEM, continue processing
                NX(1) = JX
                X(1)  = XCOMP1
                CYCLE                    ! Continue to next profile
            END IF

        END IF

    END DO   ! End loop over profiles

!*    Assign 'X'-coordinates for points 3 and 4
    X(4) = X(1)
    X(3) = X(2)

!*--- Loop over the 2 'X' points to locate the 'Y' points
    DO IP = 1,2

        NXNODE = NX(IP)

    !* ---   Set limits for loop on nodes within the profile
        IBEGIN = MAX( 1, IDINT((YRR - YBASE(NXNODE))/DYM(IDM)) )
        IENDS  = NODES(NXNODE)

    !* ---   Adjust loop bounds if IBEGIN > IENDS
        IF (IBEGIN > IENDS) IBEGIN = IENDS

    !* ---   Loop over nodes within profile
        DO IN = IBEGIN, IENDS

            IF (IN == IBEGIN) THEN
            !*             Start with y-coordinate for node just below receptor
                YCOMP1 = YBASE(NXNODE) + (IN-1)*DYM(IDM)
                YCOMP2 = YRR
            ELSE
            !*             Increment y-coordinate
                YCOMP1 = YCOMP1 + DYM(IDM)
            END IF

            IF (YCOMP1 > YCOMP2) THEN
                IF (NY1(IP) <= 0) THEN
                !*                Receptor location is outside range of nodes, use closest
                    IF (YCOMP1-YCOMP2 > DYM(IDM)) THEN
                        IF (IERR > -3) THEN
                            WRITE(DUMMY,'(I8)') IR
                            CALL ERRHDL(PATH,MODNAM,'W','400',DUMMY)
                        END IF
                        IERR = -3
                    ELSE
                        IERR = -1
                    END IF
                    NY1(IP) = IN
                    NY2(IP) = IN
                    YY1 = YCOMP1
                    YY2 = YCOMP1
                    EXIT           ! Exit loop on nodes for this 'X' point
                ELSE
                    NY2(IP) = IN
                    YY2     = YCOMP1
                    EXIT           ! Normal (non-edge) case; Exit loop
                END IF

            ELSE IF (YCOMP1 == YCOMP2) THEN
                NY2(IP) = IN
                NY1(IP) = IN
                YY1 = YCOMP1
                YY2 = YCOMP1
                EXIT              ! Normal (non-edge) case; Exit loop

            ELSE IF (YCOMP1 < YCOMP2) THEN
                NY1(IP) = IN
                YY1 = YCOMP1
                IF (IN == NODES(NXNODE)) THEN
                !*                Receptor location is outside range of profiles, use closest
                    IF (YCOMP2-YCOMP1 > DYM(IDM)) THEN
                        IF (IERR > -3) THEN
                            WRITE(DUMMY,'(I8)') IR
                            CALL ERRHDL(PATH,MODNAM,'W','400',DUMMY)
                        END IF
                        IERR = -3
                    ELSE
                        IERR = -1
                    END IF
                    NY2(IP) = IN
                    YY2 = YCOMP1
                    EXIT           ! Exit loop on nodes for this 'X' point
                END IF
            END IF

        END DO   ! End loop over nodes

        IF (IP == 1) THEN
            Y(1) = YY1
            Y(4) = YY2
        ELSE
            Y(2) = YY1
            Y(3) = YY2
        END IF

    END DO

    IF (RECDBG) THEN
        WRITE(RELVK,89)
        89 FORMAT(11X, 'Based on the Find4 Coding of:'//, &
        &             11X, '  IP= 4----3'/, &
        &             11X, '      |    |'/, &
        &             11X, '  IP= 1----2'/)
        WRITE(RELVK,90)
        90 FORMAT(9X,'IP    xbase      nx       xx     X-Receptor', &
        '       ybase      ny       yy     Y-Receptor')
        write(RELVK,91) xbase(1),     nx(1),  x(1),  XRR, &
        ybase(nx(1)), ny1(1), y(1),  YRR

        write(RELVK,92) xbase(1),     nx(2),  x(2),  XRR, &
        ybase(nx(2)), ny1(2), y(2),  YRR

        write(RELVK,93) xbase(1),     nx(2),  x(2),  XRR, &
        ybase(nx(2)), ny2(2), y(3),  YRR

        write(RELVK,94) xbase(1),     nx(1),  x(1),  XRR, &
        ybase(nx(1)), ny2(1), y(4),  YRR
        91 format(9X,' 1', F12.2, I5, 2F12.2, 3X,  F12.2, I5, 2F12.2)
        92 format(9X,' 2', F12.2, I5, 2F12.2, 3X,  F12.2, I5, 2F12.2)
        93 format(9X,' 3', F12.2, I5, 2F12.2, 3X,  F12.2, I5, 2F12.2)
        94 format(9X,' 4', F12.2, I5, 2F12.2, 3X,  F12.2, I5, 2F12.2)
        WRITE(RELVK,*) ''
    END IF


    999 RETURN
    end subroutine

    SUBROUTINE INT2D(PARAMS,X1,Y1,XVALUE,YVALUE,VALUE)
!***********************************************************************
!*    PURPOSE: LINEARLY INTERPOLATES FOUR VALUES AT FOUR POINTS
!*    ON A PLANE TO GET A VALUE AT ONE POINT
!*
!*    PROGRAMMER: Jayant Hardikar, Roger Brode
!*
!*    DATE:    September 29, 1995
!*
!*    INPUTS:  KNOWN VALUES AT THE FOUR VERTICES OF A RECTANGLE
!*             X COORDINATE LOCATION OF THE FOUR POINTS
!*             Y COORDINATE LOCATION OF THE FOUR POINTS
!*             X COORDINATE AT WHERE THE VALUE IS DESIRED
!*             Y COORDINATE AT WHERE THE VALUE IS DESIRED
!*
!*       Points are ordered as follows:
!*
!*                IP= 4----3
!*                    |    |
!*                IP= 1----2
!*
!*
!*    OUTPUTS: VALUE AT THE POINT DESIRED
!*
!*    CALLED FROM:   MAIN
!***********************************************************************

    real (kind=8) :: PARAMS(4)
    real (kind=8) :: VALUE
    real (kind=8) :: XVALUE,XVALUM,YVALUE,YVALUM,X2MX,Y2MY, &
    XMX1,YMY1,XBLKI,X1(4),Y1(4), PINT1, PINT2, &
    YBLKI
!*
    XVALUM = XVALUE
    IF (XVALUM > X1(2)) THEN
        XVALUM = X1(2)
    ELSE IF (XVALUM < X1(1)) THEN
        XVALUM = X1(1)
    END IF

    YVALUM = YVALUE
    IF (YVALUM > Y1(3)) THEN
        YVALUM = Y1(3)
    ELSE IF(YVALUM < Y1(1)) THEN
        YVALUM = Y1(1)
    END IF
!*
    IF (X1(2) - X1(1) <= 0.0D0) THEN
        X2MX = 1.0D0
        XMX1 = 0.0D0
    ELSE
        XBLKI = 1.0D0/(X1(2) - X1(1))
        X2MX  = (X1(2) - XVALUM)*XBLKI
        XMX1  = (XVALUM - X1(1))*XBLKI
    END IF
!*
    PINT1 = PARAMS(1)*X2MX + PARAMS(2)*XMX1
    PINT2 = PARAMS(4)*X2MX + PARAMS(3)*XMX1
!*
    IF (Y1(4) - Y1(1) <= 0.0D0) THEN
        Y2MY = 1.0D0
        YMY1 = 0.0D0
    ELSE
        YBLKI = 1.0D0/(Y1(4) - Y1(1))
        Y2MY  = (Y1(4) - YVALUM)*YBLKI
        YMY1  = (YVALUM - Y1(1))*YBLKI
    END IF
!*
    VALUE = PINT1*Y2MY + PINT2*YMY1
!*
    RETURN
    end subroutine
