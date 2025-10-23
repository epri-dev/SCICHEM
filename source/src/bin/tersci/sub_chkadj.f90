    SUBROUTINE CHKADJ
!***********************************************************************
!*                CHKADJ Module of AERMAP Terrain Preprocessor
!*
!*       PURPOSE: Check to See if the DEM Files are Contiguous.  They
!*                Should Have at Least 1 Common Border With Another DEM File
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
!*       MODIFIED BY: Roger W. Brode, U.S. EPA, OAQPS, AQMG
!*
!*                Modified adjacency checks to handle Alaska DEMs,
!*                "mixed" DEMs and NED data.  Modified for use of
!*                standard convention of negative for West longitude.
!*                Included adjustments to adjacency checks to account
!*                for domains that cross the 180E/180W meridian.
!*                Also included code to allow tracking input elevation
!*                files to ensure that higher resolution data are
!*                entered first.
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
!*       MODIFIED:   To correct problem with multiple DEM files.
!*                   R.W. Brode, PES, Inc. - 2/27/96
!*
!*       INPUTS:  Coordinates of the Corners of DEM Files
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
    CHARACTER(11) :: LVLNT

    LOGICAL, ALLOCATABLE :: ADJCNT(:)

    LOGICAL :: FIRST

    real (kind=8), PARAMETER :: EPSILON = 0.001D0

    real (kind=8) :: DIFLAT, DIFLON, CNRADJ_LON, CNRADJ_LAT
    real (kind=8) :: ALAT, ALON, CLAT, CLON
    real (kind=8) :: BLAT, BLON
    real (kind=8) :: DEMLAT1, DEMLAT2, DEMLON1, DEMLON2
    real (kind=8) :: DYM_MINLAT, DYM_MINUTM
    real (kind=8), ALLOCATABLE :: DELTA_LAT(:), DELTA_LON(:)

    INTEGER :: I, J, K
    INTEGER :: JDEM, NUMADJ
    INTEGER :: INOD, JNOD
    INTEGER :: ILAT, ILON

!*    Allocate arrays to hold latitude/longitude extents of files,
!*    and array for ADJCNT flag
    ALLOCATE (DELTA_LAT(NDEM), DELTA_LON(NDEM))
    ALLOCATE (ADJCNT(NDEM))

!*    Variable Initializations
    MODNAM = 'CHKADJ'

!*    Initialize last MAPNAME array element to indicate No Map Present
    MAPNAME(NUMDEM + 1) = '   No Map Present'

    WRITE(MAPK,*) ' '
    WRITE(MAPK,'(/1x,''From '',A12)') MODNAM

!*    Use DEM file type (LAT or UTM) based on first DEM file
    IF (IPLAN(1) == 0) THEN
        TYPDEM = 'LAT'
        DYM_MINLAT = DYM(1)
    !*       Save "equivalent" horizontal resolution in meters
        IF (DABS(DYM_MINLAT-(1.0D0/9.0D0)) < EPSILON) THEN
            DYM_MINUTM = 3.0D0
        ELSE IF (DABS(DYM_MINLAT-(1.0D0/3.0D0)) < EPSILON) THEN
            DYM_MINUTM = 10.0D0
        ELSE IF (DABS(DYM_MINLAT-1.0D0) < EPSILON) THEN
            DYM_MINUTM = 30.0D0
        ELSE IF (DABS(DYM_MINLAT-2.0D0) < EPSILON) THEN
            DYM_MINUTM = 60.0D0
        ELSE IF (DABS(DYM_MINLAT-3.0D0) < EPSILON) THEN
            DYM_MINUTM = 90.0D0
        ELSE
            DYM_MINUTM = DYM_MINLAT*30.0D0
        END IF

    !     If coordinate system is UTM and first DEM file is an Alaska DEM, this should
    !     be a 7.5-min DEM. Set DEM type (TYPDEM) to "LAT" (even though it is in UTM).
    ELSE IF (IPLAN(1) == 1 .AND. &
        SELAT_ARCS(1) >= 1.8D5) THEN
    !*       First file is AK 7.5-min DEM with UTM coordinates
        TYPDEM = 'LAT'
        DYM_MINUTM = DYM(1)
    !*       Save "equivalent" horizontal resolution in arc-seconds
        IF (DABS(DYM_MINUTM-3.0D0) < EPSILON) THEN
            DYM_MINLAT = 1.0D0/9.0D0
        ELSE IF (DABS(DYM_MINUTM-10.0D0) < EPSILON) THEN
            DYM_MINLAT = 1.0D0/3.0D0
        ELSE IF (DABS(DYM_MINUTM-30.0D0) < EPSILON) THEN
            DYM_MINLAT = 1.0D0
        ELSE IF (DABS(DYM_MINUTM-90.0D0) < EPSILON) THEN
            DYM_MINLAT = 3.0D0
        ELSE
            DYM_MINLAT = DYM_MINUTM/30.0D0
        END IF

    ELSE IF (IPLAN(1) == 1) THEN
        TYPDEM = 'UTM'
        DYM_MINUTM = DYM(1)
    !*       Save "equivalent" horizontal resolution in arc-seconds
        IF (DABS(DYM_MINUTM-3.0D0) < EPSILON) THEN
            DYM_MINLAT = 1.0D0/9.0D0
        ELSE IF (DABS(DYM_MINUTM-10.0D0) < EPSILON) THEN
            DYM_MINLAT = 1.0D0/3.0D0
        ELSE IF (DABS(DYM_MINUTM-30.0D0) < EPSILON) THEN
            DYM_MINLAT = 1.0D0
        ELSE IF (DABS(DYM_MINUTM-90.0D0) < EPSILON) THEN
            DYM_MINLAT = 3.0D0
        ELSE
            DYM_MINLAT = DYM_MINUTM/30.0D0
        END IF

    ELSE
    !*       Unkown file type, assume LAT
        TYPDEM = 'LAT'
        DYM_MINLAT = DYM(1)
    !*       Save "equivalent" horizontal resolution in meters
        IF (DABS(DYM_MINLAT-(1.0D0/9.0D0)) < EPSILON) THEN
            DYM_MINUTM = 3.0D0
        ELSE IF (DABS(DYM_MINLAT-(1.0D0/3.0D0)) < EPSILON) THEN
            DYM_MINUTM = 10.0D0
        ELSE IF (DABS(DYM_MINLAT-1.0D0) < EPSILON) THEN
            DYM_MINUTM = 30.0D0
        ELSE IF (DABS(DYM_MINLAT-2.0D0) < EPSILON) THEN
            DYM_MINUTM = 60.0D0
        ELSE IF (DABS(DYM_MINLAT-3.0D0) < EPSILON) THEN
            DYM_MINUTM = 90.0D0
        ELSE
            DYM_MINUTM = DYM_MINLAT*30.0D0
        END IF
    END IF

    DO IDEM = 1, NUMDEM
        ADJCNT(IDEM) = .FALSE.
    END DO


!*    Read All the Coordinates.  The WGS 72 header is reported not to have all
!     of its header data filled in.
    DO IDEM = 1, NUMDEM

    !----    Check for higher resolution files entered after lower res files
    !*       Issue warnings for now; error will be issued if receptor is
    !*       found within a higher res file after being assigned to lower res file
        IF (IDEM > 1) THEN
            IF (IPLAN(IDEM) == 0) THEN
                IF ((DYM(IDEM)-DYM_MINLAT+EPSILON) < 0.0D0) THEN
                    WRITE (DUMMY,'(I8)') IDEM
                    CALL ERRHDL(PATH,MODNAM,'W','325',DUMMY)
                ELSE IF (DYM(IDEM) > DYM_MINLAT) THEN
                    DYM_MINLAT = DYM(IDEM)
                    DYM_MINUTM = DYM_MINLAT*30.0D0
                END IF
            ELSE IF (IPLAN(IDEM) == 1) THEN
                IF ((DYM(IDEM)-DYM_MINUTM+EPSILON) < 0.0D0) THEN
                    WRITE (DUMMY,'(I8)') IDEM
                    CALL ERRHDL(PATH,MODNAM,'W','325',DUMMY)
                ELSE IF (DYM(IDEM) > DYM_MINUTM) THEN
                    DYM_MINUTM = DYM(IDEM)
                    DYM_MINLAT = DYM_MINUTM/30.0D0
                END IF
            END IF
        END IF

    !*       Initialize counter for length of "refined header data segment"
        J = MIN(40, INDEX(MAPN(IDEM),'  '))
        IF (J > 1) THEN
            MAPNAME(IDEM) = MAPN(IDEM)(1:J)
        ELSE
        !*          File header info is blank; use filename without path (FLN) instead
            J = MIN(40, LEN_TRIM(FLN(IDEM)))
            MAPNAME(IDEM) = FLN(IDEM)(1:J)
        END IF

        WRITE(MAPK,5012) TYPDAT, IDEM, MAPN(IDEM), MAPNAME(IDEM)
        5012 FORMAT(/1X,A3,' File #: ',I6, &
        /1X,A40,' <-- Raw header data segment', &
        /1X,A40,' <-- Refined header data segment')


        IF (DABS(DCI(IDEM)-0.1D0) < EPSILON) THEN
            LVLNT = 'deci-' // LVLN(ELUNIT(IDEM))
        ELSE IF (DABS(DCI(IDEM)-10.0D0) < EPSILON) THEN
            LVLNT = 'deca-' // LVLN(ELUNIT(IDEM))
        ELSE
            LVLNT = LVLN(ELUNIT(IDEM))
        END IF

        WRITE(MAPK,5022) CDLVL(DEMLVL(IDEM)), &
        PLAN(IPLAN(IDEM)), &
        DYM(IDEM), CUNITN(CUNIT(IDEM)), &
        DXM(IDEM), CUNITN(CUNIT(IDEM)), &
        IZO(IDEM)

        5022 FORMAT(3X,'DEM Level Code:  ',A6/ &
        &             3X,'Planimetric Ref: ',A25/ &
        &             3X,'  North-South Node separation: ', F8.4,2X,A11/ &
        &             3X,'  East-West Node separation:   ', F8.4,2X,A11/ &
        &             3X,'UTM/State Zone # ',I3)

    !*       Check for datum of 0
        IF (NADD(IDEM) == 0) THEN
            IF (IPLAN(IDEM) == 0) THEN
                IF (DABS((NELAT_DEGS(IDEM)) - &
                (SELAT_DEGS(IDEM)) - 1.0D0) <= &
                &                                                0.001D0) THEN
                !*                Assume 1-Degree DEM, assign NADD(IDEM) = 2
                    NADD(IDEM) = 2
                    WRITE(MAPK,*) ' '
                    WRITE(MAPK,*) '  DEM NADA = 0. DEFAULT HORIZONTAL ', &
                    'DATUM ASSIGNED: ', NADD(IDEM)
                    WRITE(MAPK,*) ' '
                ELSE
                !*                Assume Alaska 7.5-min or 15-min DEM, assign NADD(IDEM) = 1
                    NADD(IDEM) = 1
                    WRITE(MAPK,*) ' '
                    WRITE(MAPK,*) '  DEM NADA = 0. DEFAULT HORIZONTAL ', &
                    'DATUM ASSIGNED: ', NADD(IDEM)
                    WRITE(MAPK,*) ' '
                END IF
            ELSE IF (IPLAN(IDEM) == 1) THEN
            !*             Assume 7.5-Minute DEM, assign NADD(IDEM) = 1
                NADD(IDEM) = 1
                WRITE(MAPK,*) ' '
                WRITE(MAPK,*) '  DEM NADA = 0. DEFAULT HORIZONTAL ', &
                'DATUM ASSIGNED: ', NADD(IDEM)
                WRITE(MAPK,*) ' '
            END IF
        END IF

        WRITE(MAPK,5023) NADN(NADD(IDEM)), NPROF(IDEM)

        5023 FORMAT(3X,'Horizontal Datum: ',A40, &
        /3X,'Num. of Profiles: ',I5)

        IF (TYPDAT == 'DEM') THEN
            WRITE(MAPK,5024) LVLNT, ELEVMN(IDEM), LVLN(ELUNIT(IDEM)), &
            ELEVMX(IDEM), LVLN(ELUNIT(IDEM))

            5024 FORMAT(/3x,'Original elevation units of nodes: ',A11, &
            /3X,'Min. Elevation: ',F8.1, 1X, A6 &
            /3X,'Max. Elevation: ',F8.1, 1X, A6)
        END IF

        IF (TYPDAT == 'DEM' .AND. ELUNIT(IDEM) == 1) THEN
            ELEVMN(IDEM) = ELEVMN(IDEM) * 0.3048D+0
            ELEVMX(IDEM) = ELEVMX(IDEM) * 0.3048D+0
            WRITE(MAPK,5025) ELEVMN(IDEM), ELEVMX(IDEM)
            5025 FORMAT(3X,'Min & Max Elevations converted to meters:', &
            /3X,'Min. Elevation: ',F8.1, ' meters' &
            /3X,'Max. Elevation: ',F8.1, ' meters')
        END IF


        WRITE(MAPK, 5010) SWLAT_DEGS(IDEM), SWLON_DEGS(IDEM)
        5010 FORMAT('   SW Corner Lat/Lon (deg.): ',2F14.5)
        WRITE(MAPK, 5011) SWLAT_ARCS(IDEM), &
        SWLON_ARCS(IDEM)
        5011 FORMAT('   SW Corner Lat/Lon (sec.): ',2F14.3)

    !        Calculate Lat/Lon Domain of DEM File
        DELTA_LAT(IDEM) = NELAT_DEGS(IDEM) - SELAT_DEGS(IDEM)
        DELTA_LON(IDEM) = SELON_DEGS(IDEM) - SWLON_DEGS(IDEM)

        WRITE(MAPK, 5110) DELTA_LAT(IDEM), DELTA_LON(IDEM)
        5110 FORMAT('   Latitude Range = ',F7.4,' Deg.;', &
        '  Longitude Range = ',F7.4,' Deg.')

        WRITE(MAPK,*) '  Writing corner coords; UTM then Dec.Deg ', &
        '(negative for West longitude):'
        J = IDEM
        WRITE(MAPK, 5112) &
        SWE_MTRS(J),SWN_MTRS(J),NWE_MTRS(J),NWN_MTRS(J), &
        NEE_MTRS(J),NEN_MTRS(J),SEE_MTRS(J),SEN_MTRS(J), &
        SWLAT_DEGS(J),SWLON_DEGS(J),NWLAT_DEGS(J),NWLON_DEGS(J), &
        NELAT_DEGS(J),NELON_DEGS(J),SELAT_DEGS(J),SELON_DEGS(J)
        5112 FORMAT(5x,'Southwest Corner',8x,'Northwest Corner',8x, &
        'Northeast Corner',8x,'Southeast Corner'/ &
        &      5X,4('Easting    Northing ',4x)/4X,4(f10.2,f12.2,2x)/ &
        &      5X,4('Latitude   Longitude',4x)/2X,4(F10.4,F12.4,2x)/)

        WRITE(MAPK,*) ' '

    END DO

! --- Assign adjacency array if only one data file is input
    IF (NUMDEM == 1) THEN
        ADJCNT(NUMDEM) = .TRUE.
        DO K = 1, 9
            ADJMAP(1,K) = 2
        END DO
        ADJMAP(1,5) = 1
    END IF

! --- Loop through multiple DEM files to see if they are all adjacent
!     to at least one other file, and create arrays of adjacent DEMs
    IF (NUMDEM > 1) THEN

    ! ---    First check for full adjacency - two corners in common
    !        Apply a stringent test for adjacency of +/- 0.1 arc-second
    !        Also create a 3x3 grid of neighboring "maps" for each file

    !        Begin loop through DEM files
        DO IDEM = 1, NUMDEM
        !           Initialize ADJMAP array for 3x3 grid of adjacent files
        !           3x3 grid array numbered as follow:

        !             7   8   9
        !             4   5   6    current file is located at position 5
        !             1   2   3

            DO K = 1, 9
            !              Assign value of NUMDEM+1 initially for "No Map Present"
                ADJMAP(IDEM,K) = NUMDEM+1
            END DO
        !           Assign center value of ADJMAP array to local DEM
            ADJMAP(IDEM,5) = IDEM

        ! ---       Loop through DEM files for comparison
            DO JDEM = 1, NUMDEM
            !              Initialize counter for number of adjacent files
                NUMADJ = 0
                IF (IDEM == JDEM) CYCLE

                DO INOD = 1, 4
                ! ---             Loop through nodes for IDEM file
                    IF (INOD == 1) THEN
                        DEMLAT1 = SWLAT_ARCS(IDEM)
                        DEMLON1 = SWLON_ARCS(IDEM)
                    ELSE IF (INOD == 2) THEN
                        DEMLAT1 = NWLAT_ARCS(IDEM)
                        DEMLON1 = NWLON_ARCS(IDEM)
                    ELSE IF (INOD == 3) THEN
                        DEMLAT1 = NELAT_ARCS(IDEM)
                        DEMLON1 = NELON_ARCS(IDEM)
                    ELSE IF (INOD == 4) THEN
                        DEMLAT1 = SELAT_ARCS(IDEM)
                        DEMLON1 = SELON_ARCS(IDEM)
                    END IF

                    DO JNOD = 1, 4
                    ! ---                Loop through nodes for JDEM file
                        IF (JNOD == 1) THEN
                            DEMLAT2 = SWLAT_ARCS(JDEM)
                            DEMLON2 = SWLON_ARCS(JDEM)
                        ELSE IF (JNOD == 2) THEN
                            DEMLAT2 = NWLAT_ARCS(JDEM)
                            DEMLON2 = NWLON_ARCS(JDEM)
                        ELSE IF (JNOD == 3) THEN
                            DEMLAT2 = NELAT_ARCS(JDEM)
                            DEMLON2 = NELON_ARCS(JDEM)
                        ELSE IF (JNOD == 4) THEN
                            DEMLAT2 = SELAT_ARCS(JDEM)
                            DEMLON2 = SELON_ARCS(JDEM)
                        END IF

                    ! ---                Calculate differences in lat. and long. for adjacency tests
                        DIFLAT= DEMLAT1 - DEMLAT2
                        DIFLON= DEMLON1 - DEMLON2
                    ! ---                Adjust for files that cross 180E/180W meridian
                        IF (DIFLON > 648000.0D0) THEN         ! DIFLON > 180 deg.
                            DIFLON = 1296000.0D0 - DIFLON         ! Subtract from 360 deg.
                        ELSE IF (DIFLON < -648000.0D0) THEN   ! DIFLON < -180 deg.
                            DIFLON = DIFLON + 1296000.0D0         ! Add 360 deg.
                        END IF

                    ! ---                Check for difference less than 0.1 second for strict adjacency
                    !                    (about 3 meter difference)
                        IF (DABS(DIFLAT) < 1.0D-1 .AND. &
                        DABS(DIFLON) < 1.0D-1) THEN
                            NUMADJ = NUMADJ + 1
                        END IF

                    ! ---                Check for difference less than 45 seconds (about 10% of 7.5-min
                    !                    quadrangle) for determination of 3x3 grid of neighboring files
                    !                    Files must share a common corner, with +/- 45 second tolerance
                        IF (DABS(DIFLAT) < 45.0D0 .AND. &
                        DABS(DIFLON) < 45.0D0) THEN

                        ! ---                   Determine relative location of file within 3x3 grid based
                        !                       on node locations; use first file that satisfies adjacency
                        !                       criterion, unless later file with same longitude extent
                        !                       also satisfies criterion.
                            IF (INOD == 1) THEN
                                IF (JNOD == 2) THEN
                                    IF (ADJMAP(IDEM,2) == NUMDEM+1) THEN
                                        ADJMAP(IDEM,2) = JDEM
                                    ELSE IF (DABS(DELTA_LON(IDEM)- &
                                        DELTA_LON(JDEM)) < &
                                        EPSILON) THEN
                                        ADJMAP(IDEM,2) = JDEM
                                    END IF
                                ELSE IF (JNOD == 3) THEN
                                    IF (ADJMAP(IDEM,1) == NUMDEM+1) THEN
                                        ADJMAP(IDEM,1) = JDEM
                                    ELSE IF (DABS(DELTA_LON(IDEM)- &
                                        DELTA_LON(JDEM)) < &
                                        EPSILON) THEN
                                        ADJMAP(IDEM,1) = JDEM
                                    END IF
                                ELSE IF (JNOD == 4) THEN
                                    IF (ADJMAP(IDEM,4) == NUMDEM+1) THEN
                                        ADJMAP(IDEM,4) = JDEM
                                    ELSE IF (DABS(DELTA_LON(IDEM)- &
                                        DELTA_LON(JDEM)) < &
                                        EPSILON) THEN
                                        ADJMAP(IDEM,4) = JDEM
                                    END IF
                                END IF

                            ELSE IF (INOD == 2) THEN
                                IF (JNOD == 1) THEN
                                    IF (ADJMAP(IDEM,8) == NUMDEM+1) THEN
                                        ADJMAP(IDEM,8) = JDEM
                                    ELSE IF (DABS(DELTA_LON(IDEM)- &
                                        DELTA_LON(JDEM)) < &
                                        EPSILON) THEN
                                        ADJMAP(IDEM,8) = JDEM
                                    END IF
                                ELSE IF (JNOD == 3) THEN
                                    IF (ADJMAP(IDEM,4) == NUMDEM+1) THEN
                                        ADJMAP(IDEM,4) = JDEM
                                    ELSE IF (DABS(DELTA_LON(IDEM)- &
                                        DELTA_LON(JDEM)) < &
                                        EPSILON) THEN
                                        ADJMAP(IDEM,4) = JDEM
                                    END IF
                                ELSE IF (JNOD == 4) THEN
                                    IF (ADJMAP(IDEM,7) == NUMDEM+1) THEN
                                        ADJMAP(IDEM,7) = JDEM
                                    ELSE IF (DABS(DELTA_LON(IDEM)- &
                                        DELTA_LON(JDEM)) < &
                                        EPSILON) THEN
                                        ADJMAP(IDEM,7) = JDEM
                                    END IF
                                END IF

                            ELSE IF (INOD == 3) THEN
                                IF (JNOD == 1) THEN
                                    IF (ADJMAP(IDEM,9) == NUMDEM+1) THEN
                                        ADJMAP(IDEM,9) = JDEM
                                    ELSE IF (DABS(DELTA_LON(IDEM)- &
                                        DELTA_LON(JDEM)) < &
                                        EPSILON) THEN
                                        ADJMAP(IDEM,9) = JDEM
                                    END IF
                                ELSE IF (JNOD == 2) THEN
                                    IF (ADJMAP(IDEM,6) == NUMDEM+1) THEN
                                        ADJMAP(IDEM,6) = JDEM
                                    ELSE IF (DABS(DELTA_LON(IDEM)- &
                                        DELTA_LON(JDEM)) < &
                                        EPSILON) THEN
                                        ADJMAP(IDEM,6) = JDEM
                                    END IF
                                ELSE IF (JNOD == 4) THEN
                                    IF (ADJMAP(IDEM,8) == NUMDEM+1) THEN
                                        ADJMAP(IDEM,8) = JDEM
                                    ELSE IF (DABS(DELTA_LON(IDEM)- &
                                        DELTA_LON(JDEM)) < &
                                        EPSILON) THEN
                                        ADJMAP(IDEM,8) = JDEM
                                    END IF
                                END IF

                            ELSE IF (INOD == 4) THEN
                                IF (JNOD == 1) THEN
                                    IF (ADJMAP(IDEM,6) == NUMDEM+1) THEN
                                        ADJMAP(IDEM,6) = JDEM
                                    ELSE IF (DABS(DELTA_LON(IDEM)- &
                                        DELTA_LON(JDEM)) < &
                                        EPSILON) THEN
                                        ADJMAP(IDEM,6) = JDEM
                                    END IF
                                ELSE IF (JNOD == 2) THEN
                                    IF (ADJMAP(IDEM,3) == NUMDEM+1) THEN
                                        ADJMAP(IDEM,3) = JDEM
                                    ELSE IF (DABS(DELTA_LON(IDEM)- &
                                        DELTA_LON(JDEM)) < &
                                        EPSILON) THEN
                                        ADJMAP(IDEM,3) = JDEM
                                    END IF
                                ELSE IF (JNOD == 3) THEN
                                    IF (ADJMAP(IDEM,2) == NUMDEM+1) THEN
                                        ADJMAP(IDEM,2) = JDEM
                                    ELSE IF (DABS(DELTA_LON(IDEM)- &
                                        DELTA_LON(JDEM)) < &
                                        EPSILON) THEN
                                        ADJMAP(IDEM,2) = JDEM
                                    END IF
                                END IF
                            END IF

                        END IF

                    END DO   ! JNOD loop
                END DO      ! INOD loop

                IF (NUMADJ == 2) THEN
                ! ---             Two corners match, files are adjacent (share a side)
                    ADJCNT(IDEM) = .TRUE.
                END IF

            END DO         ! JDEM loop
        END DO            ! IDEM loop

    ! ---    Write out 3x3 grids to MAPPARAMS.OUT file
        write(MAPK,256)
        do idem = 1, numdem
            WRITE(MAPK,259) IDEM, MAPNAME(IDEM)(1:26), &
            (MAPNAME(ADJMAP(IDEM,K))(1:26), K = 7, 9), &
            (MAPNAME(ADJMAP(IDEM,K))(1:26), K = 4, 6), &
            (MAPNAME(ADJMAP(IDEM,K))(1:26), K = 1, 3)
        end do

        256 Format(//'MAP ADJACENCY (Based on +/- 45 arc-seconds)'// &
        ' No. Base Map Name', 15x, &
        'Adjacent map names and relative geographic', &
        ' location to Base Map.', &
        /33x,'NOTE: Adjacency for NED files, Mixed DEMs, ', &
        'non-standard DEMs, ', &
        /33x,'      and AK DEMs may not be exact.'/)
        259 FORMAT(I3,2X,A26,'  NW: ',A26,'   N: ',A26,'  NE: ',A26/ &
        &                31X,'   W: ',A26,'   C: ',A26,'   E: ',A26/ &
        &                31X,'  SW: ',A26,'   S: ',A26,'  SE: ',A26/)


    ! ---    Assign variables to determine full latitude/longitude
    !        range covered by DEM/NED data
        ALAT = MAXVAL(SELAT_DEGS)
        ALON = MAXVAL(SELON_DEGS)
        BLAT = MINVAL(SELAT_DEGS)
        BLON = MINVAL(SELON_DEGS)

    ! ---    Create Mapname array (MAPARRAY) - visually check for
    !        missing and errant files
        WRITE(MAPK,257)
        257 FORMAT(//'MAPNAME ARRAY (Based on +/- 0.03 degrees)'// &
        '  Mapnames are laid out in relative geographical positions.' &
        /'  "Grid" resolution is based on Lat/Lon extent of first file.', &
        /'  Relative positions are based on SE corner coordinates ', &
        'relative to full grid of maps.', &
        /'  Look for "No Map Present" within the field of interest.' &
        /'  This may indicate missing files or files with', &
        ' errant data in their header records.',/ &
        '  NOTE: Adjacency for NED files, Mixed DEMs, non-standard DEMs,', &
        ' and AK DEMs may not be exact.',/ &
        '        Higher resolution maps may overwrite lower resolution', &
        ' maps that overlap.'/)

    ! ---    Initialize ILAT and ILON counters for number of "grid" cells in
    !        Latitude and Longitude
        ILAT = 0
        ILON = 0

    ! ---    Loop through DEM files to determine size of "DEM grid" in
    !        Lat/Lon increments
        DO IDEM = 1, NUMDEM

            CNRADJ_LAT = DABS(SELAT_DEGS(IDEM) - NELAT_DEGS(IDEM))
            CNRADJ_LON = DABS(SELON_DEGS(IDEM) - SWLON_DEGS(IDEM))

            ILAT = MAX(ILAT,IDNINT(DABS(ALAT-BLAT)/(CNRADJ_LAT)))
            ILON = MAX(ILON,IDNINT(DABS(ALON-BLON)/(CNRADJ_LON)))
        ! ---       Adjust for files that cross the 180E/180W meridian
            IF (ILON > 180) THEN
                ILON = 360 - ILON
            END IF

        END DO

    ! ---    Add 1 to account for "grid" boundary
        ILAT = ILAT + 1
        ILON = ILON + 1

    ! ---    Allocate MAPARRAY based on size of "DEM grid", and initialize
        ALLOCATE (MAPARRAY(ILAT,ILON))

    ! ---    Initialize array to NUMDEM+1 to indicate "No Map"
        MAPARRAY = NUMDEM+1

    ! ---    Loop through the Lat/Lon "Grid" to assign DEM files to proper
    !        "grid cells" for output array.

    ! ---    Use latitude and longitude range of 1st DEM file to calculate
    !        relative locations
        CNRADJ_LAT = DABS(SELAT_DEGS(1) - NELAT_DEGS(1))
        CNRADJ_LON = DABS(SELON_DEGS(1) - SWLON_DEGS(1))

    ! ---    Adjust BLON for files that cross the 180E/180W meridian
        IF ( (ALON-BLON) > 180.0D0) THEN
            BLON = -1.0D0*ALON
        END IF

        DO I = 1, ILAT
            DO J = 1, ILON

                DO K = 1, NUMDEM

                ! ---             Calculate latitude and longitude of current file
                !                 relative to southwest corner of data domain
                    CLAT = BLAT + DBLE(I - 1) * CNRADJ_LAT
                    CLON = BLON + DBLE(J - 1) * CNRADJ_LON

                ! ---             Apply even less stringent test here than for 3X3 array,
                !                 use +/- 0.03 degrees, or about 3,000 meters.
                    IF (DABS(SELAT_DEGS(K) - CLAT) < 0.03D0) THEN
                        DIFLON = SELON_DEGS(K) - CLON
                    ! ---               Adjust for 180E/180W crossover if necessary
                        IF (DIFLON > 180.0D0) THEN
                            DIFLON = DIFLON - 360.0D0
                        ELSE IF (DIFLON < -180.0D0) THEN
                            DIFLON = DIFLON + 360.0D0
                        END IF
                        IF (DABS(DIFLON) < 0.03D0) THEN
                        ! ---                 DEM found for this map array element,
                        !                     set flag and exit the DEM loop
                            MAPARRAY(I,J) = K
                            EXIT
                        END IF
                    END IF

                END DO    ! DEM loop

            END DO       ! ILON loop
        END DO          ! ILAT loop

    ! ---    Write out full map array to MAPPARAMS.OUT file
        DO I = ILAT, 1, -1
            WRITE(MAPK, 261) (MAPNAME(MAPARRAY(I,J))(1:26), J = 1, ILON)
            261 FORMAT(12(1X,A26,1X,:))
        END DO

        WRITE(MAPK,*) ' '

        FIRST = .TRUE.
        DO IDEM = 1, NUMDEM
            IF ( .NOT. ADJCNT(IDEM)) THEN
                IF (FIRST) THEN
                    WRITE(MAPK,*) ' '
                    WRITE(MAPK,*) 'THE FOLLOWING DATA FILES ARE ', &
                    'NOT ADJACENT TO ANY OTHER FILE:'
                    WRITE(MAPK,*) '  (WITHIN +/- 0.1 ARC-SECOND)'
                    WRITE(MAPK,*) '   FILE NO.   LOCATION'
                    WRITE(MAPK,*) '  ---------   ------------------------'
                    FIRST = .FALSE.
                END IF
                WRITE(MAPK,'(4X,I8,3X,A40)') IDEM, MAPNAME(IDEM)(1:40)
                WRITE (DUMMY,'(I8)') IDEM
                CALL ERRHDL(PATH,MODNAM,'W','340',DUMMY)
            END IF
        END DO

    END IF


    999 CONTINUE

    WRITE(iounit,*) 'Exiting CHKADJ'
    WRITE(*,*) 'Exiting CHKADJ'

    RETURN

    END SUBROUTINE
