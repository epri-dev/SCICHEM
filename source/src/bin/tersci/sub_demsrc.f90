    SUBROUTINE DEMSRC
!***********************************************************************
!*               DEMSRC Module of AERMAP Terrain Preprocessor
!*
!*       PURPOSE: Determine which source falls into which DEM file
!*
!*
!*       PROGRAMMER: Peter Eckhoff
!*
!*       DATE:    January 8, 2001
!*
!*       Revision History:
!*
!*       MODIFIED: April 13, 2011
!*
!*       MODIFIED BY: Roger W. Brode, U.S. EPA, OAQPS, AQMG
!*
!*                Corrected write and format statements for sources not
!*                assigned to a terrain file to avoid runtime errors
!*                when such a condition occurs.
!*
!*       MODIFIED: February 9, 2009
!*
!*       MODIFIED BY: Roger W. Brode, U.S. EPA, OAQPS, AQMG
!*
!*                Modified to use source UTM's and file corner UTM's to
!*                check for source within file for NED data in UTM format.
!*                Incorporated checks for source being assigned to lower
!*                resolution DEM file before encountering a higher res
!*                file.  Modified "gap" check to allow for non-NAD-shift
!*                related gaps.  Modified for use of standard convention of
!*                negative for West longitude.
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

    INTEGER, ALLOCATABLE :: NSRCD(:)

    INTEGER :: ISRC, JDEM, IZONSRC, ISPHERE

    real (kind=8), PARAMETER :: EPSILON = 0.001D0
    real (kind=8) :: SLONSHFT, SLATSHFT
    real (kind=8) :: XSSMSHFT, YSSMSHFT
    real (kind=8) :: ARGE, ARGN, XARG, YARG
    real (kind=8) :: NADFLG

    LOGICAL :: GAPSRC

    ALLOCATE (NSRCD(NSRC))

    MODNAM = 'DEMSRC'

!*    Initialize logical variable for identifying "gap" sources
    GAPSRC = .FALSE.
    JDMS   = 0
    NSRCD  = 0

    IF (SRCDBG) THEN
    !*       Open debug output file
        OPEN(SRCD, FILE = SRCNDEM_FILE, STATUS = 'REPLACE')
    !*       Write Version Date Header to SRCNDEM Debug File
        WRITE(SRCD,9011) trim(VERSN), RUNDAT, RUNTIM
        9011 FORMAT('TERSCI Version ',A,1x,A8,1x,A8/)
    END IF

! --- Loop through sources to identify DEM (or NED) files that contain
!     the source
    SRCLOOP:DO ISRC = 1, NUMSRC

    !*       First loop through DEM files with no distance tolerance
    !*       relative to the edges of DEM files

        DEMLOOP:DO IDEM = 1, NUMDEM

        !*          Determine whether to apply NAD shift
            IF (NADA == 0 .OR. NADA == NADD(IDEM)) THEN

                NADFLG = 0.0D0                ! No NAD shift needed

            ELSE IF( (NADA == 1    .OR.  NADA >= 5) .AND. &
                (NADD(IDEM) >= 2 .AND. NADD(IDEM) <= 4) )THEN

                NADFLG = 1.0D0                ! Include NAD shift

            ELSE IF( (NADA >= 2    .AND. NADA <= 4) .AND. &
                (NADD(IDEM) == 1 .OR.  NADD(IDEM) >= 5) )THEN

                NADFLG = 1.0D0                ! Include NAD shift

            ELSE

                NADFLG = 0.0D0                ! No NAD shift needed

            END IF

        !*          Apply NAD shift if necessary; direction of shift is included in XSDIFS/YSDIFS
            SLONSHFT = SLON(ISRC) + (XSDIFS(ISRC)*NADFLG)
            SLATSHFT = SLAT(ISRC) + (YSDIFS(ISRC)*NADFLG)

            XSSMSHFT = XSRCU(ISRC) + (XSDIFM(ISRC)*NADFLG)
            YSSMSHFT = YSRCU(ISRC) + (YSDIFM(ISRC)*NADFLG)

            IF (IZONS(ISRC) /= IZOND(IDEM)) THEN
            !*             Source zone doesn't match DEM zone;
            !*             Convert source Lat/Lon to UTMs, based on DEM file NADD and DEM Zone

                SELECT CASE (NADD(IDEM))   ! Use datum for DEM file
                CASE (0)
                IF (IPLAN(IDEM) == 0) THEN
                    ISPHERE = 4  ! UTM/LAT-LONG conv. based on GRS80 ellipsoid
                ELSE IF (IPLAN(IDEM) == 1) THEN
                    ISPHERE = 0  ! UTM/LL conv. based on Clarke 1866 ellipsoid
                END IF
                CASE (2:4)
                ISPHERE = 4     ! UTM/LAT-LONG conv. based on GRS80 ellipsoid
                CASE (1,5:6)
                ISPHERE = 0     ! UTM/LL conv. based on Clarke 1866 ellipsoid
                CASE DEFAULT
                ISPHERE = 4     ! DEFAULT CASE shouldn't occur
                END SELECT

                ARGE = SLONSHFT
                ARGN = SLATSHFT
                XARG = 0.0D0
                YARG = 0.0D0
            !*             Pass DEM file zone (IZOND(IDEM)) to UTMGEO to get new source
            !*             coordinates (XARG, YARG) referenced to IZOND(IDEM)).
                CALL UTMGEO (555,IZOND(IDEM),IZONSRC,XARG,YARG, &
                ARGE,ARGN,ISPHERE)
                XSSMSHFT = XARG
                YSSMSHFT = YARG
            END IF

            IF (SRCDBG) THEN
                IF (IDEM == 1) THEN
                    WRITE(SRCD,730) TYPDAT, ISRC
                    730 FORMAT('LOOKING AT ',A3,'s FOR SRC: ',I7)
                    WRITE(SRCD,731) ISRC, SLAT(ISRC)/3600.0D0, &
                    SLON(ISRC)/3600.0D0, &
                    YSDIFS(ISRC), XSDIFS(ISRC)
                    731 FORMAT('SRC#:',I7,'    SLAT/LON:', 2F20.8, &
                    /13X,'SHIFTL/L(S):', 2F20.8)
                    WRITE(SRCD,73) XSRCU(ISRC), YSRCU(ISRC), IZONS(ISRC), &
                    XSDIFM(ISRC), YSDIFM(ISRC), &
                    NADA
                    73 FORMAT(12X,'UTM(E/N/Zon):', 2F20.3,1X,I4/ &
                    &                   12X,' SHIFTX/Y(M):', 2F20.8,3X,'SRC NAD:',I3)
                ELSE IF (IDEM > 1 .AND. NSRCD(ISRC) == 0) THEN
                    WRITE(SRCD,732) TYPDAT, ISRC
                    732 FORMAT('STILL LOOKING AT ',A3,'s FOR SRC: ',I7)
                END IF

                IF (NSRCD(ISRC) == 0) THEN
                ! ---           File containing source has not been located yet
                    IF (IPLAN(IDEM) == 1 .AND. &
                    (DABS(NWE_MTRS(IDEM)-SWE_MTRS(IDEM)) < &
                    &                                             0.05D0*DXM(IDEM)) .AND. &
                    (DABS(NEE_MTRS(IDEM)-SEE_MTRS(IDEM)) < &
                    &                                             0.05D0*DXM(IDEM)))THEN
                    ! ---             Current file boundaries follow UTM coordinates,
                    !                 print corner coordinates in degrees and UTM meters
                        WRITE(SRCD,*)
                        WRITE(SRCD,72) TYPDAT, IDEM, TYPDAT, MAPNAME(IDEM), &
                        TYPDAT, NADD(IDEM)
                        WRITE(SRCD,74) TYPDAT, NELAT_DEGS(IDEM), &
                        NELON_DEGS(IDEM)
                        WRITE(SRCD,75) SLATSHFT/3600.0D0, SLONSHFT/3600.0D0
                        WRITE(SRCD,76) TYPDAT, SWLAT_DEGS(IDEM), &
                        SWLON_DEGS(IDEM)
                        WRITE(SRCD,*)
                        WRITE(SRCD,77) TYPDAT, NEE_MTRS(IDEM), &
                        NEN_MTRS(IDEM)
                        WRITE(SRCD,78) XSSMSHFT, YSSMSHFT
                        WRITE(SRCD,79) TYPDAT, SWE_MTRS(IDEM), &
                        SWN_MTRS(IDEM)
                        72 FORMAT(3X,A3,' No.:',I5,1X,A3,' Name: ',A40, &
                        &                        1X,A3,' NAD:', I3)
                        74 FORMAT(8X,A3,' Max. LAT/LON(D.D):', 2F15.5)
                        75 FORMAT(8X,'     Src LAT/LON(D.D):', 2F15.5)
                        76 FORMAT(8X,A3,' Min. LAT/LON(D.D):', 2F15.5)
                        77 FORMAT(8X,A3,' Max. UTME/UTMN(M):', 2F15.3)
                        78 FORMAT(8X,'     Src UTME/UTMN(M):', 2F15.3)
                        79 FORMAT(8X,A3,' Min. UTME/UTMN(M):', 2F15.3/)
                    ELSE
                    ! ---             Current file boundaries follow geographic coordinates,
                    !                 print corner coordinates in degrees and arc-seconds
                        WRITE(SRCD,*)
                        WRITE(SRCD,72) TYPDAT, IDEM, TYPDAT, MAPNAME(IDEM), &
                        TYPDAT, NADD(IDEM)
                        WRITE(SRCD,74) TYPDAT, NELAT_DEGS(IDEM), &
                        NELON_DEGS(IDEM)
                        WRITE(SRCD,75) SLATSHFT/3600.0D0, SLONSHFT/3600.0D0
                        WRITE(SRCD,76) TYPDAT, SWLAT_DEGS(IDEM), &
                        SWLON_DEGS(IDEM)
                        WRITE(SRCD,*)
                        WRITE(SRCD,87) TYPDAT, NELAT_ARCS(IDEM), &
                        NELON_ARCS(IDEM)
                        WRITE(SRCD,88) SLATSHFT, SLONSHFT
                        WRITE(SRCD,89) TYPDAT, SWLAT_ARCS(IDEM), &
                        SWLON_ARCS(IDEM)
                        87 FORMAT(8X,A3,' Max. LAT/LON(Sec):', 2F15.5)
                        88 FORMAT(8X,'     Src LAT/LON(Sec):', 2F15.5)
                        89 FORMAT(8X,A3,' Min. LAT/LON(Sec):', 2F15.5/)
                    END IF
                END IF
            END IF

        ! ---       Determine Which Terrain File(s) a source falls within
        !           If file boundaries follow UTM coordinates, compare source
        !           UTM to corner UTM's; otherwise compare source Lat/Lon
        !           to corner Lat/Lon
            IF((IPLAN(IDEM) == 1 .AND. &
            DABS(NWE_MTRS(IDEM)-SWE_MTRS(IDEM)) < &
            &                                              0.05D0*DXM(IDEM) .AND. &
            DABS(NEE_MTRS(IDEM)-SEE_MTRS(IDEM)) < &
            &                                              0.05D0*DXM(IDEM) .AND. &
            (XSSMSHFT >= SWE_MTRS(IDEM) .AND. &
            XSSMSHFT <= NEE_MTRS(IDEM) .AND. &
            YSSMSHFT >= SWN_MTRS(IDEM) .AND. &
            YSSMSHFT <= NEN_MTRS(IDEM))) &
             .OR. &
            (SLONSHFT >= SWLON_ARCS(IDEM) .AND. &
            SLONSHFT <= NELON_ARCS(IDEM) .AND. &
            SLATSHFT >= SWLAT_ARCS(IDEM) .AND. &
            SLATSHFT <= NELAT_ARCS(IDEM))) THEN

                IF (JDMS(ISRC) == 0) THEN
                !*                This is the first file that contains the source;
                !*                assign file index to JDMS and ISIN arrays.
                    JDMS(ISRC) = IDEM

                !*                Set number of files containing source to 1
                    NSRCD(ISRC) = 1

                !*                Set ISIN array element for this source and
                !*                DEM file to 1
                    ISIN(ISRC,IDEM) = 1

                    IF (SRCDBG) THEN
                    !*                   Write message to SRCNDEM debug file
                        WRITE(SRCD,80) TYPDAT, IDEM, ISRC
                        80 FORMAT(A3,' No.:',I5,' Contains Src No.:', &
                        I7/)
                    END IF

                ELSE
                !*                Source falls inside multiple DEM files
                    NSRCD(ISRC) = NSRCD(ISRC) + 1

                !*                Set ISIN array element for this source and
                !*                DEM file to 1
                    ISIN(ISRC,IDEM) = 1

                ! ---             Check for lower resolution file preceding higher resolution file
                !                 Use latitude spacing (DYM) for comparisons since this doesn't
                !                 change for higher latitudes
                    IF (IPLAN(JDMS(ISRC)) == 0 .AND. &
                    IPLAN(IDEM) == 0) THEN
                    ! ---                Source DEM and current DEM both geograhpic
                        IF (DYM(JDMS(ISRC)) > DYM(IDEM)+EPSILON) THEN

                            CALL ERRHDL(PATH,MODNAM,'E','327',SRCID(ISRC))
                        END IF
                    ELSE IF (IPLAN(JDMS(ISRC)) == 1 .AND. &
                        IPLAN(IDEM) == 1) THEN
                    ! ---                Source DEM and current DEM both UTM
                        IF (DYM(JDMS(ISRC)) > DYM(IDEM)+EPSILON) THEN

                            CALL ERRHDL(PATH,MODNAM,'E','327',SRCID(ISRC))
                        END IF
                    ELSE IF (IPLAN(JDMS(ISRC)) == 0) THEN
                    ! ---                Source DEM geographic but current DEM is UTM;
                    !                    adjust spacing units
                        IF (DABS(DYM(JDMS(ISRC))-(1.0D0/9.0D0)) < &
                        EPSILON) THEN
                            IF (3.0D0 > DYM(IDEM)+EPSILON) THEN
                                CALL ERRHDL(PATH,MODNAM,'E','327', &
                                SRCID(ISRC))
                            END IF
                        ELSE IF (DABS(DYM(JDMS(ISRC))-(1.0D0/3.0D0)) < &
                            EPSILON) THEN
                            IF (10.0D0 > DYM(IDEM)+EPSILON) THEN
                                CALL ERRHDL(PATH,MODNAM,'E','327', &
                                SRCID(ISRC))
                            END IF
                        ELSE IF (DABS(DYM(JDMS(ISRC))-1.0D0) < &
                            EPSILON) THEN
                            IF (30.0D0 > DYM(IDEM)+EPSILON) THEN
                                CALL ERRHDL(PATH,MODNAM,'E','327', &
                                SRCID(ISRC))
                            END IF
                        ELSE IF (DABS(DYM(JDMS(ISRC))-2.0D0) < &
                            EPSILON) THEN
                            IF (60.0D0 > DYM(IDEM)+EPSILON) THEN
                                CALL ERRHDL(PATH,MODNAM,'E','327', &
                                SRCID(ISRC))
                            END IF
                        ELSE IF (DABS(DYM(JDMS(ISRC))-3.0D0) < &
                            EPSILON) THEN
                            IF (90.0D0 > DYM(IDEM)+EPSILON) THEN
                                CALL ERRHDL(PATH,MODNAM,'E','327', &
                                SRCID(ISRC))
                            END IF
                        END IF
                    ELSE IF (IPLAN(JDMS(ISRC)) == 1) THEN
                    ! ---                Source DEM is UTM but current DEM is geographic;
                    !                    adjust spacing units
                        IF (DABS(DYM(JDMS(ISRC))-3.0D0) < &
                        EPSILON) THEN
                            IF ((1.0D0/9.0D0) > DYM(IDEM)+EPSILON) THEN
                                CALL ERRHDL(PATH,MODNAM,'E','327', &
                                SRCID(ISRC))
                            END IF
                        ELSE IF (DABS(DYM(JDMS(ISRC))-10.0D0) < &
                            EPSILON) THEN
                            IF ((1.0D0/3.0D0) > DYM(IDEM)+EPSILON) THEN
                                CALL ERRHDL(PATH,MODNAM,'E','327', &
                                SRCID(ISRC))
                            END IF
                        ELSE IF (DABS(DYM(JDMS(ISRC))-30.0D0) < &
                            EPSILON) THEN
                            IF (1.0D0 > DYM(IDEM)+EPSILON) THEN
                                CALL ERRHDL(PATH,MODNAM,'E','327', &
                                SRCID(ISRC))
                            END IF
                        ELSE IF (DABS(DYM(JDMS(ISRC))-90.0D0) < &
                            EPSILON) THEN
                            IF (3.0D0 > DYM(IDEM)+EPSILON) THEN
                                CALL ERRHDL(PATH,MODNAM,'E','327', &
                                SRCID(ISRC))
                            END IF
                        END IF
                    END IF

                    IF (SRCDBG) THEN
                        IF (IPLAN(IDEM) == 1 .AND. &
                        (DABS(NWE_MTRS(IDEM)-SWE_MTRS(IDEM)) < &
                        &                                             0.05D0*DXM(IDEM)) .AND. &
                        (DABS(NEE_MTRS(IDEM)-SEE_MTRS(IDEM)) < &
                        &                                             0.05D0*DXM(IDEM)))THEN
                        ! ---                   Current file boundaries follow UTM coordinates,
                        !                       print corner coordinates in degrees and UTM meters
                            WRITE(SRCD,*)
                            WRITE(SRCD,72) TYPDAT, IDEM, TYPDAT, &
                            MAPNAME(IDEM), TYPDAT, NADD(IDEM)
                            WRITE(SRCD,74) TYPDAT, NELAT_DEGS(IDEM), &
                            NELON_DEGS(IDEM)
                            WRITE(SRCD,75) SLATSHFT/3600.0D0, &
                            SLONSHFT/3600.0D0
                            WRITE(SRCD,76) TYPDAT, SWLAT_DEGS(IDEM), &
                            SWLON_DEGS(IDEM)
                            WRITE(SRCD,*)
                            WRITE(SRCD,77) TYPDAT, NEE_MTRS(IDEM), &
                            NEN_MTRS(IDEM)
                            WRITE(SRCD,78) XSSMSHFT, YSSMSHFT
                            WRITE(SRCD,79) TYPDAT, SWE_MTRS(IDEM), &
                            SWN_MTRS(IDEM)
                        ELSE
                        ! ---                   Current file boundaries follow geographic coordinates,
                        !                       print corner coordinates in degrees and arc-seconds
                            WRITE(SRCD,*)
                            WRITE(SRCD,72) TYPDAT, IDEM, TYPDAT, &
                            MAPNAME(IDEM), TYPDAT, NADD(IDEM)
                            WRITE(SRCD,74) TYPDAT, NELAT_DEGS(IDEM), &
                            NELON_DEGS(IDEM)
                            WRITE(SRCD,75) SLATSHFT/3600.0D0, &
                            SLONSHFT/3600.0D0
                            WRITE(SRCD,76) TYPDAT, SWLAT_DEGS(IDEM), &
                            SWLON_DEGS(IDEM)
                            WRITE(SRCD,*)
                            WRITE(SRCD,87) TYPDAT, NELAT_ARCS(IDEM), &
                            NELON_ARCS(IDEM)
                            WRITE(SRCD,88) SLATSHFT, SLONSHFT
                            WRITE(SRCD,89) TYPDAT, SWLAT_ARCS(IDEM), &
                            SWLON_ARCS(IDEM)
                        END IF
                        WRITE(SRCD,81) TYPDAT, IDEM, ISRC
                        81 FORMAT(A3,' No.:',I5,' Also Contains Src No.:', &
                        I7/)
                    END IF
                END IF

            END IF

        END DO DEMLOOP

        IF (NSRCD(ISRC) > 1) THEN
            CALL ERRHDL(PATH,MODNAM,'W','333',SRCID(ISRC))

        END IF

    END DO SRCLOOP

!*    Check for any Sources Not in Any DEM File; Generate warnings if found
!*    Generate warnings if found
!*    First initialize NSGAP variable
    NSGAP = 0
    DO ISRC = 1, NUMSRC
        IF (JDMS(ISRC) == 0) THEN
        !*          Assign logical variable to indicate that "gap" sources found
            GAPSRC = .TRUE.
            NSGAP  = NSGAP + 1

        !*          Write Warning Message for Gap Sources on First Pass
        !*          If DEM file not found on second pass, Fatal error will be generated
            CALL ERRHDL(PATH,MODNAM,'W','335',SRCID(ISRC))


            IF (SRCDBG) THEN
                IF (NSGAP == 1) THEN
                    WRITE(SRCD,*) ' '
                    WRITE(SRCD,*) &
                    '*** SOURCES NOT ASSIGNED TO FILE - ', &
                    'INITIAL PASS:'
                    WRITE(SRCD,*) ' '
                END IF
                WRITE(SRCD,90) ISRC, XSRCU(ISRC),YSRCU(ISRC),IZONS(ISRC), &
                SLAT(ISRC)/3600.0D0, SLON(ISRC)/3600.0D0, &
                NADA
                90 FORMAT('SRC#: ',I5,'  UTM(E/N/Z):', 2F20.3, I4/ &
                &                              16X,'Lat/Lon:', 2F20.7, '  NAD: ',I2)
            END IF

        END IF
    END DO

!*
!*    If DEM files are found for all sources (GAPSRC = F), Skip to bottom
    IF ( .NOT. GAPSRC) GO TO 999
!*
    WRITE(IOUNIT,91) TYPDAT, NSGAP
    91 FORMAT(/,'******', &
    /'DEMSRC:  Reprocessing ',A3,'s for ',I7,'  "Gap" ', &
    'Sources!', &
    /'******'/)
    IF (SRCDBG) THEN
        WRITE(SRCD,91) TYPDAT, NSGAP
    END IF

!*    Reprocess for "Gap" sources without NAD adjustment to
!*    determine if sources are located within NAD-shift gaps

    DO ISRC = 1, NUMSRC

        IF (JDMS(ISRC) == 0) THEN
        !           DEM not found for this source yet

            GAPLOOP1:DO IDEM = 1, NUMDEM

            ! ---          Determine whether a "gap" source falls within
            !              a terrain file without applying NAD conversion;
            !              this means that the source falls within a "gap"
            !              created by the NAD shift.

            ! ---          First adjust UTM's for Zone change if needed
                IF (IZONS(ISRC) /= IZOND(IDEM)) THEN
                !*                Source zone doesn't match DEM zone;
                !*                Convert source Lat/Lon to UTMs, based on DEM file NADD and DEM Zone

                    SELECT CASE (NADD(IDEM))   ! Use datum for DEM file
                    CASE (0)
                    IF (IPLAN(IDEM) == 0) THEN
                        ISPHERE = 4  ! UTM/LAT-LONG conv. based on GRS80 ellipsoid
                    ELSE IF (IPLAN(IDEM) == 1) THEN
                        ISPHERE = 0  ! UTM/LL conv. based on Clarke 1866 ellipsoid
                    END IF
                    CASE (2:4)
                    ISPHERE = 4     ! UTM/LAT-LONG conv. based on GRS80 ellipsoid
                    CASE (1,5:6)
                    ISPHERE = 0     ! UTM/LL conv. based on Clarke 1866 ellipsoid
                    CASE DEFAULT
                    ISPHERE = 4     ! DEFAULT CASE shouldn't occur
                    END SELECT

                    ARGE = SLON(ISRC)
                    ARGN = SLAT(ISRC)
                    XARG = 0.0D0
                    YARG = 0.0D0
                !*                Pass DEM file zone (IZOND(IDEM)) to UTMGEO to get new source
                !*                coordinates (XARG, YARG) referenced to IZOND(IDEM)).
                    CALL UTMGEO (555,IZOND(IDEM),IZONSRC,XARG,YARG, &
                    ARGE,ARGN,ISPHERE)
                ! ---             Use XSSMSHFT and YSSMSHFT variables, but these only reflect
                !                 shift for UTM zone, not for NAD
                    XSSMSHFT = XARG
                    YSSMSHFT = YARG
                ELSE
                ! ---             Zone shift calculation is not needed, assign original source
                !                 UTMs to temporary variables
                    XSSMSHFT = XSRCU(ISRC)
                    YSSMSHFT = YSRCU(ISRC)
                    IZONSRC  = IZONS(ISRC)
                END IF

            ! ---          Determine whether file contains source based on whether file
            !              boundaries follow UTM or Lat/Lon lines
                IF((IPLAN(IDEM) == 1 .AND. &
                DABS(NWE_MTRS(IDEM)-SWE_MTRS(IDEM)) < &
                &                                              0.05D0*DXM(IDEM) .AND. &
                DABS(NEE_MTRS(IDEM)-SEE_MTRS(IDEM)) < &
                &                                              0.05D0*DXM(IDEM) .AND. &
                (XSSMSHFT >= SWE_MTRS(IDEM) .AND. &
                XSSMSHFT <= NEE_MTRS(IDEM) .AND. &
                YSSMSHFT >= SWN_MTRS(IDEM) .AND. &
                YSSMSHFT <= NEN_MTRS(IDEM))) &
                 .OR. &
                (SLON(ISRC) >= SWLON_ARCS(IDEM) .AND. &
                SLON(ISRC) <= NELON_ARCS(IDEM) .AND. &
                SLAT(ISRC) >= SWLAT_ARCS(IDEM) .AND. &
                SLAT(ISRC) <= NELAT_ARCS(IDEM))) THEN

                !*---             Source is contained within this file, which indicates
                !*                that source is located within a "gap" due to NAD shift
                !*                between adjacent files.
                !*                Increment counter for "gap" sources assigned on
                !*                2nd pass and set logical flag
                    NSGAP2 = NSGAP2 + 1

                    GAPSFOUND = .TRUE.

                !* ---            Loop through DEM files again with NAD shift
                !                 to find closest file for this gap source;
                !                 Use a tolerance of 1/2 of the NAD shift in
                !                 each direction

                    GAPLOOP2:DO JDEM = 1, NUMDEM

                    !                    Determine whether to apply NAD shift
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

                    !*                   Apply NAD shift if necessary; direction of shift is included
                    !                    in XRDIFS and YRDIFS
                        SLONSHFT = SLON(ISRC) + (XSDIFS(ISRC)*NADFLG)
                        SLATSHFT = SLAT(ISRC) + (YSDIFS(ISRC)*NADFLG)

                        XSSMSHFT = XSRCU(ISRC) + (XSDIFM(ISRC)*NADFLG)
                        YSSMSHFT = YSRCU(ISRC) + (YSDIFM(ISRC)*NADFLG)

                        IF (IZONS(ISRC) /= IZOND(JDEM)) THEN
                        !*                      Source zone doesn't match DEM zone;
                        !*                      Convert source Lat/Lon to UTMs, based on DEM file NADD and DEM Zone

                            SELECT CASE (NADD(JDEM))   ! Use datum for DEM file
                            CASE (0)
                            IF (IPLAN(JDEM) == 0) THEN
                                ISPHERE = 4  ! UTM/LAT-LONG conv. based on GRS80 ellipsoid
                            ELSE IF (IPLAN(JDEM) == 1) THEN
                                ISPHERE = 0  ! UTM/LL conv. based on Clarke 1866 ellipsoid
                            END IF
                            CASE (2:4)
                            ISPHERE = 4     ! UTM/LAT-LONG conv. based on GRS80 ellipsoid
                            CASE (1,5:6)
                            ISPHERE = 0     ! UTM/LL conv. based on Clarke 1866 ellipsoid
                            CASE DEFAULT
                            ISPHERE = 4     ! DEFAULT CASE shouldn't occur
                            END SELECT

                            ARGE = SLONSHFT
                            ARGN = SLATSHFT
                            XARG = 0.0D0
                            YARG = 0.0D0
                        !*                      Pass DEM file zone (IZOND(JDEM)) to UTMGEO to get new source
                        !*                      coordinates (XARG, YARG) referenced to IZOND(JDEM)).
                            CALL UTMGEO (555,IZOND(JDEM),IZONSRC,XARG,YARG, &
                            ARGE,ARGN,ISPHERE)
                            XSSMSHFT = XARG
                            YSSMSHFT = YARG
                        END IF

                        IF((IPLAN(JDEM) == 1 .AND. &
                        DABS(NWE_MTRS(JDEM)-SWE_MTRS(JDEM)) < &
                        &                                         0.05D0*DXM(JDEM) .AND. &
                        DABS(NEE_MTRS(JDEM)-SEE_MTRS(JDEM)) < &
                        &                                         0.05D0*DXM(JDEM) .AND. &
                        (XSSMSHFT >= SWE_MTRS(JDEM)-DABS(0.5D0*XSDIFM(ISRC)) .AND. &
                        XSSMSHFT <= NEE_MTRS(JDEM)+DABS(0.5D0*XSDIFM(ISRC)) .AND. &
                        YSSMSHFT >= SWN_MTRS(JDEM)-DABS(0.5D0*YSDIFM(ISRC)) .AND. &
                        YSSMSHFT <= NEN_MTRS(JDEM)+DABS(0.5D0*YSDIFM(ISRC)))) &
                         .OR. &
                        (SLONSHFT >= SWLON_ARCS(JDEM)-DABS(0.5D0*XSDIFS(ISRC)) .AND. &
                        SLONSHFT <= NELON_ARCS(JDEM)+DABS(0.5D0*XSDIFS(ISRC)) .AND. &
                        SLATSHFT >= SWLAT_ARCS(JDEM)-DABS(0.5D0*YSDIFS(ISRC)) .AND. &
                        SLATSHFT <= NELAT_ARCS(JDEM)+DABS(0.5D0*YSDIFS(ISRC))))THEN

                        !*                     This is the first file that contains the source;
                        !*                     assign file index to JDMS array.

                            JDMS(ISRC) = JDEM

                        !*                     Assign code of 2 indicating source assigned on
                        !*                     2nd pass
                            ISIN(ISRC,JDEM) = 2

                            IF (SRCDBG) THEN

                                WRITE(SRCD,731) ISRC, SLAT(ISRC)/3600.0D0, &
                                SLON(ISRC)/3600.0D0, &
                                YSDIFS(ISRC), XSDIFS(ISRC)
                                WRITE(SRCD,73) XSRCU(ISRC),YSRCU(ISRC), &
                                IZONS(ISRC), &
                                XSDIFM(ISRC), YSDIFM(ISRC), &
                                NADA

                                IF (IPLAN(JDEM) == 1 .AND. &
                                (DABS(NWE_MTRS(JDEM)-SWE_MTRS(JDEM)) < &
                                &                                             0.05D0*DXM(JDEM)) .AND. &
                                (DABS(NEE_MTRS(JDEM)-SEE_MTRS(JDEM)) < &
                                &                                             0.05D0*DXM(JDEM)))THEN
                                ! ---                      Current file boundaries follow UTM coordinates,
                                !                          print corner coordinates in degrees and UTM meters
                                    WRITE(SRCD,*)
                                    WRITE(SRCD,72) TYPDAT, JDEM, TYPDAT, &
                                    MAPNAME(JDEM), TYPDAT, &
                                    NADD(JDEM)
                                    WRITE(SRCD,74) TYPDAT, NELAT_DEGS(JDEM), &
                                    NELON_DEGS(JDEM)
                                    WRITE(SRCD,75) SLATSHFT/3600.0D0, &
                                    SLONSHFT/3600.0D0
                                    WRITE(SRCD,76) TYPDAT, SWLAT_DEGS(JDEM), &
                                    SWLON_DEGS(JDEM)
                                    WRITE(SRCD,*)
                                    WRITE(SRCD,77) TYPDAT, NEE_MTRS(JDEM), &
                                    NEN_MTRS(JDEM)
                                    WRITE(SRCD,78) XSSMSHFT, YSSMSHFT
                                    WRITE(SRCD,79) TYPDAT, SWE_MTRS(JDEM), &
                                    SWN_MTRS(JDEM)
                                ELSE
                                ! ---                      Current file boundaries follow geographic coordinates,
                                !                          print corner coordinates in degrees and arc-seconds
                                    WRITE(SRCD,*)
                                    WRITE(SRCD,72) TYPDAT, JDEM, TYPDAT, &
                                    MAPNAME(JDEM), TYPDAT, &
                                    NADD(JDEM)
                                    WRITE(SRCD,74) TYPDAT, NELAT_DEGS(JDEM), &
                                    NELON_DEGS(JDEM)
                                    WRITE(SRCD,75) SLATSHFT/3600.0D0, &
                                    SLONSHFT/3600.0D0
                                    WRITE(SRCD,76) TYPDAT, SWLAT_DEGS(JDEM), &
                                    SWLON_DEGS(JDEM)
                                    WRITE(SRCD,*)
                                    WRITE(SRCD,87) TYPDAT, NELAT_ARCS(JDEM), &
                                    NELON_ARCS(JDEM)
                                    WRITE(SRCD,88) SLATSHFT, SLONSHFT
                                    WRITE(SRCD,89) TYPDAT, SWLAT_ARCS(JDEM), &
                                    SWLON_ARCS(JDEM)
                                END IF

                                WRITE(SRCD,80) TYPDAT, JDMS(ISRC),ISRC

                            END IF

                        END IF

                    END DO GAPLOOP2

                END IF

            !*             If DEM is found, then exit the DEM loop to next source
                IF(JDMS(ISRC) > 0)THEN
                    EXIT GAPLOOP1

                ELSE IF (SRCDBG) THEN
                    WRITE(SRCD,*) 'STILL LOOKING FOR ISRC: ', ISRC
                    WRITE(SRCD,731) ISRC, SLAT(ISRC)/3600.0D0, &
                    SLON(ISRC)/3600.0D0, &
                    YSDIFS(ISRC), XSDIFS(ISRC)
                    WRITE(SRCD,73) XSRCU(ISRC), YSRCU(ISRC), IZONS(ISRC), &
                    XSDIFM(ISRC), YSDIFM(ISRC), &
                    NADA

                    IF (IPLAN(IDEM) == 1 .AND. &
                    (DABS(NWE_MTRS(IDEM)-SWE_MTRS(IDEM)) < &
                    &                                          0.05D0*DXM(IDEM)) .AND. &
                    (DABS(NEE_MTRS(IDEM)-SEE_MTRS(IDEM)) < &
                    &                                          0.05D0*DXM(IDEM)))THEN
                    ! ---                Current file boundaries follow UTM coordinates,
                    !                    print corner coordinates in degrees and UTM meters
                        WRITE(SRCD,*)
                        WRITE(SRCD,72) TYPDAT, IDEM, TYPDAT, &
                        MAPNAME(IDEM), TYPDAT, NADD(IDEM)
                        WRITE(SRCD,74) TYPDAT, NELAT_DEGS(IDEM), &
                        NELON_DEGS(IDEM)
                        WRITE(SRCD,75) SLAT(ISRC)/3600.0D0, &
                        SLON(ISRC)/3600.0D0
                        WRITE(SRCD,76) TYPDAT, SWLAT_DEGS(IDEM), &
                        SWLON_DEGS(IDEM)
                        WRITE(SRCD,*)
                        WRITE(SRCD,77) TYPDAT, NEE_MTRS(IDEM), &
                        NEN_MTRS(IDEM)
                        WRITE(SRCD,78) XSRCU(ISRC), YSRCU(ISRC)
                        WRITE(SRCD,79) TYPDAT, SWE_MTRS(IDEM), &
                        SWN_MTRS(IDEM)
                    ELSE
                    ! ---                Current file boundaries follow geographic coordinates,
                    !                    print corner coordinates in degrees and arc-seconds
                        WRITE(SRCD,*)
                        WRITE(SRCD,72) TYPDAT, IDEM, TYPDAT, &
                        MAPNAME(IDEM), TYPDAT, NADD(IDEM)
                        WRITE(SRCD,74) TYPDAT, NELAT_DEGS(IDEM), &
                        NELON_DEGS(IDEM)
                        WRITE(SRCD,75) SLAT(ISRC)/3600.0D0, &
                        SLON(ISRC)/3600.0D0
                        WRITE(SRCD,76) TYPDAT, SWLAT_DEGS(IDEM), &
                        SWLON_DEGS(IDEM)
                        WRITE(SRCD,*)
                        WRITE(SRCD,87) TYPDAT, NELAT_ARCS(IDEM), &
                        NELON_ARCS(IDEM)
                        WRITE(SRCD,88) SLAT(ISRC), SLON(ISRC)
                        WRITE(SRCD,89) TYPDAT, SWLAT_ARCS(IDEM), &
                        SWLON_ARCS(IDEM)
                    END IF
                END IF

            END DO GAPLOOP1

        END IF

    END DO   ! Source Loop

!*    Write message regarding results of 2nd pass for gap sources
    WRITE(IOUNIT,92) NSGAP2, TYPDAT
    92 FORMAT(/,'******', &
    /'DEMSRC:  A total of ',I7,' "Gap" sources were', &
    /'         assigned to ',A3,' files on the 2nd Pass.', &
    /'         Depending on the size of the gap, these', &
    /'         sources may be assigned missing elevations', &
    /'         unless the "FILLGAPS" option on the DATATYPE', &
    /'         keyword is specified.', &
    /'         These gaps are due to NAD conversion issues,', &
    /'         which should be resolved with the use of ', &
    /'         standard NED elevation data.', &
    /'******'/)
    IF (SRCDBG) THEN
        WRITE(SRCD,92) NSGAP2, TYPDAT
    END IF

!*    Check Again for Sources Not Assigned to Any DEM File
!*    This time fatal errors will be generated by "homeless" sources
    DO ISRC = 1, NUMSRC
        IF (JDMS(ISRC) == 0) THEN
            RUNERR = .TRUE.

            CALL ERRHDL(PATH,MODNAM,'E','336',SRCID(ISRC))
            WRITE(IOUNIT,*) '*** SOURCE NOT ASSIGNED TO FILE - ', &
            'SECOND PASS:'
            WRITE(IOUNIT,90) ISRC, XSRCU(ISRC), YSRCU(ISRC),IZONS(ISRC), &
            SLAT(ISRC)/3600.0D0, SLON(ISRC)/3600.0D0, &
            NADA

        END IF
    END DO

    999 CONTINUE

    IF (SRCDBG) CLOSE(SRCD)

    WRITE(iounit,*) 'Exiting DEMSRC'
    WRITE(*,*) 'Exiting DEMSRC'

    RETURN

    END SUBROUTINE
