SUBROUTINE DEMREC
!***********************************************************************
!*               DEMREC Module of AERMAP Terrain Preprocessor
!*
!*       PURPOSE: Determine which receptor falls into which DEM file
!*
!*
!*       PROGRAMMER: Peter Eckhoff
!*
!*       DATE:    January 8, 2001
!*
!*       Revision History:
!*
!*       MODIFIED: February 9, 2009
!*
!*       MODIFIED BY: Roger W. Brode, U.S. EPA, OAQPS, AQMG
!*
!*                Modified to use receptor UTM's and file corner UTM's to
!*                check for receptor within file for NED data in UTM format.
!*                Incorporated checks for receptor being assigned to lower
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

  INTEGER, ALLOCATABLE :: NRECD(:)

  INTEGER :: IREC, JDEM, IZONREC, ISPHERE

  real (kind=8), PARAMETER :: EPSILON = 0.001D0
  real (kind=8) :: RLONSHFT, RLATSHFT
  real (kind=8) :: XRRMSHFT, YRRMSHFT
  real (kind=8) :: ARGE, ARGN, XARG, YARG
  real (kind=8) :: NADFLG

  LOGICAL :: GAPREC

  ALLOCATE (NRECD(NREC))

  MODNAM = 'DEMREC'

!*    Initialize logical variable for identifying "gap" receptors
  GAPREC = .FALSE.
  JDM    = 0
  NRECD  = 0

  IF (RECDBG) THEN
!*       Open debug output file
     OPEN(RECD, FILE = RECNDEM_FILE, STATUS = 'REPLACE')
!*       Write Version Date Header to RECNDEM Debug File
     WRITE(RECD,9011) trim(VERSN), RUNDAT, RUNTIM
9011 FORMAT('TERSCI Version ',A,1x,A8,1x,A8/)
  END IF

! --- Loop through receptors to identify DEM (or NED) files that contain
!     the receptor
  RECLOOP:DO IREC = 1, NUMREC

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

!*          Apply NAD shift if necessary; direction of shift is included in XRDIFS/YRDIFS

        RLONSHFT = RLON(IREC) + (XRDIFS(IREC)*NADFLG)
        RLATSHFT = RLAT(IREC) + (YRDIFS(IREC)*NADFLG)

        XRRMSHFT = XRECU(IREC) + (XRDIFM(IREC)*NADFLG)
        YRRMSHFT = YRECU(IREC) + (YRDIFM(IREC)*NADFLG)

        IF (IZONR(IREC) /= IZOND(IDEM)) THEN
!*             Receptor zone doesn't match DEM zone;
!*             Convert receptor Lat/Lon to UTMs, based on DEM file NADD and DEM Zone

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

           ARGE = RLONSHFT
           ARGN = RLATSHFT
           XARG = 0.0D0
           YARG = 0.0D0
!*             Pass DEM file zone (IZOND(IDEM)) to UTMGEO to get new receptor
!*             coordinates (XARG, YARG) referenced to IZOND(IDEM)).
           CALL UTMGEO (555,IZOND(IDEM),IZONREC,XARG,YARG, &
                ARGE,ARGN,ISPHERE)
           XRRMSHFT = XARG
           YRRMSHFT = YARG
        END IF

        IF (RECDBG) THEN
           IF (IDEM == 1) THEN
              WRITE(RECD,730) TYPDAT, IREC
730           FORMAT('LOOKING AT ',A3,'s FOR REC: ',I7)
              WRITE(RECD,731) IREC, RLAT(IREC)/3600.0D0, &
                   RLON(IREC)/3600.0D0, &
                   YRDIFS(IREC), XRDIFS(IREC)
731           FORMAT('REC#:',I7,'    RLAT/LON:', 2F20.8, &
                   /13X,'SHIFTL/L(S):', 2F20.8)
              WRITE(RECD,73) XRECU(IREC), YRECU(IREC), IZONR(IREC), &
                   XRDIFM(IREC), YRDIFM(IREC), &
                   NADA
73            FORMAT(12X,'UTM(E/N/Zon):', 2F20.3,1X,I4/ &
                   &                   12X,' SHIFTX/Y(M):', 2F20.8,3X,'REC NAD:',I3)
           ELSE IF (IDEM > 1 .AND. NRECD(IREC) == 0) THEN
              WRITE(RECD,732) TYPDAT, IREC
732           FORMAT('STILL LOOKING AT ',A3,'s FOR REC: ',I7)
           END IF

           IF (NRECD(IREC) == 0) THEN
! ---           File containing receptor has not been located yet
              IF (IPLAN(IDEM) == 1 .AND. &
                   (DABS(NWE_MTRS(IDEM)-SWE_MTRS(IDEM)) < &
                   &                                             0.05D0*DXM(IDEM)) .AND. &
                   (DABS(NEE_MTRS(IDEM)-SEE_MTRS(IDEM)) < &
                   &                                             0.05D0*DXM(IDEM)))THEN
! ---             Current file boundaries follow UTM coordinates,
!                 print corner coordinates in degrees and UTM meters
                 WRITE(RECD,*)
                 WRITE(RECD,72) TYPDAT, IDEM, TYPDAT, MAPNAME(IDEM), &
                      TYPDAT, NADD(IDEM)
                 WRITE(RECD,74) TYPDAT, NELAT_DEGS(IDEM), &
                      NELON_DEGS(IDEM)
                 WRITE(RECD,75) RLATSHFT/3600.0D0, RLONSHFT/3600.0D0
                 WRITE(RECD,76) TYPDAT, SWLAT_DEGS(IDEM), &
                      SWLON_DEGS(IDEM)
                 WRITE(RECD,*)
                 WRITE(RECD,77) TYPDAT, NEE_MTRS(IDEM), &
                      NEN_MTRS(IDEM)
                 WRITE(RECD,78) XRRMSHFT, YRRMSHFT
                 WRITE(RECD,79) TYPDAT, SWE_MTRS(IDEM), &
                      SWN_MTRS(IDEM)
72               FORMAT(3X,A3,' No.:',I5,1X,A3,' Name: ',A40, &
                      &                        1X,A3,' NAD:', I3)
74               FORMAT(8X,A3,' Max. LAT/LON(D.D):', 2F15.5)
75               FORMAT(8X,'     Rec LAT/LON(D.D):', 2F15.5)
76               FORMAT(8X,A3,' Min. LAT/LON(D.D):', 2F15.5)
77               FORMAT(8X,A3,' Max. UTME/UTMN(M):', 2F15.3)
78               FORMAT(8X,'     Rec UTME/UTMN(M):', 2F15.3)
79               FORMAT(8X,A3,' Min. UTME/UTMN(M):', 2F15.3/)
              ELSE
! ---             Current file boundaries follow geographic coordinates,
!                 print corner coordinates in degrees and arc-seconds
                 WRITE(RECD,*)
                 WRITE(RECD,72) TYPDAT, IDEM, TYPDAT, MAPNAME(IDEM), &
                      TYPDAT, NADD(IDEM)
                 WRITE(RECD,74) TYPDAT, NELAT_DEGS(IDEM), &
                      NELON_DEGS(IDEM)
                 WRITE(RECD,75) RLATSHFT/3600.0D0, RLONSHFT/3600.0D0
                 WRITE(RECD,76) TYPDAT, SWLAT_DEGS(IDEM), &
                      SWLON_DEGS(IDEM)
                 WRITE(RECD,*)
                 WRITE(RECD,87) TYPDAT, NELAT_ARCS(IDEM), &
                      NELON_ARCS(IDEM)
                 WRITE(RECD,88) RLATSHFT, RLONSHFT
                 WRITE(RECD,89) TYPDAT, SWLAT_ARCS(IDEM), &
                      SWLON_ARCS(IDEM)
87               FORMAT(8X,A3,' Max. LAT/LON(Sec):', 2F15.5)
88               FORMAT(8X,'     Rec LAT/LON(Sec):', 2F15.5)
89               FORMAT(8X,A3,' Min. LAT/LON(Sec):', 2F15.5/)
              END IF
           END IF
        END IF

! ---       Determine Which Terrain File(s) a receptor falls within
!           If file boundaries follow UTM coordinates, compare receptor
!           UTM to corner UTM's; otherwise compare receptor Lat/Lon
!           to corner Lat/Lon
        IF((IPLAN(IDEM) == 1 .AND. &
             DABS(NWE_MTRS(IDEM)-SWE_MTRS(IDEM)) < 0.05D0*DXM(IDEM) .AND. &
             DABS(NEE_MTRS(IDEM)-SEE_MTRS(IDEM)) < 0.05D0*DXM(IDEM) .AND. &
             (XRRMSHFT >= SWE_MTRS(IDEM) .AND. &
             XRRMSHFT <= NEE_MTRS(IDEM) .AND. &
             YRRMSHFT >= SWN_MTRS(IDEM) .AND. &
             YRRMSHFT <= NEN_MTRS(IDEM))) &
             .OR. &
             (RLONSHFT >= SWLON_ARCS(IDEM) .AND. &
             RLONSHFT <= NELON_ARCS(IDEM) .AND. &
             RLATSHFT >= SWLAT_ARCS(IDEM) .AND. &
             RLATSHFT <= NELAT_ARCS(IDEM))) THEN


           IF (JDM(IREC) == 0) THEN
!*                This is the first file that contains the receptor;
!*                assign file index to JDM as first file
              JDM(IREC)   = IDEM

!*                Set number of files containing receptor to 1
              NRECD(IREC) = 1

!*                Set IRIN array element for this receptor and
!*                DEM file to 1
              IRIN(IREC,IDEM) = 1

              IF (RECDBG) THEN
!*                   Write message to RECNDEM debug file
                 WRITE(RECD,80) TYPDAT, IDEM, IREC
80               FORMAT(A3,' No.:',I5,' Contains Rec No.:', &
                      I7/)
              END IF

           ELSE
!*                Receptor falls inside multiple DEM files
              NRECD(IREC) = NRECD(IREC) + 1

!*                Set IRIN array element for this receptor and
!*                DEM file to 1
              IRIN(IREC,IDEM) = 1

! ---             Check for lower resolution file preceding higher resolution file
!                 Use latitude spacing (DYM) for comparisons since this doesn't
!                 change for higher latitudes
              IF (IPLAN(JDM(IREC)) == 0 .AND. &
                   IPLAN(IDEM) == 0) THEN
! ---                Receptor DEM and current DEM both geograhpic
                 IF (DYM(JDM(IREC)) > DYM(IDEM)+EPSILON) THEN
                    WRITE (DUMMY,'(I8)') IREC
                    CALL ERRHDL(PATH,MODNAM,'E','326',DUMMY)
                 END IF
              ELSE IF (IPLAN(JDM(IREC)) == 1 .AND. &
                   IPLAN(IDEM) == 1) THEN
! ---                Receptor DEM and current DEM both UTM
                 IF (DYM(JDM(IREC)) > DYM(IDEM)+EPSILON) THEN
                    WRITE (DUMMY,'(I8)') IREC
                    CALL ERRHDL(PATH,MODNAM,'E','326',DUMMY)
                 END IF
              ELSE IF (IPLAN(JDM(IREC)) == 0) THEN
! ---                Receptor DEM geographic but current DEM is UTM;
!                    adjust spacing units
                 IF (DABS(DYM(JDM(IREC))-(1.0D0/9.0D0)) < &
                      EPSILON) THEN
                    IF (3.0D0 > DYM(IDEM)+EPSILON) THEN
                       WRITE (DUMMY,'(I8)') IREC
                       CALL ERRHDL(PATH,MODNAM,'E','326',DUMMY)
                    END IF
                 ELSE IF (DABS(DYM(JDM(IREC))-(1.0D0/3.0D0)) < &
                      EPSILON) THEN
                    IF (10.0D0 > DYM(IDEM)+EPSILON) THEN
                       WRITE (DUMMY,'(I8)') IREC
                       CALL ERRHDL(PATH,MODNAM,'E','326',DUMMY)
                    END IF
                 ELSE IF (DABS(DYM(JDM(IREC))-1.0D0) < &
                      EPSILON) THEN
                    IF (30.0D0 > DYM(IDEM)+EPSILON) THEN
                       WRITE (DUMMY,'(I8)') IREC
                       CALL ERRHDL(PATH,MODNAM,'E','326',DUMMY)
                    END IF
                 ELSE IF (DABS(DYM(JDM(IREC))-2.0D0) < &
                      EPSILON) THEN
                    IF (60.0D0 > DYM(IDEM)+EPSILON) THEN
                       WRITE (DUMMY,'(I8)') IREC
                       CALL ERRHDL(PATH,MODNAM,'E','326',DUMMY)
                    END IF
                 ELSE IF (DABS(DYM(JDM(IREC))-3.0D0) < &
                      EPSILON) THEN
                    IF (90.0D0 > DYM(IDEM)+EPSILON) THEN
                       WRITE (DUMMY,'(I8)') IREC
                       CALL ERRHDL(PATH,MODNAM,'E','326',DUMMY)
                    END IF
                 END IF
              ELSE IF (IPLAN(JDM(IREC)) == 1) THEN
! ---                Receptor DEM is UTM but current DEM is geographic;
!                    adjust spacing units
                 IF (DABS(DYM(JDM(IREC))-3.0D0) < &
                      EPSILON) THEN
                    IF ((1.0D0/9.0D0) > DYM(IDEM)+EPSILON) THEN
                       WRITE (DUMMY,'(I8)') IREC
                       CALL ERRHDL(PATH,MODNAM,'E','326',DUMMY)
                    END IF
                 ELSE IF (DABS(DYM(JDM(IREC))-10.0D0) < &
                      EPSILON) THEN
                    IF ((1.0D0/3.0D0) > DYM(IDEM)+EPSILON) THEN
                       WRITE (DUMMY,'(I8)') IREC
                       CALL ERRHDL(PATH,MODNAM,'E','326',DUMMY)
                    END IF
                 ELSE IF (DABS(DYM(JDM(IREC))-30.0D0) < &
                      EPSILON) THEN
                    IF (1.0D0 > DYM(IDEM)+EPSILON) THEN
                       WRITE (DUMMY,'(I8)') IREC
                       CALL ERRHDL(PATH,MODNAM,'E','326',DUMMY)
                    END IF
                 ELSE IF (DABS(DYM(JDM(IREC))-90.0D0) < &
                      EPSILON) THEN
                    IF (3.0D0 > DYM(IDEM)+EPSILON) THEN
                       WRITE (DUMMY,'(I8)') IREC
                       CALL ERRHDL(PATH,MODNAM,'E','326',DUMMY)
                    END IF
                 END IF
              END IF

              IF (RECDBG) THEN
                 IF (IPLAN(IDEM) == 1 .AND. &
                      (DABS(NWE_MTRS(IDEM)-SWE_MTRS(IDEM)) < &
                      &                                             0.05D0*DXM(IDEM)) .AND. &
                      (DABS(NEE_MTRS(IDEM)-SEE_MTRS(IDEM)) < &
                      &                                             0.05D0*DXM(IDEM)))THEN
! ---                   Current file boundaries follow UTM coordinates,
!                       print corner coordinates in degrees and UTM meters
                    WRITE(RECD,*)
                    WRITE(RECD,72) TYPDAT, IDEM, TYPDAT, &
                         MAPNAME(IDEM), TYPDAT, NADD(IDEM)
                    WRITE(RECD,74) TYPDAT, NELAT_DEGS(IDEM), &
                         NELON_DEGS(IDEM)
                    WRITE(RECD,75) RLATSHFT/3600.0D0, &
                         RLONSHFT/3600.0D0
                    WRITE(RECD,76) TYPDAT, SWLAT_DEGS(IDEM), &
                         SWLON_DEGS(IDEM)
                    WRITE(RECD,*)
                    WRITE(RECD,77) TYPDAT, NEE_MTRS(IDEM), &
                         NEN_MTRS(IDEM)
                    WRITE(RECD,78) XRRMSHFT, YRRMSHFT
                    WRITE(RECD,79) TYPDAT, SWE_MTRS(IDEM), &
                         SWN_MTRS(IDEM)
                 ELSE
! ---                   Current file boundaries follow geographic coordinates,
!                       print corner coordinates in degrees and arc-seconds
                    WRITE(RECD,*)
                    WRITE(RECD,72) TYPDAT, IDEM, TYPDAT, &
                         MAPNAME(IDEM), TYPDAT, NADD(IDEM)
                    WRITE(RECD,74) TYPDAT, NELAT_DEGS(IDEM), &
                         NELON_DEGS(IDEM)
                    WRITE(RECD,75) RLATSHFT/3600.0D0, &
                         RLONSHFT/3600.0D0
                    WRITE(RECD,76) TYPDAT, SWLAT_DEGS(IDEM), &
                         SWLON_DEGS(IDEM)
                    WRITE(RECD,*)
                    WRITE(RECD,87) TYPDAT, NELAT_ARCS(IDEM), &
                         NELON_ARCS(IDEM)
                    WRITE(RECD,88) RLATSHFT, RLONSHFT
                    WRITE(RECD,89) TYPDAT, SWLAT_ARCS(IDEM), &
                         SWLON_ARCS(IDEM)
                 END IF
                 WRITE(RECD,81) TYPDAT, IDEM, IREC
81               FORMAT(A3,' No.:',I5,' Also Contains Rec No.:', &
                      I7/)
              END IF
           END IF

        END IF

     END DO DEMLOOP

     IF (NRECD(IREC) > 1) THEN
        WRITE (DUMMY,'(I8)') IREC
        CALL ERRHDL(PATH,MODNAM,'W','332',DUMMY)
     END IF

  END DO RECLOOP

!*--- Check for any Receptors Not in Any DEM File on First Pass;
!*    Generate warnings if found
!*    First initialize NRGAP variable
  NRGAP = 0
  DO IREC = 1, NUMREC
     IF (JDM(IREC) == 0) THEN
!*          Assign logical variable to indicate that "gap" receptors found
        GAPREC = .TRUE.
        NRGAP  = NRGAP + 1

!*          Write Warning Message for Gap Receptors on First Pass
!*          If DEM file not found on second pass, Fatal error will be generated
        WRITE (DUMMY,'(I8)') IREC
        CALL ERRHDL(PATH,MODNAM,'W','330',DUMMY)

        IF (RECDBG) THEN
           IF (NRGAP == 1) THEN
              WRITE(RECD,*) ' '
              WRITE(RECD,*) &
                   '*** RECEPTORS NOT ASSIGNED TO FILE - ', &
                   'INITIAL PASS:'
              WRITE(RECD,*) ' '
           END IF
           WRITE(RECD,90) IREC, XRECU(IREC),YRECU(IREC),IZONR(IREC), &
                RLAT(IREC)/3600.0D0, RLON(IREC)/3600.0D0, &
                NADA
90         FORMAT('REC#: ',I5,'  UTM(E/N/Z):', 2F20.3, I4/ &
                &                              16X,'Lat/Lon:', 2F20.7, '  NAD: ',I2)
        END IF

     END IF
  END DO

!*
!*    If DEM files are found for all receptors (GAPREC = F), Skip to bottom
  IF ( .NOT. GAPREC) GO TO 999
!*
  WRITE(IOUNIT,91) TYPDAT, NRGAP
91 FORMAT(/,'******', &
       /'DEMREC:  Reprocessing ',A3,'s for ',I7,'  "Gap" ', &
       'Receptors!', &
       /'******'/)
  IF (RECDBG) THEN
     WRITE(RECD,91) TYPDAT, NRGAP
  END IF

!*    Reprocess for "Gap" receptors without NAD adjustment to
!*    determine if receptors are located within NAD-shift gaps

  DO IREC = 1, NUMREC

     IF (JDM(IREC) == 0) THEN
!           DEM not found for this receptor yet

        GAPLOOP1:DO IDEM = 1, NUMDEM

! ---          Determine whether a "gap" receptor falls within
!              a terrain file without applying NAD conversion;
!              this means that the receptor falls within a "gap"
!              created by the NAD shift.

! ---          First adjust UTM's for Zone change if needed
           IF (IZONR(IREC) /= IZOND(IDEM)) THEN
!*                Receptor zone doesn't match DEM zone;
!*                Convert receptor Lat/Lon to UTMs, based on DEM file NADD and DEM Zone

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

              ARGE = RLON(IREC)
              ARGN = RLAT(IREC)
              XARG = 0.0D0
              YARG = 0.0D0
!*                Pass DEM file zone (IZOND(IDEM)) to UTMGEO to get new receptor
!*                coordinates (XARG, YARG) referenced to IZOND(IDEM)).
              CALL UTMGEO (555,IZOND(IDEM),IZONREC,XARG,YARG, &
                   ARGE,ARGN,ISPHERE)
! ---             Use XRRMSHFT and YRRMSHFT variables, but these only reflect
!                 shift for UTM zone, not for NAD
              XRRMSHFT = XARG
              YRRMSHFT = YARG
           ELSE
! ---             Zone shift calculation is not needed, assign original receptor
!                 UTMs to temporary variables
              XRRMSHFT = XRECU(IREC)
              YRRMSHFT = YRECU(IREC)
              IZONREC  = IZONR(IREC)
           END IF

! ---          Determine whether file contains receptor based on whether file
!              boundaries follow UTM or Lat/Lon lines
           IF((IPLAN(IDEM) == 1 .AND. &
                DABS(NWE_MTRS(IDEM)-SWE_MTRS(IDEM)) < &
                &                                              0.05D0*DXM(IDEM) .AND. &
                DABS(NEE_MTRS(IDEM)-SEE_MTRS(IDEM)) < &
                &                                              0.05D0*DXM(IDEM) .AND. &
                (XRRMSHFT >= SWE_MTRS(IDEM) .AND. &
                XRRMSHFT <= NEE_MTRS(IDEM) .AND. &
                YRRMSHFT >= SWN_MTRS(IDEM) .AND. &
                YRRMSHFT <= NEN_MTRS(IDEM))) &
                .OR. &
                (RLON(IREC) >= SWLON_ARCS(IDEM) .AND. &
                RLON(IREC) <= NELON_ARCS(IDEM) .AND. &
                RLAT(IREC) >= SWLAT_ARCS(IDEM) .AND. &
                RLAT(IREC) <= NELAT_ARCS(IDEM))) THEN

!*---             Receptor is contained within this file, which indicates
!*                that receptor is located within a "gap" due to NAD shift
!*                between adjacent files.
!*                Increment counter for "gap" receptors assigned on
!*                2nd pass and set logical flag
              NRGAP2 = NRGAP2 + 1

              GAPSFOUND = .TRUE.

!*---             Loop through DEM files again with NAD shift
!                 to find closest file for this gap receptor;
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
                 RLONSHFT = RLON(IREC) + (XRDIFS(IREC)*NADFLG)
                 RLATSHFT = RLAT(IREC) + (YRDIFS(IREC)*NADFLG)

                 XRRMSHFT = XRECU(IREC) + (XRDIFM(IREC)*NADFLG)
                 YRRMSHFT = YRECU(IREC) + (YRDIFM(IREC)*NADFLG)

                 IF (IZONR(IREC) /= IZOND(JDEM)) THEN
!*                      Receptor zone doesn't match DEM zone;
!*                      Convert receptor Lat/Lon to UTMs, based on DEM file NADD and DEM Zone

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

                    ARGE = RLONSHFT
                    ARGN = RLATSHFT
                    XARG = 0.0D0
                    YARG = 0.0D0
!*                      Pass DEM file zone (IZOND(JDEM)) to UTMGEO to get new receptor
!*                      coordinates (XARG, YARG) referenced to IZOND(JDEM)).
                    CALL UTMGEO (555,IZOND(JDEM),IZONREC,XARG,YARG, &
                         ARGE,ARGN,ISPHERE)
                    XRRMSHFT = XARG
                    YRRMSHFT = YARG
                 END IF

                 IF((IPLAN(JDEM) == 1 .AND. &
                      DABS(NWE_MTRS(JDEM)-SWE_MTRS(JDEM)) < &
                      &                                         0.05D0*DXM(JDEM) .AND. &
                      DABS(NEE_MTRS(JDEM)-SEE_MTRS(JDEM)) < &
                      &                                         0.05D0*DXM(JDEM) .AND. &
                      (XRRMSHFT >= SWE_MTRS(JDEM)-DABS(0.5D0*XRDIFM(IREC)) .AND. &
                      XRRMSHFT <= NEE_MTRS(JDEM)+DABS(0.5D0*XRDIFM(IREC)) .AND. &
                      YRRMSHFT >= SWN_MTRS(JDEM)-DABS(0.5D0*YRDIFM(IREC)) .AND. &
                      YRRMSHFT <= NEN_MTRS(JDEM)+DABS(0.5D0*YRDIFM(IREC)))) &
                      .OR. &
                      (RLONSHFT >= SWLON_ARCS(JDEM)-DABS(0.5D0*XRDIFS(IREC)) .AND. &
                      RLONSHFT <= NELON_ARCS(JDEM)+DABS(0.5D0*XRDIFS(IREC)) .AND. &
                      RLATSHFT >= SWLAT_ARCS(JDEM)-DABS(0.5D0*YRDIFS(IREC)) .AND. &
                      RLATSHFT <= NELAT_ARCS(JDEM)+DABS(0.5D0*YRDIFS(IREC))))THEN

!*                     This is the first file that contains the receptor;
!*                     assign file index to JDM array.

                    JDM(IREC) = JDEM

!*                     Assign code of 2 indicating receptor assigned on
!*                     2nd pass
                    IRIN(IREC,JDEM) = 2

                    IF (RECDBG) THEN

                       WRITE(RECD,731) IREC, RLAT(IREC)/3600.0D0, &
                            RLON(IREC)/3600.0D0, &
                            YRDIFS(IREC), XRDIFS(IREC)
                       WRITE(RECD,73) XRECU(IREC),YRECU(IREC), &
                            IZONR(IREC), &
                            XRDIFM(IREC), YRDIFM(IREC), &
                            NADA

                       IF (IPLAN(JDEM) == 1 .AND. &
                            (DABS(NWE_MTRS(JDEM)-SWE_MTRS(JDEM)) < &
                            &                                             0.05D0*DXM(JDEM)) .AND. &
                            (DABS(NEE_MTRS(JDEM)-SEE_MTRS(JDEM)) < &
                            &                                             0.05D0*DXM(JDEM)))THEN
! ---                      Current file boundaries follow UTM coordinates,
!                          print corner coordinates in degrees and UTM meters
                          WRITE(RECD,*)
                          WRITE(RECD,72) TYPDAT, JDEM, TYPDAT, &
                               MAPNAME(JDEM), TYPDAT, &
                               NADD(JDEM)
                          WRITE(RECD,74) TYPDAT, NELAT_DEGS(JDEM), &
                               NELON_DEGS(JDEM)
                          WRITE(RECD,75) RLATSHFT/3600.0D0, &
                               RLONSHFT/3600.0D0
                          WRITE(RECD,76) TYPDAT, SWLAT_DEGS(JDEM), &
                               SWLON_DEGS(JDEM)
                          WRITE(RECD,*)
                          WRITE(RECD,77) TYPDAT, NEE_MTRS(JDEM), &
                               NEN_MTRS(JDEM)
                          WRITE(RECD,78) XRRMSHFT, YRRMSHFT
                          WRITE(RECD,79) TYPDAT, SWE_MTRS(JDEM), &
                               SWN_MTRS(JDEM)
                       ELSE
! ---                      Current file boundaries follow geographic coordinates,
!                          print corner coordinates in degrees and arc-seconds
                          WRITE(RECD,*)
                          WRITE(RECD,72) TYPDAT, JDEM, TYPDAT, &
                               MAPNAME(JDEM), TYPDAT, &
                               NADD(JDEM)
                          WRITE(RECD,74) TYPDAT, NELAT_DEGS(JDEM), &
                               NELON_DEGS(JDEM)
                          WRITE(RECD,75) RLATSHFT/3600.0D0, &
                               RLONSHFT/3600.0D0
                          WRITE(RECD,76) TYPDAT, SWLAT_DEGS(JDEM), &
                               SWLON_DEGS(JDEM)
                          WRITE(RECD,*)
                          WRITE(RECD,87) TYPDAT, NELAT_ARCS(JDEM), &
                               NELON_ARCS(JDEM)
                          WRITE(RECD,88) RLATSHFT, RLONSHFT
                          WRITE(RECD,89) TYPDAT, SWLAT_ARCS(JDEM), &
                               SWLON_ARCS(JDEM)
                       END IF

                       WRITE(RECD,80) TYPDAT, JDM(IREC),IREC

                    END IF

                 END IF

              END DO GAPLOOP2

           END IF

!*             If DEM is found, then exit the DEM loop to next receptor
           IF(JDM(IREC) > 0)THEN
              EXIT GAPLOOP1

           ELSE IF (RECDBG) THEN
              WRITE(RECD,*) 'STILL LOOKING FOR IREC: ', IREC
              WRITE(RECD,731) IREC, RLAT(IREC)/3600.0D0, &
                   RLON(IREC)/3600.0D0, &
                   YRDIFS(IREC), XRDIFS(IREC)
              WRITE(RECD,73) XRECU(IREC), YRECU(IREC), IZONR(IREC), &
                   XRDIFM(IREC), YRDIFM(IREC), &
                   NADA

              IF (IPLAN(IDEM) == 1 .AND. &
                   (DABS(NWE_MTRS(IDEM)-SWE_MTRS(IDEM)) < &
                   &                                          0.05D0*DXM(IDEM)) .AND. &
                   (DABS(NEE_MTRS(IDEM)-SEE_MTRS(IDEM)) < &
                   &                                          0.05D0*DXM(IDEM)))THEN
! ---                Current file boundaries follow UTM coordinates,
!                    print corner coordinates in degrees and UTM meters
                 WRITE(RECD,*)
                 WRITE(RECD,72) TYPDAT, IDEM, TYPDAT, &
                      MAPNAME(IDEM), TYPDAT, NADD(IDEM)
                 WRITE(RECD,74) TYPDAT, NELAT_DEGS(IDEM), &
                      NELON_DEGS(IDEM)
                 WRITE(RECD,75) RLAT(IREC)/3600.0D0, &
                      RLON(IREC)/3600.0D0
                 WRITE(RECD,76) TYPDAT, SWLAT_DEGS(IDEM), &
                      SWLON_DEGS(IDEM)
                 WRITE(RECD,*)
                 WRITE(RECD,77) TYPDAT, NEE_MTRS(IDEM), &
                      NEN_MTRS(IDEM)
                 WRITE(RECD,78) XRECU(IREC), YRECU(IREC)
                 WRITE(RECD,79) TYPDAT, SWE_MTRS(IDEM), &
                      SWN_MTRS(IDEM)
              ELSE
! ---                Current file boundaries follow geographic coordinates,
!                    print corner coordinates in degrees and arc-seconds
                 WRITE(RECD,*)
                 WRITE(RECD,72) TYPDAT, IDEM, TYPDAT, &
                      MAPNAME(IDEM), TYPDAT, NADD(IDEM)
                 WRITE(RECD,74) TYPDAT, NELAT_DEGS(IDEM), &
                      NELON_DEGS(IDEM)
                 WRITE(RECD,75) RLAT(IREC)/3600.0D0, &
                      RLON(IREC)/3600.0D0
                 WRITE(RECD,76) TYPDAT, SWLAT_DEGS(IDEM), &
                      SWLON_DEGS(IDEM)
                 WRITE(RECD,*)
                 WRITE(RECD,87) TYPDAT, NELAT_ARCS(IDEM), &
                      NELON_ARCS(IDEM)
                 WRITE(RECD,88) RLAT(IREC), RLON(IREC)
                 WRITE(RECD,89) TYPDAT, SWLAT_ARCS(IDEM), &
                      SWLON_ARCS(IDEM)
              END IF
           END IF

        END DO GAPLOOP1

     END IF

  END DO   ! Receptor Loop

!*    Write message regarding results of 2nd pass for gap receptors
  WRITE(IOUNIT,92) NRGAP2, TYPDAT
92 FORMAT(/,'******', &
       /'DEMREC:  A total of ',I7,' "Gap" receptors were', &
       /'         assigned to ',A3,' files on the 2nd Pass.', &
       /'         Depending on the size of the gap, these', &
       /'         receptors may be assigned missing elevations', &
       /'         unless the "FILLGAPS" option on the DATATYPE', &
       /'         keyword is specified.', &
       /'         These gaps are due to NAD conversion issues,', &
       /'         which should be resolved with the use of ', &
       /'         standard NED elevation data.', &
       /'******'/)
  IF (RECDBG) THEN
     WRITE(RECD,92) NRGAP2, TYPDAT
  END IF

!*    Check Again for Receptors Not Assigned to Any DEM File
!*    This time fatal errors will be generated by "homeless" receptors
  DO IREC = 1, NUMREC
     IF (JDM(IREC) == 0) THEN
        RUNERR = .TRUE.
        WRITE (DUMMY,'(I8)') IREC
        CALL ERRHDL(PATH,MODNAM,'E','331',DUMMY)
        WRITE(IOUNIT,*) '*** RECEPTOR NOT ASSIGNED TO FILE - ', &
             'SECOND PASS:'
        WRITE(IOUNIT,90) IREC, XRECU(IREC), YRECU(IREC),IZONR(IREC), &
             RLAT(IREC)/3600.0D0, RLON(IREC)/3600.0D0, &
             NADA

     END IF
  END DO

999 CONTINUE

  IF (RECDBG) CLOSE(RECD)

  WRITE(iounit,*) 'Exiting DEMREC'
  WRITE(*,*) 'Exiting DEMREC'

  RETURN

END SUBROUTINE DEMREC
