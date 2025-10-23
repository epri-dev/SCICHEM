    SUBROUTINE INITER_DEM
!***********************************************************************
!*               INITER_DEM Module of AERMAP Terrain Preprocessor
!*
!*       PURPOSE: Read Terrain Data from Raw DEM Data Files and Write Out
!*                the Data to Direct Access Binary Terrain Data Files.
!*                Also Create Record Index Files
!*
!*
!*       PROGRAMMER: Jayant Hardikar, Roger Brode
!*
!*       DATE:    September 29, 1995
!*
!*       MODIFIED: February 9, 2009
!*
!*                Modified output format for UTM Zone to I3 to
!*                accommodate Southern Hemisphere applications,
!*                since Southern zones are negative.  Also
!*                adjusted output format for XBASE and YBASE
!*                to accommodate real (kind=8) values.
!*                Adjusted XBASE (longitude) for LAT files to
!*                use negative for west longitude.
!*                Modified to account for no-DOMAIN option.
!*                Also modified handling of missing elevation
!*                data and elevation of local datum (LOCEL).
!*                Added information on direct access files to
!*                the DOMDETAIL.OUT debug file.
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
!*       INPUTS:  Raw Terrain Data (DEM) Files
!*
!*       OUTPUTS: Direct Access Binary Terrain Files
!*
!*       CALLED FROM:   MAIN
!***********************************************************************

!*    Variable Declarations
    USE TER_MAIN

    IMPLICIT NONE

    SAVE

    CHARACTER(12) :: MODNAM

    LOGICAL :: NEWPRO
    real (kind=8) :: XNODEI,YNODEI
    real (kind=8) :: NADFLG
    real (kind=8) :: ARGE, ARGN, XARG, YARG
    real (kind=8) :: XBUTM, YBUTM, XNUTM, YNUTM
    real (kind=8) :: XBLON, YBLAT
    INTEGER :: IPROF, J, K, L, M, IERR, INOD

    INTEGER, ALLOCATABLE :: NUMNOD(:)

    INTEGER :: NSTART, N, JRECRD, RC, ZMINSHFT, ZMAXSHFT
    INTEGER :: IZONE, ISPHERE

    logical   :: L_FirstProf  ! flg to indicate first profile within domain

    real (kind=8) :: ELV

    MODNAM = 'INITER_DEM'

    L_FirstProf = .TRUE.

    XBUTM = 0.0D0
    YBUTM = 0.0D0
    XNUTM = 0.0D0
    YNUTM = 0.0D0
    XBLON = 0.0D0
    YBLAT = 0.0D0

!*    Loop Over Terrain Files
    DEMLOOP: DO IDEM = 1, NUMDEM

    !*       Assign J=IDEM for READ Statements
        J = IDEM

    !*       Initialize Some Variables
        NUMPRF(IDEM) = 0

    !*       Open the DEM File * Read Total Number of Elevation
    !*       Profiles etc.
        REWIND (IDMUNT(IDEM))

        IF (FT(J) /= 1) THEN
        !*          Open as standard formatted file
            OPEN(UNIT = IDMUNT(IDEM), FILE= DEMFIL(IDEM), ERR = 95)
            READ (IDMUNT(IDEM),51, ERR=97) &
            MAPN(J), FREEF(J), FILR1(J), PROCODE(J), FILR2(J), &
            SECTNL(J), MCTR(J), DEMLVL(J), ELEVPAT(J), &
            IPLAN(J), IZO(J), (MPROJ(J,L), L = 1,15), CUNIT(J), &
            ELUNIT(J), SIDZ(J), ((DMCNR(J,L,M),L=1,2),M=1,4), &
            ELEVMN(J), ELEVMX(J), CNTRC(J),ACCUC(J), &
            DXM(J), DYM(J),DCI(J), NROW(J), NPROF(J), LPRIM(J), &
            LPINT(J), SPRIM(J), SPINT(J), &
            DDATE(J), DINSP(J), INSPF(J), DVALD(J), SUSF(J), &
            VDAT(J), NADD(J), EDITN(J), PVOID(J)
        ELSE
        !*          For FT(J) = 1 (no record delimiters) open as direct access file
            OPEN(UNIT = IDMUNT(IDEM), FILE= DEMFIL(IDEM), &
            RECL = 1024, &
            ACCESS = 'DIRECT', &
            FORM ='FORMATTED', &
            ERR = 95)
            READ (IDMUNT(IDEM), FMT=51, REC = 1, ERR=97) &
            MAPN(J), FREEF(J), FILR1(J), PROCODE(J), FILR2(J), &
            SECTNL(J), MCTR(J), DEMLVL(J), ELEVPAT(J), &
            IPLAN(J), IZO(J), (MPROJ(J,L), L = 1,15), CUNIT(J), &
            ELUNIT(J), SIDZ(J), ((DMCNR(J,L,M),L=1,2),M=1,4), &
            ELEVMN(J), ELEVMX(J), CNTRC(J), ACCUC(J), &
            DXM(J),DYM(J),DCI(J), NROW(J), NPROF(J), LPRIM(J), &
            LPINT(J), SPRIM(J), SPINT(J), &
            DDATE(J), DINSP(J), INSPF(J), DVALD(J), SUSF(J), &
            VDAT(J), NADD(J), EDITN(J), PVOID(J)
        END IF

        51 FORMAT(2A40, A55, 2A1, A3, A4, 2I6, &
        &           2I6, 15D24.15, I6, &
        &           2I6, 4(2D24.15), 2D24.15, D24.15, &
        I6, 2E12.6, E12.6, 2I6, 2(I5,I1), &
        &           2I4, A1, I1, I2, &
        &           2I2, 2I4)


    !*       Assign DEM zone from Record A to IZOND(NDEM) array
        IF (IZO(IDEM) /= 0) THEN
            IZOND(IDEM) = IZO(IDEM)
        END IF

    !*       Open Temporary Direct Access Terrain Files and Index Files
        OPEN (IDRUNT(IDEM),FILE=DIRFIL(IDEM), &
        ACCESS='DIRECT',RECL=LREC_DIR)
        OPEN (IDXUNT(IDEM),FILE=IDXFIL(IDEM))


    !*       For Each Profile in the DEM File
        RC = 1

    !*       Check for blank (0) NADD from DEM Record Type A
    !*       This was already done in CHKADJ based on DEMCHK read,
    !*       but needs to be checked again here for new read.
        IF (NADD(IDEM) == 0 .AND. IPLAN(IDEM) == 0) THEN
        !*          Assume WGS72 for LAT files
            NADD(IDEM) = 2
        ELSE IF (NADD(IDEM) == 0 .AND. IPLAN(IDEM) == 1) THEN
        !*          Assum NAD27 for UTM files
            NADD(IDEM) = 1
        END IF

        IF ( .NOT. GOTDOMFLG) THEN
        !*          No user-specified domain; skip domain shift calcs
            XDMNSHFT = 0.0D0
            YDMNSHFT = 0.0D0
            XDMXSHFT = 0.0D0
            YDMXSHFT = 0.0D0
            GO TO 777
        END IF
    !*
    !*       Determine whether to apply NAD shift to Domain
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

    !*       Apply NAD shift if necessary, based on type of DEM data
        IF (TYPDEM == 'LAT') THEN
        !*          Apply datum shift in arc-seconds for LAT files
            XDMNSHFT = DOMLL(2,1) + (XDMNDIFS*NADFLG)
            YDMNSHFT = DOMLL(1,1) + (YDMNDIFS*NADFLG)
            XDMXSHFT = DOMLL(2,3) + (XDMXDIFS*NADFLG)
            YDMXSHFT = DOMLL(1,3) + (YDMXDIFS*NADFLG)
        ELSE IF (TYPDEM == 'UTM') THEN
        !*          Apply total shift in meters for UTM files
            XDMNSHFT = XDMIN + (XDMNDIFM*NADFLG)
            YDMNSHFT = YDMIN + (YDMNDIFM*NADFLG)
            XDMXSHFT = XDMAX + (XDMXDIFM*NADFLG)
            YDMXSHFT = YDMAX + (YDMXDIFM*NADFLG)
            IF (NADFLG == 0.0D0) THEN
            !*             Use original domain zones if no NAD shift
                ZMINSHFT = ZONMIN
                ZMAXSHFT = ZONMAX
            ELSE
            !*             Use UTM zones based on shifted domain
                ZMINSHFT = ZONMIN_SHFT
                ZMAXSHFT = ZONMAX_SHFT
            END IF
        END IF

    !*       Loop through profiles to extract data within domain
    !*       for this DEM file:

        777 CONTINUE

    !*       Allocate array for number of nodes, NUMNOD(NPROF(IDEM))
    !*       First deallocate if needed
        IF (ALLOCATED(NUMNOD)) DEALLOCATE(NUMNOD)
        ALLOCATE (NUMNOD(NPROF(IDEM)))

        DO IPROF = 1, NPROF(IDEM)

        !*          Initialize Variables
            NEWPRO = .TRUE.
            XNODEI = 0.0D0
            YNODEI = 0.0D0
            NUMNOD(IPROF) = 0

        !*          Read Elevation From Each Profile Record.
            CALL GETPRO(IDEM,IPROF,IERR,RC)

        !*          Check for error condition from GETPRO
            IF (IERR /= 0) GO TO 999

        !*          Check to see if this profile lies between the min
        !*          and maximum horizontal coords (LONG or UTMX)
            IF ( .NOT. GOTDOMFLG) THEN
            !*             No user-specified domain; use all profiles in data files
                GO TO 888
            END IF

            IF (TYPDEM == 'LAT' .AND. IPLAN(IDEM) == 0) THEN
            !*             DEM/DOMAIN Type matches data file type
                IF (XBASE(IPROF) < XDMNSHFT) THEN
                    CYCLE                        ! Cycle to next profile
                ELSE IF (XBASE(IPROF) > XDMXSHFT) THEN
                    EXIT                         ! Exit profile loop
                END IF
            ELSE IF (TYPDEM == 'UTM' .AND. IPLAN(IDEM) == 1) THEN
            !*             DEM/DOMAIN Type matches data file type
                IF (ZMINSHFT == ZMAXSHFT) THEN
                    IF (XBASE(IPROF) < XDMNSHFT) THEN
                        CYCLE                      ! Cycle to next profile
                    ELSE IF (XBASE(IPROF) > XDMXSHFT) THEN
                        EXIT                       ! Exit profile loop
                    END IF
                ELSE IF (IZO(IDEM) == ZMINSHFT) THEN
                    IF (XBASE(IPROF) < XDMNSHFT) THEN
                        CYCLE                      ! Cycle to next profile
                    END IF
                ELSE IF (IZO(IDEM) == ZMAXSHFT) THEN
                    IF (XBASE(IPROF) > XDMXSHFT) THEN
                        EXIT                       ! Exit profile loop
                    END IF
                END IF
            ELSE IF (TYPDEM == 'UTM' .AND. IPLAN(IDEM) == 0) THEN
            !*             DEM/DOMAIN Type does NOT match data file type, convert xbase
            !*             Calculate the Coords in UTM relative to NADA ellipsoid

                SELECT CASE (NADD(IDEM))
                CASE (0)
                ISPHERE = 4   ! UTM/LAT-LONG conversions based on GRS80 ellipsoid
                CASE (2:4)
                ISPHERE = 4   ! UTM/LAT-LONG conversions based on GRS80 ellipsoid
                CASE (1,5:6)
                ISPHERE = 0   ! UTM/LL conversions based on Clarke 1866 ellipsoid
                CASE DEFAULT
                ISPHERE = 4   ! DEFAULT CASE shouldn't occur
                END SELECT

                ARGE  = XBASE(IPROF)
                ARGN  = YBASE(IPROF)
                IZONE = 0
                XARG  = 0.0D0
                YARG  = 0.0D0
                CALL UTMGEO (555,0,IZONE,XARG,YARG,ARGE,ARGN,ISPHERE)
                XBUTM = XARG
                YBUTM = YARG
                IF (ZMINSHFT == ZMAXSHFT) THEN
                    IF (XBUTM < XDMNSHFT) THEN
                        CYCLE                     ! Cycle to next profile
                    ELSE IF (XBUTM > XDMXSHFT) THEN
                        EXIT                      ! Exit profile loop
                    END IF
                ELSE IF (IZONE == ZMINSHFT) THEN
                    IF (XBUTM < XDMNSHFT) THEN
                        CYCLE                     ! Cycle to next profile
                    END IF
                ELSE IF (IZONE == ZMAXSHFT) THEN
                    IF (XBUTM > XDMXSHFT) THEN
                        EXIT                      ! Exit profile loop
                    END IF
                END IF
            ELSE IF (TYPDEM == 'LAT' .AND. IPLAN(IDEM) == 1) THEN
            !*             DEM/DOMAIN Type does NOT match data file type, convert xbase
            !*             Calculate the Coords in Lat/Lon relative to NADA ellipsoid

                SELECT CASE (NADD(IDEM))
                CASE (0)
                ISPHERE = 4   ! UTM/LAT-LONG conversions based on GRS80 ellipsoid
                CASE (2:4)
                ISPHERE = 4   ! UTM/LAT-LONG conversions based on GRS80 ellipsoid
                CASE (1,5:6)
                ISPHERE = 0   ! UTM/LL conversions based on Clarke 1866 ellipsoid
                CASE DEFAULT
                ISPHERE = 4   ! DEFAULT CASE shouldn't occur
                END SELECT

                XARG  = XBASE(IPROF)
                YARG  = YBASE(IPROF)
                IZDUM = 0
                ARGE  = 0.0D0
                ARGN  = 0.0D0
                CALL UTMGEO (333,IZO(IDEM),IZDUM,XARG,YARG, &
                ARGE,ARGN,ISPHERE)
                XBLON = ARGE
                YBLAT = ARGN
                IF (XBLON < XDMNSHFT) THEN
                    CYCLE                        ! Cycle to next profile
                ELSE IF (XBLON > XDMXSHFT) THEN
                    EXIT                         ! Exit profile loop
                END IF
            END IF

            888 CONTINUE

        !*          For Each Node in this Profile
            DO INOD = 1, NODES(IPROF)

            !*             Determine the Coordinates of this Point (L/L)   ---  CALL GETNOD
                CALL GETNOD(IPROF,INOD)

                IF ( .NOT. GOTDOMFLG) THEN
                !*                No user-specified domain; use all nodes in each profile

                !*                Increment the Number of Nodes in this Profile
                !*                Inside the Domain
                    NUMNOD(IPROF) = NUMNOD(IPROF)+1

                    IF (NEWPRO) THEN

                    !*                   NUMPRF is the number of profiles written to
                    !*                   the direct access file.
                        NUMPRF(IDEM) = NUMPRF(IDEM) + 1

                        NSTART = INOD
                        NEWPRO = .FALSE.

                        XNODEI = XNODE
                        YNODEI = YNODE

                    END IF

                ELSE

                !*                Check to See if This Node Lies Between the Min
                !*                and Maximum vertical Coords (LAT or UTMY)

                    IF (TYPDEM == 'UTM' .AND. IPLAN(IDEM) == 0) THEN
                    !*                   DEM/DOMAIN Type does NOT match data file type, convert xbase
                    !*                   Calculate the Coords in UTM relative to NADA ellipsoid

                        SELECT CASE (NADD(IDEM))
                        CASE (0)
                        ISPHERE = 4   ! UTM/LAT-LONG conversions based on GRS80 ellipsoid
                        CASE (2:4)
                        ISPHERE = 4   ! UTM/LAT-LONG conversions based on GRS80 ellipsoid
                        CASE (1,5:6)
                        ISPHERE = 0   ! UTM/LL conversions based on Clarke 1866 ellipsoid
                        CASE DEFAULT
                        ISPHERE = 4   ! DEFAULT CASE shouldn't occur
                        END SELECT

                        ARGE  = XBASE(IPROF)
                        ARGN  = YNODE
                        IZONE = 0
                        XARG  = 0.0D0
                        YARG  = 0.0D0
                        CALL UTMGEO (555,0,IZONE,XARG,YARG,ARGE,ARGN, &
                        ISPHERE)
                        XNUTM = XARG
                        YNUTM = YARG

                    ELSE IF (TYPDEM == 'LAT' .AND. IPLAN(IDEM) == 1) THEN
                    !*                   DEM/DOMAIN Type does NOT match data file type, convert xbase
                    !*                   Calculate the Coords in Lat/Lon relative to NADA ellipsoid

                        SELECT CASE (NADD(IDEM))
                        CASE (0)
                        ISPHERE = 4   ! UTM/LAT-LONG conversions based on GRS80 ellipsoid
                        CASE (2:4)
                        ISPHERE = 4   ! UTM/LAT-LONG conversions based on GRS80 ellipsoid
                        CASE (1,5:6)
                        ISPHERE = 0   ! UTM/LL conversions based on Clarke 1866 ellipsoid
                        CASE DEFAULT
                        ISPHERE = 4   ! DEFAULT CASE shouldn't occur
                        END SELECT

                        XARG  = XBASE(IPROF)
                        YARG  = YNODE
                        IZDUM = 0
                        ARGE  = 0.0D0
                        ARGN  = 0.0D0
                        CALL UTMGEO (333,IZO(IDEM),IZDUM,XARG,YARG, &
                        ARGE,ARGN,ISPHERE)
                        XBLON = ARGE
                        YBLAT = ARGN
                    END IF

                    IF ((TYPDEM == 'UTM' .AND. IPLAN(IDEM) == 0 .AND. &
                    YNUTM >= YDMNSHFT .AND. &
                    YNUTM <= YDMXSHFT) .OR. &

                    (TYPDEM == 'LAT' .AND. IPLAN(IDEM) == 1 .AND. &
                    YBLAT >= YDMNSHFT .AND. &
                    YBLAT <= YDMXSHFT) .OR. &

                    (YNODE >= YDMNSHFT .AND. &
                    YNODE <= YDMXSHFT) ) THEN

                    !*                   This node is within the domain

                    !*                   Increment the Number of Nodes in this Profile
                    !*                   Inside the Domain
                        NUMNOD(IPROF) = NUMNOD(IPROF)+1

                        IF (NEWPRO) THEN

                        !*                      NUMPRF is the number of profiles written to
                        !*                      the direct access file.
                            NUMPRF(IDEM) = NUMPRF(IDEM) + 1

                            NSTART = INOD
                            NEWPRO = .FALSE.

                            XNODEI = XNODE
                            YNODEI = YNODE

                        END IF

                    ELSE IF ((TYPDEM == 'UTM' .AND. IPLAN(IDEM) == 0 .AND. &
                        YNUTM > YDMXSHFT) .OR. &

                        (TYPDEM == 'LAT' .AND. IPLAN(IDEM) == 1 .AND. &
                        YBLAT > YDMXSHFT) .OR. &

                        (YNODE > YDMXSHFT) ) THEN

                    !*                   This node is beyond the domain

                        EXIT                    ! Exit loop on nodes

                    END IF

                END IF

            !           End Loop on Nodes
            END DO

        !*          If there were any nodes in this profile that were inside
        !*          the domain, determine record numbers and write out data
        !*          to direct access and index files
            IF (NUMNOD(IPROF) > 0) THEN
            !*             Write the baseline coords and number of nodes
            !*             in this profile to the master index file
                WRITE (IDXUNT(IDEM),IDXFRM) XNODEI, YNODEI, &
                NUMNOD(IPROF), IZO(IDEM), MAXEL(IPROF)

            !*             For each node in the profile inside the domain
                DO N = 1, NUMNOD(IPROF)

                !*                Calculate record number for the node
                    JRECRD = JRECRD + 1

                !*                Extract elevation for this node from array
                    ELV = DBLE(ZDEM(NSTART-1+N))

                ! ---             Convert elevation units and apply other adjustments, except for missing
                    IF (ELV > -9000.0D0) THEN
                    !*                   Apply vertical resolution factor from header (DCI) to elevations
                    !                    from file, adjust for elevation of local datum (LOCEL), and
                    !                    convert from feet to meters, if necessary
                        IF (ELUNIT(IDEM) == 1) THEN
                        !                       Convert from feet to meters
                            ELV = ((ELV* DCI(IDEM))+ LOCEL(IPROF))* 0.3048D0
                        ELSE
                        !                       Elevations in meters
                            ELV = (ELV* DCI(IDEM)) + LOCEL(IPROF)
                        END IF
                    END IF

                !*                Write the elevations to a direct access file.
                    CALL WRITEZ (IDEM,JRECRD,ELV)

                !              End Loop on Nodes
                END DO

            END IF

        !*       End loop on profiles
        END DO

        IF (GOTDOMFLG) THEN
        ! ---       Write information to DOMDETAIL.OUT debug file
        !           regarding direct access files
            IF (IDEM == 1) THEN
                WRITE(DOMK,50) NUMDEM
                50 FORMAT('From INITER_DEM:', &
                //'Information on Direct Access Files Within ', &
                'User-specified Domain:', &
                //'No. of DEM Files = ',I6/)
            END IF
            WRITE(DOMK,60) IDEM, NUMPRF(IDEM)
            60 FORMAT('Direct Access File for DEM File No. ',I6, &
            ' Contains ',I7,' Profiles.')
        ! ---       If direct access is not empty, NUMPRF(IDEM) > 0,
        !           loop through profiles to print out first and last
            IF (NUMPRF(IDEM) > 0) THEN
                L_FirstProf = .TRUE.
                DO K = 1, NPROF(IDEM)
                    IF (L_FirstProf .AND. NUMNOD(K) > 0) THEN
                    ! ---                This is the first non-empty profile,
                    !                    or the first profile within the direct access file
                        WRITE(DOMK,70) XBASE(K),YBASE(K),NUMNOD(K)
                        70 FORMAT(/' XBASE, YBASE and NODES for First', &
                        ' Profile: ',2F16.4,I7)
                        L_FirstProf = .FALSE.
                    ELSE IF (( .NOT. L_FirstProf .AND. NUMNOD(K) == 0)) THEN
                    ! ---                The last non-empty profile has just been passed,
                    !                    print out info for previous profile
                        WRITE(DOMK,80) XBASE(K-1),YBASE(K-1),NUMNOD(K-1)
                        80 FORMAT(' XBASE, YBASE and NODES for Last ', &
                        ' Profile: ',2F16.4,I7/)
                        EXIT
                    ELSE IF (K == NPROF(IDEM)) THEN
                    ! ---                This is the last profile in the DEM file, this
                    !                    must also be the last profile in the direct access file
                        WRITE(DOMK,80) XBASE(K),YBASE(K),NUMNOD(K)
                        EXIT
                    END IF
                END DO
            ELSE
                WRITE(DOMK,*)
            END IF
        END IF

        CLOSE (IDXUNT(IDEM))
        CLOSE (IDRUNT(IDEM))
        CLOSE (IDMUNT(IDEM))
        JRECRD = 0

        CYCLE DEMLOOP

        95 CONTINUE
    !        Error opening a DEM file

        RUNERR = .TRUE.
        WRITE(DUMMY,'("DEM#",I4)') MIN(IDEM,9999)
        CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

        CLOSE (IDXUNT(IDEM))
        CLOSE (IDRUNT(IDEM))
        CLOSE (IDMUNT(IDEM))
        JRECRD = 0

        CYCLE DEMLOOP

        97 CONTINUE
    !        Error reading a DEM file

        RUNERR = .TRUE.
        WRITE(DUMMY,'("DEM#",I4)') MIN(IDEM,9999)
        CALL ERRHDL(PATH,MODNAM,'E','510',DUMMY)

        CLOSE (IDXUNT(IDEM))
        CLOSE (IDRUNT(IDEM))
        CLOSE (IDMUNT(IDEM))
        JRECRD = 0

        CYCLE DEMLOOP

    !     End loop on dem files
    END DO DEMLOOP

!     Close DOMDETAIL.OUT debug file
    CLOSE(DOMK)

    WRITE(*,*) 'Exiting INITER_DEM'

    999 RETURN
    end subroutine

    SUBROUTINE GETPRO (IDM,IPROF,IERR, RC)
!***********************************************************************
!*       PURPOSE:  THIS SUBROUTINE WILL READ X,Y NODE OF THE BASELINE AND
!*              THE Z VALUES FROM A DEM PROFILE
!*
!*       PROGRAMMER: Jayant Hardikar, Roger Brode
!*
!*       DATE:    September 29, 1995
!*
!*       MODIFIED: June 30, 2008
!*
!*                Added error checking for maximum number of nodes,
!*                and modified read format to accept up to 5,000 nodes.
!*                Roger W. Brode, U.S. EPA, OAQPS, AQMG
!*
!*       INPUTS:  Raw Terrain Data (DEM) Files
!*
!*       OUTPUTS: Direct Access Binary Terrain Files
!*
!*       CALLED FROM:   MAIN
!***********************************************************************


!*    Variable Declarations
    USE TER_MAIN

    IMPLICIT NONE

    SAVE

    CHARACTER(12) :: MODNAM

    INTEGER :: IERR, IDM, IPROF, ROWNUM, COLNUM, COLZ
    INTEGER :: L, J1, J2, KK, RR, RC

    MODNAM = 'GETPRO'
    IERR   = 0

    NODES(IPROF) = 0

    IF (FT(IDM) /= 1) THEN
        READ (IDMUNT(IDM), 208, ERR = 99, END = 99) &
        ROWNUM, COLNUM, NODES(IPROF)
        208 FORMAT (3I6)

    !*       Deallocate and reallocate ZDEM array for this profile
        IF (ALLOCATED(ZDEM)) DEALLOCATE(ZDEM)
        ALLOCATE(ZDEM(NODES(IPROF)))
        BACKSPACE (IDMUNT(IDM))

        READ (IDMUNT(IDM),209,ERR=99,END=99) &
        ROWNUM, COLNUM, &
        NODES(IPROF), COLZ, XBASE(IPROF), YBASE(IPROF), &
        LOCEL(IPROF), MINEL(IPROF), MAXEL(IPROF), &
        (ZDEM(L), L=1,NODES(IPROF))
    !*        Read format supports up to 100,000 nodes per profile
        209 FORMAT (2I6,2I6,2D24.15,D24.15,2D24.15, &
        &             146I6,587(:/170I6),:/64I6,:)

    ELSE

        RC = RC + 1
        READ (IDMUNT(IDM),FMT=209, REC=RC, ERR = 99) &
        ROWNUM, COLNUM, NODES(IPROF)

    !*       Deallocate and reallocate ZDEM array for this profile
        IF (ALLOCATED(ZDEM)) DEALLOCATE(ZDEM)
        ALLOCATE(ZDEM(NODES(IPROF)))

        J1 = 1
        RR = 0
        IF (NODES(IPROF) > 146) THEN
            J2 = 146
            RR = INT((NODES(IPROF) - 146)/170 + 1)
            IF (MODULO((NODES(IPROF) - 146),170) == 0) RR = RR - 1
        ELSE
            J2 = NODES(IPROF)
        END IF

        READ (IDMUNT(IDM),FMT=209, REC=RC, ERR = 99) &
        ROWNUM, COLNUM, &
        NODES(IPROF), COLZ, XBASE(IPROF), YBASE(IPROF), &
        LOCEL(IPROF), MINEL(IPROF), MAXEL(IPROF), &
        (ZDEM(L), L=J1,J2)

        IF (RR /= 0) THEN
            DO KK = 1 , RR
                RC = RC + 1
                J1 = J2 + 1
                J2 = J1 + 170 - 1
                IF (J2 > NODES(IPROF)) J2 = NODES(IPROF)
                READ (IDMUNT(IDM), FMT=211, REC=RC, ERR = 99) &
                (ZDEM(L), L=J1,J2)
                211 FORMAT(170I6)
            END DO
        END IF
    END IF

    GO TO 999

    99 RUNERR = .TRUE.
    IERR   = -1
    WRITE(DUMMY,'("DEM#",I4)') MIN(IDM,9999)
    CALL ERRHDL(PATH,MODNAM,'E','510',DUMMY)

    999 RETURN

    end subroutine

    SUBROUTINE GETNOD(IPR,INO)
!***********************************************************************
!*               GETNOD Module of AERMAP Terrain Preprocessor
!*
!*       PURPOSE: Determine the Coordinates of a DEM Point (L/L)
!*
!*       PROGRAMMER: Jayant Hardikar, Roger Brode
!*
!*       DATE:    September 29, 1995
!*
!*       MODIFIED: June 30, 2008
!*
!*                Corrected to use DYM, node spacing in 'Y' direction,
!*                to assign YNODE values in profile.
!*                Roger W. Brode, U.S. EPA, OAQPS, AQMG
!*
!*       INPUTS:  Profile Number, Node Number, Coordinates of Baseline
!*
!*       OUTPUTS: Coordinates of a DEM Point
!*
!*       CALLED FROM:   INITER
!***********************************************************************

!*    Variable Declarations
    USE TER_MAIN

    IMPLICIT NONE

    CHARACTER(12) :: MODNAM

    INTEGER :: INO, IPR

    MODNAM = 'GETNOD'

!*    For the baseline in a profile, the coords of a node are the same
!*    as the coords of the baseline
    IF (INO == 1) THEN
        XNODE = XBASE(IPR)
        YNODE = YBASE(IPR)

    !*    Otherwise, calculate the coords based on the profile and node number
    ELSE
        XNODE = XBASE(IPR)
        YNODE = YNODE + DYM(IDEM)
    END IF

    RETURN
    end subroutine
