PROGRAM TERSCI

! TERSCI v1.0 2015-05-05
!
! Reads DEM/NED data, and a user's requested lat-lon grid. Writes a file
! in SCICHEM 3.0 terrain file format
!
! Written by Bart Brashers, Ramboll-Environ, bbrasher@environcorp.com,
! The code is mostly based on USEPA's AERMAP 11103 program,
! which in turn re-used a lot of code from ISC.
!
!***********************************************************************
!*    MAIN Module of the AMS/EPA Regulatory Model Terrain Preprocessor
!*                               AERMAP
!*                         Version Dated 11103
!*
!*                            April 13, 2011
!*
!*             *** SEE AERMAP MODEL CHANGE BULLETIN MCB#3 ***
!*
!*     ON THE SUPPORT CENTER FOR REGULATORY AIR MODELS (SCRAM) WEBSITE
!*
!*                     http://www.epa.gov/scram001/
!*
!***********************************************************************
!
!*    Variable Declarations
  USE TER_MAIN
  USE CONTROL
  IMPLICIT NONE

  SAVE

  LOGICAL              :: L_FileExist, L_FIRST
  CHARACTER(12)        :: MODNAM
  INTEGER              :: IREC, ISRC, iZone, iSphere, iArg, ios2, i,j,k
  INTEGER              :: utm_zone  ! utm_zone function to derive utm_zone
  CHARACTER (len=256)  :: Command
  real (kind=8)        :: tmplat, tmplon, tmpx, tmpy

  MODNAM = 'MAIN'

!*    Initialize FileExist logical variable
  L_FileExist = .FALSE.
  L_FIRST     = .TRUE.

!--- Set some defaults for the purposes of this program

  EXTRACT = .TRUE.   ! i.e. CO TERRHGTS EXTRACT
  GOTDOMFLG = .TRUE. ! We'll specify one below
  FILLGAPS = .TRUE.  ! Even though I mostly expect to use NED data,
! which has no gaps.
  NADA = 4           ! default to NAD-83
  DOMTYP = "LAT"     ! default
  NUMDEM = 0         ! initialize
  NDEM = 0           ! initialize
  NSRC = 0
  NNET = 1

!--- Get the input file from the command line

  INPFIL = 'TERSCI.inp' ! default
  iArg = 0
  do while (iArg < COMMAND_ARGUMENT_COUNT())
     iArg = iArg + 1
     call GetArg(iArg,Command)
     if (trim(Command) == '-i') then
        iArg = iArg + 1
        call GetArg(iArg,INPFIL)
     else if (trim(Command) == '-h' .OR. trim(Command) == '--help') then
        call usage
     else if (trim(Command) == '--sample') then
        call sample_control_file
     else if (trim(Command) == '-v' .OR. trim(Command) == '--version') then
        write(*,*) 'TERSCI Version ',trim(VERSN)
        stop
     else
        write(*,*) "Error: unknown switch ",trim(Command)
        call usage
     end if
  end do

!--- Parse the control file, just to calculate NREC and NDEM

  call read_control( .false. )

!--- The rest of this code follows AERMAP very closely.

  OPEN(UNIT=IERUNT,FILE=INPFIL(1:index(INPFIL,'.', .TRUE. ))// &
       'ERRMSG.TMP',STATUS='REPLACE')

!   Open OUTPUT files                                    ---   CALL FILOPN
  CALL FILOPN

!   Allocate SETUP Array Storage
  CALL ALLSETUP

!   Variable Initializations                              ---   CALL VARINI
  CALL VARINI

!---  Re-read the control file to read the DEMFILs, now that ALLSETUP has
!     allocated the variables

  write(*,*) "Parsing ",trim(INPFIL)
  call flush(6)
  call read_control( .true. )
  RUN = .TRUE.       ! default from   RUNNOT

!     Get Date and Time using Fortran 90 functions          ---   CALL DATIME
  RUNDAT = ' '
  RUNTIM = ' '
  CALL DATIME (RUNDAT, RUNTIM)

!--- Set up the domain, normally called deep down the SETUP trail
!    If Domain type "lat-lon", the receptors need to be in UTM.
!    Convert them from evenly spaced in Lat-Lon to DISCCART in UTM.

  if (DOMTYP == "UTM") then
     k = 0
     do j = 1, IYM
        do i = 1, IXM
           XCOORD(i,1) = XDMIN + (i-1)*Spacing
           YCOORD(j,1) = YDMIN + (j-1)*Spacing
           k = k + 1
           AXR(k) = XCOORD(i,1)
           AYR(k) = YCOORD(j,1)
        end do
     end do
  end if  ! if (DOMTYP == "UTM") then

  if (DOMTYP == "LAT") then ! lon is neg = eastern hemisphere

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

     ZONMIN = utm_zone(1.0D0*XDMIN)
     ZONMAX = utm_zone(1.0D0*XDMAX) ! not sure if this is needed

     k = 0
     do j = 1, IYM
        do i = 1, IXM
           k = k + 1
           tmplon = 3600.0D0 * (XDMIN + (i-1)*Spacing) ! x-direction
           tmplat = 3600.0D0 * (YDMIN + (j-1)*Spacing) ! y-direction
           tmpx   = 0.0D0
           tmpy   = 0.0D0
! not sure if XCOORD or AXR are eventually used, so fill both
           call UTMGEO(555,ZONMIN,iZone,tmpx,tmpy,tmplon,tmplat,iSphere)
           XCOORD(i,1) = tmpx
           YCOORD(j,1) = tmpy
           AXR(k) = XCOORD(i,1)
           AYR(k) = YCOORD(j,1)
        end do
     end do

  end if   ! if (DOMTYP == "LAT") then

  XAUSER = AXR(1)
  YAUSER = AYR(1)
  XATERR = AXR(1)
  YATERR = AYR(1)
  ZATERR = ZONMIN

! --- Make the domain be slightly larger than need be, now that we've set the
!     'receptor' locations

  tmpx  =  XDMIN
  tmpy  =  XDMAX ! not really a y-coordinate, just a temp container
  XDMIN = tmpx - (tmpy - tmpx)* 0.1 ! make it 10% bigger
  XDMAX = tmpy + (tmpy - tmpx)* 0.1 ! make it 10% bigger
  tmpx  =  YDMIN
  tmpy  =  YDMAX
  YDMIN = tmpx - (tmpy - tmpx)* 0.1 ! make it 10% bigger
  YDMAX = tmpy + (tmpy - tmpx)* 0.1 ! make it 10% bigger

  IF ( .NOT. FATAL) THEN

! ---    Check for data type, 'DEM' or 'NED'
     IF (TYPDAT == 'DEM') THEN
!           Read each DEM file and analyze it.  Report any discrepancies with
!           respect to the USGS Standards for DEMs.          --- CALL DEMCHK

        CALL DEMCHK

        write(IOUNIT,*) ' '
        write(IOUNIT,*) 'Finished Reading Input DEM Data'
        write(IOUNIT,*) ' '

!*          Check for DEM file error
        IF (DEMERR) THEN
           WRITE(IOUNIT,*)' '
           WRITE(IOUNIT,*)'Error Occurred in DEMCHK'
           WRITE(IOUNIT,*)' '
           WRITE(*,*) ''
           WRITE(*,*) 'Error Occurred in DEMCHK'
           WRITE(*,*) ''
!*             Skip over data processing
           GO TO 99
        END IF

     ELSE IF (TYPDAT == 'NED') THEN
!           Read each NED file and analyze it.  Report any
!           discrepancies.                                --- CALL NEDCHK

        CALL NEDCHK

        write(IOUNIT,*) ' '
        write(IOUNIT,*) 'Finished Reading Input NED Data'
        write(IOUNIT,*) ' '

!*          Check for NED error
        IF (NEDERR) THEN
           WRITE(IOUNIT,*) ' '
           WRITE(IOUNIT,*) 'Error Occurred in NEDCHK'
           WRITE(IOUNIT,*) ' '
           WRITE(*,*) ''
           WRITE(*,*) 'Error Occurred in NEDCHK'
           WRITE(*,*) ''
!*             Skip over data processing
           GO TO 99
        END IF

     END IF

!*       Read interpolation factors for converting NAD27 to NAD83 in NADCON
!*       in NADCON 2.1, if needed

     IF ( .NOT. FATAL .AND. NADA /= 0 .AND. L_NeedNADCON) THEN

!*          Check for user-specified NADGRIDS pathname,
!*          and set length of pathname for call to NGRIDS
        IF ( .NOT. NADPATH) THEN
           ILEN_PATH = 0
           NADGRID_PATH = ''
        ELSE
           ILEN_PATH = LEN_TRIM(NADGRID_PATH)
        END IF

        CALL NGRIDS (NODATA, .TRUE. ,ILEN_PATH,NADGRID_PATH)

        IF (NODATA) THEN
!*             NADCON grid files not found; issue fatal error
!*             and skip over rest of data processing
           CALL ERRHDL(PATH,MODNAM,'E','365','NADGRIDS')
           FATAL = .TRUE.
           GO TO 99
        END IF

     ELSE
!*          NADCON grid files are not needed, either because
!*          NADA = 0 or because all file datums are consistent
!*          with NADA

        NODATA = .FALSE.

     END IF

!*       Open MAPPARAMS.OUT debug file
     OPEN (UNIT = MAPK, FILE = MAPPARAMS_FILE, STATUS = 'REPLACE')

! ---    Convert corner coordinates from native units (arc-seconds for geographic
!        or meters for UTM) in DMCNR to store all three formats (arc-seconds, degrees,
! ---    meters) in corner arrays                              --- CALL CNRCNV
     CALL CNRCNV

!*       Check for adjacency of DEM and NED files              --- CALL CHKADJ
     CALL CHKADJ

!*       Close MAPPARAMS.OUT debug file
     CLOSE(MAPK)

!*       Synchronize the Coordinate Systems of the Domain, if specified, and the
!*       Terrain File Type.                                    ---  CALL DOMCNV
     IF (GOTDOMFLG) THEN
        CALL DOMCNV
     END IF

!*       Determine Receptor X,Y Coordinates in Lat/Long        ---  CALL RECCNV
     CALL RECCNV

     IF (NUMSRC > 0) THEN
!*          Determine Source X,Y Coordinates in Lat/Long       ---  CALL SRCCNV
        CALL SRCCNV
     END IF

!*       If domain is specified, check to see that the extent of the
!*       receptors (and sources) are within the extents of the
!*       user-specified domain and the domain itself is within the
!*       supplied raw terrain files                            ---  CALL CHKEXT
     IF (GOTDOMFLG) THEN
        CALL CHKEXT
     END IF

!*       Find which DEM file each receptor falls within        --- CALL DEMREC
     CALL DEMREC

     IF (NUMSRC > 0) THEN
!*          Find which DEM file each source falls within       --- CALL DEMSRC
        CALL DEMSRC
     END IF

  END IF ! IF ( .NOT. FATAL) THEN

!*    Branch to skip processing due to data error (DEMERR or NEDERR)
99 CONTINUE

!*    Determine Number of Setup Messages by Message Type    ---   CALL TERRST
  CALL TERRST

  IF ( .NOT. RUN .OR. FATAL .OR. IWRN > 0) THEN

     WRITE (IOUNIT,*) 'TERSCI Version ',trim(VERSN)," ",RUNDAT," ",RUNTIM

!*       Write Out Summary Of Setup Error/Message Stats     ---   CALL SUMTBL

     write(IOUNIT,*)
     WRITE(IOUNIT,*) 'Message Summary For TERSCI Setup:'
     CALL SUMTBL

  END IF

  IF (FATAL) THEN
     WRITE (IOUNIT,*) "SETUP Finishes UN-successfully"
  ELSE
     WRITE (IOUNIT,*) "SETUP Finishes Successfully"
  END IF

  IF ( .NOT. FATAL .AND. RUN) THEN
!*       No Fatal Errors in Setup and RUN Option Selected

!*       Read Terrain Data from Raw Data Files and Write Out the
!*       Data to Direct Access Binary Terrain Data Files and
!*       Record Index Files

     WRITE(*,*) 'Initializing Terrain Data, This may take few a minutes...'
     call flush(6)

     IF (TYPDAT == 'DEM') THEN
!*          Initialize the terrain data for DEM files      ---   CALL INITER_DEM
        CALL INITER_DEM

     ELSE IF (TYPDAT == 'NED') THEN
!*          Initialize the terrain data for NED files      ---   CALL INITER_NED
        CALL INITER_NED

     END IF

     IF (RUNERR) GO TO 991

     WRITE(*,*)
!*       Loop Over All Receptors;
     DO IREC = 1, NUMREC

        WRITE(*,909) IREC, NUMREC
909     FORMAT('Now Processing Receptor ',I7, ' of ',I7)

!*          If the user did not supply receptor elevations, then determine
!*          receptor elevations by a 2-D interpolation between the 4 closest
!*          raw terrain data points.  Loop through DEM files and keep the
!*          first non-missing elevation.                     ---   CALL RECELV
        IF (EXTRACT .AND. .NOT. RUNERR) THEN
!*             Set logical flag for first DEM file that contains receptor
           L_FIRST = .TRUE.
!*             Set logical flag to identify receptors in gaps within files
           GAPSFOUND_IN = .FALSE.
!*             Loop through DEM files
           DO IDEM = 1, NUMDEM

              IF (IRIN(IREC,IDEM) > 0) THEN
!*                   This DEM file contains the receptor;
!*                   call subroutine RECELV to calculate elevation
                 CALL RECELV(IREC,IDEM)

                 IF (AZELEV(IREC) > -9998.99D0) THEN
!*                      Non-missing elevation; if first DEM file
!*                      for this receptor, then EXIT from DEM loop
                    IF (L_FIRST .AND. IRIN(IREC,IDEM) == 1) THEN
                       EXIT
                    ELSE IF(L_FIRST .AND. IRIN(IREC,IDEM) == 2) THEN
!*                         Non-missing elevation is from a NAD-shift gap
                       EXIT
                    ELSE IF (IRIN(IREC,IDEM) == 3) THEN
!*                         Non-missing elevation is from an internal gap;
!*                         this may be replaced by a subsequent file.
                       L_FIRST = .FALSE.
                       IF ( .NOT. GAPSFOUND_IN) THEN
!*                            This is the first internal gap for this receptor;
!*                            set flag and increment counter
                          GAPSFOUND_IN = .TRUE.
                          NRGAP_IN     = NRGAP_IN + 1
                       END IF
                       CYCLE
                    ELSE IF ( .NOT. L_FIRST .AND. &
                         (IRIN(IREC,IDEM) == 1 &
                         .OR. IRIN(IREC,IDEM) == 2)) THEN
!*                         This was not the first DEM file for this
!*                         receptor; this means that the first file
!*                         returned a missing elevation or was a gap
!*                         receptor.
!*                         Issue warning message that elevation came
!*                         from a subsequent DEM file; then EXIT loop
                       NRSUBS = NRSUBS + 1
                       WRITE(DUMMY,'(I8)') IREC
                       CALL ERRHDL(PATH,MODNAM,'W','405',DUMMY)
                       EXIT
                    END IF
                 ELSE
!*                      Elevation was missing (-9999.0); set flag for
!*                      first DEM file to false, and cycle to next file
!*                      unless IRIN(IREC,IDEM) = 2, which means receptor
!*                      was a "gap" receptor due to NAD shift, assigned
!*                      to file on the 2nd pass.
                    L_FIRST = .FALSE.
                    IF (IRIN(IREC,IDEM) == 1 .OR. &
                         IRIN(IREC,IDEM) == 3) THEN
!*                         Missing elevation may be replaced by
!*                         subsquent file
                       IF ( .NOT. GAPSFOUND_IN) THEN
!*                            This is the first internal gap for this receptor;
!*                            set flag and increment counter
                          GAPSFOUND_IN = .TRUE.
                          NRGAP_IN     = NRGAP_IN + 1
                       END IF
                       CYCLE
                    ELSE IF (IRIN(IREC,IDEM) == 2) THEN
!*                         Missing elevation for NAD-shift gap receptor;
!*                         no other file can replace this case so exit
                       EXIT
                    END IF
                 END IF

              END IF

           END DO        ! DEM file loop

           IF (IDEM >= 1 .AND. IDEM <= NUMDEM) THEN
! ---             DEM loop was exited
              IF (IRIN(IREC,IDEM) == 1) THEN
                 IF (AZELEV(IREC) < -9998.99D0) THEN
!*                      Receptor elevation is still missing, write warning,
!*                      set flag, and increment counters
                    MISSFOUND = .TRUE.
                    NRMISS    = NRMISS + 1
                    WRITE(DUMMY,'(I8)') IREC
                    CALL ERRHDL(PATH,MODNAM,'W','410',DUMMY)
                 END IF
              ELSE IF (IRIN(IREC,IDEM) == 2) THEN
                 IF (AZELEV(IREC) < -9998.99D0) THEN
!*                      Receptor elevation is still missing, write warning,
!*                      set flag, and increment counters
                    MISSFOUND = .TRUE.
                    NRMISS    = NRMISS + 1
                    WRITE(DUMMY,'(I8)') IREC
                    CALL ERRHDL(PATH,MODNAM,'W','410',DUMMY)
                 ELSE IF (LRec_FILLED(IREC,IDEM)) THEN
                    NRFILLED = NRFILLED + 1
                    WRITE(DUMMY,'(I8)') IREC
                    CALL ERRHDL(PATH,MODNAM,'W','402',DUMMY)
                 END IF
              ELSE IF (IRIN(IREC,IDEM) == 3) THEN
                 IF (AZELEV(IREC) < -9998.99D0) THEN
!*                      Receptor elevation is still missing, write warning,
!*                      set flag, and increment counters
                    MISSFOUND = .TRUE.
                    NRMISS    = NRMISS + 1
                    WRITE(DUMMY,'(I8)') IREC
                    CALL ERRHDL(PATH,MODNAM,'W','410',DUMMY)
                 ELSE IF (LRec_FILLED(IREC,IDEM)) THEN
                    NRFILLED = NRFILLED + 1
                    WRITE(DUMMY,'(I8)') IREC
                    CALL ERRHDL(PATH,MODNAM,'W','403',DUMMY)
                 END IF
              END IF
           ELSE
!*                Completed DEM loop without exiting; this means that
!*                the receptor was only located within an internal gap
              IF (AZELEV(IREC) < -9998.99D0) THEN
!*                   Receptor elevation is still missing, write warning,
!*                   set flag, and increment counters
                 MISSFOUND = .TRUE.
                 NRMISS    = NRMISS + 1
                 WRITE(DUMMY,'(I8)') IREC
                 CALL ERRHDL(PATH,MODNAM,'W','410',DUMMY)
              ELSE
                 NRFILLED = NRFILLED + 1
                 WRITE(DUMMY,'(I8)') IREC
                 CALL ERRHDL(PATH,MODNAM,'W','403',DUMMY)
              END IF
           END IF

        END IF

     END DO              ! Receptor loop

!*       Check for receptors located within gaps inside files;
!*       reset logical flag if needed
     IF (NRGAP_IN > 0) THEN
        GAPSFOUND_IN = .TRUE.
     END IF

!*  Write the receptor pathway data including receptor elevations to
!*  the output file. Fields are only allowed to be 6 chars wide.
     k = 0
     do IREC = 1, NUMREC
        k = k+1
        IF( k == 12)THEN
           if (AZELEV(IREC) <= 9999.9) then
              WRITE(IRUNIT,'(f6.1)',ADVANCE='YES') AZELEV(IREC)
           else
              WRITE(IRUNIT,'(i6)',ADVANCE='YES') NINT( AZELEV(IREC) )
           end if
           k = 0
        ELSE
           if (AZELEV(IREC) <= 9999.9) then
              WRITE(IRUNIT,'(f6.1)',ADVANCE='NO') AZELEV(IREC)
           else
              WRITE(IRUNIT,'(i6)',ADVANCE='NO')  NINT( AZELEV(IREC) )
           end if
        END IF
     end do

991  CONTINUE

  END IF

!*    Call routine to print page header                       ---   CALL HEADER
  CALL HEADER

!*    Print Summary of the Input Data                         ---   CALL INPSUM
  CALL INPSUM

!*    Call routine to print page header
  CALL HEADER

  WRITE (IOUNIT,9114)
9114 FORMAT(/1X,'*** Message Summary For TERSCI Execution ***'/)
!*    Determine Number of Errors/Messages by Message Type     ---   CALL TERRST
  CALL TERRST
!*    Write Summary of Message Stats for Model Execution      ---   CALL SUMTBL
  CALL SUMTBL

  IF (FATAL) THEN
     write(*,*) "TERSCI finished un-successfully"
  ELSE
     write(*,*) "TERSCI finished normally"

  END IF

!     Delete the direct access files of terrain data.
  DO IDEM = 1,NUMDEM
     L_FileExist = .FALSE.
     INQUIRE(IDRUNT(IDEM),EXIST=L_FileExist)
     IF (L_FileExist) THEN
        OPEN (IDRUNT(IDEM),FILE=DIRFIL(IDEM), &
             ACCESS='DIRECT',RECL=LREC_DIR)
        OPEN (IDXUNT(IDEM),FILE=IDXFIL(IDEM))

        CLOSE (IDRUNT(IDEM),STATUS='DELETE')
        CLOSE (IDXUNT(IDEM),STATUS='DELETE')
     END IF
  END DO

  CLOSE(IERUNT,STATUS='DELETE')

  STOP
END PROGRAM TERSCI


!*----------------------------------------------------------------------
!*    SUBPROGRAM UNITS FOLLOW..

SUBROUTINE VARINI
!***********************************************************************
!                 VARINI Module of AERMOD

!        PURPOSE: To Initialize Variables for Setup

!        PROGRAMMER: Roger Brode, James Paumier, Jayant Hardikar

!        DATE:    September 29, 1995

!        INPUTS:  None

!        OUTPUTS: Initialized Variables

!        CALLED FROM:   MAIN
!***********************************************************************

!     Variable Declarations
  USE TER_MAIN

  IMPLICIT NONE

  SAVE

  CHARACTER(12) :: MODNAM
  integer ::   i               ! BAB used with index()

! --- Variable Initializations
  MODNAM = 'VARINI'

! --- Initialize real (kind=8) constants
  PI = 4.0D0*DATAN(1.0D0)
  DTORAD = PI/180.0D0
  RTODEG = 180.0D0/PI

! --- Initialize counters to zero
  IDEM   = 0
  IPNUM  = 0
  IPPNUM = 0

! --- Initialize counters for receptors and sources not assigned to any
!     DEM/NED files on the first pass (potential "gap" receptors/sources)
  NSGAP    = 0
  NRGAP    = 0
! --- Initialize counters for receptors and sources located within gaps
!     inside DEM/NED files
  NSGAP_IN = 0
  NRGAP_IN = 0
! --- Initialize counters for receptors and sources that are assigned to
!     DEM/NED file on the 2nd pass; these represent "gaps" due to NAD
!     conversions
  NSGAP2   = 0
  NRGAP2   = 0
! --- Initialize counters for receptors and sources located within gaps
!     inside DEM/NED files
  NSGAP3   = 0
  NRGAP3   = 0
! --- Initialize counters for receptors and sources located within gaps
!     inside DEM/NED files, but with elevations from subsequent files
  NSSUBS   = 0
  NRSUBS   = 0
! --- Initialize counters for "gap" receptors and sources that are "filled"
!     based on closest nodes with FILLGAPS option
  NSFILLED = 0
  NRFILLED = 0
! --- Initialize logical variable identifying gap receptors and sources with
!     "filled" elevations [Lrec_FILLED(NREC,NDEM) and LSrc_FILLED(NSRC,NDEM)]
  LRec_FILLED = .FALSE.
  LSrc_FILLED = .FALSE.
! --- Initialize counters for receptors and sources with missing elevations
  NSMISS   = 0
  NRMISS   = 0

! --- Initialize the Logical Control Variables
  FATAL  = .FALSE.
  ISTART = .FALSE.
  IFINIS = .TRUE.
  ERRLST = .FALSE.
  RUN    = .FALSE.
  RECERR = .FALSE.
  ECHO   = .TRUE.
  RUNERR = .FALSE.

  HILLDBG = .FALSE.
  RECDBG  = .FALSE.
  SRCDBG  = .FALSE.

  DEMERR = .FALSE.
  NEDERR = .FALSE.

  GAPSFOUND    = .FALSE.
  GAPSFOUND_IN = .FALSE.
  MISSFOUND    = .FALSE.
  FILLGAPS     = .FALSE.

  NADPATH = .FALSE.

!*    File-dependent processing controls
  L_DEMCHECK       = .FALSE.
  L_UserElevUnits  = .FALSE.
  L_TiffDebug      = .FALSE.
  L_NEDSkip        = .FALSE.

  DOMFIX = .FALSE.

!*    Initialize default debug output filenames:
!*    Automatically-generated files:
  i = index(INPFIL,".", .TRUE. )
  MAPPARAMS_FILE = INPFIL(1:i)//'MAPPARAMS.OUT'
  MAPDET_FILE    = INPFIL(1:i)//'MAPDETAIL.OUT'
  DOMDET_FILE    = INPFIL(1:i)//'DOMDETAIL.OUT'

!*    User-specified options debug files:
  CALCHC_FILE  = INPFIL(1:i)//'CALCHCDET.OUT'
  RECDET_FILE  = INPFIL(1:i)//'RECDETAIL.OUT'
  RECNDEM_FILE = INPFIL(1:i)//'RECNDEM.OUT'
  RECELV_FILE  = INPFIL(1:i)//'RECELV.OUT'
  SRCDET_FILE  = INPFIL(1:i)//'SRCDETAIL.OUT'
  SRCNDEM_FILE = INPFIL(1:i)//'SRCNDEM.OUT'
  SRCELV_FILE  = INPFIL(1:i)//'SRCELV.OUT'

!*    Optional TIFF Debug file
  TiffDbgFil   = 'undefined'

!     Initialize counter for number of receptors
  NUMREC = 0
!*    Initialize array for assigning receptors to DEM files,
!*    IRIN(NREC,NDEM), to 0
  IRIN = 0
!*    Initialize other receptor counters and flags
  ISTA = .FALSE.
  IEND = .FALSE.
  IRXR = 0
  IRYR = 0
  IRZE = 0
  IRZF = 0
  NEWID  = .TRUE.

!*    Initialize EXTRACT to .TRUE., default is for AERMAP to EXTRACT elevations
  EXTRACT = .TRUE.
  FLGPOL = .FALSE.

!*    Initilize elevation units character strings
  REELEV = 'METERS'
  SOELEV = 'METERS'

!*    Initialize counter for number of sources
  NUMSRC = 0
!*    Initialize array for assigning sources to DEM files,
!*    ISIN(NSRC,NDEM), to 0
  ISIN   = 0

!BAB      INPFIL = ' '  ! we already read this from getarg()
  OUTFIL = ' '
  RECFIL = ' '
  SRCFIL = ' '

  TYPDAT = ' '

!     Initialize the Outputs
  TITLE1  = ' '
  TITLE2  = ' '
  DOMCARD = ' '
  DOMADJ  = ' '
  ANCHCRD = ' '
  HGTCARD = ' '

!     Initialize the Number of Error/Warning/Informational Messages, and
!     The Number of Fatal Errors.
  IERROR = 0
  NFATAL = 0
  NWARN  = 0


!***********************************************************************
!*    Initialize Receptor Arrays
!***********************************************************************

  AXR    = 0.0D0
  AYR    = 0.0D0
  AZELEV = 0.0D0
  AZFLAG = 0.0D0
  AZHILL = 0.0D0
  NDXARC = 0
  ARCID  = '        '

!***********************************************************************
!*    Initialize Source Arrays
!***********************************************************************

  AXS  = 0.0D0
  AYS  = 0.0D0
  AZS  = 0.0D0

!***********************************************************************
!*    Initialize Setup Status Arrays
!***********************************************************************

  ICSTAT = 0
  ISSTAT = 0
  IRSTAT = 0
  IOSTAT = 0

  RETURN
end subroutine VARINI

SUBROUTINE FILOPN
!***********************************************************************
!*                PCCODE Module of the AMS/EPA Regulatory Model - AERMOD

!*       PURPOSE: Controls Retrieving Input and Output File Names From
!*                an ASCII text file, FILENAMES.DAT. Opens those files.

!        PROGRAMMER: Peter Eckhoff

!        MODIFIED:   Remove non-standard option for
!                    CARRIAGECONTROL='Fortran' to control
!                    page feed in aermod.out file.  ASCII form
!                    feed character is used in subroutine HEADER
!                    to insert page feed instead of using Fortan
!                    carriage control.
!                    R.W. Brode, USEPA/OAQPS/AQMG, February 9, 2009

!*       MODIFIED:   Lahey specific code was deleted.  Instead of reading
!                    the input and output filenames from the command line,
!                    the code opens a text file with the filenames listed
!                    in the order that they are read below.  The input
!                    filename is entered first.  The filename paths need
!                    to be added if the files are located in a different
!                    subdirectory(ies) than the executable.
!*       DATE:       January 17, 2001

!*       PROGRAMMER: Roger Brode

!*       DATE:       September 29, 1995

!*       MODIFIED:   Length of command line for Lahey version changed
!*                   from 80 to 120 characters - 4/19/93

!*       INPUTS:  Input File Containing the Input/Output Filenames

!*       OUTPUTS: Input Runstream File Name
!*                Output Print File Name

!*       CALLED FROM:   MAIN
!***********************************************************************

!*    Variable Declarations
  USE TER_MAIN

  IMPLICIT NONE

  SAVE

  CHARACTER(12) :: MODNAM

!*    Variable Initializations
  MODNAM = 'FILOPN'

!*    OPEN Print Output File, Unit IOUNIT=7
  DUMMY = 'OUTPUT'
  write(OUTFIL,'(2a)') INPFIL(1:index(INPFIL,".", .TRUE. )), "out"
  OPEN (UNIT=IOUNIT,FILE=OUTFIL, &
       ERR=99,STATUS='REPLACE')

  return

!*    WRITE Error Message:  Error Opening File
99 CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

  IF (DUMMY == 'OUTPUT') THEN
     write(*,993) trim(OUTFIL)
993  FORMAT('Error Opening Output File: ',a,'   Aborting.')
     STOP
  END IF

  RETURN
end subroutine FILOPN

SUBROUTINE STONUM(STRVAR,LENGTH,FNUM,IMUTI)
!***********************************************************************
!                 STONUM Module of ISC2 Model

!        PURPOSE: Gets Number From A String Variable

!        PROGRAMMER: Jeff Wang, Roger Brode

!        DATE:    March 2, 1992

!        INPUTS:  Input String Variable
!                 Length of Character String

!        OUTPUTS: Numbers

!        CALLED FROM: (This Is A Utility Program)
!***********************************************************************

!     Variable Declarations

  IMPLICIT NONE

  SAVE

  CHARACTER STRVAR*(*), CHK, NUMS*10, MODNAM*12
  INTEGER :: LENGTH, IMUTI, I
  REAL :: FNUM, CNUM, FDEC, FDC1, HEAD
  LOGICAL :: MEND, IN, NMARK, PMARK, DMARK, MMARK, EMARK

!     Variable Initialization
  MODNAM = 'STONUM'
  NUMS = '0123456789'
  I = 1
  MEND  = .FALSE.
  IN    = .FALSE.
  NMARK = .FALSE.
  PMARK = .FALSE.
  DMARK = .FALSE.
  MMARK = .FALSE.
  EMARK = .FALSE.
  CNUM  = 0.0
  IMUTI = 1
  FDEC  = 1.0

!     Beginning the Processing
  DO WHILE ( .NOT. MEND .AND. I <= LENGTH)
     CHK = STRVAR(I:I)
     IF (CHK /= ' ') THEN
        IN = .TRUE.
        IF (CHK >= '0' .AND. CHK <= '9') THEN
!              CHK is a Number, Assign a Value
           IF ( .NOT. DMARK) THEN
              CNUM = CNUM*10.+REAL(INDEX(NUMS,CHK)-1)
           ELSE
              FDEC = FDEC/10.
              FDC1 = FDEC*REAL(INDEX(NUMS,CHK)-1)
              CNUM = CNUM+FDC1
           END IF
        ELSE
!              Handle The E-Type Real Number
           IF ( .NOT. EMARK .AND. CHK == 'E') THEN
              EMARK = .TRUE.
              IF ( .NOT. NMARK) THEN
                 HEAD = CNUM
              ELSE
                 HEAD = -CNUM
              END IF
              DMARK = .FALSE.
              NMARK = .FALSE.
              CNUM = 0.0
           ELSE IF ( .NOT. PMARK .AND. CHK == '+') THEN
!                 Set Positive Indicator
              PMARK = .TRUE.
           ELSE IF ( .NOT. NMARK .AND. CHK == '-') THEN
!                 Set Negative Indicator
              NMARK = .TRUE.
           ELSE IF ( .NOT. DMARK .AND. CHK == '.') THEN
!                 Set Decimal Indicator
              DMARK = .TRUE.
           ELSE IF ( .NOT. MMARK .AND. CHK == '*' .AND. &
                .NOT. NMARK) THEN
!                 Set Repeat Number
              MMARK = .TRUE.
              IMUTI = NINT(CNUM)
              CNUM = 0.0
           ELSE
!                 Error Occurs, Set Switch and Exit Out Of The Subroutine
              GO TO 9999
           END IF
        END IF
     ELSE IF (IN .AND. CHK == ' ') THEN
        MEND = .TRUE.
     END IF
     I = I + 1
  END DO

  FNUM = CNUM

!     In Case Of Negative Field, Value Set to Negative
  IF (NMARK) THEN
     FNUM = -FNUM
  END IF

!     In Case of E-Format, Check for Exponents Out of Range
  IF (EMARK .AND. ABS(FNUM) <= 30.) THEN
     FNUM = HEAD*10**(FNUM)
  ELSE IF (EMARK .AND. ABS(FNUM) > 30.) THEN
     IF (FNUM < 0.0) THEN
        FNUM = 0.0
     ELSE IF (FNUM > 0.0) THEN
        FNUM = HEAD * 1.0E30
     END IF
     GO TO 9999
  END IF

  GO TO 1000

!     Set Error Switch for Illegal Numerical Field (WRITE Message and Handle
!     Error in Calling Routine)
9999 IMUTI = -1

1000 RETURN
end subroutine STONUM

SUBROUTINE STODBL(STRVAR,LEN,DNUM,IMUTI)
!***********************************************************************
!                 Subroutine STODBL

!        PURPOSE: Gets Double Precision of Real Number
!                 From A Stream Variable

!        PROGRAMMER: Jeff Wang

!        DATE:    March 2, 1992

!        MODIFIED:   To Change Exponent Limit for Out-of-range
!                    Inputs - 9/29/92

!        INPUTS:  Input String Variable
!                 Length of Character String

!        OUTPUTS: Double Precision Real Numbers

!        CALLED FROM: (This Is A Utility Program)
!***********************************************************************

!     Variable Declarations

  IMPLICIT NONE

  SAVE

  CHARACTER STRVAR*(*), CHK, MODNAM*12, NUMS*10
  INTEGER :: I, IMUTI, LEN
  real (kind=8) :: DNUM, CNUM, FDEC, FDC1, HEAD
  LOGICAL :: MEND, IN, NMARK, PMARK, DMARK, MMARK, EMARK

!     Variable Initialization
  MODNAM = 'STODBL'
  NUMS = '0123456789'
  I = 1
  MEND  = .FALSE.
  IN    = .FALSE.
  NMARK = .FALSE.
  PMARK = .FALSE.
  DMARK = .FALSE.
  MMARK = .FALSE.
  EMARK = .FALSE.
  CNUM  = 0.0D0
  IMUTI = 1
  FDEC  = 1.0D0

!     Beginning the Processing
  DO WHILE ( .NOT. MEND .AND. I <= LEN)
     CHK = STRVAR(I:I)
     IF (CHK /= ' ') THEN
        IN = .TRUE.
        IF (CHK >= '0' .AND. CHK <= '9') THEN
!              CHK is a Number, Assign a Value
           IF ( .NOT. DMARK) THEN
              CNUM = CNUM*10.0D0+DBLE(INDEX(NUMS,CHK)-1)
           ELSE
              FDEC = FDEC/10.0D0
              FDC1 = FDEC*DBLE(INDEX(NUMS,CHK)-1)
              CNUM = CNUM+FDC1
           END IF
        ELSE
!              Handle The E-Type (or D-Type) Real Number
           IF ( .NOT. EMARK .AND. CHK == 'E' .OR. &
                .NOT. EMARK .AND. CHK == 'D') THEN
              EMARK = .TRUE.
              IF ( .NOT. NMARK) THEN
                 HEAD = CNUM
              ELSE
                 HEAD = -CNUM
              END IF
              DMARK = .FALSE.
              NMARK = .FALSE.
              CNUM = 0.0D0
           ELSE IF ( .NOT. PMARK .AND. CHK == '+') THEN
!                 Set Positive Indicator
              PMARK = .TRUE.
           ELSE IF ( .NOT. NMARK .AND. CHK == '-') THEN
!                 Set Negative Indicator
              NMARK = .TRUE.
           ELSE IF ( .NOT. DMARK .AND. CHK == '.') THEN
!                 Set Decimal Indicator
              DMARK = .TRUE.
           ELSE IF ( .NOT. MMARK .AND. CHK == '*' .AND. &
                .NOT. NMARK) THEN
!                 Set Repeat Indicator
              MMARK = .TRUE.
              IMUTI = IDNINT(CNUM)
              CNUM = 0.0D0
           ELSE
!                 Error Occurs, Set Switch and Exit Out Of The Subroutine
              GO TO 9999
           END IF
        END IF
     ELSE IF (IN .AND. CHK == ' ') THEN
        MEND = .TRUE.
     END IF
     I = I + 1
  END DO

  DNUM = CNUM

!     In Case Of Negative Field, Value set to Negative
  IF (NMARK) THEN
     DNUM = -DNUM
  END IF

!     In Case of *E* Format, Check for Exponents Out of Range
  IF (EMARK .AND. DABS(DNUM) <= 30.0D0) THEN
     DNUM = HEAD*10.0D0**(DNUM)
  ELSE IF (EMARK .AND. DABS(DNUM) > 30.0D0) THEN
     IF (DNUM < 0.0D0) THEN
        DNUM = 0.0D0
     ELSE IF (DNUM > 0.0D0) THEN
        DNUM = HEAD * 1.0D+30
     END IF
     GO TO 9999
  END IF

  GO TO 1000

!     Set Error Switch for Illegal Numerical Field (WRITE Message and Handle
!     Error in Calling Routine)
9999 IMUTI = -1

1000 RETURN
end subroutine STODBL

SUBROUTINE NADGRIDS
!***********************************************************************
!                 NADGRIDS Module of AERMAP

!        PURPOSE: Process NADGRIDS option to specify
!                 path location for NADCON grid files,
!                 CONUS.LAS & CONUS.LOS, etc.

!        PROGRAMMER: Roger Brode

!        DATE:    February 9, 2009

!        INPUTS:  Input Runstream Image Parameters

!        OUTPUTS: Debug File Logical Switches

!        ERROR HANDLING:   Checks for Too Few Parameters
!                          Checks for Too Many Parameters

!        CALLED FROM:   COCARD
!***********************************************************************

!     Variable Declarations
  USE TER_MAIN
  IMPLICIT NONE
  CHARACTER MODNAM*12, KOPT*8
  INTEGER :: I

  SAVE

!     Variable Initializations
  MODNAM = 'NADGRIDS'

!     Check for Too Few or Too Many Parameters
  IF (IFC < 3) THEN
!        WRITE Error Message     ! No Parameters
     CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
     GO TO 999
  ELSE IF (IFC > 4) THEN
!        WRITE Warning Message   ! Too Many Parameters
     CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
     GO TO 999
  END IF

!*    Retrive NADGRIDS pathway
  IF ((LOCE(3)-LOCB(3)) <= ILEN_FLD-1) THEN
!*       Retrieve Pathway as Character Substring to Maintain Original Case
     NADGRID_PATH = RUNST1(LOCB(3):LOCE(3))
!*       Set logical flag for NADGRIDS path
     NADPATH = .TRUE.
  ELSE
!*       WRITE Error Message:  Pathway Field is Too Long
     WRITE(DUMMY,'(I8)') ILEN_FLD
     CALL ERRHDL(PATH,MODNAM,'E','210',DUMMY)
     GO TO 999
  END IF

999 RETURN
END SUBROUTINE NADGRIDS


SUBROUTINE SINDEX(ARRIN,IDIM,ELEM,INDEXS,FOUND)
!***********************************************************************
!                 SINDEX Module of ISC2 Model

!        PURPOSE: Search The Index of An Input Array Element

!        PROGRAMMER: Jeff Wang, Roger Brode

!        DATE:    March 2, 1992

!        INPUTS:  Input Character Element

!        OUTPUTS: Index Of This Element in An Array

!        CALLED FROM:  (This Is An Utility Programm)
!***********************************************************************

!     Variable Declarations

  IMPLICIT NONE

  CHARACTER (LEN=*) :: ARRIN(IDIM), ELEM
  CHARACTER MODNAM*12
  INTEGER :: IDIM, INDEXS, I
  LOGICAL :: FOUND

!     Variable Initializations
  MODNAM = 'SINDEX'
  FOUND = .FALSE.
  I = 1
  INDEXS = 0

  DO WHILE ( .NOT. FOUND .AND. I <= IDIM)
     IF (ELEM == ARRIN(I)) THEN
        FOUND = .TRUE.
        INDEXS = I
     END IF
     I = I + 1
  END DO

  RETURN
end subroutine SINDEX

SUBROUTINE SBYVAL(ARRIN1,ARRIN2,INX)
!***********************************************************************
!*                SBYVAL Module of ISC2 Model

!*       PURPOSE: Sort Array By Its 'Index Value'

!*       PROGRAMMER: Jeff Wang, Roger Brode

!*       DATE:    March 2, 1992

!*       INPUTS:  ARRIN1: 'Index Array',  ARRIN2: 'Value Array'
!*                INX: Number of Values to Sort

!*       OUTPUTS: Sorted Array

!*       CALLED FROM: (This Is A Utility Program)
!***********************************************************************

!*    Variable Declarations
  USE TER_MAIN

  IMPLICIT NONE

  SAVE

  CHARACTER(12) :: MODNAM

  INTEGER :: I
  INTEGER :: JC, INX, IMIN
  real (kind=8) :: MIN, TEMP1, TEMP2, ARRIN1(*), ARRIN2(*)

!*    Variable Initialization
  MODNAM = 'SBYVAL'
  JC = 1

  DO WHILE (JC <= INX)
!*       Find out The First Minimum In the Array
     MIN = ARRIN1(JC)
     IMIN = JC
     DO I = JC, INX
        IF (ARRIN1(I) < MIN) THEN
           IMIN = I
           MIN = ARRIN1(I)
        END IF
     END DO
!*       Swap The Selected Array Elements
     TEMP1 = ARRIN1(JC)
     TEMP2 = ARRIN2(JC)
     ARRIN1(JC) = ARRIN1(IMIN)
     ARRIN2(JC) = ARRIN2(IMIN)
     ARRIN1(IMIN) = TEMP1
     ARRIN2(IMIN) = TEMP2
!*       Increment The Counter
     JC = JC + 1
  END DO

  RETURN
END SUBROUTINE SBYVAL

SUBROUTINE TERRST
!***********************************************************************
!*                TERRST Module of the AMS/EPA Regulatory Model
!*                Terrain Preprocessor- AERMAP

!*       PURPOSE: To Determine Total Error/Message Statistics

!*       PROGRAMMER:  Jeff Wang, Roger Brode

!*       DATE:      September 29, 1995

!        MODIFIED:  To increase secondary message string from 8 to
!                   12 characters, allowing for 12-character source IDs
!                   R. W. Brode, U.S. EPA/OAQPS/AQMG, 04/13/2011

!*       INPUTS:    Error Message Temporary File

!*       OUTPUTS:   Total Number of Messages by Message Type

!*       CALLED FROM:  This is A Utility Program
!***********************************************************************

!*    Variable Declarations
  USE TER_MAIN
  IMPLICIT NONE
  CHARACTER(12) :: MODNAM

  CHARACTER ERRTP*1, ERRCD*3, ERRMG1*50, ERRMG2*12, INPFLD*3
  INTEGER :: IERRLN, IMIT

!*    Variable Initialization
  MODNAM = 'TERRST'
  IFTL = 0
  IWRN = 0
  INFO = 0
  ICLM = 0
  IMSG = 0
  EOF1 = .FALSE.

!*    Rewind the Temporary Error/Message File
  REWIND IERUNT

  DO WHILE ( .NOT. EOF1)
     READ(IERUNT,1116,END=99,ERR=9999) PATH,ERRTP,ERRCD,IERRLN, &
          MODNAM,ERRMG1,ERRMG2
!*       Sort Error Group And Find The Index
     INPFLD = ERRCD
     CALL STONUM(INPFLD,3,FNUM,IMIT)

     IF (ERRTP == 'E') THEN
        IFTL = IFTL + 1
     ELSE IF (ERRTP == 'W') THEN
        IWRN = IWRN + 1
     ELSE IF (ERRTP == 'I') THEN
        INFO = INFO + 1
     END IF

     GO TO 11
99   EOF1 = .TRUE.
11   CONTINUE
  END DO

1116 FORMAT(A2,1X,A1,A3,I8,1X,A6,1X,A50,1X,A12)

!*    Use BACKSPACE To Reposition Temporary Error Message File Ahead of EOF1;
!*    This Is Needed in Order To Allow For Additional Message Writes
  BACKSPACE IERUNT

  GO TO 1000

!*    WRITE Error Message: Error Reading Temp Error Message File
9999 CALL ERRHDL(PATH,MODNAM,'E','510','ERRORMSG')

1000 RETURN
end subroutine TERRST

SUBROUTINE SUMTBL
!***********************************************************************
!*                SUMTBL Module of the AMS/EPA Regulatory Model - AERMOD
!*                Terrain Preprocessor- AERMAP

!*       PURPOSE: To Print Out The Error Summary Table

!*       PROGRAMMER:  Jeff Wang, Roger Brode, Jayant Hardikar

!*       DATE:    September 29, 1995

!        MODIFIED:  To increase secondary message string from 8 to
!                   12 characters, allowing for 12-character source IDs
!                   R. W. Brode, U.S. EPA/OAQPS/AQMG, 04/13/2011

!*       INPUTS:  Error Message Temporary File

!*       OUTPUTS: Summary Of Errors

!*       CALLED FROM:  This is A Utility Program
!***********************************************************************

!*    Variable Declarations
  USE TER_MAIN

  IMPLICIT NONE

  SAVE

  CHARACTER(12) :: MODNAM

  INTEGER :: J
  CHARACTER ERRTP*1, ERRCD*3, ERRMG1*50, ERRMG2*12
  INTEGER :: IERRLN

!*    Variable Initialization
  MODNAM = 'SUMTBL'

!*    Write Out The Total Error Statistics
  WRITE (IOUNIT,*) '--------- Summary of Total Messages --------'
  WRITE (IOUNIT,*) ' '
  WRITE (IOUNIT,9014) IFTL
9014 FORMAT(' A Total of ',I10,' Fatal Error Message(s)')
  WRITE (IOUNIT,9015) IWRN
9015 FORMAT(' A Total of ',I10,' Warning Message(s)')
  WRITE (IOUNIT,9016) INFO
9016 FORMAT(' A Total of ',I10,' Informational Message(s)')

  WRITE (IOUNIT,*) ' '

!*    Write Out All The Fatal Error Messages
  WRITE (IOUNIT,*) ' '
  WRITE (IOUNIT,*) '   ******** FATAL ERROR MESSAGES ******** '
  REWIND IERUNT
  EOF1 = .FALSE.
  J = 0
  DO WHILE ( .NOT. EOF1)
     READ(IERUNT,1116,END=99,ERR=9999) PATH,ERRTP,ERRCD,IERRLN, &
          MODNAM,ERRMG1,ERRMG2
     IF (ERRTP == 'E') THEN
        J = J + 1
        WRITE (IOUNIT,1117) PATH,ERRTP,ERRCD,IERRLN, &
             MODNAM(1:MIN(LEN_TRIM(MODNAM),6)), &
             ERRMG1,ERRMG2
     END IF
     GO TO 11
99   EOF1 = .TRUE.
11   CONTINUE
  END DO

!*    If No Fatal Error Messages, Then Write 'NONE'
  IF (J == 0) THEN
     WRITE (IOUNIT,*) '              ***  NONE  ***         '
     WRITE (IOUNIT,*) ' '
  END IF

!*    Write Out All The Warning Messages
  WRITE (IOUNIT,*) ' '
  WRITE (IOUNIT,*) '   ********   WARNING MESSAGES   ******** '
  REWIND IERUNT
  EOF1 = .FALSE.
  J = 0
  DO WHILE ( .NOT. EOF1)
     READ(IERUNT,1116,END=999,ERR=9999) PATH,ERRTP,ERRCD,IERRLN, &
          MODNAM(1:6),ERRMG1,ERRMG2
     IF (ERRTP == 'W') THEN
        J = J + 1
        WRITE (IOUNIT,1117) PATH,ERRTP,ERRCD,IERRLN, &
             MODNAM(1:MIN(LEN_TRIM(MODNAM),6)), &
             ERRMG1,ERRMG2
     END IF
     GO TO 111
999  EOF1 = .TRUE.
111  CONTINUE
  END DO

!*    If No Warning Messages, Then Write 'NONE'
  IF (J == 0) THEN
     WRITE (IOUNIT,*) '              ***  NONE  ***        '
     WRITE (IOUNIT,*) ' '
  END IF

1116 FORMAT(A2,1X,A1,A3,I8,1X,A6,1X,A50,1X,A12)
1117 FORMAT(1X,A2,1X,A1,A3,I8,1X,A6,':',A50,1X,A12)

!*    Use BACKSPACE To Reposition Temporary Error Message File Ahead of EOF1;
!*    This Is Needed in Order To Allow For Additional Message Writes
  BACKSPACE IERUNT

  GO TO 1000

!*    WRITE Error Message: Error Reading Temp Error Message File
9999 CALL ERRHDL(PATH,MODNAM,'E','510','ERRORMSG')

1000 RETURN
end subroutine SUMTBL

SUBROUTINE INPSUM
!***********************************************************************
!                 INPSUM Module of AERMAP

!        PURPOSE: Print Out The Input Data Summary, including statistics
!                 on receptors and/or sources located within gaps

!        PROGRAMMER: Roger Brode

!        DATE:    December 7, 2006

!*       Revision History:
!*
!*       MODIFIED: February 9, 2009
!*
!*                Included code to document the statistics on the
!*                number of receptors and/or sources located within
!*                gaps, either between files due to NAD shift or
!*                within files.  This also includes the number of
!*                receptors and/or sources with missing elevations
!*                of "filled-in" elevations based on the FILLGAPS
!*                option.

!        INPUTS:  Arrays of Source Parameters
!                 Arrays of Receptor Locations
!                 Arrays of Model Results

!        OUTPUTS: Printed Model Outputs

!        CALLED FROM:   MAIN
!***********************************************************************

!     Variable Declarations
  USE TER_MAIN
  IMPLICIT NONE
  CHARACTER MODNAM*12

  SAVE

!     Variable Initializations
  MODNAM = 'INPSUM'

!     Summarize The Model Options
  WRITE(IOUNIT,9040)
9040 FORMAT(//44X,'***     TERSCI SETUP OPTIONS SUMMARY       ***'/ &
       &        63(' -')/)

  WRITE(IOUNIT,9099)
9099 FORMAT(1X,' ')

  IF (TYPDAT == 'DEM') THEN
     WRITE(IOUNIT,9041) NUMDEM
9041 FORMAT(1X,'**This Run Includes: ',I7,'  DEM File(s)')
  ELSE IF (TYPDAT == 'NED') THEN
     WRITE(IOUNIT,9042) NUMDEM
9042 FORMAT(1X,'**This Run Includes: ',I7,'  NED File(s)')
  END IF

  WRITE(IOUNIT,9099)
  WRITE(IOUNIT,9043) NUMREC, NUMSRC
9043 FORMAT(1X,'**This Run Includes: ',I7,'  Receptor(s); and  ' &
       ,I7,'  Source(s)')

  IF (GAPSFOUND .OR. GAPSFOUND_IN) THEN
!*       Gap receptors and/or sources were found, but were assigned to DEM
!*       files on second pass, so no fatal errors issued
     WRITE(IOUNIT,9099)
     WRITE(IOUNIT,*) 'NOTE:'
     IF (GAPSFOUND .AND. .NOT. FATAL) THEN
        WRITE(IOUNIT,99043) NRGAP2, NSGAP2, TYPDAT, TYPDAT
99043   FORMAT(1X,'**This run includes: ',I7,'  Receptor(s); and  ' &
             ,I7,'  Source(s) located in GAPS BETWEEN ',A3, &
             ' files due to NAD shifts,', &
             /68X,'but ALL have been assigned to the closest ',A3,' file.')
     ELSE IF (GAPSFOUND .AND. FATAL) THEN
        WRITE(IOUNIT,99143) NRGAP2, NSGAP2, TYPDAT, TYPDAT
99143   FORMAT(1X,'**This run includes: ',I7,'  Receptor(s); and  ' &
             ,I7,'  Source(s) located in GAPS BETWEEN ',A3, &
             ' files due to NAD shifts,', &
             /68X,'but some may NOT have been assigned to another ',A3, &
             ' file.')
     END IF
     IF (GAPSFOUND_IN) THEN
        WRITE(IOUNIT,99243) NRGAP_IN, NSGAP_IN, TYPDAT, &
             NRSUBS, NSSUBS
99243   FORMAT(1X,'**This run includes: ',I7,'  Receptor(s); and  ' &
             ,I7,'  Source(s) located in GAPS INSIDE ',A3,' file(s);', &
             /'     with a total of: ',I7,'  Receptor(s); and  ' &
             ,I7,'  Source(s) assigned elevations based on subsequent', &
             ' file(s).')
     END IF
     IF (MISSFOUND) THEN
        WRITE(IOUNIT,99343) NRMISS, NSMISS
99343   FORMAT(1X,'**This run includes: ',I7,'  Receptor(s); and  ' &
             ,I7,'  Source(s) that have been assigned missing ', &
             'elevations (-9999.0)!')
     END IF
     IF (NRFILLED > 0 .OR. NSFILLED > 0) THEN
        WRITE(IOUNIT,99443) NRFILLED, NSFILLED
99443   FORMAT(1X,'**This run includes: ',I7,'  Receptor(s); and  ' &
             ,I7,'  Source(s) located in gaps and assigned ', &
             'non-missing elevations ', &
             /68X,'based on the closest nodes under the FILLGAPS option!')
     END IF
     IF (TYPDAT == 'DEM') THEN
        WRITE(IOUNIT,99543)
99543   FORMAT(1X, &
             '**These gaps may be due to NAD conversions or use of ', &
             'non-standard DEM files.', &
             /'   Consider using standard DEM files with consistent NAD ', &
             'values or NED data to avoid gaps between or within files.')
     ELSE IF (TYPDAT == 'NED') THEN
        WRITE(IOUNIT,99643)
99643   FORMAT(1X, &
             '**These gaps may be due to NAD conversions or use of ', &
             'non-standard NED files.', &
             /'   Consider using standard NED files with consistent ', &
             'NAD values to avoid gaps between or within files.')
     END IF
  END IF

  WRITE(IOUNIT,9099)
  IF (NADA == 0) THEN
     WRITE(IOUNIT,9044) NADA, XATERR-XAUSER, &
          YATERR-YAUSER, &
          XATERR, YATERR, ZATERR
9044 FORMAT(1X,'**The Input Receptors and Sources Were ', &
          'Assigned a NADA Value of ',I2,':  No NAD Shifts ', &
          'Included in Calculations',// &
          ' **The Input Receptors and Sources Are Offset:', &
          F12.2,' meters East; ',F12.2,' meters North', &
          /6X,'from the User-specified Anchor Point at:', &
          F12.2,' meters East; ',F12.2,' meters North;  ', &
          'Zone ',I3)
  ELSE
     WRITE(IOUNIT,9144) NADA, NADN(NADA), XATERR-XAUSER, &
          YATERR-YAUSER, &
          XATERR, YATERR, ZATERR
9144 FORMAT(1X,'**The Input Receptors and Sources Were ', &
          'Assigned a NADA Value of ',I2,':  ',A40,// &
          ' **The Input Receptors and Sources Are Offset:', &
          F12.2,' meters East; ',F12.2,' meters North', &
          /6X,'from the User-specified Anchor Point at:', &
          F12.2,' meters East; ',F12.2,' meters North;  ', &
          'Zone ',I3)
  END IF

  WRITE(IOUNIT,9099)
  IF ( .NOT. EXTRACT) THEN
     WRITE (IOUNIT,9245)
9245 FORMAT(1X,'**Terrain heights were PROVIDED by user')
  ELSE IF (EXTRACT) THEN
     WRITE (IOUNIT,9246) TYPDAT
9246 FORMAT(1X,'**Terrain heights were EXTRACTed from ',A3,' data')
  END IF

  WRITE(IOUNIT,9099)
  WRITE(IOUNIT,9247) TYPDAT, TYPDAT, TYPDAT, TYPDAT
9247 FORMAT(1X,'**The Following Debug Output Files Have Been ', &
       'Automatically Generated:'// &
       '   DOMDETAIL.OUT - Details of User-specified Domain ', &
       'and Relation to ',A3,' Files',/ &
       '   MAPDETAIL.OUT - Details Regarding Input ',A3,' Files',/ &
       '   MAPPARAMS.OUT - Summary of ',A3,' File Parameters and', &
       ' ',A3,' File Adjacency')

  IF (HILLDBG .OR. RECDBG .OR. SRCDBG) THEN
     WRITE(IOUNIT,9099)
     WRITE(IOUNIT,9248)
9248 FORMAT(1X,'**The Following User-specified Debug Output Files ', &
          'Have Been Generated:'/)

     IF (HILLDBG) THEN
        WRITE(IOUNIT,9249) CALCHC_FILE(1:LEN_TRIM(CALCHC_FILE))
9249    FORMAT(1X,'  Critical Hill Height Calculations:', &
             &                 2X,A)
     END IF

     IF (RECDBG) THEN
        WRITE(IOUNIT,9099)
        WRITE(IOUNIT,9250) RECDET_FILE(1:LEN_TRIM(RECDET_FILE))
9250    FORMAT(1X,'  Receptor NAD Conversion Results:  ', &
             &                 2X,A)
        WRITE(IOUNIT,9251) TYPDAT, &
             RECNDEM_FILE(1:LEN_TRIM(RECNDEM_FILE))
9251    FORMAT(1X,'  Receptor vs ',A3,' File Locations:   ', &
             &                 2X,A)
        WRITE(IOUNIT,9252) RECELV_FILE(1:LEN_TRIM(RECELV_FILE))
9252    FORMAT(1X,'  Receptor Elevation Calculations:  ', &
             &                 2X,A)
     END IF

     IF (SRCDBG .AND. NUMSRC > 0) THEN
        WRITE(IOUNIT,9099)
        WRITE(IOUNIT,9260) SRCDET_FILE(1:LEN_TRIM(SRCDET_FILE))
9260    FORMAT(1X,'  Source NAD Conversion Results:    ', &
             &                 2X,A)
        WRITE(IOUNIT,9261) TYPDAT, &
             SRCNDEM_FILE(1:LEN_TRIM(SRCNDEM_FILE))
9261    FORMAT(1X,'  Source vs ',A3,' File Locations:     ', &
             &                 2X,A)
        WRITE(IOUNIT,9262) SRCELV_FILE(1:LEN_TRIM(SRCELV_FILE))
9262    FORMAT(1X,'  Source Elevation Calculations:    ', &
             &                 2X,A)
     END IF
     WRITE(IOUNIT,9099)

  END IF

  RETURN
END SUBROUTINE INPSUM

SUBROUTINE WRITEZ (IDM,JRECRD,ELV)
!***********************************************************************
!*       PURPOSE:  THIS SUBROUTINE WILL WRITE THE Z VALUES TO THE
!*                 SPECIFIED RECORD NUMBER OF THE DIRECT ACCESS FILE.
!*
!*       PROGRAMMER: Jayant Hardikar, Roger Brode
!*
!*       DATE:    September 29, 1995
!*
!*       MODIFIED:
!*
!*       INPUTS:  File Unit of Direct Access File, Record Number
!*
!*       OUTPUTS: Elevation at the Record Number
!*
!*       CALLED FROM:   MAIN
!***********************************************************************

!*    Variable Declarations
  USE TER_MAIN

  IMPLICIT NONE

  SAVE

  CHARACTER(12) :: MODNAM

  INTEGER :: IDM, JRECRD

  real (kind=8) :: ELV

  MODNAM = 'WRITEZ'

  WRITE (IDRUNT(IDM),REC=JRECRD) ELV

  RETURN
end subroutine WRITEZ


real (kind=8) FUNCTION READZ(IDM,JRECRD)
!***********************************************************************
!*       PURPOSE:  THIS SUBROUTINE WILL READ THE Z VALUES FROM THE
!*                 SPECIFIED RECORD NUMBER OF THE DIRECT ACCESS FILE.
!*
!*       PROGRAMMER: Jayant Hardikar, Roger Brode
!*
!*       DATE:    September 29, 1995
!*
!*       MODIFIED:
!*
!*       INPUTS:  File Unit of Direct Access File, Record Number
!*
!*       OUTPUTS: Elevation at the Record Number
!*
!*       CALLED FROM:   MAIN
!***********************************************************************

!*    Variable Declarations
  USE TER_MAIN

  IMPLICIT NONE

  SAVE

  CHARACTER(12) :: MODNAM

  INTEGER :: IDM, JRECRD

  real (kind=8) :: ELV

  MODNAM = 'READZ'

  READ (IDRUNT(IDM),REC=JRECRD) ELV
  READZ = ELV

  RETURN
end function READZ


SUBROUTINE RECNUM
!***********************************************************************
!*    PURPOSE:  BASED ON A ROW & COLUMN NUMBER FROM A DEM PROFILE, THIS
!*              SUBROUTINE WILL CALCULATE A RECORD NUMBER FOR READING FROM
!*              OR WRITING TO A DIRECT ACCESS FILE
!*
!*    PROGRAMMER: Jayant Hardikar, Roger Brode
!*
!*    DATE:    September 29, 1995
!*
!*    MODIFIED:
!*
!*    INPUTS:  ROW NUMBER, COLUMN NUMBER OF DEM PROFILE
!*
!*    OUTPUTS: DIRECT ACCESS FILE RECORD NUMBER
!*
!*    CALLED FROM:
!***********************************************************************

!*    Variable Declarations
  USE TER_MAIN

  IMPLICIT NONE

  SAVE

  INTEGER :: I

  DO I = 1, NX(1)-1
     JREC(1) = JREC(1) + NODES(I)
  END DO
  JREC(1) = JREC(1) + NY1(1)

  DO I = 1, NX(2)-1
     JREC(2) = JREC(2) + NODES(I)
  END DO
  JREC(2) = JREC(2) + NY1(2)

  DO I = 1, NX(2)-1
     JREC(3) = JREC(3) + NODES(I)
  END DO
  JREC(3) = JREC(3) + NY2(2)

  DO I = 1, NX(1)-1
     JREC(4) = JREC(4) + NODES(I)
  END DO
  JREC(4) = JREC(4) + NY2(1)

  RETURN
end subroutine RECNUM

real (kind=8) FUNCTION GETDEM(ID,JR)
!***********************************************************************
!*               GETDEM Module of AERMAP Terrain Preprocessor
!*
!*       PURPOSE: Reads Elevation Data from a Direct Access File
!*
!*       PROGRAMMER: Jayant Hardikar, Roger Brode
!*
!*       DATE:    September 29, 1995
!*
!*       MODIFIED:
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

  INTEGER :: ID, JR
  real (kind=8) :: ELV

  MODNAM = 'GETDEM'

  READ (IDRUNT(ID),REC=JR) ELV
  GETDEM = ELV

  RETURN
end function GETDEM


SUBROUTINE HEADER
!***********************************************************************
!*                HEADER Module of AERMAP

!*       PURPOSE: Control Page Feed and Header Information for
!*                Printed File Output

!*       PROGRAMMER: Roger Brode, James Paumier, Jayant Hardikar

!*       DATE:       September 29, 1995
!*
!*       MODIFIED:   Use ASCII form feed character [ACHAR(12)] for
!*                   page feed in aermap.out file rather then
!*                   CARRIAGECONTROL='Fortran', which is not a
!*                   standard Fortran option.
!*                   Moved call to DATIME function for date/time
!*                   to MAIN program for use in other output files.
!*                   R.W. Brode, USEPA/OAQPS/AQMG, February 9, 2009

!*       INPUTS:  Page Number from COMMON

!*       OUTPUTS: Page Feed and Header

!*       CALLED FROM:  (This Is An Utility Program)
!***********************************************************************

!*    Variable Declarations
  USE TER_MAIN

  IMPLICIT NONE

  SAVE

  CHARACTER(12) :: MODNAM
  CHARACTER(1) ::  FFEED

!*    Variable Initializations
  MODNAM = 'HEADER'

!*    Write Header to Printed Output File

  RETURN
end subroutine HEADER

SUBROUTINE DATIME ( DCALL, TCALL )
!***********************************************************************
!                 DATIME Module

!        PURPOSE: Obtain the system date and time

!        PROGRAMMER: Jim Paumier, PES, Inc.

!        DATE:    April 15, 1994

!        MODIFIED:   Uses Fortran 90 DATE_AND_TIME routine.
!                    R.W. Brode, PES, 8/14/98

!        INPUTS:  none

!        OUTPUTS: Date and time in character format

!        CALLED FROM:  RUNTIME
!***********************************************************************

!     Variable Declarations
  IMPLICIT NONE

  CHARACTER DCALL*8, TCALL*8
  CHARACTER CDATE*8, CTIME*10, CZONE*5
  INTEGER :: IDATETIME(8)
  INTEGER :: IPTYR, IPTMON, IPTDAY, IPTHR, IPTMIN, IPTSEC

  DCALL = ' '
  TCALL = ' '

!     Call Fortran 90 date and time routine
  CALL DATE_AND_TIME (CDATE, CTIME, CZONE, IDATETIME)

!     Convert year to two digits and store array variables
  IPTYR  = IDATETIME(1) - 100 * INT(IDATETIME(1)/100)
  IPTMON = IDATETIME(2)
  IPTDAY = IDATETIME(3)
  IPTHR  = IDATETIME(5)
  IPTMIN = IDATETIME(6)
  IPTSEC = IDATETIME(7)

!     Write Date and Time to Character Variables, DCALL & TCALL
  WRITE(DCALL, '(2(I2.2,"/"),I2.2)' ) IPTMON, IPTDAY, IPTYR
  WRITE(TCALL, '(2(I2.2,":"),I2.2)' ) IPTHR, IPTMIN, IPTSEC

  RETURN
end subroutine DATIME

SUBROUTINE ERRHDL(PATHWY,MODNAM,INERTP,INERCD,INPMSG)
!***********************************************************************
!                 ERRHDL Module of the AMS/EPA Regulatory Model - AERMOD

!        PURPOSE: A General Error Handling Procedure

!        PROGRAMMER: Jeff Wang

!        DATE:    September 29, 1995

!        MODIFIED:  Sets upper limit on line number included in error
!                   message to avoid overflowing the field; also increased
!                   field length for last message field from 8 to 12 to
!                   accommodate 12 character source IDs.
!                   R.W. Brode, U.S. EPA/OAQPS/AQMG, 04/13/2011

!        INPUTS:  Error Code, Occur Locations

!        OUTPUTS: Error Message, Error Statistics..etc.

!        CALLED FROM:  (This Is An Utility Programm)
!***********************************************************************

!     Variable Declarations
  USE TER_MAIN
  IMPLICIT NONE

  INTEGER :: I, ILINE_PRT
  CHARACTER ERRMG1*50, PATHWY*2, INERTP*1, INERCD*3, ICODE*3, &
       INPMSG*(*), MODNAM*(*), TMPMOD*6, TMPMSG*12
  LOGICAL :: FOUND

!     Variable Initializations
  IERROR = IERROR + 1
  FOUND = .FALSE.
  I = 1

!     Check for Occurrence of 'E' Error Type, and Set FATAL Switch
  IF (INERTP == 'E') THEN
     FATAL = .TRUE.
     NFATAL = NFATAL + 1
     IF (NFATAL == 999) THEN
!           Number Of Fatal Errors Has Reached Limit of 999
        ERRMG1 = 'Number of Fatal Errors Has Reached Limit of 999'
        TMPMOD = 'ERRHDL'
        ICODE  = '999'
        TMPMSG = ' '
        ILINE_PRT = MIN(ILINE,99999999)
        WRITE(IERUNT,1111) PATHWY,INERTP,ICODE,ILINE_PRT,TMPMOD, &
             ERRMG1,TMPMSG
        GO TO 999
     ELSE IF (NFATAL > 999) THEN
!           Skip Any More Error WRITEs
        GO TO 999
     END IF
  END IF

!     Check for Occurrence of 'W' Type
  IF (INERTP == 'W') THEN
     NWARN = NWARN + 1
     IF (NWARN == 999) THEN
!           Number Of Warnings Has Reached Limit of 999
        ERRMG1 = 'Number of Warnings Has Reached Limit of 999'
        TMPMOD = 'ERRHDL'
        ICODE  = '888'
        TMPMSG = ' '
        ILINE_PRT = MIN(ILINE,99999999)
        WRITE(IERUNT,1111) PATHWY,INERTP,ICODE,ILINE_PRT,TMPMOD, &
             ERRMG1,TMPMSG
        GO TO 999
     ELSE IF (NWARN > 999) THEN
!           Skip Any More Error WRITEs
        GO TO 999
     END IF
  END IF

!     Go To Match The Error Massage
  DO WHILE ( .NOT. FOUND .AND. I <= IERRN)
     IF (INERCD == ERRCOD(I)) THEN
        ERRMG1 = ERRMSG(I)
        FOUND = .TRUE.
     END IF
     I = I + 1
  END DO

  IF ( .NOT. FOUND) THEN
     WRITE(ERRMG1,1001)
1001 FORMAT('SYSTEM ERROR: MESSAGE NOT FOUND FOR THIS NUMBER!')
  END IF

! --- Set upper limit on ILINE to avoid write error
  ILINE_PRT = MIN(ILINE,99999999)
!     Write Out The Error Message
  WRITE(IERUNT,1111) PATHWY,INERTP,INERCD,ILINE_PRT, &
       MODNAM(1:MIN(LEN_TRIM(MODNAM),6)),ERRMG1, &
       INPMSG(1:MIN(LEN_TRIM(INPMSG),12))
1111 FORMAT(A2,1X,A1,A3,I8,1X,A6,':',A50,1X,A12)

999 RETURN
end subroutine ERRHDL


SUBROUTINE LWRUPR
!***********************************************************************
!                 LWRUPR Module of ISC2 Model

!        PURPOSE: Transfer All Characters From Lower Case To
!                 Upper Case (Using INDEX Intrinsic Function)
!                 Note that the CHAR*ISTRG RUNST1 Variable Includes
!                 the Original Case for Echoing and for Later Use
!                 To Retrieve Filenames.

!        PROGRAMMER: Roger Brode, Kevin Stroupe

!        DATE:    March 2, 1992

!        INPUTS:  Input Runstream Card Image
!                 Number of Characters in String, PARAMETER ISTRG

!        OUTPUTS: Input Runstream Card Image (Array) in Uppercase

!        CALLED FROM:   SETUP
!***********************************************************************

!     Variable Declarations
  USE TER_MAIN

  IMPLICIT NONE

  SAVE

  INTEGER :: I
  CHARACTER(12) :: MODNAM

  CHARACTER UPCASE*26
  CHARACTER LWCASE*26

!     Variable Initializations
  DATA UPCASE/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
  DATA LWCASE/'abcdefghijklmnopqrstuvwxyz'/
  MODNAM = 'LWRUPR'

  DO I = 1, ISTRG
     IF (RUNST(I) /= ' ') THEN
        INDCHK = INDEX(LWCASE,RUNST(I))
        IF (INDCHK /= 0) THEN
           RUNST(I) = UPCASE(INDCHK:INDCHK)
        END IF
     END IF
  END DO

  RETURN
end subroutine LWRUPR


!***** Begin New Code for Allocatable Arrays



SUBROUTINE PREGENCAR
!***********************************************************************
!                 PREGENCAR Module of the AMS/EPA Regulatory Model - AERMOD

!        PURPOSE: Generates Cartesian Grid Receptor Network With
!                 Uniform Spacing

!        PROGRAMMER: Roger Brode

!        DATE:    September 24, 1996

!        INPUTS:  Input Runstream Image Parameters

!        OUTPUTS: Cartesian Grid Receptor Network With Uniform
!                 Spacing

!        CALLED FROM:   PRECART
!***********************************************************************

!     Variable Declarations
  USE TER_MAIN
  IMPLICIT NONE
  CHARACTER MODNAM*12

  SAVE
  INTEGER :: I, K, IMIT
  real (kind=8) :: TEMPP(6)
  LOGICAL :: ERROR

!     Variable Initializations
  MODNAM = 'PREGENCAR'
  ERROR = .FALSE.

!     Check for Location of Secondary Keyword, XYINC
  DO I = 1, IFC
     IF (FIELD(I) == 'XYINC') THEN
        ISC = I + 1
     END IF
  END DO

!     Determine Whether There Are Enough Parameter Fields
  IF (IFC == ISC-1) THEN
!        Missing Parameter
     RECERR = .TRUE.
     GO TO 999
  ELSE IF (IFC > ISC+5) THEN
!        Too Many Parameters
     RECERR = .TRUE.
     GO TO 999
  ELSE IF (IFC < ISC+5) THEN
!        Too Few Parameters
     RECERR = .TRUE.
     GO TO 999
  END IF

!     Input The Numerical Values
  DO K = 1,6
     CALL STODBL(FIELD(ISC + K-1),ILEN_FLD,TEMPP(K),IMIT)
!        Check The Numerical Field
     IF (IMIT == -1) THEN
        ERROR = .TRUE.
        RECERR = .TRUE.
     END IF
  END DO

  IF (ERROR) THEN
     ERROR = .FALSE.
     GO TO 999
  END IF

!     Assign Values to Appropriate Variables for Generated Network
  XINT   = TEMPP(1)
  ICOUNT = IDNINT(TEMPP(2))
  XDELTA = TEMPP(3)
  YINT   = TEMPP(4)
  JCOUNT = IDNINT(TEMPP(5))
  YDELTA = TEMPP(6)

!     Assign Them to the Coordinate Arrays
  IF (ICOUNT > IXM) THEN
     IXM = ICOUNT
  END IF
  IF (JCOUNT > IYM) THEN
     IYM = JCOUNT
  END IF

999 RETURN
END SUBROUTINE PREGENCAR

SUBROUTINE PREXYPNTS
!***********************************************************************
!                 PREXYPNTS Module of the AMS/EPA Regulatory Model - AERMOD

!        PURPOSE: Processes Cartesian Grid x,y Input Value

!        PROGRAMMER: Roger Brode

!        DATE:    September 24, 1996

!        INPUTS:  Input Runstream Image Parameters

!        OUTPUTS: Cartesian Grid x,y Input Value

!        CALLED FROM:   PRECART
!***********************************************************************

!     Variable Declarations
  USE TER_MAIN
  IMPLICIT NONE
  CHARACTER MODNAM*12

  SAVE
  INTEGER :: I, ISET, JSET, IMIT

!     Variable Initializations
  MODNAM = 'PREXYPNTS'

  IF (KTYPE == 'XPNTS') THEN
!        Check for Location of Secondary Keyword, XPNTS
     DO I = 1, IFC
        IF (FIELD(I) == 'XPNTS') THEN
           ISC = I + 1
        END IF
     END DO

!        Determine Whether There Are Enough Parameter Fields
     IF (IFC == ISC-1) THEN
!           Missing Parameter
        RECERR = .TRUE.
        GO TO 999
     END IF

     ISET = ICOUNT
     DO I = ISC, IFC
        CALL STONUM(FIELD(I),ILEN_FLD,FNUM,IMIT)
!           Check The Numerical Field
        IF (IMIT == -1) THEN
           RECERR = .TRUE.
        END IF
        ISET = ISET + 1
        IF (ISET > IXM) THEN
           IXM = ISET
        END IF
     END DO
     ICOUNT = ISET

  ELSE IF (KTYPE == 'YPNTS') THEN
!        Check for Location of Secondary Keyword, YPNTS
     DO I = 1, IFC
        IF (FIELD(I) == 'YPNTS') THEN
           ISC = I + 1
        END IF
     END DO

!        Determine Whether There Are Enough Parameter Fields
     IF (IFC == ISC-1) THEN
!           Missing Parameter
        RECERR = .TRUE.
        GO TO 999
     END IF

     JSET = JCOUNT
     DO I = ISC, IFC
        CALL STONUM(FIELD(I),ILEN_FLD,FNUM,IMIT)
!           Check The Numerical Field
        IF (IMIT == -1) THEN
           RECERR = .TRUE.
        END IF
        JSET = JSET + 1
        IF (JSET > IYM) THEN
           IYM = JSET
        END IF
     END DO
     JCOUNT = JSET

  END IF

999 RETURN
END SUBROUTINE PREXYPNTS

SUBROUTINE PREPOLR
!***********************************************************************
!                 PREPOLR Module of the AMS/EPA Regulatory Model - AERMOD

!        PURPOSE: Processes Polar Grid Receptor Network Inputs

!        PROGRAMMER:  Roger Brode

!        DATE:    September 24, 1996

!        INPUTS:  Input Runstream Image Parameters

!        OUTPUTS: Polar Receptor Network Inputs

!        CALLED FROM:   RECSIZ, PREREINC
!***********************************************************************

!     Variable Declarations
  USE TER_MAIN
  IMPLICIT NONE
  CHARACTER MODNAM*12

  SAVE

!     Variable Initializations
  MODNAM = 'PREPOLR'

  IF (IFC < 3) THEN
!        Missing Data Field
     GO TO 999
  END IF

!     READ in the Netid and Nettype
  NETIDT = FIELD(3)
  IF ( .NOT. NEWID .AND. (NETIDT == '    ' .OR. &
       NETIDT == 'ORIG' .OR. NETIDT == 'DIST' .OR. &
       NETIDT == 'DDIR' .OR. NETIDT == 'ELEV' .OR. &
       NETIDT == 'HILL' .OR. &
       NETIDT == 'FLAG' .OR. NETIDT == 'GDIR' .OR. &
       NETIDT == 'END')) THEN
     NETIDT = PNETID
     KTYPE = FIELD(3)
  ELSE IF ( .NOT. NEWID .AND. NETIDT == PNETID) THEN
     KTYPE = FIELD(4)
  ELSE IF (NEWID .AND. NETIDT /= '    ') THEN
     NEWID = .FALSE.
     KTYPE = FIELD(4)
!        The Keyword Counter
     NNET = NNET + 1
  ELSE
!        Invalid Secondary Keyword
     RECERR = .TRUE.
     GO TO 999
  END IF

!     Start to Set Up the Network
  IF (KTYPE == 'STA') THEN
     ISTA = .TRUE.
     IEND = .FALSE.
     NEWID = .FALSE.
     RECERR = .FALSE.
     ICOUNT = 0
     JCOUNT = 0
  ELSE IF (KTYPE == 'DIST') THEN
!        Read in the Distance Set                           ---   CALL PREPOLDST
     CALL PREPOLDST
  ELSE IF (KTYPE == 'GDIR') THEN
     CALL PREGENPOL
  ELSE IF (KTYPE == 'DDIR') THEN
     CALL PRERADRNG
  ELSE IF (KTYPE == 'END') THEN
     IEND = .TRUE.
!        Get the Final Result
     IF ( .NOT. RECERR) THEN
        NREC = NREC + ICOUNT*JCOUNT
     END IF
     ISTA = .FALSE.
     NEWID = .TRUE.

  ELSE IF (KTYPE /= 'ELEV' .AND. KTYPE /= 'FLAG' .AND. &
       KTYPE /= 'HILL' .AND. KTYPE /= 'ORIG') THEN
!        Invalid Secondary Keyword
     RECERR = .TRUE.
     GO TO 999

  END IF

  PNETID = NETIDT

999 RETURN
END SUBROUTINE PREPOLR

SUBROUTINE PREPOLDST
!***********************************************************************
!                 PREPOLDST Module of the AMS/EPA Regulatory Model - AERMOD

!        PURPOSE: Gets Distances for the Polar Network

!        PROGRAMMER: Roger Brode

!        DATE:    September 24, 1996

!        INPUTS:  Input Runstream Image Parameters

!        OUTPUTS: Polar Network Distance Input Value

!        CALLED FROM:   PREPOLR
!***********************************************************************

!     Variable Declarations
  USE TER_MAIN
  IMPLICIT NONE
  CHARACTER MODNAM*12

  SAVE
  INTEGER :: I, ISET, IMIT

!     Variable Initializations
  MODNAM = 'PREPOLDST'

!     Skip the Unrelated Fields
  DO I = 1, IFC
     IF (FIELD(I) == 'DIST') THEN
        ISC = I + 1
     END IF
  END DO

!     Determine Whether There Are Enough Parameter Fields
  IF (IFC == ISC-1) THEN
!        Missing Parameter
     RECERR = .TRUE.
     GO TO 999
  END IF

  ISET = ICOUNT

  DO I = ISC, IFC
     CALL STONUM(FIELD(I),ILEN_FLD,FNUM,IMIT)
!        Check The Numerical Field
     IF (IMIT == -1) THEN
        RECERR = .TRUE.
     END IF
     ISET = ISET + 1
     IF (ISET > IXM) THEN
        IXM = ISET
     END IF
  END DO

  ICOUNT = ISET

999 RETURN
END SUBROUTINE PREPOLDST

SUBROUTINE PREGENPOL
!***********************************************************************
!                 PREGENPOL Module of the AMS/EPA Regulatory Model - AERMOD

!        PURPOSE: Generates Polar Receptor Network With
!                 Uniform Spacing

!        PROGRAMMER: Roger Brode

!        DATE:    September 24, 1996

!        INPUTS:  Input Runstream Image Parameters

!        OUTPUTS: Polar Receptor Network With Uniform Direction Spacing

!        CALLED FROM:   PREPOLR
!***********************************************************************

!     Variable Declarations
  USE TER_MAIN
  IMPLICIT NONE
  CHARACTER MODNAM*12

  SAVE
  INTEGER :: I, K, IMIT
  real (kind=8) :: TEMPP(3), DIRINI, DIRINC
  LOGICAL :: ERROR

!     Variable Initializations
  MODNAM = 'PREGENPOL'
  ERROR = .FALSE.

!     Check for the Location of the Secondary Keyword, GDIR
  DO I = 1, IFC
     IF (FIELD(I) == 'GDIR') THEN
        ISC = I + 1
     END IF
  END DO

!     Determine Whether There Are Enough Parameter Fields
  IF (IFC == ISC-1) THEN
!        Missing Parameter
     RECERR = .TRUE.
     GO TO 999
  ELSE IF (IFC < ISC+2) THEN
!        Not Enough Parameters
     RECERR = .TRUE.
     GO TO 999
  ELSE IF (IFC > ISC+2) THEN
!        Too Many Parameters
     RECERR = .TRUE.
     GO TO 999
  END IF

!     Input Numerical Values
  DO K = 1, 3
     CALL STODBL(FIELD(ISC + K-1),ILEN_FLD,TEMPP(K),IMIT)
!        Check The Numerical Field
     IF (IMIT == -1) THEN
        RECERR = .TRUE.
        ERROR = .TRUE.
     END IF
  END DO

  IF (ERROR) THEN
     ERROR = .FALSE.
     GO TO 999
  END IF

  JCOUNT = IDNINT(TEMPP(1))
  DIRINI = TEMPP(2)
  DIRINC = TEMPP(3)

!     Assign Them to the Coordinate Arrays
  IF (JCOUNT > IYM) THEN
     IYM = JCOUNT
  END IF

999 RETURN
END SUBROUTINE PREGENPOL

SUBROUTINE PRERADRNG
!***********************************************************************
!                 PRERADRNG Module of the AMS/EPA Regulatory Model - AERMOD

!        PURPOSE: Processes Non-Uniform Polar Network Value

!        PROGRAMMER: Roger Brode

!        DATE:    September 24, 1996

!        INPUTS:  Input Runstream Image Parameters

!        OUTPUTS: Polar Network Directions in Non-Uniform Spacing

!        CALLED FROM:   PREPOLR
!***********************************************************************

!     Variable Declarations
  USE TER_MAIN
  IMPLICIT NONE
  CHARACTER MODNAM*12

  SAVE
  INTEGER :: I, ISET, IMIT

!     Variable Initializations
  MODNAM = 'PRERADRNG'

!     Skip the non-useful Fields
  DO I = 1, IFC
     IF (FIELD(I) == 'DDIR') THEN
        ISC = I + 1
     END IF
  END DO

!     Determine Whether There Are Enough Parameter Fields
  IF (IFC == ISC-1) THEN
!        Error Message: Missing Parameter
     RECERR = .TRUE.
     GO TO 999
  END IF

  ISET = JCOUNT

  DO I = ISC, IFC
     CALL STONUM(FIELD(I),ILEN_FLD,FNUM,IMIT)
!        Check The Numerical Field
     IF (IMIT == -1) THEN
        RECERR = .TRUE.
     END IF
     ISET = ISET + 1
     IF (ISET > IYM) THEN
        IYM = ISET
     END IF
  END DO

  JCOUNT = ISET

999 RETURN
END SUBROUTINE PRERADRNG

SUBROUTINE ALLSETUP
!***********************************************************************
!                 ALLSETUP Module for AERMAP

!        PURPOSE: Allocate Array Storage for SETUP

!        PROGRAMMER: Roger W. Brode
!                    U.S. EPA, OAQPS/AQAD

!        DATE:    May 5, 2008


!        INPUTS:


!        OUTPUTS:

!        CALLED FROM:  MAIN

!        ERROR HANDLING:   Checks for error allocating arrays
!***********************************************************************

!     Variable Declarations
  USE TER_MAIN
  IMPLICIT NONE
  CHARACTER MODNAM*12

!     Variable Initializations
  MODNAM = 'ALLSET'

  ! print*,"Allocating using the following parameters:"
  ! print*,"  NDEM    = ",NDEM
  ! print*,"  NRED    = ",NREC
  ! print*,"  NSRC    = ",NSRC
  ! print*,"  NNET    = ",NNET
  ! print*,"  ixm,iym = ",ixm,iym

  ALLOCATE (IDMUNT(NDEM), IDRUNT(NDEM), IDXUNT(NDEM))
  ALLOCATE (ITiffDbg_Unt(NDEM))
  ALLOCATE (DEMFIL(ndem), DIRFIL(ndem), IDXFIL(NDEM))
  ALLOCATE (TiffDbgFil(NDEM))

  ALLOCATE (AXR(NREC), AYR(NREC))
  ALLOCATE (AXS(NSRC), AYS(NSRC))
  ALLOCATE (RLAT(NREC), RLON(NREC))
  ALLOCATE (SLAT(NSRC), SLON(NSRC))
  ALLOCATE (XRECU(NREC), YRECU(NREC))
  ALLOCATE (XCOORD(ixm,nnet))
  ALLOCATE (YCOORD(iym,nnet))
  ALLOCATE (NETID(NREC), RECTYP(NREC), NTID(NNET))
  ALLOCATE (NTTYP(NNET), ARCID(NARC), RECNAM(NREC))

  ALLOCATE (LRec_FILLED(NREC,NDEM), LSrc_FILLED(NSRC,NDEM))

  ALLOCATE (XSRCU(NSRC), YSRCU(NSRC))
  ALLOCATE (XORIG(nnet), YORIG(nnet))
  ALLOCATE (XRDIFS(NREC), YRDIFS(NREC))
  ALLOCATE (XRDIFM(NREC), YRDIFM(NREC))
  ALLOCATE (XSDIFS(NSRC), YSDIFS(NSRC))
  ALLOCATE (XSDIFM(NSRC), YSDIFM(NSRC))
  ALLOCATE (DATUMSHFT(NREC), DATUMSHFTS(NSRC))

  ALLOCATE (HC(NREC), AZELEV(NREC), AZFLAG(NREC), AZS(NSRC))
  ALLOCATE (AZHILL(NREC))
  ALLOCATE (ZETMP1(NREC), ZETMP2(NREC))
  ALLOCATE (ZFTMP1(NREC), ZFTMP2(NREC))

  ALLOCATE (DXM(NDEM), DYM(NDEM), DCI(NDEM), UserDCI(NDEM))

  ALLOCATE (NUMPRF(NDEM), IZOND(NDEM))

  ALLOCATE (IZONR(NREC), IZONS(NSRC), IREF(NREC))
  ALLOCATE (IRIN(NREC,NDEM), ISIN(NSRC,NDEM))
  ALLOCATE (JDM(NREC), JDMS(NSRC))
  ALLOCATE (NDXARC(NREC))
  ALLOCATE (NUMXPT(NNET), NUMYPT(NNET))
  ALLOCATE (NETSTA(NNET), NETEND(NNET))

  ALLOCATE (SRCID(NSRC), SRCTYP(NSRC))

  ALLOCATE (FT(NDEM), ADJMAP(NDEM+1,9))
  ALLOCATE (DEMLVL(NDEM), ELEVPAT(NDEM), IPLAN(NDEM))
  ALLOCATE (IZO(NDEM), ACCUC(NDEM), LPINT(NDEM))
  ALLOCATE (CUNIT(NDEM), ELUNIT(NDEM), SIDZ(NDEM))
  ALLOCATE (UserElevUnits(NDEM))
  ALLOCATE (NROW(NDEM), NPROF(NDEM), LPRIM(NDEM))
  ALLOCATE (SPRIM(NDEM), SPINT(NDEM), DDATE(NDEM))
  ALLOCATE (DVALD(NDEM), SUSF(NDEM), VDAT(NDEM))
  ALLOCATE (DINSP(NDEM))
  ALLOCATE (EDITN(NDEM), PVOID(NDEM), NADD(NDEM))
  ALLOCATE (MAPNAME(NDEM+1))
  ALLOCATE (FLN(NDEM))
  ALLOCATE (Chr_UserElevUnits(NDEM))

  Allocate (tiffType(NDEM))                ! data orgination in GeoTIFF ('strip' || 'tile')
  ALLOCATE (byteSwap(NDEM))                ! endianness of NED GeoTiff file
  ALLOCATE (nCols(NDEM), nRows(NDEM))      ! number of rows and cols in NED GeoTIFF
  ALLOCATE (dataOS(NDEM))                  ! offset (bytes) to data strips or tiles in NED GeoTIFF
  ALLOCATE (rowsPerStrip(NDEM))            ! number of rows per strip of data in NED GeoTIFF
  Allocate (tileLen(NDEM))                 ! length (height) of tile in pixels
  Allocate (tileWid(NDEM))                 ! width of tile in pixels
  Allocate (bytesPerSample(NDEM))          ! number of bytes per data sample

  ALLOCATE (tiePtx(NDEM))    ! upper left X coordinate NED GeoTIFF
  ALLOCATE (tiePty(NDEM))    ! upper left Y coordinate NED GeoTIFF
  ALLOCATE (tiePtz(NDEM))    ! upper left Z coordinate NED GeoTIFF

  ALLOCATE (pxlScalex(NDEM))    ! model pixel scale X-direction
  ALLOCATE (pxlScaley(NDEM))    ! model pixel scale Y-direction
  ALLOCATE (pxlScalez(NDEM))    ! model pixel scale Z-direction

  ALLOCATE (SWE_MTRS(NDEM), SWN_MTRS(NDEM))
  ALLOCATE (NWE_MTRS(NDEM), NWN_MTRS(NDEM))
  ALLOCATE (NEE_MTRS(NDEM), NEN_MTRS(NDEM))
  ALLOCATE (SEE_MTRS(NDEM), SEN_MTRS(NDEM))
  ALLOCATE (SWLAT_ARCS(NDEM), SWLON_ARCS(NDEM))
  ALLOCATE (NWLAT_ARCS(NDEM), NWLON_ARCS(NDEM))
  ALLOCATE (NELAT_ARCS(NDEM), NELON_ARCS(NDEM))
  ALLOCATE (SELAT_ARCS(NDEM), SELON_ARCS(NDEM))
  ALLOCATE (SWLAT_DEGS(NDEM), SWLON_DEGS(NDEM))
  ALLOCATE (NWLAT_DEGS(NDEM), NWLON_DEGS(NDEM))
  ALLOCATE (NELAT_DEGS(NDEM), NELON_DEGS(NDEM))
  ALLOCATE (SELAT_DEGS(NDEM), SELON_DEGS(NDEM))

  ALLOCATE (SW_UTMZ(NDEM), SE_UTMZ(NDEM))
  ALLOCATE (NW_UTMZ(NDEM), NE_UTMZ(NDEM))

  ALLOCATE (ELEVMAX(NDEM))
  ALLOCATE (DLTN(NDEM), DLGE(NDEM))
  ALLOCATE (MPROJ(NDEM,15), DMCNR(NDEM,2,4))
  ALLOCATE (ELEVMN(NDEM), ELEVMX(NDEM))
  ALLOCATE (CNTRC(NDEM), SECTNL(NDEM), MCTR(NDEM))
  ALLOCATE (MAPN(NDEM), FREEF(NDEM), FILR1(NDEM), PROCODE(NDEM))
  ALLOCATE (FILR2(NDEM), INSPF(NDEM))

  ALLOCATE (L_DEMCHECK(NDEM), L_UserElevUnits(NDEM), &
       L_TiffDebug(NDEM), &
       L_NEDSkip(NDEM))

  RETURN
END SUBROUTINE ALLSETUP
