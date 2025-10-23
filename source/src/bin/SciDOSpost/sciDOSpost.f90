!
!*************************************************************************
!
PROGRAM runSciDosPost

! SCIDOSPOST version 5.0, 2019-01-09

USE sciDOSpostTools
USE time_tools   ! adapted from MMIF
USE netcdf_tools ! adapted from CAMx v6.50
USE SCIPtool
USE DefSize_fd
USE tooluser_fd
USE domainCoord_fd
USE sagdef_fd
USE sagstr_fd
USE PtrGrdStrItf
USE Extract_fi
USE GetTimes_fi
USE scipuff_fi, ONLY: create,ntypm,material,mat_mc,tzone
USE multcomp_fd, ONLY: MC_CHEM
!USE chem_fi,    ONLY: chem        ! uncomment    to back-port to SCICHEM 3.1
USE chem_fi,    ONLY: chemMC, chem ! comment out  to back-port to SCICHEM 3.1
USE error_fi,   ONLY: nthreads     ! comment out  to back-port to SCICHEM 3.1
USE MPI_fi,     ONLY: useMPI,myid
USE met_fi,     ONLY: PrjCoord

IMPLICIT NONE

TYPE( SAGgrid_str ), POINTER                   :: grdC, grdA, grdD
TYPE( SCIPFieldCoordinateT )                   :: RecCoordIn, RecCoordOut
TYPE( SCIPPointT  ), DIMENSION(:), ALLOCATABLE :: RecLoc

CHARACTER (len=8)                :: maxPath
CHARACTER (len=20)               :: units
CHARACTER (len=PATH_MAXLENGTH)   :: dosFile, depFile, adosFile
CHARACTER (len=256)              :: Command
character (len=8)                :: DateStr  ! current date
character (len=10)               :: TimeStr  ! current time

INTEGER             :: nOuts            ! number of Output values to find
INTEGER             :: iOut             ! index for Output
INTEGER             :: nRsets           ! number of Receptor Sets to process
INTEGER             :: iRset            ! index for Receptor Set
INTEGER             :: iRec             ! index for receptor within Receptor Set
INTEGER             :: MaxRec           ! max( nRec(:) )
INTEGER             :: MaxNX,MaxNY      ! max of Rset%Grid$NX,Rset%Grid$NX
INTEGER             :: MaxRank          ! max( output(:)%rank )
INTEGER             :: avg_time         ! used to convert units (s)
INTEGER             :: astat            ! short for alloc_stat
INTEGER             :: irv              ! return value for SCIP calls
INTEGER             :: ij               ! index of lat,lon where answer is found
INTEGER             :: iy,im,id,ih      ! year, month, day, hour
INTEGER             :: iymdh, iymdh1    ! YYYYMMDDHH used in a few places
INTEGER             :: iRun_Hours       ! Length of this run, in hours
INTEGER             :: iy1,im1,id1,ih1, iy2,im2,id2,ih2 ! used to calc iRun_Hours
INTEGER             :: iClass, iChoice, iKind, iDOS, iDEP
INTEGER             :: i, j, iTime, ios, iArg, ir, ibeg, iend
INTEGER             :: lun_dos, lun_ados, lun_dep
INTEGER             :: numCommand
INTEGER(2)          :: ios2

INTEGER, DIMENSION(:), ALLOCATABLE :: cGrdI, aGrdI, dGrdI

LOGICAL             :: lexist, lopen, lAmb, OK ! does a file exist, open ?
LOGICAL             :: lLATLON          ! is Project in LAT-LON?
LOGICAL             :: isMULT = .false. ! multi-component or tracer run?
LOGICAL             :: first  = .true.  ! first of the projName files, do init
LOGICAL             :: skip             ! either read or skip next timestamp

REAL                :: con_units_conv, dep_units_conv ! units conversion factors
REAL                :: answer, tmp      ! stepping stones toward what we seek

REAL                :: tot_SO4,tot_NO3,tot_NH4,tot_HNO3,tot_NH3
REAL                :: amb_SO4,amb_NO3,amb_NH4,amb_HNO3,amb_NH3
REAL                :: PNO3_old,PNH4_old
REAL                :: Tk,Pa,Hg,Hsx,rh  ! Temp., pressure (atm), abs. humidity,
                                        ! saturation humidity, relative humidity
REAL                :: Pmb              ! Pressure in mb

INTEGER, EXTERNAL   :: SCIPGetPrjCoord
INTEGER, EXTERNAL   :: SAG_InitList,  SAG_ClearList,  SAG_SetSpecialValue
INTEGER, EXTERNAL   :: SAG_OpenID,    SAG_CloseID,    SAG_RmvGrdStr
INTEGER, EXTERNAL   :: SAG_NewGrdStr, SAG_InitGridID, SAG_ReadHeaderID

CHARACTER(128), EXTERNAL :: AddExtension

INTEGER, EXTERNAL :: nf_close

!*************************************************************************
!--- BEGIN MAIN PROGRAM
!*************************************************************************

!--- Start up : Initialize common

nthreads      = 1 ! comment out to back-port to SCICHEM 3.1
myid          = 0
useMPI        = .FALSE.
create        = .FALSE.
lAmb          = .FALSE.

!Command line strings

PrjName   = ''
ini_file  = ''

!--- Get the input file from the command line

controlFile = 'sciDOSpost.inp' ! default
numCommand  = COMMAND_ARGUMENT_COUNT()
iArg = 0
do while (iArg < numCommand)
   iArg = iArg + 1
   CALL GET_COMMAND_ARGUMENT( iArg, Command, ios2 )
   if (Command(1:3) == '-I:') then
      read(Command(4:len_trim(Command)),'(a256)') ini_file
   elseif (trim(Command) == '-i') then
      iArg = iArg + 1
      CALL GET_COMMAND_ARGUMENT( iArg, controlFile, ios2 )
   else if (trim(Command) == '-h' .or. trim(Command) == '--help') then
      call usage
   else if (trim(Command) == '--sample') then
      call sample_control_file
   else if (trim(Command) == '--version') then
      write(*,*) "SciDOSpost version 1.1, 2017-02-19"
      stop
   else if (trim(Command) == '--debug') then
      debug = .true.
   else
      write(*,*) "Error: unknown switch ",trim(Command)
      call usage
   end if
end do

!--- Open the log file, using the basename of the controlFile

i = index(controlFile, '.', .true.) - 1 ! last char before  extension
if (i <= 0) i = len_trim(controlFile)   ! might not have an extension
logFile = controlFile(1:i) // '.log'
OPEN(30, FILE=logFile)

! INQUIRE(UNIT=30,OPENED=lopen,IOSTAT=ios)
! IF ( .NOT. lopen ) THEN
!    print*,"Appending to ",trim(logFile)
!    OPEN(30,FILE=logFile,position='append')
! ELSE
!    print*,"Creating ",trim(logFile)
!    OPEN(30,FILE=logFile)
! END IF

!--- Start up : Write Screen Header

CALL InitSciDosPost()

call date_and_time(datestr,timestr) ! current time, for the POSTFILE header

WRITE(maxPath,'(I0)') PATH_MAXLENGTH

WRITE(6,'(/,A)')'Program : SciDosPost v'//TRIM(CODE_VERSION)//' &
     &(maxPath='//ADJUSTL(TRIM(maxPath))//')'

irv = SCIPGetVersionString(USER_ID,toolString)
IF( irv == SCIPsuccess )THEN
   irv = SCIPGetPathMaxLength()
   IF( irv == PATH_MAXLENGTH )THEN
      WRITE(6,'(A)')'   uses : SCIPtool v'//TRIM(toolString%string)
   ELSE
      nError = UK_ERROR
      eMessage = 'Error:SCIPtool PATH_MAXLENGTH mismatch'
      eInform ='SCIPUFFPostProcess='//ADJUSTL(TRIM(maxPath))//' : SCIPtool='
      WRITE(maxPath,'(I0)')irv
      eInform =TRIM(eInform)//ADJUSTL(TRIM(maxPath))
      GO TO 9999
   END IF
ELSE
   nError = UK_ERROR
   eMessage = 'Error checking SCIPtool version'
   GO TO 9999
END IF

!--- Start up : Initialize SCIPtool library

CALL InitToolSciDosPost()
IF ( nError /= NO_ERROR ) GOTO 9999

!--- Count the number of occurences of a few keywords

call control_counts(nRsets,nOuts)
IF ( nError /= NO_ERROR ) GOTO 9999

!--- Allocate the arrays, based on the counts

allocate( ProjName(nProj),STAT=astat )
IF( astat > 0 )THEN
    nError = UK_ERROR
    WRITE(eMessage,*) 'Error allocating ProjName(nProj)'
    GO TO 9999
END IF

allocate( output(nOuts),STAT=astat )
IF( astat > 0 )THEN
   nError = UK_ERROR
   WRITE(eMessage,*) 'Error allocating outputs(nOuts)'
   WRITE(eInform,*)  'nOuts = ',nOuts
   GO TO 9999
END IF
output(1:nOuts)%exceeds  = HUGE(0.)  ! largest number possible
output(1:nOuts)%rank     = 1         ! default to output rank 1 (H1H)
output(1:nOuts)%lun      = 0         ! default logical unit number
output(1:nOuts)%units    = "ug/m3"   ! default output units
output(1:nOuts)%outTstep = 0         ! initialize netCDF time index

allocate( Rset(nRsets),STAT=astat )
IF( astat > 0 )THEN
   nError = UK_ERROR
   WRITE(eMessage,*) 'Error allocating Rset(nRsets)'
   GO TO 9999
END IF

allocate( nRec(nRsets), STAT=astat )
if ( astat > 0 ) then
   nError = UK_ERROR
   WRITE(eMessage,*) 'Error allocating nRec with size ',nRsets*4
   GO TO 9999
end if

!--- Parse the control file

call read_control
IF( nError /= NO_ERROR ) goto 9999

!--- Do some sanity checks for a valid list of settings

do iOut = 1,nOuts

   if (output(iOut)%ConDepVis /= "CON" .and. output(iOut)%stat == "M") then
      write(*,*) "Error for output number",iOut
      write(*,*) "  Running Max Daily averages only available for CONcentrations."
      write(*,*) "  Output number ",iOut," is for ",output(iOut)%ConDepVis, &
           " but is asking for a ",output(iOut)%Period," MaxDaily average. "
      write(*,*) "  Stopping."
      stop
   end if

   if (.not. any(output(iOut)%RecSet == Rset(:)%Name )) then
      write(*,'(a,i5,5a)') "Error for output number",iOut
      write(*,*) "  Receptor Set (",trim(output(iOut)%outFile),") is '",  &
           trim(output(iOut)%RecSet), "'."
      write(*,*) "  The list of possible Receptor Sets you gave is:"
      write(*,'(a,$)') "      "
      do iRset = 1, nRsets
         write(*,'(2a,$)') trim(Rset(iRset)%Name)," "
      end do
      write(*,*) ! trailing LF
      write(*,*) "  Receptor Set not found."
      write(*,*) "  Stopping."
      stop
   end if

   if ( output(iOut)%TimeFilter == "ALL" .and. output(iOut)%OutType == "GRD") &
        then
      write(*,*) "Error for output number",iOut
      write(*,*) "  GRD files can only handle one time period, but you asked for"
      write(*,*) "  a TIME selection criteria (4th word) of 'All'. You must request"
      write(*,*) "  something like '1st' or '4th' or '8th'."
      write(*,*) "  Stopping."
      stop
   end if

   if ( (output(iOut)%outType == "NC" .or. output(iOut)%outType == "GRD") &
        .and. output(iOut)%SpaceFilter /= "ALL") then
      write(*,*) "Error for output number",iOut
      write(*,*) "  NC and GRD are file formats that only make sense for gridded data,"
      write(*,*) "  but you asked for a SPACE Selection criteria that is not ALL,"
      write(*,*) "  so won't pass through the whole grid."
      write(*,*) "  Stopping."
      stop
   end if

   ij = 0 ! find which receptor set is goes with this output
   do iRset = 1, nRsets
      if (output(iOut)%RecSet == Rset(iRset)%Name) ij = iRset
   end do
   iRset = ij
   if ( (output(iOut)%outType == "NC" .or. output(iOut)%outType == "GRD") &
        .and. .not. Rset(iRset)%isGrid ) then
      write(*,*) "Error for output number",iOut
      write(*,*) "  NC and GRD are file formats that only make sense for gridded data,"
      write(*,*) "  but you requested NC/GRD output for a discreet RECeptor set."
      write(*,*) "  Use a different file format, or use a GRIDded receptor set."
      write(*,*) "  Stopping."
      stop
   end if

end do ! do iOut = 1,nOuts

!--- Read in the lat-lon or UTM receptors, after allocating lat and lon

call read_rec_set( nRsets, MaxRec, MaxNX, MaxNY )
IF( nError /= NO_ERROR ) goto 9999
if (debug) then
   print*,"nRsets = ",nRsets
   do i = 1,nRsets
      print*,"iRset,nRec(i),IsGrid  = ",i,nRec(i)," ",Rset(i)%IsGrid
   end do
   print*,"size,shape of lon ",size(lon),shape(lon)
   print*,"size,shape of lat ",size(lat),shape(lat)
   print*,"size,shape of xrec ",size(xrec),shape(xrec)
   print*,"size,shape of yrec ",size(yrec),shape(yrec)
   call flush(6)
end if

!--- Initialize the visibility calcuations, if we'll need them

if ( any(output(:)%ConDepVis == "VIS") ) then
   call init_visibility(nRsets)
   IF( nError /= NO_ERROR ) goto 9999
end if

ALLOCATE(cGrdI(nProj),aGrdI(nProj),dGrdI(nProj),STAT=irv )
IF( irv /= 0 )THEN
  nError = UK_ERROR
  eRoutine = 'SciDosPost'
  eMessage = 'Error allocating cGrdI'
  WRITE(eInform,'(A,I0,A,I0)')'Request =',nProj,' : Error=',irv
  GO TO 9999
END IF

!******** MAIN LOOP OVER PROJECT (Basename) FILES ********

do iProj = 1, nProj

! -- Get Plot Times

   WRITE(30,*)'Reading project files for ',TRIM(ProjName(iProj))
   CALL SplitName( ProjName(iProj),Project%name,Project%path ) !project name, path
   Project%ID      = 0
   Project%version = 0

!==== Action : Retrieve output times

   irv = SCIPNumPlotTimes( callerID,Project,nTimePuff,nTimeSrf,nTimeMet,nNotUsed )
   IF( irv /= SCIPsuccess )THEN
      CALL toolError( 'Failed retrieving number of project plot times' )
      GOTO 9999
   END IF

   CALL allocatePlotTimes( )

   irv = SCIPGetPlotTimes( callerID,Project,TimePuff,TimeSrf,TimeMet )
   IF( irv /= SCIPsuccess )THEN
      CALL toolError( 'Failed retrieving project plot times' )
      GOTO 9999
   END IF

   TimeOut  => TimeSrf
   nTimeOut = nTimeSrf

!==== Action : Set Plot Field

   Field%maxCells = limit%surfaceGrid
   Field%project  = TRIM(Project%name)
   Field%path     = TRIM(Project%path)

   Field%class    = -1
   Field%choice   = -1
   Field%category = HP_SURF
   PlotType%type  = HP_MEAN

   if (first) then ! first of the ProjName files, do some initialization stuff

      irv = SCIPNumPlotClasses( callerID,Project,nClass,nChoice,nKind )
      IF( irv /= SCIPsuccess )THEN
         CALL toolError( 'Failed retrieving number of project plot classes' )
         GOTO 9999
      END IF

      CALL allocatePlotLists( )

      irv = SCIPGetPlotClasses( callerID,Project,ClassStr,ChoiceStr,KindStr, &
           CatClassArray,ClassChoiceArray,ProjectCoordinate )
      IF( irv /= SCIPsuccess )THEN
         CALL toolError( 'Failed retrieving project plot classes' )
         GOTO 9999
      END IF

! Convert between LATLON and UTM. xRec,yRec need to be in ProjectCoordinate%mode
! format

      if (debug) then
         write(*,'(a,$)') "Project coords = "
         if (ProjectCoordinate%mode == HD_LATLON)    print*,"LATLON"
         if (ProjectCoordinate%mode == HD_CARTESIAN) print*,"CARTESIAN"
         if (ProjectCoordinate%mode == HD_UTM)       print*,"UTM"
         call flush(6)
      end if
      lLATLON = (ProjectCoordinate%mode == HD_LATLON)  ! switch for later

      DO iRset = 1,nRsets

         if (ProjectCoordinate%mode == HD_UTM) then
            Rset(iRset)%UTMzone = ProjectCoordinate%UTMZone
         else
            Rset(iRset)%UTMzone = 0 ! LATLON or CARTESION
         endif

         if (ProjectCoordinate%mode == HD_LATLON) then  ! SCICHEM run in LATLON
            if (Rset(iRset)%CoordType /= "LATLON") then ! User gave UTM

               RecCoordIn%mode     = HD_UTM
               RecCoordIn%UTMzone  = Rset(iRset)%UTMzone
               RecCoordOut%mode    = HD_LATLON
               RecCoordOut%UTMzone = 0

               allocate( RecLoc(nRec(iRset)) )
               RecLoc(:)%x = xRec(iRset,1:nRec(iRset))
               RecLoc(:)%y = yRec(iRset,1:nRec(iRset))

               irv = SCIPTransformPt( RecCoordIn,RecCoordOut,nRec(iRset),RecLoc )
               IF( irv /= SCIPsuccess )THEN
                  CALL toolError( 'Failed transforming project coordinates' )
                  GOTO 9999
               END IF

               lon(iRset,1:nRec(iRset)) = RecLoc(:)%x
               lat(iRset,1:nRec(iRset)) = RecLoc(:)%y
               deallocate(RecLoc)

            else ! SCICHEM run in LATLON, user gave LATLON, find X,Y

               RecCoordIn%mode     = HD_LATLON
               RecCoordIn%UTMzone  = 0
               RecCoordOut%mode    = HD_UTM
               RecCoordOut%UTMzone = Rset(iRset)%UTMzone

               allocate( RecLoc(nRec(iRset)) )
               RecLoc(:)%x = lon(iRset,1:nRec(iRset))
               RecLoc(:)%y = lat(iRset,1:nRec(iRset))

               irv = SCIPTransformPt( RecCoordIn,RecCoordOut,nRec(iRset),RecLoc )
               IF( irv /= SCIPsuccess )THEN
                  CALL toolError( 'Failed transforming project coordinates' )
                  GOTO 9999
               END IF

               xRec(iRset,1:nRec(iRset)) = RecLoc(:)%x
               yRec(iRset,1:nRec(iRset)) = RecLoc(:)%y
               deallocate(RecLoc)

            end if !if (Rset(iRset)%CoordType /= "LATLON") then

         else      ! SCICHEM run in one of CARTESIAN, UTM, etc.

            if (Rset(iRset)%CoordType == "LATLON") then ! convert LON,LAT to x,y

               irv = SCIPGetPrjCoord( callerID, nRec(iRset),                 &
                    lon(iRset,1:nRec(iRset)),   lat(iRset,1:nRec(iRset)),    &
                    xRec(iRset,1:nRec(iRset)),  yRec(iRset,1:nRec(iRset)) )
               IF( irv /= SCIPsuccess )THEN
                  CALL toolError( 'Failed converting to project coordinates' )
                  GOTO 9999
               END IF

            else ! SCICHEM in UTM, user gave UTM: find lat-lon of each rec

               RecCoordIn%mode     = HD_UTM
               RecCoordIn%UTMzone  = Rset(iRset)%UTMzone
               RecCoordOut%mode    = HD_LATLON
               RecCoordOut%UTMzone = 0

               allocate( RecLoc(nRec(iRset)) )
               RecLoc(:)%x = xRec(iRset,1:nRec(iRset))
               RecLoc(:)%y = yRec(iRset,1:nRec(iRset))

               irv = SCIPTransformPt( RecCoordIn,RecCoordOut,nRec(iRset),RecLoc )
               IF( irv /= SCIPsuccess )THEN
                  CALL toolError( 'Failed transforming project coordinates' )
                  GOTO 9999
               END IF

               lon(iRset,1:nRec(iRset)) = RecLoc(:)%x
               lat(iRset,1:nRec(iRset)) = RecLoc(:)%y
               deallocate(RecLoc)

            end if ! if (Rset(iRset)%CoordType == "LATLON") then
         end if    ! if (ProjectCoordinate%mode == HD_LATLON) then
      END DO       ! DO iRset = 1,nRsets

!--- Read the topography (terrain elevation) for these points

      do iRset = 1, nRsets

         allocate( RecLoc(nRec(iRset)) )

         if (ProjectCoordinate%mode == HD_UTM) then
            RecLoc(:)%x = xRec(iRset,1:nRec(iRset))
            RecLoc(:)%y = yRec(iRset,1:nrec(iRset))
         else
            RecLoc(:)%x = lon(iRset,1:nRec(iRset))
            RecLoc(:)%y = lat(iRset,1:nrec(iRset))
         endif

         irv = SCIPGetTerrainSlice( CallerID, Project, nRec(iRset),     &
              RecLoc, Topo(iRset,1:nRec(iRset)) )
         IF( irv /= SCIPsuccess )THEN
            CALL toolError( 'Failed retrieving project terrain elevations' )
            GOTO 9999
         END IF

         deallocate(RecLoc)

      end do ! do iRset = 1, nRsets

!--- Open the output files and write the headers

      nspcs = 1 ! FIXME: v3.2 can't write multiple species to one file

      call open_files_write_headers(datestr,timestr,eMessage,eInform,   &
           nOuts,nRsets,irv)
      if (irv /= 0) GOTO 9999

!--- Find the iClass for the DOSAGE (con) file, name it iDOS

      iDOS = -1
      DO i = 1,nClass
         string1 = ClassStr(i)%string
         CALL CUPPER( string1 )
         IF( INDEX(string1,'SURFACE DOSAGE') > 0 )THEN
            iDOS = i
            EXIT
         END IF
      END DO
      IF( iDOS == -1 )THEN
         nError = UK_ERROR
         eRoutine = 'SciDosPost'
         eMessage = 'SURFACE DOSAGE not available'
         GOTO 9999
      END IF
      Field%class = iDOS

!--- Find the Choice, which determines multi-component vs. tracer runs

      iChoice = -1
      DO i = 1,nChoice
         string1 = ChoiceStr(i)%string
         CALL CUPPER( string1 )
         IF( INDEX(string1,':COMPONENTS') > 0 )THEN
            iChoice = i
            EXIT
         END IF
      END DO

      isMult = .FALSE.
      IF( iChoice == -1 )THEN
         WRITE(*,*)'This is a Single-Component run.'
         iChoice = 1 ! no ambient ados or asmp files for single-component runs
         nKind   = 1
      ELSE
         WRITE(*,*)'This is a Multi-Component run.'
         isMult = .TRUE.
         if (debug) print*,"iDos,iChoice = ",iDOS,iChoice
         nKind  = ClassChoiceArray(iDOS,iChoice)%nkind
      END IF
      if (debug) print*,"nKind (number of chemical species) =",nKind
      write(*,*) ! blank line after multi-single component message

      Field%choice = iChoice
      CALL setPlotClassData(.FALSE.,.FALSE.)

!--- Find the index of each chemical species we need to use later

      allocate(iCon(nOuts)) ! This triggers whether or not to process in calc_con
      iCon = 0              ! Fill with zeroes, non-zero means 'process this one'

!--- Initialize to -1 so we can detect if they are not found

      iPSO4  = -1 ; iASO4K = -1 ; iPNO3  = -1 ; iANO3K = -1 ; iPSOA  = -1
      iPOC   = -1 ; iPEC   = -1 ; iASOIL = -1 ; iACORS = -1 ; iNO2   = -1
      iNO    = -1 ; iNO3   = -1 ; iN2O5  = -1 ; iHNO3  = -1 ; iHONO  = -1
      iPNA   = -1 ; iPAN   = -1 ; iPANX  = -1 ; iNTR   = -1 ; iNH3   = -1
      iPNH4  = -1 ; iANH4K = -1 ; iSO2   = -1 ; iSULF  = -1
      iO3    = -1 ; iPM25  = -1 ; iPM10  = -1

      write(*,'(a,$)') " Found the following chemical/particulate species: "
      do iKind = 1, nKind

         if (isMULT) then                        ! multi-species output
            Field%kind = iKind + ClassChoiceArray(iDos,iChoice)%ikind - 1
         else                                    ! Only Tracer output
            Field%kind = 0
         endif

         if (isMult) then                        ! multi-species output
            string1 = KindStr(Field%kind)%string
         else                                    ! Only Tracer output
            string1 = ChoiceStr(iChoice)%string
         endif
         CALL CUPPER( string1 )

         if (iKind == 1) then
            write(*,'(a,$)') trim(string1)
         else
            write(*,'(", ",a,$)') trim(string1)
         end if

! These concentrations are to be averaged/ranked and output

         do iOut = 1, nOuts
            if (TRIM(string1) == output(iOut)%chemSpecies) &
               iCon(iOut) = iKind      ! the index of the output species
         end do

! Treat these specially - find the index of these

         if (TRIM(string1) == 'O3'   ) iO3    = iKind ! ozone
         if (TRIM(string1) == 'PM25' ) iPM25  = iKind ! PM 2.5 microns or less
         if (TRIM(string1) == 'PM10' ) iPM10  = iKind ! PM  10 microns or less

! These species are needed for visilbity

         if (TRIM(string1) == 'PSO4' ) iPSO4  = iKind ! small sulfate
         if (TRIM(string1) == 'ASO4K') iASO4K = iKind ! large sulfate
         if (TRIM(string1) == 'PNO3' ) iPNO3  = iKind ! small nitrate
         if (TRIM(string1) == 'ANO3K') iANO3K = iKind ! large nitrate
         if (TRIM(string1) == 'PSOA' ) iPSOA  = iKind ! small organic mass comp
         if (TRIM(string1) == 'POC'  ) iPOC   = iKind ! small organic mass comp
         if (TRIM(string1) == 'PEC'  ) iPEC   = iKind ! elemental carbon
         if (TRIM(string1) == 'ASOIL') iASOIL = iKind ! soil
         if (TRIM(string1) == 'ACORS') iACORS = iKind ! coarse mass
         if (TRIM(string1) == 'NO2'  ) iNO2   = iKind ! nitrogen dioxide

! These additional species are needed for deposition
! Note: NO2, PNO3, ANO3K, PSO4, ASO4K, already found above

         if (TRIM(string1) == 'NO'   ) iNO    = iKind
         if (TRIM(string1) == 'NO3'  ) iNO3   = iKind
         if (TRIM(string1) == 'N2O5' ) iN2O5  = iKind
         if (TRIM(string1) == 'HNO3' ) iHNO3  = iKind
         if (TRIM(string1) == 'HONO' ) iHONO  = iKind
         if (TRIM(string1) == 'PNA'  ) iPNA   = iKind
         if (TRIM(string1) == 'PAN'  ) iPAN   = iKind
         if (TRIM(string1) == 'PANX' ) iPANX  = iKind
         if (TRIM(string1) == 'NTR'  ) iNTR   = iKind
         if (TRIM(string1) == 'NH3'  ) iNH3   = iKind
         if (TRIM(string1) == 'PNH4' ) iPNH4  = iKind
         if (TRIM(string1) == 'ANH4K') iANH4K = iKind
         if (TRIM(string1) == 'SO2'  ) iSO2   = iKind
         if (TRIM(string1) == 'SULF' ) iSULF  = iKind

      end do  ! do i = 1, nKind

      write(*,*) ! CR for list of chemical species found

!--- Warn the user about missing chemical species.  We'll just skip their
!    contributions.

      if (isMult) then ! Don't warn if we don't expect them
         if ( iO3     == -1) write(*,*) "Warning! Did NOT find chemical/particulate species: O3   "
         if ( iPM25   == -1) write(*,*) "Warning! Did NOT find chemical/particulate species: PM25 "
         if ( iPM10   == -1) write(*,*) "Warning! Did NOT find chemical/particulate species: PM10 "
         if ( iPSO4   == -1) write(*,*) "Warning! Did NOT find chemical/particulate species: PSO4 "
         if ( iASO4K  == -1) write(*,*) "Warning! Did NOT find chemical/particulate species: ASO4K"
         if ( iPNO3   == -1) write(*,*) "Warning! Did NOT find chemical/particulate species: PNO3 "
         if ( iANO3K  == -1) write(*,*) "Warning! Did NOT find chemical/particulate species: ANO3K"
         if ( iPSOA   == -1) write(*,*) "Warning! Did NOT find chemical/particulate species: PSOA "
         if ( iPOC    == -1) write(*,*) "Warning! Did NOT find chemical/particulate species: POC  "
         if ( iPEC    == -1) write(*,*) "Warning! Did NOT find chemical/particulate species: PEC  "
         if ( iASOIL  == -1) write(*,*) "Warning! Did NOT find chemical/particulate species: ASOIL"
         if ( iACORS  == -1) write(*,*) "Warning! Did NOT find chemical/particulate species: ACORS"
         if ( iNO2    == -1) write(*,*) "Warning! Did NOT find chemical/particulate species: NO2  "
         if ( iNO     == -1) write(*,*) "Warning! Did NOT find chemical/particulate species: NO   "
         if ( iNO3    == -1) write(*,*) "Warning! Did NOT find chemical/particulate species: NO3  "
         if ( iN2O5   == -1) write(*,*) "Warning! Did NOT find chemical/particulate species: N2O5 "
         if ( iHNO3   == -1) write(*,*) "Warning! Did NOT find chemical/particulate species: HNO3 "
         if ( iHONO   == -1) write(*,*) "Warning! Did NOT find chemical/particulate species: HONO "
         if ( iPNA    == -1) write(*,*) "Warning! Did NOT find chemical/particulate species: PNA  "
         if ( iPAN    == -1) write(*,*) "Warning! Did NOT find chemical/particulate species: PAN  "
         if ( iPANX   == -1) write(*,*) "Warning! Did NOT find chemical/particulate species: PANX "
         if ( iNTR    == -1) write(*,*) "Warning! Did NOT find chemical/particulate species: NTR  "
         if ( iNH3    == -1) write(*,*) "Warning! Did NOT find chemical/particulate species: NH3  "
         if ( iPNH4   == -1) write(*,*) "Warning! Did NOT find chemical/particulate species: PNH4 "
         if ( iANH4K  == -1) write(*,*) "Warning! Did NOT find chemical/particulate species: ANH4K"
         if ( iSO2    == -1) write(*,*) "Warning! Did NOT find chemical/particulate species: SO2  "
         if ( iSULF   == -1) write(*,*) "Warning! Did NOT find chemical/particulate species: SULF "
      end if     ! if (isMult) then
      write(*,*) ! just for looks

!--- Find the units of this DOSAGE file, and get the conversion factor

      Field%timeID   = 1
      Field%userTime = TimeOut(1)%time%runTime        ! integration time in hours
      avg_time       = TimeOut(1)%time%runTime * 3600 ! integration time in sec

      units = TRIM(ClassChoiceArray(iDOS,iChoice)%units)
      call CLOWER( units )
      call find_units_conv(units,"ug/m3", avg_time, con_units_conv)
      if (debug) print*,"units, con_units_conv = ",trim(units),con_units_conv

!--- Find the iClass for the DEPOSITION (drydep+wetdep) file, call it iDEP

      iDEP = -1
      DO i = 1,nClass
         string1 = ClassStr(i)%string
         CALL CUPPER( string1 )
         IF( INDEX(string1,'SURFACE DEPOSITION') > 0 )THEN
            iDEP = i
            EXIT
         END IF
      END DO
      IF( iDEP == -1 )THEN
         nError = UK_ERROR
         eRoutine = 'SciDosPost'
         eMessage = 'SURFACE DEPOSITION not available'
         GOTO 9999
      END IF

!--- Find the units of this DEPOSITION file, and get the conversion factor

      Field%class = iDEP
      units = TRIM(ClassChoiceArray(iDep,iChoice)%units)

      call CLOWER( units )
      call find_units_conv( units, "kg/ha", avg_time, dep_units_conv )
      if (debug) print*,"units, dep_units_conv = ",trim(units),dep_units_conv

!--- Allocate vars to hold the concentration and deposition, at each Receptor
!    set, for nKind chemical species.  Use MaxRec = max(nRec(:))

      if (debug) then
         print*,"Allocation con,TotCon,AmbCon, using size"
         print*,"     nRsets,MaxRec,nKind =",nRsets,MaxRec,nKind
      end if

      ALLOCATE( con(nRsets,MaxRec,nKind), STAT=irv )
      IF( irv /= 0 )THEN
         nError = UK_ERROR
         eRoutine = 'SciDosPost'
         eMessage = 'Error allocating con'
         WRITE(eInform,'(A,I0,A,I0)')'Request =',MaxRec,' : Error=',irv
         GO TO 9999
      END IF
      con = 0.

      ALLOCATE( TotCon(nRsets,MaxRec,nKind), STAT=irv )
      IF( irv /= 0 )THEN
         nError = UK_ERROR
         eRoutine = 'SciDosPost'
         eMessage = 'Error allocating TotCon'
         WRITE(eInform,'(A,I0,A,I0)')'Request =',MaxRec,' : Error=',irv
         GO TO 9999
      END IF
      TotCon = 0.

      ALLOCATE( AmbCon(nRsets,MaxRec,nKind), STAT=irv )
      IF( irv /= 0 )THEN
         nError = UK_ERROR
         eRoutine = 'SciDosPost'
         eMessage = 'Error allocating AmbCon'
         WRITE(eInform,'(A,I0,A,I0)')'Request =',MaxRec,' : Error=',irv
         GO TO 9999
      END IF
      AmbCon = 0.

      ALLOCATE( dep(nRsets,MaxRec,nKind), STAT=irv )
      IF( irv /= 0 )THEN
         nError = UK_ERROR
         eRoutine = 'SciDosPost'
         eMessage = 'Error allocating dep'
         WRITE(eInform,'(A,I0,A,I0)')'Request =',MaxRec,' : Error=',irv
         GO TO 9999
      END IF
      dep = 0.

      ALLOCATE( drydep(nRsets,MaxRec,nKind), STAT=irv )
      IF( irv /= 0 )THEN
         nError = UK_ERROR
         eRoutine = 'SciDosPost'
         eMessage = 'Error allocating drydep'
         WRITE(eInform,'(A,I0,A,I0)')'Request =',MaxRec,' : Error=',irv
         GO TO 9999
      END IF
      drydep = 0.

      ALLOCATE( wetdep(nRsets,MaxRec,nKind), STAT=irv )
      IF( irv /= 0 )THEN
         nError = UK_ERROR
         eRoutine = 'SciDosPost'
         eMessage = 'Error allocating wetdep'
         WRITE(eInform,'(A,I0,A,I0)')'Request =',MaxRec,' : Error=',irv
         GO TO 9999
      END IF
      wetdep = 0.

      if (isMult) then ! also has Temperature, Pressure, Humidity
         ALLOCATE( srfFldVal(nRsets,MaxRec,nKind+3,2), STAT=irv ) ! temporary
      else
         ALLOCATE( srfFldVal(nRsets,MaxRec,nKind,  2), STAT=irv ) ! temporary
      end if
      IF( irv /= 0 )THEN
         nError = UK_ERROR
         eRoutine = 'SciDosPost'
         eMessage = 'Error allocating srfFldVal'
         WRITE(eInform,'(A,I0,A,I0)')'Request =',MaxRec,' : Error=',irv
         GO TO 9999
      END IF

      MaxRank = maxval( output(:)%rank )

      ALLOCATE( sum_val(nRsets,MaxRec,nOuts), STAT=irv )
      IF( irv /= 0 )THEN
         nError = UK_ERROR
         eRoutine = 'SciDosPost'
         eMessage = 'Error allocating sum_val'
         WRITE(eInform,'(A,I0,A,I0)')'Request =',nRsets*MaxRec*nOuts,' : Error=',irv
         GO TO 9999
      END IF
      sum_val = 0.

      ALLOCATE( sum_con(nRsets,MaxRec,nOuts,0:23), STAT=irv )
      IF( irv /= 0 )THEN
         nError = UK_ERROR
         eRoutine = 'SciDosPost'
         eMessage = 'Error allocating sum_con'
         WRITE(eInform,'(A,I0,A,I0)')'Request =',nRsets*MaxRec*nOuts*24,' : Error=',irv
         GO TO 9999
      END IF
      sum_con = 0.

      ALLOCATE( grd_val(nRsets,MaxNX,MaxNY,nOuts), STAT=irv )
      IF( irv /= 0 )THEN
         nError = UK_ERROR
         eRoutine = 'SciDosPost'
         eMessage = 'Error allocating grd_val'
         WRITE(eInform,'(A,I0,A,I0)')'Request =',nRsets*MaxNX*MaxNY*nOuts, &
              ' : Error=',irv
         if (nRsets*MaxNX*MaxNY*nOuts < 0) then
            write(*,*) "Error allocating grd_val: too many GRID outputs. "
            write(*,*) "Split your sciDOSpost run into two, or reduce the size of your GRIDs."
         end if
         GO TO 9999
      END IF
      grd_val = -999. ! "missing value" flag

      ALLOCATE( max_val(nRsets,MaxRec,nOuts,MaxRank), STAT=irv )
      IF( irv /= 0 )THEN
         nError = UK_ERROR
         eRoutine = 'SciDosPost'
         eMessage = 'Error allocating max_val'
         WRITE(eInform,'(A,I0,A,I0)')'Request =',nRsets*MaxRec*nOuts*MaxRank,&
              ' : Error=',irv
         GO TO 9999
      END IF
      max_val = -HUGE(0.) ! most negative number

      ALLOCATE( max_time(nRsets,MaxRec,nOuts,MaxRank), STAT=irv )
      IF( irv /= 0 )THEN
         nError = UK_ERROR
         eRoutine = 'SciDosPost'
         eMessage = 'Error allocating max_time'
         WRITE(eInform,'(A,I0,A,I0)')'Request =',nRsets*MaxRec*nOuts*MaxRank, &
              ' : Error=',irv
         GO TO 9999
      END IF
      max_time = 0

      ALLOCATE( iHours(nRsets,nOuts), STAT=irv )
      IF( irv /= 0 )THEN
         nError = UK_ERROR
         eRoutine = 'SciDosPost'
         eMessage = 'Error allocating iHours'
         WRITE(eInform,'(A,I0,A,I0)')'Request =',nRsets*nOuts, &
              ' : Error=',irv
         GO TO 9999
      END IF
      iHours = 0

      ALLOCATE( iymdh_max(nRsets,MaxRec,nOuts), STAT=irv )
      IF( irv /= 0 )THEN
         nError = UK_ERROR
         eRoutine = 'SciDosPost'
         eMessage = 'Error allocating iymdh_max'
         WRITE(eInform,'(A,I0,A,I0)')'Request =',nOuts, &
              ' : Error=',irv
         GO TO 9999
      END IF
      iymdh_max = 0

      ALLOCATE( dBc_sum(nRsets,MaxRec,nOuts,nCont), STAT=irv )
      IF( irv /= 0 )THEN
         nError = UK_ERROR
         eRoutine = 'SciDosPost'
         eMessage = 'Error allocating dBc_sum'
         WRITE(eInform,'(A,I0,A,I0)')'Request =',nRsets*MaxRec*nOuts*nCont,' : Error=',irv
         GO TO 9999
      END IF
      dBc_sum = 0.

      ALLOCATE( dBc_max(nRsets,MaxRec,nOuts,MaxRank,nCont), STAT=irv )
      IF( irv /= 0 )THEN
         nError = UK_ERROR
         eRoutine = 'SciDosPost'
         eMessage = 'Error allocating dBc_sum'
         WRITE(eInform,'(A,I0,A,I0)')'Request =',nRsets*MaxRec*nOuts*nCont,' : Error=',irv
         GO TO 9999
      END IF
      dBc_max = -HUGE(0.) ! most negative number

      CALL init_error()

      irv = SAG_SetSpecialValue( .TRUE.,HP_SPV )

      irv = SAG_InitList()

      first = .false.

   end if ! if (first) then

   Field%project  = TRIM(Project%name)
   Field%path     = TRIM(Project%path)

   CALL ReadProject( Project )

   CALL init_srf_blocks( ntypm )

   INQUIRE(UNIT=30,OPENED=lopen,IOSTAT=ios) ! FIXME probably don't need this block
   IF ( .NOT. lopen ) then
      print*,"Re-opening ",trim(logFile)
      OPEN(30,FILE=logFile,position='append')
   end IF

!--- Dos File

   dosFile  = TRIM( AddExtension( Project%name,'dos' ) )
   adosFile = TRIM( AddExtension( Project%name,'ados' ) )
   INQUIRE(FILE=TRIM(adosFile),EXIST=lAmb)

!------ Get a new SAG structure

   irv = SAG_NewGrdStr( cgrdI(iProj) )
   IF( irv /= SAG_OK )THEN
      nError   = UK_ERROR
      eRoutine = 'InitGrid'
      eMessage = 'Error creating SAG surface grid'
      CALL ReportFileName( eInform,'File=',dosFile )
      GOTO 9999
   END IF

!------ Initialize SAG structure

   lun_dos = 100 + cgrdI(iProj)
   irv = SAG_InitGridID( dosFile,lun_dos,SAG_GRID_BOTH,0,0,0,cgrdI(iProj) )
   IF( irv /= SAG_OK )THEN
      nError   = UK_ERROR
      eRoutine = 'InitGrid'
      eMessage = 'Error initializing SAG surface grid'
      CALL ReportFileName( eInform,'File=',dosFile )
      GOTO 9999
   END IF

!------ Open surface file; read nvart for block version

   irv = SAG_OpenID( cgrdI(iProj) )
   IF( irv /= SAG_OK )THEN
      nError   = UK_ERROR
      eRoutine = 'InitGrid'
      eMessage = 'Error opening SAG file'
      CALL ReportFileName( eInform,'File=',dosFile )
      GOTO 9999
   END IF

   grdC => SAG_PtrGrdStr( cgrdI(iProj) ) ! Associate "local" grid structure pointer

   grdC%mxnam = grdC%nvart

   IF( lAmb )THEN
!------ Get a new SAG structure
      irv = SAG_NewGrdStr( agrdI(iProj) )
      IF( irv /= SAG_OK )THEN
         nError   = UK_ERROR
         eRoutine = 'InitGrid'
         eMessage = 'Error creating SAG surface grid'
         CALL ReportFileName( eInform,'File=',adosFile )
         GOTO 9999
      END IF

!------ Initialize SAG structure
      lun_ados = 100 + agrdI(iProj)
      irv = SAG_InitGridID( adosFile,lun_ados,SAG_GRID_BOTH,0,0,0,agrdI(iProj) )
      IF( irv /= SAG_OK )THEN
         nError   = UK_ERROR
         eRoutine = 'InitGrid'
         eMessage = 'Error initializing SAG surface grid'
         CALL ReportFileName( eInform,'File=',adosFile )
         GOTO 9999
      END IF

!------ Open surface file; read nvart for block version
      irv = SAG_OpenID( agrdI(iProj) )
      IF( irv /= SAG_OK )THEN
         nError   = UK_ERROR
         eRoutine = 'InitGrid'
         eMessage = 'Error opening SAG file'
         CALL ReportFileName( eInform,'File=',adosFile )
         GOTO 9999
      END IF

      grdA => SAG_PtrGrdStr( agrdI(iProj) ) ! Associate "local" grid structure pointer
      grdA%mxnam = grdA%nvart

   ELSE

      agrdI(iProj) = -1

   END IF

! Deposition file

   depFile = TRIM( AddExtension( Project%name,'dep' ) )

!------ Get a new SAG structure

   irv = SAG_NewGrdStr( dgrdI(iProj) )
   IF( irv /= SAG_OK )THEN
      nError   = UK_ERROR
      eRoutine = 'InitGrid'
      eMessage = 'Error creating SAG surface grid'
      CALL ReportFileName( eInform,'File=',depFile )
      GOTO 9999
   END IF

!------ Initialize SAG structure

   lun_dep = 100 + dgrdI(iProj)
   irv = SAG_InitGridID( depFile,lun_dep,SAG_GRID_BOTH,0,0,0,dgrdI(iProj) )
   IF( irv /= SAG_OK )THEN
      nError   = UK_ERROR
      eRoutine = 'InitGrid'
      eMessage = 'Error initializing SAG surface grid'
      CALL ReportFileName( eInform,'File=',depFile )
      GOTO 9999
   END IF

!------ Open surface file; read nvart for block version

   irv = SAG_OpenID( dgrdI(iProj) )
   IF( irv /= SAG_OK )THEN
      nError   = UK_ERROR
      eRoutine = 'InitGrid'
      eMessage = 'Error opening SAG file'
      CALL ReportFileName( eInform,'File=',depFile )
      GOTO 9999
   END IF

   grdD => SAG_PtrGrdStr( dgrdI(iProj) ) ! Associate "local" grid structure pointer

   grdD%mxnam = grdD%nvart
!
!--- This only works if a single file (single PROJECT) is specified
!
   if (iymdh_beg == 0) then ! flag to process all time found in file

      iTime = 1
      Field%timeID   = iTime
      Field%userTime = TimeOut(iTime)%time%runTime  ! integration time in hours
      iy1 = TimeOut(iTime)%time%year  ! this year
      im1 = TimeOut(iTime)%time%month ! this month
      id1 = TimeOut(iTime)%time%day   ! this month
      ih1 = TimeOut(iTime)%time%hour  ! this month

      iTime = nTimeOut
      Field%timeID   = iTime
      Field%userTime = TimeOut(iTime)%time%runTime  ! integration time in hours
      iy2 = TimeOut(iTime)%time%year  ! this year
      im2 = TimeOut(iTime)%time%month ! this month
      id2 = TimeOut(iTime)%time%day   ! this month
      ih2 = TimeOut(iTime)%time%hour  ! this month

      call TimeDiff(iy1,im1,id1,ih1, iy2,im2,id2,ih2, iRun_Hours)

   else

      call idate2ymdh(iymdh_beg ,iy1,im1,id1,ih1)
      call idate2ymdh(iymdh_end ,iy2,im2,id2,ih2)
      call TimeDiff(iy1,im1,id1,ih1, iy2,im2,id2,ih2, iRun_Hours)
      iRun_Hours = iRun_Hours + 1 ! we're counting the first time-stamp too

   end if

      chem => chemMC(1, 1)

!
!--- Do some sanity checks
!
   do iOut = 1, nOuts
      if ( iRun_Hours < output(iout)%period ) then
1234     format(a,i6,a,i6,a)
         write(*,1234) &
        "***** WARNING: Run Length < the requested averaging Period for output number ", &
              iOut
         write(*,1234) "      Run Length is ",iRun_Hours," hours, requested Period is ",  &
              output(iout)%period," hours."
      end if
   end do
!
!--- Figure out the TimeZone shift of this run:
!      TimeOut(i)%time%reference is 0 for UTC, or 1 for LST (local standard time),
!      and tzone contains the time zone of this SCICHEM run. TimeZone is the
!      output time zone requested by the user.

   if (TimeOut(1)%time%reference == 0) then ! This run is in UTC, correct
      tzShift = TimeZone                    ! to desired output TimeZone.
      write(*,'(a,f3.0)') " This SCICHEM run was done in UTC, time zone ",0
   else                                     ! This run is in LST, correct
      tzShift = nint(TimeZone - tzone)      ! to desired output TimeZone.
      write(*,'(a,f3.0)') " This SCICHEM run was done in LST, time zone ",tzone
   end if
   write(*,'(a,i4,a,i3,a)') " Output will be shifted ",tzShift, &
        " hours to requested output time zone ",TimeZone,"."

   if (debug) print*,"Working on dates from ",iymdh_cur," to ",iymdh_end
!
!--- find the molecular weight of the species, for conversion from ug/m3 to ppb
!
   IF( mat_mc%nMCtype /= 1 )THEN
     nError   = UK_ERROR
     eRoutine = 'SciDosPost'
     eMessage = 'Invalid multicomponent input'
     eInform  = 'Only one MC material is handled'
     GOTO 9999
   END IF

   IF( mat_mc%type(1) /= 1 )THEN
     nError   = UK_ERROR
     eRoutine = 'SciDosPost'
     eMessage = 'Invalid multicomponent input'
     eInform  = 'MC material type must be reactve chemistry'
     GOTO 9999
   END IF

   chem => chemMC(1,1)

   do iOut = 1, nOuts

      output(iOut)%MW = -999. ! flag for not found
      ! Note: Output(iOut)%MW == -999. is possible, for grouped species.
      do i = 1,chem%nSpecies
         if (chem%species(i)%name == output(iOut)%chemSpecies) then
            output(iOut)%MW = chem%species(i)%mw
            if (debug) then
               print*,"Setting MW for Output ",iOut,  &
                    trim(output(iOut)%chemSpecies)," to ",output(iOut)%MW
               call flush(6)
            end if
         end if
      end do

   end do ! do iOut = 1, nOuts

!*****************************************************************
!   LOOP OVER TIME STAMPS IN THIS PROJECT FILE
!*****************************************************************

   do iTime = 1, nTimeOut

!--- Get the concentrations and deposition (wet & dry) at all rececptors,
!    for this time-stamp

      Field%timeID   = iTime
      Field%userTime = TimeOut(iTime)%time%runTime  ! integration time in hours

      iy = TimeOut(iTime)%time%year  ! this year
      im = TimeOut(iTime)%time%month ! this month
      id = TimeOut(iTime)%time%day   ! this month
      ih = TimeOut(iTime)%time%hour  ! this month
      iymdh1 = iymdh2idate(iy,im,id,ih)

      ih = ih + tzShift              ! correct to desired output timezone
      CALL legal_timestamp(iy,im,id,ih,23)
      iymdh = iymdh2idate(iy,im,id,ih)
      if( iymdh_cur == 0 )iymdh_cur = iymdh  ! Use the first available time

      if (debug) then
        write(30,*)"Current time-stamp from file :",TimeOut(iTime)%time
        write(30,*)"Converted to output time zone:",iymdh
        print*
        print*,"Beginning of loop over iTime, current iTime = ",iTime
        print*,"Current time-stamp from file : ",iymdh1
        print*,"Converted to output time zone: ",iymdh
        print*,"iHours(1,1) = ",iHours(1,1)
      end if

!--- Retrieve or skip the concentration for this time-stamp

      if (iymdh_cur <= iymdh .and. iymdh <= iymdh_end) then
         skip = .false.
      else
         skip = .true.
      end if

      srfFldVal = 0.
      CALL GetAllFieldValues( cgrdI(iProj),agrdI(iProj),nRsets,maxRec,nRec, &
           .FALSE.,isMult,skip,lLATLON )
      IF( nError /= NO_ERROR )GOTO 9999


      if (skip) then ! Skip the deposition for this time-stamp
        srfFldVal = 0.
        CALL GetAllFieldValues( dgrdI(iProj),agrdI(iProj),nRsets,maxRec,nRec, &
             .TRUE.,isMult,skip,lLATLON )
        IF( nError /= NO_ERROR )GOTO 9999

        cycle ! skip the rest of the processing for this timestep
      end if

!--- Write some progress to the screen, but not too much

      if ((id == 1 .and. ih == 1) .or. iTime == 1) then
         write(*,*) ! CR
         write(*,'(a,i4,"-",i2.2,a)') "Processing YEAR-MO: ",iy,im,", Day:"
         write(30,'(a,i4,"-",i2.2,a)') "Processing YEAR-MO: ",iy,im,", Day:"
      endif
      if (ih == 1) then
         write(string1,*) id
         write(*,'(a," ",$)') trim(adjustl(string1))
      end if

!-- Increment the timestamp, so iymdh_cur is now the next desired timestamp

     call idate2ymdh(iymdh_cur,iy,im,id,ih)
     ih = ih + 1
     call legal_timestamp(iy,im,id,ih,23)
     iymdh_cur = iymdh2idate(iy,im,id,ih)

      if (debug) then
         do i = 1,nRsets
            print*
            print*,"Receptor set #",i
            print*,"max con in srfFldVal =",maxval(srfFldVal(i,1:nRec(i),1:nKind,1))
            print*,"min con in srfFldVal =",minval(srfFldVal(i,1:nRec(i),1:nKind,1))
            if (isMult) then
               print*,"max T   = ",maxval(srfFldVal(i,1:nRec(i),nKind+1,2))
               print*,"min T   = ",minval(srfFldVal(i,1:nRec(i),nKind+1,2))
            end if
         end do
      end if

      where (srfFldVal == 1.e-30) srfFldVal = 0.

      if (.not. isMult) then

         con = srfFldVal(:,:,:,1) * con_units_conv

      else

         ! TotCon = Total Concentration = plume + background
         TotCon(:,:,:) = srfFldVal(:,:,1:nKind,1) * con_units_conv
         ! AmbCon = Ambient Concentration = background
         AmbCon(:,:,:) = srfFldVal(:,:,1:nKind,2) * con_units_conv

         if ( calc_total_con ) then
            ! chem species:
            con(:,:,:) = TotCon(:,:,1:nKind)
         else
            ! chem species:
            if (debug .and. .false.) then ! FIXME: turn .false. into verbose?
               do i = 1,nRec(1)
                  print*,"i,TotCon, AmbCon:", &
                       i, TotCon(1,i,max(iCon(1),1)), AmbCon(1,i,max(iCon(1),1))
               end do
            end if
            con(:,:,:) = TotCon(:,:,1:nKind) - AmbCon(:,:,1:nKind)

            where (con >= -1.e-10 .and. con < 0.) con = 0. ! prevent small neg.

         end if
!
!---- Re-calculate the SO4/NO3/NH4/HNO3/NH3 partitioning, if possible
!
         if (iPSO4 > 0 .and. iPNO3 > 0 .and. iPNH4 > 0 .and. iHNO3 > 0 .and. &
              iNH3 > 0) then
            do iRset = 1, nRsets
               do iRec = 1, nRec(iRset)

                  Tk = srfFldVal(iRset,iRec,nKind+1,2) ! temperature, K
                  Pa = srfFldVal(iRset,iRec,nKind+2,2) ! pressure, atm
                  Hg = srfFldVal(iRset,iRec,nKind+3,2) ! absolute humidity, g/g
                  Pmb = 1013.25 * Pa                   ! pressure, mb
                  call sat_humid(Tk,Pmb,Hsx)           ! calculate saturation
                  rh = max( min( Hg/Hsx, 1.), 0.01)    ! enforce sane limits

                  if (debug .and. .false.) print*,"Tk,Pa,Hg,rh = ",Tk,Pa,Hg,rh

                  tot_SO4  = TotCon(iRset,iRec,iPSO4)
                  tot_NO3  = TotCon(iRset,iRec,iPNO3)
                  tot_NH4  = TotCon(iRset,iRec,iPNH4)
                  tot_HNO3 = TotCon(iRset,iRec,iHNO3)
                  tot_NH3  = TotCon(iRset,iRec,iNH3 )

                  if (debug .and. .false.) then ! too verbose for normal debug
                     print*,"Before inorg_aero, total concentrations"
                     print*,"tot_PSO4 = ",tot_SO4
                     print*,"tot_PNO3 = ",tot_NO3
                     print*,"tot_PNH4 = ",tot_NH4
                     print*,"tot_HNO3 = ",tot_HNO3
                     print*,"tot_NH3  = ",tot_NH3
                     print*,"tot_NO2  = ",TotCon(iRset,iRec,iNO2 )
                  end if

                  call inorg_aero(tot_SO4,tot_NO3,tot_NH4,tot_HNO3,tot_NH3,Tk,rh)

                  amb_SO4  = AmbCon(iRset,iRec,iPSO4)
                  amb_NO3  = AmbCon(iRset,iRec,iPNO3)
                  amb_NH4  = AmbCon(iRset,iRec,iPNH4)
                  amb_HNO3 = AmbCon(iRset,iRec,iHNO3)
                  amb_NH3  = AmbCon(iRset,iRec,iNH3 )

                  if (debug .and. .false.) then ! too verbose for normal debug
                     print*,"Before inorg_aero, ambient concentrations"
                     print*,"amb_PSO4 = ",amb_SO4
                     print*,"amb_PNO3 = ",amb_NO3
                     print*,"amb_PNH4 = ",amb_NH4
                     print*,"amb_HNO3 = ",amb_HNO3
                     print*,"amb_NH3  = ",amb_NH3
                     print*,"amb_NO2  = ",AmbCon(iRset,iRec,iNO2 )
                  end if

                  call inorg_aero(amb_SO4,amb_NO3,amb_NH4,amb_HNO3,amb_NH3,Tk,rh)

                  PNO3_old = con(iRset,iRec,iPNO3)
                  PNH4_old = con(iRset,iRec,iPNH4)

                  if ( calc_total_con ) then          ! plume + background = total
                     con(iRset,iRec,iPSO4)  = tot_SO4
                     con(iRset,iRec,iPNO3)  = tot_NO3
                     con(iRset,iRec,iPNH4)  = tot_NH4
                     con(iRset,iRec,iHNO3)  = tot_HNO3
                     con(iRset,iRec,iNH3 )  = tot_NH3
                  else                                ! just plume
                     con(iRset,iRec,iPSO4)  = tot_SO4  - amb_SO4
                     con(iRset,iRec,iPNO3)  = tot_NO3  - amb_NO3
                     con(iRset,iRec,iPNH4)  = tot_NH4  - amb_NH4
                     con(iRset,iRec,iHNO3)  = tot_HNO3 - amb_HNO3
                     con(iRset,iRec,iNH3 )  = tot_NH3  - amb_NH3
                  end if

                  if (debug .and. .false.) then ! too verbose for normal debug
                     print*,"After inorg_aero:"
                     print*,"tot_PSO4 = ",tot_SO4
                     print*,"tot_PNO3 = ",tot_NO3
                     print*,"tot_PNH4 = ",tot_NH4
                     print*,"tot_HNO3 = ",tot_HNO3
                     print*,"tot_NH3  = ",tot_NH3
                     print*,"amb_PSO4 = ",amb_SO4
                     print*,"amb_PNO3 = ",amb_NO3
                     print*,"amb_PNH4 = ",amb_NH4
                     print*,"amb_HNO3 = ",amb_HNO3
                     print*,"amb_NH3  = ",amb_NH3
                     print*,"con_PSO4 = ",con(iRset,iRec,iPSO4)
                     print*,"con_PNO3 = ",con(iRset,iRec,iPNO3)
                     print*,"con_PNH4 = ",con(iRset,iRec,iPNH4)
                     print*,"con_HNO3 = ",con(iRset,iRec,iHNO3)
                     print*,"con_NH3  = ",con(iRset,iRec,iNH3 )
                     print*,"con_NO2  = ",con(iRset,iRec,iNO2 )
                  endif

                  ! Adjust PM25 and PM10 for the newly-created vals

                  con(iRset,iRec,iPM25) = con(iRset,iRec,iPM25) & ! old PM2.5
                       + con(iRset,iRec,iPNO3) - PNO3_old & ! new - old PNO3
                       + con(iRset,iRec,iPNH4) - PNH4_old   ! new - old PNH4

                  con(iRset,iRec,iPM10) = con(iRset,iRec,iPM10) & ! old PM10
                       + con(iRset,iRec,iPNO3) - PNO3_old & ! new - old PNO3
                       + con(iRset,iRec,iPNH4) - PNH4_old   ! new - old PNH4

               end do ! do iRec = 1, nRec(iRset)
            end do    ! do iRset = 1, nRsets
         end if       ! if (iPSO4 > 0 .and. iPNO3 > 0 .and. iPNH4 > 0 .and. ...
      end if          ! if (.not. isMult) then

!--- Retrieve the deposition for this time-stamp

      srfFldVal = 0.
      CALL GetAllFieldValues( dgrdI(iProj),agrdI(iProj),nRsets,maxRec,nRec, &
           .TRUE.,isMult,skip,lLATLON )
      IF( nError /= NO_ERROR )GOTO 9999

      if (debug) then
         do i = 1,nRsets
            print*
            print*,"Receptor set #",i
            print*,"max dep in srfFldVal = ",maxval(srfFldVal(i,1:nRec(i),1:nKind,1))
            print*,"min dep in srfFldVal = ",minval(srfFldVal(i,1:nRec(i),1:nKind,1))
         end do
         print*
      end if

      where (srfFldVal == 1.e-30) srfFldVal = 0.
      dep(:,:,:) = (srfFldVal(:,:,:,1) + srfFldVal(:,:,:,2)) * dep_units_conv
      drydep(:,:,:) = srfFldVal(:,:,:,1) * dep_units_conv
      wetdep(:,:,:) = srfFldVal(:,:,:,2) * dep_units_conv

!*****************************************************************
!   LOOP OVER RECEPTOR SETS
!*****************************************************************

      do iOut = 1, nOuts
         do iRset = 1, nRsets

            if (output(iOut)%RecSet == Rset(iRset)%Name) then

               if (output(iOut)%Period == 8760 .and. iHours(iRset,iOut) == 0 &
                    .and. leap_yr(iy)) output(iOut)%Period = 8784

               if (output(iOut)%ConDepVis == "CON") then ! iCon is like iKind here

                  CALL calc_con( iRset, iCon(iOut), iOut, iymdh)
                  IF( nError /= NO_ERROR ) goto 9999

               else if (output(iOut)%ConDepVis == "DEP") then

                  CALL calc_dep( iRset, iCon(iout), iOut, iymdh) ! iCon == iDep
                  IF( nError /= NO_ERROR ) goto 9999

               else if (output(iOut)%ConDepVis == "DRY") then

                  CALL calc_dry_dep( iRset, iCon(iout), iOut, iymdh) ! iCon==iDep
                  IF( nError /= NO_ERROR ) goto 9999

               else if (output(iOut)%ConDepVis == "WET") then

                  CALL calc_wet_dep( iRset, iCon(iout), iOut, iymdh) ! iCon==iDep
                  IF( nError /= NO_ERROR ) goto 9999

               else if (output(iOut)%ConDepVis == "VIS" .and. &
                    Rset(iRset)%class == 1) &
                    then

                  CALL calc_vis( iRset, output(iOut)%ChemSpecies, iOut, im, iymdh)
                  IF( nError /= NO_ERROR ) goto 9999

               end if ! CON vs DEP vs DRY vs WET vs VIS if-then branch..

            end if ! if (output(iOut)%RecSet == Rset(iRset)%Name) then
         end do    ! do iRset = 1, nRsets
      end do       ! do iOut  = 1, nOuts
   end do          ! do iTime = 1, nTimeOut
end do             ! do iProj = 1, nProj

write(*,*)      ! CR on last write(*,'(1x,i2.2,$)') TimeOut(iTime)%time%day
write(*,*)      ! an extra, just for looks

! Done with calculations, now write out the final time period's answers

if (debug) then
   print*,"At end of data, calculating final answers."
   print*,"iHours(1,1) = ",iHours(1,1)
end if

do iOut = 1, nOuts
   do iRset = 1, nRsets

      if (output(iOut)%RecSet == Rset(iRset)%Name) then

         write(*,*) "Writing output ",iOut," ",      &
              trim(output(iOut)%chemSpecies)," ",    &
              trim(output(iOut)%ConDepVis), " for ", &
              trim(Rset(iRset)%Name), " to ",              &
              trim(output(iOut)%outFile)

!--- If this output's 1st full Period hasn't yet been reached, calculate the
!    average/max anyway, as long as we have reached 75% of the period, and
!    set the hours to what we actually found if it's the first output.

         if (iHours(iRset,iOut) > 0 .and. &
              iHours(iRset,iOut) > 0.75 * output(iout)%period) then
            do iRec = 1, nRec(iRset)

               if (debug) print*,"Calculating final (partial) averaging period"

               if (output(iOut)%ConDepVis == "CON") then

                  if ( output(iOut)%stat == "A" ) then   ! average: div sum/hours

                     sum_val(iRset,iRec,iOut) = &
                          sum_con(iRset,iRec,iOut,1)/iHours(iRset,iOut)

                     if (debug) then
                        print*,"Final (partial) averaging period values for ", &
                             trim(output(iOut)%outFile)
                        print*,"  iHours  = ",iHours(iRset,iOut)
                        print*,"  sum_con = ",sum_con(iRset,iRec,iOut,1)
                        print*,"  sum_val = ",sum_val(iRset,iRec,iOut)
                     end if

                  else if ( output(iOut)%stat == "S" ) then   ! sum

                     sum_val(iRset,iRec,iOut) = sum_con(iRset,iRec,iOut,1)

                     if (debug) then
                        print*,"Final (partial) averaging period values for ", &
                             trim(output(iOut)%outFile)
                        print*,"  iHours  = ",iHours(iRset,iOut)
                        print*,"  sum_con = ",sum_con(iRset,iRec,iOut,1)
                        print*,"  sum_val = ",sum_val(iRset,iRec,iOut)
                     end if

                  else if ( output(iOut)%stat == "M" ) then ! maxDaily running avg

                     answer = -HUGE(0.)
                     do iend = output(iOut)%maxDailyHrs-1, 23

! *** FIXME: handle case of last day ending at hour < 23

                        ibeg = iend - output(iOut)%maxDailyHrs + 1
                        tmp = sum( sum_con(iRset,iRec,iOut,ibeg:iend) ) / &
                             output(iOut)%maxDailyHrs
                        if (tmp > answer) then
                           answer = tmp
                           iymdh_max(iRset,iRec,iOut) = &
                                iymdh2idate(iy,im,id,iend)
                        end if

                     end do
                     sum_val(iRset,iRec,iOut) = answer

                     if (debug) then
                        print*,"Final (partial) MaxDaily values for ", &
                             trim(output(iOut)%outFile)
                        print*,"  ibeg,iend = ",ibeg,iend
                        print*,"  sum_con   = ",sum_con(iRset,iRec,iOut,ibeg:iend)
                        print*,"  sum_val   = ",sum_val(iRset,iRec,iOut)
                     end if

                  end if ! if ( output(iOut)%stat == "A" ) then

               else if (output(iOut)%ConDepVis == "VIS") then

                  if ( output(iOut)%stat == "A" ) then ! average by div sum/hours
                     sum_val(iRset,iRec,iOut) = &
                          sum_val(iRset,iRec,iOut)/iHours(iRset,iOut)
                     do i = 1,nCont
                        dBc_sum(iRset,iRec,iOut,i) = &
                             dBc_sum(iRset,iRec,iOut,i)/iHours(iRset,iOut)
                     end do
                  end if ! already have the Max and the Sum

               else ! DEP, DRY or WET

                  if ( output(iOut)%stat == "A" ) then ! averge: div sum/hours
                     sum_val(iRset,iRec,iOut) = &
                          sum_val(iRset,iRec,iOut)/iHours(iRset,iOut)
                  end if ! already have the Max and the Sum

               end if

               ir = find_rank( sum_val(iRset,iRec,iOut), iRset, iRec, iOut, &
                    output(iOut)%rank)
               ! inserts into max_val at right rank
               call insert( sum_val(iRset,iRec,iOut), iRset, iRec, iOut,     &
                    output(iOut)%rank, ir, iymdh_max(iRset,iRec,iOut) )

               if (output(iOut)%chemSpecies == "CONT") then ! must be VIS
                  do i = 1, nCont ! dBc is the dBext contributions
                     ! inserts into dBc_max
                     call insert_dBc( dBc_sum(iRset,iRec,iOut,i),iRset,iRec, &
                          iOut,i, output(iOut)%rank, ir,                     &
                          iymdh_max(iRset,iRec,iOut) )
                  end do
               end if

            end do ! do iRec = 1, nRec(iRset)
         else if (iHours(iRset,iOut) > 0) then
            write(*,*) "***** WARNING: The (last) number of hours for Output #",&
                 iout,trim(output(iOut)%outFile)
            write(*,*) "      is ",iHours(iRset,iOut),", which is < 75% of ",  &
                 output(iout)%period,"."
            write(*,*) "      This partial period will be skipped. Consider changing the STOP time."
         end if    ! if (iHours(iRset,iOut) < output(iout)%period) then

!--- Skip the exceedence outputs: we wrote those in calc_con() etc.
!    Also skip TimeFilter == "ALL" outputs, because we wrote those in calc_*().

         if (output(iOut)%SpaceFilter == "MAX" .and.                  &
              output(iOut)%TimeFilter  /= "ALL") then  ! the single-point MAX

!--- Find the maximum (over receptors) of the right rank

            if (debug) print*,"Writing SpaceFilter == MAX and TimeFile /= ALL"

            write(string1,*) output(iOut)%rank ! so H1H and H12H both work
            string1 = "H" // trim(adjustl(string1)) // "H"

            ij = -1            ! detect not found
            answer = -HUGE(0.) ! most negative number
            do iRec = 1, nRec(iRset)
               if ( max_val(iRset,iRec,iOut,output(iOut)%rank) > answer) then
                  answer = max_val(iRset,iRec,iOut,output(iOut)%rank)
                  iymdh  = max_time(iRset,iRec,iOut,output(iOut)%rank)
                  ij = iRec
               end if
            end do

!--- Write out the single-point "answer"

            if (output(iOut)%chemSpecies == "CONT") then
               if (ij > 0) tmp7(:) = dBc_max(iRset,ij,iOut,output(iOut)%rank,:)
               call write_vis( iOut, iRset, ij, iymdh, string1, &
                    answer, tmp7, nCont )
            else
               call write_output( iOut, iRset, ij, iymdh, string1, answer )
            end if

         else if  (output(iOut)%SpaceFilter == "ALL" .and.                   &
              output(iOut)%TimeFilter  /= "ALL") then ! write MAX at ALL points

!--- Write out the final "answer" at all points

            if (debug) print*, "Writing final answer at all points"

            write(string1,*) output(iOut)%rank ! so H1H and H12H both work
            string1 = "H" // trim(adjustl(string1)) // "H"

            ! print*,"iOut = ",iout,trim(output(iOut)%outFile)
            ! print*,"About to call write_output, ",max_val(iRset,1,iOut,output(iOut)%rank)
            do iRec = 1, nRec(iRset)

               if (output(iOut)%chemSpecies == "CONT") then
                  tmp7(:) = dBc_max(iRset,iRec,iOut,output(iOut)%rank,:)
                  call write_vis( iOut, iRset, iRec,                         &
                       max_time(iRset,iRec,iOut,output(iOut)%rank), string1, &
                       max_val(iRset,iRec,iOut,output(iOut)%rank),           &
                       tmp7, nCont )
               else
                  call write_output( iOut, iRset, iRec,                      &
                       max_time(iRset,iRec,iOut,output(iOut)%rank), string1, &
                       max_val(iRset,iRec,iOut,output(iOut)%rank) )
               end if

            end do ! do iRec = 1, nRec(iRset)

            if (output(iOut)%OutType == "NC") then

               ! Now that write_output has inserted the values into the grid for
               ! output, write that out.

               ! print*,"Gridded NC Output for ", trim(output(iOut)%ConDepVis)

               output(iOut)%outTstep = output(iOut)%outTstep + 1

               ! print*,"iOut = ",iout,trim(output(iOut)%outFile)
               ! print*,"This iOut's tstep = ",output(iOut)%outTstep
               ! print*,"About to call ncf_wrt, ",grd_val(iRset,1,1,iOut)

               call ncf_wrt_data_tstep(output(iOut)%outFile, output(iOut)%lun,  &
                    nspcs, output(iOut)%outTstep,                               &
                    max_time(iRset,iRec,iOut,output(iOut)%rank),                &
                    output(iOut)%period)

               call ncf_wrt_data_species(output(iOut)%outFile,output(iOut)%lun, &
                    Rset(iRset)%Grid%NX, Rset(iRset)%Grid%NY, nspcs,            &
                    output(iOut)%chemSpecies,                                   &
                    grd_val(iRset,1:Rset(iRset)%Grid%NX,                        &
                                  1:Rset(iRset)%Grid%NY,iOut),                  &
                    output(iOut)%outTstep )

             end if ! if (output(iOut)%OutType == "NC") then
         end if     ! if (output(iOut)%SpaceFilter == "MAX") then
      end if        ! if (output(iOut)%RecSet == Rset(iRset)%Name) then
   end do           ! do iRset = 1, nRsets
end do              ! do iOut  = 1, nOuts

!--- Close the output files

do iOut = 1, nOuts
   if (output(iOut)%OutType == "NC") then
      irv = nc_close( output(iOut)%lun )
   else
      inquire( output(iOut)%lun, opened=lopen )
      if ( lopen ) close( output(iOut)%lun )
   endif
end do ! do iOut  = 1, nOuts

!--- WE'RE DONE!!!

9999 CONTINUE

IF( nError /= NO_ERROR )THEN
  WRITE(*,*)TRIM(eMessage)
  WRITE(*,*)TRIM(eInform)
  WRITE(*,*)TRIM(eAction)
END IF

IF( ALLOCATED(output)         ) DEALLOCATE(output)
IF( ALLOCATED(TotCon)         ) DEALLOCATE(TotCon)
IF( ALLOCATED(AmbCon)         ) DEALLOCATE(AmbCon)
IF( ALLOCATED(con)            ) DEALLOCATE(con)
IF( ALLOCATED(dep)            ) DEALLOCATE(dep)
IF( ALLOCATED(drydep)         ) DEALLOCATE(drydep)
IF( ALLOCATED(wetdep)         ) DEALLOCATE(wetdep)
IF( ALLOCATED(Rset)           ) DEALLOCATE(Rset)
IF( ALLOCATED(srfFldVal)      ) DEALLOCATE(srfFldVal)
IF( ALLOCATED(lat)            ) DEALLOCATE(lat,lon)
IF( ALLOCATED(xRec)           ) DEALLOCATE(xRec,yRec)
IF( ALLOCATED(nRec)           ) DEALLOCATE(nRec)
IF( ALLOCATED(sum_val)        ) DEALLOCATE(sum_val)
IF( ALLOCATED(sum_con)        ) DEALLOCATE(sum_con)
IF( ALLOCATED(max_val)        ) DEALLOCATE(max_val)
IF( ALLOCATED(max_time)       ) DEALLOCATE(max_time)
IF( ALLOCATED(grd_val)        ) DEALLOCATE(grd_val)
IF( ALLOCATED(RecIJ)          ) DEALLOCATE(RecIJ)
IF( ALLOCATED(dBc_sum)        ) DEALLOCATE(dBc_sum)
IF( ALLOCATED(dBc_max)        ) DEALLOCATE(dBc_max)
IF( ALLOCATED(iHours)         ) DEALLOCATE(iHours)
IF( ALLOCATED(NatCond)        ) DEALLOCATE(NatCond)
IF( ALLOCATED(LargeRH)        ) DEALLOCATE(LargeRH)
IF( ALLOCATED(SmallRH)        ) DEALLOCATE(SmallRH)
IF( ALLOCATED(SeaSalt)        ) DEALLOCATE(SeaSalt)
IF( ALLOCATED(Bext_nat)       ) DEALLOCATE(Bext_nat)
IF( ALLOCATED(iCon)           ) DEALLOCATE(iCon)

!------ Close file and deallocate grid structure

INQUIRE(UNIT=30,OPENED=lopen,IOSTAT=ios)
IF ( lopen ) CLOSE(30)

IF( ALLOCATED(cgrdI) ) then
   DO iProj = 1, nProj
      irv = SAG_CloseID( cgrdI(iProj) )
      irv = SAG_RmvGrdStr( cgrdI(iProj) )
      IF( lAmb )THEN
         irv = SAG_CloseID( agrdI(iProj) )
         irv = SAG_RmvGrdStr( agrdI(iProj) )
      END IF
      irv = SAG_CloseID( dgrdI(iProj) )
      irv = SAG_RmvGrdStr( dgrdI(iProj) )
   END DO
END IF

!-- Release memory
CALL ClearMemory()

!-- Clean up : Exit SCIPtool
IF ( lInit ) irv = SCIPExitTool()

if (nError == NO_ERROR) then
   write(*,*) "sciDOSpost finished normally."
else
   write(*,*) "sciDOSpost did NOT run successfully."
endif

END PROGRAM runSciDosPost
