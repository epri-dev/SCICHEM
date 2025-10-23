
MODULE SciDosPostTools

  USE time_tools     ! adapted from MMIF
  USE netcdf_tools   ! adapted from CAMx v6.50
  USE tooluser_fd
  USE Extract_fi
  USE SCIPtool
  USE GetTimes_fi
  USE scipuff_fi, ONLY: create
  USE MPI_fi, ONLY: useMPI,myid
  IMPLICIT NONE

  TYPE an_output
     character (len=3)              :: ConDepVis  ! output type: con, dep, vis
     character (len=128)            :: chemSpecies! chemical species name
     real                           :: MW         ! species molecular weight
     character (len=15)             :: units      ! requested output units
     character (len=3)              :: SpaceFilter! MAX or ALL points output
     real                           :: exceeds    ! print all vals over this
     character (len=20)             :: RecSet     ! which RecSet to use
     character (len=3)              :: TimeFilter ! RANK or ALL times output
     integer                        :: rank       ! 1st or 4th or 8th high
     integer                        :: period     ! The number of hours to...
     character (len=1)              :: stat       ! take the avg or maxDaily.
     integer                        :: maxDailyHrs! Hours for MaxDaily averages
     character (len=4)              :: outType    ! CSV POST PLOT XYZ NC
     character (len=PATH_MAXLENGTH) :: outFile    ! output filename
     integer                        :: outTstep   ! netCDF output TSTEP index
     integer                        :: lun        ! unit to write to
  end TYPE an_output

  TYPE(an_output), allocatable, dimension(:)  :: output ! index: iOut

  TYPE a_RecGrid ! The specification for a grid
     integer                        :: NX, NY     ! grid dimensions
     real                           :: DX         ! grid spacing
     real                           :: xW, yS     ! grid lower-left  (SW) corner
     real                           :: xE, yN     ! grid upper-right (NE) corner
  end type a_RecGrid

  TYPE a_RecSet  ! Receptor sets, either gridded or non-gridded (list)
     integer                        :: class      ! Class I recs can do VIS
     character (len=20)             :: Name       ! user-specified name
     character (len=PATH_MAXLENGTH) :: Filename   ! only for non-gridded rec sets
     character (len=20)             :: CoordType  ! LATLON vs. UTM(km)
     integer                        :: UTMzone    ! UTM zone, 17 = US east coast
     logical                        :: IsGrid     ! flag: gridded vs non-gridded
     TYPE(a_RecGrid)                :: Grid       ! grid specs, not every (i) used
  end type a_RecSet

  TYPE(a_RecSet), allocatable, dimension(:)   :: Rset

! iRset nRset refers to the receptor set, in one file, either Class I or II
! iRec  nRec  refers to a receptor location within a receptor set
! iKind nKind refers to the chemical/aerosol species
! iRank       1 is the 1st highest high, 2 is the 2nd highest high, etc.

! TotCon is the total concentration, AmbCon is the ambient concentration
! We'll store the difference (attributable to the source) in con().

  REAL, DIMENSION(:,:,:),  ALLOCATABLE :: TotCon    !index: iRset,iRec,ikind
  REAL, DIMENSION(:,:,:),  ALLOCATABLE :: AmbCon    !index: iRset,iRec,ikind
  REAL, DIMENSION(:,:,:),  ALLOCATABLE :: con       !index: iRset,iRec,ikind
  REAL, DIMENSION(:,:,:),  ALLOCATABLE :: dep       !index: iRset,iRec,ikind
  REAL, DIMENSION(:,:,:),  ALLOCATABLE :: wetdep    !index: iRset,iRec,ikind
  REAL, DIMENSION(:,:,:),  ALLOCATABLE :: drydep    !index: iRset,iRec,ikind
  REAL, DIMENSION(:,:,:,:),ALLOCATABLE :: srfFldVal !index: iRset,iRec,ikind,2

  REAL, DIMENSION(:,:),    ALLOCATABLE :: Bext_nat  !index: iRset, Month
  REAL, DIMENSION(:,:),    ALLOCATABLE :: NatCond, LargeRH, SmallRH, SeaSalt

  REAL, DIMENSION(:,:,:,:),ALLOCATABLE :: max_val   !index: iRset,iRec,iOut,iRank
  REAL, DIMENSION(:,:,:),  ALLOCATABLE :: sum_val   !index: iRset,iRec,iOut
  REAL, DIMENSION(:,:,:,:),ALLOCATABLE :: sum_con   !index: iRset,iRec,iOut,ihr
  REAL, DIMENSION(:,:,:,:,:),ALLOCATABLE :: dBc_max !index: iRset,iRec,iOut,iRank,i
  REAL, DIMENSION(:,:),    ALLOCATABLE :: lat,lon   ! index: iRset,iRec
  REAL, DIMENSION(:,:),    ALLOCATABLE :: xRec,yRec ! index: iRset,iRec
  REAL, DIMENSION(:,:),    ALLOCATABLE :: topo      ! index: iRset,iRec

  REAL, DIMENSION(:,:,:,:),ALLOCATABLE :: dBc_sum   !index: iRset,iRec,iOut,i
! For netCDF output, we need to re-organize the iRec vals into (Grid%NX,Grid%NY)
  REAL, DIMENSION(:,:,:,:),ALLOCATABLE :: grd_val   !index: iRset,I,J, iOut

  INTEGER, PARAMETER                   :: nCont = 9 !number of VIS components
  REAL, DIMENSION(nCont)               :: tmp7      !used for passing dBc_*

  INTEGER,DIMENSION(:,:,:,:),ALLOCATABLE :: max_time!index: same as max_val
  INTEGER,DIMENSION(:,:,:),ALLOCATABLE :: iymdh_max ! index: iRset,iRec,iOut
  INTEGER,DIMENSION(:,:),  ALLOCATABLE :: iHours    ! index: iRset,iOut
  INTEGER,DIMENSION(:,:,:),ALLOCATABLE :: RecIJ     ! index: iRset,iRec,2
  INTEGER,DIMENSION(:),    ALLOCATABLE :: nRec      ! index: iRset
  INTEGER                              :: iymdh_beg = 0       ! begin output
  INTEGER                              :: iymdh_cur = 0       ! next (current) out
  INTEGER                              :: iymdh_end = HUGE(0) ! end output
  INTEGER                              :: TimeZone  = 0       ! default: UTC-0000
  INTEGER                              :: tzShift   = 0       ! shift this many hr


! The iKind index of the species we need to calc max concentrations

  INTEGER, DIMENSION(:),   ALLOCATABLE :: iCon
  INTEGER  iO3, iPM25, iPM10  ! these get some special treatment

! The iKind index of the species we need to calc total S and N deposition

  INTEGER :: iNO, iNO2, iNO3, iN2O5, iHNO3, iHONO, iPNA, iPAN, iPANX, iNTR
  INTEGER :: iPNO3, iANO3K, iPNH4, iANH4K, iNH3
  INTEGER :: iSO2,  iSULF,  iPSO4, iASO4K

! The iKind index of the species we need to calculate visibility

  ! Already declared iPSO4, iASO4K, iPNO3, iANO3K above
  INTEGER :: iPSOA, iPOC, iPEC, iASOIL, iACORS

! The iKind index of the things we need to do the partitioning

  INTEGER :: iTemperature=1, iPressure=2, iHumidity=3

  INTEGER                           :: ProjCoord

! Other path/filenames we need

  INTEGER             :: nProj  ! number of projName (basename) filenames given
  INTEGER             :: iProj  ! index for projName

  INTEGER             :: nspcs  ! netCDF number chem species per outfile


  CHARACTER (len=PATH_MAXLENGTH), dimension(:), allocatable :: projName

  CHARACTER (len=PATH_MAXLENGTH)    :: controlFile, logFile,    flagDir
  CHARACTER (len=PATH_MAXLENGTH)    :: NatCondFile, LargeRHFile
  CHARACTER (len=PATH_MAXLENGTH)    :: SmallRHFile, SeaSaltFile
  CHARACTER (len=128)               :: string1  ! tmp variable

  CHARACTER (len=1),   dimension(3) :: comment   = (/ "#", ";", "!" /)
  CHARACTER (len=1),   dimension(5) :: delim     = (/ " ", ",", "#", ";", "!" /)
  CHARACTER (len=1),   dimension(3) :: datedelim = (/ "-", "_", ":" /)

  LOGICAL                           :: calc_total_con = .false. ! default=plume
  LOGICAL                           :: debug = .false. ! print lots of debug info

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CONTAINS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE read_control

    implicit none

    integer, parameter   :: lun = 12345
    integer              :: iRset, iOut
    integer              :: ios, ios2, i, j, k, ij, NX, NY
    integer              :: num_words
    integer              :: ibyr,ibmo,ibdy,ibhr, ieyr,iemo,iedy,iehr
    real                 :: xW, yS, xE, yN, DX
    character (len=999)  :: line, word, word2

    iRset = 0     ! initialize
    iOut  = 0     ! initialize
    ibyr  = -9999 ! invalid default, so we can detect missing START keyword
    ieyr  = -9999 ! invalid default, so we can detect missing STOP  keyword

99  format(a999)

    write(*,*)
    write(*,'(3a,$)') " Parsing ",trim(controlfile),", "
    call flush(6)
    open(lun,file=controlFile,status='old',err=999)

    read(lun,99,iostat=ios) line
    call count_words(line,num_words)    ! read to first non-comment non-blank
    do while ((any(line(1:1) == comment) .or. num_words == 0) .and. ios == 0)
       read(lun,99,iostat=ios) line
       call count_words(line,num_words)
    end do

    do while (ios == 0)                 ! loop over lines in control file

       call get_word(line,1,word)
       call CUPPER( word )

       if ( word(1:4) == "PROJ" ) then
          do i = 1, nProj
             call get_word(line,i+1,projName(i))
          end do
       end if

       if ( word(1:4) == "CALC" ) then
          call get_word( line, 2, word2 )
          call CUPPER( word2 )
          if ( word2 == "PLUME") calc_total_con = .false.
          if ( word2 == "TOTAL") calc_total_con = .true.
       end if
!
!---- Special processing for dates to turn 2013-09-30_12 into 2013 09 30 12.
!     This allow WRF-style time-stamps (e.g. 2011-07-02_00:00:00).
!
       if (word == "START" .or. word == "BEGIN" .or. &
           word == "STOP"  .or. word == "END" ) then
          i = 1
          k = len_trim(line) ! like this_len
          do while (i < k)
             if (any(line(i:i) == comment)) k = i - 1
             if (any(line(i:i) == datedelim)) line(i:i) = " "
             i = i + 1
          end do
          call count_words(line,num_words) ! re-count them
       end if
!
!---- Start and Stop times
!
       if (word == "START" .or. word == "BEGIN") then
          if (num_words >= 5) then      ! like 'start 2008 07 04 01'
             call get_word(line,2,word2) ; read(word2,*,err=81) ibyr
             call get_word(line,3,word2) ; read(word2,*,err=81) ibmo
             call get_word(line,4,word2) ; read(word2,*,err=81) ibdy
             call get_word(line,5,word2) ; read(word2,*,err=81) ibhr
          else if (num_words == 2) then ! like 'start 2008070401'
             call get_word(line,2,word2)
             read(word2,'(i4,3i2)',err=81) ibyr,ibmo,ibdy,ibhr
          else
             write(*,*) "Error reading starting time-stamp: ",trim(line)
             stop
          end if
          call legal_timestamp(ibyr,ibmo,ibdy,ibhr,24)
          iymdh_beg = iymdh2idate(ibyr,ibmo,ibdy,ibhr)
          iymdh_cur = iymdh_beg ! set the current time to the start time
       end if

       if (word == "STOP" .or. word == "END") then
          if (num_words >= 5) then      ! like 'stop 2008 07 04 01'
             call get_word(line,2,word2) ; read(word2,*,err=82) ieyr
             call get_word(line,3,word2) ; read(word2,*,err=82) iemo
             call get_word(line,4,word2) ; read(word2,*,err=82) iedy
             call get_word(line,5,word2) ; read(word2,*,err=82) iehr
          else if (num_words == 2) then ! like 'stop 2008070401'
             call get_word(line,2,word2)
             read(word2,'(i4,3i2)',err=82) ieyr,iemo,iedy,iehr
          else
             write(*,*) "Error reading ending time-stamp: ",trim(line)
             stop
          end if
          call legal_timestamp(ieyr,iemo,iedy,iehr,23)
          iymdh_end = iymdh2idate(ieyr,iemo,iedy,iehr)
       end if
!
!---- TimeZone
!
       if (word == "TIMEZONE" .or. word == "TZ") then
          call get_word(line,2,word2)
          read(word2,*,err=87) TimeZone
       endif
!
!---- Non-gridded (irregularly spaced) Receptor Sets
!
       if ( word(1:3) == "REC" ) then
          iRset = iRset + 1
          Rset(iRset)%IsGrid = .false.

          call get_word( line, 2, Rset(iRset)%Name )
          ! call CUPPER( Rset(iRset)%Name )

          call get_word( line, 3, Rset(iRset)%CoordType )
          call CUPPER( Rset(iRset)%CoordType )
          if ( Rset(iRset)%CoordType == "LL" ) Rset(iRset)%CoordType = "LATLON"
          if ( Rset(iRset)%CoordType == "LONLAT") Rset(iRset)%CoordType = "LATLON"

          call get_word( line, 4, word2 )
          if (word2(1:2) == "I ") then
             Rset(iRset)%class = 1
          elseif (word2(1:2) == "II") then
             Rset(iRset)%class = 2
          else
             read(word2,*,err=84) Rset(iRset)%class
          end if

          call get_word( line, 5, Rset(iRset)%Filename )
       end if
!
!---- Gridded (regularly spaced) Receptor Sets
!
       if ( word(1:4) == "GRID" ) then
          iRset = iRset + 1
          Rset(iRset)%IsGrid = .true.
          call get_word( line, 2, Rset(iRset)%Name )      ! user-specified name
          !call CUPPER( Rset(iRset)%Name )

          call get_word( line, 3, Rset(iRset)%CoordType ) ! LATLON vs. UTM(km)
          !call CUPPER( Rset(iRset)%CoordType )
          ! I write it internally as LATLON, but it really should be LONLAT
          if ( Rset(iRset)%CoordType == "LL")     Rset(iRset)%CoordType = "LATLON"
          if ( Rset(iRset)%CoordType == "LONLAT") Rset(iRset)%CoordType = "LATLON"

          call get_word( line, 4, word2 )
          read(word2,*,err=83) Rset(iRset)%Grid%DX

          call get_word( line, 5, word2 )
          read(word2,*,err=83) Rset(iRset)%Grid%xW

          call get_word( line, 6, word2 )
          read(word2,*,err=83) Rset(iRset)%Grid%yS

          call get_word( line, 7, word2 )
          read(word2,*,err=83) Rset(iRset)%Grid%xE

          call get_word( line, 8, word2 )
          read(word2,*,err=83) Rset(iRset)%Grid%yN

          ! calculate the grid dimensions while we're here, after some sanity

          if (Rset(iRset)%Grid%xW > Rset(iRset)%Grid%xE) then
             write(*,*) "Error: 1st X-value in GRID line must be < 2nd X-value."
             write(*,*) "       Problem line is:"
             write(*,*) trim(line)
             stop
          endif
          if (Rset(iRset)%Grid%yS > Rset(iRset)%Grid%yN) then
             write(*,*) "Error: 1st Y-value in GRID line must be < 2nd Y-value."
             write(*,*) "       Problem line is:"
             write(*,*) trim(line)
             stop
          endif

          Rset(iRset)%Grid%NX = ( Rset(iRset)%Grid%xE - Rset(iRset)%Grid%xW ) &
                / Rset(iRset)%Grid%DX + 1
          Rset(iRset)%Grid%NY = ( Rset(iRset)%Grid%yN - Rset(iRset)%Grid%yS ) &
               / Rset(iRset)%Grid%DX + 1

       end if

!
!---- Outputs
!
       if (word(1:3) == "CON" .or. word(1:3) == "DEP" .or.    &
           word(1:3) == "DRY" .or. word(1:3) == "WET" .or.    &
           word(1:3) == "VIS") then
          if (debug) then
             print*,"processing OUTPUTs"
             call flush(6)
          end if
          iOut = iOut + 1

          output(iOut)%ConDepVis = word(1:3)

          ! 2nd word: species

          call get_word(line,2,output(iOut)%chemSpecies )! 2nd word = species
          if (debug) then
             print*,"word 2: ",trim(output(iOut)%chemSpecies); call flush(6)
          end if
          call CUPPER(output(iOut)%chemSpecies)          ! must be uppercase
          if (output(iOut)%chemSpecies(1:4) == "CONT")  &
               output(iOut)%chemSpecies = "CONT"         ! trim any other chars
          if (output(iOut)%chemSpecies(1:5) == "DBEXT") &
               output(iOut)%chemSpecies = "DBEXT"        ! trim any other chars

          ! 3rd word: requested output units

          call get_word(line,3,output(iOut)%units ) ! 3nd word is output units
          call CLOWER( output(iOut)%units )
          if (debug) then
             print*,"word 3: ",trim(output(iOut)%units); call flush(6)
          end if

          ! 4th word: Receptor Set to use for this output

          call get_word(line,4,output(iOut)%RecSet ) ! 4th word chooses iRset
          !call CUPPER ( output(iOut)%RecSet )
          if (debug) then
             print*,"word 4: ",trim(output(iOut)%RecSet); call flush(6)
          end if

          ! 5th word: Space filter

          call get_word( line, 5, word2 )           ! 5th word: max,all,>5
          call CUPPER( word2 )
          if (debug) then
             print*,"word 5: ",trim(word2); call flush(6)
          end if
          if (word2(1:1) == "A") then               ! Max values at ALL points
             output(iOut)%SpaceFilter = "ALL"
             output(iOut)%exceeds = HUGE(0.)        ! flag for write maximum
          else if (word2(1:1) == "M") then          ! MAX value at MAX val point
             output(iOut)%SpaceFilter = "MAX"
             output(iOut)%exceeds = HUGE(0.)        ! flag for write maximum
          else if (word2(1:1) == ">") then          ! greater than, which is the
             read(word2(2:),*) output(iOut)%exceeds ! same as >= [val - tiny()]
             output(iOut)%exceeds = output(iOut)%exceeds - TINY(0.0)
          else if (word2(1:2) == ">=") then         ! greater than or equal to
             read(word2(3:),*) output(iOut)%exceeds
          else
             write(*,*) "Error parsing 5th word of the following line:"
             write(*,*) trim(line)
             write(*,*) "5th word must be ALL, MaxPT, or >=5, etc."
             stop
          end if

          ! 6th word

          call get_word( line, 6, word2 )                ! 6th word is rank or ALL
          if (debug) then
             print*,"word 6: ",trim(word2); call flush(6)
          end if
          call CUPPER( word2 )
          if (word2(1:1) == "A") then                    ! Write all times
             output(iOut)%TimeFilter = "ALL"
             output(iOut)%rank = 1
          else
             output(iOut)%TimeFilter = ""
             i = 1
             do while (ichar(word2(i:i)) >= 48 .and. ichar(word2(i:i)) <= 57)
                i = i + 1
             end do
             i = i - 1              ! the following works for 999th too
             if (debug) then
                print*,"  Reading RANK,",i," ",trim(word2(1:i)); call flush(6)
             end if
             read(word2(1:i),*,iostat=ios2,err=85) output(iOut)%rank
          end if

          ! 7th word: numerical part of Period

          call get_word( line, 7, word2 )                ! 7th word is period
          if (debug) then
             print*,"word 7: ",trim(word2); call flush(6)
          end if
          read(word2,*,iostat=ios2,err=86) output(iOut)%Period

          ! 8th word: units part of Period

          call get_word( line, 8, word2 )                ! 8th word is hr,mo,yr
          if (debug) then
             print*,"word 8: ",trim(word2); call flush(6)
          end if
          call CUPPER( word2 )
          if ( word2 == 'YR' ) then
             if ( leap_yr(ibyr) ) then                 ! number of hours per yr
                output(iOut)%Period = output(iOut)%Period * 8784
             else
                output(iOut)%Period = output(iOut)%Period * 8760
             end if
          else if (word2 == "HR") then
             ! do nothing
          else
             write(*,*) "Error parsing the 8th word of the following line:"
             write(*,*) trim(line)
             write(*,*) "8th word must be HR or YR"
             stop
          end if

          ! 9th word: Statistic

          call get_word( line, 9, word2 )                ! 9th word AVG/MAX/SUM
          if (debug) then
             print*,"word 9: ",trim(word2); call flush(6)
          end if
          call CUPPER( word2 )
          if (word2 == "AVERAGE" .or. word2 == "AVE" .or. word2 == "AVG") then
             output(iOut)%stat = "A"
          else if (word2 == "SUM" .or. word2 == "TOTAL") then
             output(iOut)%stat = "S"
          else if (word2 == "MAXDAILY" .or. word2 == "MAX") then
             output(iOut)%stat = "M"
             output(iOut)%maxDailyHrs = output(iOut)%Period
             output(iOut)%Period = 24 ! the definition of 'daily'
          else
             write(*,*) "Error parsing 9th word of the following line:"
             write(*,*) trim(line)
             write(*,*) "9th word must be Average, MaxDaily, or Sum"
             stop
          end if

          ! 10th word: file type

          call get_word( line,10,output(iOut)%outType )! 9th is CSV, POST, etc
          if (debug) then
             print*,"word 10: ",trim(output(iOut)%outType); call flush(6)
          end if
          call CUPPER( output(iOut)%outType )
          if ( output(iOut)%outType == "NETCDF" .or. & ! allow for some synonyms
               output(iOut)%outType == "CDF")        &
               output(iOut)%outType = "NC"

          ! Temporarily prevent NC=netCDF output.
          ! It works using the PGI compliers, but with ifort compilers we're
          ! having problems. We'll sort this out shortly, and put out an update.
          ! If you're feeling like testing this, remove the following block
          ! and let us know how it goes.

          if (output(iOut)%outType == "NC") then ! FIXME remove later
             write(*,*) "This version does not support netCDF (NC) output."
             write(*,*) "An update is planned for the very near future."
             stop
          endif

          ! 11th word: file name

          call get_word( line,11,output(iOut)%outFile )! 10th word is filename
          if (debug) then
             print*,"word 8: ",trim(output(iOut)%outFile); call flush(6)
          end if

       end if

       read(lun,99,iostat=ios) line
       call count_words(line,num_words)     ! non-blank line
       do while (num_words == 0 .and. ios == 0)
          read(lun,99,iostat=ios) line
          call count_words(line,num_words)
       end do

    end do ! do while (ios == 0) ! loop over lines in control file

    write(*,'(i3,a)') iOut," outputs to be written."
    write(*,*) ! blank line to make output look nice

    close(lun)
    return

81  nError = UK_ERROR
    write(eMessage,*) "Error reading START time in control file " // &
         trim(controlFile)
    return

82  nError = UK_ERROR
    write(eMessage,*) "Error reading END time in control file " // &
         trim(controlFile)
    return

83  nError = UK_ERROR
    write(eMessage,*) "Error reading GRID specs in control file " // &
         trim(controlFile)
    return

84  nError = UK_ERROR
    write(eMessage,*) "Error reading CLASS in Receptor set  from line '" &
         // trim(line) // " in control file "//trim(controlFile)
    return

85  nError = UK_ERROR
    write(eMessage,*) "Error reading (non-ALL) RANK  from line '"       &
         // trim(line) // "' in control file "//trim(controlFile)
    return

86  nError = UK_ERROR
    write(eMessage,*) "Error reading PERIOD from line '"  &
         // trim(line) // "' in control file "//trim(controlFile)
    return

87  nError = UK_ERROR
    write(eMessage,*) "Error reading TIMEZONE from line '"  &
         // trim(line) // "' in control file "//trim(controlFile)
    return

999 nError = UK_ERROR
    write(eMessage,*) "Error opening control file "//trim(controlFile)
    return

  END SUBROUTINE read_control
!
!*************************************************************************
!
  SUBROUTINE control_counts(nRsets,nOuts)

    implicit none

    character (len=999) :: line, word
    integer             :: nRsets, nOuts, ios, num_words
    integer, parameter  :: lun = 100

    open(lun,file=controlFile,status='old',err=999)

    nRsets  = 0 ! initialize
    nOuts   = 0
    nProj   = 0

99  format(a999)

    read(lun,99,iostat=ios) line
    call count_words(line,num_words)    ! read to first non-comment non-blank
    do while ((any(line(1:1) == comment) .or. num_words == 0) .and. ios == 0)
       read(lun,99,iostat=ios) line
       call count_words(line,num_words)
    end do

    do while (ios == 0)

       call get_word(line,1,word)
       call CUPPER( word )

       if (word == "REC" .or. word == "GRID" ) nRsets = nRsets + 1
       if (word == "CON" .or. word == "DEP" .or. word == "DRY" .or. &
           word == "WET" .or. word == "VIS")   nOuts  = nOuts + 1
       if (word(1:4) == "PROJ") then
         call count_words(line,num_words)     ! non-blank line
         nProj = num_words - 1
       end if

       read(lun,99,iostat=ios) line ! read to the next non-comment,
       call count_words(line,num_words)     ! non-blank line
       do while (num_words == 0 .and. ios == 0)
          read(lun,99,iostat=ios) line
          call count_words(line,num_words)
       end do

    end do

    close(lun)
    return

999 nError = UK_ERROR
    write(eMessage,*) "Error opening control file "//trim(controlFile)

  END SUBROUTINE control_counts
!
!*************************************************************************
!
  SUBROUTINE get_word(line,which_word,word)

    IMPLICIT NONE

    character (len=*) :: line       ! input string
    character (len=*) :: word       ! output string
    integer           :: which_word ! which word to return

    integer           :: i,j, this_word, this_len  ! local variables
!
!-----Entry point
!
    i = 1
    this_len = len_trim(line)
    do while (i < this_len)
       if (any(line(i:i) == comment)) this_len = i - 1 ! ignore commented parts
       i = i + 1
    end do

    this_word = 0
    i = 1

    do while (this_word < which_word)
       if (any(line(i:i) == delim)) then ! advance to next non-space
          do while (any(line(i:i) == delim) .and. i < this_len)
             i = i + 1
          end do
       end if                           ! line(i:i) now the beginnig of next word
       if (i < this_len) then
          j = i + 1                     ! next character
       else
          j = i                         ! this character
       end if

       if (line(i:i) == '"') then       ! advance to matching double quote
          do while (line(j:j) /= '"' .and. j < this_len)
             j = j + 1                  ! advance to end of this word (next ")
          end do
       else if (line(i:i) == "'") then  ! advance to matching single quote
          do while (line(j:j) /= "'" .and. j < this_len)
             j = j + 1                  ! advance to end of this word (next ')
          end do
       else                             ! word doesn't start with a quote
          do while ((.not. any(line(j:j) == delim)) .and. j < this_len)
             j = j + 1                  ! advance to end of this word
          end do
          if (j > i .and. any(line(j:j) == delim)) j = j - 1
       end if
       this_word = this_word + 1
       if (this_word == which_word) then
          word = trim(line(i:j))
          if (word(1:1) == '"' .or. word(1:1) == "'") then
             word = word(2:(len_trim(word)-1)) ! remove the quotes
          end if
          return                        ! done, got the right word, exit
       else if (j == this_len) then
          write(*,*) "*** Error: Asked for word ",which_word,", but only ", &
               this_word," words found."
          write(*,*) "           Problematic line is:"
          write(*,*) trim(line)
          nError = UK_ERROR
          WRITE(eMessage,*)"*** Error: Asked for word ",which_word,", but only ", &
               this_word," words found."
          WRITE(eInform,*) "           Problematic line is:"//TRIM(line)

          RETURN
       end if
       i = j + 1                        ! start at next character
    end do

  END SUBROUTINE get_word
!
!*************************************************************************
!
  SUBROUTINE get_csv( line, which_word, word )

    implicit none
    character (len=*) :: line        ! input string
    character (len=*) :: word        ! output string
    character (len=1) :: comma = "," ! delimiter between words
    integer           :: which_word  ! which word to return
    integer           :: i,j, this_word, this_len  ! local variables
!
!-----Entry point
!
    this_len = len_trim(line)
    this_word = 0
    i = 1

    do while (this_word < which_word)
       if (line(i:i) == comma) then     ! advance to next non-comma
          do while (line(i:i) == comma .and. i < this_len)
             i = i + 1
          end do
       end if                           ! line(i:i) now the beginnig of next word
       if (i < this_len) then
          j = i + 1                     ! next character
       else
          j = i                         ! this character
       end if

       if (line(i:i) == '"') then       ! advance to matching double quote
          do while (line(j:j) /= '"' .and. j < this_len)
             j = j + 1                  ! advance to end of this word (next ")
          end do
       else if (line(i:i) == "'") then  ! advance to matching single quote
          do while (line(j:j) /= "'" .and. j < this_len)
             j = j + 1                  ! advance to end of this word (next ')
          end do
       else                             ! word doesn't start with a quote
          do while ((.not. line(j:j) == comma) .and. j < this_len)
             j = j + 1                  ! advance to end of this word
          end do
          if (j > i .and. line(j:j) == comma) j = j - 1
       end if
       this_word = this_word + 1
       if (this_word == which_word) then
          word = trim(line(i:j))
          if (word(1:1) == '"' .or. word(1:1) == "'") then
             word = word(2:(len_trim(word)-1)) ! remove the quotes
          end if
          return                        ! done, got the right word, exit
       else if (j == this_len) then
          write(*,*) "*** Error: Asked for word ",which_word,", but only ", &
               this_word," words found."
          write(*,*) "           Problematic line is:"
          write(*,*) trim(line)
          stop
       end if
       i = j + 1                        ! start at next character
    end do

  END SUBROUTINE get_csv
!
!*************************************************************************
!
  SUBROUTINE count_words(line,num_words)
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 3.0 2013-09-30
!
!     This subroutine counts the number of words in a line.
!
!     Development History:
!     2013-09-05  Original Development (ENVIRON International Corp.)

    implicit none

    character (len=*) :: line       ! input string
    integer           :: num_words  ! the number of words found

    integer           :: i,j,this_len
!
!-----Entry point
!
    i = 1
    this_len = len_trim(line)
    do while (i < this_len)
       if (any(line(i:i) == comment)) this_len = i - 1 ! ignore commented parts
       i = i + 1
    end do

    num_words = 0
    i = 1

    do while (i < this_len)
       if (any(line(i:i) == delim)) then ! advance to next non-space
          do while (any(line(i:i) == delim) .and. i < this_len)
             i = i + 1
          end do
          ! what if there's a delimiter at the end of the line?
          if (i == this_len .and. any(line(i:i) == delim) ) return
       end if                           ! line(i:i) now the beginnig of nextword

       j = i + 1                        ! next character
       if (line(i:i) == '"') then       ! advance to matching double quote
          do while (line(j:j) /= '"' .and. j < this_len)
             j = j + 1                  ! advance to end of this word (next ")
          end do
       else if (line(i:i) == "'") then  ! advance to matching single quote
          do while (line(j:j) /= '"' .and. j < this_len)
             j = j + 1                  ! advance to end of this word (next ')
          end do
       else                             ! word doesn't start with a quote
          do while ((.not. any(line(j:j) == delim)) .and. j < this_len)
             j = j + 1                  ! advance to end of this word
          end do
       end if
       num_words = num_words + 1
       i = j                            ! start at next character
    end do

  END SUBROUTINE count_words
!
!*************************************************************************
!
  SUBROUTINE usage

    write(*,*) "Usage: SciDosPost [-h | --help] [--sample] [-I:scpuff.ini] -i ctrl.inp"
    write(*,*) "Options:"
    write(*,*) "   -i file    Specify control file to use, sciDosPost.inp by default"
    write(*,*) "   -I:file    Specify the location of scipuff.ini to use"
    write(*,*) "   --sample   Print a sample control file to the screen"
    write(*,*) "   --help     Print this help message"
    write(*,*) "   -h         Print this help message"

    stop

  END SUBROUTINE usage
!
!*************************************************************************
!
  SUBROUTINE sample_control_file

1   format(a)

    write(*,1) "; This file can be space-delimited or comma-delimited, or a mixture."
    write(*,1) "; Comment characters are #, ;, and !.  "
    write(*,1) "; Blank lines are ignored."
    write(*,1) "; Keywords are not case-sensitive.  "
    write(*,1) "; Filenames are used verbatim, and can include spaces if enclosed in quotes. "
    write(*,1) "; This sample file uses many alternatives/synonyms, to show what is possible."
    write(*,1) ""
    write(*,1) "# The keyword PROJECT gives a space-delimited list of one or more SCICHEM "
    write(*,1) "# project basenames. A list of basenames is useful if SCICHEM was run in sections"
    write(*,1) "# (e.g. months of a year)."
    write(*,1) ""
    write(*,1) "Project fc_egu1_07s3ab"
    write(*,1) ""
    write(*,1) "# The keyword CALCulate specifies whether to include background concentrations."
    write(*,1) ""
    write(*,1) "calculate plume  ! plume (default) or total = ambient + plume"
    write(*,1) ""
    write(*,1) "# Time to process. If PROJECT is a list of only 1 element (not run in sections), "
    write(*,1) "# you can omit Start and Stop to process ALL data found."
    write(*,1) ""
    write(*,1) "Start 2010-01-01:00   ! any of these forms work, also 2010/01/01 00"
    write(*,1) "stop  2010123123"
    write(*,1) ""
    write(*,1) "# The keyword TIMEZONE specifies the timezone you want to use for the output"
    write(*,1) "# concentrations, independent of the timezone used in the SCICHEM run being"
    write(*,1) "# processed. If SCICHEM used LST of GMT-0500, give 0 to shift back to UTC."
    write(*,1) "# This affects N-hour concentrations (N > 1) and max daily rolling N-hour "
    write(*,1) "# concentrations (because the 'day' is affected by the output timezone)."
    write(*,1) "# TimeZone is relative to GMT, i.e. -5 (GMT-05) is the US East Coast."
    write(*,1) ""
    write(*,1) "TimeZone   -8    ! default is zero, i.e. GMT-00"
    write(*,1) ""
    write(*,1) "# Receptor section:"
    write(*,1) "# Receptor Sets can be specified two ways:"
    write(*,1) "#   Use the REC  keyword to specify a file containing arbitrarily-spaced points."
    write(*,1) "#   Use the GRID keyword to specify a set of regularly-spaced receptors."
    write(*,1) "# NetCDF and GRD output only works with GRIDded (regularly-spaced) receptor sets. "
    write(*,1) "# You can give zero REC lines or zero GRID lines, but you must specify at least"
    write(*,1) "# one REC or GRID to get any output. "
    write(*,1) "# The NAME of each receptor set is cross-referenced in the Output section, below."
    write(*,1) ""
    write(*,1) "# RECeptors (repeatable) keyword specifies a receptor file of (arbitrary) points."
    write(*,1) "# 2nd word is a user-specified NAME.  If the NAME matches the first 4 characters"
    write(*,1) "#    in the FLAG (2010) look-up tables, it is to be treated as a Class I area"
    write(*,1) "#    and VISilbity obscuration can be calculated."
    write(*,1) "# 3rd word is either LONLAT (synonym: LL) or anything else for UTM (Cartesian)."
    write(*,1) "#    The Class I area receptor files are lon,lat,elev so custom files must also "
    write(*,1) "#    be lon,lat (elev is ignored). UTM are in km, not meters."
    write(*,1) "# 4th word is one of:"
    write(*,1) "#    1  if the receptor set is a Class I area  (use lookup tables to calc VIS)"
    write(*,1) "#    2  if the receptor set is a Class II area (i.e. not Class I)"
    write(*,1) "# 5th word is the filename. Files can be the Class I receptor files downloaded "
    write(*,1) "#    from https://www.nature.nps.gov/air/maps/receptors/; or can contain user-"
    write(*,1) "#    specified receptors: one UTMx,UTMy, or LON,LAT pair per line."
    write(*,1) ""
    write(*,1) "#   Name   LL/UTM Class Filename"
    write(*,1) "REC meve   LonLat 1     sciData/Class1Area/meve-recep.dat   ! Mesa Verde National Park"
    write(*,1) "REC wemi   LL     1     sciData/Class1Area/wemi2-recep.dat  ! Weminuche Wilderness Area"
    write(*,1) "REC fence  UTM    2     prop_boundary.rec"
    write(*,1) "rec 50m    utm    2     50m.rec"
    write(*,1) ""
    write(*,1) "# GRID (repeatable) keyword specifies a regularly-spaced receptor grid to use."
    write(*,1) "# 2nd word is a user-specified NAME, referenced in the OUTPUT lines (see below)."
    write(*,1) "# 3rd word is either LONLAT (synonym: LL) or anything else for UTM (Cartesian)."
    write(*,1) "# 4th word is the grid spacing, DX, in cartesian units or lon (=lat)."
    write(*,1) "# 5th and 6th words are the coordinates of the southwest (lower left) gridpoint"
    write(*,1) "# 7th and 8th words are the coordinates of the northeast (upper right) gridpoint"
    write(*,1) "#   If specifying a lat-lon grid, then the X's are longitude, Y's are latitude."
    write(*,1) "#   UTM coordinate are given in km, not meters, in SCICHEM and sciDOSpost."
    write(*,1) "#   GRIDs cannot be treated as Class I areas - those REC sets are ungridded."
    write(*,1) ""
    write(*,1) "#     Name    LL/UTM  DX     xWest       ySouth    xEast       yNorth   "
    write(*,1) "GRID  2x2km   UTM     0.050  495.550     5333.400  497.550     5335.400 "
    write(*,1) "grid  hotspot UTM     0.005  496.123     5345.678  496.223     5345.778 "
    write(*,1) "grid  Example LL      0.0005 -111.059830 48.153835 -111.032951 48.171841"
    write(*,1) ""
    write(*,1) "# Output section:"
    write(*,1) "# 1st word is one of:"
    write(*,1) "#    CON for concentration          (default: ug/m3)"
    write(*,1) "#    DEP for total deposition       (default: kg/ha)"
    write(*,1) "#    WET for wet   deposition       (default: kg/ha)"
    write(*,1) "#    DRY for dry   deposition       (default: kg/ha)"
    write(*,1) "#    VIS for visibility degradation (   only: percent change)"
    write(*,1) "# 2nd word is one of:"
    write(*,1) "#    The chemical SPECIES to process, e.g. no2 or o3 or pm25, etc."
    write(*,1) "#    dBext for the change to the background extinction coefficient for visibility."
    write(*,1) "#    cont  for the contributions of each term in the equation for dBext."
    write(*,1) "# 3rd word is the requested output units:"
    write(*,1) "#    Gas species can be written in ug/m3, ppm, or ppb."
    write(*,1) "#    Particlate species can only be written in ug/m3."
    write(*,1) "#    Deposition can be written in kg/ha, g/m2, kg/m2, g/cm2, etc."
    write(*,1) "#    Visibility can only be written in Percent."
    write(*,1) "# 4th word is the Receptor Set for this output, must match either a REC or GRID"
    write(*,1) "# 5th word is a SPACE selection criteria, one of:"
    write(*,1) "#    AllRec: Output for every receptor. If TIME=ALL, it will be like a POSTFILE."
    write(*,1) "#       This is the only choice that makes sense for GRID outputs."
    write(*,1) "#    MaxRec: Output a single receptor within each receptor set, where the "
    write(*,1) "#       maximum HIGH-highest (HIGH=RANK) occurred, e.g. the H1H, H4H, H8H, etc."
    write(*,1) "#    >=N: Output all values (of HIGH=RANK) that exceed the given concentration."
    write(*,1) "#       Remember that if using 'calculate plume' negative concentrations are"
    write(*,1) "#       valid (e.g. NOx titration of O3), so >0 might discard data."
    write(*,1) "# 6th word is a TIME or RANK selection criteria, one of:"
    write(*,1) "#    All: Output values for all time Periods"
    write(*,1) "#       To get a time series, use a RECeptor set with only 1 point and 'TIME=ALL'."
    write(*,1) "#    1st, 2nd,... 8th (etc) highest HIGH or RANK (which are 'synonyms')."
    write(*,1) "# 7th and 8th words specify the PERIOD over which to take some STATISTIC."
    write(*,1) "# 9th word is the STATISTIC, one of:"
    write(*,1) "#    Average  to take the *windowed* PERIOD average (e.g. 8 for 1-8, 9-16, 17-24)."
    write(*,1) "#       See the examples for 8-hr CO below."
    write(*,1) "#    MaxDaily to take the maximum PERIOD *rolling* average from each 24 hours. "
    write(*,1) "#       See the examples for 8-hr O3 below. Note: a rolling 1-hr MaxDaily is the"
    write(*,1) "#       same as a windowed 1-hr MaxDaily value, so 1-hr NO2/SO2 use MaxDaily."
    write(*,1) "#    Sum (or Total) to take the sum over the PERIOD."
    write(*,1) "#        See the examples for annual N and S deposition below."
    write(*,1) "# 10th word is the output FILE TYPE:"
    write(*,1) "#    CSV  Comma-separated values"
    write(*,1) "#    XZY  Actually XYLLZUT: the X,Y,lat,lon, value, units, timestamp"
    write(*,1) "#    POST AERMOD-style POSTFILE (values for all times and all receptors)"
    write(*,1) "#    PLOT AERMOD-style PLOTFILE (a single high/rank value at all receptor)"
    write(*,1) "#    NC   NetCDF gridded file in I/O API-compliant format (only for GRIDs)"
    write(*,1) "#    GRD  Golden Software's Surfer 6 ascii grid file (only for GRIDs)"
    write(*,1) "# 11th word is the FILENAME, can contain spaces if enclosed in quotes."
    write(*,1) ""
    write(*,1) "# Output Examples (these are just examples, you probably don't want all of them):"
    write(*,1) "#"
    write(*,1) "# An AERMOD-formatted POSTFILE writing every time period for every receptor."
    write(*,1) "# An AERMOD-formatted PLOTFILE writing the Design Value at every receptor."
    write(*,1) "# A data file giving all the receptors that exceed the 1-hr NO2 SIL (7.5ug/m3)."
    write(*,1) "# A single line giving the NO2 Design Value at the highest 8th high receptor."
    write(*,1) "# An AERMOD-formatted PLOTFILE writing the annual average at every receptor."
    write(*,1) "#"
    write(*,1) "#    Spec  Units  RecSet  Space   Time  Period  Statistic  FType Filename"
    write(*,1) "con  no2   ug/m3  2x2km   AllRec  All    1 hr   Average    POST  no2.pst"
    write(*,1) "con  no2   ug/m3  2x2km   AllRec  8th    1 hr   MaxDaily   PLOT  no2.DV.plt"
    write(*,1) "con  no2   ug/m3  50m     >=7.5   1st    1 hr   MaxDaily   XYZ   no2.over.SIL.dat"
    write(*,1) "con  no2   ug/m3  hotspot MaxRec  8th    1 hr   MaxDaily   CSV   no2.DV.max.csv"
    write(*,1) "con  no2   ug/m3  fence   AllRec  1st    1 yr   Average    PLOT  no2.annual.plt"
    write(*,1) ""
    write(*,1) "# An AERMOD-formatted PLOTFILE writing the Design Value at every receptor."
    write(*,1) "# A single line giving the SO2 Design Value at the highest 4th high receptor."
    write(*,1) "# A single line giving the SO2 highest 2nd-high 3hr average value."
    write(*,1) "#"
    write(*,1) "#    Spec  Units  RecSet  Space   Time  Period  Statistic  FType Filename"
    write(*,1) "con  so2   ug/m3  2x2km   MaxRec  4th    1 hr   MaxDaily   PLOT  so2.DV.plt"
    write(*,1) "con  so2   ug/m3  fence   MaxRec  4th    1 hr   MaxDaily   CSV   so2.DV.max.csv"
    write(*,1) "con  so2   ug/m3  hotspot MaxRec  2nd    3 hr   Average    CSV   so2.DV.max.csv"
    write(*,1) ""
    write(*,1) "# An AERMOD-formatted POSTFILE writing every 24-hr average at every receptor."
    write(*,1) "# An AERMOD-formatted PLOTFILE writing the PM2.5 DV at every receptor."
    write(*,1) "# The highest (over space) 24-hour PM2.5 NAAQS DV (for one year)."
    write(*,1) "# The highest (over space) annual  PM2.5 NAAQS    (for one year)."
    write(*,1) "#    Spec  Units  RecSet  Space   Time  Period  Statistic  FType Filename"
    write(*,1) "con  pm25  ug/m3  2x2km   AllRec  All   24 hr   Average    POST  pm25.pst"
    write(*,1) "con  pm25  ug/m3  2x2km   AllRec  8th   24 hr   Average    PLOT  pm25.plt"
    write(*,1) "con  pm25  ug/m3  2x2km   MaxRec  8th   24 hr   Average    CSV   pm25.DV.max.csv"
    write(*,1) "con  pm25  ug/m3  2x2km   MaxRec  1st    1 yr   Average    CSV   pm25.DV.max.csv"
    write(*,1) ""
    write(*,1) "# An AERMOD-formatted POSTFILE of the 24-hour PM10 values. "
    write(*,1) "# User must manually average the output over multiple years."
    write(*,1) "#"
    write(*,1) "#    Spec  Units  RecSet  Space   Time  Period  Statistic  FType Filename"
    write(*,1) "con  pm10  ug/m3  2x2km   All     1st   24 hr   Average    POST  pm10.pst"
    write(*,1) ""
    write(*,1) "# The highest (over space) second-highest (over time) 1-hour average CO NAAQS."
    write(*,1) "# The highest (over space) H2H 8-hour (windowed: 1-8, 9-16, 17-24) average CO."
    write(*,1) "#    Spec  Units  RecSet  Space   Time  Period  Statistic  FType Filename"
    write(*,1) "con  co    ug/m3  50m     MaxRec  2nd    1 hr   Average    CSV   co.DV.max.csv"
    write(*,1) "con  co    ug/m3  50m     MaxRec  2nd    8 hr   Average    CSV   co.DV.max.csv"
    write(*,1) ""
    write(*,1) "# The 8-hour (rolling: 1-8, 2-9, ... 17-24) max daily average (MDA8)."
    write(*,1) "# A gridded netCDF file of the ozone DV in the grid 2x2km."
    write(*,1) "#"
    write(*,1) "#    Spec  Units  RecSet  Space   Time  Period  Statistic  FType Filename"
    write(*,1) "con  o3    ppb    hotspot MaxRec  4th    8 hr   MaxDaily   CSV   o3.DV.max.csv"
    write(*,1) "con  o3    ppb    2x2km   MaxRec  4th    8 hr   MaxDaily   NC    o3.dv.nc"
    write(*,1) ""
    write(*,1) "# Deposition (total dep) and its components dry and wet deposition, "
    write(*,1) "# useful for PSD Class I analyses."
    write(*,1) "#"
    write(*,1) "#    Spec  Units  RecSet  Space   Time  Period  Statistic  FType Filename"
    write(*,1) "dep  N     kg/ha  meve    MaxRec  1st    1 yr   Sum        CSV   max_dep.csv"
    write(*,1) "wet  N     ug/cm2 meve    MaxRec  1st    1 yr   Sum        CSV   max_dep.csv"
    write(*,1) "dry  N     mg/m2  meve    MaxRec  1st    1 yr   Sum        CSV   max_dep.csv"
    write(*,1) "dep  S     kg/ha  meve    MaxRec  1st    1 yr   Sum        CSV   max_dep.csv"
    write(*,1) "wet  S     kg/ha  meve    MaxRec  1st    1 yr   Sum        CSV   max_dep.csv"
    write(*,1) "dry  S     kg/ha  meve    MaxRec  1st    1 yr   Sum        CSV   max_dep.csv"
    write(*,1) "dep  N     kg/ha  wemi    MaxRec  1st    1 yr   Sum        CSV   max_dep.csv"
    write(*,1) "dep  S     kg/ha  wemi    MaxRec  1st    1 yr   Sum        CSV   max_dep.csv"
    write(*,1) ""
    write(*,1) "# Visibility extinction coefficient (dBext) from FLAG (2010), useful"
    write(*,1) "# for PSD Class I analyses. The '>5' means write each 24-hour average that"
    write(*,1) "# has a dBext > 5%, useful counting the days with greater than 5% change."
    write(*,1) "#"
    write(*,1) "#    Spec  Units  RecSet  Space   Time  Period  Statistic FType Filename"
    write(*,1) "vis  dBext %      meve    AllRec  1st   24 hr   Average   CSV   vis_daily.csv  "
    write(*,1) "vis  dBext %      meve    MaxRec  8th   24 hr   Average   CSV   vis_results.csv"
    write(*,1) "vis  Cont  %      meve    MaxRec  8th   24 hr   Average   CSV   vis_contrib.csv"
    write(*,1) "vis  dBext %      wemi    MaxRec  8th   24 hr   Average   PLOT  vis_daily.plt"
    write(*,1) "vis  dBext %      wemi    >5      1st   24 hr   Average   CSV   vis_exceedence.csv"
    write(*,1) "vis  dBext %      wemi    >10     1st   24 hr   Average   CSV   vis_exceedence.csv"

    stop

  END SUBROUTINE sample_control_file
!
!*************************************************************************
!
  SUBROUTINE calc_con( iRset, iKind, iOut, iymdh)

    IMPLICIT NONE
    integer                 :: iRset     ! which receptor set to process
    integer                 :: iKind     ! which chemical species in con() to use
    integer                 :: iOut      ! which output we're working on
    integer                 :: iymdh     ! current time in YYYYMMDDHH format
    integer                 :: iymdh_ans ! time of max over space
    integer                 :: ir        ! rank of this average (H1H, H8H, etc.)
    integer                 :: iRec      ! receptor index within this receptor set
    integer                 :: iy,im,id,ih ! current time
    integer                 :: ibeg, iend  ! begin & end hours of MaxDaily avg
    integer                 :: ij          ! index of lat,lon where answer is
    real                    :: answer,tmp  ! the Max Daily average, temporary


    call idate2ymdh(iymdh,iy,im,id,ih)

!--- add this hour to the running average

    do iRec = 1, nRec(iRset)
       if ( iymdh_max(iRset,iRec,iOut) == 0) iymdh_max(iRset,iRec,iOut) = iymdh

!      Treat concentrations specially, instead of calling max_or_avg.

       if (output(iOut)%stat == "A") then ! windowed average, just sum for now

          sum_con(iRset,iRec,iOut,1) =       & ! ih=1 because we only need one
               sum_con(iRset,iRec,iOut,1) + con(iRset,iRec,iKind)
          iymdh_max(iRset,iRec,iOut) = iymdh   ! will only use last one

       else if (output(iOut)%stat == "S") then ! sum

          sum_con(iRset,iRec,iOut,1) =       & ! ih=1 because we only need one
               sum_con(iRset,iRec,iOut,1) + con(iRset,iRec,iKind)
          iymdh_max(iRset,iRec,iOut) = iymdh   ! will only use last one

       else if (output(iOut)%stat == "M") then ! dailyMax rolling average

          sum_con(iRset,iRec,iOut,ih) = con(iRset,iRec,iKind)

       end if

    end do

    iHours(iRset,iOut) = iHours(iRset,iOut) + 1

!--- Find the n-period average value, then find the rank of this value,
!    and insert the value at the right rank

    if (debug) print*,"Rset,iOut,iHours,sum_con= ",trim(Rset(iRset)%Name), iOut, &
       iHours(iRset,iOut), sum_con(iRset,1,iOut,1)

    if (iHours(iRset,iOut) == output(iout)%period) then ! end frequency period
       if (debug) then
          write(*,*)"Averging for output: ",trim(Rset(iRset)%Name)," ",         &
               trim(Output(iOut)%chemSpecies), iHours(iRset,iOut),              &
               output(iout)%period, iymdh
!          print*
!          print*,"Averging for output(iOut) ",iRset,iOut,iHours(iRset,iOut),&
!               output(iout)%period,iymdh
!          print*,"sum_con = ",sum_con(iRset,1,iOut,1)
!          print*,"avg_val = ",sum_con(iRset,1,iOut,1)/iHours(iRset,iOut)
!          print*
       end if

       ! Find the appropriate statistic (N-hr average or N-hr MaxDaily average)
       ! and store the answer in sum_val(iRset,iRec,iOut)

       do iRec = 1, nRec(iRset)

          if ( output(iOut)%stat == "A" ) then ! average by div sum/hours

             sum_val(iRset,iRec,iOut) = &
                  sum_con(iRset,iRec,iOut,1)/iHours(iRset,iOut)

          else if (output(iOut)%stat == "S") then ! sum

             sum_val(iRset,iRec,iOut) = sum_con(iRset,iRec,iOut,1)

          else if ( output(iOut)%stat == "M" ) then ! find maxDaily running avg

             answer = -HUGE(0.)
             do iend = output(iOut)%maxDailyHrs-1, 23 ! start at hr 7 for 8-hr MD

                ! *** FIXME: handle case of first day starting at hour > 1

                ibeg = iend - output(iOut)%maxDailyHrs + 1
                tmp = sum( sum_con(iRset,iRec,iOut,ibeg:iend) ) / &
                     output(iOut)%maxDailyHrs
                if (tmp > answer) then
                   answer = tmp
                   iymdh_max(iRset,iRec,iOut) = iymdh2idate(iy,im,id-1,iend)
                end if

             end do
             sum_val(iRset,iRec,iOut) = answer

          endif

          ir = find_rank( sum_val(iRset,iRec,iOut), iRset, iRec, iOut,   &
               output(iOut)%rank)

          ! insert into max_val at right rank:

          call insert( sum_val(iRset,iRec,iOut), iRset, iRec, iOut,      &
               output(iOut)%rank, ir, iymdh_max(iRset,iRec,iOut) )

          if ( output(iOut)%TimeFilter  == "ALL" .and.                   &
               output(iOut)%SpaceFilter == "ALL") then ! all recs, all times

             write(string1,*) "All Times"
             if (debug) print*,"Output: ",string1,iOut,iRset,iRec,iymdh, &
                  sum_val(iRset,iRec,iOut)
             call write_output(iOut, iRset, iRec, iymdh_max(iRset,iRec,iOut),  &
                  string1, sum_val(iRset,iRec,iOut))

          elseif ( sum_val(iRset,iRec,iOut) >= output(iOut)%exceeds ) then

             write(string1,*) ">=",output(iOut)%exceeds
             if (debug) print*,"Output: ",string1,iOut,iRset,iRec,iymdh, &
                  sum_val(iRset,iRec,iOut)
             call write_output(iOut, iRset, iRec, iymdh_max(iRset,iRec,iOut),  &
                  string1, sum_val(iRset,iRec,iOut))

          end if

       end do ! do iRec = 1, nRec(iRset)
!
!----After the loop over iRec, we can now find the max value over iRec
!
       if ( output(iOut)%TimeFilter  == "ALL" .and.                   &
            output(iOut)%SpaceFilter == "MAX") then  ! the single-point MAX

!--- Find the maximum (over receptors) of the right rank

          if (debug) print*,"Writing SpaceFilter == MAX and TimeFilter == ALL"

          write(string1,*) output(iOut)%rank ! so H1H and H12H both work
          string1 = "H" // trim(adjustl(string1)) // "H"

          ij = -1            ! detect not found
          answer = -HUGE(0.) ! most negative number
          do iRec = 1, nRec(iRset)
             if ( sum_val(iRset,iRec,iOut) > answer ) then
                answer    = sum_val(iRset,iRec,iOut)
                iymdh_ans = iymdh_max(iRset,iRec,iOut)
                ij = iRec
             end if
          end do

!--- Write out the single-point "answer"

          call write_output( iOut, iRset, ij, iymdh_ans, string1, answer )

       end if
!
!---- After the loop over iRec, we can now write the gridded (netCDF) value
!     for this output, if TimeFilter == "ALL".

       if ( output(iOut)%TimeFilter == "ALL" .and. &
            output(iOut)%OutType    == "NC" ) then

          output(iOut)%outTstep = output(iOut)%outTstep + 1
!          print*,"Gridded NC Output for con, tstep: ",output(iOut)%outTstep
!          print*,"About to call ncf_wrt from calc_con, ",grd_val(iRset,1,1,iOut)
!          print*,"lun = ",output(iOut)%lun
!          print*,grd_val(iRset,2,2,iOut), grd_val(iRset,3,3,iOut)

          call ncf_wrt_data_tstep(output(iOut)%outFile, output(iOut)%lun,       &
               nspcs, output(iOut)%outTstep, iymdh, output(iOut)%period )

          call ncf_wrt_data_species(output(iOut)%outFile, output(iOut)%lun,     &
               Rset(iRset)%Grid%NX, Rset(iRset)%Grid%NY, nspcs,                 &
               output(iOut)%chemSpecies,                                        &
               grd_val(iRset,1:Rset(iRset)%Grid%NX,1:Rset(iRset)%Grid%NY,iOut), &
               output(iOut)%outTstep )

       end if

       if (debug) print*,"In calc_con, end of Period, setting iHours,sum_val,sum_con to zero"

       iHours(iRset,iOut) = 0
       sum_val(iRset,:,iOut) = 0.
       sum_con(iRset,:,iOut,:) = 0.

    end if ! if (iHours(iRset,iOut) == output(iOut)%Period) then

    RETURN

  END SUBROUTINE calc_con
!
!*************************************************************************
!
  SUBROUTINE calc_dep( iRset, iKind, iOut, iymdh )

    IMPLICIT NONE
    integer                 :: iRset  ! which Class I area to process
    integer                 :: iKind  ! which chemical species in dep() to use
    integer                 :: iOut   ! which output we're working on
    integer                 :: ir     ! rank of this average (H1H, H8H, etc.)
    integer                 :: iRec   ! Class I area receptor index
    integer                 :: iymdh  ! current time in YYYYMMDDHH format
    integer                 :: iymdh_ans ! time of max over space
    integer                 :: ij     ! index of lat,lon where answer is found
    real                    :: answer ! the max for all time periods
    real                    :: tmpDep ! Deposition rate for this hour

!--- add this hour to the running total deposition of N and S

    do iRec = 1, nRec(iRset)

       tmpDep = 0.

       if (output(iOut)%chemSpecies == "N") then ! Total Nitrogen Deposition

          if (iNO   > 0) tmpDep = tmpDep &
               + 14./30.  * dep(iRset,iRec,iNO   ) ! NO
          if (iNO2  > 0) tmpDep = tmpDep &
               + 14./46.  * dep(iRset,iRec,iNO2  ) ! NO2
          if (iNO3   > 0) tmpDep = tmpDep &
               + 14./62.  * dep(iRset,iRec,iNO3  ) ! NO3
          if (iN2O5  > 0) tmpDep = tmpDep &
               + 28./102. * dep(iRset,iRec,iN2O5 ) ! N2O5
          if (iHNO3  > 0) tmpDep = tmpDep &
               + 14./63.  * dep(iRset,iRec,iHNO3 ) ! HNO3
          if (iHONO  > 0) tmpDep = tmpDep &
               + 14./47.  * dep(iRset,iRec,iHONO ) ! HONO
          if (iPNA   > 0) tmpDep = tmpDep &
               + 14./79.  * dep(iRset,iRec,iPNA  ) ! HNO4
          if (iPAN   > 0) tmpDep = tmpDep &
               + 14./121. * dep(iRset,iRec,iPAN  ) ! CH3COOONO2
          if (iPANX  > 0) tmpDep = tmpDep &
               + 14./121. * dep(iRset,iRec,iPANX ) ! Higher PAN
          if (iNTR   > 0) tmpDep = tmpDep &
               + 14./130. * dep(iRset,iRec,iNTR  ) ! Org. Nitr
          if (iPNO3  > 0) tmpDep = tmpDep &
               + 28./62.  * dep(iRset,iRec,iPNO3 ) ! NH4NO3
          if (iANO3K > 0) tmpDep = tmpDep &
               + 28./62.  * dep(iRset,iRec,iANO3K) ! NH4NO3
          if (iNH3   > 0) tmpDep = tmpDep &
               + 14./17.  * dep(iRset,iRec,iNH3  ) ! ammonia
          if (iPNH4  > 0) tmpDep = tmpDep &
               + 14./18.  * dep(iRset,iRec,iPNH4 ) ! PM Ammonia
          if (iANH4K > 0) tmpDep = tmpDep &
               + 14./18.  * dep(iRset,iRec,iANH4K) ! PM Ammonia

       elseif (output(iOut)%chemSpecies == "S") then ! Total Sulfur Dep

          if (iSO2   > 0) tmpDep = tmpDep &
               + 32./64.  * dep(iRset,iRec,iSO2  )   ! SO2
          if (iSULF  > 0) tmpDep = tmpDep &
               + 32./98.  * dep(iRset,iRec,iSULF )   ! H2SO4
          if (iPSO4  > 0) tmpDep = tmpDep &
               + 32./96.  * dep(iRset,iRec,iPSO4 )   ! (NH4)2SO4
          if (iASO4K > 0) tmpDep = tmpDep &
               + 32./96.  * dep(iRset,iRec,iASO4K)   ! (NH4)2SO4

       else ! use iKind

          tmpDep = tmpDep + dep(iRset,iRec,iKind)

       end if

       call max_or_avg(sum_val(iRset,iRec,iOut), tmpDep, output(iOut)%stat, &
            iymdh, iymdh_max(iRset,iRec,iOut) )

    end do  ! do iRec = 1, nRec(iRset)

    iHours(iRset,iOut) = iHours(iRset,iOut) + 1

    if (debug) then
       print*,"In calc_dep, finished this hour for all recs for ", &
            trim(output(iOut)%outFile)
       print*,"  iHours  = ",iHours(iRset,iOut)
       print*,"  sum_val = ",sum_val(iRset,1,iOut)
    end if

!--- Find the n-period average value, then find the rank of this value,
!    and insert the value at the right rank

    if (iHours(iRset,iOut) == output(iOut)%Period) then ! end averaging period

       do iRec = 1, nRec(iRset)

          if (debug) then
             print*,"DEP: end of averaging period values for ", &
                  trim(output(iOut)%outFile)
             print*,"  iHours  = ",iHours(iRset,iOut)
             print*,"  sum_val = ",sum_val(iRset,iRec,iOut)
          end if

          if ( output(iOut)%stat == "A" ) then ! average by div sum/hours

             sum_val(iRset,iRec,iOut) = &
                  sum_val(iRset,iRec,iOut)/iHours(iRset,iOut)

          end if ! if "M", we already have maximum, if "S", we already have sum

          ir = find_rank( sum_val(iRset,iRec,iOut), iRset, iRec, iOut,   &
               output(iOut)%rank)

          ! inserts into max_val at right rank, with the right time
          call insert( sum_val(iRset,iRec,iOut), iRset, iRec, iOut,      &
               output(iOut)%rank, ir, iymdh_max(iRset,iRec,iOut) )

          if ( output(iOut)%TimeFilter  == "ALL" .and.                   &
               output(iOut)%SpaceFilter == "ALL") then ! all recs, all times

             write(string1,*) "All Times"
             if (debug) print*,"TimeFilter = ALL Output: ",iOut,iRset,iRec, &
                  iymdh,sum_val(iRset,iRec,iOut)
             call write_output(iOut, iRset, iRec, iymdh_max(iRset,iRec,iOut),  &
                  string1, sum_val(iRset,iRec,iOut))

          elseif ( sum_val(iRset,iRec,iOut) >= output(iOut)%exceeds ) then

             write(string1,*) ">=",output(iOut)%exceeds
             if (debug) print*,"Exceeds Output: ",iOut,iRset,iRec,iymdh, &
                  sum_val(iRset,iRec,iOut)
             call write_output(iOut, iRset, iRec, iymdh_max(iRset,iRec,iOut),  &
                  string1, sum_val(iRset,iRec,iOut))

          end if

       end do   ! do iRec = 1, nRec(iRset)
!
!----After the loop over iRec, we can now find the max value over iRec
!
       if ( output(iOut)%TimeFilter  == "ALL" .and.                   &
            output(iOut)%SpaceFilter == "MAX") then  ! the single-point MAX

!--- Find the maximum (over receptors) of the right rank

          if (debug) print*,"Writing SpaceFilter == MAX and TimeFilter == ALL"

          write(string1,*) output(iOut)%rank ! so H1H and H12H both work
          string1 = "H" // trim(adjustl(string1)) // "H"

          ij = -1            ! detect not found
          answer = -HUGE(0.) ! most negative number
          do iRec = 1, nRec(iRset)
             if ( sum_val(iRset,iRec,iOut) > answer) then
                answer    = sum_val(iRset,iRec,iOut)
                iymdh_ans = iymdh_max(iRset,iRec,iOut)
                ij = iRec
             end if
          end do

!--- Write out the single-point "answer"

          call write_output( iOut, iRset, ij, iymdh_ans, string1, answer )

       end if
!
!---- After the loop over iRec, we can now write the gridded (netCDF) value
!     for this output, if TimeFilter == "ALL".

       if ( output(iOut)%TimeFilter == "ALL" .and. &
            output(iOut)%OutType    == "NC" ) then

          if (debug) print*,"Gridded NC Output for dep:"

          output(iOut)%outTstep = output(iOut)%outTstep + 1

          call ncf_wrt_data_tstep(output(iOut)%outFile, output(iOut)%lun,       &
               nspcs, output(iOut)%outTstep, iymdh, output(iOut)%period )

          call ncf_wrt_data_species(output(iOut)%outFile, output(iOut)%lun,     &
               Rset(iRset)%Grid%NX, Rset(iRset)%Grid%NY, nspcs,                 &
               output(iOut)%chemSpecies,                                        &
               grd_val(iRset,1:Rset(iRset)%Grid%NX,1:Rset(iRset)%Grid%NY,iOut), &
               output(iOut)%outTstep )

       end if

       if (debug) &
            print*,"In calc_dep, end of Period, setting iHours,sum_val,sum_con to zero."

       iHours(iRset,iOut) = 0
       sum_val(iRset,:,iOut) = 0.

    end if ! if (iHours(iRset,iOut) == output(iOut)%Period) then

   RETURN

  END SUBROUTINE calc_dep
!
!*************************************************************************
!
  SUBROUTINE calc_dry_dep( iRset, iKind, iOut, iymdh )

    IMPLICIT NONE
    integer                 :: iRset  ! which Class I area to process
    integer                 :: iKind  ! which chemical species in dep() to use
    integer                 :: iOut   ! which output we're working on
    integer                 :: ir     ! rank of this average (H1H, H8H, etc.)
    integer                 :: iRec   ! Class I area receptor index
    integer                 :: iymdh  ! current time in YYYYMMDDHH format
    integer                 :: iymdh_ans ! time of max over space
    integer                 :: ij     ! index of lat,lon where answer is found
    real                    :: answer ! the max for all time periods
    real                    :: tmpDep ! Deposition rate for this hour

!--- add this hour to the running total deposition of N and S

    do iRec = 1, nRec(iRset)

       tmpDep = 0.

       if (output(iOut)%chemSpecies == "N") then ! Total Nitrogen Deposition

          if (iNO   > 0) tmpDep = tmpDep &
               + 14./30.  * drydep(iRset,iRec,iNO   ) ! NO
          if (iNO2  > 0) tmpDep = tmpDep &
               + 14./46.  * drydep(iRset,iRec,iNO2  ) ! NO2
          if (iNO3   > 0) tmpDep = tmpDep &
               + 14./62.  * drydep(iRset,iRec,iNO3  ) ! NO3
          if (iN2O5  > 0) tmpDep = tmpDep &
               + 28./102. * drydep(iRset,iRec,iN2O5 ) ! N2O5
          if (iHNO3  > 0) tmpDep = tmpDep &
               + 14./63.  * drydep(iRset,iRec,iHNO3 ) ! HNO3
          if (iHONO  > 0) tmpDep = tmpDep &
               + 14./47.  * drydep(iRset,iRec,iHONO ) ! HONO
          if (iPNA   > 0) tmpDep = tmpDep &
               + 14./79.  * drydep(iRset,iRec,iPNA  ) ! HNO4
          if (iPAN   > 0) tmpDep = tmpDep &
               + 14./121. * drydep(iRset,iRec,iPAN  ) ! CH3COOONO2
          if (iPANX  > 0) tmpDep = tmpDep &
               + 14./121. * drydep(iRset,iRec,iPANX ) ! Higher PAN
          if (iNTR   > 0) tmpDep = tmpDep &
               + 14./130. * drydep(iRset,iRec,iNTR  ) ! Org. Nitr
          if (iPNO3  > 0) tmpDep = tmpDep &
               + 28./62.  * drydep(iRset,iRec,iPNO3 ) ! NH4NO3
          if (iANO3K > 0) tmpDep = tmpDep &
               + 28./62.  * drydep(iRset,iRec,iANO3K) ! NH4NO3
          if (iNH3   > 0) tmpDep = tmpDep &
               + 14./17.  * drydep(iRset,iRec,iNH3  ) ! ammonia
          if (iPNH4  > 0) tmpDep = tmpDep &
               + 14./18.  * drydep(iRset,iRec,iPNH4 ) ! PM Ammonia
          if (iANH4K > 0) tmpDep = tmpDep &
               + 14./18.  * drydep(iRset,iRec,iANH4K) ! PM Ammonia

       elseif (output(iOut)%chemSpecies == "S") then ! Total Sulfur Dep

          if (iSO2   > 0) tmpDep = tmpDep &
               + 32./64.  * drydep(iRset,iRec,iSO2  )   ! SO2
          if (iSULF  > 0) tmpDep = tmpDep &
               + 32./98.  * drydep(iRset,iRec,iSULF )   ! H2SO4
          if (iPSO4  > 0) tmpDep = tmpDep &
               + 32./96.  * drydep(iRset,iRec,iPSO4 )   ! (NH4)2SO4
          if (iASO4K > 0) tmpDep = tmpDep &
               + 32./96.  * drydep(iRset,iRec,iASO4K)   ! (NH4)2SO4

       else ! use iKind

          tmpDep = tmpDep + drydep(iRset,iRec,iKind)

       end if

       call max_or_avg(sum_val(iRset,iRec,iOut), tmpDep, output(iOut)%stat, &
            iymdh, iymdh_max(iRset,iRec,iOut) )

    end do  ! do iRec = 1, nRec(iRset)

    iHours(iRset,iOut) = iHours(iRset,iOut) + 1

!--- Find the n-period average value, then find the rank of this value,
!    and insert the value at the right rank

    if (iHours(iRset,iOut) == output(iOut)%Period) then ! end averaging period

       do iRec = 1, nRec(iRset)

          if ( output(iOut)%stat == "A" ) then ! average by div sum/hours
             sum_val(iRset,iRec,iOut) = &
                  sum_val(iRset,iRec,iOut)/iHours(iRset,iOut)
          !else if ( output(iOut)%stat == "S" ) then ! sum_val IS the sum
             ! sum_val(iRset,iRec,iOut) = sum_val(iRset,iRec,iOut)
          !else if ( output(iOut)%stat == "M" ) then ! we already have the max
          end if

          ir = find_rank( sum_val(iRset,iRec,iOut), iRset, iRec, iOut, &
               output(iOut)%rank)

          ! inserts into max_val at right rank, with the right time
          call insert( sum_val(iRset,iRec,iOut), iRset, iRec, iOut,   &
               output(iOut)%rank, ir, iymdh_max(iRset,iRec,iOut) )

          if ( output(iOut)%TimeFilter  == "ALL" .and.                   &
               output(iOut)%SpaceFilter == "ALL") then  ! the single-point MAX

             write(string1,*) "All Times"
             if (debug) print*,"Output: ",iOut,iRset,iRec,iymdh, &
                  sum_val(iRset,iRec,iOut)
             call write_output(iOut, iRset, iRec, iymdh_max(iRset,iRec,iOut),  &
                  string1, sum_val(iRset,iRec,iOut))

          elseif ( sum_val(iRset,iRec,iOut) >= output(iOut)%exceeds ) then

             write(string1,*) ">=",output(iOut)%exceeds
             if (debug) print*,"Output: ",iOut,iRset,iRec,iymdh, &
                  sum_val(iRset,iRec,iOut)
             call write_output(iOut, iRset, iRec, iymdh_max(iRset,iRec,iOut),  &
                  string1, sum_val(iRset,iRec,iOut))

          end if

       end do

!
!----After the loop over iRec, we can now find the max value over iRec
!
       if ( output(iOut)%TimeFilter  == "ALL" .and.                   &
            output(iOut)%SpaceFilter == "MAX") then  ! the single-point MAX

!--- Find the maximum (over receptors) of the right rank

          if (debug) print*,"Writing SpaceFilter == MAX and TimeFilter == ALL"

          write(string1,*) output(iOut)%rank ! so H1H and H12H both work
          string1 = "H" // trim(adjustl(string1)) // "H"

          ij = -1            ! detect not found
          answer = -HUGE(0.) ! most negative number
          do iRec = 1, nRec(iRset)
             if ( sum_val(iRset,iRec,iOut) > answer) then
                answer    = sum_val(iRset,iRec,iOut)
                iymdh_ans = iymdh_max(iRset,iRec,iOut)
                ij = iRec
             end if
          end do

!--- Write out the single-point "answer"

          call write_output( iOut, iRset, ij, iymdh_ans, string1, answer )

       end if
!
!---- After the loop over iRec, we can now write the gridded (netCDF) value
!     for this output, if TimeFilter == "ALL".

       if ( output(iOut)%TimeFilter == "ALL" .and. &
            output(iOut)%OutType    == "NC" ) then

          output(iOut)%outTstep = output(iOut)%outTstep + 1

          call ncf_wrt_data_tstep(output(iOut)%outFile, output(iOut)%lun,       &
               nspcs, output(iOut)%outTstep, iymdh, output(iOut)%period )

          call ncf_wrt_data_species(output(iOut)%outFile, output(iOut)%lun,     &
               Rset(iRset)%Grid%NX, Rset(iRset)%Grid%NY, nspcs,                 &
               output(iOut)%chemSpecies,                                        &
               grd_val(iRset,1:Rset(iRset)%Grid%NX,1:Rset(iRset)%Grid%NY,iOut), &
               output(iOut)%outTstep )

       end if

       iHours(iRset,iOut) = 0
       sum_val(iRset,:,iOut) = 0.

    end if ! if (iHours(iRset,iOut) == output(iOut)%Period) then

   RETURN

  END SUBROUTINE calc_dry_dep
!
!*************************************************************************
!
  SUBROUTINE calc_wet_dep( iRset, iKind, iOut, iymdh )

    IMPLICIT NONE
    integer                 :: iRset  ! which Class I area to process
    integer                 :: iKind  ! which chemical species in dep() to use
    integer                 :: iOut   ! which output we're working on
    integer                 :: ir     ! rank of this average (H1H, H8H, etc.)
    integer                 :: iRec   ! Class I area receptor index
    integer                 :: iymdh  ! current time in YYYYMMDDHH format
    integer                 :: iymdh_ans ! time of max over space
    integer                 :: ij     ! index of lat,lon where answer is found
    real                    :: answer ! the max for all time periods
    real                    :: tmpDep ! Deposition rate for this hour

!--- add this hour to the running total deposition of N and S

    do iRec = 1, nRec(iRset)

       tmpDep = 0.

       if (output(iOut)%chemSpecies == "N") then ! Total Nitrogen Deposition

          if (iNO   > 0) tmpDep = tmpDep &
               + 14./30.  * wetdep(iRset,iRec,iNO   ) ! NO
          if (iNO2  > 0) tmpDep = tmpDep &
               + 14./46.  * wetdep(iRset,iRec,iNO2  ) ! NO2
          if (iNO3   > 0) tmpDep = tmpDep &
               + 14./62.  * wetdep(iRset,iRec,iNO3  ) ! NO3
          if (iN2O5  > 0) tmpDep = tmpDep &
               + 28./102. * wetdep(iRset,iRec,iN2O5 ) ! N2O5
          if (iHNO3  > 0) tmpDep = tmpDep &
               + 14./63.  * wetdep(iRset,iRec,iHNO3 ) ! HNO3
          if (iHONO  > 0) tmpDep = tmpDep &
               + 14./47.  * wetdep(iRset,iRec,iHONO ) ! HONO
          if (iPNA   > 0) tmpDep = tmpDep &
               + 14./79.  * wetdep(iRset,iRec,iPNA  ) ! HNO4
          if (iPAN   > 0) tmpDep = tmpDep &
               + 14./121. * wetdep(iRset,iRec,iPAN  ) ! CH3COOONO2
          if (iPANX  > 0) tmpDep = tmpDep &
               + 14./121. * wetdep(iRset,iRec,iPANX ) ! Higher PAN
          if (iNTR   > 0) tmpDep = tmpDep &
               + 14./130. * wetdep(iRset,iRec,iNTR  ) ! Org. Nitr
          if (iPNO3  > 0) tmpDep = tmpDep &
               + 28./62.  * wetdep(iRset,iRec,iPNO3 ) ! NH4NO3
          if (iANO3K > 0) tmpDep = tmpDep &
               + 28./62.  * wetdep(iRset,iRec,iANO3K) ! NH4NO3
          if (iNH3   > 0) tmpDep = tmpDep &
               + 14./17.  * wetdep(iRset,iRec,iNH3  ) ! ammonia
          if (iPNH4  > 0) tmpDep = tmpDep &
               + 14./18.  * wetdep(iRset,iRec,iPNH4 ) ! PM Ammonia
          if (iANH4K > 0) tmpDep = tmpDep &
               + 14./18.  * wetdep(iRset,iRec,iANH4K) ! PM Ammonia

       elseif (output(iOut)%chemSpecies == "S") then ! Total Sulfur Dep

          if (iSO2   > 0) tmpDep = tmpDep &
               + 32./64.  * wetdep(iRset,iRec,iSO2  )   ! SO2
          if (iSULF  > 0) tmpDep = tmpDep &
               + 32./98.  * wetdep(iRset,iRec,iSULF )   ! H2SO4
          if (iPSO4  > 0) tmpDep = tmpDep &
               + 32./96.  * wetdep(iRset,iRec,iPSO4 )   ! (NH4)2SO4
          if (iASO4K > 0) tmpDep = tmpDep &
               + 32./96.  * wetdep(iRset,iRec,iASO4K)   ! (NH4)2SO4

       else ! use iKind

          tmpDep = tmpDep + wetdep(iRset,iRec,iKind)

       end if

       call max_or_avg(sum_val(iRset,iRec,iOut), tmpDep, output(iOut)%stat, &
            iymdh, iymdh_max(iRset,iRec,iOut) )

    end do  ! do iRec = 1, nRec(iRset)

    iHours(iRset,iOut) = iHours(iRset,iOut) + 1

!--- Find the n-period average value, then find the rank of this value,
!    and insert the value at the right rank

    if (iHours(iRset,iOut) == output(iOut)%Period) then ! end averaging period

       do iRec = 1, nRec(iRset)

          if ( output(iOut)%stat == "A" ) then ! average by div sum/hours
             sum_val(iRset,iRec,iOut) = &
                  sum_val(iRset,iRec,iOut)/iHours(iRset,iOut)
          !else if ( output(iOut)%stat == "S" ) then ! sum_val IS the sum
             ! sum_val(iRset,iRec,iOut) = sum_val(iRset,iRec,iOut)
          !else if ( output(iOut)%stat == "M" ) then ! we already have the max
          end if

          ir = find_rank( sum_val(iRset,iRec,iOut), iRset, iRec, iOut, &
               output(iOut)%rank)

          ! inserts into max_val at right rank, with the right time
          call insert( sum_val(iRset,iRec,iOut), iRset, iRec, iOut,   &
               output(iOut)%rank, ir, iymdh_max(iRset,iRec,iOut) )

          if ( output(iOut)%TimeFilter  == "ALL" .and.                   &
               output(iOut)%SpaceFilter == "ALL") then ! all recs, all times

             write(string1,*) "All Times"
             if (debug) print*,"Output: ",iOut,iRset,iRec,iymdh, &
                  sum_val(iRset,iRec,iOut)
             call write_output(iOut, iRset, iRec, iymdh_max(iRset,iRec,iOut),  &
                  string1, sum_val(iRset,iRec,iOut))

          elseif ( sum_val(iRset,iRec,iOut) >= output(iOut)%exceeds ) then

             write(string1,*) ">=",output(iOut)%exceeds
             if (debug) print*,"Output: ",iOut,iRset,iRec,iymdh, &
                  sum_val(iRset,iRec,iOut)
             call write_output(iOut, iRset, iRec, iymdh_max(iRset,iRec,iOut),  &
                  string1, sum_val(iRset,iRec,iOut))

          end if

       end do
!
!----After the loop over iRec, we can now find the max value over iRec
!
       if ( output(iOut)%TimeFilter  == "ALL" .and.                   &
            output(iOut)%SpaceFilter == "MAX") then  ! the single-point MAX

!--- Find the maximum (over receptors) of the right rank

          if (debug) print*,"Writing SpaceFilter == MAX and TimeFilter == ALL"

          write(string1,*) output(iOut)%rank ! so H1H and H12H both work
          string1 = "H" // trim(adjustl(string1)) // "H"

          ij = -1            ! detect not found
          answer = -HUGE(0.) ! most negative number
          do iRec = 1, nRec(iRset)
             if ( sum_val(iRset,iRec,iOut) > answer) then
                answer    = sum_val(iRset,iRec,iOut)
                iymdh_ans = iymdh_max(iRset,iRec,iOut)
                ij = iRec
             end if
          end do

!--- Write out the single-point "answer"

          call write_output( iOut, iRset, ij, iymdh_ans, string1, answer )

       end if
!
!---- After the loop over iRec, we can now write the gridded (netCDF) value
!     for this output, if TimeFilter == "ALL".

       if ( output(iOut)%TimeFilter == "ALL" .and. &
            output(iOut)%OutType    == "NC" ) then

          output(iOut)%outTstep = output(iOut)%outTstep + 1

          call ncf_wrt_data_tstep(output(iOut)%outFile, output(iOut)%lun,       &
               nspcs, output(iOut)%outTstep, iymdh, output(iOut)%period )

          call ncf_wrt_data_species(output(iOut)%outFile, output(iOut)%lun,     &
               Rset(iRset)%Grid%NX, Rset(iRset)%Grid%NY, nspcs,                 &
               output(iOut)%chemSpecies,                                        &
               grd_val(iRset,1:Rset(iRset)%Grid%NX,1:Rset(iRset)%Grid%NY,iOut), &
               output(iOut)%outTstep )

       end if

       iHours(iRset,iOut) = 0
       sum_val(iRset,:,iOut) = 0.

    end if ! if (iHours(iRset,iOut) == output(iOut)%Period) then

   RETURN

  END SUBROUTINE calc_wet_dep
!
!*************************************************************************
!
  SUBROUTINE calc_vis( iRset, what, iOut, mo, iymdh )

    IMPLICIT NONE

    character (len=*)       :: what   ! what to write, dBext or contributions

    integer                 :: iRset  ! which Class I area to process
    integer                 :: iOut   ! which output we're working on
    integer                 :: mo     ! month of current time-step
    integer                 :: ir     ! rank of this average (H1H, H8H, etc.)
    integer                 :: iRec   ! Class I area receptor index
    integer                 :: iymdh  ! current time in YYYYMMDDHH format
    integer                 :: iymdh_ans ! time of max over space
    integer                 :: i
    integer                 :: ij     ! index of lat,lon where answer is found
    real                    :: answer ! the max for all time periods
    real                    :: TotSulfate, LgSulfate, SmSulfate
    real                    :: TotNitrate, LgNitrate, SmNitrate
    real                    :: TotOrganic, LgOrganic, SmOrganic
    real                    :: bNO3, bSO4, bOCM, bSOIL, bPEC, bNO2, bCORS
    real                    :: Bext, dBext
    real                    :: con_PEC, con_ASOIL, con_ACORS, con_NO2, con_PNO3

!--- Calculate this hour's visibility obscuration

    do iRec = 1, nRec(iRset)

!--- Calculate Total Sulfate, and Large Sulfate fraction, according to
!    Figure 5 from FLAG (2010).  Ditto for Nitate and Organic Mass.
!    If some component of these are not found in the DOS file, then set
!    the concentrations to zero, but continue with the calculation.

       TotSulfate = NatCond(iRset,1)
       if (iPSO4  > 0) TotSulfate = TotSulfate + con(iRset,iRec,iPSO4)
       if (iASO4K > 0) TotSulfate = TotSulfate + con(iRset,iRec,iASO4K)
       if (TotSulfate < 20.) then              ! 20 ug/m3
          LgSulfate = TotSulfate / 20. * TotSulfate
       else
          LgSulfate = TotSulfate
       end if
       SmSulfate = TotSulfate - LgSulfate

       TotNitrate = NatCond(iRset,2)
       if (iPNO3  > 0) TotNitrate = TotNitrate + con(iRset,iRec,iPNO3)
       if (iANO3K > 0) TotNitrate = TotNitrate + con(iRset,iRec,iANO3K)
       if (TotNitrate < 20.) then              ! 20 ug/m3
          LgNitrate = TotNitrate / 20. * TotNitrate
       else
          LgNitrate = TotNitrate
       end if
       SmNitrate = TotNitrate - LgNitrate

       TotOrganic = NatCond(iRset,3)
       if (iPSOA  > 0) TotOrganic = TotOrganic + con(iRset,iRec,iPSOA)
       if (iPOC   > 0) TotOrganic = TotOrganic + con(iRset,iRec,iPOC)
       if (TotOrganic < 20.) then              ! 20 ug/m3
          LgOrganic = TotOrganic / 20. * TotOrganic
       else
          LgOrganic = TotOrganic
       end if
       SmOrganic = TotOrganic - LgOrganic

       con_PEC   = 0.  ! If these values not found in this run, then set
       con_ASOIL = 0.  ! them to zero, but continue with the calculation.
       con_ACORS = 0.
       con_NO2   = 0.
       con_PNO3  = 0.
       if (iPEC > 0)   con_PEC   = con(iRset,iRec,iPEC)
       if (iASOIL > 0) con_ASOIL = con(iRset,iRec,iASOIL)
       if (iACORS > 0) con_ACORS = con(iRset,iRec,iACORS)
       if (iNO2   > 0) con_NO2   = con(iRset,iRec,iNO2)
       if (iPNO3  > 0) con_PNO3  = con(iRset,iRec,iPNO3)
!
!--- NO2 and PNO3 concentrations can be negative, but to be conservative
!    for visibility calculations we limit them to be not less than zero.

       if (con_NO2  < 0.) con_NO2  = 0.
       if (con_PNO3 < 0.) con_PNO3 = 0.

!--- Now calculate the total (project + natural background) extinction, and
!    the "delta Bext" percent change to natural background.  Note that
!    Sm/Lg Sulfate/Nitrate/Organic already have the Project Component added
!    above.

       Bext =                                      &
            2.2 * SmallRH(iRset,mo) * SmSulfate  + &       ! small sulfate
            4.8 * LargeRH(iRset,mo) * LgSulfate  + &       ! large sulfate
            2.4 * SmallRH(iRset,mo) * SmNitrate  + &       ! small nitrate
            5.1 * LargeRH(iRset,mo) * LgNitrate  + &       ! large nitrate
            2.8 *                     SmOrganic  + &       ! small organic mass
            6.1 *                     LgOrganic  + &       ! large organic mass
            10. * (NatCond(iRset,4) + con_PEC)   + &       ! elemental carbon
            1.0 * (NatCond(iRset,5) + con_ASOIL) + &       ! fine soil
            0.6 * (NatCond(iRset,6) + con_ACORS) + &       ! coarse mass
            1.7 * SeaSalt(iRset,mo) * NatCond(iRset,7) + & ! sea salt
            NatCond(iRset,8)                     + &       ! Rayleigh
            0.1755 * con_NO2                               ! NO2

       dBext = (Bext - Bext_nat(iRset,mo)) / Bext_nat(iRset,mo) * 100. ! in %

!--- Calculate source-only component contributions
!    Adapted from a code snippet contributed by Bret Anderson.

       TotSulfate = 0.
       if (iPSO4  > 0) TotSulfate = TotSulfate + con(iRset,iRec,iPSO4)
       if (iASO4K > 0) TotSulfate = TotSulfate + con(iRset,iRec,iASO4K)
       if (TotSulfate < 20.) then              ! 20 ug/m3
          LgSulfate = TotSulfate / 20. * TotSulfate
       else
          LgSulfate = TotSulfate
       end if
       SmSulfate = TotSulfate - LgSulfate

       TotNitrate = 0.

       TotNitrate = TotNitrate + con_PNO3
       if (iANO3K > 0) TotNitrate = TotNitrate + con(iRset,iRec,iANO3K)
       if (TotNitrate < 20.) then              ! 20 ug/m3
          LgNitrate = TotNitrate / 20. * TotNitrate
       else
          LgNitrate = TotNitrate
       end if
       SmNitrate = TotNitrate - LgNitrate

       TotOrganic = 0.
       if (iPSOA  > 0) TotOrganic = TotOrganic + con(iRset,iRec,iPSOA)
       if (iPOC   > 0) TotOrganic = TotOrganic + con(iRset,iRec,iPOC)
       if (TotOrganic < 20.) then              ! 20 ug/m3
          LgOrganic = TotOrganic / 20. * TotOrganic
       else
          LgOrganic = TotOrganic
       end if
       SmOrganic = TotOrganic - LgOrganic

       bSO4 = (SmallRH(iRset,mo) * SmSulfate * 2.2) + &
            (LargeRH(iRset,mo) * LgSulfate * 4.8)
       bNO3 = (SmallRH(iRset,mo) * SmNitrate * 2.4) + &
            (LargeRH(iRset,mo) * LgNitrate * 5.1)
       ! FIXME bNO3 is sometimes
       bOCM = 2.8 * SmOrganic + 6.1 * LgOrganic
       bSOIL = con_ASOIL * 1.0
       bPEC = 10.0 * con_PEC
       bNO2 = 0.1755 * con_NO2
       bCORS = 0.6 * con_ACORS

!--- Finally, save the change (delta) in extinction for this hour, as a
!    running sum.

       call max_or_avg(sum_val(iRset,iRec,iOut), dBext, output(iOut)%stat, &
            iymdh, iymdh_max(iRset,iRec,iOut) )

       call max_or_avg(dBc_sum(iRset,iRec,iOut,1), Bext-Bext_nat(iRset,mo), output(iOut)%stat, &
            iymdh, iymdh_max(iRset,iRec,iOut) )
       call max_or_avg(dBc_sum(iRset,iRec,iOut,2), Bext_nat(iRset,mo), output(iOut)%stat, &
            iymdh, iymdh_max(iRset,iRec,iOut) )
       call max_or_avg(dBc_sum(iRset,iRec,iOut,3), bNO3, output(iOut)%stat, &
            iymdh, iymdh_max(iRset,iRec,iOut) )
       call max_or_avg(dBc_sum(iRset,iRec,iOut,4), bSO4, output(iOut)%stat, &
            iymdh, iymdh_max(iRset,iRec,iOut) )
       call max_or_avg(dBc_sum(iRset,iRec,iOut,5), bOCM, output(iOut)%stat, &
            iymdh, iymdh_max(iRset,iRec,iOut) )
       call max_or_avg(dBc_sum(iRset,iRec,iOut,6), bSOIL,output(iOut)%stat, &
            iymdh, iymdh_max(iRset,iRec,iOut) )
       call max_or_avg(dBc_sum(iRset,iRec,iOut,7), bPEC, output(iOut)%stat, &
            iymdh, iymdh_max(iRset,iRec,iOut) )
       call max_or_avg(dBc_sum(iRset,iRec,iOut,8), bNO2, output(iOut)%stat, &
            iymdh, iymdh_max(iRset,iRec,iOut) )
       call max_or_avg(dBc_sum(iRset,iRec,iOut,9), bCORS,output(iOut)%stat, &
            iymdh, iymdh_max(iRset,iRec,iOut) )

    end do   ! do iRec = 1,nRec

    iHours(iRset,iOut) = iHours(iRset,iOut) + 1

!--- Find the n-period average value, then find the rank of this value,
!    and insert the value at the right rank

    if (iHours(iRset,iOut) == output(iOut)%Period) then ! end averaging period

       do iRec = 1, nRec(iRset) ! sum_val is dBext here

          if ( output(iOut)%stat == "A" ) then ! average by div sum/hours
             sum_val(iRset,iRec,iOut) = &
                  sum_val(iRset,iRec,iOut)/iHours(iRset,iOut)
             do i = 1,nCont
                dBc_sum(iRset,iRec,iOut,i) = &
                     dBc_sum(iRset,iRec,iOut,i)/iHours(iRset,iOut)
             end do
          !else if ( output(iOut)%stat == "S" ) then ! sum_val IS the sum
             ! sum_val(iRset,iRec,iOut) = sum_val(iRset,iRec,iOut)
          !else if ( output(iOut)%stat == "M" ) then ! we already have the max
          end if

          ir = find_rank( sum_val(iRset,iRec,iOut), iRset, iRec, iOut, &
               output(iOut)%rank)
          call insert( sum_val(iRset,iRec,iOut), iRset, iRec, iOut,   &
               output(iOut)%rank, ir, iymdh_max(iRset,iRec,iOut) )

          do i = 1, nCont ! dBc is the dBext contributions
             call insert_dBc( dBc_sum(iRset,iRec,iOut,i), iRset, iRec, iOut, i, &
                  output(iOut)%rank, ir, iymdh_max(iRset,iRec,iOut) )
          end do

!---- Write out dBext if it exceeds the requested criteria.

          if ( output(iOut)%TimeFilter  == "ALL" .and.                   &
               output(iOut)%SpaceFilter == "ALL") then ! all recs, all times

             write(string1,*) "All Times"
             if (debug) print*,"Output: ",iOut,iRset,iRec,iymdh, &
                  sum_val(iRset,iRec,iOut)
             call write_output(iOut, iRset, iRec, iymdh_max(iRset,iRec,iOut),  &
                  string1, sum_val(iRset,iRec,iOut))

          elseif ( sum_val(iRset,iRec,iOut) >= output(iOut)%exceeds ) then

             write(string1,*) ">=",output(iOut)%exceeds
             if (output(iOut)%chemSpecies == "CONT") then
                tmp7(:) = dBc_sum(iRset,iRec,iOut,:)
                call write_vis( iOut, iRset, iRec, iymdh_max(iRset,iRec,iOut), &
                     string1, sum_val(iRset,iRec,iOut), tmp7, nCont )
             else
                call write_output( iOut, iRset, iRec, iymdh_max(iRset,iRec,iOut),&
                      string1, sum_val(iRset,iRec,iOut) )
             end if

          end if

       end do ! do iRec = 1, nRec(iRset)
!
!----After the loop over iRec, we can now find the max value over iRec
!
       if ( output(iOut)%TimeFilter  == "ALL" .and.                   &
            output(iOut)%SpaceFilter == "MAX") then  ! the single-point MAX

!--- Find the maximum (over receptors) of the right rank

          if (debug) print*,"Writing SpaceFilter == MAX and TimeFilter == ALL"

          write(string1,*) output(iOut)%rank ! so H1H and H12H both work
          string1 = "H" // trim(adjustl(string1)) // "H"

          ij = -1            ! detect not found
          answer = -HUGE(0.) ! most negative number
          do iRec = 1, nRec(iRset)
             if ( sum_val(iRset,iRec,iOut) > answer) then
                answer    = sum_val(iRset,iRec,iOut)
                iymdh_ans = iymdh_max(iRset,iRec,iOut)
                ij = iRec
             end if
          end do

!--- Write out the single-point "answer"

          if (output(iOut)%chemSpecies == "CONT") then
             tmp7(:) = dBc_sum(iRset,ij,iOut,:)
             call write_vis( iOut, iRset, ij, iymdh_ans, string1, &
                  answer, tmp7, nCont )
          else
             call write_output( iOut, iRset, ij, iymdh_ans, string1, answer )
          end if
       end if

!--- No netCDF output here, because VIS only possible at Class I areas,
!    which are lists of receptors, not gridded receptors

       iHours(iRset,iOut) = 0
       sum_val(iRset,:,iOut) = 0.
       dBc_sum(iRset,:,iOut,:) = 0.

    end if ! if (iHours(iRset,iOut) == output(iOut)%Period) then

    RETURN

  END SUBROUTINE calc_vis
!
!*************************************************************************
!
  SUBROUTINE read_rec_set(nRsets, MaxRec, MaxNX, MaxNY)

    IMPLICIT NONE

    integer                 :: nRsets       ! the number of Class I areas
    integer                 :: iRset        ! which Class I area to process
    integer                 :: MaxRec       ! max( nRec(:) )
    integer                 :: MaxNX, MaxNY ! max of Rset%Grid$NX, Rset%Grid$NX
    integer                 :: ios, i,j, ij, astat
    integer, parameter      :: lun = 100
    real                    :: xlon, xlat
    character (len=1)       :: slash, backslash
    character (len=99)      :: line
    logical                 :: lexist, lmeters
    logical, dimension(:), allocatable :: header

    slash     = char(47) ! AKA "forward slash"
    backslash = char(92)

!
!--- Allocate local arrays to nrec
!
    allocate( header(nRsets), STAT=astat )
    if ( astat > 0 ) then
       nError = UK_ERROR
       WRITE(eMessage,*) 'Error allocating header with size ',nRsets*4
       return
    end if
!
!--- Count the number of receptors, and allocate the global vars lat,lon
!
    MaxNY = 0 ; MaxNY = 0 ! initialize
    do iRset = 1,nRsets
       if (Rset(iRset)%IsGrid) then
          nRec(iRset) = Rset(iRset)%Grid%NX * Rset(iRset)%Grid%NY
          if (Rset(iRset)%Grid%NX > MaxNX) MaxNX = Rset(iRset)%Grid%NX
          if (Rset(iRset)%Grid%NY > MaxNY) MaxNX = Rset(iRset)%Grid%NY
       else
          inquire(file=TRIM(Rset(iRset)%Filename),exist=lexist)
          if (.not. lexist) goto 99
          open(lun+iRset,file=Rset(iRset)%Filename, status='old', &
               err=99,iostat=ios)
          read(lun+iRset,'(a)') line
          read(line,*,iostat=ios) xlon,xlat
          if (ios == 0) then
             header(iRset) = .false.
             rewind(lun+iRset)
          else
             header(iRset) = .true.
          endif
          if (debug) print*,"RecSet ",iRset," header = ",header(iRset)

          nRec(iRset) = 0
          read(lun+iRset,*,iostat=ios) xlon, xlat ! make sure they're readable
          do while (ios == 0)
             nRec(iRset) = nRec(iRset) + 1
             read(lun+iRset,*,iostat=ios) xlon, xlat
          end do
       end if ! if (Rset(iRset)%IsGrid) then

    end do ! do iRset = 1,nRsets
    write(*,*)

    MaxRec = maxval(nRec(:))
!
!--- Allocate the arrays to hold the lon-lat; x,y; and topo for ALL receptor sets
!
    allocate( lat(nRsets,MaxRec), lon(nRsets,MaxRec), STAT=astat )
    if ( astat > 0 ) then
       nError = UK_ERROR
       WRITE(eMessage,*) 'Error allocating coordinates (lat-lon)'
       return
    end if
    lat = -999. ! fill with a "missing value" flag, since these will be sparse
    lon = -999.

    allocate( xRec(nRsets,MaxRec), yRec(nRsets,MaxRec), STAT=astat )
    if ( astat > 0 ) then
       nError = UK_ERROR
       WRITE(eMessage,*) 'Error allocating coordinates (xRec & yRec)'
       return
    end if
    xRec = -999. ! fill with a "missing value" flag, since these will be sparse
    yRec = -999.

    allocate( RecIJ(nRsets,MaxRec,2), STAT=astat )
    if ( astat > 0 ) then
       nError = UK_ERROR
       WRITE(eMessage,*) 'Error allocating coordinates index (RecIJ)'
       return
    end if
    RecIJ = -999  ! fill with a "missing value" flag, since these will be sparse

    allocate( topo(nRsets,MaxRec), STAT=astat )
    if ( astat > 0 ) then
       nError = UK_ERROR
       WRITE(eMessage,*) 'Error allocating topography (terrain elevation)'
       return
    end if
    topo = -999. ! fill with a "missing value" flag, since these will be sparse
!
!--- Now read or construct the receptors, but don't read the elev (not used).
!    This Rset may be in lon-lat or UTM, but we'll store it in lon() and lat()
!    for now, and will convert later.
!
    do iRset = 1,nRsets

       if (Rset(iRset)%IsGrid) then

          write(*,*) 'Creating receptor locations for ',trim(Rset(iRset)%name), &
               Rset(iRset)%Grid%NX*Rset(iRset)%Grid%NY," total locations."

          ij = 0 ! like iRec
          do j = 1, Rset(iRset)%Grid%NY
             do i = 1, Rset(iRset)%Grid%NX
                ij = ij + 1
                RecIJ(iRset,ij,1) = i ! save these for reverse lookups:
                RecIJ(iRset,ij,2) = j ! turning ij into i,j to fill grd_val
                if (Rset(iRset)%CoordType == "LATLON") then
                   lon(iRset,ij) = Rset(iRset)%Grid%xW + (i-1)*Rset(iRset)%Grid%DX
                   lat(iRset,ij) = Rset(iRset)%Grid%yS + (j-1)*Rset(iRset)%Grid%DX
                else ! not LATLON, so UTM or other CARTESIAN
                   xRec(iRset,ij) = Rset(iRset)%Grid%xW+(i-1)*Rset(iRset)%Grid%DX
                   yRec(iRset,ij) = Rset(iRset)%Grid%yS+(j-1)*Rset(iRset)%Grid%DX
                end if
             end do
          end do

       else ! not a gridded receptor set, must be discrete, read from file

          write(*,*)'Reading  receptor locations for ',trim(Rset(iRset)%name), &
               ' from ',TRIM(Rset(iRset)%Filename)

          rewind(lun+iRset)
          lmeters = .false.
          if (header(iRset)) then
             read(lun+iRset,'(a)') line !header line
             if( index(line,'UTM-M') > 0 )lmeters = .true.
          end if
          do i = 1,nRec(iRset)
             if (Rset(iRset)%CoordType == "LATLON") then
                read(lun+iRset,*,iostat=ios) lon(iRset,i), lat(iRset,i)
             else
                read(lun+iRset,*,iostat=ios) xRec(iRset,i), yRec(iRset,i)
                if (lmeters) then
                  xRec(iRset,i) = xRec(iRset,i) * 1.e-3
                  yRec(iRset,i) = yRec(iRset,i) * 1.e-3
                end if
             end if
             if (ios /= 0) goto 999
          end do
          close(lun+iRset)

       end if ! if (Rset(iRset)%IsGrid) then

    end do ! do iRset = 1,nRsets

    if (allocated( header )) deallocate( header )

    RETURN ! no errors

99  CONTINUE
    nError = UK_ERROR
    WRITE(eMessage,*) 'Error opening file ',trim(Rset(iRset)%Filename)
    RETURN

999 CONTINUE
    nError = UK_ERROR
    WRITE(eMessage,*) 'Error ',ios,'reading file ',trim(Rset(iRset)%Filename)
    RETURN

  END SUBROUTINE read_rec_set
!
!*************************************************************************
!
  SUBROUTINE init_visibility(nRsets)

    IMPLICIT NONE

    integer                 :: nRsets ! number of Class I areas to consider
    integer                 :: iRset  ! loop over Class I Rset
    integer                 :: mo     ! month
    integer                 :: ios, astat
    integer, parameter      :: lun = 100

    real                    :: TotSulfate, LgSulfate, SmSulfate
    real                    :: TotNitrate, LgNitrate, SmNitrate
    real                    :: TotOrganic, LgOrganic, SmOrganic

    character (len=256)     :: line   ! one line from a FLAG(2010) CSV table
    character (len=99)      :: word   ! one word from a FLAG(2010) CSV table
    character (len=4)       :: Class1name ! the 4-letter name of the Class I area

    logical                 :: lexist

!--- Allocate the FLAG(2010) table containers. We'll allocate for ALL receptor
!    sets, even though not all may be Class I receptor sets.  Simpler, but uses
!    more memory.

    allocate( NatCond(nRsets,8), STAT=astat )
    IF( astat > 0 )THEN
       nError = UK_ERROR
       WRITE(eMessage,*) 'Error allocating NatCond'
       GO TO 9999
    END IF
    NatCond = 0.
    allocate( LargeRH(nRsets,12), STAT=astat )
    IF( astat > 0 )THEN
       nError = UK_ERROR
       WRITE(eMessage,*) 'Error allocating LargeRH'
       GO TO 9999
    END IF
    LargeRH = 0.
    allocate( SmallRH(nRsets,12), STAT=astat )
    IF( astat > 0 )THEN
       nError = UK_ERROR
       WRITE(eMessage,*) 'Error allocating SmallRH'
       GO TO 9999
    END IF
    SmallRH = 0.
    allocate( SeaSalt(nRsets,12), STAT=astat )
    IF( astat > 0 )THEN
       nError = UK_ERROR
       WRITE(eMessage,*) 'Error allocating SeaSalt'
       GO TO 9999
    END IF
    SeaSalt = 0.

    allocate( Bext_nat(nRsets,12), STAT=astat )
    IF( astat > 0 )THEN
       nError = UK_ERROR
       WRITE(eMessage,*) 'Error allocating Bext_nat'
       GO TO 9999
    END IF
    Bext_nat = 0.

!--- Read the FLAG look-up tables and extract the values for each Class I area

    NatCondFile = "flag.2010.table.6.annual.csv"
    CALL ADDPATH(NatCondFile,trim(flagDir))
    INQUIRE( file=NatCondFile,EXIST=lexist )
    if (.not. lexist) then
       nError = UK_ERROR
       WRITE(eMessage,*) 'Error opening ',trim(NatCondFile)
       GO TO 9999
    end if
    write(*,*) "Reading ",trim(NatCondFile)          ! table 5 (best 20%)
    open(lun, file=NatCondFile, status='old',err=91) ! or table 6 (annual)
    read(lun,*,iostat=ios) ! header line
    do while (ios == 0)
       read(lun,'(A256)',iostat=ios) line
       call get_csv(line,1,word)
       Class1name = word(1:4)
       !call CUPPER ( Class1name )
       do iRset = 1, nRsets
          if (Rset(iRset)%class == 1 .and. &
               Class1name == Rset(iRset)%Name(1:4)) then
             call get_csv(line, 3,word)
             read(word,*) NatCond(iRset,1) ! (NH4)2SO4
             call get_csv(line, 4,word)
             read(word,*) NatCond(iRset,2) ! NH4NO3
             call get_csv(line, 5,word)
             read(word,*) NatCond(iRset,3) ! Organic matter
             call get_csv(line, 6,word)
             read(word,*) NatCond(iRset,4) ! Elem carbon
             call get_csv(line, 7,word)
             read(word,*) NatCond(iRset,5) ! Soil
             call get_csv(line, 8,word)
             read(word,*) NatCond(iRset,6) ! Coarse Mass
             call get_csv(line, 9,word)
             read(word,*) NatCond(iRset,7) ! Sea Salt
             call get_csv(line,10,word)
             read(word,*) NatCond(iRset,8) ! Rayleigh
          end if
       end do
    end do
    close(lun)

    do iRset = 1, nRsets
       if ( Rset(iRset)%class == 1 .and. NatCond(iRset,1) == 0.) then
         nError = UK_ERROR
         WRITE(eMessage,*)"Error in init_visibility(): can't find Class I area '",  &
               trim(Rset(iRset)%Name(1:4)),"' in NatCond file ",trim(NatCondFile)
         GO TO 9999
       endif
    end do

    LargeRHFile = "flag.2010.table.7.FlargeRH.csv"
    CALL ADDPATH(LargeRHFile,trim(flagDir))
    INQUIRE( file=LargeRHFile,EXIST=lexist )
    if (.not. lexist) then
       nError = UK_ERROR
       WRITE(eMessage,*) 'Error opening ',trim(LargeRHFile)
       GO TO 9999
    end if
    write(*,*) "Reading ",trim(LargeRHFile)
    open(lun, file=LargeRHFile, status='old',err=92) ! table 7 (large RH factors)
    read(lun,*,iostat=ios) ! header line
    do while (ios == 0)
       read(lun,'(A256)',iostat=ios) line
       call get_csv(line,1,word)
       Class1name = word(1:4)
       do iRset = 1, nRsets
          if (Rset(iRset)%class == 1 .and. Class1name == Rset(iRset)%Name(1:4)) &
               then
             do mo = 1, 12 ! read 12 months of LargeRH values
                call get_csv(line, 2+mo,word)
                read(word,*) LargeRH(iRset,mo)
             end do
          end if
       end do
    end do
    close(lun)

    SmallRHFIle = "flag.2010.table.8.FsmallRH.csv"
    CALL ADDPATH(SmallRHFIle,trim(flagDir))
    INQUIRE( file=SmallRHFile,EXIST=lexist )
    if (.not. lexist) then
       nError = UK_ERROR
       WRITE(eMessage,*) 'Error opening ',trim(SmallRHFile)
       GO TO 9999
    end if
    write(*,*) "Reading ",trim(SmallRHFile)
    open(lun, file=SmallRHFile, status='old',err=93) ! table 8 (small RH factors)
    read(lun,*,iostat=ios) ! header line
    do while (ios == 0)
       read(lun,'(A256)',iostat=ios) line
       call get_csv(line,1,word)
       Class1name = word(1:4)
       do iRset = 1, nRsets
          if (Rset(iRset)%class == 1 .and. Class1name == Rset(iRset)%Name(1:4)) &
               then
             do mo = 1, 12 ! read 12 months of SmallRH values
                call get_csv(line, 2+mo,word)
                read(word,*) SmallRH(iRset,mo)
             end do
          end if
       end do
    end do
    close(lun)

    SeaSaltFile = "flag.2010.table.9.FseasaltRH.csv"
    CALL ADDPATH(SeaSaltFile,trim(flagDir))
    INQUIRE( file=SeaSaltFile,EXIST=lexist )
    if (.not. lexist) then
       nError = UK_ERROR
       WRITE(eMessage,*) 'Error opening ',trim(SeaSaltFile)
       GO TO 9999
    end if
    write(*,*) "Reading ",trim(SeaSaltFile)
    open(lun, file=SeaSaltFile, status='old',err=94) ! table 9 (sea salt factors)
    read(lun,*,iostat=ios) ! header line
    do while (ios == 0)
       read(lun,'(A256)',iostat=ios) line
       call get_csv(line,1,word)
       Class1name = word(1:4)
       do iRset = 1, nRsets
          if (Rset(iRset)%class == 1 .and. Class1name == Rset(iRset)%Name(1:4)) &
               then
             do mo = 1, 12 ! read 12 months of SeaSalt values
                call get_csv(line, 2+mo,word)
                read(word,*) SeaSalt(iRset,mo)
             end do
          end if
       end do
    end do
    close(lun)
!
!--- Calculate the monthly natural background extinction, bext_natcond
!
    do iRset = 1, nRsets  ! loop over Class I Rsets
       if ( Rset(iRset)%class == 1 ) then
          do mo = 1, 12      ! 12 months
!
!--- Calculate Total Sulfate, and large sulfate fraction, according to
!    Figure 5 from FLAG (2010).  Ditto for Nitate and Organic Mass
!
             TotSulfate = NatCond(iRset,1)
             if (TotSulfate < 20.) then ! 20 ug/m3
                LgSulfate = TotSulfate/20.*TotSulfate
             else
                LgSulfate = TotSulfate
             end if
             SmSulfate = TotSulfate - LgSulfate

             TotNitrate = NatCond(iRset,2)
             if (TotNitrate < 20.) then ! 20 ug/m3
                LgNitrate = TotNitrate/20.*TotNitrate
             else
                LgNitrate = TotNitrate
             end if
             SmNitrate = TotNitrate - LgNitrate

             TotOrganic = NatCond(iRset,3)
             if (TotOrganic < 20.) then ! 20 ug/m3
                LgOrganic = TotOrganic/20.*TotOrganic
             else
                LgOrganic = TotOrganic
             end if
             SmOrganic = TotOrganic - LgOrganic

!--- Calculate natural background visibility obscuration, by Rset and Month

             Bext_nat(iRset,mo) =                           &
               2.2 * SmallRH(iRset,mo) * SmSulfate        + & ! small sulfate
               4.8 * LargeRH(iRset,mo) * LgSulfate        + & ! large sulfate
               2.4 * SmallRH(iRset,mo) * SmNitrate        + & ! small nitrate
               5.1 * LargeRH(iRset,mo) * LgNitrate        + & ! large nitrate
               2.8 *                     SmOrganic        + & ! sm org mass
               6.1 *                     LgOrganic        + & ! lg org mass
               10. *                     NatCond(iRset,4) + & ! elem. carbon
               1.0 *                     NatCond(iRset,5) + & ! fine soil
               0.6 *                     NatCond(iRset,6) + & ! coarse mass
               1.7 * SeaSalt(iRset,mo) * NatCond(iRset,7) + & ! sea salt
               NatCond(iRset,8)                           + & ! Rayleigh scat
               0.1755 * 0.                                    ! NO2

          end do  ! do mo = 1, 12 ! 12 months
       end if     ! if ( Rset(iRset)%class == 1 ) then
    end do        ! do iRset = 1, nRsets  ! loop over Class I Rsets

    write(*,*)    ! blank line to make it look nice

9999 RETURN

91 WRITE(eMessage,*)"SciDosPost: error opening file ",trim(NatCondFile)
   GO TO 9998

92 WRITE(eMessage,*)"SciDosPost: error opening file ",trim(LargeRHFile)
   GO TO 9998
93 WRITE(eMessage,*)"SciDosPost: error opening file ",trim(SmallRHFile)
   GO TO 9998

94 WRITE(eMessage,*)"SciDosPost: error opening file ",trim(SeaSaltFile)
   GO TO 9998

9998 nError = UK_ERROR

     RETURN

  END SUBROUTINE init_visibility
!
!*************************************************************************
!
  subroutine write_vis(iOut, iRset, iRec, iymdh, Cond, val, cont, nCont)

    real                    :: val     ! the dBext to write
    integer                 :: nCont   ! the number of Contributions
    real, dimension(nCont)  :: cont    ! the Contributions to write
    integer                 :: iOut    ! which output we're working on
    integer                 :: iRset   ! which receptor
    integer                 :: iRec    ! receptor index within this receptor set
    integer                 :: iymdh   ! current time in YYYYMMDDHH format
    character (len=*)       :: Cond    ! a description of what we're writing
    integer                 :: i

1   format(2(a,", "),4(f12.6,", "),i11.10,", ",i6,", ",a,99(", ",g16.6))


    if (iRec > 0) then
       write(output(iOut)%lun,1) trim(Rset(iRset)%Name),     &
            trim(output(iOut)%chemSpecies),                  &
            xRec(iRset,iRec), yRec(iRset,iRec),              &
            lat(iRset,iRec),  lon(iRset,iRec),               &
            iymdh, output(iOut)%Period,                      &
            trim(Cond), val, (cont(i), i=1,nCont)
    else
       write(output(iOut)%lun,1) trim(Rset(iRset)%Name),     &
            trim(output(iOut)%chemSpecies),                  &
            -999., -999.,                                    &
            -999., -999.,                                    &
            -999, output(iOut)%Period,                       &
            trim(Cond), -999., (-999., i=1,nCont)

    end if

  end subroutine write_vis

!
!*************************************************************************
!
  subroutine write_output(iOut, iRset, iRec, iymdh, Cond, val)

    integer            :: iOut        ! which output we're working on
    integer            :: iRset       ! which receptor set we're processing
    integer            :: iRec        ! receptor index within this receptor set
    integer            :: iymdh       ! current time in YYYYMMDDHH format
    integer            :: iy,im,id,ih ! current time
    character (len=*)  :: Cond        ! a description of what we're writing
    real               :: val         ! the value (likely concentration) to write

    integer            :: itmp        ! just a temporary integer
    integer            :: period      ! temporary, either %period or %maxDailyHrs
    character (len=8)  :: GroupID = "ALL     "
    character (len=8)  :: AVEstr      ! N-HR
    real               :: Z = 0.      ! ZFLAG height
    real               :: X, Y        ! used to convert UTM from KM to meters
    real               :: units_conv  ! used to convert units
    real               :: tmpval      ! used to convert units
!
!---- Convert the output val to the requested output units
!
    if (output(iOut)%ConDepVis == "CON") then     ! concentration

       if (output(iOut)%units == "ppm") then      ! have ug/m3, want PPM
          tmpval = ugm3_to_ppm(val, output(iOut)%MW)
          if (tmpval == -999.) then               ! MW is missing, so say what
             output(iOut)%units = "ug/m3"         ! we're going to write.
          else
             val = tmpval
          end if
       else if (output(iOut)%units == "ppb") then ! have ug/m3, want PPB
          tmpval = ugm3_to_ppb(val, output(iOut)%MW)
          if (tmpval == -999.) then               ! MW is missing, so say what
             output(iOut)%units = "ug/m3"         ! we're going to write.
          else
             val = tmpval
          end if
       else                                       ! have ug/m3, want ???
          call find_units_conv("ug/m3",output(iOut)%units,3600, units_conv)
          val = val * units_conv
       endif

    else if (                                   &
       output(iOut)%ConDepVis == "DEP" .or.     &
       output(iOut)%ConDepVis == "DRY" .or.     &
       output(iOut)%ConDepVis == "WET" ) then   ! deposition

       call find_units_conv("kg/ha",output(iOut)%units,3600, units_conv)
       val = val * units_conv

    else if (output(iOut)%ConDepVis == "VIS") then   ! visibility

       ! do nothing, always in percent

    endif
!
!---- Set the time period to be written correctly
!
    if (output(iOut)%stat == "A") then      ! "normal" windowed average
       period = output(iOut)%period
    else if (output(iOut)%stat == "S") then ! sum over the period
       period = output(iOut)%period
    else if (output(iOut)%stat == "M") then ! Max Daily rolling average
       period = output(iOut)%maxDailyHrs
    end if
!
!---- Now write a line, depending on OutType (file type)
!
    if (output(iOut)%OutType == "CSV") then

1      format(2(a,", "),4(f12.6,", "),i11.10,", ",i6,", ",a,", ",g16.6,", ",a5)

       if (iRec > 0) then
          write(output(iOut)%lun,1) trim(Rset(iRset)%Name),     &
               trim(output(iOut)%chemSpecies),                  &
               xRec(iRset,iRec), yRec(iRset,iRec),              &
               lat(iRset,iRec),  lon(iRset,iRec),               &
               iymdh, period,                                   &
               trim(Cond), val, output(iOut)%units
       else ! iRec == 0 means "write missing values"
          write(output(iOut)%lun,1) trim(Rset(iRset)%Name),     &
               trim(output(iOut)%chemSpecies),                  &
               -999., -999.,                                    &
               -999., -999.,                                    &
               -999,  period,                                   &
               trim(Cond), -999., output(iOut)%units
       end if

    else if (output(iOut)%OutType == "XYZ") then

2      format(4(f12.6," "), g16.6, 1x,a6, 1x,i11)

       if (iRec > 0) then
          write(output(iOut)%lun,2)                             &
               xRec(iRset,iRec), yRec(iRset,iRec),              &
               lat(iRset,iRec),  Lon(iRset,iRec),               &
               val, output(iOut)%units, iymdh
       else ! iRec == 0 means "write missing values"
          write(output(iOut)%lun,2) &
               -999., -999.,                                    &
               -999., -999.,                                    &
               -999., output(iOut)%units, -999
       end if

    else if ( output(iOut)%OutType == "POST" &
         .or. output(iOut)%OutType == "PLOT" ) then
!                   1993020100
       if (iymdh >= 2000000000) then
          itmp = iymdh - 2000000000
       else if (iymdh > 0) then
          itmp = iymdh - 1900000000 ! cheap way to get YYMMDDHH
       else
          itmp = 0 ! goes with val == -HUGE(0.)
       endif

       call idate2ymdh(itmp,iy,im,id,ih)    ! POSTFILEs are always hr 1 to 24
       call legal_timestamp(iy,im,id,ih,24)
       itmp = iymdh2idate(iy,im,id,ih)

       if (ProjectCoordinate%mode == HD_UTM) then
          x = xRec(iRset,iRec) * 1000. ! km to m
          y = yRec(iRset,iRec) * 1000. ! km to m
       else
          x = xRec(iRset,iRec)
          y = yRec(iRset,iRec)
       end if

       write(AVEstr,'(i3,"-HR")') Period

       if (iRec <= 0 .or. val == -HUGE(0.) ) then
!                                    X                Y
          write(output(iOut)%lun,3)    -999.,           -999., &
!              AVERAGE CONC
               -999.,                                    &
!              ZELEV             ZHILL            ZFLAG AVE     GRP
               topo(iRset,iRec), topo(iRset,iRec),Z,    AVEstr, GroupID,    &
!              DATE                                NET ID
               itmp

       else if (val < 1.e-5 .and. val /= 0. .and. val > -1.e-5) then

!          print*,"Writing to PLOT/POST file ",iOut,iRset,iRec
!                                    X  Y  AVERAGE CONC
          write(output(iOut)%lun,4)  x, y, val,        &
!              ZELEV             ZHILL            ZFLAG AVE     GRP
               topo(iRset,iRec), topo(iRset,iRec),Z,    AVEstr, GroupID,    &
!              DATE                                NET ID
               itmp
       else

!                                    X  Y  AVERAGE CONC
          write(output(iOut)%lun,3)  x, y, val,        &
!              ZELEV             ZHILL            ZFLAG AVE     GRP
               topo(iRset,iRec), topo(iRset,iRec),Z,    AVEstr, GroupID,    &
!              DATE                                NET ID
               itmp

!
!---- AERMOD POSTFILE output format, from AERMOD v12345
!
3         format(2(1X,F13.5),1X,F13.5,3(1X,F8.2),2X,A6,2X,A8,2X,I8.8,2X,A8)
4         format(2(1X,F13.5),1X,G13.5,3(1X,F8.2),2X,A6,2X,A8,2X,I8.8,2X,A8)

       end if

    else if ( output(iOut)%OutType == "GRD") then

       write(output(iOut)%lun,'(1p,e18.6)') val

    else if ( output(iOut)%OutType == "NC") then
!
!---- Add this receptor to the gridded output. Only grids are allowed to be
!     written to netCDF files, but at this point we only have val for one rec.
!     Save in a 2-d container.

       grd_val(iRset, RecIJ(iRset,iRec,1), RecIJ(iRset,iRec,2), iOut ) = val

    endif

    return

  end subroutine write_output
!
!*************************************************************************
!
  subroutine open_files_write_headers(datestr,timestr,eMessage,eInform,   &
       nOuts,nRsets,ierr)

    implicit none

!--- Arguments:

    character*(*)       :: datestr, timestr, eMessage, eInform
    integer             :: nOuts,nRsets,ierr

!--- Local variables:

    character (len=8)   :: AVEstr   ! AERMOD averaging time eg "    1-HR"
    CHARACTER (len=256) :: action
    integer             :: lun, iOut, iRset, i, ij, ios
    logical             :: lexist

    ierr = 0

!--- First delete all the output files, so we can (possibly) append to them

    do iOut = 1, nOuts
       open (100, file=output(iOut)%outFile)
       close(100, status='delete')
    end do

!--- Open the output files, and write headers

    lun = 500 ! start number output logical unit numbers at 500 to avoid conflicts
    do iOut = 1, nOuts
       ij = 0 ! find which receptor set is goes with this output
       do iRset = 1, nRsets
          if (output(iOut)%RecSet == Rset(iRset)%Name) ij = iRset
       end do
       iRset = ij

       inquire(file=output(iOut)%outFile, exist=lexist)
       if (.not. lexist) then ! not found, must be new, write a header

          lun = lun + 1
          output(iOut)%lun = lun

          write(*,'(1x,2(a,i4),2a)') "Output ",iOut," created new file ", &
               lun-500," ",trim(output(iOut)%outFile)

          if (output(iOut)%outType /= "NC") then ! open part of "header" for NC
             open(lun, file=output(iOut)%outFile, status="new",iostat=ios)
             if (ios /= 0) then
                nError   = UK_ERROR
                eRoutine = 'SciDosPost'
                write(eMessage,*) 'Error ',ios,'opening output file.'
                WRITE(eInform,*)  'File = ',TRIM(output(iOut)%outFile)
                ierr = 1
                return
             end if
          end if

          if (output(iOut)%outType == "CSV") then

             if (output(iOut)%ConDepVis == "VIS" .and. &
                  output(iOut)%ChemSpecies == "CONT") then
                write(lun,'(3a)') &
                     "RecSet, Species, X, Y, Lat, Lon, Time, Period(hr), ",  &
                     "Rank/Filter, dBext(%), Bext_Model, Bext_Background, ", &
                     "bNO3, bSO4, bOCM, bSOIL, bPEC, bNO2, bCORS"
             else
                write(lun,'(2a)') &
                     "RecSet, Species, X, Y, Lat, Lon, Time, Period(hr), ", &
                     "Rank/Filter, Value, units"
             end if

          else if (output(iOut)%outType == "XYZ") then

             write(lun,'(2a)') "           X            Y          Lat", &
                  "          Lon        Value      Units        Time"

          else if (output(iOut)%outType == "POST" .or. &
               output(iOut)%outType == "PLOT" ) then
!
!---- write a header to each POSTFILE
!
10           format(8a)         ! the formats used in the POSTFILE header
11           format(a,75x,6a)
12           format(a,i6,a)

             write(lun, 10) "* sciDOSpost (2015-05-06): ", ProjName(1)(1:73), &
                  datestr(3:4),"/",datestr(5:6),"/",datestr(7:8)
             write(lun, 11) "* MODELING OPTIONS USED: ",                &
                  timestr(1:2),":",timestr(3:4),":",timestr(5:6)
             ! I believe this next line is enough to keep AERMOD happy:
             write(lun, 10) "* NonDFAULT CONC                                              ELEV"
             if (output(iOut)%stat == "A") then      ! "normal" windowed average
                write(AVEstr,'(i5,"-HR")') output(iOut)%Period
             else if (output(iOut)%stat == "S") then ! sum over the period
                write(AVEstr,'(i5,"-HR")') output(iOut)%Period
             else if (output(iOut)%stat == "M") then ! Max Daily rolling average
                write(AVEstr,'(i5,"-HR")') output(iOut)%maxDailyHrs
             end if

             write(lun, 10) "*         POST/PLOT FILE OF CONCURRENT ",  &
                  AVEstr, " VALUES FOR SOURCE GROUP: ","ALL"
             write(lun, 12) "*         FOR A TOTAL OF",nRec(iRset)," RECEPTORS."
             write(lun, 10) "*         FORMAT: (3(1X,F13.5),3(1X,F8.2),2X,",&
                  "A6,2X,A8,2X,I8.8,2X,A8)"
             write(lun, 10) "*        X             Y      AVERAGE CONC", &
                  "    ZELEV    ZHILL    ZFLAG    AVE     GRP       DATE     NET ID"
             write(lun, 10) "* ____________  ____________  ____________", &
             "   ______   ______   ______  ______  ________  ________  ________"

          else if (output(iOut)%outType == "GRD") then

             write(lun,'(a)') "DSAA"
             write(lun,'(2i8)') Rset(iRset)%Grid%NX, Rset(iRset)%Grid%NY
             write(lun,'(2f13.6)') Rset(iRset)%Grid%xW, Rset(iRset)%Grid%xE
             write(lun,'(2f13.6)') Rset(iRset)%Grid%yS, Rset(iRset)%Grid%yN
             write(lun,'(2f13.6)') 0.,0. ! we don't know the range in Z yet

          else if (output(iOut)%outType == "NC") then

             if (debug) print*,'Calling from ncf_set_compress_flag()'
             call flush(6)
             call ncf_set_compress_flag()

             action = "Opening netCDF file for " // trim(output(iOut)%outFile)
             if (debug) print*,"Calling ncf_createfile"; call flush(6)
             call ncf_createfile(output(iOut)%outFile,action,output(iOut)%lun)

             if (debug) print*,"Calling ncf_set_vars_base"; call flush(6)
             call ncf_set_vars_base

             if (output(iOut)%units == "ppm") then
                spec_units  = "ppmv"
             else if (output(iOut)%units == "ppb") then
                spec_units  = "ppbv"
             else if (output(iOut)%units == "ug/m3") then
                spec_units  = "micrograms m-3"
             else
                spec_units  = output(iOut)%units
             end if
             spec_long_name = output(iOut)%chemSpecies
             spec_desc      = output(iOut)%chemSpecies // " air concentration"
             spec_coords    = "latitude longitude"

             if (debug) print*,"Calling ncf_set_global"; call flush(6)
             call ncf_set_global(CODE_VERSION,iRset,iymdh_beg)

             if (debug) print*,"Calling ncf_wrt_dim"; call flush(6)
             call ncf_wrt_dim(output(iOut)%outFile, output(iOut)%lun,           &
                  Rset(iRset)%Grid%NX, Rset(iRset)%Grid%NY, 1, nspcs)
             if (debug) print*,"Calling ncf_wrt_global"; call flush(6)
             call ncf_wrt_global(output(iOut)%outFile, output(iOut)%lun,        &
                  nspcs, output(iOut)%chemSpecies)
             if (debug) print*,"Calling ncf_wrt_vars_base"; call flush(6)
             call ncf_wrt_vars_base(output(iOut)%outFile, output(iOut)%lun)
             if (debug) print*,"Calling ncf_wrt_vars_species"; call flush(6)
             call ncf_wrt_vars_species(output(iOut)%outFile, output(iOut)%lun,  &
                  Rset(iRset)%Grid%NX, Rset(iRset)%Grid%NY, 1, nspcs,           &
                  output(iOut)%chemSpecies, 4)
             if (debug) print*,"Calling ncf_enddef_file"; call flush(6)
             call ncf_enddef_file(output(iOut)%outFile, output(iOut)%lun)
             if (debug) print*,"Calling ncf_wrt_data_grid"; call flush(6)
             call ncf_wrt_data_grid(output(iOut)%outFile, output(iOut)%lun,     &
                  Rset(iRset)%Grid%NX,      Rset(iRset)%Grid%NY,                &
                  lat(iRset,1:nRec(iRset)), lon(iRset,1:nRec(iRset)),           &
                  xRec(iRset,1:nRec(iRset)),yRec(iRset,1:nRec(iRset)),          &
                  topo(iRset,1:nRec(iRset)) )
             if (debug) print*,"Done with NC creation/headers"; call flush(6)

          end if

       else ! we've already created it, so append to it by assigning the same LUN

          if (Rset(iRset)%IsGrid .and.                 &
              (output(iOut)%outType == "GRD" .or.     &
               output(iOut)%outType == "NC") ) then
             write(*,*) "Error: Cannot append to an existing GRID output file."
             write(*,*) "       Stopping."
             stop
          end if

          do i = iOut, 1, -1
             if (output(iOut)%outFile == output(i)%outFile) &
                  output(iOut)%lun = output(i)%lun
          end do
          write(*,'(1x,2(a,i4),2a)') "Output ",iOut," appended to file ",  &
               output(iOut)%lun-500," ", trim(output(iOut)%outFile)

       end if

    end do          ! do iOut = 1, nOuts
    write(*,*)      ! make screen output look nicer

    return

  end subroutine open_files_write_headers
!
!*************************************************************************
!
  subroutine max_or_avg(sumVal,newVal,stat,iTime_cur,iTime_max)

    real              :: SumVal      ! running total or maximum values
    real              :: newVal      ! new values to add or take max of
    character (len=1) :: stat        ! "A" = average (sum here) "M" = max
    integer           :: iTime_cur   ! current time stamp
    integer           :: iTime_max   ! if this is a new max, set to iTime_cur

    if ( stat == "A" ) then      ! average by taking the sum

       !print*,"adding ",newVal," to ",sumVal
       sumVal = sumVal + newVal
       iTime_max = iTime_cur     ! will only use the last one

    else if ( stat == "S" ) then ! take the sum

       !print*,"adding ",newVal," to ",sumVal, " (sumVal)"
       sumVal = sumVal + newVal
       iTime_max = iTime_cur     ! will only use the last one

    else if ( stat == "M" ) then ! take the maximum daily running average

       !print*,"maxing ",newVal," and ",sumVal
       if (newVal > sumVal .or. sumVal == 0) then ! sumVal == 0 takes the last 0
          sumVal = newVal
          iTime_max = iTime_cur  ! will use the correct one
       endif

    end if

    return

  end subroutine max_or_avg
!
!*************************************************************************
!
  integer function find_rank(val,iRset,iRec,iOut,rank)

    integer iRset, iRec, iOut, rank, r
    real    val

    r = 1
    if (debug) then
       print 10," iRec,r,rank,val,max_val:",iRec,r,rank,val,  &
            max_val(iRset,iRec,iOut,r)
10     format(a,3i4,2g18.8)
       call flush(6)
    endif
    do while (val < max_val(iRset,iRec,iOut,r) .and. r < rank)
       if (debug) then
          print*,"r,val,max_val:",r,val,max_val(iRset,iRec,iOut,r)
          call flush(6)
       endif
       r = r + 1
    end do
    if (val < max_val(iRset,iRec,iOut,r)) r = r + 1 ! correct for r == rank
    if (debug) print*,"found rank: ",r
    find_rank = r

    return
  end function find_rank
!
!*************************************************************************
!
  SUBROUTINE insert(val, iRset, iRec, iOut, rank, ind, iymdh) ! real

! insert the value val in max_val(,) as the ind-th value, for a given iRec

    integer iRset, iRec, iOut, rank, r, ind, i
    integer iymdh  ! current time in YYYYMMDDHH format
    real    val

    if (ind > rank) return    ! nothing to be done

    if (ind < rank) then
       do r = rank, ind+1, -1 ! move those lower than ind down one notch
          max_val(iRset,iRec,iOut,r)  = max_val(iRset,iRec,iOut,r-1)
          max_time(iRset,iRec,iOut,r) = max_time(iRset,iRec,iOut,r-1)
       end do
    end if

    max_val(iRset,iRec,iOut,ind)  = val
    max_time(iRset,iRec,iOut,ind) = iymdh

    if (debug) then
       print*,"inserted ",val," at index ",ind," Time ",iymdh
       print*,"max_val now ",(max_val(iRset,iRec,iOut,i), i=1,rank)
    end if

    return
  END SUBROUTINE insert
!
!*************************************************************************
!
  SUBROUTINE insert_dBc(val, iRset, iRec, iOut, i, rank, ind, iymdh) ! real

! insert the value val in max_val(,) as the ind-th value, for a given iRec

    integer iRset, iRec, iOut, i,j, rank, r, ind
    integer iymdh  ! current time in YYYYMMDDHH format
    real    val

    if (ind > rank) return    ! nothing to be done

    if (ind < rank) then
       do r = rank, ind+1, -1 ! move those lower than ind down one notch
          dBc_max(iRset,iRec,iOut,r,i)  = dBc_max(iRset,iRec,iOut,r-1,i)
       end do
    end if

    dBc_max(iRset,iRec,iOut,ind,i)  = val

    if (debug) then
       print*,"inserted dBc ",val," at index ",ind
       print*,"dBc_max(i=1) now ",(dBc_max(iRset,iRec,iOut,j,1), j=1,rank)
    end if

    return
  END SUBROUTINE insert_dBc
!
!*************************************************************************
!
  REAL FUNCTION PPM_to_ugm3(PPM, MW)
!
!---- Convert from PPM (gasses) to ug/m3 (micro-grams per cubic meter).
!     Here are some verification values, taken from EPA definitions, which
!     are not necessarily the most accurate scientifically...
!          NO :   0.1 PPM = 123 ug/m3
!          NO2: 0.053 PPM = 100 ug/m3
!          NO2:   0.1 PPM = 188 ug/m3
!          SO2: 0.075 PPM = 196 ug/m3
!          CO :     9 PPM =  40 mg/m3 (millegrams, not micrograms)
!          O3 : 0.075 PPM = 147 ug/m3
!          Pb :     1 PPM = 8.5 mg/m3 (millegrams, not micrograms)
!     See http://www.skcinc.com/converter/converter.asp, for example.
!
    implicit none
    real            :: PPM          ! Input concentration (ppm)
    real            :: MW           ! Input Molecular Weight
    real, parameter :: Tk = 298.    ! STP air temperatire (K)
    real, parameter :: Pa = 1013.25 ! STP air pressure (mb)

    ! Tk (temperature in Kelvin) and Pa (pressure in mb) are globally defined

    if (PPM == -999. .or. MW == -999. .or. Tk == -999. .or. Pa == -999.) &
         then
       PPM_to_ugm3 = -999.  ! default to missing
       RETURN
    end if

    PPM_to_ugm3 = PPM * Pa * 750.06 / (62.4 * Tk) * MW

    RETURN

  END FUNCTION PPM_to_ugm3
!
!*************************************************************************
!
  REAL FUNCTION PPB_to_ugm3(PPB, MW)

    implicit none
    real            :: PPB          ! Input concentration (ppm)
    real            :: MW           ! Input Molecular Weight
    real, parameter :: Tk = 298.    ! STP air temperatire (K)
    real, parameter :: Pa = 1013.25 ! STP air pressure (mb)

    ! Tk (temperature in Kelvin) and Pa (pressure in mb) are globally defined

    if (PPB == -999. .or. MW == -999. .or. Tk == -999. .or. Pa == -999.) &
         then
       PPB_to_ugm3 = -999.  ! default to missing
       RETURN
    end if

    PPB_to_ugm3 = PPB * Pa * 750.06 / (62.4 * Tk) * MW / 1000.

    RETURN

  END FUNCTION PPB_to_ugm3
!
!*************************************************************************
!
  REAL FUNCTION ugm3_to_PPM(ugm3, MW)

    implicit none
    real            :: ugm3         ! Input concentration (ug/m3)
    real            :: MW           ! Input Molecular Weight
    real, parameter :: Tk = 298.    ! STP air temperatire (K)
    real, parameter :: Pa = 1013.25 ! STP air pressure (mb)

    ! Tk (temperature in Kelvin) and Pa (pressure in mb) are globally defined

    if (ugm3 == -999. .or. MW == -999. .or. Tk == -999. .or. Pa == -999.) &
         then
       ugm3_to_PPM = -999.  ! default to missing
       return
    end if

    ugm3_to_PPM = ugm3 / (Pa * 750.06 / (62.4 * Tk) * MW)

    RETURN

  END FUNCTION ugm3_to_PPM
!
!*************************************************************************
!
  REAL FUNCTION ugm3_to_PPB(ugm3, MW)

    implicit none
    real            :: ugm3         ! Input concentration (ug/m3)
    real            :: MW           ! Input Molecular Weight
    real, parameter :: Tk = 298.    ! STP air temperatire (K)
    real, parameter :: Pa = 1013.25 ! STP air pressure (mb)

    ! Tk (temperature in Kelvin) and Pa (pressure in mb) are globally defined

    if (ugm3 == -999. .or. MW == -999. .or. Tk == -999. .or. Pa == -999.) &
         then
       ugm3_to_PPB = -999.  ! default to missing
       return
    end if

    ugm3_to_PPB = ugm3 / (Pa * 750.06 / (62.4 * Tk) * MW) * 1000.

    RETURN

  END FUNCTION ugm3_to_PPB
!
!*************************************************************************
!
  SUBROUTINE find_units_conv(units_in, units_out, avg_time, units_conv)

    implicit none
! inputs:
    character (len=*) :: units_in, units_out
    integer           :: avg_time   ! in seconds

! output:
    real              :: units_conv ! output conversion factor

! local
    integer           :: i,j,k,l,m
    character (len=9) :: numer_in, numer_out, denom_in, denom_out, time_in

    if (debug .and. .false.) then ! gets VERY verbose, called at each point
       print*,"Entering find_units_conv()..."
       print*,"  units_in  = ",trim(units_in)
       print*,"  units_out = ",trim(units_out)
    end if

    i = index(units_in, "-")
    j = index(units_in, "/")
    k = len_trim(units_in)

    l = index(units_out,"/")
    m = len_trim(units_out)

    units_conv = 1.   ! default

    numer_out = units_out(1:l-1)
    denom_out = units_out(l+1:m)

    if ( i > 0 ) then ! either kg-s/m3 or g-s/m3, Tracer (not mult-component)
       time_in  = units_in(i+1:j-1)
       if ( TRIM(time_in) == "s" ) then
          units_conv = units_conv / avg_time ! s/s
       else if ( TRIM(time_in) == "hr" ) then
          units_conv = units_conv / avg_time * 3600 ! hr/s * s/hr
       else
          write(*,*) "Error in find_units_conv(): Unknown time units:", &
               trim(time_in)
          write(*,*) "  units_in  = ",trim(units_in)
          write(*,*) "  units_out = ",trim(units_out)
          stop
       endif
       numer_in = units_in(1:i-1)
       denom_in = units_in(j+1:k)
    else              ! any of kg/m2, g/m2, ug/m3 or ug/m2
       numer_in = units_in(1:j-1)
       denom_in = units_in(j+1:k)
    end if

    if ( numer_in == numer_out) then
       ! nothing to do here
    else if ( numer_in == "kg" .and. numer_out == "ug") then
       units_conv = units_conv * 1.e9  ! ug/kg
    else if ( numer_in == "ug" .and. numer_out == "kg") then
       units_conv = units_conv * 1.e-9 ! kg/ug
    else if ( numer_in == "g"  .and. numer_out == "ug") then
       units_conv = units_conv * 1.e6  ! ug/g
    else if ( numer_in == "ug" .and. numer_out == "g" ) then
       units_conv = units_conv * 1.e-6  ! g/ug
    else if ( numer_in == "kg" .and. numer_out == "mg") then
       units_conv = units_conv * 1.e6 ! kg/mg
    else if ( numer_in == "mg" .and. numer_out == "kg") then
       units_conv = units_conv * 1.e-6 ! kg/mg
    else if ( numer_in == "g"  .and. numer_out == "mg") then
       units_conv = units_conv * 1.e3 ! g/mg
    else if ( numer_in == "mg" .and. numer_out == "g" ) then
       units_conv = units_conv * 1.e-3 ! mg/g
    else
       write(*,*) "Error in find_units_conv(): unknown numerator units:"
       write(*,*) "  units_in  = ",trim(units_in)
       write(*,*) "  units_out = ",trim(units_out)
       stop
    end if

    if ( denom_in == denom_out ) then
       ! nothing to do here
    else if ( denom_in == "m2"  .and. denom_out == "ha") then
       units_conv = units_conv * 1.e4 ! m^2/hectare
    else if ( denom_in == "ha"  .and. denom_out == "m2") then
       units_conv = units_conv * 1.e-4 ! hectare/m^2
    else if ( denom_in == "cm2" .and. denom_out == "ha") then
       units_conv = units_conv * 1.e8 ! cm^2/hectare
    else if ( denom_in == "ha"  .and. denom_out == "cm2") then
       units_conv = units_conv * 1.e-8 ! hectare/cm^2
    else if ( denom_in == "cm2" .and. denom_out == "m2") then
       units_conv = units_conv * 1.e4 ! cm^2/m^2
    else if ( denom_in == "m2"  .and. denom_out == "cm2") then
       units_conv = units_conv * 1.e-4 ! m^2/cm^2
    else if ( denom_in == "m2"  .and. denom_out == "m3") then
       write(*,*) "Error in find_units_conv(): tried to convert /m2 to /m3:"
       write(*,*) "  units_in  = ",trim(units_in)
       write(*,*) "  units_out = ",trim(units_out)
       stop
    else
       write(*,*) "Error in find_units_conv(): unknown denominator units:"
       write(*,*) "  units_in  = ",trim(units_in)
       write(*,*) "  units_out = ",trim(units_out)
       stop
    end if

    if (debug .and. .false.) print*, "  units_conv = ",units_conv

    RETURN

  END SUBROUTINE find_units_conv
!======================================================================
! DataInitialization
!======================================================================

  SUBROUTINE InitSciDosPost()

! Intializes all common variables

    USE tooluser_fd
    USE Extract_fi
    USE GetTimes_fi
    USE contour_fd

    IMPLICIT NONE

!--- Initialize

    callerID = USER_ID
    maxTry   = 5
    nFields  = 0

!Error handling

    nError      = NO_ERROR
    eMessage    = ' '
    eInform     = ' '
    eAction     = ' '
    eRoutine    = ' '

!Flags

    extractMethod = GRID

    Project%name = ' '
    Project%path = ' '

!Plot type

    plotType%type     = HP_MEAN
    plotType%data     = 0.0

!Contour

    contourHead%number    = 1
    contourHead%DrawMode  = PLOT_OFF
    contourHead%LabelMode = PLOT_OFF
    contourHead%Scale     = 1.0
    contourHead%Unit      = 'default'
    contourMode           = CLOSE_CONTOUR

!Plot category data

    sliceHeight    = 0.0             !Horizontal slice height
    sliceXmin      = DEF_VAL_R       !Minimum X slice value - vertical slice
    sliceXmax      = DEF_VAL_R       !Maximum X slice value - vertical slice
    sliceYmin      = DEF_VAL_R       !Minimum Y slice value - vertical slice
    sliceYmax      = DEF_VAL_R       !Maximum Y slice value - vertical slice
    sliceZmin      = 0.0             !Minimum Z slice value - vertical slice
    sliceZmax      = 2500.           !Maximum Z slice value - vertical slice
    sliceNz        = 21              !Slice Z points - Vertical slice

    iround         = -999
    riskLevel      = 10.0

!Plot data sizes
    nClass    = 0
    nChoice   = 0
    nKind     = 0
    nTimePuff = 0
    nTimeSrf  = 0
    nTimeMet  = 0
    nTimeOut  = 0
    nTable    = 0
    nCol      = 0
    nRow      = 0
    nTriangle = 0
    nNode     = 0
    nPoint    = 0
    nLine     = 0

!Field max values

    FldMax = -HUGE(1.0)
    FldMin = HUGE(1.0)

!Grid

    maxGrd         = 0                      !Maximum grid cells
    GridType       = NATIVE
    ContourType    = AUTO
    extractMethod  = GRID

    xMin =  HUGE(1.0)
    xMax = -HUGE(1.0)
    yMin = xMin
    yMax = xMax
    zMin = sliceZmin
    zMax = SliceZmax
    dx   = 0.0
    dy   = 0.0
    dz   = 0.0

    nx = 0
    ny = 0
    nz = 0

!Unit numbers
    lun_in  = 5
    lun_out = 6
    lun_grd = 0
    lun_ter = 0

    Field%category   = HP_SSLICE              !Field category index
    Field%class      = -1
    Field%choice     = 0
    Field%kind       = 0
    Field%timeID     = 0
    Field%userTime   = 0.0
    Field%iNotUsed   = SCIPoff
    Field%maxCells   = 25000
    Field%maxLev     = 99
    Field%fldLev     = 99
    Field%resolution = 1.0
    Field%interpType = 0.0
    Field%units      = 'XX'
    Field%project    = ' '
    Field%path       = ' '

! Fill up SCIPFieldCoordinateT structure
    Field%coordinate%mode                 = HD_LATLON
    Field%coordinate%UTMZone              = NOT_SET_I
    Field%coordinate%reference%x          = NOT_SET_R
    Field%coordinate%reference%y          = NOT_SET_R
    Field%coordinate%reference%lat        = NOT_SET_R
    Field%coordinate%reference%lon        = NOT_SET_R
    Field%coordinate%vertSlice%resolution = NOT_SET_I
    Field%coordinate%vertSlice%startPt%x  = NOT_SET_R
    Field%coordinate%vertSlice%startPt%y  = NOT_SET_R
    Field%coordinate%vertSlice%endPt      = Field%coordinate%vertSlice%startPt
    Field%coordinate%horzSlice%height     = NOT_SET_R
    Field%coordinate%horzSlice%mode       = NOT_SET_I

    NULLIFY( TimePuff )
    NULLIFY( TimeSrf )
    NULLIFY( TimeMet )
    NULLIFY( TimeOut )

    contourExt = '.txt'

    RETURN
  END SUBROUTINE InitSciDosPost

!======================================================================
! ToolInitialization
!======================================================================

  SUBROUTINE InitToolSciDosPost()

    USE basic_fd
    USE tooluser_fd
    USE SCIPtool
    USE Extract_fi
    USE GetTimes_fi

    IMPLICIT NONE

    INTEGER irv
    INTEGER request
    INTEGER(LEN_ADDRESS) CallBackAddress
    INTEGER idefault
    LOGICAL lExist

    CHARACTER(32)  mode
    CHARACTER(128) string1,string2,string3,string4

    TYPE( fileNameT ) :: file

    CHARACTER(PATH_MAXLENGTH), EXTERNAL :: StripNull, AddNull

    INTEGER,              EXTERNAL :: sysGetProfileString
    INTEGER,              EXTERNAL :: sysGetProfileInt
    INTEGER(LEN_ADDRESS), EXTERNAL :: ADDRESSOF
    INTEGER,              EXTERNAL :: sppCallback

    CallBackAddress = ADDRESSOF( sppCallback )

    request = IBSET(0,HIB_UTM)

!--- Start up : Initialize

    IF( LEN_TRIM(ini_file) <= 0 )THEN
       WRITE(6,*)
       WRITE(6,'(''PATH for scipuff.ini file(press Enter for default): '',$)')
       READ(lun_in,'(A)')string2
       IF ( LEN_TRIM(string2) == 0 ) THEN
          ini_file = 'scipuff.ini'
          INQUIRE( file=ini_file,EXIST=lexist )
          IF( .NOT.lexist )THEN
             ini_file = 'DEFAULT'
          END IF
       ELSE
          ini_file = TRIM(ADJUSTL(string2))//'/scipuff.ini'
          INQUIRE( file=ini_file,EXIST=lexist )
          IF( .NOT.lexist )THEN
             WRITE(6,*)'Cannot find ',TRIM(ini_file)
             ini_file = 'DEFAULT'
          END IF
       END IF
    ELSE
       ini_file = ADJUSTL(ini_file)
    END IF

    IF( TRIM(ini_file) == 'DEFAULT' )THEN
       WRITE(6,*)'Using default scipuff.ini'
       ini_file = 'scipuff.ini'
    END IF

    INQUIRE(file=ini_file,EXIST=lexist)
    IF( .NOT.lexist )THEN
       nError      = UK_ERROR
       eMessage    = 'Unable to initialize the SCIPtool library'
       eInform     = 'Specified initialization file not found'
       eAction     = 'INI file='//TRIM(ini_file)
       eRoutine    = 'ToolInitialzaion'
       GOTO 9999
    END IF
    WRITE(6,*)
    WRITE(6,*)'Initializing SCIPtool from '//TRIM(ini_file)
    WRITE(6,*)

    string1 = AddNull( 'Paths' )
    string2 = AddNull( 'SciDataDir' )
    string3 = '.'
    string4 = AddNull( TRIM(ini_file) )
    irv = sysGetProfileString( string1,string2,string3,flagDir,string4 )
    IF( TRIM(flagDir) /= '.' )THEN
      string3 = flagDir
      flagDir = 'Flag2010'
      CALL ADDPATH(flagDir,string3)
    END IF
    WRITE(6,*)'flagDir = ',TRIM(flagDir)
    WRITE(6,*)

    string4 = AddNull( TRIM(ini_file) )
    string1 = AddNull( 'SCIPMode' )
    string2 = AddNull( 'GUIMode' )
    string3 = 'Standard'
    irv = sysGetProfileString( string1,string2,string3,mode,string4 )

    CALL cupper( mode )

    SELECT CASE( TRIM(mode) )
    CASE( 'STANDARD' )
       limit%met1D       = HUGE(1)
       limit%puffs       = 20000
       limit%surfaceGrid = 25000

    CASE( 'OPERATIONAL' )
       limit%met1D       = HUGE(1)
       limit%puffs       = 20000
       limit%surfaceGrid = 25000

    CASE( 'EXTENDED' )
       limit%met1D       = HUGE(1)
       limit%puffs       = 40000
       limit%surfaceGrid = 85000

    CASE( 'ULTIMATE' )
       limit%met1D       = HUGE(1)
       limit%puffs       = 60000
       limit%surfaceGrid = 100000

    CASE DEFAULT
       limit%met1D       = HUGE(1)
       limit%puffs       = 20000
       limit%surfaceGrid = 25000
       string2 = AddNull( 'MaxMet1D' )
       idefault = limit%met1D
       irv = sysGetProfileInt( string1,string2,idefault,limit%met1D,string4 )
       string2 = AddNull( 'MaxPuff' )
       idefault = limit%puffs
       irv = sysGetProfileInt( string1,string2,idefault,limit%puffs,string4 )
       string2 = AddNull( 'MaxGrid' )
       idefault = limit%surfaceGrid
       irv = sysGetProfileInt( string1,string2,idefault,limit%surfaceGrid,string4 )
    END SELECT

!==== Initialize PlotStub

    WRITE(6,'("Running SciDosPost in ",A," mode")')TRIM(mode)
    WRITE(6,'("  MaxPuff = ",I15)')limit%puffs
    WRITE(6,'("  MaxGrid = ",I15)')limit%surfaceGrid

    file%string = TRIM(ini_file)

    irv = SCIPInitTool( callerID,CallBackAddress,request,limit,file )
    IF( irv == SCIPFailure )THEN
       CALL toolError ('Failed to initialize the SCIPtool library' )
       GOTO 9999
    END IF

    lInit = .TRUE.

9999 CONTINUE

    RETURN
  END SUBROUTINE InitToolSciDosPost
!***********************************************************************
!
!**** NCF_SET_GLOBAL, which had to be here to "see" Rset() below
!
!***********************************************************************
  subroutine ncf_set_global(CODE_VERSION,iRset,iymdh_beg)

    implicit none
!
!-----------------------------------------------------------------------
!    Description:
!-----------------------------------------------------------------------
!
!   This routine sets the global file attributes for the NetCDF file
!
!      Argument description:
!       Inputs:
!         inname      C filetype of this kind of file
!         igrd        I grid index
!         iymdh_beg   I model begin date (YYYYMMDDHH)
!       Outputs:
!
!-----------------------------------------------------------------------
!    LOG:
!-----------------------------------------------------------------------
!
!     02/20/17   --gwilson--    Original development
!     08/10/18   --bbrashers--  Adapted for sciDOSpost
!
!-----------------------------------------------------------------------
!    Argument declarations:
!-----------------------------------------------------------------------
!
    character (len=*) :: CODE_VERSION      ! note to add to the netCDF file
    integer           :: iRset             ! index of receptor set
    integer           :: iymdh_beg         ! beginning time-stamp
!
!-----------------------------------------------------------------------
!    External functions:
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!    Local variables:
!-----------------------------------------------------------------------
!
    integer      iy1,im1,id1,ih1, ijul
!
!-----------------------------------------------------------------------
!    Entry point:
!-----------------------------------------------------------------------

!
! --- get beginning date/time ---
!
    call idate2ymdh(iymdh_beg,iy1,im1,id1,ih1)
    call dat2jul(iy1,im1,id1,ijul)

    ncf_cdate = iy1 * 1000 + ijul
    ncf_ctime = ih1 * 10000 ! + imin1 * 1000 + isec 1
    ncf_tstep =   1 * 10000 ! + tstep_min*100 + tstep_sec

    ncf_wdate = ncf_cdate
    ncf_wtime = ncf_ctime

    ncf_sdate = ncf_cdate
    ncf_stime = ncf_ctime
!
!--- domain definition attributes ---
!
    ncf_iutm  = Rset(iRset)%UTMzone
    ncf_istag = 0
!
!--- These may be mis-named, not the _origin_ of the grid (0,0) but the
!    lower-left corner of the lower-left grid cell.
!
    ncf_xorig = Rset(iRset)%Grid%xW - Rset(iRset)%Grid%DX / 2.
    ncf_yorig = Rset(iRset)%Grid%yS - Rset(iRset)%Grid%DX / 2.

    if( Rset(iRset)%CoordType == "LATLON" ) then ! lat-lon
       ncf_gdtyp = 1
       ncf_xcell = Rset(iRset)%Grid%DX
       ncf_ycell = Rset(iRset)%Grid%DX
    else
       ncf_gdtyp = 5
       ncf_xcell = Rset(iRset)%Grid%DX * 1000. ! was in km, now in m (meters)
       ncf_ycell = Rset(iRset)%Grid%DX * 1000.
    endif
    ncf_xcent = 0.
    ncf_ycent = 0.
    ncf_p_alp = float(Rset(iRset)%UTMzone)
    ncf_p_bet = 0.
    ncf_p_gam = 0.
    ncf_nlays = 1
    ncf_nrows = Rset(iRset)%Grid%NX
    ncf_ncols = Rset(iRset)%Grid%NY
    ncf_nthik = 1
    ncf_nvars = 1 + NCF_BASE_VARS ! 1 = number of species
!
! --- file description attributes ---
!
    ncf_name = "AVERAGE"
    ncf_note = CODE_VERSION
    ncf_itzon = 0             ! no idea
    ncf_ftype = 1             ! "custom 3"
    ncf_vgtyp = 6             ! vertical grid type, 6 = height, m AGL
    ncf_vgtop = 10000.        ! grid top pressure for sigma structure (hPA)
    ncf_vglvls = 0.           ! list of sigma levels
    ncf_gdnam = CODE_VERSION  ! name of the program
    ncf_upnam = CODE_VERSION  ! name of the program
    ncf_filedesc = "AVERAGE"
!
! --- simulation description attributes ---
!     Not used for sciDOSpost

    ! ncf_grid_id = 1
    ncf_i_grid_start = 1
    ncf_i_grid_end = Rset(iRset)%Grid%NX
    ncf_j_grid_start = 1
    ncf_j_grid_end = Rset(iRset)%Grid%NX
    ! ncf_grid_mesh_factor = meshold(igrd)
    ! ncf_flexi_nest = 0
    ! if( lflexi ) ncf_flexi_nest = 1
    ! ncf_advection = Advection_Solver
    ! ncf_chem_solver = Chemistry_Solver
    ! ncf_pig = PiG_Submodel
    ! ncf_probing_tool = Probing_Tool
    ! ncf_total_species = nspec
    ! ncf_radical_species = nrad
    ! ncf_gas_species = ngas
    ! ncf_pm_species = naero
    ! ncf_reactions = nreact
    ! ncf_drydep = Drydep_Model
    ! ncf_wetdep = 0
    ! if( lwet ) ncf_wetdep = 1
    ! ncf_acm2 = 0
    ! if( lacm2 ) ncf_acm2 = 1
    ! ncf_cig_model = 0
    ! if( Subgrid_Convection ) ncf_cig_model = 1
    ! ncf_surface_model = 0
    ! if( lsrfmod ) ncf_surface_model = 1
    ! ncf_inline_ix_emiss = 0
    ! if( lixemis ) ncf_inline_ix_emiss = 1
    ! ncf_super_stepping = 0
    ! if( lsuper ) ncf_super_stepping = 1
    ! ncf_gridded_emiss = 0
    ! if( larsrc ) ncf_gridded_emiss = 1
    ! ncf_point_emiss = 0
    ! if( lptsrc ) ncf_point_emiss = 1
    ! ncf_ignore_emiss_dates = 0
    ! if( le1day ) ncf_ignore_emiss_dates = 1
    ! ncf_output_3d = 0
    ! if( l3davg(igrd) ) ncf_output_3d = 1
    ! ncf_pig_sample_grid = 0
    ! if( lsample ) ncf_pig_sample_grid = 1
    ! ncf_pig_sample_bckgnd = 0
    ! if( lbckgrd ) ncf_pig_sample_bckgnd = 1
    ! ncf_conventions = "CF-1.6"
    ncf_history = 'Generated by '//CODE_VERSION

    return
  end subroutine ncf_set_global
!
!*************************************************************************
!
END MODULE SciDosPostTools
