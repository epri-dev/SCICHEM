MODULE control

  IMPLICIT NONE

  CHARACTER (len=1),   dimension(3) :: comment   = (/ "#", ";", "!" /)
  CHARACTER (len=1),   dimension(5) :: delim     = (/ " ", ",", "#", ";", "!" /)
  CHARACTER (len=1),   dimension(3) :: datedelim = (/ "-", "_", ":" /)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CONTAINS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE read_control(FillDemFileList)

    USE TER_MAIN
    implicit none

    integer                         :: nAreas, nOuts
    integer                         :: ios, i,j
    integer                         :: num_words
    integer, parameter              :: lun = 1
    character (len=200)             :: line, word, word2, word3, tmp
    logical                         :: FillDemFileList ! F = just count 'em
    logical                         :: verbose = .false.

99  format(a200)

    iDEM   = 0 ! only used if FillDemFileList == .true.
    open(lun,file=INPFIL,status='old',err=999)

    read(lun,99,iostat=ios) line
    call count_words(line,num_words)    ! read to first non-comment non-blank
    do while ((any(line(1:1) == comment) .or. num_words == 0) .and. ios == 0)
       read(lun,99,iostat=ios) line
       call count_words(line,num_words)
    end do
   if (verbose) print*,"line = ",trim(line)

    do while (ios == 0)                 ! loop over lines in control file

       call get_word(line,1,word)
       call CUPPER( word )

       if (verbose) print*,"word = ",trim(word)

       if ( word == "NADA") then
          call get_word(line, 2, word2)
          read(word2, *) NADA
       endif

       if ( word == "USE" ) then
          call get_word(line,2,word2)
          if (word2 == "LL" .or. word2 == "LATLON") then
             DOMTYP = "LAT"
          else if (word2 == "UTM") then
             DOMTYP = "UTM"
             call get_word(line,3,word3) ; read(word3,*,err=83) ZONMIN
             ZONMAX = ZONMIN
          else
             write(*,*) "Error reading keyword USE: ",trim(word2)
             write(*,*) "Acceptable values:"
             write(*,*) "   LATLAT         (or LL)"
             write(*,*) "   UTM ZONE       (where ZONE is a number)"
             exit
          endif
       end if

       if ( word == "DOMAIN" ) then
          if (DOMTYP == "UTM") then
             call get_word(line,2,word2) ; read(word2,*,err=84) XDMIN
             call get_word(line,3,word2) ; read(word2,*,err=84) YDMIN
             call get_word(line,4,word2) ; read(word2,*,err=84) XDMAX
             call get_word(line,5,word2) ; read(word2,*,err=84) YDMAX
          else
             call get_word(line,2,word2) ; read(word2,*,err=84) YDMIN ! BegLat
             call get_word(line,3,word2) ; read(word2,*,err=84) XDMIN ! BegLon
             call get_word(line,4,word2) ; read(word2,*,err=84) YDMAX ! EndLat
             call get_word(line,5,word2) ; read(word2,*,err=84) XDMAX ! EndLon
          end if
       endif

       if ( word == "SPACING") then
          call get_word(line,2,word2) ; read(word2,*,err=85) Spacing
       end if

       if (word == "INPUT")  then
          call get_word(line,2,TYPDAT) ! NED or DEM
          if (TYPDAT /= "NED" .and. TYPDAT /= "DEM") then
             write(*,*) "Error: Keyword INPUT must be followed by either NED or DEM"
             stop
          end if
          if (FillDemFileList) then

             iDEM = iDEM + 1 ! index, not total count
             call get_word(line,3,word3)
             DEMFIL(iDEM) = word3

             i = max(index(word3, ".",.true.), 1)
             j = max(index(INPFIL,".",.true.), 1)
             DIRFIL(IDEM) = word3(1:i) // INPFIL(1:j) // 'dir'
             IDXFIL(IDEM) = word3(1:i) // INPFIL(1:j) // 'idx'

!*       Set optional flags to false for this file

             L_DEMCHECK(IDEM)       = .FALSE.
             L_TiffDebug(IDEM)      = .FALSE.
             L_UserElevUnits(IDEM)  = .FALSE.

!*    Dynamically Allocate File Units (100's).
!*    Set so there is no limit to number of maps that can be processed.

             IDMUNT(IDEM) = 100 + (IDEM-1)*3 + 1
             IDRUNT(IDEM) = 100 + (IDEM-1)*3 + 2
             IDXUNT(IDEM) = 100 + (IDEM-1)*3 + 3
          else ! just count them
             NUMDEM = NUMDEM + 1
             NDEM   = NUMDEM      ! why do both these exist in AERMAP?
          end if
          ! rest is processed in read_DEM_files()
       end if

       if (word == "OUTPUT") then
          call get_word(line,2,RECFIL)
       end if

!--- Done with this line, read the next valid line

       read(lun,99,iostat=ios) line
       call count_words(line,num_words)
       do while (num_words == 0 .and. ios == 0)
          read(lun,99,iostat=ios) line
          call count_words(line,num_words)
       end do

    end do ! do while (ios == 0) ! loop over lines in control file

!--- Calculate the number of (discrete) receptors we will process,
!    so we can allocate in ALLSET.

    if (XDMAX <= XDMIN .and. DOMTYP == "LAT") then
          write(*,*) "Error: Ending longitude <= beginning longitude."
          write(*,*) "       This program assumes Western hemisphere"
          write(*,*) "       has negative longitudes."
       stop
    end if
    if (YDMAX <= YDMIN .and. DOMTYP == "LAT") then
       write(*,*) "Error: Ending latitude <= beginning latitude."
       stop
    end if

    IXM = floor( (XDMAX - XDMIN) / Spacing )
    IYM = floor( (YDMAX - YDMIN) / Spacing )
    NREC = IXM * IYM
    NUMREC = NREC

    if (NREC == 0) then
       write(*,*) "Error: no receptors within specified range:"
       write(*,*) "  DOMTYP = ",DOMTYP
       if (DOMTYP == "LAT") then
          write(*,*) "  Lat-Lon range: ",XDMIN,YDMIN, XDMAX,YDMAX
       else
          write(*,*) "  X-Y range: ",XDMIN,YDMIN, XDMAX,YDMAX
       end if
       stop
    endif

    if (FillDemFileList) then
       OPEN(UNIT=IRUNIT,FILE=RECFIL,ERR=998,STATUS='REPLACE')
       if (DOMTYP == "LAT") then
          write(IRUNIT,'(a)') "LATLON"
          write(IRUNIT,'(4(e13.5,1x),2(i6,1x))') XDMIN, YDMIN, &
               Spacing, Spacing, IXM, IYM
       else
          write(IRUNIT,'(a,i2)') "UTM     ",ZONMIN
          write(IRUNIT,'(4(e13.5,1x),2(i6,1x))') XDMIN*1.e-3, YDMIN*1.e-3, &
               Spacing*1.e-3, Spacing*1.e-3, IXM, IYM
       end if
    endif

    close(lun)
    return

83  write(*,*) "Error reading UTM Zone: ",trim(word3)
    stop

84  write(*,*) "Error reading values after DOMAIN keyword: ",trim(word2)
    stop

85  write(*,*) "Error reading value after SPACING keyword: ",trim(word2)
    stop

998 write(*,*) "Error opening output file "//trim(RECFIL)
    stop

999 write(*,*) "Error opening control file "//trim(INPFIL)
    stop

  END SUBROUTINE read_control
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
          WRITE(*,*)"*** Error: Asked for word ",which_word,", but only ", &
               this_word," words found."
          WRITE(*,*) "           Problematic line is:"//TRIM(line)

          RETURN
       end if
       i = j + 1                        ! start at next character
    end do

  END SUBROUTINE get_word
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
  SUBROUTINE CUPPER(string)

    IMPLICIT NONE

    INTEGER :: I, ind

    CHARACTER UPCASE*26
    CHARACTER LWCASE*26
    character (len=*) :: string

!     Variable Initializations
    DATA UPCASE/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
    DATA LWCASE/'abcdefghijklmnopqrstuvwxyz'/

    do i = 1, len_trim(string)
        if (string(i:i) /= ' ') then
            ind = index(LWCASE,string(i:i))
            if (ind /= 0) then
                string(i:i) = UPCASE(ind:ind)
            end if
        end if
    end do

    return
  end subroutine CUPPER
!
!*************************************************************************
!
  SUBROUTINE usage

    write(*,*) "Usage: [-h | --help] [--sample] -i ctrl.inp"
    write(*,*) "Options:"
    write(*,*) "   -i file    Specify control file to use"
    write(*,*) "   --sample   Print a sample control file to the screen"
    write(*,*) "   --version  Print the program version to the screen and exit"
    write(*,*) "   --help     Print this help message"
    write(*,*) "   -h         Print this help message"

    stop

  END SUBROUTINE usage
!
!*************************************************************************
!
  SUBROUTINE sample_control_file

1   format(a)

    write(*,1) "; Comment characters are #, ;, and !.  Blank lines are ignored."
    write(*,1) "; Values can be space-delimited OR comma-delimited, or a mix."
    write(*,1) "; Keywords are case-insenstive, filenames are not."
    write(*,1)
    write(*,1) "# USE is followed by either LATLON (alt: LL), or UTM and the UTM zone"
    write(*,1)
    write(*,1) "; USE   LATLON"
    write(*,1) "USE UTM    11"
    write(*,1)
    write(*,1) "# DOMAIN is followed by two lower-left, and two upper-right, coordinates."
    write(*,1) "#   Latitude  is positive in the Northern hemisphere"
    write(*,1) "#   Longitude is positive in the Eastern  hemisphere"
    write(*,1)
    write(*,1) "; DOMAIN   48.865219 -98.761383 48.910038 -98.692932 ! LATLON "
    write(*,1) "DOMAIN 517500  5412500  522500  5417500              ! UTM"
    write(*,1)
    write(*,1) "# SPACING gives the horizontal resolution, in degrees (LL) or meters (UTM)"
    write(*,1) ""
    write(*,1) "; SPACING  0.001  ! ~111 meters (LATLON)"
    write(*,1) "Spacing 100       !  100 meters (UTM)"
    write(*,1) ""
    write(*,1) "; NADA specifies the North American DATUM, use NADA 0 elsewhere"
    write(*,1) "NADA 4"
    write(*,1) ""
    write(*,1) "# INPUT gives the type (NED or DEM), followed by the (path+)filename."
    write(*,1) "# Can contain spaces, if enclosed in quotes. Reads the same types of"
    write(*,1) "# DEM/NED files as AERMAP.  Download NED data from:"
    write(*,1) "#   http://www.mrlc.gov/viewerjs/  (seamless GeoTiff)"
    write(*,1) "#   ftp://rockyftp.cr.usgs.gov/vdelivery/Datasets/Staged/NED (ArcGrid)"
    write(*,1) "# The ftp server has 1x1 degree ArcGrid files, which need to be converted"
    write(*,1) "# to GeoTiff files (e.g. ArgGIS or gdalwarp)."
    write(*,1) ""
    write(*,1) "INPUT  NED  n49w099.tif"
    write(*,1) ""
    write(*,1) "# OUTPUT gives the path and filename of the output terrain file."
    write(*,1) "# Can contain spaces, if enclosed in quotes."
    write(*,1) ""
    write(*,1) "OUTPUT  terrain.ter"
    write(*,1) ""

    stop

  END SUBROUTINE sample_control_file
END MODULE control
