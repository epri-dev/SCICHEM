Module netcdf_tools

! Most of the code in this module was adapted from CAMx v6.50, for which
! we thank the authors Chris Emery and Gary Wilson of Ramboll.

  USE time_tools
  USE netcdf_fd
  implicit none
!  include 'netcdf.inc' ! already included in netcdf_fd.

! MXSPEC  -- maximum number of species (used for local arrays)
  integer   MXSPEC
  parameter ( MXSPEC  =      1 ) ! FIXME: allow multiple species in one file
  integer   MXLAYER
  parameter ( MXLAYER =      1 )


  character*20 spec_units(MXSPEC)     ! FIXME: these should be allocated
  character*20 spec_long_name(MXSPEC)
  character*60 spec_desc(MXSPEC)
  character*60 spec_coords(MXSPEC)
  CHARACTER(NF_MAX_NAME), EXTERNAL :: AddNull, StripNull

  logical ncf_compress

!-----------------------------------------------------------------------
!   Parameters for number of "base" variables: (appear in each file):
!-----------------------------------------------------------------------
!
!   NCF_BASE_VARS  -- number of the base variables
!
  integer NCF_BASE_VARS
  parameter (NCF_BASE_VARS = 9 )

!-----------------------------------------------------------------------
!   Dimension definitions:
!-----------------------------------------------------------------------

  integer ncf_date_time          ! number of variables in time-stamp
  !                               (always 2 - YYYYDDD and HHMMSS)
  integer ncf_date_time_dimid    ! NetCDF dimension ID for this dimension
  integer ncf_lay                ! number of vertical layers in modeling grid
  integer ncf_lay_dimid          ! NetCDF dimension ID for this dimension
  integer ncf_col                ! number of columns in modeling grid
  integer ncf_col_dimid          ! NetCDF dimension ID for this dimension
  integer ncf_row                ! number of rows in modeling grid
  integer ncf_row_dimid          ! NetCDF dimension ID for this dimension
  integer ncf_var                ! number of variables in data section
  integer ncf_var_dimid          ! NetCDF dimension ID for this dimension
  integer ncf_tstep_dimid        ! NetCDF dimension ID for TSTEP dimension

!-----------------------------------------------------------------------
!  Domain definition variables:
!-----------------------------------------------------------------------

  character*20 ncf_x_units                  ! units of coordinate values
  character*20 ncf_x_long_name              ! long name of this variable
  character*60 ncf_x_var_desc               ! description of this variable
  character*20 ncf_y_units                  ! units of coordinate values
  character*20 ncf_y_long_name              ! long name of this variable
  character*60 ncf_y_var_desc               ! description of this variable
  character*20 ncf_layer_units              ! units of layer values
  character*20 ncf_layer_long_name          ! long name of this variable
  character*60 ncf_layer_var_desc           ! description of this variable
  character*20 ncf_longitude_units          ! units of longitude values
  character*20 ncf_longitude_long_name      ! long name of this variable
  character*60 ncf_longitude_var_desc       ! description of this variable
  character*60 ncf_longitude_coordinates    ! coordinates of this variable
  character*20 ncf_latitude_units           ! units of longitude values
  character*20 ncf_latitude_long_name       ! long name of this variable
  character*60 ncf_latitude_var_desc        ! description of this variable
  character*60 ncf_latitude_coordinates     ! coordinates of this variable
  character*20 ncf_topo_units               ! units of longitude values
  character*20 ncf_topo_long_name           ! long name of this variable
  character*60 ncf_topo_var_desc            ! description of this variable
  character*60 ncf_topo_coordinates         ! coordinates of this variable
  character*20 ncf_z_units                  ! units of layer heights
  character*20 ncf_z_long_name              ! long name of this variable
  character*60 ncf_z_var_desc               ! description of this variable
  character*60 ncf_z_coordinates

!-----------------------------------------------------------------------
!  Time stamp variables:
!-----------------------------------------------------------------------
!
!   ncf_tflag             -- array of date/times for each output interval
!   ncf_tflag_units       -- units of the time stamp variable
!   ncf_tflag_long_name   -- long name of the time stamp variable
!   ncf_tflag_var_desc    -- description of the time stamp variable
!   ncf_tflag_varid       -- variable ID for the time stamp variable
!   ncf_etflag            -- array of date/times for each output interval
!   ncf_etflag_units      -- units of the time stamp variable
!   ncf_etflag_long_name  -- long name of the time stamp variable
!   ncf_etflag_var_desc   -- description of the time stamp variable
!   ncf_etflag_varid      -- variable ID for the time stamp variable

  character*20 ncf_tflag_units
  character*20 ncf_tflag_long_name
  character*60 ncf_tflag_var_desc
  integer      ncf_tflag_varid
  character*20 ncf_etflag_units
  character*20 ncf_etflag_long_name
  character*60 ncf_etflag_var_desc
  integer      ncf_etflag_varid

!-----------------------------------------------------------------------
!  Species name variables:
!-----------------------------------------------------------------------

  character*20 ncf_species_units        ! units of species
  character*20 ncf_species_long_name    ! long_name of species
  character*60 ncf_species_var_desc     ! desciptions of species
  character*60 ncf_species_coordinates  ! desciptions of species
  character*10 spavnam(MXSPEC)          ! array of names that will be written
  !                                       to the output files

!-----------------------------------------------------------------------
!  Global attributes for IO/API (domain definition):
!-----------------------------------------------------------------------

  real    ncf_xorig     !  X-coordinate of domain origin
  real    ncf_yorig     !  Y-coordinate of domain origin
  real    ncf_xcell     !  cell width in X direction
  real    ncf_ycell     !  cell width in Y direction
  integer ncf_iutm      !  UTM zone of grid
  integer ncf_istag     !
  integer ncf_cproj     !  type of projection (CAMx codes)
  integer ncf_gdtyp     !  tyoe of projection (IOAPI codes)
  real    ncf_xcent     !  longitude pole
  real    ncf_ycent     !  latitude pole
  real    ncf_p_alp     !  tree latitude1
  real    ncf_p_bet     !  tree latitude2
  real    ncf_p_gam     !  same as longitude pole
  integer ncf_nlays     !  number of layers
  integer ncf_nrows     !  number of rows
  integer ncf_ncols     !  number of columns
  integer ncf_nthik     !  number of boundary layer cells
  integer ncf_nvars     !  number of variables
  integer ncf_nsteps    !  number of variables

!-----------------------------------------------------------------------
!  Global attributes for IO/API (file description), many are omitted for SCICHEM
!-----------------------------------------------------------------------
!
!   ncf_cdate              -- date of file creation (YYYYJJJ)
!   ncf_ctime              -- time of file creation
!   ncf_wdate              -- date of file (YYYYJJJ)
!   ncf_wtime              -- time of file
!   ncf_name               -- file type
!   ncf_note               -- simulation note
!   ncf_itzon              -- time zone of simulation
!   ncf_ftype              -- IO-API file type (always 1 = CUSTOM3)
!   ncf_vgtyp              -- IO-API grid type (always 6 = H: m above ground)
!   ncf_vgtop              -- IO-API grid tio for sigma coordinates (always -9999 = missing)
!   ncf_vglvls             -- IO-API levels
!   ncf_gdnam              -- IO-API grid name (always vX.YY)
!   ncf_upnam              -- (IO-API last program writing this file (always vX.YY)
!   ncf_filedesc           -- file tyoe (same as ncf_name)
!   ncf_sdate              -- date of simulation start (YYYYJJJ)
!   ncf_stime              -- time of simulation start
!   ncf_tstep              -- time step (hours times HHMMSS)
!   ncf_grid_id            -- grid ID - not used in SCICHEM
!   ncf_i_grid_start       -- I-cell index of start of grid in master
!   ncf_i_grid_end         -- I-cell index of dns of grid in master
!   ncf_j_grid_start       -- J-cell index of start of grid in master
!   ncf_j_grid_end         -- J-cell index of dns of grid in master
!   ncf_grid_mesh_factor   -- meshing factor for grid in master
!   ncf_flexi_nest         -- flexi-nesting flag (0 = false, 1 = true)
!   ncf_advection          -- CAMx advection scheme
!   ncf_chem_solver        -- name of CAMx chemical mechanism
!   ncf_pig                -- CAMx PiG flag
!   ncf_probing_tool       --  CAMx probing tools flag
!   ncf_chemistry          -- CAMx chemistry flag (0 = false, 1 = true)
!   ncf_total_species      -- total species in mechanism
!   ncf_radical_species    -- number of radical species in mechanism
!   ncf_gas_species        -- number of gas species in mechanism
!   ncf_pm_species         -- number of PM species in mechanism
!   ncf_reactions          -- number of reactions in mechanism
!   ncf_drydep             -- CAMx dryp deposition flag
!   ncf_wetdep             -- CAMx wet deposition flag (0 = false, 1 = true)
!   ncf_acm2               -- CAMx ACM2 option (0 = false, 1 = true)
!   ncf_cig_model          -- CAMx cloud in grid option (0 = false, 1 = true)
!   ncf_surface_model      -- chemical surface model flag (0 = false, 1 = true)
!   ncf_inline_ix_emiss    -- inline emissions for halogens flag (0 = false, 1 = true)
!   ncf_super_stepping     -- super stepping flag (0 = false, 1 = true)
!   ncf_gridded_emiss      -- flag to indicate if gridded emissions are used (0 = false, 1 = true)
!   ncf_point_emiss        --  flag to indicate if point source emissions are used (0 = false, 1 = true)
!   ncf_ignore_emiss_dates -- flag to ignore dates on emissions files (0 = false, 1 = true)
!   ncf_output_3d          -- flag to output 3D concentrations (0 = false, 1 = true)
!   ncf_pig_sample_grid    -- flag to use PiG sampling grid (0 = false, 1 = true)
!   ncf_pig_sample_bckgnd  -- flag to use grid concs in PiG sampleing grid (0 = false, 1 = true)
!   ncf_pig_sample_grid_id -- ID of CAMx grid containing PiG sampling grid
!   ncf_i_sample_start     -- I-cell index of start of PiG sampling grid
!   ncf_i_sample_end       -- I-cell index of end of PiG sampling grid
!   ncf_j_sample_start     -- J-cell index of start of PiG sampling grid
!   ncf_j_sample_end       -- J-cell index of end of PiG sampling grid
!   ncf_sample_mesh_factor -- meshing factor of PiG sampling grid
!   ncf_conventions        -- NetCDF convnetions in this file
!   ncf_history            -- history of file access
!
  integer       ncf_cdate
  integer       ncf_ctime
  integer       ncf_wdate
  integer       ncf_wtime
  character*10  ncf_name
  character*60  ncf_note
  integer       ncf_itzon
  integer       ncf_ftype
  integer       ncf_vgtyp
  real          ncf_vgtop
  real          ncf_vglvls(MXLAYER+1)
  character*16  ncf_gdnam
  character*16  ncf_upnam
  character*10  ncf_filedesc
  integer       ncf_sdate
  integer       ncf_stime
  integer       ncf_tstep
  integer       ncf_i_grid_start ! not sure if these are required, add them anyway
  integer       ncf_i_grid_end
  integer       ncf_j_grid_start
  integer       ncf_j_grid_end

! The following make sense in CAMx, but are not used in sciDOSpost output:

  ! integer       ncf_grid_id
  ! integer       ncf_grid_mesh_factor
  ! integer       ncf_flexi_nest
  ! character*10  ncf_advection
  ! character*10  ncf_chem_solver
  ! character*10  ncf_pig
  ! character*10  ncf_probing_tool
  ! character*140 ncf_chemistry
  ! integer       ncf_total_species
  ! integer       ncf_radical_species
  ! integer       ncf_gas_species
  ! integer       ncf_pm_species
  ! integer       ncf_reactions
  ! character*10  ncf_drydep
  ! integer       ncf_wetdep
  ! integer       ncf_acm2
  ! integer       ncf_cig_model
  ! integer       ncf_surface_model
  ! integer       ncf_inline_ix_emiss
  ! integer       ncf_super_stepping
  ! integer       ncf_gridded_emiss
  ! integer       ncf_point_emiss
  ! integer       ncf_ignore_emiss_dates
  ! integer       ncf_output_3d
  ! integer       ncf_pig_sample_grid
  ! integer       ncf_pig_sample_bckgnd
  ! integer       ncf_pig_sample_grid_id
  ! integer       ncf_i_sample_start
  ! integer       ncf_i_sample_end
  ! integer       ncf_j_sample_start
  ! integer       ncf_j_sample_end
  ! integer       ncf_sample_mesh_factor
  ! character*10  ncf_conventions
  character*60  ncf_history

!-----------------------------------------------------------------------
!  Variables for chunking parameters:
!-----------------------------------------------------------------------
!
!   NCF_CACHESIZE         -- total size of the raw data chunk cache in MegaBytes
!   NCF_NELEMS            -- number slots in the per-variable chunk cache
!        (should be a prime number larger than the number of chunks in the cache)
!   NCF_PREEMPTION        -- indicates how much chunks that have been fully read
!        are favored for preemption (0 means fully read chunks are treated no
!        differently than other chunks, while a value of 100 means fully read
!        chunks are always preempted before other chunks
!   NCF_SHUFFLE           -- If non-zero, turn on the shuffle filter
!   NCF_DEFLATE           -- If non-zero, turn on the deflate filter at the
!        level specified by the deflate_level parameter
!   NCF_DEFLATE_LEVEL     -- 0 = no deflate and 9 = slowest, but best deflate
!   NCF_CHUNK_SIZE_VAR_X  -- number of chunks in X direction
!   NCF_CHUNK_SIZE_VAR_Y  -- number of chunks in Y direction
!
  integer NCF_CACHESIZE
  integer NCF_NELEMS
  integer NCF_PREEMPTION
  integer NCF_SHUFFLE
  integer NCF_DEFLATE
  integer NCF_DEFLATE_LEVEL
  integer NCF_CHUNK_SIZE_VAR_X
  integer NCF_CHUNK_SIZE_VAR_Y

  parameter( NCF_CACHESIZE        = 32 )
  parameter( NCF_NELEMS           = 37 )
  parameter( NCF_PREEMPTION       = 100 )
  parameter( NCF_SHUFFLE          = 0 )
  parameter( NCF_DEFLATE          = 1 )
  parameter( NCF_DEFLATE_LEVEL    = 2 )
  parameter( NCF_CHUNK_SIZE_VAR_X = 2 )
  parameter( NCF_CHUNK_SIZE_VAR_Y = 2 )

contains
!***********************************************************************
!
!**** NCF_SET_COMPRESS_FLAG
!
!***********************************************************************
  subroutine ncf_set_compress_flag()
!
!     This routine just sets the compression flag for NetCDF based on the
!     the compiler directive. It needs to be a F90 routine so the preprocssor
!     directives can be used.
!
!     Modifications:
!        none
!
!     Input arguments:
!        none
!
!     Output arguments:
!        none
!
!     Routines called:
!        none
!
!
!     Called by:
!        READNML
!
    implicit none

! FIXME: turn on NETCDF4 options for compression
!#ifndef CHUNK
!    ncf_compress = .FALSE.
!#endif
    ncf_compress = .FALSE. ! FIXME: pasted to avoid SegFault

    return
  end subroutine ncf_set_compress_flag
!***********************************************************************
!
!**** NCF_SET_CACHE
!
!***********************************************************************
  subroutine ncf_set_cache()

    implicit none
!
!-----------------------------------------------------------------------
!    Description:
!-----------------------------------------------------------------------
!
!   This routine sets the cache parameters that will control how
!   the NetCDF output files will be chunked.
!
!      Argument description:
!       Inputs:
!       Outputs:
!
!-----------------------------------------------------------------------
!    LOG:
!-----------------------------------------------------------------------
!
!     04/20/17   --gwilson--    Original development
!     08/10/18   --bbrashers--  Adapted for sciDOSpost
!
!-----------------------------------------------------------------------
!    Argument declarations:
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!    External functions:
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!    Local variables:
!-----------------------------------------------------------------------
!
!   primes I array of prime numbers
!
    integer primes(168)
    logical lfound_prime
    integer ncolmax, nrowmax, i
!
!-----------------------------------------------------------------------
!    Data Statements:
!-----------------------------------------------------------------------
!
    data primes /2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,   &
         71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,          &
         149,151,157,163,167,173,179,181,191,193,197,199,211,            &
         223,227,229,233,239,241,251,257,263,269,271,277,281,            &
         283,293,307,311,313,317,331,337,347,349,353,359,367,            &
         373,379,383,389,397,401,409,419,421,431,433,439,443,            &
         449,457,461,463,467,479,487,491,499,503,509,521,523,            &
         541,547,557,563,569,571,577,587,593,599,601,607,613,            &
         617,619,631,641,643,647,653,659,661,673,677,683,691,            &
         701,709,719,727,733,739,743,751,757,761,769,773,787,            &
         797,809,811,821,823,827,829,839,853,857,859,863,877,            &
         881,883,887,907,911,919,929,937,941,947,953,967,971,            &
         977,983,991,997/
!
!-----------------------------------------------------------------------
!    Entry point:
!-----------------------------------------------------------------------
!
    if ( NCF_CACHESIZE .LE. 0 ) then
       write(*,'(//,A)') 'ERROR in NCF_IODAT block'
       write(*,'(A)') 'Invalid value provided for parameter NCF_CACHESIZE.'
       write(*,'(2A)') 'Value represents total size of the raw data ', &
            'chunk cache in MegaBytes.'
       write(*,'(A)') 'It must be a postive number. Stopping.'
       stop
    endif

    lfound_prime = .FALSE.
    do i=1,166
       if( NCF_NELEMS .EQ. primes(i) ) lfound_prime = .TRUE.
    enddo
    if( NCF_NELEMS .LE. 0 .OR. .NOT. lfound_prime ) then
       write(*,'(//,A)') 'ERROR in NCF_IODAT.INC:'
       write(*,'(A)') 'Invalid value provided for parameter NCF_NELEMS.'
       write(*,'(2A)') 'This value must be a prime number.'
       stop
    endif

    if( NCF_PREEMPTION .LT. 0 .OR. NCF_PREEMPTION .GT. 100 ) then
       write(*,'(//,A)') 'ERROR in NCF_IODAT.INC:'
       write(*,'(A)') 'Invalid value provided for parameter NCF_PREEMPTION.'
       write(*,'(A)') 'This value must be between 0 and 100.'
       stop
    endif

    if( NCF_DEFLATE_LEVEL .LT. 0 .OR.  NCF_DEFLATE_LEVEL .GT. 9 ) then
       write(*,'(//,A)') 'ERROR in NCF_IODAT.INC:'
       write(*,'(A)') 'Invalid value provided for parameter NCF_DEFLATE_LEVEL.'
       write(*,'(A)') 'This value must be between 0 and 9.'
       stop
    endif

    return
  end subroutine ncf_set_cache
!***********************************************************************
!
!**** NCF_SET_GLOBAL, is in mod_sciDOSpost_tools.f90, to "see" Rset().
!
!***********************************************************************
!***********************************************************************
!
!**** NCF_SET_VARS_BASE
!
!***********************************************************************
  subroutine ncf_set_vars_base

    implicit none
!
!-----------------------------------------------------------------------
!    Description:
!-----------------------------------------------------------------------
!
!   This routine sets the variable definitions and descriptions to
!   the NetCDF file
!
!      Argument description:
!       Inputs:
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
!-----------------------------------------------------------------------
!    Entry point:
!-----------------------------------------------------------------------
!
    ncf_x_units = "km"
    ncf_x_long_name = "X coordinate"
    ncf_x_var_desc = "X cartesian distance from projection origin"
    ncf_y_units = "km"
    ncf_y_long_name = "Y coordinate"
    ncf_y_var_desc = "Y cartesian distance from projection origin"
    ncf_layer_units = "Layer index"
    ncf_layer_long_name = "Model layer"
    ncf_layer_var_desc = "Model layer"
    ncf_longitude_units = "Degrees east"
    ncf_longitude_long_name = "Longitude"
    ncf_longitude_var_desc = "Longitude degrees east"
    ncf_longitude_coordinates = "latitude longitude"
    ncf_latitude_units = "Degrees north"
    ncf_latitude_long_name = "Latitude"
    ncf_latitude_var_desc = "Latitude degrees north"
    ncf_latitude_coordinates = "latitude longitude"
    ncf_topo_units = "m MSL"
    ncf_topo_long_name = "topographic elevation"
    ncf_topo_var_desc = "topographic elevation m above sea level"
    ncf_topo_coordinates = "latitude longitude"
    ncf_z_units = "m"
    ncf_z_long_name = "Layer height"
    ncf_z_var_desc = "Layer interface heights AGL"
    ncf_z_coordinates = "latitude longitude"

    ncf_tflag_units = "YYYYDDD,HHMMSS"
    ncf_tflag_long_name = "Start time flag"
    ncf_tflag_var_desc = "Timestep start date and time"
    ncf_etflag_units = "YYYYDDD,HHMMSS"
    ncf_etflag_long_name = "End time flag"
    ncf_etflag_var_desc = "Timestep end date and time"

    goto 9999
!-----------------------------------------------------------------------
!    Error messages:
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!    Return point:
!-----------------------------------------------------------------------
!
9999 continue
    return
  end subroutine ncf_set_vars_base
!***********************************************************************
!
!**** NCF_CREATEFILE
!
!***********************************************************************
  subroutine ncf_createfile(fname,action,iounit)
!
!-----------------------------------------------------------------------
!    Description:
!-----------------------------------------------------------------------
!
!   This routine opens a NetCDF file.
!
!      Argument description:
!       Inputs:
!           fname  C  name of file to open
!           action C  description of exactly what this call is doing
!       Outputs:
!           iounit I  NetCDF file ID of opened file
!
!-----------------------------------------------------------------------
!    LOG:
!-----------------------------------------------------------------------
!
!     02/20/17   --gwilson--    Original development
!     08/10/18   --bbrashers--  Adapted for sciDOSpost
!
!-----------------------------------------------------------------------
!    Include files:
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!    Argument declarations:
!-----------------------------------------------------------------------
!
    character*(*) fname
    character*(*) action
    integer       iounit
!
!-----------------------------------------------------------------------
!    External functions:
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!    Local variables:
!-----------------------------------------------------------------------
!
    integer ierr, ncf_format
!
!-----------------------------------------------------------------------
!    Entry point:
!-----------------------------------------------------------------------
!
    ncf_format = NF_FORMAT_CLASSIC
    if( ncf_compress ) ncf_format = OR(NF_NETCDF4,NF_FORMAT_CLASSIC)
    ierr = nc_create(AddNull(fname), ncf_format, iounit)
    if( ierr /= 0 ) then
       write(*,'(//,a)') 'ERROR in NCF_CREATEFILE:'
       write(*,'(A)')  trim(action)
       write(*,'(2A)') 'Could not open file: ', trim(fname)
       stop
    endif

! FIXME: turn on NETCDF4 options for compression
! #ifdef CHUNK
!     if( ncf_compress ) then
!        ierr = nf_set_chunk_cache(NCF_CACHESIZE, NCF_NELEMS, NCF_PREEMPTION)
!        if( ierr .NE. nf_noerr ) then
!           write(*,'(//,a)') 'ERROR in NCF_CREATEFILE:'
!           write(*,'(A)') trim(action)
!           write(*,'(2A)') 'Could not set chunk parameters for: ', trim(fname)
!           stop
!        endif
!     endif
! #endif
!
!-----------------------------------------------------------------------
!    Return point:
!-----------------------------------------------------------------------
!
    return
  end subroutine ncf_createfile
!***********************************************************************
!
!**** NCF_WRT_DIM
!
!***********************************************************************

  subroutine ncf_wrt_dim(action,iounit,numcols,numrows,nlays,nspcs)
!
!-----------------------------------------------------------------------
!    Description:
!-----------------------------------------------------------------------
!
!   This routine writes the dimensions to the NetCDF file
!
!      Argument description:
!       Inputs:
!           action  C  name of file to open
!           iounit  I  NetCDF file ID of file
!           numcols I number of cols in this file
!           numrows I number of cols in this file
!           nlays   I  number of layers in this file
!           nspcs   I  number of species in the file
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
    character*(*) action
    integer       iounit
    integer       numcols
    integer       numrows
    integer       nlays
    integer       nspcs
!
!-----------------------------------------------------------------------
!    Local variables:
!-----------------------------------------------------------------------
!
    integer ierr
!
!-----------------------------------------------------------------------
!    Entry point:
!-----------------------------------------------------------------------
!
    ncf_date_time = 2
    ncf_lay = nlays
    ncf_col = numcols
    ncf_row = numrows
    ncf_var = nspcs + NCF_BASE_VARS

    ierr = nc_def_dim(iounit, "TSTEP", NF_UNLIMITED, ncf_tstep_dimid )
    if( ierr /= 0 ) goto 7000

    ierr = nc_def_dim(iounit, "DATE-TIME", ncf_date_time, ncf_date_time_dimid )
    if( ierr /= 0 ) goto 7000

    ierr = nc_def_dim(iounit, "LAY", ncf_lay, ncf_lay_dimid )
    if( ierr /= 0 ) goto 7000

    ierr = nc_def_dim(iounit, "VAR", ncf_var, ncf_var_dimid )
    if( ierr /= 0 ) goto 7000

    ierr = nc_def_dim(iounit, "ROW", ncf_row, ncf_row_dimid )
    if( ierr /= 0 ) goto 7000

    ierr = nc_def_dim(iounit, "COL", ncf_col, ncf_col_dimid )
    if( ierr /= 0 ) goto 7000

    goto 9999
!
!-----------------------------------------------------------------------
!    Error messages:
!-----------------------------------------------------------------------
!
7000 continue
    write(*,'(//,a,i)') 'ERROR in NCF_WRT_DIM:',ierr
    write(*,'(A)') trim(action)
    write(*,'(A)') 'Cannot write dimensions to file. Stopping.'
    stop
!
!-----------------------------------------------------------------------
!    Return point:
!-----------------------------------------------------------------------
!
9999 continue
    return
  end subroutine ncf_wrt_dim

!***********************************************************************
!
!**** NCF_WRT_GLOBAL
!
!***********************************************************************
  subroutine ncf_wrt_global(action,iounit,nspcs,spcname)

    implicit none
!
!-----------------------------------------------------------------------
!    Description:
!-----------------------------------------------------------------------
!
!   This routine writes the Global attributes to the NetCDF file
!
!       Inputs:
!           action    C  name of file to open
!           iounit    I  NetCDF file ID of file
!           nspcs     I  numver of species in file
!           spcname   C name of each species
!       Outputs:
!-----------------------------------------------------------------------
!    LOG:
!-----------------------------------------------------------------------
!
!     02/20/17   --gwilson--    Original development
!     08/10/18   --bbrashers--  Adapted for sciDOSpost
!
!-----------------------------------------------------------------------
!    Include files:
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!    Argument declarations:
!-----------------------------------------------------------------------
!
    character*(*) action
    integer       iounit
    integer       nspcs
    character*(*) spcname(nspcs)
!-----------------------------------------------------------------------
!    External functions:
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!    Local variables:
!-----------------------------------------------------------------------
!
    character*80                 ioapi, exec_id
    character*((MXSPEC*4)*16)    string
    character*16                 varname(MXSPEC*4)
    integer                      ierr, i
    integer      nlays

!    data varname(1:NCF_BASE_VARS)  &
!         /'X ','Y ', 'layer ','TFLAG ', 'ETFLAG ','longitude ', &
!         'latitude ','topo ', 'z '/
!-----------------------------------------------------------------------
!    Entry point:
!-----------------------------------------------------------------------
!
    nlays = 1 ! DOS/ADOS/DEP files are surface-level only

! Note: the (/ and /) around the last passed variable are to pass an array. This is
! because the ifort compiler is picky - most other compilers would work just fine.

    ierr = nc_put_att_int(iounit, NF_GLOBAL, AddNull('FTYPE'), NF_INT, 1, (/ncf_ftype/))
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_int(iounit, NF_GLOBAL, AddNull('CDATE'), NF_INT, 1, (/ncf_cdate/))
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_int(iounit, NF_GLOBAL, AddNull('CTIME'), NF_INT, 1, (/ncf_ctime/))
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_int(iounit, NF_GLOBAL, AddNull('WDATE'), NF_INT, 1, (/ncf_wdate/))
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_int(iounit, NF_GLOBAL, AddNull('WTIME'), NF_INT, 1, (/ncf_wtime/))
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_int(iounit, NF_GLOBAL, AddNull('SDATE'), NF_INT, 1, (/ncf_sdate/))
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_int(iounit, NF_GLOBAL, AddNull('STIME'), NF_INT, 1, (/ncf_stime/))
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_int(iounit, NF_GLOBAL, AddNull('TSTEP'), NF_INT, 1, (/ncf_tstep/))
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_int(iounit, NF_GLOBAL, AddNull('NTHIK'), NF_INT, 1, (/ncf_nthik/))
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_int(iounit, NF_GLOBAL, AddNull('NCOLS'), NF_INT, 1, (/ncf_ncols/))
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_int(iounit, NF_GLOBAL, AddNull('NROWS'), NF_INT, 1, (/ncf_nrows/))
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_int(iounit, NF_GLOBAL, AddNull('NLAYS'), NF_INT, 1, (/ncf_nlays/))
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_int(iounit, NF_GLOBAL, AddNull('NVARS'), NF_INT, 1, (/ncf_nvars/))
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_int(iounit, NF_GLOBAL, AddNull('GDTYP'), NF_INT, 1, (/ncf_gdtyp/))
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_double(iounit, NF_GLOBAL, AddNull('P_ALP'), NF_DOUBLE, &
         1, (/DBLE(ncf_p_alp)/) )
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_double(iounit, NF_GLOBAL, AddNull('P_BET'), NF_DOUBLE, &
         1, (/DBLE(ncf_p_bet)/) )
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_double(iounit, NF_GLOBAL, AddNull('P_GAM'), NF_DOUBLE, &
         1, (/DBLE(ncf_p_gam)/) )
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_double(iounit, NF_GLOBAL, AddNull('XCENT'), NF_DOUBLE, &
         1, (/DBLE(ncf_xcent)/) )
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_double(iounit, NF_GLOBAL, AddNull('YCENT'), NF_DOUBLE, &
         1, (/DBLE(ncf_ycent)/) )
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_double(iounit, NF_GLOBAL, AddNull('XORIG'), NF_DOUBLE, &
         1, (/DBLE(ncf_xorig)/) )
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_double(iounit, NF_GLOBAL, AddNull('YORIG'), NF_DOUBLE, &
         1, (/DBLE(ncf_yorig)/) )
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_double(iounit, NF_GLOBAL, AddNull('XCELL'), NF_DOUBLE, &
         1, (/DBLE(ncf_xcell)/) )
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_double(iounit, NF_GLOBAL, AddNull('YCELL'), NF_DOUBLE, &
         1, (/DBLE(ncf_ycell)/) )
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_int( iounit, NF_GLOBAL, AddNull('VGTYP'), 1, NF_INT, (/ncf_vgtyp/))
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_float(iounit, NF_GLOBAL, AddNull('VGTOP'), 1, NF_FLOAT, (/ncf_vgtop/))
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_float( iounit, NF_GLOBAL, AddNull('VGLVLS'), nlays+1, NF_INT, &
         ncf_vglvls )
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_text(iounit, NF_GLOBAL, AddNull('GDNAM'), &
         len_trim(ncf_gdnam), AddNull(trim(ncf_gdnam)) )
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_text(iounit, NF_GLOBAL, AddNull('UPNAM'), &
         len_trim(ncf_upnam), AddNull(trim(ncf_upnam)) )
    if( ierr /= 0 ) goto 7000

    do i=1,nspcs
       varname(i) = spcname(i)
    enddo
    string = varname(1)
    do i=2,nspcs
       string = string(1:(i-1)*16) // trim(varname(i))
    enddo
    ierr = nc_put_att_text(iounit, NF_GLOBAL, AddNull('VAR-LIST'), &
         len_trim(string), trim(string) )
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_text(iounit, NF_GLOBAL, AddNull('FILEDESC'), &
         len_trim(ncf_filedesc), AddNull(trim(ncf_filedesc)))
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_text(iounit, NF_GLOBAL, AddNull('HISTORY'), &
         len_trim(ncf_history), AddNull(trim(ncf_history)))
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_int(iounit, NF_GLOBAL, AddNull('IUTM'), NF_INT, 1, (/ncf_iutm/))
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_int(iounit, NF_GLOBAL, AddNull('ISTAG'), NF_INT, 1, (/ncf_istag/))
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_int(iounit, NF_GLOBAL, AddNull('CPROJ'), NF_INT, 1, (/ncf_cproj/))
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_int(iounit, NF_GLOBAL, AddNull('NSTEPS'), NF_INT, 1, (/ncf_nsteps/))
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_text(iounit, NF_GLOBAL, AddNull('NAME'), &
         len_trim(ncf_name), ncf_name(:len_trim(ncf_name)) )
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_text(iounit, NF_GLOBAL, AddNull('NOTE'), &
         len_trim(ncf_note), AddNull(trim(ncf_note)) )
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_int(iounit, NF_GLOBAL, AddNull('ITZON'), NF_INT, 1, (/ncf_itzon/))
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_text(iounit, NF_GLOBAL, AddNull('UPDSC'), &
         len_trim(ncf_upnam), AddNull(trim(ncf_upnam)) )
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_int(iounit, NF_GLOBAL, AddNull('I_GRID_START'), NF_INT, &
         1, (/ncf_i_grid_start/))
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_int(iounit, NF_GLOBAL, AddNull('I_GRID_END'), NF_INT, &
         1, (/ncf_i_grid_end/))
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_int(iounit, NF_GLOBAL, AddNull('J_GRID_START'), NF_INT, &
         1, (/ncf_j_grid_start/))
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_int(iounit, NF_GLOBAL, AddNull('J_GRID_END'), NF_INT, &
         1, (/ncf_j_grid_end/))
    if( ierr /= 0 ) goto 7000

    goto 9999
!-----------------------------------------------------------------------
!    Error messages:
!-----------------------------------------------------------------------
!
7000 continue
    write(*,'(//,a,i)') 'ERROR in NCF_WRT_GLOBAL:',ierr
    write(*,'(A)')     trim(action)
    write(*,'(A)') 'Cannot write global attributes to file. Stopping.'
    stop
!-----------------------------------------------------------------------
!    Return point:
!-----------------------------------------------------------------------
!
9999 continue
    return
  end subroutine ncf_wrt_global
!***********************************************************************
!
!**** NCF_WRT_VARS_BASE
!
!***********************************************************************
  subroutine ncf_wrt_vars_base(action,iounit)
!
!-----------------------------------------------------------------------
!    Description:
!-----------------------------------------------------------------------
!
!   This routine writes the variable definitions and descriptions to
!    the NetCDF file
!
!       Inputs:
!           action  C  name of file to open
!           iounit  I  NetCDF file ID of file
!       Outputs:
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
    character*(*) action
    integer iounit
!
!-----------------------------------------------------------------------
!    Local variables:
!-----------------------------------------------------------------------
!
    integer ierr, grid_dimid(2), z_dimid(4), time_dimid(3), this_varid
!-----------------------------------------------------------------------
!    Entry point:
!-----------------------------------------------------------------------
!
    grid_dimid(1) = ncf_col_dimid
    grid_dimid(2) = ncf_row_dimid

    z_dimid(1) = ncf_col_dimid
    z_dimid(2) = ncf_row_dimid
    z_dimid(3) = ncf_lay_dimid
    z_dimid(4) = ncf_tstep_dimid

    time_dimid(1) = ncf_date_time_dimid
    time_dimid(2) = ncf_var_dimid
    time_dimid(3) = ncf_tstep_dimid

    ierr = nc_def_var(iounit, "X", NF_DOUBLE, 1, (/ncf_col_dimid/), this_varid )
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_text(iounit,this_varid,AddNull('units'), &
         len_trim(ncf_x_units),AddNull(trim(ncf_x_units)))
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_text(iounit,this_varid,AddNull('long_name'), &
         len_trim(ncf_x_long_name),AddNull(trim(ncf_x_long_name)))
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_text(iounit,this_varid,AddNull('var_desc'), &
         len_trim(ncf_x_var_desc),AddNull(trim(ncf_x_var_desc)))
    if( ierr /= 0 ) goto 7000

    ierr = nc_def_var(iounit, AddNull("Y"), NF_DOUBLE, 1, (/ncf_row_dimid/), &
         this_varid)
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_text(iounit,this_varid,AddNull('units'), &
         len_trim(ncf_y_units),AddNull(trim(ncf_y_units)))
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_text(iounit,this_varid,AddNull('long_name'), &
         len_trim(ncf_y_long_name),AddNull(trim(ncf_y_long_name)))
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_text(iounit,this_varid,AddNull('var_desc'), &
         len_trim(ncf_y_var_desc),AddNull(trim(ncf_y_var_desc)))
    if( ierr /= 0 ) goto 7000

    ierr = nc_def_var(iounit, AddNull("layer"), NF_DOUBLE, 1, (/ncf_lay_dimid/), this_varid)
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_text(iounit,this_varid,AddNull('units'), &
         len_trim(ncf_layer_units),AddNull(trim(ncf_layer_units)))
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_text(iounit,this_varid,AddNull('long_name'), &
         len_trim(ncf_layer_long_name),AddNull(trim(ncf_layer_long_name)))
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_text(iounit,this_varid,AddNull('var_desc'), &
         len_trim(ncf_layer_var_desc),AddNull(trim(ncf_layer_var_desc)))
    if( ierr /= 0 ) goto 7000

    ierr = nc_def_var(iounit, AddNull("TFLAG"), NF_INT, 3, (/time_dimid/), this_varid)
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_text(iounit,this_varid,AddNull('units'), &
         len_trim(ncf_tflag_units),AddNull(trim(ncf_tflag_units)) )
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_text(iounit,this_varid,AddNull('long_name'), &
         len_trim(ncf_tflag_long_name),AddNull(trim(ncf_tflag_long_name)))
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_text(iounit,this_varid,AddNull('var_desc'), &
         len_trim(ncf_tflag_var_desc),AddNull(trim(ncf_tflag_var_desc)))
    if( ierr /= 0 ) goto 7000

    ierr = nc_def_var(iounit, "ETFLAG", NF_INT, 3, (/time_dimid/), this_varid)
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_text(iounit,this_varid,AddNull('units'), &
         len_trim(ncf_etflag_units),AddNull(trim(ncf_etflag_units)))
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_text(iounit,this_varid,AddNull('long_name'), &
         len_trim(ncf_etflag_long_name),AddNull(trim(ncf_etflag_long_name)))
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_text(iounit,this_varid,AddNull('var_desc'), &
         len_trim(ncf_etflag_var_desc),AddNull(trim(ncf_etflag_var_desc)))
    if( ierr /= 0 ) goto 7000

    ierr = nc_def_var(iounit, "longitude", NF_DOUBLE, 2, (/grid_dimid/), this_varid)
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_text(iounit,this_varid,AddNull('units'), &
         len_trim(ncf_longitude_units),AddNull(trim(ncf_longitude_units)))
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_text(iounit,this_varid,AddNull('long_name'), &
         len_trim(ncf_longitude_long_name),AddNull(trim(ncf_longitude_long_name)))
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_text(iounit,this_varid,AddNull('var_desc'), &
         len_trim(ncf_longitude_var_desc),AddNull(trim(ncf_longitude_var_desc)))
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_text(iounit,this_varid,AddNull('coordinates'), &
         len_trim(ncf_longitude_coordinates),AddNull(trim(ncf_longitude_coordinates)))
    if( ierr /= 0 ) goto 7000

    ierr = nc_def_var(iounit, "latitude", NF_DOUBLE, 2, (/grid_dimid/), this_varid)
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_text(iounit,this_varid,AddNull('units'), &
         len_trim(ncf_latitude_units),AddNull(trim(ncf_latitude_units)))
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_text(iounit,this_varid,AddNull('long_name'), &
         len_trim(ncf_latitude_long_name),AddNull(trim(ncf_latitude_long_name)))
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_text(iounit,this_varid,AddNull('var_desc'), &
         len_trim(ncf_latitude_var_desc),AddNull(trim(ncf_latitude_var_desc)))
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_text(iounit,this_varid,AddNull('coordinates'), &
         len_trim(ncf_latitude_coordinates),AddNull(trim(ncf_latitude_coordinates)))
    if( ierr /= 0 ) goto 7000

    ierr = nc_def_var(iounit, "topo", NF_DOUBLE, 2, (/grid_dimid/), this_varid)
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_text(iounit,this_varid,AddNull('units'), &
         len_trim(ncf_topo_units),AddNull(trim(ncf_topo_units)))
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_text(iounit,this_varid,AddNull('long_name'), &
         len_trim(ncf_topo_long_name),AddNull(trim(ncf_topo_long_name)))
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_text(iounit,this_varid,AddNull('var_desc'), &
         len_trim(ncf_topo_var_desc),ncf_topo_var_desc)
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_text(iounit,this_varid,AddNull('coordinates'),  &
         len_trim(ncf_topo_coordinates),ncf_topo_coordinates)
    if( ierr /= 0 ) goto 7000

    ierr = nc_def_var(iounit, "z", NF_DOUBLE, 4, (/z_dimid/), this_varid)
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_text(iounit,this_varid,AddNull('units'),  &
         len_trim(ncf_z_units),ncf_z_units)
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_text(iounit,this_varid,AddNull('long_name'),  &
         len_trim(ncf_z_long_name),ncf_z_long_name)
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_text(iounit,this_varid,AddNull('var_desc'),  &
         len_trim(ncf_z_var_desc),ncf_z_var_desc)
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_att_text(iounit,this_varid,AddNull('coordinates'),  &
         len_trim(ncf_z_coordinates),ncf_z_coordinates)
    if( ierr /= 0 ) goto 7000
!
    goto 9999
!
!-----------------------------------------------------------------------
!    Error messages:
!-----------------------------------------------------------------------
!
7000 continue
    write(*,'(//,a)') 'ERROR in NCF_WRT_VARS_BASE:'
    write(*,'(A)') trim(action)
    write(*,'(A)') 'Cannot create file variable for grid variables. Stopping'
    stop
!
!-----------------------------------------------------------------------
!    Return point:
!-----------------------------------------------------------------------
!
9999 continue
    return
  end subroutine ncf_wrt_vars_base

!***********************************************************************
!
!**** NCF_WRT_VARS_SPECIES
!
!***********************************************************************

  subroutine ncf_wrt_vars_species(action,iounit,numcols,numrows,numlays, &
       nspcs,spnames,num_dims)

    implicit none
!
!-----------------------------------------------------------------------
!    Description:
!-----------------------------------------------------------------------
!
!   This routine writes the variable definitions and descriptions to
!    the NetCDF file
!
!      Argument description:
!       Inputs:
!           action   C  name of file to open
!           iounit   I  NetCDF file ID of file
!           numcols  I  number of columns in the grid
!           numrows  I  number of columns in the grid
!           numlays  I  number of columns in the grid
!           numcols  I  number of columns in the grid
!           nspcs    I  number of species in the file
!           spnames  C  name of the chemical species
!           num_dims I  number of dimensions in variable
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
    character*(*) action
    integer       iounit
    integer       numcols
    integer       numrows
    integer       numlays
    integer       nspcs
    character*(*) spnames
    integer       num_dims
!
!-----------------------------------------------------------------------
!    External functions:
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!    Local variables:
!-----------------------------------------------------------------------
!
    character*14 this_species
    integer      ierr, ispc, spec_dimid(4), spec_chunk(4), spec_varid
!
!-----------------------------------------------------------------------
!    Entry point:
!-----------------------------------------------------------------------
!
    spec_chunk(1) = INT(REAL(numcols)/NCF_CHUNK_SIZE_VAR_X)
    if( spec_chunk(1) .GT. numcols ) goto 7002
    spec_chunk(2) = INT(REAL(numrows)/NCF_CHUNK_SIZE_VAR_Y)
    if( spec_chunk(2) .GT. numrows ) goto 7003
    spec_chunk(3) = MAX(1,INT(numlays/2))
    spec_chunk(4) = 1

    spec_dimid(1) = ncf_col_dimid
    spec_dimid(2) = ncf_row_dimid
    spec_dimid(3) = ncf_lay_dimid
    spec_dimid(4) = ncf_tstep_dimid

    if( num_dims .EQ.  3) then
       spec_dimid(3) = ncf_tstep_dimid
       spec_chunk(3) = 1
    endif
!
!  --- define everything for this species ---
!
    do ispc=1,nspcs
       this_species            = trim(spnames) ! FIXME handle multiple species
       ncf_species_units       = spec_units(ispc)
       ncf_species_long_name   = spec_long_name(ispc)
       ncf_species_var_desc    = spec_desc(ispc)
       ncf_species_coordinates = spec_coords(ispc)
!
!  --- define the variable ---
!
       ierr = nc_def_var(iounit,AddNull(trim(this_species)), NF_FLOAT, num_dims, &
            spec_dimid, spec_varid)
       if( ierr /= 0 ) goto 7000
!
!  --- add the attributes ---
!
       ierr = nc_put_att_text(iounit,spec_varid,AddNull('long_name'), &
            len_trim(ncf_species_long_name),AddNull(ncf_species_long_name))
       if( ierr /= 0 ) goto 7000

       ierr = nc_put_att_text(iounit,spec_varid,AddNull('units'), &
                        len_trim(ncf_species_units),AddNull(ncf_species_units))
       if( ierr /= 0 ) goto 7000

       ierr = nc_put_att_text(iounit,spec_varid,AddNull('var_desc'), &
            len_trim(ncf_species_var_desc),AddNull(ncf_species_var_desc))
       if( ierr /= 0 ) goto 7000

       ierr = nc_put_att_text(iounit,spec_varid,AddNull('coordinates'), &
            len_trim(ncf_species_coordinates),AddNull(Trim(ncf_species_coordinates)))
       if( ierr /= 0 ) goto 7000
!
!  --- chunkers go here ... Irie! ----
!
! FIXME: turn on NETCDF4 options for compression
! #ifdef CHUNK
!        if( ncf_compress ) then
!           ierr = nc_def_var_chunking(iounit, spec_varid, &
!                NF_CHUNKED, spec_chunk)
!           if( ierr /= 0 ) goto 7001

!           ierr = nc_def_var_deflate(iounit, spec_varid, NCF_SHUFFLE,  &
!                NCF_DEFLATE, NCF_DEFLATE_LEVEL )
!           if( ierr /= 0 ) goto 7001
!        endif
! #endif
!
    enddo
!
    goto 9999
!
!-----------------------------------------------------------------------
!    Error messages:
!-----------------------------------------------------------------------
!
7000 continue
    write(*,'(//,A)') 'ERROR in NCF_WRT_VARS_SPECIES:'
    write(*,'(A)') trim(action)
    write(*,'(2A)') 'Cannot create file variable for species: ', &
         trim(this_species)
    stop

7001 continue
    write(*,'(//,A)') 'ERROR in NCF_WRT_VARS_SPECIES:'
    write(*,'(A)') trim(action)
    write(*,'(2A)') 'Cannot set chunk parameters for species: ', &
         trim(this_species)
    stop

7002 continue
    write(*,'(//,A)') 'ERROR in NCF_IODAT.INC:'
    write(*,'(A)') trim(action)
    write(*,'(A)') 'Cannot set chunk parameters for this file.'
    write(*,'(2A,/,A)') 'The NCF_CHUNK_SIZE_VAR_X parameter causes the ', &
         'chunk value to be larger','than the number of columns.'
    write(*,'(A,I3,A,I4)') 'Number of columns: ',numcols
    stop

7003 continue
    write(*,'(//,A)') 'ERROR in NCF_IODAT.INC:'
    write(*,'(A)') trim(action)
    write(*,'(A)') 'Cannot set chunk parameters for this file.'
    write(*,'(2A,/,A)') 'The NCF_CHUNK_SIZE_VAR_Y parameter causes the ', &
         'chunk value to be larger','than the number of rows.'
    write(*,'(A,I3,A,I4)') 'Number of rows: ',numrows
    stop
!
!-----------------------------------------------------------------------
!    Return point:
!-----------------------------------------------------------------------
!
9999 continue
    return
  end subroutine ncf_wrt_vars_species

!***********************************************************************
!**** NCF_ENDDEF_FILE
!***********************************************************************

  subroutine ncf_enddef_file(action,iounit)

!-----------------------------------------------------------------------
!    Description:
!-----------------------------------------------------------------------
!
!   This routine ends the definition mode of a NetCDF file
!
!      Argument description:
!       Inputs:
!           action  C  name of file to open
!           iounit  I  NetCDF file ID of file
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
!    Include files:
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!    Argument declarations:
!-----------------------------------------------------------------------
!
    character*(*) action
    integer       iounit
!
!-----------------------------------------------------------------------
!    External functions:
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!    Local variables:
!-----------------------------------------------------------------------
!
    integer ierr
!
!-----------------------------------------------------------------------
!    Entry point:
!-----------------------------------------------------------------------
!
    ierr = nc_enddef(iounit)
    if( ierr /= 0 ) goto 7000
!
    goto 9999
!
!-----------------------------------------------------------------------
!    Error messages:
!-----------------------------------------------------------------------
!
7000 continue
    write(*,'(//,a)') 'ERROR in NCF_ENDDEF_FILE:'
    write(*,'(A)') trim(action)
    write(*,'(A)') 'Cannot terminate definition mode. Stopping.'
    stop
!
!-----------------------------------------------------------------------
!    Return point:
!-----------------------------------------------------------------------
!
9999 continue
    return
  end subroutine ncf_enddef_file

!***********************************************************************
!**** NCF_WRT_DATA_GRID
!***********************************************************************

  subroutine ncf_wrt_data_grid(action,iounit,grid_ncol,grid_nrow, &
       grid_lat,grid_lon,grid_x,grid_y,grid_topo)

    implicit none
!
!-----------------------------------------------------------------------
!    Description:
!-----------------------------------------------------------------------
!
!   This routine writes the variable data for the grid definiton
!    variables to the NetCDF file
!
!      Argument description:
!       Inputs:
!           action      C name of file to open
!           iounit      I NetCDF file ID of file
!           llatlon     L whether output grid is LATLON or not
!           grid_ncol   I number of grid cells in X direction
!           grid_nrow   I number of grid cells in Y direction
!           grid_lat    R latitude value of cell center
!           grid_lon    R longitude value of cell center
!           grid_topo   R height above sea level of cell center
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
!    Include files:
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!    Argument declarations:
!-----------------------------------------------------------------------
!
    character*(*) action
    integer       iounit
    integer       grid_ncol
    integer       grid_nrow
    real          grid_lat(grid_ncol,grid_nrow)
    real          grid_lon(grid_ncol,grid_nrow)
    real          grid_X(grid_ncol,grid_nrow)
    real          grid_Y(grid_ncol,grid_nrow)
    real          grid_topo(grid_ncol,grid_nrow)
!
!-----------------------------------------------------------------------
!    External functions:
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!    Local variables:
!-----------------------------------------------------------------------
!
    character*200 this_var
    real*8,       allocatable, dimension(:)   :: darray_1d
    real*8,       allocatable, dimension(:,:) :: darray_2d
    real          factor
    integer,      allocatable, dimension(:)   :: iarray_1d
    integer       this_varid, icol, irow, ilay, ierr, nlays
!
!-----------------------------------------------------------------------
!    Entry point:
!-----------------------------------------------------------------------
!
!  --- variable for X coordinates ---
!
!    factor = 1000.
!    if( llatlon ) factor = 1.0
    this_var = "X"
    allocate( darray_1d(grid_ncol) )
    do icol=1,grid_ncol
! We already calculated this in a much more prescise way...
!       darray_1d(icol) = DBLE((grid_xorig + (REAL(icol)-0.5)*grid_deltax)/factor)
       darray_1d(icol) = DBLE( grid_X(icol,1) )
    enddo
    ierr = nc_inq_varid(iounit,AddNull(trim(this_var)),this_varid)
    if( ierr /= 0 ) goto 7000
    ierr = nc_put_var_double(iounit,this_varid,darray_1d)
    if( ierr /= 0 ) goto 7001
    deallocate( darray_1d )
!
!  --- variable for Y coordinates ---
!
    this_var = "Y"
    allocate( darray_1d(grid_nrow) )
    do irow=1,grid_nrow
! We already calculated this in a much more prescise way...
!       darray_1d(irow) = DBLE((grid_yorig + (REAL(irow)-0.5)*grid_deltay)/factor)
       darray_1d(irow) = DBLE( grid_Y(1,irow) )
    enddo
    ierr = nc_inq_varid(iounit,AddNull(trim(this_var)),this_varid)
    if( ierr /= 0 ) goto 7000
    ierr = nc_put_var_double(iounit,this_varid,darray_1d)
    if( ierr /= 0 ) goto 7001
    deallocate( darray_1d )
!
!  --- variable for Layer heights ---
!
    nlays = 1
    this_var = "layer"
    allocate( iarray_1d(nlays) )
    do ilay=1,nlays
       iarray_1d(ilay) = ilay
    enddo
    ierr = nc_inq_varid(iounit,AddNull(trim(this_var)),this_varid)
    if( ierr /= 0 ) goto 7000
    ierr = nc_put_var_int(iounit,this_varid,iarray_1d)
    if( ierr /= 0 ) goto 7001
    deallocate( iarray_1d )
!
!  --- variable for longitude coordinates ---
!
    allocate( darray_2d(grid_ncol,grid_nrow) )

    this_var = "longitude"
    do icol=1,grid_ncol
       do irow=1,grid_nrow
          darray_2d(icol,irow) = DBLE(grid_lon(icol,irow))
       enddo
    enddo
    ierr = nc_inq_varid(iounit,AddNull(trim(this_var)),this_varid)
    if( ierr /= 0 ) goto 7000
    ierr = nc_put_var_double(iounit,this_varid,darray_2d)
    if( ierr /= 0 ) goto 7001
!
!  --- variable for latitude coordinates ---
!
    this_var = "latitude"
    do icol=1,grid_ncol
       do irow=1,grid_nrow
          darray_2d(icol,irow) = DBLE(grid_lat(icol,irow))
       enddo
    enddo
    ierr = nc_inq_varid(iounit,AddNull(trim(this_var)),this_varid)
    if( ierr /= 0 ) goto 7000
    ierr = nc_put_var_double(iounit,this_varid,darray_2d)
    if( ierr /= 0 ) goto 7001
!
!  --- variable for topo ---
!
    this_var = "topo"
    do icol=1,grid_ncol
       do irow=1,grid_nrow
          darray_2d(icol,irow) = DBLE(grid_topo(icol,irow))
       enddo
    enddo
    ierr = nc_inq_varid(iounit,AddNull(trim(this_var)),this_varid)
    if( ierr /= 0 ) goto 7000
    ierr = nc_put_var_double(iounit,this_varid,darray_2d)
    if( ierr /= 0 ) goto 7001
    deallocate( darray_2d )

    goto 9999
!
!-----------------------------------------------------------------------
!    Error messages:
!-----------------------------------------------------------------------
!
7000 continue
    write(*,'(//,a)') 'ERROR in NCF_WRT_DATA_GRID:'
    write(*,'(A)') trim(action)
    write(*,'(2A)') 'Cannot find variable id for: ',trim(this_var)
    stop
!
7001 continue
    write(*,'(//,a)') 'ERROR in NCF_WRT_DATA_GRID:'
    write(*,'(A)') trim(action)
    write(*,'(2A)') 'Cannot write data for the variable: ',trim(this_var)
    stop
!
!-----------------------------------------------------------------------
!    Return point:
!-----------------------------------------------------------------------
!
9999 continue
    return
  end subroutine ncf_wrt_data_grid
!
!***********************************************************************
!**** NCF_WRT_DATA_TSTEP
!***********************************************************************
!
  subroutine ncf_wrt_data_tstep(action,iounit,nspcs,ncf_cur_tstep,iymdh,Period)

    use basic_fd

    implicit none
!
!-----------------------------------------------------------------------
!    Description:
!-----------------------------------------------------------------------
!
!   This routine writes the data for the timestep variables to the
!    NetCDF file
!
!      Argument description:
!       Inputs:
!           action      C name of file to open
!           iounit      I NetCDF file ID of file
!           nspcs       I number of species in the file
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
!    Include files:
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!    Argument declarations:
!-----------------------------------------------------------------------
!
    character*(*) action
    integer       iounit
    integer       nspcs
    integer       ncf_cur_tstep
    integer       iymdh
    integer       Period
!
!-----------------------------------------------------------------------
!    External functions:
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!    Local variables:
!-----------------------------------------------------------------------
!
    character*200 this_var
    integer,      allocatable, dimension(:,:) :: iarray_2d
    integer       this_varid, ispc, istep, ierr
!    integer       data_start(3), data_count(3)
    INTEGER(LEN_ADDRESS) :: data_start(3), data_count(3)
    integer       date_beg,time_beg, date_end,time_end, iy,im,id,ih,ijul
!
!-----------------------------------------------------------------------
!    Entry point:
!-----------------------------------------------------------------------
!

    data_start(1) = 1
    data_count(1) = 2
    data_start(2) = 1
    data_count(2) = nspcs + NCF_BASE_VARS
    data_start(3) = ncf_cur_tstep
    data_count(3) = 1
!
!  --- Values TFLAG/EFLAG aer caluclated: ending at iymdh, starting Period
!      hours before ending. Format is YYYYDDD,HHMMSS for both TFLAG and EFLAG.
!
    call idate2ymdh(iymdh,iy,im,id,ih)

    call dat2jul(iy,im,id,ijul)
    date_end = iy*1000 + ijul
    time_end = ih*10000.      ! minutes,seconds always zero for SCICHEMv3.2

    call legal_timestamp(iy,im,id,ih-Period,23) ! averaging period

    call dat2jul(iy,im,id,ijul)
    date_beg = iy*1000 + ijul
    time_beg = ih*10000.      ! minutes,seconds always zero for SCICHEMv3.2
!
!  --- variable for TFLAG ---
!
    allocate( iarray_2d(2,nspcs+NCF_BASE_VARS) )
    do ispc=1,nspcs+NCF_BASE_VARS
       iarray_2d(1,ispc) = date_beg
       iarray_2d(2,ispc) = time_beg
    enddo
    this_var = "TFLAG"
    this_varid = 0
    ierr = nc_inq_varid(iounit,AddNull(trim(this_var)),this_varid)
    if( ierr /= 0 ) goto 7000
    ierr = nc_put_vara_int(iounit,this_varid,data_start-1,data_count,iarray_2d)
    if( ierr /= 0 ) goto 7001
    deallocate( iarray_2d )
!
!  --- variable for ETFLAG ---
!
    allocate( iarray_2d(2,nspcs+NCF_BASE_VARS) )
    do ispc=1,nspcs+NCF_BASE_VARS
       iarray_2d(1,ispc) = date_end
       iarray_2d(2,ispc) = time_end
    enddo
    this_var = "ETFLAG"
    this_varid = 0
    ierr = nc_inq_varid(iounit,AddNull(trim(this_var)),this_varid)
    if( ierr /= 0 ) goto 7000
    ierr = nc_put_vara_int(iounit,this_varid,data_start-1,data_count,iarray_2d)
    if( ierr /= 0 ) goto 7001
    deallocate( iarray_2d )

    goto 9999
!
!-----------------------------------------------------------------------
!    Error messages:
!-----------------------------------------------------------------------
!
7000 continue
    write(*,'(//,a)') 'ERROR in NCF_WRT_DATA_TSTEP:'
    write(*,'(A)') trim(action)
    write(*,'(2A)') 'Cannot find variable id for: ',trim(this_var)
    stop
!
7001 continue
    write(*,'(//,a)') 'ERROR in NCF_WRT_DATA_TSTEP:'
    write(*,'(A)') trim(action)
    write(*,'(2A)') 'Cannot write data for the variable: ',trim(this_var)
    stop
!
!-----------------------------------------------------------------------
!    Return point:
!-----------------------------------------------------------------------
!
9999 continue
    return
  end subroutine ncf_wrt_data_tstep
!
!***********************************************************************
!**** NCF_WRT_DATA_SPECIES
!***********************************************************************
!
  subroutine ncf_wrt_data_species(action,iounit,                &
       num_cols,num_rows,nspcs,spec_name,cncfld,ncf_cur_tstep)

    use basic_fd

    implicit none
!
!-----------------------------------------------------------------------
!    Description:
!-----------------------------------------------------------------------
!
!   This routine writes the data for the timestep variables to the
!    NetCDF file
!
!      Argument description:
!       Inputs:
!           action        C name of file to open
!           iounit        I NetCDF file ID of file
!           num_cols      I number of columns in this grid
!           num_rows      I number of rows in this grid
!           nspcs         I number of species in the file
!           cncfld        R gridded array of concentrations
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
!    Include files:
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!    Argument declarations:
!-----------------------------------------------------------------------
!
    character*(*) action
    integer       iounit
    integer       num_cols
    integer       num_rows
    integer       nspcs
    character*(*) spec_name(nspcs)
    real          cncfld(num_cols,num_rows)
    integer       ncf_cur_tstep
!
!-----------------------------------------------------------------------
!    External functions:
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!    Local variables:
!-----------------------------------------------------------------------
!
    character*14 this_var
!    integer      data_start(4), data_count(4), ispc, i, j, k
    integer      ispc, i, j, k
    INTEGER(LEN_ADDRESS) :: data_start(4), data_count(4)
    integer      ierr, this_varid
    real,        allocatable, dimension(:,:,:) :: array_3d
    real*8,      allocatable, dimension(:,:,:) :: darray_3d
!
!-----------------------------------------------------------------------
!    Entry point:
!-----------------------------------------------------------------------
!
!  --- set the position in the NetCDF variable to write ---
!
    data_start(1) = 1
    data_count(1) = num_cols
    data_start(2) = 1
    data_count(2) = num_rows
    data_start(3) = 1
    data_count(3) = 1 ! num_lays_out
    data_start(4) = ncf_cur_tstep
    data_count(4) = 1

!    print*,"Writing ncf lun,",iounit," at ",ncf_cur_tstep
!
!  --- allocate the array (double) that will be used to write the data ---
!
    allocate( darray_3d(num_cols,num_rows,1) )
!
!  --- first do the z variable ---
!
    do k=1,1 ! was do k=1,num_lays_out
       do j=1,num_rows
          do i=1,num_cols
             darray_3d(i,j,k) = 0.d0 ! height above ground always 0.0 in DOS files
          enddo
       enddo
    enddo
!
!  --- get the id for the z variable and write the data ---
!
    this_var = "z"
    ierr = nc_inq_varid(iounit,AddNull("z"),this_varid)
    if( ierr /= 0 ) goto 7000

    ierr = nc_put_vara_double(iounit,this_varid,data_start-1,data_count,darray_3d)
    if( ierr /= 0 ) goto 7001
!
!  ---- deallocate the array ---
!
    deallocate( darray_3d )
!
!  --- allocate the array that will be used to write the data ---
!
    allocate( array_3d(num_cols,num_rows,1) )
!
!  --- loop over all variables in this file ---
!
    do ispc=1,nspcs
!
!  --- get name for this variable, and get it's variable ID
!      in this file ---
!
       this_var = adjustl(spec_name(ispc))
       ierr = nc_inq_varid(iounit,AddNull(trim(this_var)),this_varid)
       if( ierr /= 0 ) goto 7000
!
!  --- load the data into the local array to write ---
!
       do j=1,num_rows
          do i=1,num_cols
             array_3d(i,j,1) = cncfld(i,j)
!             print*,"in ncf_wrt: ",i,j,cncfld(i,j)
          enddo
       enddo

       ierr = nc_put_vara_float(iounit,this_varid,data_start,data_count,array_3d)
       if( ierr /= 0 ) goto 7001

    enddo
!
!  ---- deallocate the array ---
!
    deallocate( array_3d )

    goto 9999
!
!-----------------------------------------------------------------------
!    Error messages:
!-----------------------------------------------------------------------
!
7000 continue
    write(*,'(//,a)') 'ERROR in NCF_WRT_DATA_SPECIES:'
    write(*,'(A)') trim(action)
    write(*,'(2A)') 'Cannot find variable id for: ',trim(this_var)
    stop

7001 continue
    write(*,'(//,a)') 'ERROR in NCF_WRT_DATA_SPECIES:'
    write(*,'(A)') trim(action)
    write(*,'(2A)') 'Cannot write data for the variable: ',trim(this_var)
    write(*,'(A,i)') 'nc_put_vara_double error code is ',ierr
    stop
!
!-----------------------------------------------------------------------
!    Return point:
!-----------------------------------------------------------------------
!
9999 continue
    return
  end subroutine ncf_wrt_data_species
!
!***********************************************************************

!***********************************************************************

end Module netcdf_tools
