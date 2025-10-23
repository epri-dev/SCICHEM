!=======================================================================
!    FILES common
!=======================================================================
MODULE files_fi

  USE DefSize_fd

  SAVE

! Puff file parameters

  CHARACTER(32), PARAMETER :: PUFFFILE_VERSION_STRING = 'PUFFFILE FORMAT VERSION:'
  INTEGER,       PARAMETER :: PUFFFILE_VERSION_VALUE  = 2
  INTEGER,       PARAMETER :: PUFFFILE_TIME_HEADER    = 1
  INTEGER,       PARAMETER :: PUFFFILE_MET_HEADER     = 2
  INTEGER,       PARAMETER :: PUFFFILE_SRC_HEADER     = 3
  INTEGER,       PARAMETER :: PUFFFILE_PUFF_RECORD    = 4
  INTEGER,       PARAMETER :: PUFFFILE_AUX_RECORD     = 5
  INTEGER,       PARAMETER :: PUFFFILE_MET_RECORD     = 6
  INTEGER,       PARAMETER :: PUFFFILE_SRC_RECORD     = 7
  INTEGER,       PARAMETER :: PUFFFILE_NDATA_RECORD   = PUFFFILE_SRC_RECORD - PUFFFILE_PUFF_RECORD + 1
  INTEGER,       PARAMETER :: PUFFFILE_NUM_RECORD     = PUFFFILE_SRC_RECORD

! Puff file

  INTEGER pufffile_version
  INTEGER pufffile_record

! Path names

  CHARACTER(PATH_MAXLENGTH) path_app ! application path
  CHARACTER(PATH_MAXLENGTH) path_inv ! data path
  CHARACTER(PATH_MAXLENGTH) path_map ! DCW map path
  CHARACTER(PATH_MAXLENGTH) path_tmp ! working path
  CHARACTER(PATH_MAXLENGTH) path_usr ! user path
  CHARACTER(PATH_MAXLENGTH) path_rst ! restart file path
  CHARACTER(PATH_MAXLENGTH) path_lus ! landuse file path

! File names

  CHARACTER(PATH_MAXLENGTH) file_dbg   ! debug output
  CHARACTER(PATH_MAXLENGTH) file_dep   ! surface deposition output
  CHARACTER(PATH_MAXLENGTH) file_def   ! plot options input
  CHARACTER(PATH_MAXLENGTH) file_amr   ! ambient output
  CHARACTER(PATH_MAXLENGTH) file_dgn   ! diagnostic output
  CHARACTER(PATH_MAXLENGTH) file_ddp   ! dry deposition
  CHARACTER(PATH_MAXLENGTH) file_wdp   ! wet deposition
  CHARACTER(PATH_MAXLENGTH) file_ados  ! surface dose output for ambient concentration
  CHARACTER(PATH_MAXLENGTH) file_asmp  ! sampler time history output for ambient concentration
  CHARACTER(PATH_MAXLENGTH) file_dmp   ! dump output
  CHARACTER(PATH_MAXLENGTH) file_dos   ! surface dose output
  CHARACTER(PATH_MAXLENGTH) file_err   ! error output
  CHARACTER(PATH_MAXLENGTH) file_icn   ! SCIPUFF operation incident file
  CHARACTER(PATH_MAXLENGTH) file_inp   ! SCIPUFF namelist input
  CHARACTER(PATH_MAXLENGTH) file_mcw   ! mass-consistent wind field output
  CHARACTER(PATH_MAXLENGTH) file_met   ! met profile input
  CHARACTER(PATH_MAXLENGTH) file_msc   ! met scenario input
  CHARACTER(PATH_MAXLENGTH) file_pal   ! GUI palette input
  CHARACTER(PATH_MAXLENGTH) file_prj   ! project output
  CHARACTER(PATH_MAXLENGTH) file_puf   ! puff output
  CHARACTER(PATH_MAXLENGTH) file_sbl   ! boundary layer input
  CHARACTER(PATH_MAXLENGTH) file_scn   ! release scenario input
  CHARACTER(PATH_MAXLENGTH) file_sfc   ! met surface input
  CHARACTER(PATH_MAXLENGTH) file_smp   ! sampler time history output
  CHARACTER(PATH_MAXLENGTH) file_sps   ! binary sampler time history output
  CHARACTER(PATH_MAXLENGTH) file_ter   ! terrain description output
  CHARACTER(PATH_MAXLENGTH) file_tmp   ! temporary/local file name
  CHARACTER(PATH_MAXLENGTH) file_log   ! SCIPUFF log file output
  CHARACTER(PATH_MAXLENGTH) file_rst   ! Restart project name
  CHARACTER(PATH_MAXLENGTH) file_lus   ! landuse data file
  CHARACTER(PATH_MAXLENGTH) file_abort ! if present - implies abort (immediate)   - dispersion process, output
  CHARACTER(PATH_MAXLENGTH) file_halt  ! if present - implies halt  (next step)   - dispersion process
  CHARACTER(PATH_MAXLENGTH) file_stop  ! if present - implies stop  (next output) - dispersion process
  CHARACTER(PATH_MAXLENGTH) file_pause ! if present - implies pause (next step)   - dispersion process
  CHARACTER(PATH_MAXLENGTH) file_resum ! if present - implies resume from pause   - dispersion process
  CHARACTER(PATH_MAXLENGTH) file_clog  ! caution log file

! file unit numbers

  INTEGER lun_dat, lun_dbg, lun_dep, lun_def
  INTEGER lun_dmp, lun_dos, lun_err, lun_inp
  INTEGER lun_log, lun_mcw, lun_met, lun_msc
  INTEGER lun_pal, lun_prj, lun_puf
  INTEGER lun_sbl, lun_scn, lun_sfc, lun_smp
  INTEGER lun_fil  !Writing output files (WriteField) e.g. OVL
  INTEGER lun_amr, lun_dgn, lun_asmp, lun_ados  ! stepped ambient, diagnostic, ambient sampler and dose output
  INTEGER lun_sps !Binary sampler output file
  INTEGER lun_src, lun_ter, lun_tmp, lun_usr
  INTEGER lun_clog

END MODULE files_fi

