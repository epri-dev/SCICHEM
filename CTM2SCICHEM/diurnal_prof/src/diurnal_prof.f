      program cmaq_emis_avg
      implicit none
C
C     get n-hourly emiss averaged or summed over a multi-day period
C       - no unit conversion here!
C
C     created by bkoo - 08/26/2010
C     03/07/2012 bkoo - multi-layers
C     03/22/2012 bkoo - does AVG too!
C     03/06/2015 bkoo - added option to limit time span (BEGTIME, NRECORD)
C
      include 'PARMS3.EXT'
      include 'IODECL3.EXT'
      include 'FDESC3.EXT'

      character(256) :: MESG
      character(3)   :: INUM
      character(16)  :: INFILE, OUTFILE = 'OUTFILE'
      character(16), parameter :: PGNAME = 'CMAQ_EMIS_AVG'
      character(16), allocatable :: VNAME(:)

      integer :: ENVINT
      integer :: LOGUNIT
      integer :: istat
      logical :: ENVYN, llay, lsum

      integer :: NFILE,HSIZE,NCOLS,NROWS,NLAYS,NVARS,NREC,NREC_IN
      integer :: IDATE,ITIME,JDATE,JTIME,STIME_IN
      integer :: i,j,k,l,n

      real, allocatable :: SUM(:,:,:,:,:), TMP(:,:,:,:)

c     
c     Initialize I/O-API
c
      LOGUNIT = INIT3()
c
c     Get BEGTIME
c
      STIME_IN = ENVINT('BEGTIME','Time to Begin processing',-1,istat)
      if (istat.gt.0) then
          MESG = 'Bad value for BEGTIME'
          call M3EXIT(PGNAME,0,0,MESG,2)
      endif
c
c     Get NRECORD
c
      NREC_IN = ENVINT('NRECORD','Number of Records in a file',-1,istat)
      if (istat.gt.0) then
        MESG = 'Bad value for NRECORD'
        call M3EXIT(PGNAME,0,0,MESG,2)
      endif
c
c     Get sum?
c
      lsum = ENVYN('GET_SUM','Get Sum?',.false.,istat)
      if (istat.gt.0) then
        MESG = 'Invalid value for GET_SUM'
        call M3EXIT(PGNAME,0,0,MESG,2)
      endif
c
c     All Layers?
c
      llay = ENVYN('ALL_LAYERS','Process All Layers?',.false.,istat)
      if (istat.gt.0) then
        MESG = 'Invalid value for ALL_LAYERS'
        call M3EXIT(PGNAME,0,0,MESG,2)
      endif
c
c     Get inputs
c
      NFILE = ENVINT('NFILE','Number of Input Files',1,istat)
      if (istat.gt.0 .or. NFILE.lt.1) then
        MESG = 'Bad value for NFILE'
        call M3EXIT(PGNAME,0,0,MESG,2)
      endif
c
c     Loop through input files
c
      do n = 1, NFILE

        write(INUM,'(i3.3)') n
        INFILE = 'INFILE'//INUM
c
c     Open input file
c
        if (.not.OPEN3(INFILE,FSREAD3,PGNAME)) then
          MESG = 'Cannot open ' // TRIM(INFILE)
          call M3EXIT(PGNAME,0,0,MESG,1)
        endif
c
c     Get description of input file
c
        if (.not.DESC3(INFILE)) then
          MESG = 'Cannot get description of ' // TRIM(INFILE)
          call M3EXIT(PGNAME,0,0,MESG,1)
        endif

        if (FTYPE3D.ne.GRDDED3) then
          MESG = 'Data type must be GRDDED3'
          call M3EXIT(PGNAME,0,0,MESG,2)
        endif

        if (n.eq.1) then

          IDATE = SDATE3D
          ITIME = STIME3D
          NREC  = MXREC3D
          NCOLS = NCOLS3D
          NROWS = NROWS3D
          if (llay) then
            NLAYS = NLAYS3D
          else
            NLAYS = 1
          endif
          NVARS = NVARS3D
          allocate (VNAME(NVARS), stat = istat)
          if (istat.ne.0) then
            MESG = 'Memory allocation failed: VNAME'
            call M3EXIT(PGNAME,0,0,MESG,2)
          endif
          allocate (TMP(NCOLS,NROWS,NLAYS,NVARS), stat = istat)
          if (istat.ne.0) then
            MESG = 'Memory allocation failed: TMP'
            call M3EXIT(PGNAME,0,0,MESG,2)
          endif
          allocate (SUM(NCOLS,NROWS,NLAYS,NVARS,NREC), stat = istat)
          if (istat.ne.0) then
            MESG = 'Memory allocation failed: SUM'
            call M3EXIT(PGNAME,0,0,MESG,2)
          endif
          SUM = 0.0
          VNAME = VNAME3D(:NVARS)

        else

          if (NCOLS3D.ne.NCOLS .or. NROWS3D.ne.NROWS) then
            write(MESG,101) NCOLS,NROWS,NCOLS3D,NROWS3D,TRIM(INFILE)
101         format('Inconsistent horizontal grid dimensions:',
     &             I,'X',I,1X,'(INFILE001)',
     &             I,'X',I,1X,'(',A,    ')')
            call M3EXIT(PGNAME,0,0,MESG,2)
          endif
          if (llay) then
            if (NLAYS3D.ne.NLAYS) then
              write(MESG,104) NLAYS,NLAYS3D,TRIM(INFILE)
104           format('Inconsistent vertical layer dimension:',
     &             I,1X,'(INFILE001)',
     &             I,1X,'(',A,    ')')
              call M3EXIT(PGNAME,0,0,MESG,2)
            endif
          endif
          if (NVARS3D.ne.NVARS) then
            write(MESG,102) NVARS,NVARS3D,TRIM(INFILE)
102         format('Inconsistent number of variables:',
     &             I,1X,'(INFILE001)',
     &             I,1X,'(',A,    ')')
            call M3EXIT(PGNAME,0,0,MESG,2)
          else
            do l = 1, NVARS
              if (VNAME3D(l).ne.VNAME(l)) then
                write(MESG,103) l,TRIM(VNAME3D(l)),TRIM(VNAME(l)),
     &                            TRIM(INFILE)
103             format('Inconsistent order of variables:',I,
     &                 1X,A,1X,'(INFILE001)',
     &                 1X,A,1X,'(',A,    ')')
                call M3EXIT(PGNAME,0,0,MESG,2)
              endif
            enddo
          endif

        endif
c
c     Read records
c
        JDATE = SDATE3D
        JTIME = STIME3D
        if (STIME_IN.ge.0) JTIME = STIME_IN
        if (NREC_IN.ge.0) NREC = NREC_IN
        write(*,*) 'STIME, NREC: ',JTIME,NREC

        do i = 1, NREC
          if (llay) then
            if (.not.READ3(INFILE,ALLVAR3,ALLAYS3,JDATE,JTIME,TMP)) then ! all layers
              MESG = 'Cannot read data from ' // TRIM(INFILE)
              call M3EXIT(PGNAME,JDATE,JTIME,MESG,1)
            endif
          else
            if (.not.READ3(INFILE,ALLVAR3,1,JDATE,JTIME,TMP)) then ! 1st layer only
              MESG = 'Cannot read data from ' // TRIM(INFILE)
              call M3EXIT(PGNAME,JDATE,JTIME,MESG,1)
            endif
          endif

          SUM(:,:,:,:,i) = SUM(:,:,:,:,i) + TMP(:,:,:,:)

          CALL NEXTIME(JDATE,JTIME,TSTEP3D)
        enddo

        if (.not.CLOSE3(INFILE)) then
          MESG = 'Cannot close ' // TRIM(INFILE)
          call M3EXIT(PGNAME,0,0,MESG,1)
        endif

      enddo ! NFILE

      write(*,*)'NDAY = ',NFILE      
c
c     Get avg
c
      if (.NOT.lsum) then
        do i = 1, NREC
          SUM(:,:,:,:,i) = SUM(:,:,:,:,i) / REAL(NFILE)
        enddo
      endif
c
c     Open output file
c
      NLAYS3D = NLAYS
      SDATE3D = IDATE
      STIME3D = ITIME
      if (STIME_IN.ge.0) STIME3D = STIME_IN
      MXREC3D = NREC
      TSTEP3D = 10000

      if (.not.OPEN3(OUTFILE,FSNEW3,PGNAME)) then
        MESG = 'Cannot open ' // TRIM(OUTFILE)
        call M3EXIT(PGNAME,0,0,MESG,1)
      endif
c
c     Write sum
c
      JDATE = SDATE3D
      JTIME = STIME3D

      do i = 1, MXREC3D
        if (.not.
     &       WRITE3(OUTFILE,ALLVAR3,JDATE,JTIME,SUM(:,:,:,:,i))) then
          MESG = 'Cannot write data to ' // TRIM(OUTFILE)
          call M3EXIT(PGNAME,JDATE,JTIME,MESG,1)
        endif

        CALL NEXTIME(JDATE,JTIME,TSTEP3D)
      enddo

      MESG = 'Successful completion of ' // PGNAME
      call M3EXIT(PGNAME,0,0,MESG,0)

      end

