      program spcmap
      implicit none
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c   Copyright (C) 2006-2016  Ramboll Environ
c
c
c   This program is free software; you can redistribute it and/or
c   modify it under the terms of the GNU General Public License
c   as published by the Free Software Foundation; either version 2
c   of the License, or (at your option) any later version.
c
c   This program is distributed in the hope that it will be useful,
c   but WITHOUT ANY WARRANTY; without even the implied warranty of
c   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c   GNU General Public License for more details.
c
c   To obtain a copy of the GNU General Public License
c   go to the Free Software Foundation at http://www.fsf.org.
c
c
c   For comments and questions, send to bkoo@ramboll.com
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     SPCMAP creates a new I/O-API file with new variables each of which
c     is a linear combination of variables from the input I/O-API file.
c     Units of the new variables are user-defined.
c
c     This code is a sub-program of CAMx2CMAQ.
c
c     INPUT: via environmental variable
c       INFILE    - logical name for input file
c       OUTFILE   - logical name for output file
c       MAPTBL    - logical name for species mapping table (ASCII) whose
c                   file structure is hard-wired
c
c     OUTPUT:
c       OUTFILE
c
c     HISTORY:
c       created by bkoo (04/14/2006)
c       modified by bkoo (02/21/2008)     - changed to allow variable # of header lines
c       modified by bkoo (06/19/2014)     - changed to allow using .START and .END labels
c       modified by bkoo (02/02/2016)     - fixed a too-many-species bug
c
      include 'PARMS3.EXT'
      include 'IODECL3.EXT'
      include 'FDESC3.EXT'

      integer :: LOGUNIT,MUNIT
      integer :: GETEFILE,INDEX1
      integer :: JDATE,JTIME,SIZE

c     Maximum number of species in the right-hand side of a mapping equation
      integer, parameter :: MXMAP = 15
c     Maximum number of output species
      integer, parameter :: MXOUTS = 2048
c     Output variable description
      character(80), parameter :: OUTVDESC = 'COMBINED VARIABLE'

      character(16), parameter :: INFILE  = 'INFILE'
      character(16), parameter :: OUTFILE = 'OUTFILE'
      character(16), parameter :: MAPTBL  = 'MAPTBL'
      character(16), parameter :: PGNAME  = 'SPCMAP'
      character(256) :: MESG, MESG2

      character(16) :: newnam(MXOUTS),newuni(MXOUTS),miss1(MXMAP*MXOUTS)
      integer :: imap(0:MXMAP,MXOUTS)
      real :: coef(MXMAP,MXOUTS)

      character(16) :: tmpnam
      integer, allocatable :: miss2(:)
      real, allocatable :: BUFIN(:,:), BUFOUT(:,:)

      integer :: istat,newnsp,idxin

      integer :: i,j,k,l,n
c
c     Initialize I/O-API
c
      LOGUNIT = INIT3()
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
c
c     Check header info
c
      if (FTYPE3D.eq.GRDDED3) then
        SIZE = NCOLS3D*NROWS3D*NLAYS3D
      elseif (FTYPE3D.eq.BNDARY3) then
        SIZE = 2*ABS(NTHIK3D)*(NCOLS3D+NROWS3D+2*NTHIK3D)*NLAYS3D
      else
        MESG = 'Data type of ' // TRIM(INFILE) //
     &         ' must be GRDDED3 or BNDARY3'
        call M3EXIT(PGNAME,0,0,MESG,2)
      endif
c
c     Open mapping table file
c
      MUNIT = GETEFILE(MAPTBL,.TRUE.,.TRUE.,PGNAME)
      if (MUNIT.lt.0) then
        MESG = 'Cannot open ' // TRIM(MAPTBL)
        call M3EXIT(PGNAME,0,0,MESG,1)
      endif
c
c     Initialize arrays for missing species
c
      allocate (miss2(NVARS3D), stat = istat)
      if (istat.ne.0) then
        MESG = 'Memory allocation failed: MISS2'
        call M3EXIT(PGNAME,0,0,MESG,2)
      endif
      miss1 = ''
      miss2 = 1
c
c     Read mapping table
c
      newnsp = -9
      n = -9
      j = 0 ! Initialize the missing mapping species counter
      write(*,*) 'Species Mapping Table in effect:'
10    read(MUNIT,'(a)',END=99) MESG
      if (MESG(1:1).eq.'#') goto 10 ! Skip the comment lines
      if (MESG(1:1).eq.'.') then    ! Label
        if (MESG(2:6).eq.'START') then
          n = 0 ! Initialize the output species counter
          goto 10
        elseif (MESG(2:4).eq.'END') then
          goto 100
        else
          MESG = 'Invalid Label'
          call M3EXIT(PGNAME,0,0,MESG,2)
        endif
      endif
      if (n.lt.0) then              ! Old format
        read(MESG,'(i)') newnsp
        n = 0 ! Initialize the output species counter
        goto 10
      endif
c
c     Generate internal mapping index; skip species that gets no mapping
c
      n = n + 1
      if (n.gt.MXOUTS) then
        MESG = 'Too many output species: Increase MXOUTS'
        call M3EXIT(PGNAME,0,0,MESG,2)
      endif
      read(MESG,'(a16,a16,i16)') newnam(n),newuni(n),imap(0,n)

      if (imap(0,n).gt.MXMAP) then
        MESG = 'Maximum number of mapping species exceeded'
        call M3EXIT(PGNAME,0,0,MESG,2)
      endif

      read(MUNIT,'(a)',END=99) MESG
      read(MUNIT,'(a)',END=99) MESG2
      k = 0 ! Actual number of mapping species for the n-th output species
      do i = 1, imap(0,n)
        tmpnam = MESG((i-1)*16+1:i*16)
        idxin = INDEX1(tmpnam,NVARS3D,VNAME3D)
        if (idxin.eq.0) then
          if (INDEX1(tmpnam,MXMAP*MXOUTS,miss1).eq.0) then
            j = j + 1
            miss1(j) = tmpnam ! Missing mapping species name
          endif
        else
          k = k + 1
          imap(k,n) = idxin
          read(MESG2((i-1)*16+1:i*16),'(f)') coef(k,n)
          miss2(idxin) = 0 ! Take this off the list of unused input species
        endif
      enddo
c
c     Print the mapping equation if the output species gets mapping
c     Skip it if not
c
      if (k.gt.0) then
        imap(0,n) = k ! Set actual number of mapping species
        write(*,'(5x,a16,a2,g12.5,1x,a16)') newnam(n),'= ',
     &                                  coef(1,n),VNAME3D(imap(1,n))
        do i = 2, imap(0,n)
          write(*,'(23x,g12.5,1x,a16)') coef(i,n),VNAME3D(imap(i,n))
        enddo
      else
        n = n - 1
        newnsp = newnsp - 1 ! Old format: Adjust newnsp
      endif

      if (newnsp.gt.0 .and. n.ge.newnsp) goto 100 ! Old format: All lines read
      goto 10

99    MESG = 'Premature end of ' // MAPTBL
      call M3EXIT(PGNAME,0,0,MESG,2)

100   continue
      if (n.le.0) then
        MESG = 'No mapping species found in ' // TRIM(INFILE)
        call M3EXIT(PGNAME,0,0,MESG,2)
      endif
      newnsp = n ! Set actual number of output species
      write(*,*) 'Number of output species:'
      write(*,*) newnsp

      write(*,*) 'Mapping species not found in ' // TRIM(INFILE) // ':'
      do l = 1, j
        if (mod(l,5).eq.0) then
          write(*,'(a16)') miss1(l)
        elseif (mod(l,5).eq.1) then
          write(*,'(5x,a16$)') miss1(l)
        else
          write(*,'(a16$)') miss1(l)
        endif
      enddo
      if (mod(j,5).ne.0) write(*,*) ' '
      write(*,*) TRIM(INFILE) // ' species not used in mapping:'
      n = 0
      do l = 1, NVARS3D
        if (miss2(l).eq.1) then
          n = n + 1
          if (mod(n,5).eq.0) then
            write(*,'(a16)') VNAME3D(l)
          elseif (mod(n,5).eq.1) then
            write(*,'(5x,a16$)') VNAME3D(l)
          else
            write(*,'(a16$)') VNAME3D(l)
          endif
        endif
      enddo
      if (mod(n,5).ne.0) write(*,*) ' '
      deallocate (miss2)
c
c     Allocate buffer memory
c
      allocate (BUFIN(SIZE,NVARS3D), stat = istat)
      if (istat.ne.0) then
        MESG = 'Memory allocation failed: BUFIN'
        call M3EXIT(PGNAME,0,0,MESG,2)
      endif
      allocate (BUFOUT(SIZE,newnsp), stat = istat)
      if (istat.ne.0) then
        MESG = 'Memory allocation failed: BUFOUT'
        call M3EXIT(PGNAME,0,0,MESG,2)
      endif
c
c     Open output file with new description
c
      NVARS3D = newnsp
      do l = 1, newnsp
        VNAME3D(l) = newnam(l)
        UNITS3D(l) = newuni(l)
        VDESC3D(l) = OUTVDESC
        VTYPE3D(l) = M3REAL
      enddo

      if (.not.OPEN3(OUTFILE,FSNEW3,PGNAME)) then
        MESG = 'Cannot open ' // TRIM(OUTFILE)
        call M3EXIT(PGNAME,0,0,MESG,1)
      endif
c
c     Read input data and write to output
c
      JDATE = SDATE3D
      JTIME = STIME3D

      do j = 1, MXREC3D

        if (.not.READ3(INFILE,ALLVAR3,ALLAYS3,JDATE,JTIME,BUFIN)) then
          MESG = 'Cannot read data from ' // TRIM(INFILE)
          call M3EXIT(PGNAME,JDATE,JTIME,MESG,1)
        endif
c
c     Species mapping
c
        do l = 1, newnsp
          do n = 1, SIZE
            BUFOUT(n,l) = coef(1,l) * BUFIN(n,imap(1,l))
            do i = 2, imap(0,l)
              BUFOUT(n,l) = BUFOUT(n,l) + coef(i,l) * BUFIN(n,imap(i,l))
            enddo
          enddo
        enddo

        if (.not.WRITE3(OUTFILE,ALLVAR3,JDATE,JTIME,BUFOUT)) then
          MESG = 'Cannot write data to ' // TRIM(OUTFILE)
          call M3EXIT(PGNAME,JDATE,JTIME,MESG,1)
        endif

        CALL NEXTIME(JDATE,JTIME,TSTEP3D)

      enddo ! MXREC3D

      MESG = 'Successful completion of ' // PGNAME
      call M3EXIT(PGNAME,0,0,MESG,0)

      end

