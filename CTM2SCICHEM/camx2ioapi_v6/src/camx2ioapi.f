      program camx2ioapi
      implicit none

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Copyright (C) 2006-2016  Ramboll Environ
c
c This program is free software; you can redistribute it and/or
c modify it under the terms of the GNU General Public License
c as published by the Free Software Foundation; either version 2
c of the License, or (at your option) any later version.
c
c This program is distributed in the hope that it will be useful,
c but WITHOUT ANY WARRANTY; without even the implied warranty of
c MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c GNU General Public License for more details.
c
c To obtain a copy of the GNU General Public License
c go to the Free Software Foundation at http://www.fsf.org.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     CAMx2IOAPI converts CAMx outputs (avrg & depn) to I/O API format.
c     This program assumes hourly data and supports limited horizontal
c     and vertical coordinate systems (LatLon/UTM/Lambert/Polar/Mercator;
c     non-h sigma-P).
c
c
c     HISTORY:
c       created by bwang@cert.ucr.edu
c       modified by tcao@cert.ucr.edu (07/15/2003)
c       modified by bkoo (02/19/2004)
c       modified by bkoo (06/27/2006)
c       modified by bkoo (08/16/2006) - added SOP
c       modified by bkoo (11/29/2007) - added option to extract surface layer 
c                                       only
c       modified by bkoo (01/29/2008) - added UTM support
c       name change to camx2ioapi (01/29/2008)
c       modified by bkoo (07/15/2008) - added support for low-level emiss files
c       modified by bkoo (12/31/2008) - added option to extract only the species
c                                       for NADP
c       modified by bkoo (07/30/2009) - added option to extract any one layer
c                                       ex) setenv OUT_LAYER_INDEX 1 ! extract
c                                           the 1st layer
c       modified by bkoo (09/30/2009) - changed to output emissions starting 
c                                       from hour 0
c       modified by bkoo (09/20/2010) - added option to override TSTEP
c                                       ex) setenv TSTEP_OVERRIDE T ! make time-
c                                           independent file
c       modified by cemery (1/18/2013) - Uses new v6 header info for map 
c                                        projection data
c       modified by bkoo (03/11/2013) - added option to override UNITS3D
c                                       ex) setenv UNITS3D_OVERRIDE "moles/hr" 
c                                           ! set unit for all variables
c       modified by bkoo (04/23/2013) - added option to override VGTOP3D 
c                                       (originally by jjung)
c                                       ex) setenv VGTOP3D_OVERRIDE 5000.0
c                                           ! set VGTOP3D to 5000.0 Pa
c       modified by bkoo (03/31/2016) - added VBS PM species names
c                                     - added polar and mercator projection support
c                                     - added option to override timezone
c                                     - added option to override map projection
c
      include 'PARMS3.EXT'
      include 'IODECL3.EXT'
      include 'FDESC3.EXT'

      integer :: LOGUNIT
      integer :: in
      integer :: JDATE,JTIME
      integer :: ENVINT, KOUT
      logical :: ENVYN, LAYFLAG, LTSTOVR
      real :: ENVREAL, VGTOPOVR
      character(16) :: UNITSOVR

      character(16), parameter :: OUTFILE = 'IOAPI_OUT'
      character(16), parameter :: PGNAME = 'CAMx2IOAPI'

      character(256) :: MESG

      character(4), dimension(10) :: name
      character(4), dimension(60) :: note
      integer :: nspec,ibdate,iedate,iutm,nx,ny,nz,
     &           iproj,istag,ixseg,iyseg,nxseg,nyseg,iseg
      real :: btime,etime,plon,plat,xorg,yorg,
     &        delx,dely,tlat1,tlat2,htupp

      character(4), allocatable :: mspec(:,:)
      integer, allocatable :: idx(:)
      real, allocatable :: buff(:,:,:)
      integer :: istat

      character(10) :: tmpnam
      character(4) :: dtyp
      integer :: iyear,itzn,nstep

      integer, parameter :: npm = 23
      character(10), dimension(npm) :: pmnam = (/
     &     'PSO4 ','PNO3 ','PNH4 ','POA  ','PEC  ' ,
     &     'SOA  ','SOPA ','SOPB ','NA   ','PCL  ' ,
     &     'FPRM ','FCRS ','CPRM ','CCRS ','CRST ',
     &     'PAS  ','PBS  ','PAP  ','PCP  ','PFP  ' ,
     &     'PH2O ','HGP  ','HGIIP' /)
      integer, parameter :: nspnadp = 6
      character(10), dimension(nspnadp) :: spnadp = (/
     &     'SULF_WD', 'HNO3_WD', 'NH3_WD ',
     &     'PSO4_WD', 'PNO3_WD', 'PNH4_WD' /)
      integer, parameter :: ncoord = 10
      character(30), dimension(ncoord) :: coordnam = (/
     &     'lat-lon coords                ',
     &     'Lambert conformal conic       ',
     &     '(general) Mercator            ',
     &     '(tangent) stereographic       ',
     &     'UTM                           ',
     &     'polar stereographic           ',
     &     'equatorial Mercator           ',
     &     'transverse Mercator           ',
     &     'Albers conic Equal Area       ',
     &     'Lambert Azimuthal Equal Area  ' /)

      real, parameter :: sec2hr = 3600.

      integer :: i,j,k,l,m,n,nzo
c     
c     Initialize I/O-API
c
      LOGUNIT = INIT3()
c
c     Input file name
c
      write(*,*) 'Enter CAMx binary filename:'
      read(*,'(20x,a)') MESG
      write(*,*) TRIM(MESG)
      in = 10
      open(in,file=MESG,status='OLD',form='UNFORMATTED')
c
c     Read CAMx header
c
      read(in) name,note,itzn,nspec,ibdate,btime,iedate,etime
      read(in) plon,plat,iutm,xorg,yorg,delx,dely,nx,ny,nz,
     &         iproj,istag,tlat1,tlat2,htupp
      read(in) ixseg,iyseg,nxseg,nyseg
c
c     Data type
c
      write(*,*) 'Enter data type (AVRG, DDEP, WDEP or EMIS):'
      read(*,'(20x,a)') dtyp
      write(*,*) dtyp
      if (dtyp.ne.'AVRG' .and. dtyp.ne.'DDEP' .and. dtyp.ne.'WDEP'
     &                   .and. dtyp.ne.'EMIS' .and. dtyp.ne.'NADP') then
        MESG = 'Invalid data type - ' // dtyp
        call M3EXIT(PGNAME,0,0,MESG,2)
      endif
      if (dtyp.eq.'EMIS' .and. nz.ne.1) then
        write(*,*) 'WARNING: NZ is set to 1'
        nz = 1
      endif
c
c     Input year
c
      iyear = ibdate/1000
      if (iyear.lt.50) then
        iyear = 2000 + iyear
      else
        iyear = 1900 + iyear
      endif
      write(*,*) 'Four-digit year: ',iyear
c
c     Input time zone
c
      itzn = ENVINT('TIMEZONE_OVERRIDE','Reset Input Timezone',itzn,
     &                                                           istat)
      if (istat.gt.0) then
        MESG = 'Bad value for TIMEZONE_OVERRIDE'
        call M3EXIT(PGNAME,0,0,MESG,2)
      endif
      write(*,*) 'Time zone (e.g., 8 for PST): ',itzn
c
c     Map projection parameters
c
      call ENVSTR('MAP_PROJ_OVERRIDE','Force Map Projection','',MESG,
     &                                                           istat)
      if (istat.gt.0) then
        MESG = 'Bad value for MAP_PROJ_OVERRIDE'
        call M3EXIT(PGNAME,0,0,MESG,2)
      endif

      if (MESG.eq.'') then
        if (iproj.eq.0) then        ! LatLon
          GDTYP3D = LATGRD3
        elseif (iproj.eq.1) then    ! UTM
          GDTYP3D = UTMGRD3
          P_ALP3D = DBLE(iutm)
          XCENT3D = plon
          YCENT3D = plat
        elseif (iproj.eq.2) then    ! Lambert
          GDTYP3D = LAMGRD3
          P_ALP3D = MIN(tlat1,tlat2)
          P_BET3D = MAX(tlat1,tlat2)
          P_GAM3D = plon
          XCENT3D = plon
          YCENT3D = plat
        elseif (iproj.eq.4) then    ! Polar
          GDTYP3D = POLGRD3
          if (tlat1.ge.0.) then
            P_ALP3D = 1.0
          else
            P_ALP3D = -1.0
          endif
          P_BET3D = tlat1
          P_GAM3D = plon
          XCENT3D = plon
          YCENT3D = plat
        elseif (iproj.eq.5) then    ! Mercator
          GDTYP3D = EQMGRD3
          P_ALP3D = tlat1
          P_GAM3D = plon
          XCENT3D = plon
          YCENT3D = plat
        else
          MESG = 'Unsupported CAMx map projection'
          call M3EXIT(PGNAME,0,0,MESG,2)
        endif
      else
        read(MESG,*) GDTYP3D,P_ALP3D,P_BET3D,P_GAM3D,XCENT3D,YCENT3D
        if (GDTYP3D.lt.1 .or. GDTYP3D.gt.ncoord) then
          MESG = 'Unsupported I/O-API map projection'
          call M3EXIT(PGNAME,0,0,MESG,2)
        endif
      endif

      write(*,*) 'Horizontal coordinate system for ' // TRIM(OUTFILE) //
     &           ' is set to:'
      write(*,*) '  ',TRIM(coordnam(GDTYP3D))
      write(*,*) '  P_ALP3D = ', P_ALP3D
      write(*,*) '  P_BET3D = ', P_BET3D
      write(*,*) '  P_GAM3D = ', P_GAM3D
      write(*,*) '  XCENT3D = ', XCENT3D
      write(*,*) '  YCENT3D = ', YCENT3D
c
c     Get UNITSOVR
c
      call ENVSTR('UNITS3D_OVERRIDE','Force Output Units',
     &                               '[UNIT_UNDEFINED]',UNITSOVR,istat)
      if (istat.gt.0) then
        MESG = 'Bad value for UNITS3D_OVERRIDE'
        call M3EXIT(PGNAME,0,0,MESG,2)
      endif
c
c     Get LTSTOVR
c
      LTSTOVR = ENVYN('TSTEP_OVERRIDE','Force Time-Independent Output',
     &                                                   .false.,istat)
      if (istat.gt.0) then
        MESG = 'Bad value for TSTEP_OVERRIDE'
        call M3EXIT(PGNAME,0,0,MESG,2)
      endif
c
c     Get LAYFLAG
c
      LAYFLAG = ENVYN('SURFACE_LAYER_ONLY','Output Surface Layer Only',
     &                                                   .false.,istat)
      if (istat.gt.0) then
        MESG = 'Bad value for SURFACE_LAYER_ONLY'
        call M3EXIT(PGNAME,0,0,MESG,2)
      endif
c
c     Get KOUT
c
      KOUT = ENVINT('OUT_LAYER_INDEX','Output Layer Index',0,istat)
      if (istat.gt.0) then
        MESG = 'Bad value for OUT_LAYER_INDEX'
        call M3EXIT(PGNAME,0,0,MESG,2)
      else if (istat.eq.0 .and. (KOUT.lt.1 .or. KOUT.gt.nz)) then
        write(MESG,'(a,i2)') 'OUT_LAYER_INDEX must be between 1 and ',nz
        call M3EXIT(PGNAME,0,0,MESG,2)
      endif

      if (LAYFLAG) then
        if (KOUT.eq.0) then
          KOUT = 1
        else if (KOUT.gt.1) then
          write(*,*) '* SURFACE_LAYER_ONLY flag will be ignored ' //
     &               'because OUT_LAYER_INDEX is greater than 1.'
        endif
      endif

      nzo = nz
      if (KOUT.gt.0) nzo = 1
c
c     Get VGTOPOVR
c
      VGTOPOVR = ENVREAL('VGTOP3D_OVERRIDE','Force Output VGTOP3D [Pa]',
     &                                                    10000.0,istat)
      if (istat.gt.0) then
        MESG = 'Bad value for VGTOP3D_OVERRIDE'
        call M3EXIT(PGNAME,0,0,MESG,2)
      endif
c
c     Vertical grid parameters - sigma levels
c
      write(*,*) 'Enter sigma levels'
      read(*,'(20x,a)') MESG
      if (nzo.gt.1) then
        read(MESG,*) (VGLVS3D(k),k=2,nzo+1)
        VGLVS3D(1) = 1.0
        do k = 1, nzo+1
          write(*,'(i3,2x,f10.6)') k-1, VGLVS3D(k)
        enddo
      endif
c
c     Read species list
c
      allocate (mspec(10,nspec), stat = istat)
      if (istat.ne.0) then
        MESG = 'Memory allocation failed: MSPEC'
        call M3EXIT(PGNAME,0,0,MESG,2)
      endif
      allocate (idx(nspec), stat = istat)
      if (istat.ne.0) then
        MESG = 'Memory allocation failed: IDX'
        call M3EXIT(PGNAME,0,0,MESG,2)
      endif
      idx = 0
      read(in) ((mspec(n,l),n=1,10),l=1,nspec)
      NVARS3D = 0
      do l = 1, nspec
        write(tmpnam,'(10a1)') (mspec(n,l),n=1,10)
        if (dtyp.eq.'NADP') then
          j = 0
          do i = 1, nspnadp
            if (spnadp(i).eq.tmpnam) j = 1 ! species for NADP
          enddo
          if (j.eq.0) cycle
        endif
        if (dtyp.eq.'DDEP' .and. INDEX(tmpnam,'_DD').eq.0) cycle
        if (dtyp.eq.'WDEP' .and. INDEX(tmpnam,'_WD').eq.0) cycle
        NVARS3D = NVARS3D + 1
        idx(l) = NVARS3D
        VNAME3D(idx(l)) = tmpnam
        VDESC3D(idx(l)) = 'VARIABLE ' // TRIM(tmpnam)
        VTYPE3D(idx(l)) = M3REAL
        j = 0
        do i = 1, npm
          k = INDEX(pmnam(i),' ')
          if (pmnam(i)(:k-1).eq.tmpnam(:k-1)) j = 1 ! PM species
        enddo
        if (dtyp.eq.'AVRG') then
          UNITS3D(idx(l)) = 'ppmV'
          if (j.eq.1) UNITS3D(idx(l)) = 'micrograms/m**3' ! PM species
        else if (dtyp.eq.'EMIS') then
          UNITS3D(idx(l)) = 'moles/s'
          if (j.eq.1) UNITS3D(idx(l)) = 'g/s' ! PM species
        else
          UNITS3D(idx(l)) = 'mol/hectare'
          if (j.eq.1) UNITS3D(idx(l)) = 'g/hectare' ! PM species
        endif
      enddo

      if (UNITSOVR.ne.'[UNIT_UNDEFINED]') then
        UNITS3D = UNITSOVR
        write(*,'(//,2a,/)') 'UNITS3D is set to ',UNITSOVR
      endif

      write(*,*) 'No. Name            Unit'
      do l = 1, NVARS3D
        write(*,'(i3,2x,2a16)') l, VNAME3D(l), UNITS3D(l)
      enddo
c
c     CMAQ haeder
c
      FTYPE3D = GRDDED3 ! Gridded
      FDESC3D(1) = 'I/O API formatted CAMx ' // TRIM(dtyp) // ' output'

      GDNAM3D = CMISS3  ! '????????????????'

      if (nzo.gt.1) then
        VGTYP3D = VGSGPN3  ! non-h sigma-P
        VGTOP3D = VGTOPOVR ! Pa
      else
        VGTYP3D = IMISS3   ! -9999
        VGTOP3D = BADVAL3  ! -9.999E36
      endif

      NCOLS3D = nx
      NROWS3D = ny
      NLAYS3D = nzo
      NTHIK3D = 1

      XORIG3D = xorg    ! [m]
      YORIG3D = yorg    ! [m]
         
      XCELL3D = delx    ! [m]
      YCELL3D = dely    ! [m]

      MXREC3D = (iedate-ibdate)*24 + NINT(etime-btime)
      TSTEP3D = 10000   ! Assume hourly data
      SDATE3D = iyear*1000 + MOD(ibdate,1000)
      STIME3D = NINT( btime * 10000. )

      if (dtyp.ne.'AVRG' .and. dtyp.ne.'EMIS')
     &    STIME3D = STIME3D + TSTEP3D ! Use end time of each interval
      call NEXTIME(SDATE3D, STIME3D, itzn*10000) ! Convert to GMT

      if ( LTSTOVR ) then
        MXREC3D = 1
        TSTEP3D = 0
      endif
c
c     Open output file
c
      if (.not.OPEN3(OUTFILE,FSNEW3,PGNAME)) then
        MESG = 'Cannot open ' // TRIM(OUTFILE)
        call M3EXIT(PGNAME,0,0,MESG,1)
      endif
c
c     Memory allocation
c
      allocate (buff(nx,ny,nz), stat = istat)
      if (istat.ne.0) then
        MESG = 'Memory allocation failed: BUFF'
        call M3EXIT(PGNAME,0,0,MESG,2)
      endif
c
c     Read/write hourly data
c
      JDATE = SDATE3D
      JTIME = STIME3D

      do m = 1, MXREC3D
        read(in,end=999) ibdate,btime,iedate,etime
        do l = 1, nspec

          if (idx(l).gt.0) then
            do k = 1, nz
              if (KOUT.eq.0 .or. k.eq.KOUT) then
                read(in,end=999) iseg,(mspec(n,l),n=1,10),
     &                                    ((buff(i,j,k),i=1,nx),j=1,ny)
              else
                read(in,end=999) iseg
              endif
            enddo

            if (KOUT.gt.0) then
              if (dtyp.eq.'EMIS') buff(:,:,KOUT)=buff(:,:,KOUT)/sec2hr
              if (.not.WRITE3(OUTFILE,VNAME3D(idx(l)),JDATE,JTIME,
     &                                            buff(:,:,KOUT))) then
                MESG = 'Cannot write data to ' // TRIM(OUTFILE)
                call M3EXIT(PGNAME,JDATE,JTIME,MESG,1)
              endif
            else
              if (dtyp.eq.'EMIS') buff = buff / sec2hr
              if (.not.WRITE3(OUTFILE,VNAME3D(idx(l)),JDATE,JTIME,
     &                                               buff)) then
                MESG = 'Cannot write data to ' // TRIM(OUTFILE)
                call M3EXIT(PGNAME,JDATE,JTIME,MESG,1)
              endif
            endif
          else
            do k = 1, nz
              read(in,end=999) iseg
            enddo
          endif

        enddo ! nspec
        call NEXTIME(JDATE, JTIME, TSTEP3D)
      enddo

      goto 1000

999   MESG = 'Premature end of CAMx file'
      call M3EXIT(PGNAME,JDATE,JTIME,MESG,1)

1000  continue
      close(in)

      MESG = 'Successful completion of ' // PGNAME
      call M3EXIT(PGNAME,0,0,MESG,0)

      end

