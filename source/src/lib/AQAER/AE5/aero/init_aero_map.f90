!***********************************************************************
!Sets up aerosol species pointers and mapping for the CBLK array       *
!required for the AE5 aerosol module in CMAQ 4.7.1                     *
!                                                                      *
!Also set up deposition velocity surrogate pointers                    *
!                                                                      *
!Version 1.0, P. Karamchandani, ENVIRON, Feb 2012                      *
!***********************************************************************
subroutine init_aero_map(aero_names,naerp,cblk_map,depv_sur)

USE AERO_INFO      ! from CMAQ
USE aero_consts_inc
USE error_aqaer_fi
!USE mpi_fi, ONLY: myid

implicit none

! --- ARGUMENTS
integer :: naerp
character*(*) aero_names(naerp)
integer, dimension(naerp) :: cblk_map, depv_sur

! --- Locals
integer spc, vv, nn ! Loop variables
integer :: nerrs = 0

vv = 0

vso4aj = 0
do spc = 1, naerp
   if (TRIM(aero_names(spc)) == 'ASO4J') then
      vv = vv + 1; vso4aj = vv; cblk_map(vv) = spc; depv_sur(vv) = VDMACC
      EXIT
   end if
end do
if (vso4aj == 0 ) then
   !!WRITE(99+myid,*)'Error: aso4j not found'
   nerrs = nerrs + 1
end if

vso4ai = 0
do spc = 1, naerp
   if (TRIM(aero_names(spc)) == 'ASO4I') then
      vv = vv + 1; vso4ai = vv; cblk_map(vv) = spc; depv_sur(vv) = VDMATK
      EXIT
   end if
end do
if (vso4ai == 0 ) then
   !!WRITE(99+myid,*)'Error: aso4i not found'
   nerrs = nerrs + 1
end if

vpaminej = 0
do spc = 1, naerp
   if (TRIM(aero_names(spc)) == 'AAMINEJ') then
      vv = vv + 1; vpaminej = vv; cblk_map(vv) = spc; depv_sur(vv) = VDMACC
      EXIT
   end if
end do

if (vpaminej == 0 ) then
   !!WRITE(99+myid,*)'Error: aaminej not found'
   nerrs = nerrs + 1
end if

vpaminei = 0
do spc = 1, naerp
   if (TRIM(aero_names(spc)) == 'AAMINEI') then
      vv = vv + 1; vpaminei = vv; cblk_map(vv) = spc; depv_sur(vv) = VDMATK
      EXIT
   end if
end do

if (vpaminei == 0 ) then
   !!WRITE(99+myid,*)'Error: aaminei not found'
   nerrs = nerrs + 1
end if

vnh4aj = 0
do spc = 1, naerp
   if (TRIM(aero_names(spc)) == 'ANH4J') then
      vv = vv + 1; vnh4aj = vv; cblk_map(vv) = spc; depv_sur(vv) = VDMACC
      EXIT
   end if
end do
if (vnh4aj == 0 ) then
   !!WRITE(99+myid,*)'Error: anh4j not found'
   nerrs = nerrs + 1
end if

vnh4ai = 0
do spc = 1, naerp
   if (TRIM(aero_names(spc)) == 'ANH4I') then
      vv = vv + 1; vnh4ai = vv; cblk_map(vv) = spc; depv_sur(vv) = VDMATK
      EXIT
   end if
end do
if (vnh4ai == 0 ) then
   !!WRITE(99+myid,*)'Error: anh4i not found'
   nerrs = nerrs + 1
end if

vno3aj = 0
do spc = 1, naerp
   if (TRIM(aero_names(spc)) == 'ANO3J') then
      vv = vv + 1; vno3aj = vv; cblk_map(vv) = spc; depv_sur(vv) = VDMACC
      EXIT
   end if
end do
if (vno3aj == 0 ) then
   !!WRITE(99+myid,*)'Error: ano3j not found'
   nerrs = nerrs + 1
end if

vno3ai = 0
do spc = 1, naerp
   if (TRIM(aero_names(spc)) == 'ANO3I') then
      vv = vv + 1; vno3ai = vv; cblk_map(vv) = spc; depv_sur(vv) = VDMATK
      EXIT
   end if
end do
if (vno3ai == 0 ) then
   !!WRITE(99+myid,*)'Error: ano3i not found'
   nerrs = nerrs + 1
end if

valkj = 0
do spc = 1, naerp
   if (TRIM(aero_names(spc)) == 'AALKJ') then
      vv = vv + 1; valkj = vv; cblk_map(vv) = spc; depv_sur(vv) = VDMACC
      EXIT
   end if
end do
if (valkj == 0 ) then
   !!WRITE(99+myid,*)'Error: aalkj not found'
   nerrs = nerrs + 1
end if

vxyl1j = 0
do spc = 1, naerp
   if (TRIM(aero_names(spc)) == 'AXYL1J') then
      vv = vv + 1; vxyl1j = vv; cblk_map(vv) = spc; depv_sur(vv) = VDMACC
      EXIT
   end if
end do
if (vxyl1j == 0 ) then
   !WRITE(99+myid,*)'Error: axyl1j not found'
   nerrs = nerrs + 1
end if

vxyl2j = 0
do spc = 1, naerp
   if (TRIM(aero_names(spc)) == 'AXYL2J') then
      vv = vv + 1; vxyl2j = vv; cblk_map(vv) = spc; depv_sur(vv) = VDMACC
      EXIT
   end if
end do
if (vxyl2j == 0 ) then
   !WRITE(99+myid,*)'Error: axyl2j not found'
   nerrs = nerrs + 1
end if

vxyl3j = 0
do spc = 1, naerp
   if (TRIM(aero_names(spc)) == 'AXYL3J') then
      vv = vv + 1; vxyl3j = vv; cblk_map(vv) = spc; depv_sur(vv) = VDMACC
      EXIT
   end if
end do
if (vxyl3j == 0 ) then
   !WRITE(99+myid,*)'Error: axyl3j not found'
   nerrs = nerrs + 1
end if

vtol1j = 0
do spc = 1, naerp
   if (TRIM(aero_names(spc)) == 'ATOL1J') then
      vv = vv + 1; vtol1j = vv; cblk_map(vv) = spc; depv_sur(vv) = VDMACC
      EXIT
   end if
end do
if (vtol1j == 0 ) then
   !WRITE(99+myid,*)'Error: atol1j not found'
   nerrs = nerrs + 1
end if

vtol2j = 0
do spc = 1, naerp
   if (TRIM(aero_names(spc)) == 'ATOL2J') then
      vv = vv + 1; vtol2j = vv; cblk_map(vv) = spc; depv_sur(vv) = VDMACC
      EXIT
   end if
end do
if (vtol2j == 0 ) then
   !WRITE(99+myid,*)'Error: atol2j not found'
   nerrs = nerrs + 1
end if

vtol3j = 0
do spc = 1, naerp
   if (TRIM(aero_names(spc)) == 'ATOL3J') then
      vv = vv + 1; vtol3j = vv; cblk_map(vv) = spc; depv_sur(vv) = VDMACC
      EXIT
   end if
end do
if (vtol3j == 0 ) then
   !WRITE(99+myid,*)'Error: atol3j not found'
   nerrs = nerrs + 1
end if

vbnz1j = 0
do spc = 1, naerp
   if (TRIM(aero_names(spc)) == 'ABNZ1J') then
      vv = vv + 1; vbnz1j = vv; cblk_map(vv) = spc; depv_sur(vv) = VDMACC
      EXIT
   end if
end do
if (vbnz1j == 0 ) then
   !WRITE(99+myid,*)'Error: abnz1j not found'
   nerrs = nerrs + 1
end if

vbnz2j = 0
do spc = 1, naerp
   if (TRIM(aero_names(spc)) == 'ABNZ2J') then
      vv = vv + 1; vbnz2j = vv; cblk_map(vv) = spc; depv_sur(vv) = VDMACC
      EXIT
   end if
end do
if (vbnz2j == 0 ) then
   !WRITE(99+myid,*)'Error: abnz2j not found'
   nerrs = nerrs + 1
end if

vbnz3j = 0
do spc = 1, naerp
   if (TRIM(aero_names(spc)) == 'ABNZ3J') then
      vv = vv + 1; vbnz3j = vv; cblk_map(vv) = spc; depv_sur(vv) = VDMACC
      EXIT
   end if
end do
if (vbnz3j == 0 ) then
   !WRITE(99+myid,*)'Error: abnz3j not found'
   nerrs = nerrs + 1
end if

vtrp1j = 0
do spc = 1, naerp
   if (TRIM(aero_names(spc)) == 'ATRP1J') then
      vv = vv + 1; vtrp1j = vv; cblk_map(vv) = spc; depv_sur(vv) = VDMACC
      EXIT
   end if
end do
if (vtrp1j == 0 ) then
   !WRITE(99+myid,*)'Error: atrp1j not found'
   nerrs = nerrs + 1
end if

vtrp2j = 0
do spc = 1, naerp
   if (TRIM(aero_names(spc)) == 'ATRP2J') then
      vv = vv + 1; vtrp2j = vv; cblk_map(vv) = spc; depv_sur(vv) = VDMACC
      EXIT
   end if
end do
if (vtrp2j == 0 ) then
   !WRITE(99+myid,*)'Error: atrp2j not found'
   nerrs = nerrs + 1
end if

viso1j = 0
do spc = 1, naerp
   if (TRIM(aero_names(spc)) == 'AISO1J') then
      vv = vv + 1; viso1j = vv; cblk_map(vv) = spc; depv_sur(vv) = VDMACC
      EXIT
   end if
end do
if (viso1j == 0 ) then
   !WRITE(99+myid,*)'Error: aiso1j not found'
   nerrs = nerrs + 1
end if

viso2j = 0
do spc = 1, naerp
   if (TRIM(aero_names(spc)) == 'AISO2J') then
      vv = vv + 1; viso2j = vv; cblk_map(vv) = spc; depv_sur(vv) = VDMACC
      EXIT
   end if
end do
if (viso2j == 0 ) then
   !WRITE(99+myid,*)'Error: aiso2j not found'
   nerrs = nerrs + 1
end if

viso3j = 0
do spc = 1, naerp
   if (TRIM(aero_names(spc)) == 'AISO3J') then
      vv = vv + 1; viso3j = vv; cblk_map(vv) = spc; depv_sur(vv) = VDMACC
      EXIT
   end if
end do
if (viso3j == 0 ) then
   !WRITE(99+myid,*)'Error: aiso3j not found'
   nerrs = nerrs + 1
end if

vsqtj = 0
do spc = 1, naerp
   if (TRIM(aero_names(spc)) == 'ASQTJ') then
      vv = vv + 1; vsqtj = vv; cblk_map(vv) = spc; depv_sur(vv) = VDMACC
      EXIT
   end if
end do
if (vsqtj == 0 ) then
   !WRITE(99+myid,*)'Error: asqtj not found'
   nerrs = nerrs + 1
end if

volgaj = 0
do spc = 1, naerp
   if (TRIM(aero_names(spc)) == 'AOLGAJ') then
      vv = vv + 1; volgaj = vv; cblk_map(vv) = spc; depv_sur(vv) = VDMACC
      EXIT
   end if
end do
if (volgaj == 0 ) then
   !WRITE(99+myid,*)'Error: aolgaj not found'
   nerrs = nerrs + 1
end if

volgbj = 0
do spc = 1, naerp
   if (TRIM(aero_names(spc)) == 'AOLGBJ') then
      vv = vv + 1; volgbj = vv; cblk_map(vv) = spc; depv_sur(vv) = VDMACC
      EXIT
   end if
end do
if (volgbj == 0 ) then
   !WRITE(99+myid,*)'Error: aolgbj not found'
   nerrs = nerrs + 1
end if

vorgcj = 0
do spc = 1, naerp
   if (TRIM(aero_names(spc)) == 'AORGCJ') then
      vv = vv + 1; vorgcj = vv; cblk_map(vv) = spc; depv_sur(vv) = VDMACC
      EXIT
   end if
end do
if (vorgcj == 0 ) then
   !WRITE(99+myid,*)'Error: aorgcj not found'
   nerrs = nerrs + 1
end if

vorgpaj = 0
do spc = 1, naerp
   if (TRIM(aero_names(spc)) == 'AORGPAJ') then
      vv = vv + 1; vorgpaj = vv; cblk_map(vv) = spc; depv_sur(vv) = VDMACC
      EXIT
   end if
end do
if (vorgpaj == 0 ) then
   !WRITE(99+myid,*)'Error: aorgpaj not found'
   nerrs = nerrs + 1
end if

vorgpai = 0
do spc = 1, naerp
   if (TRIM(aero_names(spc)) == 'AORGPAI') then
      vv = vv + 1; vorgpai = vv; cblk_map(vv) = spc; depv_sur(vv) = VDMATK
      EXIT
   end if
end do
if (vorgpai == 0 ) then
   !WRITE(99+myid,*)'Error: aorgpai not found'
   nerrs = nerrs + 1
end if

vecj = 0
do spc = 1, naerp
   if (TRIM(aero_names(spc)) == 'AECJ') then
      vv = vv + 1; vecj = vv; cblk_map(vv) = spc; depv_sur(vv) = VDMACC
      EXIT
   end if
end do
if (vecj == 0 ) then
   !WRITE(99+myid,*)'Error: aecj not found'
   nerrs = nerrs + 1
end if

veci = 0
do spc = 1, naerp
   if (TRIM(aero_names(spc)) == 'AECI') then
      vv = vv + 1; veci = vv; cblk_map(vv) = spc; depv_sur(vv) = VDMATK
      EXIT
   end if
end do
if (veci == 0 ) then
   !WRITE(99+myid,*)'Error: aeci not found'
   nerrs = nerrs + 1
end if

vp25aj = 0
do spc = 1, naerp
   if (TRIM(aero_names(spc)) == 'A25J') then
      vv = vv + 1; vp25aj = vv; cblk_map(vv) = spc; depv_sur(vv) = VDMACC
      EXIT
   end if
end do
if (vp25aj == 0 ) then
   !WRITE(99+myid,*)'Error: a25j not found'
   nerrs = nerrs + 1
end if

vp25ai = 0
do spc = 1, naerp
   if (TRIM(aero_names(spc)) == 'A25I') then
      vv = vv + 1; vp25ai = vv; cblk_map(vv) = spc; depv_sur(vv) = VDMATK
      EXIT
   end if
end do
if (vp25ai == 0 ) then
   !WRITE(99+myid,*)'Error: a25i not found'
   nerrs = nerrs + 1
end if

vantha = 0
do spc = 1, naerp
   if (TRIM(aero_names(spc)) == 'ACORS') then
      vv = vv + 1; vantha = vv; cblk_map(vv) = spc; depv_sur(vv) = VDMCOR
      EXIT
   end if
end do
if (vantha == 0 ) then
   !WRITE(99+myid,*)'Error: acors not found'
   nerrs = nerrs + 1
end if

vsoila = 0
do spc = 1, naerp
   if (TRIM(aero_names(spc)) == 'ASOIL') then
      vv = vv + 1; vsoila = vv; cblk_map(vv) = spc; depv_sur(vv) = VDMCOR
      EXIT
   end if
end do
if (vsoila == 0 ) then
   !WRITE(99+myid,*)'Error: asoil not found'
   nerrs = nerrs + 1
end if

vat0 = 0
do spc = 1, naerp
   if (TRIM(aero_names(spc)) == 'NUMATKN') then
      vv = vv + 1; vat0 = vv; cblk_map(vv) = spc; depv_sur(vv) = VDNATK
      EXIT
   end if
end do
if (vat0 == 0 ) then
   !WRITE(99+myid,*)'Error: numatkn not found'
   nerrs = nerrs + 1
end if

vac0 = 0
do spc = 1, naerp
   if (TRIM(aero_names(spc)) == 'NUMACC') then
      vv = vv + 1; vac0 = vv; cblk_map(vv) = spc; depv_sur(vv) = VDNACC
      EXIT
   end if
end do
if (vac0 == 0 ) then
   !WRITE(99+myid,*)'Error: numacc not found'
   nerrs = nerrs + 1
end if

vco0 = 0
do spc = 1, naerp
   if (TRIM(aero_names(spc)) == 'NUMCOR') then
      vv = vv + 1; vco0 = vv; cblk_map(vv) = spc; depv_sur(vv) = VDNCOR
      EXIT
   end if
end do
if (vco0 == 0 ) then
   !WRITE(99+myid,*)'Error: numcor not found'
   nerrs = nerrs + 1
end if

vsurfat = 0
do spc = 1, naerp
   if (TRIM(aero_names(spc)) == 'SRFATKN') then
      vv = vv + 1; vsurfat = vv; cblk_map(vv) = spc; depv_sur(vv) = VDSATK
      EXIT
   end if
end do
if (vsurfat == 0 ) then
   !WRITE(99+myid,*)'Error: srfatkn not found'
   nerrs = nerrs + 1
end if

vsurfac = 0
do spc = 1, naerp
   if (TRIM(aero_names(spc)) == 'SRFACC') then
      vv = vv + 1; vsurfac = vv; cblk_map(vv) = spc; depv_sur(vv) = VDSACC
      EXIT
   end if
end do
if (vsurfac == 0 ) then
   !WRITE(99+myid,*)'Error: srfacc not found'
   nerrs = nerrs + 1
end if

vsurfco = 0
do spc = 1, naerp
   if (TRIM(aero_names(spc)) == 'SRFCOR') then
      vv = vv + 1; vsurfco = vv; cblk_map(vv) = spc; depv_sur(vv) = VDSCOR
      EXIT
   end if
end do
if (vsurfco == 0 ) then
   !WRITE(99+myid,*)'Error: srfcor not found'
   nerrs = nerrs + 1
end if

vh2oaj = 0
do spc = 1, naerp
   if (TRIM(aero_names(spc)) == 'AH2OJ') then
      vv = vv + 1; vh2oaj = vv; cblk_map(vv) = spc; depv_sur(vv) = VDMACC
      EXIT
   end if
end do
if (vh2oaj == 0 ) then
   !WRITE(99+myid,*)'Error: ah2oj not found'
   nerrs = nerrs + 1
end if

vh2oai = 0
do spc = 1, naerp
   if (TRIM(aero_names(spc)) == 'AH2OI') then
      vv = vv + 1; vh2oai = vv; cblk_map(vv) = spc; depv_sur(vv) = VDMATK
      EXIT
   end if
end do
if (vh2oai == 0 ) then
   !WRITE(99+myid,*)'Error: ah2oi not found'
   nerrs = nerrs + 1
end if

vnaj = 0
do spc = 1, naerp
   if (TRIM(aero_names(spc)) == 'ANAJ') then
      vv = vv + 1; vnaj = vv; cblk_map(vv) = spc; depv_sur(vv) = VDMACC
      EXIT
   end if
end do
if (vnaj == 0 ) then
   !WRITE(99+myid,*)'Error: anaj not found'
   nerrs = nerrs + 1
end if

vnai = 0
do spc = 1, naerp
   if (TRIM(aero_names(spc)) == 'ANAI') then
      vv = vv + 1; vnai = vv; cblk_map(vv) = spc; depv_sur(vv) = VDMATK
      EXIT
   end if
end do
if (vnai == 0 ) then
   !WRITE(99+myid,*)'Error: anai not found'
   nerrs = nerrs + 1
end if

vclj = 0
do spc = 1, naerp
   if (TRIM(aero_names(spc)) == 'ACLJ') then
      vv = vv + 1; vclj = vv; cblk_map(vv) = spc; depv_sur(vv) = VDMACC
      EXIT
   end if
end do
if (vclj == 0 ) then
   !WRITE(99+myid,*)'Error: aclj not found'
   nerrs = nerrs + 1
end if

vcli = 0
do spc = 1, naerp
   if (TRIM(aero_names(spc)) == 'ACLI') then
      vv = vv + 1; vcli = vv; cblk_map(vv) = spc; depv_sur(vv) = VDMATK
      EXIT
   end if
end do
if (vcli == 0 ) then
   !WRITE(99+myid,*)'Error: acli not found'
   nerrs = nerrs + 1
end if

vnak = 0
do spc = 1, naerp
   if (TRIM(aero_names(spc)) == 'ANAK') then
      vv = vv + 1; vnak = vv; cblk_map(vv) = spc; depv_sur(vv) = VDMCOR
      EXIT
   end if
end do
if (vnak == 0 ) then
   !WRITE(99+myid,*)'Error: anak not found'
   nerrs = nerrs + 1
end if

vclk = 0
do spc = 1, naerp
   if (TRIM(aero_names(spc)) == 'ACLK') then
      vv = vv + 1; vclk = vv; cblk_map(vv) = spc; depv_sur(vv) = VDMCOR
      EXIT
   end if
end do
if (vclk == 0 ) then
   !WRITE(99+myid,*)'Error: aclk not found'
   nerrs = nerrs + 1
end if

vso4k = 0
do spc = 1, naerp
   if (TRIM(aero_names(spc)) == 'ASO4K') then
      vv = vv + 1; vso4k = vv; cblk_map(vv) = spc; depv_sur(vv) = VDMCOR
      EXIT
   end if
end do
if (vso4k == 0 ) then
   !WRITE(99+myid,*)'Error: aso4k not found'
   nerrs = nerrs + 1
end if

vnh4k = 0
do spc = 1, naerp
   if (TRIM(aero_names(spc)) == 'ANH4K') then
      vv = vv + 1; vnh4k = vv; cblk_map(vv) = spc; depv_sur(vv) = VDMCOR
      EXIT
   end if
end do
if (vnh4k == 0 ) then
   !WRITE(99+myid,*)'Error: anh4k not found'
   nerrs = nerrs + 1
end if

vno3k = 0
do spc = 1, naerp
   if (TRIM(aero_names(spc)) == 'ANO3K') then
      vv = vv + 1; vno3k = vv; cblk_map(vv) = spc; depv_sur(vv) = VDMCOR
      EXIT
   end if
end do
if (vno3k == 0 ) then
   !WRITE(99+myid,*)'Error: ano3k not found'
   nerrs = nerrs + 1
end if

vh2ok = 0
do spc = 1, naerp
   if (TRIM(aero_names(spc)) == 'AH2OK') then
      vv = vv + 1; vh2ok = vv; cblk_map(vv) = spc; depv_sur(vv) = VDMCOR
      EXIT
   end if
end do
if (vh2ok == 0 ) then
   !WRITE(99+myid,*)'Error: ah2ok not found'
   nerrs = nerrs + 1
end if

if (vv /= naerp) then
   error%nError   = IV_ERROR
   error%eRoutine = 'init_aero_map'
   error%eMessage = 'Particle species mapping incomplete'
   go to 9999
end if

if (nerrs > 0) then
   error%nError   = IV_ERROR
   error%eRoutine = 'init_aero_map'
   WRITE(error%eMessage,'(A,I3,A)')'Error in particle species mapping for ',nerrs, &
                            ' species'
   go to 9999
end if

! *** Set additional species contained only in CBLK not needed in CBLK_MAP.

vv = vv + 1; vsgat = vv
vv = vv + 1; vsgac = vv
vv = vv + 1; vsgco = vv

vv = vv + 1; vdgat = vv
vv = vv + 1; vdgac = vv
vv = vv + 1; vdgco = vv

vv = vv + 1; vat2 = vv
vv = vv + 1; vac2 = vv
vv = vv + 1; vco2 = vv

vv = vv + 1; vat3 = vv
vv = vv + 1; vac3 = vv
vv = vv + 1; vco3 = vv

vv = vv + 1; vsulf = vv
vv = vv + 1; vhno3 = vv
vv = vv + 1; vnh3  = vv
vv = vv + 1; vn2o5 = vv
vv = vv + 1; vhcl  = vv

vv = vv + 1; vhplusi = vv
vv = vv + 1; vhplusj = vv
vv = vv + 1; vhplusk = vv

vv = vv + 1; vno2  = vv
vv = vv + 1; vhono = vv

vv = vv + 1; vvalk  = vv
vv = vv + 1; vvxyl1 = vv
vv = vv + 1; vvxyl2 = vv
vv = vv + 1; vvtol1 = vv
vv = vv + 1; vvtol2 = vv
vv = vv + 1; vvbnz1 = vv
vv = vv + 1; vvbnz2 = vv
vv = vv + 1; vvtrp1 = vv
vv = vv + 1; vvtrp2 = vv
vv = vv + 1; vviso1 = vv
vv = vv + 1; vviso2 = vv
vv = vv + 1; vvsqt  = vv

vv = vv + 1; vmea   = vv
vv = vv + 1; vamp   = vv
vv = vv + 1; vpz    = vv

9999 continue

return
end
