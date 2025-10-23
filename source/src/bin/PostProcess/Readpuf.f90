MODULE readpuf_inc

  USE localpuf
  USE DefSize_fd

  IMPLICIT NONE

  SAVE

  INTEGER, PARAMETER ::  MAXN = 201

  CHARACTER(PATH_MAXLENGTH) rname, wname

  CHARACTER(1) wide, narrow, output, star, blank, underscore
  CHARACTER(2) screen

  CHARACTER(8), DIMENSION(MAXN) :: vname, namey

  LOGICAL new_time
  LOGICAL new_input
  LOGICAL new_file
  LOGICAL new_output
  LOGICAL new_skip
  LOGICAL new_max
  LOGICAL new_min
  LOGICAL new_variable
  LOGICAL lminmax

  INTEGER nskip  ,nmax  ,nmin  ,nerr  ,ninu  ,npru
  INTEGER nchr   ,nvar  ,itype, ovar
  INTEGER itxt
  INTEGER ihead

  REAL    time

  INTEGER MAX_MC
  INTEGER, DIMENSION(:), ALLOCATABLE :: mcIndex
  INTEGER, DIMENSION(:), ALLOCATABLE :: mcMatIndex

END MODULE readpuf_inc

!==============================================================================

SUBROUTINE Readpuf( ToolUserID )

USE readpuf_inc
USE param_fd
USE metparam_fd
USE errorParam_fd
USE tooluser_fd
USE SCIPtool
USE chem_fi, ONLY: chemMC, chem
USE files_fi
USE class_fd
USE scipuff_fi

IMPLICIT NONE

!  this program prints the contents of a puff plot file
!  for a specified (or all) variable(s).

INTEGER, INTENT( IN ) :: ToolUserID

SAVE

INTEGER, PARAMETER :: MAX_AUX = NAUX_TOTALCC + NAUX_DYNAMICS + NAUX_BUOY + NAUX_DENSE + NAUX_LIQUID + NAUX_AEROSOL
INTEGER, PARAMETER :: NP1     = NP_ALL+1
INTEGER, PARAMETER :: NPA1    = NP1+MAX_AUX

CHARACTER(16), DIMENSION(:), ALLOCATABLE :: xppn, auxn
CHARACTER(16)                            :: name1,name2
CHARACTER(80)                            :: xtitle

INTEGER NPM1
INTEGER ioff, i, j, k, noutx, ncht, nchv,  ixpp, nvxpp
INTEGER ipuf, npxpp, mmax, nchx, iend, irv, ios
INTEGER ixbar, iybar, n1, n2, n8, i1_8, i2_8
INTEGER ID, n, nmc
CHARACTER(8) cindex, ctag
LOGICAL lwrite, lout8

INTEGER, DIMENSION(:), ALLOCATABLE :: ivar
REAL, DIMENSION(:)   , ALLOCATABLE :: pdum
REAL, DIMENSION(:,:) , ALLOCATABLE :: pmnmx
REAL, DIMENSION(:,:) , ALLOCATABLE :: rdata

REAL(8), DIMENSION(2) :: pdum8

TYPE( pflagsT )    flags
TYPE( ppuffHeadT ) puffHead

INTEGER, EXTERNAL :: GetProjectPuffs
INTEGER, EXTERNAL :: GetProjectPlotTimes

2000    FORMAT(/,' ERROR OPENING ',A)
2100    FORMAT(/,' INCORRECT NUMBER OF VARIABLES : NVAR =',I5, &
                 ' : MAX = ',I3)
2200    FORMAT(/,' ',A,' IS NOT ON THIS FILE',/, &
                 ' VARIABLE RESET TO "ALL "',/, &
                 ' VARIABLES ARE ',10(A,1X),(:,/,'               ',10(A,1X)))
2300    FORMAT(/,' ERROR OPENING ',A,' : OUTPUT SET TO SCREEN')
2400    FORMAT(/,' ERROR READING ',A)
3000    FORMAT('1',/,'       FILE : ',A)
3100    FORMAT('      TITLE : ',A)
3101    FORMAT('       TIME :',1PE12.4,/'  NO. PUFFS : ',I6,/)
3200    FORMAT('   No.  ',8X,7(A15,1X):/(16X,7(A15,1X)))
3201    FORMAT('   No.  ',8X,4(A15,1X):/(16X,4(A15,1X)))
3300    FORMAT(I6,2X,1P7E16.4:/(8X,1P7E16.4))
3301    FORMAT(I6,2X,1P4E16.4:/(8X,1P4E16.4))
3302    FORMAT(I6,2X,1P7E26.12:/(8X,1P7E26.14))
3303    FORMAT(I6,2X,1P4E26.12:/(8X,1P4E26.14))
3332    FORMAT(' ')
3333    FORMAT(' ',I4,'. Time = ',1PE11.4,'  No. Puffs =',I6)
4201    FORMAT('  Variable      Min. Value    Puff', &
               '        Max. Value    Puff')
4202    FORMAT('     Time         Variable      Min. Value    Puff', &
               '        Max. Value    Puff')
4301    FORMAT(' ',2X,A8,1X,2(1PE18.4,0P,I8))
4302    FORMAT(' ',1PE12.4,6X,A8,1X,2(1PE18.4,0P,I8))

!==== Size parameters ( Copied from inittool )

MAXPUF = limit%puffs

IF( MAXPUF <= 0 )MAXPUF = MAXPUF_DEF

!------ program initialization

ios        = 0
itxt       = -1  ! Text output flag
wide       ='W'
narrow     ='N'
screen     ='TT'
underscore ='_'
blank      =' '
star       ='*'
itype      = -999
ninu       = 5
nerr       = 6
npru       = 6

CALL init_error()
CALL set_version( iversion_code )

rname        = blank
nchr         = 1
wname        = screen
output       = narrow
nskip        = 1
nmin         = 1
nmax         = MAXPUF
nvar         = 1
namey(1)     = 'ALL'
new_input    = .TRUE.
new_output   = .FALSE.
new_file     = .TRUE.
new_time     = .TRUE.
time         = -999.
new_variable = .TRUE.
lminmax      = .FALSE.
ovar         = 0

ihead = 1

!------ get keywords

KeyWord: DO

IF( itxt < 0 )THEN
  WRITE(6,'(/," For ASCII output use TXT:filename as output filename",/)')
  itxt = 0
END IF

iend = 0
CALL get_keyword( iend )
IF( iend /= 0 )EXIT

new_output = .FALSE.

!------ open new input file

IF( new_input )THEN

  lun_prj = 2
  file_prj = rname(1:nchr-3)//'prj'

  CALL SplitName( file_prj,puffHead%project%name,puffHead%project%path )

  puffHead%project%name    = puffHead%project%name(1:LEN_TRIM(puffHead%project%name)-4)
  puffHead%project%path    = puffHead%project%path
  puffHead%Project%ID      = 0
  puffHead%Project%version = 0

  irv = GetProjectPlotTimes( puffHead%project )
  IF( irv /= SCIPSuccess )THEN
    WRITE(6,*)' Error reading puff times'
    CYCLE KeyWord
  ELSE IF( nTimePuff == 0 )THEN
    WRITE(6,*)' Cannot find any valid puff times'
    GOTO 99
  END IF

  irv = GetProjectPuffs( ToolUserID,puffHead,1,.TRUE.,.TRUE.,0 )

  MAX_MC = 0
  IF( mat_mc%nMCtype > 0 )THEN
    IF( ALLOCATED(mcIndex) )DEALLOCATE( mcIndex,STAT=ios )
    IF( ios == 0 .AND. ALLOCATED(mcMatIndex) )DEALLOCATE( mcmatIndex,STAT=ios )
    IF( ios == 0 )ALLOCATE( mcIndex(mat_mc%nMCtype ),STAT=ios )
    IF( ios == 0 )ALLOCATE( mcmatIndex(mat_mc%nMCtype ),STAT=ios )
    IF( ios /= 0 )THEN
      WRITE(6,*) 'Error allocating mcIndex arrays', mat_mc%nMCtype
      GOTO 99
    END IF
    DO i = 1,ntypp
      IF( typeID(i)%mcID > 0 )mcMatIndex(typeID(i)%mcID) = typeID(i)%imat
    END DO
    DO i = 1,mat_mc%nMCtype
      mcIndex(i) = NPA1 + MAX_MC
      ID  = mat_mc%ID(i)
      SELECT CASE( mat_mc%type(i) )
        CASE( MC_CHEM )
          chem => chemMC(ID,1)
          nmc = chem%nFast + chem%nSlow + chem%nParticle + chem%nEquilibrium
          MAX_MC = MAX_MC + 2.*nmc + chem%nStar + 4
        CASE DEFAULT
      END SELECT
    END DO
  END IF
  NPM1 = NPA1 + MAX_MC

  npxpp = 0
  DO i = 1,nTimePuff
    npxpp = npxpp + timepuff(i)%nitems
  ENDDO

  IF( ALLOCATED(rdata) )DEALLOCATE( rdata,STAT=ios )
  IF( ios == 0 )ALLOCATE( rdata(NPM1+2,npxpp),STAT=ios )
  IF( ios /= 0 )THEN
    WRITE(6,*) 'Error allocating rdata array', npuf
    GOTO 99
  END IF

  IF( ALLOCATED(xppn) )DEALLOCATE( xppn,STAT=ios )
  IF( ios == 0 )ALLOCATE( xppn(NPM1+2),STAT=ios )
  IF( ios /= 0 )THEN
    WRITE(6,*) 'Error allocating xppn array with size', NPM1+2
    GOTO 99
  END IF

  IF( ALLOCATED(auxn) )DEALLOCATE( auxn,ivar,pdum,STAT=ios )
  IF( ios == 0 )ALLOCATE( auxn(NPM1),ivar(NPM1),pdum(NPM1),STAT=ios )
  IF( ios /= 0 )THEN
    WRITE(6,*) 'Error allocating auxn array', NPM1
    GOTO 99
  END IF

  IF( ALLOCATED(pmnmx) )DEALLOCATE( pmnmx,STAT=ios )
  IF( ios == 0 )ALLOCATE( pmnmx(NPM1,5),STAT=ios )
  IF( ios /= 0 )THEN
    WRITE(6,*) 'Error allocating pmnmx array', NPM1,5
    GOTO 99
  END IF

  CALL init_param()

  j = 1
  DO i = 1,NP_ALL
    IF( TRIM(names(i)) == 'GRID' )THEN
      auxn(j) = names(i)
      j = j + 1
      auxn(j) = 'MFLD'
    ELSE
      auxn(j) = names(i)
    END IF
    j = j + 1
  END DO
  ioff = 0
  auxn(NP1+ioff+1) ='CCTB'
  auxn(NP1+ioff+2) ='CCT '
  ioff = ioff + NAUX_TOTALCC
  auxn(NP1+ioff+1) ='WCB '
  auxn(NP1+ioff+2) ='CTB '
  auxn(NP1+ioff+3) ='WCP '
  auxn(NP1+ioff+4) ='CTP '
  auxn(NP1+ioff+5) ='UCB '
  auxn(NP1+ioff+6) ='VCB '
  auxn(NP1+ioff+7) ='UCP '
  auxn(NP1+ioff+8) ='VCP '
  auxn(NP1+ioff+9) ='UP  '
  auxn(NP1+ioff+10) ='VP  '
  auxn(NP1+ioff+11) ='WP  '
  auxn(NP1+ioff+12) ='TP  '
  ioff = ioff + NAUX_DYNAMICS
  auxn(NP1+ioff+1) ='BCB '
  auxn(NP1+ioff+2) ='BCP '
  ioff = ioff + NAUX_BUOY
  auxn(NP1+ioff+1) ='UDYN'
  auxn(NP1+ioff+2) ='VDYN'
  auxn(NP1+ioff+3) ='DUDX'
  auxn(NP1+ioff+4) ='DUDY'
  auxn(NP1+ioff+5) ='DVDX'
  auxn(NP1+ioff+6) ='DVDY'
  auxn(NP1+ioff+7) ='U0  '
  auxn(NP1+ioff+8) ='MAJ '
  auxn(NP1+ioff+9) ='MIN '
  auxn(NP1+ioff+10) ='SN '
  auxn(NP1+ioff+11) ='CS '
  ioff = ioff + NAUX_DENSE
  auxn(NP1+ioff+1) ='D   '
  auxn(NP1+ioff+2) ='SIGD'
  auxn(NP1+ioff+3) ='TL  '
  auxn(NP1+ioff+4) ='CCS '
  auxn(NP1+ioff+5) ='FLD '
  auxn(NP1+ioff+6) ='CCBG'
  ioff = ioff + NAUX_LIQUID
  auxn(NP1+ioff+1) ='FL '
  auxn(NP1+ioff+2) ='TVAP'
  auxn(NP1+ioff+3) ='CO '
  auxn(NP1+ioff+4) ='FW '

  IF( MAX_MC > 0 )THEN
    ctag = ' '
    DO i = 1,mat_mc%nMCtype
      IF( mat_mc%nMCtype > 1 )THEN
        WRITE(ctag,'(I6,A1)')mcMatIndex(i),'_'
        ctag = 'M'//ADJUSTL(ctag)
      END IF
      ID  = mat_mc%ID(i)
      SELECT CASE( mat_mc%type(i) )
        CASE( MC_CHEM )
          chem => chemMC(ID,1)
          nmc = chem%nFast + chem%nSlow + chem%nParticle + chem%nEquilibrium
          n = mcIndex(i)
          DO j = 1,nmc
            n = n + 1
            auxn(n) = TRIM(ctag)//'M_'//chem%species(j)%name
            n = n + 1
            auxn(n) = TRIM(ctag)//'A_'//chem%species(j)%name
            IF( chem%species(j)%lstar)THEN
              n = n + 1
              auxn(n) = TRIM(ctag)//'C_'//chem%species(j)%name
            END IF
          END DO
          n = n + 1
          auxn(n) = TRIM(ctag)//'VOL'
          n = n + 1
          auxn(n) = TRIM(ctag)//'PRES'
          n = n + 1
          auxn(n) = TRIM(ctag)//'TEMP'
          n = n + 1
          auxn(n) = TRIM(ctag)//'VSELF'
        CASE DEFAULT
      END SELECT
    END DO
  END IF

  irv = SCIPDefaultFlagsF( ToolUserID,flags )
  IF( irv /= SCIPsuccess )THEN
    WRITE(6,*)' Error setting default flags'
    GOTO 99
  END IF

  flags%project = puffHead%project

  irv = SCIPLoadFlagsF( ToolUserID,flags )
  IF( irv /= SCIPsuccess )THEN
    WRITE(6,*)' Error reading flags'
    GOTO 99
  END IF
  title = flags%flags%audit%title

  ncht = 1
  DO i = 40,1,-1
    IF( title(i:i) /= ' ' )THEN
      ncht = i
      EXIT
    END IF
  END DO

  new_input = .FALSE.

!-------- read file variables

  new_variable = .TRUE.
  new_time     = .TRUE.
  DO i = 1,NPM1
    j = 0
    DO
      j = j+1
      IF( j <= 4 )THEN
        IF( auxn(i)(j:j) == blank )THEN
          auxn(i)(j:j) = underscore
          CYCLE
        END IF
      END IF
      EXIT
    END DO
  END DO

END IF

!------ open output file

IF( new_file )THEN
  new_file = .FALSE.
  IF( wname == screen )THEN
    noutx = 6
    ixpp  = 0
  ELSE
    noutx = 6
    ixpp  = 1
  END IF
END IF

!------ get new variable

IF( new_variable )THEN

  new_variable = .FALSE.

  lout8 = .FALSE.

  IF( TRIM(namey(1)) == 'ALL' )THEN
    nvar = NPM1
    DO i = 1,NPM1
      ivar(i) = i
    END DO
  ELSE
    IF(ovar > 0)THEN
      nvar = 1
      ivar(nvar) = 0
      DO i = 1,NPM1
        name2 = auxn(i)
        CALL cupper( name2 )
        DO j = 1,ovar
          name1 = namey(j)
          IF( INDEX(name1,'_8') /= 0 )THEN
            lout8 = .TRUE.
            name1 = name1(1:INDEX(name1,'_8')-1)
          END IF
          CALL cupper( name1 )
          IF( TRIM(name1) == TRIM(name2))THEN
            ivar(nvar) = i
            EXIT
          END IF
        END DO
        IF( ivar(nvar)==0 )THEN
          ivar(nvar) = i
          nvar = nvar + 1
        END IF
        ivar(nvar) = 0
      END DO
      nvar = nvar - 1
    ELSE
      DO j = 1,nvar
        ivar(j) = 0
        name1 = namey(j)
        IF( INDEX(name1,'_8') /= 0 )THEN
          lout8 = .TRUE.
          name1 = name1(1:INDEX(name1,'_8')-1)
        END IF
        CALL cupper( name1 )
        DO i = 1,NPM1
         name2 = auxn(i)
         CALL cupper( name2 )
         IF( TRIM(name1) == TRIM(name2))ivar(j) = i
        END DO
        IF( ivar(j) == 0 )THEN
          nchv = INDEX(namey(j),blank) - 1
          IF( nchv <= 0)nchv = 4
          WRITE(nerr,2200)TRIM(namey(j)),(TRIM(auxn(i)),i=1,NPM1)
          nvar = NPM1
          DO i = 1,NPM1
            ivar(i) = i
          END DO
          CYCLE KeyWord
        END IF
      END DO
    END IF
  END IF
END IF

!------ new time

IF( new_time )THEN

  new_time = .FALSE.
  IF( time >= 0.0 )THEN
    i = 0
    DO WHILE( i < nTimePuff )
      i = i + 1
      IF( TimePuff(i)%time%runTime >= time ) EXIT
    END DO
    IF( i > 1 )THEN
      IF( ABS(TimePuff(i-1)%time%runTime-time) < ABS(TimePuff(i)%time%runTime-time) )&
        i = i - 1
    END IF
    i = MAX(1,i)
    irv = GetProjectPuffs( ToolUserID,puffHead,i,.TRUE.,.TRUE.,0 )
    IF( irv /= SCIPsuccess )THEN
      WRITE(6,*)' Error reading project puffs at time break ',i
      GOTO 99
    END IF
    dynamic   = (puffHead%puff%dynamic == SCIPtrue)
    dense_gas = (puffHead%puff%dense   == SCIPtrue)
    buoy_gas  = (puffHead%puff%buoy    == SCIPtrue)
    CALL set_auxskp()
  END IF

END IF

!------ output

WRITE(noutx,3000)rname(1:nchr)
WRITE(noutx,3100)title(1:ncht)

lwrite = .FALSE.

IF( time < -1.0 )THEN

  ihead = 2

  IF( ixpp == 1 )THEN
    nvxpp = 2
    xppn(1) ='TIME'
    xppn(2) ='NPUF'
  END IF
  WRITE(noutx,3332)

  i = 0
  DO WHILE( i < nTimePuff )
    i = i + 1
    t     = TimePuff(i)%time%runTime
    npuf  = TimePuff(i)%nItems
    WRITE(noutx,3333)i,t,npuf
    IF( ixpp == 1 )THEN
      rdata(1,i) = t
      rdata(2,i) = npuf
      npxpp = i
    END IF
  END DO

ELSE

  ihead = 1
  IF( time >= 0.0 )THEN
    WRITE(noutx,3101)t,npuf

    IF( .NOT.lminmax )THEN
      IF( output == wide )THEN
        WRITE(noutx,3200) (auxn(ivar(i)),i=1,nvar)
      ELSE
        WRITE(noutx,3201) (auxn(ivar(i)),i=1,nvar)
      END IF
    ELSE
      WRITE(noutx,4201)
      DO i = 1,NP1
        pmnmx(i,1) =  1.E+36
        pmnmx(i,2) = 0.
        pmnmx(i,3) = -1.E+36
        pmnmx(i,4) = 0.
      END DO
    END IF

    IF( ixpp == 1 )THEN
      nvxpp = nvar + 1
      xppn(1) = 'IPUF'
      DO i = 1,nvar
        xppn(i+1) = auxn(ivar(i))
      END DO
      npxpp = 0
    END IF

    mmax = MIN(nmax,npuf)

    ixbar = 0
    iybar = 0
    IF( lout8 )THEN
      DO i = 1,nvar
        IF( ivar(i) == 1 )ixbar = i
        IF( ivar(i) == 2 )iybar = i
      END DO
      IF( ixbar /= 0 )THEN
        IF( iybar /= 0 )THEN
          IF (ixbar < iybar )THEN
            n1 = ixbar
            i1_8 = 1
            IF( iybar == ixbar + 1 )THEN
              n2 = 0
              n8 = 2
              i2_8 = 2
            ELSE
              n2 = iybar
              n8 = 1
              i2_8 = 2
            END IF
          ELSE
            n1 = iybar
            i1_8 = 2
            IF( ixbar == iybar + 1 )THEN
              n2 = 0
              n8 = 2
              i2_8 = 1
            ELSE
              n8 = 1
              i2_8 = 1
            END IF
          END IF
        ELSE
          n1 = ixbar
          n2 = 0
          n8 = 1
          i1_8 = 1
        END IF
      ELSE IF( iybar /= 0 )THEN
        n1 = iybar
        n2 = 0
        n8 = 1
        i1_8 = 2
      ELSE
        n1 = 0
        n2 = 0
        n8 = 0
      END IF
    ELSE
      n1 = 0
    END IF

    DO ipuf = nmin,mmax,nskip
      IF( itype == -999 .OR. itype == puff(ipuf)%ityp )THEN
        DO i = 1,NPM1
          pdum = 1.E36
        END DO
        CALL set_pdum( puff(ipuf),pdum,pdum8,MAX_AUX )
        IF( ixpp == 1 )THEN
          npxpp = npxpp + 1
          rdata(1,npxpp) = ipuf
          DO i = 1,nvar
            rdata(i+1,npxpp) = pdum(ivar(i))
          END DO
        ELSE
          IF( .NOT.lminmax )THEN
            IF( output == wide )THEN
              IF( n1 == 0 )THEN
                WRITE(noutx,3300) ipuf,(pdum(ivar(i)),i=1,nvar)
              ELSE IF( n2 == 0 )THEN
                IF( n8 == 1 )THEN
                  WRITE(noutx,3302) ipuf,(pdum(ivar(i)),i=1,n1-1),pdum8(i1_8),(pdum(ivar(i)),i=n1+1,nvar)
                ELSE
                  WRITE(noutx,3302) ipuf,(pdum(ivar(i)),i=1,n1-1),pdum8(i1_8),pdum8(i2_8),(pdum(ivar(i)),i=n1+2,nvar)
                END IF
              ELSE
                WRITE(noutx,3302) ipuf,(pdum(ivar(i)),i=1,n1-1),pdum8(i1_8),(pdum(ivar(i)),i=n1+1,n2-1),&
                                  pdum8(i2_8),(pdum(ivar(i)),i=n2+1,nvar)
              END IF
            ELSE
              IF( n1 == 0 )THEN
                WRITE(noutx,3301) ipuf,(pdum(ivar(i)),i=1,nvar)
              ELSE IF( n2 == 0 )THEN
                IF( n8 == 1 )THEN
                  WRITE(noutx,3303) ipuf,(pdum(ivar(i)),i=1,n1-1),pdum8(i1_8),(pdum(ivar(i)),i=n1+1,nvar)
                ELSE
                  WRITE(noutx,3303) ipuf,(pdum(ivar(i)),i=1,n1-1),pdum8(i1_8),pdum8(i2_8),(pdum(ivar(i)),i=n1+2,nvar)
                END IF
              ELSE
                WRITE(noutx,3303) ipuf,(pdum(ivar(i)),i=1,n1-1),pdum8(i1_8),(pdum(ivar(i)),i=n1+1,n2-1),&
                                  pdum8(i2_8),(pdum(ivar(i)),i=n2+1,nvar)
              END IF
            END IF
          ELSE
            DO i = 1,nvar
              j = ivar(i)
              IF( pdum(j) > pmnmx(j,3) )THEN
                pmnmx(j,3) = pdum(j)
                pmnmx(j,4) = FLOAT(ipuf)
              END IF
              IF( pdum(j) < pmnmx(j,1) )THEN
                pmnmx(j,1) = pdum(j)
                pmnmx(j,2) = FLOAT(ipuf)
              END IF
            END DO
          END IF
        END IF
      END IF
    END DO
    IF( lminmax )THEN
       DO i = 1,nvar
         j = ivar(i)
         WRITE(noutx,4301)auxn(j),pmnmx(j,1),NINT(pmnmx(j,2)), &
                             pmnmx(j,3),NINT(pmnmx(j,4))
       END DO
    END IF
  ELSE

    ihead = 2

    IF( .NOT.lminmax )THEN
      IF( output == wide )THEN
        WRITE(noutx,3200)'TIME            ',(auxn(ivar(i)),i=1,nvar)
      ELSE
        WRITE(noutx,3201)'TIME            ',(auxn(ivar(i)),i=1,nvar)
      END IF
    ELSE
      WRITE(noutx,4202)
      DO i = 1,NP1
        pmnmx(i,1) =  1.E+36
        pmnmx(i,2) = 0.
        pmnmx(i,3) = -1.E+36
        pmnmx(i,4) = 0.
      END DO
    END IF

    IF( ixpp == 1 )THEN
      nvxpp = nvar + 2
      xppn(1) = 'IPUF'
      xppn(2) = 'TIME'
      DO i = 1,nvar
        xppn(i+2) = auxn(ivar(i))
      END DO
      npxpp = 0
    END IF

    IF( ixpp == -1 .AND. time < 0. )THEN
      lwrite = .TRUE.
      WRITE(xtitle,2998) title(1:ncht),npuf
      nchx = ncht + 14
      CALL cupper( xtitle(1:ncht) )
      OPEN( unit=3,file=wname,status='UNKNOWN' )
      WRITE(3,*)nvxpp
      WRITE(3,'(10A8)')(xppn(i),i=1,nvxpp)
      WRITE(3,*)xtitle(1:nchx)
    END IF

    DO k = 1,nTimePuff

      irv = GetProjectPuffs( ToolUserID,puffHead,k,.TRUE.,.TRUE.,0 )
      IF( irv /= SCIPsuccess )THEN
        WRITE(6,*)'Error reading project puffs at time break ',k
        GOTO 99
      END IF
      dynamic   = (puffHead%puff%dynamic == SCIPtrue)
      dense_gas = (puffHead%puff%dense   == SCIPtrue)
      buoy_gas  = (puffHead%puff%buoy    == SCIPtrue)
      CALL set_auxskp()

      mmax = MIN(nmax,npuf)

      DO ipuf = nmin,mmax,nskip
        IF( itype == -999 .OR. itype == puff(ipuf)%ityp )THEN
          DO i = 1,NPM1
            pdum = 1.E36
          END DO
          CALL set_pdum( puff(ipuf),pdum,pdum8,MAX_AUX )
          IF( ixpp == 1 .AND. npxpp < 4*MAXPUF )THEN
            npxpp = npxpp + 1
            rdata(1,npxpp) = ipuf
            rdata(2,npxpp) = t
            DO i = 1,nvar
              rdata(i+2,npxpp) = pdum(ivar(i))
            END DO
          ELSE
            IF( .NOT.lminmax )THEN
              IF( output == wide )THEN
                WRITE(noutx,3300) ipuf,t,(pdum(ivar(i)),i=1,nvar)
              ELSE
                WRITE(noutx,3301) ipuf,t,(pdum(ivar(i)),i=1,nvar)
              END IF
            ELSE
              DO i = 1,nvar
                j = ivar(i)
                IF( pdum(j) > pmnmx(j,3) )THEN
                  pmnmx(j,3) = pdum(j)
                  pmnmx(j,4) = FLOAT(ipuf)
                END IF
                IF( pdum(j) < pmnmx(j,1) )THEN
                  pmnmx(j,1) = pdum(j)
                  pmnmx(j,2) = FLOAT(ipuf)
                END IF
              END DO
            END IF
          END IF
        END IF
      END DO
      IF( lminmax )THEN
        DO i = 1,nvar
          j = ivar(i)
          WRITE(noutx,4302)t,auxn(j),pmnmx(j,1),NINT(pmnmx(j,2)), &
                                 pmnmx(j,3),NINT(pmnmx(j,4))
        END DO
      END IF
      IF( ixpp == -1 .AND. time < 0. )THEN
        DO j = 1,npxpp
          WRITE(3,'(1P5E15.7)')(rdata(i,j),i=1,nvxpp)
        END DO
        npxpp = 0
      END IF


    END DO

  END IF

END IF

1999 CONTINUE

IF( ixpp == 1 .AND. .NOT.lwrite )THEN
  IF( time >= 0. )THEN
    WRITE(xtitle,2999) title(1:ncht),t,npuf
2999 FORMAT(a,' : T=',1pe11.4,' : NPUF=',i6)
    nchx = ncht + 16 + 14
  ELSE
    WRITE(xtitle,2998) title(1:ncht),npuf
2998 FORMAT(A,' : NPUF=',i6)
    nchx = ncht + 14
  END IF
  CALL cupper( xtitle(1:ncht) )
  SELECT CASE( itxt )
    CASE( 1 )
      CALL write_ascii( 3,TRIM(wname),NPM1+2,nvxpp,npxpp,xppn,xtitle,rdata,0 )
    CASE( 2 )
      CALL write_csv( 3,TRIM(wname),NPM1+2,nvxpp,npxpp,xppn,xtitle,rdata,0 )
    CASE( 3 )
      CALL write_dbg( 3,TRIM(wname),NPM1+2,nvxpp,npxpp,xppn,xtitle,rdata,0,ihead )
    CASE DEFAULT
      CALL write_xpp( 3,TRIM(wname),NPM1+2,nvxpp,npxpp,xppn,xtitle,rdata,0 )
  END SELECT
END IF

CYCLE KeyWord

300 CONTINUE
WRITE(nerr,2000)TRIM(wname)
CYCLE KeyWord

99 CONTINUE
WRITE(nerr,2000)rname(1:nchr)

END DO KeyWord

888 CONTINUE

CALL ClearProjectPuffs()

IF( ALLOCATED(xppn)  )DEALLOCATE( xppn, STAT=ios )
IF( ALLOCATED(auxn)  )DEALLOCATE( auxn, STAT=ios )
IF( ALLOCATED(rdata) )DEALLOCATE( rdata,STAT=ios )
IF( ALLOCATED(ivar)  )DEALLOCATE( ivar, STAT=ios )
IF( ALLOCATED(pdum)  )DEALLOCATE( pdum, STAT=ios )
IF( ALLOCATED(pmnmx) )DEALLOCATE( pmnmx,STAT=ios )
IF( ALLOCATED(mcIndex) )DEALLOCATE( mcIndex,STAT=ios )
IF( ALLOCATED(mcMatIndex) )DEALLOCATE( mcMatIndex,STAT=ios )

RETURN
END

!==============================================================================

SUBROUTINE get_pdum( p,pdum,pdum8 )

USE struct_fd

REAL pdum(*)
REAL(8) pdum8(2)
TYPE( puff_str ) p

INTEGER igrd,ifld

INTEGER, EXTERNAL :: getPuffifld, getPuffipgrd

pdum8(1) = p%xbar
pdum8(2) = p%ybar

pdum(1) = SNGL(p%xbar)
pdum(2) = SNGL(p%ybar)
pdum(3) = p%zbar
pdum(4) = p%sxx
pdum(5) = p%sxy
pdum(6) = p%sxz
pdum(7) = p%syy
pdum(8) = p%syz
pdum(9) = p%szz
pdum(10) = p%axx
pdum(11) = p%axy
pdum(12) = p%axz
pdum(13) = p%ayy
pdum(14) = p%ayz
pdum(15) = p%azz
pdum(16) = p%det
pdum(17) = p%c
pdum(18) = p%cc
pdum(19) = p%xuc
pdum(20) = p%xvc
pdum(21) = p%yvc
pdum(22) = p%yvsc
pdum(23) = p%yvbc
pdum(24) = p%zwc
pdum(25) = p%wc
pdum(26) = p%ccb
pdum(27) = p%si
pdum(28) = p%si2
pdum(29) = p%sv
pdum(30) = p%sr
pdum(31) = p%cfo
pdum(32) = p%zi
pdum(33) = p%zc
pdum(34) = p%uo
pdum(35) = p%vo
pdum(36) = p%wo

pdum(37) = FLOAT(p%ityp)
pdum(38) = FLOAT(p%inxt)
pdum(39) = FLOAT(p%iprv)
ifld = getPuffifld( p )
igrd = getPuffipgrd( p )
pdum(40) = FLOAT(igrd)
pdum(41) = FLOAT(ifld) !Put met field in grid level
pdum(42) = FLOAT(p%idtl)
pdum(43) = FLOAT(p%idtn)
pdum(44) = FLOAT(p%naux)

RETURN
END

!==============================================================================

SUBROUTINE set_pdum( p,pdum,pdum8,naux )

USE scipuff_fi
USE step_p_fi, ONLY: myTid
USE readpuf_inc, ONLY: mcIndex, MAX_MC
USE chem_fi, ONLY: chemMC, chem, vol, pres, temp, vself

IMPLICIT NONE

INTEGER, PARAMETER :: NP1 = NP_ALL+1

TYPE( puff_str ) p
INTEGER naux

REAL pdum(*)
REAL(8) pdum8(2)

TYPE( puff_dynamics )      pd
TYPE( puff_dynamics_data ) pdx
TYPE( puff_totalcc  )      pt
TYPE( puff_liquid )        pq
TYPE( puff_aerosol )       pa

INTEGER i,ip,NPA1
INTEGER ID, ntot, mcID
INTEGER ncomp, nvar, ios

LOGICAL, EXTERNAL :: IsLiquid, IsWetParticle
LOGICAL, EXTERNAL :: IsAerosol

NPA1 = NP1 + naux

CALL get_pdum( p,pdum,pdum8 )

IF( p%naux /= 0 )THEN
  IF( typeID(p%ityp)%ltot )THEN
    CALL get_totalcc( p,pt )
    pdum(NP1+1) = pt%cctb
    pdum(NP1+2) = pt%cct
  ELSE
    pdum(NP1+1) = 1.E36
    pdum(NP1+2) = 1.E36
  END IF
  IF( dynamic )THEN
    CALL get_dynamics( p,pd )
    pdx = TRANSFER(pd,pdx)
    ip = 2
    DO i = 1,NAUX_DYNAMICS
      pdum(NP1+i+ip) = pdx%data(i)
    END DO
    ip = ip + NAUX_DYNAMICS
    IF( buoy_gas )THEN
      DO i = 1,NAUX_BUOY
        pdum(NP1+i+ip) = pdx%data_buoy(i)
      END DO
    ELSE
      DO i = 1,NAUX_BUOY
        pdum(NP1+i+ip) = 1.E+36
      END DO
    END IF
    ip = ip + NAUX_BUOY
    IF( dense_gas )THEN
      DO i = 1,NAUX_DENSE
        pdum(NP1+i+ip) = pdx%data_dense(i)
      END DO
    ELSE
      DO i = 1,NAUX_DENSE
        pdum(NP1+i+ip) = 1.E36
      END DO
    END IF
  ELSE
    DO i = 1,NAUX_DYNAMICS+NAUX_BUOY+NAUX_DENSE
      pdum(NP1+i+2) = 1.E36
    END DO
  END IF
  ip = NAUX_DYNAMICS+NAUX_BUOY+NAUX_DENSE + NAUX_TOTALCC
  IF( IsLiquid(typeID(p%ityp)%icls) .OR. IsWetParticle(typeID(p%ityp)%icls) )THEN
    CALL get_liquid( p,pq )
    pdum(NP1+ip+1) = pq%d
    pdum(NP1+ip+2) = pq%sigd
    pdum(NP1+ip+3) = pq%t
    pdum(NP1+ip+4) = pq%ccs
    pdum(NP1+ip+5) = pq%tevap
  ELSE
    pdum(NP1+ip+1) = 1.E36
    pdum(NP1+ip+2) = 1.E36
    pdum(NP1+ip+3) = 1.E36
    pdum(NP1+ip+4) = 1.E36
    pdum(NP1+ip+5) = 1.E36
  END IF
  ip = NAUX_DYNAMICS+NAUX_BUOY+NAUX_DENSE + NAUX_TOTALCC + NAUX_LIQUID
  IF( IsAerosol(typeID(p%ityp)%icls) )THEN
    CALL get_aerosol( p,pa )
    pdum(NP1+ip+1) = pa%fl
    pdum(NP1+ip+2) = pa%tevap
    pdum(NP1+ip+3) = pa%co
    pdum(NP1+ip+4) = pa%fw
  ELSE
    pdum(NP1+ip+1) = 1.E36
    pdum(NP1+ip+2) = 1.E36
    pdum(NP1+ip+3) = 1.E36
    pdum(NP1+ip+4) = 1.E36
  END IF

  IF( MAX_MC > 0 )THEN
    DO i = 1,MAX_MC
      pdum(NPA1+i) = 1.E36
    END DO
    mcID = typeID(p%ityp)%mcID
    ID = mat_mc%ID(mcID)
    IF( mat_mc%type(ID) == MC_CHEM )THEN
      myTid = 1
      chem => chemMC(ID,1)
      CALL GetChemAux( ID,p )
      ncomp = chem%nFast + chem%nSlow + chem%nEquilibrium
      nvar = mcIndex(mcID)
      DO i = 1,ncomp
        nvar = nvar + 1
        pdum(nvar) = chem%species(i)%mass
        nvar = nvar + 1
        pdum(nvar) = chem%species(i)%amb
        IF( chem%species(i)%lstar)THEN
          nvar = nvar + 1
          pdum(nvar) = chem%species(i)%conc
        END IF
      END DO
      nvar = nvar + 1
      pdum(nvar) = vol
      nvar = nvar + 1
      pdum(nvar) = pres
      nvar = nvar + 1
      pdum(nvar) = temp
      nvar = nvar + 1
      pdum(nvar) = vself
    END IF
  END IF

END IF

9999 CONTINUE
RETURN
END

!==============================================================================

SUBROUTINE get_keyword( iend )

INTEGER, INTENT( INOUT ) :: iend

LOGICAL c_decode
INTEGER nlen, ikey, nkey, nk
INTEGER nb, nch

CHARACTER(196) line
CHARACTER(1)   blank

blank = ' '

!------ main loop

9999 CONTINUE

!     prepare to issue prompt and DO read-no-echo if input is not
!     the graphics device or if prompt flag set.

line = blank
WRITE(6,6001)
6001 FORMAT(/'Keyword : ',$)

!     issue read

READ(5,5001,END=9001,ERR=9000)line
5001 FORMAT(A)

line = ADJUSTL(line)
nlen = LEN_TRIM(line)

GOTO 10
9000 STOP'ERROR READING USER OPTION'
9001 STOP'ENCOUNTERED END OF FILE WHILE READING OPTIONS'

10 CONTINUE

!     change keyword to upper case

ikey = INDEX(line(1:nlen),blank) - 1
IF( ikey < 0 )ikey = nlen
CALL cupper( line(1:ikey) )

!     check for no switch

IF( line(1:2) =='NO' )line(1:) = line(3:)

!     check for valid responce

IF( .NOT.c_decode(line,nkey,nk) )THEN

!     did not receive a valid response

  nb = LEN_TRIM(line)
  IF( nkey == -1 )THEN
    WRITE(6,*)'Ambiguous option : '//line(1:nb)
  ELSE IF( nkey==-2 )THEN
    WRITE(6,*)'Nonexistent option : '//line(1:nb)
  ELSE
    WRITE(6,*)'Blank lines are ignored !'
  END IF
  GOTO 9999
END IF

!     good command - at least send a blank

IF( nk == 0 )THEN
  nb = LEN_TRIM(line)
  nk = nb+1
  line(nk:nk) = blank
  nlen = nk
  nch  = 1
ELSE
  nb   = LEN_TRIM(line(nk:)) - 1
  nlen = MIN(nk + nb,nlen)
  nch  = nlen - nk + 1
END IF

!     sent command to appropriate subroutine

GOTO( 100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400 ),nkey

100     CALL get_input( nch,line(nk:nlen) ) !input file
GOTO 9999

200     CALL get_filekw( nch,line(nk:nlen) ) !output file
GOTO 9999

300     CALL get_output( line(nk:nlen) ) !output width
GOTO 9999

400     CALL get_skip( nch,line(nk:nlen) ) !nskip
GOTO 9999

500     CALL get_variable( nch,line(nk:nlen) ) !variable
GOTO 9999

600     CALL get_ignore( nch,line(nk:nlen) ) !variable
GOTO 9999

700     CALL get_all( nch,line(nk:nlen) )       !go
RETURN

800     CONTINUE
iend = 99
RETURN

900     CALL get_help()                !help
GOTO 9999

1000     CALL get_time( nch,line(nk:nlen) ) !time
GOTO 9999

1100    CALL get_max( nch,line(nk:nlen) ) !nmax
GOTO 9999

1200    CALL get_min( nch,line(nk:nlen) ) !nmin
GOTO 9999

1300    CALL get_type( nch,line(nk:nlen) ) !itype
GOTO 9999

1400    CALL get_mval() !Min/Max
GOTO 9999

END

!==============================================================================

SUBROUTINE get_input( nch,line )

USE readpuf_inc

IMPLICIT NONE

INTEGER,      INTENT( IN ) :: nch
CHARACTER(*), INTENT( IN ) :: line

LOGICAL lerror
INTEGER ncht

CHARACTER(196) temp
CHARACTER(13)  dprompt

dprompt = ' Data file : '

IF( line(1:nch) == blank )GOTO 100
temp = line(1:nch)
ncht = nch

200 CONTINUE
nchr = ncht
rname = temp(1:ncht)
INQUIRE( FILE=rname(1:nchr),EXIST=lerror )
IF( .NOT.lerror )THEN
  WRITE(npru,*)'File not found - ',rname(1:nchr)
  GOTO 100
END IF

WRITE(npru,'(A)') ' Data will be read from : '//rname(1:nchr)
new_input = .TRUE.

RETURN

100     WRITE(npru,3000) dprompt
3000    FORMAT(' ',A,$)
READ(ninu,*)temp
ncht = LEN_TRIM(temp)
IF( ncht > 0 )GOTO 200

RETURN
END

!==============================================================================

SUBROUTINE get_filekw( nch,line )

USE readpuf_inc

IMPLICIT NONE

INTEGER,      INTENT( IN ) :: nch
CHARACTER(*), INTENT( IN ) :: line

LOGICAL lerror
INTEGER ncht

CHARACTER(196) temp
CHARACTER(4)   ext
CHARACTER(256) file

IF( line(1:nch) == blank )GOTO 100
temp = line(1:nch)

itxt = 0  !Default output format is binary XPP

ncht = INDEX(temp,':')

IF( ncht > 1 )THEN

  wname = temp(ncht+1:nch)
  wname = ADJUSTL(TRIM(wname))

  ext = temp(1:ncht-1)
  CALL cupper( ext )
  SELECT CASE( ext )
    CASE( 'XPP' )
      itxt = 0
    CASE( 'TXT' )
      itxt = 1
    CASE( 'CSV' )
      itxt = 2
    CASE( 'DBG' )
      itxt = 3
    CASE DEFAULT
      itxt = 1
  END SELECT

ELSE

  wname = temp(1:nch)
  wname = ADJUSTL(TRIM(wname))

  CALL SplitExtension( wname,file,ext )
  CALL cupper( ext )
  SELECT CASE( TRIM(ext) )
    CASE( 'XPP' )
      itxt = 0
    CASE( 'TXT' )
      itxt = 1
    CASE( 'CSV' )
      itxt = 2
    CASE( 'DBG' )
      itxt = 3
    CASE DEFAULT
      itxt = 0
  END SELECT

END IF

INQUIRE( FILE=TRIM(wname),EXIST=lerror )
IF( lerror )THEN
  WRITE(npru,*)'File already exists - ',TRIM(wname)
  wname = screen
END IF

201 CONTINUE

SELECT CASE( itxt )
  CASE( 0 )
    ext = 'XPP'
  CASE( 1 )
    ext = 'TXT'
  CASE( 2 )
    ext = 'CSV'
  CASE( 3 )
    ext = 'DBG'
  CASE DEFAULT
    ext = 'XPP'
END SELECT

IF( wname == screen )THEN
  WRITE(npru,2000)
ELSE
  WRITE(npru,2100)TRIM(wname),TRIM(ext)
END IF
new_file   = .TRUE.
new_output = .TRUE.
2000    format (' Output will written to the terminal')
2100    format (' Output will written to : ',a,' (',a,')')

RETURN

100     wname = screen
GOTO 201

END

!==============================================================================

SUBROUTINE get_output( line )

USE readpuf_inc

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: line

output = line(1:1)
IF( output == blank )GOTO 100

200 CONTINUE

IF( output == wide )THEN
  WRITE(npru,2000)
ELSE
  WRITE(npru,2100)
END IF

2000 FORMAT (' 132 Column output ')
2100 FORMAT (' 80 Column output ')
new_output = .TRUE.

RETURN

100 CONTINUE
output = narrow
GOTO 200

END

!==============================================================================

SUBROUTINE get_skip( nch,line )

USE readpuf_inc

IMPLICIT NONE

INTEGER,      INTENT( IN ) :: nch
CHARACTER(*), INTENT( IN ) :: line

CHARACTER(9) dprompt

dprompt = ' Nskip : '

IF( line == blank )GOTO 100
READ(line(1:nch),1000)nskip
1000 FORMAT(I10)

200 CONTINUE
nskip = MAX(1,nskip)
WRITE(npru,2000) nskip
2000 FORMAT(' Nskip = ',I5)
new_skip = .TRUE.

RETURN

100 CONTINUE
WRITE(npru,3000) dprompt
3000 FORMAT(' ',A,$)
READ(ninu,*)nskip
GOTO 200

END

!==============================================================================

SUBROUTINE get_max( nch,line )

USE readpuf_inc

IMPLICIT NONE

INTEGER,      INTENT( IN ) :: nch
CHARACTER(*), INTENT( IN ) :: line

CHARACTER(9) dprompt

dprompt = ' Nmax : '

IF( line == blank )GOTO 100
READ(line(1:nch),1000)nmax
1000 FORMAT(I10)

200 CONTINUE
nmax = MAX(1,nmax)
WRITE(npru,2000) nmax
2000 FORMAT(' NMAX = ',I5)
new_max = .TRUE.

RETURN

100 CONTINUE
WRITE(npru,3000) dprompt
3000 FORMAT(' ',A,$)
READ(ninu,*)nmax
GOTO 200

END

!==============================================================================

SUBROUTINE get_min( nch,line )

USE readpuf_inc

IMPLICIT NONE

INTEGER,      INTENT( IN ) :: nch
CHARACTER(*), INTENT( IN ) :: line

CHARACTER(9) dprompt

dprompt = ' Nmin : '

IF( line == blank )GOTO 100
READ(line(1:nch),1000)nmin
1000 FORMAT(I10)

200 CONTINUE
nmin = MAX(1,nmin)
WRITE(npru,2000) nmin
2000 FORMAT(' NMIN = ',I5)
new_min = .TRUE.

RETURN

100 CONTINUE
WRITE(npru,3000) dprompt
3000 FORMAT(' ',A,$)
READ(ninu,*)nmin
GOTO 200

END

!==============================================================================

SUBROUTINE get_variable( nch,line )

USE readpuf_inc

IMPLICIT NONE

INTEGER,      INTENT( INOUT ) :: nch
CHARACTER(*), INTENT( INOUT ) :: line

INTEGER i, iend, nchv

CHARACTER(1)  comma
CHARACTER(13) dprompt
CHARACTER(80) linex

dprompt = ' Variables : '
comma   = ','

IF( line(1:nch) == blank )GOTO 100

DO i = 1,nch
  IF( line(i:i) == comma)line(i:i)=blank
END DO
linex = line

200 CONTINUE
nvar = 0
ovar = 0

1  CONTINUE

IF( linex(1:1) == blank )THEN
  linex(1:) = linex(2:)
  nch = nch - 1
  IF( nch <= 0 )THEN
    IF( nvar == 0 )THEN
      GOTO 100
    ELSE
      GOTO 300
    END IF
  END IF
  GOTO 1
END IF

nvar = nvar + 1
iend = INDEX(linex(1:nch),blank) - 1
IF( iend <= 0)iend = nch
namey(nvar) = linex(1:iend)
nchv = LEN_TRIM(namey(nvar))
IF( nchv < 4)namey(nvar)(nchv+1:4)=blank
CALL cupper( namey(nvar) )
linex(1:)=linex(iend+2:)
nch = nch - (iend + 1)
IF( nch <= 0 )GOTO 300
GOTO 1

300 CONTINUE
WRITE(npru,2000) (TRIM(namey(i)),i=1,nvar)
2000 FORMAT(' Variables = ',10(A8,1X),:,/,13X,10(A8,1X),:, &
   /,13X,10(A8,1X),:,/,13X,10(A8,1X),:,/,13X,10(A8,1X))
new_variable = .TRUE.

RETURN

100 CONTINUE
WRITE(npru,3000) dprompt
3000 FORMAT(' ',A,$)
READ(ninu,*)linex
nch = LEN_TRIM(linex)
IF( linex(1:nch) == blank)GOTO 100
GOTO 200

RETURN
END

!==============================================================================

SUBROUTINE get_ignore( nch,line )

USE readpuf_inc

IMPLICIT NONE

INTEGER,      INTENT( INOUT ) :: nch
CHARACTER(*), INTENT( INOUT ) :: line

INTEGER i, iend, nchv

CHARACTER(1)  comma
CHARACTER(13) dprompt
CHARACTER(80) linex

dprompt = ' Ignore : '
comma   = ','

IF( line(1:nch) == blank )GOTO 100

DO i = 1,nch
  IF( line(i:i) == comma)line(i:i)=blank
END DO
linex = line

200 CONTINUE
ovar = 0

1  CONTINUE

IF( linex(1:1) == blank )THEN
  linex(1:) = linex(2:)
  nch = nch - 1
  IF( nch <= 0 )THEN
    IF( nvar == 0 )THEN
      GOTO 100
    ELSE
      GOTO 300
    END IF
  END IF
  GOTO 1
END IF

ovar = ovar + 1
iend = INDEX(linex(1:nch),blank) - 1
IF( iend <= 0)iend = nch
namey(ovar) = linex(1:iend)
nchv = LEN_TRIM(namey(ovar))
IF( nchv < 4)namey(ovar)(nchv+1:4)=blank
CALL cupper( namey(ovar) )
linex(1:)=linex(iend+2:)
nch = nch - (iend + 1)
IF( nch <= 0 )GOTO 300
GOTO 1

300 CONTINUE
WRITE(npru,2000) (TRIM(namey(i)),i=1,ovar)
2000 FORMAT(' Ignore = ',10(A8,1X),:,/,13X,10(A8,1X),:, &
   /,13X,10(A8,1X),:,/,13X,10(A8,1X),:,/,13X,10(A8,1X))
new_variable = .TRUE.

RETURN

100 CONTINUE
WRITE(npru,3000) dprompt
3000 FORMAT(' ',A,$)
READ(ninu,*)linex
nch = LEN_TRIM(linex)
IF( linex(1:nch) == blank)GOTO 100
GOTO 200

RETURN
END

!==============================================================================

SUBROUTINE get_type( nch,line )

USE readpuf_inc
!USE scipuff_fi, ONLY: typeID, ntypp

IMPLICIT NONE

INTEGER,      INTENT( INOUT ) :: nch
CHARACTER(*), INTENT( IN    ) :: line

CHARACTER(24) dprompt
CHARACTER(80) linex

dprompt = ' Itype (All=-999) : '

IF( line(1:nch) == blank )GOTO 100
linex = line

200 CONTINUE
READ(linex(1:nch),*) itype
!IF( itype == -1 )THEN
!  WRITE(npru,2000)'Rain'
!ELSE IF( itype == 0 )THEN
!  WRITE(npru,2000)'Smoke'
!ELSE IF( itype > 0 )THEN
IF( itype > 0 )THEN
  WRITE(npru,2001) itype
ELSE IF( itype == -999 )THEN
  WRITE(npru,2002)
ELSE
  GOTO 100
END IF

2000 FORMAT(1X,'Output ',A,' puffs')
!2001 FORMAT(1X,'Output Dust Group',I2,' puffs')
2001 FORMAT(1X,'Output Type',I2,' puffs.')
2002 FORMAT(1X,'Output All puffs')

RETURN

100 CONTINUE
WRITE(npru,3000) dprompt
3000 FORMAT(' ',A,$)
READ(ninu,1000)linex
1000 FORMAT(A)
nch = LEN_TRIM(linex)

IF( linex(1:nch) == blank )GOTO 100
GOTO 200

END

!==============================================================================

SUBROUTINE get_all( nch,line )

USE readpuf_inc

IMPLICIT NONE

INTEGER,      INTENT( INOUT ) :: nch
CHARACTER(*), INTENT( INOUT ) :: line

INTEGER i, iend

CHARACTER(1) comma

comma = ','

DO i = 1,nch
  IF( line(i:i) == comma)line(i:i)=blank
END DO

IF( line == blank )THEN
  RETURN
ELSE
1 CONTINUE
  IF( line(1:1) == blank )THEN
    line(1:) = line(2:)
    nch = nch - 1
    IF( nch <= 0 )RETURN
    GOTO 1
  END IF
  iend = INDEX(line(1:nch),blank) - 1
  IF( iend <= 0)iend = nch
  CALL get_input( iend,line(1:iend) )
  nch = nch - (iend + 1)
  IF( nch <= 0 )RETURN
  line(1:) = line(iend+2:)
END IF

IF( line == blank )THEN
  RETURN
ELSE
31 CONTINUE
  IF( line(1:1) == blank )THEN
    line(1:) = line(2:)
    nch = nch - 1
    IF( nch <= 0 )RETURN
    GOTO 31
  END IF
  iend = INDEX(line(1:nch),blank) - 1
  IF( iend <= 0 )iend = nch
  CALL get_time( iend,line(1:iend) )
  nch = nch - (iend + 1)
  IF( nch <= 0 )RETURN
  line(1:) = line(iend+2:)
END IF

IF( line == blank )THEN
  RETURN
ELSE
3 CONTINUE
  IF( line(1:1) == blank )THEN
    line(1:) = line(2:)
    nch = nch - 1
    IF( nch <= 0 )RETURN
    GOTO 3
  END IF
  iend = INDEX(line(1:nch),blank) - 1
  IF( iend <= 0 )iend = nch
  CALL get_skip( iend,line(1:iend) )
  nch = nch - (iend + 1)
  IF( nch <= 0 )RETURN
  line(1:) = line(iend+2:)
END IF

IF( line == blank )THEN
  RETURN
ELSE
6 CONTINUE
  IF( line(1:1) == blank )THEN
    line(1:) = line(2:)
    nch = nch - 1
    IF( nch <= 0 )RETURN
    GOTO 6
  END IF
  iend = INDEX(line(1:nch),blank) - 1
  IF( iend <= 0 )iend = nch
  CALL get_output( line(1:iend) )
  nch = nch - (iend + 1)
  IF( nch <= 0 )RETURN
  line(1:) = line(iend+2:)
END IF

IF( line == blank )THEN
  RETURN
ELSE
5 CONTINUE
  IF( line(1:1) == blank )THEN
    line(1:) = line(2:)
    nch = nch - 1
    IF( nch <= 0 )RETURN
    GOTO 5
  END IF
  iend = INDEX(line(1:nch),blank) - 1
  IF( iend <= 0 )iend = nch
  CALL get_filekw( iend,line(1:iend) )
END IF

RETURN
END

!==============================================================================

SUBROUTINE get_help()

USE readpuf_inc

IMPLICIT NONE

WRITE(npru,1000)
1000    format(/,' VALID KEYWORDS AND INPUTS ',//, &
'     KEYWORD       PARAMETERS         PURPOSE ',/ &
'     =======       ==========         ====================',/, &
'  1. READ          Filename           Data File to be read',/, &
'  2. FILE          Filename           Output to filename',/, &
'                   TT or Blank        Output to terminal',/, &
'  3. OUTPUT        W(IDE)             132 Column Output',/, &
'                   N(ARROW)           80 Column Output',/, &
'  4. SKIP          Nskip              Read only every Nskip',/, &
'  5. VARIABLE      Name               Desired variable',/, &
'                   ALL                All variables',/, &
'  6. IGNORE        Name               Desired variables to ignore',/, &
'  7. EXIT          None               Stop',/, &
'  8. TIME          Time               Time of desired output',/, &
'  9. HELP          None               Type this message',/, &
' 10. GO            See note           Generate output',/, &
' 11. MAXIMUM       Nmax               Stop read at Nmax',/, &
' 12. MINIMUM       Nmin               Start read at Nmin',/, &
' 13. TYPE          R,D,A              Rain,dust or all puffs',/, &
' 14. MVALUE        None               Toggle Min/Max value',/, &
'  ',/, &
'  Note : GO Keyword will accept input for keywords 1-4 + 7 ',/, &
'         in the following order READ TIME SKIP OUTPUT FILE .',/, &
'         The inputs may be separated by either',/, &
'         commas or blanks. Not all options need be included',/,&
'         but none may be skipped. For example',/, &
'         GO filename 10. 3      is allowed but',/, &
'         GO filename,,3         is not.',/)

RETURN
END

!==============================================================================

SUBROUTINE get_time( nch,line )

USE readpuf_inc

IMPLICIT NONE

INTEGER,      INTENT( IN ) :: nch
CHARACTER(*), INTENT( IN ) :: line

CHARACTER(13) dprompt

dprompt = ' Timebreak : '

IF( line == blank )GOTO 100
READ(line(1:nch),1000)time
1000 FORMAT(F10.0)

200 CONTINUE
WRITE(npru,2000) time
2000 FORMAT (' Timebreak = ',1PE12.5)
new_time = .TRUE.

RETURN

100 CONTINUE
WRITE(npru,3000) dprompt
3000 FORMAT(' ',A,$)
READ(ninu,*)time
GOTO 200

END

!==============================================================================

SUBROUTINE get_mval()

USE readpuf_inc

IMPLICIT NONE

lminmax = .NOT.lminmax
WRITE(npru,3000) lminmax
3000 FORMAT(' Min/Max Value flag = ',L1)

RETURN
END

!==============================================================================

LOGICAL FUNCTION c_decode( line,n,nk )

IMPLICIT NONE

!
!     this LOGICAL function examines line to determine
!     which option it represents.  c_decode RETURNs
!     true unless line contains an invalid option.
!     on return, n is the option number, and nk is the
!     offset in line at which any parameters begin.
!     if there are no parameters, nk is RETURNed as 0.
!
CHARACTER(*), INTENT( INOUT ) :: line
INTEGER,      INTENT( OUT   ) :: n
INTEGER,      INTENT( OUT   ) :: nk

INTEGER, PARAMETER :: nspecs=14

CHARACTER(9), DIMENSION(nspecs) :: commands

LOGICAL, EXTERNAL :: find_option

DATA commands/'READ'           ,'FILE'         ,'OUTPUT' &
             ,'SKIP'           ,'VARIABLE'     ,'IGNORE' &
             ,'GO'             ,'EXIT'         ,'HELP' &
             ,'TIME','MAXIMUM','MINIMUM'       ,'TYPE' &
             ,'MVALUE' /

c_decode = find_option( line,commands,nspecs,n,nk )

RETURN
END

!==============================================================================

LOGICAL FUNCTION find_option( line,commands,nspecs,n,np )

IMPLICIT NONE

!    this LOGICAL function examines line to determine
!    which option it represents.  find_option RETURNs
!    true unless line contains an invalid option.
!    on return, n is the option number, and np is the
!    offset in line at which any parameters begin.
!    if there are no parameters, np is RETURNed as 0.
!    commands contains the list of options to look for.
!    nspecs is the number of options in the list.

INTEGER,      INTENT( IN    ) :: nspecs
CHARACTER(*), INTENT( INOUT ) :: line
CHARACTER(*), INTENT( IN    ) :: commands(nspecs)
INTEGER,      INTENT( OUT   ) :: n, np

INTEGER i, j, lc, nb
LOGICAL hit

!    skip over initial blanks

nb = 1
10 CONTINUE
  IF( line(nb:nb) /= ' ' )GOTO 20
  nb = nb + 1
  IF( LEN(line)<nb )GOTO 58   ! bad value
  GOTO 10
20 CONTINUE
DO i = nb,LEN(line)
  IF( line(i:i) == ' ' )GOTO 28
END DO
i = LEN(line)+1
28 CONTINUE
lc = i - nb

!    now we need to find the rest of the line

np = INDEX(line(nb:),' ')
IF( np == 0 )GOTO 80
np = np + nb - 1
70 CONTINUE
IF( line(np:np) /= ' ' )GOTO 80
np = np + 1
IF( LEN(line) >= np )GOTO 70
np = 0
80 CONTINUE

!    now that we know where the parameters start, we can convert
!    the command part to upper case without any trouble!

IF( np > 1 )CALL cupper( line(1:np-1) )

!    check whether a valid response was given

DO 40 i = 1,LEN(commands(1))

!    first we check for 1 CHARACTER matches, THEN 2s, up to the option le > .
!    the checking is terminated when we find only one match
!    at a given level.

   hit = .FALSE.

!       initialize to no matches yet at this level

!       if(len(line)<nb+i-1)GO TO 55   !  ambiguous at previous level
   IF( lc < i )GOTO 55   !  ambiguous at previous level

!       make sure we don't exceed LINE le >

  DO j = 1,nspecs
    IF( line(nb:nb+i-1) == commands(j)(1:i) )THEN
    ! we found a match
       IF( hit )GOTO 40
       ! if we already had one at this level, THEN we
       ! need to look at more levels to resolve it
       hit = .TRUE.
       ! otherwise, we retain this command's number
       n = j
     END IF
  END DO
   IF( hit )THEN
   ! we found a unique match
     GOTO 60
   ELSE
   ! we found no matches. abort the search.
     GOTO 50    ! bad value
   END IF
40 CONTINUE

!    not a valid option

50 find_option = .FALSE.
n = -2
RETURN
55 find_option = .FALSE.
n = -1
RETURN
58 find_option = .FALSE.
n = -3
RETURN
60 CONTINUE
find_option = .TRUE.

RETURN
END

!==============================================================================
!Local copy to prevent link issues
!
SUBROUTINE get_DegradeMC( p,n,comp )

  USE scipuff_fi

  IMPLICIT NONE

  TYPE( puff_str ),   INTENT( IN  ) :: p
  INTEGER,            INTENT( IN  ) :: n
  REAL, DIMENSION(n), INTENT( OUT ) :: comp

  INTEGER i, i0

  i0 = typeID(p%ityp)%ipmc - 1

  DO i = 1,n
    comp(i) = p%aux(i0+i)
  END DO

  RETURN

END SUBROUTINE get_DegradeMC
