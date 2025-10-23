!=======================================================================
!    SCIPUFF definitions - multicomponent
!=======================================================================
MODULE chemReactions_fd
!------ List of CHEM reaction rate coefficient types

  INTEGER, PARAMETER :: ID_K_RAD       = 0   !Radiation dependent
  INTEGER, PARAMETER :: ID_K_CONST     = 1   !Constant
  INTEGER, PARAMETER :: ID_K_TEMP      = 2   !Temperature dependent
  INTEGER, PARAMETER :: ID_K_PRES      = 3   !Pressure dependent
  INTEGER, PARAMETER :: ID_K_H2O       = 4   !H2O dependent
  INTEGER, PARAMETER :: ID_K_M         = 5   !M dependent
  INTEGER, PARAMETER :: ID_K_LWC       = 6   !LWC (and drop diam) dependent
  INTEGER, PARAMETER :: ID_K_PRESB     = 7   !Pressure (type B) dependent
  INTEGER, PARAMETER :: ID_K_EQM       = 8   !Reverse decomposition rate dependent on forward rate
  INTEGER, PARAMETER :: ID_K_O2        = 9   !O2 dependent
  INTEGER, PARAMETER :: ID_K_N2        = 10  !N2 dependent
  INTEGER, PARAMETER :: ID_K_FALLOFF1  = 11  !Falloff (type 1) reaction rate
  INTEGER, PARAMETER :: ID_K_FALLOFF2  = 12  !Falloff (type 2) reaction rate
  INTEGER, PARAMETER :: ID_K_FALLOFF3  = 13  !Falloff (type 3) reaction rate
  INTEGER, PARAMETER :: ID_K_CH4       = 14  !CH4 dependent
  INTEGER, PARAMETER :: ID_K_H2OB      = 15  !H2O (type 2) dependent
  !--- Reaction types for CB05
  INTEGER, PARAMETER :: ID_K_O2_M      = 16  !O2 & M dependent
  INTEGER, PARAMETER :: ID_K_H2OSQ     = 17  !H2O dependent (type 3)
  INTEGER, PARAMETER :: ID_K_FALLOFF4  = 18  !Falloff (type 4)
  INTEGER, PARAMETER :: ID_K_FALLOFF5  = 19  !Falloff (type 5)
  INTEGER, PARAMETER :: ID_K_FALLOFF6  = 20  !Falloff (type 6)
  INTEGER, PARAMETER :: ID_K_H2        = 21  !H2 dependent
  INTEGER, PARAMETER :: ID_K_TEMPB     = 22  !Temperature dependent (type B)
  !--- Reaction types for cb6r2
  INTEGER, PARAMETER :: ID_K_FALLOFF7  = 23  !Falloff (type 7)
  INTEGER, PARAMETER :: ID_K_M2        = 24  !M dependent

END MODULE chemReactions_fd

!-------------------------------------------------------------------------

SUBROUTINE ReadChemReactions( lun,file )


USE scipuff_fi
USE chem_fi
USE chemReactions_fd
USE reallocate

!====   Reads multi-component equation definitions from project.imc

IMPLICIT NONE

INTEGER,      INTENT( IN ) :: lun
CHARACTER(*), INTENT( IN ) :: file

CHARACTER(1), PARAMETER :: SET_MODE = '#'
CHARACTER(1), PARAMETER :: BLANK    = ' '
CHARACTER(1), PARAMETER :: TAB      = '     '
CHARACTER(1), PARAMETER :: SEMI     = ';'
CHARACTER(2), PARAMETER :: PLUS     = '+ '
CHARACTER(2), PARAMETER :: EQLS     = '->'

INTEGER nch, ncc, iplus, ieqls, isemi, itype, itherm, ireact
INTEGER ios, i, nP, iP, ik, nreact, irv, alloc_stat, ndat
REAL    fac, facB

CHARACTER(4)   string
CHARACTER(512) kstring, rstring
CHARACTER(512) line, original
CHARACTER(512), DIMENSION(:),POINTER ::  rline

LOGICAL lerr

TYPE( ChemReact_str ), POINTER :: r
#if defined(LINUX)

INTEGER, EXTERNAL  :: RemoveCF
#endif

INTERFACE

  SUBROUTINE ReallocateChemReactions( nreact,line,rline )
    USE error_fi
    IMPLICIT NONE
    INTEGER, INTENT( IN )                 :: nreact
    CHARACTER(512), INTENT( IN )          :: line
    CHARACTER(512), DIMENSION(:), POINTER :: rline
  END SUBROUTINE ReallocateChemReactions

END INTERFACE

nreact   = 0
eRoutine = 'ReadChemReactions'

!---- Read species data

DO

  READ(lun,'(A)',IOSTAT=ios) line

  ! Check for end of file in all compilers
  IF( IS_IOSTAT_END(ios) )ios = -1

  IF( ios /= 0 )THEN
    IF( ios < 0 )EXIT
    nError   = RD_ERROR
    eMessage = 'Error reading Multi-component input file'
    CALL ReportFileName( eInform,'File=',file )
    GOTO 9998
  END IF
#if defined(LINUX)
  ios = RemoveCF(line)
#endif

  CALL cupper( line )
  IF( line(1:1) == SET_MODE )EXIT

!====   Increment number of reactions

  nreact = nreact + 1
  CALL ReallocateChemReactions( nreact,line,rline )
  IF( nError /= NO_ERROR )GOTO 9998

END DO

ALLOCATE( chem%reaction(nreact),STAT=alloc_stat )
IF( alloc_stat /= 0 )GOTO 9998
DO i = 1,nreact
  NULLIFY( chem%reaction(i)%data )
  NULLIFY( chem%reaction(i)%P )
  NULLIFY( chem%reaction(i)%fP )
END DO

DO ireact = 1,nreact

  line = rline(ireact)

!====   Change TABS to Blanks for reading

  nch = LEN_TRIM(line)
  CALL get_value( line,nch,TAB,BLANK,lerr )
  original = TRIM(line)
  IF( nch > LEN(original) )original(LEN(original)-2:) = '...'

!====   Locate special characters

  iplus = INDEX(line,PLUS)
  ieqls = INDEX(line,EQLS)
  isemi = INDEX(line,SEMI)
  IF( ieqls <= 0 )THEN
    eMessage = 'Error finding -> sign'
    GOTO 9999
  END IF
  IF( isemi <= 0 )THEN
    eMessage = 'Error finding semicolon'
    GOTO 9999
  END IF

!====   Strip off rate data

  kstring      = line(isemi+1:)
  line(isemi:) = BLANK

!====   Read reaction ID

  CALL get_i( line,nch,ik,lerr )
  IF( lerr )THEN
    eMessage = 'Error reading reactionID'
    GOTO 9999
  END IF

!====   Check ID for uniqueness

  IF( ik /= ireact )THEN
    eMessage = 'Reaction IDs must be in sequence'
    GOTO 9999
  END IF

  r => chem%reaction(ireact)

!====   Initialize class bits to zero

  r%class = 0

!====   Get first reactant

  CALL GetChemSpec( line,nch,fac,r%A )
  IF( nError /= NO_ERROR )GOTO 9998

!====   Get second reactant [optional]

  CALL get_c( line,nch,BLANK,string,ncc,lerr )
  IF( lerr )THEN
    eMessage = 'Error reading reactants'
    GOTO 9999
  END IF

  SELECT CASE( string(1:2) )
    CASE( EQLS )
      r%B  = 0
      r%fB = 0.0

    CASE( PLUS )
      CALL GetChemSpec( line,nch,r%fB,r%B )
      IF( nError /= NO_ERROR )GOTO 9998
      CALL get_c( line,nch,BLANK,string,ncc,lerr )
      IF( lerr .OR. string(1:2) /= EQLS )THEN
        eMessage = 'Error reading reactants'
        GOTO 9999
      END IF

    CASE DEFAULT
      eMessage = 'Unknown character in reaction'
      GOTO 9999

  END SELECT

!====   Get product names

  nP = 0
  DO WHILE( nch > 0 )
    nP = nP + 1
    IF( nP == 1 )THEN
      ALLOCATE( r%P(1),r%fP(1),STAT=irv )
    ELSE
      irv = reallocate_integer1d( r%P,1 )
      IF( irv == 0 )irv = reallocate_real1d( r%fP,1 )
    END IF
    IF( irv /= 0 )THEN
      nError   = UK_ERROR
      eMessage = 'Error reallocating reaction product storage'
      GOTO 9997
    END IF
    CALL GetChemSpec( line,nch,r%fP(nP),r%P(nP) )
    IF( nError /= NO_ERROR )GOTO 9998

    IF( nch > 0 )THEN
      CALL get_c( line,nch,BLANK,string,ncc,lerr )
      IF( lerr .OR. string(1:2) /= PLUS )THEN
        eMessage = 'Error reading products'
        GOTO 9999
      END IF
    END IF
  END DO

  r%nP = nP

!====   Get Rate coefficient

  nch = LEN_TRIM(kstring)
  CALL get_i( kstring,nch,itype,lerr )
  IF( lerr )THEN
    eMessage = 'Error reading rate coefficient'
    GOTO 9999
  END IF
  itherm = INDEX(kstring,'H:')
  IF( itherm == 0 )THEN
    rstring = kstring
  ELSE
    rstring = kstring(1:itherm-1)
  END IF

  ndat = NOT_SET_I

  SELECT CASE( itype )
    CASE( ID_K_RAD )

    CASE( ID_K_CONST,ID_K_PRESB,ID_K_LWC )
      ndat = 1

    CASE( ID_K_TEMP,ID_K_H2O,ID_K_M,ID_K_EQM,ID_K_O2,ID_K_N2,ID_K_CH4,ID_K_H2OB )
      ndat = 3

    CASE( ID_K_PRES,ID_K_FALLOFF1,ID_K_FALLOFF2 )
      ndat = 4

    CASE( ID_K_FALLOFF3 )
      ndat = 6

    !--- CB05 reaction types
   CASE( ID_K_O2_M,ID_K_M2,ID_K_H2OSQ,ID_K_H2,ID_K_TEMPB )
      ndat = 3

    CASE( ID_K_FALLOFF5,ID_K_FALLOFF6 )
       ndat = 4

    CASE( ID_K_FALLOFF4)
       ndat = 7

    CASE( ID_K_FALLOFF7)
       ndat = 8

    CASE DEFAULT
      eMessage = 'Error reading Multi-component equation.'//&
                 'Unrecognised reaction type'
      GOTO 9999

  END SELECT

  IF( ndat /= NOT_SET_I )THEN

    ALLOCATE( r%data(ndat),STAT=alloc_stat )
    IF( alloc_stat /= 0 )THEN
      eMessage = 'Error allocating reaction data'
      GOTO 9997
    END IF
    nch = LEN_TRIM(rstring)

    DO i = 1,ndat
      CALL get_d( rstring,nch,r%data(i),lerr )
      IF( lerr )THEN
        IF( ndat == 3 .AND. i == 3 )THEN
          r%data(3) = 0.0D0 !for older imc files (now T-n)
          EXIT
        ELSE
          eMessage = 'Invalid reaction data'
          GOTO 9999
        END IF
      END IF
    END DO

    IF( itype == ID_K_EQM )THEN
      IF( NINT(r%data(3)) <= 0 .OR. NINT(r%data(3)) > ireact-1 )THEN
        IF( NINT(r%data(3)) <= 0 )THEN
           eMessage = '3rd coefficent for reaction type ID_K_EQM must be > 0'
        ELSE
           eMessage = '3rd coefficent for reaction type ID_K_EQM must be the ID '// &
                      'of a previous reaction (preferably immediately previous)'
        END IF
        GOTO 9999
      END IF
    END IF

  END IF

!====   Read heat of reaction, if present

  IF( itherm > 0 )THEN

    READ(kstring(itherm+2:),*,IOSTAT=ios) r%H
    IF( ios /= 0 )THEN
      eMessage = 'Invalid heat of reaction data'
      GOTO 9999
    END IF
    r%class = IBSET(r%class,ID_REACT_THERMAL)

!--- Scale heat units (J/kg) to use concentration units

    SELECT CASE( chem%cUnits )
      CASE( UNIT_PPM )
        r%H = r%H/(82.06*298.)
      CASE( UNIT_MOLECULE )
        r%H = r%H/6.022E17
    END SELECT

  ELSE

    r%H = 0.0

  END IF

!====   Set Reaction classes

  r%type = itype
  CALL SetReactionClass( chem%species(r%A)%class,r%class )

  IF( r%B == 0 )THEN

!--- Linear reaction

    r%class = IBSET(r%class,ID_REACT_LINEAR)

  ELSE

!--- Quadratic reaction

    CALL SetReactionClass( chem%species(r%B)%class,r%class )

!--- Swap species if A is ambient

    IF( chem%species(r%A)%class == ID_SPECIES_AMBIENT )THEN

      i    = r%A
      r%A  = r%B
      r%B  = i
      facB = r%fB
      r%fB = fac
      fac  = facB

    END IF

    r%fB = r%fB/fac

!--- Set linear and ambient if B is ambient

    IF( chem%species(r%B)%class == ID_SPECIES_AMBIENT )THEN
      r%class = IBSET(r%class,ID_REACT_LINEAR)
      r%class = IBSET(r%class,ID_REACT_AMBIENT)
    END IF

  END IF

  IF( chem%species(r%A)%class == ID_SPECIES_AMBIENT )THEN
    eMessage = 'Invalid reactants - cannot all be ambient species'
    GOTO 9999
  END IF

!--- Check product list for reaction class, and remove ambient product species

  iP = 0
  DO i = 1,nP
    IF( chem%species(r%P(i))%class == ID_SPECIES_AMBIENT )CYCLE
    iP = iP + 1
    IF( i > iP )r%P(iP) = r%P(i)
    r%fP(iP) = r%fP(i)/fac
    CALL SetReactionClass( chem%species(r%P(i))%class,r%class )
  END DO

  r%nP = iP

END DO

chem%nReactions = nreact

9998 CONTINUE

IF( nError /= NO_ERROR )THEN
  nreact = MAX(0,nreact-1)
ELSE
  CALL init_error()
END IF

IF( ASSOCIATED(rline) )DEALLOCATE( rline,STAT=alloc_stat )

RETURN

9999 CONTINUE
nError  = RD_ERROR
eInform = 'Line='//TRIM(original)
GOTO 9998

9997 CONTINUE
nError = UK_ERROR
GOTO 9998

END

!========================================================================

RECURSIVE SUBROUTINE SetRateCoeff()

!====   Set reaction rates for non-radiation dependent rates

USE chem_fi
USE met_fi
USE chemReactions_fd
USE constants_fd
USE default_fd

IMPLICIT NONE

REAL, PARAMETER :: TI300  = 1./300.
REAL, PARAMETER :: CONSTC = 0.6     ! Constant for reaction type 7

INTEGER i, knum
DOUBLE PRECISION f, k0, m0, kinf, minf, b1, b2, z, k2, k3
REAL  mm, factor
REAL  o2, n2, ch4
REAL    cfact, ks_conv, kt_conv, ambH2O
REAL    h2
REAL, DIMENSION(1)      :: size_cld

TYPE( ChemReact_str ), POINTER :: r

!====   Set conversion factor (molecules/cm3 -> ppm)
cfact = 7.33981E15*(pres/temp)     !Av(molecules/mole)*P0(N/m2)/R0(J/mole/K)/10**6(PPM)
                                   !Av=6.022e23  P0=1.013e5  R0=8.314e6
IF( ABS(chem%kConv) == MOLECULE_PPM )THEN
  ks_conv = cfact
  IF( chem%kConv < 0  )ks_conv = 1./ks_conv
ELSE
  ks_conv = 1.
END IF

mm  = 1.0E06
ch4 = 1.85
h2 = 0.56  ! Added for reaction type ID_K_H2

IF( chem%kUnits /= UNIT_PPM )THEN
  mm  = mm*cfact
  ch4 = ch4*cfact
  h2 = h2 * cfact
END IF

o2 = mm*0.2095
n2 = mm*0.7808

IF( chem%cUnits == UNIT_PPM )THEN  !adjust quadratic rxns for actual puff temp & p
  factor = temp/(298.*pres)
ELSE
  factor = 1.
END IF

kt_conv = chem%tConv

IF ( H2O == NOT_SET_I ) THEN
  ambH2O = 1.0E06*hb*(29./18.)/(1. + hb*(29./18.))
  IF( chem%cUnits == UNIT_MOLECULE )ambH2O = ambH2O*cfact
ELSE
  ambH2O = chem%species(H2O)%ambient
END IF

size_cld(1) = 1.0e-5
!====   Loop over reactions setting K if not radiation dependent

DO i = 1,chem%nReactions

  r => chem%reaction(i)

  SELECT CASE( r%type )

    CASE( ID_K_CONST )    !====== Constant rates
      r%k = SNGL(r%data(1))

    CASE( ID_K_TEMP )     !====== Temperature dependent rates
      r%k = SNGL(r%data(1)*(DBLE(temp)**(-r%data(3)))*EXP(r%data(2)/DBLE(temp)))

    CASE( ID_K_PRES )     !====== Pressure dependent
      f    = 0.6D0
      k0   = r%data(1)
      m0   = r%data(2)
      kinf = r%data(3)
      minf = r%data(4)
      b1   = (k0*DBLE(mm)/kinf)*(DBLE(temp)**(m0-minf))
      b2   = k0*DBLE(mm)*(DBLE(temp)**m0)
      z    = 1.D0/(1.D0 + (LOG10(b1))**2)
      r%k = SNGL((b2/(1.D0 + b1))*(f**z))

    CASE( ID_K_PRESB )    !====== Pressure (type B) dependent
      r%k = SNGL(r%data(1))*(1.0 + CONSTC*pres)

    CASE( ID_K_H2O )      !====== Water dependent rates
      r%k = SNGL(r%data(1)*(DBLE(temp)**(-r%data(3)))*EXP(r%data(2)/DBLE(temp))*DBLE(ambH2O))

    CASE( ID_K_H2OB )     !====== Water (type B) dependent rates
      r%k = SNGL(r%data(1)*(DBLE(temp)**(-r%data(3)))*EXP(r%data(2)/DBLE(temp))*DBLE(ambH2O*ks_conv))

    CASE( ID_K_M )        !====== M dependent rates
      r%k = SNGL(r%data(1)*(DBLE(temp)**(-r%data(3)))*EXP(r%data(2)/DBLE(temp))*DBLE(mm))

    CASE( ID_K_M2 )     !====== M dependent rates with general temp. dependence
      r%k = SNGL(r%data(1)*((DBLE(temp*TI300))**(r%data(3)))*EXP(r%data(2)/DBLE(temp))*DBLE(mm))

    CASE( ID_K_O2 )       !====== O2 dependent rates
      r%k = SNGL(r%data(1)*(DBLE(temp)**(-r%data(3)))*EXP(r%data(2)/DBLE(temp))*DBLE(o2))

    CASE( ID_K_N2 )       !====== N2 dependent rates
      r%k = SNGL(r%data(1)*(DBLE(temp)**(-r%data(3)))*EXP(r%data(2)/DBLE(temp))*DBLE(n2))

    CASE( ID_K_CH4 )      !====== CH4 dependent rates
      r%k = SNGL(r%data(1)*(DBLE(temp)**(-r%data(3)))*EXP(r%data(2)/DBLE(temp))*DBLE(ch4))

    CASE( ID_K_EQM )      !====== Equilibrium (dependent on rate of previous reaction)
      knum = INT(r%data(3))
      r%k  = chem%reaction(knum)%k/SNGL((r%data(1)*EXP(r%data(2)/DBLE(temp))))

    CASE( ID_K_FALLOFF1,ID_K_FALLOFF2 )   !====== Falloff reaction rates
      f    = 0.6D0
      k0   = DBLE(mm)*r%data(1)*(DBLE(temp*TI300))**r%data(2)
      kinf = r%data(3)*(DBLE(temp*TI300))**r%data(4)
      b1   = k0/kinf
      z    = 1.D0/(1.D0 + (LOG10(b1))**2)
      r%k = SNGL((k0/(1.D0 + b1))*(f**z))
      IF ( r%type == ID_K_FALLOFF2 ) &   !====== Multiply by O2 type 2
        r%k = r%k*o2

    CASE( ID_K_FALLOFF3 )        !====== Falloff reaction rates
      k0  = r%data(1)*EXP(r%data(2)/DBLE(temp))
      k2  = r%data(3)*EXP(r%data(4)/DBLE(temp))
      k3  = DBLE(mm)*r%data(5)*EXP(r%data(6)/DBLE(temp))
      r%k = SNGL(k0 + (k3/(1.D0 + k3/k2)))

    CASE( ID_K_LWC )        !====== LWC dependent rates
      r%k = SNGL(1.2d-9*r%data(1)*DBLE(cw)/(DBLE(size_cld(1)*size_cld(1))))

    !---  CB05 reaction types
    CASE( ID_K_O2_M )     !====== O2 and M dependent rates
      r%k = SNGL(r%data(1)*((DBLE(temp*TI300))**(r%data(3)))*EXP(r%data(2)/DBLE(temp))*DBLE(o2*mm))

    CASE( ID_K_H2OSQ )    !====== Water (2 molecules) dependent rates
      r%k = SNGL(r%data(1)*(DBLE(temp)**(-r%data(3)))*EXP(r%data(2)/DBLE(temp))*DBLE((ambH2O*ks_conv)**2))

    CASE( ID_K_FALLOFF4,ID_K_FALLOFF7 )   !====== Falloff reaction rates
      f    = r%data(7)
      k0   = DBLE(mm)*r%data(1)*((DBLE(temp*TI300))**r%data(2))*EXP(r%data(3)/DBLE(temp))
      kinf = r%data(4)*((DBLE(temp*TI300))**r%data(5))*EXP(r%data(6)/DBLE(temp))
      b1   = k0/kinf

      IF ( r%type == ID_K_FALLOFF4 ) THEN
        z = 1.D0/(1.D0 + (LOG10(b1))**2)
      ELSE
        IF( r%data(8) == 0 ) THEN
          z = 1.D0/(1.D0 + (LOG10(b1))**2)
        ELSE
          z = 1.D0/(1.D0 + (LOG10(b1)/r%data(8))**2)
        END IF
      END IF
      r%k = SNGL((k0/(1.D0 + b1))*(f**z))

    CASE( ID_K_FALLOFF5,ID_K_FALLOFF6 )   !====== Falloff reaction rates
      k0  = r%data(1)*EXP(r%data(2)/DBLE(temp))
      k2  = DBLE(mm)*r%data(3)*EXP(r%data(4)/DBLE(temp))
      r%k = SNGL(k0 + k2)
      IF ( r%type == ID_K_FALLOFF6 ) &   !====== Multiply by H2O type 6
        r%k = SNGL((k0 + k2)*ambH2O*ks_conv)

    CASE( ID_K_H2 )       !====== H2 dependent rates
      r%k = SNGL(r%data(1)*(DBLE(temp)**(-r%data(3)))*EXP(r%data(2)/DBLE(temp))*DBLE(h2))

    CASE( ID_K_TEMPB )    !====== Temperature (type B) dependent rates
      r%k = SNGL(r%data(1)*((DBLE(temp*TI300))**(r%data(3)))*EXP(r%data(2)/DBLE(temp)))

  END SELECT
END DO
!====== Add conversion factors to reaction rates constants

DO i = 1,chem%nReactions

  r => chem%reaction(i)
  IF( r%type /= ID_K_LWC .AND. r%type /= ID_K_RAD )THEN
    r%k = r%k*kt_conv
    IF( .NOT.BTEST(r%class,ID_REACT_LINEAR) )THEN
      r%k = r%k*ks_conv*factor
    END IF
  END IF

  IF( BTEST(r%class,ID_REACT_AMBIENT) )THEN
    r%k = r%k*chem%species(r%B)%amb*ks_conv*factor
  END IF

END DO

RETURN
END

!========================================================================
RECURSIVE SUBROUTINE SetPhotoRate()

USE chem_fi

!====   Set reaction rates for radiation dependent rates

IMPLICIT NONE

INTEGER np, nm, ik
REAL    rat, kt_conv

kt_conv = chem%tConv

!====   Night time

IF( zen_ang >= 90.0 )THEN

  DO ik = 1,chem%nPhotoReact
    chem%pReact(ik)%r%k = 0.
  END DO

!====   Day time

ELSE

!====== Find interpolation points

  np = 1
  DO WHILE( np <= chem%nZenith )
    IF( zen_ang > chem%zenith(np) )THEN
      np = np + 1
    ELSE
      EXIT
    END IF
  END DO
  nm = MAX(np-1,1)
  np = MAX(MIN(np,chem%nZenith),1)

!====== Set interpolation rate

  IF( nm == np )THEN
    rat = 1.
  ELSE
    rat = (zen_ang-chem%zenith(nm))/(chem%zenith(np)-chem%zenith(nm))
  END IF

!====== Interpolation reaction rate constants

  DO ik = 1,chem%nPhotoReact
    chem%pReact(ik)%r%k = kt_conv*((1.-rat)*chem%pReact(ik)%r%data(nm) + &
                                       rat *chem%pReact(ik)%r%data(np))
    chem%pReact(ik)%r%k = chem%pReact(ik)%r%k * cld_att  !Cloud correction factor
  END DO

END IF

RETURN
END

!========================================================================
RECURSIVE SUBROUTINE StepRadicals( it )

USE chem_fi
USE error_fi

IMPLICIT NONE

INTEGER :: it

INTEGER i, j, k, iamb, alloc_stat
LOGICAL leqm_set

CHARACTER*8, DIMENSION(19) :: RadSpName

REAL, DIMENSION(:), ALLOCATABLE :: oldAmb

RadSpName = (/'O       ','O1D     ','OH      ','HO2     ','XO2     ',&
             &'XO2N    ','MEO2    ','HCO3    ','C2O3    ','CXO3    ',&
             &'ROR     ','TO2     ','TOLRO2  ','OPEN    ','CRO     ',&
             &'XYLRO2  ','CL      ','CLO     ','BENZRO2 '/)

! Return if using short range optimized reaction mechanism
IF( nspectot < 19 )RETURN

!$OMP SINGLE

!====   Get ambient concentrations and save initial values

ALLOCATE( oldAmb(nspectot),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError  = UK_ERROR
  eRoutine = 'StepRadicals'
  eMessage = 'Error allocating oldAmb'
  GO TO 9999
END IF

nfast     = chem%nFast
nslow     = chem%nSlow
nequil    = chem%nEquilibrium
nparticle = chem%nParticle
nspecies  = nfast + nslow + nparticle
nspectot  = nspecies + nequil
nambient  = chem%nSpecies - nspectot

species => chem%species

IF( chem%nReactions == 0 )RETURN

fast  => chem%fast
slow  => chem%slow
equil => chem%equil

nsolve_eq = chem%nSolveEq
nlin_eq   = chem%nLinearEq
ndir_eq   = chem%nDirectEq
nsubs_eq  = chem%nSubstEq

indx_eq  => chem%IndexEq
itype_eq => chem%TypeEq
irow_eq  => chem%RowEq

rtol = chem%rtol
volx = 0.

ResetEq = .FALSE.

IF( .NOT. ASSOCIATED(pdnrate) )THEN
  ALLOCATE( pdnrate(nspectot),STAT=alloc_stat )
  IF( nslow > 0 .AND. alloc_stat == 0 )ALLOCATE( ydots(nslow),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    nError   = IV_ERROR
    eRoutine = 'StepRadicals'
    eMessage = 'Error allocating work arrays'
    GOTO 9999
  END IF
ELSE
  alloc_stat = 9999
END IF

DO i = 1,nspectot
  species(i)%msav = species(i)%mass
  species(i)%csav = species(i)%conc
  oldAmb(i)       = species(i)%amb
END DO

DO k = 1,chem%Ambient%nz

  ! Initialize to fixed imc value
  DO i = 1,nspectot
    species(i)%amb = species(i)%ambient
  END DO

  ! Update with concentrations from the background ambient file value
  DO i = 1,chem%nSpecies
    IF( chem%Ambient%ID(i) > 0 )THEN
      iamb = chem%Ambient%ID(i)
      species(i)%amb = chem%Ambient%amb(k,iamb,it)
    END IF
  END DO

  DO i = 1,nspectot
    species(i)%mass = 0.
    species(i)%conc = species(i)%amb
    species(i)%amb  = 0.
  END DO

  CALL UpdateChemRate( .FALSE. )
  IF( nError /= NO_ERROR )GOTO 9999

  leqm_set = .TRUE.
  volx     = 0.

  CALL step_ode( 60.,leqm_set )
  IF( nError /= NO_ERROR )GOTO 9999

  ! Update only the radical species
  DO i = 1,chem%nSpecies
    iamb = chem%Ambient%ID(i)
    IF( iamb > 0 )THEN
      IF( ANY(RadSpName == species(i)%name(:8)) )THEN
        IF( chemID /= 999 )THEN
          DO j = 1,nthreads
!$OMP ATOMIC WRITE
            chemMC(chemID,j)%Ambient%amb(k,iamb,it) = species(i)%conc
          END DO
        ELSE
          chem%Ambient%amb(k,iamb,it) = species(i)%conc
        END IF
      END IF
    END IF
  END DO

END DO

!$OMP ATOMIC WRITE
MonthDay(it,2) = 1  ! Stepped Radicals

9999 CONTINUE
!$OMP END SINGLE

DO i = 1,nspectot
  species(i)%amb  = oldAmb(i)
  species(i)%mass = species(i)%msav
  species(i)%conc = species(i)%csav
END DO

IF( alloc_stat /= 9999 )THEN
  IF( ASSOCIATED(pdnrate) )THEN
      DEALLOCATE( pdnrate, STAT=alloc_stat )
      NULLIFY(pdnrate)
  END IF
  IF( ASSOCIATED(ydots)   )THEN
      DEALLOCATE( ydots,   STAT=alloc_stat )
       NULLIFY(ydots)
  END IF
END IF

IF( ALLOCATED(oldAmb)  )DEALLOCATE( oldAmb,  STAT=alloc_stat )

RETURN
END

!========================================================================

SUBROUTINE GetRadicalSpecies( )

USE chem_fi

IMPLICIT NONE

INTEGER i, alloc_stat
LOGICAL leqm_set

CHARACTER*8, DIMENSION(19) :: RadSpName

REAL, DIMENSION(:), ALLOCATABLE :: oldAmb

RadSpName = (/'O       ','O1D     ','OH      ','HO2     ','XO2     ',&
             &'XO2N    ','MEO2    ','HCO3    ','C2O3    ','CXO3    ',&
             &'ROR     ','TO2     ','TOLRO2  ','OPEN    ','CRO     ',&
             &'XYLRO2  ','CL      ','CLO     ','BENZRO2 '/)

!====   Get stepped ambient concentrations and save initial values

ALLOCATE( oldAmb(nspectot),STAT=alloc_stat )

DO i = 1,nspectot
  species(i)%msav = species(i)%mass
  species(i)%csav = species(i)%conc
  oldAmb(i)       = species(i)%amb
  species(i)%mass = 0.
  species(i)%conc = species(i)%amb
  species(i)%amb  = 0.
END DO

leqm_set = .TRUE.

CALL step_ode( 60.,leqm_set )

DO i = 1,nspectot
  IF( ANY(RadSpName == species(i)%name(:8)) )THEN
    species(i)%amb  = species(i)%conc
  ELSE
    species(i)%amb  = oldAmb(i)
  END IF
  species(i)%mass = species(i)%msav
  species(i)%conc = species(i)%csav
END DO

IF( ALLOCATED(oldAmb) )DEALLOCATE(oldAmb,STAT=alloc_stat)

RETURN
END

!========================================================================

REAL FUNCTION getSpAmbLim( iSp )

USE chem_fi

IMPLICIT NONE

INTEGER, INTENT(IN) :: iSp

LOGICAL lZero

lZero = .FALSE.

getSpAmbLim = -chem%species(iSp)%amb

!All particle species
IF( chem%species(iSp)%class == ID_SPECIES_PARTICLE )lZero = .TRUE.

IF( .NOT. lZero )THEN
  SELECT CASE( TRIM(chem%species(iSp)%name) )
    CASE( 'SO2','SULF','NH3','NO','NO2','HNO3' )
      lZero = .TRUE.
  END SELECT
END IF

! All semi-volatile SOA species and reaction products
IF( .NOT. lZero )THEN
  IF( INDEX(chem%species(iSp)%name,'SV_') > 0 .OR. &
      INDEX(chem%species(iSp)%name,'RXN') > 0) THEN
    lZero = .TRUE.
  END IF
END IF

IF( lZero )getSpAmbLim = 0.

RETURN
END
