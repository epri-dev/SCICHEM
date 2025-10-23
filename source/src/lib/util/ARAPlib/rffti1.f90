SUBROUTINE rffti1( N,WA,IFAC )

IMPLICIT NONE

INTEGER,                INTENT( IN  ) :: N
!REAL,    DIMENSION(N),  INTENT( OUT ) :: WA
!INTEGER, DIMENSION(15), INTENT( OUT ) :: IFAC
REAL,    DIMENSION(*), INTENT( INOUT ) :: WA
INTEGER, DIMENSION(*), INTENT( INOUT ) :: IFAC

INTEGER, DIMENSION(4), PARAMETER :: NTRYH = (/4,2,3,5/)

INTEGER NL, NF, NQ, NR, I, J, IB, IS, K1, L1, L2, NFM1, NTRY
INTEGER IP, LD, IPM, IDO, II
REAL    TPI, ARG, ARGLD, ARGH, FI

NL = N
NF = 0
J  = 0

JLoop: DO
  J = J + 1
  IF( J <= 4 )THEN
    NTRY = NTRYH(J)
  ELSE
    NTRY = NTRY + 2
  END IF

  DO
    NQ = NL/NTRY
    NR = NL-NTRY*NQ
    IF( NR /= 0 )CYCLE JLoop

    NF = NF + 1
    IFAC(NF+2) = NTRY
    NL = NQ

    IF( NTRY == 2 .AND. NF /= 1 )THEN
      DO I = 2,NF
        IB = NF - I + 2
        IFAC(IB+2) = IFAC(IB+1)
      END DO
      IFAC(3) = 2
    END IF

    IF( NL == 1 )EXIT JLoop

  END DO

END DO JLoop

IFAC(1) = N
IFAC(2) = NF
TPI  = 6.28318530717959
ARGH = TPI/FLOAT(N)
IS   = 0
NFM1 = NF-1
L1   = 1

IF( NFM1 == 0 )RETURN

DO K1 = 1,NFM1
  IP = IFAC(K1+2)
  LD = 0
  L2 = L1*IP
  IDO = N/L2
  IPM = IP - 1
  DO J = 1,IPM
    LD = LD + L1
    I  = IS
    ARGLD = FLOAT(LD)*ARGH
    FI = 0.
    DO II = 3,IDO,2
      I = I + 2
      FI  = FI + 1.
      ARG = FI*ARGLD
      WA(I-1) = COS(ARG)
      WA(I  ) = SIN(ARG)
    END DO
    IS = IS + IDO
  END DO
  L1 = L2
END DO

RETURN
END
