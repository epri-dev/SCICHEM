SUBROUTINE rfftb1( N,C,CH,WA,IFAC )

IMPLICIT NONE

!INTEGER N
!REAL CH(1), C(1), WA(1)
!INTEGER IFAC(1)

INTEGER,                INTENT( IN    ) :: N
REAL,    DIMENSION(N),  INTENT( INOUT ) :: C, CH
REAL,    DIMENSION(N),  INTENT( IN    ) :: WA
INTEGER, DIMENSION(15), INTENT( IN    ) :: IFAC

INTEGER NF, NA, L1, IW, K1, IP, L2, IDO, IDL1, IX2, IX3, IX4, I

NF = IFAC(2)
NA = 0
L1 = 1
IW = 1

DO K1 = 1,NF

  IP   = IFAC(K1+2)
  L2   = IP*L1
  IDO  = N/L2
  IDL1 = IDO*L1

  IF (IP == 4) THEN

    IX2 = IW  + IDO
    IX3 = IX2 + IDO
    IF (NA == 0) THEN
      CALL radb4 (IDO,L1,C,CH,WA(IW),WA(IX2),WA(IX3))
    ELSE
      CALL radb4 (IDO,L1,CH,C,WA(IW),WA(IX2),WA(IX3))
    END IF
    NA = 1 - NA

  ELSE IF (IP == 2) THEN

    IF (NA == 0) THEN
      CALL radb2 (IDO,L1,C,CH,WA(IW))
    ELSE
      CALL radb2 (IDO,L1,CH,C,WA(IW))
    END IF
    NA = 1 - NA

  ELSE IF (IP == 3) THEN

    IX2 = IW +IDO
    IF (NA == 0) THEN
      CALL radb3 (IDO,L1,C,CH,WA(IW),WA(IX2))
    ELSE
      CALL radb3 (IDO,L1,CH,C,WA(IW),WA(IX2))
    END IF
    NA = 1 - NA

  ELSE IF (IP == 5) THEN

    IX2 = IW  + IDO
    IX3 = IX2 + IDO
    IX4 = IX3 + IDO
    IF (NA == 0) THEN
      CALL radb5 (IDO,L1,C,CH,WA(IW),WA(IX2),WA(IX3),WA(IX4))
    ELSE
      CALL radb5 (IDO,L1,CH,C,WA(IW),WA(IX2),WA(IX3),WA(IX4))
    END IF
    NA = 1 - NA

  ELSE

    IF (NA == 0) THEN
      CALL radbg (IDO,IP,L1,IDL1,C,C,C,CH,CH,WA(IW))
    ELSE
      CALL radbg (IDO,IP,L1,IDL1,CH,CH,CH,C,C,WA(IW))
    END IF
    IF (IDO == 1) NA = 1-NA

  END IF

  L1 = L2
  IW = IW+(IP-1)*IDO

END DO

IF (NA == 0) RETURN

DO I = 1,N
  C(I) = CH(I)
END DO

RETURN
END
