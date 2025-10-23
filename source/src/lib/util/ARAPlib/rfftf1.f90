SUBROUTINE rfftf1 (N,C,CH,WA,IFAC)

IMPLICIT NONE

!INTEGER N
!REAL CH(1), C(1) , WA(1)
!INTEGER  IFAC(1)

INTEGER,                INTENT( IN    ) :: N
REAL,    DIMENSION(N),  INTENT( INOUT ) :: C, CH
REAL,    DIMENSION(N),  INTENT( IN    ) :: WA
INTEGER, DIMENSION(15), INTENT( IN    ) :: IFAC

INTEGER NF, NA, L1, L2, IW, K1, KH, IP, IDO, IDL1, IX2, IX3, IX4, I

NF = IFAC(2)
NA = 1
L2 = N
IW = N

DO K1 = 1,NF

  KH   = NF - K1
  IP   = IFAC(KH+3)
  L1   = L2/IP
  IDO  = N/L2
  IDL1 = IDO*L1
  IW   = IW-(IP-1)*IDO
  NA   = 1-NA

  IF (IP == 4) THEN

    IX2 = IW  + IDO
    IX3 = IX2 + IDO
    IF (NA == 0) THEN
      CALL radf4 (IDO,L1,C,CH,WA(IW),WA(IX2),WA(IX3))
    ELSE
      CALL radf4 (IDO,L1,CH,C,WA(IW),WA(IX2),WA(IX3))
    END IF

  ELSE IF (IP == 2) THEN

    IF (NA == 0) THEN
      CALL radf2 (IDO,L1,C,CH,WA(IW))
    ELSE
      CALL radf2 (IDO,L1,CH,C,WA(IW))
    END IF

  ELSE IF (IP == 3) THEN

    IX2 = IW + IDO
    IF (NA == 0) THEN
      CALL radf3 (IDO,L1,C,CH,WA(IW),WA(IX2))
    ELSE
      CALL radf3 (IDO,L1,CH,C,WA(IW),WA(IX2))
    END IF

  ELSE IF (IP == 5) THEN

    IX2 = IW  + IDO
    IX3 = IX2 + IDO
    IX4 = IX3 + IDO
    IF (NA == 0) THEN
      CALL radf5 (IDO,L1,C,CH,WA(IW),WA(IX2),WA(IX3),WA(IX4))
    ELSE
      CALL radf5 (IDO,L1,CH,C,WA(IW),WA(IX2),WA(IX3),WA(IX4))
    END IF

  ELSE

    IF (IDO == 1) NA = 1 - NA
    IF (NA == 0) THEN
      CALL radfg (IDO,IP,L1,IDL1,C,C,C,CH,CH,WA(IW))
      NA = 1
    ELSE
      CALL radfg (IDO,IP,L1,IDL1,CH,CH,CH,C,C,WA(IW))
      NA = 0
    END IF

  END IF

  L2 = L1

END DO

IF (NA == 1) RETURN

DO I = 1,N
  C(I) = CH(I)
END DO

RETURN
END
