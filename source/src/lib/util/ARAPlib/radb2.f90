SUBROUTINE radb2( IDO,L1,CC,CH,WA1 )

IMPLICIT NONE

!INTEGER IDO, L1
!REAL    CC(IDO,2,L1), CH(IDO,L1,2), WA1(1)

INTEGER,                   INTENT( IN  ) :: IDO, L1
REAL, DIMENSION(IDO,2,L1), INTENT( IN  ) :: CC
REAL, DIMENSION(IDO,L1,2), INTENT( OUT ) :: CH
REAL, DIMENSION(IDO),      INTENT( IN  ) :: WA1

INTEGER I, K, IDP2, IC
REAL    TR2, TI2

DO K = 1,L1
   CH(1,K,1) = CC(1,1,K) + CC(IDO,2,K)
   CH(1,K,2) = CC(1,1,K) - CC(IDO,2,K)
END DO

IF( IDO < 2 )RETURN

IF( IDO > 2 )THEN
  IDP2 = IDO + 2
  IF( (IDO-1)/2 >= L1 )THEN
    DO K = 1,L1
      DO I = 3,IDO,2
        IC = IDP2-I
        CH(I-1,K,1) = CC(I-1,1,K) + CC(IC-1,2,K)
        TR2         = CC(I-1,1,K) - CC(IC-1,2,K)
        CH(I,  K,1) = CC(I,1,K)   - CC(IC,2,K)
        TI2         = CC(I,1,K)   + CC(IC,2,K)
        CH(I-1,K,2) = WA1(I-2)*TR2 - WA1(I-1)*TI2
        CH(I,  K,2) = WA1(I-2)*TI2 + WA1(I-1)*TR2
      END DO
    END DO
  ELSE
    DO I = 3,IDO,2
      IC = IDP2-I
      DO K = 1,L1
        CH(I-1,K,1) = CC(I-1,1,K) + CC(IC-1,2,K)
        TR2         = CC(I-1,1,K) - CC(IC-1,2,K)
        CH(I,  K,1) = CC(I,1,K)   - CC(IC,2,K)
        TI2         = CC(I,1,K)   + CC(IC,2,K)
        CH(I-1,K,2) = WA1(I-2)*TR2 - WA1(I-1)*TI2
        CH(I  ,K,2) = WA1(I-2)*TI2 + WA1(I-1)*TR2
      END DO
    END DO
  END IF

  IF( MOD(IDO,2) == 1 )RETURN

END IF

DO K = 1,L1
  CH(IDO,K,1) = CC(IDO,1,K) + CC(IDO,1,K)
  CH(IDO,K,2) = -(CC(1,2,K) + CC(1,2,K))
END DO

RETURN
END
