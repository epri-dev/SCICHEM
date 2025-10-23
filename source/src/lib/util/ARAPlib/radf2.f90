SUBROUTINE radf2 (IDO,L1,CC,CH,WA1)

IMPLICIT NONE

!INTEGER IDO, L1
!REAL CH(IDO,2,L1), CC(IDO,L1,2), WA1(1)

INTEGER,                   INTENT( IN  ) :: IDO, L1
REAL, DIMENSION(IDO,L1,2), INTENT( IN  ) :: CC
REAL, DIMENSION(IDO,2,L1), INTENT( OUT ) :: CH
REAL, DIMENSION(IDO),      INTENT( IN  ) :: WA1

INTEGER K, I, IDP2, IC
REAl    TR2, TI2

DO K=1,L1
  CH(1,1,K) = CC(1,K,1)+CC(1,K,2)
  CH(IDO,2,K) = CC(1,K,1)-CC(1,K,2)
END DO

IF( IDO < 2 )RETURN

IF( IDO > 2 )THEN

  IDP2 = IDO+2
  IF( (IDO-1)/2 >= L1)THEN

    DO K=1,L1
      DO I=3,IDO,2
        IC = IDP2-I
        TR2 = WA1(I-2)*CC(I-1,K,2)+WA1(I-1)*CC(I,K,2)
        TI2 = WA1(I-2)*CC(I,K,2)-WA1(I-1)*CC(I-1,K,2)
        CH(I,1,K) = CC(I,K,1)+TI2
        CH(IC,2,K) = TI2-CC(I,K,1)
        CH(I-1,1,K) = CC(I-1,K,1)+TR2
        CH(IC-1,2,K) = CC(I-1,K,1)-TR2
      END DO
    END DO

  ELSE

    DO I=3,IDO,2
      IC = IDP2-I
      DO K=1,L1
        TR2 = WA1(I-2)*CC(I-1,K,2)+WA1(I-1)*CC(I,K,2)
        TI2 = WA1(I-2)*CC(I,K,2)-WA1(I-1)*CC(I-1,K,2)
        CH(I,1,K) = CC(I,K,1)+TI2
        CH(IC,2,K) = TI2-CC(I,K,1)
        CH(I-1,1,K) = CC(I-1,K,1)+TR2
        CH(IC-1,2,K) = CC(I-1,K,1)-TR2
      END DO
    END DO

  END IF

  IF (MOD(IDO,2) == 1) RETURN

END IF

DO  K=1,L1
  CH(1,2,K) = -CC(IDO,K,2)
  CH(IDO,1,K) = CC(IDO,K,1)
END DO

RETURN
END
