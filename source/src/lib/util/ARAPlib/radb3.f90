SUBROUTINE radb3( IDO,L1,CC,CH,WA1,WA2 )

IMPLICIT NONE

INTEGER,                   INTENT( IN  ) :: IDO, L1
REAL, DIMENSION(IDO,3,L1), INTENT( IN  ) :: CC
REAL, DIMENSION(IDO,L1,3), INTENT( OUT ) :: CH
REAL, DIMENSION(IDO),      INTENT( IN  ) :: WA1, WA2

REAL, PARAMETER :: TAUR = -0.5
REAL, PARAMETER :: TAUI =  0.866025403784439

INTEGER I, K, IC, IDP2
REAL    TR2, CR2, TI2, CI2, CR3, CI3, DR2, DI2, DR3, DI3

DO K = 1,L1
   TR2 = 2.0*CC(IDO,2,K)
   CR2 = CC(1,1,K) + TAUR*TR2
   CI3 = 2.0*TAUI*CC(1,3,K)
   CH(1,K,1) = CC(1,1,K) + TR2
   CH(1,K,2) = CR2 - CI3
   CH(1,K,3) = CR2 + CI3
END DO

IF( IDO == 1 )RETURN

IDP2 = IDO + 2

IF( (IDO-1)/2 >= L1 )THEN

  DO K = 1,L1
    DO I = 3,IDO,2
      IC = IDP2-I
      TR2 = CC(I-1,3,K) + CC(IC-1,2,K)
      CR2 = CC(I-1,1,K) + TAUR*TR2
      CH(I-1,K,1) = CC(I-1,1,K) + TR2
      TI2 = CC(I,3,K) - CC(IC,2,K)
      CI2 = CC(I,1,K) + TAUR*TI2
      CH(I,K,1) = CC(I,1,K) + TI2
      CR3 = TAUI*(CC(I-1,3,K) - CC(IC-1,2,K))
      CI3 = TAUI*(CC(I,  3,K) + CC(IC,  2,K))
      DR2 = CR2 - CI3
      DR3 = CR2 + CI3
      DI2 = CI2 + CR3
      DI3 = CI2 - CR3
      CH(I-1,K,2) = WA1(I-2)*DR2 - WA1(I-1)*DI2
      CH(I,  K,2) = WA1(I-2)*DI2 + WA1(I-1)*DR2
      CH(I-1,K,3) = WA2(I-2)*DR3 - WA2(I-1)*DI3
      CH(I,  K,3) = WA2(I-2)*DI3 + WA2(I-1)*DR3
    END DO
  END DO

ELSE

  DO I = 3,IDO,2
    IC = IDP2 - I
    DO K = 1,L1
      TR2 = CC(I-1,3,K) + CC(IC-1,2,K)
      CR2 = CC(I-1,1,K) + TAUR*TR2
      CH(I-1,K,1) = CC(I-1,1,K) + TR2
      TI2 = CC(I,3,K) - CC(IC,2,K)
      CI2 = CC(I,1,K) + TAUR*TI2
      CH(I,K,1) = CC(I,1,K) + TI2
      CR3 = TAUI*(CC(I-1,3,K) - CC(IC-1,2,K))
      CI3 = TAUI*(CC(I,3,K) + CC(IC,2,K))
      DR2 = CR2 - CI3
      DR3 = CR2 + CI3
      DI2 = CI2 + CR3
      DI3 = CI2 - CR3
      CH(I-1,K,2) = WA1(I-2)*DR2 - WA1(I-1)*DI2
      CH(I,  K,2) = WA1(I-2)*DI2 + WA1(I-1)*DR2
      CH(I-1,K,3) = WA2(I-2)*DR3 - WA2(I-1)*DI3
      CH(I,  K,3) = WA2(I-2)*DI3 + WA2(I-1)*DR3
    END DO
  END DO

END IF

RETURN
END
