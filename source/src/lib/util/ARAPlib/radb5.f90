SUBROUTINE radb5 (IDO,L1,CC,CH,WA1,WA2,WA3,WA4) !(ido,l1,cc,ch,wa1,wa2,wa3,wa4)

IMPLICIT NONE

!INTEGER IDO, L1
!REAL CC(IDO,5,L1), CH(IDO,L1,5), WA1(1), WA2(1), WA3(1), WA4(1)

INTEGER,                   INTENT( IN  ) :: IDO, L1
REAL, DIMENSION(IDO,5,L1), INTENT( IN  ) :: CC
REAL, DIMENSION(IDO,L1,5), INTENT( OUT ) :: CH
REAL, DIMENSION(IDO),      INTENT( IN  ) :: WA1, WA2, WA3, WA4

REAL, PARAMETER :: TR11 =  0.309016994374947
REAL, PARAMETER :: TI11 =  0.951056516295154
REAL, PARAMETER :: TR12 = -0.809016994374947
REAL, PARAMETER :: TI12 =  0.587785252292473

INTEGER K, I, IDP2, IC
REAl    TI5, TI4, TR5, TR4, TR2, TR3, TI2, TI3, CR2, CR3, CI4, CI5
REAl    CI2, CI3, CR4, CR5, DR2, DR3, DR4, DR5, DI2, DI3, DI4, DI5

DO K = 1,L1
  TI5 = CC(1,3,K) + CC(1,3,K)
  TI4 = CC(1,5,K) + CC(1,5,K)
  TR2 = CC(IDO,2,K) + CC(IDO,2,K)
  TR3 = CC(IDO,4,K) + CC(IDO,4,K)
  CH(1,K,1) = CC(1,1,K) + TR2 + TR3
  CR2 = CC(1,1,K) + TR11*TR2 + TR12*TR3
  CR3 = CC(1,1,K) + TR12*TR2 + TR11*TR3
  CI5 = TI11*TI5 + TI12*TI4
  CI4 = TI12*TI5 - TI11*TI4
  CH(1,K,2) = CR2 - CI5
  CH(1,K,3) = CR3 - CI4
  CH(1,K,4) = CR3 + CI4
  CH(1,K,5) = CR2 + CI5
END DO

IF( IDO == 1 )RETURN

IDP2 = IDO + 2

IF( (IDO-1)/2 >= L1 )THEN

  DO K = 1,L1
    DO I = 3,IDO,2
      IC = IDP2-I
      TI5 = CC(I,  3,K) + CC(IC,  2,K)
      TI2 = CC(I,  3,K) - CC(IC,  2,K)
      TI4 = CC(I,  5,K) + CC(IC,  4,K)
      TI3 = CC(I,  5,K) - CC(IC,  4,K)
      TR5 = CC(I-1,3,K) - CC(IC-1,2,K)
      TR2 = CC(I-1,3,K) + CC(IC-1,2,K)
      TR4 = CC(I-1,5,K) - CC(IC-1,4,K)
      TR3 = CC(I-1,5,K) + CC(IC-1,4,K)
      CH(I-1,K,1) = CC(I-1,1,K) + TR2 + TR3
      CH(I,  K,1) = CC(I,  1,K) + TI2 + TI3
      CR2 = CC(I-1,1,K) + TR11*TR2 + TR12*TR3
      CI2 = CC(I,  1,K) + TR11*TI2 + TR12*TI3
      CR3 = CC(I-1,1,K) + TR12*TR2 + TR11*TR3
      CI3 = CC(I,  1,K) + TR12*TI2 + TR11*TI3
      CR5 = TI11*TR5 + TI12*TR4
      CI5 = TI11*TI5 + TI12*TI4
      CR4 = TI12*TR5 - TI11*TR4
      CI4 = TI12*TI5 - TI11*TI4
      DR3 = CR3 - CI4
      DR4 = CR3 + CI4
      DI3 = CI3 + CR4
      DI4 = CI3 - CR4
      DR5 = CR2 + CI5
      DR2 = CR2 - CI5
      DI5 = CI2 - CR5
      DI2 = CI2 + CR5
      CH(I-1,K,2) = WA1(I-2)*DR2 - WA1(I-1)*DI2
      CH(I,  K,2) = WA1(I-2)*DI2 + WA1(I-1)*DR2
      CH(I-1,K,3) = WA2(I-2)*DR3 - WA2(I-1)*DI3
      CH(I,  K,3) = WA2(I-2)*DI3 + WA2(I-1)*DR3
      CH(I-1,K,4) = WA3(I-2)*DR4 - WA3(I-1)*DI4
      CH(I,  K,4) = WA3(I-2)*DI4 + WA3(I-1)*DR4
      CH(I-1,K,5) = WA4(I-2)*DR5 - WA4(I-1)*DI5
      CH(I,  K,5) = WA4(I-2)*DI5 + WA4(I-1)*DR5
    END DO
  END DO

ELSE

  DO I = 3,IDO,2
    IC = IDP2 - I
    DO K = 1,L1
      TI5 = CC(I,  3,K) + CC(IC,  2,K)
      TI2 = CC(I,  3,K) - CC(IC,  2,K)
      TI4 = CC(I,  5,K) + CC(IC,  4,K)
      TI3 = CC(I,  5,K) - CC(IC,  4,K)
      TR5 = CC(I-1,3,K) - CC(IC-1,2,K)
      TR2 = CC(I-1,3,K) + CC(IC-1,2,K)
      TR4 = CC(I-1,5,K) - CC(IC-1,4,K)
      TR3 = CC(I-1,5,K) + CC(IC-1,4,K)
      CH(I-1,K,1) = CC(I-1,1,K) + TR2 + TR3
      CH(I,  K,1) = CC(I,  1,K) + TI2 + TI3
      CR2 = CC(I-1,1,K) + TR11*TR2 + TR12*TR3
      CI2 = CC(I,  1,K) + TR11*TI2 + TR12*TI3
      CR3 = CC(I-1,1,K) + TR12*TR2 + TR11*TR3
      CI3 = CC(I,  1,K) + TR12*TI2 + TR11*TI3
      CR5 = TI11*TR5 + TI12*TR4
      CI5 = TI11*TI5 + TI12*TI4
      CR4 = TI12*TR5 - TI11*TR4
      CI4 = TI12*TI5 - TI11*TI4
      DR3 = CR3 - CI4
      DR4 = CR3 + CI4
      DI3 = CI3 + CR4
      DI4 = CI3 - CR4
      DR5 = CR2 + CI5
      DR2 = CR2 - CI5
      DI5 = CI2 - CR5
      DI2 = CI2 + CR5
      CH(I-1,K,2) = WA1(I-2)*DR2 - WA1(I-1)*DI2
      CH(I,  K,2) = WA1(I-2)*DI2 + WA1(I-1)*DR2
      CH(I-1,K,3) = WA2(I-2)*DR3 - WA2(I-1)*DI3
      CH(I,  K,3) = WA2(I-2)*DI3 + WA2(I-1)*DR3
      CH(I-1,K,4) = WA3(I-2)*DR4 - WA3(I-1)*DI4
      CH(I,  K,4) = WA3(I-2)*DI4 + WA3(I-1)*DR4
      CH(I-1,K,5) = WA4(I-2)*DR5 - WA4(I-1)*DI5
      CH(I,  K,5) = WA4(I-2)*DI5 + WA4(I-1)*DR5
    END DO
  END DO

END IF

RETURN
END
