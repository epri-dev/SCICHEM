SUBROUTINE radf5( IDO,L1,CC,CH,WA1,WA2,WA3,WA4 )

IMPLICIT NONE

!INTEGER IDO,L1
!REAL CC(IDO,L1,5), CH(IDO,5,L1), WA1(1), WA2(1), WA3(1), WA4(1)

INTEGER,                   INTENT( IN  ) :: IDO, L1
REAL, DIMENSION(IDO,L1,5), INTENT( IN  ) :: CC
REAL, DIMENSION(IDO,5,L1), INTENT( OUT ) :: CH
REAL, DIMENSION(IDO),      INTENT( IN  ) :: WA1, WA2, WA3, WA4

REAL, PARAMETER :: TR11 =  0.309016994374947
REAL, PARAMETER :: TI11 =  0.951056516295154
REAL, PARAMETER :: TR12 = -0.809016994374947
REAL, PARAMETER :: TI12 =  0.587785252292473

INTEGER K, IDP2, I, IC
REAL    CR2, CI5, CR3, CI4, DR2, DI2, DR3, DI3, DR4, DI4, TR4, TI4
REAL    DR5, DI5, CR5, CR4, CI2, CI3, TR2, TI2, TR3, TI3, TR5, TI5

DO K = 1,L1
  CR2 = CC(1,K,5) + CC(1,K,2)
  CI5 = CC(1,K,5) - CC(1,K,2)
  CR3 = CC(1,K,4) + CC(1,K,3)
  CI4 = CC(1,K,4) - CC(1,K,3)
  CH(1,1,K)   = CC(1,K,1) + CR2 + CR3
  CH(IDO,2,K) = CC(1,K,1) + TR11*CR2 + TR12*CR3
  CH(1,3,K)   = TI11*CI5 + TI12*CI4
  CH(IDO,4,K) = CC(1,K,1) + TR12*CR2 + TR11*CR3
  CH(1,5,K)   = TI12*CI5 - TI11*CI4
END DO

IF( IDO == 1 )RETURN

IDP2 = IDO+2

IF( (IDO-1)/2 >= L1 )THEN

  DO K = 1,L1
    DO I = 3,IDO,2
      IC = IDP2-I
      DR2 = WA1(I-2)*CC(I-1,K,2) + WA1(I-1)*CC(I,K,2)
      DI2 = WA1(I-2)*CC(I,K,2)   - WA1(I-1)*CC(I-1,K,2)
      DR3 = WA2(I-2)*CC(I-1,K,3) + WA2(I-1)*CC(I,K,3)
      DI3 = WA2(I-2)*CC(I,K,3)   - WA2(I-1)*CC(I-1,K,3)
      DR4 = WA3(I-2)*CC(I-1,K,4) + WA3(I-1)*CC(I,K,4)
      DI4 = WA3(I-2)*CC(I,K,4)   - WA3(I-1)*CC(I-1,K,4)
      DR5 = WA4(I-2)*CC(I-1,K,5) + WA4(I-1)*CC(I,K,5)
      DI5 = WA4(I-2)*CC(I,K,5)   - WA4(I-1)*CC(I-1,K,5)
      CR2 = DR2 + DR5
      CI5 = DR5 - DR2
      CR5 = DI2 - DI5
      CI2 = DI2 + DI5
      CR3 = DR3 + DR4
      CI4 = DR4 - DR3
      CR4 = DI3 - DI4
      CI3 = DI3 + DI4
      CH(I-1,1,K) = CC(I-1,K,1) + CR2 + CR3
      CH(I,1,K)   = CC(I,K,1) + CI2 + CI3
      TR2 = CC(I-1,K,1) + TR11*CR2 + TR12*CR3
      TI2 = CC(I,  K,1) + TR11*CI2 + TR12*CI3
      TR3 = CC(I-1,K,1) + TR12*CR2 + TR11*CR3
      TI3 = CC(I,  K,1) + TR12*CI2 + TR11*CI3
      TR5 = TI11*CR5 + TI12*CR4
      TI5 = TI11*CI5 + TI12*CI4
      TR4 = TI12*CR5 - TI11*CR4
      TI4 = TI12*CI5 - TI11*CI4
      CH(I-1, 3,K) = TR2 + TR5
      CH(IC-1,2,K) = TR2 - TR5
      CH(I, 3,K)   = TI2 + TI5
      CH(IC,2,K)   = TI5 - TI2
      CH(I-1, 5,K) = TR3 + TR4
      CH(IC-1,4,K) = TR3 - TR4
      CH(I, 5,K)   = TI3 + TI4
      CH(IC,4,K)   = TI4 - TI3
    END DO
  END DO

ELSE

  DO I = 3,IDO,2
    IC = IDP2 - I
    DO K = 1,L1
      DR2 = WA1(I-2)*CC(I-1,K,2) + WA1(I-1)*CC(I,K,2)
      DI2 = WA1(I-2)*CC(I,  K,2) - WA1(I-1)*CC(I-1,K,2)
      DR3 = WA2(I-2)*CC(I-1,K,3) + WA2(I-1)*CC(I,K,3)
      DI3 = WA2(I-2)*CC(I,  K,3) - WA2(I-1)*CC(I-1,K,3)
      DR4 = WA3(I-2)*CC(I-1,K,4) + WA3(I-1)*CC(I,K,4)
      DI4 = WA3(I-2)*CC(I,  K,4) - WA3(I-1)*CC(I-1,K,4)
      DR5 = WA4(I-2)*CC(I-1,K,5) + WA4(I-1)*CC(I,K,5)
      DI5 = WA4(I-2)*CC(I,  K,5) - WA4(I-1)*CC(I-1,K,5)
      CR2 = DR2 + DR5
      CI5 = DR5 - DR2
      CR5 = DI2 - DI5
      CI2 = DI2 + DI5
      CR3 = DR3 + DR4
      CI4 = DR4 - DR3
      CR4 = DI3 - DI4
      CI3 = DI3 + DI4
      CH(I-1,1,K) = CC(I-1,K,1) + CR2 + CR3
      CH(I,  1,K) = CC(I,  K,1) + CI2 + CI3
      TR2 = CC(I-1,K,1) + TR11*CR2 + TR12*CR3
      TI2 = CC(I,  K,1) + TR11*CI2 + TR12*CI3
      TR3 = CC(I-1,K,1) + TR12*CR2 + TR11*CR3
      TI3 = CC(I,  K,1) + TR12*CI2 + TR11*CI3
      TR5 = TI11*CR5 + TI12*CR4
      TI5 = TI11*CI5 + TI12*CI4
      TR4 = TI12*CR5 - TI11*CR4
      TI4 = TI12*CI5 - TI11*CI4
      CH(I-1, 3,K) = TR2 + TR5
      CH(IC-1,2,K) = TR2 - TR5
      CH(I, 3,K)   = TI2 + TI5
      CH(IC,2,K)   = TI5 - TI2
      CH(I-1, 5,K) = TR3 + TR4
      CH(IC-1,4,K) = TR3 - TR4
      CH(I, 5,K)   = TI3 + TI4
      CH(IC,4,K)   = TI4 - TI3
    END DO
  END DO

END IF

RETURN
END
