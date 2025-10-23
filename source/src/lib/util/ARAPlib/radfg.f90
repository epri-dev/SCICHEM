SUBROUTINE radfg (IDO,IP,L1,IDL1,CC,C1,C2,CH,CH2,WA)

IMPLICIT NONE

!INTEGER IDO, IP, L1, IDL1
!REAL CH(IDO,L1,IP), CC(IDO,IP,L1), C1(IDO,L1,IP), C2(IDL1,IP)
!REAl CH2(IDL1,IP), WA(1)

INTEGER                    :: IDO, IP, L1, IDL1
REAL, DIMENSION(IDO,IP,L1) :: CC
REAL, DIMENSION(IDO,L1,IP) :: C1
REAL, DIMENSION(IDO,L1,IP) :: CH
REAL, DIMENSION(IDL1,IP)   :: C2, CH2
REAL, DIMENSION(*)         :: WA

REAL, PARAMETER :: TPI = 6.28318530717959

REAL ARG, DCP, DSP, AR1, AI1, AR1H, AR2H, DC2, DS2, AR2, AI2
INTEGER IPPH, IPP2, IDP2, NBD, IK, J, IS, K, I, IDIJ, JC, L, LC, IC, J2

ARG = TPI/FLOAT(IP)
DCP = COS(ARG)
DSP = SIN(ARG)
IPPH = (IP+1)/2
IPP2 = IP+2
IDP2 = IDO+2
NBD = (IDO-1)/2

IF (IDO /= 1) THEN

  DO IK = 1,IDL1
    CH2(IK,1) = C2(IK,1)
  END DO

  DO J = 2,IP
    DO K = 1,L1
      CH(1,K,J) = C1(1,K,J)
    END DO
  END DO

  IF (NBD <= L1) THEN

    IS = -IDO
    DO J = 2,IP
      IS = IS+IDO
      IDIJ = IS
      DO I = 3,IDO,2
        IDIJ = IDIJ+2
        DO K = 1,L1
          CH(I-1,K,J) = WA(IDIJ-1)*C1(I-1,K,J) + WA(IDIJ)*C1(I,  K,J)
          CH(I,  K,J) = WA(IDIJ-1)*C1(I,  K,J) - WA(IDIJ)*C1(I-1,K,J)
        END DO
      END DO
    END DO

  ELSE

    IS = -IDO

    DO J = 2,IP
      IS = IS+IDO
      DO K = 1,L1
        IDIJ = IS
        DO I = 3,IDO,2
          IDIJ = IDIJ+2
          CH(I-1,K,J) = WA(IDIJ-1)*C1(I-1,K,J) + WA(IDIJ)*C1(I,  K,J)
          CH(I,  K,J) = WA(IDIJ-1)*C1(I,  K,J) - WA(IDIJ)*C1(I-1,K,J)
        END DO
      END DO
    END DO

  END IF

  IF (NBD >= L1) THEN

    DO J = 2,IPPH
      JC = IPP2-J
      DO K = 1,L1
        DO I = 3,IDO,2
          C1(I-1,K,J ) = CH(I-1,K,J ) + CH(I-1,K,JC)
          C1(I-1,K,JC) = CH(I,  K,J ) - CH(I,  K,JC)
          C1(I,  K,J ) = CH(I,  K,J ) + CH(I,  K,JC)
          C1(I,  K,JC) = CH(I-1,K,JC) - CH(I-1,K,J)
        END DO
      END DO
    END DO

  ELSE

    DO J = 2,IPPH
     JC = IPP2-J
     DO I = 3,IDO,2
       DO K = 1,L1
         C1(I-1,K,J ) = CH(I-1,K,J ) + CH(I-1,K,JC)
         C1(I-1,K,JC) = CH(I,  K,J ) - CH(I,  K,JC)
         C1(I,  K,J ) = CH(I,  K,J ) + CH(I,  K,JC)
         C1(I,  K,JC) = CH(I-1,K,JC) - CH(I-1,K,J)
       END DO
      END DO
    END DO

  END IF

ELSE

  DO IK = 1,IDL1
    C2(IK,1) = CH2(IK,1)
  END DO

END IF

DO J = 2,IPPH
  JC = IPP2-J
  DO K = 1,L1
    C1(1,K,J ) = CH(1,K,J ) + CH(1,K,JC)
    C1(1,K,JC) = CH(1,K,JC) - CH(1,K,J)
  END DO
END DO

AR1 = 1.
AI1 = 0.

DO L = 2,IPPH

  LC = IPP2-L
  AR1H = DCP*AR1-DSP*AI1
  AI1  = DCP*AI1+DSP*AR1
  AR1  = AR1H
  DO IK = 1,IDL1
    CH2(IK,L ) = C2(IK,1) + AR1*C2(IK,2)
    CH2(IK,LC) = AI1*C2(IK,IP)
  END DO

  DC2 = AR1
  DS2 = AI1
  AR2 = AR1
  AI2 = AI1
  DO J = 3,IPPH
    JC = IPP2-J
    AR2H = DC2*AR2-DS2*AI2
    AI2 = DC2*AI2+DS2*AR2
    AR2 = AR2H
    DO IK = 1,IDL1
      CH2(IK,L ) = CH2(IK,L ) + AR2*C2(IK,J)
      CH2(IK,LC) = CH2(IK,LC) + AI2*C2(IK,JC)
    END DO
  END DO

END DO

DO J = 2,IPPH
  DO IK=1,IDL1
    CH2(IK,1) = CH2(IK,1)+C2(IK,J)
  END DO
END DO

IF (IDO >= L1) THEN

  DO K=1,L1
    DO I=1,IDO
      CC(I,1,K) = CH(I,K,1)
    END DO
  END DO

ELSE

  DO I=1,IDO
    DO K=1,L1
      CC(I,1,K) = CH(I,K,1)
    END DO
  END DO

END IF

DO J = 2,IPPH
  JC = IPP2-J
  J2 = J+J
  DO K = 1,L1
    CC(IDO,J2-2,K) = CH(1,K,J)
    CC(1,  J2-1,K) = CH(1,K,JC)
  END DO
END DO

IF (IDO == 1) RETURN

IF (NBD >= L1) THEN

  DO J = 2,IPPH
    JC = IPP2-J
    J2 = J+J
    DO K = 1,L1
      DO I = 3,IDO,2
        IC = IDP2-I
        CC(I-1, J2-1,K) = CH(I-1,K,J ) + CH(I-1,K,JC)
        CC(IC-1,J2-2,K) = CH(I-1,K,J ) - CH(I-1,K,JC)
        CC(I,   J2-1,K) = CH(I,  K,J ) + CH(I,  K,JC)
        CC(IC,  J2-2,K) = CH(I,  K,JC) - CH(I,  K,J)
      END DO
    END DO
  END DO

ELSE

  DO J = 2,IPPH
    JC = IPP2-J
    J2 = J+J
    DO I = 3,IDO,2
      IC = IDP2-I
      DO K = 1,L1
         CC(I-1, J2-1,K) = CH(I-1,K,J ) + CH(I-1,K,JC)
         CC(IC-1,J2-2,K) = CH(I-1,K,J ) - CH(I-1,K,JC)
         CC(I,   J2-1,K) = CH(I,  K,J ) + CH(I,  K,JC)
         CC(IC,  J2-2,K) = CH(I,  K,JC) - CH(I,  K,J)
      END DO
    END DO
  END DO

END IF

RETURN
END
