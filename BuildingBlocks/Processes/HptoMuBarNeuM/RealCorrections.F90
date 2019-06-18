double precision function HptoMuBarNeuMReal()
 use constants
 use counterterms
 implicit none
#include "looptools.h"

 double precision :: totalAmplitude
 double precision :: p2, p3, E2, E3, m1, m2, m3, kappa, beta1, beta2, beta3
 double precision :: I11, I22, I33, I12, I13, I23
 double precision :: IFin, Id1Fin, Id2Fin, Id3Fin, Iu2d1Fin, Iu3d1Fin, Iu1d2Fin, Iu1d3Fin, Iu2d3Fin, Iu3d2Fin
 double precision :: OLL00, OLL01, OLL02, OLL11, OLL12, OLL22, OLR00, OLR01, OLR02, OLR11, OLR12, OLR22, OLLFull, OLRFull

 m1 = MHp
 m2 = MM
 m3 = 0

 !kappa = DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 - 2D0*m2**2*m3**2)
 !beta1 = (m1**2 - m2**2 - m3**2 + kappa)/(2D0*m2*m3)
 !beta2 = (m1**2 - m2**2 + m3**2 - kappa)/(2D0*m1*m3)
 !beta3 = (m1**2 + m2**2 - m3**2 - kappa)/(2D0*m1*m2)

 !p2 = kappa/(2D0*m1)
 !p3 = kappa/(2D0*m1)
 !E2 = DSQRT( m2**2 + p2**2 )
 !E3 = DSQRT( m3**2 + p3**2 )

 !I11 = ( kappa*DLOG(kappa**2/(DSQRT(IRLambda)*m1*m2*m3)) - kappa - (m2**2-m3**2)*DLOG(beta2/beta3) - m1**2*DLOG(beta1) )&
 !    &/(4D0*m1**4)
 !I22 = ( kappa*DLOG(kappa**2/(DSQRT(IRLambda)*m1*m2*m3)) - kappa - (m1**2-m3**2)*DLOG(beta1/beta3) - m2**2*DLOG(beta2) )&
 !    &/(4D0*m1**2*m2**2)
 !I33 = ( kappa*DLOG(kappa**2/(DSQRT(IRLambda)*m1*m2*m3)) - kappa - (m1**2-m2**2)*DLOG(beta1/beta2) - m3**2*DLOG(beta3) )&
 !    &/(4D0*m1**2*m3**2)
 !I12 = ( - 2D0*DLOG(DSQRT(IRLambda)*m1*m2*m3/kappa**2)*DLOG(beta3) + 2D0*DLOG(beta3)**2 - DLOG(beta1)**2 - DLOG(beta2)**2 + &
 !    & 2D0*Li2(1D0 - beta3**2) - Li2(1D0 - beta1**2) - Li2(1D0 - beta2**2) )/(4D0*m1**2)
 !I13 = ( - 2D0*DLOG(DSQRT(IRLambda)*m1*m2*m3/kappa**2)*DLOG(beta2) + 2D0*DLOG(beta2)**2 - DLOG(beta1)**2 - DLOG(beta3)**2 + &
 !    & 2D0*Li2(1D0 - beta2**2) - Li2(1D0 - beta1**2) - Li2(1D0 - beta3**2) )/(4D0*m1**2)
 !I23 = ( - 2D0*DLOG(DSQRT(IRLambda)*m1*m2*m3/kappa**2)*DLOG(beta1) + 2D0*DLOG(beta1)**2 - DLOG(beta2)**2 - DLOG(beta3)**2 + &
 !    & 2D0*Li2(1D0 - beta1**2) - Li2(1D0 - beta2**2) - Li2(1D0 - beta3**2) )/(4D0*m1**2)
 !IFin = ( kappa/2D0*(m1**2 + m2**2 + m3**2) + 2D0*m1**2*m2**2*DLOG(beta3) + 2D0*m1**2*m3**2*DLOG(beta2) + &
 !    & 2D0*m2**2*m3**2*DLOG(beta1) )/(4D0*m1**2)
 !Id1Fin = ( - 2D0*m2**2*DLOG(beta3) - 2D0*m3**2*DLOG(beta2) - kappa )/(4D0*m1**2)
 !Id2Fin = ( - 2D0*m1**2*DLOG(beta3) - 2D0*m3**2*DLOG(beta1) - kappa )/(4D0*m1**2)
 !Id3Fin = ( - 2D0*m1**2*DLOG(beta2) - 2D0*m2**2*DLOG(beta1) - kappa )/(4D0*m1**2)
 !Iu2d1Fin = ( m2**4*DLOG(beta3) - m3**2*(2D0*m1**2 - 2D0*m2**2 + m3**2)*DLOG(beta2) &
 !    & - kappa/4D0*(m1**2 - 3D0*m2**2 + 5D0*m3**2) )/(4D0*m1**2)
 !Iu3d1Fin = ( m3**4*DLOG(beta2) - m2**2*(2D0*m1**2 - 2D0*m3**2 + m2**2)*DLOG(beta3) &
 !    & - kappa/4D0*(m1**2 - 3D0*m3**2 + 5D0*m2**2) )/(4D0*m1**2)
 !Iu1d2Fin = ( m1**4*DLOG(beta3) - m3**2*(2D0*m2**2 - 2D0*m1**2 + m3**2)*DLOG(beta1) &
 !    & - kappa/4D0*(m2**2 - 3D0*m1**2 + 5D0*m3**2) )/(4D0*m1**2)
 !Iu1d3Fin = ( m1**4*DLOG(beta2) - m2**2*(2D0*m3**2 - 2D0*m1**2 + m2**2)*DLOG(beta1) &
 !    & - kappa/4D0*(m3**2 - 3D0*m1**2 + 5D0*m2**2) )/(4D0*m1**2)
 !Iu2d3Fin = ( m2**4*DLOG(beta1) - m1**2*(2D0*m3**2 - 2D0*m2**2 + m1**2)*DLOG(beta2) &
 !    & - kappa/4D0*(m3**2 - 3D0*m2**2 + 5D0*m1**2) )/(4D0*m1**2)
 !Iu3d2Fin = ( m3**4*DLOG(beta1) - m1**2*(2D0*m2**2 - 2D0*m3**2 + m1**2)*DLOG(beta3) &
 !    & - kappa/4D0*(m2**2 - 3D0*m3**2 + 5D0*m1**2) )/(4D0*m1**2)

 I11 = -(m1**2*DLOG(m1**2/m2 - m2) + m2**2*DLOG(m1**2/(m1**2*m2 - m2**3)) - &
    & (m1**2 - m2**2)*(-1D0 + DLOG((m1**2 - m2**2)**2/(m1*m2*DSQRT(IRLambda)))))/(4D0*m1**4)
 I22 = -(m1**2*DLOG(-m1 + m1**3/m2**2) + m2**2*DLOG(m1/(m1**2 - m2**2)) - &
    & (m1**2 - m2**2)*(-1D0 + DLOG((m1**2 - m2**2)**2/(m1*m2*DSQRT(IRLambda)))))/(4D0*m1**2*m2**2)
 I12 = (DLOG(m2/m1)*(-3D0*DLOG(m1) + DLOG(m2) + 2D0*DLOG((m1**2 - m2**2)/DSQRT(IRLambda))) + &
    & 2D0*Li2(1D0 - m2**2/m1**2))/(4D0*m1**2)
 IFin = (m1**4 - m2**4 + 4D0*m1**2*m2**2*DLOG(m2/m1))/(8D0*m1**2)
 Id1Fin = -(m1**2 - m2**2 + 2D0*m2**2*DLOG(m2/m1))/(4D0*m1**2)
 Id2Fin = (-1D0 + m2**2/m1**2 - 2D0*DLOG(m2/m1))/4D0
 Iu2d1Fin = -(m1**4 - 4D0*m1**2*m2**2 + 3D0*m2**4 - 4D0*m2**4*DLOG(m2/m1))/(16D0*m1**2) 
 Iu1d2Fin = (3D0*m1**4 - 4D0*m1**2*m2**2 + m2**4 + 4D0*m1**4*DLOG(m2/m1))/(16D0*m1**2)

 !OLR00 = 16D0*I11*m1**2*m2*m3
 !OLR01 = -16D0*Id1Fin*m2*m3 - 16D0*Id2Fin*m2*m3 + I12*(-16D0*m1**2*m2*m3 - 16D0*m2**3*m3 + 16D0*m2*m3**3)
 !OLR02 = 16D0*Id1Fin*m2*m3 + I13*(16D0*m1**2*m2*m3 - 16D0*m2**3*m3 + 16D0*m2*m3**3)
 !OLR11 = 16D0*I22*m2**3*m3
 !OLR12 = 16D0*Id2Fin*m2*m3 + I23*(-16D0*m1**2*m2*m3 + 16D0*m2**3*m3 + 16D0*m2*m3**3)
 !OLR22 = 16D0*Id3Fin*m2*m3 + 16D0*I33*m2*m3**3
 OLL00 = -8D0*Id1Fin*m1**2 + I11*(-8D0*m1**4 + 8D0*m1**2*m2**2 + 8D0*m1**2*m3**2)
 OLL01 = 4D0*IFin + 4D0*Iu1d2Fin + Id2Fin*(12D0*m1**2 - 4D0*m2**2 - 12D0*m3**2) + Id1Fin*(-8D0*m2**2 - 8D0*m3**2) + &
     & I12*(8D0*m1**4 - 8D0*m2**4 - 16D0*m1**2*m3**2 + 8D0*m3**4)
 !OLL02 = -4D0*IFin - 4D0*Iu1d3Fin + Id3Fin*(-12D0*m1**2 + 12D0*m2**2 + 4D0*m3**2) + Id1Fin*(8D0*m2**2 + 8D0*m3**2) + &
 !    & I13*(-8D0*m1**4 + 16D0*m1**2*m2**2 - 8D0*m2**4 + 8D0*m3**4)
 OLL11 = Id2Fin*(4D0*m1**2 + 4D0*m2**2 - 4D0*m3**2) + I22*(-8D0*m1**2*m2**2 + 8D0*m2**4 + 8D0*m2**2*m3**2)
 !OLL12 = 8D0*IFin + 4D0*Iu3d2Fin + 4D0*Iu2d3Fin + Id3Fin*(-12D0*m1**2 + 12D0*m2**2 + 4D0*m3**2) + Id2Fin*(-12D0*m1**2 + &
 !    & + 4D0*m2**2 + 12D0*m3**2 ) + I23*(8D0*m1**4 - 16D0*m1**2*m2**2 + 8D0*m2**4 - 16D0*m1**2*m3**2 + 16D0*m2**2*m3**2&
 !    & + 8D0*m3**4)
 !OLL22 = Id3Fin*(4D0*m1**2 - 4D0*m2**2 + 4D0*m3**2) + I33*(-8D0*m1**2*m3**2 + 8D0*m2**2*m3**2 + 8D0*m3**4)

 OLLFull = OLL00 - OLL01 + OLL11
 OLRFull = 0D0

 totalAmplitude = 1D0*EL2*m2**2*Yuk6**2/(2D0*MW2*SW2)*(EL2/(2D0*(4D0*PI)**3*m1))*&
                &(16D0*PI*m1**3)/DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 - 2D0*m2**2*m3**2 )*&
                &( 1D0*( OLLFull ) + 0D0*( OLRFull ) )

 HptoMuBarNeuMReal = totalAmplitude
end function HptoMuBarNeuMReal
