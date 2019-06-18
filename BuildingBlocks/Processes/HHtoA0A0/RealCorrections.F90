double precision function HHtoA0A0Real()
 use constants
 use counterterms
 implicit none
#include "looptools.h"

 double precision :: totalAmplitude
 double precision :: p2, p3, E2, E3, m1, m2, m3, kappa, beta1, beta2, beta3
 double precision :: I11, I22, I33, I12, I13, I23
 double precision :: IFin, Id1Fin, Id2Fin, Id3Fin, Iu2d1Fin, Iu3d1Fin, Iu1d2Fin, Iu1d3Fin, Iu2d3Fin, Iu3d2Fin

 m1 = MHH
 m2 = MA0
 m3 = MA0

 kappa = DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 - 2D0*m2**2*m3**2)
 beta1 = (m1**2 - m2**2 - m3**2 + kappa)/(2D0*m2*m3)
 beta2 = (m1**2 - m2**2 + m3**2 - kappa)/(2D0*m1*m3)
 beta3 = (m1**2 + m2**2 - m3**2 - kappa)/(2D0*m1*m2)

 p2 = kappa/(2D0*m1)
 p3 = kappa/(2D0*m1)
 E2 = DSQRT( m2**2 + p2**2 )
 E3 = DSQRT( m3**2 + p3**2 )

 I11 = ( kappa*DLOG(kappa**2/(DSQRT(IRLambda)*m1*m2*m3)) - kappa - (m2**2-m3**2)*DLOG(beta2/beta3) - m1**2*DLOG(beta1) )&
     &/(4D0*m1**4)
 I22 = ( kappa*DLOG(kappa**2/(DSQRT(IRLambda)*m1*m2*m3)) - kappa - (m1**2-m3**2)*DLOG(beta1/beta3) - m2**2*DLOG(beta2) )&
     &/(4D0*m1**2*m2**2)
 I33 = ( kappa*DLOG(kappa**2/(DSQRT(IRLambda)*m1*m2*m3)) - kappa - (m1**2-m2**2)*DLOG(beta1/beta2) - m3**2*DLOG(beta3) )&
     &/(4D0*m1**2*m3**2)
 I12 = ( - 2D0*DLOG(DSQRT(IRLambda)*m1*m2*m3/kappa**2)*DLOG(beta3) + 2D0*DLOG(beta3)**2 - DLOG(beta1)**2 - DLOG(beta2)**2 + &
     & 2D0*Li2(1D0 - beta3**2) - Li2(1D0 - beta1**2) - Li2(1D0 - beta2**2) )/(4D0*m1**2)
 I13 = ( - 2D0*DLOG(DSQRT(IRLambda)*m1*m2*m3/kappa**2)*DLOG(beta2) + 2D0*DLOG(beta2)**2 - DLOG(beta1)**2 - DLOG(beta3)**2 + &
     & 2D0*Li2(1D0 - beta2**2) - Li2(1D0 - beta1**2) - Li2(1D0 - beta3**2) )/(4D0*m1**2)
 I23 = ( - 2D0*DLOG(DSQRT(IRLambda)*m1*m2*m3/kappa**2)*DLOG(beta1) + 2D0*DLOG(beta1)**2 - DLOG(beta2)**2 - DLOG(beta3)**2 + &
     & 2D0*Li2(1D0 - beta1**2) - Li2(1D0 - beta2**2) - Li2(1D0 - beta3**2) )/(4D0*m1**2)
 IFin = ( kappa/2D0*(m1**2 + m2**2 + m3**2) + 2D0*m1**2*m2**2*DLOG(beta3) + 2D0*m1**2*m3**2*DLOG(beta2) + &
     & 2D0*m2**2*m3**2*DLOG(beta1) )/(4D0*m1**2)
 Id1Fin = ( - 2D0*m2**2*DLOG(beta3) - 2D0*m3**2*DLOG(beta2) - kappa )/(4D0*m1**2)
 Id2Fin = ( - 2D0*m1**2*DLOG(beta3) - 2D0*m3**2*DLOG(beta1) - kappa )/(4D0*m1**2)
 Id3Fin = ( - 2D0*m1**2*DLOG(beta2) - 2D0*m2**2*DLOG(beta1) - kappa )/(4D0*m1**2)
 Iu2d1Fin = ( m2**4*DLOG(beta3) - m3**2*(2D0*m1**2 - 2D0*m2**2 + m3**2)*DLOG(beta2) &
     & - kappa/4D0*(m1**2 - 3D0*m2**2 + 5D0*m3**2) )/(4D0*m1**2)
 Iu3d1Fin = ( m3**4*DLOG(beta2) - m2**2*(2D0*m1**2 - 2D0*m3**2 + m2**2)*DLOG(beta3) &
     & - kappa/4D0*(m1**2 - 3D0*m3**2 + 5D0*m2**2) )/(4D0*m1**2)
 Iu1d2Fin = ( m1**4*DLOG(beta3) - m3**2*(2D0*m2**2 - 2D0*m1**2 + m3**2)*DLOG(beta1) &
     & - kappa/4D0*(m2**2 - 3D0*m1**2 + 5D0*m3**2) )/(4D0*m1**2)
 Iu1d3Fin = ( m1**4*DLOG(beta2) - m2**2*(2D0*m3**2 - 2D0*m1**2 + m2**2)*DLOG(beta1) &
     & - kappa/4D0*(m3**2 - 3D0*m1**2 + 5D0*m2**2) )/(4D0*m1**2)
 Iu2d3Fin = ( m2**4*DLOG(beta1) - m1**2*(2D0*m3**2 - 2D0*m2**2 + m1**2)*DLOG(beta2) &
     & - kappa/4D0*(m3**2 - 3D0*m2**2 + 5D0*m1**2) )/(4D0*m1**2)
 Iu3d2Fin = ( m3**4*DLOG(beta1) - m1**2*(2D0*m2**2 - 2D0*m3**2 + m1**2)*DLOG(beta3) &
     & - kappa/4D0*(m2**2 - 3D0*m3**2 + 5D0*m1**2) )/(4D0*m1**2)

 totalAmplitude = 0D0

 HHtoA0A0Real = totalAmplitude
end function HHtoA0A0Real
