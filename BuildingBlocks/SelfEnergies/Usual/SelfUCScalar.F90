double complex function SelfUCScalarUsual(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(9)

 amplitudes(1) = (0.03125D0*CKM21*CKMC11*EL2*MD2*Yuk3*B0(x, MD2, MHp2))/(MW2*PI2*SW2*TB)

 amplitudes(2) = (0.03125D0*CKM22*CKMC12*EL2*MS2*Yuk3*B0(x, MHp2, MS2))/(MW2*PI2*SW2*TB)

 amplitudes(3) = (0.03125D0*CKM23*CKMC13*EL2*MB2*Yuk3*B0(x, MB2, MHp2))/(MW2*PI2*SW2*TB)

 amplitudes(4) = (-0.03125D0*CKM21*CKMC11*EL2*MD2*B0(x, MD2, GaugeXiW*MW2))/(MW2*PI2*SW2)

 amplitudes(5) = (-0.03125D0*CKM22*CKMC12*EL2*MS2*B0(x, MS2, GaugeXiW*MW2))/(MW2*PI2*SW2)

 amplitudes(6) = (-0.03125D0*CKM23*CKMC13*EL2*MB2*B0(x, MB2, GaugeXiW*MW2))/(MW2*PI2*SW2)

 amplitudes(7) = 0.D0

 amplitudes(8) = 0.D0

 amplitudes(9) = 0.D0

  totalAmplitude = (0D0,0D0)
 do j=1,9
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 SelfUCScalarUsual = totalAmplitude
end function SelfUCScalarUsual

