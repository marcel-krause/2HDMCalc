double complex function DSelfG0A0(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(21)

 amplitudes(1) = 0.D0

 amplitudes(2) = 0.D0

 amplitudes(3) = 0.D0

 amplitudes(4) = 0.D0

 amplitudes(5) = 0.D0

 amplitudes(6) = 0.D0

 amplitudes(7) = (0.03125D0*EL2*ME2*Yuk6*(-1.D0*B0(x, ME2, ME2) - 1.D0*x*DB0(x, ME2, ME2)))/(MW2*PI2*SW2)

 amplitudes(8) = (0.03125D0*EL2*MM2*Yuk6*(-1.D0*B0(x, MM2, MM2) - 1.D0*x*DB0(x, MM2, MM2)))/(MW2*PI2*SW2)

 amplitudes(9) = (0.03125D0*EL2*ML2*Yuk6*(-1.D0*B0(x, ML2, ML2) - 1.D0*x*DB0(x, ML2, ML2)))/(MW2*PI2*SW2)

 amplitudes(10) = (-0.09375D0*EL2*MU2*(-1.D0*B0(x, MU2, MU2) - 1.D0*x*DB0(x, MU2, MU2)))/(MW2*PI2*SW2*TB)

 amplitudes(11) = (-0.09375D0*EL2*MC2*(-1.D0*B0(x, MC2, MC2) - 1.D0*x*DB0(x, MC2, MC2)))/(MW2*PI2*SW2*TB)

 amplitudes(12) = (-0.09375D0*EL2*MT2*(-1.D0*B0(x, MT2, MT2) - 1.D0*x*DB0(x, MT2, MT2)))/(MW2*PI2*SW2*TB)

 amplitudes(13) = (0.09375D0*EL2*MD2*Yuk3*(-1.D0*B0(x, MD2, MD2) - 1.D0*x*DB0(x, MD2, MD2)))/(MW2*PI2*SW2)

 amplitudes(14) = (0.09375D0*EL2*MS2*Yuk3*(-1.D0*B0(x, MS2, MS2) - 1.D0*x*DB0(x, MS2, MS2)))/(MW2*PI2*SW2)

 amplitudes(15) = (0.09375D0*EL2*MB2*Yuk3*(-1.D0*B0(x, MB2, MB2) - 1.D0*x*DB0(x, MB2, MB2)))/(MW2*PI2*SW2)

 amplitudes(16) = (-0.015625D0*CBA*(MA02 - 1.D0*Mh02)*(EL2*(2.D0*MA02 - 1.D0*Mh02)*S2B*SBA + 2.D0*CAB*(EL2*Mh02 - 2.D0*Lambda5*MW&
  &2*SW2))*DB0(x, MA02, Mh02))/ (MW2*PI2*S2B*SW2)

 amplitudes(17) = (0.015625D0*(MA02 - 1.D0*MHH2)*SBA*(CBA*EL2*(2.D0*MA02 - 1.D0*MHH2)*S2B + 2.D0*SAB*(EL2*MHH2 - 2.D0*Lambda5*MW2&
  &*SW2))*DB0(x, MA02, MHH2))/ (MW2*PI2*S2B*SW2)

 amplitudes(18) = (0.015625D0*CBA*EL2*Mh02*(-1.D0*MA02 + Mh02)*SBA*DB0(x, Mh02, GaugeXiZ*MZ2))/(MW2*PI2*SW2)

 amplitudes(19) = (0.015625D0*CBA*EL2*(MA02 - 1.D0*MHH2)*MHH2*SBA*DB0(x, MHH2, GaugeXiZ*MZ2))/(MW2*PI2*SW2)

 amplitudes(20) = (0.015625D0*CBA*EL2*SBA*(-1.D0*A0(MZ2) + A0(GaugeXiZ*MZ2) - 2.D0*Mh02*B0(x, Mh02, MZ2) - 2.D0*MZ2*B0(x, Mh02, M&
  &Z2) + 2.D0*x*B0(x, Mh02, MZ2) + 2.D0*Mh02*B0(x, Mh02, GaugeXiZ*MZ2) - 2.D0*x*B0(x, Mh02, GaugeXiZ*MZ2) - 2.D0*Mh02*MZ2*DB0(x, &
  &Mh02, MZ2) - 2.D0*Mh02*x*DB0(x, Mh02, MZ2) - 2.D0*MZ2*x*DB0(x, Mh02, MZ2) + 2.D0*Mh02*x*DB0(x, Mh02, GaugeXiZ*MZ2) + DB0(x, Mh&
  &02, MZ2)*DBLE(Mh0**INT(4.D0)) - 1.D0*DB0(x, Mh02, GaugeXiZ*MZ2)*DBLE(Mh0**INT(4.D0)) + DB0(x, Mh02, MZ2)*DBLE(MZ**INT(4.D0)) +&
  & DB0(x, Mh02, MZ2)*DBLE(x**INT(2.D0)) - 1.D0*DB0(x, Mh02, GaugeXiZ*MZ2)*DBLE(x**INT(2.D0))))/(CW2*MZ2*PI2*SW2)

 amplitudes(21) = (0.015625D0*CBA*EL2*SBA*(A0(MZ2) - 1.D0*A0(GaugeXiZ*MZ2) + 2.D0*MHH2*B0(x, MHH2, MZ2) + 2.D0*MZ2*B0(x, MHH2, MZ&
  &2) - 2.D0*x*B0(x, MHH2, MZ2) - 2.D0*MHH2*B0(x, MHH2, GaugeXiZ*MZ2) + 2.D0*x*B0(x, MHH2, GaugeXiZ*MZ2) + 2.D0*MHH2*MZ2*DB0(x, M&
  &HH2, MZ2) + 2.D0*MHH2*x*DB0(x, MHH2, MZ2) + 2.D0*MZ2*x*DB0(x, MHH2, MZ2) - 2.D0*MHH2*x*DB0(x, MHH2, GaugeXiZ*MZ2) - 1.D0*DB0(x&
  &, MHH2, MZ2)*DBLE(MHH**INT(4.D0)) + DB0(x, MHH2, GaugeXiZ*MZ2)*DBLE(MHH**INT(4.D0)) - 1.D0*DB0(x, MHH2, MZ2)*DBLE(MZ**INT(4.D0&
  &)) - 1.D0*DB0(x, MHH2, MZ2)*DBLE(x**INT(2.D0)) + DB0(x, MHH2, GaugeXiZ*MZ2)*DBLE(x**INT(2.D0))))/(CW2*MZ2*PI2*SW2)

  totalAmplitude = (0D0,0D0)
 do j=1,21
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 DSelfG0A0 = totalAmplitude
end function DSelfG0A0

