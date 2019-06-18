double complex function DSelfG0G0(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(27)

 amplitudes(1) = 0.D0

 amplitudes(2) = 0.D0

 amplitudes(3) = 0.D0

 amplitudes(4) = 0.D0

 amplitudes(5) = 0.D0

 amplitudes(6) = 0.D0

 amplitudes(7) = 0.D0

 amplitudes(8) = 0.D0

 amplitudes(9) = (-0.03125D0*EL2*ME2*(-1.D0*B0(x, ME2, ME2) - 1.D0*x*DB0(x, ME2, ME2)))/(MW2*PI2*SW2)

 amplitudes(10) = (-0.03125D0*EL2*MM2*(-1.D0*B0(x, MM2, MM2) - 1.D0*x*DB0(x, MM2, MM2)))/(MW2*PI2*SW2)

 amplitudes(11) = (-0.03125D0*EL2*ML2*(-1.D0*B0(x, ML2, ML2) - 1.D0*x*DB0(x, ML2, ML2)))/(MW2*PI2*SW2)

 amplitudes(12) = (-0.09375D0*EL2*MU2*(-1.D0*B0(x, MU2, MU2) - 1.D0*x*DB0(x, MU2, MU2)))/(MW2*PI2*SW2)

 amplitudes(13) = (-0.09375D0*EL2*MC2*(-1.D0*B0(x, MC2, MC2) - 1.D0*x*DB0(x, MC2, MC2)))/(MW2*PI2*SW2)

 amplitudes(14) = (-0.09375D0*EL2*MT2*(-1.D0*B0(x, MT2, MT2) - 1.D0*x*DB0(x, MT2, MT2)))/(MW2*PI2*SW2)

 amplitudes(15) = (-0.09375D0*EL2*MD2*(-1.D0*B0(x, MD2, MD2) - 1.D0*x*DB0(x, MD2, MD2)))/(MW2*PI2*SW2)

 amplitudes(16) = (-0.09375D0*EL2*MS2*(-1.D0*B0(x, MS2, MS2) - 1.D0*x*DB0(x, MS2, MS2)))/(MW2*PI2*SW2)

 amplitudes(17) = (-0.09375D0*EL2*MB2*(-1.D0*B0(x, MB2, MB2) - 1.D0*x*DB0(x, MB2, MB2)))/(MW2*PI2*SW2)

 amplitudes(18) = (0.015625D0*CBA2*EL2*DB0(x, MA02, Mh02)*DBLE((MA02 - 1.D0*Mh02)**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(19) = (0.015625D0*EL2*SBA2*DB0(x, MA02, MHH2)*DBLE((MA02 - 1.D0*MHH2)**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(20) = (0.015625D0*EL2*SBA2*DB0(x, Mh02, GaugeXiZ*MZ2)*DBLE(Mh0**INT(4.D0)))/(MW2*PI2*SW2)

 amplitudes(21) = (0.015625D0*CBA2*EL2*DB0(x, MHH2, GaugeXiZ*MZ2)*DBLE(MHH**INT(4.D0)))/(MW2*PI2*SW2)

 amplitudes(22) = (0.015625D0*EL2*MW2*DB0(x, GaugeXiW*MW2, GaugeXiW*MW2)*DBLE(GaugeXiW**INT(2.D0)))/(PI2*SW2)

 amplitudes(23) = (0.015625D0*EL2*MW2*DB0(x, GaugeXiW*MW2, GaugeXiW*MW2)*DBLE(GaugeXiW**INT(2.D0)))/(PI2*SW2)

 amplitudes(24) = (0.015625D0*EL2*SBA2*(-1.D0*A0(MZ2) + A0(GaugeXiZ*MZ2) - 2.D0*Mh02*B0(x, Mh02, MZ2) - 2.D0*MZ2*B0(x, Mh02, MZ2)&
  & + 2.D0*x*B0(x, Mh02, MZ2) + 2.D0*Mh02*B0(x, Mh02, GaugeXiZ*MZ2) - 2.D0*x*B0(x, Mh02, GaugeXiZ*MZ2) - 2.D0*Mh02*MZ2*DB0(x, Mh0&
  &2, MZ2) - 2.D0*Mh02*x*DB0(x, Mh02, MZ2) - 2.D0*MZ2*x*DB0(x, Mh02, MZ2) + 2.D0*Mh02*x*DB0(x, Mh02, GaugeXiZ*MZ2) + DB0(x, Mh02,&
  & MZ2)*DBLE(Mh0**INT(4.D0)) - 1.D0*DB0(x, Mh02, GaugeXiZ*MZ2)*DBLE(Mh0**INT(4.D0)) + DB0(x, Mh02, MZ2)*DBLE(MZ**INT(4.D0)) + DB&
  &0(x, Mh02, MZ2)*DBLE(x**INT(2.D0)) - 1.D0*DB0(x, Mh02, GaugeXiZ*MZ2)*DBLE(x**INT(2.D0))))/(CW2*MZ2*PI2*SW2)

 amplitudes(25) = (0.015625D0*CBA2*EL2*(-1.D0*A0(MZ2) + A0(GaugeXiZ*MZ2) - 2.D0*MHH2*B0(x, MHH2, MZ2) - 2.D0*MZ2*B0(x, MHH2, MZ2)&
  & + 2.D0*x*B0(x, MHH2, MZ2) + 2.D0*MHH2*B0(x, MHH2, GaugeXiZ*MZ2) - 2.D0*x*B0(x, MHH2, GaugeXiZ*MZ2) - 2.D0*MHH2*MZ2*DB0(x, MHH&
  &2, MZ2) - 2.D0*MHH2*x*DB0(x, MHH2, MZ2) - 2.D0*MZ2*x*DB0(x, MHH2, MZ2) + 2.D0*MHH2*x*DB0(x, MHH2, GaugeXiZ*MZ2) + DB0(x, MHH2,&
  & MZ2)*DBLE(MHH**INT(4.D0)) - 1.D0*DB0(x, MHH2, GaugeXiZ*MZ2)*DBLE(MHH**INT(4.D0)) + DB0(x, MHH2, MZ2)*DBLE(MZ**INT(4.D0)) + DB&
  &0(x, MHH2, MZ2)*DBLE(x**INT(2.D0)) - 1.D0*DB0(x, MHH2, GaugeXiZ*MZ2)*DBLE(x**INT(2.D0))))/(CW2*MZ2*PI2*SW2)

 amplitudes(26) = (0.015625D0*EL2*(-1.D0*A0(MW2) + A0(GaugeXiW*MW2) - 2.D0*MW2*B0(x, MW2, GaugeXiW*MW2) - 2.D0*GaugeXiW*MW2*B0(x,&
  & MW2, GaugeXiW*MW2) + 2.D0*x*B0(x, MW2, GaugeXiW*MW2) + 2.D0*GaugeXiW*MW2*B0(x, GaugeXiW*MW2, GaugeXiW*MW2) - 2.D0*x*B0(x, Gau&
  &geXiW*MW2, GaugeXiW*MW2) - 2.D0*MW2*x*DB0(x, MW2, GaugeXiW*MW2) - 2.D0*GaugeXiW*MW2*x*DB0(x, MW2, GaugeXiW*MW2) + 2.D0*GaugeXi&
  &W*MW2*x*DB0(x, GaugeXiW*MW2, GaugeXiW*MW2) + DB0(x, MW2, GaugeXiW*MW2)*DBLE(MW**INT(4.D0)) - 2.D0*GaugeXiW*DB0(x, MW2, GaugeXi&
  &W*MW2)*DBLE(MW**INT(4.D0)) + DB0(x, MW2, GaugeXiW*MW2)*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0)) - 1.D0*DB0(x, GaugeXiW*MW&
  &2, GaugeXiW*MW2)*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0)) + DB0(x, MW2, GaugeXiW*MW2)*DBLE(x**INT(2.D0)) - 1.D0*DB0(x, Ga&
  &ugeXiW*MW2, GaugeXiW*MW2)*DBLE(x**INT(2.D0))))/(MW2*PI2*SW2)

 amplitudes(27) = (0.015625D0*EL2*(-1.D0*A0(MW2) + A0(GaugeXiW*MW2) - 2.D0*MW2*B0(x, MW2, GaugeXiW*MW2) - 2.D0*GaugeXiW*MW2*B0(x,&
  & MW2, GaugeXiW*MW2) + 2.D0*x*B0(x, MW2, GaugeXiW*MW2) + 2.D0*GaugeXiW*MW2*B0(x, GaugeXiW*MW2, GaugeXiW*MW2) - 2.D0*x*B0(x, Gau&
  &geXiW*MW2, GaugeXiW*MW2) - 2.D0*MW2*x*DB0(x, MW2, GaugeXiW*MW2) - 2.D0*GaugeXiW*MW2*x*DB0(x, MW2, GaugeXiW*MW2) + 2.D0*GaugeXi&
  &W*MW2*x*DB0(x, GaugeXiW*MW2, GaugeXiW*MW2) + DB0(x, MW2, GaugeXiW*MW2)*DBLE(MW**INT(4.D0)) - 2.D0*GaugeXiW*DB0(x, MW2, GaugeXi&
  &W*MW2)*DBLE(MW**INT(4.D0)) + DB0(x, MW2, GaugeXiW*MW2)*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0)) - 1.D0*DB0(x, GaugeXiW*MW&
  &2, GaugeXiW*MW2)*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0)) + DB0(x, MW2, GaugeXiW*MW2)*DBLE(x**INT(2.D0)) - 1.D0*DB0(x, Ga&
  &ugeXiW*MW2, GaugeXiW*MW2)*DBLE(x**INT(2.D0))))/(MW2*PI2*SW2)

  totalAmplitude = (0D0,0D0)
 do j=1,27
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 DSelfG0G0 = totalAmplitude
end function DSelfG0G0

