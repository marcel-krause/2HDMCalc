double complex function DSelfh0h0(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(38)

 amplitudes(1) = 0.D0

 amplitudes(2) = 0.D0

 amplitudes(3) = 0.D0

 amplitudes(4) = 0.D0

 amplitudes(5) = 0.D0

 amplitudes(6) = 0.D0

 amplitudes(7) = 0.D0

 amplitudes(8) = 0.D0

 amplitudes(9) = (-0.03125D0*EL2*ME2*(-1.D0*B0(x, ME2, ME2) + (4.D0*ME2 - 1.D0*x)*DB0(x, ME2, ME2))*DBLE(Yuk4**INT(2.D0)))/(MW2*P&
  &I2*SW2)

 amplitudes(10) = (-0.03125D0*EL2*MM2*(-1.D0*B0(x, MM2, MM2) + (4.D0*MM2 - 1.D0*x)*DB0(x, MM2, MM2))*DBLE(Yuk4**INT(2.D0)))/(MW2*&
  &PI2*SW2)

 amplitudes(11) = (-0.03125D0*EL2*ML2*(-1.D0*B0(x, ML2, ML2) + (4.D0*ML2 - 1.D0*x)*DB0(x, ML2, ML2))*DBLE(Yuk4**INT(2.D0)))/(MW2*&
  &PI2*SW2)

 amplitudes(12) = (-0.09375D0*CA2*EL2*MU2*(-1.D0*B0(x, MU2, MU2) + (4.D0*MU2 - 1.D0*x)*DB0(x, MU2, MU2)))/(MW2*PI2*SB2*SW2)

 amplitudes(13) = (-0.09375D0*CA2*EL2*MC2*(-1.D0*B0(x, MC2, MC2) + (4.D0*MC2 - 1.D0*x)*DB0(x, MC2, MC2)))/(MW2*PI2*SB2*SW2)

 amplitudes(14) = (-0.09375D0*CA2*EL2*MT2*(-1.D0*B0(x, MT2, MT2) + (4.D0*MT2 - 1.D0*x)*DB0(x, MT2, MT2)))/(MW2*PI2*SB2*SW2)

 amplitudes(15) = (-0.09375D0*EL2*MD2*(-1.D0*B0(x, MD2, MD2) + (4.D0*MD2 - 1.D0*x)*DB0(x, MD2, MD2))*DBLE(Yuk1**INT(2.D0)))/(MW2*&
  &PI2*SW2)

 amplitudes(16) = (-0.09375D0*EL2*MS2*(-1.D0*B0(x, MS2, MS2) + (4.D0*MS2 - 1.D0*x)*DB0(x, MS2, MS2))*DBLE(Yuk1**INT(2.D0)))/(MW2*&
  &PI2*SW2)

 amplitudes(17) = (-0.09375D0*EL2*MB2*(-1.D0*B0(x, MB2, MB2) + (4.D0*MB2 - 1.D0*x)*DB0(x, MB2, MB2))*DBLE(Yuk1**INT(2.D0)))/(MW2*&
  &PI2*SW2)

 amplitudes(18) = (0.0703125D0*DB0(x, Mh02, Mh02)*DBLE((2.D0*CAB*EL2*Mh02 + EL2*Mh02*S2A*SBA - 4.D0*CAB*CBA2*Lambda5*MW2*SW2)**IN&
  &T(2.D0)))/(EL2*MW2*PI2*S2B2*SW2)

 amplitudes(19) = (0.0078125D0*SBA2*DB0(x, MHH2, MHH2)*DBLE((EL2*(Mh02 + 2.D0*MHH2)*S2A - 2.D0*Lambda5*MW2*(3.D0*S2A + S2B)*SW2)*&
  &*INT(2.D0)))/(EL2*MW2*PI2*S2B2*SW2)

 amplitudes(20) = (0.015625D0*CBA2*DB0(x, Mh02, MHH2)*DBLE((EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)*&
  &*INT(2.D0)))/(EL2*MW2*PI2*S2B2*SW2)

 amplitudes(21) = (0.0078125D0*DB0(x, MA02, MA02)*DBLE((EL2*(-2.D0*MA02 + Mh02)*S2B*SBA + CAB*(-2.D0*EL2*Mh02 + 4.D0*Lambda5*MW2*&
  &SW2))**INT(2.D0)))/ (EL2*MW2*PI2*S2B2*SW2)

 amplitudes(22) = (0.0078125D0*EL2*SBA2*DB0(x, GaugeXiZ*MZ2, GaugeXiZ*MZ2)*DBLE(Mh0**INT(4.D0)))/(MW2*PI2*SW2)

 amplitudes(23) = (0.015625D0*CBA2*EL2*DB0(x, MA02, GaugeXiZ*MZ2)*DBLE((MA02 - 1.D0*Mh02)**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(24) = (0.015625D0*DB0(x, MHp2, MHp2)*DBLE((EL2*(Mh02 - 2.D0*MHp2)*S2B*SBA + CAB*(-2.D0*EL2*Mh02 + 4.D0*Lambda5*MW2*SW&
  &2))**INT(2.D0)))/ (EL2*MW2*PI2*S2B2*SW2)

 amplitudes(25) = (0.015625D0*EL2*SBA2*DB0(x, GaugeXiW*MW2, GaugeXiW*MW2)*DBLE(Mh0**INT(4.D0)))/(MW2*PI2*SW2)

 amplitudes(26) = (0.015625D0*CBA2*EL2*DB0(x, MHp2, GaugeXiW*MW2)*DBLE((Mh02 - 1.D0*MHp2)**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(27) = (0.015625D0*CBA2*EL2*DB0(x, MHp2, GaugeXiW*MW2)*DBLE((Mh02 - 1.D0*MHp2)**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(28) = (-0.015625D0*EL2*MW2*SBA2*DB0(x, GaugeXiZ*MZ2, GaugeXiZ*MZ2)*DBLE(CW**INT(-4.D0))*DBLE(GaugeXiZ**INT(2.D0)))/(P&
  &I2*SW2)

 amplitudes(29) = (-0.015625D0*EL2*MW2*SBA2*DB0(x, GaugeXiW*MW2, GaugeXiW*MW2)*DBLE(GaugeXiW**INT(2.D0)))/(PI2*SW2)

 amplitudes(30) = (-0.015625D0*EL2*MW2*SBA2*DB0(x, GaugeXiW*MW2, GaugeXiW*MW2)*DBLE(GaugeXiW**INT(2.D0)))/(PI2*SW2)

 amplitudes(31) = (0.0078125D0*EL2*MW2*SBA2*DBLE(CW**INT(-4.D0))*DBLE(MZ**INT(-4.D0))*(-4.D0*MZ2*B0(x, MZ2, MZ2) + 2.D0*x*B0(x, M&
  &Z2, MZ2) + 4.D0*MZ2*B0(x, MZ2, GaugeXiZ*MZ2) + 4.D0*GaugeXiZ*MZ2*B0(x, MZ2, GaugeXiZ*MZ2) - 4.D0*x*B0(x, MZ2, GaugeXiZ*MZ2) - &
  &4.D0*GaugeXiZ*MZ2*B0(x, GaugeXiZ*MZ2, GaugeXiZ*MZ2) + 2.D0*x*B0(x, GaugeXiZ*MZ2, GaugeXiZ*MZ2) - 4.D0*MZ2*x*DB0(x, MZ2, MZ2) +&
  & 4.D0*MZ2*x*DB0(x, MZ2, GaugeXiZ*MZ2) + 4.D0*GaugeXiZ*MZ2*x*DB0(x, MZ2, GaugeXiZ*MZ2) - 4.D0*GaugeXiZ*MZ2*x*DB0(x, GaugeXiZ*MZ&
  &2, GaugeXiZ*MZ2) + 12.D0*DB0(x, MZ2, MZ2)*DBLE(MZ**INT(4.D0)) - 2.D0*DB0(x, MZ2, GaugeXiZ*MZ2)*DBLE(MZ**INT(4.D0)) + 4.D0*Gaug&
  &eXiZ*DB0(x, MZ2, GaugeXiZ*MZ2)*DBLE(MZ**INT(4.D0)) - 2.D0*DB0(x, MZ2, GaugeXiZ*MZ2)*DBLE(GaugeXiZ**INT(2.D0))*DBLE(MZ**INT(4.D&
  &0)) + 4.D0*DB0(x, GaugeXiZ*MZ2, GaugeXiZ*MZ2)* DBLE(GaugeXiZ**INT(2.D0))*DBLE(MZ**INT(4.D0)) + DB0(x, MZ2, MZ2)*DBLE(x**INT(2.&
  &D0)) - 2.D0*DB0(x, MZ2, GaugeXiZ*MZ2)*DBLE(x**INT(2.D0)) + DB0(x, GaugeXiZ*MZ2, GaugeXiZ*MZ2)*DBLE(x**INT(2.D0))))/(PI2*SW2)

 amplitudes(32) = (0.015625D0*EL2*SBA2*(-4.D0*MW2*B0(x, MW2, MW2) + 2.D0*x*B0(x, MW2, MW2) + 4.D0*MW2*B0(x, MW2, GaugeXiW*MW2) + &
  &4.D0*GaugeXiW*MW2*B0(x, MW2, GaugeXiW*MW2) - 4.D0*x*B0(x, MW2, GaugeXiW*MW2) - 4.D0*GaugeXiW*MW2*B0(x, GaugeXiW*MW2, GaugeXiW*&
  &MW2) + 2.D0*x*B0(x, GaugeXiW*MW2, GaugeXiW*MW2) - 4.D0*MW2*x*DB0(x, MW2, MW2) + 4.D0*MW2*x*DB0(x, MW2, GaugeXiW*MW2) + 4.D0*Ga&
  &ugeXiW*MW2*x*DB0(x, MW2, GaugeXiW*MW2) - 4.D0*GaugeXiW*MW2*x*DB0(x, GaugeXiW*MW2, GaugeXiW*MW2) + 12.D0*DB0(x, MW2, MW2)*DBLE(&
  &MW**INT(4.D0)) - 2.D0*DB0(x, MW2, GaugeXiW*MW2)*DBLE(MW**INT(4.D0)) + 4.D0*GaugeXiW*DB0(x, MW2, GaugeXiW*MW2)*DBLE(MW**INT(4.D&
  &0)) - 2.D0*DB0(x, MW2, GaugeXiW*MW2)*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0)) + 4.D0*DB0(x, GaugeXiW*MW2, GaugeXiW*MW2)*D&
  &BLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0)) + DB0(x, MW2, MW2)*DBLE(x**INT(2.D0)) - 2.D0*DB0(x, MW2, GaugeXiW*MW2)*DBLE(x**IN&
  &T(2.D0)) + DB0(x, GaugeXiW*MW2, GaugeXiW*MW2)*DBLE(x**INT(2.D0))))/(MW2*PI2*SW2)

 amplitudes(33) = (0.015625D0*CBA2*EL2*(-1.D0*A0(MZ2) + A0(GaugeXiZ*MZ2) - 2.D0*MA02*B0(x, MA02, MZ2) - 2.D0*MZ2*B0(x, MA02, MZ2)&
  & + 2.D0*x*B0(x, MA02, MZ2) + 2.D0*MA02*B0(x, MA02, GaugeXiZ*MZ2) - 2.D0*x*B0(x, MA02, GaugeXiZ*MZ2) - 2.D0*MA02*MZ2*DB0(x, MA0&
  &2, MZ2) - 2.D0*MA02*x*DB0(x, MA02, MZ2) - 2.D0*MZ2*x*DB0(x, MA02, MZ2) + 2.D0*MA02*x*DB0(x, MA02, GaugeXiZ*MZ2) + DB0(x, MA02,&
  & MZ2)*DBLE(MA0**INT(4.D0)) - 1.D0*DB0(x, MA02, GaugeXiZ*MZ2)*DBLE(MA0**INT(4.D0)) + DB0(x, MA02, MZ2)*DBLE(MZ**INT(4.D0)) + DB&
  &0(x, MA02, MZ2)*DBLE(x**INT(2.D0)) - 1.D0*DB0(x, MA02, GaugeXiZ*MZ2)*DBLE(x**INT(2.D0))))/(CW2*MZ2*PI2*SW2)

 amplitudes(34) = (0.015625D0*EL2*SBA2*(-1.D0*A0(MZ2) + A0(GaugeXiZ*MZ2) - 2.D0*MZ2*B0(x, MZ2, GaugeXiZ*MZ2) - 2.D0*GaugeXiZ*MZ2*&
  &B0(x, MZ2, GaugeXiZ*MZ2) + 2.D0*x*B0(x, MZ2, GaugeXiZ*MZ2) + 2.D0*GaugeXiZ*MZ2*B0(x, GaugeXiZ*MZ2, GaugeXiZ*MZ2) - 2.D0*x*B0(x&
  &, GaugeXiZ*MZ2, GaugeXiZ*MZ2) - 2.D0*MZ2*x*DB0(x, MZ2, GaugeXiZ*MZ2) - 2.D0*GaugeXiZ*MZ2*x*DB0(x, MZ2, GaugeXiZ*MZ2) + 2.D0*Ga&
  &ugeXiZ*MZ2*x*DB0(x, GaugeXiZ*MZ2, GaugeXiZ*MZ2) + DB0(x, MZ2, GaugeXiZ*MZ2)*DBLE(MZ**INT(4.D0)) - 2.D0*GaugeXiZ*DB0(x, MZ2, Ga&
  &ugeXiZ*MZ2)*DBLE(MZ**INT(4.D0)) + DB0(x, MZ2, GaugeXiZ*MZ2)*DBLE(GaugeXiZ**INT(2.D0))*DBLE(MZ**INT(4.D0)) - 1.D0*DB0(x, GaugeX&
  &iZ*MZ2, GaugeXiZ*MZ2)*DBLE(GaugeXiZ**INT(2.D0))*DBLE(MZ**INT(4.D0)) + DB0(x, MZ2, GaugeXiZ*MZ2)*DBLE(x**INT(2.D0)) - 1.D0*DB0(&
  &x, GaugeXiZ*MZ2, GaugeXiZ*MZ2)*DBLE(x**INT(2.D0))))/(CW2*MZ2*PI2*SW2)

 amplitudes(35) = (0.015625D0*CBA2*EL2*(-1.D0*A0(MW2) + A0(GaugeXiW*MW2) - 2.D0*MHp2*B0(x, MHp2, MW2) - 2.D0*MW2*B0(x, MHp2, MW2)&
  & + 2.D0*x*B0(x, MHp2, MW2) + 2.D0*MHp2*B0(x, MHp2, GaugeXiW*MW2) - 2.D0*x*B0(x, MHp2, GaugeXiW*MW2) - 2.D0*MHp2*MW2*DB0(x, MHp&
  &2, MW2) - 2.D0*MHp2*x*DB0(x, MHp2, MW2) - 2.D0*MW2*x*DB0(x, MHp2, MW2) + 2.D0*MHp2*x*DB0(x, MHp2, GaugeXiW*MW2) + DB0(x, MHp2,&
  & MW2)*DBLE(MHp**INT(4.D0)) - 1.D0*DB0(x, MHp2, GaugeXiW*MW2)*DBLE(MHp**INT(4.D0)) + DB0(x, MHp2, MW2)*DBLE(MW**INT(4.D0)) + DB&
  &0(x, MHp2, MW2)*DBLE(x**INT(2.D0)) - 1.D0*DB0(x, MHp2, GaugeXiW*MW2)*DBLE(x**INT(2.D0))))/(MW2*PI2*SW2)

 amplitudes(36) = (0.015625D0*CBA2*EL2*(-1.D0*A0(MW2) + A0(GaugeXiW*MW2) - 2.D0*MHp2*B0(x, MHp2, MW2) - 2.D0*MW2*B0(x, MHp2, MW2)&
  & + 2.D0*x*B0(x, MHp2, MW2) + 2.D0*MHp2*B0(x, MHp2, GaugeXiW*MW2) - 2.D0*x*B0(x, MHp2, GaugeXiW*MW2) - 2.D0*MHp2*MW2*DB0(x, MHp&
  &2, MW2) - 2.D0*MHp2*x*DB0(x, MHp2, MW2) - 2.D0*MW2*x*DB0(x, MHp2, MW2) + 2.D0*MHp2*x*DB0(x, MHp2, GaugeXiW*MW2) + DB0(x, MHp2,&
  & MW2)*DBLE(MHp**INT(4.D0)) - 1.D0*DB0(x, MHp2, GaugeXiW*MW2)*DBLE(MHp**INT(4.D0)) + DB0(x, MHp2, MW2)*DBLE(MW**INT(4.D0)) + DB&
  &0(x, MHp2, MW2)*DBLE(x**INT(2.D0)) - 1.D0*DB0(x, MHp2, GaugeXiW*MW2)*DBLE(x**INT(2.D0))))/(MW2*PI2*SW2)

 amplitudes(37) = (0.015625D0*EL2*SBA2*(-1.D0*A0(MW2) + A0(GaugeXiW*MW2) - 2.D0*MW2*B0(x, MW2, GaugeXiW*MW2) - 2.D0*GaugeXiW*MW2*&
  &B0(x, MW2, GaugeXiW*MW2) + 2.D0*x*B0(x, MW2, GaugeXiW*MW2) + 2.D0*GaugeXiW*MW2*B0(x, GaugeXiW*MW2, GaugeXiW*MW2) - 2.D0*x*B0(x&
  &, GaugeXiW*MW2, GaugeXiW*MW2) - 2.D0*MW2*x*DB0(x, MW2, GaugeXiW*MW2) - 2.D0*GaugeXiW*MW2*x*DB0(x, MW2, GaugeXiW*MW2) + 2.D0*Ga&
  &ugeXiW*MW2*x*DB0(x, GaugeXiW*MW2, GaugeXiW*MW2) + DB0(x, MW2, GaugeXiW*MW2)*DBLE(MW**INT(4.D0)) - 2.D0*GaugeXiW*DB0(x, MW2, Ga&
  &ugeXiW*MW2)*DBLE(MW**INT(4.D0)) + DB0(x, MW2, GaugeXiW*MW2)*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0)) - 1.D0*DB0(x, GaugeX&
  &iW*MW2, GaugeXiW*MW2)*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0)) + DB0(x, MW2, GaugeXiW*MW2)*DBLE(x**INT(2.D0)) - 1.D0*DB0(&
  &x, GaugeXiW*MW2, GaugeXiW*MW2)*DBLE(x**INT(2.D0))))/(MW2*PI2*SW2)

 amplitudes(38) = (0.015625D0*EL2*SBA2*(-1.D0*A0(MW2) + A0(GaugeXiW*MW2) - 2.D0*MW2*B0(x, MW2, GaugeXiW*MW2) - 2.D0*GaugeXiW*MW2*&
  &B0(x, MW2, GaugeXiW*MW2) + 2.D0*x*B0(x, MW2, GaugeXiW*MW2) + 2.D0*GaugeXiW*MW2*B0(x, GaugeXiW*MW2, GaugeXiW*MW2) - 2.D0*x*B0(x&
  &, GaugeXiW*MW2, GaugeXiW*MW2) - 2.D0*MW2*x*DB0(x, MW2, GaugeXiW*MW2) - 2.D0*GaugeXiW*MW2*x*DB0(x, MW2, GaugeXiW*MW2) + 2.D0*Ga&
  &ugeXiW*MW2*x*DB0(x, GaugeXiW*MW2, GaugeXiW*MW2) + DB0(x, MW2, GaugeXiW*MW2)*DBLE(MW**INT(4.D0)) - 2.D0*GaugeXiW*DB0(x, MW2, Ga&
  &ugeXiW*MW2)*DBLE(MW**INT(4.D0)) + DB0(x, MW2, GaugeXiW*MW2)*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0)) - 1.D0*DB0(x, GaugeX&
  &iW*MW2, GaugeXiW*MW2)*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0)) + DB0(x, MW2, GaugeXiW*MW2)*DBLE(x**INT(2.D0)) - 1.D0*DB0(&
  &x, GaugeXiW*MW2, GaugeXiW*MW2)*DBLE(x**INT(2.D0))))/(MW2*PI2*SW2)

  totalAmplitude = (0D0,0D0)
 do j=1,38
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 DSelfh0h0 = totalAmplitude
end function DSelfh0h0

