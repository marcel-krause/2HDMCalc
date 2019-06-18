double complex function h0toTauPTauMProcDepVC()
 use constants
 implicit none
#include "looptools.h"
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(27)

 amplitudes(1) = (0.015625D0*EL2*ML2*(-1.D0*(Mh02 + 4.D0*ML2)*B0(Mh02, ML2, ML2) + 8.D0*ML2*B0(ML2, Mh02, ML2) - 1.D0*C0(Mh02, ML&
  &2, ML2, ML2, ML2, Mh02)*(8.D0*Mh02*ML2 + DBLE(Mh0**INT(4.D0)) - 16.D0*DBLE(ML**INT(4.D0))))*DBLE(Yuk4**INT(2.D0)))/ ((Mh02 - 4&
  &.D0*ML2)*MW2*PI2*SW2)

 amplitudes(2) = (-0.015625D0*EL2*ML2*((Mh02 + 4.D0*ML2)*B0(Mh02, ML2, ML2) - 8.D0*ML2*B0(ML2, MHH2, ML2) + (4.D0*(MHH2 - 4.D0*ML&
  &2)*ML2 + Mh02*(MHH2 + 4.D0*ML2))* C0(Mh02, ML2, ML2, ML2, ML2, MHH2))*DBLE(Yuk5**INT(2.D0)))/((Mh02 - 4.D0*ML2)*MW2*PI2*SW2)

 amplitudes(3) = (0.015625D0*EL2*ML2*(B0(Mh02, ML2, ML2) + MA02*C0(Mh02, ML2, ML2, ML2, ML2, MA02))*DBLE(Yuk6**INT(2.D0)))/(MW2*P&
  &I2*SW2)

 amplitudes(4) = (0.015625D0*EL2*ML2*(B0(Mh02, ML2, ML2) + GaugeXiZ*MZ2*C0(Mh02, ML2, ML2, ML2, ML2, GaugeXiZ*MZ2)))/(MW2*PI2*SW2&
  &)

 amplitudes(5) = (0.046875D0*ML2*(2.D0*CAB*EL2*Mh02 + EL2*Mh02*S2A*SBA - 4.D0*CAB*CBA2*Lambda5*MW2*SW2)*Yuk4* (2.D0*B0(Mh02, Mh02&
  &, Mh02) - 2.D0*B0(ML2, Mh02, ML2) + (-3.D0*Mh02 + 8.D0*ML2)*C0(Mh02, ML2, ML2, Mh02, Mh02, ML2)))/ ((Mh02 - 4.D0*ML2)*MW2*PI2*&
  &S2B*SW2)

 amplitudes(6) = (-0.015625D0*CBA*ML2*(EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)*Yuk5* (-2.D0*B0(Mh02,&
  & Mh02, MHH2) + B0(ML2, Mh02, ML2) + B0(ML2, MHH2, ML2) + 2.D0*Mh02*C0(Mh02, ML2, ML2, Mh02, MHH2, ML2) + MHH2*C0(Mh02, ML2, ML&
  &2, Mh02, MHH2, ML2) - 8.D0*ML2*C0(Mh02, ML2, ML2, Mh02, MHH2, ML2)))/((Mh02 - 4.D0*ML2)*MW2*PI2*S2B*SW2)

 amplitudes(7) = (-0.015625D0*CBA*ML2*(EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)*Yuk5* (-2.D0*B0(Mh02,&
  & Mh02, MHH2) + B0(ML2, Mh02, ML2) + B0(ML2, MHH2, ML2) + 2.D0*Mh02*C0(Mh02, ML2, ML2, Mh02, MHH2, ML2) + MHH2*C0(Mh02, ML2, ML&
  &2, Mh02, MHH2, ML2) - 8.D0*ML2*C0(Mh02, ML2, ML2, Mh02, MHH2, ML2)))/((Mh02 - 4.D0*ML2)*MW2*PI2*S2B*SW2)

 amplitudes(8) = (0.015625D0*ML2*SBA*(EL2*(Mh02 + 2.D0*MHH2)*S2A - 2.D0*Lambda5*MW2*(3.D0*S2A + S2B)*SW2)*(-2.D0*B0(Mh02, MHH2, M&
  &HH2) + 2.D0*B0(ML2, MHH2, ML2) + (Mh02 + 2.D0*MHH2 - 8.D0*ML2)*C0(Mh02, ML2, ML2, MHH2, MHH2, ML2))*DBLE(Yuk5**INT(2.D0)))/((M&
  &h02 - 4.D0*ML2)*MW2*PI2*S2B*SW2*Yuk4)

 amplitudes(9) = (0.015625D0*ML2*(EL2*(2.D0*MA02 - 1.D0*Mh02)*S2B*SBA + 2.D0*CAB*(EL2*Mh02 - 2.D0*Lambda5*MW2*SW2))* (2.D0*B0(Mh0&
  &2, MA02, MA02) - 2.D0*B0(ML2, MA02, ML2) + (-2.D0*MA02 + Mh02)*C0(Mh02, ML2, ML2, MA02, MA02, ML2))*DBLE(Yuk6**INT(2.D0)))/ ((&
  &Mh02 - 4.D0*ML2)*MW2*PI2*S2B*SW2*Yuk4)

 amplitudes(10) = (-0.015625D0*CBA*EL2*(MA02 - 1.D0*Mh02)*ML2*Yuk6*(-2.D0*B0(Mh02, MA02, GaugeXiZ*MZ2) + B0(ML2, MA02, ML2) + B0(&
  &ML2, ML2, GaugeXiZ*MZ2) + MA02*C0(Mh02, ML2, ML2, MA02, GaugeXiZ*MZ2, ML2) - 1.D0*Mh02*C0(Mh02, ML2, ML2, MA02, GaugeXiZ*MZ2, &
  &ML2) + GaugeXiZ*MZ2*C0(Mh02, ML2, ML2, MA02, GaugeXiZ*MZ2, ML2)))/((Mh02 - 4.D0*ML2)*MW2*PI2*SW2*Yuk4)

 amplitudes(11) = (-0.015625D0*CBA*EL2*(MA02 - 1.D0*Mh02)*ML2*Yuk6*(-2.D0*B0(Mh02, MA02, GaugeXiZ*MZ2) + B0(ML2, MA02, ML2) + B0(&
  &ML2, ML2, GaugeXiZ*MZ2) + MA02*C0(Mh02, ML2, ML2, MA02, GaugeXiZ*MZ2, ML2) - 1.D0*Mh02*C0(Mh02, ML2, ML2, MA02, GaugeXiZ*MZ2, &
  &ML2) + GaugeXiZ*MZ2*C0(Mh02, ML2, ML2, MA02, GaugeXiZ*MZ2, ML2)))/((Mh02 - 4.D0*ML2)*MW2*PI2*SW2*Yuk4)

 amplitudes(12) = (0.015625D0*EL2*Mh02*ML2*SBA*(2.D0*B0(Mh02, GaugeXiZ*MZ2, GaugeXiZ*MZ2) - 2.D0*B0(ML2, ML2, GaugeXiZ*MZ2) + (Mh&
  &02 - 2.D0*GaugeXiZ*MZ2)*C0(Mh02, ML2, ML2, GaugeXiZ*MZ2, GaugeXiZ*MZ2, ML2)))/((Mh02 - 4.D0*ML2)*MW2*PI2*SW2*Yuk4)

 amplitudes(13) = (0.03125D0*ML2*(-1.D0*EL2*(Mh02 - 2.D0*MHp2)*S2B*SBA + 2.D0*CAB*(EL2*Mh02 - 2.D0*Lambda5*MW2*SW2))* (B0(Mh02, M&
  &Hp2, MHp2) - 1.D0*B0(ML2, 0.D0, MHp2) + (-1.D0*MHp2 + ML2)*C0(Mh02, ML2, ML2, MHp2, MHp2, 0.D0))*DBLE(Yuk6**INT(2.D0)))/ ((Mh0&
  &2 - 4.D0*ML2)*MW2*PI2*S2B*SW2*Yuk4)

 amplitudes(14) = (0.03125D0*CBA*EL2*(Mh02 - 1.D0*MHp2)*ML2*Yuk6*(-1.D0*Mh02*B0(Mh02, MHp2, GaugeXiW*MW2) + 2.D0*ML2*B0(ML2, 0.D0&
  &, MHp2) + Mh02*B0(ML2, 0.D0, GaugeXiW*MW2) - 2.D0*ML2*B0(ML2, 0.D0, GaugeXiW*MW2) + Mh02*MHp2*C0(Mh02, ML2, ML2, MHp2, GaugeXi&
  &W*MW2, 0.D0) - 1.D0*Mh02*ML2*C0(Mh02, ML2, ML2, MHp2, GaugeXiW*MW2, 0.D0) - 2.D0*MHp2*ML2*C0(Mh02, ML2, ML2, MHp2, GaugeXiW*MW&
  &2, 0.D0) + 2.D0*GaugeXiW*ML2*MW2*C0(Mh02, ML2, ML2, MHp2, GaugeXiW*MW2, 0.D0)))/(Mh02*(Mh02 - 4.D0*ML2)*MW2*PI2*SW2*Yuk4)

 amplitudes(15) = (-0.03125D0*CBA*EL2*(Mh02 - 1.D0*MHp2)*ML2*Yuk6*(Mh02*B0(Mh02, MHp2, GaugeXiW*MW2) - 1.D0*(Mh02 - 2.D0*ML2)*B0(&
  &ML2, 0.D0, MHp2) - 2.D0*ML2*B0(ML2, 0.D0, GaugeXiW*MW2) + Mh02*ML2*C0(Mh02, ML2, ML2, MHp2, GaugeXiW*MW2, 0.D0) - 2.D0*MHp2*ML&
  &2*C0(Mh02, ML2, ML2, MHp2, GaugeXiW*MW2, 0.D0) - 1.D0*GaugeXiW*Mh02*MW2*C0(Mh02, ML2, ML2, MHp2, GaugeXiW*MW2, 0.D0) + 2.D0*Ga&
  &ugeXiW*ML2*MW2*C0(Mh02, ML2, ML2, MHp2, GaugeXiW*MW2, 0.D0)))/(Mh02*(Mh02 - 4.D0*ML2)*MW2*PI2*SW2*Yuk4)

 amplitudes(16) = (0.03125D0*EL2*Mh02*ML2*SBA*(B0(Mh02, GaugeXiW*MW2, GaugeXiW*MW2) - 1.D0*B0(ML2, 0.D0, GaugeXiW*MW2) + (ML2 - 1&
  &.D0*GaugeXiW*MW2)*C0(Mh02, ML2, ML2, GaugeXiW*MW2, GaugeXiW*MW2, 0.D0)))/((Mh02 - 4.D0*ML2)*MW2*PI2*SW2*Yuk4)

 amplitudes(17) = (0.0078125D0*EL2*(8.D0*(1.D0 - 2.D0*SW2)*SW2 - 1.D0*GaugeXiZ*B0(ML2, ML2, GaugeXiZ*MZ2) - 2.D0*GaugeXiZ*ML2*C0(&
  &Mh02, ML2, ML2, ML2, ML2, GaugeXiZ*MZ2) - (1.D0*A0(MZ2)*(1.D0 - 4.D0*SW2 + 8.D0*DBLE(SW**INT(4.D0))))/MZ2 + (A0(GaugeXiZ*MZ2)*&
  &(1.D0 - 4.D0*SW2 + 8.D0*DBLE(SW**INT(4.D0))))/MZ2 - (8.D0*ML2*B0(Mh02, ML2, ML2)*(1.D0 - 4.D0*SW2 + 8.D0*DBLE(SW**INT(4.D0))))&
  &/ (Mh02 - 4.D0*ML2) + (B0(ML2, ML2, MZ2)*(4.D0*ML2*(1.D0 + 8.D0*SW2 - 16.D0*DBLE(SW**INT(4.D0))) + Mh02*(1.D0 - 16.D0*SW2 + 32&
  &.D0*DBLE(SW**INT(4.D0)))))/ (Mh02 - 4.D0*ML2) - (2.D0*C0(Mh02, ML2, ML2, ML2, ML2, MZ2)*(Mh02*ML2*(1.D0 + 24.D0*SW2 - 48.D0*DB&
  &LE(SW**INT(4.D0))) + DBLE(Mh0**INT(4.D0))*(-4.D0*SW2 + 8.D0*DBLE(SW**INT(4.D0))) + 4.D0*ML2*(MZ2*(1.D0 - 4.D0*SW2 + 8.D0*DBLE(&
  &SW**INT(4.D0))) + ML2*(-1.D0 - 8.D0*SW2 + 16.D0*DBLE(SW**INT(4.D0))))))/(Mh02 - 4.D0*ML2)))/(CW2*PI2*SW2)

 amplitudes(18) = (0.0078125D0*CBA*EL2*Yuk6*(((1.D0 - 4.D0*SW2)*A0(MZ2))/MZ2 + ((-1.D0 + 4.D0*SW2)*A0(GaugeXiZ*MZ2))/MZ2 + (Mh02*&
  &(MA02 - 1.D0*Mh02 + MZ2)*B0(Mh02, MA02, MZ2))/((Mh02 - 4.D0*ML2)*MZ2) + ((MA02 - 1.D0*Mh02 + MZ2)*(1.D0 - 4.D0*SW2)*B0(Mh02, M&
  &A02, MZ2))/ MZ2 + (Mh02*(-1.D0*MA02 + Mh02)*B0(Mh02, MA02, GaugeXiZ*MZ2))/((Mh02 - 4.D0*ML2)*MZ2) + ((MA02 - 1.D0*Mh02)*(-1.D0&
  & + 4.D0*SW2)*B0(Mh02, MA02, GaugeXiZ*MZ2))/MZ2 - (2.D0*ML2*B0(ML2, MA02, ML2))/(Mh02 - 4.D0*ML2) + ((-2.D0*MA02*ML2 + Mh02*(2.&
  &D0*ML2 - 1.D0*MZ2) + 2.D0*ML2*MZ2)*B0(ML2, ML2, MZ2))/((Mh02 - 4.D0*ML2)*MZ2) + 2.D0*(-1.D0 + 4.D0*SW2)*B0(ML2, ML2, MZ2) + ((&
  &2.D0*(MA02 - 1.D0*Mh02)*ML2 - 1.D0*GaugeXiZ*(Mh02 - 4.D0*ML2)*MZ2)*B0(ML2, ML2, GaugeXiZ*MZ2))/ ((Mh02 - 4.D0*ML2)*MZ2) + 2.D0&
  &*MA02*(-1.D0 + 4.D0*SW2)*C0(Mh02, ML2, ML2, MA02, MZ2, ML2) + (2.D0*(MA02 - 1.D0*Mh02)*ML2*(MA02 - 1.D0*Mh02 + GaugeXiZ*MZ2)*C&
  &0(Mh02, ML2, ML2, MA02, GaugeXiZ*MZ2, ML2))/((Mh02 - 4.D0*ML2)*MZ2) - (2.D0*C0(Mh02, ML2, ML2, MA02, MZ2, ML2)*(MA02*(-2.D0*ML&
  &2*MZ2 + Mh02*(-2.D0*ML2 + MZ2)) + ML2*DBLE(MA0**INT(4.D0)) + ML2*DBLE((Mh02 - 1.D0*MZ2)**INT(2.D0))))/((Mh02 - 4.D0*ML2)*MZ2))&
  &)/(CW2*PI2*SW2*Yuk4)

 amplitudes(19) = (-0.0078125D0*EL2*SBA*(((1.D0 - 4.D0*SW2)*A0(MZ2))/MZ2 + ((-1.D0 + 4.D0*SW2)*A0(GaugeXiZ*MZ2))/MZ2 - (1.D0*Mh02&
  &*(Mh02 - 1.D0*MZ2 - 1.D0*GaugeXiZ*MZ2)*B0(Mh02, MZ2, GaugeXiZ*MZ2))/((Mh02 - 4.D0*ML2)*MZ2) - (1.D0*(-1.D0*Mh02 + MZ2 + GaugeX&
  &iZ*MZ2)*(-1.D0 + 4.D0*SW2)*B0(Mh02, MZ2, GaugeXiZ*MZ2))/MZ2 + (Mh02*(Mh02 - 1.D0*GaugeXiZ*MZ2)*B0(Mh02, GaugeXiZ*MZ2, GaugeXiZ&
  &*MZ2))/((Mh02 - 4.D0*ML2)*MZ2) + ((Mh02 - 1.D0*GaugeXiZ*MZ2)*(1.D0 - 4.D0*SW2)*B0(Mh02, GaugeXiZ*MZ2, GaugeXiZ*MZ2))/MZ2 + ((M&
  &h02*(2.D0*ML2 - 1.D0*MZ2) + 2.D0*ML2*MZ2 - 2.D0*GaugeXiZ*ML2*MZ2)*B0(ML2, ML2, MZ2))/((Mh02 - 4.D0*ML2)*MZ2) + 2.D0*(-1.D0 + 4&
  &.D0*SW2)*B0(ML2, ML2, MZ2) - (1.D0*(GaugeXiZ*(Mh02 - 6.D0*ML2)*MZ2 + 2.D0*ML2*(Mh02 + MZ2))*B0(ML2, ML2, GaugeXiZ*MZ2))/ ((Mh0&
  &2 - 4.D0*ML2)*MZ2) + 2.D0*GaugeXiZ*MZ2*(-1.D0 + 4.D0*SW2)*C0(Mh02, ML2, ML2, MZ2, GaugeXiZ*MZ2, ML2) + (2.D0*ML2*(Mh02 - 2.D0*&
  &GaugeXiZ*MZ2)*(Mh02 - 1.D0*GaugeXiZ*MZ2)*C0(Mh02, ML2, ML2, GaugeXiZ*MZ2, GaugeXiZ*MZ2, ML2))/ ((Mh02 - 4.D0*ML2)*MZ2) - (2.D0&
  &*C0(Mh02, ML2, ML2, MZ2, GaugeXiZ*MZ2, ML2)*(ML2*DBLE(GaugeXiZ**INT(2.D0))*DBLE(MZ**INT(4.D0)) + GaugeXiZ*(-2.D0*ML2*DBLE(MZ**&
  &INT(4.D0)) + Mh02*(-2.D0*ML2*MZ2 + DBLE(MZ**INT(4.D0)))) + ML2*DBLE((Mh02 - 1.D0*MZ2)**INT(2.D0))))/ ((Mh02 - 4.D0*ML2)*MZ2)))&
  &/(CW2*PI2*SW2*Yuk4)

 amplitudes(20) = (-0.015625D0*CBA*EL2*Yuk6*(6.D0*Mh02*MHp2*ML2*B0(Mh02, MHp2, MW2) + 2.D0*Mh02*ML2*MW2*B0(Mh02, MHp2, MW2) - 6.D&
  &0*Mh02*MHp2*ML2*B0(Mh02, MHp2, GaugeXiW*MW2) + 4.D0*Mh02*ML2*MW2*B0(ML2, 0.D0, MHp2) + 2.D0*Mh02*MHp2*ML2*B0(ML2, 0.D0, MW2) -&
  & 10.D0*Mh02*ML2*MW2*B0(ML2, 0.D0, MW2) - 2.D0*Mh02*MHp2*ML2*B0(ML2, 0.D0, GaugeXiW*MW2) - 4.D0*GaugeXiW*Mh02*ML2*MW2*B0(ML2, 0&
  &.D0, GaugeXiW*MW2) - 14.D0*Mh02*MHp2*ML2*MW2*C0(Mh02, ML2, ML2, MHp2, MW2, 0.D0) - 2.D0*MHp2*B0(Mh02, MHp2, MW2)*DBLE(Mh0**INT&
  &(4.D0)) - 6.D0*ML2*B0(Mh02, MHp2, MW2)*DBLE(Mh0**INT(4.D0)) - 2.D0*MW2*B0(Mh02, MHp2, MW2)*DBLE(Mh0**INT(4.D0)) + 2.D0*MHp2*B0&
  &(Mh02, MHp2, GaugeXiW*MW2)*DBLE(Mh0**INT(4.D0)) + 6.D0*ML2*B0(Mh02, MHp2, GaugeXiW*MW2)*DBLE(Mh0**INT(4.D0)) - 1.D0*ML2*B0(ML2&
  &, 0.D0, MW2)*DBLE(Mh0**INT(4.D0)) + 3.D0*MW2*B0(ML2, 0.D0, MW2)*DBLE(Mh0**INT(4.D0)) + ML2*B0(ML2, 0.D0, GaugeXiW*MW2)*DBLE(Mh&
  &0**INT(4.D0)) + GaugeXiW*MW2*B0(ML2, 0.D0, GaugeXiW*MW2)*DBLE(Mh0**INT(4.D0)) - 2.D0*MHp2*ML2*C0(Mh02, ML2, ML2, MHp2, MW2, 0.&
  &D0)*DBLE(Mh0**INT(4.D0)) + 4.D0*MHp2*MW2*C0(Mh02, ML2, ML2, MHp2, MW2, 0.D0)*DBLE(Mh0**INT(4.D0)) - 4.D0*ML2*MW2*C0(Mh02, ML2,&
  & ML2, MHp2, MW2, 0.D0)*DBLE(Mh0**INT(4.D0)) + 2.D0*MHp2*ML2*C0(Mh02, ML2, ML2, MHp2, GaugeXiW*MW2, 0.D0)* DBLE(Mh0**INT(4.D0))&
  & - 1.D0*A0(MW2)*(-4.D0*Mh02*ML2 + DBLE(Mh0**INT(4.D0))) + A0(GaugeXiW*MW2)*(-4.D0*Mh02*ML2 + DBLE(Mh0**INT(4.D0))) + 2.D0*B0(M&
  &h02, MHp2, MW2)*DBLE(Mh0**INT(6.D0)) - 2.D0*B0(Mh02, MHp2, GaugeXiW*MW2)*DBLE(Mh0**INT(6.D0)) + 2.D0*Mh02*ML2*C0(Mh02, ML2, ML&
  &2, MHp2, MW2, 0.D0)*DBLE(MHp**INT(4.D0)) - 2.D0*Mh02*ML2*C0(Mh02, ML2, ML2, MHp2, GaugeXiW*MW2, 0.D0)* DBLE(MHp**INT(4.D0)) - &
  &4.D0*MW2*B0(ML2, 0.D0, MHp2)*DBLE(ML**INT(4.D0)) - 4.D0*MHp2*B0(ML2, 0.D0, MW2)*DBLE(ML**INT(4.D0)) + 4.D0*MW2*B0(ML2, 0.D0, M&
  &W2)*DBLE(ML**INT(4.D0)) + 4.D0*MHp2*B0(ML2, 0.D0, GaugeXiW*MW2)*DBLE(ML**INT(4.D0)) + 2.D0*Mh02*MHp2*C0(Mh02, ML2, ML2, MHp2, &
  &MW2, 0.D0)*DBLE(ML**INT(4.D0)) + 6.D0*Mh02*MW2*C0(Mh02, ML2, ML2, MHp2, MW2, 0.D0)*DBLE(ML**INT(4.D0)) + 8.D0*MHp2*MW2*C0(Mh02&
  &, ML2, ML2, MHp2, MW2, 0.D0)*DBLE(ML**INT(4.D0)) - 2.D0*Mh02*MHp2*C0(Mh02, ML2, ML2, MHp2, GaugeXiW*MW2, 0.D0)* DBLE(ML**INT(4&
  &.D0)) + 4.D0*GaugeXiW*Mh02*MW2*C0(Mh02, ML2, ML2, MHp2, GaugeXiW*MW2, 0.D0)*DBLE(ML**INT(4.D0)) - 4.D0*GaugeXiW*MHp2*MW2*C0(Mh&
  &02, ML2, ML2, MHp2, GaugeXiW*MW2, 0.D0)*DBLE(ML**INT(4.D0)) + 2.D0*C0(Mh02, ML2, ML2, MHp2, MW2, 0.D0)*DBLE(Mh0**INT(4.D0))*DB&
  &LE(ML**INT(4.D0)) - 2.D0*C0(Mh02, ML2, ML2, MHp2, GaugeXiW*MW2, 0.D0)* DBLE(Mh0**INT(4.D0))*DBLE(ML**INT(4.D0)) - 4.D0*C0(Mh02&
  &, ML2, ML2, MHp2, MW2, 0.D0)*DBLE(MHp**INT(4.D0))*DBLE(ML**INT(4.D0)) + 4.D0*C0(Mh02, ML2, ML2, MHp2, GaugeXiW*MW2, 0.D0)*DBLE&
  &(MHp**INT(4.D0))*DBLE(ML**INT(4.D0)) + 4.D0*Mh02*ML2*C0(Mh02, ML2, ML2, MHp2, MW2, 0.D0)*DBLE(MW**INT(4.D0)) - 4.D0*C0(Mh02, M&
  &L2, ML2, MHp2, MW2, 0.D0)*DBLE(ML**INT(4.D0))* DBLE(MW**INT(4.D0))))/(Mh02*(Mh0 - 2.D0*ML)*(Mh0 + 2.D0*ML)*MW2*PI2*SW2*Yuk4)

 amplitudes(21) = (-0.0078125D0*EL2*SBA*(2.D0*A0(MW2) - 2.D0*A0(GaugeXiW*MW2) - 2.D0*(Mh02 - 1.D0*MW2 - 1.D0*GaugeXiW*MW2)*B0(Mh0&
  &2, MW2, GaugeXiW*MW2) + 2.D0*(Mh02 - 1.D0*GaugeXiW*MW2)*B0(Mh02, GaugeXiW*MW2, GaugeXiW*MW2) + (2.D0*(Mh02 - 2.D0*ML2)*(Mh02 -&
  & 1.D0*GaugeXiW*MW2)*B0(Mh02, GaugeXiW*MW2, GaugeXiW*MW2))/(Mh02 - 4.D0*ML2) + ((Mh02*(ML2 - 3.D0*MW2) + 2.D0*ML2*MW2 - 2.D0*Ga&
  &ugeXiW*ML2*MW2)*B0(ML2, 0.D0, MW2))/Mh02 - (1.D0*(GaugeXiW*(Mh02 - 2.D0*ML2)*MW2 + ML2*(Mh02 + 2.D0*MW2))*B0(ML2, 0.D0, GaugeX&
  &iW*MW2))/Mh02 - (1.D0*(GaugeXiW*(Mh02 - 6.D0*ML2)*MW2 + ML2*(Mh02 + 4.D0*ML2 + 6.D0*MW2))*B0(ML2, 0.D0, GaugeXiW*MW2))/(Mh02 -&
  & 4.D0*ML2) + (4.D0*ML2*(Mh02 - 1.D0*GaugeXiW*MW2)*(ML2 - 1.D0*GaugeXiW*MW2)*C0(Mh02, ML2, ML2, GaugeXiW*MW2, GaugeXiW*MW2, 0.D&
  &0))/(Mh02 - 4.D0*ML2) - (2.D0*MW2*C0(Mh02, ML2, ML2, MW2, GaugeXiW*MW2, 0.D0)*(ML2*(-1.D0*Mh02 + MW2) - 1.D0*GaugeXiW*(Mh02*(M&
  &L2 - 2.D0*MW2) + 2.D0*ML2*MW2) + ML2*MW2*DBLE(GaugeXiW**INT(2.D0))))/Mh02 - (2.D0*B0(Mh02, MW2, GaugeXiW*MW2)*(-1.D0*GaugeXiW*&
  &(Mh02 - 2.D0*ML2)*MW2 - 2.D0*ML2*MW2 - 1.D0*Mh02*(2.D0*ML2 + MW2) + DBLE(Mh0**INT(4.D0))))/(Mh02 - 4.D0*ML2) + (B0(ML2, 0.D0, &
  &MW2)*(Mh02*(ML2 - 3.D0*MW2) + 6.D0*ML2*MW2 - 2.D0*GaugeXiW*ML2*MW2 + 4.D0*DBLE(ML**INT(4.D0))))/(Mh02 - 4.D0*ML2) - (2.D0*C0(M&
  &h02, ML2, ML2, MW2, GaugeXiW*MW2, 0.D0)*(-1.D0*GaugeXiW*MW2*(Mh02*(ML2 - 2.D0*MW2) + 2.D0*ML2*(ML2 + 2.D0*MW2)) + ML2*DBLE(Gau&
  &geXiW**INT(2.D0))*DBLE(MW**INT(4.D0)) + ML2*(Mh02*(2.D0*ML2 - 3.D0*MW2) + 2.D0*ML2*MW2 + 3.D0*DBLE(MW**INT(4.D0)))))/ (Mh02 - &
  &4.D0*ML2)))/(MW2*PI2*SW2*Yuk4)

 amplitudes(22) = (-0.0078125D0*CBA*EL2*Yuk6*(((1.D0 - 4.D0*SW2)*A0(MZ2))/MZ2 + ((-1.D0 + 4.D0*SW2)*A0(GaugeXiZ*MZ2))/MZ2 - (1.D0&
  &*Mh02*(MA02 - 1.D0*Mh02 + MZ2)*B0(Mh02, MA02, MZ2))/((Mh02 - 4.D0*ML2)*MZ2) + ((MA02 - 1.D0*Mh02 + MZ2)*(1.D0 - 4.D0*SW2)*B0(M&
  &h02, MA02, MZ2))/MZ2 + ((MA02 - 1.D0*Mh02)*Mh02*B0(Mh02, MA02, GaugeXiZ*MZ2))/ ((Mh02 - 4.D0*ML2)*MZ2) + ((MA02 - 1.D0*Mh02)*(&
  &-1.D0 + 4.D0*SW2)*B0(Mh02, MA02, GaugeXiZ*MZ2))/MZ2 + (2.D0*ML2*B0(ML2, MA02, ML2))/(Mh02 - 4.D0*ML2) + ((2.D0*MA02*ML2 - 2.D0&
  &*ML2*MZ2 + Mh02*(-2.D0*ML2 + MZ2))*B0(ML2, ML2, MZ2))/ ((Mh02 - 4.D0*ML2)*MZ2) + 2.D0*(-1.D0 + 4.D0*SW2)*B0(ML2, ML2, MZ2) + (&
  &(2.D0*(-1.D0*MA02 + Mh02)*ML2 + GaugeXiZ*(Mh02 - 4.D0*ML2)*MZ2)* B0(ML2, ML2, GaugeXiZ*MZ2))/((Mh02 - 4.D0*ML2)*MZ2) + 2.D0*MA&
  &02*(-1.D0 + 4.D0*SW2)*C0(Mh02, ML2, ML2, MA02, MZ2, ML2) - (2.D0*(MA02 - 1.D0*Mh02)*ML2*(MA02 - 1.D0*Mh02 + GaugeXiZ*MZ2)*C0(M&
  &h02, ML2, ML2, MA02, GaugeXiZ*MZ2, ML2))/((Mh02 - 4.D0*ML2)*MZ2) + (2.D0*C0(Mh02, ML2, ML2, MA02, MZ2, ML2)*(MA02*(-2.D0*ML2*M&
  &Z2 + Mh02*(-2.D0*ML2 + MZ2)) + ML2*DBLE(MA0**INT(4.D0)) + ML2*DBLE((Mh02 - 1.D0*MZ2)**INT(2.D0))))/((Mh02 - 4.D0*ML2)*MZ2)))/(&
  &CW2*PI2*SW2*Yuk4)

 amplitudes(23) = (0.0078125D0*EL2*SBA*(((1.D0 - 4.D0*SW2)*A0(MZ2))/MZ2 + ((-1.D0 + 4.D0*SW2)*A0(GaugeXiZ*MZ2))/MZ2 + (Mh02*(Mh02&
  & - 1.D0*MZ2 - 1.D0*GaugeXiZ*MZ2)*B0(Mh02, MZ2, GaugeXiZ*MZ2))/((Mh02 - 4.D0*ML2)*MZ2) - (1.D0*(-1.D0*Mh02 + MZ2 + GaugeXiZ*MZ2&
  &)*(-1.D0 + 4.D0*SW2)*B0(Mh02, MZ2, GaugeXiZ*MZ2))/MZ2 - (1.D0*Mh02*(Mh02 - 1.D0*GaugeXiZ*MZ2)*B0(Mh02, GaugeXiZ*MZ2, GaugeXiZ*&
  &MZ2))/((Mh02 - 4.D0*ML2)*MZ2) + ((Mh02 - 1.D0*GaugeXiZ*MZ2)*(1.D0 - 4.D0*SW2)*B0(Mh02, GaugeXiZ*MZ2, GaugeXiZ*MZ2))/MZ2 + ((-2&
  &.D0*ML2*MZ2 + 2.D0*GaugeXiZ*ML2*MZ2 + Mh02*(-2.D0*ML2 + MZ2))*B0(ML2, ML2, MZ2))/((Mh02 - 4.D0*ML2)*MZ2) + 2.D0*(-1.D0 + 4.D0*&
  &SW2)*B0(ML2, ML2, MZ2) + ((GaugeXiZ*(Mh02 - 6.D0*ML2)*MZ2 + 2.D0*ML2*(Mh02 + MZ2))*B0(ML2, ML2, GaugeXiZ*MZ2))/ ((Mh02 - 4.D0*&
  &ML2)*MZ2) + 2.D0*GaugeXiZ*MZ2*(-1.D0 + 4.D0*SW2)*C0(Mh02, ML2, ML2, MZ2, GaugeXiZ*MZ2, ML2) - (2.D0*ML2*(Mh02 - 2.D0*GaugeXiZ*&
  &MZ2)*(Mh02 - 1.D0*GaugeXiZ*MZ2)*C0(Mh02, ML2, ML2, GaugeXiZ*MZ2, GaugeXiZ*MZ2, ML2))/ ((Mh02 - 4.D0*ML2)*MZ2) + (2.D0*C0(Mh02,&
  & ML2, ML2, MZ2, GaugeXiZ*MZ2, ML2)*(ML2*DBLE(GaugeXiZ**INT(2.D0))*DBLE(MZ**INT(4.D0)) + GaugeXiZ*(-2.D0*ML2*DBLE(MZ**INT(4.D0)&
  &) + Mh02*(-2.D0*ML2*MZ2 + DBLE(MZ**INT(4.D0)))) + ML2*DBLE((Mh02 - 1.D0*MZ2)**INT(2.D0))))/ ((Mh02 - 4.D0*ML2)*MZ2)))/(CW2*PI2&
  &*SW2*Yuk4)

 amplitudes(24) = (-0.03125D0*CBA*EL2*ML2*Yuk6*(Mh02*(Mh02 - 1.D0*MHp2 - 3.D0*MW2)*B0(Mh02, MHp2, MW2) + Mh02*MW2*B0(ML2, 0.D0, M&
  &Hp2) + 2.D0*ML2*MW2*B0(ML2, 0.D0, MHp2) - 2.D0*Mh02*ML2*B0(ML2, 0.D0, MW2) + 2.D0*MHp2*ML2*B0(ML2, 0.D0, MW2) + 2.D0*Mh02*MW2*&
  &B0(ML2, 0.D0, MW2) - 2.D0*ML2*MW2*B0(ML2, 0.D0, MW2) + 2.D0*Mh02*ML2*B0(ML2, 0.D0, GaugeXiW*MW2) - 2.D0*MHp2*ML2*B0(ML2, 0.D0,&
  & GaugeXiW*MW2) - 3.D0*Mh02*MHp2*ML2*C0(Mh02, ML2, ML2, MHp2, MW2, 0.D0) + 3.D0*Mh02*MHp2*MW2*C0(Mh02, ML2, ML2, MHp2, MW2, 0.D&
  &0) - 1.D0*Mh02*ML2*MW2*C0(Mh02, ML2, ML2, MHp2, MW2, 0.D0) - 4.D0*MHp2*ML2*MW2*C0(Mh02, ML2, ML2, MHp2, MW2, 0.D0) + 3.D0*Mh02&
  &*MHp2*ML2*C0(Mh02, ML2, ML2, MHp2, GaugeXiW*MW2, 0.D0) - 1.D0*GaugeXiW*Mh02*MHp2*MW2*C0(Mh02, ML2, ML2, MHp2, GaugeXiW*MW2, 0.&
  &D0) - 2.D0*GaugeXiW*Mh02*ML2*MW2*C0(Mh02, ML2, ML2, MHp2, GaugeXiW*MW2, 0.D0) + 2.D0*GaugeXiW*MHp2*ML2*MW2* C0(Mh02, ML2, ML2,&
  & MHp2, GaugeXiW*MW2, 0.D0) + B0(Mh02, MHp2, GaugeXiW*MW2)*(Mh02*MHp2 - 1.D0*DBLE(Mh0**INT(4.D0))) + ML2*C0(Mh02, ML2, ML2, MHp&
  &2, MW2, 0.D0)*DBLE(Mh0**INT(4.D0)) - 1.D0*MW2*C0(Mh02, ML2, ML2, MHp2, MW2, 0.D0)*DBLE(Mh0**INT(4.D0)) - 1.D0*ML2*C0(Mh02, ML2&
  &, ML2, MHp2, GaugeXiW*MW2, 0.D0)*DBLE(Mh0**INT(4.D0)) + GaugeXiW*MW2*C0(Mh02, ML2, ML2, MHp2, GaugeXiW*MW2, 0.D0)* DBLE(Mh0**I&
  &NT(4.D0)) + 2.D0*ML2*C0(Mh02, ML2, ML2, MHp2, MW2, 0.D0)*DBLE(MHp**INT(4.D0)) - 2.D0*ML2*C0(Mh02, ML2, ML2, MHp2, GaugeXiW*MW2&
  &, 0.D0)*DBLE(MHp**INT(4.D0)) + Mh02*C0(Mh02, ML2, ML2, MHp2, MW2, 0.D0)*DBLE(MW**INT(4.D0)) + 2.D0*ML2*C0(Mh02, ML2, ML2, MHp2&
  &, MW2, 0.D0)*DBLE(MW**INT(4.D0))))/(Mh02*(Mh02 - 4.D0*ML2)*MW2*PI2*SW2*Yuk4)

 amplitudes(25) = (0.03125D0*EL2*ML2*SBA*(Mh02*(Mh02 - 3.D0*MW2 - 1.D0*GaugeXiW*MW2)*B0(Mh02, MW2, GaugeXiW*MW2) - 2.D0*Mh02*ML2*&
  &B0(ML2, 0.D0, MW2) + 2.D0*Mh02*MW2*B0(ML2, 0.D0, MW2) - 2.D0*ML2*MW2*B0(ML2, 0.D0, MW2) + 2.D0*GaugeXiW*ML2*MW2*B0(ML2, 0.D0, &
  &MW2) + 2.D0*Mh02*ML2*B0(ML2, 0.D0, GaugeXiW*MW2) + Mh02*MW2*B0(ML2, 0.D0, GaugeXiW*MW2) + 2.D0*ML2*MW2*B0(ML2, 0.D0, GaugeXiW*&
  &MW2) - 2.D0*GaugeXiW*ML2*MW2*B0(ML2, 0.D0, GaugeXiW*MW2) - 1.D0*Mh02*ML2*MW2*C0(Mh02, ML2, ML2, MW2, GaugeXiW*MW2, 0.D0) - 3.D&
  &0*GaugeXiW*Mh02*ML2*MW2*C0(Mh02, ML2, ML2, MW2, GaugeXiW*MW2, 0.D0) + GaugeXiW*Mh02*ML2*MW2*C0(Mh02, ML2, ML2, GaugeXiW*MW2, G&
  &augeXiW*MW2, 0.D0) + B0(Mh02, GaugeXiW*MW2, GaugeXiW*MW2)* (GaugeXiW*Mh02*MW2 - 1.D0*DBLE(Mh0**INT(4.D0))) + ML2*C0(Mh02, ML2,&
  & ML2, MW2, GaugeXiW*MW2, 0.D0)*DBLE(Mh0**INT(4.D0)) - 1.D0*MW2*C0(Mh02, ML2, ML2, MW2, GaugeXiW*MW2, 0.D0)*DBLE(Mh0**INT(4.D0)&
  &) - 1.D0*ML2*C0(Mh02, ML2, ML2, GaugeXiW*MW2, GaugeXiW*MW2, 0.D0)* DBLE(Mh0**INT(4.D0)) + GaugeXiW*MW2*C0(Mh02, ML2, ML2, Gaug&
  &eXiW*MW2, GaugeXiW*MW2, 0.D0)*DBLE(Mh0**INT(4.D0)) + Mh02*C0(Mh02, ML2, ML2, MW2, GaugeXiW*MW2, 0.D0)*DBLE(MW**INT(4.D0)) + 3.&
  &D0*GaugeXiW*Mh02*C0(Mh02, ML2, ML2, MW2, GaugeXiW*MW2, 0.D0)* DBLE(MW**INT(4.D0)) + 2.D0*ML2*C0(Mh02, ML2, ML2, MW2, GaugeXiW*&
  &MW2, 0.D0)*DBLE(MW**INT(4.D0)) - 4.D0*GaugeXiW*ML2*C0(Mh02, ML2, ML2, MW2, GaugeXiW*MW2, 0.D0)*DBLE(MW**INT(4.D0)) + 2.D0*ML2*&
  &C0(Mh02, ML2, ML2, MW2, GaugeXiW*MW2, 0.D0)* DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0)) - 1.D0*Mh02*C0(Mh02, ML2, ML2, Gaug&
  &eXiW*MW2, GaugeXiW*MW2, 0.D0)*DBLE(GaugeXiW**INT(2.D0))* DBLE(MW**INT(4.D0))))/(Mh02*(Mh02 - 4.D0*ML2)*MW2*PI2*SW2*Yuk4)

 amplitudes(26) = (0.0078125D0*EL2*MW2*SBA*DBLE(CW**INT(-4.D0))*DBLE(MZ**INT(-4.D0))*(-2.D0*Mh02*(Mh02 - 1.D0*MZ2 - 1.D0*GaugeXiZ&
  &*MZ2)*B0(Mh02, MZ2, GaugeXiZ*MZ2) + Mh02*(Mh02 - 2.D0*GaugeXiZ*MZ2)*B0(Mh02, GaugeXiZ*MZ2, GaugeXiZ*MZ2) + 4.D0*(-1.D0 + Gauge&
  &XiZ)*ML2*MZ2*B0(ML2, ML2, GaugeXiZ*MZ2) - 4.D0*C0(Mh02, ML2, ML2, MZ2, GaugeXiZ*MZ2, ML2)*(ML2*DBLE(GaugeXiZ**INT(2.D0))*DBLE(&
  &MZ**INT(4.D0)) + GaugeXiZ*(-2.D0*ML2*DBLE(MZ**INT(4.D0)) + Mh02*(-2.D0*ML2*MZ2 + DBLE(MZ**INT(4.D0)))) + ML2*DBLE((Mh02 - 1.D0&
  &*MZ2)**INT(2.D0))) + 2.D0*ML2*C0(Mh02, ML2, ML2, GaugeXiZ*MZ2, GaugeXiZ*MZ2, ML2)*DBLE((Mh02 - 2.D0*GaugeXiZ*MZ2)**INT(2.D0)) &
  &- 4.D0*MZ2*B0(ML2, ML2, MZ2)*(-1.D0*ML2 + GaugeXiZ*ML2 + 2.D0*MZ2*(1.D0 - 4.D0*SW2 + 8.D0*DBLE(SW**INT(4.D0)))) + B0(Mh02, MZ2&
  &, MZ2)*(-2.D0*Mh02*MZ2 + DBLE(Mh0**INT(4.D0)) + 8.D0*DBLE(MZ**INT(4.D0))*(1.D0 - 4.D0*SW2 + 8.D0*DBLE(SW**INT(4.D0)))) + 2.D0*&
  &C0(Mh02, ML2, ML2, MZ2, MZ2, ML2)*(ML2*DBLE(Mh0**INT(4.D0)) - 4.D0*DBLE(MZ**INT(4.D0))*(MZ2*(1.D0 - 4.D0*SW2 + 8.D0*DBLE(SW**I&
  &NT(4.D0))) + ML2*(-1.D0 - 8.D0*SW2 + 16.D0*DBLE(SW**INT(4.D0)))) + 2.D0*Mh02*MZ2*(-2.D0*ML2 + MZ2*DBLE((1.D0 - 4.D0*SW2)**INT(&
  &2.D0))))))/ ((Mh02 - 4.D0*ML2)*PI2*SW2*Yuk4)

 amplitudes(27) = (0.015625D0*EL2*SBA*((Mh02 - 2.D0*ML2)*(Mh02 - 2.D0*GaugeXiW*MW2)*B0(Mh02, GaugeXiW*MW2, GaugeXiW*MW2) - 2.D0*M&
  &W2*(-3.D0*ML2 + GaugeXiW*ML2 + 4.D0*MW2)*B0(ML2, 0.D0, MW2) + 2.D0*(-3.D0 + GaugeXiW)*ML2*MW2*B0(ML2, 0.D0, GaugeXiW*MW2) + 2.&
  &D0*(ML2 - 1.D0*MW2)*(Mh02*(ML2 - 2.D0*MW2) + 2.D0*MW2*(ML2 + 2.D0*MW2))*C0(Mh02, ML2, ML2, MW2, MW2, 0.D0) + 2.D0*ML2*(Mh02 - &
  &2.D0*GaugeXiW*MW2)*(ML2 - 1.D0*GaugeXiW*MW2)*C0(Mh02, ML2, ML2, GaugeXiW*MW2, GaugeXiW*MW2, 0.D0) - 2.D0*B0(Mh02, MW2, GaugeXi&
  &W*MW2)*(-1.D0*GaugeXiW*(Mh02 - 2.D0*ML2)*MW2 - 2.D0*ML2*MW2 - 1.D0*Mh02*(2.D0*ML2 + MW2) + DBLE(Mh0**INT(4.D0))) + B0(Mh02, MW&
  &2, MW2)*(-4.D0*ML2*MW2 - 2.D0*Mh02*(ML2 + MW2) + DBLE(Mh0**INT(4.D0)) + 8.D0*DBLE(MW**INT(4.D0))) - 2.D0*C0(Mh02, ML2, ML2, MW&
  &2, GaugeXiW*MW2, 0.D0)*(-1.D0*GaugeXiW*MW2*(Mh02*(ML2 - 2.D0*MW2) + 2.D0*ML2*(ML2 + 2.D0*MW2)) + ML2*DBLE(GaugeXiW**INT(2.D0))&
  &*DBLE(MW**INT(4.D0)) + ML2*(Mh02*(2.D0*ML2 - 3.D0*MW2) + 2.D0*ML2*MW2 + 3.D0*DBLE(MW**INT(4.D0))))))/ ((Mh02 - 4.D0*ML2)*MW2*P&
  &I2*SW2*Yuk4)

  totalAmplitude = (0D0,0D0)
 do j=1,27
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do

h0toTauPTauMProcDepVC = totalAmplitude
end function h0toTauPTauMProcDepVC