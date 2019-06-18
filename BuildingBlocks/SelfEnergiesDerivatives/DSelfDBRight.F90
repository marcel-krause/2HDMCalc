double complex function DSelfDBRight(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(9)

 amplitudes(1) = (-0.015625D0*CKM11*CKMC13*EL2*DBLE(x**INT(-2.D0))*(MB*MD*TB2*A0(MHp2)*DBLE(Yuk3**INT(2.D0)) - 1.D0*MB*MD*TB2*A0(&
  &MU2)*DBLE(Yuk3**INT(2.D0)) - 1.D0*MB*MD*MHp2*TB2*B0(x, MHp2, MU2)*DBLE(Yuk3**INT(2.D0)) + MB*MD*MU2*TB2*B0(x, MHp2, MU2)*DBLE(&
  &Yuk3**INT(2.D0)) + MB*MD*TB2*x*B0(x, MHp2, MU2)*DBLE(Yuk3**INT(2.D0))))/(MW2*PI2*SW2*TB2) + (0.015625D0*CKM11*CKMC13*EL2*(MB*M&
  &D*TB2*B0(x, MHp2, MU2)*DBLE(Yuk3**INT(2.D0)) - 1.D0*MB*MD*MHp2*TB2*DB0(x, MHp2, MU2)*DBLE(Yuk3**INT(2.D0)) + MB*MD*MU2*TB2*DB0&
  &(x, MHp2, MU2)*DBLE(Yuk3**INT(2.D0)) + MB*MD*TB2*x*DB0(x, MHp2, MU2)*DBLE(Yuk3**INT(2.D0))))/(MW2*PI2*SW2*TB2*x)

 amplitudes(2) = (-0.015625D0*CKM21*CKMC23*EL2*DBLE(x**INT(-2.D0))*(-1.D0*MB*MD*TB2*A0(MC2)*DBLE(Yuk3**INT(2.D0)) + MB*MD*TB2*A0(&
  &MHp2)*DBLE(Yuk3**INT(2.D0)) + MB*MC2*MD*TB2*B0(x, MC2, MHp2)*DBLE(Yuk3**INT(2.D0)) - 1.D0*MB*MD*MHp2*TB2*B0(x, MC2, MHp2)*DBLE&
  &(Yuk3**INT(2.D0)) + MB*MD*TB2*x*B0(x, MC2, MHp2)*DBLE(Yuk3**INT(2.D0))))/(MW2*PI2*SW2*TB2) + (0.015625D0*CKM21*CKMC23*EL2*(MB*&
  &MD*TB2*B0(x, MC2, MHp2)*DBLE(Yuk3**INT(2.D0)) + MB*MC2*MD*TB2*DB0(x, MC2, MHp2)*DBLE(Yuk3**INT(2.D0)) - 1.D0*MB*MD*MHp2*TB2*DB&
  &0(x, MC2, MHp2)*DBLE(Yuk3**INT(2.D0)) + MB*MD*TB2*x*DB0(x, MC2, MHp2)*DBLE(Yuk3**INT(2.D0))))/(MW2*PI2*SW2*TB2*x)

 amplitudes(3) = (-0.015625D0*CKM31*CKMC33*EL2*DBLE(x**INT(-2.D0))*(MB*MD*TB2*A0(MHp2)*DBLE(Yuk3**INT(2.D0)) - 1.D0*MB*MD*TB2*A0(&
  &MT2)*DBLE(Yuk3**INT(2.D0)) - 1.D0*MB*MD*MHp2*TB2*B0(x, MHp2, MT2)*DBLE(Yuk3**INT(2.D0)) + MB*MD*MT2*TB2*B0(x, MHp2, MT2)*DBLE(&
  &Yuk3**INT(2.D0)) + MB*MD*TB2*x*B0(x, MHp2, MT2)*DBLE(Yuk3**INT(2.D0))))/(MW2*PI2*SW2*TB2) + (0.015625D0*CKM31*CKMC33*EL2*(MB*M&
  &D*TB2*B0(x, MHp2, MT2)*DBLE(Yuk3**INT(2.D0)) - 1.D0*MB*MD*MHp2*TB2*DB0(x, MHp2, MT2)*DBLE(Yuk3**INT(2.D0)) + MB*MD*MT2*TB2*DB0&
  &(x, MHp2, MT2)*DBLE(Yuk3**INT(2.D0)) + MB*MD*TB2*x*DB0(x, MHp2, MT2)*DBLE(Yuk3**INT(2.D0))))/(MW2*PI2*SW2*TB2*x)

 amplitudes(4) = (0.015625D0*CKM11*CKMC13*EL2*(MB*MD*B0(x, MU2, GaugeXiW*MW2) + MB*MD*MU2*DB0(x, MU2, GaugeXiW*MW2) - 1.D0*GaugeX&
  &iW*MB*MD*MW2*DB0(x, MU2, GaugeXiW*MW2) + MB*MD*x*DB0(x, MU2, GaugeXiW*MW2)))/(MW2*PI2*SW2*x) - (0.015625D0*CKM11*CKMC13*EL2*(-&
  &1.D0*MB*MD*A0(MU2) + MB*MD*A0(GaugeXiW*MW2) + MB*MD*MU2*B0(x, MU2, GaugeXiW*MW2) - 1.D0*GaugeXiW*MB*MD*MW2*B0(x, MU2, GaugeXiW&
  &*MW2) + MB*MD*x*B0(x, MU2, GaugeXiW*MW2))*DBLE(x**INT(-2.D0)))/(MW2*PI2*SW2)

 amplitudes(5) = (0.015625D0*CKM21*CKMC23*EL2*(MB*MD*B0(x, MC2, GaugeXiW*MW2) + MB*MC2*MD*DB0(x, MC2, GaugeXiW*MW2) - 1.D0*GaugeX&
  &iW*MB*MD*MW2*DB0(x, MC2, GaugeXiW*MW2) + MB*MD*x*DB0(x, MC2, GaugeXiW*MW2)))/(MW2*PI2*SW2*x) - (0.015625D0*CKM21*CKMC23*EL2*(-&
  &1.D0*MB*MD*A0(MC2) + MB*MD*A0(GaugeXiW*MW2) + MB*MC2*MD*B0(x, MC2, GaugeXiW*MW2) - 1.D0*GaugeXiW*MB*MD*MW2*B0(x, MC2, GaugeXiW&
  &*MW2) + MB*MD*x*B0(x, MC2, GaugeXiW*MW2))*DBLE(x**INT(-2.D0)))/(MW2*PI2*SW2)

 amplitudes(6) = (0.015625D0*CKM31*CKMC33*EL2*(MB*MD*B0(x, MT2, GaugeXiW*MW2) + MB*MD*MT2*DB0(x, MT2, GaugeXiW*MW2) - 1.D0*GaugeX&
  &iW*MB*MD*MW2*DB0(x, MT2, GaugeXiW*MW2) + MB*MD*x*DB0(x, MT2, GaugeXiW*MW2)))/(MW2*PI2*SW2*x) - (0.015625D0*CKM31*CKMC33*EL2*(-&
  &1.D0*MB*MD*A0(MT2) + MB*MD*A0(GaugeXiW*MW2) + MB*MD*MT2*B0(x, MT2, GaugeXiW*MW2) - 1.D0*GaugeXiW*MB*MD*MW2*B0(x, MT2, GaugeXiW&
  &*MW2) + MB*MD*x*B0(x, MT2, GaugeXiW*MW2))*DBLE(x**INT(-2.D0)))/(MW2*PI2*SW2)

 amplitudes(7) = 0.D0

 amplitudes(8) = 0.D0

 amplitudes(9) = 0.D0

  totalAmplitude = (0D0,0D0)
 do j=1,9
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 DSelfDBRight = totalAmplitude
end function DSelfDBRight

