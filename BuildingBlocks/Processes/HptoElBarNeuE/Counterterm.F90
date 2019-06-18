double precision function HptoElBarNeuECT(x)
 use constants
 use counterterms
 implicit none
#include "looptools.h"
 integer, intent(in) :: x
 double precision :: totalAmplitude

 select case (x)
	case (1)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMEOSUsual()/ME - dMW2Usual()/(2D0*MW2) + &
			& (1D0 + Yuk6**2)/Yuk6*dBeta1KanUsual() + dZElElOSRight()/2D0 + dZNeuENeuEOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 - 1D0/Yuk6*dZGpHpOSUsual()/2D0 )*&
			& (0.5D0*EL2*ME2*(-1.D0*ME2 + MHp2)*DBLE(Yuk6**INT(2.D0)))/(MW2*SW2)
	case (2)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMEOSUsual()/ME - dMW2Usual()/(2D0*MW2) + &
			& (1D0 + Yuk6**2)/Yuk6*dBeta2KanUsual() + dZElElOSRight()/2D0 + dZNeuENeuEOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 - 1D0/Yuk6*dZGpHpOSUsual()/2D0 )*&
			& (0.5D0*EL2*ME2*(-1.D0*ME2 + MHp2)*DBLE(Yuk6**INT(2.D0)))/(MW2*SW2)
	case (3)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMEOSAlter()/ME - dMW2Alter()/(2D0*MW2) + &
			& (1D0 + Yuk6**2)/Yuk6*dBeta1KanAlter() + dZElElOSRight()/2D0 + dZNeuENeuEOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 - 1D0/Yuk6*dZGpHpOSAlter()/2D0 )*&
			& (0.5D0*EL2*ME2*(-1.D0*ME2 + MHp2)*DBLE(Yuk6**INT(2.D0)))/(MW2*SW2)
	case (4)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMEOSAlter()/ME - dMW2Alter()/(2D0*MW2) + &
			& (1D0 + Yuk6**2)/Yuk6*dBeta2KanAlter() + dZElElOSRight()/2D0 + dZNeuENeuEOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 - 1D0/Yuk6*dZGpHpOSAlter()/2D0 )*&
			& (0.5D0*EL2*ME2*(-1.D0*ME2 + MHp2)*DBLE(Yuk6**INT(2.D0)))/(MW2*SW2)
	case (5)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMEOSAlter()/ME - dMW2Alter()/(2D0*MW2) + &
			& (1D0 + Yuk6**2)/Yuk6*dBeta1PinchPStar() + dZElElOSRight()/2D0 + dZNeuENeuEOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 - 1D0/Yuk6*dZGpHpOSAlter()/2D0 )*&
			& (0.5D0*EL2*ME2*(-1.D0*ME2 + MHp2)*DBLE(Yuk6**INT(2.D0)))/(MW2*SW2)
	case (6)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMEOSAlter()/ME - dMW2Alter()/(2D0*MW2) + &
			& (1D0 + Yuk6**2)/Yuk6*dBeta2PinchPStar() + dZElElOSRight()/2D0 + dZNeuENeuEOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 - 1D0/Yuk6*dZGpHpOSAlter()/2D0 )*&
			& (0.5D0*EL2*ME2*(-1.D0*ME2 + MHp2)*DBLE(Yuk6**INT(2.D0)))/(MW2*SW2)
	case (7)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMEOSAlter()/ME - dMW2Alter()/(2D0*MW2) + &
			& (1D0 + Yuk6**2)/Yuk6*dBeta1PinchOS() + dZElElOSRight()/2D0 + dZNeuENeuEOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 - 1D0/Yuk6*dZGpHpOSAlter()/2D0 )*&
			& (0.5D0*EL2*ME2*(-1.D0*ME2 + MHp2)*DBLE(Yuk6**INT(2.D0)))/(MW2*SW2)
	case (8)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMEOSAlter()/ME - dMW2Alter()/(2D0*MW2) + &
			& (1D0 + Yuk6**2)/Yuk6*dBeta2PinchOS() + dZElElOSRight()/2D0 + dZNeuENeuEOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 - 1D0/Yuk6*dZGpHpOSAlter()/2D0 )*&
			& (0.5D0*EL2*ME2*(-1.D0*ME2 + MHp2)*DBLE(Yuk6**INT(2.D0)))/(MW2*SW2)
	case (9)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMEOSAlter()/ME - dMW2Alter()/(2D0*MW2) + &
			& (1D0 + Yuk6**2)/Yuk6*dBetaProcDep1Alter() + dZElElOSRight()/2D0 + dZNeuENeuEOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 - 1D0/Yuk6*dZGpHpOSAlter()/2D0 )*&
			& (0.5D0*EL2*ME2*(-1.D0*ME2 + MHp2)*DBLE(Yuk6**INT(2.D0)))/(MW2*SW2)
	case (10)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMEOSAlter()/ME - dMW2Alter()/(2D0*MW2) + &
			& (1D0 + Yuk6**2)/Yuk6*dBetaProcDep2Alter() + dZElElOSRight()/2D0 + dZNeuENeuEOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 - 1D0/Yuk6*dZGpHpOSAlter()/2D0 )*&
			& (0.5D0*EL2*ME2*(-1.D0*ME2 + MHp2)*DBLE(Yuk6**INT(2.D0)))/(MW2*SW2)
	case (11)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMEOSAlter()/ME - dMW2Alter()/(2D0*MW2) + &
			& (1D0 + Yuk6**2)/Yuk6*dBetaProcDep3Alter() + dZElElOSRight()/2D0 + dZNeuENeuEOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 - 1D0/Yuk6*dZGpHpOSAlter()/2D0 )*&
			& (0.5D0*EL2*ME2*(-1.D0*ME2 + MHp2)*DBLE(Yuk6**INT(2.D0)))/(MW2*SW2)
	case (12)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMEOSAlter()/ME - dMW2Alter()/(2D0*MW2) + &
			& (1D0 + Yuk6**2)/Yuk6*dBetaOS1Alter() + dZElElOSRight()/2D0 + dZNeuENeuEOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 - 1D0/Yuk6*dZGpHpOSAlter()/2D0 )*&
			& (0.5D0*EL2*ME2*(-1.D0*ME2 + MHp2)*DBLE(Yuk6**INT(2.D0)))/(MW2*SW2)
	case (13)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMEOSAlter()/ME - dMW2Alter()/(2D0*MW2) + &
			& (1D0 + Yuk6**2)/Yuk6*dBetaOS2Alter() + dZElElOSRight()/2D0 + dZNeuENeuEOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 - 1D0/Yuk6*dZGpHpOSAlter()/2D0 )*&
			& (0.5D0*EL2*ME2*(-1.D0*ME2 + MHp2)*DBLE(Yuk6**INT(2.D0)))/(MW2*SW2)
	case (14)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMEOSAlter()/ME - dMW2Alter()/(2D0*MW2) + &
			& (1D0 + Yuk6**2)/Yuk6*dBetaOS12Alter() + dZElElOSRight()/2D0 + dZNeuENeuEOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 - 1D0/Yuk6*dZGpHpOSAlter()/2D0 )*&
			& (0.5D0*EL2*ME2*(-1.D0*ME2 + MHp2)*DBLE(Yuk6**INT(2.D0)))/(MW2*SW2)
	case (15)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMEOSAlter()/ME - dMW2Alter()/(2D0*MW2) + &
			& (1D0 + Yuk6**2)/Yuk6*dBetaBFMSAlter() + dZElElOSRight()/2D0 + dZNeuENeuEOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 - 1D0/Yuk6*dZGpHpOSAlter()/2D0 )*&
			& (0.5D0*EL2*ME2*(-1.D0*ME2 + MHp2)*DBLE(Yuk6**INT(2.D0)))/(MW2*SW2)
	case (16)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMEOSUsual()/ME - dMW2Usual()/(2D0*MW2) + &
			& (1D0 + Yuk6**2)/Yuk6*dBetaMSBarUsual() + dZElElOSRight()/2D0 + dZNeuENeuEOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 - 1D0/Yuk6*dZGpHpOSUsual()/2D0 )*&
			& (0.5D0*EL2*ME2*(-1.D0*ME2 + MHp2)*DBLE(Yuk6**INT(2.D0)))/(MW2*SW2)
	case (17)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMEOSAlter()/ME - dMW2Alter()/(2D0*MW2) + &
			& (1D0 + Yuk6**2)/Yuk6*dBetaMSBarAlter() + dZElElOSRight()/2D0 + dZNeuENeuEOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 - 1D0/Yuk6*dZGpHpOSAlter()/2D0 )*&
			& (0.5D0*EL2*ME2*(-1.D0*ME2 + MHp2)*DBLE(Yuk6**INT(2.D0)))/(MW2*SW2)
	case default
		totalAmplitude = 0D0
 end select

 HptoElBarNeuECT = totalAmplitude
end function HptoElBarNeuECT
