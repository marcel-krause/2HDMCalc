double precision function h0toMuMuBarCT(x)
 use constants
 use counterterms
 implicit none
#include "looptools.h"
 integer, intent(in) :: x
 double precision :: totalAmplitude

 select case (x)
	case (1)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSUsual()/MM - dMW2Usual()/(2D0*MW2) + Yuk6*dBeta1KanUsual() - &
			& Yuk5/Yuk4*dAlphaKanUsual() + dZh0h0OS()/2D0 + Yuk5/Yuk4 * dZHHh0OSUsual()/2D0 + dZMuMuOSLeft()/2D0 + &
			& dZMuMuOSRight()/2D0 ) * (0.5D0*EL2*(Mh02 - 4.D0*MM2)*MM2*DBLE(Yuk4**INT(2.D0)))/(MW2*SW2)
	case (2)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSUsual()/MM - dMW2Usual()/(2D0*MW2) + Yuk6*dBeta2KanUsual() - &
			& Yuk5/Yuk4*dAlphaKanUsual() + dZh0h0OS()/2D0 + Yuk5/Yuk4 * dZHHh0OSUsual()/2D0 + dZMuMuOSLeft()/2D0 + &
			& dZMuMuOSRight()/2D0 ) * (0.5D0*EL2*(Mh02 - 4.D0*MM2)*MM2*DBLE(Yuk4**INT(2.D0)))/(MW2*SW2)
	case (3)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) + Yuk6*dBeta1KanAlter() - &
			& Yuk5/Yuk4*dAlphaKanAlter() + dZh0h0OS()/2D0 + Yuk5/Yuk4 * dZHHh0OSAlter()/2D0 + dZMuMuOSLeft()/2D0 + &
			& dZMuMuOSRight()/2D0 ) * (0.5D0*EL2*(Mh02 - 4.D0*MM2)*MM2*DBLE(Yuk4**INT(2.D0)))/(MW2*SW2)
	case (4)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) + Yuk6*dBeta2KanAlter() - &
			& Yuk5/Yuk4*dAlphaKanAlter() + dZh0h0OS()/2D0 + Yuk5/Yuk4 * dZHHh0OSAlter()/2D0 + dZMuMuOSLeft()/2D0 + &
			& dZMuMuOSRight()/2D0 ) * (0.5D0*EL2*(Mh02 - 4.D0*MM2)*MM2*DBLE(Yuk4**INT(2.D0)))/(MW2*SW2)
	case (5)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) + Yuk6*dBeta1PinchPStar() - &
			& Yuk5/Yuk4*dAlphaPinchPStar() + dZh0h0OS()/2D0 + Yuk5/Yuk4 * dZHHh0OSAlter()/2D0 + dZMuMuOSLeft()/2D0 + &
			& dZMuMuOSRight()/2D0 ) * (0.5D0*EL2*(Mh02 - 4.D0*MM2)*MM2*DBLE(Yuk4**INT(2.D0)))/(MW2*SW2)
	case (6)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) + Yuk6*dBeta2PinchPStar() - &
			& Yuk5/Yuk4*dAlphaPinchPStar() + dZh0h0OS()/2D0 + Yuk5/Yuk4 * dZHHh0OSAlter()/2D0 + dZMuMuOSLeft()/2D0 + &
			& dZMuMuOSRight()/2D0 ) * (0.5D0*EL2*(Mh02 - 4.D0*MM2)*MM2*DBLE(Yuk4**INT(2.D0)))/(MW2*SW2)
	case (7)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) + Yuk6*dBeta1PinchOS() - &
			& Yuk5/Yuk4*dAlphaPinchOS() + dZh0h0OS()/2D0 + Yuk5/Yuk4 * dZHHh0OSAlter()/2D0 + dZMuMuOSLeft()/2D0 + &
			& dZMuMuOSRight()/2D0 ) * (0.5D0*EL2*(Mh02 - 4.D0*MM2)*MM2*DBLE(Yuk4**INT(2.D0)))/(MW2*SW2)
	case (8)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) + Yuk6*dBeta2PinchOS() - &
			& Yuk5/Yuk4*dAlphaPinchOS() + dZh0h0OS()/2D0 + Yuk5/Yuk4 * dZHHh0OSAlter()/2D0 + dZMuMuOSLeft()/2D0 + &
			& dZMuMuOSRight()/2D0 ) * (0.5D0*EL2*(Mh02 - 4.D0*MM2)*MM2*DBLE(Yuk4**INT(2.D0)))/(MW2*SW2)
	case (9)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) + Yuk6*dBetaProcDep1Alter() - &
			& Yuk5/Yuk4*dAlphaProcDep1Alter() + dZh0h0OS()/2D0 + Yuk5/Yuk4 * dZHHh0OSAlter()/2D0 + dZMuMuOSLeft()/2D0 + &
			& dZMuMuOSRight()/2D0 ) * (0.5D0*EL2*(Mh02 - 4.D0*MM2)*MM2*DBLE(Yuk4**INT(2.D0)))/(MW2*SW2)
	case (10)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) + Yuk6*dBetaProcDep2Alter() - &
			& Yuk5/Yuk4*dAlphaProcDep2Alter() + dZh0h0OS()/2D0 + Yuk5/Yuk4 * dZHHh0OSAlter()/2D0 + dZMuMuOSLeft()/2D0 + &
			& dZMuMuOSRight()/2D0 ) * (0.5D0*EL2*(Mh02 - 4.D0*MM2)*MM2*DBLE(Yuk4**INT(2.D0)))/(MW2*SW2)
	case (11)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) + Yuk6*dBetaProcDep3Alter() - &
			& Yuk5/Yuk4*dAlphaProcDep3Alter() + dZh0h0OS()/2D0 + Yuk5/Yuk4 * dZHHh0OSAlter()/2D0 + dZMuMuOSLeft()/2D0 + &
			& dZMuMuOSRight()/2D0 ) * (0.5D0*EL2*(Mh02 - 4.D0*MM2)*MM2*DBLE(Yuk4**INT(2.D0)))/(MW2*SW2)
	case (12)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) + Yuk6*dBetaOS1Alter() - &
			& Yuk5/Yuk4*dAlphaOS1Alter() + dZh0h0OS()/2D0 + Yuk5/Yuk4 * dZHHh0OSAlter()/2D0 + dZMuMuOSLeft()/2D0 + &
			& dZMuMuOSRight()/2D0 ) * (0.5D0*EL2*(Mh02 - 4.D0*MM2)*MM2*DBLE(Yuk4**INT(2.D0)))/(MW2*SW2)
	case (13)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) + Yuk6*dBetaOS2Alter() - &
			& Yuk5/Yuk4*dAlphaOS2Alter() + dZh0h0OS()/2D0 + Yuk5/Yuk4 * dZHHh0OSAlter()/2D0 + dZMuMuOSLeft()/2D0 + &
			& dZMuMuOSRight()/2D0 ) * (0.5D0*EL2*(Mh02 - 4.D0*MM2)*MM2*DBLE(Yuk4**INT(2.D0)))/(MW2*SW2)
	case (14)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) + Yuk6*dBetaOS12Alter() - &
			& Yuk5/Yuk4*dAlphaOS12Alter() + dZh0h0OS()/2D0 + Yuk5/Yuk4 * dZHHh0OSAlter()/2D0 + dZMuMuOSLeft()/2D0 + &
			& dZMuMuOSRight()/2D0 ) * (0.5D0*EL2*(Mh02 - 4.D0*MM2)*MM2*DBLE(Yuk4**INT(2.D0)))/(MW2*SW2)
	case (15)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) + Yuk6*dBetaBFMSAlter() - &
			& Yuk5/Yuk4*dAlphaPinchOS() + dZh0h0OS()/2D0 + Yuk5/Yuk4 * dZHHh0OSAlter()/2D0 + dZMuMuOSLeft()/2D0 + &
			& dZMuMuOSRight()/2D0 ) * (0.5D0*EL2*(Mh02 - 4.D0*MM2)*MM2*DBLE(Yuk4**INT(2.D0)))/(MW2*SW2)
	case (16)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSUsual()/MM - dMW2Usual()/(2D0*MW2) + Yuk6*dBetaMSBarUsual() - &
			& Yuk5/Yuk4*dAlphaMSBarUsual() + dZh0h0OS()/2D0 + Yuk5/Yuk4 * dZHHh0OSUsual()/2D0 + dZMuMuOSLeft()/2D0 + &
			& dZMuMuOSRight()/2D0 ) * (0.5D0*EL2*(Mh02 - 4.D0*MM2)*MM2*DBLE(Yuk4**INT(2.D0)))/(MW2*SW2)
	case (17)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) + Yuk6*dBetaMSBarAlter() - &
			& Yuk5/Yuk4*dAlphaMSBarAlter() + dZh0h0OS()/2D0 + Yuk5/Yuk4 * dZHHh0OSAlter()/2D0 + dZMuMuOSLeft()/2D0 + &
			& dZMuMuOSRight()/2D0 ) * (0.5D0*EL2*(Mh02 - 4.D0*MM2)*MM2*DBLE(Yuk4**INT(2.D0)))/(MW2*SW2)
 end select

 h0toMuMuBarCT = totalAmplitude
end function h0toMuMuBarCT
