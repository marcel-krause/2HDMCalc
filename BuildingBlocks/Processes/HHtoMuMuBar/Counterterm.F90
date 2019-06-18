double precision function HHtoMuMuBarCT(x)
 use constants
 use counterterms
 implicit none
#include "looptools.h"
 integer, intent(in) :: x
 double precision :: totalAmplitude

 select case (x)
	case (1)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSUsual()/MM - dMW2Usual()/(2D0*MW2) + Yuk6*dBeta1KanUsual() + &
			& Yuk4/Yuk5*dAlphaKanUsual() + dZHHHHOS()/2D0 + Yuk4/Yuk5 * dZh0HHOSUsual()/2D0 + dZMuMuOSLeft()/2D0 + &
			& dZMuMuOSRight()/2D0 ) * (0.5D0*EL2*(MHH2 - 4.D0*MM2)*MM2*DBLE(Yuk5**INT(2.D0)))/(MW2*SW2)
	case (2)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSUsual()/MM - dMW2Usual()/(2D0*MW2) + Yuk6*dBeta2KanUsual() + &
			& Yuk4/Yuk5*dAlphaKanUsual() + dZHHHHOS()/2D0 + Yuk4/Yuk5 * dZh0HHOSUsual()/2D0 + dZMuMuOSLeft()/2D0 + &
			& dZMuMuOSRight()/2D0 ) * (0.5D0*EL2*(MHH2 - 4.D0*MM2)*MM2*DBLE(Yuk5**INT(2.D0)))/(MW2*SW2)
	case (3)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) + Yuk6*dBeta1KanAlter() + &
			& Yuk4/Yuk5*dAlphaKanAlter() + dZHHHHOS()/2D0 + Yuk4/Yuk5 * dZh0HHOSAlter()/2D0 + dZMuMuOSLeft()/2D0 + &
			& dZMuMuOSRight()/2D0 ) * (0.5D0*EL2*(MHH2 - 4.D0*MM2)*MM2*DBLE(Yuk5**INT(2.D0)))/(MW2*SW2)
	case (4)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) + Yuk6*dBeta2KanAlter() + &
			& Yuk4/Yuk5*dAlphaKanAlter() + dZHHHHOS()/2D0 + Yuk4/Yuk5 * dZh0HHOSAlter()/2D0 + dZMuMuOSLeft()/2D0 + &
			& dZMuMuOSRight()/2D0 ) * (0.5D0*EL2*(MHH2 - 4.D0*MM2)*MM2*DBLE(Yuk5**INT(2.D0)))/(MW2*SW2)
	case (5)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) + Yuk6*dBeta1PinchPStar() + &
			& Yuk4/Yuk5*dAlphaPinchPStar() + dZHHHHOS()/2D0 + Yuk4/Yuk5 * dZh0HHOSAlter()/2D0 + dZMuMuOSLeft()/2D0 + &
			& dZMuMuOSRight()/2D0 ) * (0.5D0*EL2*(MHH2 - 4.D0*MM2)*MM2*DBLE(Yuk5**INT(2.D0)))/(MW2*SW2)
	case (6)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) + Yuk6*dBeta2PinchPStar() + &
			& Yuk4/Yuk5*dAlphaPinchPStar() + dZHHHHOS()/2D0 + Yuk4/Yuk5 * dZh0HHOSAlter()/2D0 + dZMuMuOSLeft()/2D0 + &
			& dZMuMuOSRight()/2D0 ) * (0.5D0*EL2*(MHH2 - 4.D0*MM2)*MM2*DBLE(Yuk5**INT(2.D0)))/(MW2*SW2)
	case (7)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) + Yuk6*dBeta1PinchOS() + &
			& Yuk4/Yuk5*dAlphaPinchOS() + dZHHHHOS()/2D0 + Yuk4/Yuk5 * dZh0HHOSAlter()/2D0 + dZMuMuOSLeft()/2D0 + &
			& dZMuMuOSRight()/2D0 ) * (0.5D0*EL2*(MHH2 - 4.D0*MM2)*MM2*DBLE(Yuk5**INT(2.D0)))/(MW2*SW2)
	case (8)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) + Yuk6*dBeta2PinchOS() + &
			& Yuk4/Yuk5*dAlphaPinchOS() + dZHHHHOS()/2D0 + Yuk4/Yuk5 * dZh0HHOSAlter()/2D0 + dZMuMuOSLeft()/2D0 + &
			& dZMuMuOSRight()/2D0 ) * (0.5D0*EL2*(MHH2 - 4.D0*MM2)*MM2*DBLE(Yuk5**INT(2.D0)))/(MW2*SW2)
	case (9)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) + Yuk6*dBetaProcDep1Alter() + &
			& Yuk4/Yuk5*dAlphaProcDep1Alter() + dZHHHHOS()/2D0 + Yuk4/Yuk5 * dZh0HHOSAlter()/2D0 + dZMuMuOSLeft()/2D0 + &
			& dZMuMuOSRight()/2D0 ) * (0.5D0*EL2*(MHH2 - 4.D0*MM2)*MM2*DBLE(Yuk5**INT(2.D0)))/(MW2*SW2)
	case (10)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) + Yuk6*dBetaProcDep2Alter() + &
			& Yuk4/Yuk5*dAlphaProcDep2Alter() + dZHHHHOS()/2D0 + Yuk4/Yuk5 * dZh0HHOSAlter()/2D0 + dZMuMuOSLeft()/2D0 + &
			& dZMuMuOSRight()/2D0 ) * (0.5D0*EL2*(MHH2 - 4.D0*MM2)*MM2*DBLE(Yuk5**INT(2.D0)))/(MW2*SW2)
	case (11)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) + Yuk6*dBetaProcDep3Alter() + &
			& Yuk4/Yuk5*dAlphaProcDep3Alter() + dZHHHHOS()/2D0 + Yuk4/Yuk5 * dZh0HHOSAlter()/2D0 + dZMuMuOSLeft()/2D0 + &
			& dZMuMuOSRight()/2D0 ) * (0.5D0*EL2*(MHH2 - 4.D0*MM2)*MM2*DBLE(Yuk5**INT(2.D0)))/(MW2*SW2)
	case (12)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) + Yuk6*dBetaOS1Alter() + &
			& Yuk4/Yuk5*dAlphaOS1Alter() + dZHHHHOS()/2D0 + Yuk4/Yuk5 * dZh0HHOSAlter()/2D0 + dZMuMuOSLeft()/2D0 + &
			& dZMuMuOSRight()/2D0 ) * (0.5D0*EL2*(MHH2 - 4.D0*MM2)*MM2*DBLE(Yuk5**INT(2.D0)))/(MW2*SW2)
	case (13)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) + Yuk6*dBetaOS2Alter() + &
			& Yuk4/Yuk5*dAlphaOS2Alter() + dZHHHHOS()/2D0 + Yuk4/Yuk5 * dZh0HHOSAlter()/2D0 + dZMuMuOSLeft()/2D0 + &
			& dZMuMuOSRight()/2D0 ) * (0.5D0*EL2*(MHH2 - 4.D0*MM2)*MM2*DBLE(Yuk5**INT(2.D0)))/(MW2*SW2)
	case (14)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) + Yuk6*dBetaOS12Alter() + &
			& Yuk4/Yuk5*dAlphaOS12Alter() + dZHHHHOS()/2D0 + Yuk4/Yuk5 * dZh0HHOSAlter()/2D0 + dZMuMuOSLeft()/2D0 + &
			& dZMuMuOSRight()/2D0 ) * (0.5D0*EL2*(MHH2 - 4.D0*MM2)*MM2*DBLE(Yuk5**INT(2.D0)))/(MW2*SW2)
	case (15)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) + Yuk6*dBetaBFMSAlter() + &
			& Yuk4/Yuk5*dAlphaPinchOS() + dZHHHHOS()/2D0 + Yuk4/Yuk5 * dZh0HHOSAlter()/2D0 + dZMuMuOSLeft()/2D0 + &
			& dZMuMuOSRight()/2D0 ) * (0.5D0*EL2*(MHH2 - 4.D0*MM2)*MM2*DBLE(Yuk5**INT(2.D0)))/(MW2*SW2)
	case (16)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSUsual()/MM - dMW2Usual()/(2D0*MW2) + Yuk6*dBetaMSBarUsual() + &
			& Yuk4/Yuk5*dAlphaMSBarUsual() + dZHHHHOS()/2D0 + Yuk4/Yuk5 * dZh0HHOSUsual()/2D0 + dZMuMuOSLeft()/2D0 + &
			& dZMuMuOSRight()/2D0 ) * (0.5D0*EL2*(MHH2 - 4.D0*MM2)*MM2*DBLE(Yuk5**INT(2.D0)))/(MW2*SW2)
	case (17)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) + Yuk6*dBetaMSBarAlter() + &
			& Yuk4/Yuk5*dAlphaMSBarAlter() + dZHHHHOS()/2D0 + Yuk4/Yuk5 * dZh0HHOSAlter()/2D0 + dZMuMuOSLeft()/2D0 + &
			& dZMuMuOSRight()/2D0 ) * (0.5D0*EL2*(MHH2 - 4.D0*MM2)*MM2*DBLE(Yuk5**INT(2.D0)))/(MW2*SW2)
 end select

 HHtoMuMuBarCT = totalAmplitude
end function HHtoMuMuBarCT
