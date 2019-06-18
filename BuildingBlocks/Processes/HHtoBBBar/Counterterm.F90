double precision function HHtoBBBarCT(x)
 use constants
 use counterterms
 implicit none
#include "looptools.h"
 integer, intent(in) :: x
 double precision :: totalAmplitude

 select case (x)
	case (1)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMBOSUsual()/MB - dMW2Usual()/(2D0*MW2) + Yuk3*dBeta1KanUsual() + &
			& Yuk1/Yuk2*dAlphaKanUsual() + dZHHHHOS()/2D0 + Yuk1/Yuk2*dZh0HHOSUsual()/2D0 + dZBBOSLeft()/2D0 + &
			& dZBBOSRight()/2D0 )*(1.5D0*EL2*MB2*(-4.D0*MB2 + MHH2)*DBLE(Yuk2**INT(2.D0)))/(MW2*SW2)
	case (2)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMBOSUsual()/MB - dMW2Usual()/(2D0*MW2) + Yuk3*dBeta2KanUsual() + &
			& Yuk1/Yuk2*dAlphaKanUsual() + dZHHHHOS()/2D0 + Yuk1/Yuk2*dZh0HHOSUsual()/2D0 + dZBBOSLeft()/2D0 + &
			& dZBBOSRight()/2D0 )*(1.5D0*EL2*MB2*(-4.D0*MB2 + MHH2)*DBLE(Yuk2**INT(2.D0)))/(MW2*SW2)
	case (3)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMBOSAlter()/MB - dMW2Alter()/(2D0*MW2) + Yuk3*dBeta1KanAlter() + &
			& Yuk1/Yuk2*dAlphaKanAlter() + dZHHHHOS()/2D0 + Yuk1/Yuk2*dZh0HHOSAlter()/2D0 + dZBBOSLeft()/2D0 + &
			& dZBBOSRight()/2D0 )*(1.5D0*EL2*MB2*(-4.D0*MB2 + MHH2)*DBLE(Yuk2**INT(2.D0)))/(MW2*SW2)	
	case (4)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMBOSAlter()/MB - dMW2Alter()/(2D0*MW2) + Yuk3*dBeta2KanAlter() + &
			& Yuk1/Yuk2*dAlphaKanAlter() + dZHHHHOS()/2D0 + Yuk1/Yuk2*dZh0HHOSAlter()/2D0 + dZBBOSLeft()/2D0 + &
			& dZBBOSRight()/2D0 )*(1.5D0*EL2*MB2*(-4.D0*MB2 + MHH2)*DBLE(Yuk2**INT(2.D0)))/(MW2*SW2)
	case (5)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMBOSAlter()/MB - dMW2Alter()/(2D0*MW2) + Yuk3*dBeta1PinchPStar() + &
			& Yuk1/Yuk2*dAlphaPinchPStar() + dZHHHHOS()/2D0 + Yuk1/Yuk2*dZh0HHOSAlter()/2D0 + dZBBOSLeft()/2D0 + &
			& dZBBOSRight()/2D0 )*(1.5D0*EL2*MB2*(-4.D0*MB2 + MHH2)*DBLE(Yuk2**INT(2.D0)))/(MW2*SW2)
	case (6)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMBOSAlter()/MB - dMW2Alter()/(2D0*MW2) + Yuk3*dBeta2PinchPStar() + &
			& Yuk1/Yuk2*dAlphaPinchPStar() + dZHHHHOS()/2D0 + Yuk1/Yuk2*dZh0HHOSAlter()/2D0 + dZBBOSLeft()/2D0 + &
			& dZBBOSRight()/2D0 )*(1.5D0*EL2*MB2*(-4.D0*MB2 + MHH2)*DBLE(Yuk2**INT(2.D0)))/(MW2*SW2)
	case (7)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMBOSAlter()/MB - dMW2Alter()/(2D0*MW2) + Yuk3*dBeta1PinchOS() + &
			& Yuk1/Yuk2*dAlphaPinchOS() + dZHHHHOS()/2D0 + Yuk1/Yuk2*dZh0HHOSAlter()/2D0 + dZBBOSLeft()/2D0 + &
			& dZBBOSRight()/2D0 )*(1.5D0*EL2*MB2*(-4.D0*MB2 + MHH2)*DBLE(Yuk2**INT(2.D0)))/(MW2*SW2)
	case (8)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMBOSAlter()/MB - dMW2Alter()/(2D0*MW2) + Yuk3*dBeta2PinchOS() + &
			& Yuk1/Yuk2*dAlphaPinchOS() + dZHHHHOS()/2D0 + Yuk1/Yuk2*dZh0HHOSAlter()/2D0 + dZBBOSLeft()/2D0 + &
			& dZBBOSRight()/2D0 )*(1.5D0*EL2*MB2*(-4.D0*MB2 + MHH2)*DBLE(Yuk2**INT(2.D0)))/(MW2*SW2)
	case (9)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMBOSAlter()/MB - dMW2Alter()/(2D0*MW2) + Yuk3*dBetaProcDep1Alter() + &
			& Yuk1/Yuk2*dAlphaProcDep1Alter() + dZHHHHOS()/2D0 + Yuk1/Yuk2*dZh0HHOSAlter()/2D0 + dZBBOSLeft()/2D0 + &
			& dZBBOSRight()/2D0 )*(1.5D0*EL2*MB2*(-4.D0*MB2 + MHH2)*DBLE(Yuk2**INT(2.D0)))/(MW2*SW2)
	case (10)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMBOSAlter()/MB - dMW2Alter()/(2D0*MW2) + Yuk3*dBetaProcDep2Alter() + &
			& Yuk1/Yuk2*dAlphaProcDep2Alter() + dZHHHHOS()/2D0 + Yuk1/Yuk2*dZh0HHOSAlter()/2D0 + dZBBOSLeft()/2D0 + &
			& dZBBOSRight()/2D0 )*(1.5D0*EL2*MB2*(-4.D0*MB2 + MHH2)*DBLE(Yuk2**INT(2.D0)))/(MW2*SW2)
	case (11)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMBOSAlter()/MB - dMW2Alter()/(2D0*MW2) + Yuk3*dBetaProcDep3Alter() + &
			& Yuk1/Yuk2*dAlphaProcDep3Alter() + dZHHHHOS()/2D0 + Yuk1/Yuk2*dZh0HHOSAlter()/2D0 + dZBBOSLeft()/2D0 + &
			& dZBBOSRight()/2D0 )*(1.5D0*EL2*MB2*(-4.D0*MB2 + MHH2)*DBLE(Yuk2**INT(2.D0)))/(MW2*SW2)
	case (12)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMBOSAlter()/MB - dMW2Alter()/(2D0*MW2) + Yuk3*dBetaOS1Alter() + &
			& Yuk1/Yuk2*dAlphaOS1Alter() + dZHHHHOS()/2D0 + Yuk1/Yuk2*dZh0HHOSAlter()/2D0 + dZBBOSLeft()/2D0 + &
			& dZBBOSRight()/2D0 )*(1.5D0*EL2*MB2*(-4.D0*MB2 + MHH2)*DBLE(Yuk2**INT(2.D0)))/(MW2*SW2)
	case (13)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMBOSAlter()/MB - dMW2Alter()/(2D0*MW2) + Yuk3*dBetaOS2Alter() + &
			& Yuk1/Yuk2*dAlphaOS2Alter() + dZHHHHOS()/2D0 + Yuk1/Yuk2*dZh0HHOSAlter()/2D0 + dZBBOSLeft()/2D0 + &
			& dZBBOSRight()/2D0 )*(1.5D0*EL2*MB2*(-4.D0*MB2 + MHH2)*DBLE(Yuk2**INT(2.D0)))/(MW2*SW2)
	case (14)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMBOSAlter()/MB - dMW2Alter()/(2D0*MW2) + Yuk3*dBetaOS12Alter() + &
			& Yuk1/Yuk2*dAlphaOS12Alter() + dZHHHHOS()/2D0 + Yuk1/Yuk2*dZh0HHOSAlter()/2D0 + dZBBOSLeft()/2D0 + &
			& dZBBOSRight()/2D0 )*(1.5D0*EL2*MB2*(-4.D0*MB2 + MHH2)*DBLE(Yuk2**INT(2.D0)))/(MW2*SW2)
	case (15)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMBOSAlter()/MB - dMW2Alter()/(2D0*MW2) + Yuk3*dBetaBFMSAlter() + &
			& Yuk1/Yuk2*dAlphaPinchOS() + dZHHHHOS()/2D0 + Yuk1/Yuk2*dZh0HHOSAlter()/2D0 + dZBBOSLeft()/2D0 + &
			& dZBBOSRight()/2D0 )*(1.5D0*EL2*MB2*(-4.D0*MB2 + MHH2)*DBLE(Yuk2**INT(2.D0)))/(MW2*SW2)
	case (16)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMBOSUsual()/MB - dMW2Usual()/(2D0*MW2) + Yuk3*dBetaMSBarUsual() + &
			& Yuk1/Yuk2*dAlphaMSBarUsual() + dZHHHHOS()/2D0 + Yuk1/Yuk2*dZh0HHOSUsual()/2D0 + dZBBOSLeft()/2D0 + &
			& dZBBOSRight()/2D0 )*(1.5D0*EL2*MB2*(-4.D0*MB2 + MHH2)*DBLE(Yuk2**INT(2.D0)))/(MW2*SW2)
	case (17)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMBOSAlter()/MB - dMW2Alter()/(2D0*MW2) + Yuk3*dBetaMSBarAlter() + &
			& Yuk1/Yuk2*dAlphaMSBarAlter() + dZHHHHOS()/2D0 + Yuk1/Yuk2*dZh0HHOSAlter()/2D0 + dZBBOSLeft()/2D0 + &
			& dZBBOSRight()/2D0 )*(1.5D0*EL2*MB2*(-4.D0*MB2 + MHH2)*DBLE(Yuk2**INT(2.D0)))/(MW2*SW2)
 end select

 HHtoBBBarCT = totalAmplitude
end function HHtoBBBarCT
