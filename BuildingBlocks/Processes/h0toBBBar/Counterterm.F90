double precision function h0toBBBarCT(x)
 use constants
 use counterterms
 implicit none
#include "looptools.h"
 integer, intent(in) :: x
 double precision :: totalAmplitude

 select case (x)
	case (1)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMBOSUsual()/MB - dMW2Usual()/(2D0*MW2) + Yuk3*dBeta1KanUsual() - &
			& Yuk2/Yuk1*dAlphaKanUsual() + dZh0h0OS()/2D0 + Yuk2/Yuk1*dZHHh0OSUsual()/2D0 + dZBBOSLeft()/2D0 + &
			& dZBBOSRight()/2D0 )*(1.5D0*EL2*MB2*(-4.D0*MB2 + Mh02)*DBLE(Yuk1**INT(2.D0)))/(MW2*SW2)
	case (2)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMBOSUsual()/MB - dMW2Usual()/(2D0*MW2) + Yuk3*dBeta2KanUsual() - &
			& Yuk2/Yuk1*dAlphaKanUsual() + dZh0h0OS()/2D0 + Yuk2/Yuk1*dZHHh0OSUsual()/2D0 + dZBBOSLeft()/2D0 + &
			& dZBBOSRight()/2D0 )*(1.5D0*EL2*MB2*(-4.D0*MB2 + Mh02)*DBLE(Yuk1**INT(2.D0)))/(MW2*SW2)
	case (3)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMBOSAlter()/MB - dMW2Alter()/(2D0*MW2) + Yuk3*dBeta1KanAlter() - &
			& Yuk2/Yuk1*dAlphaKanAlter() + dZh0h0OS()/2D0 + Yuk2/Yuk1*dZHHh0OSAlter()/2D0 + dZBBOSLeft()/2D0 + &
			& dZBBOSRight()/2D0 )*(1.5D0*EL2*MB2*(-4.D0*MB2 + Mh02)*DBLE(Yuk1**INT(2.D0)))/(MW2*SW2)	
	case (4)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMBOSAlter()/MB - dMW2Alter()/(2D0*MW2) + Yuk3*dBeta2KanAlter() - &
			& Yuk2/Yuk1*dAlphaKanAlter() + dZh0h0OS()/2D0 + Yuk2/Yuk1*dZHHh0OSAlter()/2D0 + dZBBOSLeft()/2D0 + &
			& dZBBOSRight()/2D0 )*(1.5D0*EL2*MB2*(-4.D0*MB2 + Mh02)*DBLE(Yuk1**INT(2.D0)))/(MW2*SW2)
	case (5)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMBOSAlter()/MB - dMW2Alter()/(2D0*MW2) + Yuk3*dBeta1PinchPStar() - &
			& Yuk2/Yuk1*dAlphaPinchPStar() + dZh0h0OS()/2D0 + Yuk2/Yuk1*dZHHh0OSAlter()/2D0 + dZBBOSLeft()/2D0 + &
			& dZBBOSRight()/2D0 )*(1.5D0*EL2*MB2*(-4.D0*MB2 + Mh02)*DBLE(Yuk1**INT(2.D0)))/(MW2*SW2)
	case (6)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMBOSAlter()/MB - dMW2Alter()/(2D0*MW2) + Yuk3*dBeta2PinchPStar() - &
			& Yuk2/Yuk1*dAlphaPinchPStar() + dZh0h0OS()/2D0 + Yuk2/Yuk1*dZHHh0OSAlter()/2D0 + dZBBOSLeft()/2D0 + &
			& dZBBOSRight()/2D0 )*(1.5D0*EL2*MB2*(-4.D0*MB2 + Mh02)*DBLE(Yuk1**INT(2.D0)))/(MW2*SW2)
	case (7)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMBOSAlter()/MB - dMW2Alter()/(2D0*MW2) + Yuk3*dBeta1PinchOS() - &
			& Yuk2/Yuk1*dAlphaPinchOS() + dZh0h0OS()/2D0 + Yuk2/Yuk1*dZHHh0OSAlter()/2D0 + dZBBOSLeft()/2D0 + &
			& dZBBOSRight()/2D0 )*(1.5D0*EL2*MB2*(-4.D0*MB2 + Mh02)*DBLE(Yuk1**INT(2.D0)))/(MW2*SW2)
	case (8)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMBOSAlter()/MB - dMW2Alter()/(2D0*MW2) + Yuk3*dBeta2PinchOS() - &
			& Yuk2/Yuk1*dAlphaPinchOS() + dZh0h0OS()/2D0 + Yuk2/Yuk1*dZHHh0OSAlter()/2D0 + dZBBOSLeft()/2D0 + &
			& dZBBOSRight()/2D0 )*(1.5D0*EL2*MB2*(-4.D0*MB2 + Mh02)*DBLE(Yuk1**INT(2.D0)))/(MW2*SW2)
	case (9)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMBOSAlter()/MB - dMW2Alter()/(2D0*MW2) + Yuk3*dBetaProcDep1Alter() - &
			& Yuk2/Yuk1*dAlphaProcDep1Alter() + dZh0h0OS()/2D0 + Yuk2/Yuk1*dZHHh0OSAlter()/2D0 + dZBBOSLeft()/2D0 + &
			& dZBBOSRight()/2D0 )*(1.5D0*EL2*MB2*(-4.D0*MB2 + Mh02)*DBLE(Yuk1**INT(2.D0)))/(MW2*SW2)
	case (10)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMBOSAlter()/MB - dMW2Alter()/(2D0*MW2) + Yuk3*dBetaProcDep2Alter() - &
			& Yuk2/Yuk1*dAlphaProcDep2Alter() + dZh0h0OS()/2D0 + Yuk2/Yuk1*dZHHh0OSAlter()/2D0 + dZBBOSLeft()/2D0 + &
			& dZBBOSRight()/2D0 )*(1.5D0*EL2*MB2*(-4.D0*MB2 + Mh02)*DBLE(Yuk1**INT(2.D0)))/(MW2*SW2)
	case (11)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMBOSAlter()/MB - dMW2Alter()/(2D0*MW2) + Yuk3*dBetaProcDep3Alter() - &
			& Yuk2/Yuk1*dAlphaProcDep3Alter() + dZh0h0OS()/2D0 + Yuk2/Yuk1*dZHHh0OSAlter()/2D0 + dZBBOSLeft()/2D0 + &
			& dZBBOSRight()/2D0 )*(1.5D0*EL2*MB2*(-4.D0*MB2 + Mh02)*DBLE(Yuk1**INT(2.D0)))/(MW2*SW2)
	case (12)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMBOSAlter()/MB - dMW2Alter()/(2D0*MW2) + Yuk3*dBetaOS1Alter() - &
			& Yuk2/Yuk1*dAlphaOS1Alter() + dZh0h0OS()/2D0 + Yuk2/Yuk1*dZHHh0OSAlter()/2D0 + dZBBOSLeft()/2D0 + &
			& dZBBOSRight()/2D0 )*(1.5D0*EL2*MB2*(-4.D0*MB2 + Mh02)*DBLE(Yuk1**INT(2.D0)))/(MW2*SW2)
	case (13)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMBOSAlter()/MB - dMW2Alter()/(2D0*MW2) + Yuk3*dBetaOS2Alter() - &
			& Yuk2/Yuk1*dAlphaOS2Alter() + dZh0h0OS()/2D0 + Yuk2/Yuk1*dZHHh0OSAlter()/2D0 + dZBBOSLeft()/2D0 + &
			& dZBBOSRight()/2D0 )*(1.5D0*EL2*MB2*(-4.D0*MB2 + Mh02)*DBLE(Yuk1**INT(2.D0)))/(MW2*SW2)
	case (14)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMBOSAlter()/MB - dMW2Alter()/(2D0*MW2) + Yuk3*dBetaOS12Alter() - &
			& Yuk2/Yuk1*dAlphaOS12Alter() + dZh0h0OS()/2D0 + Yuk2/Yuk1*dZHHh0OSAlter()/2D0 + dZBBOSLeft()/2D0 + &
			& dZBBOSRight()/2D0 )*(1.5D0*EL2*MB2*(-4.D0*MB2 + Mh02)*DBLE(Yuk1**INT(2.D0)))/(MW2*SW2)
	case (15)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMBOSAlter()/MB - dMW2Alter()/(2D0*MW2) + Yuk3*dBetaBFMSAlter() - &
			& Yuk2/Yuk1*dAlphaPinchOS() + dZh0h0OS()/2D0 + Yuk2/Yuk1*dZHHh0OSAlter()/2D0 + dZBBOSLeft()/2D0 + &
			& dZBBOSRight()/2D0 )*(1.5D0*EL2*MB2*(-4.D0*MB2 + Mh02)*DBLE(Yuk1**INT(2.D0)))/(MW2*SW2)
	case (16)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMBOSUsual()/MB - dMW2Usual()/(2D0*MW2) + Yuk3*dBetaMSBarUsual() - &
			& Yuk2/Yuk1*dAlphaMSBarUsual() + dZh0h0OS()/2D0 + Yuk2/Yuk1*dZHHh0OSUsual()/2D0 + dZBBOSLeft()/2D0 + &
			& dZBBOSRight()/2D0 )*(1.5D0*EL2*MB2*(-4.D0*MB2 + Mh02)*DBLE(Yuk1**INT(2.D0)))/(MW2*SW2)
	case (17)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMBOSAlter()/MB - dMW2Alter()/(2D0*MW2) + Yuk3*dBetaMSBarAlter() - &
			& Yuk2/Yuk1*dAlphaMSBarAlter() + dZh0h0OS()/2D0 + Yuk2/Yuk1*dZHHh0OSAlter()/2D0 + dZBBOSLeft()/2D0 + &
			& dZBBOSRight()/2D0 )*(1.5D0*EL2*MB2*(-4.D0*MB2 + Mh02)*DBLE(Yuk1**INT(2.D0)))/(MW2*SW2)
 end select

 h0toBBBarCT = totalAmplitude
end function h0toBBBarCT
