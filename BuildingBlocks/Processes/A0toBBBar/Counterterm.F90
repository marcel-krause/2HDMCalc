double precision function A0toBBBarCT(x)
 use constants
 use counterterms
 implicit none
#include "looptools.h"
 integer, intent(in) :: x
 double precision :: totalAmplitude

 select case (x)
	case (1)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMBOSUsual()/MB - dMW2Usual()/(2D0*MW2) + (1+Yuk3**2)/Yuk3*dBeta1KanUsual() + &
			& dZA0A0OS()/2D0 - 1D0/Yuk3*dZG0A0OSUsual()/2D0 + dZBBOSLeft()/2D0 + dZBBOSRight()/2D0 )*&
			& (1.5D0*EL2*MA02*MB2*DBLE(Yuk3**INT(2.D0)))/(MW2*SW2)
	case (2)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMBOSUsual()/MB - dMW2Usual()/(2D0*MW2) + (1+Yuk3**2)/Yuk3*dBeta2KanUsual() + &
			& dZA0A0OS()/2D0 - 1D0/Yuk3*dZG0A0OSUsual()/2D0 + dZBBOSLeft()/2D0 + dZBBOSRight()/2D0 )*&
			& (1.5D0*EL2*MA02*MB2*DBLE(Yuk3**INT(2.D0)))/(MW2*SW2)
	case (3)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMBOSAlter()/MB - dMW2Alter()/(2D0*MW2) + (1+Yuk3**2)/Yuk3*dBeta1KanAlter() + &
			& dZA0A0OS()/2D0 - 1D0/Yuk3*dZG0A0OSAlter()/2D0 + dZBBOSLeft()/2D0 + dZBBOSRight()/2D0 )*&
			& (1.5D0*EL2*MA02*MB2*DBLE(Yuk3**INT(2.D0)))/(MW2*SW2)
	case (4)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMBOSAlter()/MB - dMW2Alter()/(2D0*MW2) + (1+Yuk3**2)/Yuk3*dBeta2KanAlter() + &
			& dZA0A0OS()/2D0 - 1D0/Yuk3*dZG0A0OSAlter()/2D0 + dZBBOSLeft()/2D0 + dZBBOSRight()/2D0 )*&
			& (1.5D0*EL2*MA02*MB2*DBLE(Yuk3**INT(2.D0)))/(MW2*SW2)
	case (5)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMBOSAlter()/MB - dMW2Alter()/(2D0*MW2) + (1+Yuk3**2)/Yuk3*dBeta1PinchPStar() + &
			& dZA0A0OS()/2D0 - 1D0/Yuk3*dZG0A0OSAlter()/2D0 + dZBBOSLeft()/2D0 + dZBBOSRight()/2D0 )*&
			& (1.5D0*EL2*MA02*MB2*DBLE(Yuk3**INT(2.D0)))/(MW2*SW2)
	case (6)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMBOSAlter()/MB - dMW2Alter()/(2D0*MW2) + (1+Yuk3**2)/Yuk3*dBeta2PinchPStar() + &
			& dZA0A0OS()/2D0 - 1D0/Yuk3*dZG0A0OSAlter()/2D0 + dZBBOSLeft()/2D0 + dZBBOSRight()/2D0 )*&
			& (1.5D0*EL2*MA02*MB2*DBLE(Yuk3**INT(2.D0)))/(MW2*SW2)
	case (7)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMBOSAlter()/MB - dMW2Alter()/(2D0*MW2) + (1+Yuk3**2)/Yuk3*dBeta1PinchOS() + &
			& dZA0A0OS()/2D0 - 1D0/Yuk3*dZG0A0OSAlter()/2D0 + dZBBOSLeft()/2D0 + dZBBOSRight()/2D0 )*&
			& (1.5D0*EL2*MA02*MB2*DBLE(Yuk3**INT(2.D0)))/(MW2*SW2)
	case (8)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMBOSAlter()/MB - dMW2Alter()/(2D0*MW2) + (1+Yuk3**2)/Yuk3*dBeta2PinchOS() + &
			& dZA0A0OS()/2D0 - 1D0/Yuk3*dZG0A0OSAlter()/2D0 + dZBBOSLeft()/2D0 + dZBBOSRight()/2D0 )*&
			& (1.5D0*EL2*MA02*MB2*DBLE(Yuk3**INT(2.D0)))/(MW2*SW2)
	case (9)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMBOSAlter()/MB - dMW2Alter()/(2D0*MW2) + (1+Yuk3**2)/Yuk3*dBetaProcDep1Alter() + &
			& dZA0A0OS()/2D0 - 1D0/Yuk3*dZG0A0OSAlter()/2D0 + dZBBOSLeft()/2D0 + dZBBOSRight()/2D0 )*&
			& (1.5D0*EL2*MA02*MB2*DBLE(Yuk3**INT(2.D0)))/(MW2*SW2)
	case (10)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMBOSAlter()/MB - dMW2Alter()/(2D0*MW2) + (1+Yuk3**2)/Yuk3*dBetaProcDep2Alter() + &
			& dZA0A0OS()/2D0 - 1D0/Yuk3*dZG0A0OSAlter()/2D0 + dZBBOSLeft()/2D0 + dZBBOSRight()/2D0 )*&
			& (1.5D0*EL2*MA02*MB2*DBLE(Yuk3**INT(2.D0)))/(MW2*SW2)
	case (11)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMBOSAlter()/MB - dMW2Alter()/(2D0*MW2) + (1+Yuk3**2)/Yuk3*dBetaProcDep3Alter() + &
			& dZA0A0OS()/2D0 - 1D0/Yuk3*dZG0A0OSAlter()/2D0 + dZBBOSLeft()/2D0 + dZBBOSRight()/2D0 )*&
			& (1.5D0*EL2*MA02*MB2*DBLE(Yuk3**INT(2.D0)))/(MW2*SW2)
	case (12)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMBOSAlter()/MB - dMW2Alter()/(2D0*MW2) + (1+Yuk3**2)/Yuk3*dBetaOS1Alter() + &
			& dZA0A0OS()/2D0 - 1D0/Yuk3*dZG0A0OSAlter()/2D0 + dZBBOSLeft()/2D0 + dZBBOSRight()/2D0 )*&
			& (1.5D0*EL2*MA02*MB2*DBLE(Yuk3**INT(2.D0)))/(MW2*SW2)
	case (13)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMBOSAlter()/MB - dMW2Alter()/(2D0*MW2) + (1+Yuk3**2)/Yuk3*dBetaOS2Alter() + &
			& dZA0A0OS()/2D0 - 1D0/Yuk3*dZG0A0OSAlter()/2D0 + dZBBOSLeft()/2D0 + dZBBOSRight()/2D0 )*&
			& (1.5D0*EL2*MA02*MB2*DBLE(Yuk3**INT(2.D0)))/(MW2*SW2)
	case (14)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMBOSAlter()/MB - dMW2Alter()/(2D0*MW2) + (1+Yuk3**2)/Yuk3*dBetaOS12Alter() + &
			& dZA0A0OS()/2D0 - 1D0/Yuk3*dZG0A0OSAlter()/2D0 + dZBBOSLeft()/2D0 + dZBBOSRight()/2D0 )*&
			& (1.5D0*EL2*MA02*MB2*DBLE(Yuk3**INT(2.D0)))/(MW2*SW2)
	case (15)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMBOSAlter()/MB - dMW2Alter()/(2D0*MW2) + (1+Yuk3**2)/Yuk3*dBetaBFMSAlter() + &
			& dZA0A0OS()/2D0 - 1D0/Yuk3*dZG0A0OSAlter()/2D0 + dZBBOSLeft()/2D0 + dZBBOSRight()/2D0 )*&
			& (1.5D0*EL2*MA02*MB2*DBLE(Yuk3**INT(2.D0)))/(MW2*SW2)
	case (16)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMBOSUsual()/MB - dMW2Usual()/(2D0*MW2) + (1+Yuk3**2)/Yuk3*dBetaMSBarUsual() + &
			& dZA0A0OS()/2D0 - 1D0/Yuk3*dZG0A0OSUsual()/2D0 + dZBBOSLeft()/2D0 + dZBBOSRight()/2D0 )*&
			& (1.5D0*EL2*MA02*MB2*DBLE(Yuk3**INT(2.D0)))/(MW2*SW2)
	case (17)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMBOSAlter()/MB - dMW2Alter()/(2D0*MW2) + (1+Yuk3**2)/Yuk3*dBetaMSBarAlter() + &
			& dZA0A0OS()/2D0 - 1D0/Yuk3*dZG0A0OSAlter()/2D0 + dZBBOSLeft()/2D0 + dZBBOSRight()/2D0 )*&
			& (1.5D0*EL2*MA02*MB2*DBLE(Yuk3**INT(2.D0)))/(MW2*SW2)
 end select

 A0toBBBarCT = totalAmplitude
end function A0toBBBarCT
