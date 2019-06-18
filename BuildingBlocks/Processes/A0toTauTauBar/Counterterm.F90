double precision function A0toTauTauBarCT(x)
 use constants
 use counterterms
 implicit none
#include "looptools.h"
 integer, intent(in) :: x
 double precision :: totalAmplitude

 select case (x)
	case (1)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMLOSUsual()/ML - dMW2Usual()/(2D0*MW2) + (1 + Yuk6**2)/Yuk6*dBeta1KanUsual() + &
			& dZA0A0OS()/2D0 - 1D0/Yuk6 * dZG0A0OSUsual()/2D0 + dZTauTauOSLeft()/2D0 + dZTauTauOSRight()/2D0 )*&
			& (0.5D0*EL2*MA02*ML2*DBLE(Yuk6**INT(2.D0)))/(MW2*SW2)
	case (2)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMLOSUsual()/ML - dMW2Usual()/(2D0*MW2) + (1 + Yuk6**2)/Yuk6*dBeta2KanUsual() + &
			& dZA0A0OS()/2D0 - 1D0/Yuk6 * dZG0A0OSUsual()/2D0 + dZTauTauOSLeft()/2D0 + dZTauTauOSRight()/2D0 )*&
			& (0.5D0*EL2*MA02*ML2*DBLE(Yuk6**INT(2.D0)))/(MW2*SW2)
	case (3)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMLOSAlter()/ML - dMW2Alter()/(2D0*MW2) + (1 + Yuk6**2)/Yuk6*dBeta1KanAlter() + &
			& dZA0A0OS()/2D0 - 1D0/Yuk6 * dZG0A0OSAlter()/2D0 + dZTauTauOSLeft()/2D0 + dZTauTauOSRight()/2D0 )*&
			& (0.5D0*EL2*MA02*ML2*DBLE(Yuk6**INT(2.D0)))/(MW2*SW2)
	case (4)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMLOSAlter()/ML - dMW2Alter()/(2D0*MW2) + (1 + Yuk6**2)/Yuk6*dBeta2KanAlter() + &
			& dZA0A0OS()/2D0 - 1D0/Yuk6 * dZG0A0OSAlter()/2D0 + dZTauTauOSLeft()/2D0 + dZTauTauOSRight()/2D0 )*&
			& (0.5D0*EL2*MA02*ML2*DBLE(Yuk6**INT(2.D0)))/(MW2*SW2)
	case (5)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMLOSAlter()/ML - dMW2Alter()/(2D0*MW2) + (1 + Yuk6**2)/Yuk6*dBeta1PinchPStar() + &
			& dZA0A0OS()/2D0 - 1D0/Yuk6 * dZG0A0OSAlter()/2D0 + dZTauTauOSLeft()/2D0 + dZTauTauOSRight()/2D0 )*&
			& (0.5D0*EL2*MA02*ML2*DBLE(Yuk6**INT(2.D0)))/(MW2*SW2)
	case (6)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMLOSAlter()/ML - dMW2Alter()/(2D0*MW2) + (1 + Yuk6**2)/Yuk6*dBeta2PinchPStar() + &
			& dZA0A0OS()/2D0 - 1D0/Yuk6 * dZG0A0OSAlter()/2D0 + dZTauTauOSLeft()/2D0 + dZTauTauOSRight()/2D0 )*&
			& (0.5D0*EL2*MA02*ML2*DBLE(Yuk6**INT(2.D0)))/(MW2*SW2)
	case (7)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMLOSAlter()/ML - dMW2Alter()/(2D0*MW2) + (1 + Yuk6**2)/Yuk6*dBeta1PinchOS() + &
			& dZA0A0OS()/2D0 - 1D0/Yuk6 * dZG0A0OSAlter()/2D0 + dZTauTauOSLeft()/2D0 + dZTauTauOSRight()/2D0 )*&
			& (0.5D0*EL2*MA02*ML2*DBLE(Yuk6**INT(2.D0)))/(MW2*SW2)
	case (8)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMLOSAlter()/ML - dMW2Alter()/(2D0*MW2) + (1 + Yuk6**2)/Yuk6*dBeta2PinchOS() + &
			& dZA0A0OS()/2D0 - 1D0/Yuk6 * dZG0A0OSAlter()/2D0 + dZTauTauOSLeft()/2D0 + dZTauTauOSRight()/2D0 )*&
			& (0.5D0*EL2*MA02*ML2*DBLE(Yuk6**INT(2.D0)))/(MW2*SW2)
	case (9)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMLOSAlter()/ML - dMW2Alter()/(2D0*MW2) + (1+Yuk6**2)/Yuk6*dBetaProcDep1Alter() + &
			& dZA0A0OS()/2D0 - 1D0/Yuk6 * dZG0A0OSAlter()/2D0 + dZTauTauOSLeft()/2D0 + dZTauTauOSRight()/2D0 )*&
			& (0.5D0*EL2*MA02*ML2*DBLE(Yuk6**INT(2.D0)))/(MW2*SW2)
	case (10)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMLOSAlter()/ML - dMW2Alter()/(2D0*MW2) + (1+Yuk6**2)/Yuk6*dBetaProcDep2Alter() + &
			& dZA0A0OS()/2D0 - 1D0/Yuk6 * dZG0A0OSAlter()/2D0 + dZTauTauOSLeft()/2D0 + dZTauTauOSRight()/2D0 )*&
			& (0.5D0*EL2*MA02*ML2*DBLE(Yuk6**INT(2.D0)))/(MW2*SW2)
	case (11)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMLOSAlter()/ML - dMW2Alter()/(2D0*MW2) + (1+Yuk6**2)/Yuk6*dBetaProcDep3Alter() + &
			& dZA0A0OS()/2D0 - 1D0/Yuk6 * dZG0A0OSAlter()/2D0 + dZTauTauOSLeft()/2D0 + dZTauTauOSRight()/2D0 )*&
			& (0.5D0*EL2*MA02*ML2*DBLE(Yuk6**INT(2.D0)))/(MW2*SW2)	
	case (12)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMLOSAlter()/ML - dMW2Alter()/(2D0*MW2) + (1+Yuk6**2)/Yuk6*dBetaOS1Alter() + &
			& dZA0A0OS()/2D0 - 1D0/Yuk6 * dZG0A0OSAlter()/2D0 + dZTauTauOSLeft()/2D0 + dZTauTauOSRight()/2D0 )*&
			& (0.5D0*EL2*MA02*ML2*DBLE(Yuk6**INT(2.D0)))/(MW2*SW2)
	case (13)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMLOSAlter()/ML - dMW2Alter()/(2D0*MW2) + (1+Yuk6**2)/Yuk6*dBetaOS2Alter() + &
			& dZA0A0OS()/2D0 - 1D0/Yuk6 * dZG0A0OSAlter()/2D0 + dZTauTauOSLeft()/2D0 + dZTauTauOSRight()/2D0 )*&
			& (0.5D0*EL2*MA02*ML2*DBLE(Yuk6**INT(2.D0)))/(MW2*SW2)
	case (14)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMLOSAlter()/ML - dMW2Alter()/(2D0*MW2) + (1+Yuk6**2)/Yuk6*dBetaOS12Alter() + &
			& dZA0A0OS()/2D0 - 1D0/Yuk6 * dZG0A0OSAlter()/2D0 + dZTauTauOSLeft()/2D0 + dZTauTauOSRight()/2D0 )*&
			& (0.5D0*EL2*MA02*ML2*DBLE(Yuk6**INT(2.D0)))/(MW2*SW2)
	case (15)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMLOSAlter()/ML - dMW2Alter()/(2D0*MW2) + (1+Yuk6**2)/Yuk6*dBetaBFMSAlter() + &
			& dZA0A0OS()/2D0 - 1D0/Yuk6 * dZG0A0OSAlter()/2D0 + dZTauTauOSLeft()/2D0 + dZTauTauOSRight()/2D0 )*&
			& (0.5D0*EL2*MA02*ML2*DBLE(Yuk6**INT(2.D0)))/(MW2*SW2)
	case (16)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMLOSUsual()/ML - dMW2Usual()/(2D0*MW2) + (1+Yuk6**2)/Yuk6*dBetaMSBarUsual() + &
			& dZA0A0OS()/2D0 - 1D0/Yuk6 * dZG0A0OSUsual()/2D0 + dZTauTauOSLeft()/2D0 + dZTauTauOSRight()/2D0 )*&
			& (0.5D0*EL2*MA02*ML2*DBLE(Yuk6**INT(2.D0)))/(MW2*SW2)
	case (17)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMLOSAlter()/ML - dMW2Alter()/(2D0*MW2) + (1+Yuk6**2)/Yuk6*dBetaMSBarAlter() + &
			& dZA0A0OS()/2D0 - 1D0/Yuk6 * dZG0A0OSAlter()/2D0 + dZTauTauOSLeft()/2D0 + dZTauTauOSRight()/2D0 )*&
			& (0.5D0*EL2*MA02*ML2*DBLE(Yuk6**INT(2.D0)))/(MW2*SW2)
 end select

 A0toTauTauBarCT = totalAmplitude
end function A0toTauTauBarCT
