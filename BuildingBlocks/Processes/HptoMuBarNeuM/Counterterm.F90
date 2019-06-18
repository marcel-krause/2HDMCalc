double precision function HptoMuBarNeuMCT(x)
 use constants
 use counterterms
 implicit none
#include "looptools.h"
 integer, intent(in) :: x
 double precision :: totalAmplitude

 select case (x)
	case (1)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSUsual()/MM - dMW2Usual()/(2D0*MW2) + &
			& (1D0 + Yuk6**2)/Yuk6*dBeta1KanUsual() + dZMuMuOSRight()/2D0 + dZNeuMNeuMOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 - 1D0/Yuk6*dZGpHpOSUsual()/2D0 )*&
			& (0.5D0*EL2*(MHp2 - 1.D0*MM2)*MM2*DBLE(Yuk6**INT(2.D0)))/(MW2*SW2)
	case (2)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSUsual()/MM - dMW2Usual()/(2D0*MW2) + &
			& (1D0 + Yuk6**2)/Yuk6*dBeta2KanUsual() + dZMuMuOSRight()/2D0 + dZNeuMNeuMOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 - 1D0/Yuk6*dZGpHpOSUsual()/2D0 )*&
			& (0.5D0*EL2*(MHp2 - 1.D0*MM2)*MM2*DBLE(Yuk6**INT(2.D0)))/(MW2*SW2)
	case (3)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) + &
			& (1D0 + Yuk6**2)/Yuk6*dBeta1KanAlter() + dZMuMuOSRight()/2D0 + dZNeuMNeuMOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 - 1D0/Yuk6*dZGpHpOSAlter()/2D0 )*&
			& (0.5D0*EL2*(MHp2 - 1.D0*MM2)*MM2*DBLE(Yuk6**INT(2.D0)))/(MW2*SW2)
	case (4)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) + &
			& (1D0 + Yuk6**2)/Yuk6*dBeta2KanAlter() + dZMuMuOSRight()/2D0 + dZNeuMNeuMOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 - 1D0/Yuk6*dZGpHpOSAlter()/2D0 )*&
			& (0.5D0*EL2*(MHp2 - 1.D0*MM2)*MM2*DBLE(Yuk6**INT(2.D0)))/(MW2*SW2)
	case (5)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) + &
			& (1D0 + Yuk6**2)/Yuk6*dBeta1PinchPStar() + dZMuMuOSRight()/2D0 + dZNeuMNeuMOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 - 1D0/Yuk6*dZGpHpOSAlter()/2D0 )*&
			& (0.5D0*EL2*(MHp2 - 1.D0*MM2)*MM2*DBLE(Yuk6**INT(2.D0)))/(MW2*SW2)
	case (6)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) + &
			& (1D0 + Yuk6**2)/Yuk6*dBeta2PinchPStar() + dZMuMuOSRight()/2D0 + dZNeuMNeuMOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 - 1D0/Yuk6*dZGpHpOSAlter()/2D0 )*&
			& (0.5D0*EL2*(MHp2 - 1.D0*MM2)*MM2*DBLE(Yuk6**INT(2.D0)))/(MW2*SW2)
	case (7)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) + &
			& (1D0 + Yuk6**2)/Yuk6*dBeta1PinchOS() + dZMuMuOSRight()/2D0 + dZNeuMNeuMOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 - 1D0/Yuk6*dZGpHpOSAlter()/2D0 )*&
			& (0.5D0*EL2*(MHp2 - 1.D0*MM2)*MM2*DBLE(Yuk6**INT(2.D0)))/(MW2*SW2)
	case (8)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) + &
			& (1D0 + Yuk6**2)/Yuk6*dBeta2PinchOS() + dZMuMuOSRight()/2D0 + dZNeuMNeuMOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 - 1D0/Yuk6*dZGpHpOSAlter()/2D0 )*&
			& (0.5D0*EL2*(MHp2 - 1.D0*MM2)*MM2*DBLE(Yuk6**INT(2.D0)))/(MW2*SW2)
	case (9)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) + &
			& (1D0 + Yuk6**2)/Yuk6*dBetaProcDep1Alter() + dZMuMuOSRight()/2D0 + dZNeuMNeuMOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 - 1D0/Yuk6*dZGpHpOSAlter()/2D0 )*&
			& (0.5D0*EL2*(MHp2 - 1.D0*MM2)*MM2*DBLE(Yuk6**INT(2.D0)))/(MW2*SW2)
	case (10)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) + &
			& (1D0 + Yuk6**2)/Yuk6*dBetaProcDep2Alter() + dZMuMuOSRight()/2D0 + dZNeuMNeuMOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 - 1D0/Yuk6*dZGpHpOSAlter()/2D0 )*&
			& (0.5D0*EL2*(MHp2 - 1.D0*MM2)*MM2*DBLE(Yuk6**INT(2.D0)))/(MW2*SW2)
	case (11)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) + &
			& (1D0 + Yuk6**2)/Yuk6*dBetaProcDep3Alter() + dZMuMuOSRight()/2D0 + dZNeuMNeuMOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 - 1D0/Yuk6*dZGpHpOSAlter()/2D0 )*&
			& (0.5D0*EL2*(MHp2 - 1.D0*MM2)*MM2*DBLE(Yuk6**INT(2.D0)))/(MW2*SW2)
	case (12)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) + &
			& (1D0 + Yuk6**2)/Yuk6*dBetaOS1Alter() + dZMuMuOSRight()/2D0 + dZNeuMNeuMOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 - 1D0/Yuk6*dZGpHpOSAlter()/2D0 )*&
			& (0.5D0*EL2*(MHp2 - 1.D0*MM2)*MM2*DBLE(Yuk6**INT(2.D0)))/(MW2*SW2)
	case (13)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) + &
			& (1D0 + Yuk6**2)/Yuk6*dBetaOS2Alter() + dZMuMuOSRight()/2D0 + dZNeuMNeuMOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 - 1D0/Yuk6*dZGpHpOSAlter()/2D0 )*&
			& (0.5D0*EL2*(MHp2 - 1.D0*MM2)*MM2*DBLE(Yuk6**INT(2.D0)))/(MW2*SW2)
	case (14)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) + &
			& (1D0 + Yuk6**2)/Yuk6*dBetaOS12Alter() + dZMuMuOSRight()/2D0 + dZNeuMNeuMOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 - 1D0/Yuk6*dZGpHpOSAlter()/2D0 )*&
			& (0.5D0*EL2*(MHp2 - 1.D0*MM2)*MM2*DBLE(Yuk6**INT(2.D0)))/(MW2*SW2)
	case (15)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) + &
			& (1D0 + Yuk6**2)/Yuk6*dBetaBFMSAlter() + dZMuMuOSRight()/2D0 + dZNeuMNeuMOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 - 1D0/Yuk6*dZGpHpOSAlter()/2D0 )*&
			& (0.5D0*EL2*(MHp2 - 1.D0*MM2)*MM2*DBLE(Yuk6**INT(2.D0)))/(MW2*SW2)
	case (16)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSUsual()/MM - dMW2Usual()/(2D0*MW2) + &
			& (1D0 + Yuk6**2)/Yuk6*dBetaMSBarUsual() + dZMuMuOSRight()/2D0 + dZNeuMNeuMOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 - 1D0/Yuk6*dZGpHpOSUsual()/2D0 )*&
			& (0.5D0*EL2*(MHp2 - 1.D0*MM2)*MM2*DBLE(Yuk6**INT(2.D0)))/(MW2*SW2)
	case (17)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMMOSAlter()/MM - dMW2Alter()/(2D0*MW2) + &
			& (1D0 + Yuk6**2)/Yuk6*dBetaMSBarAlter() + dZMuMuOSRight()/2D0 + dZNeuMNeuMOSLeft()/2D0 + &
			& dZHpHpOS()/2D0 - 1D0/Yuk6*dZGpHpOSAlter()/2D0 )*&
			& (0.5D0*EL2*(MHp2 - 1.D0*MM2)*MM2*DBLE(Yuk6**INT(2.D0)))/(MW2*SW2)
 end select

 HptoMuBarNeuMCT = totalAmplitude
end function HptoMuBarNeuMCT
