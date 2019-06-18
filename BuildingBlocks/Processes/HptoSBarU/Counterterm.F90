double precision function HptoSBarUCT(x)
 use constants
 use counterterms
 implicit none
#include "looptools.h"
 integer, intent(in) :: x
 double precision :: totalAmplitude
 double precision :: dcLeft, dcRight

 select case (x)
	case (1)
		dcLeft = EL*CKM12/(DSQRT(2D0)*MW*SW)*MU/TB*( &
			& dZHpHpOS()/2D0 + TB*dZGpHpOSUsual()/2D0 + &
			& (CKM12/CKM12*MU/MU*dZUUOSRight() + CKM11/CKM12*dZDSOSLeft())/2D0 + &
			& (CKM22/CKM12*MC/MU*dZCUOSRight() + CKM12/CKM12*dZSSOSLeft())/2D0 + &
			& (CKM32/CKM12*MT/MU*dZTUOSRight() + CKM13/CKM12*dZBSOSLeft())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMUOSUsual()/MU + dCKM12Yamada()/CKM12 - dMW2Usual()/(2D0*MW2) - &
			& dBeta1KanUsual()/(SB*CB) )
		dcRight = EL*CKM12/(DSQRT(2D0)*MW*SW)*MS*Yuk3*( &
			& dZHpHpOS()/2D0 - dZGpHpOSUsual()/(2D0*Yuk3) + &
			& (CKM12/CKM12*dZUUOSLeft() + CKM11/CKM12*MD/MS*dZDSOSRight())/2D0 + &
			& (CKM22/CKM12*dZCUOSLeft() + CKM12/CKM12*MS/MS*dZSSOSRight())/2D0 + &
			& (CKM32/CKM12*dZTUOSLeft() + CKM13/CKM12*MB/MS*dZBSOSRight())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMSOSUsual()/MS + dCKM12Yamada()/CKM12 - dMW2Usual()/(2D0*MW2) + &
			& (1D0 + Yuk3**2)/Yuk3*dBeta1KanUsual() )

		totalAmplitude = 3D0*(MHp2 - (MU + MS)**2)*(MU/TB + MS*Yuk3)*EL*CKM12/(DSQRT(2D0)*MW*SW)*(dcLeft + dcRight)/2D0 + &
					   & 3D0*(MHp2 - (MU - MS)**2)*(MU/TB - MS*Yuk3)*EL*CKM12/(DSQRT(2D0)*MW*SW)*(dcLeft - dcRight)/2D0
	case (2)
		dcLeft = EL*CKM12/(DSQRT(2D0)*MW*SW)*MU/TB*( &
			& dZHpHpOS()/2D0 + TB*dZGpHpOSUsual()/2D0 + &
			& (CKM12/CKM12*MU/MU*dZUUOSRight() + CKM11/CKM12*dZDSOSLeft())/2D0 + &
			& (CKM22/CKM12*MC/MU*dZCUOSRight() + CKM12/CKM12*dZSSOSLeft())/2D0 + &
			& (CKM32/CKM12*MT/MU*dZTUOSRight() + CKM13/CKM12*dZBSOSLeft())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMUOSUsual()/MU + dCKM12Yamada()/CKM12 - dMW2Usual()/(2D0*MW2) - &
			& dBeta2KanUsual()/(SB*CB) )
		dcRight = EL*CKM12/(DSQRT(2D0)*MW*SW)*MS*Yuk3*( &
			& dZHpHpOS()/2D0 - dZGpHpOSUsual()/(2D0*Yuk3) + &
			& (CKM12/CKM12*dZUUOSLeft() + CKM11/CKM12*MD/MS*dZDSOSRight())/2D0 + &
			& (CKM22/CKM12*dZCUOSLeft() + CKM12/CKM12*MS/MS*dZSSOSRight())/2D0 + &
			& (CKM32/CKM12*dZTUOSLeft() + CKM13/CKM12*MB/MS*dZBSOSRight())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMSOSUsual()/MS + dCKM12Yamada()/CKM12 - dMW2Usual()/(2D0*MW2) + &
			& (1D0 + Yuk3**2)/Yuk3*dBeta2KanUsual() )

		totalAmplitude = 3D0*(MHp2 - (MU + MS)**2)*(MU/TB + MS*Yuk3)*EL*CKM12/(DSQRT(2D0)*MW*SW)*(dcLeft + dcRight)/2D0 + &
					   & 3D0*(MHp2 - (MU - MS)**2)*(MU/TB - MS*Yuk3)*EL*CKM12/(DSQRT(2D0)*MW*SW)*(dcLeft - dcRight)/2D0
	case (3)
		dcLeft = EL*CKM12/(DSQRT(2D0)*MW*SW)*MU/TB*( &
			& dZHpHpOS()/2D0 + TB*dZGpHpOSAlter()/2D0 + &
			& (CKM12/CKM12*MU/MU*dZUUOSRight() + CKM11/CKM12*dZDSOSLeft())/2D0 + &
			& (CKM22/CKM12*MC/MU*dZCUOSRight() + CKM12/CKM12*dZSSOSLeft())/2D0 + &
			& (CKM32/CKM12*MT/MU*dZTUOSRight() + CKM13/CKM12*dZBSOSLeft())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMUOSAlter()/MU + dCKM12Yamada()/CKM12 - dMW2Alter()/(2D0*MW2) - &
			& dBeta1KanAlter()/(SB*CB) )
		dcRight = EL*CKM12/(DSQRT(2D0)*MW*SW)*MS*Yuk3*( &
			& dZHpHpOS()/2D0 - dZGpHpOSAlter()/(2D0*Yuk3) + &
			& (CKM12/CKM12*dZUUOSLeft() + CKM11/CKM12*MD/MS*dZDSOSRight())/2D0 + &
			& (CKM22/CKM12*dZCUOSLeft() + CKM12/CKM12*MS/MS*dZSSOSRight())/2D0 + &
			& (CKM32/CKM12*dZTUOSLeft() + CKM13/CKM12*MB/MS*dZBSOSRight())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMSOSAlter()/MS + dCKM12Yamada()/CKM12 - dMW2Alter()/(2D0*MW2) + &
			& (1D0 + Yuk3**2)/Yuk3*dBeta1KanAlter() )

		totalAmplitude = 3D0*(MHp2 - (MU + MS)**2)*(MU/TB + MS*Yuk3)*EL*CKM12/(DSQRT(2D0)*MW*SW)*(dcLeft + dcRight)/2D0 + &
					   & 3D0*(MHp2 - (MU - MS)**2)*(MU/TB - MS*Yuk3)*EL*CKM12/(DSQRT(2D0)*MW*SW)*(dcLeft - dcRight)/2D0
	case (4)
		dcLeft = EL*CKM12/(DSQRT(2D0)*MW*SW)*MU/TB*( &
			& dZHpHpOS()/2D0 + TB*dZGpHpOSAlter()/2D0 + &
			& (CKM12/CKM12*MU/MU*dZUUOSRight() + CKM11/CKM12*dZDSOSLeft())/2D0 + &
			& (CKM22/CKM12*MC/MU*dZCUOSRight() + CKM12/CKM12*dZSSOSLeft())/2D0 + &
			& (CKM32/CKM12*MT/MU*dZTUOSRight() + CKM13/CKM12*dZBSOSLeft())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMUOSAlter()/MU + dCKM12Yamada()/CKM12 - dMW2Alter()/(2D0*MW2) - &
			& dBeta2KanAlter()/(SB*CB) )
		dcRight = EL*CKM12/(DSQRT(2D0)*MW*SW)*MS*Yuk3*( &
			& dZHpHpOS()/2D0 - dZGpHpOSAlter()/(2D0*Yuk3) + &
			& (CKM12/CKM12*dZUUOSLeft() + CKM11/CKM12*MD/MS*dZDSOSRight())/2D0 + &
			& (CKM22/CKM12*dZCUOSLeft() + CKM12/CKM12*MS/MS*dZSSOSRight())/2D0 + &
			& (CKM32/CKM12*dZTUOSLeft() + CKM13/CKM12*MB/MS*dZBSOSRight())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMSOSAlter()/MS + dCKM12Yamada()/CKM12 - dMW2Alter()/(2D0*MW2) + &
			& (1D0 + Yuk3**2)/Yuk3*dBeta2KanAlter() )

		totalAmplitude = 3D0*(MHp2 - (MU + MS)**2)*(MU/TB + MS*Yuk3)*EL*CKM12/(DSQRT(2D0)*MW*SW)*(dcLeft + dcRight)/2D0 + &
					   & 3D0*(MHp2 - (MU - MS)**2)*(MU/TB - MS*Yuk3)*EL*CKM12/(DSQRT(2D0)*MW*SW)*(dcLeft - dcRight)/2D0
	case (5)
		dcLeft = EL*CKM12/(DSQRT(2D0)*MW*SW)*MU/TB*( &
			& dZHpHpOS()/2D0 + TB*dZGpHpOSAlter()/2D0 + &
			& (CKM12/CKM12*MU/MU*dZUUOSRight() + CKM11/CKM12*dZDSOSLeft())/2D0 + &
			& (CKM22/CKM12*MC/MU*dZCUOSRight() + CKM12/CKM12*dZSSOSLeft())/2D0 + &
			& (CKM32/CKM12*MT/MU*dZTUOSRight() + CKM13/CKM12*dZBSOSLeft())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMUOSAlter()/MU + dCKM12Yamada()/CKM12 - dMW2Alter()/(2D0*MW2) - &
			& dBeta1PinchPStar()/(SB*CB) )
		dcRight = EL*CKM12/(DSQRT(2D0)*MW*SW)*MS*Yuk3*( &
			& dZHpHpOS()/2D0 - dZGpHpOSAlter()/(2D0*Yuk3) + &
			& (CKM12/CKM12*dZUUOSLeft() + CKM11/CKM12*MD/MS*dZDSOSRight())/2D0 + &
			& (CKM22/CKM12*dZCUOSLeft() + CKM12/CKM12*MS/MS*dZSSOSRight())/2D0 + &
			& (CKM32/CKM12*dZTUOSLeft() + CKM13/CKM12*MB/MS*dZBSOSRight())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMSOSAlter()/MS + dCKM12Yamada()/CKM12 - dMW2Alter()/(2D0*MW2) + &
			& (1D0 + Yuk3**2)/Yuk3*dBeta1PinchPStar() )

		totalAmplitude = 3D0*(MHp2 - (MU + MS)**2)*(MU/TB + MS*Yuk3)*EL*CKM12/(DSQRT(2D0)*MW*SW)*(dcLeft + dcRight)/2D0 + &
					   & 3D0*(MHp2 - (MU - MS)**2)*(MU/TB - MS*Yuk3)*EL*CKM12/(DSQRT(2D0)*MW*SW)*(dcLeft - dcRight)/2D0
	case (6)
		dcLeft = EL*CKM12/(DSQRT(2D0)*MW*SW)*MU/TB*( &
			& dZHpHpOS()/2D0 + TB*dZGpHpOSAlter()/2D0 + &
			& (CKM12/CKM12*MU/MU*dZUUOSRight() + CKM11/CKM12*dZDSOSLeft())/2D0 + &
			& (CKM22/CKM12*MC/MU*dZCUOSRight() + CKM12/CKM12*dZSSOSLeft())/2D0 + &
			& (CKM32/CKM12*MT/MU*dZTUOSRight() + CKM13/CKM12*dZBSOSLeft())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMUOSAlter()/MU + dCKM12Yamada()/CKM12 - dMW2Alter()/(2D0*MW2) - &
			& dBeta2PinchPStar()/(SB*CB) )
		dcRight = EL*CKM12/(DSQRT(2D0)*MW*SW)*MS*Yuk3*( &
			& dZHpHpOS()/2D0 - dZGpHpOSAlter()/(2D0*Yuk3) + &
			& (CKM12/CKM12*dZUUOSLeft() + CKM11/CKM12*MD/MS*dZDSOSRight())/2D0 + &
			& (CKM22/CKM12*dZCUOSLeft() + CKM12/CKM12*MS/MS*dZSSOSRight())/2D0 + &
			& (CKM32/CKM12*dZTUOSLeft() + CKM13/CKM12*MB/MS*dZBSOSRight())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMSOSAlter()/MS + dCKM12Yamada()/CKM12 - dMW2Alter()/(2D0*MW2) + &
			& (1D0 + Yuk3**2)/Yuk3*dBeta2PinchPStar() )

		totalAmplitude = 3D0*(MHp2 - (MU + MS)**2)*(MU/TB + MS*Yuk3)*EL*CKM12/(DSQRT(2D0)*MW*SW)*(dcLeft + dcRight)/2D0 + &
					   & 3D0*(MHp2 - (MU - MS)**2)*(MU/TB - MS*Yuk3)*EL*CKM12/(DSQRT(2D0)*MW*SW)*(dcLeft - dcRight)/2D0
	case (7)
		dcLeft = EL*CKM12/(DSQRT(2D0)*MW*SW)*MU/TB*( &
			& dZHpHpOS()/2D0 + TB*dZGpHpOSAlter()/2D0 + &
			& (CKM12/CKM12*MU/MU*dZUUOSRight() + CKM11/CKM12*dZDSOSLeft())/2D0 + &
			& (CKM22/CKM12*MC/MU*dZCUOSRight() + CKM12/CKM12*dZSSOSLeft())/2D0 + &
			& (CKM32/CKM12*MT/MU*dZTUOSRight() + CKM13/CKM12*dZBSOSLeft())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMUOSAlter()/MU + dCKM12Yamada()/CKM12 - dMW2Alter()/(2D0*MW2) - &
			& dBeta1PinchOS()/(SB*CB) )
		dcRight = EL*CKM12/(DSQRT(2D0)*MW*SW)*MS*Yuk3*( &
			& dZHpHpOS()/2D0 - dZGpHpOSAlter()/(2D0*Yuk3) + &
			& (CKM12/CKM12*dZUUOSLeft() + CKM11/CKM12*MD/MS*dZDSOSRight())/2D0 + &
			& (CKM22/CKM12*dZCUOSLeft() + CKM12/CKM12*MS/MS*dZSSOSRight())/2D0 + &
			& (CKM32/CKM12*dZTUOSLeft() + CKM13/CKM12*MB/MS*dZBSOSRight())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMSOSAlter()/MS + dCKM12Yamada()/CKM12 - dMW2Alter()/(2D0*MW2) + &
			& (1D0 + Yuk3**2)/Yuk3*dBeta1PinchOS() )

		totalAmplitude = 3D0*(MHp2 - (MU + MS)**2)*(MU/TB + MS*Yuk3)*EL*CKM12/(DSQRT(2D0)*MW*SW)*(dcLeft + dcRight)/2D0 + &
					   & 3D0*(MHp2 - (MU - MS)**2)*(MU/TB - MS*Yuk3)*EL*CKM12/(DSQRT(2D0)*MW*SW)*(dcLeft - dcRight)/2D0
	case (8)
		dcLeft = EL*CKM12/(DSQRT(2D0)*MW*SW)*MU/TB*( &
			& dZHpHpOS()/2D0 + TB*dZGpHpOSAlter()/2D0 + &
			& (CKM12/CKM12*MU/MU*dZUUOSRight() + CKM11/CKM12*dZDSOSLeft())/2D0 + &
			& (CKM22/CKM12*MC/MU*dZCUOSRight() + CKM12/CKM12*dZSSOSLeft())/2D0 + &
			& (CKM32/CKM12*MT/MU*dZTUOSRight() + CKM13/CKM12*dZBSOSLeft())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMUOSAlter()/MU + dCKM12Yamada()/CKM12 - dMW2Alter()/(2D0*MW2) - &
			& dBeta2PinchOS()/(SB*CB) )
		dcRight = EL*CKM12/(DSQRT(2D0)*MW*SW)*MS*Yuk3*( &
			& dZHpHpOS()/2D0 - dZGpHpOSAlter()/(2D0*Yuk3) + &
			& (CKM12/CKM12*dZUUOSLeft() + CKM11/CKM12*MD/MS*dZDSOSRight())/2D0 + &
			& (CKM22/CKM12*dZCUOSLeft() + CKM12/CKM12*MS/MS*dZSSOSRight())/2D0 + &
			& (CKM32/CKM12*dZTUOSLeft() + CKM13/CKM12*MB/MS*dZBSOSRight())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMSOSAlter()/MS + dCKM12Yamada()/CKM12 - dMW2Alter()/(2D0*MW2) + &
			& (1D0 + Yuk3**2)/Yuk3*dBeta2PinchOS() )

		totalAmplitude = 3D0*(MHp2 - (MU + MS)**2)*(MU/TB + MS*Yuk3)*EL*CKM12/(DSQRT(2D0)*MW*SW)*(dcLeft + dcRight)/2D0 + &
					   & 3D0*(MHp2 - (MU - MS)**2)*(MU/TB - MS*Yuk3)*EL*CKM12/(DSQRT(2D0)*MW*SW)*(dcLeft - dcRight)/2D0
	case (9)
		dcLeft = EL*CKM12/(DSQRT(2D0)*MW*SW)*MU/TB*( &
			& dZHpHpOS()/2D0 + TB*dZGpHpOSAlter()/2D0 + &
			& (CKM12/CKM12*MU/MU*dZUUOSRight() + CKM11/CKM12*dZDSOSLeft())/2D0 + &
			& (CKM22/CKM12*MC/MU*dZCUOSRight() + CKM12/CKM12*dZSSOSLeft())/2D0 + &
			& (CKM32/CKM12*MT/MU*dZTUOSRight() + CKM13/CKM12*dZBSOSLeft())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMUOSAlter()/MU + dCKM12Yamada()/CKM12 - dMW2Alter()/(2D0*MW2) - &
			& dBetaProcDep1Alter()/(SB*CB) )
		dcRight = EL*CKM12/(DSQRT(2D0)*MW*SW)*MS*Yuk3*( &
			& dZHpHpOS()/2D0 - dZGpHpOSAlter()/(2D0*Yuk3) + &
			& (CKM12/CKM12*dZUUOSLeft() + CKM11/CKM12*MD/MS*dZDSOSRight())/2D0 + &
			& (CKM22/CKM12*dZCUOSLeft() + CKM12/CKM12*MS/MS*dZSSOSRight())/2D0 + &
			& (CKM32/CKM12*dZTUOSLeft() + CKM13/CKM12*MB/MS*dZBSOSRight())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMSOSAlter()/MS + dCKM12Yamada()/CKM12 - dMW2Alter()/(2D0*MW2) + &
			& (1D0 + Yuk3**2)/Yuk3*dBetaProcDep1Alter() )

		totalAmplitude = 3D0*(MHp2 - (MU + MS)**2)*(MU/TB + MS*Yuk3)*EL*CKM12/(DSQRT(2D0)*MW*SW)*(dcLeft + dcRight)/2D0 + &
					   & 3D0*(MHp2 - (MU - MS)**2)*(MU/TB - MS*Yuk3)*EL*CKM12/(DSQRT(2D0)*MW*SW)*(dcLeft - dcRight)/2D0
	case (10)
		dcLeft = EL*CKM12/(DSQRT(2D0)*MW*SW)*MU/TB*( &
			& dZHpHpOS()/2D0 + TB*dZGpHpOSAlter()/2D0 + &
			& (CKM12/CKM12*MU/MU*dZUUOSRight() + CKM11/CKM12*dZDSOSLeft())/2D0 + &
			& (CKM22/CKM12*MC/MU*dZCUOSRight() + CKM12/CKM12*dZSSOSLeft())/2D0 + &
			& (CKM32/CKM12*MT/MU*dZTUOSRight() + CKM13/CKM12*dZBSOSLeft())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMUOSAlter()/MU + dCKM12Yamada()/CKM12 - dMW2Alter()/(2D0*MW2) - &
			& dBetaProcDep2Alter()/(SB*CB) )
		dcRight = EL*CKM12/(DSQRT(2D0)*MW*SW)*MS*Yuk3*( &
			& dZHpHpOS()/2D0 - dZGpHpOSAlter()/(2D0*Yuk3) + &
			& (CKM12/CKM12*dZUUOSLeft() + CKM11/CKM12*MD/MS*dZDSOSRight())/2D0 + &
			& (CKM22/CKM12*dZCUOSLeft() + CKM12/CKM12*MS/MS*dZSSOSRight())/2D0 + &
			& (CKM32/CKM12*dZTUOSLeft() + CKM13/CKM12*MB/MS*dZBSOSRight())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMSOSAlter()/MS + dCKM12Yamada()/CKM12 - dMW2Alter()/(2D0*MW2) + &
			& (1D0 + Yuk3**2)/Yuk3*dBetaProcDep2Alter() )

		totalAmplitude = 3D0*(MHp2 - (MU + MS)**2)*(MU/TB + MS*Yuk3)*EL*CKM12/(DSQRT(2D0)*MW*SW)*(dcLeft + dcRight)/2D0 + &
					   & 3D0*(MHp2 - (MU - MS)**2)*(MU/TB - MS*Yuk3)*EL*CKM12/(DSQRT(2D0)*MW*SW)*(dcLeft - dcRight)/2D0
	case (11)
		dcLeft = EL*CKM12/(DSQRT(2D0)*MW*SW)*MU/TB*( &
			& dZHpHpOS()/2D0 + TB*dZGpHpOSAlter()/2D0 + &
			& (CKM12/CKM12*MU/MU*dZUUOSRight() + CKM11/CKM12*dZDSOSLeft())/2D0 + &
			& (CKM22/CKM12*MC/MU*dZCUOSRight() + CKM12/CKM12*dZSSOSLeft())/2D0 + &
			& (CKM32/CKM12*MT/MU*dZTUOSRight() + CKM13/CKM12*dZBSOSLeft())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMUOSAlter()/MU + dCKM12Yamada()/CKM12 - dMW2Alter()/(2D0*MW2) - &
			& dBetaProcDep3Alter()/(SB*CB) )
		dcRight = EL*CKM12/(DSQRT(2D0)*MW*SW)*MS*Yuk3*( &
			& dZHpHpOS()/2D0 - dZGpHpOSAlter()/(2D0*Yuk3) + &
			& (CKM12/CKM12*dZUUOSLeft() + CKM11/CKM12*MD/MS*dZDSOSRight())/2D0 + &
			& (CKM22/CKM12*dZCUOSLeft() + CKM12/CKM12*MS/MS*dZSSOSRight())/2D0 + &
			& (CKM32/CKM12*dZTUOSLeft() + CKM13/CKM12*MB/MS*dZBSOSRight())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMSOSAlter()/MS + dCKM12Yamada()/CKM12 - dMW2Alter()/(2D0*MW2) + &
			& (1D0 + Yuk3**2)/Yuk3*dBetaProcDep3Alter() )

		totalAmplitude = 3D0*(MHp2 - (MU + MS)**2)*(MU/TB + MS*Yuk3)*EL*CKM12/(DSQRT(2D0)*MW*SW)*(dcLeft + dcRight)/2D0 + &
					   & 3D0*(MHp2 - (MU - MS)**2)*(MU/TB - MS*Yuk3)*EL*CKM12/(DSQRT(2D0)*MW*SW)*(dcLeft - dcRight)/2D0
	case (12)
		dcLeft = EL*CKM12/(DSQRT(2D0)*MW*SW)*MU/TB*( &
			& dZHpHpOS()/2D0 + TB*dZGpHpOSAlter()/2D0 + &
			& (CKM12/CKM12*MU/MU*dZUUOSRight() + CKM11/CKM12*dZDSOSLeft())/2D0 + &
			& (CKM22/CKM12*MC/MU*dZCUOSRight() + CKM12/CKM12*dZSSOSLeft())/2D0 + &
			& (CKM32/CKM12*MT/MU*dZTUOSRight() + CKM13/CKM12*dZBSOSLeft())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMUOSAlter()/MU + dCKM12Yamada()/CKM12 - dMW2Alter()/(2D0*MW2) - &
			& dBetaOS1Alter()/(SB*CB) )
		dcRight = EL*CKM12/(DSQRT(2D0)*MW*SW)*MS*Yuk3*( &
			& dZHpHpOS()/2D0 - dZGpHpOSAlter()/(2D0*Yuk3) + &
			& (CKM12/CKM12*dZUUOSLeft() + CKM11/CKM12*MD/MS*dZDSOSRight())/2D0 + &
			& (CKM22/CKM12*dZCUOSLeft() + CKM12/CKM12*MS/MS*dZSSOSRight())/2D0 + &
			& (CKM32/CKM12*dZTUOSLeft() + CKM13/CKM12*MB/MS*dZBSOSRight())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMSOSAlter()/MS + dCKM12Yamada()/CKM12 - dMW2Alter()/(2D0*MW2) + &
			& (1D0 + Yuk3**2)/Yuk3*dBetaOS1Alter() )

		totalAmplitude = 3D0*(MHp2 - (MU + MS)**2)*(MU/TB + MS*Yuk3)*EL*CKM12/(DSQRT(2D0)*MW*SW)*(dcLeft + dcRight)/2D0 + &
					   & 3D0*(MHp2 - (MU - MS)**2)*(MU/TB - MS*Yuk3)*EL*CKM12/(DSQRT(2D0)*MW*SW)*(dcLeft - dcRight)/2D0
	case (13)
		dcLeft = EL*CKM12/(DSQRT(2D0)*MW*SW)*MU/TB*( &
			& dZHpHpOS()/2D0 + TB*dZGpHpOSAlter()/2D0 + &
			& (CKM12/CKM12*MU/MU*dZUUOSRight() + CKM11/CKM12*dZDSOSLeft())/2D0 + &
			& (CKM22/CKM12*MC/MU*dZCUOSRight() + CKM12/CKM12*dZSSOSLeft())/2D0 + &
			& (CKM32/CKM12*MT/MU*dZTUOSRight() + CKM13/CKM12*dZBSOSLeft())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMUOSAlter()/MU + dCKM12Yamada()/CKM12 - dMW2Alter()/(2D0*MW2) - &
			& dBetaOS2Alter()/(SB*CB) )
		dcRight = EL*CKM12/(DSQRT(2D0)*MW*SW)*MS*Yuk3*( &
			& dZHpHpOS()/2D0 - dZGpHpOSAlter()/(2D0*Yuk3) + &
			& (CKM12/CKM12*dZUUOSLeft() + CKM11/CKM12*MD/MS*dZDSOSRight())/2D0 + &
			& (CKM22/CKM12*dZCUOSLeft() + CKM12/CKM12*MS/MS*dZSSOSRight())/2D0 + &
			& (CKM32/CKM12*dZTUOSLeft() + CKM13/CKM12*MB/MS*dZBSOSRight())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMSOSAlter()/MS + dCKM12Yamada()/CKM12 - dMW2Alter()/(2D0*MW2) + &
			& (1D0 + Yuk3**2)/Yuk3*dBetaOS2Alter() )

		totalAmplitude = 3D0*(MHp2 - (MU + MS)**2)*(MU/TB + MS*Yuk3)*EL*CKM12/(DSQRT(2D0)*MW*SW)*(dcLeft + dcRight)/2D0 + &
					   & 3D0*(MHp2 - (MU - MS)**2)*(MU/TB - MS*Yuk3)*EL*CKM12/(DSQRT(2D0)*MW*SW)*(dcLeft - dcRight)/2D0
	case (14)
		dcLeft = EL*CKM12/(DSQRT(2D0)*MW*SW)*MU/TB*( &
			& dZHpHpOS()/2D0 + TB*dZGpHpOSAlter()/2D0 + &
			& (CKM12/CKM12*MU/MU*dZUUOSRight() + CKM11/CKM12*dZDSOSLeft())/2D0 + &
			& (CKM22/CKM12*MC/MU*dZCUOSRight() + CKM12/CKM12*dZSSOSLeft())/2D0 + &
			& (CKM32/CKM12*MT/MU*dZTUOSRight() + CKM13/CKM12*dZBSOSLeft())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMUOSAlter()/MU + dCKM12Yamada()/CKM12 - dMW2Alter()/(2D0*MW2) - &
			& dBetaOS12Alter()/(SB*CB) )
		dcRight = EL*CKM12/(DSQRT(2D0)*MW*SW)*MS*Yuk3*( &
			& dZHpHpOS()/2D0 - dZGpHpOSAlter()/(2D0*Yuk3) + &
			& (CKM12/CKM12*dZUUOSLeft() + CKM11/CKM12*MD/MS*dZDSOSRight())/2D0 + &
			& (CKM22/CKM12*dZCUOSLeft() + CKM12/CKM12*MS/MS*dZSSOSRight())/2D0 + &
			& (CKM32/CKM12*dZTUOSLeft() + CKM13/CKM12*MB/MS*dZBSOSRight())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMSOSAlter()/MS + dCKM12Yamada()/CKM12 - dMW2Alter()/(2D0*MW2) + &
			& (1D0 + Yuk3**2)/Yuk3*dBetaOS12Alter() )

		totalAmplitude = 3D0*(MHp2 - (MU + MS)**2)*(MU/TB + MS*Yuk3)*EL*CKM12/(DSQRT(2D0)*MW*SW)*(dcLeft + dcRight)/2D0 + &
					   & 3D0*(MHp2 - (MU - MS)**2)*(MU/TB - MS*Yuk3)*EL*CKM12/(DSQRT(2D0)*MW*SW)*(dcLeft - dcRight)/2D0
	case (15)
		dcLeft = EL*CKM12/(DSQRT(2D0)*MW*SW)*MU/TB*( &
			& dZHpHpOS()/2D0 + TB*dZGpHpOSAlter()/2D0 + &
			& (CKM12/CKM12*MU/MU*dZUUOSRight() + CKM11/CKM12*dZDSOSLeft())/2D0 + &
			& (CKM22/CKM12*MC/MU*dZCUOSRight() + CKM12/CKM12*dZSSOSLeft())/2D0 + &
			& (CKM32/CKM12*MT/MU*dZTUOSRight() + CKM13/CKM12*dZBSOSLeft())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMUOSAlter()/MU + dCKM12Yamada()/CKM12 - dMW2Alter()/(2D0*MW2) - &
			& dBetaBFMSAlter()/(SB*CB) )
		dcRight = EL*CKM12/(DSQRT(2D0)*MW*SW)*MS*Yuk3*( &
			& dZHpHpOS()/2D0 - dZGpHpOSAlter()/(2D0*Yuk3) + &
			& (CKM12/CKM12*dZUUOSLeft() + CKM11/CKM12*MD/MS*dZDSOSRight())/2D0 + &
			& (CKM22/CKM12*dZCUOSLeft() + CKM12/CKM12*MS/MS*dZSSOSRight())/2D0 + &
			& (CKM32/CKM12*dZTUOSLeft() + CKM13/CKM12*MB/MS*dZBSOSRight())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMSOSAlter()/MS + dCKM12Yamada()/CKM12 - dMW2Alter()/(2D0*MW2) + &
			& (1D0 + Yuk3**2)/Yuk3*dBetaBFMSAlter() )

		totalAmplitude = 3D0*(MHp2 - (MU + MS)**2)*(MU/TB + MS*Yuk3)*EL*CKM12/(DSQRT(2D0)*MW*SW)*(dcLeft + dcRight)/2D0 + &
					   & 3D0*(MHp2 - (MU - MS)**2)*(MU/TB - MS*Yuk3)*EL*CKM12/(DSQRT(2D0)*MW*SW)*(dcLeft - dcRight)/2D0
	case (16)
		dcLeft = EL*CKM12/(DSQRT(2D0)*MW*SW)*MU/TB*( &
			& dZHpHpOS()/2D0 + TB*dZGpHpOSUsual()/2D0 + &
			& (CKM12/CKM12*MU/MU*dZUUOSRight() + CKM11/CKM12*dZDSOSLeft())/2D0 + &
			& (CKM22/CKM12*MC/MU*dZCUOSRight() + CKM12/CKM12*dZSSOSLeft())/2D0 + &
			& (CKM32/CKM12*MT/MU*dZTUOSRight() + CKM13/CKM12*dZBSOSLeft())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMUOSUsual()/MU + dCKM12Yamada()/CKM12 - dMW2Usual()/(2D0*MW2) - &
			& dBetaMSBarUsual()/(SB*CB) )
		dcRight = EL*CKM12/(DSQRT(2D0)*MW*SW)*MS*Yuk3*( &
			& dZHpHpOS()/2D0 - dZGpHpOSUsual()/(2D0*Yuk3) + &
			& (CKM12/CKM12*dZUUOSLeft() + CKM11/CKM12*MD/MS*dZDSOSRight())/2D0 + &
			& (CKM22/CKM12*dZCUOSLeft() + CKM12/CKM12*MS/MS*dZSSOSRight())/2D0 + &
			& (CKM32/CKM12*dZTUOSLeft() + CKM13/CKM12*MB/MS*dZBSOSRight())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMSOSUsual()/MS + dCKM12Yamada()/CKM12 - dMW2Usual()/(2D0*MW2) + &
			& (1D0 + Yuk3**2)/Yuk3*dBetaMSBarUsual() )

		totalAmplitude = 3D0*(MHp2 - (MU + MS)**2)*(MU/TB + MS*Yuk3)*EL*CKM12/(DSQRT(2D0)*MW*SW)*(dcLeft + dcRight)/2D0 + &
					   & 3D0*(MHp2 - (MU - MS)**2)*(MU/TB - MS*Yuk3)*EL*CKM12/(DSQRT(2D0)*MW*SW)*(dcLeft - dcRight)/2D0
	case (17)
		dcLeft = EL*CKM12/(DSQRT(2D0)*MW*SW)*MU/TB*( &
			& dZHpHpOS()/2D0 + TB*dZGpHpOSAlter()/2D0 + &
			& (CKM12/CKM12*MU/MU*dZUUOSRight() + CKM11/CKM12*dZDSOSLeft())/2D0 + &
			& (CKM22/CKM12*MC/MU*dZCUOSRight() + CKM12/CKM12*dZSSOSLeft())/2D0 + &
			& (CKM32/CKM12*MT/MU*dZTUOSRight() + CKM13/CKM12*dZBSOSLeft())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMUOSAlter()/MU + dCKM12Yamada()/CKM12 - dMW2Alter()/(2D0*MW2) - &
			& dBetaMSBarAlter()/(SB*CB) )
		dcRight = EL*CKM12/(DSQRT(2D0)*MW*SW)*MS*Yuk3*( &
			& dZHpHpOS()/2D0 - dZGpHpOSAlter()/(2D0*Yuk3) + &
			& (CKM12/CKM12*dZUUOSLeft() + CKM11/CKM12*MD/MS*dZDSOSRight())/2D0 + &
			& (CKM22/CKM12*dZCUOSLeft() + CKM12/CKM12*MS/MS*dZSSOSRight())/2D0 + &
			& (CKM32/CKM12*dZTUOSLeft() + CKM13/CKM12*MB/MS*dZBSOSRight())/2D0 + &
			& dgAtMZ()/(EL/SW) + dMSOSAlter()/MS + dCKM12Yamada()/CKM12 - dMW2Alter()/(2D0*MW2) + &
			& (1D0 + Yuk3**2)/Yuk3*dBetaMSBarAlter() )

		totalAmplitude = 3D0*(MHp2 - (MU + MS)**2)*(MU/TB + MS*Yuk3)*EL*CKM12/(DSQRT(2D0)*MW*SW)*(dcLeft + dcRight)/2D0 + &
					   & 3D0*(MHp2 - (MU - MS)**2)*(MU/TB - MS*Yuk3)*EL*CKM12/(DSQRT(2D0)*MW*SW)*(dcLeft - dcRight)/2D0
	case default
		totalAmplitude = 0D0
 end select

 HptoSBarUCT = totalAmplitude
end function HptoSBarUCT
