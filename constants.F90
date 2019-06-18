module constants
    implicit none
    save

    ! Imaginary unit for convenience
    double complex, parameter :: I = (0D0, 1D0)

    ! Pi and its square
    double precision, parameter :: PI = 4.D0*atan(1.D0)
    double precision, parameter :: PI2 = PI**2

    ! Gauge-fixing parameters
    ! double precision :: GaugeXiW, GaugeXiZ, GaugeXiA

    ! Detector sensitivity threshold (DelE was used for soft-photon corrections, but is redundant now)
    double precision, parameter :: DelE = 10D0
    double precision :: IRLambda

    ! Temporary
    double precision :: GaugeXiW = 1.0D0
    double precision :: GaugeXiZ = 1.0D0
    double precision :: GaugeXiA = 1.0D0

    ! Standard Model parameters (1101.0593 [hep-ph], 1503.07589 [hep-ex],  Chin. Phys. C38 (2014) 090001)
    ! !double precision, parameter :: Mh0 = 125.09D0
    ! double precision, parameter :: Mh0 = 125.0D0    ! TODO: Change to above!!!
	! double precision, parameter :: MW = 80.3979999999999961D0
	! double precision, parameter :: MZ = 91.1876000000000033D0
	! double precision, parameter :: ME = 0.00051099891000000D0
	! double precision, parameter :: MM = 0.10565836700000000D0
	! double precision, parameter :: ML = 1.77684000000000000D0
	! double precision, parameter :: MU = 0.19000000000000000D0
	! double precision, parameter :: MC = 1.39999999999999999D0
	! double precision, parameter :: MT = 172.500000000000000D0
	! double precision, parameter :: MD = 0.19000000000000000D0
	! double precision, parameter :: MS = 0.19000000000000000D0
	! double precision, parameter :: MB = 4.75000000000000000D0
	! ! double precision, parameter :: EL = 0.31215769999190435D0
	! double precision, parameter :: EL = 0.30282212088722120D0
	! double precision, parameter :: SW = 0.47185363558310760D0
	! double precision, parameter :: CW = 0.88167689466550270D0
	! double precision, parameter :: CKM11 = 0.97427D0
	! double precision, parameter :: CKM12 = 0.22536D0
	! double precision, parameter :: CKM13 = 0.00355D0
	! double precision, parameter :: CKM21 = -0.22522D0
	! double precision, parameter :: CKM22 = 0.97343D0
	! double precision, parameter :: CKM23 = 0.0414D0
	! double precision, parameter :: CKM31 = 0.00886D0
	! double precision, parameter :: CKM32 = -0.0405D0
	! double precision, parameter :: CKM33 = 0.99914D0
	! double precision, parameter :: CKMC11 = 0.97427D0
	! double precision, parameter :: CKMC12 = 0.22536D0
	! double precision, parameter :: CKMC13 = 0.00355D0
	! double precision, parameter :: CKMC21 = -0.22522D0
	! double precision, parameter :: CKMC22 = 0.97343D0
	! double precision, parameter :: CKMC23 = 0.0414D0
	! double precision, parameter :: CKMC31 = 0.00886D0
	! double precision, parameter :: CKMC32 = -0.0405D0
	! double precision, parameter :: CKMC33 = 0.99914D0

    ! Standard Model parameters; the values are stored in Parameters/ParametersSM.txt (1101.0593 [hep-ph], 1503.07589 [hep-ex],  Chin. Phys. C38 (2014) 090001)
    double precision :: Mh0
	double precision :: MW
	double precision :: MZ
	double precision :: ME
	double precision :: MM
	double precision :: ML
	double precision :: MU
	double precision :: MC
	double precision :: MT
	double precision :: MD
	double precision :: MS
	double precision :: MB
	double precision :: EL
	double precision :: SW
	double precision :: CW
	double precision :: CKM11
	double precision :: CKM12
	double precision :: CKM13
	double precision :: CKM21
	double precision :: CKM22
	double precision :: CKM23
	double precision :: CKM31
	double precision :: CKM32
	double precision :: CKM33
	double precision :: CKMC11
	double precision :: CKMC12
	double precision :: CKMC13
	double precision :: CKMC21
	double precision :: CKMC22
	double precision :: CKMC23
	double precision :: CKMC31
	double precision :: CKMC32
	double precision :: CKMC33
	double precision :: InputScale
	integer :: TypeOf2HDM

    ! Squared Standard Model parameters
    ! double precision, parameter :: Mh02 = Mh0**2
	! double precision, parameter :: MW2 = MW**2
	! double precision, parameter :: MZ2 = MZ**2
	! double precision, parameter :: ME2 = ME**2
	! double precision, parameter :: MM2 = MM**2
	! double precision, parameter :: ML2 = ML**2
	! double precision, parameter :: MU2 = MU**2
	! double precision, parameter :: MC2 = MC**2
	! double precision, parameter :: MT2 = MT**2
	! double precision, parameter :: MD2 = MD**2
	! double precision, parameter :: MS2 = MS**2
	! double precision, parameter :: MB2 = MB**2
	! double precision, parameter :: EL2 = EL**2
	! double precision, parameter :: SW2 = SW**2
	! double precision, parameter :: CW2 = CW**2

    ! Squared Standard Model parameters
    double precision :: Mh02
	double precision :: MW2
	double precision :: MZ2
	double precision :: ME2
	double precision :: MM2
	double precision :: ML2
	double precision :: MU2
	double precision :: MC2
	double precision :: MT2
	double precision :: MD2
	double precision :: MS2
	double precision :: MB2
	double precision :: EL2
	double precision :: SW2
	double precision :: CW2


    ! 2HDM-specific parameters (these are set in Parameters/getParameters.F90 by reading the respective input files)
    double precision :: MHH
	double precision :: MA0
	double precision :: MHp
	double precision :: alpha
	double precision :: beta
	double precision :: CA
	double precision :: SA
	double precision :: TA
	double precision :: CB
	double precision :: SB
	double precision :: TB
	double precision :: S2A
	double precision :: C2A
	double precision :: S2B
	double precision :: C2B
	double precision :: CAB
	double precision :: SAB
	double precision :: CBA
	double precision :: SBA
    double precision :: Yuk1
	double precision :: Yuk2
	double precision :: Yuk3
    double precision :: Yuk4
	double precision :: Yuk5
	double precision :: Yuk6
	double precision :: Lambda5
	double precision :: m12squared

	! UV scale 
	double precision :: UVDelta = 0D0

    ! Maximum number of data points contained in a 2HDM parameter file
    integer maxPoint

    ! List version of the 2HDM-specific parameters containing all data from the input files
    integer :: numberOfPoints
    double precision :: MHHList(1000000)
	double precision :: MA0List(1000000)
	double precision :: MHpList(1000000)
	double precision :: alphaList(1000000)
	double precision :: betaList(1000000)
	double precision :: CAList(1000000)
	double precision :: SAList(1000000)
    double precision :: TAList(1000000)
    double precision :: CBList(1000000)
    double precision :: SBList(1000000)
	double precision :: TBList(1000000)
    double precision :: S2AList(1000000)
    double precision :: C2AList(1000000)
    double precision :: S2BList(1000000)
    double precision :: C2BList(1000000)
    double precision :: CABList(1000000)
    double precision :: SABList(1000000)
    double precision :: CBAList(1000000)
    double precision :: SBAList(1000000)
	double precision :: m12squaredList(1000000)
    double precision :: Lambda5List(1000000)
    double precision :: Yuk1List(1000000)
    double precision :: Yuk2List(1000000)
    double precision :: Yuk3List(1000000)
    double precision :: Yuk4List(1000000)
    double precision :: Yuk5List(1000000)
    double precision :: Yuk6List(1000000)
    integer :: TypeOf2HDMList(1000000)

    ! 2HDM-specific parameters (TODO: temporary version)
    ! double precision :: MHH = 230.0D0
	! double precision, parameter :: MA0 = 290.0D0
	! double precision, parameter :: MHp = 640.0D0
	! double precision, parameter :: alpha = -0.446106D0
	! double precision, parameter :: beta = 1.3734D0
	! double precision, parameter :: CA = cos(alpha)
	! double precision, parameter :: SA = sin(alpha)
	! double precision, parameter :: TA = tan(alpha)
	! double precision, parameter :: CB = cos(beta)
	! double precision, parameter :: SB = sin(beta)
	! double precision, parameter :: TB = tan(beta)
	! double precision, parameter :: S2A = sin(2.*alpha)
	! double precision, parameter :: C2A = cos(2.*alpha)
	! double precision, parameter :: S2B = sin(2.*beta)
	! double precision, parameter :: C2B = cos(2.*beta)
	! double precision, parameter :: CAB = cos(alpha + beta)
	! double precision, parameter :: SAB = sin(alpha + beta)
	! double precision, parameter :: CBA = cos(beta - alpha)
	! double precision, parameter :: SBA = sin(beta - alpha)
    ! double precision, parameter :: CA = 0.902134D0
    ! double precision, parameter :: SA = 0.431455D0
    ! double precision, parameter :: TA = 0.478261D0
    ! double precision, parameter :: CB = 0.196116D0
    ! double precision, parameter :: SB = 0.980581D0
    ! double precision, parameter :: TB = 5.0D0
    ! double precision, parameter :: S2A = 0.778462D0
    ! double precision, parameter :: C2A = 0.627692D0
    ! double precision, parameter :: S2B = 0.384615D0
    ! double precision, parameter :: C2B = -0.923077D0
    ! double precision, parameter :: CAB = -0.246154D0
    ! double precision, parameter :: SAB = 0.969231D0
    ! double precision, parameter :: CBA = 0.6D0
    ! double precision, parameter :: SBA = 0.8D0
    ! double precision, parameter :: Yuk1 = 0.92D0
	! double precision, parameter :: Yuk2 = 0.44D0
	! double precision, parameter :: Yuk3 = -0.2D0
	! double precision, parameter :: Lambda5 = 1.38954D0
	! double precision, parameter :: m12squared = 103D0

    ! Squared 2HDM-specific parameters (TODO: version for the final program)
    double precision :: MHH2
	double precision :: MA02
	double precision :: MHp2
    double precision :: CA2
	double precision :: SA2
	double precision :: TA2
	double precision :: TB2
	double precision :: SB2
	double precision :: CB2
	double precision :: C2A2
	double precision :: S2A2
	double precision :: C2B2
	double precision :: S2B2
	double precision :: CAB2
	double precision :: SAB2
	double precision :: CBA2
	double precision :: SBA2

    ! Squared 2HDM-specific parameters (TODO: temporary version)
    ! double precision :: MHH2
	! double precision, parameter :: MA02 = MA0**2
	! double precision, parameter :: MHp2 = MHp**2
    ! double precision, parameter :: CA2 = CA**2
	! double precision, parameter :: SA2 = SA**2
	! double precision, parameter :: TA2 = TA**2
	! double precision, parameter :: TB2 = TB**2
	! double precision, parameter :: SB2 = SB**2
	! double precision, parameter :: CB2 = CB**2
	! double precision, parameter :: C2A2 = C2A**2
	! double precision, parameter :: S2A2 = S2A**2
	! double precision, parameter :: C2B2 = C2B**2
	! double precision, parameter :: S2B2 = S2B**2
	! double precision, parameter :: CAB2 = CAB**2
	! double precision, parameter :: SAB2 = SAB**2
	! double precision, parameter :: CBA2 = CBA**2
	! double precision, parameter :: SBA2 = SBA**2

contains
    ! This is the three-point function with C0(0,p,p,0,0,m) which diverges individually, but cancels overall
    double complex function C0Mine(a,b,c,d,e,f)
        implicit none
        double precision, intent(in) :: a,b,c,d,e,f
        C0Mine = (0D0,0D0)
    end function C0Mine

	double complex function D0Mine(a,b,c,d,e,f,g,h,j,k)
        implicit none
        double precision, intent(in) :: a,b,c,d,e,f,g,h,j,k
        D0Mine = (0D0,0D0)
    end function D0Mine

    ! These are the derivatives of the C0(0,p,p,0,0,m) integral (with respect to p) which cancel in total
    double complex function DC01Mine(a,b,c,d,e,f)
        implicit none
        double precision, intent(in) :: a,b,c,d,e,f
        DC01Mine = (0D0,0D0)
    end function DC01Mine
    double complex function DC02Mine(a,b,c,d,e,f)
        implicit none
        double precision, intent(in) :: a,b,c,d,e,f
        DC02Mine = (0D0,0D0)
    end function DC02Mine

    double complex function DiracGamma(a)
        implicit none
        double precision, intent(in) :: a
        DiracGamma = (0D0, 0D0)
    end function DiracGamma

end module constants
