module counterterms
    implicit none
    save

    contains
        ! Tadpole counterterms
            ! Alternative tadpole based counterterms
                ! Tadpole counterterms are not needed in the alternative tadpole approach

            ! Usual tadpole based counterterms
                double precision function dTHHHH()
                    use constants
                    implicit none
                    double complex TadHH, Tadh0
                    dTHHHH = (CA**3*SB+SA**3*CB)*DBLE(TadHH())/((2.D0*MW*SW/EL)*SB*CB) - &
                            & (S2A*SBA)*DBLE(Tadh0())/((2.D0*MW*SW/EL)*S2B)
                end function dTHHHH

                double precision function dTHHh0()
                    use constants
                    implicit none
                    double complex TadHH, Tadh0
                    dTHHh0 = -(S2A*SBA)*DBLE(TadHH())/((2.D0*MW*SW/EL)*S2B) + (S2A*CBA)*DBLE(Tadh0())/((2.D0*MW*SW/EL)*S2B)
                end function dTHHh0

                double precision function dTh0h0()
                    use constants
                    implicit none
                    double complex TadHH, Tadh0
                    dTh0h0 = (S2A*CBA)*DBLE(TadHH())/((2.D0*MW*SW/EL)*S2B) - &
                            & (SA**3*SB-CA**3*CB)*DBLE(Tadh0())/((2.D0*MW*SW/EL)*SB*CB)
                end function dTh0h0

                double precision function dTGpGp()
                    use constants
                    implicit none
                    double complex TadHH, Tadh0
                    dTGpGp = (CBA)*DBLE(TadHH())/(2.D0*MW*SW/EL) + (SBA)*DBLE(Tadh0())/(2.D0*MW*SW/EL)
                end function dTGpGp

                double precision function dTGpHp()
                    use constants
                    implicit none
                    double complex TadHH, Tadh0
                    dTGpHp = -(SBA)*DBLE(TadHH())/(2.D0*MW*SW/EL) + (CBA)*DBLE(Tadh0())/(2.D0*MW*SW/EL)
                end function dTGpHp

                double precision function dTHpHp()
                    use constants
                    implicit none
                    double complex TadHH, Tadh0
                    dTHpHp = (CA*SB**3+SA*CB**3)*DBLE(TadHH())/((2.D0*MW*SW/EL)*SB*CB) - &
                            & (SA*SB**3-CA*CB**3)*DBLE(Tadh0())/((2.D0*MW*SW/EL)*SB*CB)
                end function dTHpHp

                double precision function dTG0G0()
                    use constants
                    implicit none
                    double complex TadHH, Tadh0
                    dTG0G0 = (CBA)*DBLE(TadHH())/(2.D0*MW*SW/EL) + (SBA)*DBLE(Tadh0())/(2.D0*MW*SW/EL)
                end function dTG0G0

                double precision function dTG0A0()
                    use constants
                    implicit none
                    double complex TadHH, Tadh0
                    dTG0A0 = -(SBA)*DBLE(TadHH())/(2.D0*MW*SW/EL) + (CBA)*DBLE(Tadh0())/(2.D0*MW*SW/EL)
                end function dTG0A0

                double precision function dTA0A0()
                    use constants
                    implicit none
                    double complex TadHH, Tadh0
                    dTA0A0 = (CA*SB**3+SA*CB**3)*DBLE(TadHH())/((2.D0*MW*SW/EL)*SB*CB) - &
                            & (SA*SB**3-CA*CB**3)*DBLE(Tadh0())/((2.D0*MW*SW/EL)*SB*CB)
                end function dTA0A0

        ! Counterterms of the electroweak sector
            ! Alternative tadpole based counterterms
                double precision function dMZ2Alter()
                    use constants
                    implicit none
                    double complex SelfZ0Z0Alter
                    dMZ2Alter = DBLE(SelfZ0Z0Alter(MZ2))
                end function dMZ2Alter

                double precision function dMW2Alter()
                    use constants
                    implicit none
                    double complex SelfWpWpAlter
                    dMW2Alter = DBLE(SelfWpWpAlter(MW2))
                end function dMW2Alter

            ! Usual tadpole based counterterms
                double precision function dMZ2Usual()
                    use constants
                    implicit none
                    double complex SelfZ0Z0Usual
                    dMZ2Usual = DBLE(SelfZ0Z0Usual(MZ2))
                end function dMZ2Usual

                double precision function dMW2Usual()
                    use constants
                    implicit none
                    double complex SelfWpWpUsual
                    dMW2Usual = DBLE(SelfWpWpUsual(MW2))
                end function dMW2Usual

            ! Tadpole invariant counterterms
                double precision function dZWpWpOS()
                    use constants
                    implicit none
                    double complex DSelfWpWp
                    dZWpWpOS = -DBLE(DSelfWpWp(MW2))
                end function dZWpWpOS

                double precision function dZZ0Z0OS()
                    use constants
                    implicit none
                    double complex DSelfZ0Z0
                    dZZ0Z0OS = -DBLE(DSelfZ0Z0(MZ2))
                end function dZZ0Z0OS

                double precision function dZAAOS()
                    use constants
                    implicit none
                    double complex DSelfAA
                    dZAAOS = -DBLE(DSelfAA(0.D0))
                end function dZAAOS

                double precision function dZZ0AOS()
                    use constants
                    implicit none
                    double complex SelfAZ0UsualZeroMom
                    dZZ0AOS = 2.D0*DBLE(SelfAZ0UsualZeroMom(0.D0))/MZ2
                end function dZZ0AOS

                double precision function dZAZ0OS()
                    use constants
                    implicit none
                    double complex SelfAZ0Usual
                    dZAZ0OS = -2.D0*DBLE(SelfAZ0Usual(MZ2))/MZ2
                end function dZAZ0OS

                double precision function dZe()
                    use constants
                    implicit none
                    double complex SelfAZ0UsualZeroMom, DSelfAA
                    dZe = (SW*DBLE(SelfAZ0UsualZeroMom(0.D0)))/(CW*MZ2) + DBLE(DSelfAA(0.D0))/2.D0
                end function dZe

                double precision function dg()
                    use constants
                    implicit none
                    dg = EL*( dZe() + (dMW2Usual() - MW2*dMZ2Usual()/MZ2)/(2.D0*(MZ2 - MW2)) )/SW
                end function dg

                double precision function dZeAtMZ()
                    use constants
                    implicit none
                    double complex SelfAALight, DSelfAALight
                    dZeAtMZ = dZe() - 0.5D0*( DBLE(DSelfAALight(0.D0)) - DBLE(SelfAALight(MZ2))/MZ2 )
                end function dZeAtMZ

                double precision function dgAtMZ()
                    use constants
                    implicit none
                    dgAtMZ = EL*( dZeAtMZ() + (dMW2Usual() - MW2*dMZ2Usual()/MZ2)/(2.D0*(MZ2 - MW2)) )/SW
                end function dgAtMZ

        ! Counterterms of the scalar sector
            ! Alternative tadpole based counterterms
                double precision function dZHHh0OSAlter()
                    use constants
                    implicit none
                    double complex SelfHHh0Alter
                    dZHHh0OSAlter = 2.D0*(DBLE(SelfHHh0Alter(Mh02)))/(MHH2 - Mh02)
                end function dZHHh0OSAlter

                double precision function dZh0HHOSAlter()
                    use constants
                    implicit none
                    double complex SelfHHh0Alter
                    dZh0HHOSAlter = 2.D0*(DBLE(SelfHHh0Alter(MHH2)))/(Mh02 - MHH2)
                end function dZh0HHOSAlter

                double precision function dZG0A0OSAlter()
                    use constants
                    implicit none
                    double complex SelfG0A0Alter
                    dZG0A0OSAlter = -2.D0*(DBLE(SelfG0A0Alter(MA02)))/(MA02)
                end function dZG0A0OSAlter

                double precision function dZA0G0OSAlter()
                    use constants
                    implicit none
                    double complex SelfG0A0Alter
                    dZA0G0OSAlter = 2.D0*(DBLE(SelfG0A0Alter(0.D0)))/(MA02)
                end function dZA0G0OSAlter

                double precision function dZGpHpOSAlter()
                    use constants
                    implicit none
                    double complex SelfGpHpAlter
                    dZGpHpOSAlter = -2.D0*(DBLE(SelfGpHpAlter(MHp2)))/(MHp2)
                end function dZGpHpOSAlter

                double precision function dZHpGpOSAlter()
                    use constants
                    implicit none
                    double complex SelfGpHpAlter
                    dZHpGpOSAlter = 2.D0*(DBLE(SelfGpHpAlter(0.D0)))/(MHp2)
                end function dZHpGpOSAlter

                double precision function dMHH2OSAlter()
                    use constants
                    implicit none
                    double complex SelfHHHHAlter
                    dMHH2OSAlter = DBLE(SelfHHHHAlter(MHH2))
                end function dMHH2OSAlter

                double precision function dMh02OSAlter()
                    use constants
                    implicit none
                    double complex Selfh0h0Alter
                    dMh02OSAlter = DBLE(Selfh0h0Alter(Mh02))
                end function dMh02OSAlter

                double precision function dMA02OSAlter()
                    use constants
                    implicit none
                    double complex SelfA0A0Alter
                    dMA02OSAlter = DBLE(SelfA0A0Alter(MA02))
                end function dMA02OSAlter

                double precision function dMHp2OSAlter()
                    use constants
                    implicit none
                    double complex SelfHpHpAlter
                    dMHp2OSAlter = DBLE(SelfHpHpAlter(MHp2))
                end function dMHp2OSAlter

            ! Usual tadpole based counterterms
                double precision function dZHHh0OSUsual()
                    use constants
                    implicit none
                    double complex SelfHHh0Usual
                    dZHHh0OSUsual = 2.D0*(DBLE(SelfHHh0Usual(Mh02)) - DBLE(dTHHh0()))/(MHH2 - Mh02)
                end function dZHHh0OSUsual

                double precision function dZh0HHOSUsual()
                    use constants
                    implicit none
                    double complex SelfHHh0Usual
                    dZh0HHOSUsual = 2.D0*(DBLE(SelfHHh0Usual(MHH2)) - DBLE(dTHHh0()))/(Mh02 - MHH2)
                end function dZh0HHOSUsual

                double precision function dZG0A0OSUsual()
                    use constants
                    implicit none
                    double complex SelfG0A0Usual
                    dZG0A0OSUsual = -2.D0*(DBLE(SelfG0A0Usual(MA02)) - DBLE(dTG0A0()))/(MA02)
                end function dZG0A0OSUsual

                double precision function dZA0G0OSUsual()
                    use constants
                    implicit none
                    double complex SelfG0A0Usual
                    dZA0G0OSUsual = 2.D0*(DBLE(SelfG0A0Usual(0.D0)) - DBLE(dTG0A0()))/(MA02)
                end function dZA0G0OSUsual

                double precision function dZGpHpOSUsual()
                    use constants
                    implicit none
                    double complex SelfGpHpUsual
                    dZGpHpOSUsual = -2.D0*(DBLE(SelfGpHpUsual(MHp2)) - DBLE(dTGpHp()))/(MHp2)
                end function dZGpHpOSUsual

                double precision function dZHpGpOSUsual()
                    use constants
                    implicit none
                    double complex SelfGpHpUsual
                    dZHpGpOSUsual = 2.D0*(DBLE(SelfGpHpUsual(0.D0)) - DBLE(dTGpHp()))/(MHp2)
                end function dZHpGpOSUsual

                double precision function dMHH2OSUsual()
                    use constants
                    implicit none
                    double complex SelfHHHHUsual
                    dMHH2OSUsual = DBLE(SelfHHHHUsual(MHH2)) - DBLE(dTHHHH())
                end function dMHH2OSUsual

                double precision function dMh02OSUsual()
                    use constants
                    implicit none
                    double complex Selfh0h0Usual
                    dMh02OSUsual = DBLE(Selfh0h0Usual(Mh02)) - DBLE(dTh0h0())
                end function dMh02OSUsual

                double precision function dMA02OSUsual()
                    use constants
                    implicit none
                    double complex SelfA0A0Usual
                    dMA02OSUsual = DBLE(SelfA0A0Usual(MA02)) - DBLE(dTA0A0())
                end function dMA02OSUsual

                double precision function dMHp2OSUsual()
                    use constants
                    implicit none
                    double complex SelfHpHpUsual
                    dMHp2OSUsual = DBLE(SelfHpHpUsual(MHp2)) - DBLE(dTHpHp())
                end function dMHp2OSUsual

            ! Tadpole invariant counterterms
                double precision function dZHHHHOS()
                    use constants
                    implicit none
                    double complex DSelfHHHH
                    dZHHHHOS = -DBLE(DSelfHHHH(MHH2))
                end function dZHHHHOS

                double precision function dZh0h0OS()
                    use constants
                    implicit none
                    double complex DSelfh0h0
                    dZh0h0OS = -DBLE(DSelfh0h0(Mh02))
                end function dZh0h0OS

                double precision function dZG0G0OS()
                    use constants
                    implicit none
                    double complex DSelfG0G0
                    dZG0G0OS = -DBLE(DSelfG0G0(0.D0))
                end function dZG0G0OS

                double precision function dZA0A0OS()
                    use constants
                    implicit none
                    double complex DSelfA0A0
                    dZA0A0OS = -DBLE(DSelfA0A0(MA02))
                end function dZA0A0OS

                double precision function dZGpGpOS()
                    use constants
                    implicit none
                    double complex DSelfGpGp
                    dZGpGpOS = -DBLE(DSelfGpGp(0.D0))
                end function dZGpGpOS

                double precision function dZHpHpOS()
                    use constants
                    implicit none
                    double complex DSelfHpHp
                    dZHpHpOS = -DBLE(DSelfHpHp(MHp2))
                end function dZHpHpOS

        ! Counterterms of the fermion sector
            ! Alternative tadpole based counterterms
                double precision function dMEOSAlter()
                    use constants
                    implicit none
                    double complex SelfElElLeftAlter, SelfElElRightAlter, SelfElElScalarAlter
                    dMEOSAlter = ME/2D0*DBLE( SelfElElLeftAlter(ME2) + SelfElElRightAlter(ME2) + &
                        & 2D0*SelfElElScalarAlter(ME2) )
                end function dMEOSAlter

                double precision function dMMOSAlter()
                    use constants
                    implicit none
                    double complex SelfMuMuLeftAlter, SelfMuMuRightAlter, SelfMuMuScalarAlter
                    dMMOSAlter = MM/2D0*DBLE( SelfMuMuLeftAlter(MM2) + SelfMuMuRightAlter(MM2) + &
                        & 2D0*SelfMuMuScalarAlter(MM2) )
                end function dMMOSAlter

                double precision function dMLOSAlter()
                    use constants
                    implicit none
                    double complex SelfTauTauLeftAlter, SelfTauTauRightAlter, SelfTauTauScalarAlter
                    dMLOSAlter = ML/2D0*DBLE( SelfTauTauLeftAlter(ML2) + SelfTauTauRightAlter(ML2) + &
                        & 2D0*SelfTauTauScalarAlter(ML2) )
                end function dMLOSAlter

                double precision function dMLOSAlterWeak()
                    use constants
                    implicit none
                    double complex SelfTauTauLeftWeakAlter, SelfTauTauRightWeakAlter, SelfTauTauScalarWeakAlter
                    dMLOSAlterWeak = ML/2D0*DBLE( SelfTauTauLeftWeakAlter(ML2) + SelfTauTauRightWeakAlter(ML2) + &
                        & 2D0*SelfTauTauScalarWeakAlter(ML2) )
                end function dMLOSAlterWeak

                double precision function dMDOSAlter()
                    use constants
                    implicit none
                    double complex SelfDDLeftAlter, SelfDDRightAlter, SelfDDScalarAlter
                    dMDOSAlter = MD/2D0*DBLE( SelfDDLeftAlter(MD2) + SelfDDRightAlter(MD2) + &
                        & 2D0*SelfDDScalarAlter(MD2) )
                end function dMDOSAlter

                double precision function dMUOSAlter()
                    use constants
                    implicit none
                    double complex SelfUULeftAlter, SelfUURightAlter, SelfUUScalarAlter
                    dMUOSAlter = MU/2D0*DBLE( SelfUULeftAlter(MU2) + SelfUURightAlter(MU2) + &
                        & 2D0*SelfUUScalarAlter(MU2) )
                end function dMUOSAlter

                double precision function dMSOSAlter()
                    use constants
                    implicit none
                    double complex SelfSSLeftAlter, SelfSSRightAlter, SelfSSScalarAlter
                    dMSOSAlter = MS/2D0*DBLE( SelfSSLeftAlter(MS2) + SelfSSRightAlter(MS2) + &
                        & 2D0*SelfSSScalarAlter(MS2) )
                end function dMSOSAlter

                double precision function dMCOSAlter()
                    use constants
                    implicit none
                    double complex SelfCCLeftAlter, SelfCCRightAlter, SelfCCScalarAlter
                    dMCOSAlter = MC/2D0*DBLE( SelfCCLeftAlter(MC2) + SelfCCRightAlter(MC2) + &
                        & 2D0*SelfCCScalarAlter(MC2) )
                end function dMCOSAlter

                double precision function dMBOSAlter()
                    use constants
                    implicit none
                    double complex SelfBBLeftAlter, SelfBBRightAlter, SelfBBScalarAlter
                    dMBOSAlter = MB/2D0*DBLE( SelfBBLeftAlter(MB2) + SelfBBRightAlter(MB2) + &
                        & 2D0*SelfBBScalarAlter(MB2) )
                end function dMBOSAlter

                double precision function dMTOSAlter()
                    use constants
                    implicit none
                    double complex SelfTTLeftAlter, SelfTTRightAlter, SelfTTScalarAlter
                    dMTOSAlter = MT/2D0*DBLE( SelfTTLeftAlter(MT2) + SelfTTRightAlter(MT2) + &
                        & 2D0*SelfTTScalarAlter(MT2) )
                end function dMTOSAlter

            ! Usual tadpole based counterterms
                double precision function dMEOSUsual()
                    use constants
                    implicit none
                    double complex SelfElElLeftUsual, SelfElElRightUsual, SelfElElScalarUsual
                    dMEOSUsual = ME/2D0*DBLE( SelfElElLeftUsual(ME2) + SelfElElRightUsual(ME2) + &
                        & 2D0*SelfElElScalarUsual(ME2) )
                end function dMEOSUsual

                double precision function dMMOSUsual()
                    use constants
                    implicit none
                    double complex SelfMuMuLeftUsual, SelfMuMuRightUsual, SelfMuMuScalarUsual
                    dMMOSUsual = MM/2D0*DBLE( SelfMuMuLeftUsual(MM2) + SelfMuMuRightUsual(MM2) + &
                        & 2D0*SelfMuMuScalarUsual(MM2) )
                end function dMMOSUsual

                double precision function dMLOSUsual()
                    use constants
                    implicit none
                    double complex SelfTauTauLeftUsual, SelfTauTauRightUsual, SelfTauTauScalarUsual
                    dMLOSUsual = ML/2D0*DBLE( SelfTauTauLeftUsual(ML2) + SelfTauTauRightUsual(ML2) + &
                        & 2D0*SelfTauTauScalarUsual(ML2) )
                end function dMLOSUsual

                double precision function dMLOSUsualWeak()
                    use constants
                    implicit none
                    double complex SelfTauTauLeftWeakUsual, SelfTauTauRightWeakUsual, SelfTauTauScalarWeakUsual
                    dMLOSUsualWeak = ML/2D0*DBLE( SelfTauTauLeftWeakUsual(ML2) + SelfTauTauRightWeakUsual(ML2) + &
                        & 2D0*SelfTauTauScalarWeakUsual(ML2) )
                end function dMLOSUsualWeak

                double precision function dMDOSUsual()
                    use constants
                    implicit none
                    double complex SelfDDLeftUsual, SelfDDRightUsual, SelfDDScalarUsual
                    dMDOSUsual = MD/2D0*DBLE( SelfDDLeftUsual(MD2) + SelfDDRightUsual(MD2) + &
                        & 2D0*SelfDDScalarUsual(MD2) )
                end function dMDOSUsual

                double precision function dMUOSUsual()
                    use constants
                    implicit none
                    double complex SelfUULeftUsual, SelfUURightUsual, SelfUUScalarUsual
                    dMUOSUsual = MU/2D0*DBLE( SelfUULeftUsual(MU2) + SelfUURightUsual(MU2) + &
                        & 2D0*SelfUUScalarUsual(MU2) )
                end function dMUOSUsual

                double precision function dMSOSUsual()
                    use constants
                    implicit none
                    double complex SelfSSLeftUsual, SelfSSRightUsual, SelfSSScalarUsual
                    dMSOSUsual = MS/2D0*DBLE( SelfSSLeftUsual(MS2) + SelfSSRightUsual(MS2) + &
                        & 2D0*SelfSSScalarUsual(MS2) )
                end function dMSOSUsual

                double precision function dMCOSUsual()
                    use constants
                    implicit none
                    double complex SelfCCLeftUsual, SelfCCRightUsual, SelfCCScalarUsual
                    dMCOSUsual = MC/2D0*DBLE( SelfCCLeftUsual(MC2) + SelfCCRightUsual(MC2) + &
                        & 2D0*SelfCCScalarUsual(MC2) )
                end function dMCOSUsual

                double precision function dMBOSUsual()
                    use constants
                    implicit none
                    double complex SelfBBLeftUsual, SelfBBRightUsual, SelfBBScalarUsual
                    dMBOSUsual = MB/2D0*DBLE( SelfBBLeftUsual(MB2) + SelfBBRightUsual(MB2) + &
                        & 2D0*SelfBBScalarUsual(MB2) )
                end function dMBOSUsual

                double precision function dMTOSUsual()
                    use constants
                    implicit none
                    double complex SelfTTLeftUsual, SelfTTRightUsual, SelfTTScalarUsual
                    dMTOSUsual = MT/2D0*DBLE( SelfTTLeftUsual(MT2) + SelfTTRightUsual(MT2) + &
                        & 2D0*SelfTTScalarUsual(MT2) )
                end function dMTOSUsual

            ! Tadpole invariant counterterms
                double precision function dZNeuENeuEOSRight()
                    use constants
                    implicit none
                    double complex SelfNeuENeuERightUsual, DSelfNeuENeuELeft, DSelfNeuENeuERight
                    double complex DSelfNeuENeuEScalar
                    dZNeuENeuEOSRight = -DBLE( SelfNeuENeuERightUsual(0D0) ) - 0D0*DBLE( DSelfNeuENeuELeft(0D0) + &
                        & DSelfNeuENeuERight(0D0) + 2D0*DSelfNeuENeuEScalar(0D0) )
                end function dZNeuENeuEOSRight

                double precision function dZNeuENeuEOSLeft()
                    use constants
                    implicit none
                    double complex SelfNeuENeuELeftUsual, DSelfNeuENeuELeft, DSelfNeuENeuERight
                    double complex DSelfNeuENeuEScalar
                    dZNeuENeuEOSLeft = -DBLE( SelfNeuENeuELeftUsual(0D0) ) - 0D0*DBLE( DSelfNeuENeuELeft(0D0) + &
                        & DSelfNeuENeuERight(0D0) + 2D0*DSelfNeuENeuEScalar(0D0) )
                end function dZNeuENeuEOSLeft

                double precision function dZNeuMNeuMOSRight()
                    use constants
                    implicit none
                    double complex SelfNeuMNeuMRightUsual, DSelfNeuMNeuMLeft, DSelfNeuMNeuMRight
                    double complex DSelfNeuMNeuMScalar
                    dZNeuMNeuMOSRight = -DBLE( SelfNeuMNeuMRightUsual(0D0) ) - 0D0*DBLE( DSelfNeuMNeuMLeft(0D0) + &
                        & DSelfNeuMNeuMRight(0D0) + 2D0*DSelfNeuMNeuMScalar(0D0) )
                end function dZNeuMNeuMOSRight

                double precision function dZNeuMNeuMOSLeft()
                    use constants
                    implicit none
                    double complex SelfNeuMNeuMLeftUsual, DSelfNeuMNeuMLeft, DSelfNeuMNeuMRight
                    double complex DSelfNeuMNeuMScalar
                    dZNeuMNeuMOSLeft = -DBLE( SelfNeuMNeuMLeftUsual(0D0) ) - 0D0*DBLE( DSelfNeuMNeuMLeft(0D0) + &
                        & DSelfNeuMNeuMRight(0D0) + 2D0*DSelfNeuMNeuMScalar(0D0) )
                end function dZNeuMNeuMOSLeft

                double precision function dZNeuTNeuTOSRight()
                    use constants
                    implicit none
                    double complex SelfNeuTNeuTRightUsual, DSelfNeuTNeuTLeft, DSelfNeuTNeuTRight
                    double complex DSelfNeuTNeuTScalar
                    dZNeuTNeuTOSRight = -DBLE( SelfNeuTNeuTRightUsual(0D0) ) - 0D0*DBLE( DSelfNeuTNeuTLeft(0D0) + &
                        & DSelfNeuTNeuTRight(0D0) + 2D0*DSelfNeuTNeuTScalar(0D0) )
                end function dZNeuTNeuTOSRight

                double precision function dZNeuTNeuTOSLeft()
                    use constants
                    implicit none
                    double complex SelfNeuTNeuTLeftUsual, DSelfNeuTNeuTLeft, DSelfNeuTNeuTRight
                    double complex DSelfNeuTNeuTScalar
                    dZNeuTNeuTOSLeft = -DBLE( SelfNeuTNeuTLeftUsual(0D0) ) - 0D0*DBLE( DSelfNeuTNeuTLeft(0D0) + &
                        & DSelfNeuTNeuTRight(0D0) + 2D0*DSelfNeuTNeuTScalar(0D0) )
                end function dZNeuTNeuTOSLeft

                double precision function dZElElOSLeft()
                    use constants
                    implicit none
                    double complex SelfElElLeftUsual, DSelfElElLeft, DSelfElElRight, DSelfElElScalar
                    dZElElOSLeft = -DBLE( SelfElElLeftUsual(ME2) ) - ME2*DBLE( DSelfElElLeft(ME2) + &
                        & DSelfElElRight(ME2) + 2D0*DSelfElElScalar(ME2) )
                end function dZElElOSLeft

                double precision function dZElElOSRight()
                    use constants
                    implicit none
                    double complex SelfElElRightUsual, DSelfElElLeft, DSelfElElRight, DSelfElElScalar
                    dZElElOSRight = -DBLE( SelfElElRightUsual(ME2) ) - ME2*DBLE( DSelfElElLeft(ME2) + &
                        & DSelfElElRight(ME2) + 2D0*DSelfElElScalar(ME2) )
                end function dZElElOSRight

                double precision function dZMuMuOSLeft()
                    use constants
                    implicit none
                    double complex SelfMuMuLeftUsual, DSelfMuMuLeft, DSelfMuMuRight, DSelfMuMuScalar
                    dZMuMuOSLeft = -DBLE( SelfMuMuLeftUsual(MM2) ) - MM2*DBLE( DSelfMuMuLeft(MM2) + &
                        & DSelfMuMuRight(MM2) + 2D0*DSelfMuMuScalar(MM2) )
                end function dZMuMuOSLeft

                double precision function dZMuMuOSRight()
                    use constants
                    implicit none
                    double complex SelfMuMuRightUsual, DSelfMuMuLeft, DSelfMuMuRight, DSelfMuMuScalar
                    dZMuMuOSRight = -DBLE( SelfMuMuRightUsual(MM2) ) - MM2*DBLE( DSelfMuMuLeft(MM2) + &
                        & DSelfMuMuRight(MM2) + 2D0*DSelfMuMuScalar(MM2) )
                end function dZMuMuOSRight

                double precision function dZTauTauOSLeft()
                    use constants
                    implicit none
                    double complex SelfTauTauLeftUsual, DSelfTauTauLeft, DSelfTauTauRight, DSelfTauTauScalar
                    dZTauTauOSLeft = -DBLE( SelfTauTauLeftUsual(ML2) ) - ML2*DBLE( DSelfTauTauLeft(ML2) + &
                        & DSelfTauTauRight(ML2) + 2D0*DSelfTauTauScalar(ML2) )
                end function dZTauTauOSLeft

                double precision function dZTauTauOSRight()
                    use constants
                    implicit none
                    double complex SelfTauTauRightUsual, DSelfTauTauLeft, DSelfTauTauRight, DSelfTauTauScalar
                    dZTauTauOSRight = -DBLE( SelfTauTauRightUsual(ML2) ) - ML2*DBLE( DSelfTauTauLeft(ML2) + &
                        & DSelfTauTauRight(ML2) + 2D0*DSelfTauTauScalar(ML2) )
                end function dZTauTauOSRight
                
                double precision function dZTauTauOSLeftWeak()
                    use constants
                    implicit none
                    double complex SelfTauTauLeftWeakUsual, DSelfTauTauLeftWeak, DSelfTauTauRightWeak, DSelfTauTauScalarWeak
                    dZTauTauOSLeftWeak = -DBLE( SelfTauTauLeftWeakUsual(ML2) ) - ML2*DBLE( DSelfTauTauLeftWeak(ML2) + &
                        & DSelfTauTauRightWeak(ML2) + 2D0*DSelfTauTauScalarWeak(ML2) )
                end function dZTauTauOSLeftWeak

                double precision function dZTauTauOSRightWeak()
                    use constants
                    implicit none
                    double complex SelfTauTauRightWeakUsual, DSelfTauTauLeftWeak, DSelfTauTauRightWeak, DSelfTauTauScalarWeak
                    dZTauTauOSRightWeak = -DBLE( SelfTauTauRightWeakUsual(ML2) ) - ML2*DBLE( DSelfTauTauLeftWeak(ML2) + &
                        & DSelfTauTauRightWeak(ML2) + 2D0*DSelfTauTauScalarWeak(ML2) )
                end function dZTauTauOSRightWeak

                double precision function dZDDOSLeft()
                    use constants
                    implicit none
                    double complex SelfDDLeftUsual, DSelfDDLeft, DSelfDDRight, DSelfDDScalar
                    dZDDOSLeft = -DBLE( SelfDDLeftUsual(MD2) ) - MD2*DBLE( DSelfDDLeft(MD2) + &
                        & DSelfDDRight(MD2) + 2D0*DSelfDDScalar(MD2) )
                end function dZDDOSLeft

                double precision function dZDDOSRight()
                    use constants
                    implicit none
                    double complex SelfDDRightUsual, DSelfDDLeft, DSelfDDRight, DSelfDDScalar
                    dZDDOSRight = -DBLE( SelfDDRightUsual(MD2) ) - MD2*DBLE( DSelfDDLeft(MD2) + &
                        & DSelfDDRight(MD2) + 2D0*DSelfDDScalar(MD2) )
                end function dZDDOSRight

                double precision function dZUUOSLeft()
                    use constants
                    implicit none
                    double complex SelfUULeftUsual, DSelfUULeft, DSelfUURight, DSelfUUScalar
                    dZUUOSLeft = -DBLE( SelfUULeftUsual(MU2) ) - MU2*DBLE( DSelfUULeft(MU2) + &
                        & DSelfUURight(MU2) + 2D0*DSelfUUScalar(MU2) )
                end function dZUUOSLeft

                double precision function dZUUOSRight()
                    use constants
                    implicit none
                    double complex SelfUURightUsual, DSelfUULeft, DSelfUURight, DSelfUUScalar
                    dZUUOSRight = -DBLE( SelfUURightUsual(MU2) ) - MU2*DBLE( DSelfUULeft(MU2) + &
                        & DSelfUURight(MU2) + 2D0*DSelfUUScalar(MU2) )
                end function dZUUOSRight

                double precision function dZSSOSLeft()
                    use constants
                    implicit none
                    double complex SelfSSLeftUsual, DSelfSSLeft, DSelfSSRight, DSelfSSScalar
                    dZSSOSLeft = -DBLE( SelfSSLeftUsual(MS2) ) - MS2*DBLE( DSelfSSLeft(MS2) + &
                        & DSelfSSRight(MS2) + 2D0*DSelfSSScalar(MS2) )
                end function dZSSOSLeft

                double precision function dZSSOSRight()
                    use constants
                    implicit none
                    double complex SelfSSRightUsual, DSelfSSLeft, DSelfSSRight, DSelfSSScalar
                    dZSSOSRight = -DBLE( SelfSSRightUsual(MS2) ) - MS2*DBLE( DSelfSSLeft(MS2) + &
                        & DSelfSSRight(MS2) + 2D0*DSelfSSScalar(MS2) )
                end function dZSSOSRight

                double precision function dZCCOSLeft()
                    use constants
                    implicit none
                    double complex SelfCCLeftUsual, DSelfCCLeft, DSelfCCRight, DSelfCCScalar
                    dZCCOSLeft = -DBLE( SelfCCLeftUsual(MC2) ) - MC2*DBLE( DSelfCCLeft(MC2) + &
                        & DSelfCCRight(MC2) + 2D0*DSelfCCScalar(MC2) )
                end function dZCCOSLeft

                double precision function dZCCOSRight()
                    use constants
                    implicit none
                    double complex SelfCCRightUsual, DSelfCCLeft, DSelfCCRight, DSelfCCScalar
                    dZCCOSRight = -DBLE( SelfCCRightUsual(MC2) ) - MC2*DBLE( DSelfCCLeft(MC2) + &
                        & DSelfCCRight(MC2) + 2D0*DSelfCCScalar(MC2) )
                end function dZCCOSRight

                double precision function dZBBOSLeft()
                    use constants
                    implicit none
                    double complex SelfBBLeftUsual, DSelfBBLeft, DSelfBBRight, DSelfBBScalar
                    dZBBOSLeft = -DBLE( SelfBBLeftUsual(MB2) ) - MB2*DBLE( DSelfBBLeft(MB2) + &
                        & DSelfBBRight(MB2) + 2D0*DSelfBBScalar(MB2) )
                end function dZBBOSLeft

                double precision function dZBBOSRight()
                    use constants
                    implicit none
                    double complex SelfBBRightUsual, DSelfBBLeft, DSelfBBRight, DSelfBBScalar
                    dZBBOSRight = -DBLE( SelfBBRightUsual(MB2) ) - MB2*DBLE( DSelfBBLeft(MB2) + &
                        & DSelfBBRight(MB2) + 2D0*DSelfBBScalar(MB2) )
                end function dZBBOSRight

                double precision function dZTTOSLeft()
                    use constants
                    implicit none
                    double complex SelfTTLeftUsual, DSelfTTLeft, DSelfTTRight, DSelfTTScalar
                    dZTTOSLeft = -DBLE( SelfTTLeftUsual(MT2) ) - MT2*DBLE( DSelfTTLeft(MT2) + &
                        & DSelfTTRight(MT2) + 2D0*DSelfTTScalar(MT2) )
                end function dZTTOSLeft

                double precision function dZTTOSRight()
                    use constants
                    implicit none
                    double complex SelfTTRightUsual, DSelfTTLeft, DSelfTTRight, DSelfTTScalar
                    dZTTOSRight = -DBLE( SelfTTRightUsual(MT2) ) - MT2*DBLE( DSelfTTLeft(MT2) + &
                        & DSelfTTRight(MT2) + 2D0*DSelfTTScalar(MT2) )
                end function dZTTOSRight

                double precision function dZCTOSLeft()
                    use constants
                    implicit none
                    double complex SelfCTLeftUsual, SelfCTRightUsual, SelfCTScalarUsual
                    dZCTOSLeft = 2D0/( MC2 - MT2 )*DBLE( &
                            & MT2*SelfCTLeftUsual(MT2) + MC*MT*SelfCTRightUsual(MT2) + &
                            & (MC2 + MT2)*SelfCTScalarUsual(MT2) &
                        & )
                end function dZCTOSLeft

                double precision function dZCTOSRight()
                    use constants
                    implicit none
                    double complex SelfCTLeftUsual, SelfCTRightUsual, SelfCTScalarUsual
                    dZCTOSRight = 2D0/( MC2 - MT2 )*DBLE( &
                            & MT2*SelfCTRightUsual(MT2) + MC*MT*SelfCTLeftUsual(MT2) + &
                            & (2D0*MC*MT)*SelfCTScalarUsual(MT2) &
                        & )
                end function dZCTOSRight

                double precision function dZTCOSLeft()
                    use constants
                    implicit none
                    double complex SelfCTLeftUsual, SelfCTRightUsual, SelfCTScalarUsual
                    dZTCOSLeft = 2D0/( MT2 - MC2 )*DBLE( &
                            & MC2*SelfCTLeftUsual(MC2) + MC*MT*SelfCTRightUsual(MC2) + &
                            & (MC2 + MT2)*SelfCTScalarUsual(MC2) &
                        & )
                end function dZTCOSLeft

                double precision function dZTCOSRight()
                    use constants
                    implicit none
                    double complex SelfCTLeftUsual, SelfCTRightUsual, SelfCTScalarUsual
                    dZTCOSRight = 2D0/( MT2 - MC2 )*DBLE( &
                            & MC2*SelfCTRightUsual(MC2) + MC*MT*SelfCTLeftUsual(MC2) + &
                            & (2D0*MC*MT)*SelfCTScalarUsual(MC2) &
                        & )
                end function dZTCOSRight

                double precision function dZUTOSLeft()
                    use constants
                    implicit none
                    double complex SelfUTLeftUsual, SelfUTRightUsual, SelfUTScalarUsual
                    dZUTOSLeft = 2D0/( MU2 - MT2 )*DBLE( &
                            & MT2*SelfUTLeftUsual(MT2) + MU*MT*SelfUTRightUsual(MT2) + &
                            & (MU2 + MT2)*SelfUTScalarUsual(MT2) &
                        & )
                end function dZUTOSLeft

                double precision function dZUTOSRight()
                    use constants
                    implicit none
                    double complex SelfUTLeftUsual, SelfUTRightUsual, SelfUTScalarUsual
                    dZUTOSRight = 2D0/( MU2 - MT2 )*DBLE( &
                            & MT2*SelfUTRightUsual(MT2) + MU*MT*SelfUTLeftUsual(MT2) + &
                            & (2D0*MU*MT)*SelfUTScalarUsual(MT2) &
                        & )
                end function dZUTOSRight

                double precision function dZTUOSLeft()
                    use constants
                    implicit none
                    double complex SelfUTLeftUsual, SelfUTRightUsual, SelfUTScalarUsual
                    dZTUOSLeft = 2D0/( MT2 - MU2 )*DBLE( &
                            & MU2*SelfUTLeftUsual(MU2) + MU*MT*SelfUTRightUsual(MU2) + &
                            & (MU2 + MT2)*SelfUTScalarUsual(MU2) &
                        & )
                end function dZTUOSLeft

                double precision function dZTUOSRight()
                    use constants
                    implicit none
                    double complex SelfUTLeftUsual, SelfUTRightUsual, SelfUTScalarUsual
                    dZTUOSRight = 2D0/( MT2 - MU2 )*DBLE( &
                            & MU2*SelfUTRightUsual(MU2) + MU*MT*SelfUTLeftUsual(MU2) + &
                            & (2D0*MU*MT)*SelfUTScalarUsual(MU2) &
                        & )
                end function dZTUOSRight

                double precision function dZSBOSLeft()
                    use constants
                    implicit none
                    double complex SelfSBLeftUsual, SelfSBRightUsual, SelfSBScalarUsual
                    dZSBOSLeft = 2D0/( MS2 - MB2 )*DBLE( &
                            & MB2*SelfSBLeftUsual(MB2) + MS*MB*SelfSBRightUsual(MB2) + &
                            & (MS2 + MB2)*SelfSBScalarUsual(MB2) &
                        & )
                end function dZSBOSLeft

                double precision function dZSBOSRight()
                    use constants
                    implicit none
                    double complex SelfSBLeftUsual, SelfSBRightUsual, SelfSBScalarUsual
                    dZSBOSRight = 2D0/( MS2 - MB2 )*DBLE( &
                            & MB2*SelfSBRightUsual(MB2) + MS*MB*SelfSBLeftUsual(MB2) + &
                            & (2D0*MS*MB)*SelfSBScalarUsual(MB2) &
                        & )
                end function dZSBOSRight

                double precision function dZBSOSLeft()
                    use constants
                    implicit none
                    double complex SelfSBLeftUsual, SelfSBRightUsual, SelfSBScalarUsual
                    dZBSOSLeft = 2D0/( MB2 - MS2 )*DBLE( &
                            & MS2*SelfSBLeftUsual(MS2) + MS*MB*SelfSBRightUsual(MS2) + &
                            & (MS2 + MB2)*SelfSBScalarUsual(MS2) &
                        & )
                end function dZBSOSLeft

                double precision function dZBSOSRight()
                    use constants
                    implicit none
                    double complex SelfSBLeftUsual, SelfSBRightUsual, SelfSBScalarUsual
                    dZBSOSRight = 2D0/( MB2 - MS2 )*DBLE( &
                            & MS2*SelfSBRightUsual(MS2) + MS*MB*SelfSBLeftUsual(MS2) + &
                            & (2D0*MS*MB)*SelfSBScalarUsual(MS2) &
                        & )
                end function dZBSOSRight

                double precision function dZDBOSLeft()
                    use constants
                    implicit none
                    double complex SelfDBLeftUsual, SelfDBRightUsual, SelfDBScalarUsual
                    dZDBOSLeft = 2D0/( MD2 - MB2 )*DBLE( &
                            & MB2*SelfDBLeftUsual(MB2) + MD*MB*SelfDBRightUsual(MB2) + &
                            & (MD2 + MB2)*SelfDBScalarUsual(MB2) &
                        & )
                end function dZDBOSLeft

                double precision function dZDBOSRight()
                    use constants
                    implicit none
                    double complex SelfDBLeftUsual, SelfDBRightUsual, SelfDBScalarUsual
                    dZDBOSRight = 2D0/( MD2 - MB2 )*DBLE( &
                            & MB2*SelfDBRightUsual(MB2) + MD*MB*SelfDBLeftUsual(MB2) + &
                            & (2D0*MD*MB)*SelfDBScalarUsual(MB2) &
                        & )
                end function dZDBOSRight

                double precision function dZBDOSLeft()
                    use constants
                    implicit none
                    double complex SelfDBLeftUsual, SelfDBRightUsual, SelfDBScalarUsual
                    dZBDOSLeft = 2D0/( MB2 - MD2 )*DBLE( &
                            & MD2*SelfDBLeftUsual(MD2) + MD*MB*SelfDBRightUsual(MD2) + &
                            & (MD2 + MB2)*SelfDBScalarUsual(MD2) &
                        & )
                end function dZBDOSLeft

                double precision function dZBDOSRight()
                    use constants
                    implicit none
                    double complex SelfDBLeftUsual, SelfDBRightUsual, SelfDBScalarUsual
                    dZBDOSRight = 2D0/( MB2 - MD2 )*DBLE( &
                            & MD2*SelfDBRightUsual(MD2) + MD*MB*SelfDBLeftUsual(MD2) + &
                            & (2D0*MD*MB)*SelfDBScalarUsual(MD2) &
                        & )
                end function dZBDOSRight

                double precision function dZUCOSLeft()
                    use constants
                    implicit none
                    double complex SelfUCLeftUsual, SelfUCRightUsual, SelfUCScalarUsual
                    dZUCOSLeft = 2D0/( MU2 - MC2 )*DBLE( &
                            & MC2*SelfUCLeftUsual(MC2) + MC*MU*SelfUCRightUsual(MC2) + &
                            & (MC2 + MU2)*SelfUCScalarUsual(MC2) &
                        & )
                end function dZUCOSLeft

                double precision function dZUCOSRight()
                    use constants
                    implicit none
                    double complex SelfUCLeftUsual, SelfUCRightUsual, SelfUCScalarUsual
                    dZUCOSRight = 2D0/( MU2 - MC2 )*DBLE( &
                            & MC2*SelfUCRightUsual(MC2) + MC*MU*SelfUCLeftUsual(MC2) + &
                            & (2D0*MC*MU)*SelfUCScalarUsual(MC2) &
                        & )
                end function dZUCOSRight

                double precision function dZCUOSLeft()
                    use constants
                    implicit none
                    double complex SelfUCLeftUsual, SelfUCRightUsual, SelfUCScalarUsual
                    dZCUOSLeft = 2D0/( MC2 - MU2 )*DBLE( &
                            & MU2*SelfUCLeftUsual(MU2) + MC*MU*SelfUCRightUsual(MU2) + &
                            & (MC2 + MU2)*SelfUCScalarUsual(MU2) &
                        & )
                end function dZCUOSLeft

                double precision function dZCUOSRight()
                    use constants
                    implicit none
                    double complex SelfUCLeftUsual, SelfUCRightUsual, SelfUCScalarUsual
                    dZCUOSRight = 2D0/( MC2 - MU2 )*DBLE( &
                            & MU2*SelfUCRightUsual(MU2) + MC*MU*SelfUCLeftUsual(MU2) + &
                            & (2D0*MC*MU)*SelfUCScalarUsual(MU2) &
                        & )
                end function dZCUOSRight

                double precision function dZDSOSLeft()
                    use constants
                    implicit none
                    double complex SelfDSLeftUsual, SelfDSRightUsual, SelfDSScalarUsual
                    dZDSOSLeft = 2D0/( MD2 - MS2 )*DBLE( &
                            & MS2*SelfDSLeftUsual(MS2) + MS*MD*SelfDSRightUsual(MS2) + &
                            & (MS2 + MD2)*SelfDSScalarUsual(MS2) &
                        & )
                end function dZDSOSLeft

                double precision function dZDSOSRight()
                    use constants
                    implicit none
                    double complex SelfDSLeftUsual, SelfDSRightUsual, SelfDSScalarUsual
                    dZDSOSRight = 2D0/( MD2 - MS2 )*DBLE( &
                            & MS2*SelfDSRightUsual(MS2) + MS*MD*SelfDSLeftUsual(MS2) + &
                            & (2D0*MS*MD)*SelfDSScalarUsual(MS2) &
                        & )
                end function dZDSOSRight

                double precision function dZSDOSLeft()
                    use constants
                    implicit none
                    double complex SelfDSLeftUsual, SelfDSRightUsual, SelfDSScalarUsual
                    dZSDOSLeft = 2D0/( MS2 - MD2 )*DBLE( &
                            & MD2*SelfDSLeftUsual(MD2) + MS*MD*SelfDSRightUsual(MD2) + &
                            & (MS2 + MD2)*SelfDSScalarUsual(MD2) &
                        & )
                end function dZSDOSLeft

                double precision function dZSDOSRight()
                    use constants
                    implicit none
                    double complex SelfDSLeftUsual, SelfDSRightUsual, SelfDSScalarUsual
                    dZSDOSRight = 2D0/( MS2 - MD2 )*DBLE( &
                            & MD2*SelfDSRightUsual(MD2) + MS*MD*SelfDSLeftUsual(MD2) + &
                            & (2D0*MS*MD)*SelfDSScalarUsual(MD2) &
                        & )
                end function dZSDOSRight



        ! CKM renormalization
            ! Yamada's scheme (hep-ph/0103046)
                double precision function dCKM11Yamada()
                    use constants
                    implicit none 
                    double precision GaugeXiATemp, GaugeXiWTemp, GaugeXiZTemp
                    GaugeXiATemp = GaugeXiA
                    GaugeXiWTemp = GaugeXiW
                    GaugeXiZTemp = GaugeXiZ
                    GaugeXiA = 1D0
                    GaugeXiW = 1D0
                    GaugeXiZ = 1D0

                    dCKM11Yamada = 1D0/4D0*( &
                        & ( dZUTOSLeft() - dZTUOSLeft() )*CKM31 + &
                        & ( dZUCOSLeft() - dZCUOSLeft() )*CKM21 - &
                        & ( dZBDOSLeft() - dZDBOSLeft() )*CKM13 - &
                        & ( dZSDOSLeft() - dZDSOSLeft() )*CKM12 &
                    & )

                    GaugeXiA = GaugeXiATemp
                    GaugeXiW = GaugeXiWTemp
                    GaugeXiZ = GaugeXiZTemp

                end function dCKM11Yamada

                double precision function dCKM12Yamada()
                    use constants
                    implicit none 
                    double precision GaugeXiATemp, GaugeXiWTemp, GaugeXiZTemp
                    GaugeXiATemp = GaugeXiA
                    GaugeXiWTemp = GaugeXiW
                    GaugeXiZTemp = GaugeXiZ
                    GaugeXiA = 1D0
                    GaugeXiW = 1D0
                    GaugeXiZ = 1D0

                    dCKM12Yamada = 1D0/4D0*( &
                        & ( dZUTOSLeft() - dZTUOSLeft() )*CKM32 + &
                        & ( dZUCOSLeft() - dZCUOSLeft() )*CKM22 - &
                        & ( dZBSOSLeft() - dZSBOSLeft() )*CKM13 - &
                        & ( dZDSOSLeft() - dZSDOSLeft() )*CKM11 &
                    & )

                    GaugeXiA = GaugeXiATemp
                    GaugeXiW = GaugeXiWTemp
                    GaugeXiZ = GaugeXiZTemp

                end function dCKM12Yamada

                double precision function dCKM13Yamada()
                    use constants
                    implicit none 
                    double precision GaugeXiATemp, GaugeXiWTemp, GaugeXiZTemp
                    GaugeXiATemp = GaugeXiA
                    GaugeXiWTemp = GaugeXiW
                    GaugeXiZTemp = GaugeXiZ
                    GaugeXiA = 1D0
                    GaugeXiW = 1D0
                    GaugeXiZ = 1D0

                    dCKM13Yamada = 1D0/4D0*( &
                        & ( dZUTOSLeft() - dZTUOSLeft() )*CKM33 + &
                        & ( dZUCOSLeft() - dZCUOSLeft() )*CKM23 - &
                        & ( dZSBOSLeft() - dZBSOSLeft() )*CKM12 - &
                        & ( dZDBOSLeft() - dZBDOSLeft() )*CKM11 &
                    & )

                    GaugeXiA = GaugeXiATemp
                    GaugeXiW = GaugeXiWTemp
                    GaugeXiZ = GaugeXiZTemp

                end function dCKM13Yamada

                double precision function dCKM21Yamada()
                    use constants
                    implicit none 
                    double precision GaugeXiATemp, GaugeXiWTemp, GaugeXiZTemp
                    GaugeXiATemp = GaugeXiA
                    GaugeXiWTemp = GaugeXiW
                    GaugeXiZTemp = GaugeXiZ
                    GaugeXiA = 1D0
                    GaugeXiW = 1D0
                    GaugeXiZ = 1D0

                    dCKM21Yamada = 1D0/4D0*( &
                        & ( dZCTOSLeft() - dZTCOSLeft() )*CKM31 + &
                        & ( dZCUOSLeft() - dZUCOSLeft() )*CKM11 - &
                        & ( dZBDOSLeft() - dZDBOSLeft() )*CKM23 - &
                        & ( dZSDOSLeft() - dZDSOSLeft() )*CKM22 &
                    & )

                    GaugeXiA = GaugeXiATemp
                    GaugeXiW = GaugeXiWTemp
                    GaugeXiZ = GaugeXiZTemp

                end function dCKM21Yamada

                double precision function dCKM22Yamada()
                    use constants
                    implicit none 
                    double precision GaugeXiATemp, GaugeXiWTemp, GaugeXiZTemp
                    GaugeXiATemp = GaugeXiA
                    GaugeXiWTemp = GaugeXiW
                    GaugeXiZTemp = GaugeXiZ
                    GaugeXiA = 1D0
                    GaugeXiW = 1D0
                    GaugeXiZ = 1D0

                    dCKM22Yamada = 1D0/4D0*( &
                        & ( dZCUOSLeft() - dZUCOSLeft() )*CKM12 + &
                        & ( dZCTOSLeft() - dZCTOSLeft() )*CKM32 - &
                        & ( dZDSOSLeft() - dZSDOSLeft() )*CKM21 - &
                        & ( dZBSOSLeft() - dZSBOSLeft() )*CKM23 &
                    & )

                    GaugeXiA = GaugeXiATemp
                    GaugeXiW = GaugeXiWTemp
                    GaugeXiZ = GaugeXiZTemp

                end function dCKM22Yamada

                double precision function dCKM23Yamada()
                    use constants
                    implicit none 
                    double precision GaugeXiATemp, GaugeXiWTemp, GaugeXiZTemp
                    GaugeXiATemp = GaugeXiA
                    GaugeXiWTemp = GaugeXiW
                    GaugeXiZTemp = GaugeXiZ
                    GaugeXiA = 1D0
                    GaugeXiW = 1D0
                    GaugeXiZ = 1D0

                    dCKM23Yamada = 1D0/4D0*( &
                        & ( dZCTOSLeft() - dZTCOSLeft() )*CKM33 + &
                        & ( dZCUOSLeft() - dZUCOSLeft() )*CKM13 - &
                        & ( dZSBOSLeft() - dZBSOSLeft() )*CKM22 - &
                        & ( dZDBOSLeft() - dZBDOSLeft() )*CKM21 &
                    & )

                    GaugeXiA = GaugeXiATemp
                    GaugeXiW = GaugeXiWTemp
                    GaugeXiZ = GaugeXiZTemp

                end function dCKM23Yamada

                double precision function dCKM31Yamada()
                    use constants
                    implicit none 
                    double precision GaugeXiATemp, GaugeXiWTemp, GaugeXiZTemp
                    GaugeXiATemp = GaugeXiA
                    GaugeXiWTemp = GaugeXiW
                    GaugeXiZTemp = GaugeXiZ
                    GaugeXiA = 1D0
                    GaugeXiW = 1D0
                    GaugeXiZ = 1D0

                    dCKM31Yamada = 1D0/4D0*( &
                        & ( dZTCOSLeft() - dZCTOSLeft() )*CKM21 + &
                        & ( dZTUOSLeft() - dZUTOSLeft() )*CKM11 - &
                        & ( dZBDOSLeft() - dZDBOSLeft() )*CKM33 - &
                        & ( dZSDOSLeft() - dZDSOSLeft() )*CKM32 &
                    & )

                    GaugeXiA = GaugeXiATemp
                    GaugeXiW = GaugeXiWTemp
                    GaugeXiZ = GaugeXiZTemp

                end function dCKM31Yamada

                double precision function dCKM32Yamada()
                    use constants
                    implicit none 
                    double precision GaugeXiATemp, GaugeXiWTemp, GaugeXiZTemp
                    GaugeXiATemp = GaugeXiA
                    GaugeXiWTemp = GaugeXiW
                    GaugeXiZTemp = GaugeXiZ
                    GaugeXiA = 1D0
                    GaugeXiW = 1D0
                    GaugeXiZ = 1D0

                    dCKM32Yamada = 1D0/4D0*( &
                        & ( dZTCOSLeft() - dZCTOSLeft() )*CKM22 + &
                        & ( dZTUOSLeft() - dZUTOSLeft() )*CKM12 - &
                        & ( dZBSOSLeft() - dZSBOSLeft() )*CKM33 - &
                        & ( dZDSOSLeft() - dZSDOSLeft() )*CKM31 &
                    & )

                    GaugeXiA = GaugeXiATemp
                    GaugeXiW = GaugeXiWTemp
                    GaugeXiZ = GaugeXiZTemp

                end function dCKM32Yamada

                double precision function dCKM33Yamada()
                    use constants
                    implicit none 
                    double precision GaugeXiATemp, GaugeXiWTemp, GaugeXiZTemp
                    GaugeXiATemp = GaugeXiA
                    GaugeXiWTemp = GaugeXiW
                    GaugeXiZTemp = GaugeXiZ
                    GaugeXiA = 1D0
                    GaugeXiW = 1D0
                    GaugeXiZ = 1D0

                    dCKM33Yamada = 1D0/4D0*( &
                        & ( dZTUOSLeft() - dZUTOSLeft() )*CKM13 + &
                        & ( dZTCOSLeft() - dZCTOSLeft() )*CKM23 - &
                        & ( dZDBOSLeft() - dZBDOSLeft() )*CKM31 - &
                        & ( dZSBOSLeft() - dZBSOSLeft() )*CKM32 &
                    & )

                    GaugeXiA = GaugeXiATemp
                    GaugeXiW = GaugeXiWTemp
                    GaugeXiZ = GaugeXiZTemp

                end function dCKM33Yamada

        ! Mixing angle counterterms
            ! Alternative tadpole based counterterms
                double precision function dAlphaKanAlter()
                    use constants
                    implicit none
                    dAlphaKanAlter = ( dZHHh0OSAlter() - dZh0HHOSAlter() )/4.D0
                end function dAlphaKanAlter

                double precision function dBeta1KanAlter()
                    use constants
                    implicit none
                    dBeta1KanAlter = ( dZG0A0OSAlter() - dZA0G0OSAlter() )/4.D0
                end function dBeta1KanAlter

                double precision function dBeta2KanAlter()
                    use constants
                    implicit none
                    dBeta2KanAlter = ( dZGpHpOSAlter() - dZHpGpOSAlter() )/4.D0
                end function dBeta2KanAlter

                double precision function dAlphaPinchOS()
                    use constants
                    implicit none
                    double complex SelfHHh0Alter, SelfHHh0Add
                    double precision GaugeXiATemp, GaugeXiWTemp, GaugeXiZTemp
                    GaugeXiATemp = GaugeXiA
                    GaugeXiWTemp = GaugeXiW
                    GaugeXiZTemp = GaugeXiZ
                    GaugeXiA = 1D0
                    GaugeXiW = 1D0
                    GaugeXiZ = 1D0
                    dAlphaPinchOS = ( DBLE(SelfHHh0Alter(MHH2)) + DBLE(SelfHHh0Alter(Mh02)) + DBLE(SelfHHh0Add(Mh02)) + &
                                    & DBLE(SelfHHh0Add(MHH2)) )/( 2.D0*(MHH2 - Mh02) )
                    GaugeXiA = GaugeXiATemp
                    GaugeXiW = GaugeXiWTemp
                    GaugeXiZ = GaugeXiZTemp
                end function dAlphaPinchOS

                double precision function dBeta1PinchOS()
                    use constants
                    implicit none
                    double complex SelfG0A0Alter, SelfG0A0Add
                    double precision GaugeXiATemp, GaugeXiWTemp, GaugeXiZTemp
                    GaugeXiATemp = GaugeXiA
                    GaugeXiWTemp = GaugeXiW
                    GaugeXiZTemp = GaugeXiZ
                    GaugeXiA = 1D0
                    GaugeXiW = 1D0
                    GaugeXiZ = 1D0
                    dBeta1PinchOS = -( DBLE(SelfG0A0Alter(MA02)) + DBLE(SelfG0A0Alter(0.D0)) + DBLE(SelfG0A0Add(MA02)) + &
                                    & DBLE(SelfG0A0Add(0.D0)) )/( 2.D0*MA02 )
                    GaugeXiA = GaugeXiATemp
                    GaugeXiW = GaugeXiWTemp
                    GaugeXiZ = GaugeXiZTemp
                end function dBeta1PinchOS

                double precision function dBeta2PinchOS()
                    use constants
                    implicit none
                    double complex SelfGpHpAlter, SelfGpHpAdd
                    double precision GaugeXiATemp, GaugeXiWTemp, GaugeXiZTemp
                    GaugeXiATemp = GaugeXiA
                    GaugeXiWTemp = GaugeXiW
                    GaugeXiZTemp = GaugeXiZ
                    GaugeXiA = 1D0
                    GaugeXiW = 1D0
                    GaugeXiZ = 1D0
                    dBeta2PinchOS = -( DBLE(SelfGpHpAlter(MHp2)) + DBLE(SelfGpHpAlter(0.D0)) + DBLE(SelfGpHpAdd(MHp2)) + &
                                    & DBLE(SelfGpHpAdd(0.D0)) )/( 2.D0*MHp2 )
                    GaugeXiA = GaugeXiATemp
                    GaugeXiW = GaugeXiWTemp
                    GaugeXiZ = GaugeXiZTemp
                end function dBeta2PinchOS

                double precision function dAlphaPinchPStar()
                    use constants
                    implicit none
                    double complex SelfHHh0Alter
                    double precision GaugeXiATemp, GaugeXiWTemp, GaugeXiZTemp
                    GaugeXiATemp = GaugeXiA
                    GaugeXiWTemp = GaugeXiW
                    GaugeXiZTemp = GaugeXiZ
                    GaugeXiA = 1D0
                    GaugeXiW = 1D0
                    GaugeXiZ = 1D0
                    dAlphaPinchPStar = ( DBLE(SelfHHh0Alter((MHH2 + Mh02)/2.D0)) )/( MHH2 - Mh02 )
                    GaugeXiA = GaugeXiATemp
                    GaugeXiW = GaugeXiWTemp
                    GaugeXiZ = GaugeXiZTemp
                end function dAlphaPinchPStar

                double precision function dBeta1PinchPStar()
                    use constants
                    implicit none
                    double complex SelfG0A0Alter
                    double precision GaugeXiATemp, GaugeXiWTemp, GaugeXiZTemp
                    GaugeXiATemp = GaugeXiA
                    GaugeXiWTemp = GaugeXiW
                    GaugeXiZTemp = GaugeXiZ
                    GaugeXiA = 1D0
                    GaugeXiW = 1D0
                    GaugeXiZ = 1D0
                    dBeta1PinchPStar = -( DBLE(SelfG0A0Alter(MA02/2.D0)) )/( MA02 )
                    GaugeXiA = GaugeXiATemp
                    GaugeXiW = GaugeXiWTemp
                    GaugeXiZ = GaugeXiZTemp
                end function dBeta1PinchPStar

                double precision function dBeta2PinchPStar()
                    use constants
                    implicit none
                    double complex SelfGpHpAlter
                    double precision GaugeXiATemp, GaugeXiWTemp, GaugeXiZTemp
                    GaugeXiATemp = GaugeXiA
                    GaugeXiWTemp = GaugeXiW
                    GaugeXiZTemp = GaugeXiZ
                    GaugeXiA = 1D0
                    GaugeXiW = 1D0
                    GaugeXiZ = 1D0
                    dBeta2PinchPStar = -( DBLE(SelfGpHpAlter(MHp2/2.D0)) )/( MHp2 )
                    GaugeXiA = GaugeXiATemp
                    GaugeXiW = GaugeXiWTemp
                    GaugeXiZ = GaugeXiZTemp
                end function dBeta2PinchPStar

                double precision function dBetaProcDep1Alter()
                    use constants
                    implicit none
                    double complex A0toTauPTauMProcDepVC
                    dBetaProcDep1Alter = -Yuk6/(1 + Yuk6**2)*( A0toTauPTauMProcDepVC() + dgAtMZ()/(EL/SW) + dMLOSAlterWeak()/ML -&
                        & dMW2Alter()/(2D0*MW2) + dZA0A0OS()/2D0 - dZG0A0OSAlter()/(2D0*Yuk6) + dZTauTauOSLeftWeak()/2D0 + &
                        & dZTauTauOSRightWeak()/2D0 )
                end function dBetaProcDep1Alter

                double precision function dAlphaProcDep1Alter()
                    use constants
                    implicit none
                    double complex HHtoTauPTauMProcDepVC
                    dAlphaProcDep1Alter = -Yuk5/Yuk4*( HHtoTauPTauMProcDepVC() + dgAtMZ()/(EL/SW) + dMLOSAlterWeak()/ML - &
                        & dMW2Alter()/(2D0*MW2) + Yuk6*dBetaProcDep1Alter() + dZHHHHOS()/2D0 + Yuk4*dZh0HHOSAlter()/(2D0*Yuk5) + &
                        & dZTauTauOSLeftWeak()/2D0 + dZTauTauOSRightWeak()/2D0 )
                end function dAlphaProcDep1Alter

                double precision function dBetaProcDep2Alter()
                    use constants
                    implicit none
                    double complex A0toTauPTauMProcDepVC
                    dBetaProcDep2Alter = -Yuk6/(1 + Yuk6**2)*( A0toTauPTauMProcDepVC() + dgAtMZ()/(EL/SW) + dMLOSAlterWeak()/ML -&
                        & dMW2Alter()/(2D0*MW2) + dZA0A0OS()/2D0 - dZG0A0OSAlter()/(2D0*Yuk6) + dZTauTauOSLeftWeak()/2D0 + &
                        & dZTauTauOSRightWeak()/2D0 )
                end function dBetaProcDep2Alter

                double precision function dAlphaProcDep2Alter()
                    use constants
                    implicit none
                    double complex h0toTauPTauMProcDepVC
                    dAlphaProcDep2Alter = Yuk4/Yuk5*( h0toTauPTauMProcDepVC() + dgAtMZ()/(EL/SW) + dMLOSAlterWeak()/ML - &
                        & dMW2Alter()/(2D0*MW2) + Yuk6*dBetaProcDep2Alter() + dZh0h0OS()/2D0 + Yuk5*dZHHh0OSAlter()/(2D0*Yuk4) + &
                        & dZTauTauOSLeftWeak()/2D0 + dZTauTauOSRightWeak()/2D0 )
                end function dAlphaProcDep2Alter

                double precision function dBetaProcDep3Alter()
                    use constants
                    implicit none
                    double complex h0toTauPTauMProcDepVC, HHtoTauPTauMProcDepVC
                    dBetaProcDep3Alter = -1D0/(Yuk6*(Yuk4**2 + Yuk5**2))*( Yuk4*Yuk5*(dZHHh0OSAlter()/2D0 + dZh0HHOSAlter()/2D0) &
                        & + Yuk4**2*( h0toTauPTauMProcDepVC() + dZh0h0OS()/2D0 ) &
                        & + Yuk5**2*( HHtoTauPTauMProcDepVC() + dZHHHHOS()/2D0 ) &
                        & + (Yuk4**2 + Yuk5**2)*( dgAtMZ()/(EL/SW) + dMLOSAlterWeak()/ML - dMW2Alter()/(2D0*MW2) + &
                        & dZTauTauOSLeftWeak()/2D0 + dZTauTauOSRightWeak()/2D0 ) )
                end function dBetaProcDep3Alter

                double precision function dAlphaProcDep3Alter()
                    use constants
                    implicit none
                    double complex h0toTauPTauMProcDepVC, HHtoTauPTauMProcDepVC
                    dAlphaProcDep3Alter = Yuk4*Yuk5/(Yuk4**2 + Yuk5**2) * ( h0toTauPTauMProcDepVC() - HHtoTauPTauMProcDepVC() + &
                        & dZh0h0OS()/2D0 - dZHHHHOS()/2D0 + Yuk5/Yuk4*dZHHh0OSAlter()/2D0 - Yuk4/Yuk5*dZh0HHOSAlter()/2D0 )
                end function dAlphaProcDep3Alter

                double precision function dAlphaOS1Alter()
                    use constants
                    implicit none
                    double complex h0toN1N1ProcDepRelVC, HHtoN1N1ProcDepRelVC
                    dAlphaOS1Alter = ( HHtoN1N1ProcDepRelVC() - h0toN1N1ProcDepRelVC() )*SA*CA + &
                                    & SA*CA/2D0*( dZHHHHOS() - dZh0h0OS() ) + 1D0/2D0*( CA2*dZHHh0OSAlter() - SA2*dZh0HHOSAlter() )
                end function dAlphaOS1Alter

                double precision function dBetaOS1Alter()
                    use constants
                    implicit none
                    double complex h0toN1N1ProcDepRelVC, HHtoN1N1ProcDepRelVC, A0toN1N1ProcDepRelVC
                    double precision GaugeXiATemp, GaugeXiWTemp, GaugeXiZTemp
                    GaugeXiATemp = GaugeXiA
                    GaugeXiWTemp = GaugeXiW
                    GaugeXiZTemp = GaugeXiZ
                    GaugeXiA = 1D0
                    GaugeXiW = 1D0
                    GaugeXiZ = 1D0
                    dBetaOS1Alter = SB/CB*( CA2*HHtoN1N1ProcDepRelVC() + SA2*h0toN1N1ProcDepRelVC() - A0toN1N1ProcDepRelVC() - &
                            & ( dZA0A0OS() - CA2*dZHHHHOS() - SA2*dZh0h0OS() + SA*CA*( dZHHh0OSAlter() + dZh0HHOSAlter() ) )/2D0 )&
                            & + dZG0A0OSAlter()/2D0
                    GaugeXiA = GaugeXiATemp
                    GaugeXiW = GaugeXiWTemp
                    GaugeXiZ = GaugeXiZTemp
                end function dBetaOS1Alter

                double precision function dAlphaOS2Alter()
                    use constants
                    implicit none
                    double complex h0toN2N2ProcDepRelVC, HHtoN2N2ProcDepRelVC
                    dAlphaOS2Alter = ( h0toN2N2ProcDepRelVC() - HHtoN2N2ProcDepRelVC() )*SA*CA + &
                                    & SA*CA/2D0*( dZh0h0OS() - dZHHHHOS() ) + 1D0/2D0*( SA2*dZHHh0OSAlter() - CA2*dZh0HHOSAlter() )
                end function dAlphaOS2Alter

                double precision function dBetaOS2Alter()
                    use constants
                    implicit none
                    double complex h0toN2N2ProcDepRelVC, HHtoN2N2ProcDepRelVC, A0toN2N2ProcDepRelVC
                    double precision GaugeXiATemp, GaugeXiWTemp, GaugeXiZTemp
                    GaugeXiATemp = GaugeXiA
                    GaugeXiWTemp = GaugeXiW
                    GaugeXiZTemp = GaugeXiZ
                    GaugeXiA = 1D0
                    GaugeXiW = 1D0
                    GaugeXiZ = 1D0
                    dBetaOS2Alter = CB/SB*( A0toN2N2ProcDepRelVC() - SA2*HHtoN2N2ProcDepRelVC() - CA2*h0toN2N2ProcDepRelVC() + &
                            & ( dZA0A0OS() - SA2*dZHHHHOS() - CA2*dZh0h0OS() - SA*CA*( dZHHh0OSAlter() + dZh0HHOSAlter() ) )/2D0 )&
                            & + dZG0A0OSAlter()/2D0
                    GaugeXiA = GaugeXiATemp
                    GaugeXiW = GaugeXiWTemp
                    GaugeXiZ = GaugeXiZTemp
                end function dBetaOS2Alter

                double precision function dAlphaOS12Alter()
                    use constants
                    implicit none
                    double complex h0toN2N2ProcDepRelVC, HHtoN2N2ProcDepRelVC
                    dAlphaOS12Alter = ( h0toN2N2ProcDepRelVC() - HHtoN2N2ProcDepRelVC() )*SA*CA + &
                                    & SA*CA/2D0*( dZh0h0OS() - dZHHHHOS() ) + 1D0/2D0*( SA2*dZHHh0OSAlter() - CA2*dZh0HHOSAlter() )
                end function dAlphaOS12Alter

                double precision function dBetaOS12Alter()
                    use constants
                    implicit none
                    double complex h0toN1N1ProcDepRelVC, HHtoN1N1ProcDepRelVC, A0toN1N1ProcDepRelVC
                    double complex h0toN2N2ProcDepRelVC, HHtoN2N2ProcDepRelVC, A0toN2N2ProcDepRelVC
                    double precision GaugeXiATemp, GaugeXiWTemp, GaugeXiZTemp
                    GaugeXiATemp = GaugeXiA
                    GaugeXiWTemp = GaugeXiW
                    GaugeXiZTemp = GaugeXiZ
                    GaugeXiA = 1D0
                    GaugeXiW = 1D0
                    GaugeXiZ = 1D0
                    dBetaOS12Alter = SB*CB/2D0*( C2A*(dZHHHHOS() - dZh0h0OS()) - S2A*(dZHHh0OSAlter() + dZh0HHOSAlter()) ) + &
                            & SB*CB*( - A0toN1N1ProcDepRelVC() + CA2*HHtoN1N1ProcDepRelVC() + SA2*h0toN1N1ProcDepRelVC() &
                                    & + A0toN2N2ProcDepRelVC() - SA2*HHtoN2N2ProcDepRelVC() - CA2*h0toN2N2ProcDepRelVC() ) + &
                            & dZG0A0OSAlter()/2D0
                    GaugeXiA = GaugeXiATemp
                    GaugeXiW = GaugeXiWTemp
                    GaugeXiZ = GaugeXiZTemp
                end function dBetaOS12Alter

                double precision function dBetaBFMSAlter()
                    use constants
                    implicit none
                    double complex TadHH, Tadh0
                    double complex SelfHHh0Alter, SelfHHh0Add
                    double precision GaugeXiATemp, GaugeXiWTemp, GaugeXiZTemp
                    GaugeXiATemp = GaugeXiA
                    GaugeXiWTemp = GaugeXiW
                    GaugeXiZTemp = GaugeXiZ
                    GaugeXiA = 1D0
                    GaugeXiW = 1D0
                    GaugeXiZ = 1D0
                    dBetaBFMSAlter = S2B*( DBLE(SelfHHh0Alter(Mh02)) - DBLE(SelfHHh0Alter(MHH2)) + DBLE(SelfHHh0Add(Mh02)) - &
                                    & DBLE(SelfHHh0Add(MHH2)) )/( 2.D0*S2A*(MHH2 - Mh02) ) - &
                                    & EL/(2D0*MW*SW)*( CBA*Tadh0()/Mh02 - SBA*TadHH()/MHH2 )
                    GaugeXiA = GaugeXiATemp
                    GaugeXiW = GaugeXiWTemp
                    GaugeXiZ = GaugeXiZTemp
                end function dBetaBFMSAlter

                double precision function dAlphaMSBarAlter()
                    use constants
                    implicit none
                    dAlphaMSBarAlter = ( &
&((-0.09375D0*CAB*CBA*Lambda5*MA02)/(PI2*S2B) + (0.09375D0*CAB*CBA*Lambda5*MHH2)/(PI2*S2B) - (0.1875D0*CAB*CBA*Lambda5*MHp2)/(PI2*&
  &S2B) - (0.03125D0*C2B*Lambda5*MA02*S2A)/(PI2*S2B2) + (0.15625D0*CAB*CBA*Lambda5*MA02*S2A)/(PI2*S2B2) - (0.09375D0*CAB*CBA*Lamb&
  &da5*Mh02*S2A)/(PI2*S2B2) + (0.03125D0*CAB*CBA*Lambda5*MA02*MHH2*S2A)/(Mh02*PI2*S2B2) - (0.0625D0*C2B*Lambda5*MHp2*S2A)/(PI2*S2&
  &B2) + (0.3125D0*CAB*CBA*Lambda5*MHp2*S2A)/(PI2*S2B2) + (0.0625D0*CAB*CBA*Lambda5*MHH2*MHp2*S2A)/(Mh02*PI2*S2B2) - (0.1875D0*CA&
  &B*Lambda5*Mh02*SAB)/(PI2*S2B2) - (0.1875D0*CAB*Lambda5*MHH2*SAB)/(PI2*S2B2) - (0.09375D0*CBA*Lambda5*MW2*MZ2*SBA)/(CW2*Mh02*PI&
  &2) - (0.09375D0*CBA*Lambda5*MW2*MZ2*SBA)/(CW2*MHH2*PI2) + (0.046875D0*CBA*Lambda5*Mh02*S2A*SBA)/(PI2*S2B) - (0.046875D0*CBA*La&
  &mbda5*MHH2*S2A*SBA)/(PI2*S2B) + (0.28125D0*CBA*Lambda5*MW2*MZ2*S2A*SBA)/(CW2*Mh02*PI2*S2B) - (0.28125D0*CBA*Lambda5*MW2*MZ2*S2&
  &A*SBA)/(CW2*MHH2*PI2*S2B) + (0.140625D0*CBA*Lambda5*Mh02*S2A2*SBA)/(PI2*S2B2) + (0.140625D0*CBA*Lambda5*MHH2*S2A2*SBA)/(PI2*S2&
  &B2) - (0.09375D0*Lambda5*MA02*SAB*SBA)/(PI2*S2B) + (0.09375D0*Lambda5*Mh02*SAB*SBA)/(PI2*S2B) - (0.1875D0*Lambda5*MHp2*SAB*SBA&
  &)/(PI2*S2B) - (0.15625D0*Lambda5*MA02*S2A*SAB*SBA)/(PI2*S2B2) - (0.03125D0*Lambda5*MA02*Mh02*S2A*SAB*SBA)/(MHH2*PI2*S2B2) + (0&
  &.09375D0*Lambda5*MHH2*S2A*SAB*SBA)/(PI2*S2B2) - (0.3125D0*Lambda5*MHp2*S2A*SAB*SBA)/(PI2*S2B2) - (0.0625D0*Lambda5*Mh02*MHp2*S&
  &2A*SAB*SBA)/(MHH2*PI2*S2B2) + (0.03125D0*CAB*CBA*EL2*MA02*Mh02)/(MW2*PI2*S2B*SW2) - (0.046875D0*CAB*CBA*EL2*Mh02*MHH2)/(MW2*PI&
  &2*S2B*SW2) + (0.0625D0*CAB*CBA*EL2*Mh02*MHp2)/(MW2*PI2*S2B*SW2) - (0.015625D0*CAB*CBA*EL2*MA02*Mh02*S2A)/(MW2*PI2*S2B2*SW2) - &
  &(0.015625D0*CAB*CBA*EL2*MA02*MHH2*S2A)/(MW2*PI2*S2B2*SW2) - (0.03125D0*CAB*CBA*EL2*Mh02*MHp2*S2A)/(MW2*PI2*S2B2*SW2) - (0.0312&
  &5D0*CAB*CBA*EL2*MHH2*MHp2*S2A)/(MW2*PI2*S2B2*SW2) + (0.09375D0*CAB*EL2*Mh02*MHH2*SAB)/(MW2*PI2*S2B2*SW2) + (0.046875D0*CA*EL2*&
  &MC2*Mh02*SA)/(MW2*PI2*SB2*SW2) + (0.046875D0*CA*EL2*MC2*MHH2*SA)/(MW2*PI2*SB2*SW2) + (0.046875D0*CA*EL2*Mh02*MT2*SA)/(MW2*PI2*&
  &SB2*SW2) + (0.046875D0*CA*EL2*MHH2*MT2*SA)/(MW2*PI2*SB2*SW2) + (0.046875D0*CA*EL2*Mh02*MU2*SA)/(MW2*PI2*SB2*SW2) + (0.046875D0&
  &*CA*EL2*MHH2*MU2*SA)/(MW2*PI2*SB2*SW2) + (0.015625D0*CBA*EL2*GaugeXiZ*MA02*SBA)/(CW2*PI2*SW2) + (0.1875D0*CBA*EL2*MW2*SBA)/(PI&
  &2*SW2) - (0.015625D0*CBA*EL2*GaugeXiZ*MA02*MZ2*SBA)/(MW2*PI2*SW2) + (0.09375D0*CBA*EL2*Mh02*MW2*S2A*SBA)/(MHH2*PI2*S2B*SW2) - &
  &(0.09375D0*CBA*EL2*MHH2*MW2*S2A*SBA)/(Mh02*PI2*S2B*SW2) + (0.046875D0*CBA*EL2*Mh02*MZ2*S2A*SBA)/(CW2*MHH2*PI2*S2B*SW2) - (0.04&
  &6875D0*CBA*EL2*MHH2*MZ2*S2A*SBA)/(CW2*Mh02*PI2*S2B*SW2) - (0.09375D0*CBA*EL2*Mh02*MHH2*S2A2*SBA)/(MW2*PI2*S2B2*SW2) + (0.03125&
  &D0*EL2*MA02*MHH2*SAB*SBA)/(MW2*PI2*S2B*SW2) - (0.046875D0*EL2*Mh02*MHH2*SAB*SBA)/(MW2*PI2*S2B*SW2) + (0.0625D0*EL2*MHH2*MHp2*S&
  &AB*SBA)/(MW2*PI2*S2B*SW2) + (0.015625D0*EL2*MA02*Mh02*S2A*SAB*SBA)/(MW2*PI2*S2B2*SW2) + (0.015625D0*EL2*MA02*MHH2*S2A*SAB*SBA)&
  &/(MW2*PI2*S2B2*SW2) + (0.03125D0*EL2*Mh02*MHp2*S2A*SAB*SBA)/(MW2*PI2*S2B2*SW2) + (0.03125D0*EL2*MHH2*MHp2*S2A*SAB*SBA)/(MW2*PI&
  &2*S2B2*SW2) + (0.046875D0*EL2*MB2*Mh02*Yuk1*Yuk2)/(MW2*PI2*SW2) + (0.046875D0*EL2*MD2*Mh02*Yuk1*Yuk2)/(MW2*PI2*SW2) + (0.04687&
  &5D0*EL2*MB2*MHH2*Yuk1*Yuk2)/(MW2*PI2*SW2) + (0.046875D0*EL2*MD2*MHH2*Yuk1*Yuk2)/(MW2*PI2*SW2) + (0.046875D0*EL2*Mh02*MS2*Yuk1*&
  &Yuk2)/(MW2*PI2*SW2) + (0.046875D0*EL2*MHH2*MS2*Yuk1*Yuk2)/(MW2*PI2*SW2) + (0.015625D0*EL2*ME2*Mh02*Yuk4*Yuk5)/(MW2*PI2*SW2) + &
  &(0.015625D0*EL2*ME2*MHH2*Yuk4*Yuk5)/(MW2*PI2*SW2) + (0.015625D0*EL2*Mh02*ML2*Yuk4*Yuk5)/(MW2*PI2*SW2) + (0.015625D0*EL2*MHH2*M&
  &L2*Yuk4*Yuk5)/(MW2*PI2*SW2) + (0.015625D0*EL2*Mh02*MM2*Yuk4*Yuk5)/(MW2*PI2*SW2) + (0.015625D0*EL2*MHH2*MM2*Yuk4*Yuk5)/(MW2*PI2&
  &*SW2) + (0.09375D0*CBA*EL2*MW2*SBA*DBLE(CW**INT(-4.D0)))/(PI2*SW2) - (0.015625D0*CBA*EL2*MZ2*SBA*DBLE(GaugeXiZ**INT(2.D0)))/(C&
  &W2*PI2*SW2) + (0.015625D0*CBA*EL2*MW2*SBA*DBLE(CW**INT(-4.D0))*DBLE(GaugeXiZ**INT(2.D0)))/(PI2*SW2) + (0.0625D0*CAB*CBA*MA02*M&
  &W2*SW2*DBLE(Lambda5**INT(2.D0)))/(EL2*Mh02*PI2*S2B) + (0.125D0*CAB*CBA*MHp2*MW2*SW2*DBLE(Lambda5**INT(2.D0)))/(EL2*Mh02*PI2*S2&
  &B) - (0.1875D0*CAB*CBA*MA02*MW2*S2A*SW2*DBLE(Lambda5**INT(2.D0)))/(EL2*Mh02*PI2*S2B2) - (0.375D0*CAB*CBA*MHp2*MW2*S2A*SW2*DBLE&
  &(Lambda5**INT(2.D0)))/(EL2*Mh02*PI2*S2B2) + (0.375D0*CAB*MW2*SAB*SW2*DBLE(Lambda5**INT(2.D0)))/(EL2*PI2*S2B2) + (0.0625D0*CBA*&
  &MW2*SBA*SW2*DBLE(Lambda5**INT(2.D0)))/(EL2*PI2) - (0.03125D0*CBA*Mh02*MW2*SBA*SW2*DBLE(Lambda5**INT(2.D0)))/(EL2*MHH2*PI2) - (&
  &0.03125D0*CBA*MHH2*MW2*SBA*SW2*DBLE(Lambda5**INT(2.D0)))/(EL2*Mh02*PI2) - (0.5625D0*CBA*MW2*S2A2*SBA*SW2*DBLE(Lambda5**INT(2.D&
  &0)))/(EL2*PI2*S2B2) + (0.28125D0*CBA*Mh02*MW2*S2A2*SBA*SW2*DBLE(Lambda5**INT(2.D0)))/(EL2*MHH2*PI2*S2B2) + (0.28125D0*CBA*MHH2&
  &*MW2*S2A2*SBA*SW2*DBLE(Lambda5**INT(2.D0)))/(EL2*Mh02*PI2*S2B2) + (0.0625D0*MA02*MW2*SAB*SBA*SW2*DBLE(Lambda5**INT(2.D0)))/(EL&
  &2*MHH2*PI2*S2B) + (0.125D0*MHp2*MW2*SAB*SBA*SW2*DBLE(Lambda5**INT(2.D0)))/(EL2*MHH2*PI2*S2B) + (0.1875D0*MA02*MW2*S2A*SAB*SBA*&
  &SW2*DBLE(Lambda5**INT(2.D0)))/(EL2*MHH2*PI2*S2B2) + (0.375D0*MHp2*MW2*S2A*SAB*SBA*SW2*DBLE(Lambda5**INT(2.D0)))/(EL2*MHH2*PI2*&
  &S2B2) - (0.03125D0*CBA*Lambda5*SBA*DBLE(MA0**INT(4.D0)))/(Mh02*PI2) - (0.03125D0*CBA*Lambda5*SBA*DBLE(MA0**INT(4.D0)))/(MHH2*P&
  &I2) + (0.09375D0*CBA*Lambda5*S2A*SBA*DBLE(MA0**INT(4.D0)))/(Mh02*PI2*S2B) - (0.09375D0*CBA*Lambda5*S2A*SBA*DBLE(MA0**INT(4.D0)&
  &))/(MHH2*PI2*S2B) + (0.03125D0*CBA*EL2*SBA*DBLE(MA0**INT(4.D0)))/(MW2*PI2*SW2) + (0.015625D0*CBA*EL2*Mh02*S2A*SBA*DBLE(MA0**IN&
  &T(4.D0)))/(MHH2*MW2*PI2*S2B*SW2) - (0.015625D0*CBA*EL2*MHH2*S2A*SBA*DBLE(MA0**INT(4.D0)))/(Mh02*MW2*PI2*S2B*SW2) + (0.375D0*CB&
  &A*Lambda5*Yuk1*DBLE(MB**INT(4.D0)))/(Mh02*PI2) - (1.125D0*CBA*Lambda5*S2A*Yuk1*DBLE(MB**INT(4.D0)))/(Mh02*PI2*S2B) + (0.375D0*&
  &CBA*EL2*S2A*Yuk1*DBLE(MB**INT(4.D0)))/(MW2*PI2*S2B*SW2) + (0.1875D0*CBA*EL2*MHH2*S2A*Yuk1*DBLE(MB**INT(4.D0)))/(Mh02*MW2*PI2*S&
  &2B*SW2) + (0.375D0*Lambda5*SBA*Yuk2*DBLE(MB**INT(4.D0)))/(MHH2*PI2) + (1.125D0*Lambda5*S2A*SBA*Yuk2*DBLE(MB**INT(4.D0)))/(MHH2&
  &*PI2*S2B) - (0.375D0*EL2*S2A*SBA*Yuk2*DBLE(MB**INT(4.D0)))/(MW2*PI2*S2B*SW2) - (0.1875D0*EL2*Mh02*S2A*SBA*Yuk2*DBLE(MB**INT(4.&
  &D0)))/(MHH2*MW2*PI2*S2B*SW2) - (0.5625D0*EL2*Yuk1*Yuk2*DBLE(MB**INT(4.D0)))/(MW2*PI2*SW2) + (0.375D0*CA*CBA*Lambda5*DBLE(MC**I&
  &NT(4.D0)))/(Mh02*PI2*SB) - (1.125D0*CA*CBA*Lambda5*S2A*DBLE(MC**INT(4.D0)))/(Mh02*PI2*S2B*SB) + (0.375D0*Lambda5*SA*SBA*DBLE(M&
  &C**INT(4.D0)))/(MHH2*PI2*SB) + (1.125D0*Lambda5*S2A*SA*SBA*DBLE(MC**INT(4.D0)))/(MHH2*PI2*S2B*SB) + (0.375D0*CA*CBA*EL2*S2A*DB&
  &LE(MC**INT(4.D0)))/(MW2*PI2*S2B*SB*SW2) + (0.1875D0*CA*CBA*EL2*MHH2*S2A*DBLE(MC**INT(4.D0)))/(Mh02*MW2*PI2*S2B*SB*SW2) - (0.56&
  &25D0*CA*EL2*SA*DBLE(MC**INT(4.D0)))/(MW2*PI2*SB2*SW2) - (0.375D0*EL2*S2A*SA*SBA*DBLE(MC**INT(4.D0)))/(MW2*PI2*S2B*SB*SW2) - (0&
  &.1875D0*EL2*Mh02*S2A*SA*SBA*DBLE(MC**INT(4.D0)))/(MHH2*MW2*PI2*S2B*SB*SW2) + (0.375D0*CBA*Lambda5*Yuk1*DBLE(MD**INT(4.D0)))/(M&
  &h02*PI2) - (1.125D0*CBA*Lambda5*S2A*Yuk1*DBLE(MD**INT(4.D0)))/(Mh02*PI2*S2B) + (0.375D0*CBA*EL2*S2A*Yuk1*DBLE(MD**INT(4.D0)))/&
  &(MW2*PI2*S2B*SW2) + (0.1875D0*CBA*EL2*MHH2*S2A*Yuk1*DBLE(MD**INT(4.D0)))/(Mh02*MW2*PI2*S2B*SW2) + (0.375D0*Lambda5*SBA*Yuk2*DB&
  &LE(MD**INT(4.D0)))/(MHH2*PI2) + (1.125D0*Lambda5*S2A*SBA*Yuk2*DBLE(MD**INT(4.D0)))/(MHH2*PI2*S2B) - (0.375D0*EL2*S2A*SBA*Yuk2*&
  &DBLE(MD**INT(4.D0)))/(MW2*PI2*S2B*SW2) - (0.1875D0*EL2*Mh02*S2A*SBA*Yuk2*DBLE(MD**INT(4.D0)))/(MHH2*MW2*PI2*S2B*SW2) - (0.5625&
  &D0*EL2*Yuk1*Yuk2*DBLE(MD**INT(4.D0)))/(MW2*PI2*SW2) + (0.125D0*CBA*Lambda5*Yuk4*DBLE(ME**INT(4.D0)))/(Mh02*PI2) - (0.375D0*CBA&
  &*Lambda5*S2A*Yuk4*DBLE(ME**INT(4.D0)))/(Mh02*PI2*S2B) + (0.125D0*CBA*EL2*S2A*Yuk4*DBLE(ME**INT(4.D0)))/(MW2*PI2*S2B*SW2) + (0.&
  &0625D0*CBA*EL2*MHH2*S2A*Yuk4*DBLE(ME**INT(4.D0)))/(Mh02*MW2*PI2*S2B*SW2) + (0.125D0*Lambda5*SBA*Yuk5*DBLE(ME**INT(4.D0)))/(MHH&
  &2*PI2) + (0.375D0*Lambda5*S2A*SBA*Yuk5*DBLE(ME**INT(4.D0)))/(MHH2*PI2*S2B) - (0.125D0*EL2*S2A*SBA*Yuk5*DBLE(ME**INT(4.D0)))/(M&
  &W2*PI2*S2B*SW2) - (0.0625D0*EL2*Mh02*S2A*SBA*Yuk5*DBLE(ME**INT(4.D0)))/(MHH2*MW2*PI2*S2B*SW2) - (0.1875D0*EL2*Yuk4*Yuk5*DBLE(M&
  &E**INT(4.D0)))/(MW2*PI2*SW2) - (0.015625D0*CBA*Lambda5*S2A*SBA*DBLE(Mh0**INT(4.D0)))/(MHH2*PI2*S2B) - (0.140625D0*CBA*Lambda5*&
  &S2A2*SBA*DBLE(Mh0**INT(4.D0)))/(MHH2*PI2*S2B2) + (0.046875D0*CAB*CBA*EL2*S2A*DBLE(Mh0**INT(4.D0)))/(MW2*PI2*S2B2*SW2) + (0.031&
  &25D0*CBA*EL2*S2A2*SBA*DBLE(Mh0**INT(4.D0)))/(MW2*PI2*S2B2*SW2) + (0.015625D0*CBA*EL2*S2A2*SBA*DBLE(Mh0**INT(6.D0)))/(MHH2*MW2*&
  &PI2*S2B2*SW2) + (0.015625D0*CBA*Lambda5*S2A*SBA*DBLE(MHH**INT(4.D0)))/(Mh02*PI2*S2B) - (0.140625D0*CBA*Lambda5*S2A2*SBA*DBLE(M&
  &HH**INT(4.D0)))/(Mh02*PI2*S2B2) + (0.03125D0*CBA*EL2*S2A2*SBA*DBLE(MHH**INT(4.D0)))/(MW2*PI2*S2B2*SW2) - (0.046875D0*EL2*S2A*S&
  &AB*SBA*DBLE(MHH**INT(4.D0)))/(MW2*PI2*S2B2*SW2) + (0.015625D0*CBA*EL2*S2A2*SBA*DBLE(MHH**INT(6.D0)))/(Mh02*MW2*PI2*S2B2*SW2) -&
  & (0.0625D0*CBA*Lambda5*SBA*DBLE(MHp**INT(4.D0)))/(Mh02*PI2) - (0.0625D0*CBA*Lambda5*SBA*DBLE(MHp**INT(4.D0)))/(MHH2*PI2) + (0.&
  &1875D0*CBA*Lambda5*S2A*SBA*DBLE(MHp**INT(4.D0)))/(Mh02*PI2*S2B) - (0.1875D0*CBA*Lambda5*S2A*SBA*DBLE(MHp**INT(4.D0)))/(MHH2*PI&
  &2*S2B) + (0.0625D0*CBA*EL2*SBA*DBLE(MHp**INT(4.D0)))/(MW2*PI2*SW2) + (0.03125D0*CBA*EL2*Mh02*S2A*SBA*DBLE(MHp**INT(4.D0)))/(MH&
  &H2*MW2*PI2*S2B*SW2) - (0.03125D0*CBA*EL2*MHH2*S2A*SBA*DBLE(MHp**INT(4.D0)))/(Mh02*MW2*PI2*S2B*SW2) + (0.125D0*CBA*Lambda5*Yuk4&
  &*DBLE(ML**INT(4.D0)))/(Mh02*PI2) - (0.375D0*CBA*Lambda5*S2A*Yuk4*DBLE(ML**INT(4.D0)))/(Mh02*PI2*S2B) + (0.125D0*CBA*EL2*S2A*Yu&
  &k4*DBLE(ML**INT(4.D0)))/(MW2*PI2*S2B*SW2) + (0.0625D0*CBA*EL2*MHH2*S2A*Yuk4*DBLE(ML**INT(4.D0)))/(Mh02*MW2*PI2*S2B*SW2) + (0.1&
  &25D0*Lambda5*SBA*Yuk5*DBLE(ML**INT(4.D0)))/(MHH2*PI2) + (0.375D0*Lambda5*S2A*SBA*Yuk5*DBLE(ML**INT(4.D0)))/(MHH2*PI2*S2B) - (0&
  &.125D0*EL2*S2A*SBA*Yuk5*DBLE(ML**INT(4.D0)))/(MW2*PI2*S2B*SW2) - (0.0625D0*EL2*Mh02*S2A*SBA*Yuk5*DBLE(ML**INT(4.D0)))/(MHH2*MW&
  &2*PI2*S2B*SW2) - (0.1875D0*EL2*Yuk4*Yuk5*DBLE(ML**INT(4.D0)))/(MW2*PI2*SW2) + (0.125D0*CBA*Lambda5*Yuk4*DBLE(MM**INT(4.D0)))/(&
  &Mh02*PI2) - (0.375D0*CBA*Lambda5*S2A*Yuk4*DBLE(MM**INT(4.D0)))/(Mh02*PI2*S2B) + (0.125D0*CBA*EL2*S2A*Yuk4*DBLE(MM**INT(4.D0)))&
  &/(MW2*PI2*S2B*SW2) + (0.0625D0*CBA*EL2*MHH2*S2A*Yuk4*DBLE(MM**INT(4.D0)))/(Mh02*MW2*PI2*S2B*SW2) + (0.125D0*Lambda5*SBA*Yuk5*D&
  &BLE(MM**INT(4.D0)))/(MHH2*PI2) + (0.375D0*Lambda5*S2A*SBA*Yuk5*DBLE(MM**INT(4.D0)))/(MHH2*PI2*S2B) - (0.125D0*EL2*S2A*SBA*Yuk5&
  &*DBLE(MM**INT(4.D0)))/(MW2*PI2*S2B*SW2) - (0.0625D0*EL2*Mh02*S2A*SBA*Yuk5*DBLE(MM**INT(4.D0)))/(MHH2*MW2*PI2*S2B*SW2) - (0.187&
  &5D0*EL2*Yuk4*Yuk5*DBLE(MM**INT(4.D0)))/(MW2*PI2*SW2) + (0.375D0*CBA*Lambda5*Yuk1*DBLE(MS**INT(4.D0)))/(Mh02*PI2) - (1.125D0*CB&
  &A*Lambda5*S2A*Yuk1*DBLE(MS**INT(4.D0)))/(Mh02*PI2*S2B) + (0.375D0*CBA*EL2*S2A*Yuk1*DBLE(MS**INT(4.D0)))/(MW2*PI2*S2B*SW2) + (0&
  &.1875D0*CBA*EL2*MHH2*S2A*Yuk1*DBLE(MS**INT(4.D0)))/(Mh02*MW2*PI2*S2B*SW2) + (0.375D0*Lambda5*SBA*Yuk2*DBLE(MS**INT(4.D0)))/(MH&
  &H2*PI2) + (1.125D0*Lambda5*S2A*SBA*Yuk2*DBLE(MS**INT(4.D0)))/(MHH2*PI2*S2B) - (0.375D0*EL2*S2A*SBA*Yuk2*DBLE(MS**INT(4.D0)))/(&
  &MW2*PI2*S2B*SW2) - (0.1875D0*EL2*Mh02*S2A*SBA*Yuk2*DBLE(MS**INT(4.D0)))/(MHH2*MW2*PI2*S2B*SW2) - (0.5625D0*EL2*Yuk1*Yuk2*DBLE(&
  &MS**INT(4.D0)))/(MW2*PI2*SW2) + (0.375D0*CA*CBA*Lambda5*DBLE(MT**INT(4.D0)))/(Mh02*PI2*SB) - (1.125D0*CA*CBA*Lambda5*S2A*DBLE(&
  &MT**INT(4.D0)))/(Mh02*PI2*S2B*SB) + (0.375D0*Lambda5*SA*SBA*DBLE(MT**INT(4.D0)))/(MHH2*PI2*SB) + (1.125D0*Lambda5*S2A*SA*SBA*D&
  &BLE(MT**INT(4.D0)))/(MHH2*PI2*S2B*SB) + (0.375D0*CA*CBA*EL2*S2A*DBLE(MT**INT(4.D0)))/(MW2*PI2*S2B*SB*SW2) + (0.1875D0*CA*CBA*E&
  &L2*MHH2*S2A*DBLE(MT**INT(4.D0)))/(Mh02*MW2*PI2*S2B*SB*SW2) - (0.5625D0*CA*EL2*SA*DBLE(MT**INT(4.D0)))/(MW2*PI2*SB2*SW2) - (0.3&
  &75D0*EL2*S2A*SA*SBA*DBLE(MT**INT(4.D0)))/(MW2*PI2*S2B*SB*SW2) - (0.1875D0*EL2*Mh02*S2A*SA*SBA*DBLE(MT**INT(4.D0)))/(MHH2*MW2*P&
  &I2*S2B*SB*SW2) + (0.375D0*CA*CBA*Lambda5*DBLE(MU**INT(4.D0)))/(Mh02*PI2*SB) - (1.125D0*CA*CBA*Lambda5*S2A*DBLE(MU**INT(4.D0)))&
  &/(Mh02*PI2*S2B*SB) + (0.375D0*Lambda5*SA*SBA*DBLE(MU**INT(4.D0)))/(MHH2*PI2*SB) + (1.125D0*Lambda5*S2A*SA*SBA*DBLE(MU**INT(4.D&
  &0)))/(MHH2*PI2*S2B*SB) + (0.375D0*CA*CBA*EL2*S2A*DBLE(MU**INT(4.D0)))/(MW2*PI2*S2B*SB*SW2) + (0.1875D0*CA*CBA*EL2*MHH2*S2A*DBL&
  &E(MU**INT(4.D0)))/(Mh02*MW2*PI2*S2B*SB*SW2) - (0.5625D0*CA*EL2*SA*DBLE(MU**INT(4.D0)))/(MW2*PI2*SB2*SW2) - (0.375D0*EL2*S2A*SA&
  &*SBA*DBLE(MU**INT(4.D0)))/(MW2*PI2*S2B*SB*SW2) - (0.1875D0*EL2*Mh02*S2A*SA*SBA*DBLE(MU**INT(4.D0)))/(MHH2*MW2*PI2*S2B*SB*SW2) &
  &- (0.1875D0*CBA*Lambda5*SBA*DBLE(MW**INT(4.D0)))/(Mh02*PI2) - (0.1875D0*CBA*Lambda5*SBA*DBLE(MW**INT(4.D0)))/(MHH2*PI2) + (0.5&
  &625D0*CBA*Lambda5*S2A*SBA*DBLE(MW**INT(4.D0)))/(Mh02*PI2*S2B) - (0.5625D0*CBA*Lambda5*S2A*SBA*DBLE(MW**INT(4.D0)))/(MHH2*PI2*S&
  &2B))/(-1.D0*Mh02 + MHH2) &
                        & )*UVDelta
                end function dAlphaMSBarAlter

                double precision function dBetaMSBarAlter()
                    use constants
                    implicit none
                    dBetaMSBarAlter = ( &
                        & (-0.015625D0*EL2*((3.D0*MC2)/TB + (3.D0*MT2)/TB + (3.D0*MU2)/TB - 3.D0*MB2*Yuk3 - 3.D0*MD2*Yuk3 -&
                            & 3.D0*MS2*Yuk3 - 1.D0*ME2*Yuk6 - 1.D0*ML2*Yuk6 - 1.D0*MM2*Yuk6))/(MW2*PI2*SW2) - &
                            & EL/(2D0*MW*SW)*( &
&(SBA*((0.015625D0*CBA*EL*MA02*MHH2)/(MW*PI2*SW) + (0.03125D0*CBA*EL*MHH2*MHp2)/(MW*PI2*SW) - (0.03125D0*CBA*EL*GaugeXiW*MHH2*MW)/&
  &(PI2*SW) - (0.015625D0*CBA*EL*GaugeXiZ*MHH2*MZ2)/(MW*PI2*SW) - (0.09375D0*CBA*EL*MW*MZ2)/(CW2*PI2*SW) - (0.015625D0*CBA*EL*Mh0&
  &2*MHH2*S2A)/(MW*PI2*S2B*SW) - (0.03125D0*EL*MA02*MHH2*SAB)/(MW*PI2*S2B*SW) - (0.0625D0*EL*MHH2*MHp2*SAB)/(MW*PI2*S2B*SW) - (0.&
  &03125D0*CBA*Lambda5*Mh02*MW*SW)/(EL*PI2) + (0.09375D0*CBA*Lambda5*Mh02*MW*S2A*SW)/(EL*PI2*S2B) + (0.0625D0*Lambda5*MA02*MW*SAB&
  &*SW)/(EL*PI2*S2B) + (0.125D0*Lambda5*MHp2*MW*SAB*SW)/(EL*PI2*S2B) + (0.1875D0*Lambda5*MHH2*MW*SAB*SBA2*SW)/(EL*PI2*S2B) - (0.0&
  &3125D0*CBA*EL*DBLE(MA0**INT(4.D0)))/(MW*PI2*SW) + (0.375D0*EL*Yuk2*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) + (0.375D0*EL*SA*DBLE(MC**&
  &INT(4.D0)))/(MW*PI2*SB*SW) + (0.375D0*EL*Yuk2*DBLE(MD**INT(4.D0)))/(MW*PI2*SW) + (0.125D0*EL*Yuk5*DBLE(ME**INT(4.D0)))/(MW*PI2&
  &*SW) - (0.03125D0*CBA*EL*S2A*DBLE(Mh0**INT(4.D0)))/(MW*PI2*S2B*SW) + (0.046875D0*CBA*EL*S2A*DBLE(MHH**INT(4.D0)))/(MW*PI2*S2B*&
  &SW) - (0.09375D0*EL*SAB*DBLE(MHH**INT(4.D0)))/(MW*PI2*S2B*SW) - (0.0625D0*CBA*EL*DBLE(MHp**INT(4.D0)))/(MW*PI2*SW) + (0.125D0*&
  &EL*Yuk5*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) + (0.125D0*EL*Yuk5*DBLE(MM**INT(4.D0)))/(MW*PI2*SW) + (0.375D0*EL*Yuk2*DBLE(MS**INT(4&
  &.D0)))/(MW*PI2*SW) + (0.375D0*EL*SA*DBLE(MT**INT(4.D0)))/(MW*PI2*SB*SW) + (0.375D0*EL*SA*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) -&
  & (0.1875D0*CBA*EL*DBLE(MW**INT(3.D0)))/(PI2*SW)))/MHH2 + (CBA*((0.03125D0*CAB*EL*MA02*Mh02)/(MW*PI2*S2B*SW) + (0.0625D0*CAB*EL&
  &*Mh02*MHp2)/(MW*PI2*S2B*SW) - (0.015625D0*EL*MA02*Mh02*SBA)/(MW*PI2*SW) - (0.03125D0*EL*Mh02*MHp2*SBA)/(MW*PI2*SW) + (0.03125D&
  &0*EL*GaugeXiW*Mh02*MW*SBA)/(PI2*SW) + (0.015625D0*EL*GaugeXiZ*Mh02*MZ2*SBA)/(MW*PI2*SW) + (0.09375D0*EL*MW*MZ2*SBA)/(CW2*PI2*S&
  &W) - (0.015625D0*EL*Mh02*MHH2*S2A*SBA)/(MW*PI2*S2B*SW) - (0.0625D0*CAB*Lambda5*MA02*MW*SW)/(EL*PI2*S2B) - (0.1875D0*CAB*CBA2*L&
  &ambda5*Mh02*MW*SW)/(EL*PI2*S2B) - (0.125D0*CAB*Lambda5*MHp2*MW*SW)/(EL*PI2*S2B) + (0.03125D0*Lambda5*MHH2*MW*SBA*SW)/(EL*PI2) &
  &+ (0.09375D0*Lambda5*MHH2*MW*S2A*SBA*SW)/(EL*PI2*S2B) + (0.03125D0*EL*SBA*DBLE(MA0**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*Yuk1&
  &*DBLE(MB**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA*EL*DBLE(MC**INT(4.D0)))/(MW*PI2*SB*SW) - (0.375D0*EL*Yuk1*DBLE(MD**INT(4.D0)))&
  &/(MW*PI2*SW) - (0.125D0*EL*Yuk4*DBLE(ME**INT(4.D0)))/(MW*PI2*SW) + (0.09375D0*CAB*EL*DBLE(Mh0**INT(4.D0)))/(MW*PI2*S2B*SW) + (&
  &0.046875D0*EL*S2A*SBA*DBLE(Mh0**INT(4.D0)))/(MW*PI2*S2B*SW) - (0.03125D0*EL*S2A*SBA*DBLE(MHH**INT(4.D0)))/(MW*PI2*S2B*SW) + (0&
  &.0625D0*EL*SBA*DBLE(MHp**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*Yuk4*DBLE(ML**INT(4.D0)))/(MW*PI2*SW) - (0.125D0*EL*Yuk4*DBLE(M&
  &M**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*EL*Yuk1*DBLE(MS**INT(4.D0)))/(MW*PI2*SW) - (0.375D0*CA*EL*DBLE(MT**INT(4.D0)))/(MW*PI2*S&
  &B*SW) - (0.375D0*CA*EL*DBLE(MU**INT(4.D0)))/(MW*PI2*SB*SW) + (0.1875D0*EL*SBA*DBLE(MW**INT(3.D0)))/(PI2*SW)))/Mh02&
                            & ) &
                        & )*UVDelta
                end function dBetaMSBarAlter

            ! Usual tadpole based counterterms
                double precision function dAlphaKanUsual()
                    use constants
                    implicit none
                    dAlphaKanUsual = ( dZHHh0OSUsual() - dZh0HHOSUsual() )/4.D0
                end function dAlphaKanUsual

                double precision function dBeta1KanUsual()
                    use constants
                    implicit none
                    dBeta1KanUsual = ( dZG0A0OSUsual() - dZA0G0OSUsual() )/4.D0
                end function dBeta1KanUsual

                double precision function dBeta2KanUsual()
                    use constants
                    implicit none
                    dBeta2KanUsual = ( dZGpHpOSUsual() - dZHpGpOSUsual() )/4.D0
                end function dBeta2KanUsual

                double precision function dBetaProcDep1Usual()
                    use constants
                    implicit none
                    double complex A0toTauPTauMProcDepVC
                    dBetaProcDep1Usual = -Yuk6/(1 + Yuk6**2)*( A0toTauPTauMProcDepVC() + dgAtMZ()/(EL/SW) + dMLOSUsualWeak()/ML -&
                        & dMW2Usual()/(2D0*MW2) + dZA0A0OS()/2D0 - dZG0A0OSUsual()/(2D0*Yuk6) + dZTauTauOSLeftWeak()/2D0 + &
                        & dZTauTauOSRightWeak()/2D0 )
                end function dBetaProcDep1Usual

                double precision function dAlphaProcDep1Usual()
                    use constants
                    implicit none
                    double complex HHtoTauPTauMProcDepVC
                    dAlphaProcDep1Usual = -Yuk5/Yuk4*( HHtoTauPTauMProcDepVC() + dgAtMZ()/(EL/SW) + dMLOSUsualWeak()/ML - &
                        & dMW2Usual()/(2D0*MW2) + Yuk6*dBetaProcDep1Usual() + dZHHHHOS()/2D0 + Yuk4*dZh0HHOSUsual()/(2D0*Yuk5) + &
                        & dZTauTauOSLeftWeak()/2D0 + dZTauTauOSRightWeak()/2D0 )
                end function dAlphaProcDep1Usual

                double precision function dBetaProcDep2Usual()
                    use constants
                    implicit none
                    double complex A0toTauPTauMProcDepVC
                    dBetaProcDep2Usual = -Yuk6/(1 + Yuk6**2)*( A0toTauPTauMProcDepVC() + dgAtMZ()/(EL/SW) + dMLOSUsualWeak()/ML -&
                        & dMW2Usual()/(2D0*MW2) + dZA0A0OS()/2D0 - dZG0A0OSUsual()/(2D0*Yuk6) + dZTauTauOSLeftWeak()/2D0 + &
                        & dZTauTauOSRightWeak()/2D0 )
                end function dBetaProcDep2Usual

                double precision function dAlphaProcDep2Usual()
                    use constants
                    implicit none
                    double complex h0toTauPTauMProcDepVC
                    dAlphaProcDep2Usual = Yuk4/Yuk5*( h0toTauPTauMProcDepVC() + dgAtMZ()/(EL/SW) + dMLOSUsualWeak()/ML - &
                        & dMW2Usual()/(2D0*MW2) + Yuk6*dBetaProcDep2Usual() + dZh0h0OS()/2D0 + Yuk5*dZHHh0OSUsual()/(2D0*Yuk4) + &
                        & dZTauTauOSLeftWeak()/2D0 + dZTauTauOSRightWeak()/2D0 )
                end function dAlphaProcDep2Usual

                double precision function dBetaProcDep3Usual()
                    use constants
                    implicit none
                    double complex h0toTauPTauMProcDepVC, HHtoTauPTauMProcDepVC
                    dBetaProcDep3Usual = - 1D0/(Yuk6*(Yuk4**2 + Yuk5**2))*( Yuk4*Yuk5*(dZHHh0OSUsual()/2D0 + dZh0HHOSUsual()/2D0)&
                        & + Yuk4**2*( h0toTauPTauMProcDepVC() + dZh0h0OS()/2D0 ) &
                        & + Yuk5**2*( HHtoTauPTauMProcDepVC() + dZHHHHOS()/2D0 ) &
                        & + (Yuk4**2 + Yuk5**2)*( dgAtMZ()/(EL/SW) + dMLOSUsualWeak()/ML - dMW2Usual()/(2D0*MW2) + &
                        & dZTauTauOSLeftWeak()/2D0 + dZTauTauOSRightWeak()/2D0 ) )
                end function dBetaProcDep3Usual

                double precision function dAlphaProcDep3Usual()
                    use constants
                    implicit none
                    double complex h0toTauPTauMProcDepVC, HHtoTauPTauMProcDepVC
                    dAlphaProcDep3Usual = Yuk4*Yuk5/(Yuk4**2 + Yuk5**2) * ( h0toTauPTauMProcDepVC() - HHtoTauPTauMProcDepVC() + &
                        & dZh0h0OS()/2D0 - dZHHHHOS()/2D0 + Yuk5/Yuk4*dZHHh0OSUsual()/2D0 - Yuk4/Yuk5*dZh0HHOSUsual()/2D0 )
                end function dAlphaProcDep3Usual

                double precision function dAlphaMSBarUsual()
                    use constants
                    implicit none
                    dAlphaMSBarUsual = ( &
&((-0.0625D0*CAB*CBA*Lambda5*MA02)/(PI2*S2B) + (0.09375D0*CAB*CBA*Lambda5*Mh02)/(PI2*S2B) + (0.09375D0*CAB*CBA*Lambda5*MHH2)/(PI2*&
  &S2B) - (0.125D0*CAB*CBA*Lambda5*MHp2)/(PI2*S2B) - (0.03125D0*C2B*Lambda5*MA02*S2A)/(PI2*S2B2) - (0.375D0*CAB*CBA*Lambda5*Mh02*&
  &S2A)/(PI2*S2B2) - (0.0625D0*C2B*Lambda5*MHp2*S2A)/(PI2*S2B2) + (0.015625D0*CA2*CBA*Lambda5*Mh02*SA)/(CB*PI2) - (0.046875D0*CA2&
  &*CBA*Lambda5*Mh02*S2A*SA)/(CB*PI2*S2B) + (0.03125D0*CA*CAB*Lambda5*MA02*SA2)/(CB*PI2*S2B) + (0.09375D0*CA*CAB*CBA2*Lambda5*Mh0&
  &2*SA2)/(CB*PI2*S2B) + (0.0625D0*CA*CAB*Lambda5*MHp2*SA2)/(CB*PI2*S2B) - (0.1875D0*CAB*Lambda5*Mh02*SAB)/(PI2*S2B2) - (0.1875D0&
  &*CAB*Lambda5*MHH2*SAB)/(PI2*S2B2) - (0.03125D0*CA2*Lambda5*MA02*SA*SAB)/(CB*PI2*S2B) - (0.0625D0*CA2*Lambda5*MHp2*SA*SAB)/(CB*&
  &PI2*S2B) + (0.03125D0*CA2*CAB*Lambda5*MA02*SA)/(PI2*S2B*SB) + (0.09375D0*CA2*CAB*CBA2*Lambda5*Mh02*SA)/(PI2*S2B*SB) + (0.0625D&
  &0*CA2*CAB*Lambda5*MHp2*SA)/(PI2*S2B*SB) - (0.015625D0*CA*CBA*Lambda5*Mh02*SA2)/(PI2*SB) + (0.046875D0*CA*CBA*Lambda5*Mh02*S2A*&
  &SA2)/(PI2*S2B*SB) + (0.03125D0*CA*Lambda5*MA02*SA2*SAB)/(PI2*S2B*SB) + (0.0625D0*CA*Lambda5*MHp2*SA2*SAB)/(PI2*S2B*SB) - (0.03&
  &125D0*CBA*Lambda5*MA02*SBA)/PI2 - (0.0625D0*CBA*Lambda5*MHp2*SBA)/PI2 + (0.0625D0*CBA*GaugeXiW*Lambda5*MW2*SBA)/PI2 + (0.03125&
  &D0*CBA*GaugeXiZ*Lambda5*MZ2*SBA)/PI2 + (0.078125D0*CBA*Lambda5*Mh02*S2A*SBA)/(PI2*S2B) - (0.078125D0*CBA*Lambda5*MHH2*S2A*SBA)&
  &/(PI2*S2B) + (0.140625D0*CBA*Lambda5*Mh02*S2A2*SBA)/(PI2*S2B2) + (0.140625D0*CBA*Lambda5*MHH2*S2A2*SBA)/(PI2*S2B2) - (0.015625&
  &D0*CA*Lambda5*MHH2*SA2*SBA)/(CB*PI2) - (0.046875D0*CA*Lambda5*MHH2*S2A*SA2*SBA)/(CB*PI2*S2B) - (0.0625D0*Lambda5*MA02*SAB*SBA)&
  &/(PI2*S2B) + (0.09375D0*Lambda5*Mh02*SAB*SBA)/(PI2*S2B) + (0.09375D0*Lambda5*MHH2*SAB*SBA)/(PI2*S2B) - (0.125D0*Lambda5*MHp2*S&
  &AB*SBA)/(PI2*S2B) + (0.375D0*Lambda5*MHH2*S2A*SAB*SBA)/(PI2*S2B2) - (0.015625D0*CA2*Lambda5*MHH2*SA*SBA)/(PI2*SB) - (0.046875D&
  &0*CA2*Lambda5*MHH2*S2A*SA*SBA)/(PI2*S2B*SB) - (0.09375D0*CA2*Lambda5*MHH2*SA*SAB*SBA2)/(CB*PI2*S2B) + (0.09375D0*CA*Lambda5*MH&
  &H2*SA2*SAB*SBA2)/(PI2*S2B*SB) + (0.03125D0*CAB*CBA*EL2*MA02*Mh02)/(MW2*PI2*S2B*SW2) - (0.046875D0*CAB*CBA*EL2*Mh02*MHH2)/(MW2*&
  &PI2*S2B*SW2) + (0.0625D0*CAB*CBA*EL2*Mh02*MHp2)/(MW2*PI2*S2B*SW2) + (0.015625D0*CAB*CBA*EL2*MA02*Mh02*S2A)/(MW2*PI2*S2B2*SW2) &
  &+ (0.046875D0*CAB*CBA*EL2*Mh02*MHH2*S2A)/(MW2*PI2*S2B2*SW2) + (0.03125D0*CAB*CBA*EL2*Mh02*MHp2*S2A)/(MW2*PI2*S2B2*SW2) + (0.01&
  &5625D0*CA2*CBA*EL2*GaugeXiW*MHH2*SA)/(CB*PI2*SW2) - (0.0078125D0*CA2*CBA*EL2*MA02*MHH2*SA)/(CB*MW2*PI2*SW2) - (0.015625D0*CA2*&
  &CBA*EL2*MHH2*MHp2*SA)/(CB*MW2*PI2*SW2) + (0.09375D0*CA2*CBA*EL2*MW2*SA)/(CB*PI2*SW2) + (0.046875D0*CA2*CBA*EL2*MZ2*SA)/(CB*CW2&
  &*PI2*SW2) + (0.0078125D0*CA2*CBA*EL2*GaugeXiZ*MHH2*MZ2*SA)/(CB*MW2*PI2*SW2) + (0.0078125D0*CA2*CBA*EL2*Mh02*MHH2*S2A*SA)/(CB*M&
  &W2*PI2*S2B*SW2) - (0.015625D0*CA*CAB*EL2*MA02*Mh02*SA2)/(CB*MW2*PI2*S2B*SW2) - (0.03125D0*CA*CAB*EL2*Mh02*MHp2*SA2)/(CB*MW2*PI&
  &2*S2B*SW2) + (0.09375D0*CAB*EL2*Mh02*MHH2*SAB)/(MW2*PI2*S2B2*SW2) + (0.015625D0*CA2*EL2*MA02*MHH2*SA*SAB)/(CB*MW2*PI2*S2B*SW2)&
  & + (0.03125D0*CA2*EL2*MHH2*MHp2*SA*SAB)/(CB*MW2*PI2*S2B*SW2) - (0.015625D0*CA2*CAB*EL2*MA02*Mh02*SA)/(MW2*PI2*S2B*SB*SW2) - (0&
  &.03125D0*CA2*CAB*EL2*Mh02*MHp2*SA)/(MW2*PI2*S2B*SB*SW2) - (0.015625D0*CA*CBA*EL2*GaugeXiW*MHH2*SA2)/(PI2*SB*SW2) + (0.0078125D&
  &0*CA*CBA*EL2*MA02*MHH2*SA2)/(MW2*PI2*SB*SW2) + (0.015625D0*CA*CBA*EL2*MHH2*MHp2*SA2)/(MW2*PI2*SB*SW2) - (0.09375D0*CA*CBA*EL2*&
  &MW2*SA2)/(PI2*SB*SW2) - (0.046875D0*CA*CBA*EL2*MZ2*SA2)/(CW2*PI2*SB*SW2) - (0.0078125D0*CA*CBA*EL2*GaugeXiZ*MHH2*MZ2*SA2)/(MW2&
  &*PI2*SB*SW2) - (0.0078125D0*CA*CBA*EL2*Mh02*MHH2*S2A*SA2)/(MW2*PI2*S2B*SB*SW2) - (0.015625D0*CA*EL2*MA02*MHH2*SA2*SAB)/(MW2*PI&
  &2*S2B*SB*SW2) - (0.03125D0*CA*EL2*MHH2*MHp2*SA2*SAB)/(MW2*PI2*S2B*SB*SW2) + (0.046875D0*CA*EL2*MC2*Mh02*SA)/(MW2*PI2*SB2*SW2) &
  &+ (0.046875D0*CA*EL2*MC2*MHH2*SA)/(MW2*PI2*SB2*SW2) + (0.046875D0*CA*EL2*Mh02*MT2*SA)/(MW2*PI2*SB2*SW2) + (0.046875D0*CA*EL2*M&
  &HH2*MT2*SA)/(MW2*PI2*SB2*SW2) + (0.046875D0*CA*EL2*Mh02*MU2*SA)/(MW2*PI2*SB2*SW2) + (0.046875D0*CA*EL2*MHH2*MU2*SA)/(MW2*PI2*S&
  &B2*SW2) + (0.015625D0*CBA*EL2*GaugeXiZ*MA02*SBA)/(CW2*PI2*SW2) + (0.1875D0*CBA*EL2*MW2*SBA)/(PI2*SW2) - (0.015625D0*CBA*EL2*Ga&
  &ugeXiZ*MA02*MZ2*SBA)/(MW2*PI2*SW2) + (0.015625D0*CBA*EL2*GaugeXiW*Mh02*S2A*SBA)/(PI2*S2B*SW2) - (0.015625D0*CBA*EL2*GaugeXiW*M&
  &HH2*S2A*SBA)/(PI2*S2B*SW2) - (0.0078125D0*CBA*EL2*MA02*Mh02*S2A*SBA)/(MW2*PI2*S2B*SW2) + (0.0078125D0*CBA*EL2*MA02*MHH2*S2A*SB&
  &A)/(MW2*PI2*S2B*SW2) - (0.015625D0*CBA*EL2*Mh02*MHp2*S2A*SBA)/(MW2*PI2*S2B*SW2) + (0.015625D0*CBA*EL2*MHH2*MHp2*S2A*SBA)/(MW2*&
  &PI2*S2B*SW2) + (0.0078125D0*CBA*EL2*GaugeXiZ*Mh02*MZ2*S2A*SBA)/(MW2*PI2*S2B*SW2) - (0.0078125D0*CBA*EL2*GaugeXiZ*MHH2*MZ2*S2A*&
  &SBA)/(MW2*PI2*S2B*SW2) - (0.078125D0*CBA*EL2*Mh02*MHH2*S2A2*SBA)/(MW2*PI2*S2B2*SW2) - (0.015625D0*CA*EL2*GaugeXiW*Mh02*SA2*SBA&
  &)/(CB*PI2*SW2) + (0.0078125D0*CA*EL2*MA02*Mh02*SA2*SBA)/(CB*MW2*PI2*SW2) + (0.015625D0*CA*EL2*Mh02*MHp2*SA2*SBA)/(CB*MW2*PI2*S&
  &W2) - (0.09375D0*CA*EL2*MW2*SA2*SBA)/(CB*PI2*SW2) - (0.046875D0*CA*EL2*MZ2*SA2*SBA)/(CB*CW2*PI2*SW2) - (0.0078125D0*CA*EL2*Gau&
  &geXiZ*Mh02*MZ2*SA2*SBA)/(CB*MW2*PI2*SW2) + (0.0078125D0*CA*EL2*Mh02*MHH2*S2A*SA2*SBA)/(CB*MW2*PI2*S2B*SW2) + (0.03125D0*EL2*MA&
  &02*MHH2*SAB*SBA)/(MW2*PI2*S2B*SW2) - (0.046875D0*EL2*Mh02*MHH2*SAB*SBA)/(MW2*PI2*S2B*SW2) + (0.0625D0*EL2*MHH2*MHp2*SAB*SBA)/(&
  &MW2*PI2*S2B*SW2) - (0.015625D0*EL2*MA02*MHH2*S2A*SAB*SBA)/(MW2*PI2*S2B2*SW2) - (0.046875D0*EL2*Mh02*MHH2*S2A*SAB*SBA)/(MW2*PI2&
  &*S2B2*SW2) - (0.03125D0*EL2*MHH2*MHp2*S2A*SAB*SBA)/(MW2*PI2*S2B2*SW2) - (0.015625D0*CA2*EL2*GaugeXiW*Mh02*SA*SBA)/(PI2*SB*SW2)&
  & + (0.0078125D0*CA2*EL2*MA02*Mh02*SA*SBA)/(MW2*PI2*SB*SW2) + (0.015625D0*CA2*EL2*Mh02*MHp2*SA*SBA)/(MW2*PI2*SB*SW2) - (0.09375&
  &D0*CA2*EL2*MW2*SA*SBA)/(PI2*SB*SW2) - (0.046875D0*CA2*EL2*MZ2*SA*SBA)/(CW2*PI2*SB*SW2) - (0.0078125D0*CA2*EL2*GaugeXiZ*Mh02*MZ&
  &2*SA*SBA)/(MW2*PI2*SB*SW2) + (0.0078125D0*CA2*EL2*Mh02*MHH2*S2A*SA*SBA)/(MW2*PI2*S2B*SB*SW2) + (0.046875D0*EL2*MB2*Mh02*Yuk1*Y&
  &uk2)/(MW2*PI2*SW2) + (0.046875D0*EL2*MD2*Mh02*Yuk1*Yuk2)/(MW2*PI2*SW2) + (0.046875D0*EL2*MB2*MHH2*Yuk1*Yuk2)/(MW2*PI2*SW2) + (&
  &0.046875D0*EL2*MD2*MHH2*Yuk1*Yuk2)/(MW2*PI2*SW2) + (0.046875D0*EL2*Mh02*MS2*Yuk1*Yuk2)/(MW2*PI2*SW2) + (0.046875D0*EL2*MHH2*MS&
  &2*Yuk1*Yuk2)/(MW2*PI2*SW2) + (0.015625D0*EL2*ME2*Mh02*Yuk4*Yuk5)/(MW2*PI2*SW2) + (0.015625D0*EL2*ME2*MHH2*Yuk4*Yuk5)/(MW2*PI2*&
  &SW2) + (0.015625D0*EL2*Mh02*ML2*Yuk4*Yuk5)/(MW2*PI2*SW2) + (0.015625D0*EL2*MHH2*ML2*Yuk4*Yuk5)/(MW2*PI2*SW2) + (0.015625D0*EL2&
  &*Mh02*MM2*Yuk4*Yuk5)/(MW2*PI2*SW2) + (0.015625D0*EL2*MHH2*MM2*Yuk4*Yuk5)/(MW2*PI2*SW2) - (0.1875D0*CAB*Lambda5*Mh02*S2A*DBLE(C&
  &BA**INT(3.D0)))/(PI2*S2B2) - (0.09375D0*CAB*Lambda5*MHH2*S2A*DBLE(CBA**INT(3.D0)))/(PI2*S2B2) + (0.09375D0*CBA*EL2*MW2*SBA*DBL&
  &E(CW**INT(-4.D0)))/(PI2*SW2) - (0.015625D0*CBA*EL2*MZ2*SBA*DBLE(GaugeXiZ**INT(2.D0)))/(CW2*PI2*SW2) + (0.015625D0*CBA*EL2*MW2*&
  &SBA*DBLE(CW**INT(-4.D0))*DBLE(GaugeXiZ**INT(2.D0)))/(PI2*SW2) + (0.375D0*CAB*MW2*SAB*SW2*DBLE(Lambda5**INT(2.D0)))/(EL2*PI2*S2&
  &B2) + (0.0625D0*CBA*MW2*SBA*SW2*DBLE(Lambda5**INT(2.D0)))/(EL2*PI2) - (0.5625D0*CBA*MW2*S2A2*SBA*SW2*DBLE(Lambda5**INT(2.D0)))&
  &/(EL2*PI2*S2B2) - (0.1875D0*CAB*MW2*SW2*DBLE(CBA**INT(3.D0))*DBLE(Lambda5**INT(2.D0)))/(EL2*PI2*S2B) + (0.5625D0*CAB*MW2*S2A*S&
  &W2*DBLE(CBA**INT(3.D0))*DBLE(Lambda5**INT(2.D0)))/(EL2*PI2*S2B2) + (0.015625D0*CA2*CBA*EL2*SA*DBLE(MA0**INT(4.D0)))/(CB*MW2*PI&
  &2*SW2) - (0.015625D0*CA*CBA*EL2*SA2*DBLE(MA0**INT(4.D0)))/(MW2*PI2*SB*SW2) + (0.03125D0*CBA*EL2*SBA*DBLE(MA0**INT(4.D0)))/(MW2&
  &*PI2*SW2) - (0.015625D0*CA*EL2*SA2*SBA*DBLE(MA0**INT(4.D0)))/(CB*MW2*PI2*SW2) - (0.015625D0*CA2*EL2*SA*SBA*DBLE(MA0**INT(4.D0)&
  &))/(MW2*PI2*SB*SW2) + (0.1875D0*CA*EL2*SA2*Yuk1*DBLE(MB**INT(4.D0)))/(CB*MW2*PI2*SW2) + (0.1875D0*CA2*EL2*SA*Yuk1*DBLE(MB**INT&
  &(4.D0)))/(MW2*PI2*SB*SW2) - (0.1875D0*CA2*EL2*SA*Yuk2*DBLE(MB**INT(4.D0)))/(CB*MW2*PI2*SW2) + (0.1875D0*CA*EL2*SA2*Yuk2*DBLE(M&
  &B**INT(4.D0)))/(MW2*PI2*SB*SW2) - (0.5625D0*EL2*Yuk1*Yuk2*DBLE(MB**INT(4.D0)))/(MW2*PI2*SW2) - (0.5625D0*CA*EL2*SA*DBLE(MC**IN&
  &T(4.D0)))/(MW2*PI2*SB2*SW2) + (0.1875D0*EL2*SA*DBLE(CA**INT(3.D0))*DBLE(MC**INT(4.D0)))/(MW2*PI2*SB2*SW2) + (0.1875D0*CA*EL2*S&
  &A2*Yuk1*DBLE(MD**INT(4.D0)))/(CB*MW2*PI2*SW2) + (0.1875D0*CA2*EL2*SA*Yuk1*DBLE(MD**INT(4.D0)))/(MW2*PI2*SB*SW2) - (0.1875D0*CA&
  &2*EL2*SA*Yuk2*DBLE(MD**INT(4.D0)))/(CB*MW2*PI2*SW2) + (0.1875D0*CA*EL2*SA2*Yuk2*DBLE(MD**INT(4.D0)))/(MW2*PI2*SB*SW2) - (0.562&
  &5D0*EL2*Yuk1*Yuk2*DBLE(MD**INT(4.D0)))/(MW2*PI2*SW2) + (0.0625D0*CA*EL2*SA2*Yuk4*DBLE(ME**INT(4.D0)))/(CB*MW2*PI2*SW2) + (0.06&
  &25D0*CA2*EL2*SA*Yuk4*DBLE(ME**INT(4.D0)))/(MW2*PI2*SB*SW2) - (0.0625D0*CA2*EL2*SA*Yuk5*DBLE(ME**INT(4.D0)))/(CB*MW2*PI2*SW2) +&
  & (0.0625D0*CA*EL2*SA2*Yuk5*DBLE(ME**INT(4.D0)))/(MW2*PI2*SB*SW2) - (0.1875D0*EL2*Yuk4*Yuk5*DBLE(ME**INT(4.D0)))/(MW2*PI2*SW2) &
  &+ (0.140625D0*CAB*CBA*EL2*S2A*DBLE(Mh0**INT(4.D0)))/(MW2*PI2*S2B2*SW2) + (0.015625D0*CA2*CBA*EL2*S2A*SA*DBLE(Mh0**INT(4.D0)))/&
  &(CB*MW2*PI2*S2B*SW2) - (0.046875D0*CA*CAB*EL2*SA2*DBLE(Mh0**INT(4.D0)))/(CB*MW2*PI2*S2B*SW2) - (0.046875D0*CA2*CAB*EL2*SA*DBLE&
  &(Mh0**INT(4.D0)))/(MW2*PI2*S2B*SB*SW2) - (0.015625D0*CA*CBA*EL2*S2A*SA2*DBLE(Mh0**INT(4.D0)))/(MW2*PI2*S2B*SB*SW2) + (0.039062&
  &5D0*CBA*EL2*S2A2*SBA*DBLE(Mh0**INT(4.D0)))/(MW2*PI2*S2B2*SW2) - (0.0234375D0*CA*EL2*S2A*SA2*SBA*DBLE(Mh0**INT(4.D0)))/(CB*MW2*&
  &PI2*S2B*SW2) - (0.0234375D0*CA2*EL2*S2A*SA*SBA*DBLE(Mh0**INT(4.D0)))/(MW2*PI2*S2B*SB*SW2) - (0.0234375D0*CA2*CBA*EL2*S2A*SA*DB&
  &LE(MHH**INT(4.D0)))/(CB*MW2*PI2*S2B*SW2) + (0.046875D0*CA2*EL2*SA*SAB*DBLE(MHH**INT(4.D0)))/(CB*MW2*PI2*S2B*SW2) + (0.0234375D&
  &0*CA*CBA*EL2*S2A*SA2*DBLE(MHH**INT(4.D0)))/(MW2*PI2*S2B*SB*SW2) - (0.046875D0*CA*EL2*SA2*SAB*DBLE(MHH**INT(4.D0)))/(MW2*PI2*S2&
  &B*SB*SW2) + (0.0390625D0*CBA*EL2*S2A2*SBA*DBLE(MHH**INT(4.D0)))/(MW2*PI2*S2B2*SW2) + (0.015625D0*CA*EL2*S2A*SA2*SBA*DBLE(MHH**&
  &INT(4.D0)))/(CB*MW2*PI2*S2B*SW2) - (0.140625D0*EL2*S2A*SAB*SBA*DBLE(MHH**INT(4.D0)))/(MW2*PI2*S2B2*SW2) + (0.015625D0*CA2*EL2*&
  &S2A*SA*SBA*DBLE(MHH**INT(4.D0)))/(MW2*PI2*S2B*SB*SW2) + (0.03125D0*CA2*CBA*EL2*SA*DBLE(MHp**INT(4.D0)))/(CB*MW2*PI2*SW2) - (0.&
  &03125D0*CA*CBA*EL2*SA2*DBLE(MHp**INT(4.D0)))/(MW2*PI2*SB*SW2) + (0.0625D0*CBA*EL2*SBA*DBLE(MHp**INT(4.D0)))/(MW2*PI2*SW2) - (0&
  &.03125D0*CA*EL2*SA2*SBA*DBLE(MHp**INT(4.D0)))/(CB*MW2*PI2*SW2) - (0.03125D0*CA2*EL2*SA*SBA*DBLE(MHp**INT(4.D0)))/(MW2*PI2*SB*S&
  &W2) + (0.0625D0*CA*EL2*SA2*Yuk4*DBLE(ML**INT(4.D0)))/(CB*MW2*PI2*SW2) + (0.0625D0*CA2*EL2*SA*Yuk4*DBLE(ML**INT(4.D0)))/(MW2*PI&
  &2*SB*SW2) - (0.0625D0*CA2*EL2*SA*Yuk5*DBLE(ML**INT(4.D0)))/(CB*MW2*PI2*SW2) + (0.0625D0*CA*EL2*SA2*Yuk5*DBLE(ML**INT(4.D0)))/(&
  &MW2*PI2*SB*SW2) - (0.1875D0*EL2*Yuk4*Yuk5*DBLE(ML**INT(4.D0)))/(MW2*PI2*SW2) + (0.0625D0*CA*EL2*SA2*Yuk4*DBLE(MM**INT(4.D0)))/&
  &(CB*MW2*PI2*SW2) + (0.0625D0*CA2*EL2*SA*Yuk4*DBLE(MM**INT(4.D0)))/(MW2*PI2*SB*SW2) - (0.0625D0*CA2*EL2*SA*Yuk5*DBLE(MM**INT(4.&
  &D0)))/(CB*MW2*PI2*SW2) + (0.0625D0*CA*EL2*SA2*Yuk5*DBLE(MM**INT(4.D0)))/(MW2*PI2*SB*SW2) - (0.1875D0*EL2*Yuk4*Yuk5*DBLE(MM**IN&
  &T(4.D0)))/(MW2*PI2*SW2) + (0.1875D0*CA*EL2*SA2*Yuk1*DBLE(MS**INT(4.D0)))/(CB*MW2*PI2*SW2) + (0.1875D0*CA2*EL2*SA*Yuk1*DBLE(MS*&
  &*INT(4.D0)))/(MW2*PI2*SB*SW2) - (0.1875D0*CA2*EL2*SA*Yuk2*DBLE(MS**INT(4.D0)))/(CB*MW2*PI2*SW2) + (0.1875D0*CA*EL2*SA2*Yuk2*DB&
  &LE(MS**INT(4.D0)))/(MW2*PI2*SB*SW2) - (0.5625D0*EL2*Yuk1*Yuk2*DBLE(MS**INT(4.D0)))/(MW2*PI2*SW2) - (0.5625D0*CA*EL2*SA*DBLE(MT&
  &**INT(4.D0)))/(MW2*PI2*SB2*SW2) + (0.1875D0*EL2*SA*DBLE(CA**INT(3.D0))*DBLE(MT**INT(4.D0)))/(MW2*PI2*SB2*SW2) - (0.5625D0*CA*E&
  &L2*SA*DBLE(MU**INT(4.D0)))/(MW2*PI2*SB2*SW2) + (0.1875D0*EL2*SA*DBLE(CA**INT(3.D0))*DBLE(MU**INT(4.D0)))/(MW2*PI2*SB2*SW2) + (&
  &0.1875D0*CA*EL2*DBLE(MC**INT(4.D0))*DBLE(SA**INT(3.D0)))/(MW2*PI2*SB2*SW2) + (0.1875D0*CA*EL2*DBLE(MT**INT(4.D0))*DBLE(SA**INT&
  &(3.D0)))/(MW2*PI2*SB2*SW2) + (0.1875D0*CA*EL2*DBLE(MU**INT(4.D0))*DBLE(SA**INT(3.D0)))/(MW2*PI2*SB2*SW2) + (0.09375D0*Lambda5*&
  &Mh02*S2A*SAB*DBLE(SBA**INT(3.D0)))/(PI2*S2B2) + (0.1875D0*Lambda5*MHH2*S2A*SAB*DBLE(SBA**INT(3.D0)))/(PI2*S2B2) - (0.1875D0*MW&
  &2*SAB*SW2*DBLE(Lambda5**INT(2.D0))*DBLE(SBA**INT(3.D0)))/(EL2*PI2*S2B) - (0.5625D0*MW2*S2A*SAB*SW2*DBLE(Lambda5**INT(2.D0))*DB&
  &LE(SBA**INT(3.D0)))/(EL2*PI2*S2B2))/(-1.D0*Mh02 + MHH2) &
                        & )*UVDelta
                end function dAlphaMSBarUsual

                double precision function dBetaMSBarUsual()
                    use constants
                    implicit none
                    dBetaMSBarUsual = ( &
                        & (-0.015625D0*EL2*((3.D0*MC2)/TB + (3.D0*MT2)/TB + (3.D0*MU2)/TB - 3.D0*MB2*Yuk3 - 3.D0*MD2*Yuk3 -&
                            & 3.D0*MS2*Yuk3 - 1.D0*ME2*Yuk6 - 1.D0*ML2*Yuk6 - 1.D0*MM2*Yuk6))/(MW2*PI2*SW2) &
                        & )*UVDelta
                end function dBetaMSBarUsual

        ! 2HDM Z2-soft-breaking parameter m_{12}^2
            ! Tadpole invariant counterterms
                double precision function dm122MSBarUsual()
                    use constants
                    implicit none
                    dm122MSBarUsual = m12squared/(16D0*PI**2*(4D0*MW2*SW2/EL2))*( &
                        & 2D0*3D0*MU2*(1D0/TB - C2B/S2B)*1D0/TB + 2D0*3D0*MC2*(1D0/TB - C2B/S2B)*1D0/TB + &
                        & 2D0*3D0*MT2*(1D0/TB - C2B/S2B)*1D0/TB + 2D0*3D0*MD2*(-Yuk3 - C2B/S2B)*(-Yuk3) + &
                        & 2D0*3D0*MS2*(-Yuk3 - C2B/S2B)*(-Yuk3) + 2D0*3D0*MB2*(-Yuk3 - C2B/S2B)*(-Yuk3) + &
                        & 2D0*1D0*ME2*(-Yuk6 - C2B/S2B)*(-Yuk6) + 2D0*1D0*MM2*(-Yuk6 - C2B/S2B)*(-Yuk6) + &
                        & 2D0*1D0*ML2*(-Yuk6 - C2B/S2B)*(-Yuk6) + 8D0*m12squared/S2B - 2D0*MHp2 - MA02 + &
                        & S2A/S2B*(MHH2 - Mh02) - 3D0*(2D0*MW2 + MZ2) &
                    & )*UVDelta
                end function dm122MSBarUsual

                double precision function dm122MSBarAlter()
                    use constants
                    implicit none
                    dm122MSBarAlter = m12squared/(16D0*PI**2*(4D0*MW2*SW2/EL2))*( &
                        & 2D0*3D0*MU2*(1D0/TB - C2B/S2B)*1D0/TB + 2D0*3D0*MC2*(1D0/TB - C2B/S2B)*1D0/TB + &
                        & 2D0*3D0*MT2*(1D0/TB - C2B/S2B)*1D0/TB + 2D0*3D0*MD2*(-Yuk3 - C2B/S2B)*(-Yuk3) + &
                        & 2D0*3D0*MS2*(-Yuk3 - C2B/S2B)*(-Yuk3) + 2D0*3D0*MB2*(-Yuk3 - C2B/S2B)*(-Yuk3) + &
                        & 2D0*1D0*ME2*(-Yuk6 - C2B/S2B)*(-Yuk6) + 2D0*1D0*MM2*(-Yuk6 - C2B/S2B)*(-Yuk6) + &
                        & 2D0*1D0*ML2*(-Yuk6 - C2B/S2B)*(-Yuk6) + 8D0*m12squared/S2B - 2D0*MHp2 - MA02 + &
                        & S2A/S2B*(MHH2 - Mh02) - 3D0*(2D0*MW2 + MZ2) &
                    & )*UVDelta
                end function dm122MSBarAlter

end module counterterms
