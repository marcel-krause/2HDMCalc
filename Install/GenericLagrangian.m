(* ::Package:: *)

parameters = {MW -> 80.3979999999999961`17.90524524526876, MZ -> 91.1876000000000033`17.959935785502633, Mh0 -> 125.`18.09691001300806, ME -> 0.00051099891, MM -> 0.105658367, ML -> 1.77684, MU -> 0.19, MC -> 1.4, 
    MT -> 172.5`18.236789099409293, MD -> 0.19, MS -> 0.19, MB -> 4.75, EL -> 0.3081471920854466, Alfa -> 0.0075562543, SW -> 0.4718536355831076, CW -> 0.8816768946655027, SW2 -> 0.2226458534129961, 
    CW2 -> 0.7773541465870039, MW2 -> 6463.8384039999991728`19.810490490537518, MZ2 -> 8315.1783937600011996`19.919871571005267, Mh02 -> 15625.`20.193820026016112, ME2 -> 2.61119886*^-7, MM2 -> 0.0111636905171067, 
    ML2 -> 3.1571603856, MU2 -> 0.0361, MC2 -> 1.9599999999999997, MT2 -> 29756.25`20.47357819881859, MD2 -> 0.0361, MS2 -> 0.0361, MB2 -> 22.5625`17.353387219249733, EL2 -> 0.0949546919901451, Alfa2 -> 0.0000570969790463, 
    MHH -> 230.`18.361727836017593, MA0 -> 290.`18.462397997898957, MHp -> 340.`18.531478917042257, TB -> 5., CB -> 0.196116135138184, SB -> 0.9805806756909201, TA -> 0.4782608695652174, CA -> 0.9021342216356465, 
    SA -> 0.4314554973040049, C2A -> 0.6276923076923075, S2A -> 0.7784615384615384, CAB -> -0.2461538461538462, SAB -> 0.9692307692307691, CBA -> 0.6, SBA -> 0.7999999999999998, M1 -> -4.8217*^-12, M2 -> -4.822*^-13, 
    MKan -> 205.2315765178447862`18.312244181256727, Lambda1 -> 0., Lambda2 -> 0., Lambda3 -> 0., Lambda4 -> 0., Lambda5 -> 1.3895356530712122, vev -> 246.2205697015816384`18.391324331839524, 
    MHH2 -> 52900.`20.723455672035186, MA02 -> 84100.`20.92479599579791, MHp2 -> 115600.`21.062957834084514, TB2 -> 25.`17.39794000867204, CB2 -> 0.0384615384615384, SB2 -> 0.9615384615384615, S2B -> 0.3846153846153845, 
    C2B -> -0.923076923076923, TA2 -> 0.2287334593572779, CA2 -> 0.8138461538461538, SA2 -> 0.1861538461538462, C2A2 -> 0.3939976331360945, S2A2 -> 0.6060023668639053, CAB2 -> 0.0605917159763314, SAB2 -> 0.9394082840236684, 
    CBA2 -> 0.36, SBA2 -> 0.6399999999999997, Yuk1 -> 0.92, Yuk2 -> 0.4400000000000001, Yuk3 -> -0.2, CKM[2, 3] -> 0.0411012287728911, CKM[3, 1] -> 0.0052401347621813, CKM[3, 2] -> -0.040974490775551, 
    CKM[3, 3] -> 0.9991464517743925, CKM[1, 1] -> 0.9742791272990069, CKM[1, 2] -> 0.2253067358280383, CKM[1, 3] -> 0.00413, CKM[2, 1] -> -0.2252836503103578, CKM[2, 2] -> 0.9734258913220867, 
    CKMC[1, 1] -> 0.9742791272990069, CKMC[1, 2] -> 0.2253067358280383, CKMC[1, 3] -> 0.00413, CKMC[2, 1] -> -0.2252836503103578, CKMC[2, 2] -> 0.9734258913220867, CKMC[2, 3] -> 0.0411012287728911, 
    CKMC[3, 1] -> 0.0052401347621813, CKMC[3, 2] -> -0.040974490775551, CKMC[3, 3] -> 0.9991464517743925, alpha -> 0.4461055489434037, beta -> 1.373400766945016}; 
\[CapitalPhi]1[x_] = {\[Omega]1p[x], (\[Rho]1[x] + v1 + I*\[Eta]1[x])/Sqrt[2]}; 
\[CapitalPhi]2[x_] = {\[Omega]2p[x], (\[Rho]2[x] + v2 + I*\[Eta]2[x])/Sqrt[2]}; 
\[CapitalPhi]1\[Dagger][x_] = Simplify[ComplexExpand[Conjugate[\[CapitalPhi]1[x] /. {\[Omega]1p[x_] :> \[Omega]1m[x], Derivative[1][\[Omega]1p][x_] :> Derivative[1][\[Omega]1m][x], \[Omega]2p[x_] :> \[Omega]2m[x], Derivative[1][\[Omega]2p][x_] :> Derivative[1][\[Omega]2m][x]}]]]; 
\[CapitalPhi]2\[Dagger][x_] = Simplify[ComplexExpand[Conjugate[\[CapitalPhi]2[x] /. {\[Omega]1p[x_] :> \[Omega]1m[x], Derivative[1][\[Omega]1p][x_] :> Derivative[1][\[Omega]1m][x], \[Omega]2p[x_] :> \[Omega]2m[x], Derivative[1][\[Omega]2p][x_] :> Derivative[1][\[Omega]2m][x]}]]]; 
massbasis = {\[Rho]1[x_] :> (RotationMatrix[\[Alpha]] . {H0[x], h0[x]})[[1]], \[Rho]2[x_] :> (RotationMatrix[\[Alpha]] . {H0[x], h0[x]})[[2]], \[Eta]1[x_] :> (RotationMatrix[\[Beta]] . {G0[x], A0[x]})[[1]], 
    \[Eta]2[x_] :> (RotationMatrix[\[Beta]] . {G0[x], A0[x]})[[2]], \[Omega]1p[x_] :> (RotationMatrix[\[Beta]] . {Gp[x], Hp[x]})[[1]], \[Omega]2p[x_] :> (RotationMatrix[\[Beta]] . {Gp[x], Hp[x]})[[2]], 
    \[Omega]1m[x_] :> (RotationMatrix[\[Beta]] . {Gm[x], Hm[x]})[[1]], \[Omega]2m[x_] :> (RotationMatrix[\[Beta]] . {Gm[x], Hm[x]})[[2]]}; 
massbasisderivatives = {Derivative[1][\[Rho]1][x_] :> (RotationMatrix[\[Alpha]] . {Derivative[1][H0][x], Derivative[1][h0][x]})[[1]], Derivative[1][\[Rho]2][x_] :> (RotationMatrix[\[Alpha]] . {Derivative[1][H0][x], Derivative[1][h0][x]})[[2]], 
    Derivative[1][\[Eta]1][x_] :> (RotationMatrix[\[Beta]] . {Derivative[1][G0][x], Derivative[1][A0][x]})[[1]], Derivative[1][\[Eta]2][x_] :> (RotationMatrix[\[Beta]] . {Derivative[1][G0][x], Derivative[1][A0][x]})[[2]], 
    Derivative[1][\[Omega]1p][x_] :> (RotationMatrix[\[Beta]] . {Derivative[1][Gp][x], Derivative[1][Hp][x]})[[1]], Derivative[1][\[Omega]2p][x_] :> (RotationMatrix[\[Beta]] . {Derivative[1][Gp][x], Derivative[1][Hp][x]})[[2]], 
    Derivative[1][\[Omega]1m][x_] :> (RotationMatrix[\[Beta]] . {Derivative[1][Gm][x], Derivative[1][Hm][x]})[[1]], Derivative[1][\[Omega]2m][x_] :> (RotationMatrix[\[Beta]] . {Derivative[1][Gm][x], Derivative[1][Hm][x]})[[2]]}; 
massbasiselectroweak = {W1[x_] :> (Wm[x] + Wp[x])/Sqrt[2], W2[x_] :> (Wp[x] - Wm[x])*(I/Sqrt[2]), W3[x_] :> Cos[\[Theta]W]*Z[x] + Sin[\[Theta]W]*A[x], B[x_] :> (-Sin[\[Theta]W])*Z[x] + Cos[\[Theta]W]*A[x]}; 
fieldzero = {\[Omega]1p[x_] :> 0, \[Omega]1m[x_] :> 0, \[Omega]2p[x_] :> 0, \[Omega]2m[x_] :> 0, \[Rho]1[x_] :> 0, \[Rho]2[x_] :> 0, \[Eta]1[x_] :> 0, \[Eta]2[x_] :> 0}; 
massfieldzero = {H0[x_] :> 0, h0[x_] :> 0, A0[x_] :> 0, G0[x_] :> 0, Gp[x_] :> 0, Hp[x_] :> 0, Gm[x_] :> 0, Hm[x_] :> 0, Z[x_] :> 0, Wp[x_] :> 0, Wm[x_] :> 0, A[x_] :> 0}; 
Dmu = dmu*IdentityMatrix[2] - I*g1*((W1[x]*PauliMatrix[1] + W2[x]*PauliMatrix[2] + W3[x]*PauliMatrix[3])/2) - (I/2)*g2*B[x]*IdentityMatrix[2]; 
D1 = Expand[ComplexExpand[Expand[Dmu . \[CapitalPhi]1[x]] /. {dmu*(f_)[x_] :> D[f[x], x], dmu*v1 :> 0, dmu*v2 :> 0}]]; 
D1\[Dagger] = Simplify[ComplexExpand[Conjugate[D1] /. {\[Omega]1p[x_] :> \[Omega]1m[x], Derivative[1][\[Omega]1p][x_] :> Derivative[1][\[Omega]1m][x], \[Omega]2p[x_] :> \[Omega]2m[x], Derivative[1][\[Omega]2p][x_] :> Derivative[1][\[Omega]2m][x]}]]; 
T1 = (D1 /. massbasiselectroweak /. massbasis /. massbasisderivatives) . (D1\[Dagger] /. massbasiselectroweak /. massbasis /. massbasisderivatives); 
D2 = Expand[ComplexExpand[Expand[Dmu . \[CapitalPhi]2[x]] /. {dmu*(f_)[x_] :> D[f[x], x], dmu*v1 :> 0, dmu*v2 :> 0}]]; 
D2\[Dagger] = Simplify[ComplexExpand[Conjugate[D2] /. {\[Omega]1p[x_] :> \[Omega]1m[x], Derivative[1][\[Omega]1p][x_] :> Derivative[1][\[Omega]1m][x], \[Omega]2p[x_] :> \[Omega]2m[x], Derivative[1][\[Omega]2p][x_] :> Derivative[1][\[Omega]2m][x]}]]; 
T2 = (D2 /. massbasiselectroweak /. massbasis /. massbasisderivatives) . (D2\[Dagger] /. massbasiselectroweak /. massbasis /. massbasisderivatives); 
T = ComplexExpand[T1 + T2]; 
Tconv = ComplexExpand[T /. {v1 -> 2*MW*(Cos[\[Beta]]/g), v2 -> 2*MW*(Sin[\[Beta]]/g), g2 -> Tan[\[Theta]W]*g1} /. {g1 -> g} /. {g -> e/Sin[\[Theta]W]}]; 
V = \[Lambda]1*(\[CapitalPhi]1\[Dagger] . \[CapitalPhi]1 - v1^2)^2 + \[Lambda]2*(\[CapitalPhi]2\[Dagger] . \[CapitalPhi]2 - v2^2)^2 + \[Lambda]3*((\[CapitalPhi]1\[Dagger] . \[CapitalPhi]1 - v1^2) + (\[CapitalPhi]2\[Dagger] . \[CapitalPhi]2 - v2^2))^2 + \[Lambda]4*(\[CapitalPhi]1\[Dagger] . \[CapitalPhi]1*\[CapitalPhi]2\[Dagger] . \[CapitalPhi]2 - \[CapitalPhi]1\[Dagger] . \[CapitalPhi]2*\[CapitalPhi]2\[Dagger] . \[CapitalPhi]1) + \[Lambda]5*(Re[\[CapitalPhi]1\[Dagger] . \[CapitalPhi]2] - v1*v2)^2 + 
    \[Lambda]6*Im[\[CapitalPhi]1\[Dagger] . \[CapitalPhi]2]^2; 
Vfields = ComplexExpand[V /. {\[CapitalPhi]1 :> \[CapitalPhi]1[x], \[CapitalPhi]1\[Dagger] :> \[CapitalPhi]1\[Dagger][x], \[CapitalPhi]2 :> \[CapitalPhi]2[x], \[CapitalPhi]2\[Dagger] :> \[CapitalPhi]2\[Dagger][x]}]; 
Vmass = Expand[Vfields /. massbasis /. {v1 -> 2*MW*(Cos[\[Beta]]/g), v2 -> 2*MW*(Sin[\[Beta]]/g), g2 -> Tan[\[Theta]W]*g1} /. {g1 -> g} /. {g -> e/Sin[\[Theta]W]}]; 
fieldlist = {H0[x], h0[x], Gp[x], Hp[x], Gm[x], Hm[x], G0[x], A0[x], Wp[x], Wm[x], A[x], Z[x]}; 
fieldlist2 = {H0, h0, Gp, Hp, Gm, Hm, G0, A0, Wp, Wm, A, Z, H0, h0, Gp, Hp, Gm, Hm, G0, A0, Wp, Wm, A, Z}; 
fieldlist3 = Join[fieldlist, D[fieldlist, x]]; 
ScalarSector = {H0[x_] -> ({{Sqrt[ZH0H0], Sqrt[ZH0h0]}, {Sqrt[Zh0H0], Sqrt[Zh0h0]}} . {H0[x], h0[x]})[[1]], h0[x_] -> ({{Sqrt[ZH0H0], Sqrt[ZH0h0]}, {Sqrt[Zh0H0], Sqrt[Zh0h0]}} . {H0[x], h0[x]})[[2]], 
    G0[x_] -> ({{Sqrt[ZG0G0], Sqrt[ZG0A0]}, {Sqrt[ZA0G0], Sqrt[ZA0A0]}} . {G0[x], A0[x]})[[1]], A0[x_] -> ({{Sqrt[ZG0G0], Sqrt[ZG0A0]}, {Sqrt[ZA0G0], Sqrt[ZA0A0]}} . {G0[x], A0[x]})[[2]], 
    Gp[x_] -> ({{Sqrt[ZGpGp], Sqrt[ZGpHp]}, {Sqrt[ZHpGp], Sqrt[ZHpHp]}} . {Gp[x], Hp[x]})[[1]], Hp[x_] -> ({{Sqrt[ZGpGp], Sqrt[ZGpHp]}, {Sqrt[ZHpGp], Sqrt[ZHpHp]}} . {Gp[x], Hp[x]})[[2]], 
    Gm[x_] -> ({{Sqrt[ZGmGm], Sqrt[ZGmHm]}, {Sqrt[ZHmGm], Sqrt[ZHmHm]}} . {Gm[x], Hm[x]})[[1]], Hm[x_] -> ({{Sqrt[ZGmGm], Sqrt[ZGmHm]}, {Sqrt[ZHmGm], Sqrt[ZHmHm]}} . {Gm[x], Hm[x]})[[2]]}; 
ElectroweakSector = {Wp[x_] -> Sqrt[ZWW]*Wp[x], Wm[x_] -> Sqrt[ZWW]*Wm[x], Z[x_] -> ({{Sqrt[ZZZ], Sqrt[ZZA]}, {Sqrt[ZAZ], Sqrt[ZAA]}} . {Z[x], A[x]})[[1]], 
    A[x_] -> ({{Sqrt[ZZZ], Sqrt[ZZA]}, {Sqrt[ZAZ], Sqrt[ZAA]}} . {Z[x], A[x]})[[2]]}; 
L = 0; 
Tred = Tconv; 
Vred = Vmass; 
Print["Calculate SVV contributions from the kinetic terms ..."];
For[i = 1, i <= Length[fieldlist], i++, For[j = 1, j <= Length[fieldlist], j++, For[k = 1, k <= Length[fieldlist], k++, 
    If[(Simplify[Coefficient[Tred, fieldlist[[i]]*fieldlist[[j]]*fieldlist[[k]], 1] /. massfieldzero] /. {\[Theta]W -> ArcCos[CW], e -> EL, \[Alpha] -> alpha, \[Beta] -> beta} /. parameters) != 0, 
     prefactor = ""; For[o = 1, o <= Length[fieldlist], o++, If[Exponent[fieldlist2[[i]]*fieldlist2[[j]]*fieldlist2[[k]], fieldlist2[[o]]] != 0, 
        If[Exponent[fieldlist2[[i]]*fieldlist2[[j]]*fieldlist2[[k]], fieldlist2[[o]]] != 1, prefactor = StringJoin[prefactor, StringJoin[ToString[Exponent[fieldlist2[[i]]*fieldlist2[[j]]*fieldlist2[[k]], fieldlist2[[o]]]], 
            "!"]], Continue[]], Continue[]]]; If[prefactor == "", prefactor = "1", prefactor = prefactor; ]; numprefactor = ToExpression[StringJoin["(", prefactor, ")^(-1)"]]; 
      L += numprefactor*ToExpression[StringJoin["g", ToString[fieldlist2[[i]]], ToString[fieldlist2[[j]]], ToString[fieldlist2[[k]]]]]*fieldlist[[i]]*fieldlist[[j]]*fieldlist[[k]]; 
      Tred = Coefficient[Tred, fieldlist[[i]]*fieldlist[[j]]*fieldlist[[k]], 0]; , Null; ]]]];
Print["Done."];
Print["Calculate SSVV contributions from the kinetic terms ..."];
For[i = 1, i <= Length[fieldlist], i++, For[j = 1, j <= Length[fieldlist], j++, For[k = 1, k <= Length[fieldlist], k++, For[m = 1, m <= Length[fieldlist], m++, 
     If[(Simplify[Coefficient[Tred, fieldlist[[i]]*fieldlist[[j]]*fieldlist[[k]]*fieldlist[[m]], 1]] /. {\[Theta]W -> ArcCos[CW], e -> EL, \[Alpha] -> alpha, \[Beta] -> beta} /. parameters) != 0, 
      prefactor = ""; For[o = 1, o <= Length[fieldlist], o++, If[Exponent[fieldlist2[[i]]*fieldlist2[[j]]*fieldlist2[[k]]*fieldlist2[[m]], fieldlist2[[o]]] != 0, 
         If[Exponent[fieldlist2[[i]]*fieldlist2[[j]]*fieldlist2[[k]]*fieldlist2[[m]], fieldlist2[[o]]] != 1, 
          prefactor = StringJoin[prefactor, StringJoin[ToString[Exponent[fieldlist2[[i]]*fieldlist2[[j]]*fieldlist2[[k]]*fieldlist2[[m]], fieldlist2[[o]]]], "!"]], Continue[]], Continue[]]]; 
       If[prefactor == "", prefactor = "1", prefactor = prefactor; ]; numprefactor = ToExpression[StringJoin["(", prefactor, ")^(-1)"]]; 
       L += numprefactor*ToExpression[StringJoin["g", ToString[fieldlist2[[i]]], ToString[fieldlist2[[j]]], ToString[fieldlist2[[k]]], ToString[fieldlist2[[m]]]]]*fieldlist[[i]]*fieldlist[[j]]*fieldlist[[k]]*fieldlist[[m]]; 
       Tred = Coefficient[Tred, fieldlist[[i]]*fieldlist[[j]]*fieldlist[[k]]*fieldlist[[m]], 0]; , Null; ]]]]];
Print["Done."];
Print["Calculate SSV contributions from the kinetic terms ..."];
For[i = 1, i <= Length[fieldlist3], i++, For[j = 1, j <= Length[fieldlist3], j++, For[k = 1, k <= Length[fieldlist3], k++, 
    If[(Simplify[Coefficient[Tred, fieldlist3[[i]]*fieldlist3[[j]]*fieldlist3[[k]], 1] /. massfieldzero] /. {\[Theta]W -> ArcCos[CW], e -> EL, \[Alpha] -> alpha, \[Beta] -> beta} /. parameters) != 0, 
     prefactor = ""; For[o = 1, o <= Length[fieldlist3], o++, If[Exponent[fieldlist3[[i]]*fieldlist3[[j]]*fieldlist3[[k]], fieldlist3[[o]]] != 0, 
        If[Exponent[fieldlist3[[i]]*fieldlist3[[j]]*fieldlist3[[k]], fieldlist3[[o]]] != 1, prefactor = StringJoin[prefactor, StringJoin[ToString[Exponent[fieldlist3[[i]]*fieldlist3[[j]]*fieldlist3[[k]], fieldlist3[[o]]]], 
            "!"]], Continue[]], Continue[]]]; If[prefactor == "", prefactor = "1", prefactor = prefactor; ]; numprefactor = ToExpression[StringJoin["(", prefactor, ")^(-1)"]]; 
      L += numprefactor*ToExpression[StringJoin["g", ToString[fieldlist2[[i]]], ToString[fieldlist2[[j]]], ToString[fieldlist2[[k]]]]]*fieldlist3[[i]]*fieldlist3[[j]]*fieldlist3[[k]] /. {Derivative[1][f_][x] :> f[x]}; 
      Tred = Coefficient[Tred, fieldlist3[[i]]*fieldlist3[[j]]*fieldlist3[[k]], 0] /. {Derivative[1][fieldlist2[[i]]][x] :> 0}; , Null; ]]]];
Print["Done."];
Print["Calculate SSSS contributions from the scalar potential ..."];
For[i = 1, i <= 8, i++, For[j = 1, j <= 8, j++, For[k = 1, k <= 8, k++, For[m = 1, m <= 8, m++, 
     If[(Simplify[Coefficient[Vred, fieldlist[[i]]*fieldlist[[j]]*fieldlist[[k]]*fieldlist[[m]], 1]] /. {\[Theta]W -> ArcCos[CW], e -> EL, 1/e -> 1/EL, \[Alpha] -> alpha, \[Beta] -> beta, \[Lambda]1 :> 21.234885, \[Lambda]2 :> 1.234885, \[Lambda]3 :> 5.234885, 
          \[Lambda]4 :> 9.234885, \[Lambda]5 :> Lambda5, \[Lambda]6 :> 9.234885, \[Mu]1 :> 1.238945, \[Mu]2 :> 9.2484} /. parameters) != 0, 
      prefactor = ""; For[o = 1, o <= 8, o++, If[Exponent[fieldlist2[[i]]*fieldlist2[[j]]*fieldlist2[[k]]*fieldlist2[[m]], fieldlist2[[o]]] != 0, 
         If[Exponent[fieldlist2[[i]]*fieldlist2[[j]]*fieldlist2[[k]]*fieldlist2[[m]], fieldlist2[[o]]] != 1, 
          prefactor = StringJoin[prefactor, StringJoin[ToString[Exponent[fieldlist2[[i]]*fieldlist2[[j]]*fieldlist2[[k]]*fieldlist2[[m]], fieldlist2[[o]]]], "!"]], Continue[]], Continue[]]]; 
       If[prefactor == "", prefactor = "1", prefactor = prefactor; ]; numprefactor = ToExpression[StringJoin["(", prefactor, ")^(-1)"]]; 
       L += numprefactor*ToExpression[StringJoin["g", ToString[fieldlist2[[i]]], ToString[fieldlist2[[j]]], ToString[fieldlist2[[k]]], ToString[fieldlist2[[m]]]]]*fieldlist[[i]]*fieldlist[[j]]*fieldlist[[k]]*fieldlist[[m]]; 
       Vred = Coefficient[Vred /. {fieldlist[[i]]*fieldlist[[j]]*fieldlist[[k]]*fieldlist[[m]] :> temp}, temp, 0]; , Null; ]]]]];
Print["Done."];
Print["Calculate SSS contributions from the scalar potential ..."];
For[i = 1, i <= 8, i++, For[j = 1, j <= 8, j++, For[k = 1, k <= 8, k++, 
    If[(Simplify[Coefficient[Vred, fieldlist[[i]]*fieldlist[[j]]*fieldlist[[k]], 1] /. massfieldzero] /. {\[Theta]W -> ArcCos[CW], e -> EL, 1/e -> 1/EL, \[Alpha] -> alpha, \[Beta] -> beta, \[Lambda]1 :> 21.234885, \[Lambda]2 :> 1.234885, \[Lambda]3 :> 5.234885, 
         \[Lambda]4 :> 9.234885, \[Lambda]5 :> Lambda5, \[Lambda]6 :> 9.234885, \[Mu]1 :> 1.238945, \[Mu]2 :> 9.2484} /. parameters) != 0, 
     prefactor = ""; For[o = 1, o <= 8, o++, If[Exponent[fieldlist2[[i]]*fieldlist2[[j]]*fieldlist2[[k]], fieldlist2[[o]]] != 0, If[Exponent[fieldlist2[[i]]*fieldlist2[[j]]*fieldlist2[[k]], fieldlist2[[o]]] != 1, 
         prefactor = StringJoin[prefactor, StringJoin[ToString[Exponent[fieldlist2[[i]]*fieldlist2[[j]]*fieldlist2[[k]], fieldlist2[[o]]]], "!"]], Continue[]], Continue[]]]; 
      If[prefactor == "", prefactor = "1", prefactor = prefactor; ]; numprefactor = ToExpression[StringJoin["(", prefactor, ")^(-1)"]]; 
      L += numprefactor*ToExpression[StringJoin["g", ToString[fieldlist2[[i]]], ToString[fieldlist2[[j]]], ToString[fieldlist2[[k]]]]]*fieldlist[[i]]*fieldlist[[j]]*fieldlist[[k]]; 
      Vred = Coefficient[Vred /. {fieldlist[[i]]*fieldlist[[j]]*fieldlist[[k]] :> temp}, temp, 0]; , Null; ]]]];
Print["Done."];
dir = DirectoryName[$InputFileName];
filename = FileNameJoin[{dir, "..", "BuildingBlocks", "Lagrangian", "GenericLagrangian.txt"}];
Export[filename, ToString[InputForm[L]]]; 
Exit[]; 
