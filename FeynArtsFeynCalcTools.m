(* ::Package:: *)

feyncalcToCpp::usage="
feyncalcToCpp[expr_]
Expects a Mathematica expression expr which is converted into a convenient form for usage in C++.
";

feyncalcToFortran::usage="
feyncalcToFortran[expr_,pre_]
Expects a Mathematica expression expr which is converted into a convenient form for usage in Fortran90. The string pre is prepended to the expression and is taken into account when creating the Fortran expression with maximum of 130 characters per line (including line continuation characters).
";

getParticleContent::usage="
getParticleContent[file_,dir_]
Reads the particle content stored in file and sets various variables for usage in the FeynArts and FeynCalc scripts. Prepends the dir to the Fortran saving paths.
";

getParticleMasses::usage="
getParticleMasses[file_]
Reads the particle masses stored in file and sets various variables for usage in the FeynArts and FeynCalc scripts.
";

feyncalcToCpp[expr_]:=Module[{exprConv},StringDelete[StringReplace[ToString[N[Replace[Replace[Replace[((expr)/.{Pi:>PI}/.{MZ^2:>MZ2,MW^2:>MW2,MA0^2:>MA02,MHH^2:>MHH2,Mh0^2:>Mh02,MHp^2:>MHp2,SW^2:>SW2,CW^2:>CW2,ME^2:>ME2,MM^2:>MM2,ML^2:>ML2,MU^2:>MU2,MC^2:>MC2,MT^2:>MT2,MD^2:>MD2,MS^2:>MS2,MB^2:>MB2,EL^2:>EL2,SA^2:>SA2,CA^2:>CA2,TA^2:>TA2,SB^2:>SB2,CB^2:>CB2,TB^2:>TB2,SAB^2:>SAB2,SBA^2:>SBA2,CAB^2:>CAB2,CBA^2:>CBA2}/.{1/MZ^2:>1/MZ2,1/MW^2:>1/MW2,1/MA0^2:>1/MA02,1/MHH^2:>1/MHH2,1/Mh0^2:>1/Mh02,1/MHp^2:>1/MHp2,1/SW^2:>1/SW2,1/CW^2:>1/CW2,1/ME^2:>1/ME2,1/MM^2:>1/MM2,1/ML^2:>1/ML2,1/MU^2:>1/MU2,1/MC^2:>1/MC2,1/MT^2:>1/MT2,1/MD^2:>1/MD2,1/MS^2:>1/MS2,1/MB^2:>1/MB2,1/EL^2:>1/EL2,1/SA^2:>1/SA2,1/CA^2:>1/CA2,1/TA^2:>1/TA2,1/SB^2:>1/SB2,1/CB^2:>1/CB2,1/TB^2:>1/TB2,1/SAB^2:>1/SAB2,1/SBA^2:>1/SBA2,1/CAB^2:>1/CAB2,1/CBA^2:>1/CBA2}),{Conjugate[CKM[a_,b_]]:>CKMC[a,b],Conjugate[CKMC[c_,d_]]:>CKM[c,d]},Infinity],{CKM[a_,b_]:>ToExpression["CKM"<>ToString[a]<>ToString[b]],CKMC[c_,d_]:>ToExpression["CKMC"<>ToString[c]<>ToString[d]],Power[x_,N_]:>pow[x,N]/;N!=-1,x_^N_:>pow[x,N]/;N!=-1,GaugeXi[W]:>GaugeXiW,GaugeXi[A]:>GaugeXiA,GaugeXi[Z]:>GaugeXiZ}],{CKM[a_,b_]:>ToExpression["CKM"<>ToString[a]<>ToString[b]],CKMC[c_,d_]:>ToExpression["CKMC"<>ToString[c]<>ToString[d]],Power[x_,N_]:>pow[x,N]/;N!=-1,x_^N_:>pow[x,N]/;N!=-1,GaugeXi[W]:>GaugeXiW,GaugeXi[A]:>GaugeXiA,GaugeXi[Z]:>GaugeXiZ},Infinity]],InputForm],{"["->"(","]"->")","Re"->"std::real","Im"->"std::imag"}], "\n" | "\r"]];

feyncalcToFortran[expr_,pre_]:=Module[{exprConv},
exportFortranTemp="";
tempStringPre=(pre<>StringTrim[StringReplace[StringReplace[StringReplace[ToString[N[Replace[Replace[Replace[((expr)/.{Pi:>PI}/.{PI^2:>PI2,MZ^2:>MZ2,MW^2:>MW2,MA0^2:>MA02,MHH^2:>MHH2,Mh0^2:>Mh02,MHp^2:>MHp2,SW^2:>SW2,CW^2:>CW2,ME^2:>ME2,MM^2:>MM2,ML^2:>ML2,MU^2:>MU2,MC^2:>MC2,MT^2:>MT2,MD^2:>MD2,MS^2:>MS2,MB^2:>MB2,EL^2:>EL2,SA^2:>SA2,CA^2:>CA2,TA^2:>TA2,SB^2:>SB2,CB^2:>CB2,TB^2:>TB2,SAB^2:>SAB2,SBA^2:>SBA2,CAB^2:>CAB2,CBA^2:>CBA2,S2A^2:>S2A2,S2B^2:>S2B2,C2A^2:>C2A2,C2B^2:>C2B2}/.{1/PI^2:>1/PI2,1/MZ^2:>1/MZ2,1/MW^2:>1/MW2,1/MA0^2:>1/MA02,1/MHH^2:>1/MHH2,1/Mh0^2:>1/Mh02,1/MHp^2:>1/MHp2,1/SW^2:>1/SW2,1/CW^2:>1/CW2,1/ME^2:>1/ME2,1/MM^2:>1/MM2,1/ML^2:>1/ML2,1/MU^2:>1/MU2,1/MC^2:>1/MC2,1/MT^2:>1/MT2,1/MD^2:>1/MD2,1/MS^2:>1/MS2,1/MB^2:>1/MB2,1/EL^2:>1/EL2,1/SA^2:>1/SA2,1/CA^2:>1/CA2,1/TA^2:>1/TA2,1/SB^2:>1/SB2,1/CB^2:>1/CB2,1/TB^2:>1/TB2,1/SAB^2:>1/SAB2,1/SBA^2:>1/SBA2,1/CAB^2:>1/CAB2,1/CBA^2:>1/CBA2,1/S2A^2:>1/S2A2,1/S2B^2:>1/S2B2,1/C2A^2:>1/C2A2,1/C2B^2:>1/C2B2}),{Conjugate[CKM[a_,b_]]:>CKMC[a,b],Conjugate[CKMC[c_,d_]]:>CKM[c,d]},Infinity],{CKM[a_,b_]:>ToExpression["CKM"<>ToString[a]<>ToString[b]],CKMC[c_,d_]:>ToExpression["CKMC"<>ToString[c]<>ToString[d]],Power[x_,N_]:>DBLE[x**INT[N]]/;N!=-1&&(N-IntegerPart[N]==0),x_^N_:>DBLE[x**INT[N]]/;N!=-1&&(N-IntegerPart[N]==0),GaugeXi[W]:>GaugeXiW,GaugeXi[A]:>GaugeXiA,GaugeXi[Z]:>GaugeXiZ}],{CKM[a_,b_]:>ToExpression["CKM"<>ToString[a]<>ToString[b]],CKMC[c_,d_]:>ToExpression["CKMC"<>ToString[c]<>ToString[d]],Power[x_,N_]:>DBLE[x**INT[N]]/;N!=-1&&(N-IntegerPart[N]==0),x_^N_:>DBLE[x**INT[N]]/;N!=-1&&(N-IntegerPart[N]==0),GaugeXi[W]:>GaugeXiW,GaugeXi[A]:>GaugeXiA,GaugeXi[Z]:>GaugeXiZ},Infinity]],InputForm],{"["->"(","]"->")","Re"->"std::real","Im"->"std::imag"}], {"\n":>"","\r":>""}],RegularExpression[" +"]->" "]]);
toBeReplaced=StringCases[tempStringPre,RegularExpression["[0-9]*\\.[0-9]*"]];

toBeReplacedPrepared=tempStringPre;
For[m=1,m<=Length[toBeReplaced],m++,
toBeReplacedArray["NUMBERTOREPLACE"<>ToString[m]]=toBeReplaced[[m]];
toBeReplacedPrepared=StringReplace[toBeReplacedPrepared,toBeReplaced[[m]]:>("NUMBERTOREPLACE"<>ToString[m]),1];
];
toBeReplacedPrepared2=toBeReplacedPrepared;
For[m=1,m<=Length[toBeReplaced],m++,
toBeReplacedPrepared2=StringReplace[toBeReplacedPrepared2,("NUMBERTOREPLACE"<>ToString[m]):>(toBeReplacedArray[("NUMBERTOREPLACE"<>ToString[m])]<>"D0"),1];
];
tempString=toBeReplacedPrepared2;
If[StringLength[tempString]<129,
exportFortranTemp=tempString;,
While[StringLength[tempString]>129,
exportFortranTemp=exportFortranTemp<>StringTake[tempString,129]<>"&\n";
tempString="  &"<>StringDrop[tempString,129];
]
If[StringLength[tempString]>0,
exportFortranTemp=exportFortranTemp<>StringTake[tempString,StringLength[tempString]];
]
];
exportFortranTemp];

getParticleContent[file_,dir_]:=Module[{exprConv},
importFile = Import[file];
ToExpression[StringReplace[StringTake[importFile,{StringPosition[importFile,RegularExpression["vectorSelfEnergies = .*?(?=\]\])"]][[1]][[1]],StringPosition[importFile,RegularExpression["vectorSelfEnergies = .*?(?=\]\])"]][[1]][[2]]+1}]<>"]",{"["->"{","]"->"}"}]];
ToExpression[StringReplace[StringTake[importFile,{StringPosition[importFile,RegularExpression["vectorSelfTypes = .*?(?=\]\])"]][[1]][[1]],StringPosition[importFile,RegularExpression["vectorSelfTypes = .*?(?=\]\])"]][[1]][[2]]+1}]<>"]",{"["->"{","]"->"}"}]];
ToExpression[StringReplace[StringTake[importFile,{StringPosition[importFile,RegularExpression["scalarSelfEnergies = .*?(?=\]\])"]][[1]][[1]],StringPosition[importFile,RegularExpression["scalarSelfEnergies = .*?(?=\]\])"]][[1]][[2]]+1}]<>"]",{"["->"{","]"->"}"}]];
ToExpression[StringReplace[StringTake[importFile,{StringPosition[importFile,RegularExpression["scalarSelfTypes = .*?(?=\]\])"]][[1]][[1]],StringPosition[importFile,RegularExpression["scalarSelfTypes = .*?(?=\]\])"]][[1]][[2]]+1}]<>"]",{"["->"{","]"->"}"}]];
ToExpression[StringReplace[StringTake[importFile,{StringPosition[importFile,RegularExpression["fermionSelfEnergies = .*?(?=\]\]\])"]][[1]][[1]],StringPosition[importFile,RegularExpression["fermionSelfEnergies = .*?(?=\]\]\])"]][[1]][[2]]+1}]<>"]"<>"]",{"["->"{","]"->"}"}]];
ToExpression[StringReplace[StringTake[importFile,{StringPosition[importFile,RegularExpression["fermionSelfTypes = .*?(?=\]\])"]][[1]][[1]],StringPosition[importFile,RegularExpression["fermionSelfTypes = .*?(?=\]\])"]][[1]][[2]]+1}]<>"]",{"["->"{","]"->"}"}]];
ToExpression[StringReplace[StringTake[importFile,{StringPosition[importFile,RegularExpression["scalarTadpoles = .*?(?=\]\])"]][[1]][[1]],StringPosition[importFile,RegularExpression["scalarTadpoles = .*?(?=\]\])"]][[1]][[2]]+1}]<>"]",{"["->"{","]"->"}"}]];
ToExpression[StringReplace[StringTake[importFile,{StringPosition[importFile,RegularExpression["scalarTadpoleTypes = .*?(?=\]\])"]][[1]][[1]],StringPosition[importFile,RegularExpression["scalarTadpoleTypes = .*?(?=\]\])"]][[1]][[2]]+1}]<>"]",{"["->"{","]"->"}"}]];
ToExpression[StringTake[importFile,{StringPosition[importFile,RegularExpression["feynartsScalars = .*?(?=})"]][[1]][[1]],StringPosition[importFile,RegularExpression["feynartsScalars = .*?(?=})"]][[1]][[2]]+1}]];
ToExpression[StringTake[importFile,{StringPosition[importFile,RegularExpression["feynartsVectors = .*?(?=})"]][[1]][[1]],StringPosition[importFile,RegularExpression["feynartsVectors = .*?(?=})"]][[1]][[2]]+1}]];
ToExpression[StringTake[importFile,{StringPosition[importFile,RegularExpression["feynartsFermions = .*?(?=})"]][[1]][[1]],StringPosition[importFile,RegularExpression["feynartsFermions = .*?(?=})"]][[1]][[2]]+1}]];
scalarSelfEnergiesParticles={};For[k=1,k<=Length[scalarSelfEnergies],k++,scalarSelfEnergiesParticles=Append[scalarSelfEnergiesParticles,feynartsScalars[[scalarSelfEnergies[[k]][[1]]]]<>feynartsScalars[[scalarSelfEnergies[[k]][[2]]]]];];
vectorSelfEnergiesParticles={};For[k=1,k<=Length[vectorSelfEnergies],k++,vectorSelfEnergiesParticles=Append[vectorSelfEnergiesParticles,feynartsVectors[[vectorSelfEnergies[[k]][[1]]]]<>feynartsVectors[[vectorSelfEnergies[[k]][[2]]]]];];
fermionSelfEnergiesParticles={};For[k=1,k<=Length[fermionSelfEnergies],k++,fermionSelfEnergiesParticles=Append[fermionSelfEnergiesParticles,feynartsFermions[[(fermionSelfEnergies[[k]][[1]][[1]]-1)*3+fermionSelfEnergies[[k]][[1]][[2]]]]<>feynartsFermions[[(fermionSelfEnergies[[k]][[2]][[1]]-1)*3+fermionSelfEnergies[[k]][[2]][[2]]]]];];
scalarTadpolesParticles={};For[k=1,k<=Length[scalarTadpoles],k++,scalarTadpolesParticles=Append[scalarTadpolesParticles,feynartsScalars[[scalarTadpoles[[k]][[1]]]]];];
scalarSelfEnergiesFAamp={};For[l=1,l<=Length[scalarSelfEnergiesParticles],l++,scalarSelfEnergiesFAamp=Append[scalarSelfEnergiesFAamp,"Self"<>scalarSelfEnergiesParticles[[l]]];];
vectorSelfEnergiesFAamp={};For[l=1,l<=Length[vectorSelfEnergiesParticles],l++,vectorSelfEnergiesFAamp=Append[vectorSelfEnergiesFAamp,"Self"<>vectorSelfEnergiesParticles[[l]]];];
fermionSelfEnergiesFAamp={};For[l=1,l<=Length[fermionSelfEnergiesParticles],l++,fermionSelfEnergiesFAamp=Append[fermionSelfEnergiesFAamp,"Self"<>fermionSelfEnergiesParticles[[l]]];];
scalarSelfEnergiesSaver={};For[l=1,l<=Length[scalarSelfEnergiesParticles],l++,scalarSelfEnergiesSaver=Append[scalarSelfEnergiesSaver,FileNameJoin[{dir, "Self"<>scalarSelfEnergiesParticles[[l]]<>".txt"}]];];
vectorSelfEnergiesSaver={};For[l=1,l<=Length[vectorSelfEnergiesParticles],l++,vectorSelfEnergiesSaver=Append[vectorSelfEnergiesSaver,FileNameJoin[{dir, "Self"<>vectorSelfEnergiesParticles[[l]]<>".txt"}]];];
fermionSelfEnergiesSaver={};For[l=1,l<=Length[fermionSelfEnergiesParticles],l++,fermionSelfEnergiesSaver=Append[fermionSelfEnergiesSaver,FileNameJoin[{dir, "Self"<>fermionSelfEnergiesParticles[[l]]<>"Left.txt"}]];fermionSelfEnergiesSaver=Append[fermionSelfEnergiesSaver,FileNameJoin[{dir, "Self"<>fermionSelfEnergiesParticles[[l]]<>"Right.txt"}]];fermionSelfEnergiesSaver=Append[fermionSelfEnergiesSaver,FileNameJoin[{dir, "Self"<>fermionSelfEnergiesParticles[[l]]<>"Scalar.txt"}]];];
scalarSelfEnergiesDerivSaver={};For[l=1,l<=Length[scalarSelfEnergiesParticles],l++,scalarSelfEnergiesDerivSaver=Append[scalarSelfEnergiesDerivSaver,FileNameJoin[{dir, "..", "..", "SelfEnergiesDerivatives", "DSelf"<>scalarSelfEnergiesParticles[[l]]<>".txt"}]];];
vectorSelfEnergiesDerivSaver={};For[l=1,l<=Length[vectorSelfEnergiesParticles],l++,vectorSelfEnergiesDerivSaver=Append[vectorSelfEnergiesDerivSaver,FileNameJoin[{dir, "..", "..", "SelfEnergiesDerivatives", "DSelf"<>vectorSelfEnergiesParticles[[l]]<>".txt"}]];];
fermionSelfEnergiesDerivSaver={};For[l=1,l<=Length[fermionSelfEnergiesParticles],l++,fermionSelfEnergiesDerivSaver=Append[fermionSelfEnergiesDerivSaver,FileNameJoin[{dir, "..", "..", "SelfEnergiesDerivatives", "DSelf"<>fermionSelfEnergiesParticles[[l]]<>"Left.txt"}]];fermionSelfEnergiesDerivSaver=Append[fermionSelfEnergiesDerivSaver,FileNameJoin[{dir, "..", "..", "SelfEnergiesDerivatives", "DSelf"<>fermionSelfEnergiesParticles[[l]]<>"Right.txt"}]];fermionSelfEnergiesDerivSaver=Append[fermionSelfEnergiesDerivSaver,FileNameJoin[{dir, "..", "..", "SelfEnergiesDerivatives", "DSelf"<>fermionSelfEnergiesParticles[[l]]<>"Scalar.txt"}]];];
scalarTadpolesSaver={};For[l=1,l<=Length[scalarTadpolesParticles],l++,scalarTadpolesSaver=Append[scalarTadpolesSaver,FileNameJoin[{dir, "Tad"<>scalarTadpolesParticles[[l]]<>".txt"}]];];
scalarTadpolesFAamp={};For[l=1,l<=Length[scalarTadpolesParticles],l++,scalarTadpolesFAamp=Append[scalarTadpolesFAamp,"Tad"<>scalarTadpolesParticles[[l]]];];
Clear[k,l];
];

getParticleMasses[file_]:=Module[{exprConv},
importFile = Import[file];
ToExpression[StringReplace[StringTake[importFile,{StringPosition[importFile,RegularExpression["particleMasses = .*?(?=\]\])"]][[1]][[1]],StringPosition[importFile,RegularExpression["particleMasses = .*?(?=\]\])"]][[1]][[2]]+1}]<>"]",{"["->"{","]"->"}"}]];
For[m=1,m<=Length[particleMasses],m++,
particleMassesList[StringReplace[particleMasses[[m]][[1]],{"{"->"[","}"->"]"}]]=particleMasses[[m]][[2]];
];
Clear[m];
];
