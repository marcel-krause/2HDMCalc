(* ::Package:: *)

dir = DirectoryName[$InputFileName]; 
commandLine = StringSplit[$CommandLine[[4]],".m "][[2]];
argv = StringSplit[commandLine, " "];
ImportPaths = FileNameJoin[{dir, "..", "Paths.m"}]; 
Get[ImportPaths]; 
$LoadFeynArts = $LoadPhi = False; 
Get[FeyncalcDirectory]; 
Get[FeynArtsFeynCalcToolsDirectory];
SetOptions[B1,BReduce->False];
Switch[argv[[2]],
"1", targetDir = "Usual";,
"2", targetDir = "Alternative";,
_, Exit[];]
ampVersion = StringTake[targetDir,5];
ImportFile = FileNameJoin[{dir, "..", "BuildingBlocks", "SelfEnergies", targetDir, "SelfEnergyTauTauQEDFA.txt"}]; 
ToExpression[Import[ImportFile]]; 
p1 = p3 = k; 
fermionParts = {"Left", "Right", "Scalar"};
truncated = {Spinor[Momentum[p_],m_,1]:>1,Spinor[Momentum[p_],m_,1]:>1};
ampCounter = 1;
For[n=1, n<=Length[fermionParts], n++,
	export = 0; 
	exportLeftHanded=0;
	exportRightHanded=0;
	exportScalar=0;
	exportFortran = "";
	exportFortran = exportFortran <> "double complex function SelfTauTau" <> fermionParts[[n]] <> "QED" <> ampVersion <> "(x)\n";
	exportFortran = exportFortran <> " use constants\n";
	exportFortran = exportFortran <> " implicit none\n";
	exportFortran = exportFortran <> "#include \"looptools.h\"\n";
	exportFortran = exportFortran <> " double precision, intent(in) :: x\n";
	exportFortran = exportFortran <> " integer :: j\n";
	exportFortran = exportFortran <> " double complex :: totalAmplitude\n";
	exportFortran = exportFortran <> " double complex :: amplitudes("<>ToString[Length[SelfTauTauQED]]<>")\n\n";
	exportFortranDeriv = "";
	exportFortranDeriv = exportFortranDeriv <> "double complex function DSelfTauTau" <> fermionParts[[n]] <> "QED(x)\n";
	exportFortranDeriv = exportFortranDeriv <> " use constants\n";
	exportFortranDeriv = exportFortranDeriv <> " implicit none\n";
	exportFortranDeriv = exportFortranDeriv <> "#include \"looptools.h\"\n";
	exportFortranDeriv = exportFortranDeriv <> " double precision, intent(in) :: x\n";
	exportFortranDeriv = exportFortranDeriv <> " integer :: j\n";
	exportFortranDeriv = exportFortranDeriv <> " double complex :: totalAmplitude\n";
	exportFortranDeriv = exportFortranDeriv <> " double complex :: amplitudes("<>ToString[Length[SelfTauTauQED]]<>")\n\n";
	For[i = 1, i <= Length[SelfTauTauQED], i++, 
		Print["Calculating sub-amplitude number "<> ToString[ampCounter] <>" ..."];
		ampCounter++;
		tracer = (((SelfTauTauQED[[i]]/.truncated)/.{PropagatorDenominator[0,0]:>PropagatorDenominator[0,\[Mu]]})/.{DiracTrace -> Tr}); 
		If[tracer == 0, exportFortran=exportFortran<>" amplitudes("<>ToString[i]<>") = 0D0"<>"\n\n"; 
			Print["Sub-amplitude number "<>ToString[ampCounter]<>" finished."];
			Continue[];
		];
		If[FreeQ[tracer, l], 
			exportFortran=exportFortran<>" amplitudes("<>ToString[i]<>") = 0D0"<>"\n\n"; 
			Continue[];
		];
		simpler = OneLoopSimplify[tracer, l];
		oneloop = OneLoop[l, simpler];
        result = Simplify[PaVeReduce[oneloop /. {Pair[Momentum[k], Momentum[k]] -> x}]];
		resultTemp = result;
		resultLeftHanded = Coefficient[resultTemp,\!\(TraditionalForm\`Dot[DiracGamma[Momentum[p]], DiracGamma[7]]\),1];
		resultTemp = Coefficient[resultTemp,\!\(TraditionalForm\`Dot[DiracGamma[Momentum[p]], DiracGamma[7]]\),0];
		resultLeftHanded += Coefficient[resultTemp,\!\(TraditionalForm\`DiracGamma[Momentum[k]] . DiracGamma[7]\),1];
		resultTemp = Coefficient[resultTemp,\!\(TraditionalForm\`DiracGamma[Momentum[k]] . DiracGamma[7]\),0];
		resultRightHanded = Coefficient[resultTemp,Dot[DiracGamma[Momentum[p]],DiracGamma[6]],1];
		resultTemp = Coefficient[resultTemp,\!\(TraditionalForm\`Dot[DiracGamma[Momentum[p]], DiracGamma[6]]\),0];
		resultRightHanded += Coefficient[resultTemp,DiracGamma[Momentum[k]].DiracGamma[6],1];
		resultTemp = Coefficient[resultTemp,DiracGamma[Momentum[k]].DiracGamma[6],0];
		resultScalar = (Simplify[resultTemp]/.{(DiracGamma[6]+DiracGamma[7]):>1})/ML;
		Switch[n,
			1, exportFinal = resultLeftHanded/.{C0[0,x_,x_,0,0,b_]:>C0Mine[DBLE[0],DBLE[x],DBLE[x],DBLE[0],DBLE[0],DBLE[b]]}; exportToDeriv = resultLeftHanded;,
			2, exportFinal = resultRightHanded/.{C0[0,x_,x_,0,0,b_]:>C0Mine[DBLE[0],DBLE[x],DBLE[x],DBLE[0],DBLE[0],DBLE[b]]}; exportToDeriv = resultRightHanded;,
			3, exportFinal = resultScalar/.{C0[0,x_,x_,0,0,b_]:>C0Mine[DBLE[0],DBLE[x],DBLE[x],DBLE[0],DBLE[0],DBLE[b]]}; exportToDeriv = resultScalar;
		];
		exportFortran=exportFortran<>feyncalcToFortran[exportFinal," amplitudes("<>ToString[i]<>") = "]<>"\n\n";
		resultDeriv = D[exportToDeriv,x];
		exportDeriv = (resultDeriv/.{C0[0,x_,x_,0,0,b_]:>C0Mine[DBLE[0],DBLE[x],DBLE[x],DBLE[0],DBLE[0],DBLE[b]]}/.{Derivative[1,0,0][B00][x_,m1_,m2_]:>DB00[DBLE[x],DBLE[m1],DBLE[m2]],Derivative[1,0,0][B11][x_,m1_,m2_]:>DB11[DBLE[x],DBLE[m1],DBLE[m2]],Derivative[0,1,0,0,0,0][C0][0,x_,x_,0,0,m_]:>DC01Mine[DBLE[0],DBLE[x],DBLE[x],DBLE[0],DBLE[0],DBLE[m]],Derivative[0,0,1,0,0,0][C0][0,x_,x_,0,0,m_]:>DC02Mine[DBLE[0],DBLE[x],DBLE[x],DBLE[0],DBLE[0],DBLE[m]]});
		exportFortranDeriv=exportFortranDeriv<>feyncalcToFortran[exportDeriv," amplitudes("<>ToString[i]<>") = "]<>"\n\n";
		Print["Sub-amplitude number "<>ToString[ampCounter]<>" finished."];
		ClearAll[tracer, simpler, oneloop, result, resultDeriv, export, exportDeriv];
	];
	exportFortran=exportFortran<>"  totalAmplitude = (0D0,0D0)\n";
	exportFortran=exportFortran<>" do j=1,"<>ToString[Length[SelfTauTauQED]]<>"\n";
	exportFortran=exportFortran<>"  totalAmplitude = totalAmplitude + amplitudes(j)\n";
	exportFortran=exportFortran<>" end do\n";
	exportFortran=exportFortran<>" SelfTauTau" <> fermionParts[[n]] <> "QED" <> ampVersion <> " = totalAmplitude\n";
	exportFortran=exportFortran<>"end function SelfTauTau" <> fermionParts[[n]] <> "QED" <> ampVersion <> "\n\n";
	exportFortranDeriv=exportFortranDeriv<>"  totalAmplitude = (0D0,0D0)\n";
	exportFortranDeriv=exportFortranDeriv<>" do j=1,"<>ToString[Length[SelfTauTauQED]]<>"\n";
	exportFortranDeriv=exportFortranDeriv<>"  totalAmplitude = totalAmplitude + amplitudes(j)\n";
	exportFortranDeriv=exportFortranDeriv<>" end do\n";
	exportFortranDeriv=exportFortranDeriv<>" DSelfTauTau" <> fermionParts[[n]] <> "QED = totalAmplitude\n";
	exportFortranDeriv=exportFortranDeriv<>"end function DSelfTauTau" <> fermionParts[[n]] <> "QED\n\n";
	filename = FileNameJoin[{dir, "..", "BuildingBlocks", "SelfEnergies", targetDir, "SelfTauTau" <> fermionParts[[n]] <> "QED.txt"}];
	Export[filename, exportFortran]; 
	If[FileExistsQ[StringReplace[filename,".txt"->".F90"]],DeleteFile[StringReplace[filename,".txt"->".F90"]];];
	RenameFile[filename,StringReplace[filename,".txt"->".F90"]];
	filenameDeriv = FileNameJoin[{dir, "..", "BuildingBlocks", "SelfEnergiesDerivatives", "DSelfTauTau" <> fermionParts[[n]] <> "QED.txt"}];
	Export[filenameDeriv, exportFortranDeriv]; 
	If[FileExistsQ[StringReplace[filenameDeriv,".txt"->".F90"]],DeleteFile[StringReplace[filenameDeriv,".txt"->".F90"]];];
	RenameFile[filenameDeriv,StringReplace[filenameDeriv,".txt"->".F90"]];
];
DeleteFile[ImportFile];
Exit[];
