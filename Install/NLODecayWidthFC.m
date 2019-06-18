(* ::Package:: *)

dir = DirectoryName[$InputFileName]; 
ImportPaths = FileNameJoin[{dir, "..", "Paths.m"}]; 
Get[ImportPaths]; 
commandLine = $CommandLine; 
stringlist = StringSplit[commandLine]; 
ampFolder = StringTrim[stringlist[[-1]], "-"][[1]]; 
$LoadFeynArts = $LoadPhi = False; 
Get[FeyncalcDirectory]; 
Get[FeynArtsFeynCalcToolsDirectory];
mOut2 = ToExpression[StringTrim[stringlist[[-2]], "-"][[1]]]; 
mOut1 = ToExpression[StringTrim[stringlist[[-3]], "-"][[1]]]; 
mIn1 = ToExpression[StringTrim[stringlist[[-4]], "-"][[1]]]; 
ImportFileAmpConj = FileNameJoin[{dir, "..", "BuildingBlocks", "Processes", ampFolder, "TreeLevel", "treeConj.txt"}]; 
ImportFilesNLOAmps = FileNames["*.txt", FileNameJoin[{dir, "..", "BuildingBlocks", "Processes", ampFolder, "VertexCorrections"}]];
ImportFilesNLOTadAmps = FileNames["*.txt", FileNameJoin[{dir, "..", "BuildingBlocks", "Processes", ampFolder, "VertexTadpoles"}]];
\[Lambda][a_,b_,c_]:=Sqrt[a^2+b^2+c^2-2*a*b-2*a*c-2*b*c];
polarizationSums={EMPTY:>EMPTY};
polarizationSums2={EMPTY:>EMPTY};
ToExpression[Import[FileNameJoin[{dir, "..", "BuildingBlocks", "Processes", ampFolder, "polarizationReplace.txt"}]]];
momentaReplace = {p1:>mIn1, p3:>mOut1, p4:>mOut2};
ampConj = ToExpression[Import[ImportFileAmpConj]]; 
exportFortran = "";
exportFortran = exportFortran <> "double complex function " <> ampFolder <> "VC()\n";
exportFortran = exportFortran <> " use constants\n";
exportFortran = exportFortran <> " implicit none\n";
exportFortran = exportFortran <> "#include \"looptools.h\"\n";
exportFortran = exportFortran <> " integer :: j\n";
exportFortran = exportFortran <> " double complex :: totalAmplitude\n";
exportFortran = exportFortran <> " double complex :: amplitudes("<>ToString[Length[ImportFilesNLOAmps]]<>")\n\n";
For[i=1,i<=Length[ImportFilesNLOAmps],i++,
	ampNLO = ToExpression[Import[ImportFilesNLOAmps[[i]]]];
	ampTreeSquared = ampNLO*ampConj;
	ampTreeSquaredPolSums = (ampTreeSquared/.polarizationSums/.polarizationSums2)/.momentaReplace;	
	If[(needsSpecialTreatment/.polarizationSums),
		If[Not[NumericQ[(overallColorFactor/.polarizationSums)]],
			colorFactor=1;
		,
			colorFactor=(overallColorFactor/.polarizationSums);
		];
		ampExp=Expand[ampTreeSquared/.{StandardMatrixElement[a__]:>a}];
		ampSpinSum=FermionSpinSum[ampExp];
		ampSpinTrace=(ampSpinSum/.{DiracTrace:>Tr}/.{Pair[Momentum[p3], Momentum[p4]] :> (mIn1^2 - mOut1^2 - mOut2^2)/2})*colorFactor;
		ampTreeSquaredPolSums = Simplify[ampSpinTrace/.momentaReplace];
	,
		ampTreeSquaredPolSums = (ampTreeSquared/.polarizationSums/.polarizationSums2)/.momentaReplace;
	];
	export = (ampTreeSquaredPolSums/.{C0[0,x_,x_,0,0,b_]:>C0Mine[DBLE[0],DBLE[x],DBLE[x],DBLE[0],DBLE[0],DBLE[b]],D0[__]:>D0Mine[0,0,0,0,0,0,0,0,0,0]}); 
	exportFortran=exportFortran<>feyncalcToFortran[export," amplitudes("<>ToString[i]<>") = "]<>"\n\n";
	Print["VC diagram "<>ToString[i]<>" done."];
];
exportFortran=exportFortran<>"  totalAmplitude = (0D0,0D0)\n";
exportFortran=exportFortran<>" do j=1,"<>ToString[Length[ImportFilesNLOAmps]]<>"\n";
exportFortran=exportFortran<>"  totalAmplitude = totalAmplitude + amplitudes(j)\n";
exportFortran=exportFortran<>" end do\n\n";
exportFortran=exportFortran<>" " <> ampFolder <> "VC = totalAmplitude\n";
exportFortran=exportFortran<>"end function " <> ampFolder <> "VC";
exportFortran=StringReplace[exportFortran,"Global`"->""];
filename = FileNameJoin[{dir, "..", "BuildingBlocks", "Processes", ampFolder, "NLOWidthRed.txt"}];
Export[filename, exportFortran]; 
If[FileExistsQ[StringReplace[filename,".txt"->".F90"]],DeleteFile[StringReplace[filename,".txt"->".F90"]];];
RenameFile[filename,StringReplace[filename,".txt"->".F90"]];
exportTadFortran = "";
exportTadFortran = exportTadFortran <> "double complex function " <> ampFolder <> "Tad()\n";
exportTadFortran = exportTadFortran <> " use constants\n";
exportTadFortran = exportTadFortran <> " implicit none\n";
exportTadFortran = exportTadFortran <> "#include \"looptools.h\"\n";
exportTadFortran = exportTadFortran <> " integer :: j\n";
exportTadFortran = exportTadFortran <> " double complex :: totalAmplitude\n";
If[Length[ImportFilesNLOTadAmps]!=0,
	exportTadFortran = exportTadFortran <> " double complex :: amplitudes("<>ToString[Length[ImportFilesNLOTadAmps]]<>")\n\n";
	For[i=1,i<=Length[ImportFilesNLOTadAmps],i++,
		ampNLO = ToExpression[Import[ImportFilesNLOTadAmps[[i]]]];
		ampTreeSquared = ampNLO*ampConj;
		ampTreeSquaredPolSums = (ampTreeSquared/.polarizationSums/.polarizationSums2)/.momentaReplace;
		If[(needsSpecialTreatment/.polarizationSums),
			If[Not[NumericQ[(overallColorFactor/.polarizationSums)]],
				colorFactor=1;
			,
				colorFactor=(overallColorFactor/.polarizationSums);
			];
			ampExp=Expand[ampTreeSquared/.{StandardMatrixElement[a__]:>a}];
			ampSpinSum=FermionSpinSum[ampExp];
			ampSpinTrace=(ampSpinSum/.{DiracTrace:>Tr}/.{Pair[Momentum[p3], Momentum[p4]] :> (mIn1^2 - mOut1^2 - mOut2^2)/2})*colorFactor;
			ampTreeSquaredPolSums = Simplify[ampSpinTrace/.momentaReplace];
		,
			ampTreeSquaredPolSums = (ampTreeSquared/.polarizationSums/.polarizationSums2)/.momentaReplace;
		];
		export = (ampTreeSquaredPolSums/.{C0[0,x_,x_,0,0,b_]:>C0Mine[DBLE[0],DBLE[x],DBLE[x],DBLE[0],DBLE[0],DBLE[b]],D0[__]:>D0Mine[0,0,0,0,0,0,0,0,0,0]}); 
		exportTadFortran=exportTadFortran<>feyncalcToFortran[export," amplitudes("<>ToString[i]<>") = "]<>"\n\n";
		Print["Tadpole-VC diagram "<>ToString[i]<>" done."];
	];
	exportTadFortran=exportTadFortran<>"  totalAmplitude = (0D0,0D0)\n";
	exportTadFortran=exportTadFortran<>" do j=1,"<>ToString[Length[ImportFilesNLOTadAmps]]<>"\n";
	exportTadFortran=exportTadFortran<>"  totalAmplitude = totalAmplitude + amplitudes(j)\n";
	exportTadFortran=exportTadFortran<>" end do\n\n";,
	exportTadFortran = exportTadFortran <> " double complex :: amplitudes(1)\n\n";
	exportTadFortran = exportTadFortran <> " amplitudes(1) = (0D0,0D0)\n\n";
	exportTadFortran=exportTadFortran<>"  totalAmplitude = (0D0,0D0)\n";
	exportTadFortran=exportTadFortran<>" do j=1,1\n";
	exportTadFortran=exportTadFortran<>"  totalAmplitude = totalAmplitude + amplitudes(j)\n";
	exportTadFortran=exportTadFortran<>" end do\n\n";
];
exportTadFortran=exportTadFortran<>" " <> ampFolder <> "Tad = totalAmplitude\n";
exportTadFortran=exportTadFortran<>"end function " <> ampFolder <> "Tad";
exportTadFortran=StringReplace[exportTadFortran,"Global`"->""];
filename = FileNameJoin[{dir, "..", "BuildingBlocks", "Processes", ampFolder, "NLOTadWidthRed.txt"}];
Export[filename, exportTadFortran]; 
If[FileExistsQ[StringReplace[filename,".txt"->".F90"]],DeleteFile[StringReplace[filename,".txt"->".F90"]];];
RenameFile[filename,StringReplace[filename,".txt"->".F90"]];
Exit[];
