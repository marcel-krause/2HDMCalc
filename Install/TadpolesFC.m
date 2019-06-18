(* ::Package:: *)

dir = DirectoryName[$InputFileName]; 
ImportPaths = FileNameJoin[{dir, "..", "Paths.m"}]; 
Get[ImportPaths]; 
$LoadFeynArts = $LoadPhi = False; 
Get[FeyncalcDirectory]; 
Get[FeynArtsFeynCalcToolsDirectory];
selfies := scalarTadpolesFAamp; 
saver := scalarTadpolesSaver; 
targetfile = "ScalarTadpolesFA.txt";
getParticleContent[ParticleContentFile,FileNameJoin[{dir, "..", "BuildingBlocks", "Tadpoles"}]];
ImportFile = FileNameJoin[{dir, "..", "BuildingBlocks", "Tadpoles", targetfile}]; 
ToExpression[Import[ImportFile]]; 
p1 = p3 = k; 
For[counter=1,counter<=Length[selfies],counter++,
Print["Calculating tadpole "<> selfies[[counter]] <> " ..."];
export = 0; 
exportFortran = "";
exportFortran = exportFortran <> "double complex function " <> selfies[[counter]] <> "()\n";
exportFortran = exportFortran <> " use constants\n";
exportFortran = exportFortran <> " implicit none\n";
exportFortran = exportFortran <> "#include \"looptools.h\"\n";
exportFortran = exportFortran <> " integer :: j\n";
exportFortran = exportFortran <> " double complex :: totalAmplitude\n";
exportFortran = exportFortran <> " double complex :: amplitudes("<>ToString[Length[ToExpression[selfies[[counter]]]]]<>")\n\n";
For[i = 1, i <= Length[ToExpression[selfies[[counter]]]], i++, 
 Print["Calculating sub-amplitude number "<>ToString[i]<>" ..."];
  tracer = (((ToExpression[selfies[[counter]]][[i]])/.{PropagatorDenominator[0,0]:>PropagatorDenominator[0,\[Mu]]})/.{DiracTrace -> Tr}); If[tracer == 0, exportFortran=exportFortran<>" amplitudes("<>ToString[i]<>") = 0D0"<>"\n\n"; Continue[]; ]; If[FreeQ[tracer, l], exportFortran=exportFortran<>" amplitudes("<>ToString[i]<>") = 0D0"<>"\n\n"; Continue[]; ];
   simpler = OneLoopSimplify[tracer, l]; oneloop = OneLoop[l, simpler]; 
   result = Simplify[PaVeReduce[oneloop /. {Pair[Momentum[k], Momentum[k]] -> x}]]; export = (result/.{C0[0,x_,x_,0,0,b_]:>C0Mine[DBLE[0],DBLE[x],DBLE[x],DBLE[0],DBLE[0],DBLE[b]]}); exportFortran=exportFortran<>feyncalcToFortran[export," amplitudes("<>ToString[i]<>") = "]<>"\n\n";
	Print["Sub-amplitude number "<>ToString[i]<>" finished."];   
	ClearAll[tracer, simpler, oneloop, result, export]; ];
exportFortran=exportFortran<>"  totalAmplitude = (0D0,0D0)\n";
exportFortran=exportFortran<>" do j=1,"<>ToString[Length[ToExpression[selfies[[counter]]]]]<>"\n";
exportFortran=exportFortran<>"  totalAmplitude = totalAmplitude + amplitudes(j)\n";
exportFortran=exportFortran<>" end do\n";
exportFortran=exportFortran<>" " <> selfies[[counter]] <> " = totalAmplitude\n";
exportFortran=exportFortran<>"end function " <> selfies[[counter]] <> "\n\n";
filename = saver[[counter]]; 
Export[filename, exportFortran]; 
If[FileExistsQ[StringReplace[filename,".txt"->".F90"]],DeleteFile[StringReplace[filename,".txt"->".F90"]];];
RenameFile[filename,StringReplace[filename,".txt"->".F90"]];
Print["Tadpole "<> selfies[[counter]] <> " finished."];
Clear[FAamp, simpler, result, export, filename, exportFortran]; 
]
DeleteFile[ImportFile];
Exit[]; 
