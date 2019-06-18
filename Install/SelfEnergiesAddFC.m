(* ::Package:: *)

dir = DirectoryName[$InputFileName]; 
ImportPaths = FileNameJoin[{dir, "..", "Paths.m"}]; 
Get[ImportPaths]; 
$LoadFeynArts = $LoadPhi = False; 
Get[FeyncalcDirectory]; 
Get[FeynArtsFeynCalcToolsDirectory];
selfies = {"SelfHHh0Add", "SelfG0A0Add", "SelfGpHpAdd"};
selfiesExpressions = {EL^2*SBA*CBA/(32*Pi^2*CW^2*SW^2)*(x-(MHH^2+Mh0^2)/2)*(B0[x,MZ^2,MA0^2]-B0[x,MZ^2,MZ^2]+2*CW^2*(B0[x,MW^2,MHp^2]-B0[x,MW^2,MW^2])), EL^2*SBA*CBA/(32*Pi^2*CW^2*SW^2)*(x-MA0^2/2)*(B0[x,MZ^2,MHH^2]-B0[x,MZ^2,Mh0^2]), EL^2*SBA*CBA/(16*Pi^2*SW^2)*(x-MHp^2/2)*(B0[x,MW^2,MHH^2]-B0[x,MW^2,Mh0^2])};
For[counter=1,counter<=Length[selfies],counter++,
	Print["Calculating additional self-energy contribution "<> selfies[[counter]] <> " ..."];
	export = 0; 
	exportFortran = "";
	exportFortran = exportFortran <> "double complex function " <> selfies[[counter]] <> "(x)\n";
	exportFortran = exportFortran <> " use constants\n";
	exportFortran = exportFortran <> " implicit none\n";
	exportFortran = exportFortran <> "#include \"looptools.h\"\n";
	exportFortran = exportFortran <> " double precision, intent(in) :: x\n";
	exportFortran = exportFortran <> " double complex :: totalAmplitude\n\n";
	export = selfiesExpressions[[counter]];
	exportFortran=exportFortran<>feyncalcToFortran[export," totalAmplitude = "]<>"\n\n";
	exportFortran=exportFortran<>" " <> selfies[[counter]] <> " = totalAmplitude\n";
	exportFortran=exportFortran<>"end function " <> selfies[[counter]] <> "\n\n";
	filename = FileNameJoin[{dir, "..", "BuildingBlocks", "SelfEnergies", "Alternative", selfies[[counter]] <> ".txt"}];
	Export[filename, exportFortran]; 
	If[FileExistsQ[StringReplace[filename,".txt"->".F90"]],DeleteFile[StringReplace[filename,".txt"->".F90"]];];
	RenameFile[filename,StringReplace[filename,".txt"->".F90"]];
	Print["Additional self-energy contribution "<> selfies[[counter]] <> " finished."];
	Clear[FAamp, simpler, result, export, filename, exportFortran]; 
];
Exit[]; 
