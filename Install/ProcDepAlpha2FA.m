(* ::Package:: *)

dir = DirectoryName[$InputFileName]; 
ImportPaths = FileNameJoin[{dir, "..", "Paths.m"}]; 
Get[ImportPaths]; 
$CKM = True; 
Get[FeynartsDirectory]; 
topology = CreateTopologies[1, 1 -> 2, ExcludeTopologies -> {Internal}]; 
AA = InsertFields[topology, S[1]->{-F[2,{3}],F[2,{3}]}, Model -> "THDM2", InsertionLevel -> Particles, LastSelections->{!V[1]}]; 
amp = CreateFeynAmp[AA, GaugeRules -> {}]; 
amplitudes = PickLevel[Particles][amp]; 
amplituderange = Range[1, Length[amplitudes]]; 
FAamp = ""; 
ToExpression[Import[FeynartsparserDirectory]]; 
ampString = StringJoin["{", FAamp, "}"]; 
ampList = ToExpression[ampString]; 
For[i = 1, i <= Length[ampList], i++, 
	If[i < 10,
		ampNumber = "0"<>ToString[i];,
		ampNumber = ToString[i];
	];
	Export[FileNameJoin[{dir,"..","Temp","procDepAlpha2",StringJoin["amp", ampNumber, ".txt"]}], ampList[[i]]]; 
];
Exit[]; 
