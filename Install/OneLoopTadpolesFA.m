(* ::Package:: *)

dir = DirectoryName[$InputFileName]; 
ImportPaths = FileNameJoin[{dir, "..", "Paths.m"}]; 
Get[ImportPaths]; 
commandLine = $CommandLine;
stringlist = StringSplit[commandLine];
process = StringTrim[stringlist[[-1]], "-"][[1]];
inputList = StringSplit[process, "to"];
incomingList = StringSplit[inputList[[1]], ","];
outgoingList = StringSplit[inputList[[2]], ","];
If[StringFreeQ[inputList[[1]],"{"],
	incomingCount=Length[incomingList];
,
	incomingCount=StringCount[ToString[incomingList],{"}","{"}]/2-1;
];
If[StringFreeQ[inputList[[2]],"{"],
	outgoingCount=Length[outgoingList];
,
	outgoingCount=StringCount[ToString[outgoingList],{"}","{"}]/2-1;
];
processString = ToString[incomingList] <> "->" <> ToString[outgoingList];
$CKM = True; 
Get[FeynartsDirectory]; 
topology = CreateTopologies[1, incomingCount -> outgoingCount, ExcludeTopologies -> {Loops[Except[1]], WFCorrections}]; 
AA = InsertFields[topology, ToExpression[processString], Model -> "THDM2", InsertionLevel -> Particles]; 
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
	Export[FileNameJoin[{dir,"..","Temp","tads",StringJoin["tads", ampNumber, ".txt"]}], ampList[[i]]]; 
];
Exit[]; 
