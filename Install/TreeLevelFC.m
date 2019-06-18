(* ::Package:: *)

dir = DirectoryName[$InputFileName]; 
ImportPaths = FileNameJoin[{dir, "..", "Paths.m"}]; 
Get[ImportPaths]; 
ImportFile = FileNameJoin[{dir, "..", "Temp", "tree", "amp.txt"}]; 
$LoadFeynArts = $LoadPhi = False; 
Get[FeyncalcDirectory]; 
amp = ToExpression[Import[ImportFile]]; 
FAamp = Simplify[DiracSimplify2[Contract[amp]]]/.{SumOver[SUNFIndex[Col_], c_, External]:>1,IndexDelta[SUNFIndex[Col2_], SUNFIndex[Col3_]]:>1};
FAampConj = ComplexConjugate[FAamp];
Export[FileNameJoin[{dir, "..", "Temp", "treeRes", "amp.txt"}], FAamp]; 
Export[FileNameJoin[{dir, "..", "Temp", "treeRes", "ampConj.txt"}], FAampConj]; 
Exit[];
