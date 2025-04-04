(* ::Package:: *)

(* ::Input:: *)
(*(*Define the values of jj for classification*)jjValues={10,20,100,300,500,1000};*)
(**)
(*(*Classification function*)*)
(*ClassifyEigenvalues[jj_]:=Module[{eigenvaluesRaw,eigenvalues,pqValues,modulusApprox1,remainingEigensols,hyperbolic,misurewicz,parabolic,siegelDisks,others,expectedCount,actualCount},(*Load Precomputed Eigenvalues from.wl file*)eigenvaluesRaw=Get["/home/dakini/Downloads/eigenpoints_"<>ToString[jj]<>".wl"];*)
(*(*Convert {Re,Im} format into complex numbers*)eigenvalues=Table[eigenvaluesRaw[[j,1]]+I*eigenvaluesRaw[[j,2]],{j,1,Length[eigenvaluesRaw]}];*)
(*(*Validate the total count of eigenvalues*)expectedCount=(jj+1) (jj/2)-1;*)
(*actualCount=Length[eigenvalues];*)
(*If[actualCount!=expectedCount,Print["\:26a0\:fe0f ERROR: Expected ",expectedCount," eigenvalues, but found ",actualCount," in n = ",jj];*)
(*Return[];];*)
(*(*Initialize Storage for Cases*)hyperbolic={};*)
(*misurewicz={};*)
(*parabolic={};*)
(*siegelDisks={};*)
(*others={};(*To capture unclassified points*)epsilon=0.01;(*Threshold for modulus approximation to 1*)(*Compute p/q values from ArcTan(Im/Re)/Pi*)pqValues=Table[If[Re[eigenvalues[[j]]]==0,0,ArcTan[Im[eigenvalues[[j]]]/Re[eigenvalues[[j]]]]/Pi],{j,1,Length[eigenvalues]}];*)
(*(*STEP 1:Separate eigenvalues with modulus~1 before classification*)modulusApprox1=Select[eigenvalues,1-epsilon<=Abs[#]<=1+epsilon&];*)
(*(*STEP 2:Classify Modulus~1 Eigenvalues into Parabolic or Siegel Disk*)Do[Module[{lambda,pq},lambda=modulusApprox1[[j]];*)
(*pq=pqValues[[j]];*)
(*(*Classification*)If[Abs[pq]<10^-6,(*Parabolic if p/q is near zero*)AppendTo[parabolic,lambda],AppendTo[siegelDisks,lambda] (*Otherwise,Siegel Disk*)]],{j,1,Length[modulusApprox1]}];*)
(*(*STEP 3:Classify the Rest of the Eigenvalues*)modulusApprox1Q[z_]:=(1-epsilon<=Abs[z]<=1+epsilon);*)
(*modulusApprox1=Select[eigenvalues,modulusApprox1Q];*)
(*remainingEigensols=Select[eigenvalues,Not@*modulusApprox1Q];*)
(*(*Get the ones that are NOT modulus~1*)Do[Module[{lambda,modulus},lambda=remainingEigensols[[j]];*)
(*modulus=Abs[lambda];(*Direct modulus computation*)(*Assign to Buckets*)If[modulus<1,AppendTo[hyperbolic,lambda],If[modulus>1,(*Misurewicz Points:Preperiodic behavior*)If[Length[DeleteDuplicates[NestList[#^2+lambda&,0,100]]]<100,AppendTo[misurewicz,lambda],AppendTo[others,lambda] (*Anything else goes here*)]]]],{j,1,Length[remainingEigensols]}];*)
(*(*Validate classification count*)classifiedCount=Length[hyperbolic]+Length[misurewicz]+Length[parabolic]+Length[siegelDisks]+Length[others];*)
(*If[classifiedCount==actualCount,Print["\:2705 Successfully classified ",classifiedCount," eigenvalues for n = ",jj];,Print["\:26a0\:fe0f WARNING: Classification count mismatch for n = ",jj," Expected: ",actualCount,", Classified: ",classifiedCount];];*)
(*(*Print classification summary in notebook*)Print["For n = ",jj," -> Hyperbolic: ",Length[hyperbolic],", Misurewicz: ",Length[misurewicz],", Parabolic: ",Length[parabolic],", Siegel: ",Length[siegelDisks],", Others: ",Length[others]];*)
(*(*Export Classification Summary*)summaryText=StringJoin["n = ",ToString[jj],"\n","Hyperbolic: ",ToString[Length[hyperbolic]],"\n","Misurewicz: ",ToString[Length[misurewicz]],"\n","Parabolic (\[Lambda] \[TildeTilde] 1): ",ToString[Length[parabolic]],"\n","Siegel Disks: ",ToString[Length[siegelDisks]],"\n","Others: ",ToString[Length[others]],"\n"];*)
(*Export["classification_summary_"<>ToString[jj]<>".txt",summaryText,"Text"];*)
(*(*Define a Function to Export CSVs*)ExportCSV[classification_,name_]:=Export["/home/dakini/Downloads/"<>name<>"_size_"<>ToString[jj]<>".csv",Prepend[Table[{Re[classification[[j]]],Im[classification[[j]]]},{j,1,Length[classification]}],{"RealPart","ImagPart"}],"CSV"];*)
(*(*Export Each Classification as a Separate CSV File*)ExportCSV[hyperbolic,"Hyperbolic"];*)
(*ExportCSV[misurewicz,"Misurewicz"];*)
(*ExportCSV[parabolic,"Parabolic"];*)
(*ExportCSV[siegelDisks,"Siegel"];*)
(*ExportCSV[others,"Others"];*)
(*Print["\:2705 Classification and export complete for n = ",jj,". Files saved in /home/dakini/Downloads/"];];*)
(**)
(*(*Run classification for each jj value*)*)
(*Do[ClassifyEigenvalues[jj],{jj,jjValues}];*)
(**)
(*Print["\:2705 All requested eigenvalue sets classified and exported!"];*)
(**)



