{binspersize = {}; (* Stores classified eigenvalues for each matrix size *),"

",RowBox[ {"bins = {
    {0.49, 0.51}, {0.50, 0.52}, {0.51, 0.53}, {0.53, 0.55}, {0.54, 0.56}, 
    {0.60, 0.62}, {0.61, 0.63}, {0.99, 1.01}, {1.00, 1.02}, {1.01, 1.03}, 
    {1.02, 1.04}, {1.03, 1.05}, {1.04, 1.06}, {1.05, 1.07}, {1.06, 1.08}, 
    {1.07, 1.09}, {1.08, 1.10}, {1.09, 1.11}, {1.10, 1.12}, {1.11, 1.13}, 
    {1.12, 1.14}, {1.13, 1.15}, {1.14, 1.16}, {1.15, 1.17}, {1.16, 1.18}, 
    {1.18, 1.20}, {1.19, 1.21}, {1.21, 1.23}, {1.22, 1.24}, {1.28, 1.30}, 
    {1.29, 1.31}, {1.34, 1.36}, {1.35, 1.37}, {1.60, 1.62}, {1.61, 1.63}
};"}],"

",RowBox[ {"Do[
  (* Compute eigenvalues for matrices up to size jj *)
  eigensols = {};
  Do[
    ak = ConstantArray[0, {n, n}];
    Do[ak[[1, j]] = 1, {j, 1, n}];
    ak[[2, 1]] = 1;
    Do[ak[[j + 2, j + 1]] = 1, {j, 1, n - 2}];
    (* Numerical eigenvalues *)
    AppendTo[eigensols, Eigenvalues[N[ak]]],
    {n, 2, jj}
  ];
  
  eigensols = 1/N[Flatten[eigensols]]; (* Keep this step as requested! *)
  Print[\"jj= \", jj];
  
  (* Store absolute values of eigenvalues for classification *)
  AppendTo[binspersize, Abs[eigensols]],
  {jj, {10, 20, 100, 300, 500, 1000}}
];"}],"

",RowBox[ {"(* Binning: Count eigenvalues in each bin for each matrix size *)
binnedCounts = Table[
  Table[
    Count[binspersize[[k]], _?(bins[[i, 1]] \[LessEqual] # < bins[[i, 2]] &)],
    {i, Length[bins]}
  ],
  {k, Length[binspersize]}
];"}],"

",RowBox[ {"(* Generate LaTeX table *)
matrixSizes = {10, 20, 100, 300, 500, 1000};"}],"
",RowBox[ {"tableHeader = \"\\\\begin{table}[htbp]\
\\\\centering\
\\\\caption{Distribution of eigenvalues of $\\\\mathcal{L}$ in different interval bins, for various matrix sizes.}\
\\\\label{tab:eigenvalue_distribution}\
\\\\begin{tabular}{|c|\" <> StringJoin[Table[\"c|\", {Length[matrixSizes]}]] <> \"}\
\\\\hline\
Interval & \" <> StringJoin[Riffle[ToString /@ matrixSizes, \" & \"]] <> \" \\\\\\\\ \\\\hline\
\";"}],"
",RowBox[ {"tableBody = StringJoin[
  Table[
    ToString[bins[[i, 1]]] <> \" -- \" <> ToString[bins[[i, 2]]] <> \" & \" <> StringJoin[Riffle[ToString /@ binnedCounts[[All, i]], \" & \"]] <> \" \\\\\\\\ \\\\hline\
\",
    {i, Length[bins]}
  ]
];"}],"
",RowBox[ {"tableFooter = \"\\\\end{tabular}\
\\\\end{table}\";"}],"

",latexTable = tableHeader <> tableBody <> tableFooter;,"
",RowBox[ {"Print[latexTable];
"}]}

InterpretationBox[\"jj= \\[InvisibleSpace]10,SequenceForm["jj= ", 10],Editable -> False],StandardForm],"Print",ExpressionUUID -> "d030db28-e8f3-45e9-9ed6-c981ce0924f5"],Cell[ BoxData[ InterpretationBox[\"jj= \\[InvisibleSpace]20,SequenceForm["jj= ", 20],Editable -> False],StandardForm],"Print",ExpressionUUID -> "03cc317a-d0d5-415b-a466-0e9bd285b9d8"],Cell[ BoxData[ InterpretationBox[\"jj= \\[InvisibleSpace]100,SequenceForm["jj= ", 100],Editable -> False],StandardForm],"Print",ExpressionUUID -> "ea5f985a-702c-4331-b68a-b208d6c22a5e"],Cell[ BoxData[ InterpretationBox[\"jj= \\[InvisibleSpace]300,SequenceForm["jj= ", 300],Editable -> False],StandardForm],"Print",ExpressionUUID -> "c8b9d0de-d939-4630-a939-81dc5b61d475"],Cell[ BoxData[ InterpretationBox[\"jj= \\[InvisibleSpace]500,SequenceForm["jj= ", 500],Editable -> False],StandardForm],"Print",ExpressionUUID -> "742184d2-f60e-40ee-b002-da62e3f73c74"],Cell[ BoxData[ InterpretationBox[\"jj= \\[InvisibleSpace]1000,SequenceForm["jj= ", 1000],Editable -> False],StandardForm],"Print",ExpressionUUID -> "bbf96b09-eace-43f1-b49c-7000d5bf170e"],Cell[ BoxData[ "\"\\\\begin{table}[htbp]\
\\\\centering\
\\\\caption{Distribution of eigenvalues of $\\\\mathcal{L}$ in different interval bins, for various matrix sizes.}\
\\\\label{tab:eigenvalue_distribution}\
\\\\begin{tabular}{|c|c|c|c|c|c|c|}\
\\\\hline\
Interval & 10 & 20 & 100 & 300 & 500 & 1000 \\\\\\\\ \\\\hline\
0.49 -- 0.51 & 6 & 16 & 96 & 296 & 496 & 996 \\\\\\\\ \\\\hline\
0.5 -- 0.52 & 7 & 17 & 97 & 297 & 497 & 997 \\\\\\\\ \\\\hline\
0.51 -- 0.53 & 1 & 1 & 1 & 1 & 1 & 1 \\\\\\\\ \\\\hline\
0.53 -- 0.55 & 1 & 1 & 1 & 1 & 1 & 1 \\\\\\\\ \\\\hline\
0.54 -- 0.56 & 1 & 1 & 1 & 1 & 1 & 1 \\\\\\\\ \\\\hline\
0.6 -- 0.62 & 1 & 1 & 1 & 1 & 1 & 1 \\\\\\\\ \\\\hline\
0.61 -- 0.63 & 1 & 1 & 1 & 1 & 1 & 1 \\\\\\\\ \\\\hline\
0.99 -- 1.01 & 0 & 10 & 2054 & 41759 & 121659 & 496409 \\\\\\\\ \\\\hline\
1. -- 1.02 & 0 & 22 & 4171 & 44071 & 123971 & 498721 \\\\\\\\ \\\\hline\
1.01 -- 1.03 & 0 & 24 & 2550 & 2745 & 2745 & 2745 \\\\\\\\ \\\\hline\
1.02 -- 1.04 & 2 & 26 & 580 & 580 & 580 & 580 \\\\\\\\ \\\\hline\
1.03 -- 1.05 & 4 & 36 & 219 & 219 & 219 & 219 \\\\\\\\ \\\\hline\
1.04 -- 1.06 & 4 & 57 & 114 & 114 & 114 & 114 \\\\\\\\ \\\\hline\
1.05 -- 1.07 & 2 & 55 & 62 & 62 & 62 & 62 \\\\\\\\ \\\\hline\
1.06 -- 1.08 & 4 & 35 & 35 & 35 & 35 & 35 \\\\\\\\ \\\\hline\
1.07 -- 1.09 & 6 & 25 & 25 & 25 & 25 & 25 \\\\\\\\ \\\\hline\
1.08 -- 1.1 & 4 & 17 & 17 & 17 & 17 & 17 \\\\\\\\ \\\\hline\
1.09 -- 1.11 & 8 & 15 & 15 & 15 & 15 & 15 \\\\\\\\ \\\\hline\
1.1 -- 1.12 & 9 & 11 & 11 & 11 & 11 & 11 \\\\\\\\ \\\\hline\
1.11 -- 1.13 & 5 & 5 & 5 & 5 & 5 & 5 \\\\\\\\ \\\\hline\
1.12 -- 1.14 & 6 & 6 & 6 & 6 & 6 & 6 \\\\\\\\ \\\\hline\
1.13 -- 1.15 & 7 & 7 & 7 & 7 & 7 & 7 \\\\\\\\ \\\\hline\
1.14 -- 1.16 & 5 & 5 & 5 & 5 & 5 & 5 \\\\\\\\ \\\\hline\
1.15 -- 1.17 & 4 & 4 & 4 & 4 & 4 & 4 \\\\\\\\ \\\\hline\
1.16 -- 1.18 & 2 & 2 & 2 & 2 & 2 & 2 \\\\\\\\ \\\\hline\
1.18 -- 1.2 & 1 & 1 & 1 & 1 & 1 & 1 \\\\\\\\ \\\\hline\
1.19 -- 1.21 & 1 & 1 & 1 & 1 & 1 & 1 \\\\\\\\ \\\\hline\
1.21 -- 1.23 & 4 & 4 & 4 & 4 & 4 & 4 \\\\\\\\ \\\\hline\
1.22 -- 1.24 & 4 & 4 & 4 & 4 & 4 & 4 \\\\\\\\ \\\\hline\
1.28 -- 1.3 & 1 & 1 & 1 & 1 & 1 & 1 \\\\\\\\ \\\\hline\
1.29 -- 1.31 & 1 & 1 & 1 & 1 & 1 & 1 \\\\\\\\ \\\\hline\
1.34 -- 1.36 & 2 & 2 & 2 & 2 & 2 & 2 \\\\\\\\ \\\\hline\
1.35 -- 1.37 & 2 & 2 & 2 & 2 & 2 & 2 \\\\\\\\ \\\\hline\
1.6 -- 1.62 & 1 & 1 & 1 & 1 & 1 & 1 \\\\\\\\ \\\\hline\
1.61 -- 1.63 & 1 & 1 & 1 & 1 & 1 & 1 \\\\\\\\ \\\\hline\
\\\\end{tabular}\
\\\\end{table}\"", StandardForm],"Print",ExpressionUUID -> "e31e669e-911a-4202-a1ad-8e638087c748"]}, Open],ExpressionUUID -> "dcf9e2d0-8db9-4628-8680-bd5dd05db3b2"],Cell[ BoxData[RowBox[ {"(*",RowBox[{"The above code \"worked\ except that it overcalculated eigenvalues,";"," ",RowBox[ {let',"s"," ","keep"," ","it"," ","for"," ","the"," ","moment"," ","and"," ","try"," ","with"," ","another"," ","code"}]}],"*)"}]

{(* Stores classified eigenvalues for each matrix size *)
binspersize = {};,"

",RowBox[ {"(* Non-overlapping binning intervals: each bin is [a, b) *)
bins = {
    {0.49, 0.51}, {0.51, 0.53}, {0.53, 0.55}, {0.55, 0.57}, {0.57, 0.59}, 
    {0.60, 0.62}, {0.62, 0.64}, {0.99, 1.01}, {1.01, 1.03}, {1.03, 1.05}, 
    {1.05, 1.07}, {1.07, 1.09}, {1.09, 1.11}, {1.11, 1.13}, {1.13, 1.15}, 
    {1.15, 1.17}, {1.17, 1.19}, {1.19, 1.21}, {1.21, 1.23}, {1.23, 1.25}, 
    {1.25, 1.27}, {1.27, 1.29}, {1.29, 1.31}, {1.31, 1.33}, {1.33, 1.35}, 
    {1.35, 1.37}, {1.37, 1.39}, {1.39, 1.41}, {1.41, 1.43}, {1.43, 1.45}, 
    {1.60, 1.62}, {1.62, 1.64}
};"}],"

",RowBox[ {"(* Matrix sizes to evaluate *)
matrixSizes = {10, 20, 100, 300, 500, 1000};"}],"

",RowBox[ {"Do[
  eigensols = {};  (* Reset eigenvalue storage for each jj *)

  (* Compute eigenvalues for matrices up to size jj *)
  Do[
    ak = ConstantArray[0, {n, n}];
    Do[ak[[1, j]] = 1, {j, 1, n}];
    ak[[2, 1]] = 1;
    Do[ak[[j + 2, j + 1]] = 1, {j, 1, n - 2}];

    (* Compute and store eigenvalues *)
    AppendTo[eigensols, Eigenvalues[N[ak]]],
    {n, 2, jj}
  ];

  eigensols = 1/N[Flatten[eigensols]]; (* Convert and normalize *)

  (* Diagnostic: Checking if eigenvalue count matches expectations *)
  expectedCount = Sum[n, {n, 2, jj}];
  actualCount = Length[eigensols];
  Print[\"jj= \", jj, \" | Expected eigenvalues: \", expectedCount, \" | Computed eigenvalues: \", actualCount];

  (* Store absolute values of eigenvalues for classification *)
  AppendTo[binspersize, Abs[eigensols]],
  {jj, matrixSizes}
];"}],"

",RowBox[ {"(* Binning: Count eigenvalues in each bin for each matrix size *)
binnedCounts = Table[
  Table[
    Count[binspersize[[k]], _?(bins[[i, 1]] \[LessEqual] # < bins[[i, 2]] &)], (* Properly handles non-overlapping bins *)
    {i, Length[bins]}
  ],
  {k, Length[binspersize]}
];"}],"

",RowBox[ {"(* Generate LaTeX table *)
tableHeader = \"\\\\begin{table}[htbp]\
\\\\centering\
\\\\caption{Distribution of eigenvalues of $\\\\mathcal{L}$ in different interval bins, for various matrix sizes.}\
\\\\label{tab:eigenvalue_distribution}\
\\\\begin{tabular}{|c|\" <> StringJoin[Table[\"c|\", {Length[matrixSizes]}]] <> \"}\
\\\\hline\
Interval & \" <> StringJoin[Riffle[ToString /@ matrixSizes, \" & \"]] <> \" \\\\\\\\ \\\\hline\
\";"}],"

",RowBox[ {"tableBody = StringJoin[
  Table[
    ToString[bins[[i, 1]]] <> \" -- \" <> ToString[bins[[i, 2]]] <> \" & \" <> StringJoin[Riffle[ToString /@ binnedCounts[[All, i]], \" & \"]] <> \" \\\\\\\\ \\\\hline\
\",
    {i, Length[bins]}
  ]
];"}],"

",RowBox[ {"tableFooter = \"\\\\end{tabular}\
\\\\end{table}\";"}],"

",latexTable = tableHeader <> tableBody <> tableFooter;,"
",RowBox[ {"Print[latexTable];
"}]}