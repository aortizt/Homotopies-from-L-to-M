(* Zoom into a Baby Mandelbrot Set (Period 3) *)
p1 = MandelbrotSetPlot[{-0.25 + 0.6 I, 0.05 + 0.9 I}, ColorFunction -> "RedBlueTones"];

(* Douady's tuning function: Maps a point from the main cardioid to a baby Mandelbrot set *)
TuningTransform[z_, lambda_, centerMain_, centerBaby_] := 
    centerBaby + lambda (z - centerMain);

(* Homotopy function: Expands TheConstruct towards the baby Mandelbrot cardioid *)
HomotopyTransform[z_, t_, center_, initialSize_, finalSize_] := Module[{r, phi, homotopyZ},
   r = Abs[z - center]; (* Distance from center *)
   phi = Arg[z - center]; (* Angle relative to center *)
   (* Expands TheConstruct smoothly towards final size *)
   homotopyZ = (1 - t) (initialSize Exp[I phi] + center) + t (finalSize Exp[I phi] + center);
   homotopyZ
];

(* Define parameters for Douady's tuning *)
centerMain = -0.75;  (* Center of the main cardioid in the Mandelbrot set *)
centerBaby = -0.122 + 0.744 I;  (* Center of the baby Mandelbrot cardioid *)
lambda = 0.1;  (* Keeps initial tuning similar to previous attempt *)

(* Loop to generate eigenvalues and apply Douady's tuning transformation *)
Do[
   eigensols = {}; (* Storage for eigenvalues *)

   (* Construct the Companion Matrix for Generalized Lucas Sequences *)
   Do[
      ak = ConstantArray[0, {n, n}];
      Do[ak[[1, j]] = 1, {j, 1, n}];
      ak[[2, 1]] = 1;
      Do[ak[[j + 2, j + 1]] = 1, {j, 1, n - 2}];

      (* Compute Eigenvalues Numerically *)
      AppendTo[eigensols, Eigenvalues[N[ak]]],
      {n, 2, jj}
   ];

   (* Invert eigenvalues *)
   eigensols = 1/N[Flatten[eigensols]];
   eigenpoints = {};

   (* Convert Eigenvalues into {Re, Im} format and apply tuning *)
   Do[
      AppendTo[eigenpoints, {Re[eigensols[[j]]], Im[eigensols[[j]]]}],
      {j, 1, Length[eigensols]}
   ];

   (* Apply Tuning to Eigenvalues to move them to a baby Mandelbrot set *)
   tunedEigenpoints = Table[
      {Re[TuningTransform[eigenpoints[[j, 1]] + I eigenpoints[[j, 2]], lambda, centerMain, centerBaby]], 
       Im[TuningTransform[eigenpoints[[j, 1]] + I eigenpoints[[j, 2]], lambda, centerMain, centerBaby]]},
      {j, 1, Length[eigenpoints]}
   ];

   (* Apply Homotopy to expand towards baby Mandelbrot cardioid *)
   homotopyStages = Table[
      Table[
         {Re[HomotopyTransform[tunedEigenpoints[[j, 1]] + I tunedEigenpoints[[j, 2]], t, centerBaby, 0.050, 0.093]], 
          Im[HomotopyTransform[tunedEigenpoints[[j, 1]] + I tunedEigenpoints[[j, 2]], t, centerBaby, 0.050, 0.093]]},
         {j, 1, Length[tunedEigenpoints]}
      ],
      {t, {0, 0.5, 1}} (* Capture homotopy at t = 0, 0.5, 1 *)
   ];

   (* Create plots for each stage *)
   plots = Table[
      ListPlot[homotopyStages[[k]], 
         PlotRange -> {{-0.25, 0.05}, {0.6, 0.9}}, (* New window settings *)
         AspectRatio -> Automatic, 
         PlotStyle -> {Red, PointSize[0.015]}, 
         PlotLabel -> "Homotopy at t = " <> ToString[{0, 0.5, 1}[[k]]]
      ],
      {k, 1, 3}
   ];

   (* Show all three plots *)
   Print["jj= ", jj];
   Print[Show[{p1, plots[[1]]}, ImageSize -> Medium]]; (* t = 0 *)
   Print[Show[{p1, plots[[2]]}, ImageSize -> Medium]]; (* t = 0.5 *)
   Print[Show[{p1, plots[[3]]}, ImageSize -> Medium]]; (* t = 1 *)

   , {jj, {20}}
];
