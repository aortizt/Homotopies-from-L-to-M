
(* Zoom into the Baby Mandelbrot Set *)
p1 = MandelbrotSetPlot[{-0.18 + 1.024 I, -0.14 + 1.042 I}, 
   ColorFunction -> "RedBlueTones"];

(* Douady's tuning function *)
TuningTransform[z_, lambda_, centerMain_, centerBaby_] := 
  centerBaby + lambda (z - centerMain);

(* 30° clockwise rotation *)
RotateEigenset[z_] := z Exp[-I Pi/6];

(* Cardioid homotopy with rotation and scaled deformation *)
CardioidHomotopy[z_, t_, center_, scale_] := Module[
  {theta, targetZ},
  If[t == 0, Return[z]];
  theta = Arg[z - center];
  targetZ = scale * (1/4 (2 Cos[theta] - Cos[2 theta]) + 
                     I/4 (2 Sin[theta] - Sin[2 theta]));
  targetZ = RotateEigenset[targetZ];
  (1 - t) z + t (targetZ + center)
];

(* Parameters *)
centerMain = -0.75;
centerBaby = -0.1575 + 1.0325 I;
lambda = 0.01;
cardioidScale = 0.0095;
jj = 20;

(* Compute eigenvalues and transform them *)
eigensols = {};
Do[
  ak = ConstantArray[0, {n, n}];
  Do[ak[[1, j]] = 1, {j, 1, n}];
  ak[[2, 1]] = 1;
  Do[ak[[j + 2, j + 1]] = 1, {j, 1, n - 2}];
  AppendTo[eigensols, Eigenvalues[N[ak]]],
  {n, 2, jj}
];

(* Process eigenvalues *)
eigensols = 1/N[Flatten[eigensols]];
eigenpoints = Table[
  Module[{z = RotateEigenset[eigensols[[j]]]},
    {Re[z], Im[z]}
  ],
  {j, 1, Length[eigensols]}
];

(* Apply Douady tuning to rotated points *)
tunedEigenpoints = Table[
  TuningTransform[eigenpoints[[j, 1]] + I eigenpoints[[j, 2]], 
    lambda, centerMain, centerBaby],
  {j, 1, Length[eigenpoints]}
];

(* Apply cardioid homotopy at 3 stages *)
homotopyStages = Table[
  Table[
    Module[{pt = CardioidHomotopy[
       tunedEigenpoints[[j]], t, centerBaby, cardioidScale]},
      {Re[pt], Im[pt]}
    ],
    {j, 1, Length[tunedEigenpoints]}
  ],
  {t, {0, 0.5, 1}}
];

(* Plot each stage *)
plots = Table[
  ListPlot[homotopyStages[[k]],
    PlotRange -> {{-0.18, -0.14}, {1.024, 1.042}},
    AspectRatio -> Automatic,
    PlotStyle -> {Red, PointSize[0.03]},
    PlotLabel -> "Cardioid Homotopy at t = " <> ToString[{0, 0.5, 1}[[k]]]
  ],
  {k, 1, 3}
];

(* Show the overlays *)
Print["jj = ", jj];
Print[Show[{p1, plots[[1]]}, ImageSize -> Medium]];
Print[Show[{p1, plots[[2]]}, ImageSize -> Medium]];
Print[Show[{p1, plots[[3]]}, ImageSize -> Medium]];
