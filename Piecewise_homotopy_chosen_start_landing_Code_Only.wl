(*Data*)

savedLists$$ = <|
    \"Construct_Cardioid\" \[Rule] {
        {{1.04854208082306-1.5995872961875521}{-0.5440231030427529-0.9116384241849749}{-0.4369422682708666-0.9592299154567951}{-0.3457252608725927-0.9909575763046756}}
        {{-0.7542188157431229-0.021260305784039923}{-0.7502528588997197-0.07281775466184537}{-0.73835498836951-0.13230711875162093}{-0.734389031526107-0.18386456762942638}}
    }

    \"Construct-2nd-period-Bulb\" \[Rule] {
        {{-0.5479890598861563-0.8937916149580423}{-0.6312741535976236-0.8382682084742521}{-0.7224911609958973-0.7708469291725064}}
        {}
    }

    \"Mandelbrot-2ndPeriod-Bulb\" \[Rule] {
        {}
        {{-0.768610602717956-0.026493396611379616}{-0.7855397537596225-0.11101974107535173}{-0.8306841565374004-0.17300572701559758}}
    }

    \"Construct-4-period\" \[Rule] {
        {{-0.9096868613985114-0.5609170784742006}{-0.9379021131346226-0.49893109253395473}{-0.971760415217956-0.4651205547483659}}
        {}
    }

    \"Mandelbrot-4-period\" \[Rule] {
        {{-1.0564061704262893-0.0030432345692077423}{-1.078978371815178-0.008678324200139143}{-1.10155057320406710.002591855061723658}}
        {}
    }

    \"Construct-Tail\" \[Rule] {
        {{-1.0564061704262893-0.002104024302504559}{-1.078978371815178-0.002104024302504559}{-1.0902644725096224-0.002104024302504559}}
        {}
    }

    \"Mandelbrot-tail\" \[Rule] {
        {}
        {{-1.40063224160684460.01107387476021704}{-1.4344905436901780.01107387476021704}{-1.4570627450790670.01107387476021704}}
    }
|>;

(*RowBox[{"Piecewise homotopy code,"*)"}]

DynamicModule[
  {t = 0savedListsweights}

  (* Data: Construct points and corresponding Mandelbrot points *)
  savedLists = <|
    \"Cardioid\" \[Rule] {
      {{1.0485-1.5996}{-0.5440-0.9116}{-0.4369-0.9592}{-0.3457-0.9910}}
      {{-0.7542-0.0213}{-0.7503-0.0728}{-0.7384-0.1323}{-0.7344-0.1839}}
    }

    \"2nd-period-Bulb\" \[Rule] {
      {{-0.5480-0.8938}{-0.6313-0.8383}{-0.7225-0.7708}}
      {{-0.7686-0.0265}{-0.7855-0.1110}{-0.8307-0.1730}}
    }

    \"4-period\" \[Rule] {
      {{-0.9097-0.5609}{-0.9379-0.4989}{-0.9718-0.4651}}
      {{-1.0564-0.0030}{-1.0790-0.0087}{-1.10160.0026}}
    }

    \"Tail\" \[Rule] {
      {{-1.0564-0.0021}{-1.0790-0.0021}{-1.0903-0.0021}}
      {{-1.40060.0111}{-1.43450.0111}{-1.45710.0111}}
    }
  |>;

  (* Weights for different regions (piecewise deformation speed) *)
  weights = <|
    \"Cardioid\" \[Rule] Function[t0.5 + 0.5 Sin[Pi t]]
    \"2nd-period-Bulb\" \[Rule] Function[tt^2]
    \"4-period\" \[Rule] Function[t1 - Exp[-5 t]]
    \"Tail\" \[Rule] Function[tt]
  |>;

  (* Homotopy function: deforms construct points to Mandelbrot landing points *)
  homotopyFunction[t_p1_p2_w_] := (1 - w[t]) p1 + w[t] p2;

  (* Compute transformed points dynamically *)
  Dynamic[
    Column[{
      \"Homotopy Transition:\
      Slider[Dynamic[t]{01,

      (* Visualization of transformation *)
      Graphics[{
        (* Moving points (red) *)
        {Red, PointSize[Medium], 
         Point[
           Flatten[
             Table[
               MapThread[
                 homotopyFunction[t, #1, #2, weights[name]] &, 
                 {savedLists[name][[1]], savedLists[name][[2]]}
               ], 
               {name, Keys[savedLists]}
             ], 
             1
           ]
         ]
        },

        (* Original construct points (blue) for reference *)
        {Blue, PointSize[Small], 
         Point[
           Flatten[
             Table[savedLists[name][[1]], {name, Keys[savedLists]}], 1
           ]
         ]
        }
      }, ImageSize \[Rule] Large]
    }]
  ]
]
"}]