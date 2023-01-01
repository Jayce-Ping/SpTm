(* ::Package:: *)

BeginPackage["SpTm`"]


STensor::usage = "STensor[TensorName_Symbol, subindex_List, superindex_List] \:4e3aSTensor\:5bf9\:8c61\:7684\:6807\:51c6\:683c\:5f0f\:ff0c\:662f\:5f20\:91cf\:8ba1\:7b97\:65f6\:6240\:7528\:7684\:8868\:8fbe\:5f62\:5f0f. "<>"\:4e5f\:53ef\:4ee5\:76f4\:63a5\:91c7\:53d6ctrl+_\:8f93\:5165\:4e0b\:6807\:548cctrl+^\:8f93\:5165\:4e0a\:6807\:7684\:5f62\:5f0f\:8f93\:5165\:5f20\:91cf\:ff08\!\(\*SuperscriptBox[SubscriptBox[\(\:5982R\), \(a\\\ b\\\ c\)], \(d\)]\),\:6ce8\:610f\:6307\:6807\:4e4b\:95f4\:9700\:8981\:7528\:7a7a\:683c\:9694\:5f00\:ff09."

ShowForm::usage = "ShowForm[expr]"<>" "<>"\:5c06STensor\:8f6c\:5316\:4e3a\:6807\:51c6\:683c\:5f0f\:ff0c\:4e0e\:8868\:8fbe\:5f0f\:5176\:4f59\:90e8\:5206\:5747\:4ee5StandardForm\:683c\:5f0f\:8f93\:51fa."

InputExplain::usage = "InputExplain[expr]"<>" "<>"\:5c06\:62bd\:8c61\:6307\:6807\:5f20\:91cf\:8868\:8fbe\:5f0fexpr\:8f6c\:5316\:4e3a\:5185\:90e8\:8ba1\:7b97\:65f6\:4f7f\:7528\:7684\:5f62\:5f0f."

STCalculate::usage = "SpTmCalculate[expr]"<>" "<>"\:5c06\:62bd\:8c61\:6307\:6807\:5f20\:91cf\:8868\:8fbe\:5f0fexpr\:8fdb\:884c\:8ba1\:7b97\:548c\:5316\:7b80."

SetCoodinates::usage = "SetCoodinates[coodinates_List]"<>" "<>"coodinates\:662f\:4e00\:4e2a\:7b26\:53f7\:5217\:8868\:ff0c\:5305\:542b\:5750\:6807\:7cfb\:6240\:7528\:7684\:7b26\:53f7."

SetMetric::usage = "SetMetric[components_Array]"<>" "<>"\:5728\:8bbe\:7f6e\:5750\:6807\:7cfb\:540e\:ff0c\:8bbe\:7f6e\:5ea6\:89c4g\:5728\:8be5\:7cfb\:4e0b\:7684\:5206\:91cf\:77e9\:9635components"<>"\n"<>"SetMetric[components_Array, coodinates_List]"<>" "<>"\:8bbe\:7f6e\:5ea6\:89c4g\:5728\:5750\:6807\:7cfbcoodinates\:4e0b\:7684\:5206\:91cf\:77e9\:9635components.\n"<>"SetMetric[components_Array, coodinates_List, metricSymbol_Symbol] \:8bbe\:7f6e\:5ea6\:89c4g\:5728\:5750\:6807\:7cfbcoodinates\:4e0b\:7684\:5206\:91cf\:77e9\:9635components\:ff0c\:548c\:5ea6\:89c4\:7684\:8868\:793a\:7b26\:53f7metricSymbol. "

SetMetricSymbol::usage = "SetMetricSymbol[metricSymbol_Symbol]"<>" "<>"\:8bbe\:7f6e\:5ea6\:89c4\:7b26\:53f7."

GetMetric::usage = "GetMetric[]"<>" "<>"\:83b7\:53d6\:5ea6\:89c4\:4fe1\:606f."

SetTensor::usage = "SetTensor[T_STensor,components_List]"<>" "<>"\:8bbe\:7f6e\:5f20\:91cfT\:5728\:5f53\:524d\:5750\:6807\:7cfb\:4e0b\:7684\:5206\:91cf."

ATensorAdd::usage = "ATensorAdd[T_ATensor, S_ATensor]"<>" "<>"\:8fd4\:56de\:4e24\:4e2aATensor\:5bf9\:8c61\:7684\:5f20\:91cf\:548c"

ATensorTimes::usage = "ATensorTimes[k_Number|Symbol, T_ATensor]"<>" "<>"\:8fd4\:56dek\:6570\:4e58\:5f20\:91cfT\:7684\:7ed3\:679c"<>"\n"<>"ATensorTimes[T_ATensor, S_ATensor]"<>" "<>"\:8fd4\:56deT\:548cS\:5f20\:91cf\:79ef\:6216\:7f29\:5e76\:7684\:7ed3\:679c."


SCalcChristoffel::usage = "SCalcChristoffel[metricComponentsMatrix,coodinateSystem]"<>"\n"<>"\:7ed9\:5b9a\:5750\:6807\:7cfb\:ff0c\:5e76\:7ed9\:51fa\:5ea6\:89c4\:5728\:8be5\:5750\:6807\:7cfb\:4e0b\:7684\:5206\:91cf\:77e9\:9635\:ff0c\!\(\*SuperscriptBox[SubscriptBox[\(\:8ba1\:7b97\:514b\:6c0f\:7b26\:7684\:5206\:91cf\[CapitalGamma]\), \(\[Mu]\[Nu]\)], \(\[Sigma]\)]\) -(\:6ce8\:610f\:4e0a\:4e0b\:6307\:6807\:987a\:5e8f)."

SCalcRiemannTensor::usage = "SCalcRiemannTensor[metricComponentsMatrix,coodinateSystem]"<>"\t"<>"\:7ed9\:5b9a\:5750\:6807\:7cfb\:ff0c\:5e76\:7ed9\:51fa\:5ea6\:89c4\:5728\:8be5\:5750\:6807\:7cfb\:4e0b\:7684\:5206\:91cf\:77e9\:9635\:ff0c\!\(\*SuperscriptBox[SubscriptBox[\(\:8ba1\:7b97Riemann\:66f2\:7387\:5f20\:91cf\:7684\:5206\:91cfR\), \(\[Mu]\[Nu]\[Sigma]\)], \(\[Rho]\)]\)-(\:6ce8\:610f\:4e0a\:4e0b\:6307\:6807\:987a\:5e8f)."

SCalcRicciTensor::usage = "SCalcRicciTensor[metricComponentsMatrix,coodinateSystem]"<>"\t"<>"\:7ed9\:5b9a\:5750\:6807\:7cfb\:ff0c\:5e76\:7ed9\:51fa\:5ea6\:89c4\:5728\:8be5\:5750\:6807\:7cfb\:4e0b\:7684\:5206\:91cf\:77e9\:9635\:ff0c\!\(\*SubscriptBox[\(\:8ba1\:7b97Ricci\:5f20\:91cf\:7684\:5206\:91cfR\), \(\[Mu]\[Nu]\)]\)."


Begin["Private`"]


SpTmHelp={
	"\:5728\:672c\:7a0b\:5e8f\:5305\:4e2d\:ff0c\:7b26\:53f7g\:5c06\:88ab\:8ba4\:4e3a\:662f\:9ed8\:8ba4\:7684\:5ea6\:89c4\:5f20\:91cf\:8868\:793a\:7b26\:53f7\:ff0c\:53ef\:4ee5\:901a\:8fc7SetMetric\:6765\:4fee\:6539. \[Del]\:4e3a\:4e0e\:5176\:76f8\:9002\:914d\:7684\:534f\:53d8\:5bfc\:6570\:7b97\:7b26. \:56e0\:6b64\:ff0c\:8be5\:7a0b\:5e8f\:5305\:4e2d\:540c\:65f6\:53ea\:80fd\:5b58\:5728\:4e00\:4e2a\:5ea6\:89c4.\n",
	"\[FilledSmallCircle] \:4f7f\:7528SpTmCalculate[expr]\:6765\:8ba1\:7b97\:62bd\:8c61\:6307\:6807\:7684\:5f20\:91cf\:8868\:8fbe\:5f0fexpr.\n",
	"\[FilledSmallCircle] \:4f7f\:7528ShowForm[expr]\:5c06\:8868\:8fbe\:5f0f\:663e\:793a\:4e3a\:6807\:51c6\:683c\:5f0f.\n",
	"\[FilledSmallCircle] \:4f7f\:7528SpTmCalculate[expr]\:5c06\:8868\:8fbe\:5f0f\:8fdb\:884c\:9002\:5f53\:8ba1\:7b97\:548c\:5316\:7b80.\n",
	"\[FilledSmallCircle] \:4f7f\:7528SpTmCalcComponent[expr]\:8ba1\:7b97\:8868\:8fbe\:5f0f\:7684\:5206\:91cf.\n"
};
SpTmHelp::usage = StringJoin[SpTmHelp]
Print[StringJoin[SpTmHelp]]

(*STensor\:5199\:6cd5\:7684\:6807\:51c6\:683c\:5f0f*)
STensor[TensorName_Symbol,subindex_List,superindex_List];(*STensor\:6807\:51c6\:683c\:5f0f*)

Protect[STensor];


(* ::Section:: *)
(*\:8bbe\:7f6e\:5f20\:91cf\:5206\:91cf*)


Unprotect[TensorComponents];
TensorComponents = Association[];
Protect[TensorComponents];
SetTensor::ErrorExpression = "\:5f20\:91cf\:683c\:5f0f\:8f93\:5165\:9519\:8bef."
SetTensor[expr__, components_List] := Module[{},
	T = InputExplain[expr];
	If[Head[T] =!= STensor,
		Message[SetTensor::ErrorExpression];
		Abort[]
	];
	SetTensor[T, components]
]

SetTensor[T_STensor, components_List] := Module[{},
	(*\:5f02\:5e38\:5904\:7406 undone*)
	
	Unprotect[TensorComponents];
	AppendTo[TensorComponents, T->components];
	Protect[TensorComponents];
]


(* ::Section:: *)
(*\:8bbe\:7f6e\:5750\:6807\:7cfb*)


Unprotect[SCoodinates]
SCoodinates = List[];
Protect[SCoodinates]
SetCoodinates::ErrorSymbol = "\:8f93\:5165\:5750\:6807\:7cfb\:4e2d\:5b58\:5728\:975e\:7b26\:53f7\:ff08Symbol\:ff09\:5143\:7d20."
SetCoodinates[Coodinates_List] := Module[{},
	If[
		!AllTrue[Coodinates,MatchQ[#,_Symbol]&],
		Message[SetCoodinates::ErrorSymbol];
		Abort[]
	]
	Unprotect[SCoodinates];
	SCoodinates = Coodinates;
	Protect[SCoodinates];
]


(* ::Section:: *)
(*\:8bbe\:7f6e\:5ea6\:89c4*)


Protect[MetricComponents]

Unprotect[MetricSymbol]
MetricSymbol = Global`g;
Protect[MetricSymbol]

SetMetric::NoCoodinates = "\:6ca1\:6709\:8bbe\:7f6e\:5750\:6807\:7cfb.";
SetMetric::ErrorDimensions = "\:5206\:91cf\:77e9\:9635\:548c\:5750\:6807\:7cfb\:7ef4\:6570\:4e0d\:5339\:914d.";


SetMetricSymbol[metricSymbol_Symbol] := Module[{},
	Unprotect[MetricSymbol];
	MetricSymbol = metricSymbol;
	Protect[MetricSymbol];
]


SetMetric[Components_?ArrayQ]:=Module[ {}, SetMetric[Components, SCoodinates] ]

SetMetric[Components_?ArrayQ, Coodinates_List]:=Module[ {}, SetMetric[Components, Coodinates, MetricSymbol] ]

SetMetric[Components_?ArrayQ, Coodinates_List, metricSymbol_Symbol]:=Module[{},
	If[
		Coodinates=={} && Head[SCoodinates] =!= List,
		Message[SetMetric::NoCoodinates];
		Abort[]
	];
	If[
		First@Dimensions[Components]!=Length[Coodinates],
		Message[SetMetric::ErrorDimensions];
		Abort[]
	];
	If[Coodinates =!= SCoodinates, SetCoodinates[Coodinates]];
	If[metricSymbol =!= MetricSymbol, SetMetricSymbol[metricSymbol]];
	
	Unprotect[MetricComponents];
	MetricComponents = Components;
	Protect[MetricComponents];
	(*\:8bbe\:7f6e\:5ea6\:89c4\:5f20\:91cf\:7684\:5206\:91cf*)(*\:4e4b\:524d\:7684\:4f1a\:88ab\:8986\:76d6*)
	SetTensor[STensor[metricSymbol, {a, b}, {}], Components];
	SetTensor[STensor[metricSymbol, {}, {a, b}], Inverse[Components]];
	(*Subscript[\:8bbe\:7f6e\[Delta], a]^b*)
	SetTensor[STensor[\[Delta], {a}, {b}], IdentityMatrix[Length[Coodinates]]];
]


GetMetric[]:=Module[{},
	Row[{ Subscript[MetricSymbol, Row[{"\[Mu]","\[Nu]"}]], "=" , MatrixForm[MetricComponents] }]
]


(* ::Section:: *)
(*\:8f93\:5165\:89e3\:91ca*)


(*\:8f93\:5165\:89e3\:91ca\:ff0c\:4e0a\:4e0b\:6807\:7528\:7a7a\:683c\:9694\:5f00\:ff0c\:9ed8\:8ba4\:89e3\:91ca\:4e3a\:4e3aTimes*)

InputExplain[expr__]:=expr//.InputExplainRule;

(*\:751f\:6210\:6307\:6807\:66ff\:6362\:5217\:8868*)
generateInputExplainRule[x__]:={(*\:591a\:4e2a\:6307\:6807\:66ff\:6362\:4e3a\:5217\:8868*)Times->List,(*\:5355\:4e2a\:6307\:6807\:8f6c\:4e3a\:5217\:8868*)x:>{x}/;MatchQ[x,_Symbol]};

InputExplainRule:={
	Subscript[T_Symbol,subIndex__]:>STensor[T,subIndex/.generateInputExplainRule[subIndex],{}],
	Power[T_Symbol,superIndex__]:>STensor[T,{},superIndex/.generateInputExplainRule[superIndex]],
	Power[Subscript[T_Symbol,subIndex__],superIndex__]:>
	STensor[T,subIndex/.generateInputExplainRule[subIndex],superIndex/.generateInputExplainRule[superIndex]]
};


(* ::Section:: *)
(*\:683c\:5f0f\:5316\:8f93\:51fa*)


Superscript[x_,y_,superIndex__]:=Superscript[x,Row[{y,superIndex}]];(*Good !!!*)

Subscript[x_,y_,subIndex__]:=Subscript[x,Row[{y,subIndex},""]];


Unprotect[ShowForm, ShowSTensor];

SetAttributes[ShowSTensor, Listable];

ShowForm[expr___] := StandardForm[expr /. {T_STensor -> ShowSTensor[T]}];

ShowSTensor[tensor_STensor] :=
    Module[{out = tensor[[1]], r = {{} -> "", List -> Sequence}},
        If[Length[tensor[[2]]] > 0, out = Subscript[out, tensor[[2]] /. r]];
        If[Length[tensor[[3]]] > 0, out = Superscript[out, tensor[[3]] /. r]];
        out
    ];

Protect[ShowForm, ShowSTensor];


(* ::Section::Closed:: *)
(*\:62bd\:8c61\:6307\:6807\:8fd0\:7b97\:89c4\:5219*)


(* ::Subsection:: *)
(*\:5ea6\:89c4\:8fd0\:7b97\:5f8b*)


(*\:5ea6\:89c4\:8fd0\:7b97\:5f8b*)
MetricCalcRule:={
	(*Subscript[\:5ea6\:89c4g, ab]\:4e0e\:5176\:9006\:6620\:5c04g^ab*)
	STensor[g_,{a_,b_},{}] STensor[g_,{},{a_,c_}]:>STensor[\[Delta],{a},{c}]/;g==MetricSymbol,
	
	(*\:5ea6\:89c4\:964d\:6307\:6807*)
	STensor[T_,Tsub__,Tsup__]STensor[g_,{a_,b_} ,Null|{}]:>STensor[T,Append[Tsub,b],DeleteElements[Tsup,{a}]]/;g==MetricSymbol && MemberQ[Tsup,a],
	
	(*\:5ea6\:89c4\:5347\:6307\:6807*)
	STensor[T_,Tsub__,Tsup__] STensor[g_,Null|{},{a_,b_}]:>STensor[T,DeleteElements[Tsup,{a}],Append[Tsub,b]]/;g==MetricSymbol && MemberQ[Tsub,a]
};


(* ::Subsection:: *)
(*\:5f20\:91cf\:8fd0\:7b97\:5f8b*)


(*\:5f20\:91cf\:8fd0\:7b97\:5f8b*)
STensorCalcRule:={
	(*\:81ea\:5e26\:52a0\:6cd5\:548c\:4e58\:6cd5\:7684\:4ea4\:6362\:5f8b\:3001\:7ed3\:5408\:5f8b*)

	(*\:5206\:914d\:5f8b*)
	T_STensor(P_STensor+Q_STensor):>T P+T Q,
	T_STensor ((\[Alpha]_?NumberQ|_Symbol) P_STensor+ Q_STensor):>\[Alpha] T P+T Q,
	T_STensor ((\[Alpha]_?NumberQ|_Symbol) P_STensor+(\[Beta]_?NumberQ|_Symbol) Q_STensor):>\[Alpha] T P+\[Beta] T Q
};


(* ::Subsection:: *)
(*\:5bfc\:6570\:7b97\:7b26\:8fd0\:7b97\:5f8b*)


(*\:5bfc\:6570\:7b97\:7b26\:8fd0\:7b97\:5f8b*)
DerivativeCalcRule:={

(*\:4e0e\:5ea6\:89c4\:7684\:9002\:914d\:6027*)(*\:5e94\:6709\:5bfc\:6570\:7b97\:7b26\[Del]\:4e0e\:5ea6\:89c4g\:7684\:5bf9\:5e94\:5173\:7cfb*)
	\!\(
\*SubscriptBox[\(\[Del]\), \(c_\)]\(STensor[g_, {a_, b_}, {}]\)\):>0/;g==MetricSymbol,
	
(*\:53ef\:52a0\:6027*)(*\[Del](a+b)==\[Del]a+\[Del]b*)
	\!\(
\*SubscriptBox[\(\[Del]\), \(a_\)]\(Plus[x_, y___]\)\):>Plus[\!\(
\*SubscriptBox[\(\[Del]\), \(a\)]x\),\!\(
\*SubscriptBox[\(\[Del]\), \(a\)]\ \(Plus[y]\)\)],
	
(*\:7ebf\:6027\:6027*)(*\[Del](\[Alpha]...)==\[Alpha]\[Del](...)*)
	\!\(
\*SubscriptBox[\(\[Del]\), \(a_\)]\(Times[\((\[Alpha]_?NumberQ | _Symbol)\), \ x__]\)\):>\[Alpha] \!\(
\*SubscriptBox[\(\[Del]\), \(a\)]\(Times[x]\)\),
	
(*Liebniz Rule*)
	\!\(
\*SubscriptBox[\(\[Del]\), \(a_\)]\((Times[T_STensor, S__STensor])\)\):>Plus[Times[T,\!\(
\*SubscriptBox[\(\[Del]\), \(a\)]\((S)\)\)],Times[S,\!\(
\*SubscriptBox[\(\[Del]\), \(a\)]T\)]]
};


STCalculate[expr__]:= Module[
{
	CalcRule = Join[DerivativeCalcRule, STensorCalcRule]
},
	InputExplain[expr]//.CalcRule
];


(* ::Section:: *)
(*\:5177\:4f53\:6307\:6807\:8f6c\:5316\:4e0e\:8fd0\:7b97*)


(*\:83b7\:53d6\:4e00\:4e2a\:6ca1\:6709\:7528\:8fc7\:7684\:6307\:6807\:7b26\:53f7*)
GetANotUsedIndex[expr__] := Module[{
	usedIndeces,
	unUsedIndeces,
	alphabet = ToExpression@Alphabet[]
},
	usedIndeces = DeleteDuplicates@Flatten@Cases[expr, STensor[T_,subIndex_List,superIndex_List]:>Join[superIndex,subIndex], All];
	unUsedIndeces = Complement[alphabet, usedIndeces];
	First@unUsedIndeces
]


(* ::Subsection:: *)
(*\:57fa\:672c\:8fd0\:7b97*)


(*\:8bbe\:7f6e\:52a0\:6cd5\:548c\:4e58\:6cd5\:7684\:4ea4\:6362\:5f8b\:548c\:7ed3\:5408\:5f8b*)
SetAttributes[{ATensorAdd, ATensorTimes}, {Flat, Orderless}]

(*\:5c06\:8868\:8fbe\:5f0f\:8f6c\:5316\:4e3a\:5177\:4f53\:6307\:6807\:8fdb\:884c\:8ba1\:7b97*)
(*\:5c06\:6240\:6709STensor\:8f6c\:5316\:4e3aATensor (Anonymous Tensor) \:533f\:540d\:5f20\:91cf\:8fdb\:884c\:8ba1\:7b97*)
(*\:7531\:4e8e\:5177\:4f53\:6307\:6807\:8ba1\:7b97\:65f6\:ff0c\:5f20\:91cf\:7684\:540d\:79f0\:4e0d\:518d\:91cd\:8981\:ff0c\:800c\:5176\:6307\:6807\:548c\:5206\:91cf\:624d\:662f\:91cd\:8981\:7684\:ff0c\:56e0\:6b64\:8f6c\:5316\:4e3aATensor[components_List, subIndex_List, superIndex_List]\:7684\:5f62\:5f0f*)
(*\:8fd9\:6837\:80fd\:591f\:8ba9\:52a0\:6cd5\:3001\:6570\:4e58\:548c\:5f20\:91cf\:79ef\:3001\:7f29\:5e76\:7b49\:8fd0\:7b97\:90fd\:6709\:76f8\:540c\:7684\:8f93\:5165\:548c\:8f93\:51fa\:ff1af[ATesnor]->ATensor*)
(*\:9700\:8981\:5c06\:534f\:53d8\:5bfc\:6570\:7b97\:7b26\:8f6c\:5316\:4e3a\:5f53\:524d\:5750\:6807\:7cfb\:4e0b\:7684 \:666e\:901a\:5bfc\:6570\:7b97\:7b26\:4f5c\:7528\[OpenCurlyDoubleQuote]\:52a0\[CloseCurlyDoubleQuote]\:514b\:6c0f\:7b26\:7684\:4f5c\:7528*)


(* ::Subsubsection:: *)
(*\:52a0\:6cd5*)


ATensorAdd::Error = "\:5f20\:91cf\:4e0d\:540c\:578b\:6216\:6307\:6807\:4e0d\:4e00\:81f4.";


ATensorAdd[T_ATensor, S_ATensor] := Module[{},
	(*\:4e24\:4e2a\:5f20\:91cf\:76f8\:52a0\:8981\:6c42\:662f\:540c\:578b\:5f20\:91cf\:4e14\:6307\:6807\:76f8\:540c*)
	If[
		T[[1]] =!= S[[1]] || T[[2]] =!= S[[2]] || Dimensions[T[[3]]] != Dimensions[S[[3]]],
		Message[ATensorAdd::Error];
		Abort[]
	];
	ATensor[T[[1]], T[[2]], T[[3]]+S[[3]]]
];

ATensorAdd[T_ATensor, S__ATensor] := ATensorAdd[T, ATensorAdd[S]];

ATensorAdd[T_ATensor] := T;


(* ::Subsubsection:: *)
(*\:6570\:4e58*)


(*\:6570\:4e58*)
ATensorTimes[k(_Symbol|_?NumberQ), T_ATensor] := Module[{},
	ATensor[T[[1]], T[[2]], k*T[[3]]]
]


(* ::Subsubsection:: *)
(*\:5f20\:91cf\:79ef\:4e0e\:7f29\:5e76*)


(*\:5f20\:91cf\:79ef\:4e0e\:7f29\:5e76*)
ATensorTimes[T_ATensor, S_ATensor] := Module[{},
	Print["TensorProduct and Construct"]
];


ATensorTimes[T_ATensor, S__ATensor] := ATensorTimes[T, ATensorTimes[S]];


ATensorTimes[T_ATensor] := T;


(* ::Subsection:: *)
(*\:8868\:8fbe\:5f0f\:8f6c\:5316*)


SCalcSpecific[expr__] := Module[
{
	expression = expr/.{T_STensor :> ATensor[T[[2]], T[[3]], TensorComponents[T]]}
},
	
	Print["SCalcSpecific"]
]


(* ::Section::Closed:: *)
(*\:7279\:6b8a\:5f20\:91cf\:8ba1\:7b97*)


SCalcChristoffel[g_?ArrayQ, coodinateSystem_List]:=Module[
	{
		invg=Inverse[g],(*inverse of metric g*)
		dimension=First@Dimensions[coodinateSystem],(*dimension of space*)
		\[Gamma],(*Christoffel Symbol Component Calculating Function*)
		\[CapitalGamma](*Christoffel Symbol Component Matrix*)
	},
	(*Subscript[\:7528x, i]\:6765\:8bb0\:5f55\:5750\:6807\:7cfb,\:548c\:6570\:5b66\:516c\:5f0f\:66f4\:76f8\:7b26*)
	Evaluate[Array[Subscript[x, #]&,dimension]]=coodinateSystem;
	\[Gamma][\[Mu]_,\[Nu]_,\[Sigma]_]:=1/2 Sum[invg[[\[Sigma],\[Rho]]] (D[g[[\[Rho],\[Mu]]],Subscript[x, \[Nu]]]+D[g[[\[Rho],\[Nu]]],Subscript[x, \[Mu]]]-D[g[[\[Mu],\[Nu]]],Subscript[x, \[Rho]]]),{\[Rho],dimension}];(*Subscript[\:514b\:6c0f\:7b26\:5206\:91cf\[CapitalGamma], \[Mu]\[Nu]]^\[Sigma]*)
	(*\:5229\:7528\:4e0b\:6307\:6807\:7684\:5bf9\:79f0\:6027\:6765\:964d\:4f4e\:8ba1\:7b97\:590d\:6742\:5ea6*)
	\[CapitalGamma]=Simplify@Array[If[#1<=#2,\[Gamma][#1,#2,#3],Null]&,{dimension,dimension,dimension}];(*\:514b\:6c0f\:7b26\:5206\:91cf\:77e9\:9635*)
	Array[If[#1>#2,\[CapitalGamma][[#1,#2,#3]]=\[CapitalGamma][[#2,#1,#3]]]&,{dimension,dimension,dimension}];
	(*Print[Subsuperscript["\[CapitalGamma]","\[Mu]\[Nu]","  \[Sigma]"]->MatrixForm[\[CapitalGamma]]]*);
	\[CapitalGamma]
]


SCalcRiemannTensor[g_?ArrayQ,coodinateSystem_List]:=Module[
	{
		invg=Inverse[g],(*inverse of metric g*)
		dimension=First@Dimensions[coodinateSystem],(*dimension of space*)
		\[CapitalGamma],(*Christoffel Symbol Component Matrix*)
		r,(*Riemann Tensor Conponent Calculating Function*)
		Riemann(*Riemann Tensor Component Matrix*)
	},
	(*\:7528\:5750\:6807\:6cd5\:8ba1\:7b97Riemann\:66f2\:7387\:5f20\:91cf*)
	(*\:5148\:8ba1\:7b97\:514b\:6c0f\:7b26*)
	\[CapitalGamma]=SCalcChristoffel[g,coodinateSystem];
	(*\:8bb0\:5f55\:5750\:6807\:7cfb*)
	Evaluate[Array[Subscript[x, #]&,dimension]]=coodinateSystem;
	r[\[Mu]_,\[Nu]_,\[Sigma]_,\[Rho]_]:=D[\[CapitalGamma][[\[Mu],\[Sigma],\[Rho]]],Subscript[x, \[Nu]]]-D[\[CapitalGamma][[\[Sigma],\[Nu],\[Rho]]],Subscript[x, \[Mu]]]+Sum[\[CapitalGamma][[\[Sigma],\[Mu],\[Lambda]]] \[CapitalGamma][[\[Nu],\[Lambda],\[Rho]]]-\[CapitalGamma][[\[Sigma],\[Nu],\[Lambda]]] \[CapitalGamma][[\[Mu],\[Lambda],\[Rho]]],{\[Lambda],dimension}];(*Subscript[\:9ece\:66fc\:5f20\:91cf\:7684\:5206\:91cfR, \[Mu]\[Nu]\[Sigma]]^\[Rho]*)
	(*\:5229\:7528\:7b2c1\:30012\:4e0b\:6307\:6807\:7684\:53cd\:79f0\:6027\:6765\:964d\:4f4e\:8ba1\:7b97\:590d\:6742\:5ea6*)
	Riemann=Simplify@Array[If[#1<=#2,r[#1,#2,#3,#4],Null]&,{dimension,dimension,dimension,dimension}];
	Array[If[#1>#2,Riemann[[#1,#2,#3,#4]]=-Riemann[[#2,#1,#3,#4]]]&,{dimension,dimension,dimension,dimension}];
	(*Print[Subsuperscript["R","\[Mu]\[Nu]\[Sigma]","   \[Rho]"]->MatrixForm@Riemann]*);
	Riemann
]



SCalcRicciTensor[g_?ArrayQ,coodinateSystem_List]:=Module[
	{
		invg=Inverse[g],(*inverse of metric g*)
		dimension=First@Dimensions[coodinateSystem],(*dimension of space*)
		R,(*Riemann Tensor Component Matrix*)
		ricci,(*Ricci Tensor Component Calculating Function*)
		Ricci(*Ricci Tensor Component Matrix*)
	},
	(*\:8ba1\:7b97Ricci\:5f20\:91cf*)
	(*\:5148\:8ba1\:7b97Riemann\:66f2\:7387\:5f20\:91cf*)
	R=SCalcRiemannTensor[g,coodinateSystem];
	(*Subscript[\:7528x, #]\:6765\:8bb0\:5f55\:5750\:6807\:7cfb*)
	Evaluate[Array[Subscript[x, #]&,dimension]]=coodinateSystem;
	ricci[\[Mu]_,\[Nu]_]:=Sum[R[[\[Mu],\[Sigma],\[Nu],\[Sigma]]],{\[Sigma],dimension}];
	(*\:5229\:7528\:5bf9\:79f0\:6027\:6765\:964d\:4f4e\:8ba1\:7b97\:590d\:6742\:5ea6*)
	Ricci=Simplify@Array[If[#1<=#2,ricci[#1,#2],Null]&,{dimension,dimension}];
	Array[If[#1>#2,Ricci[[#1,#2]]=Ricci[[#2,#1]]]&,{dimension,dimension}];
	(*Print[Subscript["R","\[Mu]\[Nu]"]->MatrixForm@Ricci]*);
	Ricci
]


(* ::Section::Closed:: *)
(*End*)


End[]


EndPackage[]
