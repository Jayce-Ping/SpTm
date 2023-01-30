(* ::Package:: *)

(*The package, Space-Time (SpTm), is developed by Jayce Ping (Chinese name: \:5e73\:535a\:6587), a student in XJTU, who is interested in both Mathematica, Differential Geometry and General Relativity. *)


(*\:672c\:4eba\:51fa\:4e8e\:5174\:8da3\:800c\:5236\:4f5c\:4e86\:8be5\:7a0b\:5e8f\:5305\:ff0c\:65b9\:4fbf\:8fdb\:884c\:4e00\:4e9b\:7b80\:5355\:7684\:5fae\:5206\:51e0\:4f55\:8fd0\:7b97\:3002\:540c\:65f6\:ff0c\:4e5f\:4e3a\:4e86\:7eaa\:5ff5\:6211\:7684\:5fae\:5206\:51e0\:4f55\:4e0e\:5e7f\:4e49\:76f8\:5bf9\:8bba\:7684\:5165\:95e8\:5bfc\:5e08\[LongDash]\[LongDash]\:6881\:707f\:5f6c\:6559\:6388\:3002\:672c\:5e0c\:671b\:6709\:751f\:4e4b\:5e74\:80fd\:591f\:62dc\:8bbf\:6881\:8001\:5e08\:ff0c\:4f46\:5948\:4f55\:6881\:8001\:5e08\:4e8e2022\:5e742\:670818\:65e5\:9a7e\:9e64\:897f\:53bb\:ff0c\:6b64\:751f\:65e0\:7f18\:76f8\:89c1\:4e86\:3002*)


(*The MIT License (MIT)
Copyright \[Copyright] 2023 Bowen Ping

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the \[OpenCurlyDoubleQuote]Software\[CloseCurlyDoubleQuote]), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED \[OpenCurlyDoubleQuote]AS IS\[CloseCurlyDoubleQuote], WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.*)


BeginPackage["SpTm`"]


(* ::Section::Closed:: *)
(*::usage information*)


STensor::usage = "STensor[TensorName_Symbol, subindices_List, superindices_List] \:4e3aSTensor\:5bf9\:8c61\:7684\:6807\:51c6\:683c\:5f0f\:ff0c\:662f\:5f20\:91cf\:8fdb\:884c\:62bd\:8c61\:6307\:6807\:8ba1\:7b97\:65f6\:6240\:7528\:7684\:8868\:8fbe\:5f62\:5f0f. "<>"\:4e5f\:53ef\:4ee5\:76f4\:63a5\:91c7\:53d6ctrl+_\:8f93\:5165\:4e0b\:6807\:548cctrl+^\:8f93\:5165\:4e0a\:6807\:7684\:5f62\:5f0f\:8f93\:5165\:5f20\:91cf\:ff08\!\(\*SuperscriptBox[SubscriptBox[\(\:5982R\), \(a\\\ b\\\ c\)], \(d\)]\),\:6ce8\:610f\:6307\:6807\:4e4b\:95f4\:4e0d\:9700\:8981\:7528\:7a7a\:683c\:9694\:5f00\:ff09."

ShowForm::usage = "ShowForm[expr]"<>" "<>"\:5c06STensor\:8f6c\:5316\:4e3a\:6807\:51c6\:683c\:5f0f\:ff0c\:4e0e\:8868\:8fbe\:5f0f\:5176\:4f59\:90e8\:5206\:5747\:4ee5StandardForm\:683c\:5f0f\:8f93\:51fa."

InputExplain::usage = "InputExplain[expr]"<>" "<>"\:5c06\:62bd\:8c61\:6307\:6807\:5f20\:91cf\:8868\:8fbe\:5f0fexpr\:8f6c\:5316\:4e3a\:5185\:90e8\:8ba1\:7b97\:65f6\:4f7f\:7528\:7684\:5f62\:5f0f."


STensorInfo::usage = "STensorInfo[expr]"<>" "<>"\!\(\*SuperscriptBox[SubscriptBox[\(expr\:662f\:4e00\:4e2a\:5f62\:5982T\), \(a\)], \(b\)]\)\:7684\:5f20\:91cf\:ff0c\:7ed9\:51fa\:8be5\:5f20\:91cf\:7684\:5206\:91cf\:4fe1\:606f."<>"\n"<>"STensorInfo[T]"<>" "<>"\:7ed9\:51fa\:540d\:4e3aT\:7684\:6240\:6709\:5f20\:91cf\:7684\:5206\:91cf\:4fe1\:606f\:5217\:8868."<>"\n"<>"STensorInfo[T_STensor]"<>" "<>"\:7ed9\:51faSTensor\:5bf9\:8c61T\:7684\:5206\:91cf\:4fe1\:606f."

MetricInfo::usage = "MetricInfo[]"<>" "<>"\:83b7\:53d6\:5ea6\:89c4\:4fe1\:606f."

CoordinatesInfo::usage = "CoordinatesInfo[]"<>" "<>"\:83b7\:53d6\:5750\:6807\:7cfb\:5217\:8868."


LineElementInfo::usage = "STLineElement[]"<>" "<>"\:83b7\:53d6\:5f53\:524d\:5ea6\:89c4\:5728\:5f53\:524d\:5750\:6807\:7cfb\:4e0b\:7684\:7ebf\:5143\:8868\:8fbe\:5f0f."

VolumeElementInfo::usage = "VolumeElementInfo[]"<>" "<>"\:83b7\:53d6\:5f53\:524d\:5ea6\:89c4\:5728\:5f53\:524d\:5750\:6807\:7cfb\:4e0b\:7684\:9002\:914d\:4f53\:5143\:8868\:8fbe\:5f0f."


SetCoordinates::usage = "SetCoordinates[coodinates_List]"<>" "<>"coodinates\:662f\:4e00\:4e2a\:7b26\:53f7\:5217\:8868\:ff0c\:5305\:542b\:5750\:6807\:7cfb\:6240\:7528\:7684\:7b26\:53f7."

SCoordinatesTransform::usage = "SCoordinatesTransform[targetCoordinates, transformation]"<>" "<>"targetCoordinates\:662f\:76ee\:6807\:5750\:6807\:7cfb\:5217\:8868\:ff0ctransformation\:4e3a\:53d8\:6362\:89c4\:5219."

SetMetric::usage = "SetMetric[components]"<>" "<>"\:5728\:8bbe\:7f6e\:5750\:6807\:7cfb\:540e\:ff0c\:8bbe\:7f6e\:5ea6\:89c4g\:5728\:8be5\:7cfb\:4e0b\:7684\:5206\:91cf\:77e9\:9635components"<>"\n"<>"SetMetric[components_Array, coodinates_List]"<>" "<>"\:8bbe\:7f6e\:5ea6\:89c4g\:5728\:5750\:6807\:7cfbcoodinates\:4e0b\:7684\:5206\:91cf\:77e9\:9635components.\n"<>"SetMetric[components_Array, coodinates_List, metricSymbol_Symbol] \:8bbe\:7f6e\:5ea6\:89c4g\:5728\:5750\:6807\:7cfbcoodinates\:4e0b\:7684\:5206\:91cf\:77e9\:9635components\:ff0c\:548c\:5ea6\:89c4\:7684\:8868\:793a\:7b26\:53f7metricSymbol. "

SetMetricSymbol::usage = "SetMetricSymbol[g]"<>" "<>"\:8bbe\:7f6e\:5ea6\:89c4\:7b26\:53f7\:4e3ag."

SetTensor::usage = "SetTensor[\!\(\*SuperscriptBox[SubscriptBox[\(T\), \(sub\)], \(sup\)]\), comopents]"<>" "<>"\:8bbe\:7f6e\:5f20\:91cfT\:53ca\:5176\:5728\:5f53\:524d\:5750\:6807\:7cfb\:4e0b\:7684\:5206\:91cf"<>"\n"<>"SetTensor[T_STensor, components]"<>" "<>"\:8bbe\:7f6e\:5f20\:91cfT\:5728\:5f53\:524d\:5750\:6807\:7cfb\:4e0b\:7684\:5206\:91cf."


STSymmetrize::usage = "STSymmetrize[expr, permutation_List]"<>" "<>"\:5c06expr\:8868\:793a\:7684\:5f20\:91cf\:8fdb\:884c\:5bf9\:79f0\:5316\:ff0c\:5bf9\:79f0\:5316\:7684\:6307\:6807\:4e3apermutation\:5217\:8868\:4e2d\:7684\:6307\:6807."<>"\n"<>"STSymmetrize[T_STensor, permutation_List]"<>" "<>"\:5c06STensor T\:8fdb\:884c\:5bf9\:79f0\:5316\:ff0c\:5bf9\:79f0\:5316\:7684\:6307\:6807\:4e3apermutation\:5217\:8868\:4e2d\:7684\:6307\:6807."

STAntiSymmetrize::usage = "STAntiSymmetrize[expr, permutation_List]"<>" "<>"\:5c06expr\:8868\:793a\:7684\:5f20\:91cf\:8fdb\:884c\:53cd\:79f0\:5316\:ff0c\:53cd\:79f0\:5316\:7684\:6307\:6807\:4e3apermutation\:5217\:8868\:4e2d\:7684\:6307\:6807."<>"\n"<>"STAntiSymmetrize[T_STensor, permutation_List]"<>" "<>"\:5c06STensor T\:8fdb\:884c\:53cd\:79f0\:5316\:ff0c\:53cd\:79f0\:5316\:7684\:6307\:6807\:4e3apermutation\:5217\:8868\:4e2d\:7684\:6307\:6807."


STCalculate::usage = "STCalculate[expr]"<>" "<>"\:5c06\:62bd\:8c61\:6307\:6807\:5f20\:91cf\:8868\:8fbe\:5f0fexpr\:8fdb\:884c\:8ba1\:7b97\:548c\:5316\:7b80."

STSpecify::usage = "STSpecify[expr]"<>" "<>"\:5c06\:62bd\:8c61\:6307\:6807\:8868\:8fbe\:5f0fexpr\:76f4\:63a5\:8f6c\:5316\:4e3a\:5177\:4f53\:6307\:6807\:8fdb\:884c\:8ba1\:7b97\:ff0c\:5f97\:5230\:7ed3\:679c\:5f20\:91cf\:7684\:5177\:4f53\:6307\:6807\:5206\:91cf\:5f62\:5f0f."

STSimSpecify::usage = "STSimSpecify[expr]"<>" "<>"\:5c06\:8868\:8fbe\:5f0f\:5148\:8fdb\:884c\:53ef\:80fd\:7684\:7b80\:5316\:518d\:8ba1\:7b97\:5176\:5177\:4f53\:6307\:6807\:7684\:5206\:91cf\:5f62\:5f0f."


STCalcTensor::usage = "STCalcTensor[\"Tensor\"]"<>" "<>"\:8ba1\:7b97\:540d\:4e3aTensor\:7684\:5f20\:91cf.Tensor\:53ef\:9009:Christoffel, RiemannTensor, RicciTensor, RicciScalar\:ff0cEinsteinTensor."<>"\n"<>"STCalcTensor[\"Tensor\", metric]"<>" "<>"\:91cd\:65b0\:7ed9\:5b9a\:5ea6\:89c4\:5206\:91cf\:ff0c\:5728\:5f53\:524d\:5750\:6807\:7cfb\:4e0b\:8ba1\:7b97\:5f20\:91cfTensor."<>"\n"<>"STCalcTensor[\"Tensor\", metric, coodinates]"<>" "<>"\:91cd\:65b0\:7ed9\:5b9a\:5ea6\:89c4\:5206\:91cf\:548c\:5750\:6807\:7cfb\:ff0c\:8ba1\:7b97\:5f20\:91cfTensor."


SCalcChristoffel::usage = "SCalcChristoffel[metric, coodinates]"<>" "<>"\:7ed9\:5b9a\:5750\:6807\:7cfb\:ff0c\:5e76\:7ed9\:51fa\:5ea6\:89c4\:5728\:8be5\:5750\:6807\:7cfb\:4e0b\:7684\:5206\:91cf\:77e9\:9635\:ff0c\:8ba1\:7b97\:514b\:6c0f\:7b26\:7684\:5206\:91cf\!\(\*SuperscriptBox[SubscriptBox[\(\[CapitalGamma]\), \(\[Mu]\[Nu]\)], \(\[Sigma]\)]\) -(\:6ce8\:610f\:4e0a\:4e0b\:6307\:6807\:987a\:5e8f)."

SCalcRiemannTensor::usage = "SCalcRiemannTensor[metric, coodinates]"<>" "<>"\:7ed9\:5b9a\:5750\:6807\:7cfb\:ff0c\:5e76\:7ed9\:51fa\:5ea6\:89c4\:5728\:8be5\:5750\:6807\:7cfb\:4e0b\:7684\:5206\:91cf\:77e9\:9635\:ff0c\:8ba1\:7b97Riemann\:66f2\:7387\:5f20\:91cf\:7684\:5206\:91cf\!\(\*SuperscriptBox[SubscriptBox[\(R\), \(\[Mu]\[Nu]\[Sigma]\)], \(\[Rho]\)]\)-(\:6ce8\:610f\:4e0a\:4e0b\:6307\:6807\:987a\:5e8f)."

SCalcRicciTensor::usage = "SCalcRicciTensor[metric, coodinates]"<>" "<>"\:7ed9\:5b9a\:5750\:6807\:7cfb\:ff0c\:5e76\:7ed9\:51fa\:5ea6\:89c4\:5728\:8be5\:5750\:6807\:7cfb\:4e0b\:7684\:5206\:91cf\:77e9\:9635\:ff0c\:8ba1\:7b97Ricci\:5f20\:91cf\:7684\:5206\:91cf\!\(\*SubscriptBox[\(R\), \(\[Mu]\[Nu]\)]\)."

SCalcRicciScalar::usage = "SCalcRicciScalar[metric, coodinates]"<>" "<>"\:7ed9\:5b9a\:5750\:6807\:7cfb\:ff0c\:5e76\:7ed9\:51fa\:5ea6\:89c4\:5728\:8be5\:5750\:6807\:7cfb\:4e0b\:7684\:5206\:91cf\:77e9\:9635\:ff0c\:8ba1\:7b97Ricci\:6807\:91cf."

SCalcEinsteinTensor::usage = "SCalcEinsteinTensor[metric, coodinates]"<>" "<>"\:7ed9\:5b9a\:5750\:6807\:7cfb\:ff0c\:5e76\:7ed9\:51fa\:5ea6\:89c4\:5728\:8be5\:5750\:6807\:7cfb\:4e0b\:7684\:5206\:91cf\:77e9\:9635\:ff0c\:8ba1\:7b97Einstein\:5f20\:91cf\:7684\:5206\:91cf."


Begin["Private`"]


(* ::Section::Closed:: *)
(*SpTmHelp*)


SpTmHelp = {
	"\:6b22\:8fce\:4f7f\:7528SpTm\:ff01\n",
	"\[FilledSmallCircle] \:5f20\:91cf\:7684\:4e0b\:6807\:901a\:8fc7Ctrl+-\:8f93\:5165\:ff0c\:4e0a\:6807\:901a\:8fc7Ctrl+^\:8f93\:5165\:ff0c\:901a\:8fc7InputExplain\:51fd\:6570\:53ef\:4ee5\:67e5\:770b\:8868\:8fbe\:5f0f\:5728\:7a0b\:5e8f\:5305\:4e2d\:8f6c\:5316\:540e\:7684\:5f62\:5f0f.\n",
	"\[FilledSmallCircle] \:7a0b\:5e8f\:5305\:4e2d\:4f7f\:7528STensor[T, subIndices, supIndices]\:6765\:8868\:793a\:540d\:79f0\:4e3aT\:ff0c\:4e0b\:6807\:5217\:8868\:4e3asubIndices\:ff0c\:4e0a\:6807\:5217\:8868\:4e3asupIndices\:7684\:5f20\:91cf.\n",
	"\[FilledSmallCircle] \:7b26\:53f7g\:5c06\:88ab\:8ba4\:4e3a\:662f\:9ed8\:8ba4\:7684\:5ea6\:89c4\:5f20\:91cf\:8868\:793a\:7b26\:53f7\:ff0c\:53ef\:4ee5\:901a\:8fc7SetMetric\:6765\:4fee\:6539. \[Del]\:4e3a\:4e0e\:5176\:76f8\:9002\:914d\:7684\:534f\:53d8\:5bfc\:6570\:7b97\:7b26.\n",
	"\[FilledSmallCircle] \:4f7f\:7528STensorInfo[T]\:6765\:83b7\:53d6\:5f20\:91cfT\:7684\:5206\:91cf\:4fe1\:606f.\n",
	"\[FilledSmallCircle] \:4f7f\:7528STCalculate[expr]\:6765\:5316\:7b80\:62bd\:8c61\:6307\:6807\:7684\:5f20\:91cf\:8868\:8fbe\:5f0fexpr.\n",
	"\[FilledSmallCircle] \:4f7f\:7528ShowForm[expr]\:5c06\:8868\:8fbe\:5f0f\:663e\:793a\:4e3a\:6807\:51c6\:683c\:5f0f.\n",
	"\[FilledSmallCircle] \:4f7f\:7528STSpecify[expr]\:5c06\:8868\:8fbe\:5f0f\:76f4\:63a5\:8f6c\:5316\:4e3a\:5177\:4f53\:6307\:6807\:8868\:8fbe\:5f0f\:5e76\:8ba1\:7b97.\n",
	"\[FilledSmallCircle] \:4f7f\:7528STSimSpecify[expr]\:5c06\:62bd\:8c61\:6307\:6807\:8868\:8fbe\:5f0f\:8fdb\:884c\:9002\:5f53\:5316\:7b80\:540e\:8f6c\:5316\:4e3a\:5177\:4f53\:6307\:6807\:8868\:8fbe\:5f0f\:8fdb\:884c\:8ba1\:7b97.\n",
	"\[FilledSmallCircle] \:5728\:4f7f\:7528SpTm\:65f6\:ff0c\:6240\:6709\:5c0f\:5199\:82f1\:6587\:5b57\:6bcd(a,b...)\:548c\:5e0c\:814a\:5b57\:6bcd(\[Alpha],\[Beta]...)\:90fd\:4f1a\:88ab\:4fdd\:62a4\:ff0c\:7528\:4e8e\:8868\:793a\:5f20\:91cf\:6307\:6807\:ff0c\:8bf7\:4e0d\:8981\:5c06\:5b83\:4eec\:4f5c\:4e3a\:53d8\:91cf\:540d\:ff0c\:4f46\:53ef\:4ee5\:4f5c\:4e3a\:5750\:6807\:7cfb\:7b26\:53f7\:6216\:5f20\:91cf\:7b26\:53f7.\n",
	"\[FilledSmallCircle] \:8f93\:5165\"?SpTm`*\"\:67e5\:770b\:53ef\:7528\:51fd\:6570\:4ee5\:53ca\:4f7f\:7528\:65b9\:6cd5."
};
SpTmHelp::usage = StringJoin[SpTmHelp];
Print[StringJoin[SpTmHelp]]

(*STensor\:5199\:6cd5\:7684\:6807\:51c6\:683c\:5f0f*)
STensor[TensorName_Symbol, subindex_List, superindex_List];(*STensor\:6807\:51c6\:683c\:5f0f*)
ATensor[subindex_List, superindex_List, components_List];(*ATensor\:6807\:51c6\:683c\:5f0f*)
Protect[STensor, ATensor];


(* ::Section::Closed:: *)
(*\:5168\:5c40\:53d8\:91cf Global Variables in this Package*)


(*\:5f20\:91cf\:5206\:91cf*)
Unprotect[TensorComponents]
TensorComponents = Association[];
Protect[TensorComponents]

(*\:5f53\:524d\:5750\:6807\:7cfb*)
Unprotect[CurrentCoordinates]
CurrentCoordinates = List[];
Protect[CurrentCoordinates]

(*\:5ea6\:89c4\:5206\:91cf*)
Unprotect[MetricComponents]
MetricComponents = List[];
Protect[MetricComponents]

(*\:5ea6\:89c4\:7b26\:53f7*)
Unprotect[MetricSymbol]
Unprotect[Global`g]
MetricSymbol = Global`g;
Protect[MetricSymbol]
Protect[Global`g]


(*\:4fdd\:62a4\:5168\:5c40\:53d8\:91cf\:4e2d\:7684\:6240\:6709\:5c0f\:5199\:82f1\:6587\:5b57\:6bcd\:548c\:5e0c\:814a\:5b57\:6bcd\:7528\:4f5c\:6307\:6807*)
globalabc = ToExpression //@ StringJoin["Global`",#]& /@ Join[Alphabet[], Alphabet["Greek"]];
Unprotect[##]& //@ globalabc;
Protect[#]& //@ globalabc;


(*\:62bd\:8c61\:6307\:6807\:5411\:5177\:4f53\:6307\:6807\:7684\:8f6c\:5316\:89c4\:5219*)
specificReplaceRule = Thread[#1->#2&[ToExpression @ StringJoin["Global`",#]& /@ Alphabet[][[1;;24]], ToExpression @ StringJoin["Global`",#]& /@ Alphabet["Greek"][[1;;24]]]];


(* ::Section::Closed:: *)
(*\:83b7\:53d6\:5f20\:91cf\:4fe1\:606f Get Information of Tensors*)


(* ::Subsection::Closed:: *)
(*\:683c\:5f0f\:5316\:6307\:6807 Indices Format*)


(*\:5c06STensor\:91cd\:65b0\:7528abc\:5c06\:6307\:6807\:6309\:987a\:5e8f\:8fdb\:884c\:6539\:5199*)
STensorReIndex[T_STensor] := Module[
{
	sublen = Length @ T[[2]],
	suplen = Length @ T[[3]],
	alphabet = ToExpression @ StringJoin[{"Global`",#}]& /@ Alphabet[]
},
	STensor[T[[1]], alphabet[[1 ;; sublen]], alphabet[[(sublen + 1) ;; (sublen + suplen)]]]
]


(* ::Subsection::Closed:: *)
(*\:83b7\:53d6\:5f20\:91cf\:4fe1\:606f Get STensor Information*)


STensorInfo::ErrorInput = "\:8f93\:5165\:683c\:5f0f\:6709\:8bef.";
STensorInfo::NoSuchTensor = "\!\(\*SuperscriptBox[SubscriptBox[\(\:5f20\:91cf`1`\), \(`2`\)], \(`3`\)]\)\:672a\:8bbe\:7f6e\:5206\:91cf.";
STensorInfo::NoTensorName = "\:540d\:4e3a`1`\:7684\:5f20\:91cf\:672a\:8bbe\:7f6e\:5206\:91cf.";
(*\:83b7\:53d6\:5f20\:91cf\:4fe1\:606f*)
STensorInfo[expr__] := Module[
{
	tensor = InputExplain[expr]
},
	If[
		(Head @ tensor =!= STensor) && (Head @ tensor =!= Symbol),
		Message[STensorInfo::ErrorInput];
		Abort[]
	];
	
	STensorInfo[tensor]
];

(*\:83b7\:53d6\:7279\:5b9aSTensor\:7684\:4fe1\:606f*)
STensorInfo[T_STensor] := Module[
{
	specificT,
	(*\:6307\:6807\:91cd\:65b0\:683c\:5f0f\:5316*)
	rT = STensorReIndex[T]
},
	If[
		!KeyExistsQ[TensorComponents, rT],
		Message[STensorInfo::NoSuchTensor, T[[1]], StringJoin[ ToString/@(T[[2]]) ], StringJoin[ ToString/@(T[[3]]) ] ];
		Abort[]
	];
	specificT = STensor[T[[1]], T[[2]]/.specificReplaceRule, T[[3]]/.specificReplaceRule];
	ShowSTensor[specificT, TensorComponents[rT]]
]
(*\:83b7\:53d6\:540d\:4e3aT\:7684\:5f20\:91cf\:4fe1\:606f*)
STensorInfo[T_Symbol] := Module[
{
	keys
},
	keys = KeySelect[TensorComponents, MatchQ[#, STensor[T, __, __]]& ];
	If[
		Length @ keys == 0,
		Message[STensorInfo::NoTensorName, T];
		Abort[]
	];
	(*\:5c06keys\:8f6c\:5316\:4e3a\:5217\:8868\:ff0c\:7136\:540e\:5c06\:5176\:4e2d\:7684STensor\:8f6c\:5316\:4e3a\:5177\:4f53\:6307\:6807*)
	ShowSTensor[#1, #2]& @@@ (keys//.{Association|Rule -> List}/.{t_STensor :> STensor[t[[1]], t[[2]]/.specificReplaceRule, t[[3]]/.specificReplaceRule]})
]



(* ::Section::Closed:: *)
(*\:8bbe\:7f6e\:5f20\:91cf\:5206\:91cf Set Components of Tensors*)


SetTensor::ErrorExpression = "\:5f20\:91cf\:683c\:5f0f\:8f93\:5165\:9519\:8bef.";
SetTensor::NoCoordinates = "\:672a\:8bbe\:7f6e\:5750\:6807\:7cfb.";
SetTensor::WrongDimension = "\:5f20\:91cf\:5206\:91cf\:7684\:7ef4\:6570 (`1`) \:4e0e\:5750\:6807\:7cfb\:7ef4\:6570 (`2`) \:4e0d\:5339\:914d.";
SetTensor::WrongShape = "\:5f20\:91cf\:5206\:91cf\:7684\:5c42\:6570 (`1`) \:4e0e\:5f20\:91cf\:578b\:53f7 (`2`) \:4e0d\:5339\:914d.";

SetTensor[T_Symbol, components_?NumberQ] := Module[
{
	temp = STensor[T, {}, {}]
},
	Unprotect[TensorComponents];
	AppendTo[TensorComponents, temp -> components];
	Protect[TensorComponents];
]

SetTensor[expr__, components_List] := Module[
{
	T = InputExplain[expr]
},
	(*\:5224\:65ad\:683c\:5f0f\:662f\:5426\:6b63\:786e*)
	If[
		Head[T] =!= STensor,
		Message[SetTensor::ErrorExpression];
		Abort[]
	];
	
	SetTensor[T, components]
]

(*\:8bbe\:7f6e\:5f20\:91cf\:5206\:91cf*)
SetTensor[T_STensor, components_List] := Module[
{
	temp = STensorReIndex[T],
	comp = components
},
	If[
		Length[CurrentCoordinates] == 0,
		Message[SetTensor::NoCoordinates];
		Abort[]
	];
	(*\:68c0\:67e5\:5f20\:91cf\:578b\:53f7\:4e0e\:5206\:91cf\:7ef4\:6570\:662f\:5426\:5339\:914d*)	
	If[
		Length @ Dimensions @ components != Length @ Join[T[[2]], T[[3]]],
		Message[SetTensor::WrongShape, Length @ Dimensions @ components, Length @ Join[T[[2]], T[[3]]]];
		Abort[]
	];
	(*\:68c0\:67e5\:5206\:91cf\:7ef4\:6570\:4e0e\:5750\:6807\:7cfb\:7ef4\:6570\:662f\:5426\:5339\:914d*)
	If[
		First @ Dimensions @ components != Length @ CurrentCoordinates,
		Message[SetTensor::WrongDimension, First @ Dimensions @ components, Length @ CurrentCoordinates];
		Abort[]
	];
	Unprotect[TensorComponents];
	AppendTo[TensorComponents, temp -> components];
	Protect[TensorComponents];
]


(* ::Section::Closed:: *)
(*\:8bbe\:7f6e\:5750\:6807\:7cfb Set Coordinate System*)


SetCoordinates::ErrorSymbol = "\:8f93\:5165\:5750\:6807\:7cfb\:4e2d\:5b58\:5728\:975e\:7b26\:53f7\:ff08Symbol\:ff09\:5143\:7d20.";
SetCoordinates[Coordinates_List] := Module[{},
	If[
		!AllTrue[Coordinates, MatchQ[#,_Symbol]&],
		Message[SetCoordinates::ErrorSymbol];
		Abort[]
	];
	Unprotect[CurrentCoordinates];
	(*\:91ca\:653e\:5750\:6807\:7cfb\:4e2d\:7528\:5230\:7684\:7b26\:53f7*)
	If[Length @ CurrentCoordinates > 0, Unprotect@@CurrentCoordinates];
	CurrentCoordinates = Coordinates;
	(*\:4fdd\:62a4\:5750\:6807\:7cfb\:4e2d\:7528\:5230\:7684\:7b26\:53f7*)
	Protect@@CurrentCoordinates;
	Protect[CurrentCoordinates];
	(*\:6e05\:7a7a\:4e4b\:524d\:5750\:6807\:7cfb\:4e0b\:8bbe\:5b9a\:7684\:6240\:6709\:5f20\:91cf\:5206\:91cf*)
	Unprotect[TensorComponents];
	TensorComponents = Association[];
	Protect[TensorComponents];
]
(*\:83b7\:53d6\:5f53\:524d\:5750\:6807\:7cfb\:5217\:8868*)
CoordinatesInfo[] := CurrentCoordinates;


(* ::Section::Closed:: *)
(*\:5750\:6807\:53d8\:6362 Coordinates Transformation*)


(* ::Subsection::Closed:: *)
(*\:4e3b\:53d8\:6362\:51fd\:6570 Main Function*)


SCoordinatesTransform::DimensionNotMatch = "\:5750\:6807\:7cfb\:7ef4\:6570\:4e0d\:5339\:914d.";
SCoordinatesTransform::ErrorSymbol = "\:5750\:6807\:7cfb\:4e2d\:5b58\:5728\:975e\:7b26\:53f7\:ff08Symbol\:ff09\:5143\:7d20.";
SCoordinatesTransform::ErrorTransformation = "\:5750\:6807\:53d8\:6362\:683c\:5f0f\:9519\:8bef.";
SCoordinatesTransform::NoMetricComonents = "\:672a\:8bbe\:7f6e\:5ea6\:89c4\:5206\:91cf.";

SCoordinatesTransform[target_List, transformation_List] := Module[
{
	record,
	keys,
	newTensorComponents,
	newMetricComponents,
	newMetricComponentsInv,
	transAs = transformation/.{List -> Association},
	trans
},
	If[
		Length[target] != Length[CurrentCoordinates],
		Message[SCoordinateTransform::DimensionNotMatch];
		Abort[]
	];
	If[
		!AllTrue[target, MatchQ[#,_Symbol]&],
		Message[SCoordinateTransform::ErrorSymbol];
		Abort[]
	];
	If[
		!AllTrue[transformation, MatchQ[#, __->__]&],
		Message[SCoordinateTransform::ErrorTransformation];
		Abort[]
	];
	If[
		Length[MetricComponents] == 0,
		Message[SCoordinateTransform::NoMetricComonents];
		Abort[]
	];
	(*\:5c06\:53d8\:6362\:8865\:5168*)
	trans = Table[
		If[
			KeyExistsQ[transAs, CurrentCoordinates[[i]]],
			CurrentCoordinates[[i]] -> transAs[CurrentCoordinates[[i]]],
			CurrentCoordinates[[i]] -> CurrentCoordinates[[i]]
		],
		{i,1,Length[CurrentCoordinates]}
	];
	
	(*\:5f53\:524d\:5750\:6807\:7cfb\:4e0b\:7684\:6240\:6709\:8bbe\:7f6e\:4e86\:5206\:91cf\:7684\:5f20\:91cf,\:9664\:53bb\:5ea6\:89c4\:5f20\:91cf\:548c\[Delta]*)
	keys = Delete[Keys[TensorComponents], {{1},{2},{3}}];
	record = Array[keys[[#]] -> STensorTrans[keys[[#]], target, trans]&, Length[keys], 1, Association];
	
	(*\:5148\:4fee\:6539\:5ea6\:89c4\:7684\:5206\:91cf*)
	newMetricComponents = Simplify @ componentsTrans[MetricComponents, target, trans];
	
	(*\:5ea6\:89c4\:9006\:6620\:5c04\:7684\:5206\:91cf*)
	newMetricComponentsInv = Inverse[newMetricComponents];
	
	(*\:91cd\:65b0\:8bbe\:7f6e\:5ea6\:89c4*)
	SetMetric[newMetricComponents, target];
	
	(*\:5c06record\:540e\:534a\:90e8\:5206\:90fd\:8f6c\:5316\:4e3aATensor*)
	record = record/.{STensor[MetricSymbol, {}, supIndex__] :> ATensor[{}, supIndex, newMetricComponentsInv]};
	
	(*\:5c06\:7b2c\:4e8c\:90e8\:5206\:4e58\:6cd5\:6539\:4e3a\:5f20\:91cf\:79ef\:8ba1\:7b97*)
	record = record/.{Times[t_ATensor, s_ATensor] :> ATensorTimes[t,s]};
	
	(*\:5c06\:4e24\:90e8\:5206\:8fdb\:884c\:5f20\:91cf\:4e58\:79ef*)
	newTensorComponents = Array[keys[[#]] -> Simplify@(ATensorTimes[#1, #2]&[record[[#,1]], record[[#,2]]][[3]])& , Length[record], 1, Association];
	
	Unprotect[TensorComponents];
	Table[TensorComponents[keys[[i]]] = newTensorComponents[keys[[i]]], {i, Length[keys]}];
	Protect[TensorComponents];
];


(* ::Subsection::Closed:: *)
(*\:5c06STensor\:8fdb\:884c\:53d8\:6362 Transform STensor*)


(*\:5c06STensor\:7684\:5206\:91cf\:8fdb\:884c\:5750\:6807\:53d8\:6362*)
STensorTrans[T_STensor, target_List, transformation_List] := Module[
{
	(*\:5c06T\:7684\:6307\:6807\:5168\:90e8\:964d\:4e0b*)
	DownT = IndicesDown[T],
	oldDownTComponents,
	newDownTComponents,
	usedIndices,
	usableIndices,
	metriclis
},
	(*\:65e7\:5750\:6807\:7cfb\:4e0b\:7684downT\:7684\:5206\:91cf*)
	oldDownTComponents = SCalcSpecificExpression[DownT][[1,3,1]];
	(*\:65b0\:5750\:6807\:7cfb\:4e0b\:7684downT\:7684\:5206\:91cf*)
	newDownTComponents = componentsTrans[oldDownTComponents, target, transformation];
	(*\:4f7f\:7528\:8fc7\:7684\:7b26\:53f7*)
	usedIndices = Flatten @ DeleteDuplicates @ Cases[DownT, tensor_STensor :> {tensor[[2]], tensor[[3]]}];
	(*\:53ef\:4f7f\:7528\:7684\:7b26\:53f7*)
	usableIndices = Complement[ToExpression @ Alphabet[], usedIndices];
	(*\:7528\:4e8e\:5347\:6307\:6807\:7684\:5ea6\:89c4\:5217\:8868*)
	metriclis = Array[ STensor[MetricSymbol, {}, {#1, #2}]&[T[[3]][[#]], usableIndices[[#]]]&, {Length[T[[3]]]}, 1, Times];
	(*\:5148\:8fd4\:56de\:964d\:4e0b\:6307\:6807\:540e\:8ba1\:7b97\:65b0\:5206\:91cf\:7684\:5f20\:91cf\:ff0c\:4ee5\:53ca\:5347\:6307\:6807\:6240\:7528\:7684\:5ea6\:89c4\:5217\:8868*)
	{ATensor[Join[T[[2]], T[[3]]], {}, newDownTComponents], metriclis}
]


(* ::Subsection::Closed:: *)
(*\:5c06(0,n)\:578b\:5f20\:91cf\:8fdb\:884c\:5750\:6807\:53d8\:6362\:ff0c\:4e0d\:9700\:8981\:6d89\:53ca\:5750\:6807\:7cfb\:7684\:9006\:53d8\:6362*)
(* Do Coordinate Transformation to  a (0,n) Tensor ;Don't Need Inverse Transformation*)


(*\:5c06(0, n)\:578b\:5f20\:91cf\:7684\:5206\:91cf\:8fdb\:884c\:5750\:6807\:53d8\:6362*)
componentsTrans[components_?ArrayQ, target_List, transformation_List] := Module[
{
	(*\:8bb0\:5f55\:7ef4\:6570*)
	dimension = Dimensions[components],
	(*\:5f20\:91cf\:53d8\:6362\:5f8b\:7684\:7cfb\:6570*)
	coeff,
	(*\:5bfc\:6570\:77e9\:9635*)
	jacobi = D[transformation[[All,2]],{target}],
	term
},
	(*(0,n)\:578b\:5f20\:91cf*)
	coeff[sup_List,sub_List]:=Module[
	{
		temp = Table[{sup[[i]],sub[[i]]},{i,Length[sup]}]
	},
		Array[jacobi[[sup[[#]],sub[[#]]]]&, {Length[sup]}, 1, Times]
	];
	(*\:4e0b\:6807\:4e3asub\:7684\:9879*)
	term[sub_List] := Array[coeff[{##},sub]components[[##]]/.transformation&, dimension, 1, Plus];
	(*undone*)
	Array[term[{##}]&, dimension]
]


(* ::Section::Closed:: *)
(*\:8bbe\:7f6e\:5ea6\:89c4 Set Metric*)


SetMetric::NoCoordinates = "\:6ca1\:6709\:8bbe\:7f6e\:5750\:6807\:7cfb.";
SetMetric::ErrorDimensions = "\:5206\:91cf\:77e9\:9635\:7ef4\:6570 (`1`) \:548c\:5750\:6807\:7cfb\:7ef4\:6570 (`2`) \:4e0d\:5339\:914d.";


SetMetricSymbol[metricSymbol_Symbol] := Module[{},
	Unprotect[MetricSymbol];
	MetricSymbol = metricSymbol;
	Protect[MetricSymbol];
];


SetMetric[Components_?ArrayQ]:=Module[ {}, SetMetric[Components, CurrentCoordinates] ];

SetMetric[Components_?ArrayQ, Coordinates_List]:=Module[ {}, SetMetric[Components, Coordinates, MetricSymbol] ];

SetMetric[Components_?ArrayQ, Coordinates_List, metricSymbol_Symbol]:=Module[
{
	aa = Global`a,
	bb = Global`b
},
	If[
		Coordinates == {} && Length[CurrentCoordinates] == 0,
		Message[SetMetric::NoCoordinates];
		Abort[]
	];
	If[
		First @ Dimensions[Components] != Length[Coordinates],
		Message[SetMetric::ErrorDimensions, First @ Dimensions[Components], Length[Coordinates]];
		Abort[]
	];
	If[Coordinates =!= CurrentCoordinates, SetCoordinates[Coordinates]];
	If[metricSymbol =!= MetricSymbol, SetMetricSymbol[metricSymbol]];
	
	Unprotect[MetricComponents];
	MetricComponents = Components;
	Protect[MetricComponents];
	(*\:8bbe\:7f6e\:5ea6\:89c4\:5f20\:91cf\:7684\:5206\:91cf*)(*\:4e4b\:524d\:7684\:4f1a\:88ab\:8986\:76d6*)
	SetTensor[STensor[metricSymbol, {aa, bb}, {}], Components];
	SetTensor[STensor[metricSymbol, {}, {aa, bb}], Inverse[Components]];
	(*Subscript[\:8bbe\:7f6e\[Delta], a]^b*)
	SetTensor[STensor[Global`\[Delta], {aa}, {bb}], IdentityMatrix[Length[Coordinates]]];
];


MetricInfo[]:=Module[{},
	Row[{ Subscript[MetricSymbol, Row[{"\[Mu]","\[Nu]"}]], "=" , MatrixForm[MetricComponents] }]
];


(* ::Section::Closed:: *)
(*\:7ebf\:5143 Line Element*)


(* ::Subsection::Closed:: *)
(*\:7ebf\:5143\:8868\:8fbe\:5f0f Expression of Line Element*)


LineElementInfo::NoCoordinates = "\:672a\:8bbe\:7f6e\:5750\:6807\:7cfb.";
LineElementInfo::NoMetric = "\:672a\:8bbe\:7f6e\:5f20\:91cf.";
LineElementInfo[] := Module[
{
	diffCoordinatesVector
},
	If[
		Length @ CurrentCoordinates == 0,
		Message[LineElementInfo::NoCoordinates];
		Abort[]
	];
	If[
		Length @ MetricComponents == 0,
		Message[LineElementInfo::NoMetric];
		Abort[]
	];
	diffCoordinatesVector = DifferentialD /@ CurrentCoordinates;
	diffCoordinatesVector . MetricComponents . diffCoordinatesVector
]


(* ::Section::Closed:: *)
(*\:4f53\:5143 Volume Element*)


VolumeElementInfo::NoCoordinates = "\:672a\:8bbe\:7f6e\:5750\:6807\:7cfb.";
VolumeElementInfo::NoMetric = "\:672a\:8bbe\:7f6e\:5f20\:91cf.";
VolumeElementInfo[] := Module[
{
	diffCoordinatesVector
},
	If[
		Length @ CurrentCoordinates == 0,
		Message[LineElementInfo::NoCoordinates];
		Abort[]
	];
	If[
		Length @ MetricComponents == 0,
		Message[LineElementInfo::NoMetric];
		Abort[]
	];
	diffCoordinatesVector = DifferentialD /@ CurrentCoordinates;
	\[PlusMinus]Simplify@Sqrt[Abs[Det[MetricComponents]]] * Wedge @@ diffCoordinatesVector
]


(* ::Section::Closed:: *)
(*\:8f93\:5165\:89e3\:91ca Input Explain*)


(*\:8f93\:5165\:89e3\:91ca\:ff0c\:6307\:6807\:4e0d\:9700\:8981\:9694\:5f00\:ff0c\:6bcf\:4e2a\:82f1\:6587\:5b57\:6bcd\:88ab\:8ba4\:4e3a\:662f\:4e00\:4e2a\:6307\:6807*)

InputExplain::DuplicateSubIndex = "\:4e0b\:6307\:6807\:4e2d\:6709\:91cd\:590d. Duplicate Subindex.";
InputExplain::DuplicateSupIndex = "\:4e0a\:6307\:6807\:4e2d\:6709\:91cd\:590d. Duplicate Superindex.";
InputExplain[expr__] := Module[
{
	res,
	sup,
	sub,
	InputExplainRule,
	split,
	powerplusRule
},
	
	split[x_] := ToExpression[StringSplit[ToString[x], ""]];
	
	InputExplainRule = {
		Power[Subscript[x_Symbol, y_], z_] :> STensor[x, split[y], split[z]],
		Subscript[x_Symbol, y_] :> STensor[x, split[y], {}],
		Power[x_Symbol, y_] :> STensor[x, {}, split[y]]
	};
	powerplusRule =
	{
		Power[Subscript[x_Symbol, y_], Plus[a_Symbol, b___Symbol]] :> Times[ STensor[x, split[y], split[a]], Power[Subscript[x, y], Plus[b]]],
		Power[x_Symbol, Plus[a_Symbol, b___Symbol]] :> Times[ STensor[x, {}, split[a]], Power[x, Plus[b]]]
	};
	
	(*\:7279\:6b8a\:5904\:7406Mathematica\:7b14\:8bb0\:672c\:5c06\:81ea\:52a8\:5c06\:6307\:6570\:76f8\:52a0\:7684\:60c5\:51b5*)
	res = expr //. powerplusRule;
	
	res = res /. InputExplainRule;
	
	res
];


(* ::Section::Closed:: *)
(*\:683c\:5f0f\:5316\:8f93\:51fa Show Form*)


Superscript[x_, y_, superIndex__] := Superscript[x, Row[{y, superIndex}]];

Subscript[x_, y_, subIndex__] := Subscript[x, Row[{y, subIndex},""]];



ShowForm[expr___] := StandardForm[expr /. {T_STensor :> ShowSTensor[T]}];

ShowSTensor[T_STensor] := Module[
{
	out = T[[1]],
	r = {{} -> "", List -> Sequence}
},
	If[Length[T[[2]]] > 0, out = Subscript[out, T[[2]] /.r]];
	If[Length[T[[3]]] > 0, out = Superscript[out, T[[3]] /.r]];
	out
];

ShowSTensor[T_STensor, components_] := Row[{ShowSTensor[T], "=", MatrixForm[components]}];


(* ::Section::Closed:: *)
(*\:62bd\:8c61\:6307\:6807\:8fd0\:7b97 Calculation of Abstract Indices Expression*)


(* ::Subsection::Closed:: *)
(*\:5bf9\:79f0\:5316 Symmetrize*)


(*\:5173\:4e8eperlist\:7684\:6307\:6807\:5bf9\:79f0\:5316\:67d0\:4e2a\:8868\:8fbe\:5f0f*)
STSymmetrize[expr__, perList_List] := Module[
{
	exp = InputExplain[expr],
	terms
},
	terms = Flatten[exp //.{Plus[x_,y_] :> {x,y}}];
	If[
		(*\:4ec5\:6709\:4e00\:9879*)
		Head[terms] =!= List,
		STSymmetrizeTerm[terms, perList],
		Plus @@ (STSymmetrizeTerm[#, perList]& /@ terms)
	]
]


STSymmetrize[T_STensor, perList_List] := Module[
{
	sublen = Length @ T[[2]],
	suplen = Length @ T[[3]],
	indices = Join[T[[2]], T[[3]]],
	perRule,
	resultIndices
},
	perRule = Table[perList[[i]]->#[[i]], {i,Length[perList]} ]& /@ Permutations[perList];
	resultIndices = Table[indices/.perRule[[i]],{i, Length[perRule]}];
	1/Length[resultIndices] * Array[STensor[T[[1]], resultIndices[[#, 1;;sublen]], resultIndices[[#, sublen+1;;sublen+suplen]]]&, Length[resultIndices], 1, Plus]
]


(*\:5bf9\:79f0\:5316\:67d0\:4e00\:9879*)
STSymmetrizeTerm[term_, perList_List] := Module[
{
	perRule,
	outputTerms
},
	(*\:7f6e\:6362\:89c4\:5219*)
	perRule = Table[perList[[i]]->#[[i]], {i,Length[perList]} ]& /@ Permutations[perList];
	
	(*\:7f6e\:6362\:7ed3\:679c\:5404\:9879\:7ec4\:6210\:7684\:5217\:8868,\:7528Plus\:76f8\:52a0*)
	outputTerms = Array[
		Replace[term,
		{
			STensor[x_, subIndex__, supIndex__] :> STensor[x, subIndex/.perRule[[#]], supIndex/.perRule[[#]] ],
			Grad[x_, subIndex_] :> Grad[x, subIndex/.perRule[[#]]]
		}, All]&,
	Length[perRule], 1, Plus];
	
	1 / Length[perRule] * outputTerms
]


(* ::Subsection::Closed:: *)
(*\:53cd\:79f0\:5316 Antisymmetrize*)


STAntiSymmetrize::WrongInput = "\:8f93\:5165\:683c\:5f0f\:9519\:8bef.";
STAntiSymmetrize[expr__, perList_List]  := Module[
{
	exp = InputExplain[expr],
	terms
},
	terms = Flatten[exp //.{Plus[x_,y_] :> {x,y}}];
	If[
		(*\:4ec5\:6709\:4e00\:9879*)
		Head[terms] =!= List,
		STAntiSymmetrizeTerm[terms, perList],
		Plus @@ (STAntiSymmetrizeTerm[#, perList]& /@ terms)
	]
]


STAntiSymmetrize[T_STensor, perList_List] := Module[
{
	sublen = Length @ T[[2]],
	suplen = Length @ T[[3]],
	indices = Join[T[[2]], T[[3]]],
	perRule,
	resultIndices,
	originSign,
	perSign
},
	perRule = Table[perList[[i]]->#[[i]], {i, Length[perList]} ]& /@ Permutations[perList];
	
	(*\:539f\:6307\:6807\:6392\:5217\:7684\:7f6e\:6362\:7b26\:53f7*)
	originSign = Signature[indices];
	
	(*\:6307\:6807\:7f6e\:6362\:7ed3\:679c\:5217\:8868*)
	resultIndices = Table[indices/.perRule[[i]],{i, Length[perRule]}];
	
	(*\:7f6e\:6362\:7ed3\:679c\:6bcf\:4e00\:9879\:7684\:7b26\:53f7*)
	perSign = originSign * Signature /@ resultIndices;
	
	1/Length[resultIndices] * Array[perSign[[#]] * STensor[T[[1]], resultIndices[[#, 1;;sublen]], resultIndices[[#, sublen+1;;sublen+suplen]]]&, Length[resultIndices], 1, Plus]
]


(*\:53cd\:79f0\:5316\:67d0\:4e00\:9879*)(*\:9700\:8981\:5904\:7406\:7b26\:53f7\:95ee\:9898*)
STAntiSymmetrizeTerm[term__, perList_List] := Module[
{
	perRule,
	outputTerms,
	originSign,
	perSign
},
	(*\:7f6e\:6362\:89c4\:5219*)
	perRule = Table[perList[[i]]->#[[i]], {i, Length[perList]} ]& /@ Permutations[perList];

	(*\:539f\:6307\:6807\:6392\:5217\:7684\:7f6e\:6362\:7b26\:53f7*)
	originSign = Signature[perList];

	(*\:7f6e\:6362\:7ed3\:679c\:6bcf\:4e00\:9879\:7684\:7b26\:53f7*)
	perSign = originSign * Signature /@ Table[perList/.perRule[[i]],{i, Length[perRule]}];

	(*\:7f6e\:6362\:7ed3\:679c\:5404\:9879\:7ec4\:6210\:7684\:5217\:8868,\:7528Plus\:76f8\:52a0*)
	outputTerms = Array[
		perSign[[#]] * Replace[term,
		{
			STensor[x_, subIndex__, supIndex__] :> STensor[x, subIndex/.perRule[[#]], supIndex/.perRule[[#]] ],
			Grad[x_, subIndex_] :> Grad[x, subIndex/.perRule[[#]]]
		}, All]&,
	Length[perRule], 1, Plus];
	
	1 / Length[perRule] * outputTerms
]


(* ::Subsection::Closed:: *)
(*\:5347\:964d\:6307\:6807\:51fd\:6570 Function of Up and Down Index*)


(*\:5c06\:6240\:6709\:4e0a\:6307\:6807\:7528\:5ea6\:89c4\:964d\:4e0b\:6765\:ff0c\:8fd4\:56de(0,n)\:578b\:540c\:540d\:5f20\:91cf\:8868\:8fbe\:5f0f*)
IndicesDown[T_STensor] := Module[
{
	alphabet = ToExpression @ Alphabet[],
	Tsub = T[[2]],
	Tsup = T[[3]],
	usedIndices = DeleteDuplicates @ Join[T[[2]], T[[3]]],
	usableIndices,
	metriclis
},
	usableIndices = Complement[alphabet, usedIndices];
	metriclis = Table[ STensor[MetricSymbol, {#1, #2}, {}]&[Tsup[[i]], usableIndices[[i]]], {i, Length[Tsup]}];
	Times @@ Prepend[metriclis, STensor[T[[1]], Tsub, usableIndices[[1;;Length[Tsup]]]]]
]


(* ::Subsection::Closed:: *)
(*\:5ea6\:89c4\:8fd0\:7b97\:5f8b Rule of Metric Calculation*)


(*\:5ea6\:89c4\:8fd0\:7b97\:5f8b*)
MetricInverseRule := {
	(*g\:4e0e\:5176\:9006\:6620\:5c04\:7f29\:5e76\:5f97\:5230\[Delta]*)
	STensor[g_, {a_,b_} ,{}] STensor[g_, {}, {a_,c_}] :> STensor[\[Delta], {a}, {c}] /; g == MetricSymbol
};
MetricDownIndex := {
	(*\:5ea6\:89c4\:964d\:6307\:6807*)
	STensor[T_, Tsub__, Tsup__]STensor[g_,{a_,b_} ,Null|{}] :> STensor[T, Append[Tsub,b], DeleteElements[Tsup,{a}]] /; g == MetricSymbol && MemberQ[Tsup,a]
};
MetricUpIndex :={
	(*\:5ea6\:89c4\:5347\:6307\:6807*)
	STensor[T_, Tsub__, Tsup__] STensor[g_, Null|{}, {a_,b_}] :> STensor[T, DeleteElements[Tsup,{a}], Append[Tsub,b]] /; g == MetricSymbol && MemberQ[Tsub,a]
};


(* ::Subsection::Closed:: *)
(*\:5f20\:91cf\:8fd0\:7b97\:5f8b Rule of Tensor Calculation*)


(*\:5f20\:91cf\:8fd0\:7b97\:5f8b*)
STensorCalcRule:={
	(*\:81ea\:5e26\:52a0\:6cd5\:548c\:4e58\:6cd5\:7684\:4ea4\:6362\:5f8b\:3001\:7ed3\:5408\:5f8b*)

	(*\:5206\:914d\:5f8b*)
	T_STensor*(P_STensor+Q_STensor):>T*P+T*Q,
	T_STensor*((\[Alpha]_?NumberQ|_Symbol)*P_STensor+ Q_STensor):>\[Alpha]*T*P+T*Q,
	T_STensor*((\[Alpha]_?NumberQ|_Symbol)*P_STensor+(\[Beta]_?NumberQ|_Symbol)*Q_STensor):>\[Alpha]*T*P+\[Beta]*T*Q
};


(* ::Subsection::Closed:: *)
(*\:5bfc\:6570\:7b97\:7b26\:8fd0\:7b97\:5f8b Rule of Differential Operator Calculation*)


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


(* ::Subsection::Closed:: *)
(*\:62bd\:8c61\:6307\:6807\:8868\:8fbe\:5f0f\:5316\:7b80 Simplify the Abstract Expression*)


STCalculate[expr__]:= Module[
{
	CalcRule = Join[DerivativeCalcRule, STensorCalcRule, MetricInverseRule]
},
	InputExplain[expr]//.CalcRule
];


(* ::Section::Closed:: *)
(*\:5177\:4f53\:6307\:6807\:8f6c\:5316\:4e0e\:8fd0\:7b97 Abstract Indices Expression's Transformation and Calculation*)


(* ::Subsection::Closed:: *)
(*\:83b7\:53d6\:672a\:4f7f\:7528\:7684\:6307\:6807\:7b26\:53f7 Get Unused Indices*)


(*\:83b7\:53d6\:4e00\:4e2a\:6ca1\:6709\:7528\:8fc7\:7684\:6307\:6807\:7b26\:53f7*)
GetanUnusedIndex[expr__] := Module[
{
	usedIndices,
	unUsedIndices,
	gradIndices,
	alphabet = ToExpression@Alphabet[]
},
	usedIndices = DeleteDuplicates@Flatten@Cases[expr, STensor[T_, subIndex_List, superIndex_List] :> Join[superIndex,subIndex], All];
	gradIndices = DeleteDuplicates@Flatten@Cases[expr, Grad[any__, subIndex_] :> subIndex, All];
	unUsedIndices = Complement[alphabet, usedIndices, gradIndices];
	First @ unUsedIndices
];

GetanUnusedIndex[lis__List] := Module[
{
	usedIndices = Flatten @ Join[lis],
	alphabet = ToExpression @ Alphabet[]
},
	First @ Complement[alphabet, usedIndices]
]


(* ::Subsection::Closed:: *)
(*\:57fa\:672c\:8fd0\:7b97 Fundamental Operations*)


(* ::Text:: *)
(*(*\:5c06\:8868\:8fbe\:5f0f\:8f6c\:5316\:4e3a\:5177\:4f53\:6307\:6807\:8fdb\:884c\:8ba1\:7b97*)*)
(*(*\:5c06\:6240\:6709STensor\:8f6c\:5316\:4e3aATensor (Anonymous Tensor) \:533f\:540d\:5f20\:91cf\:8fdb\:884c\:8ba1\:7b97*)*)
(*(*\:7531\:4e8e\:5177\:4f53\:6307\:6807\:8ba1\:7b97\:65f6\:ff0c\:5f20\:91cf\:7684\:540d\:79f0\:4e0d\:518d\:91cd\:8981\:ff0c\:800c\:5176\:6307\:6807\:548c\:5206\:91cf\:624d\:662f\:91cd\:8981\:7684\:ff0c\:56e0\:6b64\:8f6c\:5316\:4e3aATensor[components_List, subIndex_List, superIndex_List]\:7684\:5f62\:5f0f*)*)
(*(*\:8fd9\:6837\:80fd\:591f\:8ba9\:52a0\:6cd5\:3001\:6570\:4e58\:548c\:5f20\:91cf\:79ef\:3001\:7f29\:5e76\:7b49\:8fd0\:7b97\:90fd\:6709\:76f8\:540c\:7684\:8f93\:5165\:548c\:8f93\:51fa\:ff1af[ATesnor]->ATensor*)*)
(*(*\:9700\:8981\:5c06\:534f\:53d8\:5bfc\:6570\:7b97\:7b26\:8f6c\:5316\:4e3a\:5f53\:524d\:5750\:6807\:7cfb\:4e0b\:7684 \:666e\:901a\:5bfc\:6570\:7b97\:7b26\:4f5c\:7528"\:52a0"\:514b\:6c0f\:7b26\:7684\:4f5c\:7528*)*)


(* ::Subsubsection::Closed:: *)
(*\:52a0\:6cd5 Add*)


(*\:8bbe\:7f6e\:52a0\:6cd5\:7684\:4ea4\:6362\:5f8b\:548c\:7ed3\:5408\:5f8b*)
(*SetAttributes[ATensorAdd, {Flat, Orderless}]*)

ATensorAdd::Error = "\:5f20\:91cf\:4e0d\:540c\:578b\:6216\:6307\:6807\:4e0d\:5e73\:8861.";

ATensorAdd[T_ATensor, S_ATensor] := Module[
{
	sortTsub = Sort[T[[1]]],
	sortTsup = Sort[T[[2]]],
	sortSsub = Sort[S[[1]]],
	sortSsup = Sort[S[[2]]],
	indexT = Join[T[[1]], T[[2]]],
	indexS = Join[S[[1]], S[[2]]],
	p
},
	(*\:4e24\:4e2a\:5f20\:91cf\:76f8\:52a0\:8981\:6c42\:662f\:540c\:578b\:5f20\:91cf\:4e14\:6307\:6807\:76f8\:540c*)
	If[
		sortTsub =!= sortSsub || sortTsup =!= sortSsup || Dimensions[T[[3]]] != Dimensions[S[[3]]],
		Message[ATensorAdd::Error];
		Abort[]
	];
	(*\:6307\:6807\:4e4b\:95f4\:6700\:591a\:5dee\:4e00\:4e2a\:7f6e\:6362*)
	(*\:627e\:5230\:6307\:6807\:7f6e\:6362*)
	p = Permute[Range[Length[indexT]], FindPermutation[indexS, indexT]];
	ATensor[T[[1]], T[[2]], If[Head[S[[3]]] === List, T[[3]]+Transpose[S[[3]], p], T[[3]] + S[[3]]]]
];

ATensorAdd[T_ATensor] := T;

ATensorAdd[T_ATensor, P_ATensor, Q__ATensor] := ATensorAdd[T, ATensorAdd[P, Q]];


(* ::Subsubsection::Closed:: *)
(*\:6570\:4e58 Scalar Multiplication*)


(* ::Code::Initialization::"Tags"-><|"UppercasePattern" -> <|Enabled -> False|>|>:: *)
(*SetAttributes[ATensorTimes, Orderless]*)
(*\:6570\:4e58*)(*\:6570\:4e58\:6ee1\:8db3\:4ea4\:6362\:5f8b*)
ATensorTimes[k_Symbol|k_?NumberQ, T_ATensor] := ATensor[T[[1]], T[[2]], k T[[3]]];

ATensorTimes[T_ATensor, k_Symbol|k_?NumberQ] := ATensor[T[[1]], T[[2]], k T[[3]]];

ATensorTimes[k_Symbol|k_?NumberQ, T_ATensor, S__ATensor] := ATensorTimes[k, ATensorTimes[T, S]];


(* ::Subsubsection::Closed:: *)
(*\:5f20\:91cf\:79ef\:4e0e\:7f29\:5e76 Tensor Product and Contract*)


(*\:5f20\:91cf\:79ef\:4e0e\:7f29\:5e76*)
ATensorTimes[T_ATensor, S_ATensor] := Module[
{
		commonIndex = Join[Intersection[T[[1]], T[[2]]], Intersection[T[[1]], S[[2]]], Intersection[S[[1]], T[[2]]], Intersection[S[[1]], S[[2]]]], 
		sub = Join[T[[1]], S[[1]]],
		sup = Join[T[[2]], S[[2]]],
		outputIndex,
		IndicesPos,
		contractIndex,
		product,
		outputComponents,
		unArrangedIndex,
		TargetIndex,
		p
},
	(*\:7ed3\:679c\:5f20\:91cf\:7684\:6307\:6807*)
	outputIndex = {Select[sub, !MemberQ[sup, #]&], Select[sup, !MemberQ[sub, #]&]};
	(*\:8ba1\:7b97\:65f6\:5bf9\:6307\:6807\:8fdb\:884c\:7f16\:53f7,\:5f62\:5f0f\:4e3a*)
	(*\:5bf9T\:7684\:4e0b\:6307\:6807\:7f16\:53f7\:4e3a1, 2, 3...a\:ff0c\:7136\:540e\:5bf9\:4e0a\:6307\:6807\:7f16\:53f7\:4e3aa+1, a+2...a+b*)
	(*\:5bf9S\:7684\:4e0b\:6307\:6807\:7f16\:53f7\:4e3aa+b+1, a+b+2 ... a+b+c\:ff0c\:4e0a\:6307\:6807\:7f16\:53f7\:4e3aa+b+c+1, ... a+b+c+d*)
	(*\:5148\:5c06\:6307\:6807\:6309\:987a\:5e8f\:6392\:597d*)(*\:8bb0\:5f55\:6bcf\:4e2a\:6307\:6807\:7684\:7f16\:53f7*)
	IndicesPos = PositionIndex[Join[T[[1]], T[[2]], S[[1]], S[[2]]]];
	(*\:5148\:6c42\:51fa\:5f20\:91cf\:79ef*)
	product = TensorProduct[T[[3]], S[[3]]];
	(*\:5c06\:9700\:8981\:7f29\:5e76\:7684\:6307\:6807\:8f6c\:5316\:4e3a\:4f4d\:7f6e\:5217\:8868*)
	contractIndex = IndicesPos[#]& /@ commonIndex;
	(*\:5bf9\:6307\:6807\:8fdb\:884c\:7f29\:5e76*)
	outputComponents = Simplify @ TensorContract[product, contractIndex];
	(*\:8fd8\:9700\:8981\:5c06\:4e0b\:6307\:6807\:653e\:5230\:524d\:9762\:ff0c\:4e0a\:6307\:6807\:653e\:5230\:540e\:9762\:ff0c\:5bf9\:5206\:91cf\:8fdb\:884c\:4f4d\:7f6e\:8c03\:6574*)
	(*unArrangedIndex\:662foutputComponents\:5bf9\:5e94\:7684\:6307\:6807\:987a\:5e8f*)
	unArrangedIndex = Keys[DeleteCases[IndicesPos, x_List /; Length[x] > 1]];
	(*\:8f93\:51fa\:5f20\:91cf\:7684\:6307\:6807\:987a\:5e8f*)
	TargetIndex = Flatten[outputIndex];
	(*\:627e\:5230\:4ece\:5f53\:524d\:6307\:6807\:96c6\:5411\:7ed3\:679c\:6307\:6807\:96c6\:8f6c\:5316\:7684\:7f6e\:6362\:ff0c\:5e76\:5c06\:5176\:4f5c\:7528\:4e8e{1,2,3...}\:521d\:59cb\:5217\:8868\:ff0c\:5f97\:5230\:8f6c\:7f6e\:5173\:7cfb\:5bf9\:5e94\:5217\:8868*)
	p = Permute[Range[Length[TargetIndex]], FindPermutation[unArrangedIndex, TargetIndex]];
	
	ATensor[outputIndex[[1]], outputIndex[[2]], If[Head[outputComponents] === List, Transpose[outputComponents, p], outputComponents]]
];

(*\:5355\:4e2a\:5f20\:91cf\:7684\:81ea\:8eab\:6307\:6807\:7f29\:5e76*)
ATensorTimes[T_ATensor] := Module[
{
	commonIndex = Intersection[T[[1]], T[[2]]],
	contractIndex,
	sub = T[[1]],
	sup = T[[2]],
	outputIndex,
	outputComponents,
	IndicesPos
},
	(*\:7ed3\:679c\:7684\:6307\:6807*)
	outputIndex = {Select[sub, !MemberQ[sup, #]&], Select[sup, !MemberQ[sub, #]&]};
	
	IndicesPos = PositionIndex[Join[T[[1]], T[[2]]]];
	
	(*\:9700\:8981\:7f29\:5e76\:7684\:6307\:6807\:7684\:4f4d\:7f6e*)
	contractIndex = IndicesPos[#]& /@ commonIndex;
	
	outputComponents = Simplify @ TensorContract[T[[3]], contractIndex];
	
	ATensor[outputIndex[[1]], outputIndex[[2]], outputComponents]
];
ATensorTimes[T_ATensor, P_ATensor, Q__ATensor] := ATensorTimes[T, ATensorTimes[P, Q]];



(* ::Subsubsection::Closed:: *)
(*\:6954\:79ef Tensor Wedge Product*)


ATensorWedge::wrongTensor = "\:53c2\:4e0e\:8fd0\:7b97\:7684\:5f20\:91cf\:4e0d\:5168\:662f(0,n)\:578b\:5f20\:91cf";

(*\:82e5\:53c2\:4e0e\:8fd0\:7b97\:7684(0,n)\:578b\:5f20\:91cf\:4e0d\:662f\:53cd\:79f0\:7684\:ff0c\:5219\:4f1a\:5148\:8fdb\:884c\:53cd\:79f0\:5316*)

ATensorWedge[T_ATensor] := Module[
{
	subIndex = T[[1]],
	supIndex = T[[2]]
},
	If[
		T[[2]] =!= {},
		Message[ATensorWedge::wrongTensor];
		Abort[]
	];
	ATensor[subIndex, supIndex, Normal @ TensorWedge[T[[3]]]]
];

ATensorWedge[T_ATensor, S_ATensor] := Module[
{
	outIndices = Join[T[[1]], S[[1]]]
},
	Scan[
		If[
			#[[2]] =!= {},
			Message[ATensorWedge::wrongTensor];
			Abort[]
		]&,
		{T, S}
	];
	
	ATensor[outIndices, {}, Normal @ TensorWedge[T[[3]], S[[3]]]]
];

ATensorWedge[T_ATensor, S_ATensor, Q__ATensor] := ATensorWedge[T, ATensorWedge[S, Q]];


(* ::Subsubsection::Closed:: *)
(*\:5bfc\:6570\:7b97\:7b26\:8fd0\:7b97 Derivative Operator Calculation*)


(*\:534f\:53d8\:5bfc\:6570\[Del]*)
SCovariantDerivative::metricMiss ="\:672a\:8bbe\:7f6e\:5ea6\:89c4\:5206\:91cf.";
SCovariantDerivative[T_ATensor, dIndex_Symbol, coodinates_List] := Module[
{
	\[CapitalGamma],
	subIndex = T[[1]],(*\:4e0b\:6807*)
	supIndex = T[[2]],(*\:4e0a\:6807*)
	tempSub = T[[1]],(*\:4e34\:65f6\:4e0b\:6807*)
	tempSup = T[[2]],(*\:4e34\:65f6\:4e0a\:6807*)
	Christoffel,
	tempIndex = GetanUnusedIndex[T[[1]], T[[2]], {dIndex}],
	ordinaryPart,
	upContractPart,
	downContractPart
},
	(*\:68c0\:67e5\:662f\:5426\:8bbe\:7f6e\:4e86\:5ea6\:89c4\:5206\:91cf*)
	If[
		Length[MetricComponents] == 0,
		Message[SCovariantDerivative::metricMiss];
		Abort[]
	];
	(*\:8ba1\:7b97\:5f53\:524d\:5750\:6807\:7cfb\:4e0b\:7684\:5ea6\:89c4\:5bf9\:5e94\:7684\:514b\:6c0f\:7b26\:5206\:91cf*)
	\[CapitalGamma] = SCalcChristoffel[MetricComponents, coodinates];

	(*\:5148\:6c42\:51fa\:666e\:901a\:5bfc\:6570\:7b97\:7b26\:90e8\:5206*)
	ordinaryPart = SOrdinaryDerivative[T, dIndex, coodinates];
	
	(*\:5982\:679c\:514b\:6c0f\:7b26\:5168\:4e3a0\:5219\:65e0\:9700\:518d\:8ba1\:7b97\:4e0b\:53bb\:ff0c\:5426\:5219\:9700\:8981\:7ee7\:7eed\:8ba1\:7b97\:53e6\:5916\:4e24\:90e8\:5206*)
	If[
		!TrueQ[Thread[\[CapitalGamma] == Array[0&,Dimensions[\[CapitalGamma]]]]],
		(*\:7b97\:51fa\:548c\:4e0a\:6307\:6807\:7f29\:5e76\:7684\:90e8\:5206*)
		If[supIndex != {},
			upContractPart = ATensorAdd @@ Table[
				tempSup = T[[2]];
				tempSup[[i]] = tempIndex;
				ATensorTimes[ATensor[{dIndex, tempIndex}, {supIndex[[i]]}, \[CapitalGamma]], ATensor[subIndex, tempSup, T[[3]]]],
				{i, Length[supIndex]}
				],
			upContractPart = {}
		];
		
		(*\:7b97\:51fa\:548c\:4e0b\:6307\:6807\:7f29\:5e76\:7684\:90e8\:5206*)
		If[subIndex != {},
			downContractPart = ATensorAdd @@ Table[
				tempSub = T[[1]];
				tempSub[[i]] = tempIndex;
				ATensorTimes[ATensor[{dIndex, subIndex[[i]]}, {tempIndex}, \[CapitalGamma]], ATensor[tempSub, supIndex, T[[3]]]],
				{i, Length[subIndex]}
			];
			(*\:5bf9\:4e0b\:6807\:6c42\:548c\:7684\:7ed3\:679c\:90e8\:5206\:524d\:9762\:4e3a\:8d1f\:53f7*)
			downContractPart = ATensorTimes[-1, downContractPart],
			downContractPart = {}
		];
		
		(*\:5c06\:5404\:90e8\:5206\:52a0\:8d77\:6765*)
		If[downContractPart != {}, ordinaryPart = ATensorAdd[ordinaryPart, downContractPart]];
		If[upContractPart != {}, ordinaryPart = ATensorAdd[ordinaryPart, upContractPart]];
		
	];
	
	ordinaryPart
];


(*\:666e\:901a\:5bfc\:6570\:7b97\:7b26*)
SOrdinaryDerivative[T_ATensor, dIndex_Symbol, coodinates_List] := Module[
{
	components = T[[3]],
	dimension = Length[coodinates],
	indexPos,
	outputComponents
},
	
	outputComponents = Array[D[components, coodinates[[#]]]&, dimension];
	
	If[
		(*\:5224\:65ad\:662f\:5426\:9700\:8981\:7f29\:5e76*)
		MemberQ[T[[2]], dIndex],
		
		outputComponents = TensorContract[outputComponents, Position[Join[{dIndex}, T[[1]], T[[2]]], dIndex]];
		ATensor[T[[1]], DeleteCases[T[[2]], dIndex], outputComponents],
		
		ATensor[Prepend[T[[1]], dIndex], T[[2]], outputComponents]
	]
	
	(*\:4e5f\:53ef\:4ee5\:5229\:7528ATensorTimes\:8ba1\:7b97\:662f\:5426\:9700\:8981\:4e0a\:4e0b\:6307\:6807\:7f29\:5e76*)
	(*ATensorTimes@ATensor[Prepend[T[[1]], dIndex], T[[2]], Array[D[components, coodinates[[#]]]&, dimension]]*)
]


(* ::Subsection::Closed:: *)
(*\:62bd\:8c61\:6307\:6807\:8868\:8fbe\:5f0f\:8f6c\:5316\:4e3a\:5177\:4f53\:6307\:6807\:8868\:8fbe\:5f0f Transform Abstract Expressions to Specific Expressions*)


(*\:76f4\:63a5\:8ba1\:7b97\:8868\:8fbe\:5f0f\:7684\:5206\:91cf*)
STSpecify[expr__] := Module[{},
	SCalcSpecificExpression @ InputExplain @ expr
]


(*\:5148\:5c06\:62bd\:8c61\:6307\:6807\:8868\:8fbe\:5f0f\:8fdb\:884c\:4e00\:5b9a\:7684\:7b80\:5316*)
STSimSpecify[expr__] := Module[
{
	t
},
	(*\:5148\:8fdb\:884c\:7b80\:5316*)
	t = STCalculate[expr];
	(*\:5982\:679c\:8fd4\:56de\:4e860,\:5373\:96f6\:5411\:91cf*)
	If[
		t === 0,
		Row[{"Result is a None Tensor"}],
		SCalcSpecificExpression[t]
	]
]


SCalcSpecificExpression::componentsMiss = "\!\(\*SuperscriptBox[SubscriptBox[\(\:5f20\:91cf`1`\), \(`2`\)], \(`3`\)]\)\:672a\:8bbe\:7f6e\:5206\:91cf.";
SCalcSpecificExpression::coodinatesMiss = "\:672a\:9009\:53d6\:5750\:6807\:7cfb.";

SCalcSpecificExpression[expr__] := Module[
{
	expression,
	coodinates,
	calcReplaceRule,
	sTensors = Cases[expr, _STensor, {0, Infinity}],
	setTest,
	keys = Association[],
	output,
	components
},
	(*\:68c0\:67e5\:662f\:5426\:9009\:53d6\:4e86\:5750\:6807\:7cfb*)
	If[
		Length[CurrentCoordinates] == 0,
		Message[SCalcSpecificExpression::coodinatesMiss];
		Abort[]
	];
	coodinates = CurrentCoordinates;
	
	calcReplaceRule = {
		Plus[T_ATensor, S__ATensor] :> ATensorAdd[T, S],
		Times[k_Symbol|k_?NumericQ, T_ATensor] :> ATensorTimes[k, T],
		Times[T_ATensor, S__ATensor] :> ATensorTimes[T, S],
		Grad[T_ATensor, dIndex_Symbol] :> SCovariantDerivative[T, dIndex, coodinates],
		Wedge[T_ATensor, S__ATensor] :> ATensorWedge[T, S],
		T_ATensor :> ATensorTimes[T] /; IntersectingQ[T[[1]], T[[2]]]
	};
	
	(*\:68c0\:67e5\:5f20\:91cfT\:662f\:5426\:5df2\:7ecf\:8bbe\:7f6e\:5206\:91cf\:7684\:51fd\:6570*)
	setTest[T_STensor] := Module[
	{
		tkeys,
		tkey,
		tas
	},
			(*\:627e\:5230\:7b26\:53f7\:548c\:578b\:53f7\:90fd\:76f8\:540c\:7684\:5f20\:91cf*)
			tas = KeySelect[TensorComponents, MatchQ[#, STensor[T[[1]], ___, ___]] && Length[#[[2]]] == Length[T[[2]]] && Length[#[[3]]] == Length[T[[3]]]& ];
			(*\:4e0d\:5b58\:5728\:952e\:503c\:ff0c\:8be5\:5f20\:91cf\:672a\:8bbe\:7f6e\:5206\:91cf;\:6216\:5b58\:5728\:591a\:4e2a\:5339\:914d\:7684\:952e\:503c*)
			If[
				Length[tas] != 1,
				Message[SCalcSpecificExpression::componentsMiss, T[[1]], StringJoin[ToString/@T[[2]]], StringJoin[ToString/@T[[3]]]];
				Return[False]
			];
			(*\:6b64\:5904\:4fdd\:8bc1tas\:53ea\:6709\:4e00\:4e2a\:952e\:503c\:5bf9*)
			tkey = <|T -> First@tas|>;
			AppendTo[keys, tkey];
			Return[True]
		];
		
	(*\:68c0\:67e5\:662f\:5426\:6240\:6709\:53c2\:4e0e\:8fd0\:7b97\:7684\:5f20\:91cf\:90fd\:5df2\:7ecf\:8bbe\:7f6e\:5206\:91cf*)
	If[
		!AllTrue[sTensors, setTest[#]&],
		Abort[]
	];
	
	(*\:5c06\:6240\:6709STensor\:66ff\:6362\:4e3aATensor*)
	expression = expr/.{T_STensor :> ATensor[T[[2]], T[[3]], keys[T]]};
	
	(*\:4f7f\:7528\:66ff\:6362\:8ba1\:7b97\:8868\:8fbe\:5f0f*)
	expression = expression//.calcReplaceRule;
	
	If[
		NumberQ[expression],
		ShowSTensor[STensor[Global`\[ScriptCapitalT], {}, {}], 0],
		(*\:7ed3\:679c\:7684expression\:5e94\:8be5\:4e3a\:4e00\:4e2aATensor*)
		components = Last[expression];(*\:8bb0\:5f55\:5206\:91cf*)
		(*\:5c06ATensor\:66ff\:6362\:56deSTensor*)
		output = expression//.{T_ATensor :> STensor[Global`\[ScriptCapitalT], T[[1]]/.specificReplaceRule, T[[2]]/.specificReplaceRule]};
		ShowSTensor[output, components]
	]
];


(* ::Section::Closed:: *)
(*\:5e38\:7528\:5f20\:91cf\:8ba1\:7b97 Common Tensor Calculation *)


(* ::Subsection::Closed:: *)
(*\:4e3b\:51fd\:6570 Main Function*)


STCalcTensor::NoMetric = "\:672a\:8bbe\:7f6e\:5ea6\:89c4\:5206\:91cf."
STCalcTensor::NoCoordinates = "\:672a\:8bbe\:7f6e\:5750\:6807\:7cfb."
STCalcTensor::CanNotCalc = "\:65e0\:6cd5\:8ba1\:7b97\:8be5\:5f20\:91cf\:ff0c\:56e0\:4e3a\:5176\:8d85\:51fa\:53ef\:9009\:8303\:56f4."
STCalcTensor::DimensionError = "\:5206\:91cf\:77e9\:9635\:7684\:7ef4\:6570\:4e0e\:5750\:6807\:7cfb\:7ef4\:6570\:4e0d\:5339\:914d."
STCalcTensor[name_String] := STCalcTensor[name, MetricComponents, CurrentCoordinates];

STCalcTensor[name_String, components_?ArrayQ] := STCalcTensor[name, components, CurrentCoordinates];

STCalcTensor[name_String, components_?ArrayQ, coodinates_List] := Module[
{
	func,
	record = <|
		"Christoffel" -> STensor["\[CapitalGamma]", {"\[Mu]","\[Nu]"}, {"\[Sigma]"}],
		"RiemannTensor" -> STensor["R", {"\[Mu]","\[Nu]","\[Sigma]"},{"\[Rho]"}],
		"RicciTensor" -> STensor["R", {"\[Mu]","\[Nu]"},{}],
		"RicciScalar" -> STensor["R", {}, {}],
		"EinsteinTensor" -> STensor["G", {"\[Mu]", "\[Nu]"}, {}]
	|>
},
	If[
		Length @ components == 0,
		Message[STCalcTensor::NoMetric];
		Abort[]
	];
	If[
		Length @ coodinates == 0,
		Message[STCalcTensor::NoCoordinates];
		Abort[];
	];
	If[
		!KeyExistsQ[record, name],
		Message[STCalcTensor::CanNotCalc];
		Abort[];
	];
	If[
		First @ Dimensions @ components != Length @ coodinates,
		Message[STCalcTensor::DimensionError];
		Abort[];
	];
	func = ToExpression @ StringJoin[{"SCalc",name}];
	
	ShowSTensor[record[name], func[components, coodinates]]
]


(* ::Subsection::Closed:: *)
(*\:514b\:6c0f\:7b26 Christoffel Symbol*)


(*\:514b\:6c0f\:7b26*)
SCalcChristoffel[g_?ArrayQ, coodinateSystem_List] := Module[
	{
		invg = Inverse[g],(*inverse of metric g*)
		dimension = Length @ coodinateSystem,(*dimension of space*)
		\[Gamma],(*Christoffel Symbol Component Calculating Function*)
		\[CapitalGamma](*Christoffel Symbol Component Matrix*)
	},
	If[
		First @ Dimensions @ g != Length @ coodinateSystem,
		Message[STCalcTensor::DimensionError];
		Abort[]
	];
	
	\[Gamma][\[Mu]_,\[Nu]_,\[Sigma]_]:=1/2 Sum[ invg[[\[Sigma],\[Rho]]] (D[ g[[\[Rho],\[Mu]]], coodinateSystem[[\[Nu]]] ]+D[ g[[\[Rho],\[Nu]]], coodinateSystem[[\[Mu]]] ]-D[ g[[\[Mu],\[Nu]]], coodinateSystem[[\[Rho]]] ]),{\[Rho],dimension}];(*Subscript[\:514b\:6c0f\:7b26\:5206\:91cf\[CapitalGamma], \[Mu]\[Nu]]^\[Sigma]*)
	
	(*\:5229\:7528\:4e0b\:6307\:6807\:7684\:5bf9\:79f0\:6027\:6765\:964d\:4f4e\:8ba1\:7b97\:590d\:6742\:5ea6*)
	\[CapitalGamma]=Simplify@Array[If[ #1<=#2, \[Gamma][#1,#2,#3], Null]&, {dimension,dimension,dimension}];(*\:514b\:6c0f\:7b26\:5206\:91cf\:77e9\:9635*)
	
	Array[If[#1>#2, \[CapitalGamma][[#1,#2,#3]]=\[CapitalGamma][[#2,#1,#3]]]&, {dimension,dimension,dimension}];
	
	(*Print[Subsuperscript["\[CapitalGamma]","\[Mu]\[Nu]","  \[Sigma]"]->MatrixForm[\[CapitalGamma]]]*);
	\[CapitalGamma]
];


(* ::Subsection::Closed:: *)
(*\:9ece\:66fc\:5f20\:91cf Riemann Tensor*)


(*\:9ece\:66fc\:5f20\:91cf*)
SCalcRiemannTensor[g_?ArrayQ, coodinateSystem_List] := Module[
	{
		invg = Inverse[g],(*inverse of metric g*)
		dimension = Length @ coodinateSystem,(*dimension of space*)
		\[CapitalGamma],(*Christoffel Symbol Component Matrix*)
		r,(*Riemann Tensor Conponent Calculating Function*)
		Riemann(*Riemann Tensor Component Matrix*)
	},
	If[
		First @ Dimensions @ g != Length @ coodinateSystem,
		Message[STCalcTensor::DimensionError];
		Abort[]
	];
	(*\:7528\:5750\:6807\:6cd5\:8ba1\:7b97Riemann\:66f2\:7387\:5f20\:91cf*)
	
	(*\:5148\:8ba1\:7b97\:514b\:6c0f\:7b26*)
	\[CapitalGamma] = SCalcChristoffel[g, coodinateSystem];
	r[\[Mu]_,\[Nu]_,\[Sigma]_,\[Rho]_] := D[ \[CapitalGamma][[\[Mu],\[Sigma],\[Rho]]], coodinateSystem[[\[Nu]]]]-D[ \[CapitalGamma][[\[Sigma],\[Nu],\[Rho]]], coodinateSystem[[\[Mu]]]]+Sum[\[CapitalGamma][[\[Sigma],\[Mu],\[Lambda]]] \[CapitalGamma][[\[Nu],\[Lambda],\[Rho]]] - \[CapitalGamma][[\[Sigma],\[Nu],\[Lambda]]] \[CapitalGamma][[\[Mu],\[Lambda],\[Rho]]],{\[Lambda],dimension}];(*Subscript[\:9ece\:66fc\:5f20\:91cf\:7684\:5206\:91cfR, \[Mu]\[Nu]\[Sigma]]^\[Rho]*)
	
	(*\:5229\:7528\:7b2c1\:30012\:4e0b\:6307\:6807\:7684\:53cd\:79f0\:6027\:6765\:964d\:4f4e\:8ba1\:7b97\:590d\:6742\:5ea6*)
	Riemann = Simplify@Array[If[#1<=#2,r[#1,#2,#3,#4],Null]&,{dimension,dimension,dimension,dimension}];
	
	Array[If[#1>#2,Riemann[[#1,#2,#3,#4]] = -Riemann[[#2,#1,#3,#4]]]&,{dimension,dimension,dimension,dimension}];
	
	(*Print[Subsuperscript["R","\[Mu]\[Nu]\[Sigma]","   \[Rho]"]->MatrixForm@Riemann]*);
	
	Riemann
];



(* ::Subsection::Closed:: *)
(*\:91cc\:5947\:5f20\:91cf Ricci Tensor*)


(*\:91cc\:5947\:5f20\:91cf*)
SCalcRicciTensor[g_?ArrayQ, coodinateSystem_List]:=Module[
	{
		invg = Inverse[g],(*inverse of metric g*)
		dimension = Length @ coodinateSystem,(*dimension of space*)
		R,(*Riemann Tensor Component Matrix*)
		ricci,(*Ricci Tensor Component Calculating Function*)
		Ricci(*Ricci Tensor Component Matrix*)
	},
	If[
		First @ Dimensions @ g != Length @ coodinateSystem,
		Message[STCalcTensor::DimensionError];
		Abort[]
	];
	(*\:8ba1\:7b97Ricci\:5f20\:91cf*)
	
	(*\:5148\:8ba1\:7b97Riemann\:66f2\:7387\:5f20\:91cf*)
	R = SCalcRiemannTensor[g,coodinateSystem];
	ricci[\[Mu]_,\[Nu]_] := Sum[ R[[\[Mu],\[Sigma],\[Nu],\[Sigma]]], {\[Sigma],dimension}];
	
	(*\:5229\:7528\:5bf9\:79f0\:6027\:6765\:964d\:4f4e\:8ba1\:7b97\:590d\:6742\:5ea6*)
	Ricci = Simplify@Array[If[#1<=#2, ricci[#1,#2], Null]&, {dimension,dimension}];
	
	Array[If[#1>#2, Ricci[[#1,#2]] = Ricci[[#2,#1]]]&, {dimension,dimension}];
	
	(*Print[Subscript["R","\[Mu]\[Nu]"]->MatrixForm@Ricci]*);
	Ricci
];


(* ::Subsection::Closed:: *)
(*\:91cc\:5947\:6807\:91cf Ricci Scalar*)


(*\:91cc\:5947\:6807\:91cf*)
SCalcRicciScalar[g_?ArrayQ, coodinateSystem_List] := Module[
{
	dimension = Length @ coodinateSystem,
	Ricci
},
	If[
		First @ Dimensions @ g != Length @ coodinateSystem,
		Message[STCalcTensor::DimensionError];
		Abort[]
	];
	
	Ricci = SCalcRicciTensor[g, coodinateSystem];
	Sum[Ricci[[\[Mu],\[Nu]]] g[[\[Mu],\[Nu]]],{\[Mu], dimension},{\[Nu], dimension}]
];


(* ::Subsection::Closed:: *)
(*\:7231\:56e0\:65af\:5766\:5f20\:91cf Einstein Tensor*)


(*\:7231\:56e0\:65af\:5766\:5f20\:91cf*)
SCalcEinsteinTensor[g_?ArrayQ, coodinateSystem_List] := Module[
{
	invg = Inverse[g],
	dimension = Length @ coodinateSystem,
	RicciTensor,
	RicciScalar
},
	If[
		First @ Dimensions @ g != Length @ coodinateSystem,
		Message[STCalcTensor::DimensionError];
		Abort[]
	];
	
	(*\:8ba1\:7b97\:91cc\:5947\:5f20\:91cf*)
	RicciTensor = SCalcRicciTensor[g, coodinateSystem];
	
	(*\:8ba1\:7b97\:91cc\:5947\:6807\:91cf*)
	RicciScalar = Sum[RicciTensor[[\[Mu],\[Nu]]] g[[\[Mu],\[Nu]]],{\[Mu], dimension},{\[Nu], dimension}];
	
	RicciTensor - RicciScalar / 2 * g
]


(* ::Section::Closed:: *)
(*End*)


End[]


EndPackage[]
