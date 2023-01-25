(* ::Package:: *)

(* :Title:Sticks*)
(* :Context:Sticks`*)
(* :Summary: Sticks methods*)
(* :Package Version: 17,Maggio 2022*)
(* :Mathematica Version: 13.0*)


BeginPackage["Sticks`"];
showSticks::usage="showSticks[]";
showSumSticks::usage="showSumSticks[]";
generateFirstSticksEx::usage="generateFirstSticksEx[]";


Begin["`Private`"]


(*costanti dimensioni style*)
cSize1 = 20;
cSize2 = 35;
cSize3 = 50;


(*le seguenti strutture rappresentano i 10 regoli con colore e posizioni corrispondenti*)
stick1 = {White,{0,0},{1,1}};
stick2 = {Red,{2,0},{3,2}};
stick3 = {LightGreen,{4,0},{5,3}};
stick4 = {Pink,{6,0},{7,4}};
stick5 = {Yellow,{8,0},{9,5}};
stick6 = {Green,{10,0},{11,6}};
stick7 = {Black,{12,0},{13,7}};
stick8 = {Brown,{14,0},{15,8}};
stick9 = {Blue,{16,0},{17,9}};
stick10 = {Orange,{18,0},{19,10}};

sticksColorList = {White, Red,LightGreen, Pink, Yellow, Green, Black, Brown, Blue, Orange};
sticksList = {stick1,stick2,stick3,stick4,stick5,stick6,stick7,stick8,stick9,stick10};


imageSize = Full;
imageSecondSize = 1250;
(*a partire dalle strutture sopra vengono generati i rettangoli che identificheranno i regoli all'interno delle diapositive della presentazione*)
generateStick[x_]:= (
	rectWithColor = {Part[x,1],Rectangle[Part[x,2],Part[x,3]]};
	Return[rectWithColor];
);

(*la funzione riceve in input un numero \[EGrave] restituisce la lista contenente il numero relativo a ciascun regolo da ritornare:*)
getSticks[num_]:= (
	splittedInput = Reverse[IntegerDigits[num]];  (*lista contenente le cifre che compongono il numero in input nel seguente formato {unit\[AGrave],decine,centinaia}*)
	splittedInputLength = Length[splittedInput]; (*cardinalit\[AGrave] lista precedente*)
	sList = {}; (*lista contenente il numero relativo a ciascun regolo da ritornare, ad esempio la lista {1,2,10,10} rappresenta la divisione in regoli del numero 23 e indica che per comporlo sono necessari due regoli da 10, uno da 1 e uno da 2*)
	
	(*
	ciclo attraverso cui viene popolata la lista dei regoli:
	se index == 1 significa che stiamo aggiungendo un regolo relativo alle unit\[AGrave]
	se index == 2 significa che stiamo aggiungendo un regolo relativo alle decine 

	*)
	Table[(
		If[index == 1,(
			If[Part[splittedInput,index ] > 0,AppendTo[sList, Part[splittedInput,index]]];
	)];
		If[index == 2, Table[(
			AppendTo[sList, 10];
			),{index2,Part[splittedInput,index]}]];
		),{index,splittedInputLength}];
	Return[Reverse[sList]];
);


(*Attraverso questa funzionevengono generati i regoli mostrati nella diapositiva di spiegazione relativa ai regoli.*)
showSticks[]:= Module[
{},

sticks = Table[{EdgeForm[Thick],generateStick[Part[sticksList,i]]},{i,Length[sticksList]}]; (*lista di regoli *)

labelsList = {};
Table[(
coordsMin = Part[Part[sticksList,i],2];
coordsMax = Part[Part[sticksList,i],3];

coordX1 = Part[coordsMin,1];
coordX2= Part[coordsMax,1];
coordY1 = Part[coordsMin,2];
coordY2= Part[coordsMax,2];

AppendTo[labelsList,{White,Rectangle[coordsMin,{coordX2,1}],Text[Style[i,FontSize->Scaled[.03],Black,Bold],{(coordX2+coordX1)/2,0.5}]}];

),{i,Length[sticksList]}]; 

Pane[Framed[GraphicsColumn[{Graphics[sticks],Graphics[labelsList]},ImageSize->imageSecondSize],RoundingRadius->2, FrameMargins-> 30],imageSize,Alignment->Center]

]


(*Attraverso questa funzione vengono generati i regoli con le rispettive label che identificano i due numeri corrispondenti e la somma di essi .*)
showSumSticks[] := Module[
{},

(*Attraverso le seguenti righe di codice vengono generati i 2 numeri randomici attraverso i quali strutturare gli esempi nella slide di spiegazione relativa alla somma con i regoli*)
(*Si \[EGrave] scelto di selezionare il numero randomicamente nel range [1,19] per una migliore visualizzazione degli esempi*)
SeedRandom[];
stickNum1 = RandomInteger[{1,19}];
stickNum2 = RandomInteger[{1,19}];
stickSumNum = stickNum1 + stickNum2; 

(*stickCol1,stickCol2 e stickCol3 contengono le tre diverse istanze dei regoli che corrispondono rispettivamente ai due valori da sommare e alla loro somma oltre che a una label che ne indica il valore*)
stickCol1 = GraphicsColumn[{Style[stickNum1,cSize2,Bold], generateSumSticks[stickNum1,1]}];
stickCol2 = GraphicsColumn[{Style[stickNum2,cSize2,Bold], generateSumSticks[stickNum2,1]}];
stickCol3= GraphicsColumn[{Style[stickNum1 + stickNum2,cSize2,Bold], generateSumSticks[stickSumNum,1]}];

stickSum = GraphicsRow[{stickCol1, Style["+" ,cSize3,Bold], stickCol2, Style["=",cSize3,Bold], stickCol3}];

paneSticks = Pane[Framed[GraphicsRow[{generateSumSticks[stickNum1,2],generateSumSticks[stickNum2,2]},ImageSize->{imageSecondSize}],RoundingRadius->2, FrameMargins-> 30],Full,Alignment->Center];
paneStickSum = Pane[Framed[GraphicsRow[{stickSum},ImageSize->imageSecondSize],RoundingRadius->2, FrameMargins-> cSize3],imageSize,Alignment->Center];

Return[{paneSticks, paneStickSum}]
]


(*
Attraverso questa funzione vengono generati i regoli utilizzati per spiegare la somma.
n rappresenta il numero in input, mentre m rappresenta la diversa modalit\[AGrave], infatti:
se m = 1 -> vengono stampati solo i regoli
se m = 2 -> venogno stampati i regoli e le label corrispondenti
*)
generateSumSticks[n_,m_] := Module[  
{
	numToSplit = n,
	mode = m
},


sticksToShow = getSticks[numToSplit]; (*lista contenente i numeri relativi a ciascun regolo da mostrare*)

lastItemX = 0; (*valore che identifica la coordinata x dell'ultimo regolo renderizzato*)
sticksToReturn = {};
offset = 1; (*valore che identifica l'offset che rappresenta la distanza tra i regoli adiacenti*)


(*con la seguente Table viene creato l'insieme di regoli adiacenti che identificano un unico numero*)
Table[(
rectToAppend = {EdgeForm[Thick],{Part[sticksColorList,Part[sticksToShow,index]], Rectangle[{lastItemX,0},{lastItemX + Part[sticksToShow,index],1}]}};
AppendTo[sticksToReturn,rectToAppend];
lastItemX = lastItemX + Part[sticksToShow,index] + offset;
),{index,Length[sticksToShow]}];

If[mode == 1, Graphics[sticksToReturn], If[mode == 2, GraphicsRow[{Style[numToSplit,cSize2,Bold],Style["=" ,cSize3,Bold],Graphics[sticksToReturn]},ImageSize->{1200}]]]
]


(*Attraverso questa funzione vengono generati i regoli e i campi di input utilizzati nel esercizio in cui l'utente deve inserire il valore di ogni regolo attraverso i tasti "+","-"*)
generateStickWithInput[] := Module[{},

sticksToShow = {1,2,3,4,5,6,7,8,9,10}; (*lista dei regoli da mostrare*)

lastItemX = 0; (*valore che identifica la coordinata x dell'ultimo regolo renderizzato*)
sticksToReturn = {};
offset = 5; (*valore che identifica l'offset che rappresenta la distanza tra i regoli adiacenti*)


(*con la seguente Table viene creato l'insieme di regoli che verranno mostrati*)

Table[(
rectToAppend = {EdgeForm[Thick],{Part[sticksColorList,Part[sticksToShow,index]], Rectangle[{lastItemX,0},{lastItemX + offset,1}]}};
AppendTo[sticksToReturn,rectToAppend];
lastItemX = lastItemX + Part[sticksToShow,index] + offset;
),{index,Length[sticksToShow]}];

valuesList = {0,0,0,0,0,0,0,0,0,0}; (*lista contenente le quantit\[AGrave] di ciascun regolo che sono inizializzate tutte a 0*)

(*pulsanti che permettono di aggiungere/rimuovere i regoli *)
button1Add = Button[Style["+",cSize1,Bold],Part[valuesList,1]+= 1,{FrameMargins->12, Background-> LightGreen}]; 
button1Sub = Button[Style["-",cSize1,Bold],If[Part[valuesList,1]>0,Part[valuesList,1]-= 1],{FrameMargins->12, Background-> LightRed}];

button2Add = Button[Style["+",cSize1,Bold],Part[valuesList,2]+= 1,{FrameMargins->12, Background-> LightGreen}]; 
button2Sub = Button[Style["-",cSize1,Bold],If[Part[valuesList,2]>0,Part[valuesList,2]-= 1],{FrameMargins->12, Background-> LightRed}];

button3Add = Button[Style["+",cSize1,Bold],Part[valuesList,3]+= 1,{FrameMargins->12, Background-> LightGreen}]; 
button3Sub = Button[Style["-",cSize1,Bold],If[Part[valuesList,3]>0,Part[valuesList,3]-= 1],{FrameMargins->12, Background-> LightRed}];

button4Add = Button[Style["+",cSize1,Bold],Part[valuesList,4]+= 1,{FrameMargins->12, Background-> LightGreen}]; 
button4Sub = Button[Style["-",cSize1,Bold],If[Part[valuesList,4]>0,Part[valuesList,4]-= 1],{FrameMargins->12, Background-> LightRed}];

button5Add = Button[Style["+",cSize1,Bold],Part[valuesList,5]+= 1,{FrameMargins->12, Background-> LightGreen}]; 
button5Sub = Button[Style["-",cSize1,Bold],If[Part[valuesList,5]>0,Part[valuesList,5]-= 1],{FrameMargins->12, Background-> LightRed}];

button6Add = Button[Style["+",cSize1,Bold],Part[valuesList,6]+= 1,{FrameMargins->12, Background-> LightGreen}]; 
button6Sub = Button[Style["-",cSize1,Bold],If[Part[valuesList,6]>0,Part[valuesList,6]-= 1],{FrameMargins->12, Background-> LightRed}];

button7Add = Button[Style["+",cSize1,Bold],Part[valuesList,7]+= 1,{FrameMargins->12, Background-> LightGreen}]; 
button7Sub = Button[Style["-",cSize1,Bold],If[Part[valuesList,7]>0,Part[valuesList,7]-= 1],{FrameMargins->12, Background-> LightRed}];

button8Add = Button[Style["+",cSize1,Bold],Part[valuesList,8]+= 1,{FrameMargins->12, Background-> LightGreen}]; 
button8Sub = Button[Style["-",cSize1,Bold],If[Part[valuesList,8]>0,Part[valuesList,8]-= 1],{FrameMargins->12, Background-> LightRed}];

button9Add = Button[Style["+",cSize1,Bold],Part[valuesList,9]+= 1,{FrameMargins->12, Background-> LightGreen}]; 
button9Sub = Button[Style["-",cSize1,Bold],If[Part[valuesList,9]>0,Part[valuesList,9]-= 1],{FrameMargins->12, Background-> LightRed}];

button10Add = Button[Style["+",cSize1,Bold],Part[valuesList,10]+= 1,{FrameMargins->12, Background-> LightGreen}]; 
button10Sub = Button[Style["-",cSize1,Bold],If[Part[valuesList,10]>0,Part[valuesList,10]-= 1],{FrameMargins->12, Background-> LightRed}];

(*
colonne composte da:
1) label che identifica il numero relativo al regolo considerato
2) rappresentazione grafica del regolo considerato
3) pulsanti "+","-" che permettono di aggiungere o rimuovere un regolo di quel tipo
4) label che identifica il numero di regoli selezionato del tipo considerato 
*)
stickCol1 = GraphicsColumn[{Pane[Style[1,cSize2,Bold],FrameMargins->30], Graphics[Part[sticksToReturn,1]],GraphicsRow[{button1Add,button1Sub}], Pane[Style[Dynamic[Part[valuesList,1]],25,Bold],FrameMargins->30]}];
stickCol2 = GraphicsColumn[{Pane[Style[2,cSize2,Bold],FrameMargins->30], Graphics[Part[sticksToReturn,2]],GraphicsRow[{button2Add,button2Sub}], Pane[Style[Dynamic[Part[valuesList,2]],25,Bold],FrameMargins->30]}];
stickCol3 = GraphicsColumn[{Pane[Style[3,cSize2,Bold],FrameMargins->30], Graphics[Part[sticksToReturn,3]],GraphicsRow[{button3Add,button3Sub}], Pane[Style[Dynamic[Part[valuesList,3]],25,Bold],FrameMargins->30]}];
stickCol4 = GraphicsColumn[{Pane[Style[4,cSize2,Bold],FrameMargins->30], Graphics[Part[sticksToReturn,4]],GraphicsRow[{button4Add,button4Sub}], Pane[Style[Dynamic[Part[valuesList,4]],25,Bold],FrameMargins->30]}];
stickCol5 = GraphicsColumn[{Pane[Style[5,cSize2,Bold],FrameMargins->30], Graphics[Part[sticksToReturn,5]],GraphicsRow[{button5Add,button5Sub}], Pane[Style[Dynamic[Part[valuesList,5]],25,Bold],FrameMargins->30]}];
stickCol6 = GraphicsColumn[{Pane[Style[6,cSize2,Bold],FrameMargins->30], Graphics[Part[sticksToReturn,6]],GraphicsRow[{button6Add,button6Sub}], Pane[Style[Dynamic[Part[valuesList,6]],25,Bold],FrameMargins->30]}];
stickCol7 = GraphicsColumn[{Pane[Style[7,cSize2,Bold],FrameMargins->30], Graphics[Part[sticksToReturn,7]],GraphicsRow[{button7Add,button7Sub}], Pane[Style[Dynamic[Part[valuesList,7]],25,Bold],FrameMargins->30]}];
stickCol8= GraphicsColumn[{Pane[Style[8,cSize2,Bold],FrameMargins->30], Graphics[Part[sticksToReturn,8]],GraphicsRow[{button8Add,button8Sub}], Pane[Style[Dynamic[Part[valuesList,8]],25,Bold],FrameMargins->30]}];
stickCol9 = GraphicsColumn[{Pane[Style[9,cSize2,Bold],FrameMargins->30], Graphics[Part[sticksToReturn,9]],GraphicsRow[{button9Add,button9Sub}], Pane[Style[Dynamic[Part[valuesList,9]],25,Bold],FrameMargins->30]}];
stickCol10 = GraphicsColumn[{Pane[Style[10,cSize2,Bold],FrameMargins->30], Graphics[Part[sticksToReturn,10]],GraphicsRow[{button10Add,button10Sub}], Pane[Style[Dynamic[Part[valuesList,10]],25,Bold],FrameMargins->30]}];

sticksCol = GraphicsRow[{stickCol1,stickCol2,stickCol3,stickCol4,stickCol5,stickCol6,stickCol7,stickCol8,stickCol9,stickCol10},ImageSize->Full,Alignment->Center];

Return[{finalVal,sticksCol}];

]


(*Attraverso questa funzione viene generato l'esercizio relativo ai regoli che utilizza i metodi sopra*)
generateFirstSticksEx[] := Module[{},

(*Attraverso le seguenti righe di codice viene generato il numero randomico che l'utente deve "indovinare"*)
SeedRandom[];
maxNum = 99; (*si \[EGrave] scelto di utilizzare il valore massimo 99 per far si che l'inserimento dell'input non richieda troppo tempo*)
num = RandomInteger[{1,maxNum}];

returnedSticks = generateStickWithInput[];
fVal = Part[returnedSticks,1];
sCol = Part[returnedSticks,2];

(*
pulsante attraverso il quale l'utente pu\[OGrave] controllare la sua risposta e passare al prossimo esercizio. 
Verr\[AGrave] mostrato un pop-up con una delle seguenti risposte:
"Risposta Esatta! Premi OK per continuare" se la risposta inserita \[EGrave] corretta;
"Sbagliato...riprova! Premi OK per continuare" se la risposta inserita \[EGrave] errata
*)
inputRulesList = {};

buttonFinal1 = Button[Style["Avanti",15,Bold],(
	finalVal = 0;
	inputRulesList = {};
	Table[(
		finalVal += Part[valuesList,valIndex]*valIndex; (*viene effettuata la somma di tutti i regoli inseriti dall'utente per controllare se questa \[EGrave] uguale al valore da indovinare*)
		If[Part[valuesList,valIndex] != 0, (
			Table[AppendTo[inputRulesList,valIndex],{vIndex,Part[valuesList,valIndex]}]
		)]
	),{valIndex, Length[valuesList]}];
	If[finalVal == num,  If[Length[Reverse[inputRulesList]] === Length[getSticks[num]],MessageDialog["Risposta Esatta! \nPremi OK per continuare"];num = RandomInteger[{1,maxNum}];valuesList = {0,0,0,0,0,0,0,0,0,0},MessageDialog["Risposta Esatta ma non la migliore! \nPremi OK per continuare"];],MessageDialog["Sbagliato...riprova! \nPremi OK per continuare"]];
),{FrameMargins->30, Background-> LightCyan}];

(*pulsante attraverso il quale l'utente pu\[OGrave] resettare i valori relativi a ciascun regolo portando la quantit\[AGrave] di ognuno di essi a 0 e ricominciare l'esercizio dall'inizio*)
buttonFinal2 = Button[Style["Reset",15,Bold], valuesList = {0,0,0,0,0,0,0,0,0,0},{FrameMargins->30, Background-> LightOrange}];

outputCol = GraphicsColumn[{Framed[Style[Dynamic[num],cSize2,Bold],RoundingRadius->2, FrameMargins-> 30],sticksCol,GraphicsRow[{buttonFinal1,buttonFinal2},ImageSize->{Automatic,100}]},ImageSize->{Full,1000},Alignment->Center,Spacings->0];

outputCol
]


End[];
EndPackage[];
