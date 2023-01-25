(* ::Package:: *)

(* :Title:Abacus*)
(* :Context:Abacus`*)
(* :Summary: Abacus methods*)
(* :Package Version: 17,Maggio 2022*)
(* :Mathematica Version: 13.0*)


BeginPackage["Abacus`"]
getAbacusExplanation::usage="getAbacusExplanation[]";
getAbacusFirstEx::usage="getAbacusFirstEx[]";
getSecondAbacusEx::usage="getSecondAbacusEx[]";
getThirdAbacusEx::usage="getThirdAbacusEx[]";


Begin["`Private`"]


correctMessage = "Risposta Esatta! \nPremi OK per continuare";
wrongMessage = "Sbagliato...riprova! \nPremi OK per continuare";
wrongTypeMessage = "Controlla quello che hai scritto... DEVE essere un numero!";

(*costanti dimensioni style*)
constSize1 = 20;
constSize2 = 35;
constSize3 = 50;


(*
Attraverso questa funzione viene generato un'abaco che indica il numero num passato in input.
Il parametro noItems indica se l'abaco restituito deve essere popolato con le palline corrispondenti o meno (serve nell'esercizio in cui l'utente deve inserire le palline all'interno dell'abaco)
*)

showAbacus[num_,noItems_:False]:= Module[
{},

(* Prende in input il numero da rappresentare all'interno dell'abaco e ritorna in output il numero di aste da inserire all'interno dell'abaco a seconda che il numero in input sia nel range [1,9], [10,99],[100,999]*)
getNumStick[x_] := (
numStick = 1;
If[x>9, numStick = 2];
If[x>99 ,numStick = 3];  
Return[numStick];
);
(* Prende in input:
	itemsNum_: numero da rappresentare tramite l'abaco
	sticksDistanceBetween: distanza tra le aste dell'abaco (variabile in quanto \[EGrave] prevista la presenza di 1,2 o 3 aste)
	stickWidth: lunghezza della singola asta
	index: attraverso questo valore siamo in grado di capire se ci riferiamo a unit\[AGrave], decine o centinaia. Questo pu\[OGrave] assumere i valori 1,2 o 3 che corrispondono rispettivamente a unit\[AGrave], decine e centinaia
	listLen: attraverso il valore listLen, siamo in grado di capire la classe di appartenza dell'input (unit\[AGrave], decine, centinaia) e quindi di stabilire la tipologia e quindi il colore della pallina da ritornare
*)
getAbacusItems[itemsNum_,sticksDistanceBetween,stickWidth,index_, listLen_]:= (
circleRadius = 0.3; (* definisce il raggio di ogni singola pallina*)

(*
la variabile x rappresenta la coordinata x delle palline ed \[EGrave] calcolata in relazione al fatto che le pallina da ritornare siano unit\[AGrave], decine o centinaia

attraverso la seguente somma: ((index  sticksDistanceBetween )+ (stickWidth  index)), ci posizioniamo in corrispondenza dell'asta dell'abaco corretta
andando poi a sottrarre la seguente quantit\[AGrave] ((((index  sticksDistanceBetween )+ (stickWidth  index)) - (( index sticksDistanceBetween) + (stickWidth (index-1))))/2 ); 
siamo in grado di posizionare ogni pallina esattamente al centro dell'asta
*)

x =  ((index  sticksDistanceBetween )+ (stickWidth  index)) -  ((((index  sticksDistanceBetween )+ (stickWidth  index)) - (( index sticksDistanceBetween) + (stickWidth (index-1))))/2 );

(*Attraverso i seguenti 3 controlli, determiniamo la classe e quindi il colore delle pallina da restituire in output*)
If[listLen == 1, colorList = {Blue}];
If[listLen == 2, colorList = {Red,Blue}];
If[listLen == 3, colorList = {Green, Red, Blue}];

(*
Viene ritornata in output la lista delle palline relative ad una determinata asta (dipende dal valore di index) e attraverso il loop ottenuto mediante il costrutto Table, 
siamo in grado di posizionare una sopra l'altra le palline andando ad aggiungere un offset, attraverso il seguente calcolo: (i*2*circleRadius) + (2 * circleRadius) alla coordinata y
ad ogni iterazione
*)
Return[Table[{Part[colorList,index],Disk[{x , (i*2*circleRadius) + (2 * circleRadius)},0.3]},{i,itemsNum}]];

);

numStick = getNumStick[num]; (*a partire dal numero ricevuto in input si ottiene il numero di aste da posizionare sull'abaco*)

(*le seguenti 4 variabili rappresentano delle costanti attraverso le quali vengono definite lunghezza e altezza di della base dell'abaco e delle aste che saranno poste su di essa*)
abacusBaseWidth = 6;
abacusBaseHeight= 1;
stickWidth = 0.2;
stickHeight= 6.5;

(*base dell'abaco*)
abacusBase = Rectangle[{0,0},{abacusBaseWidth,abacusBaseHeight}];

(*
Le righe di codie sottostanti vengono utilizzate per suddividere la base dell'abaco in modo da porre le aste alla stessa distanza l'una dall'altra.
La variabile sticksDistanceBetween infatti, rappresenta la distanza tra ogni asta dell'abaco.

Con la seguente riga di codice la lunghezza della base dell'abaco viene divisa in numStick + 1 parti in modo da calcolare poi la lunghezza di ogni singola parte ed assegnarla alla
variabile sticksDistanceBetween.
*)

sticksDistanceBetween = (abacusBaseWidth - (stickWidth  numStick)) / (numStick + 1);

(*
Attraverso la seguente Table vengono create un numero di aste pari a numStick. 
Ogni asta sar\[AGrave] un rettangolo con le seguenti coordinate:
 
x1: (i sticksDistanceBetween) + (stickWidth (i-1)) -> stiamo aggiungendo (stickWidth (i-1)) perch\[EGrave] dobbiamo tenere in considerazione la lunghezza delle aste precedenti;
x2: ((i sticksDistanceBetween ) + (stickWidth i)) -> stiamo aggiungendo (stickWidth (i)) perch\[EGrave] dobbiamo tenere in considerazione la lunghezza delle aste precedenti e dell'asta che stiamo aggiungendo

y1: 1 poich\[EGrave] \[EGrave] poggiata sulla base dell'abaco che ha altezza 1
y2: stickHeight 
*)

sticks= Table[{Rectangle[{(i sticksDistanceBetween) + (stickWidth (i-1)),1},{(i  sticksDistanceBetween )+ (stickWidth  i),stickHeight}]}, {i, numStick}];

(*Viene suddiviso il numero in cifre che ci permetteranno poi di discriminare tra loro unit\[AGrave], decine e centinaia in modo da colorare le palline con colori diversi e posizionarle sulle aste corrispondenti*)
splittedInput = IntegerDigits[num];

(*Attraverso la seguente Table vengono disegnate sull'abaco le palline corrispondenti a unit\[AGrave], decine e centinaia all'inerno dell'abaco con colore e posizione corretti (maggiori dettagli nei commenti della funzione getAbacusItem)*)
itemsList = Table[getAbacusItems[Part[splittedInput,i],sticksDistanceBetween,stickWidth ,i,Length[splittedInput]],{i,Length[splittedInput]}];
flattedItemList = Flatten[itemsList,1];

Graphics[{EdgeForm[Thick],Brown,abacusBase, Gray, sticks, flattedItemList}]
]



(*Attraverso questa funzione viene generata la slide che contiene la spiegazione relativa all'utilizzo dell'abaco*)
getAbacusExplanation[] := Module[{},

(*Attraverso le seguenti 3 righe di codice vengono generati i 2 numeri randomici da attraverso i quali strutturare gli esempi nella slide di spiegazione relativa all'utilizzo dell'abaco*)
SeedRandom[];
val1 = RandomInteger[{1,199}]; (*si \[EGrave] scelto di usare due numeri nel range [1,199] per facilitare la comprensione della spiegazione*)
val2 = RandomInteger[{1,199}];

(*col1,col2 e col3 contengono le tre diverse istanze dell'abaco che corrispondono rispettivamente ai due valori da sommare e alla loro somma oltre che a una label che ne indica il valore*)
col1 = GraphicsColumn[{Style[val1,constSize2,Bold], showAbacus[val1]}];
col2 = GraphicsColumn[{Style[val2,constSize2,Bold], showAbacus[val2]}];
col3 = GraphicsColumn[{Style[val1 + val2,constSize2,Bold], showAbacus[val1 + val2]}];

(*abacusVal1 e abacusVal2 rappresentano i due numeri da sommare rappresentati attraverso il rispettivo abaco seguiti dal simbolo = e da una label che indica numero corrispondente*)
abacusVal1= GraphicsRow[{showAbacus[val1], Style["=" ,constSize3,Bold], Style[val1,constSize2,Bold]}];
abacusVal2= GraphicsRow[{showAbacus[val2], Style["=" ,constSize3,Bold], Style[val2,constSize2,Bold]}];

(*\[EGrave] l'abaco che rappresenta la somma seguito dal simbolo = e dalla label relativa al numero corrispondente*)
abacusSum = GraphicsRow[{col1, Style["+" ,50,Bold], col2, Style["=",constSize3,Bold], col3}];

(*di seguito vengono creati i 2 pane da restituire sottoforma di lista allineati al centro della slide  ai quali viene applicato un frame per evidenziarne i bordi *)
paneFirstExpl = Pane[GraphicsColumn[{abacusVal1,abacusVal2}, ImageSize->{1000}, Frame->True], Full, Alignment->Center];
paneSecondExpl = Pane[GraphicsRow[{abacusSum}, ImageSize->{1000},Frame->True],Full, Alignment->Center];

Return[{paneFirstExpl,paneSecondExpl}]

]


(*Attraverso questa funzione viene generato il primo esercizio relativo all'abaco*)

getAbacusFirstEx[] := Module[{},

(*Vengono generati randomicamente nummAnswer numeri che corrispondono alle alternative proposte all'utente di cui una sola \[EGrave] la risposta esatta*)
SeedRandom[];
numAnswers = 3;
answer = 0;

correctAnswerIndex= RandomInteger[{1,numAnswers}]; (* contiene la posizione nella lista della risposta corretta *)

valList ={}; (*lista contenente i numAnswer valori generati randomicamente attraverso la Table seguente*)
Table[(
	(*si \[EGrave] scelto di usare un numero nel range [1,199] per facilitare la comprensione degli esercizi in quanto i bambini di seconda elementaro sanno contare fino alle centinaia*)
	val= RandomInteger[{1,199}]; 
	AppendTo[valList,val];
),{index, numAnswers}];

dynAbac = Dynamic[showAbacus[Part[valList,correctAnswerIndex]]];
paneFirstEx = Pane[dynAbac, Full, Alignment->Center]; (*abaco che mostra il numero che l'utente deve "indovinare"*)

correctAnswer = Part[valList,correctAnswerIndex]; 

(*con la seguente Table vengono generati i numAnswer pulsanti attraverso i quali l'utente potr\[AGrave] compiere la sua scelta in merito alla risposta da dare*)
buttonList = Table[(With[{i=i},
Dynamic[Button[Style[Part[valList,i] ,constSize1,Bold],(
	answer = Part[valList,i];
	If[answer == correctAnswer,(
		MessageDialog[correctMessage];
		valList ={}; (*lista contenente i numAnswer valori generati randomicamente attraverso la Table seguente*)
		Table[(
			val= RandomInteger[{1,199}]; 
			AppendTo[valList,val];
		),{index, numAnswers}];
		correctAnswerIndex= RandomInteger[{1,numAnswers}];
		correctAnswer = Part[valList,correctAnswerIndex];
	) ,MessageDialog[wrongMessage]];
),{ FrameMargins->constSize3, Background-> RandomColor[]}]]]),{i,Length[valList]}];
 
  
paneFirstEx
Pane[GraphicsRow[buttonList],Full, Alignment->Center]
]


(*Attraverso questa funzione viene generato il secondo esercizio relativo all'abaco*)
getSecondAbacusEx[] := Module[{},

(*Vengono generati randomicamente nummAnswer2 numeri che corrispondono alle alternative proposte all'utente di cui una sola \[EGrave] la risposta esatta*)
SeedRandom[];
numAnswers2 = 1;
val2= RandomInteger[{1,199}]; 

inputVal2 = Null;
abacus2 = Dynamic[showAbacus[val2]]; (*abaco che mostra il numero che l'utente deve inserire nella casella di input *)
inputF2 = InputField[Dynamic[inputVal2,If[#=!=Null,inputVal2 = #]&],FieldSize->{Automatic,{0,Infinity}},BaseStyle->"Section"]; (*campo di testo nel quale inserire la risposta dell'utente*)

(*
pulsante attraverso il quale l'utente pu\[OGrave] controllare la sua risposta e che mostrer\[AGrave] un pop-up con una delle seguenti risposte:
"Controlla il valore inserito... DEVE essere un numero!" se il valore inserito non \[EGrave] un numero;
"Risposta Esatta! Premi OK per continuare" se la risposta inserita \[EGrave] corretta;
"Sbagliato...riprova! Premi OK per continuare" se la risposta inserita \[EGrave] errata
*)
button1Ex2 = Button[Style["Controlla risposta",constSize1,Bold],If[!NumericQ[inputVal2],MessageDialog[wrongTypeMessage],If[inputVal2 == val2, MessageDialog[correctMessage],MessageDialog[wrongMessage]]] ,{FrameMargins->constSize1, Background-> LightGreen}];

(*pulsante attraverso il quale l'utente pu\[OGrave] passare all'esercizio successivo:*)
button2Ex2 = Button[Style["Prossimo esercizio",constSize1,Bold], (
	val2= RandomInteger[{1,199}]; 
	inputVal2 = Null;	
),{FrameMargins->constSize1, Background-> LightCyan}];


pane2 = Pane[GraphicsColumn[{inputF2, abacus2,GraphicsRow[{button1Ex2,button2Ex2}]},ImageSize->{1000}],Full, Alignment->Center];

Return[{Style[inputF2,TextAlignment->Center],Pane[GraphicsColumn[{abacus2,GraphicsRow[{button1Ex2,button2Ex2}]},ImageSize->{1000}],Full, Alignment->Center]}];
]


(*Attraverso questa funzione viene generato il terzo esercizio relativo all'abaco*)
getThirdAbacusEx[] := Module[{},

(*Vengono generati randomicamente nummAnswers numeri che corrispondono alle alternative proposte all'utente di cui una sola \[EGrave] la risposta esatta*)
SeedRandom[];
num = RandomInteger[{1,199}]; 
userInput = 0;

(*i seguenti pulsanti servono per aggiungere/rimuovere le palline relative a unit\[AGrave], decine e centinaia sull'abaco*)
hundredsAdd = Button[Style["Aggiungi centinaia (+)",constSize1,Bold,Black], If[userInput < 900, userInput+= 100, userInput],{FrameMargins->constSize1, Background-> Green}];
hundredsSub =Button[Style["Rimuovi centinaia (-)",constSize1,Bold,Black], If[userInput >= 100, userInput-=100, userInput],{FrameMargins->constSize1, Background-> Green}];
dozensAdd = Button[Style["Aggiungi decine (+)",constSize1,Bold,Black], If[userInput < 990, userInput+=10, userInput],{FrameMargins->constSize1, Background-> Red}];
dozensSub = Button[Style["Rimuovi decine (-)",constSize1,Bold,Black], If[userInput >= 10, userInput-=10, userInput],{FrameMargins->constSize1, Background-> Red}];
unitsAdd = Button[Style["Aggiungi unit\[AGrave] (+)",constSize1,Bold,Black], If[userInput < 999, userInput+=1, userInput],{FrameMargins->constSize1, Background-> Blue}];
unitsSub = Button[Style["Rimuovi unit\[AGrave] (-)",constSize1,Bold,Black], If[userInput > 0, userInput-=1, userInput],{FrameMargins->constSize1, Background-> Blue}];

(*
pulsante attraverso il quale l'utente pu\[OGrave] controllare la sua risposta e passare al prossimo esercizio. 
Verr\[AGrave] mostrato un pop-up con una delle seguenti risposte:
"Risposta Esatta! Premi OK per continuare" se la risposta inserita \[EGrave] corretta;
"Sbagliato...riprova! Premi OK per continuare" se la risposta inserita \[EGrave] errata
*)
nextEx = Button[Style["Prossimo esercizio",constSize1,Bold,Black],(
	If[userInput == num,MessageDialog[correctMessage];num = RandomInteger[{1,199}];userInput = 0, MessageDialog[wrongMessage]]
),{FrameMargins->constSize1, Background-> Cyan,ImageSize->{600}}];

buttonsList = {hundredsAdd,hundredsSub,dozensAdd,dozensSub,unitsAdd,unitsSub,nextEx};

container = Pane[Framed[Style[Dynamic[num],constSize2,Bold],RoundingRadius->2, FrameMargins-> 30],Full,Alignment->Center];
dynamicAbacus = Dynamic[showAbacus[userInput]];
GraphicsColumn[{container,dynamicAbacus,GraphicsRow[buttonsList,ImageSize->Full]},ImageSize->Full]

]


End[];
EndPackage[]
