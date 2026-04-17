grafobase = {{1, -1}, {2, -1}, {3, -1}};



inserzione3[grafo_] := Module[{nuovigrafi, indici, npt, nvert}, 
    indici = Flatten[grafo]; 
    npt = Max[indici]; (*Leggo valore massimo (e poi minimo) degli indici per determinare quali sono i nuovi nodi del grafo*)
    nvert = Min[indici]; 
    nuovigrafi = Table[inserisci3[i, npt + 1, nvert - 1, grafo], 
        {i, 1, Length[grafo]}]; (*Su tutti i nodi esterni del grafo attacca un grafobase*)
    
    Return[nuovigrafi];
]; 

        
inserisci3[pos_, ext_, int_, grafo_] := Module[{temp}, 
    temp = ReplacePart[grafo, pippo[{ext, int}, {grafo[[pos,1]], int}, {grafo[[pos,2]], int}], pos]; (*non ho definito pippo, quindi mathematica lo inserisce a forza bruta*)
    temp = temp /. {pippo -> Sequence}; (*Ora pippo diventa una Sequence, e il nodo esterno è sostituito correttamente*)
    
    Return[temp];
]; 
    

inserzione4[grafo_] := Module[{temp}, 
    indici = Flatten[grafo]; 
    npt = Max[indici]; 
    nvert = Min[indici]; (*Ora sappiamo indice più alto e più basso*)
    nuovigrafi = (*creo nuovo grafo provando a inserire una linea in più attaccata ad ogni vertice interno*)
       Table[inserisci4[i, npt + 1, grafo], {i, -1, nvert, -1}]; (*i=-1, -2, ..., nvert*)
    
    Return[nuovigrafi];
]; 
      
inserisci4[vertice_, ext_, grafo_] := 
    If[Count[grafo, vertice, 2] === 3, Append[grafo, {ext, vertice}], {}];
  
(*La funzione genera grafi con n nodi esterni, a partire dal grafobase*)
generagrafi[n_] := Module[{temp}, 
         i = 3; (*Numero di nodi esterni del grafico base*)
         listagrafi = {grafobase}; (*Il mio output sarà una lista di grafi, comincio a rendere il mio grafo iniziale una lista*)
         
         While[i < n, (*Ciclo sul numero dei nodi esterni di nodi da creare*)
         nuovigrafi3 = inserzione3 /@ listagrafi; 
         nuovigrafi3 = Flatten[nuovigrafi3, 1]; 
         nuovigrafi4 = inserzione4 /@ listagrafi; 
         nuovigrafi4 = Flatten[nuovigrafi4, 1]; 
         listagrafi = Join[nuovigrafi3, nuovigrafi4]; 
         listagrafi = DeleteCases[listagrafi, 
          {}]; ++i]; 
          
        Return[listagrafi];
]; 