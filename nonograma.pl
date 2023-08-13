% ------------------------------------------------------------
% ASIGNATURA: Lenguajes de Programacion
% CURSO: 2022-2023
% TRABAJO: Practica 2 - Nonograma
% AUTORES: Mario Ventura, Luis Miguel Vargas, Alberto Ruiz
% -------------------------------------------------------------

nono([[verd,lila,vermell,vermell],[blau,verd,blau,blau],[lila, blau,verd,verd],[verd,blau,vermell, verd]]).

% -------------------------------------------------------------


% ---> PREDICADO escriuNonograma
% Imprime por pantalla el nonograma que recibe por parametro
% Caso trivial: Nonograma vacio (0 filas de 0 columnas), imprimimos una
% nueva linea en salida estandar
escriuNonograma([]):- nl.
% Caso de nonograma no vacio
escriuNonograma([X|L]):-
    % El primer elemento es una lista, se imprime y se hace llamada recursiva
    % para el cdr de la lista que forma el nonograma
    printLine(X),
    escriuNonograma(L).


% ---> PREDICADO printLine
% Actua como 'funcion' auxiliar que imprime una fila del nonograma
% Imprime una X por cada elemento de cada lista (lista de listas)
% Caso trivial: Lista vacia (fin de recursion o nonograma vacio)
printLine([]):-
    % Imprimimos una lista sin elementos
    write('[]'),
    % Nueva linea
    nl.
% Caso de lista no vacia
printLine([X|L]):-
    % Inicio de la lista
    write('['),
    % Primer elemento de la lista
    write(X),
    % Uso del predicado writeElements para el resto de elementos
    writeElements(L),
    % Fin de la fila
    write(']'),
    % Nueva linea
    nl.

% ---> PREDICADO writeElements
% Actua como 'funcion' auxiliar que imprime elementos de una lista
% Caso trivial: Lista vacia
writeElements([]).
% Caso de lista no vacia
writeElements([X|L]):-
    % Elementos separados por comas ','
    write(','),
    % Escribimos el elemento
    write(X),
    % Recursividad para iterar y escribir el resto de elementos
    writeElements(L).


% -------------------------------------------------------------


% ---> PREDICADO mostraNonograma
% Pinta en la pantalla un nonograma dadas sus filas, columnas y la
% separacion entre cada fila y columna
mostraNonograma(Nono,Files,Columnes,IncFiles,IncColumnes):-
    % Limpiamos pantalla
    cls(),
    % Escribimos el nonograma
    printNono(Nono,Files,Columnes,IncFiles,IncColumnes).


% ---> PREDICADO printNono
% Actua como 'funcion' auxiliar para imprimir un nonograma en la consola
% Caso trivial: Nono vacio, no nos importan las filas, columnas o
% incrementos
printNono([], _, _, _, _).
% Caso de nonograma no vacio: El primer elemento del primer parametro es
% una lista que representa la primera fila
printNono([Fila|L], F, C, IncFila, IncCol):-
    % Ponemos el cursor en la fila F columna C
    gotoXY(C,F),
    % Escribimos la fila
    writeRow(Fila, F, C, IncCol),
    % Añadimos el incremento a la fila
    F1 is F + IncFila,
    % Llamada recursiva a la siguiente fila
    printNono(L, F1, C, IncFila, IncCol).


% ---> PREDICADO writeRow
% Actua como 'funcion' auxiliar para escribir la fila del nonograma
% Caso trivial: lista vacia, no nos importan el resto de parametros
writeRow([], _, _, _).
% Caso de lista no vacia: Nonograma no vacio
writeRow([Col|L], F, C, IncCol) :-
    % Movemos el cursor
    gotoXY(C, F),
    % Escogemos el color adecuado para el elemento de la fila en la que estamos
    color(Col),
    % Escribimos la X del color indicado
    write('X'),
    % Añadimos el incremento a las columnas
    C1 is C + IncCol,
    % Llamada recursiva para el siguiente elemento de la lista
    writeRow(L, F, C1, IncCol).


% -------------------------------------------------------------


% ---> PREDICADO ferNonograma
% Construye un nonograma aleatorio a partir de una lista de colores,
% un numero de filas y un numero de columnas
ferNonograma(Colors, Files, Columnes, Nono):-
    % Generamos el nonograma
    generateNonograma(Colors, Files, Columnes, [], Nono),
    % Mediante predicados ya creados, escribimos y mostramos el nonograma
    escriuNonograma(Nono),
    mostraNonograma(Nono, 3, 5, 1, 3).


% ---> PREDICADO generateNonograma
% Genera una fila aleatoria y se añade a una lista auxiliar. Esto se
% hara hasta que la lista auxiliar tenga las dimensiones especificadas
% del nonograma dado
% Caso trivial: 0 filas = nonograma vacio
generateNonograma(_, 0, _, Nono, Nono).
% Caso de filas >= 1
generateNonograma(Colors, Files, Columnes, NonoAux, Nono):-
    % Generamos una fila aleatoria
    generateRandomRow(Columnes, Colors, Fila),
    % Añadimos la fila aleatoria a la lista auxiliar
    NouAux = [Fila|NonoAux],
    % Decrementamos el total de filas para iterar solo f veces
    F is Files - 1,
    % Llamada recursiva para generar la siguiente fila aleatoria
    generateNonograma(Colors, F, Columnes, NouAux, Nono).


% ---> PREDICADO generateRandomRow
generateRandomRow(0, _, []).
/*se generan colores aleatorios, se coge uno cualquiera (el primero), se decrementa el nÃºmero de
columnas para llegar al caso de base de 0 columnas y se vuelve a invocar para hacer la siguiente columna*/
generateRandomRow(Columnes, Colors, [Color|Fila]):-
    % Generamos una permutacion aleatoria en los colores para escoger uno al azar
    random_permutation(Colors, RandomColor),
    % Dada la permutacion anterior, escogemos el primer color dado guardado
    nth0(0, RandomColor, Color),
    % Actualizamos las columnas para la proxima iteracion
    C is Columnes - 1,
    % Llamada recursiva para el siguiente elemento de la fila
    generateRandomRow(C, Colors, Fila).


% -------------------------------------------------------------


% ---> PREDICADO descriuNonograma
% Extrae, a partir de un nonograma, la descripcion de los colores de
% sus filas y columnas. Debe devolver una lista donde el primer
% componente sea la descripcion de las filas y la segunda sea la
% descripcion de las columnas
descriuNonograma(Nono, L):-
    % Generamos la traspuesta para hacer lo mismo 2 veces
    trasposta(Nono, L1),
    % Mostramos las pistas de cada fila de la matriz transpuesta
    rowsInfo(L1, L2),
    % Mostramos las pistas de cada fila de la matriz original
    rowsInfo(Nono, L3),
    % Ambos resultados se juntan en una sola lista, donde cada elemento en sí es una lista
    append([L3], [L2], L),
    % Mostramos la lista generada
    printList(L),
    !.


% ---> PREDICADO trasposta
% Devuelve la traspuesta de una matriz (igual al predicado del
% examen)
trasposta([[]|_], []).
trasposta(L1,[L3|L5]):-agafaPrimers(L1,L3), agafaDarrers(L1,L4),trasposta(L4,L5).

agafaPrimers([],[]).
agafaPrimers([[X|_]|L2],[X|L3]):-agafaPrimers(L2,L3).

agafaDarrers([],[]).
agafaDarrers([[_|L1]|L2],[L1|L3]):-agafaDarrers(L2,L3).


% ---> PREDICADO printList
% Predicado que sirve para mostrar todos los elementos de una lista
% Caso trivial: lista vacia
printList([]):-
    % Se imprime '[]' para mostrar que la lista esta vacia
    write('[]'),
    % Cortamos la recursividad
    !.
% Caso de lista no vacia
printList([X|L]):-
    % Se escribe '[' pora indicar el inicio de fila
    write('['),
    % Nueva linea
    nl,
    % Dado que el primer elemento es una lista, se imprime también
    printSubList1(X),
    % Mostramos el resto de la lista que ha quedado en L
    printSubList2(L),
    % Se escribe ']' para mostrar el final de la lista
    write(']'),
    % Nueva linea
    nl,nl.


% ---> PREDICADO printSubList
% Actúa como 'funcion' auxiliar que muestra todos los elementos de una
% sublista
% Caso trivial: Lista vacia
printSubList1([]):-
    % No imprimiremos nada para mantener el formato ya que los simbolos '[]'
    % que indican una lista vacia ya se han usado en printList
    !.
% Caso de lista no vacia
printSubList1([X|L]):-
    % Escribimos el primer elemento
    write(X),
    % Nueva linea
    nl,
    % Imprimimos el resto de la lista
    printSubList3(L).


% ---> PREDICADO printSubList2
% Imprime la sublista X y el resto de la subLista L
% Caso trivial: Lista vacia
printSubList2([]):- !.
% Caso de lista no vacia
printSubList2([X|L]):-
    % Escribimos la coma que separa elementos
    write(', '),
    % Escribimos el siguiente elemento
    printSubList1(X),
    % Escribimos el resto de la lista
    printSubList2(L).


% ---> PREDICADO printSubList3
% Imprime el resto de la sublista L
% Caso trivial: Lista vacia
printSubList3([]):-
    !.
% Caso de lista no vacia
printSubList3([X|L]) :-
    % Escribimos la coma que separa elementos
    write(', '),
    % Escribimos el elemento
    write(X),
    % Nueva linea
    nl,
    % Llamada recursiva para el resto de elementos de la lista
    printSubList3(L).


% ---> PREDICADO rowsInfo
% Nos permite conocer la informaciond de la fila en cuanto a la cantidad
% de apariciones de cada color
% Caso trivial: Lista vacia, se devuelve una lista vacia pq no hay info
rowsInfo([],[]).
% Caso de lista no vacia: Se obtiene info de la fila y se llama
% recursivamente para la info de la siguiente fila
rowsInfo([X|L1],[Y|L2]):-
    rowInfo(X,Y),
    rowsInfo(L1,L2).


% ---> PREDICADO rowInfo
% Nos permite conocer la informacion de una fila en cuanto a la cantidad
% de apariciones de cada color
% Caso trivial: Lista vacia
rowInfo([],[]).
% Caso de que el primer elemento sea algo que no nos interesa
rowInfo([elemento|L1],L2):-
    % Llamada recursiva para el resto de la lista
    rowInfo(L1,L2).
% Caso de que el elemento aparece 1 vez consecutiva
rowInfo([X|L1],[[seguits,X,1]|L2]):-
    % Contamos las apariciones del elemento en la lista
    numAparicions(X,[X|L1],1),
    !,
    % Llamada recursiva para el resto de la lista
    rowInfo(L1,L2).
% Caso de que el elemento aparece mas de 1 vez consecutiva
rowInfo([X|L1],[[seguits,X,Y]|L2]):-
    % Contamos las apariciones del elemento en la lista
    numAparicions(X,[X|L1],Y),
    % Comprobamos si X esta Y veces de forma consecutiva
    follow(X,Y,[X|L1]),
    !,
    % Borramos el elemento de la lista
    deleteElement(X,[X|L1],L3),
    % Miramos la informacion del resto de la lista
    rowInfo(L3,L2).
% Caso de que el elemento aparece mas de una vez pero de forma no
% consecutiva
rowInfo([X|L1],[[no_seguits,X,N]|L2]):-
    % Contamos las apariciones del elemento
    numAparicions(X,[X|L1],N),
    !,
    % Sustituimos el elemento
    replace(X,L1,L3),
    % Seguimos para el resto de la lista con el elemento sustituido
    rowInfo(L3,L2).


% ---> PREDICADO follow
% Nos permite saber si dos elementos dados por parametro son
% consecutivos
% Caso trivial
follow(_,_,[]):-fail.
% El elemento pasado por parametro es el elemento leido
follow(X,N,[X|L1]):-
    % Se comprueba que sean consecutivos con el predicado following
    following(X,N,[X|L1]),
    !.
% El elemento pasado por parametro no es el elemento leido
follow(X,N,[_|L1]):-
    % Se descarta y se lee el resto de la lista
    follow(X,N,L1).


% ---> PREDICADO numAparicions
% Similar al implementado en los ejercicios para practicar
% Cuenta el numero de apariciones de un elemento en una lista
% Caso trivial: Lista vacia, apariciones = 0
numAparicions(_, [], 0).
% Lista no vacia pero elemento no coincidente, se mira el resto de la
% lista sin incrementar contador
numAparicions(X, [Y|L], N):-
    % Confirmamos que no es el elemento que buscamos
    X \= Y,
    % Llamada recursiva para el resto de la lista
    numAparicions(X, L, N).
% Caso de que el elemento leido sea el que buscamos
numAparicions(X, [X|L], N):-
    % Llamada recursiva para el resto de la lista
    numAparicions(X, L, N1),
    % Incrementamos contador de apariciones
    N is N1 + 1.


% ---> PREDICADO following
% Permite la comprobacion de que dos elementos estan puestos de forma
% consecutiva partiendo de la premisa de que ya sabemos que se ha leido
% el elemento correcto
% Caso trivial
following(X,1,[X|_]).
% Caso de elementos consecutivos
following(X,N,[X|L1]):-
    % Se disminuye el contador de numero de veces consecutivas para la siguiente llamada recursiva
    N1 is N-1,
    % Llamada recursiva para el resto de la lista
    following(X,N1,L1).


% ---> PREDICADO replace
% Sustituye un elemento de la lista
% Caso trivial: Lista vacia
replace(_,[],[]).
% Caso de lista no vacia y elemento es primer elemento
replace(X,[X|L1],[elemento|L2]):-
    replace(X,L1,L2).
% Lista no vacia y elemento no es primer elemento
replace(X, [Y|L1], [Y|L2]):-
    X \= Y,
    replace(X, L1, L2).


% ---> PREDICADO deleteElement
% Permite eliminar un elemento dado de una lista dada
% Caso trivial: Lista vacia
deleteElement(_, [], []).
% Caso de lista no vacia y es el primer elemento
deleteElement(X, [X|L1], L2):-
    % Llamada recursiva
    deleteElement(X, L1, L2).
% Caso de lista no vacia pero no es el primer elemento
deleteElement(X,[Y|L1],[Y|L2]):-
    % Comprobamos si es diferente del primer elemento
    X \= Y,
    % Llamada recusriva para el resto de la lista
    deleteElement(X, L1, L2).


% -------------------------------------------------------------


% ---> PREDICADO mostraPistesHoritzontals
% Permite pintar las pistas de una descripcion dada
% PISTAS HORIZONTALES
% Caso trivial: Lista vacia
mostraPistesHoritzontals([],_,_,_,_).
% Caso de lista no vacia
% Mostramos la info de la primera fila y seguimos para el resto
mostraPistesHoritzontals([Fila|Info], F, C, IncF, IncC):-
    % Mostramos informacion de la primera fila
    showRowInfo(Fila, F, C, IncC),
    % Añadimos incremento
    F1 is F + IncF,
    % Llamada recursiva para el resto de filas
    mostraPistesHoritzontals(Info, F1, C, IncF, IncC).

% PISTAS VERTICALES
% Se hace lo mismo que para las horizontales
mostraPistesVerticals([], _, _, _, _).
mostraPistesVerticals([Fila|Info], F, C, IncF, IncC):-
    showColumnInfo(Fila, F, C, IncF),
    C1 is C + IncC,
    mostraPistesVerticals(Info, F, C1, IncF, IncC).


% ---> PREDICADO showRowInfo
% Muestra la info de la fila
showRowInfo([], _, _, _).
% Caso de lista no vacia
showRowInfo([[A,C,N]|L1], F, Col, IncC):-
    % Se va a la posicion dada por parametro
    gotoXY(Col, F),
    % Se muestra la informacion
    printElement(A,C,N),
    % Se aumenta la posicion mediante el incremento
    C1 is Col + IncC,
    % Se hace lo mismo para el proximo elemento
    showRowInfo(L1, F, C1, IncC).


% ---> PREDICADO showColumnInfo
% Mismo planteamiento que en showRowInfo
showColumnInfo([], _, _, _):- nl.
showColumnInfo([[A,C,N]|L1], F, Col, IncF):-
    gotoXY(Col, F),
    printElement(A,C,N),
    F1 is F + IncF,
    showColumnInfo(L1, F1, Col, IncF).


% ---> PREDICADO printElement
% Permite imprimir el elemento individual de la matriz
% Caso trivial: El elemento aparece solo 1 vez
printElement(seguits, X, 1):-
    % Poner color
    color(X),
    % Escribir un '1'
    write(1).
% El elemento aparece mas de una vez pero de forma no consecutiva
printElement(no_seguits, Y, X):-
    % Poner color
    color(Y),
    % Mostrar elemento
    write(X).
% El elemento aparece mas de una vez de forma consecutiva
printElement(seguits, Y, X):-
    % Comprobamos que es diferente de 1
    X \= 1,
    % Poner color
    color(Y),
    % Escribimos el elemento en el formato '<elemento>'
    % Se escribe el '<'
    write("<"),
    % Se ecribe el elemento
    write(X),
    % Se escribe el '>'
    write(">").


% -------------------------------------------------------------


% ---> PREDICADO resolNonograma
% Permite resolver un nonograma a partir de la descripcion de las pistas
% para las filas y columnas
resolNonograma([A,B], Nono):-
    % Resolvemos la fila dada la info de esa fila
    solveRows(A,Nono),
    % Hacemos la traspuesta y la guardamos en R
    trasposta(Nono,R),
    % Se comprueban el resto
    checkAllInfo(B,R),!.


% ---> PREDICADO solveRows
% Permite comprobar todas las combinaciones y escoger una correcta
% Caso trivial: lista vacia
solveRows([],[]).
% Caso de pista no vacia
solveRows([X|L1],[Y|L2]):-
    % Se pone la pista a color
    colorRows(X,C),
    % Se estudian todas las posibles combinaciones de colores
    generateAllCombinations(C,Y),
    % Se comprueba que combinacion de las producidas es valida
    checkCombinations(X,Y),
    % Llamada recursiva para resolver para el resto de filas
    solveRows(L1,L2).


% ---> PREDICADO colorRows
% Actua como 'funcion' auxiliar que permite pasar las pistas a color
% Caso trivial: lista vacia
colorRows([],[]).
% Caso de lista no vacia
colorRows([[_,C,NUM]|L1], L2):-
    % Se crean N repeticiones del color
    repeat(NUM,C,Y),
    % Miramos siguiente color
    colorRows(L1,L3),
    % Juntamos la lista con todos los colores
    append(Y,L3,L2).



% ---> PREDICADO repeat
% Actua como 'funcion' auxiliar que crea una lista con N veces el
% elemento X
% Caso trivial: N=1, se devuelve [X]
repeat(1,X,[X]):-
    !.
% Caso de N>1
repeat(N,X,[X|L1]):-
    % Decrementa el valor de N
    N1 is N-1,
    % Llamada recursiva con el nuevo valor de N
    repeat(N1,X,L1).


% ---> PREDICADO generateAllCombinations
% Actua como 'funcion' auxiliar que permite generar todas las
% combinaciones posibles dada una situacion concreta
% Caso trivial: Lista vacia
generateAllCombinations([],[]).
% Caso de lista no vacia
generateAllCombinations([X|L1],R):-
    % Generamos nuevas combinaciones en el resto de la lista
    generateAllCombinations(L1,L2),
    % Insertamos el elemento generado en L2 y eso se guarda en R
    inserir(X,L2,R).


% ---> PREDICADO inserir
% Igual al de la lista de ejercicios
% Permite insertar un elemento E en la lista L
inserir(E,L,[E|L]).
inserir(E,[X|Y],[X|Z]):-
    inserir(E,Y,Z).


% ---> PREDICADO
% Actua como 'funcion' auxiliar y nos permite buscar una combinacion
% valida
% Caso trivial: Lista vacia
checkCombinations([],_).
% Caso de lista no vacia y 1 aparicion
checkCombinations([[seguits,C,1]|L1], L2):-
    % Si es seguido y aparece una vez, se aplana la lista y se mira si el color es valido
    % Aplanamos la lista
    aplanar(L2,L3),
    % Miramos si el color es valio (pertenece a la lista)
    member(C,L3),
    !,
    % Llamada recursiva para el siguiente color
    checkCombinations(L1,L2).
% Color aparece mas de una vez de forma consecutiva
checkCombinations([[seguits,C,N]|L1], L2):-
    % Comprobamos si estas apariciones son consecutivas
    follow(C,N,L2),
    !,
    % Se hace lo mismo para el siguiente color
    checkCombinations(L1,L2).
% Color aparece mas de una vez de forma no consecutiva
checkCombinations([[no_seguits,C,_]|L1], L2):-
    % Comprobamos cuantas veces aparece
    numAparicions(C,L2,N2),
    % Verificamos que no este de forma consecutiva
    not(follow(C,N2,L2)),
    !,
    % Comprobar con el siguiente color
    checkCombinations(L1,L2).


% ---> PREDICADO checkAllInfo
% Nos permite comprobar toda la informacion de todas las pistas
% Caso de lista vacia
checkAllInfo([],_).
% Caso de lista no vacia
checkAllInfo([X|L1],[Y|L2]):-
    % Comprobamos las combinaciones del primer elemento (recordar que es lista de listas)
    checkCombinations(X,Y),
    % Llamada recursiva para el proximo elemento
    checkAllInfo(L1,L2).


% ---> PREDICADO aplanar
% Igual que el de la lista de ejercicios
% Actua como funcion auxiliar y elimina todas las sublistas de una lista
aplanar([],[]).
aplanar([X|L1],L2):-
    is_list(X),
    aplanar(X,L3),
    aplanar(L1,L4),
    append(L3,L4,L2).
aplanar([X|L1],[X|L2]):-
    aplanar(L1,L2).


% -------------------------------------------------------------
% ---> Funciones de ayuda
% -------------------------------------------------------------

cls:-write('\e[2J'), gotoXY(0,0).
gotoXY(X,Y):-write('\e['),write(X),write(";"),write(Y),write("H").

colorsValids([negre,vermell,verd,groc,blau,lila,cel]).

color(negre):-write("\e[1;90m").
color(vermell):-write("\e[1;91m").
color(verd):-write("\e[1;92m").
color(groc):-write("\e[1;93m").
color(blau):-write("\e[1;94m").
color(lila):-write("\e[1;95m").
color(cel):-write("\e[1;96m").
color(blanc):-write("\e[1;97m").
