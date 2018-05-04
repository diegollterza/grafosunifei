
/* 	




	Nome: Diego La Terza Pinelli Ferreira
	Professor: Edmilson Marmo Moreira
 */




Domains

listaInt=integer*
no=no(integer, listaInt)
grafo=no*

Predicates

adjacencias(integer, listaInt).
montaNo(integer, no).
montaGrafo(integer, grafo).
grau(listaInt,integer).
regular(grafo).
buscaProf(grafo,integer, listaInt).
buscaPAux(grafo,listaInt,listaInt,listaInt).
temAresta(integer, integer).
aresta(integer, integer).
vizinhos(grafo,integer,listaInt).
insereAdj(listaInt,listaInt,listaInt,listaInt,listaInt).
pertence(integer,listaInt).
ordem(grafo,integer).
tamanho(grafo,integer).
somaGrau(grafo,integer).
somaVertices(grafo,integer).
euleriano(grafo).
quantidadeImpares(grafo, integer).
complemento(grafo,grafo)
listaVertices(grafo, listaInt).
diferenca(listaInt,listaInt,listaInt).
membro(listaInt,integer).
criaComplemento(grafo,listaInt,grafo).
multigrafo(grafo).
temLaco(grafo).
temDuplaAresta(grafo).
componentesConex(grafo,integer).
removeVertices(grafo, listaInt, grafo).

Clauses

aresta(1,2).
aresta(1,3).
aresta(1,4).
aresta(2,4).
aresta(3,4).


temAresta(X,Y) :- aresta(X,Y); aresta(Y,X).

adjacencias(X, Lista) :-
findAll(Z, temAresta(X,Z), Lista).

montaNo(N, no(N, Lista)) :-
adjacencias(N, Lista).

montaGrafo(0, []) :- !.
montaGrafo(N, [Cab|Cauda]) :-
montaNo(N, Cab), N1 = N - 1,
montaGrafo(N1, Cauda).

grau([],0).
grau([_|Cauda],N):-grau(Cauda,N1),N=N1+1.

regular([_]).
regular([no(_,Adj1),no(V,Adj2)|Cauda]):-
grau(Adj1,T),grau(Adj2,T),
regular([no(V,Adj2)|Cauda]).

buscaProf(G,V,L) :-
buscaPAux(G,[V],[],L).

buscaPAux(_,[],_,[]):- !.
buscaPAux(G,[V|Pilha],S,[V|T]):-
vizinhos(G,V,Adj),
insereAdj(Pilha,Adj,NovaPilha,[V|S],SModif),
buscaPAux(G,NovaPilha,SModif,T).

vizinhos([no(X,Adj)|_],X,Adj):-!.
vizinhos([_|Cauda],X,Adj):-Vizinhos(Cauda,X,Adj).

insereAdj(X,[],X,S,S):-!.
insereAdj(Pilha,[X|Y],NovaPilha,S,SModif):-
not(pertence(X,S)),!,
insereAdj([X|Pilha],Y,NovaPilha,[X|S],SModif).
insereAdj(Pilha,[_|Y],NovaPilha,S,SModif):-
insereAdj(Pilha,Y,NovaPilha,S,SModif).

pertence(X,[X|_]).
pertence(X,[_|Y]):-pertence(X,Y).

ordem([],0).
ordem([_|T],L1):- ordem(T,L2), L1 = L2 + 1.

somaGrau([],0).
somaGrau([no(_,L)|H],N):- somaGrau(H,N1), grau(L,N2), N = N1 + N2.

somaVertices([],0).
somaVertices([_|T],N):- somaVertices(T,N1), N = N1 + 1. 

tamanho(G,N):- somaGrau(G,N1), somaVertices(G,N2), N = N2 + (N1/2).

quantidadeImpares([],0).
quantidadeImpares([no(_,L)|T], N):- quantidadeImpares(T,N1), grau(L,N2), N = N1 + (N2 mod 2). 

euleriano(G):- quantidadeImpares(G,N), N = 0, write("euleriano"), nl ,!.
euleriano(G):- quantidadeImpares(G,N), N = 2, write("euleriano"), nl ,!.
euleriano(_):- write("Nao Euleriano"), nl.

listaVertices([],[]).
listaVertices([no(V,_)|T],[V|T2]):- listaVertices(T,T2). 

%	soh verifiquei que havia a funcao pertence depois de copiar a busca das notas de aula
membro([H|_],H).
membro([_|T],H):- membro(T,H).

%	diferenca lista 1 - lista 2
diferenca([],_,[]).
diferenca([H|T],L,R):- membro(L,H), !, diferenca(T,L,R).
diferenca([H|T],L,[H|T2]):- diferenca(T,L,T2).

% 	gera o complemento de um grafo
criaComplemento([],_,[]).
criaComplemento([no(V,L1)|T1], L2, [no(V,L3)|T2]):- criaComplemento(T1,L2,T2), diferenca(L2,[V|L1],L3).

complemento(G,G2):- listaVertices(G,L), criaComplemento(G,L,G2). 

%	verifica a ocorrencia de lacos
temLaco([no(V,L)|_]):- membro(L,V).
temLaco([_|T]):- temLaco(T).

temDuplaAresta([no(V,_)|_]):- aresta(V,X), aresta(X,V).
temDuplaAresta([_|T]):- temDuplaAresta(T).

multigrafo(G):- temLaco(G); temDuplaAresta(G), write("Multigrafo"), nl, !.
multigrafo(_):- write("Nao Multigrafo"), nl.

%	remove vertices do grafo pela lista dada
removeVertices([],_,[]).
removeVertices([no(V,_)|G],L2,T):- membro(L2,V),!, removeVertices(G,L2,T).
removeVertices([no(V,L)|G],L2,[no(V,L)|T]):- removeVertices(G,L2,T).

componentesConex([],0).
componentesConex([no(V,L)|T], N):- buscaProf([no(V,L)|T], V, L2), removeVertices([no(V,L)|T], L2, G), componentesConex(G, N2), N = N2 + 1.