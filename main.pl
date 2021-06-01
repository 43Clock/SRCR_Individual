%Declaracoes iniciaias

:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

%Definicoes iniciais
:- op( 900,xfy,'::' ).
:- use_module(library(lists)).

:- include('./arcos.pl').
:- include('./contentores.pl').
:- include('./contentorRua.pl').
:- include('./ruas.pl').

%--------------- Predicados --------------------------------

solucoes(X,Y,Z) :- findall(X,Y,Z).

inverso(X,Y):-reverse(X,Y).

nao( Questao ) :-
    Questao, !, fail.
nao( Questao ).

membro(X, [X|_]).
membro(X, [_|Xs]):-membro(X, Xs).

escrever([]).

escrever([H|T]) :-

    write(H), write("\n"), 

    escrever(T).

seleciona(E,[E|Xs],Xs).

seleciona(E,[X|Xs],[X|Ys]):- seleciona(E,Xs,Ys).

pertence( X,[X|_] ).
pertence( X,[Y|L] ) :-
    X \= Y,
    pertence( X,L ).

comprimento( [],0 ).
comprimento( [_|L],N ) :-
    comprimento( L,N1 ),
    N is N1+1.


%-------------------------------------------------------------

litrosContentores([],0).
litrosContentores([H],R):- contentor(H,_,_,_,_,_,_,R).
litrosContentores([H|T],R):- litrosContentores(T,R1),contentor(H,_,_,_,_,_,_,X), R is R1+X.

contentoresRua(ID,L,R):- (solucoes(C,(contentorRua(C,ID),(contentor(C,_,_,T,_,_,_,_),pertence(T,L);contentor(C,_,_,T,_,_,_,_),comprimento(L,0))),R)).

litrosRua(ID,L,R):- contentoresRua(ID,L,C),litrosContentores(C,R).

deposicao(100).
garagem(0).

resolve_pp(Nodo,[Nodo|Caminho]) :- profundidadeprimeiro1Recolha(Nodo,[Nodo],Caminho,Capacidade).

profundidadeprimeiro1Recolha(Nodo,_,[],0):-deposicao(Nodo).

profundidadeprimeiro1Recolha(Nodo,_,_,Capacidade):-adjacente(Nodo,ProxNodo),litrosRua(ProxNodo,[],R),Capacidade+R>15000.

profundidadeprimeiro1Recolha(Nodo,Historico,[ProxNodo|Caminho],Capacidade) :- adjacente(Nodo,ProxNodo),
                                                                              nao(membro(ProxNodo,Historico)),
                                                                              profundidadeprimeiro1Recolha(ProxNodo,[ProxNodo|Historico],Caminho,C1),
                                                                              litrosRua(Nodo,[],C), Capacidade is C1+C.

adjacente(A,B) :- arco(A,B,_).
adjacente(A,B) :- arco(B,A,_).