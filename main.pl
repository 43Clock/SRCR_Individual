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


deleteLastElement([],[]).
deleteLastElement([_], []).
deleteLastElement([Head, Next|Tail], [Head|NTail]):-
  deleteLastElement([Next|Tail], NTail).

add_tail([],X,[X]).
add_tail([H|T],X,[H|L]):-add_tail(T,X,L).

%-------------------------------------------------------------

litrosContentores([],0).
litrosContentores([H],R):- contentor(H,_,_,_,_,_,_,R).
litrosContentores([H|T],R):- litrosContentores(T,R1),contentor(H,_,_,_,_,_,_,X), R is R1+X.

contentoresRua(ID,L,R):- (solucoes(C,(contentorRua(C,ID),(contentor(C,_,_,T,_,_,_,_),pertence(T,L);contentor(C,_,_,T,_,_,_,_),comprimento(L,0))),R)).

litrosRua(ID,L,R):- contentoresRua(ID,L,C),litrosContentores(C,R).

litrosRuas([],_,0).
litrosRuas([H|T],L,R):-litrosRuas(T,L,R1),litrosRua(H,L,C), R is R1+C.

contentoresTipos([H],Tipos):-contentor(H,_,_,Tipo,_,_,_,_),pertence(Tipo,Tipos).
contentoresTipos([H|T],Tipos):- contentor(H,_,_,Tipo,_,_,_,_),pertence(Tipo,Tipos).
contentoresTipos([H|T],Tipos):- contentoresTipos(T,Tipos).

ruaTipos(ID,Tipos):-(solucoes(C,contentorRua(C,ID),Cont)),contentoresTipos(Cont,Tipos).

deposicao(100).
garagem(0).

resolve_pp(Nodo,Lixos,[Nodo|Trunc]) :- profundidadeprimeiro1Recolha(Nodo,[Nodo],Caminho,Lixo),toCapacity(Caminho,Trunc).

resolve_pp2(Nodo,Lixos,[Nodo|Caminho]) :- profundidadeprimeiro1Inicial(Nodo,[Nodo],Caminho,Lixo).

profundidadeprimeiro1Inicial(Nodo,_,[],Lixos):-ruaTipos(Nodo,Lixos).

profundidadeprimeiro1Inicial(Nodo,Historico,[ProxNodo|Caminho],Lixos) :- adjacente(Nodo,ProxNodo),
                                                                        nao(membro(ProxNodo,Historico)),
                                                                        profundidadeprimeiro1Inicial(ProxNodo,[ProxNodo|Historico],Caminho,Lixos).

profundidadeprimeiro1Recolha(Nodo,_,[],Lixos):-deposicao(Nodo).

profundidadeprimeiro1Recolha(Nodo,Historico,[ProxNodo|Caminho],Lixos) :- adjacente(Nodo,ProxNodo),
                                                                        nao(membro(ProxNodo,Historico)),
                                                                        profundidadeprimeiro1Recolha(ProxNodo,[ProxNodo|Historico],Caminho,Lixos).

toCapacity(X,X):-litrosRuas(X,[],C),C=<15000.
toCapacity(X,R):-deleteLastElement(X,A),toCapacity(A,R).






adjacente(A,B) :- arco(A,B,_).
adjacente(A,B) :- arco(B,A,_).