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
nao( _ ).

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

last([X],X).
last([_|T],X):- last(T,X).

first([X],X).
first([H|_],H).

concat([], List, List).
concat([Head|Tail], List, [Head|Rest]) :-
    concat(Tail, List, Rest).

%-------------------------------------------------------------

litrosContentores([],0).
litrosContentores([H],R):- contentor(H,_,_,_,_,_,_,R).
litrosContentores([H|T],R):- litrosContentores(T,R1),contentor(H,_,_,_,_,_,_,X), R is R1+X.

contentoresRua(ID,L,R):- (solucoes(C,(contentorRua(C,ID),(contentor(C,_,_,T,_,_,_,_),pertence(T,L);contentor(C,_,_,T,_,_,_,_),comprimento(L,0))),R)).

litrosRua(ID,L,R):- contentoresRua(ID,L,C),litrosContentores(C,R).

litrosRuas([],_,0).
litrosRuas([H|T],L,R):-litrosRuas(T,L,R1),litrosRua(H,L,C), R is R1+C.

contentoresTipos(_,[]).
contentoresTipos([H],Tipos):-contentor(H,_,_,Tipo,_,_,_,_),pertence(Tipo,Tipos).
contentoresTipos([H|_],Tipos):- contentor(H,_,_,Tipo,_,_,_,_),pertence(Tipo,Tipos).
contentoresTipos([_|T],Tipos):- contentoresTipos(T,Tipos).

ruaTipos(ID,Tipos):-(solucoes(C,contentorRua(C,ID),Cont)),contentoresTipos(Cont,Tipos).

ruasTipos([H|_],Tipos):-ruaTipos(H,Tipos).
ruasTipos([_|T],Tipos):-ruasTipos(T,Tipos).

adjComTipo([H|_],Tipos,H):- ruaTipos(H,Tipos). 
adjComTipo([_|T],Tipos,X):- adjComTipo(T,Tipos,X). 

deposicao(100).
garagem(0).

%--------------------------------------------------------------------------------------------------
resolve_pp(Lixos,Final) :- profundidadeprimeiro1Inicial(0,[0],C1,Lixos),last(C1,A),profundidadeprimeiro1Recolha(A,[0|C1],Caminho,Lixos),
                    concat([0|C1],Caminho,Middle),profundidadeprimeiro1Regresso(100,[100],CRegresso,Lixos),concat(Middle,CRegresso,Final),
                    write(C1),write('\n'),write(Caminho),write('\n'),write(CRegresso).

resolve_pp2(Nodo,[Nodo|Caminho],Lixos):-profundidadeprimeiro1Inicial(Nodo,[Nodo],Caminho,Lixos).

profundidadeprimeiro1Inicial(Nodo,_,[],Lixos):-ruaTipos(Nodo,Lixos),not(garagem(Nodo)).

profundidadeprimeiro1Inicial(Nodo,Historico,[ProxNodo|Caminho],Lixos) :- adjacenteSimples(Nodo,ProxNodo),
                                                                        nao(membro(ProxNodo,Historico)),
                                                                        profundidadeprimeiro1Inicial(ProxNodo,[ProxNodo|Historico],Caminho,Lixos).

profundidadeprimeiro1Recolha(Nodo,_,[],Lixos):-deposicao(Nodo).

profundidadeprimeiro1Recolha(Nodo,Historico,[ProxNodo|Caminho],Lixos) :-(solucoes(ID,adjacenteSimples(Nodo,ID),Adj)),
                                                                        ((comprimento(Lixos,Comp),Comp>0,ruasTipos(Adj,Lixos),adjComTipo(Adj,Lixos,ProxNodo));
                                                                        adjacenteSimples(Nodo,ProxNodo)),
                                                                        nao(membro(ProxNodo,Historico)),
                                                                        profundidadeprimeiro1Recolha(ProxNodo,[ProxNodo|Historico],Caminho,Lixos).

profundidadeprimeiro1Regresso(Nodo,_,[],Lixos):-garagem(Nodo).

profundidadeprimeiro1Regresso(Nodo,Historico,[ProxNodo|Caminho],Lixos) :- adjacenteSimples(Nodo,ProxNodo),
                                                                        nao(membro(ProxNodo,Historico)),
                                                                        profundidadeprimeiro1Regresso(ProxNodo,[ProxNodo|Historico],Caminho,Lixos).


toCapacity(X,X):-litrosRuas(X,[],C),C=<15000.
toCapacity(X,R):-deleteLastElement(X,A),toCapacity(A,R).

adjacenteSimples(A,B) :- arco(A,B,_).
adjacenteSimples(A,B) :- arco(B,A,_).

adjacente(A,B,C) :- arco(A,B,C).
adjacente(A,B,C) :- arco(B,A,C).

%--------------------------------------------------------------------------------------------------
resolve_gulosa(CaminhoFinal/CustoTotal) :-
    litrosRua(0,[],Estima),
    agulosa_Recolha([[0]/0/Estima],InvCaminho/Custo/_),
    inverso(InvCaminho,Caminho),
    litrosRua(100,[],Estima2),
    agulosa_Regresso([[100]/0/Estima2],InvCaminho2/Custo2/_),
    inverso(InvCaminho2,Caminho2),
    CustoTotal is Custo+Custo2,
    concat(Caminho,Caminho2,CaminhoFinal).




agulosa_Recolha(Caminhos,Caminho) :-
    obtem_melhor_g_Recolha(Caminhos,Caminho),
    Caminho = [Nodo|_]/_/_,
    deposicao(Nodo).

agulosa_Recolha(Caminhos,SolucaoCaminho):-
    obtem_melhor_g_Recolha(Caminhos,MelhorCaminho),
    seleciona(MelhorCaminho,Caminhos,OutrosCaminhos),
    expande_gulosa_Recolha(MelhorCaminho,ExpCaminhos),
    append(OutrosCaminhos,ExpCaminhos,NovoCaminhos),
    agulosa_Recolha(NovoCaminhos,SolucaoCaminho).

obtem_melhor_g_Recolha([Caminho],Caminho) :- !.

obtem_melhor_g_Recolha([Caminho1/Custo1/Est1,Caminho2/Custo2/Est2|Caminhos],MelhorCaminho) :- 
                        Est1 =< Est2, !,
                        obtem_melhor_g_Recolha([Caminho2/Custo2/Est2|Caminhos],MelhorCaminho).

obtem_melhor_g_Recolha([_|Caminhos],MelhorCaminho) :- obtem_melhor_g_Recolha(Caminhos,MelhorCaminho).


expande_gulosa_Recolha(Caminho,ExpCaminhos):-
    solucoes(NovoCaminho, adjacente3_Recolha(Caminho,NovoCaminho), ExpCaminhos).


adjacente3_Recolha([Nodo|Caminho]/Custo/_,[ProxNodo,Nodo|Caminho]/NovoCusto/Est):-
    adjacente(Nodo,ProxNodo,PassoCusto),\+membro(ProxNodo,Caminho),
    NovoCusto is Custo + PassoCusto,
    litrosRua(ProxNodo,[],Est).





agulosa_Regresso(Caminhos,Caminho) :-
    obtem_melhor_g_Regresso(Caminhos,Caminho),
    Caminho = [Nodo|_]/_/_,
    garagem(Nodo).

agulosa_Regresso(Caminhos,SolucaoCaminho):-
    obtem_melhor_g_Regresso(Caminhos,MelhorCaminho),
    seleciona(MelhorCaminho,Caminhos,OutrosCaminhos),
    expande_gulosa_Regresso(MelhorCaminho,ExpCaminhos),
    append(OutrosCaminhos,ExpCaminhos,NovoCaminhos),
    agulosa_Regresso(NovoCaminhos,SolucaoCaminho).


obtem_melhor_g_Regresso([Caminho],Caminho) :- !.

obtem_melhor_g_Regresso([Caminho1/Custo1/Est1,Caminho2/Custo2/Est2|Caminhos],MelhorCaminho) :- 
                        Est1 =< Est2, !,
                        obtem_melhor_g_Regresso([Caminho1/Custo1/Est1|Caminhos],MelhorCaminho).

obtem_melhor_g_Regresso([_|Caminhos],MelhorCaminho) :- obtem_melhor_g_Regresso(Caminhos,MelhorCaminho).





expande_gulosa_Regresso(Caminho,ExpCaminhos):-
    solucoes(NovoCaminho, adjacente3_Regresso(Caminho,NovoCaminho), ExpCaminhos).


adjacente3_Regresso([Nodo|Caminho]/Custo/_,[ProxNodo,Nodo|Caminho]/NovoCusto/Custo):-
    adjacente(Nodo,ProxNodo,PassoCusto),\+membro(ProxNodo,Caminho),
    NovoCusto is Custo + PassoCusto.