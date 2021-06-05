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

ruaTipos(0,_).
ruaTipos(100,_).
ruaTipos(ID,Tipos):-(solucoes(C,contentorRua(C,ID),Cont)),contentoresTipos(Cont,Tipos).

ruasTipos([H|_],Tipos):-ruaTipos(H,Tipos).
ruasTipos([_|T],Tipos):-ruasTipos(T,Tipos).

checkTipos(_,[]).
checkTipos([H],Tipos):-ruaTipos(H,Tipos).
checkTipos([H|T],Tipos):-ruaTipos(H,Tipos),checkTipos(T,Tipos).

checkCaminhos(_,[]).
checkCaminhos(C/_/_,Tipos):-checkTipos(C,Tipos).

filtraLista([],[],_).
filtraLista([H|T],[H|X],Tipos):-checkCaminhos(H,Tipos),filtraLista(T,X,Tipos).
filtraLista([_|T],X,Tipos):-filtraLista(T,X,Tipos).

adjComTipo([H],Tipos,H):- ruaTipos(H,Tipos).
adjComTipo([H|_],Tipos,H):- ruaTipos(H,Tipos). 
adjComTipo([_|T],Tipos,X):- adjComTipo(T,Tipos,X). 

deposicao(100).
garagem(0).

%--------------------------------------------------------------------------------------------------
resolve_pp(Lixos,Final/Custo/Litros) :- profundidadeprimeiro_Recolha(0,[0],Caminho,Lixos,Custo1),
                                    profundidadeprimeiro_Regresso(100,[100],CRegresso,Lixos,Custo2),
                                    concat([0|Caminho],CRegresso,Final),
                                    litrosRuas(Caminho,Lixos,Litros),
                                    Custo is Custo1+Custo2,write([0|Caminho]),write('\n'),write(CRegresso).


profundidadeprimeiro_Recolha(Nodo,_,[],_,0):-deposicao(Nodo).

profundidadeprimeiro_Recolha(Nodo,Historico,[ProxNodo|Caminho],Lixos,Custo) :- adjacenteSimples(Nodo,ProxNodo),
                                                                            (solucoes(ID,adjacenteSimples(Nodo,ID),Adj)),
                                                                            ((comprimento(Lixos,Comp),Comp>0,ruaTipos(ProxNodo,Lixos));
                                                                            (comprimento(Lixos,A),A =:= 0,adjacenteSimples(Nodo,ProxNodo));
                                                                            (comprimento(Lixos,Cmp),Cmp>0,nao(ruaTipos(ProxNodo,Lixos)),!,fail)),
                                                                            nao(membro(ProxNodo,Historico)),
                                                                            profundidadeprimeiro_Recolha(ProxNodo,[ProxNodo|Historico],Caminho,Lixos,C1),
                                                                            adjacente(Nodo,ProxNodo,C),
                                                                            Custo is C1+C.

profundidadeprimeiro_Regresso(Nodo,_,[],_,0):-garagem(Nodo).

profundidadeprimeiro_Regresso(Nodo,Historico,[ProxNodo|Caminho],Lixos,Custo) :- adjacenteSimples(Nodo,ProxNodo),
                                                                                nao(membro(ProxNodo,Historico)),
                                                                                profundidadeprimeiro_Regresso(ProxNodo,[ProxNodo|Historico],Caminho,Lixos,C1),
                                                                                adjacente(Nodo,ProxNodo,C),
                                                                                Custo is C1+C.
                                                                                

adjacenteSimples(A,B) :- arco(A,B,_).
adjacenteSimples(A,B) :- arco(B,A,_).

adjacente(A,B,C) :- arco(A,B,C).
adjacente(A,B,C) :- arco(B,A,C).

%--------------------------------------------------------------------------------------------------
resolve_pp_limitada(Lixos,Max,Final/Custo/Litros) :- profundidadeprimeiroLimitada_Recolha(0,[0],Caminho,Lixos,Custo1,Max),
                                                     profundidadeprimeiroLimitada_Regresso(100,[100],CRegresso,Lixos,Custo2,Max),
                                                     concat([0|Caminho],CRegresso,Final),
                                                     litrosRuas(Caminho,Lixos,Litros),
                                                     Custo is Custo1+Custo2,write([0|Caminho]),write('\n'),write(CRegresso).


profundidadeprimeiroLimitada_Recolha(Nodo,_,[],_,0,_):-deposicao(Nodo).

profundidadeprimeiroLimitada_Recolha(Nodo,Historico,[ProxNodo|Caminho],Lixos,Custo,Max) :- 
                                                                            adjacenteSimples(Nodo,ProxNodo),
                                                                            comprimento([ProxNodo|Caminho],M),
                                                                            ((M=<Max);((M>Max),!,fail)),
                                                                            (solucoes(ID,adjacenteSimples(Nodo,ID),Adj)),
                                                                            ((comprimento(Lixos,Comp),Comp>0,ruaTipos(ProxNodo,Lixos));
                                                                            (comprimento(Lixos,A),A =:= 0,adjacenteSimples(Nodo,ProxNodo));
                                                                            (comprimento(Lixos,Cmp),Cmp>0,nao(ruaTipos(ProxNodo,Lixos)),!,fail)),
                                                                            nao(membro(ProxNodo,Historico)),
                                                                            profundidadeprimeiroLimitada_Recolha(ProxNodo,[ProxNodo|Historico],Caminho,Lixos,C1,Max),
                                                                            adjacente(Nodo,ProxNodo,C),
                                                                            Custo is C1+C.

profundidadeprimeiroLimitada_Regresso(Nodo,_,[],_,0,_):-garagem(Nodo).

profundidadeprimeiroLimitada_Regresso(Nodo,Historico,[ProxNodo|Caminho],Lixos,Custo,Max) :- 
                                                                                adjacenteSimples(Nodo,ProxNodo),
                                                                                comprimento([ProxNodo|Caminho],M),
                                                                                ((M=<Max);((M>Max),!,fail)),
                                                                                nao(membro(ProxNodo,Historico)),
                                                                                profundidadeprimeiroLimitada_Regresso(ProxNodo,[ProxNodo|Historico],Caminho,Lixos,C1,Max),
                                                                                adjacente(Nodo,ProxNodo,C),
                                                                                Custo is C1+C.


%--------------------------------------------------------------------------------------------------

resolve_gulosa(Lixos,CaminhoFinal/CustoTotal/Litros) :-
    estimaDeposicao(0,Estima),
    agulosa_Recolha([[0]/0/Estima],InvCaminho/Custo/_,Lixos),
    inverso(InvCaminho,Caminho),
    estimaGaragem(100,Estima2),
    agulosa_Regresso([[100]/0/Estima2],InvCaminho2/Custo2/_),
    inverso(InvCaminho2,Caminho2),
    CustoTotal is Custo+Custo2,
    concat(Caminho,Caminho2,CaminhoFinal),
    litrosRuas(Caminho,Lixos,Litros),
    write(Caminho),write('\n'),write(Caminho2).


obtem_melhor_g([Caminho],Caminho) :- !.

obtem_melhor_g([Caminho1/Custo1/Est1,_/_/Est2|Caminhos],MelhorCaminho) :- 
                        Est1 =< Est2, !, 
                        obtem_melhor_g([Caminho1/Custo1/Est1|Caminhos],MelhorCaminho).

obtem_melhor_g([_|Caminhos],MelhorCaminho) :- obtem_melhor_g(Caminhos,MelhorCaminho).


agulosa_Recolha(Caminhos,Caminho,_) :-
    obtem_melhor_g(Caminhos,Caminho),
    Caminho = [Nodo|_]/_/_,
    deposicao(Nodo).

agulosa_Recolha(Caminhos,SolucaoCaminho,Lixos):-
    obtem_melhor_g(Caminhos,MelhorCaminho),
    seleciona(MelhorCaminho,Caminhos,OutrosCaminhos),
    expande_gulosa_Recolha(MelhorCaminho,ExpCaminhos,Lixos),
    append(OutrosCaminhos,ExpCaminhos,NovoCaminhos),
    agulosa_Recolha(NovoCaminhos,SolucaoCaminho,Lixos).


expande_gulosa_Recolha(Caminho,ExpCaminhos,Lixos):-
    solucoes(NovoCaminho, adjacente_Recolha(Caminho,NovoCaminho), ExpCaminhosT),
    filtraLista(ExpCaminhosT,ExpCaminhos,Lixos).


adjacente_Recolha([Nodo|Caminho]/Custo/_,[ProxNodo,Nodo|Caminho]/NovoCusto/Est):-
    adjacente(Nodo,ProxNodo,PassoCusto),nao(membro(ProxNodo,Caminho)),
    NovoCusto is Custo + PassoCusto,
    estimaDeposicao(ProxNodo,Est).


agulosa_Regresso(Caminhos,Caminho) :-
    obtem_melhor_g(Caminhos,Caminho),
    Caminho = [Nodo|_]/_/_,
    garagem(Nodo).

agulosa_Regresso(Caminhos,SolucaoCaminho):-
    obtem_melhor_g(Caminhos,MelhorCaminho),
    seleciona(MelhorCaminho,Caminhos,OutrosCaminhos),
    expande_gulosa_Regresso(MelhorCaminho,ExpCaminhos),
    append(OutrosCaminhos,ExpCaminhos,NovoCaminhos),
    agulosa_Regresso(NovoCaminhos,SolucaoCaminho).


expande_gulosa_Regresso(Caminho,ExpCaminhos):-
    solucoes(NovoCaminho, adjacente_Regresso(Caminho,NovoCaminho), ExpCaminhos).
    
    
adjacente_Regresso([Nodo|Caminho]/Custo/_,[ProxNodo,Nodo|Caminho]/NovoCusto/Est):-
    adjacente(Nodo,ProxNodo,PassoCusto),nao(membro(ProxNodo,Caminho)),
    NovoCusto is Custo + PassoCusto,
    estimaGaragem(ProxNodo,Est).

%--------------------------------------------------------------------------------------------------
resolve_aestrela(Lixos,CaminhoFinal/CustoTotal/Litros):-
        estimaDeposicao(0,Estima),
        aestrela_Recolha([[0]/0/Estima],InvCaminho/Custo/_,Lixos),
        inverso(InvCaminho,Caminho),
        estimaGaragem(100,Estima2),
        aestrela_Regresso([[100]/0/Estima2],InvCaminho2/Custo2/_),
        inverso(InvCaminho2,Caminho2),
        CustoTotal is Custo+Custo2,
        concat(Caminho,Caminho2,CaminhoFinal),
        litrosRuas(Caminho,Lixos,Litros),
        write(Caminho),write('\n'),write(Caminho2).

aestrela_Recolha(Caminhos,Caminho,_):-obtem_melhor(Caminhos,Caminho),
                                    Caminho = [Nodo|_]/_/_,
                                    deposicao(Nodo).

aestrela_Recolha(Caminhos,SolucaoCaminho,Lixos):-
        obtem_melhor(Caminhos,MelhorCaminho),
        seleciona(MelhorCaminho,Caminhos,OutrosCaminhos),
        expande_aestrela_Recolha(MelhorCaminho,ExpCaminhos,Lixos),
        append(OutrosCaminhos,ExpCaminhos,NovoCaminhos),
        aestrela_Recolha(NovoCaminhos,SolucaoCaminho,Lixos).

expande_aestrela_Recolha(Caminho,ExpCaminhos,Lixos):-
        solucoes(NovoCaminho,adjacente_Recolha(Caminho,NovoCaminho),ExpCaminhosT),
        filtraLista(ExpCaminhosT,ExpCaminhos,Lixos).



aestrela_Regresso(Caminhos,Caminho):-obtem_melhor(Caminhos,Caminho),
                                    Caminho = [Nodo|_]/_/_,
                                    garagem(Nodo).

aestrela_Regresso(Caminhos,SolucaoCaminho):-
        obtem_melhor(Caminhos,MelhorCaminho),
        seleciona(MelhorCaminho,Caminhos,OutrosCaminhos),
        expande_aestrela_Regresso(MelhorCaminho,ExpCaminhos),
        append(OutrosCaminhos,ExpCaminhos,NovoCaminhos),
        aestrela_Regresso(NovoCaminhos,SolucaoCaminho).

expande_aestrela_Regresso(Caminho,ExpCaminhos):-
        solucoes(NovoCaminho,adjacente_Recolha(Caminho,NovoCaminho),ExpCaminhos).


obtem_melhor([Caminho],Caminho):- !.

obtem_melhor([Caminho1/Custo1/Est1,_/Custo2/Est2|Caminhos],MelhorCaminho):-
            Custo1 + Est1 =< Custo2 + Est2,!,
            obtem_melhor([Caminho1/Custo1/Est1|Caminhos],MelhorCaminho).

obtem_melhor([_|Caminhos],MelhorCaminho):-
    obtem_melhor(Caminhos,MelhorCaminho).