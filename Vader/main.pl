%Declaracoes iniciaias

:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

%Definicoes iniciais
:- op( 900,xfy,'::' ).
:- use_module(library(lists)).

:- include('./arcos.pl').
:- include('./pontosRecolha.pl').
:- include('./ruaIdNome.pl').

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

juntaLista([], List, List).
juntaLista([Head|Tail], List, [Head|Rest]) :-
    juntaLista(Tail, List, Rest).

sum([],0).
sum([H|T],R):-sum(T,R1),R is R1+H.

%-------------------------------------------------------------

%Nmr total de litros de um certo tipo num ponto de recolha
capacidadeRuaTipos(ID,[],R):- (solucoes(C,(pontoRecolha(ID,_,C)),Rs)),sum(Rs,R).
capacidadeRuaTipos(ID,L,R):- (solucoes(C,(pontoRecolha(ID,T,C),pertence(T,L)),Rs)),sum(Rs,R).

%Nmr total de litros por tipos de varios pontos de recolha
capacidadeRuasTipos([],_,0).
capacidadeRuasTipos([H|T],L,R):-capacidadeRuasTipos(T,L,R1),capacidadeRuaTipos(H,L,C), R is R1+C.

%Verifica se existe algum elemento da primeira lista na segunda
ocorreTipo(_,[]).
ocorreTipo([H],Tipos):-pertence(H,Tipos).
ocorreTipo([H|_],Tipos):-pertence(H,Tipos).
ocorreTipo([_|T],Tipos):- ocorreTipo(T,Tipos).

%Verifica se algum tipo de lixo de uma certa rua esta numa lista de Tipos de lixo
pontoRecolhaTipos(0,_).
pontoRecolhaTipos(70,_).
pontoRecolhaTipos(ID,Tipos):-(solucoes(C,(pontoRecolha(ID,C,_)),Ts)),ocorreTipo(Ts,Tipos).

%Verifica se , de uma lista de rua, alguma tem um certo tipo de lixo 
pontosDeRecolhaTipo([H|_],Tipos):-pontoRecolhaTipos(H,Tipos).
pontosDeRecolhaTipo([_|T],Tipos):-pontosDeRecolhaTipo(T,Tipos).

%Verifica se todos os pontos do caminho tem pelo menos um dos Tipos de lixo
verificaTipos(_,[]).
verificaTipos([H],Tipos):-pontoRecolhaTipos(H,Tipos).
verificaTipos([H|T],Tipos):-pontoRecolhaTipos(H,Tipos),verificaTipos(T,Tipos).

%Verifica se todos os pontos do caminho tem pelo menos um dos Tipos de lixo
verificaTiposCaminho(_,[]).
verificaTiposCaminho(C/_/_,Tipos):-verificaTipos(C,Tipos).

%Filtra uma lista para conter apenas os caminhos com determinados tipos
filtraLista([],[],_).
filtraLista([H|T],[H|X],Tipos):-verificaTiposCaminho(H,Tipos),filtraLista(T,X,Tipos).
filtraLista([_|T],X,Tipos):-filtraLista(T,X,Tipos).

%De uma lista de pontos adjacentes verifica qual é o que tem um certo tipo de lixo
ruasAdjComTipo([H],Tipos,H):- pontoRecolhaTipos(H,Tipos).
ruasAdjComTipo([H|_],Tipos,H):- pontoRecolhaTipos(H,Tipos). 
ruasAdjComTipo([_|T],Tipos,X):- ruasAdjComTipo(T,Tipos,X). 

deposicao(70).
garagem(0).

%--------------------------------------------------------------------------------------------------
resolve_pp(Lixos,Caminho/Custo1) :- profundidadeprimeiro1Recolha(0,[0],Caminho,Lixos,Custo1).
                                        %profundidadeprimeiro1Regresso(70,[70],CRegresso,Lixos,Custo2),
                                        %juntaLista([0|Caminho],CRegresso,Final),
                                        %capacidadeRuasTipos(Caminho,Lixos,Litros),
                                        %Custo is Custo1+Custo2,write([0|Caminho]),write('\n'),write(CRegresso).


profundidadeprimeiro1Recolha(Nodo,_,[],_,0):-deposicao(Nodo).

profundidadeprimeiro1Recolha(Nodo,Historico,[ProxNodo|Caminho],Lixos,Custo) :-(solucoes(ID,adjacencia(Nodo,ID),Adj)),
                                                                            ((comprimento(Lixos,A),A =:= 0,adjacencia(Nodo,ProxNodo));
                                                                            (comprimento(Lixos,Comp),Comp>0,pontosDeRecolhaTipo(Adj,Lixos),ruasAdjComTipo(Adj,Lixos,ProxNodo));!),
                                                                            nao(membro(ProxNodo,Historico)),
                                                                            profundidadeprimeiro1Recolha(ProxNodo,[ProxNodo|Historico],Caminho,Lixos,C1),
                                                                            adjacenteDistancia(Nodo,ProxNodo,C),
                                                                            Custo is C1+C.

profundidadeprimeiro1Regresso(Nodo,_,[],_,0):-garagem(Nodo).

profundidadeprimeiro1Regresso(Nodo,Historico,[ProxNodo|Caminho],Lixos,Custo) :- adjacencia(Nodo,ProxNodo),
                                                                                nao(membro(ProxNodo,Historico)),
                                                                                profundidadeprimeiro1Regresso(ProxNodo,[ProxNodo|Historico],Caminho,Lixos,C1),
                                                                                adjacenteDistancia(Nodo,ProxNodo,C),
                                                                                Custo is C1+C.
                                                                                
adjacencia(A,B) :- arco(A,B,_).
adjacencia(A,B) :- arco(B,A,_).

adjacenteDistancia(A,B,C) :- arco(A,B,C).
adjacenteDistancia(A,B,C) :- arco(B,A,C).

%--------------------------------------------------------------------------------------------------

resolve_gulosa(Lixos,CaminhoFinal/CustoTotal/Litros) :-
    estimaDeposito(0,Estima),
    agulosa_Recolha([[0]/0/Estima],InvCaminho/Custo/_,Lixos),
    inverso(InvCaminho,Caminho),
    estimaGaragem(70,Estima2),
    agulosa_Regresso([[70]/0/Estima2],InvCaminho2/Custo2/_),
    inverso(InvCaminho2,Caminho2),
    CustoTotal is Custo+Custo2,
    juntaLista(Caminho,Caminho2,CaminhoFinal),
    capacidadeRuasTipos(Caminho,Lixos,Litros),
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
    adjacenteDistancia(Nodo,ProxNodo,PassoCusto),nao(membro(ProxNodo,Caminho)),
    NovoCusto is Custo + PassoCusto,
    estimaDeposito(ProxNodo,Est).


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
    adjacenteDistancia(Nodo,ProxNodo,PassoCusto),nao(membro(ProxNodo,Caminho)),
    NovoCusto is Custo + PassoCusto,
    estimaGaragem(ProxNodo,Est).

%--------------------------------------------------------------------------------------------------
resolve_aestrela(Lixos,CaminhoFinal/CustoTotal/Litros):-
        estimaDeposito(0,Estima),
        aestrela_Recolha([[0]/0/Estima],InvCaminho/Custo/_,Lixos),
        inverso(InvCaminho,Caminho),
        estimaGaragem(70,Estima2),
        aestrela_Regresso([[70]/0/Estima2],InvCaminho2/Custo2/_),
        inverso(InvCaminho2,Caminho2),
        CustoTotal is Custo+Custo2,
        juntaLista(Caminho,Caminho2,CaminhoFinal),
        capacidadeRuasTipos(Caminho,Lixos,Litros),
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