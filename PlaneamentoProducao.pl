%F�BRICA

% Linhas

linhas([lA]).


% Maquinas


maquinas([ma]).



% Ferramentas


ferramentas([fa,fa,fc]).


% Maquinas que constituem as Linhas

tipos_maq_linha(lA,[ma]).
% ...


% Opera��es

tipo_operacoes([opt1,opt2,opt3,opt4,opt5]).


% Afeta��o de tipos de opera��es a tipos de m�quinas
% com ferramentas, tempos de setup e tempos de execucao)

operacao_maquina(opt1,ma,fa,5,60).
operacao_maquina(opt2,ma,fb,6,30).
operacao_maquina(opt3,ma,fc,8,40).

%operacoes_atrib_maq depois deve ser criado dinamicamente
operacoes_atrib_maq(ma,[op1,op2,op3,op4,op5,op6,op7]).

% classif_operacoes/2 deve ser criado dinamicamente %%atomic_concat(op,NumOp,Resultado)
classif_operacoes(op1,opt1).
classif_operacoes(op2,opt2).
classif_operacoes(op3,opt1).
classif_operacoes(op4,opt2).
classif_operacoes(op5,opt3).
classif_operacoes(op6,opt3).
classif_operacoes(op7,opt2).



% PRODUTOS

produtos([pA,pB,pC]).

operacoes_produto(pA,[opt1]).
operacoes_produto(pB,[opt2]).
operacoes_produto(pC,[opt3]).


op_prod_client(op1,ma,fa,pA,clA,1,20,5,60).
op_prod_client(op2,ma,fb,pB,clA,1,500,6,30).
op_prod_client(op5,ma,fc,pC,clA,1,20,8,40).


% ENCOMENDAS

%Clientes

clientes([clA,clB]).


% prioridades dos clientes

prioridade_cliente(clA,1).
prioridade_cliente(clB,2).
% ...

% Encomendas do cliente,
% termos e(<produto>,<n.unidades>,<tempo_conclusao>)

encomenda(clA,[e(pA,1,20),e(pB,1,500),e(pC,1,20)]).
encomenda(clB,[e(pA,1,110),e(pB,1,150),e(pC,1,300)]).









/* ########### MELHOR ESCALONAMENTOS DOS TEMPOS DE OCUPA�AO ####################*/
:- dynamic melhor_sol_to/2.
melhor_escalonamento1(M,Lm,Tm):-(melhor_escalonamento11(M);true),
				retract(melhor_sol_to(Lm,Tm)).
melhor_escalonamento11(M):- asserta(melhor_sol_to(_,10000)),!,
				permuta_tempo(M,LP,Tempo),
				atualiza(LP,Tempo),
				fail.
atualiza(LP,T):-melhor_sol_to(_,Tm),
				T<Tm,
                                retract(melhor_sol_to(_,_)),
                                asserta(melhor_sol_to(LP,T)),!.
% permuta/2 gera permuta��es de listas
permuta([ ],[ ]).
permuta(L,[X|L1]):-
  apaga1(X,L,Li),
  permuta(Li,L1).
apaga1(X,[X|L],L).
apaga1(X,[Y|L],[Y|L1]):-
  apaga1(X,L,L1).
% permuta_tempo/3 faz uma permuta��o das opera��es atribu�das a uma maquina e calcula tempo de ocupa��o incluindo trocas de ferramentas
permuta_tempo(M,LP,Tempo):- operacoes_atrib_maq(M,L),
				permuta(L,LP),
				soma_tempos(semfer,M,LP,Tempo).
soma_tempos(_,_,[],0).
soma_tempos(Fer,M,[Op|LOp],Tempo):- classif_operacoes(Op,Opt),
	operacao_maquina(Opt,M,Fer1,Tsetup,Texec),
	soma_tempos(Fer1,M,LOp,Tempo1),
	((Fer1==Fer,!,Tempo is Texec+Tempo1);
        Tempo is Tsetup+Texec+Tempo1).










/* ############### HEURISTICA TEMPO DE OCUPA�AO ########################*/
/* h_m_tempo_ocupacao/3 Heur�stica que minimiza o tempo de ocupa��o das opera��es de uma m�quina M.
Coloca lista de opera��es e tempo de ocupa��o em Lm e Tm respetivamente*/
h_m_tempo_ocupacao(M,Lm,Tm):-operacoes_atrib_maq(M,LOp),
                h_m_tempo_ocupacao1(M,LOp,Lm,Tm,sem_ferramenta),
                soma_tempos(sem_ferramenta,M,Lm,Tm),!.
h_m_tempo_ocupacao1(_,[],_,_,_):-!.
h_m_tempo_ocupacao1(M,Lop,Lm,Tm,F):-get_op_ideal(M,Lop,F,Op),
                delete_one(Op,Lop,LopN),
                classif_operacoes(Op,Opt),operacao_maquina(Opt,M,Fn,_,_),
                h_m_tempo_ocupacao1(M,LopN,Ln,Tm,Fn),append([Op],Ln,Lm).
/* get_op_ideal/4 vai buscar a operacao ideal sendo dada a maquina, uma lista de operacoes e
a ferramente inserida na maquina. Vai buscar sempre que possivel uma operacao que use a mesma
ferramenta */
get_op_ideal(_,[Op|[]],_,Op):-!.
get_op_ideal(M,[H|LOp],F,Op):-classif_operacoes(H,Opt),
  operacao_maquina(Opt,M,Fn,_,_),
  (Fn==F,Op=H;
  get_op_ideal(M,LOp,F,Op)).












/* ############# A STAR TEMPO DE OCUPA�AO ###############*/
aStar(M,Cam,Custo):-findall(Opt,operacao_maquina(Opt,M,_,_,_),[Ltratados|Lfaltam]),
				operacao_maquina(Ltratados,_,_,CustoSet,CustoX),
				aStar2([(_,CustoSet+CustoX,[Ltratados],Lfaltam)],Cam,Custo).
aStar2([(_,Custo,Ltratados,[])|_],Cam,Custo):-reverse(Ltratados,Cam),!.
aStar2([(_,Ca,Ltratados,Lfaltam)|Outros],Cam,Custo):-Ltratados=[Act|_],
  findall((CEX,CaX,[X|Ltratados],Lfaltam2),
          (operacao_maquina(X,_,_,CustoSet,CustoX),
           member(X,Lfaltam),somaCaX(Act,X,Ca,CustoSet,CustoX,CaX),
           delete_one(X,Lfaltam,Lfaltam2),estimativa(Lfaltam2,Est),
           CEX is Est + CaX),
          Novos),
  append(Outros,Novos,Todos),
  sort(Todos,TodosOrd),
  aStar2(TodosOrd,Cam,Custo).
estimativa(LOp,Estimativa):-findall(p(FOp,Tsetup),
                                    (member(Op,LOp),
                                     operacao_maquina(Op,_,FOp,Tsetup,_)),
                                    LFTsetup),
  elimina_repetidos(LFTsetup,L),
  soma_setups(L,Estimativa).
elimina_repetidos([],[]).
elimina_repetidos([X|L],L1):-member(X,L),!,elimina_repetidos(L,L1).
elimina_repetidos([X|L],[X|L1]):-elimina_repetidos(L,L1).
soma_setups([],0).
soma_setups([p(_,Tsetup)|L],Ttotal):- soma_setups(L,T1), Ttotal is Tsetup+T1.
delete_one(_, [], []).
delete_one(Term, [Term|Tail], Tail):-!.
delete_one(Term, [Head|Tail], [Head|Result]) :-
  delete_one(Term, Tail, Result).
somaCaX(Act,X,Ca,CustoSet,CustoX,CaX):-Act==X,CaX is CustoX+Ca,!;CaX is CustoX+Ca+CustoSet.










/* ########################################## MELHOR ESCALONAMENTO DOS TEMPOS DE ATRASO ###############################################################################*/
:- dynamic melhor_sol_to_atraso/2.
melhor_escalonamento1_atraso(Cliente,Lm,Tm):-(melhor_escalonamento11_atraso(Cliente);true),retract(melhor_sol_to_atraso(Lm,Tm)).
melhor_escalonamento11_atraso(Cliente):- asserta(melhor_sol_to_atraso(_,10000)),!,
				permuta_tempo_atraso(Cliente,LP,Tempo),
				atualiza_atraso(LP,Tempo),
				fail.

atualiza_atraso(LP,T):-melhor_sol_to_atraso(_,Tm),
				T<Tm,retract(melhor_sol_to_atraso(_,_))
                                ,asserta(melhor_sol_to_atraso(LP,T)),!.
% permuta/2 gera permuta��es de listas
permuta_atraso([ ],[ ]).
permuta_atraso(L,[X|L1]):-apaga1_atraso(X,L,Li),permuta_atraso(Li,L1).
apaga1_atraso(X,[X|L],L).
apaga1_atraso(X,[Y|L],[Y|L1]):-apaga1_atraso(X,L,L1).
% permuta_tempo/3 faz uma permuta��o das opera��es atribu�das a uma maquina e calcula tempo de ocupa��o incluindo trocas de ferramentas
permuta_tempo_atraso(Cliente,LP,Tempo):- encomenda(Cliente,Lista),
			   permuta_atraso(Lista,LP),
                           soma_tempos_atraso(semfer,LP,Cliente,Tempo,_),
                           write(LP),nl,write(Tempo),nl.
soma_tempos_atraso(_,[],_,0,0).
soma_tempos_atraso(Fer,[e(Prod,_,_)|OutrasE],Client,TempoAtraso,TempoTotal):-
				op_prod_client(_,_,F,Prod,Client,Qt,TConc,Tsetup,Texec),
				soma_tempos_atraso(F,OutrasE,Client,TempoAtraso1,TempoTotal1),
				(Fer==F,TempoTotal is Texec*Qt+TempoTotal1;
				TempoTotal is Texec*Qt+Tsetup+TempoTotal1),
				(TempoTotal>TConc,!,
                                 TempoAtraso is TempoTotal-TConc+TempoAtraso1;
                                TempoAtraso is TempoAtraso1).












/* ###################### HEURISTICA TEMPO DE ATRASO ###########################*/
/* h_m_tempo_atraso_edd/3. Heur�stica de minimiza��o do tempo de atraso usando EDD (Early-due-date).
Dado o cliente devolve a lista de encomendas ordenadas por tempo de conclusao tal como o a sua soma de tempos de atraso. */
h_m_tempo_atraso_edd(Cliente,Lm,Tm):-
%findall que procura todas as encomendas do cliente no formato e(Produto,Quantidade,TempoConclusao)
				findall(e(Prod,Qtd,TConc)
                                       ,op_prod_client(_,_,_,Prod,Cliente,Qtd,TConc,_,_),
                                        Todos),
                                %ordena lista Todos, pelo o seu 4� elemento(TConc),
                                sort(3,@=<,Todos,Lm),
                                soma_tempos_atraso1(semfer,Lm,Cliente,Tm,_).
soma_tempos_atraso1(_,[],_,0,0).
soma_tempos_atraso1(Fer,[e(Prod,_,_)|OutrasE],Client,TempoAtraso,TempoTotal):-
				op_prod_client(_,_,F,Prod,Client,Qt,TConc,Tsetup,Texec),
				soma_tempos_atraso1(F,OutrasE,Client,TempoAtraso1,TempoTotal1),
				(Fer==F,TempoTotal is Texec*Qt+TempoTotal1;
				TempoTotal is Texec*Qt+Tsetup+TempoTotal1),
				(TempoTotal>TConc,!,
                                 TempoAtraso is TempoTotal-TConc+TempoAtraso1;
                                TempoAtraso is TempoAtraso1).















/* ########################################## A STAR TEMPO DE ATRASO ###############################################################################*/

/* aStar_atraso/3 . Define ordem de realizacao de operacoes de forma a minimizar tempo de atraso usando o m�todo aStar */
aStar_atraso(Cliente,Cam,Custo):-%vai buscar todas as operacoes das encomendas do cliente
				findall(Op,op_prod_client(Op,_,_,_,Cliente,_,_,_,_),Lfaltam),
				%predicado auxiliar que agora recebe cliente e TempoTotal (iniciado a 0)
				aStar2_atraso(Cliente,[(_,0,0,[],Lfaltam)],Cam,Custo).
/* criterio de paragem e quando o Lfaltam for vazia, colocamos no Cam o Ltratados na ordem correta*/
aStar2_atraso(_,[(_,Custo,_,Ltratados,[])|_],Cam,Custo):-reverse(Ltratados,Cam),!.
/* Predicado usado na primeira vez, ou seja quando o Ttotal � 0. Necessario pois aqui nao existe Operacao atual*/
aStar2_atraso(Cliente,[(_,Ca,0,Ltratados,Lfaltam)|Outros],Cam,Custo):-findall((CEX,CaX,TtotalX,[X|Ltratados],Lfaltam2),
             (op_prod_client(X,_,F,_,Cliente,Qt,TConc,CustoSet,CustoX),
             member(X,Lfaltam),soma_CaX_atraso(none,X,Ca,CustoSet,CustoX,TConc,Qt,0,CaX,TtotalX),
              delete_one(X,Lfaltam,Lfaltam2),estimativa_atraso(F,Cliente,Lfaltam2,TtotalX,Est),
             CEX is Est + CaX),
            Novos),
    append(Outros,Novos,Todos),
    sort(Todos,TodosOrd),
    aStar2_atraso(Cliente,TodosOrd,Cam,Custo).
aStar2_atraso(Cliente,[(_,Ca,Ttotal,Ltratados,Lfaltam)|Outros],Cam,Custo):-
  Ltratados=[Act|_],
  findall((CEX,CaX,TtotalX,[X|Ltratados],Lfaltam2),
          (op_prod_client(X,_,F,_,Cliente,Qt,TConc,CustoSet,CustoX),
           member(X,Lfaltam),
           soma_CaX_atraso(Act,X,Ca,CustoSet,CustoX,TConc,Qt,Ttotal,CaX,TtotalX),
           delete_one(X,Lfaltam,Lfaltam2),estimativa_atraso(F,Cliente,Lfaltam2,TtotalX,Est),
           CEX is Est + CaX),
          Novos),
  append(Outros,Novos,Todos),
  sort(Todos,TodosOrd),
  aStar2_atraso(Cliente,TodosOrd,Cam,Custo).
/*soma_CaX_atraso/10 faz a conta para obtermos o novo valor de CaX */
/* Usado quando nao existe operacao anterior, ou seja na primeira itera��o,soma se sempre o Custo de setup*/
soma_CaX_atraso(none,_,Ca,CustoSet,CustoX,TConc,Qt,Ttotal,CaX,TtotalX):-
				TtotalX is CustoX*Qt+CustoSet+Ttotal,
				(TtotalX>TConc,!,CaX is TtotalX-TConc+Ca;CaX is Ca).
soma_CaX_atraso(Act,X,Ca,CustoSet,CustoX,TConc,Qt,Ttotal,CaX,TtotalX):-classif_operacoes(Act,OptAct),classif_operacoes(X,Opt),
				(OptAct==Opt,TtotalX is CustoX*Qt+Ca;
				TtotalX is CustoX*Qt+CustoSet+Ttotal),
				%verifica se o Ttotal � maior que o tempo de conclus�o da encomenda, caso seja, soma-se o tempo de atraso ao Ca
				(TtotalX>TConc,!,CaX is TtotalX-TConc+Ca;CaX is Ca).
/*estimativa_atraso/5 faz a estimativa em rela��o a tempo de atraso, somando o atraso de todas as operacoes restantes caso fossem realizadas no mesmo instante */
estimativa_atraso(_,_,[],_,0):-!.
estimativa_atraso(F,Cliente,[Op|Lop],Ttotal,Estimativa):-estimativa_atraso(F,Cliente,Lop,Ttotal,Estimativa1),
				op_prod_client(Op,_,Fer,_,Cliente,Qt,TConc,Tsetup,Texec),
				(Fer==F,TempoTotal1 is Texec*Qt+Ttotal;
				TempoTotal1 is Texec*Qt+Tsetup+Ttotal),
				(TempoTotal1>TConc,!,Estimativa is TempoTotal1-TConc+Estimativa1;Estimativa is Estimativa1).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).

:- http_handler(/, root_handler, []).


root_handler(_):-
        format('Content-Type: text/html~n~n', []),
                format('Hello from G088: ~w', []).
