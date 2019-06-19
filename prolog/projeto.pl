:- set_prolog_flag(verbose, silent).
:- [mensagens].
:- initialization(main).

escreve_mensagem(NumeroMensagem) :-
  get_mensagem(NumeroMensagem, M),
  write(M).

creditos :- halt.

sair :- halt.

start :- halt.

menu :-
  escreve_mensagem(11),
  read(Escolha),
  shell(clear),
  (((((Escolha = 1) ->
  start) ;
  (Escolha = 2) ->
  creditos) ;
  (Escolha = 3) ->
  sair) ;
  menu).

init :-
  escreve_mensagem(0),
  menu.

main :-
  shell(clear),
  init.