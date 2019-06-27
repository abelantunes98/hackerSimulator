:- set_prolog_flag(verbose, silent).
:- [servidores].
:- [mensagens].
:- initialization(main).

% o predicado diretorio_atual(Dir) unifica Dir com o diretorio atual
% set_diretorio_atual(Dir) fara Dir ser o novo diretorio atual
% Dir eh a lista que tem primeiro o ip do servidor e depois o caminho de diretorios ate o atual
:- dynamic diretorio_atual(Dir).
set_diretorio_atual(Dir) :- retractall(diretorio_atual(_)), asserta(diretorio_atual(Dir)).

% o predicado reset_arquivos_apagados reseta a lista para o inicial
% o arquivo apagado eh representado pela tupla (servidor, nome)
% apaga_arquivo(Arq) adiciona Arq aos arquivos apagados
% nao tem como "desapagar" um arquivo, apenas resetar toda a lista
:- dynamic arquivos_apagados(ListArq).
reset_arquivos_apagados :-
  retractall(arquivos_apagados(_)),
  assertz(arquivos_apagados([])).
apaga_arquivo(Arq) :-
  arquivos_apagados(ListArqAntiga),
  append(ListArqAntiga, [Arq], ListAqrNova),
  retractall(arquivos_apagados(_)),
  assertz(arquivos_apagados(ListAqrNova)).

% set_id_mensagem(Id) fara Id ser a nova mensagem atual
:- dynamic id_mensagem(Id).
set_id_mensagem(Id) :- retractall(id_mensagem), assertz(id_mensagem(Id)).

% set_nao_imprimiu_mensagem define que a mensagem nao foi impressa
% set_imprimiu_mensagem define que a mensagem foi impressa
% ja_imprimiu_mensagem pergunta se a mensagem foi impressa
:- dynamic ja_imprimiu_mensagem.
set_nao_imprimiu_mensagem :- retract(ja_imprimiu_mensagem).
set_imprimiu_mensagem :- assertz(ja_imprimiu_mensagem).

% metodo de leitura customizado pra nao precisar de ponto
readc(Output) :- read_line_to_string(user_input, Output).

% escreve a mensagem de numero Numero na tela
escreve_mensagem(Numero) :-
  get_mensagem(Numero, M),
  write(M).

espera_enter :- 
  write("\nDigite Enter para continuar..."),
  readc(_).




% retorna o diretorio atual do programa (na forma diretorio(nome, subdiretorios, arquivos) podendo unificar se quiser)
retorna_diretorio_atual(DiretorioAtual) :-
  diretorio_atual([IP|[]]),
  DiretorioAtual = diretorio(IP, _, _).

retorna_diretorio_atual(DiretorioAtual) :-
  diretorio_atual([IP|Path]),
  diretorio(IP, Subdiretorios, _),
  retorna_diretorio_atual_t(Subdiretorios, Path, DiretorioAtual).

retorna_diretorio_atual_t(Diretorios, [Name|[]], DirAtual) :-
  retorna_diretorio_de_lista(Name, Diretorios, DirAtual).

retorna_diretorio_atual_t(_, [_|[]], _) :- false.

retorna_diretorio_atual_t(Diretorios, [Name|Path], DiretorioAtual) :-
  retorna_diretorio_de_lista(Name, Diretorios, Dir),
  diretorio(_,Subdirs,_) = Dir,
  retorna_diretorio_atual_t(Subdirs, Path, DiretorioAtual).




% retorna_diretorio_de_lista(Name, Lista, Saida): retorna o diretorio de nome Name em Saida caso esteja em Lista, false se nao estiver.
retorna_diretorio_de_lista(_, [], _) :- false.

retorna_diretorio_de_lista(Name, [Diretorio1|_], Diretorio1) :-
  diretorio(Name,_,_) = Diretorio1.

retorna_diretorio_de_lista(Name, [_|Resto], X) :-
  retorna_diretorio_de_lista(Name, Resto, X).

% aqui o cd
remove_last([], []).
remove_last([_], []).
remove_last([X|Xs], [X|Removed]) :- 
    remove_last(Xs, Removed).

change_directory("..") :-
  diretorio_atual([Dir|[]]).

change_directory("..") :-
  diretorio_atual(Dir),
  remove_last(Dir, NewDir),
  set_diretorio_atual(NewDir).

change_directory(Name) :-
  retorna_diretorio_atual(DirAtual),
  diretorio(_,Subdirs,_) = DirAtual,
  retorna_diretorio_de_lista(Name, Subdirs, _), % essa linha serve para verificar que o dir de destino existe (mas não usa o pŕoprio)
  diretorio_atual(Dir),
  append(Dir, [Name], Out),
  set_diretorio_atual(Out).




% retorna true se o arquivo está apagado.
arquivo_esta_apagado(Servidor, Nome) :-
  arquivos_apagados(Apagados),
  arquivo_esta_apagado_r(Servidor, Nome, Apagados).

arquivo_esta_apagado_r(_, _, []) :- false.

arquivo_esta_apagado_r(Servidor, Nome, [[Servidor, Nome]|_]).

arquivo_esta_apagado_r(Servidor, Nome, [_|Tail]) :-
  arquivo_esta_apagado_r(Servidor, Nome, Tail).


% Connect
connect("").

connect("135.110.60.200") :-
 writeln("Esse é o seu Host.").

connect("112.84.211.124") :-
  set_diretorio_atual(["112.84.211.124", "home"]).

connect("150.189.56.65") :-
  set_diretorio_atual(["150.189.56.65", "home"]).

connect("112.84.211.124") :-
  set_diretorio_atual(["112.84.211.124", "home"]).

connect("220.99.134.37") :-
  set_diretorio_atual(["220.99.134.37"]).

% Disconnect
disconnect(["135.110.60.200"|_]) :-
  writeln("Você não está conectado a um Host externo.").

disconnect([_|_]) :-
  set_diretorio_atual(["135.110.60.200", "home"]).

% aqui o ls
escreve_lista_arquivos([]).

escreve_lista_arquivos([Arquivo1|Tail]) :-
  Arquivo1 = arquivo(Nome,_),
  diretorio_atual([Servidor|_]),
  arquivo_esta_apagado(Servidor, Nome),
  escreve_lista_arquivos(Tail).

escreve_lista_arquivos([Arquivo1|Tail]) :-
  Arquivo1 = arquivo(Nome,_),
  writeln(Nome),
  escreve_lista_arquivos(Tail).


escreve_lista_diretorios([]).

escreve_lista_diretorios([Diretorio1|Tail]) :-
  Diretorio1 = diretorio(Nome,_,_),
  writeln(Nome),
  escreve_lista_diretorios(Tail).

list_files :-
  retorna_diretorio_atual(DirAtual),
  diretorio(_,Diretorios,Arquivos) = DirAtual,
  escreve_lista_arquivos(Arquivos),
  escreve_lista_diretorios(Diretorios). 

% aqui o rm
remove_arquivo_de_lista([], _) :- writeln("Arquivo não encontrado no diretório.").

remove_arquivo_de_lista([Arquivo1|_], Name) :-
  arquivo(Name,_) = Arquivo1,
  diretorio_atual([Servidor|_]),
  arquivo_esta_apagado(Servidor, Name),
  writeln("Arquivo não encontrado no diretório.").

remove_arquivo_de_lista([Arquivo1|_], Name) :-
  arquivo(Name,_) = Arquivo1,
  diretorio_atual([Servidor|_]),
  apaga_arquivo([Servidor, Name]).

remove_arquivo_de_lista([_|Resto], Name) :-
  remove_arquivo_de_lista(Resto, Name).

remove_file(Name) :-
  retorna_diretorio_atual(DirAtual),
  diretorio(_,_,Arquivos) = DirAtual,
  remove_arquivo_de_lista(Arquivos, Name).

chamaFuncao("", _).
chamaFuncao("clear", []) :- shell(clear).
chamaFuncao("clear", _) :- writeln("A função clear não precisa de parâmetros.").
chamaFuncao("connect", []) :- writeln("Informe um Host.").
chamaFuncao("connect", [Host|_]) :- connect(Host).
chamaFuncao("connect", [Host|_]) :- writeln("Informe um Host válido.").
chamaFuncao("disconnect", []) :- diretorio_atual(DirX), disconnect(DirX).
chamaFuncao("disconnect", _) :- writeln("A função disconnect não precisa de parâmetros.").
chamaFuncao("ls", []) :- list_files.
chamaFuncao("ls", _) :- writeln("A função ls não precisa de parâmetros").
chamaFuncao("cd", [Param|_]) :- change_directory(Param).
chamaFuncao("cd", [Param|_]) :- write("Diretório não encontrado: "), writeln(Param).
chamaFuncao("cd", _) :- writeln("A função cd precisa de um parâmetro.").
chamaFuncao("rm", [Param|_]) :- remove_file(Param).
chamaFuncao("rm", _) :- writeln("A função rm precisa de um parâmetro.").
%chamaFuncao("cat", Params) :- cat(Params).
chamaFuncao("getmessage", []) :-
  id_mensagem(Id),
  escreve_mensagem(Id).
chamaFuncao("getmessage", _) :-
  write("A função getmessage não precisa de parâmetros"), nl.
%chamaFuncao("sshinterpol", Params) :-
%  sshinterpol(Params).
%chamaFuncao("connect", Params) :-
%  connect(Params),
%  write("Connecting to "), write(Mensagem), write("...").
chamaFuncao("connect", _) :-
  write("Informe um host válido."), nl.
chamaFuncao("help", _) :- help, nl.
chamaFuncao("exit", _) :- 
  write("Terminando jogo e voltando para o menu..."), nl,
  %shell(clear),
  menu.
%chamaFuncao("disconnect", []) :- disconnect.
%chamaFuncao("disconnect", _) :- write("A função disconnect não precisa de parâmetros").
chamaFuncao(Funcao, _) :- write("Função desconhecida: "), writeln(Funcao).

formataCaminhoAtual([], "").
formataCaminhoAtual([Dh|[]], CaminhoFormatado) :-
  string_concat(Dh, "/", CaminhoFormatado).

formataCaminhoAtual([Dh|Dt], CaminhoFormatado) :-
  formataCaminhoAtual(Dt, T),
  string_concat(Dh, "/", TPart),
  string_concat(TPart, T, CaminhoFormatado).

escreve_prompt :-
  write("root@"),
  diretorio_atual(DA),
  formataCaminhoAtual(DA, CaminhoFormatado),
  write(CaminhoFormatado),
  write(":>> ").

talvez_imprime_mensagem :-
  ja_imprimiu_mensagem.

talvez_imprime_mensagem :-
  id_mensagem(Id),
  escreve_mensagem(Id), nl,
  set_imprimiu_mensagem.

main_loop :-
  talvez_imprime_mensagem,
  escreve_prompt,
  readc(Entrada),
  split_string(Entrada, " ", " ", [NomeFuncao|Params]),
  chamaFuncao(NomeFuncao, Params),
  main_loop.

start :-
  %shell(clear),
  set_diretorio_atual(["135.110.60.200", "home"]),
  reset_arquivos_apagados,
  write("starting"), nl,
  escreve_mensagem(1), espera_enter,
  escreve_mensagem(2), espera_enter,
  escreve_mensagem(3), espera_enter,
  escreve_mensagem(4), espera_enter,
  set_imprimiu_mensagem,
  set_id_mensagem(4),
  main_loop.

escolha_menu(1) :- start.
escolha_menu(2) :- % creditos
  %shell(clear),
  escreve_mensagem(12),
  espera_enter,
  %shell(clear),
  menu.
escolha_menu(3) :- halt. % sair
escolha_menu(A) :- write(A), menu.

menu :-
  %shell(clear),
  escreve_mensagem(11),
  readc(Escolha),
  number_string(N, Escolha),
  escolha_menu(N).

main :-
  shell(clear),
  escreve_mensagem(0),
  menu.
