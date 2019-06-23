:- set_prolog_flag(verbose, silent).
:- [mensagens].
:- initialization(main).

% o predicado get_diretorio_atual(Dir) unifica Dir com o diretorio atual
% set_diretorio_atual(Dir) fara Dir ser o novo diretorio atual
:- dynamic diratual(Dir).
get_diretorio_atual(Dir) :- diratual(Dir).
set_diretorio_atual(Dir) :- retractall(diratual), assertz(diratual(Dir)).

% o predicado reset_arquivos_apagados reseta a lista para o inicial
% get_arquivos_apagados(ListArq) unifica ListArq com a lista de arquivos
% o arquivo apagado eh representado pela tupla (servidor, nome)
% apaga_arquivo(Arq) adiciona Arq aos arquivos apagados
% nao tem como "desapagar" um arquivo, apenas resetar toda a lista
:- dynamic arquivos_apagados(ListArq).
reset_arquivos_apagados :-
  retractall(arquivos_apagados),
  assertz(arquivos_apagados([('135.110.60.200', 'i'), ('','')])).
get_arquivos_apagados(ListArq) :- arquivos_apagados(ListArq).
apaga_arquivo(Arq) :-
  arquivos_apagados(ListArqAntiga),
  append(ListArqAntiga,[Arq], ListAqrNova),
  retractall(arquivos_apagados),
  assertz(ListAqrNova).

% o predicado get_id_mensagem(Id) unifica Id com o id da mensagem atual
% set_id_mensagem(Id) fara Id ser a nova mensagem atual
:- dynamic id_mensagem(Id).
get_id_mensagem(Id) :- id_mensagem(Id).
set_id_mensagem(Id) :- retractall(id_mensagem), assertz(id_mensagem(Id)).

% set_nao_imprimiu_mensagem define que a mensagem nao foi impressa
% set_imprimiu_mensagem define que a mensagem foi impressa
% ja_imprimiu_mensagem pergunta se a mensagem foi impressa
:- dynamic ja_imprimiu_mensagem.
set_nao_imprimiu_mensagem(Id) :- retractall(ja_imprimiu_mensagem).
set_imprimiu_mensagem(Id) :- assertz(ja_imprimiu_mensagem).

% metodo de leitura customizado pra nao precisar de ponto
readc(Output) :- read_line_to_string(user_input, Output).

escreve_mensagem(NumeroMensagem) :-
  get_mensagem(NumeroMensagem, M),
  write(M).

espera_enter :- 
  write('\nDigite Enter para continuar...'),
  readc(_),
  write('').

retornaServidor([A, _, _, _, _], '135.110.60.200', A).
retornaServidor([_, B, _, _, _], '112.84.211.124', B).
retornaServidor([_, _, C, _, _], '150.189.56.65', C).
retornaServidor([_, _, _, D, _], '220.99.134.37', D).
retornaServidor([_, _, _, _, E], _, E).

retornaSubdiretorio(ListaDiretorios, NomeEsperado, IndiceAtual, NomeEsperado) :-
  nth0(ListaDiretorios, IndiceAtual, NomeEsperado).

% parei aqui...

retornaSubdiretorio array nomeEsperado indiceAtual
 | indiceAtual < len array && ((nome elemento) == nomeEsperado) = elemento
 | indiceAtual < len array = retornaSubdiretorio array nomeEsperado (indiceAtual + 1)
 | otherwise = Diretorio "" [] []
 where elemento = array !! indiceAtual

retornaDiretorioAtual dir subdirs nomes indice num
retornaDiretorioAtual :: Diretorio -> [Diretorio] -> [String] -> Int -> Int -> Diretorio
retornaDiretorioAtual(Diretorio, ListaDiretorio, Nomes, Indice, Num, DiretorioAtual)
retornaDiretorioAtual(Diretorio, ListaDiretorio, [Nome|_], 0, Num, DiretorioAtual) :-
  (Num > 1 ->(
    retornaServidor(ListaDiretorio, Nome, Servidor),
    let subDirsServ = retornaSubdiretorios servidor
    retornaDiretorioAtual servidor subDirsServ nomes 1 (num - 1)
  ); (
    retornaServidor subdirs (nomes !! 0)
  ))

retornaDiretorioAtual dir subdirs nomes indice 1 = do
 retornaSubdiretorio subdirs (nomes !! indice) 0

retornaDiretorioAtual dir subdirs nomes indice num = do
 let subdir = retornaSubdiretorio subdirs (nomes !! (indice)) 0
 retornaDiretorioAtual subdir (retornaSubdiretorios subdir) nomes (indice + 1) (num - 1)

% ate aqui

main_loop :-
  let dirVazio = Diretorio "" [] []
  let diretorio = (retornaDiretorioAtual dirVazio (lerJSON d) dirAtual 0 (len dirAtual))

  let mensagemRecebida = retornaConteudoMensagem (retornaMensagem (lerJSONM m) idMsg)

  if (not jaImprimiuMsg) then putStrLn mensagemRecebida else (putStr "")

  putStr ("root@" ++ (formataCaminhoAtual dirAtual) ++ ":>> ")

  entrada <- getLine

  let idMensagem = retornaNovaMensagem entrada idMsg arquivosApagados
  let splitted = Data.List.Split.splitOn " " entrada
  let nomeFuncao = Prelude.head splitted
  let nomeArquivo = if (len splitted > 1) then Prelude.head (Prelude.tail splitted) else ""

  let resultChamaFuncao = (chamaFuncao nomeFuncao diretorio nomeArquivo dirAtual arquivosApagados mensagemRecebida)
  if (resultChamaFuncao == "") then (putStr "") else (putStrLn resultChamaFuncao)

  let resultadoTroca = (if nomeFuncao == "cd" then (cd nomeArquivo diretorio dirAtual) else (if nomeFuncao == "connect" then (connect nomeArquivo) else (if (nomeFuncao == "disconnect") then (if (dirAtual !! 0 /= "135.110.60.200") then ["135.110.60.200", "home"] else (dirAtual) ) else dirAtual)))

  let novoDirAtual = if (Prelude.head resultadoTroca == "erro") then dirAtual else resultadoTroca

  let saidaRm = if nomeFuncao == "rm" then rm diretorio nomeArquivo arquivosApagados dirAtual else arquivosApagados
  let novosArquivosApagados = if saidaRm /= [("erro", "")] then saidaRm else arquivosApagados

  if nomeFuncao == "clear" then clearScreen else (putStr "")

  let novoJaImprimiuMsg = idMensagem == idMsg
  if entrada == "exit" then (putStr "") else mainLoop novoDirAtual novosArquivosApagados idMensagem novoJaImprimiuMsg -- cada comando aumenta a pilha de recursão! tadinho do stack


start :-
  escreve_mensagem(1), espera_enter,
  escreve_mensagem(2), espera_enter,
  escreve_mensagem(3), espera_enter,
  escreve_mensagem(4), espera_enter,
  main_loop(['135.110.60.200', 'home'],
  [('135.110.60.200', 'i'),('','')], 4, true).

escolha_menu(1) :- start.
escolha_menu(2) :- % creditos
  escreve_mensagem(12),
  espera_enter,
  menu.
escolha_menu(3) :- halt. % sair
escolha_menu(A) :- write(A), menu.

menu :-
  escreve_mensagem(11),
  readc(Escolha),
  %shell(clear),
  number_string(N, Escolha),
  escolha_menu(N).

init :-
  escreve_mensagem(0),
  menu.

main :-
  %shell(clear),
  init.


% string_chars(String, CharList).