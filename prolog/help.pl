help :- write("CAT - Comando usado para ler o conteudo de um arquivo\n\t(cat nomeArquivo)\n\nLS - Comando usado para listar os Arquivos e Subdiretorios do Diretorio atual\n\t(ls)\n\nCD - Comando usado para navegar entre diretorios\n\t(cd nomeSubdiretorio ou cd .. para voltar para o Diretorio anterior)\n\nRM - Comando usado para apagar um arquivo no Diretorio atual\n\t(rm nomeArquivo)\n\nCONNECT - Comando usado para se conectar a um outro servidor\n\t(connect ipServidor)\n\nDISCONNECT - Comando usado para desconectar de um servidor remoto\n\t(disconnect)\n\nEXIT - Comando usado para sair do simulador\n\t(exit)\n-- As modificações feitas no simulador, serão apagadas.\n\nHELP - Comando com informacoes sobre os comandos do sistema\n\t(help)\n\nGETMESSAGE - Exibe a ultima mensagem recebida\n\t(getmessage)\n").

:- initialization(main).

main :- help.