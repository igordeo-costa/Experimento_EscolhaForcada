# Experimento: Escolha Forçada com Escala Likert
Este repositório tem por objetivo armazenar as informações sobre um experimento psicolinguístico de **escolha forçada** ao qual foi acoplado uma tarefa de julgamento com **Escala Likert**.

O trabalho está sendo realizado como parte do meu doutorado em psicolínguista pela Pontifícia Universidade Católica do Rio de Janeiro - LAPAL/PUC-Rio.

A atividade foi programada em javascript usando a plataforma [PCIbex](https://www.pcibex.net/).

## O que este repositório contém

- Todos os arquivos pertinentes estão na pasta ``Estudo normativo``.

- O *script* principal (``main.js``) encontra-se na subpasta ``data_includes``.

- Imagens, arquivos de áudio, formulários ``html`` e arquivos `` .csv`` com estímulos experimentais, distratores e treino encontram-se na subpasta ``chunk_includes``.

- As demais subpastas (``ccs_includes`` e ``js_includes`` ) guardam códigos internos padronizados do PCIbex. Recomendamos não alterar, a menos que você saiba o que está fazendo.

- A pasta ``Resultados`` contém um arquivo ``.R`` que faz uma limpeza inicial dos resultados e organiza os nomes das colunas de modo adequado; e um arquivo ``.css`` com os resultados de três participantes que testaram a tarefa.

- Para baixar todo o conjunto de arquivos do experimento, clique em ``code`` e em seguida em ``Download ZIP``.

**IMPORTANTE**: Esta é a versão final do experimento, testada e estável, que parece se comportar de modo adequado. Qualquer *bug* e outros pequenos problemas podem ainda não terem sido percebidos, já que a tarefa ainda não foi aplicada para um grande número de participantes.

## Nota sobre os estímulos experimentais

Os estímulos experimentais estão divididos em quatro listas (A, B, C, e D), nomeadas na coluna ``group`` da tabela ``Experimentais.csv``. Os estímulos de cada grupo (teste, coletivo, distributivo) foram aleatoriamente atribuídos a cada lista, que são apresentadas na tabela ``listas.csv``, de modo que cada uma delas contenha:

- 8 estímulos teste;
- 4 estímulos distributivos;
- 4 estímulos coletivos.
