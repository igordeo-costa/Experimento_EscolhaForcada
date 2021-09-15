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

## Nota sobre o desenho experimental

#### Quanto aos itens
Os estímulos experimentais estão divididos em quatro listas (A, B, C, e D), nomeadas na coluna ``group`` da tabela ``Experimentais.csv``. O desenho experimental usado é o chamado [*randomized block design*](https://link.springer.com/referenceworkentry/10.1007%2F978-0-387-32833-1_344), tal que as sentenças (ou itens) experimentais foram agrupadas em três blocos quanto ao **suposto** viés pragmático que apresentavam:

- **Teste:** supostamente ambíguo, ou seja, sem tendência ou viés;
- **Coletivo:** com viés ou tendência para uma interpretação singular do DP indefinido;
- **Distributivo:** com viés ou tendência para uma interpretação plural do DP indefinido.

Os estímulos de cada um desses três grupos foram aleatoriamente atribuídos a cada lista (que são apresentadas na tabela ``listas.csv``), de modo que nenhum deles se repetisse nas demais listas, cada uma contendo:

- 8 estímulos teste;
- 4 estímulos distributivos;
- 4 estímulos coletivos.

#### Quanto aos sujeitos
Cada sujeito será aleatoriamente submetido a uma das quatro listas (A, B, C e D), sendo que, para cada um, os itens experimentais e os distratores são aleatorizados pelo programa, buscando-se garantir, no entanto, que [itens experimentais não surjam em sequência](https://github.com/addrummond/ibex/blob/master/docs/manual.md#shuffle-sequences), sendo entremeados por itens distratores.

#### Quanto às sentenças distratoras
Em todas as sentenças experimentais, a pergunta de interpretação incidia sobre o objeto do verbo (*v.g.* As manisfestantes incendiaram *um ônibus*. Quantos *ônibus* foram incendiados?). A fim de garantir que o mesmo número de perguntas recairia sobre o sujeito e sobre o objeto, construímos as distratoras de tal modo a conterem: 24 perguntas sobre o sujeito (codificadas como ```suj``` na coluna ```posicao``` da tabela ```Distratores.csv```) e apenas 8 sobre o objeto (codificadas como ```obj```). Desse modo, como cada lista apresenta 16 experimentais, cada sujeito será submetido a 24 perguntas que incidem sobre o objeto e 24 que incidem sobre o sujeito.

Quanto às respostas à pergunta de interpretação, metade das distratoras (8 sentenças) têm como respostas certas ou mais plausíveis "Apenas um" (```um``` na coluna ```resposta``` da tabela ```Distratores.csv```) e outras 8 têm como resposta certa "Mais de um" (codificadas como ```+um```).
