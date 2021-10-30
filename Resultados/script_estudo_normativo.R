# Função para limpeza dos dados
read.pcibex <- function(filepath, auto.colnames=TRUE, fun.col=function(col,cols){cols[cols==col]<-paste(col,"Ibex",sep=".");return(cols)}) {
  n.cols <- max(count.fields(filepath,sep=",",quote=NULL),na.rm=TRUE)
  if (auto.colnames){
    cols <- c()
    con <- file(filepath, "r")
    while ( TRUE ) {
      line <- readLines(con, n = 1, warn=FALSE)
      if ( length(line) == 0) {
        break
      }
      m <- regmatches(line,regexec("^# (\\d+)\\. (.+)\\.$",line))[[1]]
      if (length(m) == 3) {
        index <- as.numeric(m[2])
        value <- m[3]
        if (is.function(fun.col)){
          cols <- fun.col(value,cols)
        }
        cols[index] <- value
        if (index == n.cols){
          break
        }
      }
    }
    close(con)
    return(read.csv(filepath, comment.char="#", header=FALSE, col.names=cols))
  }
  else{
    return(read.csv(filepath, comment.char="#", header=FALSE, col.names=seq(1:n.cols)))
  }
}

# Pacotes necessários
require(dplyr)
require(tidyr)
require(stringr)
require(ggplot2)
require(scales)
require(forcats)
require(RColorBrewer)
require(gridExtra)

# Diretório de trabalho
setwd("/home/dados/Acadêmicos/Doutorado/EXPERIMENTOS_2021/EscolhaForcada/Resultados")

# Carregamento dos dados
dados <- read.pcibex("/home/dados/Acadêmicos/Doutorado/EXPERIMENTOS_2021/EscolhaForcada/Resultados/Backup_ResultadosFinal_122Part.csv")

#-------------------------------------------------------------------------------
# Filtrando apenas os dados das sentenças 'teste'
#-------------------------------------------------------------------------------
clean_data <- dados %>%
  filter(Inner.element.number %in% c("trein", "distrat", "experiment")) %>%
  select(-c("Controller.name", "Order.number.of.item", "Label", "nativo"))

colnames(clean_data) <- c("UniqueReceptionTime", "MD5_Hash", "Label", "PennType", "PennName",
                          "Parameter", "Escolha", "EventTime", "vies", "item", "frase", "lista",
                          "genero", "escolaridade", "nativo")

clean_data <- clean_data %>%
  filter(vies %in% c("coletivo", "distributivo", "teste"))

#-------------------------------------------------------------------------------
# Analisando os critérios de exclusão
#-------------------------------------------------------------------------------
# Falante nativo & taxa mínima de respostas às distratoras imcompatível com o mínimo
# Ver abaixo, na análise das distratoras, por que tiramos esses dois participantes
clean_data <- clean_data %>%
  filter(nativo == "Sim") %>%
  filter(!UniqueReceptionTime %in% c("1632311528", "1631919148"))

# Dados de escolaridade (exclusão)
clean_data %>%
  group_by(UniqueReceptionTime, escolaridade) %>%
  tally() %>%
  group_by(UniqueReceptionTime, escolaridade) %>%
  summarise(n = n/n) %>%
  group_by(escolaridade) %>%
  tally()

# Exclusão dos participantes que não atenderam ao critério de escolaridade
clean_data <- clean_data %>%
  filter(!escolaridade %in% c("Ensino Fundamental incompleto", "Ensino Médio incompleto"))

# Análise das declarações de gênero
clean_data %>%
  group_by(UniqueReceptionTime, genero) %>%
  tally() %>%
  group_by(UniqueReceptionTime, genero) %>%
  summarise(n = n/n) %>%
  group_by(genero) %>%
  tally()

# Essa a lista final de participantes por lista
listas <- as.data.frame(table(clean_data$UniqueReceptionTime, clean_data$lista))
colnames(listas) <- c("IDPart", "Lista", "Quantidade")

listas <- listas %>%
  group_by(Lista) %>%
  mutate(Quantidade = Quantidade/Quantidade) %>%
  summarise(Quantidade = sum(Quantidade, na.rm = T))

# Exportando essa tabela para o material escrito
# write.csv(listas, "ParticipantesPorLista.csv")

#-------------------------------------------------------------------------------
# Análise das sentenças distratoras
#-------------------------------------------------------------------------------
dist <- dados %>%
  filter(Inner.element.number %in% c("distrat")) %>%
  select(-c("Controller.name", "Order.number.of.item", "Label", "nativo"))

colnames(dist) <- c("UniqueReceptionTime", "MD5_Hash", "Label", "PennType", "PennName",
                          "Parameter", "Escolha", "EventTime", "vies", "item", "frase", "posicao",
                          "genero", "escolaridade", "nativo")

# Carregando a tabela com a resposta esperada por cada frase direto do GitHub
expected_answers <- read.csv("https://raw.githubusercontent.com/igordeo-costa/Experimento_EscolhaForcada/main/Estudo%20normativo/chunk_includes/Distratores.csv")

# Uma frase distratora foi catalogada erroneamente, de modo que havia 17 (e não 16) com resposta esperada + de um
# O código abaixo conserta isso
expected_answers <- expected_answers %>%
  mutate(resposta = ifelse(frase == "Os pedreiros moveram alguns tijolos.", "+um", resposta))

# Mesclando as duas tabelas e filtrando apenas respostas à escolha forçada
dist <- expected_answers %>%
  select(c(posicao, resposta, frase)) %>%
  inner_join(dist, by = c("frase", "posicao")) %>%
  filter(Parameter == "Choice") %>%
  mutate(Escolha = str_replace_all(Escolha, c("uma" = "um"))) %>%
  filter(Escolha %in% c("Apenas um", "Mais de um"))

# Análise por sujeitos
dist$UniqueReceptionTime <- as.factor(dist$UniqueReceptionTime)


# Análise das distratoras por frase
cores <- brewer.pal(name = "Set1", n = 2)

dist %>%
  group_by(resposta, UniqueReceptionTime, Escolha) %>%
  tally() %>%
  mutate(porc = n/sum(n, na.rm = T)) %>%
  mutate(SE=sqrt((porc*(1-porc))/n)) %>% # Calcula o erro padrão (Standad Error ou SE) da proporção
  mutate(SE_inf=porc-SE) %>%
  mutate(SE_sup=porc+SE) %>%
  mutate(SE_inf=if_else(SE_inf<0, 0, SE_inf)) %>%
  filter(Escolha == "Apenas um") %>% # Ou "Mais de um" aqui... Tanto faz...
  filter(!UniqueReceptionTime %in% c("1632311528", "1631919148")) %>%
  ggplot(aes(x = UniqueReceptionTime, y = porc, color = resposta)) +
  geom_hline(aes(yintercept = 0.5), linetype = "dashed", color = "grey60") +
  geom_errorbar(aes(ymin = SE_inf, ymax = SE_sup), width = 0.3, size = 0.7, color = "grey") +
  geom_point(size = 2, stroke = 1, fill = "white", shape = 21) +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1L)) +
  scale_color_manual(values = cores, name = "Resposta Esperada:",
                     labels = c("Mais de um", "Apenas um")) +
  labs(x = "", y = "") +
  theme(text = element_text(size=14),
        legend.position = "top",
        legend.text = element_text(size=14),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) 

# Participantes que tiveram respostas no nível da chance:
# 1632311528
# 1631919148

# Análise global das distratoras
dist %>%
  filter(!UniqueReceptionTime %in% c("1632311528", "1631919148")) %>%
  group_by(resposta, Escolha) %>%
  tally() %>%
  mutate(porc = n/sum(n))

dist %>%
  filter(!UniqueReceptionTime %in% c("1632311528", "1631919148")) %>%
  group_by(resposta, frase, Escolha) %>%
  tally() %>%
  mutate(porc = n/sum(n, na.rm = T)) %>%
  mutate(SE=sqrt((porc*(1-porc))/n)) %>% # Calcula o erro padrão (Standad Error ou SE) da proporção
  mutate(SE_inf=porc-SE) %>%
  mutate(SE_sup=porc+SE) %>%
  mutate(SE_inf=if_else(SE_inf<0, 0, SE_inf)) %>%
  filter(Escolha == "Apenas um") %>% # Ou "Mais de um" aqui... Tanto faz...
  ggplot(aes(x = frase, y = porc, color = resposta)) +
  geom_errorbar(aes(ymin = SE_inf, ymax = SE_sup), width = 0.3, size = 0.7, color = "grey") +
  geom_point(size = 2, stroke = 1, fill = "white", shape = 21) +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1L)) +
  scale_color_manual(values = cores, name = "Resposta Esperada:",
                     labels = c("Mais de um", "Apenas um")) +
  labs(x = "", y = "") +
  theme(text = element_text(size=14),
        legend.position = "top",
        legend.text = element_text(size=14)) 

#-------------------------------------------------------------------------------
# Análise dos dados das sentenças-teste
#-------------------------------------------------------------------------------

# Modificando as expressões 'uma' em 'um' para facilitar a análise
choice_data <- clean_data %>%
  filter(Parameter == "Choice") %>%
  mutate(Escolha = str_replace_all(Escolha, c("uma" = "um")))

# Agora temos uma coluna apenas com as respostas ("Escolha")
# Vamos separar em dois grupos a escolha forçada ("resposta" em "PennName")
# e o julgamento ("minhaescala" em PennName)

missing_data <- choice_data %>%
  filter(PennName == "resposta") %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.integer, as.factor) %>%
  group_by(vies, lista) %>%
  tally(is.na(Escolha))

# Total de respostas à pergunta é à escala
choice_data %>%
  group_by(PennName) %>%
  tally()

# Diferença entre respostas perdidas e total de respostas
1856-1751

# Apenas conferindo
missing_data %>%
  summarise(total = sum(n)) %>%
  summarise(total = sum(total))

# Calculando a porcentagem
round(105/1751*100, 2)

# Exportar a tabela missing_data


#-------------------------------------------------------------------------------
# Preparando a tabela final de dados a ser usada nas análises
#-------------------------------------------------------------------------------

# Pareando escolha forçada com julgamentos correspondentes
escala <- choice_data %>%
  pivot_wider(values_from = Escolha, names_from = PennName) %>%
  filter(!is.na(minhaescala)) %>%
  select(-c("resposta"))

forced_choice <- choice_data %>%
  pivot_wider(values_from = Escolha, names_from = PennName) %>%
  filter(!is.na(resposta)) %>%
  select(-c("minhaescala"))
head(forced_choice)

forced_choice$minhaescala <- escala$minhaescala

choice_data <- forced_choice

#-------------------------------------------------------------------------------
# Realizando a análise após exclusão de respostas perdidas
#-------------------------------------------------------------------------------
# Respostas 'Apenas um'
#-------------------------------------------------------------------------------
apenas_um <- choice_data %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.integer, as.factor) %>%
  group_by(vies, resposta) %>%
  tally() %>%
  mutate(prop = n/sum(n, na.rm = T)) %>%
  filter(resposta == "Apenas um") %>%
  mutate(SE=sqrt((prop*(1-prop))/n)) %>% # Calcula o erro padrão (Standad Error ou SE) da proporção
  mutate(SE_inf=prop-SE) %>%
  mutate(SE_sup=prop+SE) %>%
  mutate(SE_inf=if_else(SE_inf<0, 0, SE_inf)) %>% # Limita os valores inferiores a zero
  mutate(across(3:6, round, 4))

cor <- brewer.pal("Set1", n = 3)

g1 <- apenas_um %>%
  ggplot(aes(x = fct_reorder(vies, prop), y = prop, color = vies)) +
  geom_errorbar(aes(ymin = SE_inf, ymax = SE_sup), width = 0.08, size = 0.7, color = "grey60") +
  geom_point(fill="white", size = 1.5, shape = 21, stroke = 1) +
  theme_bw() +
  ylab("") + xlab("") +
  coord_flip() +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1L),
                     limits = c(.45, .94),
                     breaks = seq(from = 0, to = 1, by = .1)) +
  scale_color_manual(values = cor, name = NULL,
                     labels = c("Coletivo", "Distributivo", "Sem Viés")) +
  theme(legend.position = "top", 
        text = element_text(size=12),
        legend.text = element_text(size=10),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(size=12)) +
  ggtitle("Resposta dada: 'Apenas um'")
# -------------------------------------------------------------------------------
# Analisando os julgamentos em escala likert: Respostas 'Apenas um'
judg_apenasUm <- choice_data %>%
  filter(resposta == "Apenas um") %>%
  group_by(vies, minhaescala) %>%
  tally() %>%
  mutate(perc=n/sum(n)) %>%
  select(-n) %>%
  group_by(vies) %>%
  spread(minhaescala, perc)

colnames(judg_apenasUm) <- c("vies", "Totalmente_inseguro", "Inseguro", "Neutro", "Seguro", "Totalmente_seguro")

# Dividir o meio da escala (os julgamentos "Neutro"):
dados_meio <- judg_apenasUm %>%
  mutate(c1 = Neutro / 2,
         c2 = Neutro / 2) %>%
  select(vies, Totalmente_inseguro, Inseguro, c1, c2, Seguro, Totalmente_seguro) %>%
  gather(key = Escolha, value = perc, 2:7)

# Separando a escala em dois conjuntos, o "alto" e o "baixo":
meio_alto <- dados_meio %>%
  filter(Escolha %in% c("Totalmente_seguro", "Seguro", "c2")) %>%
  mutate(Escolha = factor(Escolha, levels = c("Totalmente_seguro", "Seguro", "c2"))) # Níveis na ordem normal!

meio_baixo <- dados_meio %>%
  filter(Escolha %in% c("c1", "Inseguro", "Totalmente_inseguro")) %>%
  mutate(Escolha = factor(Escolha, levels = c("Totalmente_inseguro", "Inseguro", "c1"))) # Níveis na ordem inversa!

# Estabelecimento de uma paleta de cores para organização!
legend_pal<-c("#2B83BA", "#ABDDA4", "#FFFFBF", "#FFFFBF", "#FDAE61", "#D7191C")
legend_pal <- gsub("#FFFFBF", "#9C9C9C", legend_pal) 
names(legend_pal) <- c("Totalmente_seguro", "Seguro", "c1", "c2", "Inseguro", "Totalmente_inseguro")

# Produzindo o gráfico, finalmente!
g2 <- ggplot() + 
  geom_bar(data = meio_alto, aes(x = fct_relevel(vies, "distributivo", "teste", "coletivo"),
                                 y=perc, fill = Escolha), stat="identity") +
  geom_bar(data = meio_baixo, aes(x = fct_relevel(vies, "distributivo", "teste", "coletivo"),
                                  y=-perc, fill = Escolha), stat="identity") + 
  geom_hline(yintercept = 0, color = c("black"), linetype = "dashed") +
  scale_fill_manual(values = legend_pal, 
                    breaks = c("Totalmente_inseguro", "Inseguro", "c2", "Seguro", "Totalmente_seguro"),
                    labels = c("T. inseguro", "Inseguro", "N.", "Seguro", "T. seguro")) +
  labs(x = "", y = "", fill="") + 
  theme_bw() +
  coord_flip() +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1L),
                     limits = c(-0.3, 1),
                     breaks = seq(from = -0.3, to = 1, by = .3)) +
  theme(text = element_text(size=12),
        legend.position = "top",
        legend.text = element_text(size=8),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(t = 0, r = 6, b = 0, l = 0, "mm"),
        plot.title = element_text(size=12)
  ) +
  guides(fill = guide_legend(override.aes = list(size = 5))) +
  ggtitle("")

#setwd("/home/dados/Acadêmicos/Doutorado/Qualificação/Texto/Imagens/")
#g <- grid.arrange(g1, g2, ncol = 2)
#ggsave("VisaoGeral.png", g, dpi = 300, width = 230, height = 85, units = "mm")

#-------------------------------------------------------------------------------
# Respostas 'Apenas um'
#-------------------------------------------------------------------------------
Maisde_um <- choice_data %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.integer, as.factor) %>%
  group_by(vies, resposta) %>%
  tally() %>%
  mutate(prop = n/sum(n, na.rm = T)) %>%
  filter(resposta == "Mais de um") %>%
  mutate(SE=sqrt((prop*(1-prop))/n)) %>% # Calcula o erro padrão (Standad Error ou SE) da proporção
  mutate(SE_inf=prop-SE) %>%
  mutate(SE_sup=prop+SE) %>%
  mutate(SE_inf=if_else(SE_inf<0, 0, SE_inf)) %>% # Limita os valores inferiores a zero
  mutate(across(3:6, round, 4))

g3 <- Maisde_um %>%
  ggplot(aes(x = fct_reorder(vies, -prop), y = prop, color = vies)) +
  geom_errorbar(aes(ymin = SE_inf, ymax = SE_sup), width = 0.08, size = 0.7, color = "grey60") +
  geom_point(fill="white", size = 1.5, shape = 21, stroke = 1) +
  theme_bw() +
  ylab("") + xlab("") +
  coord_flip() +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1L),
                     limits = c(0.01, .57),
                     breaks = seq(from = 0, to = 1, by = .1)) +
  scale_color_manual(values = cor, name = NULL,
                     labels = c("Coletivo", "Distributivo", "Sem Viés")) +
  theme(legend.position = "none", 
        text = element_text(size=12),
        legend.text = element_text(size=10),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(size=12)) +
  ggtitle("Resposta dada: 'Mais de um'")
#-------------------------------------------------------------------------------
# Analisando os julgamentos em escala likert: Respostas 'Mais de um'
#-------------------------------------------------------------------------------
judg_MaisdeUm <- choice_data %>%
  filter(resposta == "Mais de um") %>%
  group_by(vies, minhaescala) %>%
  tally() %>%
  mutate(perc=n/sum(n)) %>%
  select(-n) %>%
  group_by(vies) %>%
  spread(minhaescala, perc)

colnames(judg_MaisdeUm) <- c("vies", "Totalmente_inseguro", "Inseguro", "Neutro", "Seguro", "Totalmente_seguro")

# Dividir o meio da escala (os julgamentos "Neutro"):
dados_meio <- judg_MaisdeUm %>%
  mutate(c1 = Neutro / 2,
         c2 = Neutro / 2) %>%
  select(vies, Totalmente_inseguro, Inseguro, c1, c2, Seguro, Totalmente_seguro) %>%
  gather(key = Escolha, value = perc, 2:7)

# Separando a escala em dois conjuntos, o "alto" e o "baixo":
meio_alto <- dados_meio %>%
  filter(Escolha %in% c("Totalmente_seguro", "Seguro", "c2")) %>%
  mutate(Escolha = factor(Escolha, levels = c("Totalmente_seguro", "Seguro", "c2"))) # Níveis na ordem normal!

meio_baixo <- dados_meio %>%
  filter(Escolha %in% c("c1", "Inseguro", "Totalmente_inseguro")) %>%
  mutate(Escolha = factor(Escolha, levels = c("Totalmente_inseguro", "Inseguro", "c1"))) # Níveis na ordem inversa!

# Produzindo o gráfico, finalmente!

g4 <- ggplot() + 
  geom_bar(data = meio_alto, aes(x = fct_relevel(vies, "distributivo", "teste", "coletivo"),
                                 y=perc, fill = Escolha), stat="identity") +
  geom_bar(data = meio_baixo, aes(x = fct_relevel(vies, "distributivo", "teste", "coletivo"),
                                  y=-perc, fill = Escolha), stat="identity") + 
  geom_hline(yintercept = 0, color = c("black"), linetype = "dashed") +
  scale_fill_manual(values = legend_pal, 
                    breaks = c("Totalmente_inseguro", "Inseguro", "c2", "Seguro", "Totalmente_seguro"),
                    labels = c("T. inseguro", "Inseguro", "N.", "Seguro", "T. seguro")) +
  labs(x = "", y = "", fill="") + 
  theme_bw() +
  coord_flip() +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1L),
                     limits = c(-0.3, 1),
                     breaks = seq(from = -0.3, to = 1, by = .3)) +
  theme(text = element_text(size=12),
        legend.position = "none",
        legend.text = element_text(size=8),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(t = 0, r = 6, b = 0, l = 0, "mm"),
        plot.title = element_text(size=12)) +
  guides(fill = guide_legend(override.aes = list(size = 5))) +
  ggtitle("")

#setwd("/home/dados/Acadêmicos/Doutorado/Qualificação/Texto/Imagens/")
#g <- grid.arrange(g1, g2, g3, g4, ncol = 2)
#ggsave("VisaoGeral.png", g, dpi = 300, width = 230, height = 170, units = "mm")

# A4 size in mm: 210 x 297
################################################################################
# Filtrando por frase, agora:
################################################################################
# Respostas 'Apenas um'

porfrase_apenasUm <- choice_data %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.integer, as.factor) %>%
  group_by(vies, lista, frase, resposta) %>%
  tally() %>%
  mutate(prop = n/sum(n, na.rm = T)) %>%
  filter(resposta == "Apenas um") %>%
  mutate(SE=sqrt((prop*(1-prop))/n)) %>%
  mutate(SE_inf=prop-SE) %>%
  mutate(SE_sup=prop+SE) %>%
  mutate(SE_inf=if_else(SE_inf<0, 0, SE_inf)) %>%
  mutate(across(3:6, round, 4)) #%>%
  #pivot_longer(prop:SE_sup) %>%
  #mutate(value = value*100) %>%
  #pivot_wider(values_from = value, names_from = name)
  
# Estabelecimento de uma paleta de cores para organização!
paleta <- brewer.pal(name = "Set1", n = 3)

g5 <- porfrase_apenasUm %>%
  ggplot(aes(x = fct_reorder(frase, prop), y = prop, color = vies)) +
  geom_hline(aes(yintercept = .5), color = "grey40", linetype = "dashed") +
  geom_errorbar(aes(ymin = SE_inf, ymax = SE_sup),
                width = 0.5, size = 0.7, color = "grey") +
  geom_point(fill = "white", size = 2,  shape = 21, stroke = 1) +
  theme_bw() +
  ylab("") + xlab("") +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1L)) +
  scale_color_manual(values = paleta, labels = c("Coletivo", "Distributivo", "Sem Viés")) +
  coord_flip() +
  scale_x_discrete(position = "bottom") +
  theme(legend.title = element_blank(),
    legend.position = "top",
    text = element_text(size=12),
    legend.text = element_text(size=12),
    axis.line = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    plot.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
    )

#--------------------------------------------------------------------------------
# Escala Likert associada às frases
#--------------------------------------------------------------------------------

# -------------------------------------------------------------------------------
# Analisando os julgamentos em escala likert

julgFrase_ApenasUm <- choice_data %>%
  filter(resposta == "Apenas um") %>%
  group_by(lista, vies, frase, minhaescala) %>%
  tally() %>%
  mutate(perc=n/sum(n)) %>%
  select(-n) %>%
  group_by(vies, frase) %>%
  spread(minhaescala, perc) %>%
  pivot_longer(`1`:`5`) %>%
  #mutate(value = value*100) %>%
  #mutate(value = round(value, 2)) %>%
  pivot_wider(names_from = name, values_from = value)

colnames(julgFrase_ApenasUm) <- c("lista", "vies", "frase", "Totalmente_inseguro", "Inseguro", "Neutro", "Seguro", "Totalmente_seguro")

# Dividir o meio da escala (os julgamentos "Neutro"):
dados_meio2 <- julgFrase_ApenasUm %>%
  mutate(c1 = Neutro / 2,
         c2 = Neutro / 2) %>%
  select(vies, frase, Totalmente_inseguro, Inseguro, c1, c2, Seguro, Totalmente_seguro) %>%
  gather(key = Escolha, value = perc, 3:8)

# Separando a escala em dois conjuntos, o "alto" e o "baixo":
meio_alto2 <- dados_meio2 %>%
  filter(Escolha %in% c("Totalmente_seguro", "Seguro", "c2")) %>%
  mutate(Escolha = factor(Escolha, levels = c("Totalmente_seguro", "Seguro", "c2"))) # Níveis na ordem normal!

meio_baixo2 <- dados_meio2 %>%
  filter(Escolha %in% c("c1", "Inseguro", "Totalmente_inseguro")) %>%
  mutate(Escolha = factor(Escolha, levels = c("Totalmente_inseguro", "Inseguro", "c1"))) # Níveis na ordem inversa!

# Organizando os níveis da variável para combinar com o gráfico de escolha forçada
meio_alto2 <- porfrase_apenasUm %>%
  select(c("frase", "prop")) %>%
  inner_join(meio_alto2, by = c("frase", "vies"))

meio_baixo2 <- porfrase_apenasUm %>%
  select(c("frase", "prop")) %>%
  inner_join(meio_baixo2, by = c("frase", "vies"))


g6 <- ggplot() + 
  geom_bar(data = meio_alto2, aes(x = fct_reorder(frase, prop),
                                 y=perc, fill = Escolha), stat="identity") +
  geom_bar(data = meio_baixo2, aes(x = fct_reorder(frase, prop),
                                  y=-perc, fill = Escolha), stat="identity") + 
  geom_hline(yintercept = 0, color = c("black"), linetype = "dashed") +
  scale_fill_manual(values = legend_pal, 
                    breaks = c("Totalmente_inseguro", "Inseguro", "c2", "Seguro", "Totalmente_seguro"),
                    labels = c("Total. Inseguro", "Inseguro", "Neutro", "Seguro", "Total. seguro")
                    ) +
  coord_flip() +
  labs(x = "", y = "", fill="") + 
  theme_bw() +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1L),
                     limits = c(-0.44, 1),
                     breaks = seq(from = -0.4, to = 1, by = .2)) +
  theme(text = element_text(size=12),
        legend.position = "top",
        legend.text = element_text(size=12),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  guides(fill = guide_legend(override.aes = list(size = 5)))


#setwd("/home/dados/Acadêmicos/Doutorado/Qualificação/Texto/Imagens/")
#export1 <- grid.arrange(g5, g6, ncol = 2)
#ggsave("PorFrase.png", export1, dpi = 300, width = 370, height = 230, units = "mm")

#--------------------------------------------------------------------------------
# Resposta 'Mais de um'
#--------------------------------------------------------------------------------
porfrase_Maisdeum <- choice_data %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.integer, as.factor) %>%
  group_by(vies, lista, frase, resposta) %>%
  tally() %>%
  mutate(prop = n/sum(n, na.rm = T)) %>%
  filter(resposta == "Mais de um") %>%
  mutate(SE=sqrt((prop*(1-prop))/n)) %>%
  mutate(SE_inf=prop-SE) %>%
  mutate(SE_sup=prop+SE) %>%
  mutate(SE_inf=if_else(SE_inf<0, 0, SE_inf)) %>%
  mutate(across(3:6, round, 4))

# Estabelecimento de uma paleta de cores para organização!
paleta <- brewer.pal(name = "Set1", n = 3)

g7 <- porfrase_Maisdeum %>%
  ggplot(aes(x = fct_reorder(frase, prop), y = prop, color = vies)) +
  geom_hline(aes(yintercept = .5), color = "grey40", linetype = "dashed") +
  geom_errorbar(aes(ymin = SE_inf, ymax = SE_sup),
                width = 0.5, size = 0.7, color = "grey") +
  geom_point(fill = "white", size = 2,  shape = 21, stroke = 1) +
  theme_bw() +
  ylab("") + xlab("") +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1L)) +
  scale_color_manual(values = paleta, labels = c("Coletivo", "Distributivo", "Sem Viés")) +
  coord_flip() +
  scale_x_discrete(position = "bottom") +
  theme(legend.title = element_blank(),
        legend.position = "top",
        text = element_text(size=12),
        legend.text = element_text(size=12),
        axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()
  )

#--------------------------------------------------------------------------------
# Escala Likert associada às frases
#--------------------------------------------------------------------------------

# -------------------------------------------------------------------------------
# Analisando os julgamentos em escala likert

julgFrase_Maisdeum <- choice_data %>%
  filter(resposta == "Mais de um") %>%
  group_by(lista, vies, frase, minhaescala) %>%
  tally() %>%
  mutate(perc=n/sum(n)) %>%
  select(-n) %>%
  group_by(vies, frase) %>%
  spread(minhaescala, perc) %>%
  pivot_longer(`1`:`5`) %>%
  #mutate(value = value*100) %>%
  #mutate(value = round(value, 2)) %>%
  pivot_wider(names_from = name, values_from = value)

colnames(julgFrase_Maisdeum) <- c("lista", "vies", "frase", "Totalmente_inseguro", "Inseguro", "Neutro", "Seguro", "Totalmente_seguro")

# Dividir o meio da escala (os julgamentos "Neutro"):
dados_meio2 <- julgFrase_Maisdeum %>%
  mutate(c1 = Neutro / 2,
         c2 = Neutro / 2) %>%
  select(vies, frase, Totalmente_inseguro, Inseguro, c1, c2, Seguro, Totalmente_seguro) %>%
  gather(key = Escolha, value = perc, 3:8)

# Separando a escala em dois conjuntos, o "alto" e o "baixo":
meio_alto2 <- dados_meio2 %>%
  filter(Escolha %in% c("Totalmente_seguro", "Seguro", "c2")) %>%
  mutate(Escolha = factor(Escolha, levels = c("Totalmente_seguro", "Seguro", "c2"))) # Níveis na ordem normal!

meio_baixo2 <- dados_meio2 %>%
  filter(Escolha %in% c("c1", "Inseguro", "Totalmente_inseguro")) %>%
  mutate(Escolha = factor(Escolha, levels = c("Totalmente_inseguro", "Inseguro", "c1"))) # Níveis na ordem inversa!

# Organizando os níveis da variável para combinar com o gráfico de escolha forçada
meio_alto2 <- porfrase_Maisdeum %>%
  select(c("frase", "prop")) %>%
  inner_join(meio_alto2, by = c("frase", "vies"))

meio_baixo2 <- porfrase_Maisdeum %>%
  select(c("frase", "prop")) %>%
  inner_join(meio_baixo2, by = c("frase", "vies"))


g8 <- ggplot() + 
  geom_bar(data = meio_alto2, aes(x = fct_reorder(frase, prop),
                                  y=perc, fill = Escolha), stat="identity") +
  geom_bar(data = meio_baixo2, aes(x = fct_reorder(frase, prop),
                                   y=-perc, fill = Escolha), stat="identity") + 
  geom_hline(yintercept = 0, color = c("black"), linetype = "dashed") +
  scale_fill_manual(values = legend_pal, 
                    breaks = c("Totalmente_inseguro", "Inseguro", "c2", "Seguro", "Totalmente_seguro"),
                    labels = c("Total. Inseguro", "Inseguro", "Neutro", "Seguro", "Total. seguro")
  ) +
  coord_flip() +
  labs(x = "", y = "", fill="") + 
  theme_bw() +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1L),
                     limits = c(-1, 1),
                     breaks = seq(from = -1, to = 1, by = .2)) +
  theme(text = element_text(size=12),
        legend.position = "top",
        legend.text = element_text(size=12),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  guides(fill = guide_legend(override.aes = list(size = 5)))


#setwd("/home/dados/Acadêmicos/Doutorado/Qualificação/Texto/Imagens/")
#export2 <- grid.arrange(g7, g8, ncol = 2)
#ggsave("PorFrase_Maisdeum.png", export2, dpi = 300, width = 370, height = 230, units = "mm")

#------------------------------------------------------------------------------------
# As análises feitas abaixo são baseadas na tabela 'choice_data', construída no script até a linha 259
# Tenha certeza de que rodou primeiro aquela parte e que não está com aquela tabela desatualizada
#------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------
# Análises globais
#------------------------------------------------------------------------------------
escolha_apenas_um <- choice_data %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.integer, as.factor) %>%
  group_by(vies, resposta) %>%
  tally() %>%
  mutate(prop = n/sum(n, na.rm = T)) %>%
  filter(resposta == "Apenas um") %>%
  mutate(SE=sqrt((prop*(1-prop))/n)) %>% # Calcula o erro padrão (Standad Error ou SE) da proporção
  mutate(across(prop:SE, round, 4))


julgamento_apenasUm <- choice_data %>%
  filter(resposta == "Apenas um") %>%
  group_by(vies, minhaescala) %>%
  tally() %>%
  mutate(perc=n/sum(n)) %>%
  select(-n) %>%
  group_by(vies) %>%
  spread(minhaescala, perc) %>%
  mutate(across(`1`:`5`, round, 4))

colnames(julgamento_apenasUm) <- c("vies", "Totalmente_inseguro", "Inseguro", "Neutro", "Seguro", "Totalmente_seguro")

tabela_apenasum <- escolha_apenas_um %>%
  inner_join(julgamento_apenasUm) %>%
  pivot_longer(prop:Totalmente_seguro) %>%
  mutate(value = round(value*100, 2)) %>%
  pivot_wider(values_from = value, names_from = name)

escolha_maisde_um <- choice_data %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.integer, as.factor) %>%
  group_by(vies, resposta) %>%
  tally() %>%
  mutate(prop = n/sum(n, na.rm = T)) %>%
  filter(resposta == "Mais de um") %>%
  mutate(SE=sqrt((prop*(1-prop))/n)) %>% # Calcula o erro padrão (Standad Error ou SE) da proporção
  mutate(across(prop:SE, round, 4))

julgamento_maisdeUm <- choice_data %>%
  filter(resposta == "Mais de um") %>%
  group_by(vies, minhaescala) %>%
  tally() %>%
  mutate(perc=n/sum(n)) %>%
  select(-n) %>%
  group_by(vies) %>%
  spread(minhaescala, perc) %>%
  mutate(across(`1`:`5`, round, 4))

colnames(julgamento_maisdeUm) <- c("vies", "Totalmente_inseguro", "Inseguro", "Neutro", "Seguro", "Totalmente_seguro")

tabela_maisdeum <- escolha_maisde_um %>%
  inner_join(julgamento_maisdeUm) %>%
  pivot_longer(prop:Totalmente_seguro) %>%
  mutate(value = round(value*100, 2)) %>%
  pivot_wider(values_from = value, names_from = name)

tabela_final <- tabela_apenasum %>%
  bind_rows(tabela_maisdeum)

write.csv(tabela_final, "julgamento_por_vies.csv")

#------------------------------------------------------------------------------------
# Análise por frases
#------------------------------------------------------------------------------------
# 1. Tabela com as porcentagens de respostas 'apenas um'

interp_table <- choice_data %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.integer, as.factor) %>%
  group_by(vies, lista, frase, resposta) %>%
  tally() %>%
  mutate(prop = n/sum(n, na.rm = T)) %>%
  filter(resposta == "Apenas um") %>%
  mutate(SE=sqrt((prop*(1-prop))/n)) %>%
  mutate(across(prop:SE, round, 4)) %>%
  pivot_longer(prop:SE) %>%
  mutate(value = value*100) %>%
  pivot_wider(values_from = value, names_from = name)

# 2. Tabela de Julgamentos para 'apenas um'

judg_table <- choice_data %>%
  filter(resposta == "Apenas um") %>%
  group_by(lista, vies, frase, minhaescala) %>%
  tally() %>%
  mutate(perc=n/sum(n)) %>%
  select(-n) %>%
  group_by(vies, frase) %>%
  spread(minhaescala, perc) %>%
  pivot_longer(`1`:`5`) %>%
  mutate(value = value*100) %>%
  mutate(value = round(value, 2)) %>%
  pivot_wider(names_from = name, values_from = value)

colnames(judg_table) <- c("lista", "vies", "frase", "Totalmente_inseguro", "Inseguro", "Neutro", "Seguro", "Totalmente_seguro")

judg_table_apenasum <- interp_table %>%
  inner_join(judg_table)

# setwd("/home/dados/Acadêmicos/Doutorado/Qualificação/Texto/Tabelas/")
# write.csv(judg_table_apenasum, "julgamento_por_vies_e_frase_apenasum.csv")


# 3. Tabela com as porcentagens de respostas 'Mais de um'

interp_table_maisdeum <- choice_data %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.integer, as.factor) %>%
  group_by(vies, lista, frase, resposta) %>%
  tally() %>%
  mutate(prop = n/sum(n, na.rm = T)) %>%
  filter(resposta == "Mais de um") %>%
  mutate(SE=sqrt((prop*(1-prop))/n)) %>%
  mutate(across(prop:SE, round, 4)) %>%
  pivot_longer(prop:SE) %>%
  mutate(value = value*100) %>%
  pivot_wider(values_from = value, names_from = name)

# 4. Tabela de Julgamentos para 'Mais de um'

judg_table_maisdeum <- choice_data %>%
  filter(resposta == "Mais de um") %>%
  group_by(lista, vies, frase, minhaescala) %>%
  tally() %>%
  mutate(perc=n/sum(n)) %>%
  select(-n) %>%
  group_by(vies, frase) %>%
  spread(minhaescala, perc) %>%
  pivot_longer(`1`:`5`) %>%
  mutate(value = value*100) %>%
  mutate(value = round(value, 2)) %>%
  pivot_wider(names_from = name, values_from = value)

colnames(judg_table_maisdeum) <- c("lista", "vies", "frase", "Total_inseguro", "Inseg", "Neut", "Seg", "Total_seguro")

judg_table_maisdeum <- interp_table_maisdeum %>%
  inner_join(judg_table_maisdeum)

# write.csv(judg_table_maisdeum, "julgamentopor_vies_e_frase_maisdeum.csv")