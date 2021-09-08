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

setwd("/home/dados/Acadêmicos/Doutorado/EXPERIMENTOS_2021/EscolhaForcada/Resultados")

dados <- read.pcibex("/home/dados/Acadêmicos/Doutorado/EXPERIMENTOS_2021/EscolhaForcada/Resultados/results.csv")

head(dados)

clean_data <- dados %>%
  filter(Inner.element.number %in% c("trein", "distrat", "experiment")) %>%
  select(-c("Controller.name", "Order.number.of.item", "Label", "nativo"))

colnames(clean_data) <- c("UniqueReceptionTime", "MD5_Hash", "Label", "PennType", "PennName",
                          "Parameter", "Escolha", "EventTime", "vies", "item", "frase", "lista",
                          "genero", "escolaridade", "nativo")
head(clean_data)
