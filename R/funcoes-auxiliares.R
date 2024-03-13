
#----------------------------------------------------------------------
# Funções auxiliares
#----------------------------------------------------------------------

### Tabela de frequencia simples não ordenada 

freq_n_ord <- function(vetor){
  fa <- table(vetor) # frequência absoluta
  fr <- prop.table(fa) # frequência relativa
  
  table <- data.frame(Niveis = names(fa),
                      Frequencia = as.vector(fa),
                      `Frequencia relativa` = as.vector(fr)) # unindo
  
  table
}

#----------------------------------------------------------------------

### Tabela de frequencia simples ordenada

freq_ord <- function(vetor){
  fa <- table(vetor) # frequência absoluta
  fr <- prop.table(fa) # frequência relativa
  
  table <- data.frame(Niveis = names(fa),
                      Frequencia = as.vector(fa),
                      `Frequencia relativa` = as.vector(fr)) # unindo
  
  table <- arrange(table, desc(table$Frequencia))
  
  table    
}

#----------------------------------------------------------------------

### Tabela por classes  

tab_classes <- function(vetor){
  h <- hist(vetor, plot = FALSE) #histograma
  
  breaks <- h$breaks #armazenando os breaks do histograma 
  
  classes <- cut(vetor, breaks = breaks, 
                 include.lowest = TRUE, right = TRUE) #gerando classes
  
  table <- as.data.frame(table(classes)) #gerando tabela com faixas
  
  table$fr <- prop.table(table$Freq)
  
  names(table) <- c('Classes', 'Frequencia', 'Frequencia relativa')
  
  table    
}

#----------------------------------------------------------------------

### Medidas de posição

posicao <- function(vetor){
  table <- data.frame(Minimo = quantile(vetor)[1],
                      Quartil_1 = quantile(vetor)[2],
                      Media = mean(vetor),
                      Mediana = quantile(vetor)[3],
                      Quartil_3 = quantile(vetor)[4],
                      Maximo = quantile(vetor)[5])
  
  row.names(table) <- NULL
  
  table  
}

#----------------------------------------------------------------------

### Medidas de dispersao

dispersao <- function(vetor){
  table <- data.frame(Amplitude = diff(range(vetor)),
                      Variancia = var(vetor),
                      Desvio_padrao = sd(vetor)#,
                      #Coef_variacao = 100*sd(vetor)/mean(vetor)
  )
  
  table
}

#----------------------------------------------------------------------