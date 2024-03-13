#-----------------------------------------------------------------
# Tabelas e Gráficos básicos
#-----------------------------------------------------------------

## Funções

#-----------------------------------------------------------------

### Tabela de frequencia simples não ordenada 

freq_n_ord <- function(vetor){
  fa <- table(vetor) # frequência absoluta
  fr <- prop.table(fa) # frequência relativa
  
  table <- data.frame(Niveis = names(fa),
                      Frequencia = as.vector(fa),
                      `Frequencia relativa` = as.vector(fr)) # unindo
  
  table
}

#-----------------------------------------------------------------

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

#-----------------------------------------------------------------

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

#-----------------------------------------------------------------

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

#-----------------------------------------------------------------

### Medidas de dispersao

dispersao <- function(vetor){
  table <- data.frame(Amplitude = diff(range(vetor)),
                      Variancia = var(vetor),
                      Desvio_padrao = sd(vetor)#,
                      #Coef_variacao = 100*sd(vetor)/mean(vetor)
                      )
  
  table
}

#-----------------------------------------------------------------

## Conjunto de dados genérico

v1 = rnorm(100, 20, 5)
v2 = rnorm(100, 20, 5 )

discreta = rpois(100, 5)
continua1 = v1*2
continua2 = v1+v2
categorica1 = sample(letters[1:3], 100, replace = T)
categorica2 = sample(letters[4:8], 100, replace = T)

df <- data.frame(discreta, continua1,
                 continua2, categorica1,
                 categorica2)

#-----------------------------------------------------------------

## Tabela de frequências absolutas para variável categórica
freq_n_ord(df$categorica1)

#-----------------------------------------------------------------

## Tabela de frequências absolutas para variável discreta
freq_n_ord(df$discreta)

#-----------------------------------------------------------------

## Tabela de frequências por faixas de valores 
tab_classes(df$continua1)

#-----------------------------------------------------------------

## Tabela com medidas descritivas
posicao(df$continua1)
dispersao(df$continua1)

#-----------------------------------------------------------------

## Gráfico de setores
table1 <- freq_n_ord(df$categorica1)

table1 <- table1 %>% 
  arrange(desc(Niveis)) %>%
  mutate(ypos = cumsum(Frequencia.relativa)- 0.5*Frequencia.relativa)

ggplot(table1, aes(x="", y=Frequencia.relativa, fill=Niveis)) +
  geom_bar(stat="identity", 
           width=1,
           col = 1,
           lwd = 1) +
  coord_polar("y", start=0) +
  theme_classic() + 
  #theme(legend.position="none") +
  geom_text(aes(y = ypos, label = Frequencia), 
            color = 1, 
            size = 5)+
  ylab("") +
  xlab("") +
  ggtitle("Gráfico de setores \n\nVariável categórica 1")+
  theme(legend.position = 'bottom',
        plot.title = element_text(family = "Helvetica", 
                                  face = "bold", 
                                  size = (20),
                                  hjust = 0.5),
        axis.title = element_text(face = "bold",
                                  size = 15),
        axis.text = element_blank(),
        legend.title = element_blank(),
        text = element_text(size=20))

#-----------------------------------------------------------------

## Gráfico de barras para variável qualitativa
ggplot(data=table1, aes(x=reorder(Niveis, -Frequencia), 
                        y=Frequencia#, 
                        #fill = Niveis
)) + 
  geom_bar(stat="identity", 
           col=1,
           lwd=1)+
  ylim(c(0, 
         (max(table1$Frequencia)+ (max(table1$Frequencia)*0.2))))+
  geom_text(aes(label=Frequencia), 
            vjust=-1, 
            color=1, 
            size=8)+
  ylab("Frequência") +
  xlab("") +
  ggtitle("Gráfico de barras\n\nVariável categórica 1")+
  theme_classic() + theme(legend.position = 'bottom',
                          plot.title = element_text(family = "Helvetica", 
                                                    face = "bold", 
                                                    size = (20),
                                                    hjust = 0.5),
                          axis.title = element_text(face = "bold",
                                                    size = 15),
                          #axis.text = element_blank(),
                          legend.title = element_blank(),
                          text = element_text(size=20))

#-----------------------------------------------------------------

## Gráfico de barras para variável discreta
table1 <- freq_n_ord(df$discreta)
table1$Niveis <- as.numeric(table1$Niveis)

ggplot(data=table1, aes(x=Niveis, 
                        y=Frequencia#, 
                        #fill = Niveis
)) + 
  geom_bar(stat="identity", 
           col=1,
           lwd=1)+
  ylim(c(0, 
         (max(table1$Frequencia)+ (max(table1$Frequencia)*0.2))))+
  geom_text(aes(label=Frequencia), 
            vjust=-1, 
            color=1, 
            size=8)+
  ylab("Frequência") +
  xlab("Variável discreta") +
  ggtitle("Gráfico de barras\n\nVariável discreta")+
  scale_x_discrete(limits = min(table1$Niveis):max(table1$Niveis))+ 
  theme_classic() + theme(legend.position = 'bottom',
                          plot.title = element_text(family = "Helvetica", 
                                                    face = "bold", 
                                                    size = (20),
                                                    hjust = 0.5),
                          axis.title = element_text(face = "bold",
                                                    size = 15),
                          #axis.text = element_blank(),
                          legend.title = element_blank(),
                          text = element_text(size=20))

#-----------------------------------------------------------------

## Gráfico de linhas
table <- as.data.frame(table(df$discreta))
table$Var1 <- as.numeric(as.vector(table$Var1))

ggplot(table, aes(x=Var1, y=Freq)) +
  geom_point() + 
  geom_segment(aes(x=Var1, 
                   xend=Var1, 
                   y=0, 
                   yend=Freq),
               lwd = 1.5)+
  ylab("Frequência") +
  xlab("Variável discreta") +
  ggtitle("Gráfico de linhas\n\nVariável discreta")+
  scale_x_discrete(limits = min(table1$Niveis):max(table1$Niveis))+ 
  theme_classic() + theme(legend.position = 'none',
                          plot.title = element_text(family = "Helvetica", 
                                                    face = "bold", 
                                                    size = (20),
                                                    hjust = 0.5),
                          axis.title = element_text(face = "bold",
                                                    size = 15),
                          text = element_text(size=15)) 

#-----------------------------------------------------------------

## Histograma
ggplot(df, aes(x=continua2)) +
  geom_histogram(col=1,
                 lwd=1,
                 breaks = hist(df$continua2, 
                               plot = FALSE)$breaks) + 
  xlab("Variável contínua 2") +
  ylab("Frequência") +
  ggtitle("Histograma\n\nVariável contínua 2")+
  theme_classic() + theme(legend.position = 'none',
                          plot.title = element_text(family = "Helvetica", 
                                                    face = "bold", 
                                                    size = (20),
                                                    hjust = 0.5),
                          axis.title = element_text(face = "bold",
                                                    size = 15),
                          text = element_text(size=15)) 

#-----------------------------------------------------------------

## Densidade
ggplot(df, aes(x=continua2)) +
  geom_density(col=1,
               lwd=1) + 
  xlab("Variável contínua 2") +
  ylab("Densidade") +
  ggtitle("Densidade\n\nVariável contínua 2")+
  theme_classic() + theme(legend.position = 'none',
                          plot.title = element_text(family = "Helvetica", 
                                                    face = "bold", 
                                                    size = (20),
                                                    hjust = 0.5),
                          axis.title = element_text(face = "bold",
                                                    size = 15),
                          text = element_text(size=15)) 

#-----------------------------------------------------------------

## Boxplot
ggplot(data = df, 
       mapping = aes(y=continua2, x='1')) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 1)+
  stat_summary(fun=mean, 
               geom="point", 
               shape=20, 
               size=3, 
               color="red", 
               fill="red")+ 
  xlab("") +
  ylab("") +
  ggtitle("Boxplot\n\nVariável contínua 2")+
  theme_classic() + theme(legend.position = 'none',
                          plot.title = element_text(family = "Helvetica", 
                                                    face = "bold", 
                                                    size = (20),
                                                    hjust = 0.5),
                          axis.title = element_text(face = "bold",
                                                    size = 15),
                          axis.text.x=element_blank(),
                          text = element_text(size=15)) #+coord_flip()

#-----------------------------------------------------------------

## Histograma + Boxplot
p1 = ggplot(df, aes(x=continua2)) +
  geom_histogram(col=1,
                 lwd=1,
                 breaks = hist(df$continua2, plot = FALSE)$breaks) + 
  ylab("Frequência") +
  xlab("") +
  ggtitle("Histograma+Boxplot\n\nVariável contínua 2")+
  theme_classic() + theme(legend.position = 'none',
                          plot.title = element_text(family = "Helvetica", 
                                                    face = "bold", 
                                                    size = (20),
                                                    hjust = 0.5),
                          axis.title = element_text(face = "bold",
                                                    size = 15),
                          text = element_text(size=15))

p2 = ggplot(data = df, 
            mapping = aes(y=continua2, x='1')) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 1)+
  stat_summary(fun=mean, 
               geom="point", 
               shape=20, 
               size=3, 
               color="red", 
               fill="red")+ 
  ylab("Variável") +
  xlab("") +
  #ggtitle("Titulo")+
  theme_classic() + theme(legend.position = 'none',
                          plot.title = element_text(family = "Helvetica", 
                                                    face = "bold", 
                                                    size = (20),
                                                    hjust = 0.5),
                          axis.title = element_text(face = "bold",
                                                    size = 15),
                          axis.text.y=element_blank(),
                          text = element_text(size=15)) +coord_flip()

ggarrange(p1, p2, 
          heights = c(2, 1), 
          align = "hv", 
          ncol = 1, 
          nrow = 2)

#-----------------------------------------------------------------

## Histograma + Boxplot + Densidade
p1 = ggplot(df) + 
  geom_histogram(aes(x=continua2,
                     y=..density..), 
                 position="identity",
                 col = 1,
                 lwd = 1,
                 breaks = hist(df$continua2, plot = FALSE)$breaks) + 
  geom_density(aes(x=continua2,
                   y=..density..),
               col = 4,
               lwd = 1) + 
  ylab("Densidade") +
  xlab("") +
  ggtitle("Histograma+Boxplot+Density\n\nVariável contínua 2")+
  theme_classic() + theme(legend.position = 'none',
                          plot.title = element_text(family = "Helvetica", 
                                                    face = "bold", 
                                                    size = (20),
                                                    hjust = 0.5),
                          axis.title = element_text(face = "bold",
                                                    size = 15),
                          text = element_text(size=15))

p2 = ggplot(data = df, 
            mapping = aes(y=continua2, x='1')) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 1)+
  stat_summary(fun=mean, 
               geom="point", 
               shape=20, 
               size=3, 
               color="red", 
               fill="red")+ 
  ylab("Variável") +
  xlab("") +
  #ggtitle("Titulo")+
  theme_classic() + theme(legend.position = 'none',
                          plot.title = element_text(family = "Helvetica", 
                                                    face = "bold", 
                                                    size = (20),
                                                    hjust = 0.5),
                          axis.title = element_text(face = "bold",
                                                    size = 15),
                          axis.text.y=element_blank(),
                          text = element_text(size=15)) +coord_flip()

ggarrange(p1, p2, 
          heights = c(2, 1), 
          align = "hv", 
          ncol = 1, 
          nrow = 2)

#-----------------------------------------------------------------

## Diagrama de dispersão
ggplot(data = df, 
       mapping = aes(x = continua1,
                     y = continua2)) +
  geom_point()+
  geom_smooth(method = 'lm', se=F, col=2)+
  #  geom_smooth(se=F)+
  xlab("Variável contínua 1") +
  ylab("Variável contínua 2") +
  ggtitle("Diagrama de dispersão")+
  theme_classic() + theme(legend.position = 'none',
                          plot.title = element_text(family = "Helvetica", 
                                                    face = "bold", 
                                                    size = (20),
                                                    hjust = 0.5),
                          axis.title = element_text(face = "bold",
                                                    size = 15),
                          text = element_text(size=15))

#-----------------------------------------------------------------

## Boxplots para níveis de um fator
ggplot(data = df, 
       mapping = aes(x = categorica1,
                     y=continua1)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 1,
               fill = 'white')+
  stat_summary(fun=mean, 
               geom="point", 
               shape=20, 
               size=3, 
               color="red", 
               fill="red")+ 
  xlab("Variável categórica 1") +
  ylab("Variável contínua 1") +
  ggtitle("Boxplot para níveis de um fator")+
  theme_classic() + theme(legend.position = 'none',
                          plot.title = element_text(family = "Helvetica", 
                                                    face = "bold", 
                                                    size = (20),
                                                    hjust = 0.5),
                          axis.title = element_text(face = "bold",
                                                    size = 15),
                          text = element_text(size=15)) 

#-----------------------------------------------------------------

## Barras para variáveis categóricas
table <- as.data.frame(table(df$categorica1,
                             df$categorica2))

table$freq_r <- table$Freq/sum(table$Freq)
table$freq_r <- round(table$freq_r, 2)

names(table) <- c('categorica1', 'categorica2', 'Freq', 'freq_r')

ggplot(table, 
       aes(x=categorica1, y=Freq, fill=categorica2)) + 
  geom_bar(stat = 'identity',
           col = 1,
           lwd = 1, 
           position = 'dodge') +  
  ylim(c(0, 
         (max(table$Freq)+ (max(table$Freq)*0.05))))+
  geom_text(aes(label = Freq),
            #hjust=-0.1, 
            color=1, 
            size=8, 
            position=position_dodge(width=0.9),
            vjust=-0.5
  )+
  xlab("Categórica 1") +
  ylab("Frequência") +
  ggtitle("Barras para 2 categóricas")+
  theme_classic() + theme(legend.position = 'top',
                          plot.title = element_text(family = "Helvetica", 
                                                    face = "bold", 
                                                    size = (20),
                                                    hjust = 0.5),
                          axis.title = element_text(face = "bold",
                                                    size = 15),
                          #axis.text = element_blank(),
                          #legend.title = element_blank(),
                          text = element_text(size=15)) 

#-----------------------------------------------------------------

## Barras proporcionais para 2 variáveis categóricas
table2 <- as.matrix(prop.table(table(df$categorica2,
                                     df$categorica1), 
                               mar = 2))


table2 <- as.data.frame(table2)

names(table2) <- c('categorica2', 'categorica1', 'Freq')


ggplot(table2, 
       aes(x=categorica1, y=Freq, fill=categorica2)) + 
  geom_bar(stat = 'identity',
           col = 1,
           lwd = 1)+ 
  #  geom_text(aes(label = freq_r),
  #            hjust=0.5, 
  #            color=1, 
  #            size=8, 
  #            position=position_stack(), 
  #            vjust=1)+
  xlab("Variável categórica 1") +
  ylab("Proporção") +
  ggtitle("Barras empilhadas proporcionais para\n 2 variáveis categóricas")+
  theme_classic() + theme(legend.position = 'top',
                          plot.title = element_text(family = "Helvetica", 
                                                    face = "bold", 
                                                    size = (20),
                                                    hjust = 0.5),
                          axis.title = element_text(face = "bold",
                                                    size = 15),
                          #axis.text = element_blank(),
                          #legend.title = element_blank(),
                          text = element_text(size=15)) 

#-----------------------------------------------------------------

## Gráfico de mosaico para 2 variáveis categóricas
ggplot(data = df) +
  geom_mosaic(aes(x = product(categorica1, categorica2), 
                  fill=categorica1)) +
  xlab("Variável categórica 2") +
  ylab("Variável categórica 1") +
  ggtitle("Gráfico de mosaico para\n 2 variáveis categóricas")+
  theme_classic() + theme(legend.position = 'top',
                          plot.title = element_text(family = "Helvetica", 
                                                    face = "bold", 
                                                    size = (20),
                                                    hjust = 0.5),
                          axis.title = element_text(face = "bold",
                                                    size = 15),
                          #axis.text = element_blank(),
                          #legend.title = element_blank(),
                          text = element_text(size=15)) 

#-----------------------------------------------------------------
