#-----------------------------------------------------------------
# FUNÇÕES e MODELOS
#-----------------------------------------------------------------

## TABELAS

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
                      Desvio_padrao = sd(vetor),
                      Coef_variacao = 100*sd(vetor)/mean(vetor))
  
  table
}

#-----------------------------------------------------------------

## MODELOS DE GRÁFICOS

#-----------------------------------------------------------------

### Barras horizontais

ggplot(data = table, aes(x = Niveis, 
                         y = Frequencia, 
                         fill = Niveis)) +
  geom_bar(stat = 'identity',
           col = 1,
           lwd = 1) +  
  ylim(c(0, 
         (max(table$Frequencia)+ (max(table$Frequencia)*0.05))))+
  coord_flip() +
  geom_text(aes(label = Frequencia),
            hjust=-0.5, 
            color=1, 
            size=8)+
  ylab("Eixo x") +
  xlab("Eixo y") +
  ggtitle("Titulo")+
  theme_classic() + theme(legend.position = 'none',
    plot.title = element_text(family = "Helvetica", 
                              face = "bold", 
                              size = (20),
                              hjust = 0.5),
    axis.title = element_text(face = "bold",
                              size = 15)) 

#-----------------------------------------------------------------

### Setores

table <- freq_ord(quest$ln_setorArea)

table <- table %>% 
  arrange(desc(Niveis)) %>%
  mutate(ypos = cumsum(Frequencia.relativa)- 0.5*Frequencia.relativa)

ggplot(table, aes(x="", y=Frequencia.relativa, fill=Niveis)) +
  geom_bar(stat="identity", 
           width=1,
           col = 1,
           lwd = 1) +
  coord_polar("y", start=0) +
  theme_classic() + 
  #theme(legend.position="none") +
  geom_text(aes(y = ypos, label = Frequencia), 
            color = 1, 
            size = 8)+
  ylab("Eixo x") +
  xlab("Eixo y") +
  ggtitle("Titulo")+
  theme(#legend.position = 'none',
    plot.title = element_text(family = "Helvetica", 
                              face = "bold", 
                              size = (20),
                              hjust = 0.5),
    axis.title = element_text(face = "bold",
                              size = 15),
    axis.text = element_blank())


#-----------------------------------------------------------------

### Barras horizontais para níveis (comparativo)

table <- data.frame(Niveis = rep(letters[1:6], 2),
                    freq_r = round(runif(12),2),
                    ano = c(rep('ano1',6),rep('ano2',6)))

ggplot(table, 
       aes(x=Niveis, y=freq_r, fill=ano)) + 
  geom_bar(stat = 'identity',
           col = 1,
           lwd = 1, 
           position = 'dodge') +  
  ylim(c(0, 
         (max(table$freq_r)+ (max(table$freq_r)*0.05))))+
  coord_flip() +
  geom_text(aes(label = freq_r),
            hjust=-0.1, 
            color=1, 
            size=8, 
            position=position_dodge(width=0.9), 
            vjust=0.5)+
  ylab("Eixo x") +
  xlab("Eixo y") +
  ggtitle("Titulo")+
  theme_classic() + theme(legend.position = 'none',
                          plot.title = element_text(family = "Helvetica", 
                                                    face = "bold", 
                                                    size = (20),
                                                    hjust = 0.5),
                          axis.title = element_text(face = "bold",
                                                    size = 15)) 

#-----------------------------------------------------------------

### Barras verticais

ggplot(data=table, aes(x=Niveis, 
                       y=Frequencia, 
                       fill = Niveis)) + 
  geom_bar(stat="identity", 
           col=1,
           lwd=1)+
  ylim(c(0, 
         (max(table$Frequencia)+ (max(table$Frequencia)*0.05))))+
  geom_text(aes(label=Frequencia), 
            vjust=-1, 
            color=1, 
            size=8)+
  ylab("Eixo x") +
  xlab("Eixo y") +
  ggtitle("Titulo")+
  theme_classic() + theme(#legend.position = 'none',
                          plot.title = element_text(family = "Helvetica", 
                                                    face = "bold", 
                                                    size = (20),
                                                    hjust = 0.5),
                          axis.title = element_text(face = "bold",
                                                    size = 15)) 

#-----------------------------------------------------------------

### Gráfico de linhas

table <- as.data.frame(table(quest$td_artigo))
table$Var1 <- as.numeric(as.vector(table$Var1))

# Plot
ggplot(table, aes(x=Var1, y=Freq)) +
  geom_point() + 
  geom_segment(aes(x=Var1, 
                   xend=Var1, 
                   y=0, 
                   yend=Freq),
               lwd = 1.5)+
  ylab("Eixo x") +
  xlab("Eixo y") +
  ggtitle("Titulo")+
  theme_classic() + theme(legend.position = 'none',
                          plot.title = element_text(family = "Helvetica", 
                                                    face = "bold", 
                                                    size = (20),
                                                    hjust = 0.5),
                          axis.title = element_text(face = "bold",
                                                    size = 15)) 

#-----------------------------------------------------------------

### Histograma

ggplot(quest, aes(x=td_artigo)) +
  geom_histogram(col=1,
                 lwd=1) + 
  ylab("Eixo x") +
  xlab("Eixo y") +
  ggtitle("Titulo")+
  theme_classic() + theme(legend.position = 'none',
                          plot.title = element_text(family = "Helvetica", 
                                                    face = "bold", 
                                                    size = (20),
                                                    hjust = 0.5),
                          axis.title = element_text(face = "bold",
                                                    size = 15)) 

#-----------------------------------------------------------------

### Boxplot

ggplot(data = quest, 
       mapping = aes(y=td_estatPGrad, x='1')) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 1)+
  stat_summary(fun.y=mean, 
               geom="point", 
               shape=20, 
               size=3, 
               color="red", 
               fill="red")+ 
  ylab("Eixo x") +
  xlab("Eixo y") +
  ggtitle("Titulo")+
  theme_classic() + theme(legend.position = 'none',
                          plot.title = element_text(family = "Helvetica", 
                                                    face = "bold", 
                                                    size = (20),
                                                    hjust = 0.5),
                          axis.title = element_text(face = "bold",
                                                    size = 15)) #+coord_flip()

#-----------------------------------------------------------------

### Boxplot com jitter

ggplot(data = quest, 
       mapping = aes(y=td_estatPGrad, x='1')) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 1)+
  stat_summary(fun.y=mean, 
               geom="point", 
               shape=20, 
               size=3, 
               color="red", 
               fill="red")+ 
  ylab("Eixo x") +
  xlab("Eixo y") +
  ggtitle("Titulo")+
  theme_classic() + theme(legend.position = 'none',
                          plot.title = element_text(family = "Helvetica", 
                                                    face = "bold", 
                                                    size = (20),
                                                    hjust = 0.5),
                          axis.title = element_text(face = "bold",
                                                    size = 15)) +
  geom_jitter(col=2) #+coord_flip()

#-----------------------------------------------------------------

### Densidade

ggplot(quest, aes(x=td_artigo)) +
  geom_density(col=1,
                 lwd=1) + 
  ylab("Eixo x") +
  xlab("Eixo y") +
  ggtitle("Titulo")+
  theme_classic() + theme(legend.position = 'none',
                          plot.title = element_text(family = "Helvetica", 
                                                    face = "bold", 
                                                    size = (20),
                                                    hjust = 0.5),
                          axis.title = element_text(face = "bold",
                                                    size = 15)) 

#-----------------------------------------------------------------

### Histograma + boxplot

p1 = ggplot(quest, aes(x=td_artigo)) +
  geom_histogram(col=1,
                 lwd=1) + 
  ylab("Eixo x") +
  xlab("") +
  ggtitle("Titulo")+
  theme_classic() + theme(legend.position = 'none',
                          plot.title = element_text(family = "Helvetica", 
                                                    face = "bold", 
                                                    size = (20),
                                                    hjust = 0.5),
                          axis.title = element_text(face = "bold",
                                                    size = 15))

p2 = ggplot(data = quest, 
            mapping = aes(y=td_estatPGrad, x='1')) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 1)+
  stat_summary(fun.y=mean, 
               geom="point", 
               shape=20, 
               size=3, 
               color="red", 
               fill="red")+ 
  ylab("Eixo x") +
  xlab("") +
  #ggtitle("Titulo")+
  theme_classic() + theme(legend.position = 'none',
                          plot.title = element_text(family = "Helvetica", 
                                                    face = "bold", 
                                                    size = (20),
                                                    hjust = 0.5),
                          axis.title = element_text(face = "bold",
                                                    size = 15)) +coord_flip()

ggarrange(p1, p2, 
          heights = c(2, 1), 
          align = "hv", 
          ncol = 1, 
          nrow = 2)

#-----------------------------------------------------------------

### Histograma + densidade

ggplot(quest) + 
  geom_histogram(aes(x=tc_idadeProf,
                     y=..density..), 
                 position="identity",
                 col = 1,
                 lwd = 1) + 
  geom_density(aes(x=tc_idadeProf,
                   y=..density..),
               col = 4,
               lwd = 1) + 
  ylab("Eixo x") +
  xlab("Eixo y") +
  ggtitle("Titulo")+
  theme_classic() + theme(legend.position = 'none',
                          plot.title = element_text(family = "Helvetica", 
                                                    face = "bold", 
                                                    size = (20),
                                                    hjust = 0.5),
                          axis.title = element_text(face = "bold",
                                                    size = 15)) 


#-----------------------------------------------------------------

### Histograma + boxplot + densidade

p1 = ggplot(quest) + 
  geom_histogram(aes(x=tc_idadeProf,
                     y=..density..), 
                 position="identity",
                 col = 1,
                 lwd = 1) + 
  geom_density(aes(x=tc_idadeProf,
                   y=..density..),
               col = 4,
               lwd = 1) + 
  ylab("Eixo x") +
  xlab("") +
  ggtitle("Titulo")+
  theme_classic() + theme(legend.position = 'none',
                          plot.title = element_text(family = "Helvetica", 
                                                    face = "bold", 
                                                    size = (20),
                                                    hjust = 0.5),
                          axis.title = element_text(face = "bold",
                                                    size = 15))

p2 = ggplot(data = quest, 
            mapping = aes(y=tc_idadeProf, x='1')) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 1)+
  stat_summary(fun.y=mean, 
               geom="point", 
               shape=20, 
               size=3, 
               color="red", 
               fill="red")+ 
  ylab("Eixo x") +
  xlab("") +
  #ggtitle("Titulo")+
  theme_classic() + theme(legend.position = 'none',
                          plot.title = element_text(family = "Helvetica", 
                                                    face = "bold", 
                                                    size = (20),
                                                    hjust = 0.5),
                          axis.title = element_text(face = "bold",
                                                    size = 15)) +coord_flip()

ggarrange(p1, p2, 
          heights = c(2, 1), 
          align = "hv", 
          ncol = 1, 
          nrow = 2)


#-----------------------------------------------------------------

### Barras para fatores lado a lado

ggplot(table, 
       aes(x=Niveis, y=freq_r, fill=ano)) + 
  geom_bar(stat = 'identity',
           col = 1,
           lwd = 1, 
           position = 'dodge') +  
  ylim(c(0, 
         (max(table$freq_r)+ (max(table$freq_r)*0.05))))+
  geom_text(aes(label = freq_r),
            #hjust=-0.1, 
            color=1, 
            size=8, 
            position=position_dodge(width=0.9),
            vjust=-0.5
  )+
  ylab("Eixo x") +
  xlab("Eixo y") +
  ggtitle("Titulo")+
  theme_classic() + theme(legend.position = 'none',
                          plot.title = element_text(family = "Helvetica", 
                                                    face = "bold", 
                                                    size = (20),
                                                    hjust = 0.5),
                          axis.title = element_text(face = "bold",
                                                    size = 15)) 

#-----------------------------------------------------------------

### Barras para fatores empilhadas

table <- data.frame(Niveis = rep(letters[1:6], 3),
                    freq_r = round(runif(18),2),
                    ano = c(rep('ano1',6),rep('ano2',6),rep('ano3',6)))

ggplot(table, 
       aes(x=Niveis, y=freq_r, fill=ano)) + 
  geom_bar(stat = 'identity',
           col = 1,
           lwd = 1)+ 
  #  geom_text(aes(label = freq_r),
  #            hjust=0.5, 
  #            color=1, 
  #            size=8, 
  #            position=position_stack(), 
  #            vjust=1)+
  ylab("Eixo x") +
  xlab("Eixo y") +
  ggtitle("Titulo")+
  theme_classic() + 
  theme(legend.position = 'none',
        plot.title = element_text(family = "Helvetica", 
                                  face = "bold", 
                                  size = (20),
                                  hjust = 0.5),
        axis.title = element_text(face = "bold",
                                  size = 15)) 

#-----------------------------------------------------------------

### Boxplots para níveis de um fator

ggplot(data = quest, 
       mapping = aes(x = ln_setorArea,
                     y=td_estatPGrad, 
                     fill = ln_setorArea)) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 1)+
  stat_summary(fun.y=mean, 
               geom="point", 
               shape=20, 
               size=3, 
               color="red", 
               fill="red")+ 
  ylab("Eixo x") +
  xlab("Eixo y") +
  ggtitle("Titulo")+
  theme_classic() + theme(legend.position = 'none',
                          plot.title = element_text(family = "Helvetica", 
                                                    face = "bold", 
                                                    size = (20),
                                                    hjust = 0.5),
                          axis.title = element_text(face = "bold",
                                                    size = 15)) #+coord_flip(

#-----------------------------------------------------------------

### Dispersão

ggplot(data = quest, 
       mapping = aes(x = tc_peso,
                     y = tc_altura)) +
  geom_point()+
  geom_smooth(method = 'lm', se=F, col=2)+
  geom_smooth(se=F)+
  ylab("Eixo x") +
  xlab("Eixo y") +
  ggtitle("Titulo")+
  theme_classic() + 
  theme(legend.position = 'none',
        plot.title = element_text(family = "Helvetica", 
                                  face = "bold", 
                                  size = (20),
                                  hjust = 0.5),
        axis.title = element_text(face = "bold",
                                  size = 15))

#-----------------------------------------------------------------

### Correlograma

numericas <- data.frame(x=rnorm(100),
                        y=rgamma(100,2),
                        z=runif(100))


names(numericas) <- c('nome1','nome2','nome3')

cor1 <- cor(numericas, method = 'pearson')

corrplot::corrplot.mixed(cor1, 
                         upper = 'ellipse',
                         tl.pos = "lt",
                         diag = 'n')

#-----------------------------------------------------------------

### Matriz de gráficos

ggpairs(Filter(is.numeric, quest)[1:5])+
  ylab("Eixo x") +
  xlab("Eixo y") +
  ggtitle("Titulo")+
  theme_bw() + 
  theme(legend.position = 'none',
        plot.title = element_text(family = "Helvetica", 
                                  face = "bold", 
                                  size = (20),
                                  hjust = 0.5),
        axis.title = element_text(face = "bold",
                                  size = 15))


#-----------------------------------------------------------------