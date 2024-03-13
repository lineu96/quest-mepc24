
#----------------------------------------------------------------------
# Leitura e tratamento dos dados do questionário
#----------------------------------------------------------------------

## 2024

quest_24 <- read.csv("./respostas/respostas-2024.csv", 
                     sep = ',', 
                     encoding = 'UTF-8', 
                     header = T)

# Nome das colunas
names(quest_24) <-
  c('data_hora', 'instituicao', 'setor_ufpr', 'atividade_ies',
    'area_conhec', 'fim_grad', 'inst_grad', 'programas_grad', 
    'inicio_msc', 'final_msc', 'inst_msc', 'artigos', 'bolsa',
    'materias_est', 'materias_est_pos', 'software_sn', 
    'software_list', 'met_est', 'import_met_est', 
    'expectativa_pos_pg', 'conhec_trans', 'youtube', 
    'contat_coord', 'contat_colab', 'ensino_medio1', 'ensino_medio2',
    'sexo', 'altura', 'peso', 'nascimento', 'emprego', 'residencia',
    'irmaos', 'origem', 'transporte', 'tempo_ies', 'app_transporte',
    'pet', 'instrumento', 'redes_sociais', 'atv_fisica_reg',
    'atv_fisica_n', 'atv_fisica_qual', 'idade_foto', 'kiki_bouba',
    'quadrados')

#----------------------------------------------------------------------

## 2023

quest_23 <- read.csv("./respostas/respostas-2023.csv", 
                     sep = ',', 
                     encoding = 'UTF-8', 
                     header = T)

# Nome das colunas
names(quest_23) <-
  c('data_hora', 'instituicao', 'setor_ufpr', 'atividade_ies',
    'area_conhec', 'fim_grad', 'inst_grad', 'programas_grad', 
    'inicio_msc', 'final_msc', 'inst_msc', 'artigos', 'bolsa',
    'materias_est', 'materias_est_pos', 'software_sn', 
    'software_list', 'met_est', 'import_met_est', 
    'expectativa_pos_pg', 'conhec_trans', 'youtube', 
    'contat_coord', 'contat_colab', 'ensino_medio1', 'ensino_medio2',
    'sexo', 'altura', 'peso', 'nascimento', 'emprego', 'residencia',
    'irmaos', 'origem', 'transporte', 'tempo_ies', 'app_transporte',
    'pet', 'instrumento', 'redes_sociais', 'atv_fisica_reg',
    'atv_fisica_n', 'atv_fisica_qual', 'idade_foto', 'kiki_bouba',
    'quadrados')

#----------------------------------------------------------------------

## 2022

quest_22 <- read.csv("./respostas/respostas-2022.csv", 
                     sep = ',', 
                     encoding = 'UTF-8', 
                     header = T)

# Nome das colunas
names(quest_22) <-
  c('data_hora', 'instituicao', 'setor_ufpr', 'atividade_ies',
    'area_conhec', 'fim_grad', 'inst_grad', 'programas_grad', 
    'inicio_msc', 'final_msc', 'inst_msc', 'artigos', 'bolsa',
    'materias_est', 'materias_est_pos', 'software_sn', 
    'software_list', 'met_est', 'import_met_est', 
    'expectativa_pos_pg', 'conhec_trans', 'youtube', 
    'contat_coord', 'contat_colab', 'ensino_medio1', 'ensino_medio2',
    'sexo', 'altura', 'peso', 'nascimento', 'emprego', 'residencia',
    'irmaos', 'origem', 'transporte', 'tempo_ies', 'app_transporte',
    'pet', 'instrumento', 'redes_sociais', 'atv_fisica_reg',
    'atv_fisica_n', 'atv_fisica_qual', 'idade_foto', 'kiki_bouba',
    'quadrados')

#----------------------------------------------------------------------

## 2021

quest_21 <- read.csv("./respostas/respostas-2021.csv", 
                     sep = ',', 
                     encoding = 'UTF-8', 
                     header = T)

# Nome das colunas
names(quest_21) <-
  c('data_hora', 'instituicao', 'setor_ufpr', 'atividade_ies',
    'area_conhec', 'fim_grad', 'inst_grad', 'programas_grad', 'inicio_msc',
    'final_msc', 'inst_msc', 'artigos', 'bolsa', 'materias_est', 
    'materias_n_est', 'software_sn', 'software_list', 'met_est',
    'import_met_est', 'expectativa_pos_pg', 'conhec_trans', 'youtube', 
    'contat_coord', 'contat_colab', 'ensino_medio1', 'ensino_medio2',
    'sexo', 'altura', 'peso', 'nascimento', 'emprego', 'residencia',
    'irmaos', 'origem', 'cnh', 'transporte', 'tempo_ies', 'app_transporte',
    'pet', 'instrumento', 'redes_sociais', 'atv_fisica_reg', 'atv_fisica_n',
    'atv_fisica_qual', 'covid_positivo', 'covid_conhecido',
    'covid_gr_risco_me', 'covid_gr_risco_outro', 'covid_vacina', 
    'medidas_eficazes', 'medidas_seguidas', 'covid_renda', 'covid_estudos',
    'covid_aprendizado', 'covid_ere', 'dificuldade_ere', 'covid_conclusao', 'descartada',
    'idade_foto', 'kiki_bouba', 'quadrados')

quest_21 <- quest_21[4:nrow(quest_21),]

#----------------------------------------------------------------------

## 2020

quest_20 <- read.csv("./respostas/respostas-2020.csv", 
                     sep = ',', 
                     encoding = 'UTF-8')

# Excluindo a primeira coluna referente aos carimbos de data e hora do google forms
quest_20 <- quest_20[,-1]

# Classificacao das variaves
# lo/ln - var qualitativa ordinal, nominal
# td - var quantitativa discreta

# Renomeando as colunas
names(quest_20) <- 
  c(
    # questoes academicas
    "ln_instituicao", "ln_setorAtuacao", "lo_ativAtuacao",
    "ln_setorArea", "tc_anoFimGrad", "ln_localGrad",
    "ln_progGrad", "tc_inicMest", "tc_fimMest",
    "ln_localMest", "td_artigo", "ln_bolsaEstudo",
    "td_estatGrad", "td_estatPGrad", "lo_soft",
    "ln_enumSoft", "lo_importancia", "td_notaImport",
    "ln_expectAposPG", "ln_conhecTransv", "lo_videoTransv",
    "ln_turma", "lo_contatoProfDisc", "lo_contatoProfColabDisc",
    "ln_medio1", "ln_medio2",
    
    # perfil do aluno
    "ln_sexo", "tc_altura", "tc_peso",
    "dataNasc", "ln_tipoTrab", "ln_compMoradia",
    "td_numIrmao", "ln_origem", "ln_tipoHab",
    "ln_tipoTransp", "tc_tempoPUniv", "td_qtddServTransp", 
    "ln_pet", "ln_instr","ln_rede",
    
    #questoes extra
    "tc_idadeProf", "ln_kiki"
  )

#----------------------------------------------------------------------

# 2019

quest_19 <- read.csv("./respostas/respostas-2019.csv", 
                     sep = ',', 
                     encoding = 'UTF-8')

quest_19 <- data.frame(quest_19[,-1], stringsAsFactors = FALSE)

names(quest_19) <- c("lo_ativAtuacao", "ln_setorAtuacao", 
                     "tc_anoFimGrad", "ln_localGrad", 
                     "ln_progGrad", "tc_inicMest", "tc_fimMest", 
                     "ln_localMest", "td_artigo", "ln_bolsaEstudo", 
                     "td_estatGrad", "td_estatPGrad", "lo_soft", 
                     "ln_enumSoft", "lo_importancia", "td_notaImport", 
                     "ln_tipoTrab", "ln_sexo", "td_numIrmao", 
                     "ln_origem", "ln_compMoradia", "ln_tipoTransp", 
                     "ln_tipoHab", "tc_tempoPUniv", "tc_altura", 
                     "tc_peso", "ln_pet", "ln_instr", "dataNasc", 
                     "ln_rede", "td_qtddServTransp", "lo_provNetflix", 
                     "ln_arrozNatal", "tc_idadeProf", "ln_expectAposPG", 
                     "ln_conhecTransv", "lo_videoTransv", 
                     "lo_contatoProfDisc", "lo_contatoProfColabDisc") 

#----------------------------------------------------------------------