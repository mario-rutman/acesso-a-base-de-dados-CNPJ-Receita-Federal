# Fernando_Correa1 sugeriu que fosse direto na base em SQLite: 
# https://bit.ly/3cRruVw
# 
# Baixa esta base de dados no C: ou D: onde tiver alguns bons 20 gigas.

# Depois instala os pacotes
#install.packages("DBI")
#install.packages("RSQLite")
#install.packages("dplyr")

library(dplyr)
library(dbplyr)
library(magrittr)
library(stringr)

# Criando a conexão com o banco de dados.
con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "bd_dados_qsa_cnpj.db")

# Vendo as tabelas no banco de dados "bd_dados_qsa_cnpj.db".
DBI::dbListTables(con)

# Vendo os nomes das colunas de algumas tabelas no banco de dados "bd_dados_qsa_cnpj.db".  
DBI::dbListFields(con, "cnpj_dados_cadastrais_pj")
DBI::dbListFields(con, "tab_natureza_juridica")
DBI::dbListFields(con, "tab_situacao_cadastral")

# Para ver a cara de uma tabela. 
cnpj2 <- dplyr::tbl(con, "cnpj_dados_cadastrais_pj")
cnpj2
nat_jur <- dplyr::tbl(con, "tab_natureza_juridica")
nat_jur


# Criando lista de CNPJs.
lista_de_cnpjs <- c('10.626.543/0001-72','01.575.709/0001-88','29.345.741/0001-96',
                    '20.008.729/0024-65','13513.325/0045-30') %>% # Removeno todo tipo de pontuação
  stringr::str_remove_all("[:punct:]") %>% # Colocando zeros antes dos algaritmos do CNPJ.
  stringr::str_pad(pad = "0", width = 14, side = "left")

# Criando objeto lista cujo nome é cnpj.
# Na verdade estou extraindo a tabela "cnpj_dados_cadastrais_pj" e
# chamando de cnpj.
cnpj <- dplyr::tbl(con, "cnpj_dados_cadastrais_pj")

# Criando a tabela com os dados cadastrais dos CNPJs desejados.
dados_cadastrais <- cnpj %>%
dplyr::select(c(4,6,7,26,27,29)) %>% 
dplyr::filter(cnpj %in% local(lista_de_cnpjs)) %>% 
# O collect transforma a tabela num tibble local.
dplyr::collect() %>% 
dplyr::mutate(lista_original = TRUE)

###############################################################################
# Fazendo a filtragem por razão social.
lista_de_razao_social <- c("INFINEUM BRASIL LTDA", "GOL LINHAS AEREAS S/A",
                           "ROMA MOBILI INDUSTRIA LTDA",
                           "MUNDIVOX TELECOMUNICACOES LTDA",
                           "FSTP BRASIL LTDA", "FACULDADE DO SABOR REFEICOES LTDA")

dados_cadastrais_raz_soc <- cnpj %>%
  dplyr::select(c(4,6,7,26,27,29)) %>% 
  dplyr::filter(razao_social %in% local(lista_de_razao_social)) %>% 
  dplyr::collect() %>% 
  dplyr::mutate(lista_original = TRUE)

################################################################################
# Agora vou criar uma coluna da raizes de CNPJ, e repetir o procedimento para 
# raízes de CNPJ.
lista_de_raizes_cnpjs <- c('10626543','01575709','29345741',
                           '20008729','13513325', '42147496', '35820448',
                           '28598795', '29667227', '05990431')

# Criando tabela com a coluna raiz_cnpj.
dados_cadastrais <- cnpj %>%
  dplyr::select(c(4,6,7,26,27,29)) %>% 
  dplyr::mutate(raiz_cnpj = str_sub(cnpj, 1, 8)) %>% 
  dplyr::filter(raiz_cnpj %in% local(lista_de_raizes_cnpjs)) %>% 
  dplyr::collect() %>%
  dplyr::mutate(lista_original = TRUE) 

# 
fone_1 <- dados_cadastrais %>% 
  dplyr::group_by(razao_social) %>% 
  dplyr::summarise(tel_01 = unique(ddd_telefone_1)) %>% 
  dplyr::pull() %>% 
  unique()

fone_2 <- dados_cadastrais %>% 
  dplyr::group_by(razao_social) %>% 
  dplyr::summarise(tel_02 = unique(ddd_telefone_2))

email <- dados_cadastrais %>% 
  dplyr::group_by(razao_social) %>% 
  dplyr::summarise(e_mail = unique(correio_eletronico)) %>% 
  dplyr::mutate(e_mail = str_to_lower(e_mail))



  