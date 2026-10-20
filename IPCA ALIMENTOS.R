directory <- getwd()
setwd(directory)

library(tidyverse)
library(rbcb)
library(bizdays)
library(sidrar)
library(httr2)
library(jsonlite)
library(tseries)
library(lubridate)
library(purrr)
library(seasonal)
library(readxl)
library(seastests)
library(dotenv)
library(mFilter)

load_dot_env()
fred_key <- Sys.getenv("API_FRED")

## 2938
info_2938 <- info_sidra(2938)

var_2938 = list(c(7169, 7170, 7171, 7432))

ipca_2938 <- get_sidra(
  x = 2938,
  variable = 63,
  period = "all",
  geo = "Brazil",
  classific = "c315",
  category = var_2938)

###1419
info_1419 <- info_sidra(1419)

var_1419 = list(c(7169, 7170, 7171, 7432))

ipca_1419 <- get_sidra(
  x = 1419,
  variable = 63,
  period = "all",
  geo = "Brazil",
  classific = "c315",
  category = var_1419)

### 7060
var_7060 = list(c(7169, 7170, 7171, 7432))

ipca_7060 <- get_sidra(
  x = 7060,
  variable = 63,
  period = "all",
  geo = "Brazil",
  classific = "c315",
  category = var_7060)

ipca_cons <- rbind(ipca_2938,
                   ipca_1419,
                   ipca_7060) %>%
  mutate(
    periodo = ymd(paste0(`Mês (Código)`, "01"))
  ) %>% arrange(periodo) %>%
  rename(grupo_codigo = `Geral, grupo, subgrupo, item e subitem (Código)`,
         grupo = `Geral, grupo, subgrupo, item e subitem`) %>%
  filter(periodo < "2026-01-01") %>%
  filter(periodo > "2006-12-01")

ggplot(ipca_cons, aes(x = periodo, y = Valor, color = grupo)) +
  geom_line(linewidth = 0.8) +
  labs(
    x = NULL,
    y = "Variação mensal (%)",
    color = NULL
  ) +
  theme_minimal() + facet_wrap(~grupo, scales = "free_y")

####

search_sidra("IPCA")

dados_sgs <- c(
  ibc_br       = 24363,
  selic_ac_mes = 4189,
  dolar_medmen = 3698,
  commodities_BR = 27574,
  meta_inf     = 13521,
  ipca_admi = 4449,
  ipca_dozemeses = 13522
  
)

dados <- get_series(
  dados_sgs,
  start_date = "2007-01-01"
)

for (i in names(dados)) {
  assign(i, as.data.frame(dados[[i]]))
}

calendar_df <- data.frame(
  periodo = seq(
    as.Date("2007-01-01"),
    as.Date("2025-12-01"),
    by = "month"
  )
) %>% mutate(ano = year(periodo))

meta_inf <- meta_inf %>% mutate(date = year(date)) %>% rename(ano = date)

######### serie fao

fao <- read_excel("fao_real_index.xlsx") %>% 
  filter(Month > "2006-12-01") %>% 
  filter(Month < "2026-01-01") %>% select(Month, `Food Price Index`) %>%
  rename(indice_alim_fao = `Food Price Index`) %>% 
  mutate(Month = floor_date(Month, unit = "month"))

# PREÇOS PETROLEO

data_inicio_petro <- "2007-01-01"
data_fim_petro    <- "2026-01-01"

url <- paste0(
  "https://api.stlouisfed.org/fred/series/observations?",
  "series_id=MCOILBRENTEU",
  "&api_key=", fred_key,
  "&file_type=json",
  "&observation_start=", data_inicio_petro,
  "&observation_end=", data_fim_petro
)

res <- request(url) %>%
  req_perform() %>%
  resp_body_json()

preços_petro <- tibble(
  periodo = as.Date(sapply(res$observations, `[[`, "date")),
  mcoilbrentEU = as.numeric(sapply(res$observations, `[[`, "value"))
)

preços_petro

##### dados

dados = calendar_df %>% 
  left_join(meta_inf, by = "ano") %>%
  left_join(ibc_br, by = c("periodo" = "date")) %>% 
  left_join(selic_ac_mes, by = c("periodo" = "date")) %>% 
  left_join(dolar_medmen, by = c("periodo" = "date")) %>%
  left_join(fao, by = c("periodo" = "Month")) %>%
  left_join(preços_petro, by = c("periodo" = "periodo")) %>%
  left_join(ipca_admi, by = c("periodo" = "date")) %>%
  left_join(commodities_BR, by = c("periodo" = "date"))

###### puxar datas das reunioes

selic_meta_df <- data.frame()

for (i in(2007:2026))
  
{
  
  d_start = make_date(i, 1, 1)
  d_end = make_date(i, 12, 31)
  
  selic_meta_x <- get_series(
    c(selic_meta = 432), 
    start_date = d_start, 
    end_date = d_end)
  
  selic_meta_df <- rbind(selic_meta_df, selic_meta_x) 
  
  Sys.sleep(1.5)
  
}

selic_meta_df <- selic_meta_df %>% filter(date < "2026-01-02")

###### puxar datas das reunioes

library(httr2)
library(httr)

url <- "https://www.bcb.gov.br/api/servico/sitebcb/copom/atas"

response <- VERB(
  "GET",
  url,
  query = list(quantidade = "1500"),
  content_type("application/octet-stream"),
  accept("application/json")
  
)

txt <- content(response, "text", encoding = "UTF-8")

json <- fromJSON(txt, flatten = TRUE)

df_copom <- as.data.frame(json$conteudo) %>% 
  filter(dataReferencia > "2006-12-31") %>% 
  filter(dataReferencia < "2026-01-01") %>% arrange(asc = dataReferencia)

df_copom_aux <- df_copom %>%
  group_by(ano = lubridate::year(dataReferencia)) %>%
  mutate(R = row_number()) %>%
  mutate(referencia_focus = paste0("R", R, "/", ano)) %>%
  ungroup() %>%
  select(nroReuniao, dataReferencia, referencia_focus)

####### puxar as expectativas para decisao do copom antes da reuniao

focus_selic_copom_aux <- get_market_expectations(
  type = "selic",
  start_date = "2007-01-01",
  end_date = "2025-12-31"
) %>% filter(baseCalculo == 0) %>% 
  arrange(Data, Reuniao) %>% 
  left_join(df_copom_aux, by = c("Reuniao" = "referencia_focus"))

focus_selic_copom <- focus_selic_copom_aux %>%
  filter(Data < dataReferencia) %>%
  group_by(Reuniao) %>%
  slice_max(Data, n = 1) %>%
  ungroup() %>% arrange(nroReuniao)

#### puxar expectativa 12 meses

expc_inf <- get_market_expectations(
  type = "inflation-12-months",
  indic = "IPCA",
  start_date = "2006-12-31",
  end_date = "2026-01-01"
) %>% filter(Suavizada == "S")

exp_ipca_12m <- expc_inf %>%
  mutate(
    date = as.Date(Data),
    periodo = floor_date(date, "month")
  ) %>%
  group_by(periodo) %>%
  summarise(
    exp_12m_media = last(Media, order_by = date),
    exp_12m_mediana = last(Mediana, order_by = date)
  ) %>%
  ungroup()

#########

copom_join <- focus_selic_copom %>% 
  select(dataReferencia, nroReuniao, Media, Mediana) %>%
  mutate(dataReferencia = as.Date(dataReferencia))

###### precisa tirar os feriados por causa da reuniao de 2017/09/06, que ocorreu em
###### vespera do feriado, assim, a taxa passou a valer apenas no dia 8 e não no 7.

bizdays("2007-01-01", "2026-01-01", "Brazil/ANBIMA")

selic_meta_df <- selic_meta_df %>%
  filter(is.bizday(date, "Brazil/ANBIMA")) %>%
  arrange(date) %>%
  mutate(
    selic_meta_forw = lead(selic_meta)
  ) %>%
  filter(date < "2026-01-01") %>%
  left_join(copom_join, by = c("date" = "dataReferencia")) %>%
  mutate(choque_media = selic_meta_forw - Media,
         choque_mediana = selic_meta_forw - Mediana)

choque_mensal <- selic_meta_df %>%
  mutate(
    mes = floor_date(date, unit = "month")
  ) %>%
  group_by(mes) %>%
  summarise(
    choque_media = sum(choque_media, na.rm = TRUE),
    choque_mediana = sum(choque_mediana, na.rm = TRUE),
    .groups = "drop"
  )

#check sazonalidade

acf_choque <- acf(
  na.omit(choque_mensal$choque_mediana), 
  lag.max = 36, 
  plot = TRUE,
  main = "ACF - Choque Mediana"
)

serie_choque <- ts(na.omit(choque_mensal$choque_mediana), frequency = 12)

##### conferir grafico choques

choque_mensal_long <- choque_mensal %>%
  pivot_longer(
    cols = c(choque_media, choque_mediana),
    names_to = "tipo",
    values_to = "choque"
  )

ggplot(choque_mensal_long, aes(x = mes, y = choque, color = tipo)) +
  geom_hline(yintercept = 0, linewidth = 0.5) +
  geom_line(linewidth = 0.8) +
  labs(
    x = NULL,
    y = "Choque da Selic (p.p.)",
    color = NULL
  ) +
  scale_color_manual(
    values = c(
      "choque_media" = "steelblue",
      "choque_mediana" = "firebrick"
    ),
    labels = c(
      "choque_media" = "Média",
      "choque_mediana" = "Mediana"
    )
  ) +
  theme_minimal()

########

ipca_cons_cols <- ipca_cons %>% select(periodo, grupo, Valor) %>% 
  pivot_wider(names_from = grupo,
              values_from = Valor) %>%
  rename(ipca = `Índice geral`,
         ipca_alim = `1.Alimentação e bebidas`,
         ipca_alim_dom = `11.Alimentação no domicílio`,
         ipca_alim_foradom = `12.Alimentação fora do domicílio`)


livres <- get_series(
  c(
    livres = 11428,
    alim_domicilio = 27864,
    bens_industriais = 27863,
    servicos = 10844
  ),
  start_date = "2007-01-01",
  end_date = "2026-01-01"
)

tabela_precos_livres <- livres %>% 
  reduce(full_join, by = colnames(livres[[1]])[1])

dados <- dados %>% 
  left_join(ipca_cons_cols, by = c("periodo" = "periodo")) %>% 
  left_join(choque_mensal, by = c("periodo" = "mes")) %>%
  left_join(exp_ipca_12m, by = c("periodo" = "periodo")) %>% 
  left_join(tabela_precos_livres, by = c("periodo" = "date"))


###### RODAR AQUIIIIIIIIII
###### EDITAR MUTATES DE DADOS AQUI!!!!!!!!

### dessazonalizar todas as séries

series_desaz <- dados %>% select(-periodo, -ano, -meta_inf, -choque_media, -choque_mediana) %>% colnames()
print(series_desaz)

dados_desaz <- dados

for (col in series_desaz) {
  
  if (col %in% c("choque_media", "choque_mediana")) {
    next
  }
  
  serie_ts <- ts(dados_desaz[[col]], start = c(2007, 1), frequency = 12)
  
  # Roda o X-13 e sobrescreve direto no dados_final
  dados_desaz[[col]] <- as.numeric(final(seas(serie_ts)))
}

#####

dados_final <- dados_desaz %>% mutate(
  log_dolar = log(dolar_medmen),
  dif_log_dolar = log_dolar - lag(log_dolar),
  dif_dolar = dolar_medmen - lag(dolar_medmen),
  log_petro = log(mcoilbrentEU),
  dif_log_petro = log_petro - lag(log_petro),
  dif_petro = mcoilbrentEU - lag(mcoilbrentEU),
  dif_selic = selic_ac_mes - lag(selic_ac_mes),
  log_comm = log(commodities_BR),
  dif_log_commod = log_comm - lag(log_comm),
  desvio_ipca = exp_12m_mediana - meta_inf,
  dif_desvio_ipca = desvio_ipca - lag(desvio_ipca),
  log_ibc = log(ibc_br),
  dif_log_ibc = log_ibc - lag(log_ibc),
  nivel_precos_alim = 100 * cumprod(1 + ipca_alim / 100),
  nivel_precos_livres_alim = 100 * cumprod(1+alim_domicilio/100))

hp_result <- hpfilter(dados_final$ibc_br, freq = 14400)
dados_final$ibc_br_ciclico <- hp_result$cycle

plot(hp_result)

variaveis_data <- dados_final %>%
  select(
    -periodo,
    -ano,
    -dolar_medmen,
    -log_dolar,
    -log_petro,
    -mcoilbrentEU,
    -ibc_br,
    -selic_ac_mes,
    -choque_media,
    -meta_inf,
    -desvio_ipca
  ) %>%
  names()

variaveis_data

dados_final <- dados_final %>%
  drop_na(all_of(variaveis_data))

##### olhometro

dados_long <- dados_final %>%
  select(periodo, all_of(variaveis_data)) %>%
  pivot_longer(
    cols = all_of(variaveis_data),
    names_to = "variavel",
    values_to = "valor"
  )

ggplot(dados_long, aes(x = periodo, y = valor)) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~ variavel, scales = "free_y", ncol = 2) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

####### estacionariedade

resultados_estacionariedade <- map_dfr(variaveis_data, function(var) {
  
  adf <- adf.test(dados_final[[var]])
  kpss <- kpss.test(dados_final[[var]])
  
  tibble(
    variavel = var,
    adf_statistica = as.numeric(adf$statistic),
    adf_pvalor = adf$p.value,
    kpss_statistica = as.numeric(kpss$statistic),
    kpss_pvalor = kpss$p.value
  )
})

resultados_estacionariedade <- resultados_estacionariedade %>% 
  mutate(interpret = ifelse(adf_pvalor < 0.05, "I(0)", "Não é I(0)"))

resultados_estacionariedade

######## sazonalidade

acf_dados <- map_dfr(variaveis_data, function(var) {
  
  serie <- na.omit(dados_final[[var]])
  
  acf_result <- acf(
    serie,
    lag.max = 36,
    plot = FALSE
  )
  
  tibble(
    variavel = var,
    lag = as.numeric(acf_result$lag),
    acf = as.numeric(acf_result$acf)
  )
})

limite_acf <- 1.96 / sqrt(nrow(dados_final))

ggplot(acf_dados, aes(x = lag, y = acf)) +
  geom_hline(yintercept = 0) +
  geom_hline(
    yintercept = c(-limite_acf, limite_acf),
    linetype = "dashed"
  ) +
  geom_segment(
    aes(xend = lag, yend = 0)
  ) +
  facet_wrap(~ variavel, scales = "free_y", ncol = 2) +
  scale_x_continuous(
    breaks = c(12, 24, 36)
  ) +
  labs(
    x = "Defasagem",
    y = "Autocorrelação"
  ) +
  theme_minimal()

#### matriz de correlacao

vars_para_remover <- c("ipca", "ipca_alim_dom", "ipca_alim_foradom")
variaveis_corr <- variaveis_data[!variaveis_data %in% vars_para_remover]

matriz_corr <- dados_final %>%
  select(any_of(variaveis_corr)) %>%
  cor(use = "pairwise.complete.obs", method = "pearson")

print(round(matriz_corr, 2))

corr_long <- matriz_corr %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "var1") %>%
  pivot_longer(
    cols = -var1,
    names_to = "var2",
    values_to = "correlacao"
  )

ggplot(corr_long, aes(x = var1, y = var2, fill = correlacao)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", correlacao)), color = "black", size = 3.5) +
  scale_fill_gradient2(
    low = "#B2182B",
    mid = "#F7F7F7",
    high = "#2166AC",
    midpoint = 0,
    limit = c(-1, 1),
    name = "Correlação"
  ) +
  labs(
    title = "Matriz de Correlação das Variáveis",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, face = "bold"),
    axis.text.y = element_text(face = "bold"),
    panel.grid = element_blank()
  )

########## check desaz

dados_plot <- bind_rows(
  ipca_cons_cols %>% 
    select(periodo, ipca_alim) %>%
    mutate(serie = "Original"),
  
  dados_final %>% 
    select(periodo, ipca_alim) %>%
    mutate(serie = "Dessazonalizada")
)

ggplot(dados_plot, aes(x = periodo, y = ipca_alim)) +
  geom_line() +
  facet_wrap(~serie, ncol = 1) +
  labs(
    x = NULL,
    y = "IPCA - Alimentação"
  ) +
  theme_minimal()

#### RODAR MODELO POR PROJECOES LOCAIS 
#### - passar essa parte pra um script separado e salvar os dados num rds dps

library(sandwich)
library(car)
library(lmtest)
library(sandwich)
library(dplyr)
library(tibble)

###### REFAZER O EXERCICIO COM A MESMA SÉRIE Q O BCB

dados_LP_livres_alim <- dados_final %>%
  arrange(periodo) %>%
  mutate(
    log_nivel_precos_alim = log(nivel_precos_alim),
    dif_log_nivel_precos_alim = log_nivel_precos_alim - lag(log_nivel_precos_alim),
    log_nivel_precos_livres_alim = log(nivel_precos_livres_alim),
    dif_log_nivel_precos_livres_alim = log_nivel_precos_livres_alim - lag(log_nivel_precos_livres_alim),
    dif_log_dolar_lag_1 = lag(dif_log_dolar, 1),
    ibc_br_ciclico_lag_1 = lag(ibc_br_ciclico, 1),
    ibc_br_ciclico_lag_2 = lag(ibc_br_ciclico, 2),
    indice_alim_fao_lag_1 = lag(indice_alim_fao, 1),
    indice_alim_fao_lag_2 = lag(indice_alim_fao, 2),
    dif_log_petro_lag_1 = lag(dif_log_petro, 1),
    dif_log_petro_lag_2 = lag(dif_log_petro, 2),
    log_fao = log(indice_alim_fao),
    dif_log_fao = log_fao - lag(log_fao),
    dif_log_fao_lag_1 = lag(dif_log_fao),
    dif_alim_fao = indice_alim_fao - lag(indice_alim_fao),
    dif_log_nivel_precos_livres_alim_lag_1 = lag(dif_log_nivel_precos_livres_alim, 1)
  )

########## teste adf variaveis modelo

vars_adf <- c(
  "log_dolar",
  "log_fao",
  "log_petro",
  "ibc_br_ciclico",
  "log_nivel_precos_livres_alim",
  "log_nivel_precos_alim",
  "dif_log_dolar",
  "dif_log_fao",
  "dif_log_petro",
  "dif_log_nivel_precos_livres_alim",
  "dif_log_nivel_precos_alim"
)

lp_data_adf <- dados_LP_livres_alim %>%
  select(all_of(vars_adf)) %>% drop_na()  

adf_modelo <- map_dfr(vars_adf, function(var) {
  
  adf <- adf.test(lp_data_adf[[var]])
  kpss <- kpss.test(lp_data_adf[[var]])
  
  tibble(
    variavel = var,
    adf_statistica = as.numeric(adf$statistic),
    adf_pvalor = adf$p.value,
    kpss_statistica = as.numeric(kpss$statistic),
    kpss_pvalor = kpss$p.value
  )
})

adf_modelo <- adf_modelo %>% 
  mutate(interpret = ifelse(adf_pvalor < 0.05, "I(0)", "Não é I(0)"))

print(adf_modelo)

library(gt)

tabela_adf <- adf_modelo %>%
  mutate(
    Variável = case_when(
      variavel == "log_fao" ~ "Índice FAO (log)",
      variavel == "log_dolar" ~ "Taxa de câmbio (log)",
      variavel == "log_petro" ~ "Petróleo (log)",
      variavel == "ibc_br_ciclico" ~ "IBC-Br cíclico",
      variavel == "log_nivel_precos_livres_alim" ~ "Preços livres – alimentação",
      variavel == "dif_log_dolar" ~ "Δ log câmbio",
      variavel == "dif_log_fao" ~ "Δ log índice FAO",
      variavel == "dif_log_petro" ~ "Δ log petróleo",
      variavel == "log_nivel_precos_alim" ~ "Ipca Grupo 1 (log)",
      variavel == "dif_log_nivel_precos_alim" ~ "Δ log ipca grupo 1",
      variavel == "dif_log_nivel_precos_livres_alim" ~ 
        "Δ log preços livres – alimentação"
    ),
    `ADF (p-valor)` = adf_pvalor,
    Resultado = ifelse(adf_pvalor < 0.05, "Estacionária", "Não estacionária")
  ) %>%
  select(
    Variável,
    `ADF (p-valor)`,
    Resultado
  ) %>%
  gt() %>%
  fmt_number(
    columns = `ADF (p-valor)`,
    decimals = 3
  ) %>%
  cols_align(
    align = "center",
    columns = c(`ADF (p-valor)`, Resultado)
  ) %>%
  tab_header(
    title = "Teste de estacionariedade — ADF"
  ) %>%
  tab_options(
    table.font.size = px(14),
    heading.title.font.size = px(20),
    column_labels.font.weight = "bold"
  )

tabela_adf

##### setar regressao

H <- 24

for (h in 0:H) {
  dados_LP_livres_alim[[paste0("y_h", h)]] <-
    lead(dados_LP_livres_alim$log_nivel_precos_livres_alim, h) -
    lag(dados_LP_livres_alim$log_nivel_precos_livres_alim, 1)
}

vars_rhs_livres_alim <- c(
  "dif_log_dolar",
  "dif_log_dolar_lag_1",
  "dif_log_fao",
  "dif_log_petro",
  "dif_log_petro_lag_1",
  "ibc_br_ciclico",
  "dif_log_nivel_precos_livres_alim_lag_1"
)


plot(dados_LP_livres_alim$dif_log_fao)
plot(dados_LP_livres_alim$log_fao)

rhs_livres_alim <- paste0(vars_rhs_livres_alim, collapse = " + ")

resultados_LP_livres_alim <- map_dfr(0:H, function(h) {
  
  dep <- paste0("y_h", h)
  
  formula_lp <- as.formula(
    paste0(dep, " ~ ", rhs_livres_alim)
  )
  
  modelo <- lm(
    formula_lp,
    data = dados_LP_livres_alim
  )
  
  vcov_nw <- NeweyWest(
    modelo,
    lag = h, #### a sugestão do que pesquisei é usar h, mas o automatico não mudou quase nada, então ok
    prewhite = FALSE,
    adjust = TRUE
  )
  
  teste_nw <- coeftest(
    modelo,
    vcov. = vcov_nw
  )
  
  beta <- teste_nw["dif_log_dolar", "Estimate"]
  se <- teste_nw["dif_log_dolar", "Std. Error"]
  
  tibble(
    h = h,
    estimate = beta,
    std_error = se,
    p_value = teste_nw["dif_log_dolar", "Pr(>|t|)"],
    ic_inf = beta - qnorm(0.95) * se,
    ic_sup = beta + qnorm(0.95) * se
  )
})


##### GRÁFICO IRF

grafico_irf = 
  ggplot(resultados_LP_livres_alim, aes(x = h, y = estimate)) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.5,
    color = "gray50"
  ) +
  geom_ribbon(
    aes(ymin = ic_inf, ymax = ic_sup),
    fill = "#FF1B1C",
    alpha = 0.18
  ) +
  geom_line(
    color = "#F84444",
    linewidth = 1
  ) +
  geom_point(
    color = "#F84444",
    size = 2
  ) +
  scale_x_continuous(
    breaks = seq(0, H, by = 3),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  labs(
    title = "Repasse Cambial: Alimentação (Preços Livres)",
    x = "Horizonte (meses)",
    y = "Resposta acumulada do nível de preços"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14,
      hjust = 0.5
    ),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      color = "gray90",
      linewidth = 0.4
    ),
    plot.margin = margin(10, 15, 10, 10)
  )

print(
  resultados_LP_livres_alim %>%
    arrange(h) %>%
    select(h, estimate, std_error, p_value), n = 1000
)


##########

dados_LP_livres_alim <- dados_LP_livres_alim %>%
  mutate(
    dif_log_fao_lag_2 = lag(dif_log_fao, 2)
  )

mod_h0_atual <- lm(
  as.formula(paste0("y_h0 ~ ", rhs_livres_alim)),
  data = dados_LP_livres_alim
)

vif_h0 <- vif(mod_h0_atual)

tabela_vif <- tibble(
  Variavel = names(vif_h0),
  VIF = as.numeric(vif_h0)
) %>% 
  arrange(desc(VIF))

print("--- TABELA VIF (Modelo Atual) ---")
print(tabela_vif)

cor.test(
  dados_LP_livres_alim$dif_log_fao,
  dados_LP_livres_alim$dif_log_commod,
  use = "complete.obs",
  method = "pearson"
)

#### coeficientes add slides

library(gt)
library(dplyr)

coef_lp <- resultados_LP_livres_alim %>%
  arrange(h) %>%
  mutate(
    `β` = estimate,
    `Erro-padrão (NW)` = std_error,
    `p-valor` = p_value
  ) %>%
  select(h, `β`, `Erro-padrão (NW)`, `p-valor`)


# =========================
# HORIZONTES 0–12
# =========================

tabela_0_12 <- coef_lp %>%
  filter(h <= 12) %>%
  gt() %>%
  fmt_number(
    columns = c(`β`, `Erro-padrão (NW)`),
    decimals = 3
  ) %>%
  fmt_number(
    columns = `p-valor`,
    decimals = 6
  ) %>%
  cols_label(
    h = "h"
  ) %>%
  tab_header(
    title = "Horizontes 0–12"
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  tab_options(
    table.font.size = px(13),
    heading.title.font.size = px(16),
    column_labels.font.weight = "bold",
    data_row.padding = px(3)
  )


# =========================
# HORIZONTES 13–24
# =========================

tabela_13_24 <- coef_lp %>%
  filter(h >= 13) %>%
  gt() %>%
  fmt_number(
    columns = c(`β`, `Erro-padrão (NW)`),
    decimals = 3
  ) %>%
  fmt_number(
    columns = `p-valor`,
    decimals = 6
  ) %>%
  cols_label(
    h = "h"
  ) %>%
  tab_header(
    title = "Horizontes 13–24"
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  tab_options(
    table.font.size = px(13),
    heading.title.font.size = px(16),
    column_labels.font.weight = "bold",
    data_row.padding = px(3)
  )


tabela_0_12
tabela_13_24

####testes

v1 <- adf.test(na.omit(dados_LP_livres_alim$log_nivel_precos_livres_alim))

v2 <- adf.test(na.omit(dados_LP_livres_alim$dif_log_nivel_precos_livres_alim))

print(v1)
print(v2)

library(writexl)

write_xlsx(coef_lp, 
            "coef_lp_ALIMLIVRES.xlsx")
