# importação --------------------------------------------------------------

library("googledrive")
library("tidyverse")
library("haven") # para ler arquivos .sav
library("googlesheets4")
library("flextable")

# Ler arquivos
## É importante que a sheet seja salva como Planilha Google
## e não como .XLSX, .CSV, .SAV... read_sheet lê no formato do Google.

# url <- "https://docs.google.com/spreadsheets/d/1lbLgsoh1dFUEB3nr42g9BdmNbCPvA1LNew8dTocNT-I/edit?usp=sharing"
# df_raw <- read_sheet(url, range = "A1:CF2407")
# View(df_raw)

# df_raw <- readxl::read_excel("dados/bd_2006_2018.xlsx")
# View(df_raw)

df_raw <- haven::read_sav("dados/BANCO_PONDERADO_COMPLETO (2006-2019).sav")

# Organizando a base de dados ----------------------------------------------

## Selecionando as variáveis que vamos trabalhar
df <- df_raw |> 
  dplyr::select(
    c("ANO_INGRESSO", "P1.sexo", "P2.idade",
           "P3.cor", "P32.orsex")
    )

## Modificando o nome das colunas, para ficar mais fácil acessar.
df <- df |> 
  dplyr::rename(
    ano = ANO_INGRESSO,
    sexo = P1.sexo,
    idade = P2.idade,
    cor = P3.cor,
    orsex = P32.orsex
  ) 

## Modificando o tipo das variáveis
df <- df |> 
  dplyr::mutate(
    sexo = case_when(
      sexo == 1 ~ "Masculino",
      TRUE ~ "Feminino"
    ),
    cor = factor(cor, levels = c(1, 2, 3, 4, 5),
                 labels = c("Branca", "Preta", "Parda", "Amarela", "Indígena")),
    orsex = factor(orsex, levels = c(1, 2, 3, 4),
                 labels = c("somente homens", "somente mulheres", "ambos", "outros"))
  )

## Removendo valores indesejados
df <- df |>
  dplyr::filter(
    !is.na(cor)
  )


# Estatística descritiva básica -------------------------------------------

df |> 
  summarise(across(where(is.numeric),
                   list(media = mean, mediana = median, desvio = sd,
                        min = min, max = max),
                   na.rm = TRUE)) |> 
  View()

## Distribuição de variáveis categóricas
df |> count(sexo) |> 
  mutate(perc = n / sum(n) * 100) |> 
  print()

df |> count(cor) |> 
  mutate(perc = n / sum(n) * 100) |> 
  print()

df |> count(orsex) |> 
  mutate(perc = n / sum(n) * 100) |> 
  print()

## Tabelas cruzadas
df |> count(sexo, cor) |> 
  mutate(
    porcentagem = paste0(round((n / sum(n)) * 100, 1), "%")) |> 
  flextable::flextable() |> 
  set_caption(" Tabela cruzada de Sexo por Cor") |> 
  add_footer_lines("Elaborado por Artur Damião (2025), a partir dos dados do PET") |> 
  font(fontname = "Modern", part = "all")|>
  align(align = "left", part = "all")

df |> count(sexo, orsex) |> 
  mutate(
    porcentagem = paste0(round((n / sum(n)) * 100, 1), "%")) |> 
  flextable::flextable() |> 
  font(fontname = "Modern", part = "all")|>
  align(align = "left", part = "all")

df |> count(cor, orsex) |> 
  mutate(
    porcentagem = paste0(round((n / sum(n)) * 100, 1), "%")) |> 
  flextable::flextable() |> 
  font(fontname = "Modern", part = "all")|>
  align(align = "left", part = "all")


# Visualização gráfica ----------------------------------------------------

# Ano e sexo
df |> 
  count(ano, sexo) |> 
  group_by(ano) |> 
  mutate(perc = n / sum(n) * 100) |> 
  ggplot(aes(x = factor(ano), y = perc, color = sexo, group = sexo)) +
  geom_line(size = 1.1) +
  geom_point() +
  labs(title = "Variação da Distribuição de Sexo ao Longo dos Anos",
       subtitle = "(2006-2017)",
       caption = "Elaborado por Artur Damião (2025), a partir dos dados do PET",
       x = "Ano", 
       y = "Porcentagem (%)",
       color = "Sexo") +
  theme_classic()

# Ano e orientação
df |> 
  filter(!is.na(orsex) & orsex != "outros") |> 
  count(ano, orsex) |> 
  group_by(ano) |> 
  mutate(perc = n / sum(n) * 100) |> 
  ggplot(aes(x = factor(ano), y = perc, color = orsex, group = orsex)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title = "Variação da Distribuição de Orientação Sexual ao Longo dos Anos",
       subtitle = "(2014-2017)",
       caption = "Elaborado por Artur Damião (2025), a partir dos dados do PET",
       x = "Ano", 
       y = "Porcentagem (%)",
       color = "Sexo") +
  theme_classic()



