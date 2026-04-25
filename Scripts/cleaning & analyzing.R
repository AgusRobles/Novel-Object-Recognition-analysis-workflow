rm(list = ls())


source("R/load_packages.R")
source("R/statistics_functions.R")

# Limpieza de datos -------------------------------------------------------


datos <- read.csv("raw_data/Testeo.csv", stringsAsFactors = T, fileEncoding = "Latin1")

datos <- datos %>%
  mutate(
    Tratamiento = fct_recode(Tratamiento, VEH_1 = "VEH"), #Completar nombres de tratamientos
    Animal = factor(Animal), 
    Tipo_objeto = factor(tolower(Tipo_objeto), levels = c("familiar", "novedoso"))) %>%
  rename(Sex = Sexo) %>% 
  drop_na() 

str(datos)

datos$Tratamiento <- factor(datos$Tratamiento, levels = c("VEH_1", "RG")) ## CHEQUEAR NOMBRES TRATAMIENTOS ##

datos <- filter(datos, Animal != "3II")


datos_TS <- datos %>% 
  filter(Sesion == "TS")

datos_TR <- datos %>% 
  filter(Sesion == "TR")


# Tabla para calcular ID

datos2 <- subset(datos, select = c(-Objeto, -Posicion)) 

datos_id <- spread(datos2, Tipo_objeto, t_exploracion)

# Creo la columna tiempo total e ID

datos_id <- datos_id %>% 
  mutate(t_total = novedoso + familiar, 
         id_beta = novedoso/t_total, 
         ID = (novedoso - familiar)/t_total * 100)

str(datos_id)

datos_id_TS <- datos_id %>% 
  filter(Sesion == "TS")

datos_id_TR <- datos_id %>% 
  filter(Sesion == "TR")

# Descriptiva -------------------------------------------------------------


desc_t_ts <- descriptiva(datos, c("Tratamiento", "Tipo_objeto"), "t_exploracion")
desc_t_ts

desc_id_ts <- descriptiva(datos_id, c("Tratamiento"), "id_beta")
desc_id_ts

desc_ttot_ts <- descriptiva(datos_id, "Tratamiento", "t_total")
desc_ttot_ts


desc_t_tr <- descriptiva(datos_TR, c("Tratamiento", "Tipo_objeto"), "t_exploracion")
desc_t_tr

desc_id_tr <- descriptiva(datos_id_TR, c("Tratamiento"), "id_beta")
desc_id_tr

desc_ttot_tr <- descriptiva(datos_id_TR, "Tratamiento", "t_total")
desc_ttot_tr

# Exploratory plots -------------------------------------------------------

## TESTEO

# Tiempo de exploración por objeto 

ggplot(data = desc_t_ts, mapping = aes(x = interaction(Tipo_objeto, Tratamiento), y = media, color = Tratamiento)) +
  geom_point(size = 4) +
  geom_line(aes(group = Tratamiento), linewidth = 0.8) +
  geom_jitter(data = datos_TS, aes(x = interaction(Tipo_objeto, Tratamiento), y = t_exploracion), width = 0, size = 1.5) +
  geom_line(data = datos_TS, aes(x = interaction(Tipo_objeto, Tratamiento), y = t_exploracion, group = Animal), linewidth = 0.5, alpha = 0.5) + 
  labs(x = "Objeto", 
       y = "tiempo de exploración (s)", 
       title = "Tiempo de exploración durante el testeo") +
  scale_x_discrete(labels = c("Familiar", "Novedoso", "Familiar", "Novedoso"))

# ID

ggplot(data = desc_id_ts, mapping = aes(x = Tratamiento, y = media, color = Tratamiento)) +
  geom_point(size = 4) +
  geom_jitter(data = datos_id_TS, aes(x = Tratamiento, y = id_beta), width = 0, size = 1.5) +
  labs(x = "Objeto", y = "ID%", title = "Índice de discriminación TS")

# Tiempo total

ggplot(data = desc_ttot_ts, mapping = aes(x = Tratamiento, y = media, color = Tratamiento)) +
  geom_point(size = 4) +
  geom_jitter(data = datos_id_TS, aes(x = Tratamiento, y = t_total), width = 0, size = 1.5) +
  labs(x = "", y = "tiempo de exploración", title = "Tiempo total de exploración")


# Modelos -----------------------------------------------------------------

# Modelo 1: t_exploracion ~ Tratamiento * Tipo_objeto (TS)

modelo01 <- lmer(t_exploracion ~ Tratamiento * Tipo_objeto + (1|Sex / Animal), datos)

supuestos_m01 <- supuestos(modelo01)

anova_m01 <- Anova(modelo01, test.statistic = "F")

emmeans(modelo01, pairwise ~ Tipo_objeto | Tratamiento)
emm_m01 <- emmeans(modelo01, pairwise ~ Tipo_objeto | Tratamiento)


# Modelo 2: id ~ Tratamiento (TS)

modelo02 <- glmmTMB(id_beta ~ Tratamiento + (1 | Sex), family = beta_family(link = "logit"), data = datos_id_TS)

supuestos_m02 <- simulation_modelo2 <- simulateResiduals(fittedModel = modelo02, refit = T, plot = T)

hist(supuestos_m02)

testDispersion(supuestos_m02)

plotResiduals(modelo02, datos_id_TS$Tratamiento)

anova_m02 <- Anova(modelo02)

emm_m02 <- emmeans(modelo02, pairwise ~ Tratamiento)


# Modelo 3: t_total ~ Tratamiento (TS)

modelo03 <- lmer(t_total ~ Tratamiento + (1| Sex), datos_id_TS)

supuestos_m03 <- supuestos(modelo03)

anova_m03 <- Anova(modelo03)

emm_m03 <- emmeans(modelo03, pairwise ~ Tratamiento)


# Modelo 4: t_exploracion ~ Tratamiento * Tipo_objeto (TR)

modelo04 <- lmer(t_exploracion ~ Tratamiento * Tipo_objeto + (1|Animal), datos_TR)

supuestos_m04 <- supuestos(modelo04)

anova_m04 <- Anova(modelo04, test.statistic = "F")

emmeans(modelo04, pairwise ~ Tipo_objeto | Tratamiento)
emm_m04 <- emmeans(modelo04, pairwise ~ Tipo_objeto * Tratamiento)


# Modelo 5: id ~ Tratamiento (TR)

modelo05 <- glmmTMB(id_beta ~ Tratamiento, family = beta_family(link = "logit"),
                    dispformula = ~1|Tratamiento, data = datos_id_TR)

supuestos_m05 <- simulation_modelo5 <- simulateResiduals(fittedModel = modelo05, refit = T, plot = T)

hist(simulation_modelo05)

testDispersion(simulation_modelo05)

plotResiduals(modelo05, datos_id_TR$Tratamiento)

anova_m05 <- Anova(modelo05)

emm_m05 <- emmeans(modelo05, pairwise ~ Tratamiento)


# Modelo 6: t_total ~ Tratamiento (TR)

modelo06 <- lm(t_total ~ Tratamiento, datos_id_TR)

supuestos_m06 <- supuestos(modelo06)

anova_m06 <- Anova(modelo06)

emm_m06 <- emmeans(modelo06, pairwise ~ Tratamiento)


# Save --------------------------------------------------------------------

#Tables

saveRDS(datos_TS, "data/datosTS_clean.rds")
saveRDS(datos_id_TS, "data/datosTS_id.rds")

saveRDS(datos_TR, "data/datosTR_clean.rds")
saveRDS(datos_id_TR, "data/datosTR_id.rds")



# Descriptive statistics

saveRDS(
  list(
    desc_t_ts = desc_t_ts,
    desc_id_ts = desc_id_ts,
    desc_ttot_ts = desc_ttot_ts
  ),
  "data/descriptives_TS.rds"
)

saveRDS(
  list(
    desc_t_tr = desc_t_tr,
    desc_id_tr = desc_id_tr,
    desc_ttot_tr = desc_ttot_tr
  ),
  "data/descriptives_TR.rds"
)

# Models 

saveRDS(
  list(
    modelo01,
    supuestos_m01, 
    anova_m01, 
    emm_m01
  ),
  "results/modelo_texp_TS.rds"
)

saveRDS(
  list(
    modelo02,
    supuestos_m02, 
    anova_m02, 
    emm_m02
  ),
  "results/modelo_DI_TS.rds"
)

saveRDS(
  list(
    modelo03,
    supuestos_m03, 
    anova_m03, 
    emm_m03
  ),
  "results/modelo_ttot_TS.rds"
)

saveRDS(
  list(
    modelo04,
    supuestos_m04, 
    anova_m04, 
    emm_m04
  ),
  "results/modelo_texp_TR.rds"
)

saveRDS(
  list(
    modelo05,
    supuestos_m05, 
    anova_m05, 
    emm_m05
  ),
  "results/modelo_DI_TR.rds"
)

saveRDS(
  list(
    modelo06,
    supuestos_m06, 
    anova_m06, 
    emm_m06
  ),
  "results/modelo_ttot_TR.rds"
)