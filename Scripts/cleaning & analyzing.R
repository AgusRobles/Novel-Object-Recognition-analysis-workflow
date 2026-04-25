rm(list = ls())


source("Functions/load_packages.R")
source("Functions/statistics_functions.R")

# Limpieza de datos -------------------------------------------------------


data <- read.csv("raw_data/test_dataset.csv", stringsAsFactors = T, fileEncoding = "Latin1")

data <- data %>%
  drop_na() 

str(data)

data$Treatment <- factor(data$Treatment, levels = c("1_min", "3_min")) 




TS_data <- data %>% 
  filter(Session == "TS")

TR_data <- data %>% 
  filter(Session == "TR")


# Tabla para calcular ID

data2 <- subset(data, select = c(-Object, -Location)) 

di_data <- spread(data2, Type_object, Exploration)

# Creo la columna tiempo total e ID

di_data <- di_data %>% 
  mutate(Total = Nov + Fam, 
         beta_di = Nov/Total, 
         DI = (Nov - Fam)/Total * 100)

str(di_data)

TS_di_data <- di_data %>% 
  filter(Session == "TS")

TR_di_data <- di_data %>% 
  filter(Session == "TR")

# Descriptive statistics --------------------------------------------------


# Exploration time for each object
desc_t_ts <- descriptive(TS_data, c("Treatment", "Type_object"), "Exploration")
desc_t_ts

# Discrimination index as a proportion of time exploring novel object
desc_di_ts <- descriptive(TS_di_data, c("Treatment"), "beta_di")
desc_di_ts

# Total exploration time
desc_ttot_ts <- descriptive(TS_di_data, "Treatment", "Total")
desc_ttot_ts


desc_t_tr <- descriptive(TR_data, c("Treatment", "Type_object"), "Exploration")
desc_t_tr

desc_di_tr <- descriptive(TR_di_data, c("Treatment"), "beta_di")
desc_di_tr

desc_ttot_tr <- descriptive(TR_di_data, "Treatment", "Total")
desc_ttot_tr

# Exploratory plots -------------------------------------------------------

## TEST

# Exploration time by object

ggplot(data = desc_t_ts, mapping = aes(x = interaction(Type_object, Treatment), y = avg, color = Treatment)) +
  geom_point(size = 4) +
  geom_line(aes(group = Treatment), linewidth = 0.8) +
  geom_jitter(data = TS_data, aes(x = interaction(Type_object, Treatment), y = Exploration), width = 0, size = 1.5) +
  geom_line(data = TS_data, aes(x = interaction(Type_object, Treatment), y = Exploration, group = Animal), linewidth = 0.5, alpha = 0.5) + 
  labs(x = "Object", 
       y = "Exploration (s)", 
       title = "Exploration times during testing session") +
  scale_x_discrete(labels = c("Familiar", "Novedoso", "Familiar", "Novedoso"))

# DI

ggplot(data = desc_di_ts, mapping = aes(x = Treatment, y = avg, color = Treatment)) +
  geom_point(size = 4) +
  geom_jitter(data = TS_di_data, aes(x = Treatment, y = beta_di), width = 0, size = 1.5) +
  labs(x = "Objet", y = "DI", title = "Discrimination Index")

# Tiempo total

ggplot(data = desc_ttot_ts, mapping = aes(x = Treatment, y = avg, color = Treatment)) +
  geom_point(size = 4) +
  geom_jitter(data = TS_di_data, aes(x = Treatment, y = Total), width = 0, size = 1.5) +
  labs(x = "", y = "Exploration (s)", title = "Total exploration time")


# Models -----------------------------------------------------------------

# Model 1: t_exploracion ~ Tratamiento * Tipo_objeto (TS)

model01 <- lmer(Exploration ~ Treatment * Type_object + (1|Sex / Animal), data)

assumpt_m01 <- assumptions(model01)

anova_m01 <- Anova(model01, test.statistic = "F")

emm_m01 <- emmeans(model01, pairwise ~ Type_object | Treatment)


# Modelo 2: id ~ Tratamiento (TS)

model02 <- glmmTMB(beta_di ~ Treatment + (1 | Sex), family = beta_family(link = "logit"), data = TS_di_data)

assumpt_m02 <- simulateResiduals(fittedModel = model02, refit = T, plot = T)

hist(assumpt_m02)

testDispersion(assumpt_m02)

plotResiduals(model02, TS_di_data$Treatment)

anova_m02 <- Anova(model02)

emm_m02 <- emmeans(model02, pairwise ~ Treatment)


# Modelo 3: t_total ~ Tratamiento (TS)

model03 <- lmer(Total ~ Treatment + (1| Sex), TS_di_data)

assumpt_m03 <- assumptions(model03)

anova_m03 <- Anova(model03)

emm_m03 <- emmeans(model03, pairwise ~ Treatment)


# Modelo 4: t_exploracion ~ Tratamiento * Tipo_objeto (TR)

model04 <- lmer(Exploration ~ Treatment * Type_object + (1|Animal), TR_data)

assumpt_m04 <- assumptions(model04)

anova_m04 <- Anova(model04, test.statistic = "F")

emm_m04 <- emmeans(modelo04, pairwise ~ Type_object | Treatment)


# Modelo 5: id ~ Tratamiento (TR)

model05 <- glmmTMB(beta_di ~ Treatment, family = beta_family(link = "logit"), data = TR_di_data)

assumpt_m05 <- simulateResiduals(fittedModel = model05, refit = T, plot = T)

hist(assumpt_m05)

testDispersion(assumpt_m05)

plotResiduals(model05, TR_di_data$Treatment)

anova_m05 <- Anova(model05)

emm_m05 <- emmeans(model05, pairwise ~ Treatment)


# Modelo 6: t_total ~ Tratamiento (TR)

model06 <- lm(Total ~ Treatment, TR_di_data)

assumpt_m06 <- assumptions(model06)

anova_m06 <- Anova(model06)

emm_m06 <- emmeans(model06, pairwise ~ Treatment)


# Save --------------------------------------------------------------------

#Tables

saveRDS(TS_data, "data/TSdata_clean.rds")
saveRDS(TS_di_data, "data/TSdata_di.rds")

saveRDS(TR_data, "data/TRdata_clean.rds")
saveRDS(TR_di_data, "data/TRdata_di.rds")



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
    model01,
    assumpt_m01, 
    anova_m01, 
    emm_m01
  ),
  "results/model_texp_TS.rds"
)

saveRDS(
  list(
    model02,
    assumpt_m02, 
    anova_m02, 
    emm_m02
  ),
  "results/model_DI_TS.rds"
)

saveRDS(
  list(
    model03,
    assumpt_m03, 
    anova_m03, 
    emm_m03
  ),
  "results/model_total_TS.rds"
)

saveRDS(
  list(
    model04,
    assumpt_m04, 
    anova_m04, 
    emm_m04
  ),
  "results/model_texp_TR.rds"
)

saveRDS(
  list(
    model05,
    assumpt_m05, 
    anova_m05, 
    emm_m05
  ),
  "results/model_DI_TR.rds"
)

saveRDS(
  list(
    model06,
    assumpt_m06, 
    anova_m06, 
    emm_m06
  ),
  "results/model_total_TR.rds"
)