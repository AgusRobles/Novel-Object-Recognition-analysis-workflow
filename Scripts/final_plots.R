rm(list = ls())

source("Functions/load_packages.R")
source("Functions/themes.R")
source("Functions/plotting_functions.R")

TS_data <- readRDS("data/TSdata_clean.rds")
TS_di_data <- readRDS("data/TSdata_di.rds")
desc_TS <- readRDS("data/descriptives_TS.rds")

TR_data <- readRDS("data/TRdata_clean.rds")
TR_di_data <- readRDS("data/TRdata_di.rds")
desc_TR <- readRDS("data/descriptives_TR.rds")



# TEST ------------------------------------------------------------------



t_exp <- plot_behavior("exploration", TS_data, desc_TS$desc_t_ts)
t_exp

ggsave("figures/Exploration by object.pdf", t_exp, width = 7, height = 7)

DI <- plot_behavior("DI", TS_di_data, desc_TS$desc_id_ts)
DI

ggsave("figures/DI.pdf", DI, width = 5.5, height = 7)


t_tot <- plot_behavior("total", TS_di_data, desc_TS$desc_ttot_ts, y_var = Total)
t_tot

ggsave("figures/Total exploration time.pdf", t_tot, width = 5.5, height = 7)



# ENTRENAMIENTO -----------------------------------------------------------



t_exp_tr <- plot_behavior("exploration", datos_TR, desc_TR$desc_t_tr)
t_exp_tr

ggsave("/figures/Tiempo de exploracion por objeto TR.pdf", t_exp, width = 7, height = 7)

DI_tr <- plot_behavior("DI", datos_id_TR, desc_TR$desc_id_tr)
DI_tr

ggsave("/figures/DI TR.pdf", DI_tr, width = 5.5, height = 7)


t_tot_tr <- plot_behavior("total", datos_id_TR, desc_TR$desc_ttot_tr)
t_tot_tr

ggsave("/figures/Tiempo de exploracion total TR.pdf", t_tot_tr, width = 5.5, height = 7)
