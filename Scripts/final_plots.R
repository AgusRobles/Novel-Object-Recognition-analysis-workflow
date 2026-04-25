rm(list = ls())

source("R/load_packages.R")
source("R/themes.R")
source("R/plotting_functions.R")

datos_TS <- readRDS("/data/datosTS_clean.rds")
datos_id_TS <- readRDS("/data/datosTS_id.rds")
desc_TS <- readRDS("/data/descriptives_TS.rds")

datos_TR <- readRDS("/data/datosTR_clean.rds")
datos_id_TR <- readRDS("/data/datosTR_id.rds")
desc_TR <- readRDS("/data/descriptives_TR.rds")



# TESTEO ------------------------------------------------------------------



t_exp <- plot_behavior("exploration", datos_TS, desc_TS$desc_t_ts)
t_exp

ggsave("/figures/Tiempo de exploracion por objeto TS.pdf", t_exp, width = 7, height = 7)

DI <- plot_behavior("DI", datos_id_TS, desc_TS$desc_id_ts)
DI

ggsave("/figures/DI TS.pdf", DI, width = 5.5, height = 7)


t_tot <- plot_behavior("total", datos_id_TS, desc_TS$desc_ttot_ts)
t_tot

ggsave("/figures/Tiempo de exploracion total TS.pdf", t_tot, width = 5.5, height = 7)



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
