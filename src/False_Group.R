library(R2jags)
library(dplyr)
library(tidyr)
library(stringr)
library(tidybayes)
library(ggplot2)
library(posterior)
library(ggridges)
library(gridExtra)

setwd("/work/mrks/Exam")

files_u17 <- c("logfiles/logfile_3_U17.csv",
               "logfiles/logfile_20_U17.csv",
               "logfiles/logfile_25_U17.csv")

ids <- c("U17_3","U17_20","U17_25")

read_and_prepare <- function(file, id) {
  dat <- read.csv(file)
  dat$ID <- id
  
  stim <- as.integer(str_sub(dat$Video, -5, -5))
  
  resp <- dat$Decision
  resp[resp=="highest free man" | resp=="false highest free man" |
         resp=="highest free man2" | resp=="false highest free man2"] <- 1
  resp[resp=="link out" | resp=="false link out" |
         resp=="link out2" | resp=="false link out2"] <- 2
  resp[resp=="switch play" | resp=="false switch play" |
         resp=="switch play2" | resp=="false switch play2"] <- 3
  resp[resp=="pass it over" | resp=="false pass it over" |
         resp=="pass it over2" | resp=="false pass it over2"] <- 4
  
  resp[resp=="false link out "]     <- 2
  resp[resp=="switch play "]        <- 3
  resp[resp=="false switch play2 "] <- 3
  
  resp <- as.integer(resp)
  
  tibble(
    ID   = id,
    nA   = sum(stim==1),
    nB   = sum(stim==2),
    nC   = sum(stim==3),
    nD   = sum(stim==4),
    HA   = sum(stim==1 & resp==1),
    MA   = sum(stim==1 & resp>1),
    FA   = sum(stim>1 & resp==1),
    HB   = sum(stim==2 & resp==2),
    MB   = sum(stim==2 & resp>2),
    FB   = sum(stim>2 & resp==2),
    HC   = sum(stim==3 & resp==3),
    MC   = sum(stim==3 & resp==4),
    FC   = sum(stim==4 & resp==3),
    HD   = sum(stim==4 & resp==4)
  )
}

summary_u17 <- bind_rows(
  Map(read_and_prepare, files_u17, ids)
)

Nsubj <- nrow(summary_u17)

data <- list(
  Nsubj = Nsubj,
  nA = summary_u17$nA,
  nB = summary_u17$nB,
  nC = summary_u17$nC,
  nD = summary_u17$nD,
  HA = summary_u17$HA,
  HB = summary_u17$HB,
  HC = summary_u17$HC,
  HD = summary_u17$HD,
  FA = summary_u17$FA,
  FB = summary_u17$FB,
  FC = summary_u17$FC,
  MA = summary_u17$MA,
  MB = summary_u17$MB,
  MC = summary_u17$MC
)

params <- c(
  # group-level
  "kappa_A_mu","kappa_A_sigma",
  "kappa_B_mu","kappa_B_sigma",
  "kappa_C_mu","kappa_C_sigma",
  "kappa_D_mu","kappa_D_sigma",
  "gamma_A_mu","gamma_A_sigma",
  "gamma_B_mu","gamma_B_sigma",
  "gamma_C_mu","gamma_C_sigma",
  "gamma_D_mu","gamma_D_sigma",
  
  # subject-level
  "kappa_A","kappa_B","kappa_C","kappa_D",
  "gamma_A","gamma_B","gamma_C","gamma_D"
)


samples <- jags(data, inits=NULL, params,
                model.file ="Models/MPT_false_group.txt",
                n.chains=3, n.iter=10000, n.burnin=2000, n.thin=1)


### Plotting 
post <- as.data.frame(samples$BUGSoutput$sims.matrix)

plot_param_ridge <- function(param) {
  
  df <- post %>%
    select(matches(paste0("^", param, "\\[")), paste0(param, "_mu")) %>%
    pivot_longer(cols = everything(),
                 names_to = "parameter",
                 values_to = "value")
  
  ggplot(df, aes(x = value, y = parameter, fill = parameter)) +
    geom_density_ridges(alpha = 0.7, scale = 1.2) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "none") +
    labs(
      title = paste("Posterior for", param),
      x = "Estimated Value",
      y = ""
    )
}


p1 <- plot_param_ridge("gamma_A")
p2 <- plot_param_ridge("gamma_B")
p3 <- plot_param_ridge("gamma_C")
p4 <- plot_param_ridge("gamma_D")
g <- arrangeGrob(p1, p2, p3, p4, ncol = 2)

ggsave("results/ridge_gamma.png", g, width = 15, height = 10, dpi = 300)

samples
