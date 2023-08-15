# -----------------------------------------------------------------
# TASK 1: READ DATA
# TASK 2: IRT MODEL
# TASK 3: DIFFERENT FIGURES
# TASK 4: EXPLORE THE LATENT TRAIT
# TASK 5: DIAGNOSTICS - GOODNESS OF FIT CURVES
# -----------------------------------------------------------------
# THIS FILE IS SIMILAR TO simulation.R IN Supp_Mat/jmlr_Revisions

library(dplyr)
library(tidyr)
library(ggplot2)
library(airt)
library(latex2exp)
library(gridExtra)
library(xgboost)
library(SHAPforxgboost)
library(RWeka)
library(RColorBrewer)


# -----------------------------------------------------------------
# TASK 1: READ DATA
# -----------------------------------------------------------------
# http://coseal.github.io/aslib-r/scenario-pages
dat <- read.arff("Data_Input/ASLIB/CSP_Minizinc_Obj_2016/algorithm_runs.arff")
unique(dat$repetition)
dat1 <- dat %>% mutate( runtime2 = ifelse(runstatus  =='timeout', 6000, time ))
dat2 <- dat1[ ,c("instance_id", "algorithm","runtime2")]
df <- pivot_wider(dat2, names_from = algorithm, values_from=runtime2)
timout <- apply(df[ ,-1], 1, function(x)sum(x==6000))
rminds <- which(timout == max(timout))
length(rminds)
dft <- df2 <- df[-rminds, ]
dim(df2)
df2[ ,-1] <- 1/(df2[ ,-1]+1)
head(df2)

df2 <- df2[ ,-1]
dft <- dft[ ,-1]
max_item  <- max(df2)
min_item <- 0
max.item <- rep(max_item, dim(df2)[2])
min.item <- rep(min_item, dim(df2)[2])
df2 <- as.data.frame(df2)


all_algos <- dim(df2)[2]
write_folder <- "../../Data_Output/JMLR_Revisions_2/"
prefix <- "csp_minizinc_"


# -----------------------------------------------------------------
# TASK 2: IRT MODEL
# -----------------------------------------------------------------
max_item  <- max(df2)
min_item <- min(df2)
max.item <- rep(max_item, dim(df2)[2])
min.item <- rep(min_item, dim(df2)[2])
df2 <- as.data.frame(df2)
modout <- cirtmodel(df2, max.item, min.item)
paras <- modout$model$param
paras[c(1,9,15), ]
obj <- heatmaps_crm(modout)
autoplot(obj,nrow = 4)

gdf <- obj$df

gdf2 <- gdf %>%
  filter(Algorithm %in% c('iZplus-free', 'MZN/Gurobi-free', 'HaifaCSP-free') ) %>%  # ,
  filter(z %in% c(-2, 0, 1)) %>%
  rename(zold = z) %>%
  mutate(z = as.character(zold))

ggplot(gdf2, aes(x = theta, y = pdf, color = z)) +
  geom_line(aes(color = z)) +
  facet_wrap(~Algorithm) +
  xlab(TeX("$\\theta$")) +
  ylab("probability density") +
  theme_bw()

gdf3 <- gdf %>%
  filter(Algorithm %in% c('iZplus-free', 'MZN/Gurobi-free', 'HaifaCSP-free') )

ggplot(gdf3, aes(theta, z)) +
  geom_raster(aes(fill=pdf))  +
  xlab(TeX("$\\theta$")) +
  facet_wrap(~Algorithm) +
  coord_fixed(ratio=1) +
  theme_bw()  +
  scale_fill_viridis_c(option = "plasma")

# -----------------------------------------------------------------
# TASK 3: DIFFERENT FIGURES
# -----------------------------------------------------------------
# Anomalous figure
gdf41 <- gdf %>%
  filter(Algorithm %in% c('LCG-Glucose-free') )
gdf42 <- gdf %>%
  filter(Algorithm == "MZN/CPLEX-free") %>%
  rename(z1 = z, Algo = Algorithm) %>%
  mutate(z = -z1, Algorithm = "Modified (Anomalous)") %>%
  select(-c(z1, Algo))
gdf43 <- bind_rows(gdf41, gdf42)

ganom1 <- ggplot(gdf43, aes(theta, z)) +
  geom_raster(aes(fill=pdf))  +
  xlab(TeX("$\\theta$")) +
  facet_wrap(~Algorithm) +
  theme_bw()  +
  scale_fill_viridis_c(option = "plasma")

gdf44 <- gdf43 %>%
  filter(z %in% c(-2, 0, 1)) %>%
  rename(z1 = z) %>%
  mutate(z = as.character(z1))

ganom2 <- ggplot(gdf44, aes(x = theta, y = pdf, color = z)) +
  geom_line(aes(color = z)) +
  facet_wrap(~Algorithm) +
  xlab(TeX("$\\theta$")) +
  ylab("probability density") +
  theme_bw()

grid.arrange(ganom1, ganom2)

# Stability figure
gdf51 <- gdf %>%
  filter(Algorithm %in% c('Choco-free') )

stab1 <- ggplot(gdf51, aes(theta, z)) +
  geom_raster(aes(fill=pdf))  +
  xlab(TeX("$\\theta$")) +
  facet_wrap(~Algorithm) +
  theme_bw()  +
  scale_fill_viridis_c(option = "plasma", end = 0.1)

gdf52 <- gdf51 %>%
  filter(z %in% c(-2, 0, 1)) %>%
  rename(z1 = z) %>%
  mutate(z = as.character(z1))
stab2 <- ggplot(gdf52, aes(x = theta, y = pdf, color = z)) +
  geom_line(aes(color = z)) +
  facet_wrap(~Algorithm) +
  xlab(TeX("$\\theta$")) +
  ylab("probability density") +
  theme_bw() +
  ylim(0, 0.5)

grid.arrange(stab1, stab2, nrow = 1)

# Easiness threshold figure
gdf61 <- gdf %>%
  filter(Algorithm %in% c('MZN/SCIP-free', 'Mistral-free') )

geasy1 <- ggplot(gdf61, aes(theta, z)) +
  geom_raster(aes(fill=pdf))  +
  xlab(TeX("$\\theta$")) +
  facet_wrap(~Algorithm) +
  theme_bw()  +
  scale_fill_viridis_c(option = "plasma")

gdf62 <- gdf61 %>%
  filter(z %in% c(-2, 0, 1)) %>%
  rename(z1 = z) %>%
  mutate(z = as.character(z1))


geasy2 <- ggplot(gdf62, aes(x = theta, y = pdf, color = z)) +
  geom_line(aes(color = z)) +
  facet_wrap(~Algorithm) +
  xlab(TeX("$\\theta$")) +
  ylab("probability density") +
  theme_bw()

grid.arrange(geasy1, geasy2)

# -----------------------------------------------------------------
# TASK 4: EXPLORE THE LATENT TRAIT
# -----------------------------------------------------------------
df2 <- as.data.frame(df2)
obj2 <- airt::latent_trait_analysis(df2,modout$model$param,min.item,max.item, epsilon = 0.0)
autoplot(obj2, plottype = 1)
autoplot(obj2, plottype = 2)
autoplot(obj2, plottype = 3)
autoplot(obj2, plottype = 4)

dfl <- obj2$longdf
lto_eps0 <- obj2$strengths$proportions
lto_eps0 <- lto_eps0 %>%
  rename(Proportion0 = Proportion)

# Performance and latent scores figure
gdf4 <- gdf %>%
  filter(Algorithm %in% c('LCG-Glucose-free') )

gdf5 <- gdf4 %>%
  filter(z %in% c(-1, 0, 1)) %>%
  rename(zold = z) %>%
  mutate(z = as.character(zold))

ga <- ggplot(gdf4, aes(theta, z)) +
  geom_raster(aes(fill=pdf))  +
  xlab(TeX("$\\theta$")) +
  theme_bw()  +
  scale_fill_viridis_c(option = "plasma")

gb <- ggplot(gdf5, aes(x = theta, y = pdf, color = z)) +
  geom_line(aes(color = z)) +
  xlab(TeX("$\\theta$")) +
  ylab("probability density") +
  theme_bw()

latent_scores <- obj2$crmtheta$thetas
colnames(latent_scores)[2] <- 'Theta_Est'
latent_scores <- as.data.frame(latent_scores)

gc <- ggplot(latent_scores, aes(Theta_Est)) +
   geom_histogram() +
  xlab("Latent Scores") +
  theme_bw()

grid.arrange(ga, gb, gc, nrow = 1)

grid.arrange(ga, gb, gc, layout_matrix = rbind(c(1, 2),c(3, 3)))


# Dataset difficulty plot
num_algos1 <- length(unique(dfl$Algorithm))
colrs <- colorRampPalette(brewer.pal(8, "Dark2"))(num_algos1)

# Tweaking autoplot plottype 1 with different colours and no legend
g1 <- ggplot(dfl, aes(Latent_Trait, value)) +
  geom_point(aes(color=Algorithm)) +
  xlab("Dataset Difficulty") +
  ylab("Performance")  +
  theme_bw() +
  theme(legend.position = "none") +
  scale_color_manual(values = colrs)
g1


### Curve fitting - smoothing splines - latent trait
# Tweaking autooplot plottype 3 with different colours and no SE
g3 <- ggplot(dfl, aes(Latent_Trait, value)) +
  geom_smooth(aes(color=Algorithm), se = FALSE, method = "gam", formula = y ~s(x, bs="cs"))+
  xlab("Dataset difficulty") +
  ylab("Performance")  +
  theme_bw() +
  guides(color=guide_legend(ncol=1))+
  scale_color_manual(values = colrs)

g3


gridExtra::grid.arrange(g1, g3, nrow = 1, widths = c(1,1.7))

# LATENT TRAIT -- Strengths and Weaknesses
# A different epsilon
obj2 <- airt::latent_trait_analysis(df2,modout$model$param,min.item,max.item, epsilon = 0.01)
autoplot(obj2, plottype = 4)

num_algos <- length(unique(dfl$Algorithm))
# colrs <- scales::hue_pal()(num_algos)
colrs <- colorRampPalette(brewer.pal(8, "Dark2"))(num_algos)
lto_eps1 <- obj2$strengths$proportions
lto_eps1 <- lto_eps1 %>%
  rename(Proportion1 = Proportion)

lto_eps <- full_join(lto_eps0, lto_eps1) %>%
  select(algorithm, Proportion0, Proportion1)
lto_eps

latenttr <- obj2$strengths$multilatent
dfl2 <- tidyr::pivot_longer(latenttr, cols = 2:dim(latenttr)[2])
colnames(dfl2)[2] <- "Algorithm"
dfl2 <- dfl2[dfl2$value!=0, ]
dfl2$value <- dfl2$value*0.1

latenttr2 <- obj2$weakness$multilatent
dfl3 <- tidyr::pivot_longer(latenttr2, cols = 2:dim(latenttr)[2])
colnames(dfl3)[2] <- "Algorithm"
dfl3 <- dfl3[dfl3$value!=0, ]
dfl3$value <- dfl3$value*0.1

dfl21 <- dfl2 %>% mutate(type = "Strengths", epsilon = "e=0.01")
dfl31 <- dfl3 %>% mutate(type = "Weaknesses", epsilon = "e=0.01")

# epsilon = 0
obj1 <- airt::latent_trait_analysis(df2,modout$model$param,min.item,max.item, epsilon = 0)
autoplot(obj1, plottype = 4)

# This part puts both plots from autoplot plottype = 4 together in
# one horizontal bar plot for epsilon = 0
latenttr <- obj1$strengths$multilatent
dfl2 <- tidyr::pivot_longer(latenttr, cols = 2:dim(latenttr)[2])
colnames(dfl2)[2] <- "Algorithm"
dfl2 <- dfl2[dfl2$value!=0, ]
dfl2$value <- dfl2$value*0.1  # This line makes the y axis in strengths diagram
dfl2$value <- 1


latenttr2 <- obj1$weakness$multilatent
dfl3 <- tidyr::pivot_longer(latenttr2, cols = 2:dim(latenttr)[2])
colnames(dfl3)[2] <- "Algorithm"
dfl3 <- dfl3[dfl3$value!=0, ]
dfl3$value <- dfl3$value*0.1  # This line makes the y axis in strengths diagram
dfl3$value <- 1

dfl22 <- dfl2 %>% mutate(type = "Strengths", epsilon = "e=0")
dfl32 <- dfl3 %>% mutate(type = "Weaknesses", epsilon = "e=0")

dfl33 <- bind_rows(dfl22, dfl32)
colrs2 <- colrs[which(sort(unique(dfl$Algorithm)) %in% unique(dfl33$Algorithm))]

# The horizontal bar version for epsilon = 0
ggplot(dfl33, aes(x = latenttrait, y =value, fill = Algorithm)) +
  geom_tile() +
  facet_grid(epsilon ~ type) +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(),axis.ticks.y=element_blank())  +
  scale_fill_manual(values = colrs2) + xlab("Dataset Difficulty") +
  coord_fixed(ratio=0.5)


dflall <- bind_rows(dfl21, dfl31, dfl22, dfl32)
colrs2 <- colrs[which(sort(unique(dfl$Algorithm)) %in% unique(dflall$Algorithm))]

unique(dflall$Algorithm)

g5 <- ggplot(dflall, aes(x = latenttrait, y =value, fill = Algorithm)) +
  geom_tile() +
  facet_grid(epsilon ~ type,  scales = "free", space = "free") +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(),axis.ticks.y=element_blank())  +
  scale_fill_manual(values = colrs2) + xlab("Dataset Difficulty") #+
  # coord_fixed(ratio=2)

g5

# -----------------------------------------------------------------
# TASK 5: DIAGNOSTICS - GOODNESS OF FIT CURVES
# -----------------------------------------------------------------

# Accuracy and Reliability
good <- airt::model_goodness_crm(modout)
autoplot(good)

algos <- rownames(modout$model$param)
nn <- length(good$residuals[ ,1])
resid <- data.frame(id = 1:nn, residuals = abs(good$residuals[ ,1]))

p1 <- ggplot(resid, aes(residuals)) +
  geom_histogram() +
  theme_bw() +
  xlab("Absolute residuals") +
  ggtitle(algos[1])
p1

p2 <- ggplot(resid, aes(x=residuals)) +
  stat_ecdf(geom = "step") +
  xlab("Absolute residuals") +
  ylab("Probability") +
  theme_bw()

p2

resid$rho <- resid$residuals/max(resid$residuals)
p3 <- ggplot(resid, aes(x=rho)) +
  stat_ecdf(geom = "step") +
  xlab(TeX('Scaled absolute residuals $\\rho$')) +
  ylab("Probability")+
  theme_bw()
p3

grid.arrange(p1, p2, p3, layout_matrix = rbind(c(1, 2),c(1, 3)))


# Effectiveness
deff <- modout$model$data[ ,3] %>%
  as.data.frame()
colnames(deff) <- "Performance"
p1 <- ggplot(deff, aes(Performance)) +
  geom_histogram() +
  xlab("Performance") +
  theme_bw() +
  ggtitle(algos[3])
p1
p2  <- ggplot(deff, aes(Performance)) +
  stat_ecdf() +
  theme_bw() +
  xlab("Performance") +
  ylab("Probability")
p2

res2 <- max(deff$Performance) - deff$Performance
df2 <- as.data.frame(res2)
p4  <- ggplot(df2, aes(res2)) +
  stat_ecdf() +
  theme_bw() +
  xlab("Tolerance") +
  ylab("Probability")
p4

grid.arrange(p1, p2, p4, layout_matrix = rbind(c(1, 2),c(1, 3)))


# Effectiveness curves
eff <- effectiveness_crm(modout)
# Quicker way to get the plots
autoplot(good) # Equivalent to p1 below
autoplot(eff, plottype = 1) # Equivalent to p2
autoplot(eff, plottype = 2) # p3
autoplot(eff, plottype = 3) #p4


# To produce the plots exactly in the paper
num_algos <- NROW(eff$effectivenessAUC)
colrs2 <- colorRampPalette(brewer.pal(8, "Dark2"))(num_algos)

# Plot CDFs of rho_j
good_curves <- as.data.frame(good$curves)
good_df <- good_curves %>%
  pivot_longer(cols=2:dim(good_curves)[2], names_to=c("Algorithm"))
p1 <- ggplot(good_df, aes(x,value))  +
  geom_line(aes(color = Algorithm), size=1)  +
  ylab(TeX("CDF  $F(\\rho_j)$")) +
  xlab("Scaled Absolute Error") +
  theme_bw() +
  theme(legend.position = "none") +
  scale_color_manual(values = colrs2)
p1


# Plot Actual effectiveness
num_algos1 <- length(unique(dfl$Algorithm))
colrs <- colorRampPalette(brewer.pal(8, "Dark2"))(num_algos1)
eff_curves <- as.data.frame(eff$actcurves)
eff_df1 <- eff_curves %>%
  pivot_longer(cols=2:dim(eff_curves)[2], names_to=c("Algorithm"))
p2 <- ggplot(eff_df1, aes(x,value)) +
  geom_line(aes(color = Algorithm), size=1)  +
  ylab("Actual Effectiveness") + xlab("Effectiveness Tolerance") +
  theme_bw() +
  theme(legend.position = "none") +
  scale_color_manual(values = colrs2)
p2


# Plot Predited effectiveness
eff_curves <- as.data.frame(eff$prdcurves)
eff_df2 <- eff_curves %>%
  pivot_longer(cols=2:dim(eff_curves)[2], names_to=c("Algorithm"))
p3 <- ggplot(eff_df2, aes(x,value)) +
  geom_line(aes(color = Algorithm), size=1)  +
  ylab("Predicted Effectiveness") +
  xlab("Effectiveness Tolerance") +
  theme_bw() +
  scale_color_manual(values = colrs2)
p3



# Plot AUAEC and AUPEC points
df_eff <- cbind.data.frame(as.data.frame(eff$effectivenessAUC), rownames(eff$effectivenessAUC) )
colnames(df_eff)[3] <- "Algorithm"
p4 <- ggplot(df_eff, aes(Actual, Predicted)) +
  geom_jitter(aes(color=Algorithm), size=2) +
  geom_abline(aes(intercept=0,slope=1), linetype="dotted") +
  xlab("AUAEC") +
  ylab("AUPEC") +
  xlim(0, 0.5) +
  theme_bw() +
  scale_color_manual(values = colrs2)
p4


gridExtra::grid.arrange(
  p2, p3, p1, p4,
  ncol = 2, nrow = 2,
  layout_matrix = rbind(c(1,2), c(3,4)), widths = c(1, 2.5))

