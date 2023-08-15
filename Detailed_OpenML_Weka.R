# -----------------------------------------------------------------
# TASK 1: READ DATA
# TASK 2: IRT MODEL
# TASK 3: DIFFERENT FIGURES
# TASK 4: EXPLORE THE LATENT TRAIT
# TASK 5: DIAGNOSTICS - GOODNESS OF FIT CURVES
# -----------------------------------------------------------------

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
library(stringr)


# -----------------------------------------------------------------
# TASK 1: READ DATA
# -----------------------------------------------------------------
# http://coseal.github.io/aslib-r/scenario-pages
dat <- read.arff("Data_Input/ASLIB/OPENML_WEKA_2017/algorithm_runs.arff")
unique(dat$repetition)
dat2 <- dat[ ,c("instance_id", "algorithm","predictive_accuracy")]

dat3 <- dat2 %>%
  mutate(algorithm2 = str_replace(algorithm, pattern = "_weka.", replacement = "_")) %>%
  select(-algorithm) %>%
  rename(algorithm = algorithm2) %>%
  mutate(algorithm2 = ifelse(!str_detect(algorithm, c("MultilayerPerceptron|IBk")), str_sub(algorithm, start = 6, end = -1), algorithm))  %>%
  mutate(algorithm3 = str_replace(algorithm2, pattern = "MultilayerPerceptron", replacement = "MLP"))  %>%
  mutate(algorithm4 = str_replace(algorithm3, pattern = "AdaBoostM1", replacement = "AdaB"))  %>%
  mutate(algorithm5 = str_replace(algorithm4, pattern = "DecisionStump", replacement = "DSt"))  %>%
  select(-algorithm, -algorithm2, -algorithm3, -algorithm4) %>%
  rename(algorithm = algorithm5)


df <- pivot_wider(dat3, names_from = algorithm, values_from=predictive_accuracy)
df2 <- df
dim(df2)
df2 <- df[ ,-1]
max_item  <- max(df2)
min_item <- 0
max.item <- rep(max_item, dim(df2)[2])
min.item <- rep(min_item, dim(df2)[2])
df2 <- as.data.frame(df2)
prefix <- "openml_weka_"



# -----------------------------------------------------------------
# TASK 2: IRT MODEL
# -----------------------------------------------------------------
library(dplyr)
max_item  <- max(df2)
min_item <- min(df2)
max.item <- rep(max_item, dim(df2)[2])
min.item <- rep(min_item, dim(df2)[2])
df2 <- as.data.frame(df2)
modout <- cirtmodel(df2, max.item, min.item)
paras <- modout$model$param

obj <- heatmaps_crm(modout)
autoplot(obj,nrow = 5)

# gdf <- prepare_for_plots_crm(modout) # , thetarange = c(-8, -2)
# ggplot(gdf, aes(theta, z)) +
#   geom_raster(aes(fill=pdf))  +
#   xlab(TeX("$\\theta$")) +
#   facet_wrap(~Algorithm) +
#   coord_fixed(ratio=1) +
#   theme_bw()  +
#   scale_fill_viridis_c(option = "plasma") +
#   facet_wrap(~Algorithm)


consistency <- modout$consistency
diff_limit <- modout$difficulty_limit
anom <- modout$anomalous
summary1 <- cbind.data.frame(consistency, diff_limit, anom)
summary <- cbind.data.frame(rownames(summary1), summary1)
rownames(summary) <- NULL
colnames(summary)[1] <- "algorithm"
summary


# -----------------------------------------------------------------
# TASK 4: EXPLORE THE LATENT TRAIT
# -----------------------------------------------------------------
df2 <- as.data.frame(df2)
obj2 <- airt::latent_trait_analysis(df2,modout$model$param,min.item,max.item, epsilon = 0.0)
autoplot(obj2, plottype = 1)
autoplot(obj2, plottype = 2)
autoplot(obj2, plottype = 4)

dfl <- obj2$longdf
lto_eps0 <- obj2$strengths$proportions
lto_eps0 <- lto_eps0 %>%
  select(algorithm, Proportion) %>%
  rename(Proportion0 = Proportion)
summary_lto <- full_join(summary, lto_eps0) %>%
  replace_na(list(Proportion0 = 0))

obj3 <- airt::latent_trait_analysis(df2,modout$model$param,min.item,max.item, epsilon = 0.01)
autoplot(obj3, plottype = 4)

lto_eps30 <- obj3$strengths$proportions
lto_eps30 <- lto_eps30 %>%
  select(algorithm, Proportion) %>%
  rename(Proportion01 = Proportion)
summary_lto2 <- full_join(summary_lto, lto_eps30) %>%
  replace_na(list(Proportion01 = 0))
summary_lto2
cols <- colnames(summary_lto2)[-1]
summary_lto2 %>% mutate_at(2:6, round, 3)


# Dataset difficulty plot
num_algos1 <- length(unique(dfl$Algorithm))
colrs <- colorRampPalette(brewer.pal(8, "Dark2"))(num_algos1)
g1 <- ggplot(dfl, aes(Latent_Trait, value)) +
  geom_point(aes(color=Algorithm)) +
  xlab("Dataset Difficulty") +
  ylab("Performance")  +
  theme_bw() +
  theme(legend.position = "none") +
  scale_color_manual(values = colrs)
g1


g3alt <- ggplot(dfl, aes(Latent_Trait, value)) +
  geom_smooth(aes(color=Algorithm), se = FALSE, method = "gam", formula = y ~s(x, bs="cs"))+
  xlab("Dataset difficulty") +
  ylab("Performance")  +
  theme_bw() +
  scale_color_manual(values = colrs) +
  theme(legend.position="bottom")

g3alt

gridExtra::grid.arrange(g1, g3alt, nrow = 2, heights = c(1, 1.5))


# LATENT TRAIT -- Strengths and Weaknesses
obj2 <- airt::latent_trait_analysis(df2,modout$model$param,min.item,max.item, epsilon = 0.01)
num_algos <- length(unique(dfl$Algorithm))
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
good <- model_goodness_crm(modout)
# Easy autoplot commands
autoplot(good)

# If you want to do it manually
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
# Easy autoplot commands
autoplot(eff, plottype = 1)
autoplot(eff, plottype = 2)
autoplot(eff, plottype = 3)

# If you want to produce the same plots manually
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
  theme_bw() +
  xlim(0, 1) +
  ylim(0, 1) +
  scale_color_manual(values = colrs2)
p4


gridExtra::grid.arrange(
  p2, p3, p1, p4,
  ncol = 2, nrow = 2,
  layout_matrix = rbind(c(1,2), c(3,4)), widths = c(1, 2.5))


# TABLE
df_diagnostics1 <- data.frame(algorithm = rownames(good$goodnessAUC),
                              mse = good$mse,
                              AUCDF = good$goodnessAUC)
rownames(df_diagnostics1) <- NULL

df_diagnostics2 <- data.frame(algorithm = rownames(eff$effectivenessAUC),
                              AUAEC = eff$effectivenessAUC[ ,1],
                              AUPEC = eff$effectivenessAUC[ ,2])
rownames(df_diagnostics2) <- NULL

diag <- full_join(df_diagnostics1, df_diagnostics2) %>%
  mutate(across(2:5, round, 3))
abs(diag$AUAEC - diag$AUPEC)

