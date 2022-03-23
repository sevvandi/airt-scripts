# ------------------------------------------------------------------
# THIS FILE DOES THE GRAPH COLORING EXAMPLE FULLY
# TASK 1  : LOAD LIBRARIES AND SET FOLDER
# TASK 2  : AIRT MODEL, CURVES, STRENGTHS AND WEAKNESSES
# TASK 3  : PERFORMANCE DETERIORATION ETC
# TASK 4  : MODEL GOODNESS MEASURES
# ------------------------------------------------------------------



# -----------------------------------------------------------------
# TASK 1  : LOAD LIBRARIES AND SET FOLDER
# -----------------------------------------------------------------
library(tidyr)
library(ggplot2)
library(airt)
library(latex2exp)
library(gridExtra)
library(dplyr)
folder <- "C:/Users/Sevvandi/Documents/repos/datagen/Data_Input/"


# ------------------------------------------------------------------
# TASK  2 : AIRT MODEL, CURVES, STRENGTHS AND WEAKNESSES
# ------------------------------------------------------------------
dat <- read.csv(paste(folder, "graph_coloring_metadata.csv", sep=""))

# This dataset has the number of colours the algorithim took to paint the graph
# So smaller the better - normalise per row
# Divide each row by the number of nodes
# df <- dat[ ,6:13]/dat$Nodes

df <- dat[ ,6:13]
# Divide each row by the mean number of colours
df <- t(apply(df, 1, function(x) x/min(x)))
df2 <- 1/df
newcolnames <- substring(colnames(df2), 6, nchar(colnames(df2)))
colnames(df2) <- newcolnames

max_item  <- max(df2)
min_item <- 0
max.item <- rep(max_item, dim(df2)[2])
min.item <- rep(min_item, dim(df2)[2])
df2 <- as.data.frame(df2)
all_algos <- dim(df2)[2]

write_folder <- "../Data_Output/Top_and_Bottom_Lt4/"
prefix <- "graph_colouring_"

# Fit the airt model
modout <- cirtmodel(df2, max.item, min.item)
paras <- modout$model$param

gdf <- prepare_for_plots_crm(modout$model) # , thetarange = c(-8, -2)
g1 <- ggplot(gdf, aes(theta, z)) + geom_raster(aes(fill=pdf))  + xlab(TeX("$\\theta$")) + facet_wrap(~Algorithm, nrow=2) + coord_fixed(ratio=0.5) + theme_bw()  +  scale_fill_viridis_c(option = "plasma")
g1

# LATENT TRAIT AND DATASETS
obj2 <- latent_trait_analysis(df2,modout$model$param,min.item,max.item, epsilon = 0.01)
dfl <- obj2$longdf
obj2$strengths$proportions

g2 <- ggplot(dfl, aes(Latent_Trait, value)) +  geom_point(aes(color=Algorithm)) + xlab("Problem Difficulty") + ylab("Performance")  + theme_bw() #+ theme(legend.position = "none")
g2

g3 <- ggplot(dfl, aes(Latent_Trait, value)) +  geom_point(aes(color=Algorithm)) + xlab("Problem Difficulty") + facet_wrap(~Algorithm, nrow=2) + ylab("Performance") + theme_bw() + theme(legend.position = "none") #  + coord_fixed(ratio=6)
g3
gridExtra::grid.arrange(g2, g3, nrow= 1)

# LATENT TRAIT -- Curve fitting - smoothing splines
g4 <- ggplot(dfl, aes(Latent_Trait, value)) +  geom_smooth(aes(color=Algorithm), se = FALSE, method = "gam", formula = y ~s(x, bs="cs"))+  xlab("Problem Difficulty") + ylab("Performance")  + theme_bw()
g4

# LATENT TRAIT -- Strengths and Weaknesses
obj2 <- latent_trait_analysis(df2,modout$model$param,min.item,max.item, epsilon = 0.01)
num_algos <- length(unique(dfl$Algorithm))
colrs <- scales::hue_pal()(num_algos)

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

dfl21 <- dfl2 %>% mutate(type = "Strengths", epsilon = "e = 0.01")
dfl31 <- dfl3 %>% mutate(type = "Weaknesses", epsilon = "e = 0.01")

# epsilon = 0
obj1 <- latent_trait_analysis(df2,modout$model$param,min.item,max.item, epsilon = 0)

latenttr <- obj1$strengths$multilatent
dfl2 <- tidyr::pivot_longer(latenttr, cols = 2:dim(latenttr)[2])
colnames(dfl2)[2] <- "Algorithm"
dfl2 <- dfl2[dfl2$value!=0, ]
dfl2$value <- dfl2$value*0.1

latenttr2 <- obj1$weakness$multilatent
dfl3 <- tidyr::pivot_longer(latenttr2, cols = 2:dim(latenttr)[2])
colnames(dfl3)[2] <- "Algorithm"
dfl3 <- dfl3[dfl3$value!=0, ]
dfl3$value <- dfl3$value*0.1

dfl22 <- dfl2 %>% mutate(type = "Strengths", epsilon = "e = 0")
dfl32 <- dfl3 %>% mutate(type = "Weaknesses", epsilon = "e = 0")

dflall <- bind_rows(dfl21, dfl31, dfl22, dfl32)
colrs2 <- colrs[which(sort(unique(dfl$Algorithm)) %in% unique(dflall$Algorithm))]

g5 <- ggplot(dflall, aes(x = latenttrait, y =value, fill = Algorithm)) + geom_tile() + facet_grid(epsilon ~ type) + theme(axis.title.y=element_blank(), axis.text.y=element_blank(),axis.ticks.y=element_blank())  + scale_fill_manual(values = colrs2) + xlab("Problem Difficulty") +  coord_fixed(ratio=1)
g5

gridExtra::grid.arrange(g4, g5)

obj1$strengths$proportions

stab <- modout$stability
anomalous <- modout$anomalous
difflim <- modout$difficulty_limit
model_charac <- cbind.data.frame(stab, difflim, anomalous)
model_charac <- model_charac %>% mutate(algorithm = rownames(model_charac))

airt_charac <- model_charac %>% full_join(obj1$strengths$proportions) %>% relocate(algorithm) %>% rename(LTO_eps0 = Proportion) %>% select(-colour, -group) %>%  full_join(obj2$strengths$proportions) %>% rename(LTO_eps01 = Proportion) %>% select(-colour, -group) %>% replace(is.na(.), 0)
airt_charac  # Table 2 in the paper

# ------------------------------------------------------------------
# TASK 3  : PERFORMANCE DETERIORATION ETC
# ------------------------------------------------------------------
colrs <- scales::hue_pal()(4)

write_folder_1 <- "../Data_Output/LT4_Eps_0/"
prefix <- "matilda_graph_colouring_"
df_cv <- read.csv(paste(write_folder_1, prefix, "_CV_10_fold.csv", sep=""))
df_cv <- df_cv[10:14, ]
colnames(df_cv)[2] <- "strong-airt"
df_cv_long <- pivot_longer(df_cv, cols = 2:4)
colnames(df_cv_long)[2:3] <- c("Meta_Algo", "Perf_Diff")
l1 <- ggplot(df_cv_long, aes(x=quantiles, y=Perf_Diff, color=Meta_Algo)) + geom_point() + geom_line() + ylab("Performance Deterioration") + scale_color_manual(values = colrs[1:3]) + theme_bw()
l1


worst_algo_props_mean <- read.csv(paste(write_folder_1, prefix, "_worst_algo_props.csv", sep=""))
num <- min(10, ceiling(all_algos/2), (dim(worst_algo_props_mean)[2]-1))
worst_algo_props_mean <- worst_algo_props_mean[ ,1:(num+1)]
worst_algo_props_mean[1,1] <- "weak-airt"
worst_algo_long <- pivot_longer(worst_algo_props_mean, cols =2:dim(worst_algo_props_mean)[2])
colnames(worst_algo_long)[2:3] <- c("Num_Algos", "Best_Algo_Proportion")
worst_algo_long$Num_Algos <- as.numeric(paste(substr(worst_algo_long$Num_Algos, 2, nchar(worst_algo_long$Num_Algos))))
worst_algo_long1 <- worst_algo_long %>% mutate(type = "epsilon = 0")


write_folder_1 <- "../Data_Output/LT4_Eps_0.01/"
worst_algo_props_mean <- read.csv(paste(write_folder_1, prefix, "_worst_algo_props.csv", sep=""))
num <- min(10, ceiling(all_algos/2), (dim(worst_algo_props_mean)[2]-1))
worst_algo_props_mean <- worst_algo_props_mean[ ,1:(num+1)]
worst_algo_props_mean[1,1] <- "weak-airt"
worst_algo_long <- pivot_longer(worst_algo_props_mean, cols =2:dim(worst_algo_props_mean)[2])
colnames(worst_algo_long)[2:3] <- c("Num_Algos", "Best_Algo_Proportion")
worst_algo_long$Num_Algos <- as.numeric(paste(substr(worst_algo_long$Num_Algos, 2, nchar(worst_algo_long$Num_Algos))))
worst_algo_long2 <- worst_algo_long %>% mutate(type = "epsilon = 0.01")
worst_algo_long_all <- bind_rows(worst_algo_long1, worst_algo_long2)

l2 <- ggplot(worst_algo_long_all, aes(x=Num_Algos, y=Best_Algo_Proportion)) + geom_point(aes(color=Meta_Algo)) + geom_line(aes(color=Meta_Algo)) + ylab("Best Algo Proportion") + facet_wrap(~type) + theme_bw() + scale_color_manual(values = colrs[c(4,2)])
l2

gridExtra::grid.arrange(l1, l2, nrow= 1)

# ------------------------------------------------------------------
# TASK 4  : MODEL GOODNESS MEASURES
# ------------------------------------------------------------------
# Accuracy and Reliability
good <- model_goodness_crm(modout$model)
eff <- effectiveness_crm(modout$model)
stab <- modout$stability
anomalous <- modout$anomalous
difflim <- modout$difficulty_limit
acc_rel <- cbind.data.frame(good$mse, good$goodness, eff$effectiveness,  apply(eff$effectiveness, 1, function(x) abs(diff(x))))
colnames(acc_rel) <- c("MSE", "AUCDF", "AUAEC", "AUPEC", "|AUAEC - AUPEC|")
print(round(acc_rel, 4))  # TABLE 3


# Plot CDFs of rho_j
good_curves <- as.data.frame(good$curves)
good_df <- good_curves %>% pivot_longer(cols=2:dim(good_curves)[2], names_to=c("Algorithm"))
p1 <- ggplot(good_df, aes(x,value))  + geom_line(aes(color = Algorithm), size=1)  + ylab(TeX("CDF  $F(\\rho_j)$")) + xlab("Scaled Absolute Error") + theme_bw() + theme(legend.position = "none")
p1
# write.csv(good_curves, "Data_Output/Graph_Colouring/good_curves.csv", row.names = FALSE)

# Plot Actual effectiveness
eff_curves <- as.data.frame(eff$actcurves)
eff_df1 <- eff_curves %>% pivot_longer(cols=2:dim(eff_curves)[2], names_to=c("Algorithm"))
p2 <- ggplot(eff_df1, aes(x,value)) + geom_line(aes(color = Algorithm), size=1)  + ylab("Actual Effectiveness") + xlab("Effectiveness Tolerance") + theme_bw() + theme(legend.position = "none")
p2
# write.csv(eff_curves, "Data_Output/Graph_Colouring/actual_eff_curves.csv", row.names = FALSE)

# Plot Predited effectiveness
eff_curves <- as.data.frame(eff$prdcurves)
eff_df2 <- eff_curves %>% pivot_longer(cols=2:dim(eff_curves)[2], names_to=c("Algorithm"))
p3 <- ggplot(eff_df2, aes(x,value)) + geom_line(aes(color = Algorithm), size=1)  + ylab("Predicted Effectiveness") + xlab("Effectiveness Tolerance") + theme_bw()
p3
# write.csv(eff_curves, "Data_Output/Graph_Colouring/pred_eff_curves.csv", row.names = FALSE)


# Plot AUAEC and AUPEC points
df_eff <- cbind.data.frame(as.data.frame(eff$effectivenessAUC), rownames(eff$effectivenessAUC) )
colnames(df_eff)[3] <- "Algorithm"
p4 <- ggplot(df_eff, aes(Actual, Predicted)) + geom_jitter(aes(color=Algorithm), size=2) + geom_abline(aes(intercept=0,slope=1), linetype="dotted") + xlim(c(0,1)) + ylim(c(0,1)) + xlab("AUAEC") + ylab("AUPEC") +  theme_bw()
p4
# write.csv(df_eff, "Data_Output/Graph_Colouring/eff_AUC.csv", row.names = FALSE)

gridExtra::grid.arrange(
  p2, p3, p1, p4,
  ncol = 2, nrow = 2,
  layout_matrix = rbind(c(1,2), c(3,4)), widths = c(1, 1.5))




