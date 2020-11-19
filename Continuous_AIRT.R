# -----------------------------------------------------------------
# CONTINUOUS RESPONSE MODELS - IRT
# TASK 1 - LOAD LIBRARIES AND SET FOLDER
# TASK 2 - CLASSIFICATION ALGORITHMS
# TASK 3 - OUTLIER DETECTION ALGORITHMS
# TASK 4 - GRAPH COLOURING ALGORITHMS
# TASK 5 - SIMULATION
# TASK ** - COMMON TASK  - IRT MODEL AND ALGORITHM PERF OUTPUT
# TASK ** - COMMON TASK  - LATENT TRAIT ANALYSIS
# -----------------------------------------------------------------

# -----------------------------------------------------------------
# TASK 1 - LOAD LIBRARIES AND SET FOLDER
# -----------------------------------------------------------------
library(tidyr)
library(ggplot2)
library(airt)
library(latex2exp)
library(gridExtra)
# folder <- paste(getwd(), "/Data_Input/", sep="" )
folder <- "C:/Users/Sevvandi/Documents/repos/datagen/Data_Input/"

# -----------------------------------------------------------------
# TASK 2 - CLASSIFICATION ALGORITHMS
# -----------------------------------------------------------------
# DATASET AT https://matilda.unimelb.edu.au/matilda/problems/learning/classification#classification
dat <- read.csv(paste(folder, "classification_metadata.csv", sep=""))
df <- dat[ ,12:21]
df2 <- 1 - df # As df has error rates

newcolnames <- substring(colnames(df2), 6, nchar(colnames(df2)))
colnames(df2) <- newcolnames


# -----------------------------------------------------------------
# TASK 3 - OUTLIER DETECTION ALGORITHMS
# -----------------------------------------------------------------
# DATASET AT https://matilda.unimelb.edu.au/matilda/problems/learning/anomaly_detection#anomaly_detection
dat <- read.csv(paste(folder, "anomaly_detection_metadata.csv", sep=""))
df2 <- dat[ ,348:355]

newcolnames <- substring(colnames(df2), 6, nchar(colnames(df2)))
colnames(df2) <- newcolnames


# -----------------------------------------------------------------
# TASK 4 - GRAPH COLOURING ALGORITHMS
# -----------------------------------------------------------------
# DATASET AT https://matilda.unimelb.edu.au/matilda/problems/opt/graphcoloring#graphcoloring
dat <- read.csv(paste(folder, "graph_coloring_metadata.csv", sep=""))
df <- dat[ ,6:13]
# This dataset has the number of colours the algorithim took to paint the graph
# So smaller the better - normalise per row
# Divide each row by the minimum number of colours
df <- t(apply(df, 1, function(x) x/min(x)))
# Smaller values are better - Invert this
df2 <- 1/df

newcolnames <- substring(colnames(df), 6, nchar(colnames(df)))
colnames(df2) <- newcolnames


# -----------------------------------------------------------------
# TASK 5 - SIMULATION
# -----------------------------------------------------------------
set.seed(1)
algo1 <- runif(200)
algo2 <- 2*algo1 + rnorm(200, mean=0, sd=0.1)
algo2 <- (algo2 - min(algo2))/(max(algo2) - min(algo2))
plot(algo1, algo2)
algo3 <- 1 - algo1 + rnorm(200, mean=0, sd=0.1)
algo3 <- (algo3 - min(algo3))/(max(algo3) - min(algo3))
plot(algo1, algo3)
algo4 <- 2 + 0.2*algo1 + rnorm(200, mean=0, sd=0.1)
algo4 <- algo4/max(algo4)
plot(sort(algo4))

df <- cbind.data.frame(algo1, algo2, algo3, algo4)
colnames(df) <- c("A", "B", "C", "D")
g1 <- ggplot(df, aes(A, B)) + geom_point()+ coord_fixed() + xlab("Algorithm A") + ylab("Algorithm B") + theme_bw()
g1
g2 <- ggplot(df, aes(A, C)) + geom_point() + coord_fixed() + xlab("Algorithm A") + ylab("Algorithm C")  + theme_bw()
g2
g3 <- ggplot(df, aes(A, D)) + geom_point()+  xlab("Algorithm A") + ylab("Algorithm D") + coord_fixed() + ylim(0,1) + theme_bw()
g3

grid.arrange(g1, g2, g3, layout_matrix=cbind(1,2,3))

df2 <- df


# -----------------------------------------------------------------
# TASK ** - COMMON TASK  - IRT MODEL AND ALGORITHM PERF OUTPUT
# -----------------------------------------------------------------
max_item  <- max(df2)
min_item <- 0
max.item <- rep(max_item, dim(df2)[2])
min.item <- rep(min_item, dim(df2)[2])
df2 <- as.data.frame(df2)
modout <- cirtmodel(df2, max.item, min.item)
paras <- modout$model$param

gdf <- prepare_for_plots_crm(modout$model) # , thetarange = c(-8, -2)
ggplot(gdf, aes(theta, z)) + geom_raster(aes(fill=pdf))  + xlab(TeX("$\\theta$")) + facet_wrap(~Algorithm, nrow=2) + coord_fixed(ratio=0.5) + theme_bw()  +  scale_fill_viridis_c(option = "plasma")  # + facet_wrap(~Algorithm, nrow=2)


# Accuracy and Reliability
good <- model_goodness_crm(modout$model)
eff <- effectiveness_crm(modout$model)
stab <- modout$stability
anomalous <- modout$anomalous
easinessthres <- modout$easiness_threshold
acc_rel <- cbind.data.frame(good$mse, good$goodness, eff$effectiveness, stab, anomalous, easinessthres)
colnames(acc_rel) <- c("MSE", "AUCDF", "Act_Effectiveness", "Pred_Effectiveness", "Stability", "Anomalous", "Easiness Threshold")
print(round(acc_rel, 4))


# Plot CDFs of rho_j
good_curves <- as.data.frame(good$curves)
good_df <- good_curves %>% pivot_longer(cols=2:dim(good_curves)[2], names_to=c("Algorithm"))
p1 <- ggplot(good_df, aes(x,value)) + geom_point() + geom_line(aes(color = Algorithm), size=1)  + ylab(TeX("CDF  $F(\\rho_j)$")) + xlab("Scaled Absolute Error") + theme_bw()
p1

# Plot Actual effectiveness
eff_curves <- as.data.frame(eff$actcurves)
eff_df1 <- eff_curves %>% pivot_longer(cols=2:dim(eff_curves)[2], names_to=c("Algorithm"))
p2 <- ggplot(eff_df1, aes(x,value)) + geom_point() + geom_line(aes(color = Algorithm), size=1)  + ylab("Actual Effectiveness") + xlab("Effectiveness Tolerance") + theme_bw()
p2

# Plot Predited effectiveness
eff_curves <- as.data.frame(eff$prdcurves)
eff_df2 <- eff_curves %>% pivot_longer(cols=2:dim(eff_curves)[2], names_to=c("Algorithm"))
p3 <- ggplot(eff_df2, aes(x,value)) + geom_point() + geom_line(aes(color = Algorithm), size=1)  + ylab("Predicted Effectiveness") + xlab("Effectiveness Tolerance") + theme_bw()
p3



# Plot AUAEC and AUPEC points
df_eff <- cbind.data.frame(as.data.frame(eff$effectivenessAUC), rownames(eff$effectivenessAUC) )
colnames(df_eff)[3] <- "Algorithm"
p4 <- ggplot(df_eff, aes(Actual, Predicted)) + geom_jitter(aes(color=Algorithm), size=2) + geom_abline(aes(intercept=0,slope=1), linetype="dotted") + xlim(c(0,1)) + ylim(c(0,1)) + xlab("AUAEC") + ylab("AUPEC") +  theme_bw()
p4

gridExtra::grid.arrange(
  p2, p3, p1, p4,
  ncol = 2, nrow = 2,
  layout_matrix = rbind(c(1,2), c(3,4)))


# -----------------------------------------------------------------
# TASK ** - COMMON TASK  - EXPLORE THE LATENT TRAIT
# -----------------------------------------------------------------

obj <- latent_trait_analysis(df2,modout$model$param,min.item,max.item )

dfl <- obj$longdf

g1 <- ggplot(dfl, aes(Latent_Trait, value)) +  geom_point(aes(color=Algorithm)) + xlab("Latent Trait (Dataset Easiness)") + ylab("Performance")  + theme_bw() + theme(legend.position = "none")
g1


g3 <- ggplot(dfl, aes(Latent_Trait, value)) +  geom_point(aes(color=Algorithm)) + xlab("Latent Trait (Dataset Easiness)") + facet_wrap(~Algorithm, nrow=2) + coord_fixed(ratio=6) + ylab("Performance") + theme_bw() + theme(legend.position = "none")
g3



### Curve fitting - smoothing splines - latent trait
g2 <- ggplot(dfl, aes(Latent_Trait, value)) +  geom_smooth(aes(color=Algorithm), se = FALSE, method = "gam", formula = y ~s(x, bs="cs"))+  xlab("Latent Trait (Dataset Easiness)") + ylab("Performance")  + theme_bw()  +theme(legend.position="bottom", legend.box = "horizontal")
g2

latent <- obj$latent
latent$proportions

setColors <- setNames( latent$proportions$colour, latent$proportions$algorithm)

df2 <- latent$latent
df3 <- cbind(df2, y=1)
df3 <- df3[ ,c(1,3,2)]
g4 <- ggplot(df3, aes(x,y)) + geom_point(aes(color=Algorithm),size=2, shape=15) + ylab("") + coord_fixed(ratio = 2) + theme(axis.title.y=element_blank(), axis.text.y=element_blank(),axis.ticks.y=element_blank()) + scale_color_manual(values = setColors) + xlab("Latent Trait") + theme(legend.position="bottom", legend.box="vertical", legend.margin=margin())  +guides(group=guide_legend(nrow=3))
g4


gridExtra::grid.arrange(
  g1, g2, g3, g4,
  ncol = 2, nrow = 3,
  layout_matrix = rbind(c(1,2), c(3,4)))

