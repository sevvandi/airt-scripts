# -----------------------------------------------------------------
# POLYTOMOUS RESPONSE MODELS
# TASK 1 - LOAD LIBRARIES AND SET FOLDER
# TASK 2 - CLASSIFICATION ALGORITHMS
# TASK 3 - OUTLIER DETECTION ALGORITHMS
# TASK 4 - GRAPH COLOURING ALGORITHMS
# TASK ** - COMMON TASK  - IRT MODEL AND ALGORITHM PERF OUTPUT
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
df <- 1 - df # As df has error rates

# Make polytomous data
df2 <- make_polyIRT_data(df, method=2)
newcolnames <- substring(colnames(df), 6, nchar(colnames(df)))
colnames(df2) <- newcolnames


# -----------------------------------------------------------------
# TASK 3 - OUTLIER DETECTION ALGORITHMS
# -----------------------------------------------------------------
# DATASET AT https://matilda.unimelb.edu.au/matilda/problems/learning/anomaly_detection#anomaly_detection
dat <- read.csv(paste(folder, "anomaly_detection_metadata.csv", sep=""))
df <- dat[ ,348:355]

# Make polytomous data
df2 <- make_polyIRT_data(df, method=2)
newcolnames <- substring(colnames(df), 6, nchar(colnames(df)))
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
df <- max(df) - df

# Make polytomous data
df2 <- make_polyIRT_data(df, method=2)
newcolnames <- substring(colnames(df), 6, nchar(colnames(df)))
colnames(df2) <- newcolnames

# -----------------------------------------------------------------
# TASK ** - COMMON TASK  - IRT MODEL AND ALGORITHM PERF OUTPUT
# -----------------------------------------------------------------
modout <- pirtmodel(df2)

easiness <- modout$easiness_threshold
print(round(easiness, 4))

gdf <- prepare_for_plots_poly(modout$model)
ggplot(gdf, aes(Theta, value)) + geom_line(aes(color=Level)) + facet_wrap(.~Algorithm, nrow=2) + ylab("Probability") + xlab(TeX("$\\theta$")) + theme_bw()

# Accuracy and Reliability
good <- model_goodness_poly(modout$model)
eff <- effectiveness_poly(modout$model)
stab <- modout$stability
anomalous <- modout$anomalous
mse <- good$mse
easiness <- modout$easiness_threshold
acc_rel <- cbind.data.frame(mse, good$goodnessAUC, eff$effectiveness, stab, anomalous, easiness)
colnames(acc_rel) <- c("MSE", "AUCDF", "Act_Effectiveness", "Pred_Effectiveness", "Stability", "Anomalous", "Easiness")
print(round(acc_rel, 4))


# Plot model goodness curves
good_curves <- as.data.frame(good$curves)
good_df <- good_curves %>% pivot_longer(cols=2:dim(good_curves)[2], names_to=c("Algorithm"))
p1 <- ggplot(good_df, aes(x,value)) + geom_point() + geom_line(aes(color = Algorithm), size=1)  + ylab(TeX("CDF  $F(\\rho_j)$")) + xlab("Scaled Absolute Error") + theme_bw()
p1


# Actual effectiveness
eff_curves <- as.data.frame(eff$actcurves)
eff_df1 <- eff_curves %>% pivot_longer(cols=2:dim(eff_curves)[2], names_to=c("Algorithm"))
p2 <- ggplot(eff_df1, aes(x,value)) + geom_point() + geom_line(aes(color = Algorithm), size=1)  + ylab("Actual Effectiveness") + xlab("Effectiveness Tolerance") + theme_bw()
p2


# Precited effectiveness
eff_curves <- as.data.frame(eff$prdcurves)
eff_df2 <- eff_curves %>% pivot_longer(cols=2:dim(eff_curves)[2], names_to=c("Algorithm"))
p3 <- ggplot(eff_df2, aes(x,value)) + geom_point() + geom_line(aes(color = Algorithm), size=1)  + ylab("Predicted Effectiveness") + xlab("Effectiveness Tolerance") + theme_bw()
p3


# Plot actual and predicted effectiveness AUC curves
df_eff <- cbind.data.frame(as.data.frame(eff$effectivenessAUC), rownames(eff$effectivenessAUC) )
colnames(df_eff)[3] <- "Algorithm"
p4 <- ggplot(df_eff, aes(Actual, Predicted)) + geom_jitter(aes(color=Algorithm), size=3) + geom_abline(aes(intercept=0,slope=1), linetype="dotted") + xlim(c(0,1)) + ylim(c(0,1)) + xlab("AUAEC") + ylab("AUPEC") +  theme_bw()
p4

gridExtra::grid.arrange(
  p2, p3, p1, p4,
  ncol = 2, nrow = 2,
  layout_matrix = rbind(c(1,2), c(3,4)))

