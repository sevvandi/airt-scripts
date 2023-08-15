# ------------------------------------------------------------------------------
# TASK  1: LOAD LIBRARIES
# TASK  2: READ AND ORGANISE DATA
# TASK  3: CROSS VALIDATION
# TASK  4: PLOTTING
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# TASK  1: LOAD LIBRARIES
# ------------------------------------------------------------------------------
library(tidyr)
library(ggplot2)
library(airt)
library(latex2exp)
library(gridExtra)
library(dplyr)
library(xgboost)
library(SHAPforxgboost)
library(RWeka)

# ------------------------------------------------------------------------------
# TASK  2: READ AND ORGANISE DATA
# ------------------------------------------------------------------------------
# http://coseal.github.io/aslib-r/scenario-pages
dat <- read.arff("Data_Input/ASLIB/OPENML_WEKA_2017/algorithm_runs.arff")
unique(dat$repetition)
dat2 <- dat[ ,c("instance_id", "algorithm","predictive_accuracy")]
df <- pivot_wider(dat2, names_from = algorithm, values_from=predictive_accuracy)
df2 <- df
dim(df2)
df2 <- df[ ,-1]
max_item  <- max(df2)
min_item <- 0
max.item <- rep(max_item, dim(df2)[2])
min.item <- rep(min_item, dim(df2)[2])
df2 <- as.data.frame(df2)
prefix <- "openml_weka_"

# ------------------------------------------------------------------------------
# TASK  3: CROSS VALIDATION
# ------------------------------------------------------------------------------

mean_par10_gap <- data.frame(fold = numeric(),
                             num_algos = numeric(),
                             airt_mu = numeric(),
                             shapley_mu = numeric(),
                             topset_mu = numeric(),
                             airt_sd = numeric(),
                             shapley_sd = numeric(),
                             topset_sd = numeric(),
                             num_obs_infold = numeric())
set.seed(1)
#Randomly shuffle the data
randorder <- sample(nrow(df2))
df3 <- df2[randorder, ]  # this dataset has performance

#Create 10 equally size folds
folds <- cut(seq(1,nrow(df3)),breaks=10,labels=FALSE)

num_algos_all <- c()
kk <- 1
#Perform 10 fold cross validation
for(i in 1:10){
  # Segment your data by fold
  testIndices <- which(folds==i,arr.ind=TRUE)
  testData <- df3[testIndices, ]
  trainData <- df3[-testIndices, ]

  # Train AIRT model
  modout <- cirtmodel(trainData, max.item, min.item)
  paras <- modout$model$param
  # The latent trait
  obj2 <- airt::latent_trait_analysis(trainData,modout$model$param,min.item,max.item, epsilon = 0.01 )

  # The best algorithms using different methods
  # Shapley values
  maxperf <- apply(trainData, 1, max)
  X1 <- cbind.data.frame(trainData, maxperf)
  X1 <- as.matrix(trainData)
  mod1 <- xgboost::xgboost(data = X1, label = maxperf, gamma = 0, eta = 1, lambda = 0,nrounds = 1, verbose = F)
  shap_values <- SHAPforxgboost::shap.values(xgb_model = mod1, X_train = X1)
  shap_vals <- shap_values$mean_shap_score/(sum(shap_values$mean_shap_score))*100

  # Topset algorithms ordered
  dford1 <- as_tibble(trainData) %>%
    tibble(max_algo_num = apply(trainData, 1, which.max), algo_name = colnames(trainData)[max_algo_num] ) %>%
    group_by(max_algo_num, algo_name) %>%
    summarize(count = n()) %>%
    arrange(desc(count))
  dford <- dford1[ ,1:2]
  ord_algos <- dford %>% pull(max_algo_num )
  algo_names <- colnames(trainData)
  algo_orderd <- algo_names[ord_algos]

  # Best algo in the test set
  best_perf <- apply(testData, 1, max)
  best_algo <- algo_names[apply(testData, 1, which.max)]

  # airt algorithm basket
  algo_set1 <- obj2$strengths$proportions$algorithm

  # number of algorithms = min(shapley_set, top_set, airt_set )
  num_algos <- min(length(algo_set1), sum(shap_vals>0), length(algo_orderd))
  airtcols <- algo_set1[1:num_algos]

  # Shapley values algorithm basket
  algo_set2 <- names(shap_vals[1:num_algos])

  # top algorithms basket
  algo_set3 <- algo_orderd[1:num_algos]

  testairt <- testData[ ,algo_set1]
  testshapley <- testData[ ,algo_set2]
  testtopset <- testData[ ,algo_set3]


  # TOP 1 to n - BEGIN
  for(ll in 1:num_algos){

    # initialising rows to zero
    mean_par10_gap[kk, ] <- 0
    mean_par10_gap[kk,1] <- i
    mean_par10_gap[kk,2] <- ll
    mean_par10_gap[kk,9] <- dim(testData)[1]

    # airt
    df4 <- data.frame(testData[ ,airtcols[1:ll]])
    best_perf_1 <- apply(df4, 1, max)
    worst_perf_1 <- apply(df4, 1, min)
    mean_perf_1 <- apply(df4, 1, mean)
    # mean gap
    mean_perf_diff_1 <- best_perf - mean_perf_1
    mean_par10_gap[kk, 3] <- mean(mean_perf_diff_1)
    mean_par10_gap[kk, 6] <- sd(mean_perf_diff_1)


    # Shapley values algorithm basket
    df5 <- data.frame(testData[ ,algo_set2[1:ll]])
    best_perf_2 <- apply(df5, 1, max)
    worst_perf_2 <- apply(df5, 1, min)
    mean_perf_2 <- apply(df5, 1, mean)
    # mean gap
    mean_perf_diff_2 <- best_perf - mean_perf_2
    mean_par10_gap[kk, 4] <- mean(mean_perf_diff_2)
    mean_par10_gap[kk, 7] <- sd(mean_perf_diff_2)


    # top algorithms basket
    df6 <- data.frame(testData[ ,algo_set3[1:ll]])
    best_perf_3 <- apply(df6, 1, max)
    worst_perf_3 <- apply(df6, 1, min)
    mean_perf_3 <- apply(df6, 1, mean)
    # mean gap
    mean_perf_diff_3 <- best_perf - mean_perf_3
    mean_par10_gap[kk, 5] <- mean(mean_perf_diff_3)
    mean_par10_gap[kk, 8] <- sd(mean_perf_diff_3)

    kk <- kk +1
  }
}

fname_mean <- paste("Data_Output/", prefix, "mean_gap_par10.csv", sep="")
write.csv(mean_par10_gap, fname_mean, row.names = FALSE)


# ------------------------------------------------------------------------------
# TASK  4: PLOTTING
# ------------------------------------------------------------------------------
fname_mean <- paste("Data_Output/", prefix, "mean_gap_par10.csv", sep="")
mean_par10_gap <-  read.csv(fname_mean)


# Gap of the average (PAR10)
mean_par10_gap_sum <- mean_par10_gap %>%
  select(num_algos, airt_mu, shapley_mu, topset_mu) %>%
  group_by(num_algos) %>%
  summarize(mean_airt = mean(airt_mu),
            mean_shapley = mean(shapley_mu),
            mean_topset = mean(topset_mu),
            sd_airt = sd(airt_mu),
            sd_shapley = sd(shapley_mu),
            sd_topset = sd(topset_mu),
            count = n())

mean_par10_gap_lng1 <- mean_par10_gap_sum %>%
  select(num_algos, mean_airt, mean_shapley, mean_topset) %>%
  rename(airt = mean_airt, shapley = mean_shapley, topset = mean_topset) %>%
  pivot_longer(cols = 2:4) %>%
  rename(mean = value)

mean_par10_gap_lng2 <- mean_par10_gap_sum %>%
  select(num_algos, sd_airt, sd_shapley, sd_topset, count) %>%
  mutate(airt = sd_airt/sqrt(count), shapley = sd_shapley/sqrt(count), topset = sd_topset/sqrt(count)) %>%
  select(num_algos, airt, shapley, topset) %>%
  pivot_longer(cols = 2:4) %>%
  rename(sd = value)

mean_par10_gap_lng <- full_join(mean_par10_gap_lng1, mean_par10_gap_lng2)

mean_par10_gap_lng <- mean_par10_gap_lng %>%
  rename(Portfolio = name)

ggplot(mean_par10_gap_lng, aes(x = num_algos, y = mean, color = Portfolio)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2) +
  ylab("Mean Performance Gap") +
  xlab("Number of Algorithms in Portfolio")



