write_folder_1 <- "Data_Output/LT4_Eps_0/"
write_folder_2 <- "Data_Output/LT4_Eps_0/Dataset_Difficulty_Latent_Trait/"
library(gridExtra)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)


title <- c("asp_potassco", "csp-minizinc-time-2016", "graphs_2015", "maxsat-pms-2016", "openml_weka_2017", "proteus-2014", "sat11_indu")

# CHANGE iii FROM 1 TO 7 FOR DIFFERENT SCENARIOS
iii <- 7
prefix <- paste(title[iii], "_", sep="")
dfl <- read_csv(paste(write_folder_1, prefix, "_for_latent_curves.csv", sep=""))
g1 <- ggplot(dfl, aes(Latent_Trait, value)) +  geom_smooth(aes(color=Algorithm), se = FALSE, method = "gam", formula = y ~s(x, bs="cs"))+  xlab("Problem difficulty") + ylab("Performance") + ggtitle(title[iii]) + theme_bw() + theme(legend.position = "none")
g1

latenttr <- read_csv(paste(write_folder_1, prefix, "_for_latent_trait_bars_strengths.csv", sep=""))
dfl2 <- pivot_longer(latenttr, cols = 2:dim(latenttr)[2])
colnames(dfl2)[2] <- "Algorithm"
dfl2 <- dfl2[dfl2$value!=0, ]
dfl2$value <- 1
dfl21 <- dfl2 %>% mutate(type = "Strengths")
dfl21 %>% group_by(Algorithm) %>% summarize(proportion = n())

latenttr <- read_csv(paste(write_folder_1, prefix, "_for_latent_trait_bars_weakness.csv", sep=""))
dfl2 <- pivot_longer(latenttr, cols = 2:dim(latenttr)[2])
colnames(dfl2)[2] <- "Algorithm"
dfl2 <- dfl2[dfl2$value!=0, ]
dfl2$value <- 1 # dfl2$value*0.1
dfl22 <- dfl2 %>% mutate(type = "Weaknesses")

dfl22 %>% group_by(Algorithm) %>% summarize(proportion = n())

num_algos <- length(unique(dfl$Algorithm))
colrs <- scales::hue_pal()(num_algos)
dflall <- bind_rows(dfl21, dfl22)
colrs2 <- colrs[which(sort(unique(dfl$Algorithm)) %in% unique(dflall$Algorithm))]

if(iii == 1){
  # for asp_potassco
  dflall$Algorithm <- substring(dflall$Algorithm, 13, nchar(dflall$Algorithm))
}


g2 <- ggplot(dflall, aes(x = latenttrait, y =value, fill = Algorithm)) + geom_tile() + facet_grid(~type) + theme(axis.title.y=element_blank(), axis.text.y=element_blank(),axis.ticks.y=element_blank())  + scale_fill_manual(values = colrs2) + xlab("Problem Difficulty") +  coord_fixed(ratio=0.3) + theme(legend.position = "right") + theme(legend.key.size = unit(0.1, 'cm'), legend.box="vertical") # + guides(color = guide_legend(override.aes = list(size = 0.01))) #+  theme(legend.title = element_text( size=0.1))
g2
#
# gridExtra::grid.arrange(g1, g2)

# CV FOLDS
colrs <- scales::hue_pal()(4)
df_cv <- read.csv(paste(write_folder_1, prefix, "_CV_10_fold.csv", sep=""))
colnames(df_cv)[2] <-  "strong-airt"
df_cv <- df_cv[10:14, ]
df_cv_long <- pivot_longer(df_cv, cols = 2:4)
colnames(df_cv_long)[2:3] <- c("Meta_Algo", "Perf_Diff")
g3 <- ggplot(df_cv_long, aes(x=quantiles, y=Perf_Diff, color=Meta_Algo)) + geom_point() + geom_line() + ylab("Performance Deterioration") + scale_color_manual(values = colrs[1:3]) + theme_bw()
g3

# Number of algorithms
all_algos <- length(unique(dfl$Algorithm))

# Best Algo props mean - worst set
worst_algo_props_mean <- read.csv(paste(write_folder_1, prefix, "_worst_algo_props.csv", sep=""))
worst_algo_props_mean[1, 1] <- "weak-airt"
ind <- min(apply(apply(worst_algo_props_mean[ ,-1], 1, diff), 2, function(x) which(x<0)))
num <- min(10, ceiling(all_algos/2), (dim(worst_algo_props_mean)[2]-1), (ind))
worst_algo_props_mean <- worst_algo_props_mean[ ,1:(num+1)]
worst_algo_long <- pivot_longer(worst_algo_props_mean, cols =2:dim(worst_algo_props_mean)[2])
colnames(worst_algo_long)[2:3] <- c("Num_Algos", "Best_Algo_Proportion")
worst_algo_long$Num_Algos <- as.numeric(paste(substr(worst_algo_long$Num_Algos, 2, nchar(worst_algo_long$Num_Algos))))
g4 <- ggplot(worst_algo_long, aes(x=Num_Algos, y=Best_Algo_Proportion)) + geom_point(aes(color=Meta_Algo)) + geom_line(aes(color=Meta_Algo)) + ylab("Best Algo Proportion") + theme_bw() + scale_color_manual(values = colrs[c(4,2)])
# g4
#
# gridExtra::grid.arrange(g3, g4, nrow = 1)


if(iii == 7){
  gridExtra::grid.arrange(g1, g2, g3, g4, layout_matrix = matrix(c(1, 1, 2, 2, 3, 4), byrow = TRUE, nrow = 3))
}else{
  gridExtra::grid.arrange(g1, g2, g3, g4, layout_matrix = matrix(c(1, 1, 2, 2, 2, 2, 3, 3, 3, 4, 4, 4), byrow = TRUE, nrow = 2))
}

# 700 x 400
# 700 x 500 for proteus iii = 6
