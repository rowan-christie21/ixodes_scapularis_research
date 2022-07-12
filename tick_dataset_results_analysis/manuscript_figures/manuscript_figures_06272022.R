
#----------------------------------------------------------------------------------------


# 6/26/2022 Code for figures in manuscript

# title: Longer study length, standardized sampling techniques, and broader geographic scope leads to higher likelihood of detecting stable abundance patterns in long term deer tick studies
# doi: https://doi.org/10.1101/2021.03.06.434217

# github repo: https://github.com/SMCCoder/tick_dataset_results_analysis


#----------------------------------------------------------------------------------------

#-------------------------------------------
# load libraries

library(ggplot2)
library(ggpubr)

#import script from QsRutils package to add functions for creating letter assignments for groups that are not signifcantly different
#reference: Piepho, H. P. 2004. An algorithm for a letter-based representation of all-pairwise comparisons. Journal of Computational and Graphical Statistics **13**:456-466.
source("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/QsRutils_05182020/make_letter_assignments.R")
source("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/QsRutils_05182020/get_plot_limits.R")

#-------------------------------------------

# read in tick dataset results from 7/11/2022
tick_dataset_results <- readxl::read_xlsx("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/data/tick dataset results_07112022.xlsx", sheet = 1)

#-------------------------------------------

##############################

# Figure 1: The fraction of datasets that take y years to reach stability

##############################

#need to create new column indicating # of datasets with stability time greater than a certain value
tick_dataset_results$stability_time_culamative <- 0

#calculating number of datasets with stability time higher than years
for(years in tick_dataset_results$stability_time) {
  tick_dataset_results[tick_dataset_results$stability_time == years,]$stability_time_culamative <- length(tick_dataset_results[tick_dataset_results$stability_time>years,]$stability_time)
}

#need to calculate proportion of datasets for each stability time value
tick_dataset_results$stability_time_proportion <- 0
for(years in tick_dataset_results$stability_time) {
  tick_dataset_results[tick_dataset_results$stability_time == years,]$stability_time_proportion <- length(tick_dataset_results[tick_dataset_results$stability_time<=years,]$stability_time)
}

# create lineplot for years to reach stability for each range of years culamative
years_to_reach_stability_num <- ggplot(tick_dataset_results, aes(x = stability_time_proportion, y = stability_time)) +
  geom_line(color="skyblue", size=2)+
  scale_y_continuous(name = "Years to reach stability", expand = c(0,0), limits = c(0,25)) +
  xlab("Number of datasets") + 
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_text(colour="black", size = 18),
        axis.text.y=element_text(colour="black", size = 18),
        axis.title.x = element_text(size = 23, margin=margin(15,0,0,0)),
        axis.title.y = element_text(size = 23, margin=margin(0,15,0,0)),
        plot.margin = margin(10, 20, 5, 5))

years_to_reach_stability_num

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/manuscript_figures/figure_1_years_to_reach_stability_num_line_chart ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
years_to_reach_stability_num
dev.off()


##############################

# Figure 2: Comparison of study length, years to reach stability and the number of datasets

##############################

length(tick_dataset_results$vector)
#confirm 289 observations (raw datasets)

t.test(tick_dataset_results$stability_time, tick_dataset_results$data_range)
#t = -15.933, df = 485.02, p-value < 2.2e-16
#difference between datasets is significant

#compare stability time with data range
cor(tick_dataset_results$stability_time, tick_dataset_results$data_range)
#0.8302934

#compare stability time with total number of datasets
cor(tick_dataset_results$stability_time, tick_dataset_results$stability_time_culamative)
# -0.9782056

#x axis overall study length and y axis 'years to stability' and 
#use the colors/symbols to graph EVERY observation
# create lineplot for years to reach stability for each range of years culamative
years_to_reach_stability_length <- ggplot(tick_dataset_results, aes(x = data_range, y = stability_time, size=stability_time_proportion)) +
  geom_point()+
  scale_y_continuous(name = "Years to reach stability", expand = c(0,0), limits = c(0,25)) +
  xlab("Study length (years)") + 
  labs(size="Number of datasets") +
  scale_size_continuous(limits = c(1,300), breaks=seq(50,300,by=50)) +
  xlim(0,25) +
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title=element_text(size = 27, margin=margin(0,0,15,0)),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 17),
        legend.position = c(0.2, 0.8),
        axis.text.x=element_text(colour="black", size = 18),
        axis.text.y=element_text(colour="black", size = 18),
        axis.title.x = element_text(size = 23, margin=margin(15,0,0,0)),
        axis.title.y = element_text(size = 23, margin=margin(0,15,0,0)),
        plot.margin = margin(10, 20, 5, 5))

years_to_reach_stability_length

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/manuscript_figures/figure_2_years_to_reach_stability_length_line_chart ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
years_to_reach_stability_length
dev.off()


##############################

# Figure 3: Overall proportion wrong compared to proportion wrong before stability

##############################

#proportion wrong (overall) is looking at all the iterations (the breakups) of the data and 
#regression fits to see your odds of getting misleading patterns (across all years and subsets 
#of years)

#Proportion wrong before stability is looking specifically at the iterations (breakups) 
#*before stability is reached* to give more specific insight into the odds of finding a misleading 
#pattern (outside the error bounds around the stability trend line) if your iteration (or study 
#length) is shorter than the stability time calculation.  This is more informative about stability 
#time as a meaningful function than proportion wrong overall.

#comparing overall proportion wrong with proportion wrong before stability
plot(tick_dataset_results$`proportion wrong`)
plot(tick_dataset_results$`proportion wrong before stability`)

t.test(tick_dataset_results$proportion_wrong, tick_dataset_results$proportion_wrong_before_stability)
#t = -1.2137, df = 571.46, p-value = 0.2254
#insignificant difference
#no need for letter assignment


tick_dataset_results$pw_label <- "Overall"
tick_dataset_results$pwbs_label <- "Before reaching stability"

#organizing labels and values into dataframe
overall_pw_vs_pwbs_lab <- c(tick_dataset_results$pw_label, tick_dataset_results$pwbs_label)
overall_pw_vs_pwbs_value <- c(tick_dataset_results$proportion_wrong, tick_dataset_results$proportion_wrong_before_stability)
overall_pw_vs_pwbs_df <- data.frame(overall_pw_vs_pwbs_lab, overall_pw_vs_pwbs_value)

#overall proportion significantly wrong by proportion wrong before stability
overall_pw_vs_pwbs <- ggplot(overall_pw_vs_pwbs_df, aes(x = reorder(overall_pw_vs_pwbs_lab, overall_pw_vs_pwbs_value), y = overall_pw_vs_pwbs_value)) +
  geom_boxplot() + 
  geom_jitter() +
  scale_x_discrete(name=NULL) +
  scale_y_continuous(name = "Proportion wrong", limits = c(0,1.05)) +
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title=element_text(size = 27, margin=margin(0,0,15,0)),
        axis.text.x=element_text(colour="black", size = 16),
        axis.text.y=element_text(colour="black", size = 18),
        axis.title.x = element_text(size = 23, margin=margin(15,0,0,0)),
        axis.title.y = element_text(size = 23, margin=margin(0,15,0,0)))

overall_pw_vs_pwbs

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/manuscript_figures/figure_3_overall_pw_vs_pwbs ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
overall_pw_vs_pwbs
dev.off()

##############################

# Figure 4A, 4B comparing stability time and proportion significantly wrong for sampling technique

##############################

dragging <- subset(tick_dataset_results, sampling_technique == "dragging")
found <- subset(tick_dataset_results, sampling_technique == "found on a person")

length(dragging$stability_time)
#90 datasets
length(found$stability_time)
#198 datasets

length(subset(tick_dataset_results, sampling_technique == "bites found on a person")$stability_time)
#one instance was recorded with sampling technique = bites found on a person which was excluded from this analysis

median(dragging$stability_time)
#7
median(found$stability_time)
#12

t.test(dragging$stability_time, found$stability_time)
#t = -8.5346, df = 236.23, p-value = 1.724e-15
#significant
#use letter assignment to differentiate groups

t.test(dragging$proportion_wrong_before_stability, found$proportion_wrong_before_stability)
#t = 0.083576, df = 155.58, p-value = 0.9335
#insignificant
#no need for letter assignment

###############
# 4A sampling technique vs stability time
###############

# create boxplot for proportion significantly wrong between different sampling methods
tick_dataset_results_drag_found <- subset(tick_dataset_results, sampling_technique == "dragging" | sampling_technique == "found on a person")

#set up compact letter display
box.rslt <- with(tick_dataset_results_drag_found, graphics::boxplot(stability_time ~ sampling_technique, plot = FALSE))
ttest.rslt <- with(tick_dataset_results_drag_found, pairwise.t.test(stability_time, sampling_technique, pool.sd = FALSE))
ltrs <- make_letter_assignments(ttest.rslt)
x <- c(1:length(ltrs$Letters))
y <- box.rslt$stats[5, ]
cbd <- ltrs$Letters
ltr_df <- data.frame(x, y, cbd)

stability_time_by_samp_tech <- ggplot(tick_dataset_results_drag_found, aes(x = sampling_technique, y = stability_time)) +
  geom_boxplot() + 
  geom_jitter() +
  geom_text(data = ltr_df, aes(x=x, y=y, label=cbd), nudge_y = 1.25,color="red",size=6) +
  scale_x_discrete(name = "Sampling technique") +
  scale_y_continuous(name = "Stability time", limits = c(0,25)) +
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title=element_text(size = 27, margin=margin(0,0,15,0)),
        axis.text.x=element_text(colour="black", size = 18),
        axis.text.y=element_text(colour="black", size = 18),
        axis.title.x = element_text(size = 23, margin=margin(15,0,0,0)),
        axis.title.y = element_text(size = 23, margin=margin(0,15,0,0)))

stability_time_by_samp_tech

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/manuscript_figures/figure_4A_stability_time_by_samp_tech ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
stability_time_by_samp_tech
dev.off()

###############
# 4B sampling technique vs proportion wrong before stability
###############

# create boxplot for proportion significantly wrong between different sampling methods
tick_dataset_results_drag_found <- subset(tick_dataset_results, sampling_technique == "dragging" | sampling_technique == "found on a person")

proportion_wrong_before_stab_by_samp_tech <- ggplot(tick_dataset_results_drag_found, aes(x = sampling_technique, y = proportion_wrong_before_stability)) +
  geom_boxplot() + 
  geom_jitter() +
  scale_x_discrete(name = "Sampling technique") +
  scale_y_continuous(name = "Proportion significantly wrong \nbefore stability", limits = c(0,1.05)) +
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title=element_text(size = 27, margin=margin(0,0,15,0)),
        axis.text.x=element_text(colour="black", size = 18),
        axis.text.y=element_text(colour="black", size = 18),
        axis.title.x = element_text(size = 23, margin=margin(15,0,0,0)),
        axis.title.y = element_text(size = 23, margin=margin(0,15,0,0)))

proportion_wrong_before_stab_by_samp_tech

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/manuscript_figures/figure_4B_proportion_wrong_before_stab_by_samp_tech ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
proportion_wrong_before_stab_by_samp_tech
dev.off()

###############
# Combined plots 4A and 4B
###############

#arrange plots 4A and 4B into single image
figure4AB <- ggarrange(
  stability_time_by_samp_tech + 
    scale_x_discrete(name = NULL) + 
    theme(axis.title.y = element_text(margin=margin(0,-20,0,0))), 
  proportion_wrong_before_stab_by_samp_tech, 
  labels = c("A", "B"), nrow = 2, ncol=1, align = "v", font.label = list(size=25), hjust=-7
)

figure4AB

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/manuscript_figures/figure_4AB ",Sys.Date(),".png", sep = ''), width = 600, height = 908)
figure4AB
dev.off()


##############################

# Figure 5A, 5B comparing stability time and proportion significantly wrong for life stage

##############################

adults <- subset(tick_dataset_results, life_stage == "adult" | life_stage == "adults")
nymphs <- subset(tick_dataset_results, life_stage == "nymph" | life_stage == "nymphs")
larvae <- subset(tick_dataset_results, life_stage == "larvae")

t.test(adults$stability_time, nymphs$stability_time)
#t = -0.63139, df = 128.99, p-value = 0.5289
#insignificant
t.test(adults$stability_time, larvae$stability_time)
#t = -5.9627, df = 10.111, p-value = 0.0001328
#significant
t.test(nymphs$stability_time, larvae$stability_time)
#t = -5.5593, df = 10.325, p-value = 0.0002145
#significant

t.test(adults$proportion_wrong_before_stability, nymphs$proportion_wrong_before_stability)
#t = 2.9877, df = 112.55, p-value = 0.003451
#significant
t.test(adults$proportion_wrong_before_stability, larvae$proportion_wrong_before_stability)
#t = 0.43788, df = 8.7735, p-value = 0.6721
#insignificant
t.test(nymphs$proportion_wrong_before_stability, larvae$proportion_wrong_before_stability)
#t = -0.77913, df = 7.8459, p-value = 0.4588
#insignificant

length(adults$stability_time)
#63 datasets
length(nymphs$stability_time)
#68 datasets
length(larvae$stability_time)
#8 datasets

length(subset(tick_dataset_results, life_stage == "unspecified" | life_stage == "not specified")$stability_time)
#150
#all subsets add up to 289

median(adults$stability_time)
#7
median(nymphs$stability_time)
#7
median(larvae$stability_time)
#11.5

median(adults$proportion_wrong_before_stability)
#0.1
median(nymphs$proportion_wrong_before_stability)
#0.04166667
median(larvae$proportion_wrong_before_stability)
#0.07340067

###############
# 5A life stage vs stability time
###############

#change any nymphs to nymph in life stage column in case of any spelling errors
for(i in 1:nrow(tick_dataset_results)) {
  if(tick_dataset_results$life_stage[i] == "nymphs") {
    tick_dataset_results$life_stage[i] = "nymph"
  }
}

tick_dataset_results_ls <- subset(tick_dataset_results, life_stage == "larvae" | life_stage == "nymph" | life_stage == "adult")
tick_dataset_results_ls$life_stage <- factor(tick_dataset_results_ls$life_stage, c("larvae", "nymph", "adult"))

#set up compact letter display
box.rslt <- with(tick_dataset_results_ls, graphics::boxplot(stability_time ~ life_stage, plot = FALSE))
ttest.rslt <- with(tick_dataset_results_ls, pairwise.t.test(stability_time, life_stage, pool.sd = FALSE))
ltrs <- make_letter_assignments(ttest.rslt)
x <- c(1:length(ltrs$Letters))
y <- box.rslt$stats[5, ]
cbd <- ltrs$Letters
ltr_df <- data.frame(x, y, cbd)

stability_time_by_life_stage <- ggplot(tick_dataset_results_ls, aes(x = life_stage, y = stability_time)) +
  geom_boxplot() + 
  geom_jitter() +
  geom_text(data = ltr_df, aes(x=x, y=y, label=cbd), nudge_y = 1.25,color="red", size=6) +
  scale_x_discrete(name = "Life stage") +
  scale_y_continuous(name = "Stability time", limits = c(0,25)) +
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title=element_text(size = 27, margin=margin(0,0,15,0)),
        axis.text.x=element_text(colour="black", size = 18),
        axis.text.y=element_text(colour="black", size = 18),
        axis.title.x = element_text(size = 23, margin=margin(15,0,0,0)),
        axis.title.y = element_text(size = 23, margin=margin(0,15,0,0)))

stability_time_by_life_stage

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/manuscript_figures/figure_5A_stability_time_by_life_stage ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
stability_time_by_life_stage
dev.off()

###############
#life stage vs proportion wrong before stability
###############

# create boxplot for proportion significantly wrong between different life stages
tick_dataset_results_ls <- subset(tick_dataset_results, life_stage == "larvae" | life_stage == "nymph" | life_stage == "adult")
tick_dataset_results_ls$life_stage <- factor(tick_dataset_results_ls$life_stage, c("larvae", "nymph", "adult"))

#set up compact letter display
box.rslt <- with(tick_dataset_results_ls, graphics::boxplot(proportion_wrong_before_stability ~ life_stage, plot = FALSE))
ttest.rslt <- with(tick_dataset_results_ls, pairwise.t.test(proportion_wrong_before_stability, life_stage, pool.sd = FALSE))
ltrs <- make_letter_assignments(ttest.rslt)
x <- c(1:length(ltrs$Letters))
y <- box.rslt$stats[5, ]
cbd <- ltrs$Letters
ltr_df <- data.frame(x, y, cbd)

proportion_wrong_before_stab_by_life_stage <- ggplot(tick_dataset_results_ls, aes(x = life_stage, y = proportion_wrong_before_stability)) +
  geom_boxplot() + 
  geom_jitter() +
  geom_text(data = ltr_df, aes(x=x, y=y, label=cbd), nudge_y = 0.05,color="red", size=6) +
  scale_x_discrete(name = "Life stage") +
  scale_y_continuous(name = "Proportion significantly wrong \nbefore stability", limits = c(0,1.05)) +
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title=element_text(size = 27, margin=margin(0,0,15,0)),
        axis.text.x=element_text(colour="black", size = 18),
        axis.text.y=element_text(colour="black", size = 18),
        axis.title.x = element_text(size = 23, margin=margin(15,0,0,0)),
        axis.title.y = element_text(size = 23, margin=margin(0,15,0,0)))

proportion_wrong_before_stab_by_life_stage

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/manuscript_figures/figure_5B_proportion_wrong_before_stab_by_life_stage ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
proportion_wrong_before_stab_by_life_stage
dev.off()

###############
# Combined plots 5A and 5B
###############

#arrange plots 5A and 5B into single image
figure5AB <- ggarrange(
  stability_time_by_life_stage + 
    scale_x_discrete(name = NULL) + 
    theme(axis.title.y = element_text(margin=margin(0,-20,0,0))), 
  proportion_wrong_before_stab_by_life_stage, 
  labels = c("A", "B"), nrow = 2, ncol=1, align = "v", font.label = list(size=25), hjust=-7
)

figure5AB

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/manuscript_figures/figure_5AB ",Sys.Date(),".png", sep = ''), width = 600, height = 908)
figure5AB
dev.off()


##############################

# Figure 6A, 6B comparing stability time and proportion significantly wrong for geographic scope

##############################

county <- subset(tick_dataset_results, geographic_scope == "County")
town <- subset(tick_dataset_results, geographic_scope == "Town")
state_forest <- subset(tick_dataset_results, geographic_scope == "State forest")
grid <- subset(tick_dataset_results, geographic_scope == "Grid")

length(county$stability_time)
#73 datasets
length(town$stability_time)
#186 datasets
length(state_forest$stability_time)
#6 datasets
length(grid$stability_time)
#24 datasets

median(county$stability_time)
#73 
median(town$stability_time)
#186 
median(state_forest$stability_time)
#6 
median(grid$stability_time)
#24 

t.test(county$stability_time, town$stability_time)
#t = -17.029, df = 243.09, p-value < 2.2e-16
#signficiant

t.test(county$stability_time, state_forest$stability_time)
#t = -5.5457, df = 5.2241, p-value = 0.002278
#significant

t.test(county$stability_time, grid$stability_time)
#t = -17.207, df = 33.87, p-value < 2.2e-16
#signifcant

t.test(town$stability_time, state_forest$stability_time)
#t = 0.47098, df = 6.1169, p-value = 0.654
#insignificant

t.test(town$stability_time, grid$stability_time)
#t = 0.22406, df = 86.176, p-value = 0.8232
#insignificant

t.test(state_forest$stability_time, grid$stability_time)
#t = -0.37552, df = 6.0307, p-value = 0.7201
#insignificant

#----

t.test(county$proportion_wrong_before_stability, town$proportion_wrong_before_stability)
#t = 0.71555, df = 115.03, p-value = 0.4757
#insignficiant

t.test(county$proportion_wrong_before_stability, state_forest$proportion_wrong_before_stability)
#t = 5.2409, df = 74.293, p-value = 1.445e-06
#significant

t.test(county$proportion_wrong_before_stability, grid$proportion_wrong_before_stability)
#t = 1.3299, df = 43.066, p-value = 0.1905
#insignifcant

t.test(town$proportion_wrong_before_stability, state_forest$proportion_wrong_before_stability)
#t = 7.4221, df = 46.441, p-value = 2.013e-09
#significant

t.test(town$proportion_wrong_before_stability, grid$proportion_wrong_before_stability)
#t = 1.0046, df = 28.519, p-value = 0.3235
#insignificant

t.test(state_forest$proportion_wrong_before_stability, grid$proportion_wrong_before_stability)
#t = -1.9093, df = 25.101, p-value = 0.06772
#insignificant

###############
#geographic scope vs stability time
###############

#order factors
tick_dataset_results$geographic_scope <- factor(tick_dataset_results$geographic_scope, c("Grid", "State forest", "Town", "County"))

#set up compact letter display
box.rslt <- with(tick_dataset_results, graphics::boxplot(stability_time ~ geographic_scope, plot = FALSE))
ttest.rslt <- with(tick_dataset_results, pairwise.t.test(stability_time, geographic_scope, pool.sd = FALSE))
ltrs <- make_letter_assignments(ttest.rslt)
x <- c(1:length(ltrs$Letters))
y <- box.rslt$stats[5, ]
cbd <- ltrs$Letters
ltr_df <- data.frame(x, y, cbd)

# create boxplot for stability time between different geographic scopes
stability_time_by_geographic_scope <- ggplot(tick_dataset_results, aes(x = geographic_scope, y = stability_time)) +
  geom_boxplot() + 
  geom_jitter() +
  geom_text(data = ltr_df, aes(x=x, y=y, label=cbd), nudge_y = 1.25,color="red", size=6) +
  scale_x_discrete(name = "Geographic scope") +
  scale_y_continuous(name = "Stability time", limits = c(0,25), breaks = c(0,5,10,15,20,25)) +
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title=element_text(size = 27, margin=margin(0,0,15,0)),
        axis.text.x=element_text(colour="black", size = 18),
        axis.text.y=element_text(colour="black", size = 18),
        axis.title.x = element_text(size = 23, margin=margin(15,0,0,0)),
        axis.title.y = element_text(size = 23, margin=margin(0,15,0,0)))

stability_time_by_geographic_scope

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/manuscript_figures/figure_6A_stability_time_by_geographic_scope ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
stability_time_by_geographic_scope
dev.off()

###############
#geographic scope vs proportion wrong before stability
###############

# create boxplot for proportion significantly wrong between different geographic scopes
tick_dataset_results$geographic_scope <- factor(tick_dataset_results$geographic_scope, c("Grid", "State forest", "Town", "County"))

#set up compact letter display
box.rslt <- with(tick_dataset_results, graphics::boxplot(proportion_wrong_before_stability ~ geographic_scope, plot = FALSE))
ttest.rslt <- with(tick_dataset_results, pairwise.t.test(proportion_wrong_before_stability, geographic_scope, pool.sd = FALSE))
ltrs <- make_letter_assignments(ttest.rslt)
x <- c(1:length(ltrs$Letters))
y <- box.rslt$stats[5, ]
cbd <- ltrs$Letters
ltr_df <- data.frame(x, y, cbd)

proportion_wrong_before_stab_by_geographic_scope <- ggplot(tick_dataset_results, aes(x = geographic_scope, y = proportion_wrong_before_stability)) +
  geom_boxplot() + 
  geom_jitter() +
  geom_text(data = ltr_df, aes(x=x, y=y, label=cbd), nudge_y = 0.05,color="red", size=6) +
  scale_x_discrete(name = "Geographic scope") +
  scale_y_continuous(name = "Proportion significantly wrong \nbefore stability", limits = c(0,1.06)) +
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title=element_text(size = 27, margin=margin(0,0,15,0)),
        axis.text.x=element_text(colour="black", size = 18),
        axis.text.y=element_text(colour="black", size = 18),
        axis.title.x = element_text(size = 23, margin=margin(15,0,0,0)),
        axis.title.y = element_text(size = 23, margin=margin(0,15,0,0)))

proportion_wrong_before_stab_by_geographic_scope

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/manuscript_figures/figure_6B_proportion_wrong_before_stab_by_geographic_scope ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
proportion_wrong_before_stab_by_geographic_scope
dev.off()

###############
# Combined plots 6A and 6B
###############

#arrange plots 6A and 6B into single image
figure6AB <- ggarrange(
  stability_time_by_geographic_scope + 
    scale_x_discrete(name = NULL) + 
    theme(axis.title.y = element_text(margin=margin(0,-20,0,0))), 
  proportion_wrong_before_stab_by_geographic_scope, 
  labels = c("A", "B"), nrow = 2, ncol=1, align = "v", font.label = list(size=25), hjust=-7
)

figure6AB

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/manuscript_figures/figure_6AB ",Sys.Date(),".png", sep = ''), width = 600, height = 908)
figure6AB
dev.off()


##############################

# Figure 7A, 7B comparing stability time and proportion significantly wrong for sampling metric

##############################

pathogen <- subset(tick_dataset_results, tested_for_b_burgdoferi == "Y")
abundance <- subset(tick_dataset_results, tested_for_b_burgdoferi == "N")

length(pathogen$stability_time)
#114 datasets
length(abundance$stability_time)
#175 datasets

t.test(pathogen$stability_time, abundance$stability_time)
#t = -1.2879, df = 283.9, p-value = 0.1988

t.test(pathogen$proportion_wrong_before_stability, abundance$proportion_wrong_before_stability)
#t = -1.1828, df = 232.98, p-value = 0.2381

###############
#ticks infected vs stability time
###############

tested_for_path <- tick_dataset_results[tick_dataset_results$tested_for_b_burgdoferi == "Y",] 
tested_for_path$label <- "Tested for infection \nof B. burgdorferi"

test_abuance <- tick_dataset_results[tick_dataset_results$tested_for_b_burgdoferi == "N",] 
test_abuance$label <- "Sampled for \nAbundance"

abundance_vs_infected_ticks <- c(tested_for_path$label, test_abuance$label)
stability_time_for_abudance_vs_infected_ticks <- c(tested_for_path$stability_time, test_abuance$stability_time)

tick_infection_data <- data.frame(abundance_vs_infected_ticks, stability_time_for_abudance_vs_infected_ticks)

t.test(tick_infection_data[tick_infection_data$abundance_vs_infected_ticks == "Tested for infection \nof B. burgdorferi",]$stability_time_for_abudance_vs_infected_ticks, tick_infection_data[tick_infection_data$abundance_vs_infected_ticks == "Sampled for \nAbundance",]$stability_time_for_abudance_vs_infected_ticks)
# t = -1.2879, df = 283.9, p-value = 0.1988
# t-value indicates probability below 0.5
# low probability of difference between datasets
#
# p-value higher than 0.05, accept null hypothesis
# therefore alternative hypothesis: true difference in means is not equal to 0 is not supported
# statiscally insignificant
# no need for letter assignment

#proportion significant by ticks infected and total ticks
stability_time_by_metric <- ggplot(tick_infection_data, aes(x = abundance_vs_infected_ticks, y = stability_time_for_abudance_vs_infected_ticks)) +
  geom_boxplot() + 
  geom_jitter() +
  scale_x_discrete(name = "Sampling metric") +
  scale_y_continuous(name = "Stability time", limits = c(0, 25)) +
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title=element_text(size = 27, margin=margin(0,0,15,0)),
        axis.text.x=element_text(colour="black", size = 18),
        axis.text.y=element_text(colour="black", size = 18),
        axis.title.x = element_text(size = 23, margin=margin(15,0,0,0)),
        axis.title.y = element_text(size = 23, margin=margin(0,15,0,0)))

stability_time_by_metric

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/manuscript_figures/figure_7A_stability_time_by_metric ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
stability_time_by_metric
dev.off()

###############
#ticks infected vs proportion wrong before stability
###############

tested_for_path <- tick_dataset_results[tick_dataset_results$tested_for_b_burgdoferi == "Y",] 
tested_for_path$label <- "Tested for infection \nof B. burgdorferi"

test_abuance <- tick_dataset_results[tick_dataset_results$tested_for_b_burgdoferi == "N",] 
test_abuance$label <- "Sampled for \nAbundance"

abundance_vs_infected_ticks <- c(tested_for_path$label, test_abuance$label)
proportion_wrong_before_stab_for_abudance_vs_infected_ticks <- c(tested_for_path$proportion_wrong_before_stability, test_abuance$proportion_wrong_before_stability)

tick_infection_data <- data.frame(abundance_vs_infected_ticks, proportion_wrong_before_stab_for_abudance_vs_infected_ticks)

t.test(tick_infection_data[tick_infection_data$abundance_vs_infected_ticks == "Tested for infection \nof B. burgdorferi",]$proportion_wrong_before_stab_for_abudance_vs_infected_ticks, tick_infection_data[tick_infection_data$abundance_vs_infected_ticks == "Sampled for \nAbundance",]$proportion_wrong_before_stab_for_abudance_vs_infected_ticks)
# t = -1.1828, df = 232.98, p-value = 0.2381
# t-value indicates probability below 0.5
# low probability of difference between datasets
#
# p-value higher than 0.05, accept null hypothesis
# therefore alternative hypothesis: true difference in means is equal to 0 is supported
# statiscally insignificant
# no need for letter assignment

#proportion significant by ticks infected and total ticks
proportion_wrong_before_stab_by_metric <- ggplot(tick_infection_data, aes(x = abundance_vs_infected_ticks, y = proportion_wrong_before_stab_for_abudance_vs_infected_ticks)) +
  geom_boxplot() + 
  geom_jitter() +
  scale_x_discrete(name = "Sampling metric") +
  scale_y_continuous(name = "Proportion significantly wrong \nbefore stability", limits = c(0, 1.05)) +
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title=element_text(size = 27, margin=margin(0,0,15,0)),
        axis.text.x=element_text(colour="black", size = 18),
        axis.text.y=element_text(colour="black", size = 18),
        axis.title.x = element_text(size = 23, margin=margin(15,0,0,0)),
        axis.title.y = element_text(size = 23, margin=margin(0,15,0,0)))

proportion_wrong_before_stab_by_metric

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/manuscript_figures/figure_7B_proportion_wrong_before_stab_by_metric ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
proportion_wrong_before_stab_by_metric
dev.off()

###############
# Combined plots 7A and 7B
###############

#arrange plots 7A and 7B into single image
figure7AB <- ggarrange(
  stability_time_by_metric + 
    scale_x_discrete(name = NULL) + 
    theme(axis.title.y = element_text(margin=margin(0,-20,0,0))), 
  proportion_wrong_before_stab_by_metric, 
  labels = c("A", "B"), nrow = 2, ncol=1, align = "v", font.label = list(size=25), hjust=-7
)

figure7AB

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/manuscript_figures/figure_7AB ",Sys.Date(),".png", sep = ''), width = 600, height = 908)
figure7AB
dev.off()
