# Oct 29 2019
# Running t-test on tick_dataset_results.xlsx file with response variables (density, count) on
# y-axis and methods/life stage/etc as x-axis to determine probability of difference between methods

# Oct 30 2019
# Running more t-tests and further analyzing the data

# Nov 5 2019
# Testing more factors potentially affecting stability time and proportion significant

# Feb 11 2020
# Running functions on updated dataset (tick_dataset_results_02_11_2020)

# March 21 2020
# Creating culmative line plot figures for stability time

# March 25 2020
# Loading new datafile and comparing proportion wrong by stability time

# April 3 2020
# making adjustments to figures

# April 8 2020
# Changing number of datasets to proportion of datasets

# April 11 2020
# Add scatter plots for geographic scope and life stage figures

# April 13 2020
# Adjusting life stage and geographic scope figures for proportion of datasets

# April 23 2020
# Adjusted figures w/o titles

# April 28 2020
# Adjusted figures

#-------------------------------------------
# load libraries

library(ggplot2)
library(dplyr)
library(wesanderson)
library(RColorBrewer)
library(MuMIn)
library(car)
library(nlme)  
library(ncf)
library(lme4)
library(Matrix)
library(coefplot)
library(MASS)

#-------------------------------------------

#----------------------------------------------------------------------------------------
#
#
#
#
#
#             Analysis of 10/29/2020 tick dataset results
#
#
#
#
#
#
#----------------------------------------------------------------------------------------

# read in tick dataset results from 10/29/2019
tick_dataset_results <- readxl::read_xlsx("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/data/tick_dataset_results_10292019.xlsx", sheet = 1)

# subset data between two different methods - dragging and found on a person
dragging <- tick_dataset_results[tick_dataset_results$sampling_technique == "dragging",]
found_on_people <- tick_dataset_results[tick_dataset_results$sampling_technique == "found on a person",]
# subset deer tick life stages
adult <- tick_dataset_results[tick_dataset_results$life_stage == "adult",]
nymph <- tick_dataset_results[tick_dataset_results$life_stage == "nymph",]
larvae <- tick_dataset_results[tick_dataset_results$life_stage == "larvae",]

t.test(adult$stability_time, nymph$stability_time)
#t = 0.5784, df = 82.59, p-value = 0.5646
# t-value indicates probability between 0.5 and 0.75
# low probability of difference between datasets
#
# p-value greater than 0.05, accept null hypothesis
# statistically insignificant

t.test(adult$stability_time, larvae$stability_time)
#t = -5.1721, df = 12.859, p-value = 0.000186
# t-value indicates probability below 0.5
# very low probability of difference between datasets
#
# p-value less than 0.05, reject null hypothesis
# therefore alternative hypothesis: true difference in means is not equal to 0 is supported
# statiscally significant

t.test(nymph$stability_time, larvae$stability_time)
# t = -5.755, df = 11.406, p-value = 0.0001107
# t-value indicates probability below 0.5
# very low probability of difference between datasets
#
# p-value less than 0.05, reject null hypothesis
# therefore alternative hypothesis: true difference in means is not equal to 0 is supported
# statiscally significant


# create barplot for years to reach stability for each range of years
years_to_reach_stability <- ggplot(tick_dataset_results, aes(x = stability_time)) +
  geom_bar() +
  scale_y_continuous(name = "# of datasets", expand = c(0,0)) +
  xlab("Years to reach stability") + 
  xlim(low=0, high=14)+
  ggtitle("Years to reach stability for all datasets") +
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

years_to_reach_stability

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/years_to_reach_stability ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
years_to_reach_stability
dev.off()


# create lineplot for years to reach stability for each range of years
years_to_reach_stability <- ggplot(tick_dataset_results, aes(x = stability_time)) +
  geom_smooth(stat = "count", size=2) +
  scale_y_continuous(name = "# of datasets", expand = c(0,0)) +
  xlab("Years to reach stability") + 
  xlim(low=0, high=14)+
  ggtitle("Years to reach stability for all datasets") +
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

years_to_reach_stability

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/years_to_reach_stability_line_chart ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
years_to_reach_stability
dev.off()


# run t-test between two different datasets: dragging and found_on_people, for stability time
# assumes that the variances of y1 and y2 are unequal
t.test(dragging$stability_time, found_on_people$stability_time)
# t = 4.1311, df = 128.88, p-value = 6.451e-05
# t-value indicates probability above 0.9995
# very high probability of difference between datasets
#
# p-value less than 0.05, reject null hypothesis
# therefore alternative hypothesis: true difference in means is not equal to 0 is supported

# create boxplot for years to reach stability between different sampling methods
tick_dataset_results_drag_found <- subset(tick_dataset_results, sampling_technique == "dragging" | sampling_technique == "found on a person")
yrs_stab_by_samp_tech <- ggplot(tick_dataset_results_drag_found, aes(x = sampling_technique, y = stability_time)) +
  geom_boxplot() + 
  scale_x_discrete(name = "Sampling Technique") +
  scale_y_continuous(name = "Time to reach stability (years)") +
  ggtitle("Years to reach stability for each sampling \ntechnique") + 
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

yrs_stab_by_samp_tech

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/yrs_stab_by_samp_tech ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
yrs_stab_by_samp_tech
dev.off()


# run t-test between two different datasets: dragging and found_on_people, for proportion right
# assumes that the variances of y1 and y2 are unequal
t.test(dragging$proportion_right, found_on_people$proportion_right)
#t = -1.9102, df = 102.04, p-value = 0.05892
# t-value indicates probability below 0.5
# low probability of difference between datasets
#
# p-value above 0.05, supports null hypothesis
# therefore true difference in means is equal to 0 is supported
# and not statiscally significant

# create boxplot for proportion significant between different sampling methods
tick_dataset_results_drag_found <- subset(tick_dataset_results, sampling_technique == "dragging" | sampling_technique == "found on a person")
proportion_sig_by_samp_tech <- ggplot(tick_dataset_results_drag_found, aes(x = sampling_technique, y = proportion_significant)) +
  geom_boxplot() + 
  scale_x_discrete(name = "Sampling Technique") +
  scale_y_continuous(name = "Proportion Significantly Right") +
  ggtitle("Proportion significantly right for \neach sampling technique") + 
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

proportion_sig_by_samp_tech

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/proportion_sig_by_samp_tech ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
proportion_sig_by_samp_tech
dev.off()


# create dataframe with average number of phase changes by each data range
ave_phase_changes_by_start_year <- tick_dataset_results %>% group_by(start_year) %>% summarise(ave_num_phases = mean(number_phases))

# create boxplot for average number phase changes by data range
ave_num_phases_by_start_year <- ggplot(ave_phase_changes_by_start_year, aes(x = start_year, y = ave_num_phases)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(name = "Start year of dataset") +
  scale_y_continuous(name = "Average # of phase changes", expand = c(0,0)) +
  ggtitle("Average # of phase changes by start year") + 
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

ave_num_phases_by_start_year

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/ave_num_phases_by_start_year ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
ave_num_phases_by_start_year
dev.off()

# create line plot for years to reach stability for each range of years for all life stages
pal<-brewer.pal(4, "Set2")
tick_dataset_results_sub_lf <- subset(tick_dataset_results, life_stage == "adult" | life_stage == "nymph" | life_stage == "larvae")
years_to_reach_stability_ls <- ggplot(tick_dataset_results_sub_lf, aes(x=stability_time, color=life_stage)) +
  scale_color_manual(values = pal)+
  geom_line(stat = "count", size=2) +
  scale_y_continuous(name = "Number of datasets", expand = c(0,0)) +
  xlab("Years to reach stability") + 
  xlim(low=0, high=14)+
  labs(color="Life Stage") +
  ggtitle("Years to reach stability for all datasets for \neach life stage") +
  theme(axis.line.x = element_line(size = 1, colour = "black"),
        axis.line.y = element_line(size = 1, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title=element_text(size = 27, margin=margin(0,0,15,0)),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 17),
        legend.position = c(0.9, 0.9),
        axis.text.x=element_text(colour="black", size = 18),
        axis.text.y=element_text(colour="black", size = 18),
        axis.title.x = element_text(size = 23, margin=margin(15,0,0,0)),
        axis.title.y = element_text(size = 23, margin=margin(0,15,0,0)))

years_to_reach_stability_ls

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/years_to_reach_stability_for_each_ls ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
years_to_reach_stability_ls
dev.off()


plot(tick_dataset_results$stability_time, tick_dataset_results$geographic_scope)

# run t-test between four different datasets: county, grid, town, and state_forest, for stability_time
# assumes that the variances of y1 and y2 are unequal
county_gs <- tick_dataset_results[tick_dataset_results$geographic_scope == "County",]
grid_gs <- tick_dataset_results[tick_dataset_results$geographic_scope == "Grid",]
state_forest_gs <- tick_dataset_results[tick_dataset_results$geographic_scope == "State forest",]
town_gs <- tick_dataset_results[tick_dataset_results$geographic_scope == "Town",]

#county vs grid
t.test(county_gs$stability_time, grid_gs$stability_time)
# t = -17.476, df = 33.014, p-value < 2.2e-16
# t-value indicates probability below 0.5
# low probability of difference between datasets
#
# p-value lower than 0.05, does not support null hypothesis
# therefore true difference in means is equal to 0 is not supported
# and is statiscally significant

#county vs town
t.test(county_gs$stability_time, town_gs$stability_time)
# t = -0.54916, df = 79.244, p-value = 0.5844
# t-value indicates probability below 0.5
# low probability of difference between datasets
#
# p-value greater than 0.05, does support null hypothesis
# therefore true difference in means is equal to 0 is supported
# and is not statiscally significant

#county vs state forest
t.test(county_gs$stability_time, state_forest_gs$stability_time)
# t = -5.6035, df = 5.2081, p-value = 0.002194
# t-value indicates probability below 0.5
# low probability of difference between datasets
#
# p-value lower than 0.05, does support not null hypothesis
# therefore true difference in means is equal to 0 is not supported
# and is statiscally significant

#grid vs state forest
t.test(grid_gs$stability_time, state_forest_gs$stability_time)
# t = 0.37552, df = 6.0307, p-value = 0.7201
# t-value indicates probability below 0.5
# low probability of difference between datasets
#
# p-value greater than 0.05, does support null hypothesis
# therefore true difference in means is equal to 0 is supported
# and is not statiscally significant

#grid vs town
t.test(grid_gs$stability_time, town_gs$stability_time)
# t = 16.259, df = 38.381, p-value < 2.2e-16
# t-value indicates probability below 0.5
# low probability of difference between datasets
#
# p-value lower than 0.05, does support not null hypothesis
# therefore true difference in means is equal to 0 is not supported
# and is statiscally significant

#state forest vs town
t.test(state_forest_gs$stability_time, town_gs$stability_time)
# t = 5.4429, df = 5.3393, p-value = 0.002317
# t-value indicates probability greater than 0.005
# very high probability of difference between datasets
#
# p-value lower than 0.05, does support not null hypothesis
# therefore true difference in means is equal to 0 is not supported
# and is statiscally significant

# create line plot for years to reach stability for all datasets for each geographic scope
pal<-brewer.pal(4, "Set2")
years_to_reach_stability_gs <- ggplot(tick_dataset_results, aes(x=stability_time, color=geographic_scope)) +
  scale_color_manual(values = pal)+
  geom_line(stat = "count", size=2) +
  scale_y_continuous(name = "Number of datasets", expand = c(0,0)) +
  xlab("Years to reach stability") + 
  xlim(low=0, high=14)+
  labs(color="Geographic scope") +
  ggtitle("Years to reach stability for all datasets for \neach geographic scope") +
  theme(axis.line.x = element_line(size = 1, colour = "black"),
        axis.line.y = element_line(size = 1, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title=element_text(size = 27, margin=margin(0,0,15,0)),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 17),
        legend.position = c(0.9, 0.9),
        axis.text.x=element_text(colour="black", size = 18),
        axis.text.y=element_text(colour="black", size = 18),
        axis.title.x = element_text(size = 23, margin=margin(15,0,0,0)),
        axis.title.y = element_text(size = 23, margin=margin(0,15,0,0)))

years_to_reach_stability_gs

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/years_to_reach_stability_for_each_gs ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
years_to_reach_stability_gs
dev.off()


# create line plot for years to reach stability for each range of years for all life stages
pal<-brewer.pal(4, "Set2")
years_to_reach_stability_gs <- ggplot(tick_dataset_results, aes(x=stability_time, color=geographic_scope)) +
  scale_color_manual(values = pal)+
  geom_line(stat = "count", size=2) +
  scale_y_continuous(name = "Number of datasets", expand = c(0,0)) +
  xlab("Years to reach stability") + 
  xlim(low=0, high=14)+
  labs(color="Geographic scope") +
  ggtitle("Years to reach stability for all datasets for \neach geographic scope") +
  theme(axis.line.x = element_line(size = 1, colour = "black"),
        axis.line.y = element_line(size = 1, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title=element_text(size = 27, margin=margin(0,0,15,0)),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 17),
        legend.position = c(0.9, 0.9),
        axis.text.x=element_text(colour="black", size = 18),
        axis.text.y=element_text(colour="black", size = 18),
        axis.title.x = element_text(size = 23, margin=margin(15,0,0,0)),
        axis.title.y = element_text(size = 23, margin=margin(0,15,0,0)))

years_to_reach_stability_gs

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/years_to_reach_stability_for_each_gs ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
years_to_reach_stability_gs
dev.off()



ggplot(tick_dataset_results, aes(x=state, y=stability_time)) + geom_point()
# ny seemed to take the longest time to reach stability

ggplot(tick_dataset_results, aes(x=state, y=proportion_wrong)) + geom_boxplot() + geom_point() 
# ny seemed to have the greatest proportion significance
nj <- tick_dataset_results[tick_dataset_results$state == "NJ",]
ny <- tick_dataset_results[tick_dataset_results$state == "NY",]
ma <- tick_dataset_results[tick_dataset_results$state == "MA",]

t.test(nj$proportion_wrong, ny$proportion_wrong)
# t = -1.869, df = 97.947, p-value = 0.06461
# t-value indicates probability below 0.5
# very low probability of difference between datasets
#
# not a statistically significant difference between states NY and NJ

t.test(ny$proportion_wrong, ma$proportion_wrong)
# t = 3.8321, df = 89, p-value = 0.0002364
# t-value indicates probability above 0.995
# very high probability of difference between datasets
#
# p-value less than 0.05, reject null hypothesis
# therefore alternative hypothesis: true difference in means is not equal to 0 is supported
# statiscally significant

t.test(nj$proportion_wrong, ma$proportion_wrong)
# t = 0.85054, df = 40, p-value = 0.4001
# t-value indicates probability between 0.20 and 0.25
# low probability of difference between datasets
#
# p-value above than 0.05, accept null hypothesis
# therefore not statistically significant

#create boxplot of state vs proportion wrong
proportion_sig_w_by_state <- ggplot(tick_dataset_results, aes(x = state, y = proportion_wrong)) +
  geom_boxplot() + 
  scale_x_discrete(name = "State") +
  scale_y_continuous(name = "Proportion Significantly Wrong") +
  ggtitle("Proportion significantly wrong for each \nstate") + 
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

proportion_sig_w_by_state

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/proportion_sig_w_by_state ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
proportion_sig_w_by_state
dev.off()


ggplot(tick_dataset_results, aes(x=data_range, y=number_phases)) + geom_point()
# larger data ranges seemed to have a larger number of phases

# run t-test between two different datasets: data_range and number_phases
# assumes that the variances of y1 and y2 are unequal
t.test(tick_dataset_results$data_range, tick_dataset_results$number_phases)
# t = 24.071, df = 142.62, p-value < 2.2e-16
# t-value indicates probability above 0.9995
# very high probability of difference between datasets
#
# p-value less than 0.05, reject null hypothesis
# therefore alternative hypothesis: true difference in means is not equal to 0 is supported
# statiscally significant


ggplot(tick_dataset_results, aes(x=geographic_scope, y=proportion_significant)) + geom_point()
#proportion significant seems to be smaller with smaller geographic scope

ggplot(tick_dataset_results, aes(x=geographic_scope, y=stability_time)) + geom_point()
#stability time seems to increase with smaller geographic scope


ggplot(tick_dataset_results, aes(x=start_year, y=number_phases)) + geom_point()
# number of phases seems to decrease with more recent start years

# define recent datasets as older than 2005 and older datasets as older than 1999
recent <- tick_dataset_results[tick_dataset_results$start_year > 2005,]
old <- tick_dataset_results[tick_dataset_results$start_year < 1999,]

t.test(recent$number_phases, old$number_phases)
# t = -11.765, df = 32.4, p-value = 3.088e-13
# t-value indicates probability below 0.5
# very low probability of difference between datasets
#
# p-value less than 0.05, reject null hypothesis
# therefore alternative hypothesis: true difference in means is not equal to 0 is supported
# statiscally significant

num_phases_by_start_year <- ggplot(tick_dataset_results, aes(x = start_year, y = number_phases, group=start_year)) +
  geom_violin(width = 2, size=1) +
  geom_count(color="darkgray") + 
  scale_x_continuous(name = "Start year of dataset") +
  scale_y_continuous(name = "Number of phase changes", expand = c(0,0)) +
  ggtitle("Number of phase changes by start year") + 
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 1, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title=element_text(size = 27, margin=margin(0,0,15,0)),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 17),
        legend.position = c(0.9, 0.9),
        axis.text.x=element_text(colour="black", size = 18),
        axis.text.y=element_text(colour="black", size = 18),
        axis.title.x = element_text(size = 23, margin=margin(15,0,0,0)),
        axis.title.y = element_text(size = 23, margin=margin(0,15,0,0)))

num_phases_by_start_year

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/num_phases_by_start_year ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
num_phases_by_start_year
dev.off()

tick_dataset_results_sub_frac_nym_inf <- subset(tick_dataset_results, life_stage == "fraction of nymphal ticks infected with B. burgdorferi")
tick_dataset_results_sub_frac_adu_inf <- subset(tick_dataset_results, life_stage == "fraction of adult ticks infected with B. burgdorferi")

tick_dataset_results_sub_frac_inf <- subset(tick_dataset_results, life_stage == "fraction of adult ticks infected with B. burgdorferi" | life_stage == "fraction of nymphal ticks infected with B. burgdorferi")


ggplot(tick_dataset_results_sub_frac_inf, aes(x=life_stage, y=stability_time)) + geom_point()
#fraction of nymphal ticks appear reach stability after longer periods of time


ggplot(tick_dataset_results, aes(x=data_range, y=number_phases)) + geom_point()

large <- tick_dataset_results[tick_dataset_results$data_range > 17,]
small <- tick_dataset_results[tick_dataset_results$data_range < 14,]
t.test(small$number_phases, large$number_phases)
# t = 11.749, df = 32.165, p-value = 3.547e-13
# t-value indicates probability above 0.995
# very high probability of difference between datasets
#
# p-value less than 0.05, reject null hypothesis
# therefore alternative hypothesis: true difference in means is not equal to 0 is supported
# statiscally significant

num_phases_by_data_range <- ggplot(tick_dataset_results, aes(x = data_range, y = number_phases, group=data_range)) +
  geom_violin(width = 2, size=1) +
  geom_count(color="darkgray") + 
  scale_x_continuous(name = "Data range (years)") +
  scale_y_continuous(name = "Number of phase changes", expand = c(0,0)) +
  ggtitle("Number of phase changes by data range") + 
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 1, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title=element_text(size = 27, margin=margin(0,0,15,0)),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 17),
        legend.position = c(0.1, 0.9),
        axis.text.x=element_text(colour="black", size = 18),
        axis.text.y=element_text(colour="black", size = 18),
        axis.title.x = element_text(size = 23, margin=margin(15,0,0,0)),
        axis.title.y = element_text(size = 23, margin=margin(0,15,0,0)))

num_phases_by_data_range

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/num_phases_by_data_range ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
num_phases_by_data_range
dev.off()

ggplot(tick_dataset_results, aes(x=stability_time, y=proportion_wrong)) + geom_point()
# seems that proportion wrong is higher with smaller years it took to reach stability

large_stab <- tick_dataset_results[tick_dataset_results$stability_time > 10,]
small_stab <- tick_dataset_results[tick_dataset_results$stability_time < 9,]

t.test(large_stab$proportion_wrong, small_stab$proportion_wrong)
# t = -0.099863, df = 35.402, p-value = 0.921
# t-value indicates probability lower than 0.5
# very low probability of difference between datasets
#
# p-value higher than 0.05, accept null hypothesis
# statiscally insignificant

por_wrong_by_stab_time <- ggplot(tick_dataset_results, aes(stability_time, proportion_wrong)) +
  geom_point() +
  geom_smooth(size=2) +
  scale_x_continuous(name = "Years to stability", expand = c(0,0)) +
  scale_y_continuous(name = "Proportion significantly wrong", expand = c(0,0)) +
  ggtitle("Proportion significantly wrong by years to \nstability") + 
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 1, colour = "black"),
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

por_wrong_by_stab_time

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/por_wrong_by_stab_time ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
por_wrong_by_stab_time
dev.off()


ggplot(tick_dataset_results, aes(x=data_range, y=proportion_wrong)) + geom_point()
# seems that proportion wrong is slightly higher with shorter data ranges

large <- tick_dataset_results[tick_dataset_results$data_range > 17,]
small <- tick_dataset_results[tick_dataset_results$data_range < 14,]

t.test(small$data_range, large$data_range)
# t = -30.643, df = 27.647, p-value < 2.2e-16
# t-value indicates probability below 0.5
# very low probability of difference between datasets
#
# p-value less than 0.05, reject null hypothesis
# therefore alternative hypothesis: true difference in means is not equal to 0 is supported
# statiscally significant

por_wrong_by_data_range <- ggplot(tick_dataset_results, aes(data_range, proportion_wrong)) +
  geom_point() +
  geom_smooth(method=lm, size=2) +
  scale_x_continuous(name = "Data range (years)", expand = c(0,0)) +
  scale_y_continuous(name = "Proportion significantly wrong", expand = c(0,0)) +
  ggtitle("Proportion significantly wrong by total years\n within study") + 
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 1, colour = "black"),
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

por_wrong_by_data_range

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/por_wrong_by_data_range ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
por_wrong_by_data_range
dev.off()



tick_dataset_results_sub_reported_tni <- subset(tick_dataset_results, life_stage != "fraction of adult ticks infected with B. burgdorferi" | life_stage != "fraction of nymphal ticks infected with B. burgdorferi")
tick_dataset_results_sub_ti <- subset(tick_dataset_results, life_stage == "fraction of adult ticks infected with B. burgdorferi" | life_stage == "fraction of nymphal ticks infected with B. burgdorferi")

ggplot(tick_dataset_results, aes(x=all_ticks_vs_infected_ticks, y=proportion_right_for_all_ticks_vs_infected_ticks)) + geom_point()

t.test(tick_dataset_results$proportion_right, tick_dataset_results_sub_ti$proportion_right)
# t = 2.2926, df = 51.901, p-value = 0.02596
# t-value indicates probability between 0.975 and 0.99
# very high probability of difference between datasets
#
# p-value less than 0.05, reject null hypothesis
# therefore alternative hypothesis: true difference in means is not equal to 0 is supported
# statiscally significant

proportion_sig_by_ticks_infected <- ggplot(tick_dataset_results, aes(x = all_ticks_vs_infected_ticks, y = proportion_right_for_all_ticks_vs_infected_ticks)) +
  geom_boxplot() + 
  scale_x_discrete(name = "Ticks") +
  scale_y_continuous(name = "Proportion significantly right") +
  ggtitle("Proportion significantly right by ticks \ninfected") + 
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

proportion_sig_by_ticks_infected

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/proportion_sig_by_ticks_infected ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
proportion_sig_by_ticks_infected
dev.off()



ggplot(tick_dataset_results, aes(x=all_ticks_vs_infected_ticks, y=stability_time_for_all_ticks_vs_infected_ticks)) + geom_point()

#running t-test between total ticks and all infected ticks
t.test(tick_dataset_results$stability_time, tick_dataset_results_sub_ti$stability_time)
# t = 0.53468, df = 58.775, p-value = 0.5949
# t-value indicates probability between 0.50 and 0.75
# somewhat high probability of difference between datasets
#
# p-value less than 0.05, accept null hypothesis
# therefore alternative hypothesis: true difference in means is not equal to 0 is not supported
# statiscally insignificant

stability_time_by_ticks_infected <- ggplot(tick_dataset_results, aes(x = all_ticks_vs_infected_ticks, y = stability_time_for_all_ticks_vs_infected_ticks)) +
  geom_boxplot() + 
  scale_x_discrete(name = "Ticks") +
  scale_y_continuous(name = "Years to stability") +
  ggtitle("Years to stability by ticks infected") + 
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

stability_time_by_ticks_infected

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/stability_time_by_ticks_infected ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
stability_time_by_ticks_infected
dev.off()


fit1=lmer(stability_time ~ data_range + state + life_stage + sampling_technique + geographic_scope + (1|location), data = tick_dataset_results, REML=FALSE)
anova(fit1)
summary(fit1)


#----------------------------------------------------------------------------------------
#
#
#
#
#
#             Analysis of 2/11/2020 tick dataset results
#
#
#
#
#
#
#----------------------------------------------------------------------------------------


# read in tick dataset results from 2/11/2020
tick_dataset_results <- readxl::read_xlsx("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/data/tick_dataset_results_2_11_2020.xlsx", sheet = 1)

# subset data between two different methods - dragging and found on a person
dragging <- tick_dataset_results[tick_dataset_results$sampling_technique == "dragging",]
found_on_people <- tick_dataset_results[tick_dataset_results$sampling_technique == "found on a person",]
# subset deer tick life stages
adult <- tick_dataset_results[tick_dataset_results$life_stage == "adult",]
nymph <- tick_dataset_results[tick_dataset_results$life_stage == "nymph",]
larvae <- tick_dataset_results[tick_dataset_results$life_stage == "larvae",]

t.test(adult$stability_time, nymph$stability_time)
#t = -0.63883, df = 126.84, p-value = 0.5241
# t-value indicates probability below 0.5
# very low probability of difference between datasets
#
# p-value greater than 0.05, accept null hypothesis
# statistically insignificant

t.test(adult$stability_time, larvae$stability_time)
# t = -5.9627, df = 10.111, p-value = 0.0001328
# t-value indicates probability below 0.5
# very low probability of difference between datasets
#
# p-value less than 0.05, reject null hypothesis
# therefore alternative hypothesis: true difference in means is not equal to 0 is supported
# statiscally significant

t.test(nymph$stability_time, larvae$stability_time)
# t = -5.5196, df = 10.54, p-value = 0.0002109
# t-value indicates probability below 0.5
# very low probability of difference between datasets
#
# p-value less than 0.05, reject null hypothesis
# therefore alternative hypothesis: true difference in means is not equal to 0 is supported
# statiscally significant


# create barplot for years to reach stability for each range of years
years_to_reach_stability <- ggplot(tick_dataset_results, aes(x = stability_time)) +
  geom_bar() +
  scale_y_continuous(name = "# of datasets", expand = c(0,0)) +
  xlab("Years to reach stability") + 
  xlim(low=0, high=14)+
  ggtitle("Years to reach stability for all datasets") +
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

years_to_reach_stability

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/years_to_reach_stability ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
years_to_reach_stability
dev.off()


# create lineplot for years to reach stability for each range of years
years_to_reach_stability <- ggplot(tick_dataset_results, aes(x = stability_time)) +
  geom_smooth(stat = "count", size=2) +
  scale_y_continuous(name = "# of datasets", expand = c(0,0)) +
  xlab("Years to reach stability") + 
  xlim(low=0, high=14)+
  ggtitle("Years to reach stability for all datasets") +
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

years_to_reach_stability

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/years_to_reach_stability_line_chart ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
years_to_reach_stability
dev.off()


# run t-test between two different datasets: dragging and found_on_people, for stability time
# assumes that the variances of y1 and y2 are unequal
t.test(dragging$stability_time, found_on_people$stability_time)
# t = -8.5346, df = 236.23, p-value = 1.724e-15
# t-value indicates probability below 0.5
# very high probability of difference between datasets
#
# p-value less than 0.05, reject null hypothesis
# therefore alternative hypothesis: true difference in means is not equal to 0 is supported
# statiscally significant

# create boxplot for years to reach stability between different sampling methods
tick_dataset_results_drag_found <- subset(tick_dataset_results, sampling_technique == "dragging" | sampling_technique == "found on a person")
yrs_stab_by_samp_tech <- ggplot(tick_dataset_results_drag_found, aes(x = sampling_technique, y = stability_time)) +
  geom_boxplot() + 
  scale_x_discrete(name = "Sampling Technique") +
  scale_y_continuous(name = "Years to stability", limits = c(0, 25)) +
  ggtitle("Years to stability for each sampling \ntechnique") + 
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

yrs_stab_by_samp_tech

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/yrs_stab_by_samp_tech ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
yrs_stab_by_samp_tech
dev.off()


# run t-test between two different datasets: dragging and found_on_people, for proportion right
# assumes that the variances of y1 and y2 are unequal
t.test(dragging$proportion_right, found_on_people$proportion_right)
# t = 0.64665, df = 162.9, p-value = 0.5188
# t-value indicates probability below 0.25
# low probability of difference between datasets
#
# p-value above 0.05, supports null hypothesis
# therefore true difference in means is equal to 0 is supported
# and not statiscally significant

# create boxplot for proportion significant between different sampling methods
tick_dataset_results_drag_found <- subset(tick_dataset_results, sampling_technique == "dragging" | sampling_technique == "found on a person")
proportion_sig_by_samp_tech <- ggplot(tick_dataset_results_drag_found, aes(x = sampling_technique, y = proportion_significant)) +
  geom_boxplot() + 
  scale_x_discrete(name = "Sampling Technique") +
  scale_y_continuous(name = "Proportion Significantly Right", limits = c(0,1)) +
  ggtitle("Proportion significantly right for \neach sampling technique") + 
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

proportion_sig_by_samp_tech

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/proportion_sig_by_samp_tech ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
proportion_sig_by_samp_tech
dev.off()


# create dataframe with average number of phase changes by each data range
ave_phase_changes_by_start_year <- tick_dataset_results %>% group_by(start_year) %>% summarise(ave_num_phases = mean(number_phases))

# create boxplot for average number phase changes by data range
ave_num_phases_by_start_year <- ggplot(ave_phase_changes_by_start_year, aes(x = start_year, y = ave_num_phases)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(name = "Start year of dataset") +
  scale_y_continuous(name = "Average # of phase changes", expand = c(0,0)) +
  ggtitle("Average # of phase changes by start year") + 
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

ave_num_phases_by_start_year

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/ave_num_phases_by_start_year ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
ave_num_phases_by_start_year
dev.off()

# create line plot for years to reach stability for each range of years for all life stages
pal<-brewer.pal(4, "Set2")
tick_dataset_results_sub_lf <- subset(tick_dataset_results, life_stage == "adult" | life_stage == "nymph" | life_stage == "larvae")
years_to_reach_stability_ls <- ggplot(tick_dataset_results_sub_lf, aes(x=stability_time, color=life_stage)) +
  scale_color_manual(values = pal)+
  geom_line(stat = "count", size=2) +
  scale_y_continuous(name = "Number of datasets", expand = c(0,0)) +
  xlab("Years to reach stability") + 
  xlim(low=0, high=14)+
  labs(color="Life Stage") +
  ggtitle("Years to reach stability for all datasets for \neach life stage") +
  theme(axis.line.x = element_line(size = 1, colour = "black"),
        axis.line.y = element_line(size = 1, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title=element_text(size = 27, margin=margin(0,0,15,0)),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 17),
        legend.position = c(0.9, 0.9),
        axis.text.x=element_text(colour="black", size = 18),
        axis.text.y=element_text(colour="black", size = 18),
        axis.title.x = element_text(size = 23, margin=margin(15,0,0,0)),
        axis.title.y = element_text(size = 23, margin=margin(0,15,0,0)))

years_to_reach_stability_ls

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/years_to_reach_stability_for_each_ls ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
years_to_reach_stability_ls
dev.off()


plot(tick_dataset_results$stability_time, tick_dataset_results$geographic_scope)

# run t-test between four different datasets: county, grid, town, and state_forest, for stability_time
# assumes that the variances of y1 and y2 are unequal
county_gs <- tick_dataset_results[tick_dataset_results$geographic_scope == "County",]
grid_gs <- tick_dataset_results[tick_dataset_results$geographic_scope == "Grid",]
state_forest_gs <- tick_dataset_results[tick_dataset_results$geographic_scope == "State forest",]
town_gs <- tick_dataset_results[tick_dataset_results$geographic_scope == "Town",]

#county vs grid
t.test(county_gs$stability_time, grid_gs$stability_time)
# t = -17.476, df = 33.014, p-value < 2.2e-16
# t-value indicates probability below 0.5
# low probability of difference between datasets
#
# p-value lower than 0.05, does not support null hypothesis
# therefore true difference in means is equal to 0 is not supported
# and is statiscally significant

#county vs town
t.test(county_gs$stability_time, town_gs$stability_time)
# t = -0.54916, df = 79.244, p-value = 0.5844
# t-value indicates probability below 0.5
# low probability of difference between datasets
#
# p-value greater than 0.05, does support null hypothesis
# therefore true difference in means is equal to 0 is supported
# and is not statiscally significant

#county vs state forest
t.test(county_gs$stability_time, state_forest_gs$stability_time)
# t = -5.6035, df = 5.2081, p-value = 0.002194
# t-value indicates probability below 0.5
# low probability of difference between datasets
#
# p-value lower than 0.05, does support not null hypothesis
# therefore true difference in means is equal to 0 is not supported
# and is statiscally significant

#grid vs state forest
t.test(grid_gs$stability_time, state_forest_gs$stability_time)
# t = 0.37552, df = 6.0307, p-value = 0.7201
# t-value indicates probability below 0.5
# low probability of difference between datasets
#
# p-value greater than 0.05, does support null hypothesis
# therefore true difference in means is equal to 0 is supported
# and is not statiscally significant

#grid vs town
t.test(grid_gs$stability_time, town_gs$stability_time)
# t = 16.259, df = 38.381, p-value < 2.2e-16
# t-value indicates probability below 0.5
# low probability of difference between datasets
#
# p-value lower than 0.05, does support not null hypothesis
# therefore true difference in means is equal to 0 is not supported
# and is statiscally significant

#state forest vs town
t.test(state_forest_gs$stability_time, town_gs$stability_time)
# t = 5.4429, df = 5.3393, p-value = 0.002317
# t-value indicates probability greater than 0.005
# very high probability of difference between datasets
#
# p-value lower than 0.05, does support not null hypothesis
# therefore true difference in means is equal to 0 is not supported
# and is statiscally significant

# create line plot for years to reach stability for all datasets for each geographic scope
pal<-brewer.pal(4, "Set2")
years_to_reach_stability_gs <- ggplot(tick_dataset_results, aes(x=stability_time, color=geographic_scope)) +
  scale_color_manual(values = pal)+
  geom_line(stat = "count", size=2) +
  scale_y_continuous(name = "Number of datasets", expand = c(0,0)) +
  xlab("Years to reach stability") + 
  xlim(low=0, high=14)+
  labs(color="Geographic scope") +
  ggtitle("Years to reach stability for all datasets for \neach geographic scope") +
  theme(axis.line.x = element_line(size = 1, colour = "black"),
        axis.line.y = element_line(size = 1, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title=element_text(size = 27, margin=margin(0,0,15,0)),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 17),
        legend.position = c(0.9, 0.9),
        axis.text.x=element_text(colour="black", size = 18),
        axis.text.y=element_text(colour="black", size = 18),
        axis.title.x = element_text(size = 23, margin=margin(15,0,0,0)),
        axis.title.y = element_text(size = 23, margin=margin(0,15,0,0)))

years_to_reach_stability_gs

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/years_to_reach_stability_for_each_gs ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
years_to_reach_stability_gs
dev.off()


# create line plot for years to reach stability for each range of years for all life stages
pal<-brewer.pal(4, "Set2")
years_to_reach_stability_gs <- ggplot(tick_dataset_results, aes(x=stability_time, color=geographic_scope)) +
  scale_color_manual(values = pal)+
  geom_line(stat = "count", size=2) +
  scale_y_continuous(name = "Number of datasets", expand = c(0,0)) +
  xlab("Years to reach stability") + 
  xlim(low=0, high=14)+
  labs(color="Geographic scope") +
  ggtitle("Years to reach stability for all datasets for \neach geographic scope") +
  theme(axis.line.x = element_line(size = 1, colour = "black"),
        axis.line.y = element_line(size = 1, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title=element_text(size = 27, margin=margin(0,0,15,0)),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 17),
        legend.position = c(0.9, 0.9),
        axis.text.x=element_text(colour="black", size = 18),
        axis.text.y=element_text(colour="black", size = 18),
        axis.title.x = element_text(size = 23, margin=margin(15,0,0,0)),
        axis.title.y = element_text(size = 23, margin=margin(0,15,0,0)))

years_to_reach_stability_gs

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/years_to_reach_stability_for_each_gs ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
years_to_reach_stability_gs
dev.off()



ggplot(tick_dataset_results, aes(x=state, y=stability_time)) + geom_point()
# ny seemed to take the longest time to reach stability

ggplot(tick_dataset_results, aes(x=state, y=proportion_wrong)) + geom_boxplot() + geom_point() 
# ny seemed to have the greatest proportion significance
nj <- tick_dataset_results[tick_dataset_results$state == "NJ",]
ny <- tick_dataset_results[tick_dataset_results$state == "NY",]
ma <- tick_dataset_results[tick_dataset_results$state == "MA",]

t.test(nj$proportion_wrong, ny$proportion_wrong)
# t = -1.869, df = 97.947, p-value = 0.06461
# t-value indicates probability below 0.5
# very low probability of difference between datasets
#
# not a statistically significant difference between states NY and NJ

t.test(ny$proportion_wrong, ma$proportion_wrong)
# t = 3.8321, df = 89, p-value = 0.0002364
# t-value indicates probability above 0.995
# very high probability of difference between datasets
#
# p-value less than 0.05, reject null hypothesis
# therefore alternative hypothesis: true difference in means is not equal to 0 is supported
# statiscally significant

t.test(nj$proportion_wrong, ma$proportion_wrong)
# t = 0.85054, df = 40, p-value = 0.4001
# t-value indicates probability between 0.20 and 0.25
# low probability of difference between datasets
#
# p-value above than 0.05, accept null hypothesis
# therefore not statistically significant

#create boxplot of state vs proportion wrong
proportion_sig_w_by_state <- ggplot(tick_dataset_results, aes(x = state, y = proportion_wrong)) +
  geom_boxplot() + 
  scale_x_discrete(name = "State") +
  scale_y_continuous(name = "Proportion Significantly Wrong") +
  ggtitle("Proportion significantly wrong for each \nstate") + 
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

proportion_sig_w_by_state

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/proportion_sig_w_by_state ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
proportion_sig_w_by_state
dev.off()


ggplot(tick_dataset_results, aes(x=data_range, y=number_phases)) + geom_point()
# larger data ranges seemed to have a larger number of phases

# run t-test between two different datasets: data_range and number_phases
# assumes that the variances of y1 and y2 are unequal
t.test(tick_dataset_results$data_range, tick_dataset_results$number_phases)
# t = 24.071, df = 142.62, p-value < 2.2e-16
# t-value indicates probability above 0.9995
# very high probability of difference between datasets
#
# p-value less than 0.05, reject null hypothesis
# therefore alternative hypothesis: true difference in means is not equal to 0 is supported
# statiscally significant


ggplot(tick_dataset_results, aes(x=geographic_scope, y=proportion_significant)) + geom_point()
#proportion significant seems to be smaller with smaller geographic scope

ggplot(tick_dataset_results, aes(x=geographic_scope, y=stability_time)) + geom_point()
#stability time seems to increase with smaller geographic scope


ggplot(tick_dataset_results, aes(x=start_year, y=number_phases)) + geom_point()
# number of phases seems to decrease with more recent start years

# define recent datasets as older than 2005 and older datasets as older than 1999
recent <- tick_dataset_results[tick_dataset_results$start_year > 2005,]
old <- tick_dataset_results[tick_dataset_results$start_year < 1999,]

t.test(recent$number_phases, old$number_phases)
# t = -11.765, df = 32.4, p-value = 3.088e-13
# t-value indicates probability below 0.5
# very low probability of difference between datasets
#
# p-value less than 0.05, reject null hypothesis
# therefore alternative hypothesis: true difference in means is not equal to 0 is supported
# statiscally significant

num_phases_by_start_year <- ggplot(tick_dataset_results, aes(x = start_year, y = number_phases, group=start_year)) +
  geom_violin(width = 2, size=1) +
  geom_count(color="darkgray") + 
  scale_x_continuous(name = "Start year of dataset") +
  scale_y_continuous(name = "Number of phase changes", expand = c(0,0)) +
  ggtitle("Number of phase changes by start year") + 
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 1, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title=element_text(size = 27, margin=margin(0,0,15,0)),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 17),
        legend.position = c(0.9, 0.9),
        axis.text.x=element_text(colour="black", size = 18),
        axis.text.y=element_text(colour="black", size = 18),
        axis.title.x = element_text(size = 23, margin=margin(15,0,0,0)),
        axis.title.y = element_text(size = 23, margin=margin(0,15,0,0)))

num_phases_by_start_year

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/num_phases_by_start_year ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
num_phases_by_start_year
dev.off()

tick_dataset_results_sub_frac_nym_inf <- subset(tick_dataset_results, life_stage == "fraction of nymphal ticks infected with B. burgdorferi")
tick_dataset_results_sub_frac_adu_inf <- subset(tick_dataset_results, life_stage == "fraction of adult ticks infected with B. burgdorferi")

tick_dataset_results_sub_frac_inf <- subset(tick_dataset_results, life_stage == "fraction of adult ticks infected with B. burgdorferi" | life_stage == "fraction of nymphal ticks infected with B. burgdorferi")


ggplot(tick_dataset_results_sub_frac_inf, aes(x=life_stage, y=stability_time)) + geom_point()
#fraction of nymphal ticks appear reach stability after longer periods of time


ggplot(tick_dataset_results, aes(x=data_range, y=number_phases)) + geom_point()

large <- tick_dataset_results[tick_dataset_results$data_range > 17,]
small <- tick_dataset_results[tick_dataset_results$data_range < 14,]
t.test(small$number_phases, large$number_phases)
# t = 1.7325, df = 262.61, p-value = 0.08435
# t-value indicates probability above 0.90
# high probability of difference between datasets
#
# p-value higher than 0.05, accept null hypothesis
# therefore alternative hypothesis: true difference in means is not equal to 0 is not supported
# statiscally insignificant

num_phases_by_data_range <- ggplot(tick_dataset_results, aes(x = data_range, y = number_phases, group=data_range)) +
  geom_violin(width = 2, size=1) +
  geom_count(color="darkgray") + 
  scale_x_continuous(name = "Data range (years)") +
  scale_y_continuous(name = "Number of phase changes", expand = c(0,0)) +
  ggtitle("Number of phase changes by data range") + 
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 1, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title=element_text(size = 27, margin=margin(0,0,15,0)),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 17),
        legend.position = c(0.1, 0.9),
        axis.text.x=element_text(colour="black", size = 18),
        axis.text.y=element_text(colour="black", size = 18),
        axis.title.x = element_text(size = 23, margin=margin(15,0,0,0)),
        axis.title.y = element_text(size = 23, margin=margin(0,15,0,0)))

num_phases_by_data_range

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/num_phases_by_data_range ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
num_phases_by_data_range
dev.off()

ggplot(tick_dataset_results, aes(x=stability_time, y=proportion_wrong)) + geom_point()
# seems that proportion wrong is higher with smaller years it took to reach stability

large_stab <- tick_dataset_results[tick_dataset_results$stability_time > 10,]
small_stab <- tick_dataset_results[tick_dataset_results$stability_time < 9,]

t.test(large_stab$proportion_wrong, small_stab$proportion_wrong)
# t = -0.099863, df = 35.402, p-value = 0.921
# t-value indicates probability lower than 0.5
# very low probability of difference between datasets
#
# p-value higher than 0.05, accept null hypothesis
# statiscally insignificant

cor(na.omit(tick_dataset_results$stability_time), na.omit(tick_dataset_results$proportion_wrong))
#0.2266395
#little correlation between stability time and proportion right

por_wrong_by_stab_time <- ggplot(tick_dataset_results, aes(stability_time, proportion_wrong)) +
  geom_point() +
  geom_smooth(size=2) +
  scale_x_continuous(name = "Years to stability", expand = c(0,0)) +
  scale_y_continuous(name = "Proportion significantly wrong", expand = c(0,0)) +
  ggtitle("Proportion significantly wrong by years to \nstability") + 
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 1, colour = "black"),
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

por_wrong_by_stab_time

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/por_wrong_by_stab_time ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
por_wrong_by_stab_time
dev.off()


ggplot(tick_dataset_results, aes(x=data_range, y=proportion_wrong)) + geom_point()
# seems that proportion wrong is slightly higher with shorter data ranges

large <- tick_dataset_results[tick_dataset_results$data_range > 17,]
small <- tick_dataset_results[tick_dataset_results$data_range < 14,]

t.test(small$data_range, large$data_range)
# t = -105.05, df = 283.1, p-value < 2.2e-16
# t-value indicates probability below 0.5
# very low probability of difference between datasets
#
# p-value less than 0.05, reject null hypothesis
# therefore alternative hypothesis: true difference in means is not equal to 0 is supported
# statiscally significant

por_wrong_by_data_range <- ggplot(tick_dataset_results, aes(data_range, proportion_wrong)) +
  geom_point() +
  geom_smooth(method=lm, size=2) +
  scale_x_continuous(name = "Data range (years)", expand = c(0,0)) +
  scale_y_continuous(name = "Proportion significantly wrong", expand = c(0,0)) +
  ggtitle("Proportion significantly wrong by total years\n within study") + 
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 1, colour = "black"),
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

por_wrong_by_data_range

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/por_wrong_by_data_range ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
por_wrong_by_data_range
dev.off()


tested_for_path <- tick_dataset_results[tick_dataset_results$tested_for_b_burgdoferi == "Y",] 
tested_for_path$label <- "Ticks infected with B. burgdorferi"

all_tick_dataset_results <- tick_dataset_results
all_tick_dataset_results$label <- "All ticks"

all_ticks_vs_infected_ticks <- c(tested_for_path$label, all_tick_dataset_results$label)
proportion_right_for_all_ticks_vs_infected_ticks <- c(tested_for_path$proportion_right, all_tick_dataset_results$proportion_right)

tick_infection_data <- data.frame(all_ticks_vs_infected_ticks, proportion_right_for_all_ticks_vs_infected_ticks)


ggplot(tick_infection_data, aes(x = tick_infection_data$all_ticks_vs_infected_ticks, y = tick_infection_data$proportion_right_for_all_ticks_vs_infected_ticks)) + geom_point()

t.test(tick_infection_data[tick_infection_data$all_ticks_vs_infected_ticks == "Ticks infected with B. burgdorferi",]$proportion_right_for_all_ticks_vs_infected_ticks, tick_infection_data[tick_infection_data$all_ticks_vs_infected_ticks == "All ticks",]$proportion_right_for_all_ticks_vs_infected_ticks)
# t = 0.52632, df = 198.69, p-value = 0.5993
# t-value indicates probability below 0.5
# low probability of difference between datasets
#
# p-value higher than 0.05, accept null hypothesis
# therefore alternative hypothesis: true difference in means is not equal to 0 is not supported
# statiscally insignificant

#proportion significant by ticks infected and total ticks
proportion_sig_by_ticks_infected <- ggplot(tick_infection_data, aes(x = all_ticks_vs_infected_ticks, y = proportion_right_for_all_ticks_vs_infected_ticks)) +
  geom_boxplot() + 
  scale_x_discrete(name = "Ticks") +
  scale_y_continuous(name = "Proportion significantly right") +
  ggtitle("Proportion significantly right by ticks \ninfected") + 
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

proportion_sig_by_ticks_infected

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/proportion_sig_by_ticks_infected ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
proportion_sig_by_ticks_infected
dev.off()


tested_for_path <- tick_dataset_results[tick_dataset_results$tested_for_b_burgdoferi == "Y",] 
tested_for_path$label <- "Ticks infected with B. burgdorferi"

all_tick_dataset_results <- tick_dataset_results
all_tick_dataset_results$label <- "All ticks"

all_ticks_vs_infected_ticks <- c(tested_for_path$label, all_tick_dataset_results$label)
stability_time_for_all_ticks_vs_infected_ticks <- c(tested_for_path$stability_time, all_tick_dataset_results$stability_time)

tick_infection_data <- data.frame(all_ticks_vs_infected_ticks, stability_time_for_all_ticks_vs_infected_ticks)

ggplot(tick_infection_data, aes(x=all_ticks_vs_infected_ticks, y=stability_time_for_all_ticks_vs_infected_ticks)) + geom_point()

#running t-test between total ticks and all infected ticks
t.test(tick_infection_data[tick_infection_data$all_ticks_vs_infected_ticks == "Ticks infected with B. burgdorferi",]$stability_time_for_all_ticks_vs_infected_ticks, tick_infection_data[tick_infection_data$all_ticks_vs_infected_ticks == "All ticks",]$stability_time_for_all_ticks_vs_infected_ticks)
#t = -0.9203, df = 256.55, p-value = 0.3583
# t-value indicates probability between 0.50 and 0.75
# somewhat high probability of difference between datasets
#
# p-value less than 0.05, accept null hypothesis
# therefore alternative hypothesis: true difference in means is not equal to 0 is not supported
# statiscally insignificant

stability_time_by_ticks_infected <- ggplot(tick_infection_data, aes(x = all_ticks_vs_infected_ticks, y = stability_time_for_all_ticks_vs_infected_ticks)) +
  geom_boxplot() + 
  scale_x_discrete(name = "Ticks") +
  scale_y_continuous(name = "Years to stability") +
  ggtitle("Years to stability by ticks infected") + 
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

stability_time_by_ticks_infected

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/stability_time_by_ticks_infected ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
stability_time_by_ticks_infected
dev.off()

############################

# Culmative line plots 4/2/2020

############################

# read in tick dataset results from 3/25/2020
tick_dataset_results <- readxl::read_xlsx("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/data/tick dataset results_3_14_2020.xlsx", sheet = 1)

###########
#stability time
############

#need to create new column indicating # of datasets with stability time greater than a certain value
tick_dataset_results$stability_time_culamative <- 0

#calculating number of datasets with stability time higher than years
for(years in tick_dataset_results$stability_time) {
  tick_dataset_results[tick_dataset_results$stability_time == years,]$stability_time_culamative <- length(tick_dataset_results[tick_dataset_results$stability_time>years,]$stability_time)
}

# create lineplot for years to reach stability for each range of years culamative
years_to_reach_stability <- ggplot(tick_dataset_results, aes(x = stability_time_culamative, y = stability_time)) +
  geom_line(color="skyblue", size=2)+
  scale_y_continuous(name = "Years to reach stability", expand = c(0,0)) +
  xlab("Number of datasets") + 
  ggtitle("Years to reach stability for all datasets") +
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
        axis.title.y = element_text(size = 23, margin=margin(0,15,0,0)),
        plot.margin = margin(0, 20, 0, 0))

years_to_reach_stability

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/years_to_reach_stability_line_chart ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
years_to_reach_stability
dev.off()

################
#life stage with number of datasets
################

tick_dataset_results_sub_lf <- subset(tick_dataset_results, life_stage == "adult" | life_stage == "nymph" | life_stage == "larvae")
tick_dataset_results_sub_lf$life_stage <- factor(tick_dataset_results_sub_lf$life_stage, levels = c("adult", "nymph", "larvae"))

# create line plot for years to reach stability for each range of years for all life stages
pal<-brewer.pal(4, "Set2")
years_to_reach_stability_ls <- ggplot(tick_dataset_results_sub_lf, aes(x=stability_time_culamative, y=stability_time, color=life_stage)) +
  scale_color_manual(values = pal)+
  geom_line(size=2) +
  xlim(0, 300) +
  scale_y_continuous(name = "Years to reach stability", expand = c(0,0)) +
  xlab("Number of datasets") + 
  labs(color="Life Stage") +
  ggtitle("Years to reach stability for all datasets for \neach life stage") +
  theme(axis.line.x = element_line(size = 1, colour = "black"),
        axis.line.y = element_line(size = 1, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title=element_text(size = 27, margin=margin(0,0,15,0)),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 17),
        legend.position = c(0.9, 0.9),
        axis.text.x=element_text(colour="black", size = 18),
        axis.text.y=element_text(colour="black", size = 18),
        axis.title.x = element_text(size = 23, margin=margin(15,0,0,0)),
        axis.title.y = element_text(size = 23, margin=margin(0,15,0,0)),
        plot.margin = margin(0, 20, 0, 0))

years_to_reach_stability_ls

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/years_to_reach_stability_for_each_ls ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
years_to_reach_stability_ls
dev.off()

################
#life stage with proportion of datasets
################

tick_dataset_results$stability_time_culamative_proportion <- tick_dataset_results$stability_time_culamative/max(tick_dataset_results$stability_time_culamative)

#sub dataset to include only data with adult, nymph, and larvae data
tick_dataset_results_sub_lf <- subset(tick_dataset_results, life_stage == "adult" | life_stage == "nymph" | life_stage == "larvae")

#order factors by adult, nymph, larvae
tick_dataset_results_sub_lf$life_stage <- factor(tick_dataset_results_sub_lf$life_stage, levels = c("adult", "nymph", "larvae"))

# create line plot for years to reach stability for each range of years for all life stages
pal<-brewer.pal(4, "Set2")
years_to_reach_stability_ls_pro <- ggplot(tick_dataset_results_sub_lf, aes(x=stability_time_culamative_proportion, y=stability_time, color=life_stage)) +
  scale_color_manual(values = pal)+
  geom_line(size=2) +
  scale_y_continuous(name = "Years to reach stability", expand = c(0,0)) +
  xlab("Proportion of datasets") + 
  labs(color="Life Stage") +
  ggtitle("Years to reach stability for all datasets for \neach life stage") +
  theme(axis.line.x = element_line(size = 1, colour = "black"),
        axis.line.y = element_line(size = 1, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title=element_text(size = 27, margin=margin(0,0,15,0)),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 17),
        legend.position = c(0.9, 0.9),
        axis.text.x=element_text(colour="black", size = 18),
        axis.text.y=element_text(colour="black", size = 18),
        axis.title.x = element_text(size = 23, margin=margin(15,0,0,0)),
        axis.title.y = element_text(size = 23, margin=margin(0,15,0,0)))

years_to_reach_stability_ls_pro

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/years_to_reach_stability_for_each_ls_pro_datasets ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
years_to_reach_stability_ls_pro
dev.off()


################
#life stage with proportion of datasets and tri-paneled
################

tick_dataset_results$stability_time_culamative_proportion <- tick_dataset_results$stability_time_culamative/max(tick_dataset_results$stability_time_culamative)

#sub dataset to include only data with adult, nymph, and larvae data
tick_dataset_results_sub_lf <- subset(tick_dataset_results, life_stage == "adult" | life_stage == "nymph" | life_stage == "larvae")

#order factors by adult, nymph, larvae
tick_dataset_results_sub_lf$life_stage <- factor(tick_dataset_results_sub_lf$life_stage, levels = c("adult", "nymph", "larvae"))

# create line plot for years to reach stability for proportion of datasets for all life stages in tri-paneled form
pal<-brewer.pal(4, "Set2")
years_to_reach_stability_ls_pro_tri <- ggplot(tick_dataset_results_sub_lf, aes(x=stability_time_culamative_proportion, y=stability_time, color=life_stage)) +
  scale_color_manual(values = pal)+
  geom_line(size=2) +
  facet_grid(cols = vars(life_stage))+
  scale_y_continuous(name = "Years to reach stability", expand = c(0,0)) +
  xlab("Proportion of datasets") + 
  labs(color="") +
  ggtitle("Years to stability for all datasets for each life stage") +
  theme(panel.spacing = unit(2, "lines"),
        axis.line.x = element_line(size = 1, colour = "black"),
        axis.line.y = element_line(size = 1, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title=element_text(size = 27, margin=margin(0,0,15,0)),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 17),
        legend.position = "top",
        legend.direction = "horizontal",
        axis.text.x=element_text(colour="black", size = 18),
        axis.text.y=element_text(colour="black", size = 18),
        axis.title.x = element_text(size = 23, margin=margin(15,0,0,0)),
        axis.title.y = element_text(size = 23, margin=margin(0,15,0,0)))

years_to_reach_stability_ls_pro_tri

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/years_to_reach_stability_for_each_ls_pro_datasets_tri ",Sys.Date(),".png", sep = ''), width = 1000, height = 454)
years_to_reach_stability_ls_pro_tri
dev.off()

################
#geographic scope with number of datasets
################

#order factors by adult, nymph, larvae
tick_dataset_results$geographic_scope <- factor(tick_dataset_results$geographic_scope, levels = c("County", "Town", "State forest", "Grid"))

# create line plot for years to reach stability for number datasets for each geographic scope
pal<-brewer.pal(4, "Set2")
years_to_reach_stability_gs <- ggplot(tick_dataset_results, aes(x=stability_time_culamative, y=stability_time, color=geographic_scope)) +
  scale_color_manual(values = pal)+
  geom_line(size=2) +
  scale_y_continuous(name = "Years to reach stability", expand = c(0,0)) +
  xlab("Number of datasets") + 
  labs(color="Geographic scope") +
  ggtitle("Years to reach stability for all datasets for \neach geographic scope") +
  theme(axis.line.x = element_line(size = 1, colour = "black"),
        axis.line.y = element_line(size = 1, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title=element_text(size = 27, margin=margin(0,0,15,0)),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 17),
        legend.position = c(0.9, 0.9),
        axis.text.x=element_text(colour="black", size = 18),
        axis.text.y=element_text(colour="black", size = 18),
        axis.title.x = element_text(size = 23, margin=margin(15,0,0,0)),
        axis.title.y = element_text(size = 23, margin=margin(0,15,0,0)),
        plot.margin = margin(0, 20, 0, 0))

years_to_reach_stability_gs

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/years_to_reach_stability_for_each_gs ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
years_to_reach_stability_gs
dev.off()

################
#geographic scope with proportion of datasets
################

# create line plot for years to reach stability for proportion of datasets for each geographic scope
pal<-brewer.pal(4, "Set2")
years_to_reach_stability_gs_pro <- ggplot(tick_dataset_results, aes(x=stability_time_culamative_proportion, y=stability_time, color=geographic_scope)) +
  scale_color_manual(values = pal)+
  geom_line(size=2) +
  scale_y_continuous(name = "Years to reach stability", expand = c(0,0)) +
  xlab("Proportion of datasets") + 
  labs(color="Geographic scope") +
  ggtitle("Years to reach stability for all datasets for \neach geographic scope") +
  theme(axis.line.x = element_line(size = 1, colour = "black"),
        axis.line.y = element_line(size = 1, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title=element_text(size = 27, margin=margin(0,0,15,0)),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 17),
        legend.position = c(0.9, 0.9),
        axis.text.x=element_text(colour="black", size = 18),
        axis.text.y=element_text(colour="black", size = 18),
        axis.title.x = element_text(size = 23, margin=margin(15,0,0,0)),
        axis.title.y = element_text(size = 23, margin=margin(0,15,0,0)))

years_to_reach_stability_gs_pro

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/years_to_reach_stability_for_each_gs_pro ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
years_to_reach_stability_gs_pro
dev.off()

################
#geographic scope with proportion of datasets and tri-paneled
################

# create line plot for years to reach stability for proportion of datasets for each geographic scope
pal<-brewer.pal(4, "Set2")
years_to_reach_stability_gs_pro_tri <- ggplot(tick_dataset_results, aes(x=stability_time_culamative_proportion, y=stability_time, color=geographic_scope)) +
  scale_color_manual(values = pal)+
  geom_line(size=2) +
  facet_grid(cols = vars(geographic_scope))+
  scale_y_continuous(name = "Years to reach stability", expand = c(0,0)) +
  xlab("Proportion of datasets") + 
  labs(color="") +
  ggtitle("Years to reach stability for all datasets for each geographic scope") +
  theme(panel.spacing = unit(2, "lines"),
        axis.line.x = element_line(size = 1, colour = "black"),
        axis.line.y = element_line(size = 1, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title=element_text(size = 27, margin=margin(0,0,15,0)),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 17),
        legend.position = "top",
        legend.direction = "horizontal",
        axis.text.x=element_text(colour="black", size = 18),
        axis.text.y=element_text(colour="black", size = 18),
        axis.title.x = element_text(size = 23, margin=margin(15,0,0,0)),
        axis.title.y = element_text(size = 23, margin=margin(0,15,0,0)))

years_to_reach_stability_gs_pro_tri

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/years_to_reach_stability_for_each_gs_pro_tri ",Sys.Date(),".png", sep = ''), width = 1200, height = 454)
years_to_reach_stability_gs_pro_tri
dev.off()

################################
#
# 4/11/2020 adding scatter plots for life stage and geographic scope vs stability time
#
################################

# read in tick dataset results from 3/25/2020
tick_dataset_results <- readxl::read_xlsx("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/data/tick dataset results_3_14_2020.xlsx", sheet = 1)

#need to create new column indicating # of datasets with stability time greater than a certain value
tick_dataset_results$stability_time_culamative <- 0

#calculating number of datasets with stability time higher than years
for(years in tick_dataset_results$stability_time) {
  tick_dataset_results[tick_dataset_results$stability_time == years,]$stability_time_culamative <- length(tick_dataset_results[tick_dataset_results$stability_time>years,]$stability_time)
}

################
#life stage with proportion of datasets
################

tick_dataset_results$stability_time_culamative_proportion <- tick_dataset_results$stability_time_culamative/max(tick_dataset_results$stability_time_culamative)

#sub dataset to include only data with adult, nymph, and larvae data
tick_dataset_results_sub_lf <- subset(tick_dataset_results, life_stage == "adult" | life_stage == "nymph" | life_stage == "larvae")

#order factors by adult, nymph, larvae
tick_dataset_results_sub_lf$life_stage <- factor(tick_dataset_results_sub_lf$life_stage, levels = c("adult", "nymph", "larvae"))

# create line plot for years to reach stability for each range of years for all life stages
pal<-brewer.pal(4, "Set2")
override.shape = c(6, 7, 16)
years_to_reach_stability_ls_pro <- ggplot(tick_dataset_results_sub_lf, aes(x=stability_time_culamative_proportion, y=stability_time)) +
  geom_point(size=5, aes(color=life_stage, shape=life_stage)) +
  scale_color_manual(values = pal)+
  scale_shape_manual(values = override.shape, guide = FALSE) +
  guides(colour = guide_legend(override.aes = list(shape = override.shape))) +
  scale_y_continuous(name = "Years to reach stability", expand = c(0,0), limits = c(0, 25)) +
  xlab("Proportion of datasets") + 
  labs(color="Life Stage") +
  ggtitle("Years to reach stability for all datasets for \neach life stage") +
  theme(axis.line.x = element_line(size = 1, colour = "black"),
        axis.line.y = element_line(size = 1, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title=element_text(size = 27, margin=margin(0,0,15,0)),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 17),
        legend.position = c(0.9, 0.9),
        axis.text.x=element_text(colour="black", size = 18),
        axis.text.y=element_text(colour="black", size = 18),
        axis.title.x = element_text(size = 23, margin=margin(15,0,0,0)),
        axis.title.y = element_text(size = 23, margin=margin(0,15,0,0)))

years_to_reach_stability_ls_pro

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/years_to_reach_stability_for_each_ls_pro_datasets ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
years_to_reach_stability_ls_pro
dev.off()


################
#life stage with proportion of datasets and tri-paneled
################

tick_dataset_results$stability_time_culamative_proportion <- tick_dataset_results$stability_time_culamative/max(tick_dataset_results$stability_time_culamative)

#sub dataset to include only data with adult, nymph, and larvae data
tick_dataset_results_sub_lf <- subset(tick_dataset_results, life_stage == "adult" | life_stage == "nymph" | life_stage == "larvae")

#order factors by adult, nymph, larvae
tick_dataset_results_sub_lf$life_stage <- factor(tick_dataset_results_sub_lf$life_stage, levels = c("adult", "nymph", "larvae"))

# create line plot for years to reach stability for proportion of datasets for all life stages in tri-paneled form
pal<-brewer.pal(4, "Set2")
override.shape = c(6, 7, 16)
years_to_reach_stability_ls_pro_tri <- ggplot(tick_dataset_results_sub_lf, aes(x=stability_time_culamative_proportion, y=stability_time, color=life_stage)) +
  geom_point(size=5, aes(color=life_stage, shape=life_stage)) +
  scale_color_manual(values = pal)+
  scale_shape_manual(values = override.shape, guide = FALSE) +
  facet_grid(cols = vars(life_stage))+
  guides(colour = guide_legend(override.aes = list(shape = override.shape))) +
  scale_y_continuous(name = "Years to reach stability", expand = c(0,0), limits = c(0, 25)) +
  xlab("Proportion of datasets") + 
  labs(color="") +
  ggtitle("Years to reach stability for all datasets for each life stage") +
  theme(panel.spacing = unit(2, "lines"),
        axis.line.x = element_line(size = 1, colour = "black"),
        axis.line.y = element_line(size = 1, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title=element_text(size = 27, margin=margin(0,0,15,0)),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 17),
        legend.position = "top",
        legend.direction = "horizontal",
        axis.text.x=element_text(colour="black", size = 18),
        axis.text.y=element_text(colour="black", size = 18),
        axis.title.x = element_text(size = 23, margin=margin(15,0,0,0)),
        axis.title.y = element_text(size = 23, margin=margin(0,15,0,0)))

years_to_reach_stability_ls_pro_tri

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/years_to_reach_stability_for_each_ls_pro_datasets_tri ",Sys.Date(),".png", sep = ''), width = 1000, height = 454)
years_to_reach_stability_ls_pro_tri
dev.off()

################
#geographic scope with proportion of datasets
################

# create line plot for years to reach stability for proportion of datasets for each geographic scope
pal<-brewer.pal(4, "Set2")
override.shape = c(6, 7, 8, 16)
years_to_reach_stability_gs_pro <- ggplot(tick_dataset_results, aes(x=stability_time_culamative_proportion, y=stability_time)) +
  geom_point(size=5, aes(color=geographic_scope, shape=geographic_scope)) +
  scale_color_manual(values = pal)+
  scale_shape_manual(values = override.shape, guide = FALSE) +
  guides(colour = guide_legend(override.aes = list(shape = override.shape))) +
  scale_y_continuous(name = "Years to reach stability", expand = c(0,0), limits = c(0, 25)) +
  xlab("Proportion of datasets") + 
  labs(color="Geographic scope") +
  ggtitle("Years to reach stability for all datasets for \neach geographic scope") +
  theme(axis.line.x = element_line(size = 1, colour = "black"),
        axis.line.y = element_line(size = 1, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title=element_text(size = 27, margin=margin(0,0,15,0)),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 17),
        legend.position = c(0.9, 0.9),
        axis.text.x=element_text(colour="black", size = 18),
        axis.text.y=element_text(colour="black", size = 18),
        axis.title.x = element_text(size = 23, margin=margin(15,0,0,0)),
        axis.title.y = element_text(size = 23, margin=margin(0,15,0,0)))

years_to_reach_stability_gs_pro

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/years_to_reach_stability_for_each_gs_pro ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
years_to_reach_stability_gs_pro
dev.off()

################
#geographic scope with proportion of datasets and tri-paneled
################

# create line plot for years to reach stability for proportion of datasets for each geographic scope
pal<-brewer.pal(4, "Set2")
override.shape = c(6, 7, 8, 16)
years_to_reach_stability_gs_pro_tri <- ggplot(tick_dataset_results, aes(x=stability_time_culamative_proportion, y=stability_time, color=geographic_scope)) +
  geom_point(size=5, aes(color=geographic_scope, shape=geographic_scope)) +
  scale_color_manual(values = pal)+
  scale_shape_manual(values = override.shape, guide = FALSE) +
  guides(colour = guide_legend(override.aes = list(shape = override.shape))) +
  scale_y_continuous(name = "Years to reach stability", expand = c(0,0), limits = c(0, 25)) +
  facet_grid(cols = vars(geographic_scope))+
  xlab("Proportion of datasets") + 
  labs(color="") +
  ggtitle("Years to reach stability for all datasets for each geographic scope") +
  theme(panel.spacing = unit(2, "lines"),
        axis.line.x = element_line(size = 1, colour = "black"),
        axis.line.y = element_line(size = 1, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title=element_text(size = 27, margin=margin(0,0,15,0)),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 17),
        legend.position = "top",
        legend.direction = "horizontal",
        axis.text.x=element_text(colour="black", size = 18),
        axis.text.y=element_text(colour="black", size = 18),
        axis.title.x = element_text(size = 23, margin=margin(15,0,0,0)),
        axis.title.y = element_text(size = 23, margin=margin(0,15,0,0)))

years_to_reach_stability_gs_pro_tri

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/years_to_reach_stability_for_each_gs_pro_tri ",Sys.Date(),".png", sep = ''), width = 1200, height = 454)
years_to_reach_stability_gs_pro_tri
dev.off()

################################
#
# 4/13/2020 adjusting proportion of datasets value for scatter plots for life stage and geographic scope vs stability time
#
################################

# read in tick dataset results from 3/25/2020
tick_dataset_results <- readxl::read_xlsx("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/data/tick dataset results_3_14_2020.xlsx", sheet = 1)

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
tick_dataset_results$stability_time_proportion <- tick_dataset_results$stability_time_proportion/max(tick_dataset_results$stability_time_proportion)

###########
#stability time
############

# create lineplot for years to reach stability for each range of years culamative
years_to_reach_stability_pro <- ggplot(tick_dataset_results, aes(x = stability_time_proportion, y = stability_time)) +
  geom_line(color="skyblue", size=2)+
  scale_y_continuous(name = "Years to stability", expand = c(0,0), limits = c(0,25)) +
  xlab("Proportion of datasets") + 
  ggtitle("Years to stability for all datasets") +
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
        axis.title.y = element_text(size = 23, margin=margin(0,15,0,0)),
        plot.margin = margin(0, 20, 0, 0))

years_to_reach_stability_pro

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/years_to_reach_stability_proportion_line_chart ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
years_to_reach_stability_pro
dev.off()

################
#life stage with proportion of datasets
################

#sub dataset to include only data with adult, nymph, and larvae data
tick_dataset_results_sub_lf <- subset(tick_dataset_results, life_stage == "adult" | life_stage == "nymph" | life_stage == "larvae")

#order factors by adult, nymph, larvae
tick_dataset_results_sub_lf$life_stage <- factor(tick_dataset_results_sub_lf$life_stage, levels = c("adult", "nymph", "larvae"))

# create line plot for years to reach stability for each range of years for all life stages
pal<-brewer.pal(4, "Set2")
override.shape = c(6, 7, 16)
years_to_reach_stability_ls_pro <- ggplot(tick_dataset_results_sub_lf, aes(x=stability_time_proportion, y=stability_time)) +
  geom_point(size=5, aes(color=life_stage, shape=life_stage)) +
  scale_color_manual(values = pal)+
  scale_shape_manual(values = override.shape, guide = FALSE) +
  guides(colour = guide_legend(override.aes = list(shape = override.shape))) +
  scale_y_continuous(name = "Years to reach stability", expand = c(0,0), limits = c(0, 25)) +
  xlab("Proportion of datasets") + 
  xlim(0,1) +
  labs(color="Life Stage") +
  ggtitle("Years to reach stability for all datasets for \neach life stage") +
  theme(axis.line.x = element_line(size = 1, colour = "black"),
        axis.line.y = element_line(size = 1, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title=element_text(size = 27, margin=margin(0,0,15,0)),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 17),
        legend.position = c(0.9, 0.9),
        axis.text.x=element_text(colour="black", size = 18),
        axis.text.y=element_text(colour="black", size = 18),
        axis.title.x = element_text(size = 23, margin=margin(15,0,0,0)),
        axis.title.y = element_text(size = 23, margin=margin(0,15,0,0)))

years_to_reach_stability_ls_pro

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/years_to_reach_stability_for_each_ls_pro_datasets ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
years_to_reach_stability_ls_pro
dev.off()


################
#life stage with proportion of datasets and tri-paneled
################

#sub dataset to include only data with adult, nymph, and larvae data
tick_dataset_results_sub_lf <- subset(tick_dataset_results, life_stage == "adult" | life_stage == "nymph" | life_stage == "larvae")

#order factors by adult, nymph, larvae
tick_dataset_results_sub_lf$life_stage <- factor(tick_dataset_results_sub_lf$life_stage, levels = c("adult", "nymph", "larvae"))

# create line plot for years to reach stability for proportion of datasets for all life stages in tri-paneled form
pal<-brewer.pal(4, "Set2")
override.shape = c(6, 7, 16)
years_to_reach_stability_ls_pro_tri <- ggplot(tick_dataset_results_sub_lf, aes(x=stability_time_proportion, y=stability_time, color=life_stage)) +
  geom_point(size=5, aes(color=life_stage, shape=life_stage)) +
  scale_color_manual(values = pal)+
  scale_shape_manual(values = override.shape, guide = FALSE) +
  facet_grid(cols = vars(life_stage))+
  guides(colour = guide_legend(override.aes = list(shape = override.shape))) +
  scale_y_continuous(name = "Years to stability", expand = c(0,0), limits = c(0, 25)) +
  xlab("Proportion of datasets") + 
  xlim(0,1)+
  labs(color="") +
  ggtitle("Years to stability for all datasets for each life stage") +
  theme(panel.spacing = unit(2, "lines"),
        axis.line.x = element_line(size = 1, colour = "black"),
        axis.line.y = element_line(size = 1, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title=element_text(size = 27, margin=margin(0,0,15,0)),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 17),
        legend.position = "top",
        legend.direction = "horizontal",
        axis.text.x=element_text(colour="black", size = 18),
        axis.text.y=element_text(colour="black", size = 18),
        axis.title.x = element_text(size = 23, margin=margin(15,0,0,0)),
        axis.title.y = element_text(size = 23, margin=margin(0,15,0,0)))

years_to_reach_stability_ls_pro_tri

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/years_to_reach_stability_for_each_ls_pro_datasets_tri ",Sys.Date(),".png", sep = ''), width = 1000, height = 454)
years_to_reach_stability_ls_pro_tri
dev.off()

################
#geographic scope with proportion of datasets
################

# create line plot for years to reach stability for proportion of datasets for each geographic scope
pal<-brewer.pal(4, "Set2")
override.shape = c(6, 7, 8, 16)
years_to_reach_stability_gs_pro <- ggplot(tick_dataset_results, aes(x=stability_time_proportion, y=stability_time)) +
  geom_point(size=5, aes(color=geographic_scope, shape=geographic_scope)) +
  scale_color_manual(values = pal)+
  scale_shape_manual(values = override.shape, guide = FALSE) +
  guides(colour = guide_legend(override.aes = list(shape = override.shape))) +
  scale_y_continuous(name = "Years to reach stability", expand = c(0,0), limits = c(0, 25)) +
  xlab("Proportion of datasets") + 
  labs(color="Geographic scope") +
  ggtitle("Years to reach stability for all datasets for \neach geographic scope") +
  theme(axis.line.x = element_line(size = 1, colour = "black"),
        axis.line.y = element_line(size = 1, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title=element_text(size = 27, margin=margin(0,0,15,0)),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 17),
        legend.position = c(0.9, 0.9),
        axis.text.x=element_text(colour="black", size = 18),
        axis.text.y=element_text(colour="black", size = 18),
        axis.title.x = element_text(size = 23, margin=margin(15,0,0,0)),
        axis.title.y = element_text(size = 23, margin=margin(0,15,0,0)))

years_to_reach_stability_gs_pro

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/years_to_reach_stability_for_each_gs_pro ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
years_to_reach_stability_gs_pro
dev.off()

################
#geographic scope with proportion of datasets and tri-paneled
################

# create line plot for years to reach stability for proportion of datasets for each geographic scope
pal<-brewer.pal(4, "Set2")
override.shape = c(6, 7, 8, 16)
years_to_reach_stability_gs_pro_tri <- ggplot(tick_dataset_results, aes(x=stability_time_proportion, y=stability_time, color=geographic_scope)) +
  geom_point(size=5, aes(color=geographic_scope, shape=geographic_scope)) +
  scale_color_manual(values = pal)+
  scale_shape_manual(values = override.shape, guide = FALSE) +
  guides(colour = guide_legend(override.aes = list(shape = override.shape))) +
  scale_y_continuous(name = "Years to stability", expand = c(0,0), limits = c(0, 25)) +
  facet_grid(cols = vars(geographic_scope))+
  xlab("Proportion of datasets") + 
  labs(color="") +
  ggtitle("Years to stability for all datasets for each geographic scope") +
  theme(panel.spacing = unit(2, "lines"),
        axis.line.x = element_line(size = 1, colour = "black"),
        axis.line.y = element_line(size = 1, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title=element_text(size = 27, margin=margin(0,0,15,0)),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 17),
        legend.position = "top",
        legend.direction = "horizontal",
        axis.text.x=element_text(colour="black", size = 18),
        axis.text.y=element_text(colour="black", size = 18),
        axis.title.x = element_text(size = 23, margin=margin(15,0,0,0)),
        axis.title.y = element_text(size = 23, margin=margin(0,15,0,0)))

years_to_reach_stability_gs_pro_tri

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/years_to_reach_stability_for_each_gs_pro_tri ",Sys.Date(),".png", sep = ''), width = 1200, height = 454)
years_to_reach_stability_gs_pro_tri
dev.off()


################################
#
# 4/13/2020 adjusting number of datasets value for scatter plots for life stage and geographic scope vs stability time
#
################################

# read in tick dataset results from 3/25/2020
tick_dataset_results <- readxl::read_xlsx("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/data/tick dataset results_3_14_2020.xlsx", sheet = 1)

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

###########
#stability time
############

# create lineplot for years to reach stability for each range of years culamative
years_to_reach_stability_num <- ggplot(tick_dataset_results, aes(x = stability_time_proportion, y = stability_time)) +
  geom_line(color="skyblue", size=2)+
  scale_y_continuous(name = "Years to reach stability", expand = c(0,0), limits = c(0,25)) +
  xlab("Number of datasets") + 
  ggtitle("Years to reach stability for all datasets") +
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
        axis.title.y = element_text(size = 23, margin=margin(0,15,0,0)),
        plot.margin = margin(0, 20, 0, 0))

years_to_reach_stability_num

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/years_to_reach_stability_num_line_chart ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
years_to_reach_stability_num
dev.off()

################
#life stage with proportion of datasets
################

#sub dataset to include only data with adult, nymph, and larvae data
tick_dataset_results_sub_lf <- subset(tick_dataset_results, life_stage == "adult" | life_stage == "nymph" | life_stage == "larvae")

#order factors by adult, nymph, larvae
tick_dataset_results_sub_lf$life_stage <- factor(tick_dataset_results_sub_lf$life_stage, levels = c("adult", "nymph", "larvae"))

# create line plot for years to reach stability for each range of years for all life stages
pal<-brewer.pal(4, "Set2")
override.shape = c(6, 7, 16)
years_to_reach_stability_ls_num <- ggplot(tick_dataset_results_sub_lf, aes(x=stability_time_proportion, y=stability_time)) +
  geom_point(size=5, aes(color=life_stage, shape=life_stage)) +
  scale_color_manual(values = pal)+
  scale_shape_manual(values = override.shape, guide = FALSE) +
  guides(colour = guide_legend(override.aes = list(shape = override.shape))) +
  scale_y_continuous(name = "Years to reach stability", expand = c(0,0), limits = c(0, 25)) +
  xlab("Number of datasets") + 
  xlim(0,300) +
  labs(color="Life Stage") +
  ggtitle("Years to reach stability for all datasets for \neach life stage") +
  theme(axis.line.x = element_line(size = 1, colour = "black"),
        axis.line.y = element_line(size = 1, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title=element_text(size = 27, margin=margin(0,0,15,0)),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 17),
        legend.position = c(0.9, 0.9),
        axis.text.x=element_text(colour="black", size = 18),
        axis.text.y=element_text(colour="black", size = 18),
        axis.title.x = element_text(size = 23, margin=margin(15,0,0,0)),
        axis.title.y = element_text(size = 23, margin=margin(0,15,0,0)))

years_to_reach_stability_ls_num

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/years_to_reach_stability_for_each_ls_num_datasets ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
years_to_reach_stability_ls_num
dev.off()


################
#life stage with proportion of datasets and tri-paneled
################

#sub dataset to include only data with adult, nymph, and larvae data
tick_dataset_results_sub_lf <- subset(tick_dataset_results, life_stage == "adult" | life_stage == "nymph" | life_stage == "larvae")

#order factors by adult, nymph, larvae
tick_dataset_results_sub_lf$life_stage <- factor(tick_dataset_results_sub_lf$life_stage, levels = c("adult", "nymph", "larvae"))

# create line plot for years to reach stability for proportion of datasets for all life stages in tri-paneled form
pal<-brewer.pal(4, "Set2")
override.shape = c(6, 7, 16)
years_to_reach_stability_ls_num_tri <- ggplot(tick_dataset_results_sub_lf, aes(x=stability_time_proportion, y=stability_time, color=life_stage)) +
  geom_point(size=5, aes(color=life_stage, shape=life_stage)) +
  scale_color_manual(values = pal)+
  scale_shape_manual(values = override.shape, guide = FALSE) +
  facet_grid(cols = vars(life_stage))+
  guides(colour = guide_legend(override.aes = list(shape = override.shape))) +
  scale_y_continuous(name = "Years to reach stability", expand = c(0,0), limits = c(0, 25)) +
  xlab("Number of datasets") + 
  xlim(0,300)+
  labs(color="") +
  ggtitle("Years to reach stability for all datasets for each life stage") +
  theme(panel.spacing = unit(2, "lines"),
        axis.line.x = element_line(size = 1, colour = "black"),
        axis.line.y = element_line(size = 1, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title=element_text(size = 27, margin=margin(0,0,15,0)),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 17),
        legend.position = "top",
        legend.direction = "horizontal",
        axis.text.x=element_text(colour="black", size = 18),
        axis.text.y=element_text(colour="black", size = 18),
        axis.title.x = element_text(size = 23, margin=margin(15,0,0,0)),
        axis.title.y = element_text(size = 23, margin=margin(0,15,0,0)))

years_to_reach_stability_ls_num_tri

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/years_to_reach_stability_for_each_ls_num_datasets_tri ",Sys.Date(),".png", sep = ''), width = 1000, height = 454)
years_to_reach_stability_ls_num_tri
dev.off()

################
#geographic scope with proportion of datasets
################

# create line plot for years to reach stability for proportion of datasets for each geographic scope
pal<-brewer.pal(4, "Set2")
override.shape = c(6, 7, 8, 16)
years_to_reach_stability_gs_num <- ggplot(tick_dataset_results, aes(x=stability_time_proportion, y=stability_time)) +
  geom_point(size=5, aes(color=geographic_scope, shape=geographic_scope)) +
  scale_color_manual(values = pal)+
  scale_shape_manual(values = override.shape, guide = FALSE) +
  guides(colour = guide_legend(override.aes = list(shape = override.shape))) +
  scale_y_continuous(name = "Years to reach stability", expand = c(0,0), limits = c(0, 25)) +
  xlab("Number of datasets") + 
  xlim(0,300) +
  labs(color="Geographic scope") +
  ggtitle("Years to reach stability for all datasets for \neach geographic scope") +
  theme(axis.line.x = element_line(size = 1, colour = "black"),
        axis.line.y = element_line(size = 1, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title=element_text(size = 27, margin=margin(0,0,15,0)),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 17),
        legend.position = c(0.9, 0.9),
        axis.text.x=element_text(colour="black", size = 18),
        axis.text.y=element_text(colour="black", size = 18),
        axis.title.x = element_text(size = 23, margin=margin(15,0,0,0)),
        axis.title.y = element_text(size = 23, margin=margin(0,15,0,0)))

years_to_reach_stability_gs_num

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/years_to_reach_stability_for_each_gs_num ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
years_to_reach_stability_gs_num
dev.off()

################
#geographic scope with proportion of datasets and tri-paneled
################

# create line plot for years to reach stability for proportion of datasets for each geographic scope
pal<-brewer.pal(4, "Set2")
override.shape = c(6, 7, 8, 16)
years_to_reach_stability_gs_num_tri <- ggplot(tick_dataset_results, aes(x=stability_time_proportion, y=stability_time, color=geographic_scope)) +
  geom_point(size=5, aes(color=geographic_scope, shape=geographic_scope)) +
  scale_color_manual(values = pal)+
  scale_shape_manual(values = override.shape, guide = FALSE) +
  guides(colour = guide_legend(override.aes = list(shape = override.shape))) +
  scale_y_continuous(name = "Years to reach stability", expand = c(0,0), limits = c(0, 25)) +
  facet_grid(cols = vars(geographic_scope))+
  xlab("Number of datasets") +
  xlim(0,300) +
  labs(color="") +
  ggtitle("Years to reach stability for all datasets for each geographic scope") +
  theme(panel.spacing = unit(2, "lines"),
        axis.line.x = element_line(size = 1, colour = "black"),
        axis.line.y = element_line(size = 1, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title=element_text(size = 27, margin=margin(0,0,15,0)),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 17),
        legend.position = "top",
        legend.direction = "horizontal",
        axis.text.x=element_text(colour="black", size = 18),
        axis.text.y=element_text(colour="black", size = 18),
        axis.title.x = element_text(size = 23, margin=margin(15,0,0,0)),
        axis.title.y = element_text(size = 23, margin=margin(0,15,0,0)))

years_to_reach_stability_gs_num_tri

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/years_to_reach_stability_for_each_gs_num_tri ",Sys.Date(),".png", sep = ''), width = 1200, height = 454)
years_to_reach_stability_gs_num_tri
dev.off()

###################
#compare study length with stability time
###################

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
  xlab("Study length") + 
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
        plot.margin = margin(10, 20, 0, 0))

years_to_reach_stability_length

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/years_to_reach_stability_length_line_chart ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
years_to_reach_stability_length
dev.off()

######################################
#
#Compare proportion right to stability time 4/15/2020
#
######################################

# read in tick dataset results from 3/25/2020
tick_dataset_results <- readxl::read_xlsx("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/data/tick dataset results_3_14_2020.xlsx", sheet = 1)

###############
#sampling technique vs proportion signigicantly wrong
###############

dragging <- subset(tick_dataset_results, sampling_technique == "dragging")
found <- subset(tick_dataset_results, sampling_technique == "found on a person")

t.test(dragging$proportion_wrong, found$proportion_wrong)
#t = -0.64665, df = 162.9, p-value = 0.5188

t.test(dragging$proportion_wrong_before_stability, found$proportion_wrong_before_stability)
#t = 0.083576, df = 155.58, p-value = 0.9335


# create boxplot for proportion significantly wrong between different sampling methods
tick_dataset_results_drag_found <- subset(tick_dataset_results, sampling_technique == "dragging" | sampling_technique == "found on a person")
proportion_wrong_by_samp_tech <- ggplot(tick_dataset_results_drag_found, aes(x = sampling_technique, y = proportion_wrong)) +
  geom_boxplot() + 
  scale_x_discrete(name = "Sampling technique") +
  scale_y_continuous(name = "Proportion significantly wrong", limits = c(0,1)) +
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

proportion_wrong_by_samp_tech

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/proportion_wrong_by_samp_tech ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
proportion_wrong_by_samp_tech
dev.off()


###############
#sampling technique vs proportion wrong before stability
###############

# create boxplot for proportion significantly wrong between different sampling methods
tick_dataset_results_drag_found <- subset(tick_dataset_results, sampling_technique == "dragging" | sampling_technique == "found on a person")
proportion_wrong_before_stab_by_samp_tech <- ggplot(tick_dataset_results_drag_found, aes(x = sampling_technique, y = proportion_wrong_before_stability)) +
  geom_boxplot() + 
  scale_x_discrete(name = "Sampling technique") +
  scale_y_continuous(name = "Proportion significantly wrong \nbefore stability", limits = c(0,1)) +
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

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/proportion_wrong_before_stab_by_samp_tech ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
proportion_wrong_before_stab_by_samp_tech
dev.off()



#############
#data length by proportion wrong
#############

por_wrong_by_data_length <- ggplot(tick_dataset_results, aes(as.numeric(data_range), as.numeric(proportion_wrong))) +
  geom_point() +
  geom_smooth(method = 'lm', size=2, formula = y~x) +
  scale_x_continuous(name = "Study length", expand = c(0,0)) +
  scale_y_continuous(name = "Proportion significantly wrong", expand = c(0,0), limits = c(0,1)) +
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 1, colour = "black"),
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

por_wrong_by_data_length

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/por_wrong_by_data_length ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
por_wrong_by_data_length
dev.off()


######################################
#
#Adding data points for proportion wrong before stability 4/28/2020
#
######################################

# read in tick dataset results from 3/25/2020
tick_dataset_results <- readxl::read_xlsx("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/data/tick dataset results_3_14_2020.xlsx", sheet = 1)

dragging <- subset(tick_dataset_results, sampling_technique == "dragging")
found <- subset(tick_dataset_results, sampling_technique == "found on a person")

length(dragging$stability_time)
#90 datasets
length(found$stability_time)
#198 datasets

median(dragging$stability_time)
#7
median(found$stability_time)
#12

t.test(dragging$stability_time, found$stability_time)
#t = -8.5346, df = 236.23, p-value = 1.724e-15
#significant

t.test(dragging$proportion_wrong_before_stability, found$proportion_wrong_before_stability)
#t = 0.083576, df = 155.58, p-value = 0.9335
#insignificant

adults <- subset(tick_dataset_results, life_stage == "adult")
nymphs <- subset(tick_dataset_results, life_stage == "nymph")
larvae <- subset(tick_dataset_results, life_stage == "larvae")

t.test(adults$stability_time, nymphs$stability_time)
#t = -0.63883, df = 126.84, p-value = 0.5241
#insignificant
t.test(adults$stability_time, larvae$stability_time)
#t = -5.9627, df = 10.111, p-value = 0.0001328
#significant
t.test(nymphs$stability_time, larvae$stability_time)
#t = -5.5196, df = 10.54, p-value = 0.0002109
#significant

t.test(adults$proportion_wrong_before_stability, nymphs$proportion_wrong_before_stability)
#t = 2.8993, df = 113.64, p-value = 0.00449
#significant
t.test(adults$proportion_wrong_before_stability, larvae$proportion_wrong_before_stability)
#t = 0.43788, df = 8.7735, p-value = 0.6721
#insignificant
t.test(nymphs$proportion_wrong_before_stability, larvae$proportion_wrong_before_stability)
#t = -0.75244, df = 7.8941, p-value = 0.4736
#insignificant

length(adults$stability_time)
#63 datasets
length(nymphs$stability_time)
#66 datasets
length(larvae$stability_time)
#8 datasets

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

#--------------------------------------

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

pathogen <- subset(tick_dataset_results, tested_for_b_burgdoferi == "Y")
abundance <- subset(tick_dataset_results, tested_for_b_burgdoferi == "N")

length(pathogen$stability_time)
#114 datasets
length(abundance$stability_time)
#175 datasets

cor(tick_dataset_results$stability_time, tick_dataset_results$data_range, use = "complete.obs")
#0.903413

t.test(tick_dataset_results$stability_time, tick_dataset_results$data_range)
#t = -9.6598, df = 190.48, p-value < 2.2e-16

###############
#sampling technique vs proportion wrong before stability
###############

# create boxplot for proportion significantly wrong between different sampling methods
tick_dataset_results_drag_found <- subset(tick_dataset_results, sampling_technique == "dragging" | sampling_technique == "found on a person")
proportion_wrong_before_stab_by_samp_tech <- ggplot(tick_dataset_results_drag_found, aes(x = sampling_technique, y = proportion_wrong_before_stability)) +
  geom_boxplot() + 
  geom_jitter() +
  scale_x_discrete(name = "Sampling technique") +
  scale_y_continuous(name = "Proportion significantly wrong \nbefore stability", limits = c(0,1)) +
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

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/proportion_wrong_before_stab_by_samp_tech ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
proportion_wrong_before_stab_by_samp_tech
dev.off()

###############
#life stage vs proportion wrong before stability
###############

# create boxplot for proportion significantly wrong between different sampling methods
tick_dataset_results_ls <- subset(tick_dataset_results, life_stage == "adult" | life_stage == "nymph" | life_stage == "larvae")
proportion_wrong_before_stab_by_life_stage <- ggplot(tick_dataset_results_ls, aes(x = life_stage, y = proportion_wrong_before_stability)) +
  geom_boxplot() + 
  geom_jitter() +
  scale_x_discrete(name = "Life stage") +
  scale_y_continuous(name = "Proportion significantly wrong \nbefore stability", limits = c(0,1)) +
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

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/proportion_wrong_before_stab_by_life_stage ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
proportion_wrong_before_stab_by_life_stage
dev.off()


###############
#geographic scope vs proportion wrong before stability
###############

# create boxplot for proportion significantly wrong between different sampling methods
#tick_dataset_results_gs <- subset(tick_dataset_results, life_stage == "adult" | life_stage == "nymph" | life_stage == "larvae")
proportion_wrong_before_stab_by_geographic_scope <- ggplot(tick_dataset_results, aes(x = geographic_scope, y = proportion_wrong_before_stability)) +
  geom_boxplot() + 
  geom_jitter() +
  scale_x_discrete(name = "Geographic scope") +
  scale_y_continuous(name = "Proportion significantly wrong \nbefore stability", limits = c(0,1)) +
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

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/proportion_wrong_before_stab_by_geographic_scope ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
proportion_wrong_before_stab_by_geographic_scope
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

#proportion significant by ticks infected and total ticks
proportion_wrong_before_stab_by_response <- ggplot(tick_infection_data, aes(x = abundance_vs_infected_ticks, y = proportion_wrong_before_stab_for_abudance_vs_infected_ticks)) +
  geom_boxplot() + 
  geom_jitter() +
  scale_x_discrete(name = "Response variable") +
  scale_y_continuous(name = "Proportion significantly wrong \nbefore stability", limits = c(0, 1)) +
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

proportion_wrong_before_stab_by_response

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/proportion_wrong_before_stab_by_response ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
proportion_wrong_before_stab_by_response
dev.off()

#-------------------

###############
#sampling technique vs stability time
###############

# create boxplot for proportion significantly wrong between different sampling methods
tick_dataset_results_drag_found <- subset(tick_dataset_results, sampling_technique == "dragging" | sampling_technique == "found on a person")
stability_time_by_samp_tech <- ggplot(tick_dataset_results_drag_found, aes(x = sampling_technique, y = stability_time)) +
  geom_boxplot() + 
  geom_jitter() +
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

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/stability_time_by_samp_tech ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
stability_time_by_samp_tech
dev.off()

###############
#life stage vs stability time
###############

# create boxplot for proportion significantly wrong between different sampling methods
tick_dataset_results_ls <- subset(tick_dataset_results, life_stage == "adult" | life_stage == "nymph" | life_stage == "larvae")
stability_time_by_life_stage <- ggplot(tick_dataset_results_ls, aes(x = life_stage, y = stability_time)) +
  geom_boxplot() + 
  geom_jitter() +
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

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/stability_time_by_life_stage ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
stability_time_by_life_stage
dev.off()


###############
#geographic scope vs stability time
###############

# create boxplot for proportion significantly wrong between different sampling methods
#tick_dataset_results_gs <- subset(tick_dataset_results, life_stage == "adult" | life_stage == "nymph" | life_stage == "larvae")
stability_time_by_geographic_scope <- ggplot(tick_dataset_results, aes(x = geographic_scope, y = stability_time)) +
  geom_boxplot() + 
  geom_jitter() +
  scale_x_discrete(name = "Geographic scope") +
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

stability_time_by_geographic_scope

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/stability_time_by_geographic_scope ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
stability_time_by_geographic_scope
dev.off()

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

#proportion significant by ticks infected and total ticks
stability_time_by_response <- ggplot(tick_infection_data, aes(x = abundance_vs_infected_ticks, y = stability_time_for_abudance_vs_infected_ticks)) +
  geom_boxplot() + 
  geom_jitter() +
  scale_x_discrete(name = "Response variable") +
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

stability_time_by_response

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/stability_time_by_response ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
stability_time_by_response
dev.off()

######################################

# Linear Regression 2/16/2020

#####################################

#Predictors:
#life stage
#geographic scope
#start year
#end year
#data range
#response units

#Possible random variables:
#study
#state

#Response variables
#Stability time
#Absolute range
#Relative range
#Proportion significant

#testing correlation of numerical variables
#start year vs end year
cor(tick_dataset_results$start_year, tick_dataset_results$end_year)
#r = -0.1419066
#little relationship

#start year vs data range
cor(tick_dataset_results$start_year, tick_dataset_results$data_range)
#r = -0.9637017
#strong relationship, so should not be used in the same model

#end year vs data range
cor(tick_dataset_results$end_year, tick_dataset_results$data_range)
#r = 0.4010352
#some correlation

cor(tick_dataset_results$life_stage, tick_dataset_results$stability_time)

fit1=lmer(stability_time ~ data_range + life_stage + sampling_technique + geographic_scope + response_units + (1|state), data = tick_dataset_results, REML=FALSE)
anova(fit1)
#Analysis of Variance Table
#Df  Sum Sq Mean Sq  F value
#data_range          1 2901.93 2901.93 868.4617
#life_stage          5   35.39    7.08   2.1181
#sampling_technique  2    0.41    0.21   0.0617
#geographic_scope    3    2.08    0.69   0.2072
#response_units      2  303.95  151.97  45.4810


summary(fit1)
# Linear mixed model fit by maximum likelihood  ['lmerMod']
# Formula: stability_time ~ data_range + life_stage + sampling_technique +  
#   geographic_scope + response_units + (1 | state)
# Data: tick_dataset_results
# 
# AIC      BIC   logLik deviance df.resid 
# 1200.8   1259.5   -584.4   1168.8      273 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -2.8960 -0.5366  0.0104  0.4908  4.4843 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# state    (Intercept) 0.000    0.000   
# Residual             3.341    1.828   
# Number of obs: 289, groups:  state, 5
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)                         -0.948617   2.814101  -0.337
# data_range                           0.490808   0.060219   8.150
# life_stagelarvae                    -0.248536   0.765657  -0.325
# life_stagenot specified              1.999962   2.362721   0.846
# life_stagenymph                     -0.298792   0.326666  -0.915
# life_stagenymphs                     2.060671   1.454953   1.416
# life_stageunspecified                1.741347   0.825576   2.109
# sampling_techniquedragging           2.888281   2.693024   1.073
# sampling_techniquefound on a person  0.490808   2.585837   0.190
# geographic_scopeGrid                -0.365381   0.876579  -0.417
# geographic_scopeState forest        -0.757537   1.309246  -0.579
# geographic_scopeTown                -0.285854   0.694767  -0.411
# response_units# per person           2.816490   0.550289   5.118
# response_units%                      0.007436   0.464676   0.016
# 
# Correlation matrix not shown by default, as p = 14 > 12.
# Use print(x, correlation=TRUE)  or
# vcov(x)        if you need it
# 
# fit warnings:
#   fixed-effect model matrix is rank deficient so dropping 1 column / coefficient
# convergence code: 0
# boundary (singular) fit: see ?isSingular



########################

# Comparing overall proportion wrong by proportion wrong before stability 3/25/2020

########################

cor(tick_dataset_results$stability_time, tick_dataset_results$proportion_wrong)
#0.2266395

# read in tick dataset results from 3/25/2020
tick_dataset_results <- readxl::read_xlsx("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/data/tick dataset results_3_14_2020.xlsx", sheet = 1)

lm_fit <- lm(proportion_wrong ~ stability_time, data=tick_dataset_results)
summary(lm_fit)

# save predictions of the model in the new data frame 
# together with variable you want to plot against
predicted_df <- data.frame(pro_pred = predict(lm_fit, tick_dataset_results), proportion_wrong=tick_dataset_results$proportion_wrong, stability_time=tick_dataset_results$stability_time)


por_wrong_by_stab_time <- ggplot(tick_dataset_results, aes(stability_time, proportion_wrong)) +
  geom_point() +
  geom_smooth(method = 'lm', size=2, formula = y~x) +
  geom_line(color='red',data = predicted_df, aes(x=pro_pred, y=proportion_wrong))+
  scale_x_continuous(name = "Years to stability", expand = c(0,0)) +
  scale_y_continuous(name = "Proportion significantly wrong", expand = c(0,0)) +
  ggtitle("Proportion significantly wrong by years to \nstability") + 
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 1, colour = "black"),
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

por_wrong_by_stab_time

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/por_wrong_by_stab_time ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
por_wrong_by_stab_time
dev.off()


##############################

# read in tick dataset results from 3/25/2020
tick_dataset_results <- readxl::read_xlsx("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/data/tick dataset results_3_14_2020.xlsx", sheet = 1)

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


tick_dataset_results$pw_label <- "Overall proportion wrong"
tick_dataset_results$pwbs <- "Proportion wrong before \nstability time"

overall_pw_vs_pwbs_lab <- c(tick_dataset_results$pw_label, tick_dataset_results$pwbs)
overall_pw_vs_pwbs_value <- c(tick_dataset_results$proportion_wrong, tick_dataset_results$proportion_wrong_before_stability)
overall_pw_vs_pwbs_df <- data.frame(overall_pw_vs_pwbs_lab, overall_pw_vs_pwbs_value)

overall_pw_vs_pwbs <- ggplot(overall_pw_vs_pwbs_df, aes(x = overall_pw_vs_pwbs_lab, y = overall_pw_vs_pwbs_value)) +
  geom_boxplot() + 
  geom_jitter() +
  scale_x_discrete(name = "Statistic") +
  scale_y_continuous(name = "Proportion") +
  ggtitle("Overall proportion wrong by proportion \nwrong before stability") + 
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title=element_text(size = 27, margin=margin(0,0,15,0)),
        axis.text.x=element_text(colour="black", size = 16),
        axis.text.y=element_text(colour="black", size = 12),
        axis.title.x = element_text(size = 23, margin=margin(15,0,0,0)),
        axis.title.y = element_text(size = 23, margin=margin(0,15,0,0)))

overall_pw_vs_pwbs

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_analysis/overall_pw_vs_pwbs ",Sys.Date(),".png", sep = ''), width = 600, height = 454)
overall_pw_vs_pwbs
dev.off()