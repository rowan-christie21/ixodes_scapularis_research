# Oct 29
# Running t-test on tick_dataset_results.xlsx file with response variables (density, count) on
# y-axis and methods/life stage/etc as x-axis to determine probability of difference between methods

#Oct 31
# Running t-test on additional variables and creating more figures to analyze results

#-------------------------------------------
# load libraries

library("ggplot2")
library("dplyr")

#-------------------------------------------

tick_dataset_results <- readxl::read_xlsx("D:/Ixodes_scapularis_research_2019/tick_dataset_results_algorithms/tick_dataset_results_10292019.xlsx", sheet = 1)

# subset data between two different methods - dragging and found on a person
dragging <- tick_dataset_results[tick_dataset_results$sampling_technique == "dragging",]
found_on_people <- tick_dataset_results[tick_dataset_results$sampling_technique == "found on a person",]

# run t-test between two different datasets: dragging and found_on_people, for stability time
# assumes that the variances of y1 and y2 are unequal
t.test(dragging$stability_time, found_on_people$stability_time)
# t = 4.1311, df = 158.88, p-value = 6.451e-05
# t-value indicates probability above 0.9995
# very high probability of difference between datasets
#
# p-value less than 0.05, reject null hypothesis
# therefore alternative hypothesis: true difference in means is not equal to 0 is supported

# run t-test between two different datasets: dragging and found_on_people, for proportion right
# assumes that the variances of y1 and y2 are unequal
t.test(dragging$proportion_right, found_on_people$proportion_right)
# t = -1.9102, df = 102.04, p-value = 0.05892
# t-value indicates probability below 0.5
# very low probability of difference between datasets
#
# p-value less than above, accept null hypothesis
# therefore true difference in means is equal to 0

# create boxplot for years to reach stability for each range of years
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
        plot.title=element_text(size = 20, margin=margin(0,0,20,0)),
        text=element_text(size = 16),
        axis.text.x=element_text(colour="black", size = 15),
        axis.text.y=element_text(colour="black", size = 15),
        axis.title.x = element_text(margin=margin(20,0,0,0)),
        axis.title.y = element_text(margin=margin(0,20,0,0)))

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_algorithms/years_to_reach_stability ",Sys.Date(),".png", sep = ''), width = 876, height = 604)
years_to_reach_stability
dev.off()


# create boxplot for years to reach stability between different sampling methods
yrs_stab_by_samp_tech <- ggplot(tick_dataset_results, aes(x = sampling_technique, y = stability_time)) +
  geom_boxplot() + 
  scale_x_discrete(name = "Sampling Technique") +
  scale_y_continuous(name = "Time to reach stability (years)") +
  ggtitle("Years to reach stability for each sampling technique") + 
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title=element_text(size = 20, margin=margin(0,0,20,0)),
        text=element_text(size = 16),
        axis.text.x=element_text(colour="black", size = 15),
        axis.text.y=element_text(colour="black", size = 15),
        axis.title.x = element_text(margin=margin(20,0,0,0)),
        axis.title.y = element_text(margin=margin(0,20,0,0)))

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_algorithms/yrs_stab_by_samp_tech ",Sys.Date(),".png", sep = ''), width = 876, height = 604)
yrs_stab_by_samp_tech
dev.off()


# create boxplot for proportion significant between different sampling methods
proportion_sig_by_samp_tech <- ggplot(tick_dataset_results, aes(x = sampling_technique, y = proportion_significant)) +
  geom_boxplot() + 
  scale_x_discrete(name = "Sampling Technique") +
  scale_y_continuous(name = "Proportion Significant") +
  ggtitle("Proportion significant for each sampling technique") + 
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title=element_text(size = 20, margin=margin(0,0,20,0)),
        text=element_text(size = 16),
        axis.text.x=element_text(colour="black", size = 15),
        axis.text.y=element_text(colour="black", size = 15),
        axis.title.x = element_text(margin=margin(20,0,0,0)),
        axis.title.y = element_text(margin=margin(0,20,0,0)))

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_algorithms/proportion_sig_by_samp_tech ",Sys.Date(),".png", sep = ''), width = 876, height = 604)
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
        plot.title=element_text(size = 20, margin=margin(0,0,20,0)),
        text=element_text(size = 16),
        axis.text.x=element_text(colour="black", size = 15),
        axis.text.y=element_text(colour="black", size = 15),
        axis.title.x = element_text(margin=margin(20,0,0,0)),
        axis.title.y = element_text(margin=margin(0,20,0,0)))

png(filename = paste("D:/Ixodes_scapularis_research_2019/tick_dataset_results_algorithms/ave_num_phases_by_start_year ",Sys.Date(),".png", sep = ''), width = 876, height = 604)
ave_num_phases_by_start_year
dev.off()


plot(tick_dataset_results$stability_time, tick_dataset_results$life_stage)


# create line graph for stability time for each life stage




