# load distance measures and function
source("scripts/select_distance_measures.R")
source("scripts/ts_testing_uncon_plot.R")

# perform uncontrolled testing and save plot of results
uncon_plot_fn(time_series = tests,
              dataset_name = "synthcontrol",
              q_min = 1,
              q_max = 20,
              increment = 1,
              package = "ph",
              fn_list = dist.fnlist2,
              args_list = dist.argslist2,
              names_list = dist.nameslist2,
              plotname_special = "test")

# plot time series used for uncontrolled testing
tiff(filename="plots/realworld/yoga_and_synthcontrol_ts_plot.jpg", 
     width = 5000, 
     height = 3000, 
     units = "px", 
     res = 1000,
     compression="lzw")
par(mfrow=c(2,1), mai=c(0.5,0.5,0.1,0.1))
plot(1:427, testy2, type="l", xlab="Time", ylab="", ylim=c(-2,2))
plot(1:61, tests2, type="l", xlab="Time", ylab="", ylim=c(-2,2))
dev.off()
  
  #################
  


