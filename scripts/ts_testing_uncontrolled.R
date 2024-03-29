# load distance measures and function
source("scripts/select_distance_measures.R")
source("scripts/ts_testing_uncon_plot.R")

# import Yoga dataset from UCR time series archive
yoga_train <- read.csv(file="UCR_datasets/Yoga/Yoga_TRAIN.tsv", sep="")

# select one time series for uncontrolled testing
test_y <- as.numeric(yoga_train[1,])

# make a copy
test_y2 <- test_y

# translate time series to avoid non-positive values
test_y <- test_y + 100

# import Synthetic Control dataset from UCR time series archive
synth_control_train <- read.csv(file="UCR_datasets/SyntheticControl/SyntheticControl_TRAIN.tsv", sep="")

# select one time series for uncontrolled testing
test_s <- as.numeric(synth_control_train[1,])

# make a copy
test_s2 <- test_s

# translate time series to avoid non-positive values
test_s <- test_s + 100

# perform uncontrolled testing and save plot of results
temp_uncon <- uncon_plot_fn(time_series = test_y,
                            dataset_name = "yoga",
                            q_min = 1,
                            q_max = 200,
                            increment = 10,
                            package = "ph",
                            fn_list = dist.fnlist2,
                            args_list = dist.argslist2,
                            names_list = dist.nameslist2,
                            plotname_special = "_test6") # recommend putting an underscore first

# plot time series used for uncontrolled testing
if(!dir.exists("figures/")) {dir.create("figures/")}
tiff(filename="figures/yoga_and_synthcontrol_ts_plot.tiff",
     width = 5000, 
     height = 3000, 
     units = "px", 
     res = 1000,
     compression="lzw")

par(mfrow=c(2,1), mai=c(0.5,0.5,0.1,0.1))
plot(1:427, test_y2, type="l", xlab="Time", ylab="", ylim=c(-2,2))
plot(1:61, test_s2, type="l", xlab="Time", ylab="", ylim=c(-2,2))

dev.off()
  
  #################
  


