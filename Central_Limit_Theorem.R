#################################################################################################
## OZKAN EMRE OZDEMIR                                                                           #
## HOMEWORK 4 : Chicago Diabetes Homework (Lecture 4)                                                           #
## 04/28/16                                                                                     #
## Class:  Methods for Data Analysis                                                            #
##                                                                                              #
#################################################################################################
## Clear objects from Memory :
rm(list=ls())
##Clear Console:
cat("\014")

## Get the libraries
library(logging)


# Get the log file name that has a date-time in the name
get_log_filename = function(){
        log_file_name = format(Sys.time(), format="HW4_log_%Y_%m_%d_%H%M%S.log")
        return(log_file_name)
}

# Unit test to check that log file name doesn't exist
test_log_file_name_uniqueness = function(log_file_name){
        all_files = list.files()
        stopifnot(!log_file_name%in%all_files)
}


if (interactive()){
        # Get logger file name
        log_file_name = get_log_filename()
        basicConfig()
        addHandler(writeToFile, file=log_file_name, level='INFO')
    
        # Test for uniqueness
        test_log_file_name_uniqueness(log_file_name)
        
        # Setup working directory
        setwd('~/DataAnalysis/4_HypothesisTesting_CentralLimit/')
        
        #Define the slope function based on the first and the last values
        slope <-function(x1,x2,y1,y2){
        (y1-y2)/(x1-x2)
        }

        # Define the y-intercept function (Pick a point, and calculate the y-intercept based on the slope):
        # b = y - mx
        y_int<-function(x1,y1,slope_guess){
                (y1-slope_guess*x1)
        }

        # Get the data and orginize it

        data = read.csv('ChicagoDiabetesData.csv', stringsAsFactors = FALSE)

        data_sums = apply(data[-1],2,sum)

        hospitalizations = data_sums[grepl('Hospitalizations', names(data_sums))]
        admit_rate = data_sums[grepl('Crude.Rate.[0-9]+$', names(data_sums), perl = TRUE)]
        
        #########################################################################
        # Part I : Num. Hospitalizations vs. Crude Admittance Rate
        #########################################################################
        
        hospitalizations_order = hospitalizations[order(admit_rate)]
        admit_rate_order = admit_rate[order(admit_rate)]
        plot(hospitalizations, admit_rate,
             main="Part I : Num. Hospitalizations vs. Crude Admittance Rate")

        # Calculate the slope & the y-intercept
        x1 <-hospitalizations_order[[1]]
        x2 <-hospitalizations_order [[length(hospitalizations_order)]]
        y1 <-admit_rate_order[[1]]
        y2 <-admit_rate_order[[length(admit_rate_order)]]

        slope_guess_I = slope(x1,x2,y1,y2)

        y_int_guess_I = y_int(x1,y1,slope_guess_I)
        
        ## add the linear ....
        abline(y_int_guess_I, slope_guess_I, col="gray23",lty=2)
        
        
        ## Calculate the SSE
        
        i = 0
        true_y = 0
        SSE = 0
        x = 0
        y = 0
        
        for (i in 1:length(admit_rate_order)){
                y[i]<-admit_rate_order[[i]]
                x[i]<-hospitalizations_order[[i]]
                true_y[i] <- (y_int_guess_I + slope_guess_I*x[[i]])
        }
        
        SSE = sum((y-true_y)^2)
        

        ## Calculate the SST
        
        y_avg = rep(mean(true_y), length(x))
        SST = sum((y - y_avg)^2)
        

        ## Calculate the R^2, this value is interpreted as
        #  the % of variance explained in the model (more than the average)
        
        R_SQR = 1 - SSE/SST
        
        ##-----Least Squares Regression in R-----
        best_fit_line = lm(y~x)
        summary(best_fit_line)
        
        ## add the best fit line to the graph        
        abline(best_fit_line,col="red")

        # add the mean line to the graph
        y_mean = rep(mean(y), length(x))
        lines(x, y_mean, col="blue")
        
        legend("topleft",
               c("predicted curve","best fit", "mean line"),
               fill=c("gray","red", "blue"))
        grid()
        
        ## Part-I Results
        Slope_I = slope_guess_I
        SSE_I = SSE
        SST_I = SST
        R_SQR_I = R_SQR
        
        
        #########################################################################
        # Part II : Delta Num. Hospitalizations vs. Delta Crude Admittance Rate #
        #########################################################################
        
        hospitalizations_diff = diff(hospitalizations)
        admit_rate_diff = diff(admit_rate)

        hospitalizations_order = hospitalizations_diff[order(admit_rate_diff)]
        admit_rate_order = admit_rate_diff[order(admit_rate_diff)]

        plot(hospitalizations_diff, admit_rate_diff,
             main="Part II : Delta Num. Hospitalizations vs. Delta Crude Admittance Rate")

        # Calculate the slope & the y-intercept :
        x1 <-hospitalizations_order[[1]]
        x2 <-hospitalizations_order [[length(hospitalizations_order)]]
        y1 <-admit_rate_order[[1]]
        y2 <-admit_rate_order[[length(admit_rate_order)]]

        slope_guess_II = slope(x1,x2,y1,y2)

        y_int_guess_II = y_int(x1,y1,slope_guess_II)

        abline(y_int_guess_II, slope_guess_II,col="gray23",lty=2)
        
        ## Calculate the SSE
        
        i = 0
        true_y = 0
        SSE = 0
        x = 0
        y = 0
        
        for (i in 1:length(admit_rate_order)){
                y[i]<-admit_rate_order[[i]]
                x[i]<-hospitalizations_order[[i]]
                true_y[i] <- (y_int_guess_II + slope_guess_II*x[[i]])
        }
        
        SSE = sum((y-true_y)^2)
        

        ## Calculate the SST
        
        y_avg = rep(mean(true_y), length(x))
        SST = sum((y - y_avg)^2)
        

        ## Calculate the R^2, this value is interpreted as
        #  the % of variance explained in the model (more than the average)
        
        R_SQR = 1 - SSE/SST

        ##-----Least Squares Regression in R-----
        best_fit_line = lm(y~x)
        summary(best_fit_line)
        
        ## add the best fit line to the graph        
        abline(best_fit_line,col="red")
        
        # add the mean line to the graph
        y_mean = rep(mean(y), length(x))
        lines(x, y_mean, col="blue")
        
        legend("topleft",
               c("predicted curve","best fit", "mean line"),
               fill=c("gray","red", "blue"))
        
        grid()
        
        ## Part-II Results
        Slope_II = slope_guess_II
        SSE_II = SSE
        SST_II = SST
        R_SQR_II = R_SQR
        
        
        # Log Results
        loginfo(paste('PART I \n',' "Hospitalizations" is the total number of people discharged from hospitals (irrespective of their health/disease status)\n',
                      '"Crude Admittance" is the number discharged with diabetes. Both numbers are "per 10,000"\n', 
                      'The Ratio between Hospitalizations vs. Crude Admittance Rate ~',Slope_I , 
                      '\n which means If the the total number of people discharged from hospitals goes up by 10,000 then the number discharged with diabetes goes up by', Slope_I*10000, 
                        '\n The SSE value is', SSE_I,' \n The SST value is', SST_I, '\n and the R Square value is obtained as', R_SQR_I,
                        '\n which means that predicted linear regression line can approximates the', R_SQR_I*100, '% of the data points '))
        
        loginfo(paste('PART II \n','The annual change between Num. Hospitalizations vs. Crude Admittance Rate ~',Slope_II,
                       '\n which means the annual change in the total number of discharges goes up by 10,000 then the annual change in discharges with diabetes goes up by', Slope_II*10000,
                        '\n The SSE value is', SSE_II,' \n The SST value is', SST_II, '\n and the R Square value is obtained as', R_SQR_II,
                      '\n which means that predicted linear regression line can approximates the', R_SQR_II*100, '% of the data points '))
}
#############################           End             #############################  
