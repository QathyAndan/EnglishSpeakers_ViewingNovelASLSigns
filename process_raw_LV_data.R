# Writing a formula in R

# in excel, I have to do the following steps:

# 1. save file to folder A
# 2. Delete rows where column value &&& is equal 
# to either "practice" or "start_eyetracking"
# 3. Sort the entire table first by Trial_Nr, then by 
# times, then by variable name. (These are variable names)
# 4. Create a column, time_index, in column i, and then
# enter the formula: 
# =if(not(J2=J1), 0, if(or(H2=H1, abs(H2-H1)<10), I1, I1+1))
# what this is doing is given data that looks like this:

# times (col H) |  time_index (col i)  | Trial_Nr   | variable_name
# 1597182574985 |  0                   | 1          | eyeX
# 1597182574985 |  0                   | 1          | eyeY
# 1597182575001 |  1                   | 1          | eyeX
# 1597182575001 |  1                   | 1          | eyeY
# ....
# 1597182575121 |  0                   | 2          | eyeX
# 1597182575121 |  0                   | 2          | eyeY

# The end goal is to create a variable, time_index (as shown above)
# in column i that, for each trial, starts at 0, stays at that same
# number for each identical time stamp value (there should only be two, 
# one for variable_name = eyeX and one for eyeY, but sometimes there is only one,
# so for example sometimes the eyeY is collected at a slightly different millisecond
# than eyeX, and so the two time stamps do not match).

# the excel formula checks to see whether the current trial_Nr is still the same
# trial as for the previous row. If it isn't, reset the time_index at 0.
# If it is the same, that means it's still the same trial; in which 
# case, if the time stamp for the current row is the same as the previous row,
# the same time_index should be used, but if the timestamp has changed for the current
# row, that means it is also a new time index, and so time_index should be incremented by one.

# install.packages("tidyverse")
# install.packages("imputeTS")
# install.packages("readxl")
# install.packages("writexl")
# install.packages("stringr")

library(tidyverse)
library(readxl)
library(writexl)
library(stringr)
setwd("C:/Users/qathy/Desktop/Matlab_and_R/data_raw")

## List all files in data_raw folder
timeseries_list <- list.files(path = "C:/Users/qathy/Desktop/Matlab_and_R/data_raw", pattern = "*timeseries.xlsx")
triallevel_list <- list.files(path = "C:/Users/qathy/Desktop/Matlab_and_R/data_raw/", pattern = "*trialLevel.xlsx")
# triallevel_list <- triallevel_list[!str_detect(triallevel_list,pattern="~")] # removing temp ~$ file

for (i in seq_along(timeseries_list)){
  ts_fileName <- timeseries_list[[i]]
  tl_fileName <- triallevel_list[[i]]
  
##################### Time series data
  
  # import time series data 
  ts_data <- readxl::read_xlsx(ts_fileName)
  
  # Clean time series data
  ts_data=ts_data %>%
    filter(Task_Name == "View videos and pick") %>%
    # filter(Task_Name != "start eyetracking", Task_Name != "practice") %>%  #remove these
    # select(Trial_Nr, times, variable_name) %>% #just these columns
    arrange(Trial_Nr, times, variable_name) %>% #sort in ascending order
    mutate(time_index=as.integer(NA)) %>% #make a new variable
    select(-c(Block_Name, Block_Nr, session_nr, Task_Name, Task_Nr)) # remove these columns
    
    
    ts_data$time_index[1] = 0 # set initial time index to zero.
    
    for (numrows in 2:nrow(ts_data)) # loop through the remainder of the time indices
    { 
      if(ts_data$Trial_Nr[numrows]==ts_data$Trial_Nr[numrows-1])  #if the current trial is the same as the previous
      { 
        if(abs(ts_data$times[numrows]-ts_data$times[numrows-1]) < 10) # if the timestamp is within 10 of the previous
        { 
          ts_data$time_index[numrows] = ts_data$time_index[numrows-1]  # make the time index the same
          ts_data$times[numrows] = ts_data$times[numrows-1] # make the timestamp same as previous
        }
        else { ts_data$time_index[numrows] = ts_data$time_index[numrows-1] + 1 } # increase time index by 1
      }
      else { ts_data$time_index[numrows] = 0 } # set 0 if it's a new trial 
    }
    
    # # Now, reshape the time series data so that eyeX and eyeY coordinates are in separate columns
    # ts_data <- ts_data %>% pivot_wider(names_from = variable_name, values_from = values)
    
    subID = as.character(ts_data$exp_subject_id[1])
    newFileName <- paste0("C:/Users/qathy/Desktop/Matlab_and_R/data_post_excel/sub_", subID, "_timeseries.xlsx")
    write_xlsx(ts_data, path = newFileName, col_names = TRUE)
    
    ############################## Trial level data
    
    # import trial level data
    trial_data <- readxl::read_xlsx(tl_fileName)
    
    # Clean the trial level data
    trial_data=trial_data %>%
      filter(Task_Name == "View videos and pick") %>%
      # filter(Task_Name != "start eyetracking", Task_Name != "practice") %>%
      arrange(Trial_Id) %>% #sort in order of Trial ID
      # delete unnecessary columns
      select(!any_of(c("Task_Name", "Task_Nr", "Block_Nr", "Block_Name", "calib_Fails", 
                "calibProgress", "calibX", "calibY", "Condition_Id", "counterFails",
                "counterSuccess", "errEye", "errEyeX", "errEyeY", "eyeX", "eyeY", "numCalibrated",
                "completed", "crowdsourcing_code", "crowdsourcing_subj_id", "displayedLanguage",
                "end_time", "group_name", "group_nr", "role_id", "selected_age", "selected_gender",
                "selected_language", "selected_location", "session_name", "session_nr",
                "subj_counter_per_group", "subject_code", "time_delay_offset",
                "time_measure_std", "unlocked", "pixelDensityPerMM")))
    
    insert_both = readxl::read_xlsx("C:/Users/qathy/Desktop/Matlab_and_R/inserts/Diss_Exp3_bothblocks.xlsx")
    
    trial_data$Item = insert_both$Item
    trial_data$XXposition = insert_both$XXposition
    trial_data$doublingResponse = insert_both$doublingResponse
    trialFileName <- paste0("C:/Users/qathy/Desktop/Matlab_and_R/data_post_excel/sub_", subID, "_trialLevel.xlsx")
    write_xlsx(trial_data, path = trialFileName, col_names = TRUE)
}

# NOW, to process the post_excel documents into a long, joined format that includes all the data. (Previously, this was the file process_raw_csv_data.R)

setwd("C:/Users/qathy/Desktop/Matlab_and_R/data_post_excel")
# To get data into shape, first delete all non-eyetracking tasks;sort by Trial #, times, variable name, and then create column time_index at col i, 
# and in column i enter formula: =IF(NOT(J2=J1), 0, IF(H2=H1, I1, I1+1)) [i.e. checks if trial id changes; if so, start over at 0. 
# otherwise, either same time stamp but different var, or different time stamp. 
# if time stamp remains the same, use same time index; if not, increment time index by one]

## List all files in post_excel folder
timeseries_ExcelList <- list.files(path = "C:/Users/qathy/Desktop/Matlab_and_R/data_post_excel/", pattern = "*timeseries.xlsx")
triallevel_ExcelList <- list.files(path = "C:/Users/qathy/Desktop/Matlab_and_R/data_post_excel/", pattern = "*trialLevel.xlsx")

for (j in seq_along(timeseries_ExcelList)){
  ts_excelFileName <- timeseries_ExcelList[[j]]
  tl_excelFileName <- triallevel_ExcelList[[j]]
  sub_data <- readxl::read_xlsx(ts_excelFileName)
  # # reshape the data so that eyeX and eyeY coordinates are in separate columns -- DOES THIS STILL DO ANYTHING???
  # reshaped_data <- sub_data %>% pivot_wider(names_from = variable_name, values_from = values)
  
  # import trial level data
  trialLevel_data <- readxl::read_xlsx(tl_excelFileName)
  
  #### OPTIONAL
  # # fill NAs with linearly interpolated data
  # library(imputeTS)
  # # interpolate missing eyeX values from surrounding eyeX values
  # eyeX_filledNAs_linear <- na_interpolation(reshaped_data$eyeX)
  # # do the same for eyeY values
  # eyeY_filledNAs_linear <- na_interpolation(reshaped_data$eyeY)
  # 
  # # make new copy of dataset for interpolated data
  # filled_data <- reshaped_data
  # # replace old eyeX and old eyeY with interpolated data in a new copy of dataset
  # filled_data$eyeX <- eyeX_filledNAs_linear
  # filled_data$eyeY <- eyeY_filledNAs_linear
  
  
  
  # merge the data 
  mergedData <- left_join(sub_data, trialLevel_data)
  choseDoubling <- mergedData$doublingResponse == mergedData$whichArrow
  mergedData$choseDoubling <- choseDoubling
  # reshape the data so that eyeX and eyeY coordinates are in separate columns
  reshaped_merge <- mergedData %>% pivot_wider(names_from = variable_name, values_from = values)
  
  # for block_Name, set values dependent on trial_Group, and do same thing for block #
  reshaped_merge$Block_Name[reshaped_merge$trial_Group=="block_1_trials"] <- "block 1"
  reshaped_merge$Block_Nr[reshaped_merge$trial_Group=="block_1_trials"] <- 1
  
  reshaped_merge$Block_Name[reshaped_merge$trial_Group=="block_2_trials"] <- "block 2"
  reshaped_merge$Block_Nr[reshaped_merge$trial_Group=="block_2_trials"] <- 2
  
  timeSeries_All <- reshaped_merge
  subjectID = as.character(timeSeries_All$exp_subject_id[1])
  
  postRFileName <- paste0("C:/Users/qathy/Desktop/Matlab_and_R/data_post_R/exp3_subject_", subjectID, ".xlsx")
  write_xlsx(timeSeries_All, path = postRFileName, col_names = TRUE)
}
