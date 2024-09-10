

rm(list = ls())

len<-length

count <- function(x) { 
  length(na.omit(x)) 
}


round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

install.packages("xlsx")
# install.packages("BayesFactor")
# install.packages("extrafont")
# install.packages("zoo")
library(BayesFactor)
library(extrafont)
library("zoo")
library("Matrix")
library("lme4")
library("lmerTest")
library("ggplot2")
library("eyetrackingR")
library("readxl")
library("writexl")
library("xlsx")



mainRootPath <- "C:/Users/qathy/OneDrive/Documents/Matlab_and_R/analysis_files/"
outputFolder <- "9. Results from big analysis/"
inputFolder <- "7. Analysis ready data (fed to R)/"

setwd(mainRootPath)
# SPECIFY EXPERIMENT TO ANALYZE -------------------
expNameList <- c("exp1",         #1 Words, phonology
                 "exp2_Hom_all", #2 Words, morphology
                 "exp3",          #4 Signs, phonology
                 "exp4_Hom_all",  #5 Signs, morphology
                 "exp4_Het_all", #6 Signs, illicit control
                 "exp2_Het_all") #3 Words, illicit control)
# expNameList <- c("exp2_Het_all")
# expName <- expNameList[[1]] # change number according to above list
trialVariable = "pair"
for(expName in expNameList){
  for(windowOfInterest in c(1, 2, 3)){
  # for(windowOfInterest in c(1)){
    if(windowOfInterest==1){
      win_start_col <- "win1start"
      win_end_col <- "win1end"
    } else if(windowOfInterest==2){
      win_start_col <- "win2start"
      win_end_col <- "win2end"
    } else if(windowOfInterest==3){
      win_start_col <- "win3start"
      win_end_col <- "win3end"
    }
    if(expName=="exp1") {
      # WORDS - Phonology condition
      csvName <- "TrimmedRetimedData_Spoken Phonology_Phon_v1.11.xlsx"
      prettyName <- paste0("spoken phonology_", windowOfInterest)
      smooth_spar <- .3
      experimentPath <- paste0(mainRootPath, outputFolder, "Exp 1 - Spoken Phonology/window ", windowOfInterest, "/")
      
      # WORDS - Morphology conditions
    } else if(expName=="exp2_Hom_all"){
      csvName <- "TrimmedRetimedData_Spoken Morphology_Hom_All_v1.11.xlsx"
      prettyName <- paste0("spoken morphology_", windowOfInterest)
      smooth_spar <- .3
      experimentPath <- paste0(mainRootPath, outputFolder, "Exp 2 - Spoken Morphology/Licit/window ", windowOfInterest, "/")
    } else if(expName=="exp2_Het_all"){
      csvName <- "TrimmedRetimedData_Spoken Morphology_Het_All_v1.11.xlsx"
      prettyName <- paste0("spoken ILLICIT_", windowOfInterest)
      smooth_spar <- .3
      experimentPath <- paste0(mainRootPath, outputFolder, "Exp 2 - Spoken Morphology/Illicit/window ", windowOfInterest, "/")
      # SIGNS - Phonology condition
    } else if(expName=="exp3"){
      csvName <- "TrimmedRetimedData_Signed Phonology_Phon_v1.11.xlsx"
      prettyName <- paste0("signed phonology_", windowOfInterest)
      # smooth_spar <- .55
      smooth_spar <- .3
      experimentPath <- paste0(mainRootPath, outputFolder, "Exp 3 - Signed Phonology/window ", windowOfInterest, "/")
      # SIGNS - Morphology conditions
    } else if(expName=="exp4_Hom_all"){
      csvName <- "TrimmedRetimedData_Signed Morphology_Hom_All_v1.11.xlsx"
      prettyName <- paste0("signed morphology_", windowOfInterest)
      # smooth_spar <- .5
      smooth_spar <- .3
      experimentPath <- paste0(mainRootPath, outputFolder, "Exp 4 - Signed Morphology/Licit/window ", windowOfInterest, "/")
    } else if(expName=="exp4_Het_all"){
      csvName <- "TrimmedRetimedData_Signed Morphology_Het_All_v1.11.xlsx"
      prettyName <- paste0("signed ILLICIT_", windowOfInterest)
      # smooth_spar <- .5
      smooth_spar <- .3
      experimentPath <- paste0(mainRootPath, outputFolder, "Exp 4 - Signed Morphology/Illicit/window ", windowOfInterest, "/")
    }
    
    # specify where to save outputs
    trackloss <- paste0(experimentPath, "Trackloss/")
    rawGraphs <- paste0(experimentPath, "Plot raw proportions/")
    smoothedGraphs <- paste0(experimentPath, "Smoothed/")
    analysisTables <- paste0(experimentPath, "Tables/")
    byWordChosen <- paste0(experimentPath, "Subset by word chosen/")
    # CREATE DATAFRAMES FOR ANALYSIS-----------------------------------
    
    dat <- readxl::read_xlsx(paste0(mainRootPath, inputFolder, csvName))
    dat$left_Type <- dat$targetSide == 1
    dat$right_Type <- dat$targetSide == 2
    
    timeBin <- 100 # time bin grain size = 100ms
    
    datPre_Left <- 
      make_eyetrackingr_data(dat, 
                             participant_column = "subID", 
                             trial_column = "pair", 
                             item_column = "trial",
                             aoi_columns = c("left", "right", "other"),
                             time_column = "time",
                             trackloss_column = "trackloss", 
                             treat_non_aoi_looks_as_missing = TRUE)
    datPre_Left["win1start"] <- 0 # prepopulate with zero to set aside memory
    datPre_Left["win2start"] <- datPre_Left["win1end"] + timeBin #100 ms = 1 time bin, 
    datPre_Left["win3start"] <- datPre_Left["win2end"] + timeBin
    # 1 cycle is distance from window 1 end to window 2 end, and the third cycle start from the end of the second cycle
    datPre_Left["win3end"] <- datPre_Left["win2end"] + (datPre_Left["win2end"] - datPre_Left["win1end"]) 
    # so this should guarantee that the timebins of w1 and w2 are not overlapping
    
    datPre_Right <- 
      make_eyetrackingr_data(dat, 
                             participant_column = "subID", 
                             trial_column = "pair", 
                             item_column = "trial",
                             aoi_columns = c("left", "right", "other"),
                             time_column = "time",
                             trackloss_column = "trackloss", 
                             treat_non_aoi_looks_as_missing = TRUE)
    datPre_Right["win1start"] <- 0
    datPre_Right["win2start"] <- datPre_Right["win1end"] + timeBin
    datPre_Right["win3start"] <- datPre_Right["win2end"] + timeBin
    # 1 cycle is distance from window 1 end to window 2 end, and the third cycle start from the end of the second cycle
    datPre_Right["win3end"] <- datPre_Right["win2end"] + (datPre_Right["win2end"] - datPre_Right["win1end"])
    
    # 0 - 5s
    # subset to response window post word-onset
    response_window_Left <- subset_by_window(datPre_Left, 
                                             window_start_col = win_start_col, 
                                             window_end_col = win_end_col, 
                                             rezero = FALSE)
    response_window_Right <- subset_by_window(datPre_Right, 
                                              window_start_col = win_start_col, 
                                              window_end_col = win_end_col, 
                                              rezero = FALSE)

    
    # ----- TRACKLOSS ANALYSIS----------------------------------------------
    #remove subs and trials with (> 90% of trackloss), export trackloss data both before and after cleaning
    

    # LEFT
    # trackloss before cleaning
    trackloss_preclean_Left <- trackloss_analysis(data = response_window_Left)
    write.csv(trackloss_preclean_Left, paste0(trackloss, prettyName, '_Pre-cleaned data_Left_', windowOfInterest, '.csv'))
    #Now clean response windows to exclude subs or trials with >90% trackloss
    response_window_clean_Left <- clean_by_trackloss(data = response_window_Left,
                                                     participant_prop_thresh = .9,
                                                     trial_prop_thresh = .9)
    # analyze trackloss still remaining after excluded subs and trials
    trackloss_clean_Left <- trackloss_analysis(data = response_window_clean_Left)
    write.csv(trackloss_clean_Left, paste0(trackloss, prettyName, '_CLEANED data_Left_', windowOfInterest, '.csv'))

    # RIGHT
    # trackloss before cleaning
    trackloss_preclean_Right <- trackloss_analysis(data = response_window_Right)
    write.csv(trackloss_preclean_Right, paste0(trackloss, prettyName, '_Pre-cleaned data_Right_', windowOfInterest, '.csv'))
    #Now clean response windows to exclude subs or trials with >90% trackloss
    response_window_clean_Right <- clean_by_trackloss(data = response_window_Right,
                                                      participant_prop_thresh = .9,
                                                      trial_prop_thresh = .9)
    # analyze trackloss still remaining after excluded subs and trials
    trackloss_clean_Right <- trackloss_analysis(data = response_window_clean_Right)
    write.csv(trackloss_clean_Right, paste0(trackloss, prettyName, '_CLEANED data_Right_', windowOfInterest, '.csv'))

    #---------------------DIVERGENCE ANALYSIS--------------------------------
    
    # ANALYSIS PREP - Create target (our predictor)
    
    # create Target condition column
    response_window_clean_Left$Target <- as.factor( ifelse(response_window_clean_Left$left_Type==1, 
                                                           yes = 'Left is XX', 
                                                           no  = 'Left is XY') )
    response_window_clean_Right$Target <- as.factor( ifelse(response_window_clean_Right$right_Type==1, 
                                                            yes = 'Right is XX', 
                                                            no  = 'Right is XY') )
    
    #---------------------------------------
    
    #create dataframes, fill in nans, and smooth data using smooth.spline
    
    #left
    response_timeL <- make_time_sequence_data(response_window_clean_Left, 
                                              time_bin_size = timeBin, 
                                              predictor_columns = c("Target"),
                                              other_dv_columns = "chosen",
                                              aois = "left")
    #right
    response_timeR <- make_time_sequence_data(response_window_clean_Right, 
                                              time_bin_size = timeBin, 
                                              predictor_columns = c("Target"),
                                              other_dv_columns = "chosen",
                                              aois = "right")
    
    
    
    # sum-code and center predictor, -> TargetC:
    # left
    response_timeL$TargetC <- ifelse(response_timeL$Target == 'Left is XX', .5, -.5)
    response_timeL$TargetC <- as.numeric(scale(response_timeL$TargetC, 
                                               center=TRUE, 
                                               scale=FALSE))
    # right
    response_timeR$TargetC <- ifelse(response_timeR$Target == 'Right is XX', .5, -.5)
    response_timeR$TargetC <- as.numeric(scale(response_timeR$TargetC, 
                                               center=TRUE, 
                                               scale=FALSE))
    
    # --SMOOTHING LOOP
    #Create dummy column, "Smoothed", and pre-populate it with zeros (will eventually store smoothed data)
    response_timeL$smoothed <- 0
    response_timeR$smoothed <- 0
    
    
    # initialize and define some variables for the smoothing loop
    # create a new, placeholder response dataset for left and right, to be populated in the loop with the smoothed NaN-free version. 
    response_timeL_new <- slice(response_timeL, 0) # left ; creates a new dataframe with same columns but 0 rows
    response_timeR_new <- slice(response_timeR, 0) # right
    sanity_counter = 0
    participants <- unique(response_timeL$subID)
    
    # Loop
    for(part in participants){
      #filter response_time dataframes to include only rows for current participant
      curr_frame_L <- response_timeL[response_timeL$subID == part,]
      curr_frame_R <- response_timeR[response_timeR$subID == part,]
      for(tr in unique(curr_frame_L$pair)){
        curr_frame_Lt <- curr_frame_L[curr_frame_L$pair == tr,]
        curr_frame_Rt <- curr_frame_R[curr_frame_R$pair == tr,]
        
        
        #Interpolate non-trailing NaNs in the trial's subset of data using na.spline
        # NOTE: this code prevents extrapolation in na.spline, using strictly interpolation. As a result, we will later be able to simple remove any remaining trailing nans from the data without sacrificing real data.
        len_Lt <- length(curr_frame_Lt$TimeBin)
        len_Rt <- length(curr_frame_Rt$TimeBIn)
        remove_missing_L <- na.spline(curr_frame_Lt$Prop) + 0*na.approx(curr_frame_Lt$Prop, na.rm = FALSE)
        remove_missing_R <- na.spline(curr_frame_Rt$Prop) + 0*na.approx(curr_frame_Rt$Prop, na.rm = FALSE)
        # fix data to prevent interpolation that exceeds bounds of a proportion (i.e. negative vals or vals exceeding 1)
        remove_missing_L[remove_missing_L > 1] <- 1
        remove_missing_L[remove_missing_L < 0] <- 0
        remove_missing_R[remove_missing_R > 1] <- 1
        remove_missing_R[remove_missing_R < 0] <- 0
        
        
        # put the interpolated data (which still contains trailing and leading NaNs) back into curr frame Prop
        if(length(remove_missing_L) == 0 | length(remove_missing_L) == 0){ # if data for entire trial is empty
          # don't put anything back into prop because there's nothing to put back.
        } else {
          curr_frame_Lt$Prop <- remove_missing_L 
          curr_frame_Rt$Prop <- remove_missing_R  
        }
        
        # filter out trailing na's from curr_frame_Lt and Rt- this often WILL change the length of the df
        curr_frame_Lt_trimmed <- curr_frame_Lt[!is.na(curr_frame_Lt$Prop) & !is.nan(curr_frame_Lt$Prop),]
        curr_frame_Rt_trimmed <- curr_frame_Rt[!is.na(curr_frame_Rt$Prop) & !is.nan(curr_frame_Rt$Prop),]
        
        #smooth data for participant-trial using smooth.spline (cubic spline)
        # curr_frame_Lt_trimmed$smoothed <- supsmu(curr_frame_Lt_trimmed$TimeBin, curr_frame_Lt_trimmed$Prop, span = 0.05)$y
        # curr_frame_Rt_trimmed$smoothed <- supsmu(curr_frame_Rt_trimmed$TimeBin, curr_frame_Rt_trimmed$Prop, span = 0.05)$y
        if(nrow(curr_frame_Lt_trimmed)<4){
          curr_frame_Lt_trimmed$smoothed <- curr_frame_Lt_trimmed$Prop #if only one data point, just use raw prop
        } else {
          curr_frame_Lt_trimmed$smoothed <- smooth.spline(curr_frame_Lt_trimmed$TimeBin, curr_frame_Lt_trimmed$Prop, spar = smooth_spar)$y
          curr_frame_Rt_trimmed$smoothed <- smooth.spline(curr_frame_Rt_trimmed$TimeBin, curr_frame_Rt_trimmed$Prop, spar = smooth_spar)$y
        }
        
        #build new response_timeL as a concatenation with the new dataframe built thus far plus the most recent trial's worth   of smoothed, trimmed, interpolated data.
        response_timeL_new <- bind_rows(response_timeL_new, curr_frame_Lt_trimmed)
        response_timeR_new <- bind_rows(response_timeR_new, curr_frame_Rt_trimmed)
      }
    }
    
    
    # Remove wonky window-end timebins with only a few trials total
    # arbitrarily pick at least 10 data points as the cut off to include a timebin
    unique_timebins <- unique(response_timeL_new$TimeBin)
    for(un in unique_timebins){
      if (sum(response_timeL_new$TimeBin==un)<20){
        response_timeL_new <- response_timeL_new %>% 
          filter(!response_timeL_new$TimeBin==un)
      }
      if (sum(response_timeR_new$TimeBin==un)<20){
        response_timeR_new <- response_timeR_new %>% 
          filter(!response_timeR_new$TimeBin==un)
      }
    }
    
    
    # create sub-data frames that only include trials on which doubling was chosen
    
    # chose doubling
    response_timeL_choseXX <- response_timeL_new %>% filter(!response_timeL_new$chosen==0)
    response_timeR_choseXX <- response_timeR_new %>% filter(!response_timeR_new$chosen==0)
    # chose control
    response_timeL_choseXY <- response_timeL_new %>% filter(!response_timeL_new$chosen==1)
    response_timeR_choseXY <- response_timeR_new %>% filter(!response_timeR_new$chosen==1)
    
    
    #------ Analyze smooth data-----
    # mixed effects regression at each time bin, displaying 
    # standard error (shaded) and significant time bins (blue)
    
    #----LEFT LOOKS---------
    
    # conduct mixed effects model, XX - XY, at each time bin - raw proportions
    tb_analysisL_all <- analyze_time_bins(data = response_timeL, 
                                          predictor_column = "TargetC", 
                                          test = "lmer", 
                                          alpha = .05, 
                                          formula = "Prop ~ TargetC + (1 | trial) + (1 | subID)",
                                          p_adjust_method = "none")
    
    # #same analysis using proportions where missing data have been interpolated using na.spline
    # tb_analysisL_interp <- analyze_time_bins(data = response_timeL_new,
    #                                          predictor_column = "TargetC",
    #                                          test = "lmer",
    #                                          alpha = .05, 
    #                                          formula = "Prop ~ TargetC + (1 | trial) + (1 | subID)",
    #                                          p_adjust_method = "none")
    
    #same analysis using smoothed proportions, which were smoothed using Friedman's Super Smoother
    tb_analysisL_smooth <- analyze_time_bins(data = response_timeL_new,
                                             predictor_column = "TargetC",
                                             test = "lmer",
                                             alpha = .05, 
                                             formula = "smoothed ~ TargetC + (1 | trial) + (1 | subID)",
                                             p_adjust_method = "none")
    

    # --- PLOTS
    # raw proportions
    jpeg(file = paste0(rawGraphs, prettyName, "Raw_Left_", windowOfInterest, ".jpeg"))
    print(plot(tb_analysisL_all, 
               type = "estimate") + theme_light() + ylab("Difference in looks (XX-XY)") + xlab("Time (ms)")) 
    dev.off()
             
    
    # smoothed
    jpeg(file = paste0(smoothedGraphs, prettyName, "Smooth_Left_", windowOfInterest, ".jpeg"))
    print(plot(tb_analysisL_smooth, 
               type = "estimate") + 
            theme_light() + 
            # coord_cartesian(ylim = c(-.12, .25)) +
            ylab("Difference in looks (XX-XY)") + 
            xlab("Time (ms)") + 
            theme(axis.text = element_text(size = 10),axis.title=element_text(size=14)))
    dev.off()
    sink(paste0(analysisTables, prettyName, "Output Table_Left_", windowOfInterest, ".csv"))
    print(summary(tb_analysisL_smooth))
    sink()  # returns output to the console
    # summary(tb_analysisL_smooth)
    
    
    #----RIGHT LOOKS---------
    
    tb_analysisR_all <- analyze_time_bins(data = response_timeR, 
                                          predictor_column = "TargetC", 
                                          test = "lmer", 
                                          alpha = .05, 
                                          formula = "Prop ~ TargetC + (1 | trial) + (1 | subID)",
                                          p_adjust_method = "none")
    
    # #same analysis using proportions where missing data have been interpolated using na.spline
    # tb_analysisR_interp <- analyze_time_bins(data = response_timeR_new,
    #                                          predictor_column = "TargetC",
    #                                          test = "lmer",
    #                                          alpha = .05, 
    #                                          formula = "Prop ~ TargetC + (1 | trial) + (1 | subID)",
    #                                          p_adjust_method = "none")
    
    #same analysis using smoothed proportions, which were smoothed using Friedman's Super Smoother
    tb_analysisR_smooth <- analyze_time_bins(data = response_timeR_new,
                                             predictor_column = "TargetC",
                                             test = "lmer",
                                             alpha = .05, 
                                             formula = "smoothed ~ TargetC + (1 | trial) + (1 | subID)",
                                             p_adjust_method = "none")
    # Raw proportions
    jpeg(file = paste0(rawGraphs, prettyName, "Raw_Right_", windowOfInterest, ".jpeg"))
    print(plot(tb_analysisR_all, 
               type = "estimate") + theme_light() + ylab("Difference in looks (XX-XY)") + xlab("Time (ms)"))
    dev.off()
    
    # summary(tb_analysisR_all)
    
    # smoothed
    jpeg(file = paste0(smoothedGraphs, prettyName, "Smooth_Right_", windowOfInterest, ".jpeg")) 
    print(plot(tb_analysisR_smooth, 
               type = "estimate") + 
            theme_light() + 
            # coord_cartesian(ylim = c(-.12, .25)) +
            ylab("Difference in looks (XX-XY)") + 
            xlab("Time (ms)") + 
            theme(axis.text = element_text(size = 10),axis.title=element_text(size=14)))
    dev.off()
    sink(paste0(analysisTables, prettyName, "Output Table_Right_", windowOfInterest, ".csv"))
    print(summary(tb_analysisR_smooth))
    sink()  # returns output to the console
    # summary(tb_analysisR_smooth)
    
    # ---- Analysis looking at trials where participanst chose XX or XY specifically
    # Do analysis by trials where participant chose doubling, and again for trials where participants chose control
    # -- XX
    # Left looks, XX trials only
    tb_analysisL_XX <- analyze_time_bins(data = response_timeL_choseXX,
                                             predictor_column = "TargetC",
                                             test = "lmer",
                                             alpha = .05, 
                                             formula = "smoothed ~ TargetC + (1 | trial) + (1 | subID)",
                                             p_adjust_method = "none")
    jpeg(file = paste0(byWordChosen, prettyName, "looks when XX was chosen_Left_", windowOfInterest, ".jpeg")) 
    print(plot(tb_analysisL_XX, 
               type = "estimate")  + 
            theme_light() + 
            ylab("Difference in looks (XX-XY)") + 
            xlab("Time (ms)") + 
            # coord_cartesian(ylim = c(-.12, .25)) +
            theme(axis.text = element_text(size = 10),axis.title=element_text(size=14)))
    dev.off()
    sink(paste0(analysisTables, prettyName, "looks when XX was chosen_Left_", windowOfInterest, ".csv"))
    print(summary(tb_analysisL_XX))
    sink()  # returns output to the console
    
    # Right looks, XX trials only
    tb_analysisR_XX <- analyze_time_bins(data = response_timeR_choseXX,
                                         predictor_column = "TargetC",
                                         test = "lmer",
                                         alpha = .05, 
                                         formula = "smoothed ~ TargetC + (1 | trial) + (1 | subID)",
                                         p_adjust_method = "none")
    jpeg(file = paste0(byWordChosen, prettyName, "looks when XX was chosen_Right_", windowOfInterest, ".jpeg")) 
    print(plot(tb_analysisR_XX, 
               type = "estimate")  + 
            theme_light() + 
            ylab("Difference in looks (XX-XY)") + 
            # coord_cartesian(ylim = c(-.12, .25)) +
            xlab("Time (ms)") + 
            theme(axis.text = element_text(size = 10),axis.title=element_text(size=14)))
    dev.off()
    sink(paste0(analysisTables, prettyName, "looks when XX was chosen_Right_", windowOfInterest, ".csv"))
    print(summary(tb_analysisR_XX))
    sink()  # returns output to the console
    
    # -- XY
    # Left looks, XY trials only
    tb_analysisL_XY <- analyze_time_bins(data = response_timeL_choseXY,
                                         predictor_column = "TargetC",
                                         test = "lmer",
                                         alpha = .05, 
                                         formula = "smoothed ~ TargetC + (1 | trial) + (1 | subID)",
                                         p_adjust_method = "none")
    jpeg(file = paste0(byWordChosen, prettyName, "looks when XY was chosen_Left_", windowOfInterest, ".jpeg")) 
    print(plot(tb_analysisL_XY, 
               type = "estimate")  + 
            theme_light() + 
            ylab("Difference in looks (XX-XY)") + 
            # coord_cartesian(ylim = c(-.12, .25)) +
            xlab("Time (ms)") + 
            theme(axis.text = element_text(size = 10),axis.title=element_text(size=14)))
    dev.off()
    sink(paste0(analysisTables, prettyName, "looks when XY was chosen_Left_", windowOfInterest, ".csv"))
    print(summary(tb_analysisL_XY))
    sink()  # returns output to the console
    
    # Right looks, XY trials only
    tb_analysisR_XY <- analyze_time_bins(data = response_timeR_choseXY,
                                         predictor_column = "TargetC",
                                         test = "lmer",
                                         alpha = .05, 
                                         formula = "smoothed ~ TargetC + (1 | trial) + (1 | subID)",
                                         p_adjust_method = "none")
    jpeg(file = paste0(byWordChosen, prettyName, "looks when XY was chosen_Right_", windowOfInterest, ".jpeg")) 
    print(plot(tb_analysisR_XY, 
               type = "estimate")  + 
            theme_light() + 
            # coord_cartesian(ylim = c(-.12, .25)) +
            ylab("Difference in looks (XX-XY)") + 
            xlab("Time (ms)") + 
            theme(axis.text = element_text(size = 10),axis.title=element_text(size=14)))
    dev.off()
    sink(paste0(analysisTables, prettyName, "looks when XY was chosen_Right_", windowOfInterest, ".csv"))
    print(summary(tb_analysisR_XY))
    sink()  # returns output to the console
    
    # # -- OUT OF CURIOSITY
    # # Does proportion looking at XX at each time point predict which word participants will choose?
    # analysis_lookPredictChoice_left <- analyze_time_bins(data = response_timeL_new,
    #                                      predictor_column = "intercept",
    #                                      test = "glmer",
    #                                      family = "binomial",
    #                                      alpha = .05,
    #                                      formula = "chosen ~ TargetC + Prop + (1 | trial) + (1 | subID)",
    #                                      p_adjust_method = "none")
    # jpeg(file = paste0(byWordChosen, prettyName, "chosen ~ TargetC + Prop_Left", windowOfInterest, ".jpeg"))
    # print(plot(analysis_lookPredictChoice_left,
    #            type = "estimate")  +
    #         theme_light() +
    #         ylab("Difference in looks (XX-XY)") +
    #         xlab("Time (ms)") +
    #         theme(axis.text = element_text(size = 10),axis.title=element_text(size=14)))
    # dev.off()
    # sink(paste0(analysisTables, prettyName, "chosen ~ TargetC + Prop_Left", windowOfInterest, ".txt"))
    # print(summary(analysis_lookPredictChoice_left))
    # sink()  # returns output to the console
    # 
    # analysis_lookPredictChoice_right <- analyze_time_bins(data = response_timeR_new,
    #                                                      predictor_column = c("TargetC"),
    #                                                      test = "glmer",
    #                                                      family = "binomial",
    #                                                      alpha = .05, 
    #                                                      formula = "chosen ~ TargetC + Prop + (1 | trial) + (1 | subID)",
    #                                                      p_adjust_method = "none")
    # jpeg(file = paste0(byWordChosen, prettyName, "chosen ~ TargetC + Prop_Right", windowOfInterest, ".jpeg"))
    # print(plot(analysis_lookPredictChoice_right,
    #            type = "estimate")  +
    #         theme_light() +
    #         ylab("Difference in looks (XX-XY)") +
    #         xlab("Time (ms)") +
    #         theme(axis.text = element_text(size = 10),axis.title=element_text(size=14)))
    # dev.off()
    # sink(paste0(analysisTables, prettyName, "chosen ~ TargetC + Prop_Left", windowOfInterest, ".txt"))
    # print(summary(analysis_lookPredictChoice_right))
    # sink()  # returns output to the console
    
    # ---- CLUSTER ANALYSIS --------------------
    # Determine appropriate parameters for cluster-based analysis
    # num_sub = length(unique((response_window_clean_Left$subID)))
    # # num_timeBins = length(unique((response_timeL_new$TimeBin)))
    # num_timeBins = max(response_timeL_new$TimeBin)
    # threshold_t_Left = matrix(0, num_timeBins, 1)
    # threshold_t_Right = matrix(0, num_timeBins, 1)
    # t_counter = 1
    
    library("lmerTest")
    # for (tl in c(curr_frame_Lt$TimeBin[1]:curr_frame_Lt$TimeBin[num_timeBins])){
    #   # left
    #   left_words_Model <- lmer(smoothed ~ TargetC + (1 | pair) + (1 | subID), data = response_timeL_new[response_timeL_new$TimeBin == (tl),], REML = FALSE)
    #   # extract df for desired effect
    #   c_Left <- coef(summary(left_words_Model, ddf = "Satterthwaite"))
    #   df_Left <- c_Left[2, 3] # extract third column (df column), second row (TargetC effect)
    #   # threshold_t = qt(p = 1 - .05/2,
    #   #                  df = num_sub - 1) # pick threshold t based on alpha = .05 two tailed.
    #   threshold_t_Left[t_counter,1] = qt(p = 1 - .05/2,
    #                                 df = df_Left) # pick threshold t based on alpha = .05 two tailed.
    #   
    #   # right
    #   right_words_Model <- lmer(smoothed ~ TargetC + (1 | pair) + (1 | subID), data = response_timeR_new[response_timeR_new$TimeBin == (tl),], REML = FALSE)
    #   # extract df for desired effect
    #   c_Right <- coef(summary(right_words_Model, ddf = "Satterthwaite"))
    #   df_Right <- c_Right[2, 3] # extract third column (df column), second row (TargetC effect)
    #   # threshold_t = qt(p = 1 - .05/2,
    #   #                  df = num_sub - 1) # pick threshold t based on alpha = .05 two tailed.
    #   threshold_t_Right[t_counter,1] = qt(p = 1 - .05/2,
    #                                  df = df_Right) # pick threshold t based on alpha = .05 two tailed.
    #   
    #   t_counter = t_counter+1
    # }
    
    
    # ---FIND INITIAL CLUSTERS FOR CLUSTER ANALYSIS
    # df_timeclustL <- make_time_cluster_data(response_timeL_new,
    #                                         predictor_column = 'TargetC',
    #                                         test = "lmer",
    #                                         threshold = max(threshold_t_Left),
    #                                         formula = "smoothed ~ TargetC + (1 | trial) + (1 | subID)")
    # jpeg(file = paste0("C:/Users/qathy/OneDrive/Documents/Matlab_and_R/analysis_files/results from big analysis/candidate clusters_left_",
    #                    prettyName, ".jpeg"))
    # print(plot(df_timeclustL) + ylab("mixed effects estimate statistic, XX-XY") + theme_light())
    # dev.off()
    # # summary(df_timeclustL)
    # 
    # 
    # df_timeclustR <- make_time_cluster_data(response_timeR_new,
    #                                         predictor_column = 'TargetC',
    #                                         test = "lmer",
    #                                         threshold = max(threshold_t_Right),
    #                                         formula = "smoothed ~ TargetC + (1 | trial) + (1 | subID)")
    # 
    # jpeg(file = paste0("C:/Users/qathy/OneDrive/Documents/Matlab_and_R/analysis_files/results from big analysis/candidate clusters_right_",
    #                    prettyName, ".jpeg"))
    # print(plot(df_timeclustR) + ylab("mixed effects estimate statistic, XX-XY") + theme_light())
    # dev.off()
    # # summary(df_timeclustR)
    # 
    # # Generate a null distribution using blindly shuffled data, compare obtained divergences to this null
    # # left
    # 
    # clust_analysisL <- analyze_time_clusters(df_timeclustL,
    #                                          within_subj=TRUE,
    #                                          formula = "smoothed ~ TargetC + (1 | trial) + (1 | subID)",
    #                                          shuffle_by = 'Target',
    #                                          quiet = TRUE,
    #                                          samples=1000)
    # jpeg(file = paste0("C:/Users/qathy/OneDrive/Documents/Matlab_and_R/analysis_files/results from big analysis/shuffled dist_left",
    #                    prettyName, ".jpeg"))
    # print(plot(clust_analysisL) +  theme_light() + ylab("Frequency") + xlab("Sum statistic"))
    # dev.off()
    # 
    # clustL_sum <- summary(clust_analysisL)
    # 
    # 
    # clust_analysisR <- analyze_time_clusters(df_timeclustR,
    #                                          within_subj=TRUE,
    #                                          formula = "smoothed ~ TargetC + (1 | trial) + (1 | subID)",
    #                                          shuffle_by = 'Target',
    #                                          quiet = TRUE,
    #                                          samples=1000)
    # jpeg(file = paste0("C:/Users/qathy/OneDrive/Documents/Matlab_and_R/analysis_files/results from big analysis/shuffled dist_right",
    #                    prettyName, ".jpeg"))
    # print(plot(clust_analysisR) + theme_light() + ylab("Frequency") + xlab("Sum statistic"))
    # dev.off()
    # 
    # clustR_sum <- summary(clust_analysisR)
    # #
    # tryCatch({
    #   sink(paste0("C:/Users/qathy/OneDrive/Documents/Matlab_and_R/analysis_files/results from big analysis/cluster results left_", prettyName, ".txt"))
    #   print(summary(clustL_sum))
    #   sink()  # returns output to the console
    #   sink(paste0("C:/Users/qathy/OneDrive/Documents/Matlab_and_R/analysis_files/results from big analysis/cluster results right_", prettyName, ".txt"))
    #   print(summary(clustR_sum))
    #   sink()  # returns output to the console
    # 
    # }, error=function(e){})
  }
}
