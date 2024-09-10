function [subID, respCompiled, ts_Data, tl_Data] = genMatFromDat(sNum, numTrials, rawfileList, insertFolder, mDataFolder, experimentName, tcu)
%generate .mat file using data from post_R files and inserts.
% respCompiled - the table containing the complete data for current sub.
% ts_Data - the time series data for the current sub.
% tl_Data - the trial level data for the current sub.
% tcu - stands for time conversion unit. e.g. a tcu of 1000 can be used to convert milliseconds to seconds.
% On the other hand, a tcu of 1 can be used to keep the numbers the same.

subID = string(extractBetween(rawfileList(sNum).name, 'subject_', '.xlsx'));
matFileName = sprintf('SubjectVariables_%s.mat', subID); % file used to store matlab variables
% info from data files
ts_Data = readtable(rawfileList(sNum).name);
tl_Data = ts_Data(ts_Data.time_index == 0, :); % trial level data (data where
% time_index == 0, i.e. the first data point for each trial which includes all relevant trial level variables.

% info from inserts
trials_block1 = readtable(sprintf('%s\\%s_block1.csv', insertFolder, experimentName)); % insert including: itemNumber, itemName, block, quartetNumber,
% doublingResponse, movieName, base, audio
trials_block2 = readtable(sprintf('%s\\%s_block2.csv', insertFolder, experimentName));
trials = [trials_block1; trials_block2];
% Note: will not need to load in each block separately and compile them,
% can probably do both blocks at once. May need to modify analysis script
% accordingly.

% 1. Variables I will need to put into respCompiled
% itemNames = ; % column from insert, can't likely get it from labvanced. WILL USE AS ROW LABELS
doublingResponse = tl_Data.doublingResponse; % 1 = left, 2 = right
sideChosen = tl_Data.whichArrow; % 1 = left, 2 = right
% typeChosen = ~strcmp(tl_Data.choseDoubling, 'FALSE'); % will return 1 = if chose doubling, 0 = chose control
typeChosen = tl_Data.choseDoubling;
% will become expOrder in respCompiled; corresponds to trial Number in labvanced
blocks = tl_Data.Block_Nr; % called "block" in resp; if trial_Group = "block_1_trials", 1, else 2.
movieNameVect = trials.movieName; % in resp, movieName; names of movie files, likely from insert.
audioChooseSign = trials.audio; % from insert; will be imported into respCompiled as column labeled "audio"

% KEEP ALL UNIX TIMESTAMPS AS MILLISECONDS Except for reaction time and vidViewingTime- 8/6/2020
itemNames = trials.itemName;
% Time variables (cell arrays containing all eye movement data within each
% trial)

% loop preallocation
Time = cell(numTrials, 1);
timeIndices = cell(numTrials, 1);
timeNumDataPts = zeros(numTrials, 1);
gazeData = cell(numTrials, 1);
vidStartTimes = zeros(numTrials, 1);
vidEndTimes = zeros(numTrials, 1);
trialStart = zeros(numTrials, 1);
trialEnd = zeros(numTrials, 1);
reactionTimes = zeros(numTrials, 1);
videoPlayNum = zeros(numTrials, 1);
randOrder = zeros(numTrials, 1);

% loop through trials for all gaze data and time stamp variables.
for tr = 1:numTrials
    % gaze data - extracted from time series level dataset
    rel_Data = ts_Data(ts_Data.Trial_Id == tr, :); % for each tr in nTrials, subset ts_Data according to trial id that matches current tr
    Time{tr, 1} = rel_Data.times; % cell array; KEEP AS MILLISECONDS as of 8/6/2020
    % LeftEye / RightEye = ; % leftData, rightData not applicable in new code-- will instead have gazeData
    timeIndices{tr, 1}(:, 1) = rel_Data.time_index; % in resp, timeData "times" from labvanced post_r file
    timeNumDataPts(tr, 1) = length(rel_Data.time_index); % store the length of the data for curr item
    gazeData{tr, 1}(:, 1) = rel_Data.eyeX;
    gazeData{tr, 1}(:, 2) = rel_Data.eyeY;
    
    % other time data - extracted from trial level dataset
    selectorArray = tl_Data.Trial_Id==tr; % use this to select only the single value relevant to the current trial ID.
    vidStartTimes(tr, 1) = tl_Data.vidStartTime(selectorArray, 1);
    vidEndTimes(tr, 1) = tl_Data.videoEndTime(selectorArray, 1);
    trialStart(tr, 1) = tl_Data.audioStartTime(selectorArray, 1);
    trialEnd(tr, 1) = str2double(tl_Data.trialEndTime(selectorArray, 1)); % For some reason, after changing the r code based on fred's loop this variable became a cell array containing strings
    reactionTimes(tr, 1) = tl_Data.reactionTime(selectorArray, 1) / tcu;
    videoPlayNum(tr, 1) = tl_Data.videoPlays(selectorArray, 1);
    
    % randOrder
    randOrder(tr, 1) = tl_Data.Trial_Nr(tl_Data.Trial_Id==tr, 1); % WARNING: saved as a 1x32 row in matlab save file for some reason;

end

totalVidViewingTime = (vidEndTimes - vidStartTimes) / tcu; 
clear rel_Data tr selectorArray

%% generate respCompiled
respCompiled = table();
respCompiled.doublingResponse = doublingResponse;
respCompiled.sideChosen = sideChosen;
respCompiled.typeChosen = typeChosen;
respCompiled.expOrder = randOrder;
respCompiled.reactionTime = reactionTimes;
% --> will need to loop trial by trial
respCompiled.gazeData = gazeData; % note: different from leftData and rightData, will need to adapt code.
respCompiled.timeData = Time;
respCompiled.numDataPts = timeNumDataPts;
%------
respCompiled.block = blocks;
respCompiled.currItem = randOrder;
respCompiled.itemName = itemNames;
respCompiled.movieName = movieNameVect;
respCompiled.audioName = audioChooseSign;
respCompiled.trialStart = trialStart;
respCompiled.trialEnd = trialEnd;
respCompiled.vidViewingStart = vidStartTimes;
respCompiled.vidViewingEnd = vidEndTimes;
respCompiled.vidViewingTime = totalVidViewingTime;
respCompiled.videoPlays = videoPlayNum;
% code to use itemNames as row labels
respCompiled.Properties.RowNames = itemNames;
%% 2. Other variables I MIGHT need, just in case
vidFrameRect = [256, 103, 794, 428]; % entire video rect, not useful for analysis but good to know.
frameHeight = 325; %
frameWidth = 265; %

% AOI COORDINATES - based on left_AOI_h, _w, etc. from trial level
% data.
leftRectShifted = [256, 103, 521, 428]; % rect points A, B, C and D of left video -
rightRectShifted = [529, 103, 794, 428]; % rect points of right video
leftRectX = 256; % X coordinate of top left corner.
rightRectX = 529; % upper left corner
% "Showing my work":
% (256, 103)______(256+265, 103)      (529, 103)________(529+265, 103)
% |                             |       |                           |
% |                             |       |                           |
% |                             |       |                           |
% (256, 103+325)__(256+265, 103+325)  (529, 103+325)____(529+265, 103+325)
% Thus, according to above calculations, left rect = [256, 103, 521, 428] and
% right rect = [529, 103, 794, 428].

%     videoAOIheight = ;
%     videoAOIwidth = ;
%     vidFrameRect = ;


%% Save everything %%%%%%%%%%%%%%%%%%%%%%%%
%
% save variables for current subject in matlab data folder.
cd(mDataFolder);
save(fullfile(mDataFolder, matFileName));

end

