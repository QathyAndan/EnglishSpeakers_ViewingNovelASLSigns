function [ frame ] = genDataFrame( exp, folder, for_secs, loss_method, screenW, screenH, allowed_error )
%frame = genDataFrame(condition to analyze, folder with data,
%secs to include, loss handling, tolerance factor (to be multiplied by 
%pixPerDeg).
%   7/11/18 -- created after first meeting with Inder
%   Generates a data frame of raw, sample-by-sample data for all trials and
%   subjects. This frame will be used to conduct analysis using a r pipeline
%   data processing method.
%   Plan: build each variable in table separately, then create table at the end.
%   It will generate this frame by compiling data for all participants'
%   data in supplied folder, according to the protocol specified by exp,
%   for_secs, loss_method, and desired tolerance (allowed error).
%   --- inputs ---
%   ::exp -- string, either 'Single', 'Plural', or 'Illicit'
%   ::folder -- absolute file path specifying folder containing participants'
%   data, default is current working directory
%   ::for_secs -- how many seconds worth of data (up to 5) to include in
%   analysis for each trial. If string 'All' is supplied, use all data.
%   ::loss_method -- string, either 'Use_Both' (takes average of two eyes,
%   and if either eye is missing it returns NaN) or 'Use_Any' (takes
%   average if data for both eyes is present, otherwise uses single eye's
%   data if only one eye, and if both eyes are missing it returns NaN).
%   ::allowed_error -- tolerance / error allowed in pixels.

% save initial/starting file path (will return here at function's end)
oldPath = pwd;
% save contents of folder to myFiles; will use to run ET analysis files and
% also to extract subject IDs.
myFiles = dir(folder); % store contents of folder in myFiles
myFiles = myFiles(arrayfun(@(x) x.name(1), myFiles) ~= '.'); % strip '.'s
sampleSize = length(myFiles); % # participants = number of data files
subjectIDs = cell(sampleSize, 1);

switch exp
    case 'Single'
        nTrials = 60;
    case 'Plural'
        nTrials = 30;
    case 'Illicit'
        nTrials = 30;
    otherwise
        error('Exp (first input) must be either ''Single'', ''Plural'', or ''Illicit''');
end

allFlag = false;
if strcmp(for_secs, 'All')
    allFlag = true;
else
    timeCutOff = round(for_secs * 60); % cut off in samples (using sampling rate)
end

% initialize variable that will be used to pre-allocate appropriate amount
% of memory to the data frame variables.
preAllocator = 0;

% determine how much memory to set aside for frame variables
% note: Time is a variable imported from participant data
for s = 1:sampleSize
    % cd to folder containing participant data
    cd(folder);
    % load current subject's data
    load(myFiles(s).name);
    for n = 1:nTrials
        cd(oldPath);
        % clean the "time line" of eye samples for current trial
        tl = inpaint_nans(Time{1, n}, 1);
        tl = tZeroConverter_firstnotzero(tl); % differs from how I did it originally (before, first index = 0)
        lenTl = length(tl);
        if allFlag % if string "All" was supplied to for_secs
            timeCutOff = lenTl; % set timeCutOff = to total data
        end
        if isnan(tl) % if no data collected for entire trial
            % skip trial's data
        else
            for t = 1:min(lenTl, timeCutOff);
                preAllocator = preAllocator + 1;
            end
        end
    end
end

subCol = cell(preAllocator, 1);
trialCol = zeros(preAllocator, 1);
timeStampCol = zeros(preAllocator, 1);
aoi_leftCol = zeros(preAllocator, 1);
aoi_rightCol = zeros(preAllocator, 1);
aoi_objectCol = zeros(preAllocator, 1);
aoi_otherCol = zeros(preAllocator, 1);
tracklossCol = zeros(preAllocator, 1);
targetSideCol = zeros(preAllocator, 1);

% initialize variable that will store "current row" of data frame as
% variables are created.
frameRow = 0;

% loop to generate frame variables
for subjectNum = 1:sampleSize
    cd(folder);
    % load current subject's data
    load(myFiles(subjectNum).name);
    subjectIDs{subjectNum, 1} = myFiles(subjectNum).name(1, 1:11);
    for trialNo = 1:nTrials
        cd(oldPath);
        % clean the "time line" of eye samples for current trial
        timeL = inpaint_nans(Time{1, trialNo}, 1);
        timeL = tZeroConverter_firstnotzero(timeL); % differs from how I did it originally (before, first index = 0)
        lenTimeL = length(timeL);
        if allFlag % if string "All" was supplied to for_secs
            timeCutOff = lenTimeL; % set timeCutOff = to total data
        end
        if isnan(timeL) % if no data collected for entire trial
            % skip trial's data
        else
            for t = 1:min(lenTimeL, timeCutOff); % loop until end of data or time cut off
                frameRow = frameRow + 1; % increment current row for all variables
                %% Basic data specifier variables (sub, trial, time stamp)
                % subject ID
                subCol{frameRow, 1} = subjectIDs{subjectNum, 1};
                % trial number
                trialCol(frameRow, 1) = trialNo;
                % time stamp
                timeStampCol(frameRow, 1) = timeL(t);
                
                
                %% AOI determination -- assign time point an AOI
                % Note: LeftEye and RightEye are imported from participant
                % data.
                % left eye data for current eye position
                leftXData = LeftEye{1, trialNo}(t,1);
                leftYData = LeftEye{1, trialNo}(t,2);
                % right eye data for current eye position
                rightXData = RightEye{1, trialNo}(t,1);
                rightYData = RightEye{1, trialNo}(t,2);
                
                % Determine gaze according to specified track loss method
                % % -- if 'Use_Both' was chosen, only count data if both
                % eyes had data for a given time point. Otherwise gaze = NaN.
                % % -- if 'Use_Any' was chosen as track loss method, count
                % data if either eye had data for a given time point. Else,
                % gaze = NaN.
                % % -- if no data for either eye for given time point,
                % regardless of track loss method, gaze = NaN.
                if and(~isnan(leftXData), ~isnan(rightXData)) % if both eyes have data
                    % 1) data for both left and right eye exist;
                    % take the average X and Y coordinates of two eye positions
                    gazeX = (leftXData + rightXData) / 2*screenW; % work out mean position of left and right eye x position, scale for screen width
                    gazeY = (leftYData + rightYData) / 2*screenH;  % work out mean position of left and right eye y position, scale for screen height
                elseif and(~isnan(leftXData), isnan(rightXData))
                    % 2) Data exist for left eye (isn't nan) but no data exists for right eye (is nan)
                    switch loss_method
                        case 'Use_Both' % data only counts if both eyes have data
                            % Here, since only the left eye has data, we
                            % will treat the data as if it is lost.
                            gazeX = NaN;
                            gazeY = NaN;
                        case 'Use_Any' % data counts if either eye has data
                            % use left eye coordinates, scaled for screen width and height
                            gazeX = leftXData*screenW;
                            gazeY = leftYData*screenH;
                    end
                elseif and(isnan(leftXData), ~isnan(rightXData))
                    % 3) Data exist for right eye (isn't nan) but not for left
                    switch loss_method
                        case 'Use_Both' % data only counts if both eyes have data
                            % since only the right eye has data, treat data
                            % as if it is lost.
                            gazeX = NaN;
                            gazeY = NaN;
                        case 'Use_Any' % data counts if either eye has data
                            % use right eye coordinates, scaled for screen width
                            % and height
                            gazeX = rightXData*screenW;
                            gazeY = rightYData*screenH;
                    end
                elseif and(isnan(leftXData), isnan(rightXData))
                    % 4) No data for either eye exists (both are nans);
                    gazeX = NaN;
                    gazeY = NaN;
                end
                
                % Classify data according to its AOI.
                % Note: rightTextBounds, leftTextBounds, and onScreenRects
                % are loaded in from loading participant data
                if or(isnan(gazeX), isnan(gazeY)) % if gaze X & Y are NaN for this data point
                    %% FIX LOST
                    aoi_leftCol(frameRow, 1) = 0;
                    aoi_rightCol(frameRow, 1) = 0;
                    aoi_objectCol(frameRow, 1) = 0;
                    aoi_otherCol(frameRow, 1) = 0;
                    tracklossCol(frameRow, 1) = 1;
                else
                    if isPtInRect(gazeX, gazeY, rightTextBounds(trialNo, :), allowed_error)
                        %% LOOKED AT RIGHT WORD
                        aoi_leftCol(frameRow, 1) = 0;
                        aoi_rightCol(frameRow, 1) = 1;
                        aoi_objectCol(frameRow, 1) = 0;
                        aoi_otherCol(frameRow, 1) = 0;
                        tracklossCol(frameRow, 1) = 0;
                    elseif isPtInRect(gazeX, gazeY, leftTextBounds(trialNo, :), allowed_error)
                        %% LOOKED AT LEFT WORD
                        aoi_leftCol(frameRow, 1) = 1;
                        aoi_rightCol(frameRow, 1) = 0;
                        aoi_objectCol(frameRow, 1) = 0; 
                        aoi_otherCol(frameRow, 1) = 0;
                        tracklossCol(frameRow, 1) = 0;
                    elseif isPtInRect(gazeX, gazeY, onScreenRects, allowed_error)
                        %% LOOKED AT OBJECT
                        aoi_leftCol(frameRow, 1) = 0;
                        aoi_rightCol(frameRow, 1) = 0;
                        aoi_objectCol(frameRow, 1) = 1; 
                        aoi_otherCol(frameRow, 1) = 0;
                        tracklossCol(frameRow, 1) = 0;
                    else
                        %% LOOKED AT SOMETHING ELSE
                        aoi_leftCol(frameRow, 1) = 0;
                        aoi_rightCol(frameRow, 1) = 0;
                        aoi_objectCol(frameRow, 1) = 0; 
                        aoi_otherCol(frameRow, 1) = 1;
                        tracklossCol(frameRow, 1) = 0;
                    end
                end
                % target side (from word sequence)
                targetSideCol(frameRow, 1) = wordSeq(trialNo);
            end
        end
    end
end

% navigate to folder
% calculate sampleSize as number of data files in folder
% Build frame to store variables (table)
frame = table;
frame.subID = subCol; % long column, with the subID for sub 1 listed nTrial
%times, followed by the subID for sub 2 listed nTrial times; etc.
frame.trial = trialCol;
frame.time = timeStampCol;
frame.left = aoi_leftCol;
frame.right = aoi_rightCol;
frame.object = aoi_objectCol;
frame.other = aoi_otherCol;
frame.trackloss = tracklossCol;
frame.targetSide = targetSideCol;
% return to original path
cd(oldPath);
    
end

