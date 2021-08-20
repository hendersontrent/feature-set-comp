% Assumes hctsa is in the path (run 'startup' from hctsa main directory)
​
% TRENT TO FILL:

fileNamesToRead = {'file1.csv','file2.csv',};
​
% Set up feature information:

Operations = SQL_Add('ops','INP_ops.txt',false,false);
MasterOperations = SQL_Add('mops','INP_mops.txt',false,false);
​
% Loop over files:

numFiles = length(fileNamesToRead);
timeTaken = nan(numFiles,1);
for i = 1:numFiles
    X = dlmread(fileNamesToRead{i});
    tic;
    TS_CalculateFeatureVector(X,false,Operations,MasterOperations,false,false);
    timeTaken(i) = toc;
end
​
csvwrite('outputTimes.csv','timeTaken');