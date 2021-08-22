% Assumes hctsa is in the path (run 'startup' from hctsa main directory)
​
% File names containing time series

fileNamesToRead = {'/Users/trenthenderson/Documents/Git/feature-set-comp/data/sims/hctsa/100_1.mat','/Users/trenthenderson/Documents/Git/feature-set-comp/data/sims/100_2.csv','/Users/trenthenderson/Documents/Git/feature-set-comp/data/sims/100_3.csv','/Users/trenthenderson/Documents/Git/feature-set-comp/data/sims/100_4.csv','/Users/trenthenderson/Documents/Git/feature-set-comp/data/sims/100_5.csv','/Users/trenthenderson/Documents/Git/feature-set-comp/data/sims/250_1.csv','/Users/trenthenderson/Documents/Git/feature-set-comp/data/sims/250_2.csv','/Users/trenthenderson/Documents/Git/feature-set-comp/data/sims/250_3.csv','/Users/trenthenderson/Documents/Git/feature-set-comp/data/sims/250_4.csv','/Users/trenthenderson/Documents/Git/feature-set-comp/data/sims/250_5.csv','/Users/trenthenderson/Documents/Git/feature-set-comp/data/sims/500_1.csv','/Users/trenthenderson/Documents/Git/feature-set-comp/data/sims/500_2.csv', '/Users/trenthenderson/Documents/Git/feature-set-comp/data/sims/500_3.csv','/Users/trenthenderson/Documents/Git/feature-set-comp/data/sims/500_4.csv','/Users/trenthenderson/Documents/Git/feature-set-comp/data/sims/500_5.csv','/Users/trenthenderson/Documents/Git/feature-set-comp/data/sims/750_1.csv','/Users/trenthenderson/Documents/Git/feature-set-comp/data/sims/750_2.csv', '/Users/trenthenderson/Documents/Git/feature-set-comp/data/sims/750_3.csv','/Users/trenthenderson/Documents/Git/feature-set-comp/data/sims/750_4.csv','/Users/trenthenderson/Documents/Git/feature-set-comp/data/sims/750_5.csv','/Users/trenthenderson/Documents/Git/feature-set-comp/data/sims/1000_1.csv','/Users/trenthenderson/Documents/Git/feature-set-comp/data/sims/1000_2.csv', '/Users/trenthenderson/Documents/Git/feature-set-comp/data/sims/1000_3.csv','/Users/trenthenderson/Documents/Git/feature-set-comp/data/sims/1000_4.csv','/Users/trenthenderson/Documents/Git/feature-set-comp/data/sims/1000_5.csv'};
​
% Set up feature information

Operations = SQL_Add('ops','INP_ops.txt',false,false);
MasterOperations = SQL_Add('mops','INP_mops.txt',false,false);
​
% Loop over files and track computation time

numFiles = length(fileNamesToRead);
timeTaken = nan(numFiles,1);
for i = 1:numFiles
    X = dlmread(fileNamesToRead{i});
    tic;
    TS_CalculateFeatureVector(X,false,Operations,MasterOperations,false,false);
    timeTaken(i) = toc;
end
​
csvwrite('/Users/trenthenderson/Documents/Git/feature-set-comp/output/comptime/outputTimes.csv','timeTaken');