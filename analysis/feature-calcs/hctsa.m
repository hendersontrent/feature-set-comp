% Load in data and initiate hctsa calculation

TS_Init('/Users/trenthenderson/Documents/Git/feature-set-comp/data/empirical1000.mat','INP_mops.txt','INP_ops.txt');

% Run main computation using multiple cores in parallel

TS_Compute(true);