# Feature Alignment R Package
*This is an R package for feature alignment issues in vertical federated learning, we named it "FeatureAlignment"*

## Key functions in FeatureAlignment:

### Blocking Functions: 
**BaseBlock()**  
Automatically block the input dataset into five different clusters, which are "Unmatchable", "Int", "Double", "Time", and "String". 

**AdvancedBlock()**  
Take "int", "double", "string" data as input, and return "Binary", "Categorical_not_binary", and "Neither" (Purely Numeric) features.

Blocking functions can reduce the complexity of the algorithm, because the most of time only "binary" features and "binary" features but not "binary" features and "string" features will match each other. We do not want to go through all the features in the dataset.

Moreover, it can help us customize different matching algorithms for different data structures, hence improving the accuracy of the alogorithm.

### Comparison Functions: 
We explored Wasserstein distance, KS (Kolmogorov–Smirnov) test, KL (Kullback-Leibler) divergency, JS (Jensen-Shannon) divergency, in which KS test gives us the best result. Therefore, we mainly use KS test to do the numeric matching works.

For the string matching issue, we use cosine similarity as our metric.

**KSFeatureComparison()**
This function takes two numerical vectors and one optional argument "input sample size" (for sampling). Output is a graph of their density plots and returns the result of KS test.

**AutoMatching()**
This function returns the ultimate feature-wise matching result for two input datasets. Features will be blocked first (automatically) and then do the matching. Users may customize the function by specifying the optional parameters (sample size numeric, sample size string, and characters to match) to increase matching accuracy.

## Team
- Jialong Li, UIUC  
- Zekun Li, UIUC

