# Problem setting 

*This is an R package for feature alignment issues in vertical federated learning, we named it "FeatureAlignment"*

## FeatureAlignment will help you:

### Blocking Functions: 
**BaseBlock**  
Automatically block the input dataset into five different clusters, which are "unmatchable", "int", "double", "time", and "string". 

**AdvancedBlock**  
Take "int", "double", "string" data as input, and return "binary", "categorical_not_binary", and "neither" (Purely Numeric) features.

Blocking functions can reduce the complexity of the algorithm, because the most of time only "binary" features and "binary" features but not "binary" features and "string" features will match each other. We do not want to go through all the features in the dataset.

Moreover, it can help us customize different matching algorithms for different data structures, hence improving the accuracy of the alogorithm.


