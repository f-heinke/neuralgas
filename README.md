# neuralgas
This R packages provides a straightforward implementation of the Neural Gas algorithm proposed by Klaus Schulten and Thomas Martinetz. See Martinetz, T., & Schulten, K. (1991). *A" neural-gas" network learns topologies* for further details.

## Installation 

Since this package is not on CRAN, use devtools to install the latest version.

```{R}
devtools::install_github("https://github.com/f-heinke/neuralgas")
```

## How to use

To run the algorithm, a data matrix of N observations with n features is needed as input.

```{R}
library("neuralgas")
ng_result <- neuralgas( scale(X[, 1:4])
print( ng_result ) 
```

See the documentation for how to specify the number of prototypes or initial prototype data, or how to provide non-euclidean dissimilarity computations and update rules.
