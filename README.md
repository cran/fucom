FUCOM: Full Consistency Method 

This package provides an R implementation of the FUCOM method. 
The model implies the definition of two groups of constraints that need to satisfy the optimal values of weight coefficients.
After defining the constraints and solving the model, in addition to optimal weight values, a deviation from full consistency (DFC) is obtained.

The main advantages of FUCOM in relation to the existing multi-criteria decision-making (MCDM) methods are as follows: 
(1) a significantly smaller number of pairwise comparisons (only n − 1), 
(2) a consistent pairwise comparison of criteria, and 
(3) the calculation of the reliable values of criteria weight coefficients, which contribute to rational judgment.


The goal is to determine the weights of criteria such that the deviation from full consistency (DFC) is minimized method. 
the Full Consistency Method (FUCOM) for multi-criteria decision making create by Dragam Pamucar (2018).
Given a character vector specifying the rank of each criterion.A character vector specifying the ranking of each criterion.
The ranking is performed according to the significance of the criteria,  starting from the criterion that is expected to have the highest weight coefficient to the criterion of the least significance.
Given numeric vector specifying the priority values of each criterion. The comparison is made with respect to the first-ranked (the most significant) criterion.

The the function return the optimized weights for each criterion, summing to 1, the comparative priority (Phi) values, 
the condition of mathematical transitivity (w) value and the minimum deviation from full consistency (DFC) value.

More information about the method can be found at <https://doi.org/10.3390/sym10090393>. 
More information about the implementation at https://github.com/mateusvanzetta/fucom.

Installation:

Option 1: From CRAN

install.packages("fucom") library(fucom)

Option 2: From GitHub
# Install devtools if you haven't already
install.packages("devtools")

# Install rfucomr from GitHub
devtools::install_github("mateusvanzetta/fucom")

Example Calculation Using Data from DOI Reference

Here’s how you can use the FUCOM method with sample data:

# Load the rfucomr package
library(fucom)

# Define the criteria rank and priority
criteria_rank <- c("Criterion 1", "Criterion 2", "Criterion 3", "Criterion 4", "Criterion 5", "Criterion 6", "Criterion 7", "Criterion 8")
criteria_priority <- c(1, 1, 1, 2, 4, 4, 4, 4)

# Call the FUCOM method
results <- fucom_method(criteria_rank, criteria_priority)

# Print the results
print(results$weights)  # Optimized weights for each criterion
print(results$Phi)      # Comparative priority values
print(results$w)        # Condition of mathematical transitivity values
print(results$DFC)      # Minimum deviation from full consistency (DFC)
Function Documentation
fucom_method(criteria_rank, criteria_priority, DFC_threshold = 0.025)

Description: Implements the Full Consistency Method (FUCOM) for multi-criteria decision-making.

Arguments:

    criteria_rank: A character vector specifying the rank of each criterion.
    criteria_priority: A numeric vector specifying the priority values of each criterion.
    DFC_threshold: A numeric value specifying the threshold for the deviation from full consistency (DFC). Default is 0.025.

Returns: A list containing:

    weights: A numeric vector of the optimized weights for each criterion, summing to 1.
    Phi: A numeric vector of comparative priority (Phi) values.
    w: A numeric vector of the condition of mathematical transitivity (w) values.
    DFC: The minimum deviation from full consistency (DFC) value.

Imports:

    nloptr: For optimization.
    stats: For random number generation.

Suggests:

    knitr, rmarkdown, spelling, testthat: For documentation, testing, and additional functionality.
