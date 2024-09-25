## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## ----install------------------------------------------------------------------
library(fucom)

criteria_rank <- c("Criterion 1", "Criterion 2", "Criterion 3", 
                   "Criterion 4", "Criterion 5", "Criterion 6", 
                   "Criterion 7", "Criterion 8")

criteria_priority <- c(1, 1, 1, 2, 4, 4, 4, 4)

results <- fucom_method(criteria_rank, criteria_priority)

# Display the results
results$weights  # Optimized weights
results$Phi      # Comparative priority (Phi) values
results$w        # Mathematical transitivity condition (w)
results$DFC      # Minimum deviation from full consistency (DFC)


