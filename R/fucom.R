#’ Full Consistency Method - FUCOM Method for Multi-Criteria Decision Making
#'
#' Implementation of Full Consistency Method (FUCOM) for multi-criteria decision making.
#' More information about the method at https://doi.org/10.3390/sym10090393.
#' More information about the implementation at https://github.com/mateusvanzetta/fucom.
#' The goal is to determine the weights of criteria such that the deviation from full consistency (DFC) is minimized.
#'
#' @param criteria_rank A character vector specifying the rank of each criterion.
#' @param criteria_priority A numeric vector specifying the priority values of each criterion.
#' @param DFC_threshold A numeric value specifying the threshold for the deviation from full consistency (DFC).
#'                      It must be a positive number and less than or equal to 0.025. Default is 0.025.
#'
#' @return A list containing:
#' \describe{
#'   \item{weights}{A numeric vector of the optimized weights for each criterion, summing to 1.}
#'   \item{Phi}{A numeric vector of comparative priority (Phi) values.}
#'   \item{w}{A numeric vector of the condition of mathematical transitivity (w) values.}
#'   \item{DFC}{The minimum deviation from full consistency (DFC) value.}
#' }
#'
#' @importFrom nloptr nloptr
#' @importFrom stats runif
#' @export
#'
#' @examples
#' criteria_rank <- c("Criterion 1", "Criterion 2", "Criterion 3",
#'  "Criterion 4", "Criterion 5", "Criterion 6", "Criterion 7", "Criterion 8")
#' criteria_priority <- c(1, 1, 1, 2, 4, 4, 4, 4)
#' results <- fucom_method(criteria_rank, criteria_priority)
#' results$weights
#' results$Phi
#' results$w
#' results$DFC
fucom_method <- function(criteria_rank, criteria_priority, DFC_threshold = 0.025) {

  # Input validation for criteria_rank and criteria_priority
  if (!is.character(criteria_rank) || !is.numeric(criteria_priority)) {
    stop("`criteria_rank` must be a character vector and `criteria_priority` must be a numeric vector.")
  }

  if (length(criteria_rank) != length(criteria_priority)) {
    stop("`criteria_rank` and `criteria_priority` must have the same length.")
  }

  # Validation for DFC_threshold
  if (DFC_threshold <= 0 || DFC_threshold > 0.025) {
    stop("`DFC_threshold` must be a positive number and less than or equal to 0.025.")
  }

  # Calculating Phi based on criteria_priority
  # Phi represents the comparative priority between consecutive criteria
  Phi <- criteria_priority[-1] / criteria_priority[-length(criteria_priority)]
  # Calculating w based on criteria_priority
  # w represents the condition of mathematical transitivity for these priorities
  w <- criteria_priority[-c(1, 2)] / criteria_priority[-c(length(criteria_priority), length(criteria_priority) - 1)]


  # Objective function for optimization
  # This function calculates the maximum deviation from full consistency (DFC)
  objective_function <- function(weight) {
    # Calculating target Phi and w based on the current weight vector
    target_Phi <- weight[-length(weight)] / weight[-1]
    target_w <- weight[-c(length(weight), length(weight) - 1)] / weight[-c(1, 2)]

    # Calculating deviations (DFC) from target Phi and w
    DFC_1 <- abs(target_Phi - Phi)
    DFC_2 <- abs(target_w - w)

    # Returning the maximum deviation from full consistency (DFC)
    return(max(c(DFC_1, DFC_2)))
  }

  # Gradient of the objective function for optimization
  grad_objective_function <- function(weight) {
    epsilon <- .Machine$double.eps^0.5  # Small value for numerical differentiation
    grad <- numeric(length(weight))

    for (i in seq_along(weight)) {
      weight_plus <- weight
      weight_minus <- weight
      weight_plus[i] <- weight_plus[i] + epsilon
      weight_minus[i] <- weight_minus[i] - epsilon

      # Numerical gradient calculation
      grad[i] <- (objective_function(weight_plus) - objective_function(weight_minus)) / (2 * epsilon)
    }

    return(grad)
  }

  # Performing optimization to minimize DFC
  # Initialize weight with random values between 0.001 and 1
  weight <- runif(length(criteria_priority), min = 0.001, max = 1)

  # Using Sequential Quadratic Programming (SQP) for optimization
  results <- nloptr::nloptr(
    x0 = weight,
    eval_f = objective_function,
    eval_grad_f = grad_objective_function,
    lb = rep(0.0001, length(criteria_priority)),  # Lower bounds for weights
    ub = rep(1.0, length(criteria_priority)),     # Upper bounds for weights
    opts = list(
      algorithm = "NLOPT_LD_SLSQP",  # SQP algorithm for constrained optimization
      xtol_rel = 1e-9,               # Relative tolerance for convergence
      maxeval = 10000,               # Maximum number of function evaluations
      print_level = 0                # Suppress output from the optimizer
    )
  )

  # Extracting the optimized weight vector
  weight <- results$solution

  # Calculating final weights and ensuring the sum of weights equals 1
  weights <- weight / sum(weight)

  # Returning the results as a list
  return(list(weights = weights, Phi = Phi, w = w, DFC = objective_function(weight)))
}
