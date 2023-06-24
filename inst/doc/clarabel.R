## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo = FALSE------------------------------------------------------
library(clarabel)

## -----------------------------------------------------------------------------
P <- Matrix::Matrix(2 * c(3, 0, 0, 2), nrow = 2, ncol = 2, sparse = TRUE)
P <- as(P, "symmetricMatrix")  # P needs to be a symmetric matrix
q <- c(-1, -4)
A <- Matrix::Matrix(c(1, 1, 0, -1, 0, -2, 0, 1, 0, -1), ncol = 2, sparse = TRUE)
b <- c(0, 1, 1, 1, 1)
cones <- list(z = 1L, l = 4L)  ## 1 equality and 4 inequalities, in order
s <- clarabel(A = A, b = b, q = q, P = P, cones = cones)
cat(sprintf("Solution status, description: = (%d, %s)\n",
            s$status, solver_status_descriptions()[s$status]))
cat(sprintf("Solution: (x1, x2) = (%f, %f)\n", s$x[1], s$x[2]))

## -----------------------------------------------------------------------------
P <- Matrix::Matrix(2 * c(0, 0, 0, 1), nrow = 2, ncol = 2, sparse = TRUE)
P <- as(P, "symmetricMatrix") # P needs to be a symmetric matrix
q <- c(0, 0)
A <- Matrix::Matrix(c(0, -2.0, 0, 0, 0, 1.0), nrow = 3, ncol = 2, sparse = TRUE)
b <- c(1, -2, -2)
cones <- list(q = 3L)
s <- clarabel(A = A, b = b, q = q, P = P, cones = cones)
cat(sprintf("Solution status, description: = (%d, %s)\n",
            s$status, solver_status_descriptions()[s$status]))
cat(sprintf("Solution (x1, x2) = (%f, %f)\n", s$x[1], s$x[2]))

## -----------------------------------------------------------------------------
#' Return an vectorization of symmetric matrix using the upper triangular part,
#' still in column order.
#' @param S a symmetric matrix
#' @return vector of values
vec <- function(S) {
  n <- nrow(S)
  sqrt2 <- sqrt(2.0)
  upper_tri <- upper.tri(S, diag = FALSE)
  S[upper_tri] <- S[upper_tri] * sqrt2
  S[upper.tri(S, diag = TRUE)]
}

#' Return the symmetric matrix from the [vec] vectorization
#' @param v a vector
#' @return a symmetric matrix
mat <- function(v) {
  n <- (sqrt(8 * length(v) + 1) - 1) / 2
  sqrt2 <- sqrt(2.0)
  S <- matrix(0, n, n)
  upper_tri <- upper.tri(S, diag = TRUE)
  S[upper_tri] <- v / sqrt2
  S <- S + t(S)
  diag(S) <- diag(S) / sqrt(2)
  S
}

## ---- echo = TRUE-------------------------------------------------------------
q <- c(1, -1, 1) # objective: x_1 - x2 + x_3
A11 <- matrix(c(-7, -11, -11, 3), nrow = 2)
A12 <- matrix(c(7, -18, -18, 8), nrow = 2)
A13 <- matrix(c(-2, -8, -8, 1), nrow = 2)

A21 <- matrix(c(-21, -11, 0, -11, 10, 8, 0, 8, 5), nrow = 3)
A22 <- matrix(c(0, 10, 16, 10, -10, -10, 16, -10, 3), nrow = 3)
A23 <- matrix(c(-5, 2, -17, 2, -6, 8, -17, 8, 6), nrow = 3)

B1 <- matrix(c(33, -9, -9, 26), nrow = 2)
B2 <- matrix(c(14, 9, 40, 9, 91, 10, 40, 10, 15), nrow = 3)

A <- rbind(
  cbind(vec(A11), vec(A12), vec(A13)), # first psd constraint
  cbind(vec(A21), vec(A22), vec(A23))  # second psd constraint
)
b <- c(vec(B1), vec(B2)) # stack both psd constraints
cones <- list(s = c(2, 3)) # cone dimensions
s <- clarabel(A = A, b = b, q = q, cone = cones)
cat(sprintf("Solution status, description: = (%d, %s)\n",
            s$status, solver_status_descriptions()[s$status]))
cat(sprintf("Solution (x1, x2, x3) = (%f, %f, %f)\n", s$x[1], s$x[2], s$x[3]))

## -----------------------------------------------------------------------------
P <- Matrix::Matrix(2 * c(0, 0, 0, 1), nrow = 2, ncol = 2, sparse = TRUE)
P <- as(P, "symmetricMatrix") # P needs to be a symmetric matrix
q <- c(0, 0)
A <- Matrix::Matrix(c(0, -2.0, 0, 0, 0, 1.0), nrow = 3, ncol = 2, sparse = TRUE)
b <- c(1, -2, -2)
cones <- list(q = 3L)
s <- clarabel(A = A, b = b, q = q, P = P, cones = cones,
              control = list(max_iter = 3)) ## Reduced number of iterations
cat(sprintf("Solution status, description: = (%d, %s)\n",
            s$status, solver_status_descriptions()[s$status]))
cat(sprintf("Solution (x1, x2) = (%f, %f)\n", s$x[1], s$x[2]))

