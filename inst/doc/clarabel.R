## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(clarabel)

## -----------------------------------------------------------------------------
P <- Matrix::Matrix(2 * c(3, 0, 0, 2), nrow = 2, ncol = 2, sparse = TRUE)
P <- as(P, "symmetricMatrix")  # P needs to be a symmetric matrix
q <- c(-1, -4)
A <- Matrix::Matrix(c(1, 1, 0, -1, 0, -2, 0, 1, 0, -1), ncol = 2, sparse = TRUE)
b <- c(0, 1, 1, 1, 1)
cones <- c(z = 1L, l = 4L)  ## 1 equality and 4 inequalities, in order
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
cones <- c(q = 3L)
s <- clarabel(A = A, b = b, q = q, P = P, cones = cones)
cat(sprintf("Solution status, description: = (%d, %s)\n",
            s$status, solver_status_descriptions()[s$status]))
cat(sprintf("Solution (x1, x2) = (%f, %f)\n", s$x[1], s$x[2]))

## -----------------------------------------------------------------------------
P <- Matrix::Matrix(2 * c(0, 0, 0, 1), nrow = 2, ncol = 2, sparse = TRUE)
P <- as(P, "symmetricMatrix") # P needs to be a symmetric matrix
q <- c(0, 0)
A <- Matrix::Matrix(c(0, -2.0, 0, 0, 0, 1.0), nrow = 3, ncol = 2, sparse = TRUE)
b <- c(1, -2, -2)
cones <- c(q = 3L)
s <- clarabel(A = A, b = b, q = q, P = P, cones = cones,
              control = list(max_iter = 3)) ## Reduced number of iterations
cat(sprintf("Solution status, description: = (%d, %s)\n",
            s$status, solver_status_descriptions()[s$status]))
cat(sprintf("Solution (x1, x2) = (%f, %f)\n", s$x[1], s$x[2]))

