#' Match rows in two matrices
#'
#' @param A first matrix
#' @param B second matrix
#'
#' @return matching row
#'
#' @examples
#' # ParetoFrontier(indices[,,selected_models[,"row"]],selected_models,parameters,SelectedIndices)
#'

RowMatch <- function(A,B) {
  # Rows in A that match the rows in B
  # The row indexes correspond to A
  f <- function(...) paste(..., sep=":")
  #if(!is.matrix(B)) B <- matrix(B, 1, length(B))
  a <- do.call("f", as.data.frame(A))
  b <- do.call("f", as.data.frame(B))
  match(b, a)
}
