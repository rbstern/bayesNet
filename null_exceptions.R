empty_to_zero = function(MM)
{
  if(is.matrix(MM) & prod(dim(MM)) == 0) return(0)
  return(MM)
}

inner_join_NULL = function(df_1, df_2, by = NULL) {
  if(is.null(by)) return(dplyr::bind_cols(df_1, df_2))
  dplyr::inner_join(df_1, df_2, by = by)
}

rmvnorm_empty = function(size, mean = NULL, sigma = NULL) {
  if(ncol(sigma) == 0) return(NULL)
  mvtnorm::rmvnorm(size, 
                   mean = mean,
                   sigma = sigma)
}

select_at_NULL = function(df, variables)
{
  if(is.null(variables)) return(tibble::tibble())
  dplyr::select(df, dplyr::one_of(variables))
}

solve_empty = function(XX) {
  if(ncol(XX) == 0) return(XX)
  solve(XX)
}