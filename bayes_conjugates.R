source("null_exceptions.R")

#Special operators
`%>%` <- magrittr::`%>%`
`%<>%` <- magrittr::`%<>%`
`%||%` <- purrr::`%||%`

###################################
### Multinomial Dirichlet model ###
###################################
r_multinomial_dirichlet_prior = function(feature_list, labels)
{
    all_vars = feature_list
    all_vars$.labels = labels
    param = purrr::cross_df(all_vars) %>%
      dplyr::mutate(.probs = rep(1/length(labels)))
    invisible(param)
}

r_multinomial_dirichlet_post = function(data, features, label_name)
{
  all_vars = c(label_name, features)
  param = data %>%
    dplyr::select(dplyr::one_of(all_vars)) %>%
    dplyr::group_by_at(all_vars) %>%
    dplyr::summarise(.probs = n() + 1) %>%
    dplyr::group_by_at(features %||% dplyr::vars(-dplyr::everything())) %>%
    dplyr::mutate(.probs = LaplacesDemon::rdirichlet(1,.probs)) %>% 
    dplyr::rename(.labels=!!label_name) %>%
    dplyr::ungroup()
  invisible(param)
}

gen_multinomial = function(.perm_data, .prob_table, label_name) {
  choices = sample(1:nrow(.prob_table),
                   size = nrow(.perm_data),
                   prob = .prob_table$.probs,
                   replace = TRUE)
  .perm_data[[label_name]] = .prob_table$.labels[choices]
  invisible(.perm_data)
}

r_multinomial_dirichlet_data = function(data, features, label_name, prob_table)
{
  prob_table %<>% 
    dplyr::group_by_at(dplyr::vars(features %||% -dplyr::everything())) %>%
    tidyr::nest(.key = .prob_table)
  
  complete_data = data %>%
    dplyr::group_by_at(features %||% dplyr::vars(-dplyr::everything())) %>%
    tidyr::nest(.key = .perm_data) %>%
    inner_join_NULL(prob_table, by = features) %>%
    dplyr::mutate(.perm_data = purrr::map2(.perm_data, .prob_table, 
                                           function(x, y) gen_multinomial(x, y, label_name))) %>%
    dplyr::select(-.prob_table) %>%
    tidyr::unnest() %>%
    dplyr::ungroup()
}

### Normal NIG model

r_normal_nig_prior = function(covariates, feature_list)
{
  all_vars = feature_list
  all_vars$.param_name = c(covariates, ".sigma")
  reg_table = purrr::cross_df(all_vars) %>%
    dplyr::mutate(.param_value = ifelse(.param_name == ".sigma", 1, 0))
  invisible(reg_table)
}

r_normal_nig_post_simple = function(.reg_data, resp_name)
{
  XX = .reg_data %>% 
    dplyr::select(-dplyr::one_of(resp_name)) %>% 
    as.matrix()
  covariates = colnames(XX)
  yy = .reg_data %>% 
    dplyr::select(dplyr::one_of(resp_name)) %>%
    as.matrix()
  
  aux1 = (t(XX) %*% XX) + diag(1, ncol(XX))
  aux2 = solve_empty(aux1)
  coef_mean = aux2 %*% t(XX) %*% yy
  coef_var = aux2
  prec_alfa = 1 + nrow(XX)/2
  prec_beta = 1 + 0.5*(sum(yy^2) - t(coef_mean) %*% aux1 %*% coef_mean)
  .sigma = sqrt(1/(rgamma(1, prec_alfa, prec_beta)))
  .means = rmvnorm_empty(1, 
                         mean = coef_mean,
                         sigma = .sigma^2 * coef_var) %>%
    as.numeric()
  
  .reg_param = tibble::tibble(.param_name = c(covariates, ".sigma"),
                              .param_value = c(.means, .sigma))
  invisible(.reg_param)
}

r_normal_nig_post = function(covariates, data, features, resp_name)
{
  all_vars = c(covariates, features, resp_name)
  quick_post = function(x) r_normal_nig_post_simple(x, resp_name)
  reg_table = data %>%
    dplyr::select(dplyr::one_of(all_vars)) %>%
    dplyr::group_by_at(features %||% dplyr::vars(-dplyr::everything())) %>%
    tidyr::nest(.key = .reg_data) %>%
    dplyr::mutate(.reg_param = purrr::map(.reg_data, quick_post)) %>%
    dplyr::select(-.reg_data) %>%
    tidyr::unnest() %>%
    dplyr::ungroup()
  invisible(reg_table)
}

gen_linear_reg = function(.perm_data, .reg_table, covariates, resp_name) {
  XX = .perm_data %>%
    select_at_NULL(covariates) %>%
    as.matrix()
  sd = .reg_table %>% 
    dplyr::filter(.param_name == ".sigma") %>%
    dplyr::select(.param_value) %>%
    dplyr::pull()
  
  coef_table = .reg_table %>%
    dplyr::filter(.param_name != ".sigma")
  beta = tibble::tibble(.param_name = c(character(), covariates)) %>%
    dplyr::inner_join(coef_table, by = ".param_name") %>%
    dplyr::select(.param_value) %>%
    dplyr::pull()
  yy = empty_to_zero(XX %*% beta) + rnorm(nrow(.perm_data), mean = 0, sd = sd)
  if(ncol(XX) == 0) return(tibble::tibble(!!resp_name := yy))
  tibble::as.tibble(cbind(XX, yy))
}

r_normal_nig_data = function(covariates, data, features, reg_table, resp_name)
{
  reg_table %<>% 
    dplyr::group_by_at(dplyr::vars(features %||% -dplyr::everything())) %>%
    tidyr::nest(.key = .reg_table)
  
  complete_data = data %>%
    dplyr::group_by_at(features %||% dplyr::vars(-dplyr::everything())) %>%
    tidyr::nest(.key = .perm_data) %>%
    inner_join_NULL(reg_table, by = features) %>%
    dplyr::mutate(.perm_data = purrr::map2(.perm_data, .reg_table, 
                                           function(x, y) gen_linear_reg(x, y, 
                                                                         covariates, 
                                                                         resp_name))) %>%
    dplyr::select(-.reg_table) %>%
    tidyr::unnest() %>%
    dplyr::ungroup()
  invisible(complete_data)
}
