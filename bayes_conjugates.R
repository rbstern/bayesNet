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

Multinomial_Generator = R6::R6Class("Multinomial_Generator",
                                    public = list(
                                      initialize = function(multinomial_table) {
                                        private$multinomial_table = multinomial_table
                                      },
                                      generate_data = function() {
                                        choice = sample(1:nrow(private$multinomial_table),
                                                        size = 1 , 
                                                        prob = private$multinomial_table$.probs)
                                        invisible(private$multinomial_table$.labels[choice])
                                      }
                                    ),
                                    private = list(
                                      multinomial_table = NULL
                                    )
)

r_multinomial_dirichlet_data = function(data, features, label_name, prob_table)
{
  prob_table %<>% 
    dplyr::group_by_at(features %||% dplyr::vars(-dplyr::everything())) %>%
    tidyr::nest(.key = .multinomial_table) %>%
    dplyr::mutate(.generators = purrr::map(.multinomial_table,
                                           function(x) Multinomial_Generator$new(x))) %>%
    dplyr::select(-.multinomial_table) %>%
    dplyr::ungroup()
    
  complete_data = data %>%
    dplyr::inner_join(prob_table, by = features) %>%
    dplyr::mutate(!!label_name := purrr::map(.generators, 
                                             function(x) x$generate_data())) %>%
    tidyr::unnest_(label_name) %>%
    dplyr::select(-.generators)
  invisible(complete_data)
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
  aux2 = solve(aux1)
  coef_mean = aux2 %*% t(XX) %*% yy
  coef_var = aux2
  prec_alfa = 1 + nrow(XX)/2
  prec_beta = 1 + 0.5*(sum(yy^2) - t(coef_mean) %*% aux1 %*% coef_mean)
  .sigma = sqrt(1/(rgamma(1, prec_alfa, prec_beta)))
  .means = mvtnorm::rmvnorm(1, 
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

gen_linear_reg = function(.perm_data, .reg_table, resp_name) {
  XX = .perm_data %>% as.matrix()
  covariates = colnames(XX)
  sd = .reg_table %>% 
    dplyr::filter(.param_name == ".sigma") %>%
    dplyr::select(.param_value) %>%
    dplyr::pull()
  beta = .reg_table %>%
    dplyr::filter(.param_name != ".sigma") %>%
    dplyr::inner_join(tibble::tibble(.param_name = covariates),
                      by = ".param_name") %>%
    dplyr::select(.param_value) %>%
    dplyr::pull()
  yy = XX %*% beta + rnorm(nrow(XX), mean = 0, sd = sd)
  colnames(yy) = c(resp_name)
  invisible(tibble::as.tibble(cbind(XX, yy)))
}

r_normal_nig_data = function(covariates, data, features, reg_table, resp_name)
{
  reg_table %<>% 
    dplyr::group_by_at(dplyr::vars(features %||% -dplyr::everything())) %>%
    tidyr::nest(.key = .reg_table)
  
  complete_data = data %>%
    dplyr::select(dplyr::one_of(c(covariates, features))) %>%
    dplyr::group_by_at(features %||% dplyr::vars(-dplyr::everything())) %>%
    tidyr::nest(.key = .perm_data) %>%
    dplyr::inner_join(reg_table, by = features) %>%
    dplyr::mutate(.perm_data = purrr::map2(.perm_data, .reg_table, 
                                           function(x, y) gen_linear_reg(x, y, resp_name))) %>%
    dplyr::select(-.reg_table) %>%
    tidyr::unnest() %>%
    dplyr::ungroup()
  invisible(complete_data)
}
