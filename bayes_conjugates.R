#Special operators
`%>%` <- magrittr::`%>%`
`%<>%` <- magrittr::`%<>%`
`%||%` <- purrr::`%||%`

######################
### Data Generator ###
######################
Multinomial_Generator = R6::R6Class("Data_Generator",
                                    public = list(
                                      initialize = function(multinomial_table) {
                                        private$multinomial_table = multinomial_table
                                      },
                                      generate_data = function() {
                                        choice = sample(1:nrow(private$multinomial_table),
                                                        size =1 , 
                                                        prob = private$multinomial_table$.probs)
                                        invisible(private$multinomial_table$.labels[choice])
                                      }
                                    ),
                                    private = list(
                                      multinomial_table = NULL
                                    )
)

###################################
### Multinomial Dirichlet model ###
###################################
r_multinomial_dirichlet_prior = function(feature_list, label_name, labels)
{
    all_vars = feature_list
    all_vars[[label_name]] = labels
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
    dplyr::ungroup()
  invisible(param)
}

r_multinomial_dirichlet_data = function(features, features_data, prob_table, this_var)
{
  features %||% -dplyr::everything()
  
  complete_data = features_data %>% 
    dplyr::inner_join(prob_table, by = features) %>%
    mutate(!!this_var := map(.probs, function(x) rmulti_new(x, this_var))) %>% 
    unnest_(this_var) %>%
    select(-.probs)
  invisible(complete_data)
}

##################################
### Multinomial Dirichlet test ###
##################################

### Test data
#label_name = "f1"
#labels = c(1,2,3)
#feature_list = list(f2=c("a","b"), f3=c("d","e"))
#features = names(feature_list)
#covariates = c("c1", "c2")
#data <- c(feature_list, list(f1=labels)) %>%
#  purrr::cross_df() %>%
#  dplyr::mutate(c1=rnorm(length(f1)),
#                c2=rnorm(length(f1)))

#r_multinomial_dirichlet_prior(feature_list, label_name, labels) %>% print()
#prob_table = r_multinomial_dirichlet_post(data, features, label_name)

### Normal NIG model

r_normal_nig_prior = function(feature_list, label_name, labels)
{
  all_vars = feature_list
  all_vars[[label_name]] = labels
  prob_table = cross_df(all_vars) %>%
    mutate(.probs=rep(1/length(labels)))
  invisible(prob_table)
}

r_normal_nig_post = function(data, features, this_var)
{
  all_vars = c(this_var, features)
  prob_table = data %>%
    select(one_of(all_vars)) %>%
    group_by_at(all_vars) %>%
    summarise(.probs=n()+1) %>%
    group_by_at(vars(features %||% -everything())) %>%
    mutate(.probs=rdirichlet(1,.probs)) %>% 
    nest(.key=.probs) %>%
    ungroup()
  invisible(prob_table)
}

r_normal_nig_data = function(features, features_data, prob_table, this_var)
{
  rmulti_new = function(multinomial_table, this_var)
  {
    choice = multinomial_table %>%
      summarise(!!this_var := sample(1:length(.probs), size = 1, prob = .probs)) %>%
      inner_join(multinomial_table, by = this_var) %>%
      pull(!!this_var)
    invisible(choice)
  }
  rmulti_new(multinomial_table, this_var) %>% print()
  
  complete_data = features_data %>% 
    inner_join(prob_table, by = features) %>%
    mutate(!!this_var := map(.probs, function(x) rmulti_new(x, this_var))) %>% 
    unnest_(this_var) %>%
    select(-.probs)
  invisible(complete_data)
}

## Lixo

Cond_Reg_Table = R6Class("Cond_Reg_Table",
                         public=list(
                           covariates = NULL,
                           features = NULL,
                           node_name = NULL,
                           regression_coefs = NULL,
                           regression_precisions = NULL,
                           regression_precisions_hyp = NULL,
                           initialize = function(covariates, feature_list, node_name) {
                             self$covariates = covariates
                             self$features = names(feature_list)
                             self$node_name = node_name
                             all_vars = feature_list
                             all_vars[[".parameters"]] = c(covariates, ".precision")
                             self$regression_param = all_vars %>%
                               cross_df %>%
                               mutate(.values=0.1)
                             invisible(self)
                           },
                           #Gibbs
                           update = function() {
                             new_param <- function(df, covariates, node_name) {
                               XX = data %>%
                                 select(one_of(covariates)) %>%
                                 as.matrix()
                               yy = data %>%
                                 select(one_of(node_name)) %>%
                                 as.matrix()
                               aux1 = solve(diag(1,ncol(XX)) + t(XX) %*% XX)
                               coef_mean = aux1 %*% t(XX) %*% yy
                               coef_var = aux1
                               prec_alfa = 1 + nrow(XX)/2
                               prec_beta = 1 + 0.5*(sum(yy^2) - sum(coef_mean^2))
                               
                               new_prec = 1/rgamma(prec_alfa + prec_beta)
                               new_coef = rmvnorm(1, mean = coef_mean, sigma = coef_var)
                               
                             }
                             data %>%
                               select(one_of(all_vars)) %>%
                               group_by_at(vars(self$features %||% -everything())) %>%
                               summarise(n = n(), 
                                         .alfa1 = 1+n/2,
                                         .beta1 = ) %>% #RESOLVER
                               ungroup()
                             
                             all_vars = c(self$node_name, self$covariates, self$features)
                             
                             
                             data %>%
                               select(one_of(all_vars)) %>%
                               group_by_at(vars(self$features %||% -everything())) %>%
                               #Resolver %>% 
                               ungroup()
                             invisible(self)
                           }
                         )
)
