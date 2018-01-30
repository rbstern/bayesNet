source("bayes_conjugates.R")

##################################
### Multinomial Dirichlet test ###
##################################

# Tasks: 
# - verificar se as estimativas sao boas com n grande.

### Test data (2 features)
label_name = "f1"
labels = c(1,2,3)
feature_list = list(f2 = c("a","b"), f3 = c("d","e"))
features = names(feature_list)
covariates = c("c1", "c2")
data = c(feature_list, list(f1=labels)) %>%
  purrr::cross_df() %>%
  dplyr::mutate(c1 = rnorm(length(f1)),
                c2 = rnorm(length(f1)))

r_multinomial_dirichlet_prior(feature_list, labels) %>% print()
prob_table = r_multinomial_dirichlet_post(data, features, label_name) %>% print()
r_multinomial_dirichlet_data(data, features, label_name, prob_table) %>% print()

### Test data (0 features)
label_name = "f1"
labels = c(1,2,3)
data = tibble::tibble(!!label_name := rep(labels, 100))
feature_list = list()
features = names(feature_list)

r_multinomial_dirichlet_prior(feature_list, labels) %>% print()
prob_table = r_multinomial_dirichlet_post(data, features, label_name) %>% print()
r_multinomial_dirichlet_data(data, features, label_name, prob_table) %>% print()

#######################
### Normal NIG test ###
#######################

### Test data (2 features and 2 covariates)
feature_list = list(f2 = c("a","b"), f3 = c("d","e"))
features = names(feature_list)
resp_name = "c1"
covariates = c("c2", "c3")
data = feature_list %>%
  purrr::cross_df()
for(ii in 1:10) { data %<>% rbind(data) }
data %<>%
  dplyr::mutate(c2 = rnorm(nrow(data)),
                c3 = rnorm(nrow(data)),
                c1 = c2 + 0.5*c3 + rnorm(nrow(data)))
r_normal_nig_prior(covariates, feature_list) %>% print()
r_normal_nig_post(covariates, data, features, resp_name) %>% print()
reg_table = r_normal_nig_post(covariates, data, features, resp_name)
r_normal_nig_data(covariates, data, features, reg_table, resp_name) %>% print()

### Test data (2 features and 0 covariates)
feature_list = list(f2 = c("a","b"), f3 = c("d","e"))
features = names(feature_list)
resp_name = "c1"
covariates = NULL
data = feature_list %>%
  purrr::cross_df()
for(ii in 1:10) { data %<>% rbind(data) }
data %<>% dplyr::mutate(c1 = rnorm(nrow(data)))
r_normal_nig_prior(covariates, feature_list) %>% print()
r_normal_nig_post(covariates, data, features, resp_name) %>% print()
reg_table = r_normal_nig_post(covariates, data, features, resp_name)
r_normal_nig_data(covariates, data, features, reg_table, resp_name) %>% print()

## Test data 2 (2 covariates, 0 features)
feature_list = list()
features = names(feature_list)
resp_name = "c1"
covariates = c("c2", "c3")
data = tibble::tibble(c2 = rnorm(1024),
                      c3 = rnorm(1024),
                      c1 = c2 + 0.5 * c3 + rnorm(1024))
r_normal_nig_prior(covariates, feature_list) %>% print()
r_normal_nig_post(covariates, data, features, resp_name) %>% print()

reg_table = r_normal_nig_post(covariates, data, features, resp_name)
r_normal_nig_data(covariates, data, features, reg_table, resp_name) %>% print()


# Test data 3 (0 covariates, 0 features)
covariates = NULL
feature_list = list()
features = names(feature_list)
resp_name = "c1"
data = tibble::tibble(c1 = rnorm(1024))
r_normal_nig_prior(covariates, feature_list) %>% print()
r_normal_nig_post(covariates, data, features, resp_name) %>% print()

reg_table = r_normal_nig_post(covariates, data, features, resp_name)
r_normal_nig_data(covariates, data, features, reg_table, resp_name) %>% print()
