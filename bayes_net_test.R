source("bayes_net.R")

############################################
## Example 1: Bayes net with no NA's,     ##
##            2 labels and no covariates. ##
############################################

nn = 10^4
label1 = c(rep("a", nn/2), rep("b", nn/2))
label2 = c(rbinom(nn/2, 1, 0.2), rbinom(nn/2, 1, 0.9))
this_data = tibble::tibble(label1 = label1,
                           label2 = label2)
this_data_ref = Data_Ref$new(this_data)
#this_data_ref$get_df()

label1_node = Cat_Node$new(this_data_ref, "label1", NULL)
#label1_node$get_labels()
#label1_node$do_update_params()
#label1_node$get_param()

label2_node = Cat_Node$new(this_data_ref, "label2", c(label1_node))
#label2_node$get_param()
#label2_node$get_labels()
#label2_node$do_update_params()
#label2_node$get_param()

############################################
## Example 2: Bayes net with NA's,        ##
##            2 labels and no covariates. ##
############################################

nn = 10^4
label1 = c(rep("a", nn/2), rep("b", nn/2))
label2 = c(rbinom(nn/2, 1, 0.2), rbinom(nn/2, 1, 0.9))
label1 = ifelse(rbinom(nn,1,0.1), NA, label1)
label2 = ifelse(rbinom(nn,1,0.1), NA, label2)
this_data = tibble::tibble(label1 = label1,
                           label2 = label2)
this_data_ref = Data_Ref$new(this_data)
#this_data_ref$get_df()
#this_data_ref$get_data_na()

label1_node = Cat_Node$new(this_data_ref, "label1", NULL)
#label1_node$get_labels()
#label1_node$do_update_params()
#label1_node$get_param()

label2_node = Cat_Node$new(this_data_ref, "label2", c(label1_node))
#label2_node$get_param()
#label2_node$get_labels()
#label2_node$do_update_params()
#label2_node$get_param()

#Gibbs
nodes = c(label1_node, label2_node)
for(ii in 1:10) {
  random_node = sample(nodes, 1)
  random_node$do_update_params()
}

############################################
## Example 3: Bayes net with NA's,        ##
##            2 labels and 1 covariate.   ##
############################################

nn = 10^4
label1 = c(rep("a", nn/2), rep("b", nn/2))
label2 = c(rbinom(nn/2, 1, 0.2), rbinom(nn/2, 1, 0.9))
label1 = ifelse(rbinom(nn, 1, 0.1), NA, label1)
label2 = ifelse(rbinom(nn, 1, 0.1), NA, label2)
covariate1 = c(rnorm(nn/2, 0, 1), rnorm(nn/2, 0, 2))
covariate1 = ifelse(rbinom(nn, 1, 0.1), NA, covariate1)
this_data = tibble::tibble(label1 = label1,
                           label2 = label2,
                           covariate1 = covariate1)
this_data_ref = Data_Ref$new(this_data)
label1_node = Cat_Node$new(this_data_ref, "label1", NULL)
label2_node = Cat_Node$new(this_data_ref, "label2", c(label1_node))
cov1_node = Double_Node$new(this_data_ref, 
                            "covariate1",
                            c(label1_node, label2_node))
#cov1_node$get_node_name()
#cov1_node$get_param()
cov1_node$do_update_params()
cov1_node$get_param()

nodes = c(label1_node, label2_node, cov1_node)
for(ii in 1:10) {
  random_node = sample(nodes, 1)[[1]]
  random_node$do_update_params()
}
