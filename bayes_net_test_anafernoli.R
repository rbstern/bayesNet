# Arquivo de testes da Ana Noli.

# Carrega as funcoes da rede Bayesiana
source("bayes_net.R")

# Copia do Exemplo 1 do bayes_net_test.R
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
