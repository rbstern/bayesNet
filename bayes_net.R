library(LaplacesDemon)
library(magrittr)
library(mvtnorm)
library(R6)
library(tidyverse)

Node = R6Class("Node",
               public=list(
                 initialize = function(data, node_name, node_type, parents_cat, parents_dbl) {
                   self$data = data
                   private$node_name = node_name
                   private$node_type = node_type
                   private$parents_cat = parents_cat
                   for(parent in private$parents_cat) {
                     private$feature_list[[parent$get_node_name()]] = parent$get_labels()
                   }
                   private$features = private$feature_list %>% names()
                   private$parents_dbl = parents_dbl
                   private$parents = c(parents_cat, parents_dbl)
                   private$parents_num = length(private$parents)
                   for(parent in private$parents) parent$add_child(self)
                 },
                 add_child = function(child) private$children %<>% append(child),
                 get_node_name = function() private$node_name,
                 get_node_type = function() private$node_type,
                 get_param = function() private$param,
                 do_update_params = function() private$update_params()
               ),
               private=list(
                 children = NULL,
                 data = NULL,
                 features = NULL,
                 feature_list = list(),
                 node_name = NULL,
                 node_type = "node",
                 param = NULL,
                 parents = NULL,
                 parents_cat = NULL,
                 parents_dbl = NULL,
                 parents_num = NULL,
                 update_data = function() {},
                 update_param = function() {}
               )
)

Cat_Node = R6Class("Cat_Node",
                   inherit = Node,
                   public = list(
                     initialize = function(data, node_name, parents) {
                       #Cat_Node has only Cat_Node as parent
                       super$initialize(data, node_name, "cat_node", parents, c())
                       private$labels = data$get_df() %>%
                         select(one_of(node_name)) %>%
                         unique() %>%
                         na.omit() %>%
                         pull() %>%
                         sort()
                       private$param = r_multinomial_dirichlet_prior(private$feature_list,
                                                                     private$labels,
                                                                     private$node_name)
                       private$update_data = function()
                       {
                         1
                         r_multinomial_dirichlet_data(features, features_data, prob_table, this_var)
                       }
                       private$update_param = function()
                       {
                         private$param = r_multinomial_dirichlet_post(private$data$get_df(), 
                                                                      private$features, 
                                                                      private$node_name)
                       }
                     },
                     get_labels = function() private$labels
                   ),
                   private = list(
                     labels = NULL,
                     param = NULL
                   )
)

Double_Node = R6Class("Double_Node",
                       public=list(
                         inherit = Node,
                         feature_list = list(),
                         labels = NULL,
                         initialize = function(node_name, parents) {
                           #How to make tidy?
                           parents_cat = c()
                           parents_dbl = c()
                           for(parent in parents) {
                             if(parent$get_node_type() == "cat_node") {
                               parents_cat %<>% append(parent)
                             }
                             else {
                               parents_dbl %<>% append(parent)
                             }
                           }
                           #
                           super$initialize(node_name, "double_node", parents_cat, parents_dbl)
                         }
                       )
)

Data_Node = R6Class("Data_Node",
                    public=list(
                      1
                    )
)

#feat <- list(a1=c(1,2,3),a2=c("a","b"),a3=c(5,6),a4=c(7,8))
#data <- cross_df(feat)
                             
#test = Cond_Prob_Table$new(list(), c(1,2,3), "a1")
#test$update()
#test = Cat_Node$new("a1", c())
#test2 = Cat_Node$new("a2", c(test))
