source("bayes_conjugates.R")

Node = R6::R6Class("Node",
               public = list(
                 initialize = function(data, node_name, node_type, parents_cat, parents_dbl) {
                   private$data = data
                   private$node_name = node_name
                   private$node_type = node_type
                   private$parents_cat = parents_cat
                   for(parent in private$parents_cat) {
                     private$feature_list[[parent$get_node_name()]] = parent$get_labels()
                   }
                   private$features = private$feature_list %>% names()
                   private$parents_dbl = parents_dbl
                   for(parent in private$parents_dbl) {
                     private$covariates %<>% append(parent$get_node_name())
                   }
                   private$parents = c(parents_cat, parents_dbl)
                   private$parents_num = length(private$parents)
                   for(parent in private$parents) parent$add_child(self)
                 },
                 add_child = function(child) private$children %<>% append(child),
                 get_node_name = function() private$node_name,
                 get_node_type = function() private$node_type,
                 get_param = function() private$param,
                 do_update_params = function() private$update_param(),
                 update_data = NULL
               ),
               private=list(
                 children = NULL,
                 covariates = NULL,
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
                 update_param = NULL
               )
)

Cat_Node = R6::R6Class("Cat_Node",
                   inherit = Node,
                   public = list(
                     initialize = function(data, node_name, parents) {
                       #Cat_Node can only have Cat_Node as parent
                       super$initialize(data, node_name, "cat_node", parents, c())
                       private$labels = data$get_df() %>%
                         dplyr::select(dplyr::one_of(node_name)) %>%
                         unique() %>%
                         na.omit() %>%
                         dplyr::pull() %>%
                         sort()
                       private$param = r_multinomial_dirichlet_prior(private$feature_list,
                                                                     private$labels)
                       self$update_data = function(data) {
                         r_multinomial_dirichlet_data(data, 
                                                      private$features, 
                                                      private$node_name, 
                                                      private$param)
                       }
                       private$update_param = function()
                       {
                         private$param = r_multinomial_dirichlet_post(private$data$get_df(), 
                                                                      private$features, 
                                                                      private$node_name)
                         private$data$do_update(self)
                         invisible()
                       }
                       private$data$do_update(self)
                     },
                     get_labels = function() private$labels
                   ),
                   private = list(
                     labels = NULL
                   )
)

Double_Node = R6::R6Class("Double_Node",
                          inherit = Node,   
                          public=list(
                            feature_list = list(),
                            labels = NULL,
                            initialize = function(data, node_name, parents) {
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
                              super$initialize(data,
                                               node_name, 
                                               "double_node", 
                                               parents_cat, 
                                               parents_dbl)
                              private$param = r_normal_nig_prior(private$covariates,
                                                                 private$feature_list)
                              self$update_data = function(data) { 
                                r_normal_nig_data(private$covariates, 
                                                  data, 
                                                  private$features, 
                                                  private$param, 
                                                  private$node_name)
                              }
                              private$update_param = function()
                              {
                                private$param = r_normal_nig_post(private$covariates, 
                                                                  private$data$get_df(), 
                                                                  private$features, 
                                                                  private$node_name)
                                private$data$do_update(self)
                                invisible()
                              }
                              private$data$do_update(self)
                            }
                          )
)

Data_Ref = R6::R6Class("Data_Ref",
                       public = list(
                         initialize = function(data) {
                           data$.id = 1:nrow(data)
                           private$data = data
                           private$data_na = data %>%
                             is.na() %>%
                             tibble::as.tibble()
                         },
                         get_df = function() private$data,
                         do_update = function(node) {
                           node_name = node$get_node_name()
                           na_samples = which(private$data_na[[node_name]])
                           if(length(na_samples) == 0) return()
                           private$data[na_samples,] %<>%
                             node$update_data()
                           private$data %<>% dplyr::arrange(.id)
                         },
                         get_data_na = function() private$data_na
                       ),
                       private = list(
                         data = NULL,
                         data_na = NULL
                       )
)
