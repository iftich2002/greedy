#'Knapsack problem solverwith greedy algorithm
#'
#'take the weights, values as parameters and return maximum values and their possible combination
#'@param x: data frame of weights (first column) and value (second column)
#'@param W: Maximum capacity (numeric)
#'@return Return the maximum value and elements (index of values) and weight





greedy_knapsack<-function(x,W){
  ##attach dplyr package for filter option
  dplyr::filter
  ##ratio of value and weights
  ratio<-x[,2]/x[,1]
  ##combine the index, ratio and data frame
  knapsack_ratio<-cbind(c(1:length(ratio)),ratio,x)
  ##sorting the data according to ratio
  knapsack_ratio_sort<-knapsack_ratio[order(ratio),]
  ##changing column names
  colnames(knapsack_ratio_sort)<-c("sort","ratio","w","v")
  # initialization
  
  total_value <- 0
  elements <- c()
  total_weight <- 0
  
  ##delete the rows where weight is greater than 3500
  knapsack_clean<-filter(knapsack_ratio_sort, w <=W)
  
  for(i in 1:nrow(knapsack_clean)){
    
    w <- total_weight + (knapsack_clean$w[i])
    
    if(w <= W){
      total_weight <- w
      total_value <- total_value + (knapsack_clean$v[i])
      elements <- c(elements, (knapsack_clean$sort[i]))
    } 
  }
  return(list(total_weight, total_value, elements))
}


set.seed(42)
n <- 2000
knapsack_objects <- data.frame(w = sample(1:4000, size = n, replace = TRUE),
                               v <- runif(n = n, 0, 10000))
