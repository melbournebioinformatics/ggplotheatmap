
# x will be an illegal column name here

# Converts from wide to tall and encodes the original row and column ordering in x and y, and the corresponding value in value
#
wide_to_tall <- function(data,id.vars=c()){
  rownames(data) <- NULL
  data %>% add_rownames(var = "rowid") %>% 
    mutate(rowid=as.integer(rowid)) %>% 
    gather_("colid","value",setdiff(colnames(data),id.vars))
}

# Expects input from wide_to_tall
#
# tall_to_wide <- function(data,value.var="value"){
#   data %>% spread_("y",value.var) %>% arrange(x) %>% select(-x)
# }



