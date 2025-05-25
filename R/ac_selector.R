#'@title Selector
#'@description Ancestor class for model selection
#'@return Selector object
#'@examples
#'# See ?dd_ddm for an example of DDM drift detector
#'@import daltoolbox
#'@export
selector <- function() {
  obj <- dal_base()

  class(obj) <- append("selector", class(obj))
  return(obj)
}

select_model <- function(obj, slots_list) {
  UseMethod("select_model")
}

#'@export
select_model.selector <- function(obj, slots_list){
  
  selected_model <- slots_list[1]
  
  return(selected_model)
}