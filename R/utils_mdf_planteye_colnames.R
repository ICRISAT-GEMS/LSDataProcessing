#' mdf_planteye_colnames
#'
#' @description reduce the column names by removing the units.
#'
#' @return col_names
#'
#' @noRd

mdf_planteye_colnames = function(col_names){

  n_nm <- length(col_names)

  for(i in 1:n_nm){

    ## get the position of the first [

    pos_i <- unlist(gregexpr('\\[', col_names[i]))[1]

    if(pos_i != -1){

      ## remove the braces if its present.
      col_names[i] <- substr(col_names[i], start = 1, stop = pos_i - 1)

    }

  }
   ## removal of white spaces if any
  col_names <- trimws(x = col_names, which = 'right')

  ## replacing the space by underscore.
  col_names <- gsub(pattern = ' ', replacement = '_', x = col_names)

  ## replacing the space by underscore.
  col_names <- gsub(pattern = '\\(', replacement = '', x = col_names)
  col_names <- gsub(pattern = '\\)', replacement = '', x = col_names)

  return(col_names)

}
