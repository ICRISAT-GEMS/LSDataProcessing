#' mdf_raw_pe_colnames
#'
#' @description A function to detect the desired column names: digital biomass,
#' height, leaf angle, etc. and format them with standard names accepted by the
#' \cite{\link{median_data_computation function}}.
#'
#' @param colnames String vector of column names.
#'
#' @return Modified column names.
#'
#' @export


mdf_raw_pe_colnames <- function(colnames){

  # Digital biomass
  p_i <- agrep(pattern = "Digital biomass", x = colnames)
  colnames[p_i] <- "Digital_biomass"

  # Height: First detect height max then Height (contains height but not height max)
  p_max <- agrep(pattern = "Height Max", x = colnames)
  p_i <- agrep(pattern = "Height", x = colnames)
  p_height <- p_i[!(p_i %in% p_max)]
  colnames[p_height] <- "Height"

  # Leaf angle
  p_i <- agrep(pattern = "Leaf angle", x = colnames)
  colnames[p_i] <- "Leaf_angle"

  # Leaf area, leaf area index and leaf area projected
  p_index <- agrep(pattern = "Leaf area index", x = colnames)
  p_project <- agrep(pattern = "Leaf.area.projected", x = colnames)
  p_i <- agrep(pattern = "Leaf area", x = colnames)
  p_area <- p_i[!(p_i %in% c(p_index, p_project))]
  colnames[p_area] <- "Leaf_area"
  colnames[p_index] <- "Leaf_area_index"
  colnames[p_project] <- "Leaf_area_projected"

  # Leaf inclination
  p_i <- agrep(pattern = "Leaf inclination", x = colnames)
  colnames[p_i] <- "Leaf_inclination"

  # Leaf penetration
  p_i <- agrep(pattern = "Light penetration", x = colnames)
  colnames[p_i] <- "Light_penetration_depth"

  return(colnames)

}
