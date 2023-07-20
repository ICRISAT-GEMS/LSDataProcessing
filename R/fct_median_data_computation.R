#' median_data_computation
#'
#' @description Median calculation
#'
#' @param data plant eye data with specific column and column names required.
#'
#' @param meta_data meta data.
#'
#' @param input_file path to the input plant eye data (.csv)
#'
#' @param meta_file path to the meta data (.csv)
#'
#' @param new_format Logical value specifying if new format. Default = TRUE.
#'
#' @param row_wise Logical value specifying if the output required is in wide format
#'
#' @param output output file location
#'
#' @return list containing input data, meta data, median calculated from the traits.
#'
#' @examples
#'
#' library(plyr)
#' data("pe_data")
#' data("meta_data")
#'
#' \dontrun{
#'
#' # specify output directory
#' out_dir <- c()
#'
#' test_median <- median_data_computation(data = pe_data, meta_data = meta_data,
#' output = out_dir)
#'
#' }
#'
#' @import DescTools
# #' @import plyr
#' @import dplyr
#' @import sqldf
#' @import stringr
#' @importFrom maditr dcast
#'
#' @export


# @import lubridate
# @import chron
# @import tis
median_data_computation <- function(data = NULL, meta_data = NULL,
                                    input_file = NULL, meta_file = NULL,
                                    new_format = TRUE, row_wise = TRUE, output){

  ##### 1. Load and pre process exp data ----

  if(is.null(data) & is.null(input_file)){

    stop("You must provide information .one of data or input_file.")

  }

  if(!is.null(data) & !is.null(input_file)){

    stop("You must provide information only one of data or input_file.")

  }

  if(!is.null(data)){

    # check that the data contains the expected columns
    ref_var <- c("unit", "genotype", "g_alias", "treatment", "timestamp",
                 "Digital_biomass", "Height", "Leaf_angle",
                 "Leaf_area", "Leaf_area_index",
                 "Leaf_area_projected", "Leaf_inclination",
                 "Light_penetration_depth")

    test <- ref_var %in% colnames(data)

    if(any(!test)){

      miss_c_nm <- ref_var[!test]
      err_mess <- paste0("The following columns are missing or misspecified: ",
                         paste(miss_c_nm, sep = ", "))
      stop(err_mess)

    }

    exp.data <- data
    rm(data)


  } else {

    exp.data <- read.csv(input_file, check.names = FALSE)

    # modify the names of the columns to improve manipulation remove units
    # the units can be added back at the end in a more general format

    colnames(exp.data) <- mdf_planteye_colnames(colnames(exp.data))

  }

  if (IsDate(exp.data$timestamp[1]))
  {
    exp.data$timestamp<-POSIXct(strptime(exp.data$timestamp, "%Y-%m-%d"))
  }

  # subset requested columns

  exp.data <- exp.data %>%
    select(unit, genotype, g_alias, treatment, timestamp,
           Digital_biomass, Height, Leaf_angle,
           Leaf_area, Leaf_area_index,
           Leaf_area_projected, Leaf_inclination,
           Light_penetration_depth)

  ##### 2. Load and pre process meta data ----

  if(is.null(meta_data) & is.null(meta_file)){

    stop("You must provide information .one of meta_data or meta_file.")

  }

  if(!is.null(meta_data) & !is.null(meta_file)){

    stop("You must provide information only one of meta_data or meta_file.")

  }

  if(!is.null(meta_data)){

    md <- meta_data

  } else {

    if(tools::file_ext(meta_file) == "csv")
    {
      md <- read.csv(meta_file)
    }else{
      sheetnames = excel_sheets(meta_file)
      cat(sprintf("the sheetnames are: %s\n", sheetnames))
      sh = readline("Enter sheet name:");
      r = readline("Enter range:")
      md <- read_excel(meta_file, sheet = sh, range = r);
    }

  }

  if (new_format)
  {

    if (all(c("barcode", "unit.column", "unit.row") %in% colnames(md)))
    {
      md$unit=str_c(md$barcode,":",md$unit.column,":",md$unit.row)
      exp.data.ad<-sqldf("select * from md join 'exp.data' using(unit)")
      exp.data.ad<-exp.data.ad[,-c(1:5)]

    }else{
      exp.data.ad<-sqldf("select * from md join 'exp.data' using(unit)")
      # exp.data.ad<-exp.data.ad[,-c(1:5)]
    }

    names(exp.data.ad)[1]<-paste("Sector")
    exp.data.ad[exp.data.ad == 0] <- NA

    exp.data.meta=exp.data.ad[,c(1:4)]
    exp.data.meta<-unique(exp.data.meta)
  }

  ### median calculation

  # exp.data.ad <- exp.data.ad %>% group_by(Sector, timestamp) %>%
  #   dplyr::summarise(across(c(6:13), function(x) median(x, na.rm = TRUE)))

  n_trait <- 8
  dataset_list <- vector(mode = 'list', length = n_trait) # one space for each trait

  dataset_nm <- c('dbm_med', 'ht_med', 'la_med',
                  'area3d_med', 'lai_med', 'proj_la_med', 'li_med', 'lpd_med')

  for(i in 1:n_trait){

    d_i <- data.frame(exp.data.ad %>% select(Sector, timestamp),
                      trait = exp.data.ad[, i+5])

    # take some time. Later we could replace with data.table format
    d_i <- ddply(d_i, Sector ~ timestamp, summarise,
                 median_val=median(trait, na.rm=TRUE))

    if(row_wise){d_i <- dcast(d_i, Sector ~ timestamp, value.var="median_val")}

    d_i <- sqldf("select * from 'exp.data.meta' join 'd_i' using(Sector)")

    dataset_list[[i]] <- d_i

    write.csv(x = d_i, file = file.path(output, paste0(dataset_nm[i], '.csv')), row.names = FALSE)

  }

  names(dataset_list) <- dataset_nm

  return(dataset_list)

}

