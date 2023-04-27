#' median_data_computation
#'
#' @description Median calculation
#'
#' @param input input file (.csv)
#'
#' @param meta metafile
#'
#' @param new_format Logical value specifying if new format. Default = TRUE.
#'
#' @param row_wise Logical value specifying if the output required is in wide format
#'
#' @param output output file location
#'
#' @return list containing input data, meta data, median calculated from the traits.
#'
#' @export

median_data_computation <- function(input, meta, new_format = TRUE, row_wise = TRUE, output){

  exp.data <- read.csv(input, check.names = FALSE)

  # modify the names of the columns to improve manipulation remove units
  # the units can be added back at the end in a more general format

  colnames(exp.data) <- mdf_planteye_colnames(colnames(exp.data))

  # subset requested columns

  exp.data <- exp.data %>%
    select(unit, genotype, g_alias, treatment, timestamp,
           Digital_biomass, Height, Leaf_angle,
           Leaf_area, Leaf_area_index,
           Leaf_area_projected, Leaf_inclination,
           Light_penetration_depth)

  if(tools::file_ext(meta) == "csv")
  {
    md <- read.csv(meta)
  }else{
    sheetnames = excel_sheets(meta)
    cat(sprintf("the sheetnames are: %s\n", sheetnames))
    sh = readline("Enter sheet name:");
    r = readline("Enter range:")
    md <- read_excel(meta, sheet = sh, range = r);
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

    if (IsDate(exp.data$timestamp[1]))
    {
      exp.data.ad$timestamp<-POSIXct(strptime(exp.data.ad$timestamp, "%Y-%m-%d"))
    }

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

