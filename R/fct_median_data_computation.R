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

  exp.data<-read.csv(input)

  # subset requested columns

  exp.data <- exp.data %>%
    select(unit, genotype, g_alias, treatment, timestamp,
           `Digital.biomass..mm³.` , Height..mm., Leaf.angle..?..,
           `Leaf.area..mm².`, `Leaf.area.index..mm².mm².`,
           `Leaf.area..projected...mm².`, `Leaf.inclination..mm².mm².`,
           Light.penetration.depth..mm.)

  colnames(exp.data)[6:ncol(exp.data)] <- c("Digital.biomass.mmaa3",
                                            "Height.mm", "Leaf.angle.a",
                                            "Leaf.area.mma2",
                                            "Leaf.area.index.mmaa2.mmaa2",
                                            "Leaf.area.projected.mmaa2",
                                            "Leaf.inclination.mmaa2.mmaa2",
                                            "Light.penetration.depth.mm")


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

  dbm.med<-ddply(exp.data.ad,Sector ~ timestamp,summarise,Digital_biomass_Median=median(Digital.biomass.mmaa3, na.rm=TRUE))
  ht.med<-ddply(exp.data.ad,Sector ~ timestamp,summarise,Plant_height_Median=median(Height.mm,na.rm=TRUE))
  la.med<-ddply(exp.data.ad,Sector ~ timestamp,summarise,Leaf_Angle_Median=median(Leaf.angle.a,na.rm=TRUE))
  area3d.med<-ddply(exp.data.ad,Sector ~ timestamp,summarise,Area3d_Median=median(Leaf.area.mma2,na.rm=TRUE))
  lai.med<-ddply(exp.data.ad,Sector ~ timestamp,summarise,Leaf_area_index_Median=median(Leaf.area.index.mmaa2.mmaa2,na.rm=TRUE))
  proj.la.med<-ddply(exp.data.ad,Sector ~ timestamp,summarise,Projected_leaf_Median=median(Leaf.area.projected.mmaa2,na.rm=TRUE))
  li.med<-ddply(exp.data.ad,Sector ~ timestamp,summarise,Leaf_Incl_Median=median(Leaf.inclination.mmaa2.mmaa2,na.rm=TRUE))
  lpd.med<-ddply(exp.data.ad,Sector ~ timestamp,summarise,Light_Pen_Dept_Median=median(Light.penetration.depth.mm,na.rm=TRUE))

  if(row_wise)
  {
    ###transposing the table based on date
    dbm.med<-dcast(dbm.med, Sector ~ timestamp, value.var="Digital_biomass_Median")
    ht.med<-dcast(ht.med, Sector ~ timestamp, value.var="Plant_height_Median")
    la.med<-dcast(la.med, Sector ~ timestamp, value.var="Leaf_Angle_Median")
    area3d.med<-dcast(area3d.med, Sector ~ timestamp, value.var="Area3d_Median")
    lai.med<-dcast(lai.med, Sector ~ timestamp, value.var="Leaf_area_index_Median")
    proj.la.med<-dcast(proj.la.med, Sector ~ timestamp, value.var="Projected_leaf_Median")
    li.med<-dcast(li.med, Sector ~ timestamp, value.var="Leaf_Incl_Median")
    lpd.med<-dcast(lpd.med, Sector ~ timestamp, value.var="Light_Pen_Dept_Median")
  }

  # ####combining the metadata to final median
  dbm.med<-sqldf("select * from 'exp.data.meta' join 'dbm.med' using(Sector)")
  ht.med<-sqldf("select * from 'exp.data.meta' join 'ht.med' using(Sector)")
  la.med<-sqldf("select * from 'exp.data.meta' join 'la.med' using(Sector)")
  area3d.med<-sqldf("select * from 'exp.data.meta' join 'area3d.med' using(Sector)")
  lai.med<-sqldf("select * from 'exp.data.meta' join 'lai.med' using(Sector)")
  proj.la.med<-sqldf("select * from 'exp.data.meta' join 'proj.la.med' using(Sector)")
  li.med<-sqldf("select * from 'exp.data.meta' join 'li.med' using(Sector)")
  lpd.med<-sqldf("select * from 'exp.data.meta' join 'lpd.med' using(Sector)")

  #### save the file

  ##automation
  # file.path(output_file, 'dbm_med.csv')

  write.csv(dbm.med, paste(output_file, 'dbm_med.csv', sep = " "), row.names = FALSE)
  write.csv(ht.med, paste(output_file, 'ht_med.csv', sep = " "), row.names = FALSE)
  write.csv(la.med, paste(output_file, 'la_med.csv', sep = " "), row.names = FALSE)
  write.csv(area3d.med, paste(output_file, 'area3d_med.csv', sep = " "), row.names = FALSE)
  write.csv(lai.med, paste(output_file, 'lai_med.csv', sep = " "), row.names = FALSE)
  write.csv(proj.la.med, paste(output_file, 'proj_la_med.csv', sep = " "), row.names = FALSE)
  write.csv(li.med, paste(output_file, 'li_med.csv', sep = " "), row.names = FALSE)
  write.csv(lpd.med, paste(output_file, 'lpd_med.csv', sep = " "), row.names = FALSE)

  return(list(exp.data, md, exp.data.ad, exp.data.meta, dbm.med, ht.med, la.med,
              area3d.med, lai.med, proj.la.med, li.med, lpd.med))

}

