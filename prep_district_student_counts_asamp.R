# Prepare student count data

# prep environment ----

  ## get workspace ready
  rm(list = ls())
  setwd("~/Documents/Michigan/PUBPOL 712/rp/")

  ## set directories
  rawdatadir = "./raw_data/"
  workdatadir = "./working_data/"
  outdir = "./output/"
  codedir = "./code/"

  ## libraries
  library(readxl)
  library(data.table)
  library(lubridate)
  library(janitor)
  library(stringr)
  
# load csvs ----
  
  ## school-level information
  rcd_location = fread(paste0(workdatadir, 'rcd_location.csv'))
  
  ## average daily membership
  rcd_adm = fread(paste0(workdatadir, 'rcd_adm.csv'))
  
  ## analytical sample
  analysis_district_file = fread(paste0(workdatadir, 'prep_did_analysis_district_data.csv')) 
  
# prep data ----
  
  ## identify sample of schools with test scores in all five years
  analysis_district_file[, count := .N, by=c('lea_code', 'subject')]
  balanced_for_some_test = unique(analysis_district_file[count == 5 & exclude == 0, .(lea_code)])
  balanced_for_some_test[grepl('CH', lea_code), agency_code := str_remove(lea_code, 'CH_')]
  balanced_for_some_test = balanced_for_some_test[!is.na(agency_code), .(agency_code, charter_keep = 1)]
  
  ## prep attendance data
  
    ### drop state-wide numbers and one charter that never appears in location file
    rcd_adm = rcd_adm[!(agency_code %in% c('NC-SEA', '64B000'))]

    ### merge in school type
    nrow(rcd_adm)
    rcd_adm = merge.data.table(rcd_adm,
                               unique(rcd_location[, .(year, agency_code, category_code_temp = category_code)]),
                               by=c('year', 'agency_code'),
                               all.x = TRUE)
    nrow(rcd_adm)
    
    ### if missing category_code_temp, update
    rcd_adm[is.na(category_code_temp) & agency_code == '870312', category_code_temp := 'A'] ## from rcd_location - missing this year in that file
    rcd_adm[is.na(category_code_temp) & agency_code %in% c('298202', '298203', '298204'), category_code_temp := 'A'] ## from rcd_location - different year in that file
    rcd_adm[is.na(category_code_temp) & agency_code == '298LEA', category_code_temp := ''] ## just get rid of missing values here, there isn't an LEA record for this district in rcd_location
    rcd_adm[is.na(category_code_temp) & agency_code == '530358', category_code_temp := 'E'] ## from rcd_location - missing this year in that file
    rcd_adm[is.na(category_code_temp) & agency_code == '49E000', category_code_temp := 'A'] ## from rcd_location - missing this year in that file
    rcd_adm[is.na(category_code_temp) & agency_code == '92E000', category_code_temp := 'I'] ## from rcd_location - missing this year in that file
    rcd_adm[is.na(category_code_temp) & agency_code == '08A000', category_code_temp := 'I'] ## from rcd_location - missing this year in that file
    
    ### update school type if no value in adm file
    rcd_adm[category_code == '', category_code := category_code_temp]
    rcd_adm[, category_code_temp := NULL]
    
    ### add lea_code
    
      ### create agency_code to lea_code mapping
      agency_to_lea_code = unique(rcd_location[lea_code != '' & !is.na(lea_code), .(agency_code, lea_code)])
    
      ### make sure mapping is 1 to 1
      stopifnot(nrow(agency_to_lea_code) == nrow(unique(agency_to_lea_code[, .(agency_code)])))
      
      ### merge in
      rcd_adm = merge.data.table(rcd_adm,
                                 agency_to_lea_code,
                                 by=c('agency_code'),
                                 all.x = TRUE)
      
      ### if missing, replace with agency code - these are all lea_code values
      rcd_adm[is.na(lea_code)]
      rcd_adm[is.na(lea_code), lea_code := agency_code]
      
  ## how many schools of each type in each district?
  district_counts_schools_bytype = rcd_adm[!(agency_code %like% 'LEA'),
                                           .(school_n_adm = .N), 
                                           by=c('category_code', 'lea_code', 'year')]
  
  ## how many students?
    ## rcd_adm has observations at the school, school type, and district level
    ## school observations have unique agency ids. average daily attendance is over the year at that school
    ## school type observations have a non-missing value for category_code in the original file. i merged in category code at the school level, so to distinguish,
      ## these are obs with category_code and agency_code == lea_code. average daily attendance is among all schools of that type
    ## district observations have agency_code == lea_code and missing category_code. average daily attendance is sum across all schools
    
    ### filter to district values only
    district_counts_student = rcd_adm[agency_code %like% 'LEA']
    district_counts_student[, agency_code := NULL] ## do not need, identical to lea_code
  
    ### demonstrate that charter aggregation is valid: use same approach with non-charters, compare to given values
  
      ### isolate non-charters
      noncharters = rcd_adm[lea_code != agency_code & lea_code != 'CH']
      
      ### calculate average across school type
      noncharters_avg_by_type = noncharters[, .(avg_student_num_test = round(mean(avg_student_num))), by=c('year', 'category_code', 'lea_code')]
      
      ### calculate total average within year
      noncharters_avg_year = noncharters[, .(avg_student_num_test = round(sum(avg_student_num))), by=c('year', 'lea_code')]
      
      ### combine 
      noncharters_test = rbind(noncharters_avg_by_type,
                               noncharters_avg_year,
                               fill = TRUE)
      noncharters_test[is.na(category_code), category_code := '']
      
      ### compare
      nrow(noncharters_test)
      noncharters_test = merge.data.table(noncharters_test,
                                          district_counts_student,
                                          by=c('lea_code', 'year', 'category_code'),
                                          all.x = TRUE)
      nrow(noncharters_test)
      
      noncharters_test[, diff := avg_student_num - avg_student_num_test]
      
        ### off by 1 - rounding errors
      
    ### construct summary entries for charters
    
      ### isolate charter records
      charters = rcd_adm[lea_code == 'CH']
      
      ### remove obs not in analytical sample
      charters = merge.data.table(charters,
                                  balanced_for_some_test,
                                  by=c('agency_code'),
                                  all.x = TRUE)
      nrow(charters)
      charters = charters[!(lea_code == 'CH' & is.na(charter_keep))]
      nrow(charters)
      
      ### calculate average across school type
      charters_avg_by_type = charters[, .(avg_student_num = round(mean(avg_student_num))), by=c('year', 'category_code')]
      
      ### calculate total average within year
      charters_avg_year = charters[, .(avg_student_num = round(sum(avg_student_num))), by=c('year')]
      
      ### combine 
      charters_adm = rbind(charters_avg_by_type,
                           charters_avg_year,
                           fill = TRUE)
      
      charters_adm[is.na(category_code), category_code := '']
      charters_adm[, lea_code := 'CH']
    
    ### add charters
    district_counts_student = rbind(district_counts_student,
                                    charters_adm,
                                    fill = TRUE)
    
    ### merge in school counts by type to go from average to total
    nrow(district_counts_student)
    district_counts_student = merge.data.table(district_counts_student,
                                               district_counts_schools_bytype,
                                               by=c('year', 'lea_code', 'category_code'),
                                               all.x = TRUE)
    nrow(district_counts_student)
    
    ### create category variable from category_code
    district_counts_student[category_code == 'E', category := 'elementary']
    district_counts_student[category_code == 'M', category := 'middle']
    district_counts_student[category_code == 'H', category := 'high']
    district_counts_student[category_code == 'A', category := 'elem/middle/high']
    district_counts_student[category_code == 'I', category := 'elem/middle']
    district_counts_student[category_code == 'T', category := 'middle/high']
    district_counts_student[category_code == '', category := 'all']
    district_counts_student[, category_code := NULL]
      
    ### totals
    district_counts_student[, student_num := avg_student_num * school_n_adm]
    district_counts_student[category == 'all', student_num := avg_student_num]
  
# save data ----
fwrite(district_counts_student, paste0(workdatadir, 'prep_district_student_counts_asamp.csv'))