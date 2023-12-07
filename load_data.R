########################
# Load data            #
########################

# load all data stored in excel files and save as individual .csv files, for fast loading later

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
  
# load/save data in csv ----
  
  ## school-level information
  rcd_location = data.table(read_excel(paste0(rawdatadir, 'src_datasets-SY21-22/rcd_location.xlsx')))
  fwrite(rcd_location, paste0(workdatadir, 'rcd_location.csv'))
  
  ## average daily membership
  rcd_adm = data.table(read_excel(paste0(rawdatadir, 'src_datasets-SY21-22/rcd_adm.xlsx')))
  fwrite(rcd_adm, paste0(workdatadir, 'rcd_adm.csv'))
  
  ## teacher counts
  rcd_eq = data.table(read_excel(paste0(rawdatadir, 'src_datasets-SY21-22/rcd_eq.xlsx')))
  fwrite(rcd_eq, paste0(workdatadir, 'rcd_eq.csv'))
  
  ## demographics 
  
    ### economically disadvantaged students
    rcd_acc_eds = data.table(read_excel(paste0(rawdatadir, 'src_datasets-SY21-22/rcd_acc_eds.xlsx')))
    fwrite(rcd_acc_eds, paste0(workdatadir, 'rcd_acc_eds.csv'))
    
  ## performance grades
  
    ## 2022-23
    spg_23 = data.table(read_excel(paste0(rawdatadir, '2022-23 School Performance Grades.xlsx'), sheet = 'SPG Data For Download'))
    fwrite(spg_23, paste0(workdatadir, 'spg_23.csv'))
    
  ## school assessments
    
    ## years available
    school_assessment_files = list(c('23', '2022-23 School Assessment and Other Indicator Data.xlsx', 'Assess-Ind Data Set', 0),
                                   c('22', '2021-22 School Assessment and Other Indicator Data.xlsx', 'Assess-Ind Data Set', 1),
                                   c('21', '2020-21 School Assessment and Other Indicator Data.xlsx', 'Assess-Ind Data Set', 0),
                                   c('21_o', '2020-21 School Assessment and Other Indicator Data_1.xlsx', 'Assess-Ind Data Set', 0),
                                   c('19', 'data-report2019_final.xlsx', 'Assess-Ind Data Set', 0),
                                   c('18', 'data-report.xlsx', 'Assess-Ind Data Set', 0))
    
    ## loop through and load
    for(s in school_assessment_files) {
      
      cat(as.numeric(s[4]))
      
      ### load, allowing for skips
      if(as.numeric(s[4]) > 0) {
        school_assessment = data.table(read_excel(paste0(rawdatadir, 'school_assessments/', s[2]), sheet = s[3], skip = as.numeric(s[4])))
      } else {
        school_assessment = data.table(read_excel(paste0(rawdatadir, 'school_assessments/', s[2]), sheet = s[3]))
      }
      
      ### filter: currently only need reading and EOG scores, for all students
      school_assessment = school_assessment[subject %like% '^RD|^EOG' & subgroup == 'ALL']
      
      ### save
      fwrite(school_assessment, paste0(workdatadir, 'school_assessment_', s[1], '.csv'))
      
      ### remove to save space
      rm(school_assessment)
      
    }
    
    ## coodebooks
    school_assessment_23 = data.table(read_excel(paste0(rawdatadir, 'school_assessments/2022-23 School Assessment and Other Indicator Data.xlsx'), sheet = ' Asses-Ind Data Set Format'))
    school_assessment_22 = data.table(read_excel(paste0(rawdatadir, 'school_assessments/2021-22 School Assessment and Other Indicator Data.xlsx'), sheet = ' Asses-Ind Data Set Format'))
    school_assessment_21 = data.table(read_excel(paste0(rawdatadir, 'school_assessments/2020-21 School Assessment and Other Indicator Data.xlsx'), sheet = ' Asses-Ind Data Set Format'))
    school_assessment_19 = data.table(read_excel(paste0(rawdatadir, 'school_assessments/data-report2019_final.xlsx'), sheet = ' Asses-Ind Data Set Format'))
    school_assessment_18 = data.table(read_excel(paste0(rawdatadir, 'school_assessments/data-report.xlsx'), sheet = ' Asses-Ind Data Set Format'))
    
    setnames(school_assessment_18, c('var', 'y18'))
    setnames(school_assessment_19, c('var', 'y19'))
    setnames(school_assessment_21, c('var', 'y21'))
    setnames(school_assessment_22, c('var', 'y22'))
    setnames(school_assessment_23, c('var', 'y23'))
    
    
    
    school_assessment_codebook_comp = merge.data.table(school_assessment_18,
                                                       school_assessment_19,
                                                       by=c('var'),
                                                       all = TRUE)
    
    school_assessment_codebook_comp = merge.data.table(school_assessment_codebook_comp,
                                                       school_assessment_21,
                                                       by=c('var'),
                                                       all = TRUE)
    
    school_assessment_codebook_comp = merge.data.table(school_assessment_codebook_comp,
                                                       school_assessment_22,
                                                       by=c('var'),
                                                       all = TRUE)
    
    school_assessment_codebook_comp = merge.data.table(school_assessment_codebook_comp,
                                                       school_assessment_23,
                                                       by=c('var'),
                                                       all = TRUE)
    
    school_assessment_codebook_comp =school_assessment_codebook_comp[!is.na(var)]
    