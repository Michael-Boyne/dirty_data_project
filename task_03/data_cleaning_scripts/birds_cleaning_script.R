

bird_data_record <- read_excel("~/dirty_data_project/task_03/data/seabirds.xls", sheet = 2)
bird_data_codes <- read_excel("~/dirty_data_project/task_03/data/seabirds.xls", sheet = 4)


clean_birds <- bird_data_record %>%
  select(RECORD, 
         `RECORD ID`, 
         `Species common name (taxon [AGE / SEX / PLUMAGE PHASE])`, 
         `Species  scientific name (taxon [AGE /SEX /  PLUMAGE PHASE])`, 
         `Species abbreviation`, 
         COUNT
) %>%
  
