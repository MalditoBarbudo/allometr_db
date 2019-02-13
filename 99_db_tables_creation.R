#### Allometries main table ####
ALLOMETRIES %>%
  copy_to(
    allometr_db, df = ., name = 'ALLOMETRIES', overwrite = TRUE, temporary = FALSE
  )
