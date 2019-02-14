#### Allometries main table ####
temp_allometries_creaf %>%
  copy_to(
    allometr_db, df = ., name = 'ALLOMETRIES', overwrite = TRUE, temporary = FALSE
  )

thesaurus_variables_creaf %>%
  copy_to(
    allometr_db, df = ., name = 'THESAURUS_VARIABLES', overwrite = TRUE, temporary = FALSE
  )
