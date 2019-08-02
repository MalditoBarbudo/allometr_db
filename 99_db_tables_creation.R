#### Allometries main table ####
temp_allometries_creaf %>%
  copy_to(
    allometr_db, df = ., name = tolower('ALLOMETRIES'), overwrite = TRUE, temporary = FALSE
  )

thesaurus_variables_creaf %>%
  copy_to(
    allometr_db, df = ., name = tolower('THESAURUS_VARIABLES'), overwrite = TRUE, temporary = FALSE
  )

thesaurus_cubication_shape %>%
  copy_to(
    allometr_db, df = ., name = tolower('THESAURUS_CUBICATION'), overwrite = TRUE, temporary = FALSE
  )

thesaurus_sources %>%
  copy_to(
    allometr_db, df = ., name = tolower('THESAURUS_SOURCES'), overwrite = TRUE, temporary = FALSE
  )

thesaurus_allo_level_creaf %>%
  copy_to(
    allometr_db, df = ., name = tolower('THESAURUS_ALLO_LEVEL'), overwrite = TRUE, temporary = FALSE
  )

thesaurus_functional_group_level_creaf %>%
  copy_to(
    allometr_db, df = ., name = tolower('THESAURUS_FG_LEVEL'), overwrite = TRUE, temporary = FALSE
  )

thesaurus_spatial_level_creaf %>%
  copy_to(
    allometr_db, df = ., name = tolower('THESAURUS_SPATIAL_LEVEL'), overwrite = TRUE, temporary = FALSE
  )

thesaurus_special_param_creaf %>%
  copy_to(
    allometr_db, df = ., name = tolower('THESAURUS_SPECIAL_PARAM'), overwrite = TRUE, temporary = FALSE
  )

app_translations %>%
  copy_to(
    allometr_db, df = ., name = tolower('THESAURUS_APP'), overwrite = TRUE, temporary = FALSE
  )
