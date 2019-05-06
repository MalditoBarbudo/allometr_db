#### Allometries main table ####
temp_allometries_creaf %>%
  copy_to(
    allometr_db, df = ., name = 'ALLOMETRIES', overwrite = TRUE, temporary = FALSE
  )

thesaurus_variables_creaf %>%
  copy_to(
    allometr_db, df = ., name = 'THESAURUS_VARIABLES', overwrite = TRUE, temporary = FALSE
  )

thesaurus_cubication_shape %>%
  copy_to(
    allometr_db, df = ., name = 'THESAURUS_CUBICATION', overwrite = TRUE, temporary = FALSE
  )

thesaurus_sources %>%
  copy_to(
    allometr_db, df = ., name = 'THESAURUS_SOURCES', overwrite = TRUE, temporary = FALSE
  )

thesaurus_allo_level_creaf %>%
  copy_to(
    allometr_db, df = ., name = 'THESAURUS_ALLO_LEVEL', overwrite = TRUE, temporary = FALSE
  )

thesaurus_functional_group_level_creaf %>%
  copy_to(
    allometr_db, df = ., name = 'THESAURUS_FG_LEVEL', overwrite = TRUE, temporary = FALSE
  )

thesaurus_spatial_level_creaf %>%
  copy_to(
    allometr_db, df = ., name = 'THESAURUS_SPATIAL_LEVEL', overwrite = TRUE, temporary = FALSE
  )

thesaurus_special_param_creaf %>%
  copy_to(
    allometr_db, df = ., name = 'THESAURUS_SPECIAL_PARAM', overwrite = TRUE, temporary = FALSE
  )

app_translations %>%
  copy_to(
    allometr_db, df = ., name = 'THESAURUS_APP', overwrite = TRUE, temporary = FALSE
  )
