# loading the Catalonia level allometries created in the CREAF (or from other sources)
# present in the equacio table from oracle_db

## temporal creaf allometries table ####
# variable renaming
tbl(oracle_db, 'equacio') %>%
  # Dn in mm must be converted to cm in the equation
  mutate(
    equacio = case_when(
      equacio == "VCC = a +b (Dn)².Ht" ~ "VCC = a +b ((Dn·10))²·Ht",
      equacio == "BH = a ·Dn^b" ~ "BH = a ·(Dn·10)^b",
      equacio == "BR = a ·Dn^b" ~ "BR = a ·(Dn·10)^b",
      equacio == "DP = a ·Dn^b" ~ "DP = a ·(Dn·10)^b",
      equacio == "IAVC = a + b.Dn + c.Dn² + d.Dn³" ~ "IAVC = a + b.(Dn·10) + c.(Dn·10)² + d.(Dn·10)³",
      equacio == "VLE = a + b.Dn + c.Dn² + d.Dn³" ~ "VLE = a + b.(Dn·10) + c.(Dn·10)² + d.(Dn·10)³",
      TRUE ~ equacio
    )
  ) %>% 
  rename(
    spatial_level_name = ambit,
    functional_group_level_name = idespecie,
    cubication_shape = formacubicacio,
    variables = variables,
    source = origen,
    special_param = parametreespecial,
    equation = equacio,
    param_a = a,
    param_b = b,
    param_c = c,
    param_d = d,
    n_obs = n,
    r_sqr = r2
  ) %>%
  collect() %>%
  mutate(
    spatial_level = case_when(
      spatial_level_name == 'Catalunya' ~ 'aut_community',
      spatial_level_name %in% c('Barcelona', 'Girona', 'Tarragona', 'Lleida') ~ 'province',
      TRUE ~ 'county'
    ),
    functional_group_level = 'species'
    ## TODO revisar el grupo funcional, no todas son especies
    # functional_group_level = case_when(
    #   length(str_split(functional_group_level_name, ' ') > 2) ~ 'species',
    #   TRUE ~ 'genus'
    # )
  ) %>%
  select(
    spatial_level, spatial_level_name,
    functional_group_level, functional_group_level_name,
    everything()
  ) %>%
  separate(variables, c('var1', 'var2', 'var3'), sep = ' - ') %>%
  mutate(
    # create the vars variables
    dependent_var = if_else(is.na(var3), var2, var3),
    independent_var_1 = var1,
    independent_var_2 = if_else(is.na(var3), NA_character_, var2),
    independent_var_3 = NA_character_
  ) %>%
  separate(
    # now we must retain the abbreviation and create a column for the name_cat
    dependent_var, c('dependent_var', 'dv_description_cat'), sep = '='
  ) %>%
  mutate(
    dependent_var = str_trim(dependent_var),
    dv_description_cat = str_trim(dv_description_cat),
  ) %>%
  # the same for the independent variables
  separate(
    independent_var_1, c('independent_var_1', 'iv1_description_cat'), sep = '='
  ) %>%
  separate(
    independent_var_2, c('independent_var_2', 'iv2_description_cat'), sep = '='
  ) %>%
  separate(
    independent_var_3, c('independent_var_3', 'iv3_description_cat'), sep = '='
  ) %>%
  mutate(
    allometry_level_name = NA_character_,
    # removing trailing spaces
    independent_var_1 = str_trim(independent_var_1),
    iv1_description_cat = str_trim(iv1_description_cat),
    independent_var_2 = str_trim(independent_var_2),
    iv2_description_cat = str_trim(iv2_description_cat),
    independent_var_3 = str_trim(independent_var_3),
    iv3_description_cat = str_trim(iv3_description_cat),
    # uniformization of the equations
    equation = str_replace_all(equation, '\\.', '·'),
    equation = str_replace_all(equation, '\\+b \\(', '+ b·('),
    equation = str_replace_all(equation, ' · ', '·'),
    equation = str_replace_all(equation, ' ·', '·'),
    equation = str_replace_all(equation, '· ', '·'),
    # Dc in the equations appears as DP, fix it
    equation = str_replace_all(equation, 'DP =', 'DC ='),
    # Dc and DC uniformization
    dependent_var = if_else(dependent_var == 'Dc', 'DC', dependent_var),
    # cubication shapes totes to any
    cubication_shape = if_else(cubication_shape == 'Totes', 'Any', cubication_shape)
  ) %>%
  # pull(equation)
  select(
    allometry_level_name, spatial_level, spatial_level_name, functional_group_level, functional_group_level_name,
    dependent_var, independent_var_1, independent_var_2, independent_var_3, equation,
    everything(), -var1, -var2, -var3, -contains('_description_')
  ) -> temp_allometries_catalonia

## temporal creaf spain level allometries ####
tbl(oracle_db, 'equacio_espanya') %>%
  rename(
    spatial_level_name = ambit,
    functional_group_level_name = idespecie,
    cubication_shape = formacubicacio,
    variables = variables,
    source = origen,
    special_param = parametreespecial,
    equation = equacio,
    param_a = a,
    param_b = b,
    param_c = c,
    param_d = d,
    n_obs = n,
    r_sqr = r2,
    see = see
  ) %>%
  collect() %>%
  left_join(
    tbl(oracle_db, 'tesaureprovincia') %>% select(spatial_level_name = idprovincia, nom) %>% collect(),
    by = 'spatial_level_name'
  ) %>%
  left_join(
    tbl(oracle_db, 'tesaureespecie_espanya') %>% select(functional_group_level_name = idespecie, especie) %>% collect(),
    by = 'functional_group_level_name'
  ) %>%
  mutate(
    spatial_level_name = case_when(
      spatial_level_name == 'Catalunya' ~ 'Catalunya',
      spatial_level_name == 'Espanya' ~ 'España',
      TRUE ~ nom
    ),
    spatial_level = case_when(
      spatial_level_name == 'España' ~ 'country',
      spatial_level_name %in% c(
        'Asturias', 'Cantabria', 'Catalunya', 'La Rioja', 'Madrid', 'Navarra', 'Murcia'
      ) ~ 'aut_community',
      TRUE ~ 'province'
    ),
    functional_group_level_name = especie,
    functional_group_level = 'species'
  ) %>%
  select(-nom, -especie) %>%
  separate(variables, c('var1', 'var2', 'var3'), sep = '( - )|( i )|(- )') %>%
  mutate(
    # create the vars variables
    dependent_var = if_else(is.na(var3), var2, var3),
    independent_var_1 = var1,
    independent_var_2 = if_else(is.na(var3), NA_character_, var2),
    independent_var_3 = NA_character_,
    # In the case of P_BST, the order is inverted as the first one is de dependent, we
    # must take care of it
    dependent_var = if_else(str_detect(dependent_var, '^BAT = '), var1, dependent_var),
    independent_var_1 = if_else(str_detect(independent_var_1, '^P_BST='), var2, independent_var_1)
  ) %>%
  separate(
    # now we must retain the abbreviation and create a column for the name_cat
    dependent_var, c('dependent_var', 'dv_description_cat'), sep = '='
  ) %>%
  mutate(
    dependent_var = str_trim(dependent_var),
    dv_description_cat = str_trim(dv_description_cat),
  ) %>%
  # the same for the independent variables
  separate(
    independent_var_1, c('independent_var_1', 'iv1_description_cat'), sep = '='
  ) %>%
  separate(
    independent_var_2, c('independent_var_2', 'iv2_description_cat'), sep = '='
  ) %>%
  separate(
    independent_var_3, c('independent_var_3', 'iv3_description_cat'), sep = '='
  ) %>%
  mutate(
    allometry_level_name = NA_character_,
    # removing trailing spaces
    independent_var_1 = str_trim(independent_var_1),
    iv1_description_cat = str_trim(iv1_description_cat),
    independent_var_2 = str_trim(independent_var_2),
    iv2_description_cat = str_trim(iv2_description_cat),
    independent_var_3 = str_trim(independent_var_3),
    iv3_description_cat = str_trim(iv3_description_cat),
    # uniformization of the equations
    equation = str_replace_all(equation, ' · ', '·'),
    equation = str_replace_all(equation, ' \\^ ', '\\^'),
    # cubication shapes totes to any
    cubication_shape = if_else(cubication_shape == 'Totes', 'Any', cubication_shape)
  ) %>%
  # pull(equation)
  select(
    allometry_level_name, spatial_level, spatial_level_name, functional_group_level, functional_group_level_name,
    dependent_var, independent_var_1, independent_var_2, independent_var_3, equation,
    everything(), -var1, -var2, -var3, -contains('_description_'), -concentraciocarboni,
    -util
  ) -> temp_allometries_spain

## miquel allometries ####
temp_allometries_miquel <- readxl::read_excel('Tree_Allometries_Miquel.xlsx', 2) %>%
  dplyr::slice(1:34) %>%
  dplyr::rename(
    # spatial_level_name = ambit,
    functional_group_level_name = Name,
    # cubication_shape = formacubicacio,
    # variables = variables,
    source = Source,
    # special_param = parametreespecial,
    # equation = equacio,
    param_a = a_fbt,
    param_b = b_fbt,
    param_c = c_fbt,
    param_d = d_fbt,
    n_obs = n,
    r_sqr = r2
  ) %>%
  dplyr::mutate(
    allometry_level_name = NA_character_,
    spatial_level = 'aut_community',
    spatial_level_name = 'Catalunya',
    functional_group_level = 'species',
    dependent_var = 'BH',
    independent_var_1 = 'DBH',
    independent_var_2 = 'BAL',
    independent_var_3 = NA_character_,
    equation = 'BH = a·DBH^b·exp(c·BAL)·DBH^(d·BAL)',
    cubication_shape = 'Any',
    special_param = "No n'hi ha",
    see = NA_real_,
    n_obs = as.numeric(n_obs)
    # miquel allometries calculate BH in kg, so we need to mutate param_a to
    # change it to g
    # param_a = param_a*1000
  ) %>%
  dplyr::select(
    allometry_level_name, spatial_level, spatial_level_name, functional_group_level, functional_group_level_name,
    dependent_var, independent_var_1, independent_var_2, independent_var_3, equation,
    cubication_shape, source, special_param, param_a, param_b, param_c, param_d,
    n_obs, r_sqr, see
  )

## MedFuels allometries ####
temp_allometries_medfuels_bat <- readr::read_delim(
  'Wtotal_Vtotal.txt', delim = '\t', locale = readr::locale(decimal_mark = ',')
) %>%
  dplyr::select(
    ## TODO fix species names
    functional_group_level_name = Name,
    param_a = a1,
    param_b = b1,
    n_obs = n,
    r_sqr = sp_r2
  ) %>%
  dplyr::mutate(
    allometry_level_name = 'shrub',
    spatial_level = 'aut_community',
    spatial_level_name = 'Catalunya',
    functional_group_level = 'species',
    dependent_var = 'BAT',
    independent_var_1 = 'PHV',
    independent_var_2 = NA_character_,
    independent_var_3 = NA_character_,
    equation = 'BAT = a · PHV^b',
    cubication_shape = 'Any',
    source = 'De Cáceres (submitted, AFSC-D-19-*****)',
    special_param = "No n'hi ha",
    param_c = NA_real_,
    param_d = NA_real_,
    see = NA_real_
  ) %>%
  dplyr::select(
    allometry_level_name, spatial_level, spatial_level_name, functional_group_level, functional_group_level_name,
    dependent_var, independent_var_1, independent_var_2, independent_var_3, equation,
    cubication_shape, source, special_param, param_a, param_b, param_c, param_d,
    n_obs, r_sqr, see
  )

temp_allometries_medfuels_batf <- readr::read_delim(
  'Wfine_Vtotal.txt', delim = '\t', locale = readr::locale(decimal_mark = ',')
) %>%
  dplyr::select(
    ## TODO fix species names
    functional_group_level_name = Name,
    param_a = a1,
    param_b = b1,
    n_obs = n,
    r_sqr = sp_r2
  ) %>%
  dplyr::mutate(
    allometry_level_name = 'shrub',
    spatial_level = 'aut_community',
    spatial_level_name = 'Catalunya',
    functional_group_level = 'species',
    dependent_var = 'BFAT',
    independent_var_1 = 'PHV',
    independent_var_2 = NA_character_,
    independent_var_3 = NA_character_,
    equation = 'BFAT = a · PHV^b',
    cubication_shape = 'Any',
    source = 'De Cáceres (submitted, AFSC-D-19-*****)',
    special_param = "No n'hi ha",
    param_c = NA_real_,
    param_d = NA_real_,
    see = NA_real_
  ) %>%
  dplyr::select(
    allometry_level_name, spatial_level, spatial_level_name, functional_group_level, functional_group_level_name,
    dependent_var, independent_var_1, independent_var_2, independent_var_3, equation,
    cubication_shape, source, special_param, param_a, param_b, param_c, param_d,
    n_obs, r_sqr, see
  )

temp_allometries_medfuels_area <- readr::read_delim(
  'Area_H.txt', delim = '\t', locale = readr::locale(decimal_mark = ',')
) %>%
  dplyr::select(
    ## TODO fix species names
    functional_group_level_name = Name,
    param_a = a2,
    param_b = b2,
    n_obs = n,
    r_sqr = sp_r2
  ) %>%
  dplyr::mutate(
    allometry_level_name = 'shrub',
    spatial_level = 'aut_community',
    spatial_level_name = 'Catalunya',
    functional_group_level = 'species',
    dependent_var = 'Area',
    independent_var_1 = 'Ht',
    independent_var_2 = NA_character_,
    independent_var_3 = NA_character_,
    equation = 'Area = a · Ht^b',
    cubication_shape = 'Any',
    source = 'De Cáceres (submitted, AFSC-D-19-*****)',
    special_param = "No n'hi ha",
    param_c = NA_real_,
    param_d = NA_real_,
    see = NA_real_
  ) %>%
  dplyr::select(
    allometry_level_name, spatial_level, spatial_level_name, functional_group_level, functional_group_level_name,
    dependent_var, independent_var_1, independent_var_2, independent_var_3, equation,
    cubication_shape, source, special_param, param_a, param_b, param_c, param_d,
    n_obs, r_sqr, see
  )

## final table ####
temp_allometries_catalonia %>%
  mutate(see = NA_integer_) %>%
  union(temp_allometries_spain) %>%
  union(temp_allometries_miquel) %>%
  union(temp_allometries_medfuels_bat) %>%
  union(temp_allometries_medfuels_batf) %>%
  union(temp_allometries_medfuels_area) %>%
  # fixing vars
  mutate(
    # fixing species
    # for that, some assumtions are made:
    # 1. length of fg name = 1, then genus (there is no families I'm aware of)
    # 2. Especial groups: Conifers wo pines, Riparian species, other ripioles species...
    fg_name_length = str_count(functional_group_level_name, ' '),
    functional_group_level = case_when(
      fg_name_length == 0 ~ 'genus',
      fg_name_length > 4 ~ 'species_group',
      functional_group_level_name == 'Abies sp' ~ 'genus',
      functional_group_level_name %in% c(
        "Coníferes excepte pins", "Arbres de ribera"
      ) ~ 'species_group',
      str_detect(functional_group_level_name, 'Altres ') ~ 'species_group',
      TRUE ~ functional_group_level
    ),
    # adding the allometry level (forest, tree, organ...)
    allometry_level = if_else(independent_var_1 == 'DR', 'organ', 'tree'),
    # allometry_level_name = if_else(allometry_level == 'tree', 'tree', 'branch'),
    allometry_level_name = dplyr::case_when(
      is.na(allometry_level_name) ~ if_else(allometry_level == 'tree', 'tree', 'branch'),
      TRUE ~ allometry_level_name
    ),
    # changing Dn* to DBH_SC in all ocurrences
    dependent_var = stringr::str_replace_all(dependent_var, 'Dn\\*', 'DBH_SC'),
    independent_var_1 = stringr::str_replace_all(independent_var_1, 'Dn\\*', 'DBH_SC'),
    independent_var_2 = stringr::str_replace_all(independent_var_2, 'Dn\\*', 'DBH_SC'),
    independent_var_3 = stringr::str_replace_all(independent_var_3, 'Dn\\*', 'DBH_SC'),
    equation = stringr::str_replace_all(equation, 'Dn\\*', 'DBH_SC'),
    # changing Dn to DBH in all ocurrences
    dependent_var = stringr::str_replace_all(dependent_var, 'Dn', 'DBH'),
    independent_var_1 = stringr::str_replace_all(independent_var_1, 'Dn', 'DBH'),
    independent_var_2 = stringr::str_replace_all(independent_var_2, 'Dn', 'DBH'),
    independent_var_3 = stringr::str_replace_all(independent_var_3, 'Dn', 'DBH'),
    equation = stringr::str_replace_all(equation, 'Dn', 'DBH'),
    # changing VCC to VOB in all ocurrences
    dependent_var = stringr::str_replace_all(dependent_var, 'VCC', 'VOB'),
    independent_var_1 = stringr::str_replace_all(independent_var_1, 'VCC', 'VOB'),
    independent_var_2 = stringr::str_replace_all(independent_var_2, 'VCC', 'VOB'),
    independent_var_3 = stringr::str_replace_all(independent_var_3, 'VCC', 'VOB'),
    equation = stringr::str_replace_all(equation, 'VCC', 'VOB'),
    # changing VSC to VUB in all ocurrences
    dependent_var = stringr::str_replace_all(dependent_var, 'VSC', 'VUB'),
    independent_var_1 = stringr::str_replace_all(independent_var_1, 'VSC', 'VUB'),
    independent_var_2 = stringr::str_replace_all(independent_var_2, 'VSC', 'VUB'),
    independent_var_3 = stringr::str_replace_all(independent_var_3, 'VSC', 'VUB'),
    equation = stringr::str_replace_all(equation, 'VSC', 'VUB'),
    # creating the allometry_id variable
    allometry_id = glue::glue("{dependent_var}_{row_number(dependent_var)}")
  ) %>%
  # selecting/reordering the variables
  select(
    # id
    allometry_id,
    # levels and names
    allometry_level, allometry_level_name,
    spatial_level, spatial_level_name,
    functional_group_level, functional_group_level_name,
    # vars
    dependent_var, independent_var_1, independent_var_2, independent_var_3,
    # equation and params
    equation, param_a, param_b, param_c, param_d,
    special_param, cubication_shape, source,
    # quality
    n_obs, r_sqr, see
  ) -> temp_allometries_creaf


## Fixing species names
offending_species <- c(
  "Evonimus europaeus",
  "Robinia pseudacacia",
  "Salix elaeagnos",
  "Eucalyptus vimitalis",
  "Eucalyptus gomphocephallus",
  "Crataegus lacinata"
)

correct_species <- c(
  "Euonymus europaeus",
  "Robinia pseudoacacia",
  "Salix eleagnos",
  "Eucalyptus viminalis",
  "Eucalyptus gomphocephala",
  "Crataegus laciniata"
)

temp_allometries_creaf <-
  temp_allometries_creaf |>
  dplyr::mutate(
    functional_group_level_name = dplyr::case_match(
      functional_group_level_name,
      "Evonimus europaeus" ~ "Euonymus europaeus",
      "Robinia pseudacacia" ~ "Robinia pseudoacacia",
      "Salix elaeagnos" ~ "Salix eleagnos",
      "Eucalyptus vimitalis" ~ "Eucalyptus viminalis",
      "Eucalyptus gomphocephallus" ~ "Eucalyptus gomphocephala",
      "Crataegus lacinata" ~ "Crataegus laciniata",
      .default = functional_group_level_name) 
  )
