# loading the Catalonia level allometries created in the CREAF (or from other sources)
# present in the equacio table from oracle_db

## temporal creaf allometries table ####
# variable renaming
tbl(oracle_db, 'equacio') %>%
  rename(
    spatial_name = ambit,
    functional_group_name = idespecie,
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
      spatial_name == 'Catalunya' ~ 'aut_community',
      spatial_name %in% c('Barcelona', 'Girona', 'Tarragona', 'Lleida') ~ 'province',
      TRUE ~ 'county'
    ),
    functional_group_level = 'species'
    ## TODO revisar el grupo funcional, no todas son especies
    # functional_group_level = case_when(
    #   length(str_split(functional_group_name, ' ') > 2) ~ 'species',
    #   TRUE ~ 'genus'
    # )
  ) %>%
  select(
    spatial_level, spatial_name,
    functional_group_level, functional_group_name,
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
    spatial_level, spatial_name, functional_group_level, functional_group_name,
    dependent_var, independent_var_1, independent_var_2, independent_var_3, equation,
    everything(), -var1, -var2, -var3, -contains('_description_')
  ) -> temp_allometries_catalonia

## temporal creaf spain level allometries ####
tbl(oracle_db, 'equacio_espanya') %>%
  rename(
    spatial_name = ambit,
    functional_group_name = idespecie,
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
    tbl(oracle_db, 'tesaureprovincia') %>% select(spatial_name = idprovincia, nom) %>% collect(),
    by = 'spatial_name'
  ) %>%
  left_join(
    tbl(oracle_db, 'tesaureespecie_espanya') %>% select(functional_group_name = idespecie, especie) %>% collect(),
    by = 'functional_group_name'
  ) %>%
  mutate(
    spatial_name = case_when(
      spatial_name == 'Catalunya' ~ 'Catalunya',
      spatial_name == 'Espanya' ~ 'España',
      TRUE ~ nom
    ),
    spatial_level = case_when(
      spatial_name == 'España' ~ 'country',
      spatial_name %in% c(
        'Asturias', 'Cantabria', 'Catalunya', 'La Rioja', 'Madrid', 'Navarra', 'Murcia'
      ) ~ 'aut_community',
      TRUE ~ 'province'
    ),
    functional_group_name = especie,
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
    spatial_level, spatial_name, functional_group_level, functional_group_name,
    dependent_var, independent_var_1, independent_var_2, independent_var_3, equation,
    everything(), -var1, -var2, -var3, -contains('_description_'), -concentraciocarboni,
    -util
  ) -> temp_allometries_spain

temp_allometries_catalonia %>%
  mutate(see = NA_integer_) %>%
  union(temp_allometries_spain) %>%
  # fixing vars
  mutate(
    # fixing species
    # for that, some assumtions are made:
    # 1. length of fg name = 1, then genus (there is no families I'm aware of)
    # 2. Especial groups: Conifers wo pines, Riparian species, other ripioles species...
    fg_name_length = str_count(functional_group_name, ' '),
    functional_group_level = case_when(
      fg_name_length == 0 ~ 'genus',
      fg_name_length > 4 ~ 'species_group',
      functional_group_name == 'Abies sp' ~ 'genus',
      functional_group_name %in% c(
        "Coníferes excepte pins", "Arbres de ribera"
      ) ~ 'species_group',
      str_detect(functional_group_name, 'Altres ') ~ 'species_group',
      TRUE ~ functional_group_level
    ),
    # adding the allometry level (forest, tree, organ...)
    allometry_level = if_else(independent_var_1 == 'DR', 'organ', 'tree'),
    allometry_name = if_else(allometry_level == 'tree', 'tree', 'branch')
  ) %>%
  # selecting/reordering the variables
  select(
    # levels
    allometry_level, spatial_level, functional_group_level,
    # names
    allometry_name, spatial_name, functional_group_name,
    # vars
    dependent_var, independent_var_1, independent_var_2, independent_var_3,
    # equation and params
    equation, param_a, param_b, param_c, param_d,
    special_param, cubication_shape, source,
    # quality
    n_obs, r_sqr, see
  ) -> temp_allometries_creaf
