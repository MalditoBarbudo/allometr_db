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
  ) -> temp_allometries_creaf

## thesaurus variables ####
# lets create a thesuarus of variable names
temp_allometries %>% gather('variable', 'var_id', contains('_var')) %>%
  select(var_id) %>% distinct() %>% filter(!is.na(var_id)) %>%
  mutate(
    translation_cat = case_when(
      var_id == 'BRH' ~ glue::glue("Biomassa de branques amb fulles ({var_id})"),
      var_id == 'BH' ~ glue::glue("Biomassa de fulles ({var_id})"),
      var_id == 'Ht' ~ glue::glue("Altura total ({var_id})"),
      var_id == 'DC' ~ glue::glue("Diàmetre de capçada ({var_id})"),
      var_id == 'GC' ~ glue::glue("Gruix d'escorça ({var_id})"),
      var_id == 'VCC' ~ glue::glue("Volum amb escorça ({var_id})"),
      var_id == 'IAVC' ~ glue::glue("Increment anual del volum amb escorça ({var_id})"),
      var_id == 'VLE' ~ glue::glue("Volum de llenyes ({var_id})"),
      var_id == 'VSC' ~ glue::glue("Volum sense escorça ({var_id})"),
      var_id == 'BR' ~ glue::glue("Biomassa de branques sense fulles ({var_id})"),
      var_id == 'BC' ~ glue::glue("Biomassa d'escorça ({var_id})"),
      var_id == 'VC' ~ glue::glue("Volum d'escorça ({var_id})"),
      var_id == 'BAT' ~ glue::glue("Biomassa Aèria Total ({var_id})"),
      var_id == 'BM' ~ glue::glue("Biomassa de fusta ({var_id})"),
      var_id == 'VM' ~ glue::glue("Volum de fusta ({var_id})"),
      var_id == 'DR' ~ glue::glue("Diàmetre de la branca ({var_id})"),
      var_id == 'Dn' ~ glue::glue("Diàmetre normal ({var_id})"),
      var_id == 'Dn*' ~ glue::glue("Diàmetre normal sense escorça ({var_id})")
    ),
    translation_eng = case_when(
      var_id == 'BRH' ~ 'Branches and leaves Biomass (BLB)',
      var_id == 'BH' ~ 'Leaves Biomass (LB)',
      var_id == 'Ht' ~ 'Total Height (Ht)',
      var_id == 'DC' ~ 'Crown diameter (DC)',
      var_id == 'GC' ~ 'Bark Thickness (BT)',
      var_id == 'VCC' ~ 'Over Bark Volume (OBV)',
      var_id == 'IAVC' ~ 'Annual Increment of Over Bark Volume (AIOBV)',
      var_id == 'VLE' ~ 'Fuel Wood Volume (FWV)',
      var_id == 'VSC' ~ 'Under Bark Volume (UBV)',
      var_id == 'BR' ~ 'Branches Biomass (BB)',
      var_id == 'BC' ~ 'Bark biomass (BB)',
      var_id == 'VC' ~ 'Bark Volume (BV)',
      var_id == 'BAT' ~ 'Total Aerial Biomas (ABt)',
      var_id == 'BM' ~ 'Wood Biomass (WB)',
      var_id == 'VM' ~ 'Wood Volume (WV)',
      var_id == 'DR' ~ 'Branch Diameter (BD)',
      var_id == 'Dn' ~ 'Diameter at Breast Height (DBH)',
      var_id == 'Dn*' ~ 'Diameter Inside Bark (DIB)'
    ),
    translation_spa = case_when(
      var_id == 'BRH' ~ glue::glue("Biomasa de ramas con hojas ({var_id})"),
      var_id == 'BH' ~ glue::glue("Biomasa de hojas ({var_id})"),
      var_id == 'Ht' ~ glue::glue("Altura total ({var_id})"),
      var_id == 'DC' ~ glue::glue("Diámetro de copa ({var_id})"),
      var_id == 'GC' ~ glue::glue("Grosor de corteza ({var_id})"),
      var_id == 'VCC' ~ glue::glue("Volumen con corteza ({var_id})"),
      var_id == 'IAVC' ~ glue::glue("Incremento anual del volumen con corteza ({var_id})"),
      var_id == 'VLE' ~ glue::glue("Volumen de leña ({var_id})"),
      var_id == 'VSC' ~ glue::glue("Volumen sin corteza ({var_id})"),
      var_id == 'BR' ~ glue::glue("Biomasa de ramas ({var_id})"),
      var_id == 'BC' ~ glue::glue("Biomasa de corteza ({var_id})"),
      var_id == 'VC' ~ glue::glue("Volumen de corteza ({var_id})"),
      var_id == 'BAT' ~ glue::glue("Biomasa aérea total ({var_id})"),
      var_id == 'BM' ~ glue::glue("Biomasa de madera ({var_id})"),
      var_id == 'VM' ~ glue::glue("Volumen de madera ({var_id})"),
      var_id == 'DR' ~ glue::glue("Diámetro de rama ({var_id})"),
      var_id == 'Dn' ~ glue::glue("Diámetro normal ({var_id})"),
      var_id == 'Dn*' ~ glue::glue("Diámetro sin corteza ({var_id})")
    ),
    var_units = case_when(
      var_id == 'BRH' ~ 'g',
      var_id == 'BH' ~ 'g',
      var_id == 'Ht' ~ 'm',
      var_id == 'DC' ~ 'm',
      var_id == 'GC' ~ 'mm',
      var_id == 'VCC' ~ 'dm3',
      var_id == 'IAVC' ~ 'dm3',
      var_id == 'VLE' ~ 'dm3',
      var_id == 'VSC' ~ 'dm3',
      var_id == 'BR' ~ 'g',
      var_id == 'BC' ~ 'kg',
      var_id == 'VC' ~ 'dm3',
      var_id == 'BAT' ~ 'kg',
      var_id == 'BM' ~ 'kg',
      var_id == 'VM' ~ 'dm3',
      var_id == 'DR' ~ 'mm',
      var_id == 'Dn' ~ 'cm',
      var_id == 'Dn*' ~ 'cm'
    ),
    var_abbr_cat = var_id,
    var_abbr_spa = var_id,
    var_abbr_eng = str_extract(translation_eng, '\\([a-z|A-Z]+\\)') %>%
      str_remove_all('[(|)]')
  ) -> thesaurus_variables_creaf
  
## TODO thesaurus of origins


