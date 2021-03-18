## thesaurus variables ####
# lets create a thesuarus of variable names
temp_allometries_creaf %>% gather('variable', 'var_id', contains('_var')) %>%
  select(var_id) %>% distinct() %>% filter(!is.na(var_id)) %>%
  mutate(
    var_units = case_when(
      var_id == 'BRH' ~ 'kg',
      var_id == 'BH' ~ 'kg',
      var_id == 'Ht' ~ 'm',
      var_id == 'DC' ~ 'm',
      var_id == 'GC' ~ 'mm',
      var_id == 'VOB' ~ 'dm3',
      var_id == 'IAVC' ~ 'dm3',
      var_id == 'VLE' ~ 'dm3',
      var_id == 'VUB' ~ 'dm3',
      var_id == 'BR' ~ 'kg',
      var_id == 'BC' ~ 'kg',
      var_id == 'VC' ~ 'dm3',
      var_id == 'BAT' ~ 'kg',
      var_id == 'BM' ~ 'kg',
      var_id == 'VM' ~ 'dm3',
      var_id == 'DR' ~ 'mm',
      var_id == 'DBH' ~ 'cm',
      var_id == 'DBH_SC' ~ 'cm',
      var_id == 'P_BST' ~ '%',
      var_id == 'BST' ~ 'kg',
      var_id == 'BAL' ~ 'm2/ha',
      var_id == 'PHV' ~ 'cm3',
      var_id == 'BFAT' ~ 'kg',
      var_id == 'Area' ~ 'cm2/cm'
    ),
    translation_cat = case_when(
      var_id == 'BRH' ~ glue::glue("Biomassa de branques amb fulles ({var_id} [{var_units}])"),
      var_id == 'BH' ~ glue::glue("Biomassa de fulles ({var_id} [{var_units}])"),
      var_id == 'Ht' ~ glue::glue("Altura total ({var_id} [{var_units}])"),
      var_id == 'DC' ~ glue::glue("Diàmetre de capçada ({var_id} [{var_units}])"),
      var_id == 'GC' ~ glue::glue("Gruix d'escorça ({var_id} [{var_units}])"),
      var_id == 'VOB' ~ glue::glue("Volum amb escorça ({var_id} [{var_units}])"),
      var_id == 'IAVC' ~ glue::glue("Increment anual del volum amb escorça ({var_id} [{var_units}])"),
      var_id == 'VLE' ~ glue::glue("Volum de llenyes ({var_id} [{var_units}])"),
      var_id == 'VUB' ~ glue::glue("Volum sense escorça ({var_id} [{var_units}])"),
      var_id == 'BR' ~ glue::glue("Biomassa de branques sense fulles ({var_id} [{var_units}])"),
      var_id == 'BC' ~ glue::glue("Biomassa d'escorça ({var_id} [{var_units}])"),
      var_id == 'VC' ~ glue::glue("Volum d'escorça ({var_id} [{var_units}])"),
      var_id == 'BAT' ~ glue::glue("Biomassa Aèria Total ({var_id} [{var_units}])"),
      var_id == 'BM' ~ glue::glue("Biomassa de fusta ({var_id} [{var_units}])"),
      var_id == 'VM' ~ glue::glue("Volum de fusta ({var_id} [{var_units}])"),
      var_id == 'DR' ~ glue::glue("Diàmetre de la branca ({var_id} [{var_units}])"),
      var_id == 'DBH' ~ glue::glue("Diàmetre normal ({var_id} [{var_units}])"),
      var_id == 'DBH_SC' ~ glue::glue("Diàmetre normal sense escorça ({var_id} [{var_units}])"),
      var_id == 'P_BST' ~ glue::glue("Percentatge de biomassa Subterrània Total ({var_id} [{var_units}])"),
      var_id == 'BST' ~ glue::glue("Biomassa Subterrània Total ({var_id} [{var_units}])"),
      var_id == 'BAL' ~ glue::glue("Àrea Basal d’Arbres Grans ({var_id} [{var_units}])"),
      var_id == 'PHV' ~ glue::glue("Fitovolum ({var_id} [{var_units}])"),
      var_id == 'BFAT' ~ glue::glue("Biomassa Fina Aèria Total ({var_id} [{var_units}])"),
      var_id == 'Area' ~ glue::glue("Àrea ({var_id} [{var_units}])")
    ),
    translation_eng = case_when(
      var_id == 'BRH' ~ glue::glue('Branches and leaves Biomass (BLB [{var_units}])'),
      var_id == 'BH' ~ glue::glue('Leaves Biomass (LB [{var_units}])'),
      var_id == 'Ht' ~ glue::glue('Total Height (Ht [{var_units}])'),
      var_id == 'DC' ~ glue::glue('Crown Diameter (CD [{var_units}])'),
      var_id == 'GC' ~ glue::glue('Bark Thickness (BT [{var_units}])'),
      var_id == 'VOB' ~ glue::glue('Over Bark Volume (VOB [{var_units}])'),
      var_id == 'IAVC' ~ glue::glue('Annual Increment of Over Bark Volume (VOBi [{var_units}])'),
      var_id == 'VLE' ~ glue::glue('Fuel Wood Volume (FWV [{var_units}])'),
      var_id == 'VUB' ~ glue::glue('Under Bark Volume (VUB [{var_units}])'),
      var_id == 'BR' ~ glue::glue('Branches Biomass (BB [{var_units}])'),
      var_id == 'BC' ~ glue::glue('Bark Biomass (BaB [{var_units}])'),
      var_id == 'VC' ~ glue::glue('Bark Volume (BaV [{var_units}])'),
      var_id == 'BAT' ~ glue::glue('Total Aerial Biomass (TAB [{var_units}])'),
      var_id == 'BM' ~ glue::glue('Wood Biomass (WB [{var_units}])'),
      var_id == 'VM' ~ glue::glue('Wood Volume (WV [{var_units}])'),
      var_id == 'DR' ~ glue::glue('Branch Diameter (BD [{var_units}])'),
      var_id == 'DBH' ~ glue::glue('Diameter at Breast Height (DBH [{var_units}])'),
      var_id == 'DBH_SC' ~ glue::glue('Diameter Inside Bark (DIB [{var_units}])'),
      var_id == 'P_BST' ~ glue::glue("Total Subterranean Biomass percentage (TSBp [{var_units}])"),
      var_id == 'BST' ~ glue::glue("Total Subterranean Biomass (TSB [{var_units}])"),
      var_id == 'BAL' ~ glue::glue("Basal Area of Larger Trees (BAlt [{var_units}])"),
      var_id == 'PHV' ~ glue::glue("Phytovolume ({var_id} [PHV])"),
      var_id == 'BFAT' ~ glue::glue("Total Fine Aerial Biomass (TFAB [{var_units}])"),
      var_id == 'Area' ~ glue::glue("Area (Area [{var_units}])")
    ),
    translation_spa = case_when(
      var_id == 'BRH' ~ glue::glue("Biomasa de ramas con hojas ({var_id} [{var_units}])"),
      var_id == 'BH' ~ glue::glue("Biomasa de hojas ({var_id} [{var_units}])"),
      var_id == 'Ht' ~ glue::glue("Altura total ({var_id} [{var_units}])"),
      var_id == 'DC' ~ glue::glue("Diámetro de copa ({var_id} [{var_units}])"),
      var_id == 'GC' ~ glue::glue("Grosor de corteza ({var_id} [{var_units}])"),
      var_id == 'VOB' ~ glue::glue("Volumen con corteza ({var_id} [{var_units}])"),
      var_id == 'IAVC' ~ glue::glue("Incremento anual del volumen con corteza ({var_id} [{var_units}])"),
      var_id == 'VLE' ~ glue::glue("Volumen de leña ({var_id} [{var_units}])"),
      var_id == 'VUB' ~ glue::glue("Volumen sin corteza ({var_id} [{var_units}])"),
      var_id == 'BR' ~ glue::glue("Biomasa de ramas ({var_id} [{var_units}])"),
      var_id == 'BC' ~ glue::glue("Biomasa de corteza ({var_id} [{var_units}])"),
      var_id == 'VC' ~ glue::glue("Volumen de corteza ({var_id} [{var_units}])"),
      var_id == 'BAT' ~ glue::glue("Biomasa aérea total ({var_id} [{var_units}])"),
      var_id == 'BM' ~ glue::glue("Biomasa de madera ({var_id} [{var_units}])"),
      var_id == 'VM' ~ glue::glue("Volumen de madera ({var_id} [{var_units}])"),
      var_id == 'DR' ~ glue::glue("Diámetro de rama ({var_id} [{var_units}])"),
      var_id == 'DBH' ~ glue::glue("Diámetro normal ({var_id} [{var_units}])"),
      var_id == 'DBH_SC' ~ glue::glue("Diámetro sin corteza ({var_id} [{var_units}])"),
      var_id == 'P_BST' ~ glue::glue("Porcentaje de biomasa subterránea total ({var_id} [{var_units}])"),
      var_id == 'BST' ~ glue::glue("Biomasa subterránea total ({var_id} [{var_units}])"),
      var_id == 'BAL' ~ glue::glue("Área Basal de Árboles Grandes ({var_id} [{var_units}])"),
      var_id == 'PHV' ~ glue::glue("Fitovolúmen ({var_id} [{var_units}])"),
      var_id == 'BFAT' ~ glue::glue("Biomasa Aérea FIna Total ({var_id} [{var_units}])"),
      var_id == 'Area' ~ glue::glue("Área ({var_id} [{var_units}])")
    ),
    var_abbr_cat = var_id,
    var_abbr_spa = var_id,
    var_abbr_eng = case_when(
      var_id == 'BRH' ~ 'BLB',
      var_id == 'BH' ~ 'LB',
      var_id == 'Ht' ~ 'Ht',
      var_id == 'DC' ~ 'CD',
      var_id == 'GC' ~ 'BT',
      var_id == 'VOB' ~ 'VOB',
      var_id == 'IAVC' ~ 'VOBi',
      var_id == 'VLE' ~ 'FWV',
      var_id == 'VUB' ~ 'VUB',
      var_id == 'BR' ~ 'BB',
      var_id == 'BC' ~ 'BaB',
      var_id == 'VC' ~ 'BaV',
      var_id == 'BAT' ~ 'TAB',
      var_id == 'BM' ~ 'WB',
      var_id == 'VM' ~ 'WV',
      var_id == 'DR' ~ 'BD',
      var_id == 'DBH' ~ 'DBH',
      var_id == 'DBH_SC' ~ 'DIB',
      var_id == 'P_BST' ~ 'TSBp',
      var_id == 'BST' ~ 'TSB',
      var_id == 'BAL' ~ 'BAlt',
      var_id == 'PHV' ~ 'PHV',
      var_id == 'BFAT' ~ 'TFAB',
      var_id == 'Area' ~ 'Area'
    ),
    var_dependent = if_else(
      var_id %in% {temp_allometries_creaf %>% pull(dependent_var) %>% unique}, TRUE, FALSE
    ),
    var_independent = if_else(
      var_id %in% {
        c(
          temp_allometries_creaf %>% pull(independent_var_1) %>% unique,
          temp_allometries_creaf %>% pull(independent_var_2) %>% unique,
          temp_allometries_creaf %>% pull(independent_var_3) %>% unique
        )
      }, TRUE, FALSE
    )
  ) -> thesaurus_variables_creaf

## thesaurus allometry levels ####
temp_allometries_creaf %>%
  select(allometry_level) %>%
  distinct() %>%
  mutate(
    translation_cat = case_when(
      allometry_level == 'organ' ~ "Nivell d'òrgan",
      allometry_level == 'tree' ~ "Nivell de planta"
    ),
    translation_eng = case_when(
      allometry_level == 'organ' ~ 'Organ level',
      allometry_level == 'tree' ~ 'Plant level'
    ),
    translation_spa = case_when(
      allometry_level == 'organ' ~ 'Nivel de órgano',
      allometry_level == 'tree' ~ 'Nivel de planta'
    )
  ) -> thesaurus_allo_level_creaf

## thesaurus spatial levels ####
temp_allometries_creaf %>%
  select(spatial_level) %>%
  distinct() %>%
  mutate(
    translation_cat = case_when(
      spatial_level == 'county' ~ "Comarca",
      spatial_level == 'province' ~ "Provincia",
      spatial_level == 'aut_community' ~ "Comunitat autònoma",
      spatial_level == 'country' ~ "País"
    ),
    translation_eng = case_when(
      spatial_level == 'county' ~ "County",
      spatial_level == 'province' ~ "Province",
      spatial_level == 'aut_community' ~ "Autonomous community",
      spatial_level == 'country' ~ "Country"
    ),
    translation_spa = case_when(
      spatial_level == 'county' ~ "Comarca",
      spatial_level == 'province' ~ "Provincia",
      spatial_level == 'aut_community' ~ "Comunidad autónoma",
      spatial_level == 'country' ~ "País"
    )
  ) -> thesaurus_spatial_level_creaf

## thesaurus functional groups ####
temp_allometries_creaf %>%
  select(functional_group_level) %>%
  distinct() %>%
  mutate(
    translation_cat = case_when(
      functional_group_level == 'species' ~ "Espècie",
      functional_group_level == 'species_group' ~ "Grup d'espècies",
      functional_group_level == 'genus' ~ "Gènere"
    ),
    translation_eng = case_when(
      functional_group_level == 'species' ~ "Species",
      functional_group_level == 'species_group' ~ "Species group",
      functional_group_level == 'genus' ~ "Genus"
    ),
    translation_spa = case_when(
      functional_group_level == 'species' ~ "Especie",
      functional_group_level == 'species_group' ~ "Grupo de expecies",
      functional_group_level == 'genus' ~ "Génere"
    )
  ) -> thesaurus_functional_group_level_creaf

## thesaurus special parameter ####
temp_allometries_creaf %>%
  select(translation_cat = special_param) %>%
  distinct() %>%
  mutate(
    special_param_id = 1:5,
    translation_eng = case_when(
      special_param_id == 1 ~ "None",
      special_param_id == 2 ~ "No peeled (first cork present)",
      special_param_id == 3 ~ "Peeled before, not now",
      special_param_id == 4 ~ "Peeled trunk and branches",
      special_param_id == 5 ~ "Peeled trunk only"
    ),
    translation_spa = case_when(
      special_param_id == 1 ~ "Ninguno",
      special_param_id == 2 ~ "Bornizo (sin pelar)",
      special_param_id == 3 ~ "Pelado antes, no ahora",
      special_param_id == 4 ~ "Pelado en tronco y ramas",
      special_param_id == 5 ~ "Pelado en tronco solo"
    )
  ) %>%
  select(
    special_param_id, translation_cat, translation_eng, translation_spa
  ) -> thesaurus_special_param_creaf

## thesaurus cubication shapes ####
tbl(oracle_db, 'tesaureformacubicacio') %>%
  select(
    cubication_shape_id = idforma,
    translation_cat = formacubicacio,
    translation_spa = formacubicacion,
  ) %>%
  collect() %>%
  mutate(
    translation_eng = c(
      'Fusiform tree in almost all the shaft, with timber-yielding, clean, straight and bigger than 6m trunks. Arrow below 1% of its length, straight grain and DBH about 20cm',
      'Tree complying with these 4 conditions: Fusiform, timber-yielding trunk above 4m, branching in the upper part and not belonging to the cubication shape 1',
      'Small fusiform tree, with the 75mm shaft diameter located below the 4m of three height',
      'Tree with the main trunk branched below 4m height and belonging to Acacia, Castanea sativa, Corylus avellana, Fagus sylvatica, Fraxinus, Juglans, Laurus nobilis, Olea europaea, Pinus pinea, Platanus, Pyrus, Quercus, Salix and Ulmus',
      'Tree with tortuous, damaged or very branched main trunk (which avoid classifying it as 1, 2 or 3 class. Also trees with shaft below 4m from species different from 4 or 6 classes',
      'Headless tree or pollard with the upper part pruned as well as the branches in points near the trunk, of the following species: Castanea sativa, Fagus sylvatica, Fraxinus, Quercus robur, Quercus petraea and Laurus nobilis'
    )
  ) -> thesaurus_cubication_shape

## sources thesaurus ####
temp_allometries_creaf %>%
  select(source_id = source) %>%
  distinct() %>%
  mutate(
    translation_eng = case_when(
      source_id == 'IFN3' ~ 'Spanish National Forest Inventory version 3',
      source_id == 'IFN2' ~ 'Spanish National Forest Inventory version 2',
      source_id == 'IEFC' ~ 'Catalonian Ecological and Forest Inventory',
      source_id == 'INIA' ~ 'Spanish Agricultural Research National Institute'
    ),
    translation_spa = case_when(
      source_id == 'IFN3' ~ 'Inventario Forestal Nacional 3',
      source_id == 'IFN2' ~ 'Inventario Forestal Nacional 2',
      source_id == 'IEFC' ~ 'Inventario Ecológico y Forestal de Cataluña',
      source_id == 'INIA' ~ 'Instituto Nacional de Investigaciones Agrarias'
    ),
    translation_cat = case_when(
      source_id == 'IFN3' ~ 'Inventari Forestal Nacional 3',
      source_id == 'IFN2' ~ 'Inventari Forestal Nacional 2',
      source_id == 'IEFC' ~ 'Inventari Ecológico y Forestal de Catalunya',
      source_id == 'INIA' ~ 'Institut Nacional de Recerca Agraria'
    )
  ) -> thesaurus_sources

## app translations ####
tibble::tribble(
  ~text_id, ~translation_cat, ~translation_eng, ~translation_spa,
  
  "explore_tab_title", 'Explora', 'Explore', 'Explora',
  "sidebar_filter_h4", 'Filtra les al·lometries', 'Filter the alllometries', 'Filtra las alometrias',
  "allometry_id", "ID al·lometria", 'Allometry ID', 'ID Alometría',
  "allometry_level_name", "Valor nivell d'al·lometria", 'Allometry level value', 'Valor del nivel de alometría',
  "equation", "Ecuació", "Equation", "Ecuación",
  "param_a", "Paràmetre a", "Parameter a", 'Parámetro a',
  "param_b", "Paràmetre b", "Parameter b", "Parámetro b",
  "param_c", "Paràmetre c", "Parameter c", "Parámetro c",
  "param_d", "Paràmetre d", "Parameter d", "Parámetro d",
  "source", "Origen", "Source", "Orígen",
  "n_obs", "Nombre d'observacions", 'Number of observations', 'Número de observaciónes',
  "r_sqr", "R quadrat", "R square", "R cuadrado",
  "see", "Error estàndard", 'Standard error', 'Error estándar',
  "dependent_var", 'Variable dependent', 'Dependent variable', 'Variable dependiente',
  "independent_var_1", 'Variable independent 1', 'Independent variable 1', 'Variable independiente 1',
  "independent_var_2", 'Variable independent 2', 'Independent variable 2', 'Variable independiente 2',
  "independent_var_3", 'Variable independent 3', 'Independent variable 3', 'Variable independiente 3',
  "allometry_level", "Nivell d'al·lometria", 'Allometry level', 'Nivel de alometría',
  'spatial_level', 'Nivell espacial', 'Spatial level', 'Nivel espacial',
  'spatial_level_name', 'Valor nivell espacial', 'Spatial level value', 'Valor nivel espacial',
  'functional_group_level', 'Nivell de grup funcional', 'Functional group level', 'Nivel de grupo funcional',
  'functional_group_level_name', 'Valor de grup funcional', 'Funcional group value', 'Valor de grupo funcional',
  'cubication_shape', 'Forma de cubicació', 'Cubication shape', 'Forma de cubicación',
  'special_param', 'Paràmetre especial (nemés per Q. suber)', 'Special parameter (only for Q. suber)', 'Parámetro especial (sólo para Q. suber)',
  'dependent_var_units', "Unitats de la variable dependent", "Dependent variable units", "Unidades de la variable dependiente",
  'dependent_var_translation_cat', "Descripció de la variable dependent", "Dependent variable description (cat)", "Descripcion de la variable dependiente (cat)",
  'dependent_var_translation_spa', "Descripció de la variable dependent (spa)", "Dependent variable description (spa)", "Descripcion de la variable dependiente",
  'dependent_var_translation_eng', "Descripció de la variable dependent (eng)", "Dependent variable description", "Descripcion de la variable dependiente (eng)",
  'independent_var_1_units', "Unitats de la variable independent 1", "Independent variable 1 units", "Unidades de la variable independiente 1",
  'independent_var_1_translation_cat', "Descripció de la variable independent 1", "Independent variable 1 description (cat)", "Descripcion de la variable independiente 1 (cat)",
  'independent_var_1_translation_spa', "Descripció de la variable independent 1 (spa)", "Independent variable 1 description (spa)", "Descripcion de la variable independiente 1",
  'independent_var_1_translation_eng', "Descripció de la variable independent 1 (eng)", "Independent variable 1 description", "Descripcion de la variable independiente 1 (eng)",
  'independent_var_2_units', "Unitats de la variable independent 2", "Independent variable 2 units", "Unidades de la variable independiente 2",
  'independent_var_2_translation_cat', "Descripció de la variable independent 2", "Independent variable 2 description (cat)", "Descripcion de la variable independiente 2 (cat)",
  'independent_var_2_translation_spa', "Descripció de la variable independent 2 (spa)", "Independent variable 2 description (spa)", "Descripcion de la variable independiente 2",
  'independent_var_2_translation_eng', "Descripció de la variable independent 2 (eng)", "Independent variable 2 description", "Descripcion de la variable independiente 2 (eng)",
  'independent_var_3_units', "Unitats de la variable independent 3", "Independent variable 3 units", "Unidades de la variable independiente 3",
  'independent_var_3_translation_cat', "Descripció de la variable independent 3", "Independent variable 3 description (cat)", "Descripcion de la variable independiente 3 (cat)",
  'independent_var_3_translation_spa', "Descripció de la variable independent 3 (spa)", "Independent variable 3 description (spa)", "Descripcion de la variable independiente 3",
  'independent_var_3_translation_eng', "Descripció de la variable independent 3 (eng)", "Independent variable 3 description", "Descripcion de la variable independiente 3 (eng)",
  'sidebar_download_h4', 'Descarrega la taula de al·lometries', 'Download the allometry table', 'Descarga la tabla de alometrías',
  'table_tab_title', 'Taula', 'Table', 'Tabla',
  'calculate_tab_title', 'Calcula', 'Calculate', 'Calcula','calculate_panel_heading', 'Selecciona les dades per convertir', 'Select the data to convert', 'Selecciona los datos para convertir',
  'calculate_panel_upload_p', 'Selecciona un arxiu amb les dades per convertir. Els formats acceptats són csv i xlsx, i la primera fila ha de ser un capçal amb els noms de les columnes.',
                              'Please, select a file to load with the data to be converted. Accepted formats are csv and xlsx. Both of them must have a header wwith column names.',
                              'Selecciona un archivo con los datos para convertir. Los formatos aceptados son csv y xlsx, y la primera fila ha de ser un cabecero con los nombres de las columnas.',
  'user_data_button_label', 'Inspecciona...', 'Browse...', 'Inspecciona...',
  'user_data_button_placeholder', 'Cap fitxer seleccionat...', 'No file selected...', 'Ningún archivo seleccionado...',
  'calculate_panel_allosel_p', 'Selecciona les al·lometries a calcular. En cas de dubte consulta la ',
                               'Select the allometries to use. If in doubt check the ',
                               'Selecciona las alometrías a calcular. En caso de duda consulta la ',
  'calculate_panel_allotable_link', 'taula de al·lometries', 'allometry table', 'tabla de alometrías',
  'calculate_panel_vardec_p', "Selecciona les variables de les dades carregades que es corresponen amb les variables independents en l'equació lomètrica:",
                              'Select the variables from the uploaded data corresponding to the independent variables from the equation:',
                              'Selecciona las variables de los datos cargados que se corresponden con las variables independientes en la ecuación alométrica:',
  'calculate_panel_download_h4', 'Descarrega las al·lometries calculades', 'Download the calculated allometries', 'Descarga las alometrías calculadas',
  'nothing_selected', 'Res seleccionat', 'Nothing selected', 'Nada seleccionado',
  'need_user_data', "No hi ha dades carregats", "No user data provided", 'No hay datos cargados',
  'need_allosel', 'Sense al·lometries seleccionades', 'No allometries selected', 'Sin alometrías seleccionadas',
  'need_vardec', "No hi ha declaració de variables", 'No variables declaration', 'No hay declaración de variables',
  'calculate_panel_vardec_inputs', 'Variable a fer servir com {x} [{units}]', 'Variable acting as {x} [{units}]', 'Variable a usar como {x} [{units}]'
  ## TODO continue translations thesaurus
) %>%
  full_join(thesaurus_variables_creaf %>% select(text_id = var_id, everything())) %>%
  full_join(thesaurus_spatial_level_creaf %>% select(text_id = spatial_level, everything())) %>%
  full_join(thesaurus_allo_level_creaf %>% select(text_id = allometry_level, everything())) %>%
  full_join(thesaurus_functional_group_level_creaf %>% select(text_id = functional_group_level, everything())) %>%
  # full_join(thesaurus_special_param_creaf %>% select(text_id = special_param_id, everything())) %>% 
  {.} -> app_translations


