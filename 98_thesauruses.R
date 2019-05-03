## thesaurus variables ####
# lets create a thesuarus of variable names
temp_allometries_creaf %>% gather('variable', 'var_id', contains('_var')) %>%
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
      var_id == 'DnSC' ~ glue::glue("Diàmetre normal sense escorça ({var_id})"),
      var_id == 'P_BST' ~ glue::glue("Percentatge de biomassa Subterrània Total ({var_id})"),
      var_id == 'BST' ~ glue::glue("Biomassa Subterrània Total ({var_id})")
    ),
    translation_eng = case_when(
      var_id == 'BRH' ~ 'Branches and leaves Biomass (BLB)',
      var_id == 'BH' ~ 'Leaves Biomass (LB)',
      var_id == 'Ht' ~ 'Total Height (Ht)',
      var_id == 'DC' ~ 'Crown diameter (CD)',
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
      var_id == 'DnSC' ~ 'Diameter Inside Bark (DIB)',
      var_id == 'P_BST' ~ "Total Subterranean Biomass percentage (TSBp)",
      var_id == 'BST' ~ "Total Subterranean Biomass (TSB)"
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
      var_id == 'DnSC' ~ glue::glue("Diámetro sin corteza ({var_id})"),
      var_id == 'P_BST' ~ glue::glue("Porcentaje de biomasa subterránea total ({var_id})"),
      var_id == 'BST' ~ glue::glue("Biomasa subterránea total ({var_id})")
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
      var_id == 'DnSC' ~ 'cm',
      var_id == 'P_BST' ~ '%',
      var_id == 'BST' ~ 'kg'
    ),
    var_abbr_cat = var_id,
    var_abbr_spa = var_id,
    var_abbr_eng = str_extract(translation_eng, '\\([a-z|A-Z]+\\)') %>%
      str_remove_all('[(|)]'),
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
  ) -> theasurus_sources
