#' Recommended nutrient intakes (RNI)
#'
#' Adult RNIs per capita per day, from Sherwin's GTAP paper. Units are in the
#' nutrient code suffix (g, mg, mcg).
#'
#' @format A tibble with columns `Nutrients` and `RNI`.
#' @export
rni_lookup <- tibble::tribble(
    ~Nutrients,       ~RNI,
    "protein_g",      52,
    "vit_a_rae_mcg",  700,
    "thiamin_mg",     1.2,
    "riboflavin_mg",  1.3,
    "niacin_mg",      16,
    "vit_b6_mg",      1.3,
    "folate_mcg",     400,
    "vit_c_mg",       90,
    "calcium_mg",     1000,
    "iron_mg",        18,
    "magnesium_mg",   420,
    "phosphorus_mg",  700,
    "potassium_g",    3.4,
    "zinc_mg",        11)

#' Display names for nutrient codes
#'
#' @format A named character vector, names are `Nutrients` codes.
#' @export
nutrient_labels <- c(
    protein_g      = "Protein",
    vit_a_rae_mcg  = "Vitamin A",
    thiamin_mg     = "Thiamin",
    riboflavin_mg  = "Riboflavin",
    niacin_mg      = "Niacin",
    vit_b6_mg      = "Vitamin B6",
    folate_mcg     = "Folate",
    vit_c_mg       = "Vitamin C",
    calcium_mg     = "Calcium",
    iron_mg        = "Iron",
    magnesium_mg   = "Magnesium",
    phosphorus_mg  = "Phosphorus",
    potassium_g    = "Potassium",
    zinc_mg        = "Zinc"
)

#' Commodities excluded from nutrient availability
#'
#' Raw/intermediate commodities that would double count, plus palm kernel
#' (food availability only in IDN and MYS, and small).
#'
#' @format A character vector of IMPACT demand commodity codes.
#' @export
exclude_cmdty <- c("cgdnt", "ctont", "crpnt", "csbnt", "csugr", "cteas", "ccafe", "csfnt", "cpkol")

#' Share of calories from fat by commodity
#'
#' Fraction of a commodity's calories that come from fat (not per 100 g).
#' Used to approximate the fat share of dietary energy.
#'
#' @format A tibble with columns `name`, `fat_kcal_share` and `notes`.
#' @export
fat_kcal_share <- tibble::tribble(
    ~name,                       ~fat_kcal_share, ~notes,
    "O&S-Other Oils (p)",        1.00,            "pure fat, ~884 kcal/100g all from fat",
    "O&S-Palm Fruit Oil (p)",    1.00,            "pure fat, ~884 kcal/100g all from fat",
    "O&S-Rapeseed Oil (p)",      1.00,            "pure fat, ~884 kcal/100g all from fat",
    "O&S-Soybean Oil (p)",       1.00,            "pure fat, ~884 kcal/100g all from fat",
    "O&S-Sunflower Oil (p)",     1.00,            "pure fat, ~884 kcal/100g all from fat",
    "O&S-Groundnut Oil (p)",     1.00,            "pure fat, ~884 kcal/100g all from fat",
    "O&S-Palm Kernel Oil (p)",   1.00,            "pure fat, ~884 kcal/100g all from fat",
    "O&S-Sugar  (p)",            0.00,            "pure carbohydrate, no fat",
    "O&S-Groundnut",             0.73,            "whole nut, ~49g fat x 9 kcal / ~567 kcal/100g",
    "O&S-Soybean",               0.40,            "whole bean, ~20g fat x 9 kcal / ~446 kcal/100g",
    "O&S-Sunflower",             0.75,            "whole seed, ~51g fat x 9 kcal / ~584 kcal/100g",
    "O&S-Other Oilseeds",        0.60,            "rough average proxy across mixed oilseeds (sesame, rapeseed, etc.)",
    "ASF-Beef",                  0.65,            "~20g fat x 9 kcal / ~250 kcal/100g, typical retail beef",
    "ASF-Pork",                  0.70,            "similar logic to beef, pork generally fattier",
    "ASF-Lamb",                  0.70,            "similar to pork/beef range",
    "ASF-Poultry",               0.50,            "chicken with skin",
    "ASF-Dairy",                 0.50,            "whole milk basis",
    "ASF-Eggs",                  0.65,            "whole egg",
    "ASF-Aquatic Food",          0.30,            "average across fish/shellfish, highly variable by species (white fish <5%, fatty fish like salmon >50%)"
)

#' Placeholder nutrient values per kg for aquatic foods
#'
#' Aquatic commodities are missing from `Nutrients_per_kg.gdx`. These are
#' rough, globally generic approximations (same value for every country),
#' informed by FAO/INFOODS uFiSh ranges and USDA food composition data for
#' representative species in each IMPACT aquatic category.
#'
#' Caveats: not country-specific; rough averages across highly variable species
#' groups (small whole fish vs. large fillet fish differ by an order of
#' magnitude for vitamin A, iron, zinc, folate). Treat as a placeholder and flag
#' as a known limitation. Units match `Nutrients` codes, per kg edible portion, raw.
#'
#' @format A tibble with columns `cmdty`, `Nutrients` and `value`.
#' @export
aquatic_nutrient_per_kg <- tibble::tribble(
  ~cmdty,     ~Nutrients,       ~value,

  # Protein (g/kg)
  "c-shrimp", "protein_g",      190,
  "c-Crust",  "protein_g",      180,
  "c-Mllsc",  "protein_g",      150,
  "c-Salmon", "protein_g",      200,
  "c-FrshD",  "protein_g",      180,
  "c-Tuna",   "protein_g",      230,
  "c-OPelag", "protein_g",      190,
  "c-ODmrsl", "protein_g",      180,
  "c-OMarn",  "protein_g",      180,

  # Iron (mg/kg)
  "c-shrimp", "iron_mg",        2.0,
  "c-Crust",  "iron_mg",        3.0,
  "c-Mllsc",  "iron_mg",        50.0,
  "c-Salmon", "iron_mg",        3.0,
  "c-FrshD",  "iron_mg",        10.0,
  "c-Tuna",   "iron_mg",        10.0,
  "c-OPelag", "iron_mg",        15.0,
  "c-ODmrsl", "iron_mg",        5.0,
  "c-OMarn",  "iron_mg",        5.0,

  # Zinc (mg/kg)
  "c-shrimp", "zinc_mg",        15,
  "c-Crust",  "zinc_mg",        50,
  "c-Mllsc",  "zinc_mg",        30,
  "c-Salmon", "zinc_mg",        5,
  "c-FrshD",  "zinc_mg",        8,
  "c-Tuna",   "zinc_mg",        5,
  "c-OPelag", "zinc_mg",        10,
  "c-ODmrsl", "zinc_mg",        8,
  "c-OMarn",  "zinc_mg",        8,

  # Vitamin A (mcg RAE/kg)
  "c-shrimp", "vit_a_rae_mcg",  10,
  "c-Crust",  "vit_a_rae_mcg",  10,
  "c-Mllsc",  "vit_a_rae_mcg",  150,
  "c-Salmon", "vit_a_rae_mcg",  120,
  "c-FrshD",  "vit_a_rae_mcg",  50,
  "c-Tuna",   "vit_a_rae_mcg",  50,
  "c-OPelag", "vit_a_rae_mcg",  150,
  "c-ODmrsl", "vit_a_rae_mcg",  100,
  "c-OMarn",  "vit_a_rae_mcg",  50,

  # Calcium (mg/kg)
  "c-shrimp", "calcium_mg",     700,
  "c-Crust",  "calcium_mg",     500,
  "c-Mllsc",  "calcium_mg",     800,
  "c-Salmon", "calcium_mg",     120,
  "c-FrshD",  "calcium_mg",     300,
  "c-Tuna",   "calcium_mg",     100,
  "c-OPelag", "calcium_mg",     600,
  "c-ODmrsl", "calcium_mg",     300,
  "c-OMarn",  "calcium_mg",     300,

  # Magnesium (mg/kg)
  "c-shrimp", "magnesium_mg",   400,
  "c-Crust",  "magnesium_mg",   400,
  "c-Mllsc",  "magnesium_mg",   300,
  "c-Salmon", "magnesium_mg",   300,
  "c-FrshD",  "magnesium_mg",   270,
  "c-Tuna",   "magnesium_mg",   300,
  "c-OPelag", "magnesium_mg",   400,
  "c-ODmrsl", "magnesium_mg",   300,
  "c-OMarn",  "magnesium_mg",   300,

  # Phosphorus (mg/kg)
  "c-shrimp", "phosphorus_mg",  2000,
  "c-Crust",  "phosphorus_mg",  2000,
  "c-Mllsc",  "phosphorus_mg",  1500,
  "c-Salmon", "phosphorus_mg",  2400,
  "c-FrshD",  "phosphorus_mg",  2000,
  "c-Tuna",   "phosphorus_mg",  2500,
  "c-OPelag", "phosphorus_mg",  2200,
  "c-ODmrsl", "phosphorus_mg",  2000,
  "c-OMarn",  "phosphorus_mg",  2000,

  # Potassium (g/kg)
  "c-shrimp", "potassium_g",    2.0,
  "c-Crust",  "potassium_g",    2.5,
  "c-Mllsc",  "potassium_g",    3.0,
  "c-Salmon", "potassium_g",    3.6,
  "c-FrshD",  "potassium_g",    3.0,
  "c-Tuna",   "potassium_g",    4.0,
  "c-OPelag", "potassium_g",    3.0,
  "c-ODmrsl", "potassium_g",    3.0,
  "c-OMarn",  "potassium_g",    3.0,

  # Thiamin (mg/kg)
  "c-shrimp", "thiamin_mg",     0.3,
  "c-Crust",  "thiamin_mg",     0.3,
  "c-Mllsc",  "thiamin_mg",     1.0,
  "c-Salmon", "thiamin_mg",     2.0,
  "c-FrshD",  "thiamin_mg",     0.5,
  "c-Tuna",   "thiamin_mg",     0.3,
  "c-OPelag", "thiamin_mg",     0.5,
  "c-ODmrsl", "thiamin_mg",     0.5,
  "c-OMarn",  "thiamin_mg",     0.5,

  # Riboflavin (mg/kg)
  "c-shrimp", "riboflavin_mg",  0.3,
  "c-Crust",  "riboflavin_mg",  0.5,
  "c-Mllsc",  "riboflavin_mg",  2.0,
  "c-Salmon", "riboflavin_mg",  4.0,
  "c-FrshD",  "riboflavin_mg",  1.0,
  "c-Tuna",   "riboflavin_mg",  0.5,
  "c-OPelag", "riboflavin_mg",  2.0,
  "c-ODmrsl", "riboflavin_mg",  1.0,
  "c-OMarn",  "riboflavin_mg",  1.0,

  # Niacin (mg/kg)
  "c-shrimp", "niacin_mg",      20,
  "c-Crust",  "niacin_mg",      30,
  "c-Mllsc",  "niacin_mg",      15,
  "c-Salmon", "niacin_mg",      80,
  "c-FrshD",  "niacin_mg",      30,
  "c-Tuna",   "niacin_mg",      130,
  "c-OPelag", "niacin_mg",      50,
  "c-ODmrsl", "niacin_mg",      30,
  "c-OMarn",  "niacin_mg",      30,

  # Vitamin B6 (mg/kg)
  "c-shrimp", "vit_b6_mg",      1.5,
  "c-Crust",  "vit_b6_mg",      1.0,
  "c-Mllsc",  "vit_b6_mg",      1.0,
  "c-Salmon", "vit_b6_mg",      6.0,
  "c-FrshD",  "vit_b6_mg",      3.0,
  "c-Tuna",   "vit_b6_mg",      9.0,
  "c-OPelag", "vit_b6_mg",      4.0,
  "c-ODmrsl", "vit_b6_mg",      3.0,
  "c-OMarn",  "vit_b6_mg",      3.0,

  # Folate (mcg/kg)
  "c-shrimp", "folate_mcg",     30,
  "c-Crust",  "folate_mcg",     40,
  "c-Mllsc",  "folate_mcg",     300,
  "c-Salmon", "folate_mcg",     250,
  "c-FrshD",  "folate_mcg",     150,
  "c-Tuna",   "folate_mcg",     20,
  "c-OPelag", "folate_mcg",     150,
  "c-ODmrsl", "folate_mcg",     100,
  "c-OMarn",  "folate_mcg",     100,

  # Vitamin C (mg/kg)
  "c-shrimp", "vit_c_mg",       17,
  "c-Crust",  "vit_c_mg",       5,
  "c-Mllsc",  "vit_c_mg",       30,
  "c-Salmon", "vit_c_mg",       4,
  "c-FrshD",  "vit_c_mg",       10,
  "c-Tuna",   "vit_c_mg",       10,
  "c-OPelag", "vit_c_mg",       10,
  "c-ODmrsl", "vit_c_mg",       10,
  "c-OMarn",  "vit_c_mg",       10
)
