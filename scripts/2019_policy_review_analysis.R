# 2019 O&G Policy Review of OOOOa
# Analysis Script


# This script loads and processes information for the policy reconsideration economic analysis

# load libraries and source functions
source("scripts/setup.R")

# load policy scenario functions
source("scripts/policy_2019policyreview.R")


# load ONG analysis parameters:
# Includes model plant types, control fates, and lists for consistent table displays.
source("scripts/read_parameter_lists.R")


# unit-level costs and emissions of model plants with different controls
mp_fates_2019_polrev <- read_excel("data-raw/params_2019_policy_review.xlsx", 
                            sheet = "mp_fates_2019_polrev", 
                            na = c("", "NA")) 


# activity data (AD)
# these are projections of new and modified facilities for each model plant
# activity data projections are held constant across policy scenarios

source(here('scripts/2019_policy-review_activity-data-calcs.R'))
#activity data script returns ad_proj_2019_polrev



# policy scenarios
# policy scenario functions define how model plants AD projections are mapped to emissions controls under a particular policy scenario based on a combination of federal and state regulations. They result in mp-fate projections, which are similar to the AD projections, but with the addition of information on emissions controls, in the `fate` column.

# (ad_proj, policy scenario) -> (mpf_proj)
# e.g, each of form:
# mpf_bau <- ad_proj_2019_polrev %>%
#   mutate(fate = polrev_2019_baseline(.))

mpf_proj <- ad_proj_2019_polrev %>% {
  list(
    "pre2016"          = mutate(., fate = pre_2016_baseline(.)),
    "baseline_prb"    = mutate(., fate = polrev_2019_baseline_prb(.)),
    "baseline_crb"  = mutate(., fate = polrev_2019_baseline_crb(.)),
    "baseline_techalt" = mutate(., fate = polrev_2019_baseline_techalt(.)),
    "proposal_opt1"    = mutate(., fate = polrev_2019_proposed_opt1(.)),
    "alternative_opt2" = mutate(., fate = polrev_2019_alternative_opt2(.))
  )
}


# Calculate scenario metrics using the unit-level cost and emissions information for each model plant and control fate combination

# (mpf_proj, mp_fates_chars) -> (scn_proj)
# e.g., each of form:
# scn_bau <- add_scenario_calcs_v(mpf_bau, mp_fates_2019_polrev)

scn_proj <- mp_fates_2019_polrev %>% {
  list(
    "pre2016"           = add_scenario_calcs_v(mpf_proj[["pre2016"]], .),
    "baseline_prb"     = add_scenario_calcs_v(mpf_proj[["baseline_prb"]], .),
    "baseline_crb"   = add_scenario_calcs_v(mpf_proj[["baseline_crb"]], .),
    "baseline_techalt"  = add_scenario_calcs_v(mpf_proj[["baseline_techalt"]], .),
    "proposal_opt1"     = add_scenario_calcs_v(mpf_proj[["proposal_opt1"]], .),
    "alternative_opt2"  = add_scenario_calcs_v(mpf_proj[["alternative_opt2"]], .)
  )
}


# Scenario Comparisons
# Calculate changes in metrics from moving from one policy scenario to another. The proposed policy option is compared against two baselines, one which includes the proposed option from the 2018 technical reconsideration, and one which includes only current policy, excluding the technical reconsideration.

# (scn_proj, scn_proj) -> (scn_comp)
# e.g., each of form:
# scn_comp <- compare_two_scenarios(scn_bau, scn_policy) %>%
#   mutate(year = as.character(year),
#          vintage = as.character(vintage))

# Cases give results for opt1 against two baselines
scn_comps <- list(
  "opt1-prb"    = compare_two_scenarios(scn_proj[["baseline_prb"]], scn_proj[["proposal_opt1"]]),
  "opt1-crb"  = compare_two_scenarios(scn_proj[["baseline_crb"]], scn_proj[["proposal_opt1"]])
)

scn_combo <- bind_rows(
  mutate(scn_comps[["opt1-prb"]],   pol_scn = "1.opt1-prb"),
  mutate(scn_comps[["opt1-crb"]], pol_scn = "2.opt1-crb")
)

#### additional context comparisons, especially for sensitivity of tech recon co-proposed option

scn_comp_context <- list(
  "pre2016-proposal" = compare_two_scenarios(scn_proj[["pre2016"]], scn_proj[["proposal_opt1"]]),
  "baseline_crb-proposal" = compare_two_scenarios(scn_proj[["baseline_crb"]], scn_proj[["proposal_opt1"]]),
  "baseline_prb-proposal" = compare_two_scenarios(scn_proj[["baseline_prb"]], scn_proj[["proposal_opt1"]]),
  "baseline_techalt-proposal" = compare_two_scenarios(scn_proj[["baseline_techalt"]], scn_proj[["proposal_opt1"]])
)

scn_combo_context <- bind_rows(
  mutate(scn_comp_context[["pre2016-proposal"]],   pol_scn = "0a.pre2016-proposal"),
  mutate(scn_comp_context[["baseline_crb-proposal"]],   pol_scn = "0b.baseline_crb-proposal"),
  mutate(scn_comp_context[["baseline_prb-proposal"]],   pol_scn = "0c.baseline_prb-proposal"),
  mutate(scn_comp_context[["baseline_techalt-proposal"]], pol_scn = "0d.baseline_techalt-proposal")
)

### Update other outputs ------

source("scripts/polrev_state_policy_inspect.R") # table to check state policies
write_csv(pol_tbl_combo, "output/csv/polrev_options_table.csv")


####### Excel output scenario summaries --------

scn_summ_tab <- function(comp_dat) {
  scn_comp_table(comp_dat,
                 var_list = list("Methane", "VOC", "HAP", "CH4_CO2e", 
                                 "capital_cost", "annual_cost", "gas_revenue",
                                 "ann_7", "ann_7_wgas", "ann_3", "ann_3_wgas"),
                 row_detail = "total",
                 show_years = 2014:2025)
}


scn_summ_list <- map(
  list(scn_comps[["opt1-prb"]],
       scn_comps[["opt1-crb"]]),
  .f = scn_summ_tab
) %>%
  set_names(c("opt1-prb", "opt1-crb"))


##### Text output for informal change detection ----------

# (scn_comp, table_specs) -> (table_data)

detail_tables <- scn_comp_table(scn_comps[["opt1-prb"]],
                                var_list = list("fac_affected", "Methane", "VOC", "HAP", "CH4_CO2e", "capital_cost", "ann_7_wgas"),
                                row_detail = "detail",
                                show_years = c(2020, 2025))

write_csv(detail_tables, "data/2019polrev_detail_tables.csv")
write_csv(bind_rows(scn_summ_list, .id = "id"), "output/csv/polrev_scn_summ_list.csv")


#### Ask the user whether to update xlsx outputs
# these are not automatically updated because they don't work well w/version control

switch(menu(c("Yes, Update RIA and TSD xlsx outputs.", 
              "No, do not update."),
            title = "Do you want to update EIA and TSD xlsx outputs?"), {
              
              # Update RIA xlsx outputs by rendering Rmarkown file
              rmarkdown::render("scripts/2019_policy_review_xlsx_output.Rmd",
                                output_dir = "output/docs")
              
              # Update TSD xlsx outputs by sourcing Rscript
              #source("scripts/tsd_tech_analysis_xlsx_output.R")
              #source("scripts/tsd_tables_proj_xlsx_output.R")
            }, "not updated")



