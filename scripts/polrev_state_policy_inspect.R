# state_policy_inspect.R

# Purpose of this is to verify how wellsites by state are treated in baseline and policy cases.
source('scripts/policy_2019policyreview.R')


  

############################

# placeholder activity data for policy table
ad_examples <- crossing(
  mp = c("gaswellsite", "oilassocgaswellsite", "oilwellsite", 
         "lp_gaswellsite", "lp_oilassocgaswellsite", "lp_oilwellsite",
         "gbstation", "transstation", "storstation", "cert"),
  location = c("AK-NS", "other_places")
) %>%
  mutate(
    year = 2020,
    vintage = 2020,
    attrs = NA_character_,
    fac_count = 1
  ) %>%
  select(mp, attrs, location, vintage, year, fac_count)

#### apply scenarios to example activity
mpf_examples <- ad_examples %>% {
  
  list(
    "pre2016"              = mutate(., fate = pre_2016_baseline(.),
                              pol_scn = "0a.pre2016"),
    "baseline_prb"   = mutate(., fate = polrev_2019_baseline_prb(.),
                                    pol_scn = "0b.baseline_prb"),
    "baseline_crb" = mutate(., fate = polrev_2019_baseline_crb(.),
                              pol_scn = "0c.baseline_crb"),
    "baseline_techalt" = mutate(., fate = polrev_2019_baseline_techalt(.),
                               pol_scn = "0d.baseline_techalt"),
    
    "proposed_opt1"       = mutate(., fate = polrev_2019_proposed_opt1(.),
                              pol_scn = "1.Proposed"),
    "alternative_opt2"    = mutate(., fate = polrev_2019_alternative_opt2(.),
                              pol_scn = "2.Alternative")
  )
} %>% map(function(mpf_ex){
  mpf_ex %>%
    mutate(mp = case_when(
      location == "AK-NS" & mp %in% c("gaswellsite", 
                                      "oilassocgaswellsite", 
                                      "oilwellsite", 
                                      "lp_gaswellsite",
                                      "lp_oilassocgaswellsite",
                                      "lp_oilwellsite") ~ "AK-NS wells",
      location == "AK-NS" & mp %in% c("gbstation") ~ "AK-NS G&B compr stns",
      location == "AK-NS" & mp %in% c("transstation", 
                                      "storstation") ~ "AK-NS T&S compr stns",
      TRUE ~ mp)) %>%
    select(pol_scn, mp, fate)
})

mp_ex_order <- c("gaswellsite", "oilassocgaswellsite", "oilwellsite",  
                 "lp_gaswellsite", 
                 "lp_oilassocgaswellsite", 
                 "lp_oilwellsite", "AK-NS wells", 
                 "gbstation", "transstation", "storstation", 
                 "AK-NS G&B compr stns", "AK-NS T&S compr stns", "cert")

pol_tbl_combo <-  bind_rows(
  mpf_examples[["pre2016"]],
  mpf_examples[["baseline_prb"]],
  mpf_examples[["baseline_crb"]],
  mpf_examples[["baseline_techalt"]],
  mpf_examples[["proposed_opt1"]],
  mpf_examples[["alternative_opt2"]]
) %>%
  distinct(pol_scn, mp, fate) %>%
  spread(pol_scn, fate) %>%
  mutate(mp = factor(mp, levels = mp_ex_order)) %>%
  arrange(mp)

rm(ad_examples, mp_ex_order, mpf_examples)

# outputs is pol_tbl_combo







