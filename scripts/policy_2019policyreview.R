#policy_2019policyreconsideration.R

# this script defines functions that take activity data scenarios (in the form of a data frame)
# and return a vector of fates. Usually these functiosn will be called with something like:
# `ad_proj %>% mutate(fate = policyfun(.))`

# the input data will have columns:
# mp
# year <- series, will be maintained
# fac_count <- will be summed, should not be added over differences in mp

# attrs - attributes of the facility that don't change the mp (e.g., emissions/cost outcomes) NOTE: not fully clear that attrs is necessary
# location - jurisdiction, could be grouped
# vintage - when was the facility constructed or modified

# mp, year, attrs, location, and vintage are used to determine mp-fate outcomes

# each of these functions needs to handle all mp types, attrs/location/vintage, and just once. 

source('scripts/policy_2018techreconsideration.R')

# using from tech recon policy script:
# * `more_stringent()`
# * `state_policy_2018`
# nsps2_effective_year <- 2015
# recon_effective_year <- 2019
# `pre_2016_baseline()`
# `policy_2018_baseline()` # e.g., pre-technical

polrev_effective_year <- 2019


#### Baseline Scenario Functions ------------------------

#' First baseline for policy review is proposed option 3 from technical reconsideration
polrev_2019_baseline_prb <- policy_2018_opt3

#' Alternative baseline for policy review is 2019 current policy (excluding technical proposal)
polrev_2019_baseline_crb <- policy_2018_baseline

# A third baseline, used for sensitivity, uses the technical reconsideration alternative with annual monitoring at compressor stations
polrev_2019_baseline_techalt <- policy_2018_ann_comps


#### Policy Scenario Functions ------------------------------

#' PROPOSAL SCENARIO, OPTION 1 (RESCINDS REQUIREMENTS IN T&S)
#' This function assumes the current policy baseline, so care must be taken in interpreting multiple baseline analyses.
#' 
polrev_2019_proposed_opt1 <- function(ad_proj) {
  #pol_scn <- "2019 Policy Reconsideration Proposed Option 1"
  
  state_fate <- state_policy_2018(ad_proj)
  
  scn_fate <- ad_proj %>%
    
    # reconsidered wellsite fugitive monitoring
    mutate(fate = case_when(
      
      vintage < 2015 ~ pre_2016_baseline(ad_proj),
      
      year < polrev_effective_year ~ polrev_2019_baseline_crb(ad_proj), 
      
      mp %in% c("transstation",
                "storstation") ~ "bau",
      
      mp %in% c("recip_trans",
                "recip_stor") ~ "bau",
      
      mp %in% c("centri_trans",
                "centri_stor") ~ "wetseal",
      
      mp == "contbleed_contr" ~ "highbleed",
      
      mp == "cert" ~ "no_cert",
      
      TRUE ~ polrev_2019_baseline_crb(ad_proj) # keep at baseline
    ))
  
  result <- more_stringent(state_fate, scn_fate$fate)
}


#' CO-PROPOSED ALTERNATIVE, OPTION 2 (NO REMOVAL OF T&S FROM REGULATION)
#' This function assumes the current policy baseline, so care must be taken in analyzing multiple baseline results.
#' 
polrev_2019_alternative_opt2 <- function(ad_proj) {
 #pol_scn <- "2019 Policy Review Alternative Option"

 state_fate <- state_policy_2018(ad_proj)

 scn_fate <- ad_proj %>%

   # no changes in standards
   mutate(fate = case_when(

     vintage < 2015 ~ pre_2016_baseline(ad_proj),

     year < polrev_effective_year ~ polrev_2019_baseline_crb(ad_proj),

     TRUE ~ polrev_2019_baseline_crb(ad_proj) # keep at baseline
   ))

 result <- more_stringent(state_fate, scn_fate$fate)
}




## Test Data:

# assuming that activity data script has been run:
# ad_proj <- ad_proj_2019_polrev

#mpf_bau <- mutate(ad_proj, fate = polrev_2019_baseline_prb(ad_proj))
#mpf_policy <- ad_proj %>% mutate(fate = polrev_2019_proposed_opt1(.))
