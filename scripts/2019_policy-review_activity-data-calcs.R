# 2019_policy_activity-data-calcs.R
# Calculation of activity data projections for the 2019 Policy Review Analysis RIA

######

# Activity data projections datasets include the following information:
# mp -- the model plant type being tracked
# attrs -- variation attributes on mp
# location -- state or region where the facility is located (for state program applicability)
# vintage -- when the plant was established/created/modified for regulatory applicability purposes
# year -- the projection year
# fac_count -- count of facilities



#### Activity data projection options ----------------
base_year <- 2014
year_range <- 2014:2025

#### No well-based activity data in policy reconsideration analysis, so DrillingInfo and AEO-based calculations are not necessary. 


####### GHGI typical new facilities based on Y-to-Y calculations -------

ghgi_srcs <- tribble(
  ~mp,              ~count_per_year,
  "transstation",     36,
  "storstation",      2,
  "recip_trans",      72,
  "recip_stor",       4,
  "centri_trans",     18,
  "centri_stor",      0, 
  "contbleed_contr",  308
) %>%
  mutate(attrs = NA_character_,
         location = NA_character_) %>%
  select(mp, attrs, location, count_per_year)

new_ts_proj <- expand(ghgi_srcs,
                  nesting(mp, attrs, location),
                  crossing(vintage = year_range)) %>%
  left_join(ghgi_srcs, by = c("mp", "attrs", "location")) %>%
  rename(fac_count = count_per_year)

ts_proj_default <- expand(new_ts_proj,
                          nesting(mp, attrs, location),
                          crossing(vintage = year_range,
                                   year = year_range)) %>%
  
  left_join(new_ts_proj, by = c("mp", "attrs", "location", "vintage")) %>%
  filter(year >= vintage)


###### Closed Vent Systems projections ----------------

# CVS design / infeasibility certification counts are estimated by a combination of estimates of storage tanks, pneumatic pumps, and centrifugal compressors.

base_cvs <- tibble(
  mp = c("cert"),
  attrs = c(NA_character_),
  location = c(NA_character_),
  base_year_count = 0, # storage tanks ~ wells, change per year
  fixed_count = 8 + 18 # 10% of 76 recip -> 8, all of 18 centri compressors
)

new_cvs_proj <- expand(base_cvs,
                  nesting(mp, attrs, location),
                  crossing(vintage = year_range)) %>%
  
  left_join(base_cvs, by = c("mp", "attrs", "location")) %>%
  mutate(year = vintage) %>%
  
  group_by(mp, attrs, location) %>%
  mutate(fac_count = fixed_count) %>%
  ungroup() %>%
  
  # Delete unnecessary columns used in prior calculation
  select(-base_year_count) %>%
  select(mp, attrs, location, vintage, year, fac_count)


cvs_proj_default <- expand(new_cvs_proj,
                           nesting(mp, attrs, location),
                           crossing(vintage = year_range,
                                    year = year_range)) %>%
  left_join(select(new_cvs_proj, -year), by = c("mp", "attrs", "location", "vintage")) %>%
  filter(year >= vintage)




##### Assemble overall activity data projection of relevant facilities ----------------

ad_proj_2019_polrev <- bind_rows(ts_proj_default,
                                 new_cvs_proj)








