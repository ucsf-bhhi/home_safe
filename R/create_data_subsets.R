# Create data subsets

demo_dat <- full_dat %>%
  select(id, reporting_agency, age:other_income)

int_dat <- full_dat %>%
  select(id, reporting_agency, intervention_1_type:intervention_6_amount)

follow_dat <- full_dat %>%
  select(id, reporting_agency, six_month_follow_up_living_situation_verified_date:twelve_month_follow_up_substantiated_aps_reports)

case_dat <- full_dat %>%
  select(reporting_agency, id, case_start_date:client_referred_to_ces, case_closure_date:living_situation_at_exit)

# Test that all variables were selected

length(full_dat) == length(demo_dat) + length(int_dat) +length(follow_dat) + length(case_dat) -3
