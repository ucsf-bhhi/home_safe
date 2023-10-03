Random sample
================
Sara Colom
2023-10-03

# Read libraries and data

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 4.2.3

``` r
library(tidyr)
library(stringr)

path_to_proj <- Sys.getenv("HOME_SAFE")

raw <- readxl::read_excel(paste(path_to_proj, "/Quantitative/data/report_1_data/home_safe_data_upload_090623.xlsx", sep = "")) %>% 
  janitor::clean_names()
```

# Filters

Filter for cases with `case_start_date` on or after `10/01/2022`

``` r
dim(raw)
```

    ## [1] 13087    74

``` r
dat <- raw %>% 
  mutate(case_start_date_recode = case_when(str_detect(case_start_date, ".427") ~ "2019-04-16",
                                            str_detect(case_start_date, ".527") ~ "2022-01-14",
                                            TRUE ~ case_start_date)) %>% 
  mutate(case_start_date_recode = lubridate::ymd(case_start_date_recode)) %>% 
  
  filter(case_start_date_recode >= lubridate::ymd("2022-10-01"))

dim(dat)
```

    ## [1] 3051   75

Filter those where ALL intervention types are either `NA` or state
`No Intervention`

``` r
interv <- dat %>% 
  select(id, case, ends_with("type")) %>% 
  pivot_longer(ends_with("type"), names_to = "intervention", values_to = "intervention_type")

interv %>% 
  count(intervention_type)
```

<div class="kable-table">

| intervention_type                 |     n |
|:----------------------------------|------:|
| Back Rent                         |     1 |
| Caregiver Services/Respite Care   |    53 |
| Deep Cleaning                     |    19 |
| Deep Cleaning/Hoarding Assistance |   110 |
| Emergency Shelter                 |   460 |
| Enhanced Case Management          |  1127 |
| External Housing Navigation       |     1 |
| Home Habitability                 |   116 |
| Home Repair                       |    28 |
| Housing Navigation                |   497 |
| Housing Navigator                 |    20 |
| Intrem Housing                    |     1 |
| Legal Services                    |    98 |
| Mortgage Payment                  |     8 |
| Moving Cost Assistance            |     1 |
| No Intervention                   | 10878 |
| Other                             |  1007 |
| Relocation                        |     6 |
| Relocation Assistance/Storage     |   120 |
| Rent Back-Pay                     |   115 |
| Rent Payment                      |   419 |
| Rental Assistance                 |    30 |
| Rental Assistance/Temp Housing    |     1 |
| Rental Asst./Temp Housing         |     1 |
| Security Deposit                  |   171 |
| Temp Housing/Rental Asst          |     1 |
| Temporary Housing                 |   548 |
| Utilities                         |   145 |
| Utility Payments                  |     1 |
| NA                                |  2323 |

</div>

To simplify the filter algorithim, I will make values binary where an
intervention value = `1` and `NA` or `No Intervention` = `O`

``` r
interv <- interv %>% 
  mutate(intervention_binary = case_when(is.na(intervention_type) ~ 0L,
                                         intervention_type == "No Intervention" ~ 0L,
                                         TRUE ~ 1L))

head(interv)
```

<div class="kable-table">

| id     | case | intervention         | intervention_type        | intervention_binary |
|:-------|:-----|:---------------------|:-------------------------|--------------------:|
| 113443 | 1    | intervention_1\_type | Enhanced Case Management |                   1 |
| 113443 | 1    | intervention_2\_type | No Intervention          |                   0 |
| 113443 | 1    | intervention_3\_type | No Intervention          |                   0 |
| 113443 | 1    | intervention_4\_type | No Intervention          |                   0 |
| 113443 | 1    | intervention_5\_type | No Intervention          |                   0 |
| 113443 | 1    | intervention_6\_type | No Intervention          |                   0 |

</div>

``` r
interv_summary <- interv %>% 
  group_by(id, case) %>% 
  summarise(total_interventions = sum(intervention_binary, na.rm = T))
```

    ## `summarise()` has grouped output by 'id'. You can override using the `.groups`
    ## argument.

Based on this list, we want to `exclude` individuals with
`No intervention` and where `intervention_type` is `NA`

``` r
records_to_exclude <- interv_summary %>% 
  filter(total_interventions == 0)

records_to_exclude %>% 
  length()
```

    ## [1] 3

Only three unique `id` by `case` combinations have NO intervention data.

``` r
exclude_these <- records_to_exclude %>% 
  mutate(exclude_id_case = paste(id, case, sep = "_")) %>% 
  pull(exclude_id_case) 
```

``` r
dim(dat)
```

    ## [1] 3051   75

``` r
dat <- dat %>% 
  mutate(id_case = paste(id, case, sep = "_")) %>% 
  filter(!id_case %in% exclude_these)

dim(dat)
```

    ## [1] 2421   76

# Data cleaning

Do any `ids` occur more than once?

``` r
dat <- dat %>% 
  add_count(id, name = "id_count")

dat %>% 
  count(id) %>% 
  filter(n > 1) %>% 
  nrow()
```

    ## [1] 64

It appears there are 64 `id`’s with more than once case.

In order to limit the sampling to the most **recent** case per ID, I
will first, `group_by` `id`, then sort descending by
`case_start_date_recode`, and finally `slice` for the first occurence
(i.e. most recent case by `id`).

``` r
# Quick sanity check before running below

dat %>% 
    filter(id_count > 1) %>% 
    group_by(id) %>% 
    arrange(id, desc(case_start_date_recode)) %>% 
    head()
```

<div class="kable-table">

| id     | case | reporting_agency |   age | case_start_date | location_of_participation | gender_identity | race_1                         | race_2             | ethnicity                   | current_marital_status          | sexual_orientation    | preferred_language | veteran_status     | medi_cal           | medicare           | representative_payee_or_conservator | living_situation_upon_entry | monthly_rent_mortgage_contribution | client_homeless_within_the_last_three_years | number_of_times_homelessness_occurred_in_the_last_three_years | total_duration_of_homelessness | last_period_of_homelessness | current_eviction_or_foreclosures | previous_evictions_or_foreclosures | discharge_from_institution_in_the_last_six_months | aps_report_date | aps_report_location | abuse_by_other_financial | abuse_by_other_non_financial | self_neglect | reporting_source | previous_aps_involvement | income_from_benefits | work_for_pay | other_income | client_referred_to_ces | intervention_1\_type     | intervention_1\_other_explanation | intervention_1\_date | intervention_1\_amount | intervention_2\_type | intervention_2\_other_explanation | intervention_2\_date | intervention_2\_amount | intervention_3\_type | intervention_3\_other_explanation | intervention_3\_date | intervention_3\_amount | intervention_4\_type | intervention_4\_other_explanation | intervention_4\_date | intervention_4\_amount | intervention_5\_type | intervention_5\_other_explanation | intervention_5\_date | intervention_5\_amount | intervention_6\_type | intervention_6\_other_explanation | intervention_6\_date | intervention_6\_amount | case_closure_date | living_situation_at_exit | six_month_follow_up_living_situation_verified_date | six_month_follow_up_method | six_month_follow_up_living_situation | six_month_follow_up_homelessness | six_month_follow_up_substantiated_aps_reports | twelve_month_follow_up_living_situation_verified_date | twelve_month_follow_up_method | twelve_month_follow_up_living_situation | twelve_month_follow_up_homelessness | twelve_month_follow_up_substantiated_aps_reports | comments | case_start_date_recode | id_case  | id_count |
|:-------|:-----|:-----------------|------:|:----------------|:--------------------------|:----------------|:-------------------------------|:-------------------|:----------------------------|:--------------------------------|:----------------------|:-------------------|:-------------------|:-------------------|:-------------------|:------------------------------------|:----------------------------|:-----------------------------------|:--------------------------------------------|:--------------------------------------------------------------|:-------------------------------|:----------------------------|:---------------------------------|:-----------------------------------|:--------------------------------------------------|:----------------|:--------------------|:-------------------------|:-----------------------------|:-------------|:-----------------|:-------------------------|---------------------:|:-------------|:-------------|:-----------------------|:-------------------------|:----------------------------------|:---------------------|:-----------------------|:---------------------|:----------------------------------|:---------------------|:-----------------------|:---------------------|:----------------------------------|:---------------------|:-----------------------|:---------------------|:----------------------------------|:---------------------|:-----------------------|:---------------------|:----------------------------------|:---------------------|:-----------------------|:---------------------|:----------------------------------|:---------------------|:-----------------------|:------------------|:-------------------------|:---------------------------------------------------|:---------------------------|:-------------------------------------|:---------------------------------|:----------------------------------------------|:------------------------------------------------------|:------------------------------|:----------------------------------------|:------------------------------------|:-------------------------------------------------|:---------|:-----------------------|:---------|---------:|
| 100315 | 3    | Riverside        | 65.37 | 2023-01-11      | Riverside                 | Female          | White                          | Data Not Collected | Non-Hispanic/Latin(A)(O)(X) | Divorced                        | Client Doesn’t Know   | English            | Data Not Collected | Data Not Collected | Data Not Collected | Data Not Collected                  | Homeless                    | 0                                  | Data Not Collected                          | Data Not Collected                                            | Data Not Collected             | Currently Homeless          | No                               | No                                 | No                                                | 2023-01-11      | Riverside           | No                       | No                           | Yes          | Self             | Yes                      |                 1308 | NA           | NA           | No                     | Enhanced Case Management | NA                                | 2023-01-11           | 0                      | Emergency Shelter    | NA                                | 2023-01-23           | 1190                   | Security Deposit     | NA                                | 2023-02-03           | 644                    | No Intervention      | NA                                | NA                   | 0                      | No Intervention      | NA                                | NA                   | 0                      | No Intervention      | NA                                | NA                   | 0                      | 2023-03-20        | Rent Leaseholder         | NA                                                 | NA                         | Data Not Collected                   | NA                               | 0                                             | NA                                                    | Data Not Collected            | Data Not Collected                      | Data Not Collected                  | 0                                                | NA       | 2023-01-11             | 100315_3 |        2 |
| 100315 | 2    | Riverside        | 65.15 | 2022-10-24      | Riverside                 | Female          | White                          | Data Not Collected | Non-Hispanic/Latin(A)(O)(X) | Divorced                        | Client Doesn’t Know   | English            | Data Not Collected | Data Not Collected | Data Not Collected | Data Not Collected                  | Temporary Housing           | NA                                 | Data Not Collected                          | Data Not Collected                                            | Data Not Collected             | Within The Last Month       | No                               | No                                 | No                                                | 2022-10-24      | Riverside           | No                       | No                           | Yes          | Self             | Yes                      |                 1258 | NA           | NA           | No                     | Enhanced Case Management | NA                                | 2022-11-01           | 0                      | Emergency Shelter    | NA                                | 2022-11-01           | 0                      | No Intervention      | NA                                | NA                   | 0                      | No Intervention      | NA                                | NA                   | 0                      | No Intervention      | NA                                | NA                   | 0                      | No Intervention      | NA                                | NA                   | 0                      | 2022-11-17        | Temporary Housing        | NA                                                 | NA                         | Data Not Collected                   | NA                               | 1                                             | NA                                                    | Data Not Collected            | Data Not Collected                      | Data Not Collected                  | 1                                                | NA       | 2022-10-24             | 100315_2 |        2 |
| 101080 | 2    | Riverside        | 71.42 | 2023-05-03      | Corona                    | Female          | Black/African American/African | Data Not Collected | Non-Hispanic/Latin(A)(O)(X) | Divorced                        | Straight/Heterosexual | English            | Data Not Collected | Data Not Collected | Data Not Collected | Data Not Collected                  | Data Not Collected          | NA                                 | Data Not Collected                          | Data Not Collected                                            | Data Not Collected             | Data Not Collected          | Data Not Collected               | Data Not Collected                 | Data Not Collected                                | 2023-05-03      | Corona              | No                       | No                           | Yes          | Unknown          | Yes                      |                   NA | NA           | NA           | Data Not Collected     | Other                    | NA                                | 2023-05-03           | 0                      | Temporary Housing    | NA                                | 2023-05-03           | 7470                   | Rent Payment         | NA                                | 2023-05-03           | 0                      | Housing Navigation   | NA                                | 2023-06-01           | 2450                   | No Intervention      | NA                                | NA                   | 0                      | No Intervention      | NA                                | NA                   | 0                      | NA                | Data Not Collected       | NA                                                 | NA                         | NA                                   | NA                               | NA                                            | NA                                                    | NA                            | NA                                      | NA                                  | NA                                               | NA       | 2023-05-03             | 101080_2 |        2 |
| 101080 | 1    | Riverside        | 71.29 | 2023-03-16      | Corona                    | Female          | Black/African American/African | Data Not Collected | Non-Hispanic/Latin(A)(O)(X) | Divorced                        | Straight/Heterosexual | English            | Data Not Collected | Data Not Collected | Yes                | Data Not Collected                  | Data Not Collected          | NA                                 | Data Not Collected                          | Data Not Collected                                            | Data Not Collected             | Data Not Collected          | Data Not Collected               | Data Not Collected                 | Data Not Collected                                | 2023-03-16      | Corona              | No                       | No                           | Yes          | Unknown          | Yes                      |                  210 | NA           | NA           | Data Not Collected     | Other                    | NA                                | 2023-03-17           | 0                      | Housing Navigation   | NA                                | 2023-03-17           | 0                      | Temporary Housing    | NA                                | 2023-03-17           | 0                      | No Intervention      | NA                                | NA                   | 0                      | No Intervention      | NA                                | NA                   | 0                      | No Intervention      | NA                                | NA                   | 0                      | 2023-04-10        | Homeless                 | NA                                                 | NA                         | Data Not Collected                   | NA                               | 0                                             | NA                                                    | Data Not Collected            | Data Not Collected                      | Data Not Collected                  | 0                                                | NA       | 2023-03-16             | 101080_1 |        2 |
| 101251 | 7    | Riverside        | 73.16 | 2023-03-06      | Murrieta                  | Male            | White                          | Data Not Collected | Non-Hispanic/Latin(A)(O)(X) | Not Married/Living With Partner | Straight/Heterosexual | English            | Data Not Collected | Data Not Collected | Data Not Collected | Data Not Collected                  | Homeless                    | 0                                  | Yes                                         | Four/More Times                                               | More Than A Year               | Within The Last Year        | No                               | No                                 | No                                                | 2023-03-06      | Murrieta            | No                       | No                           | Yes          | Self             | Yes                      |                 1000 | 0            | 0            | No                     | Enhanced Case Management | NA                                | 2023-03-23           | 0                      | No Intervention      | NA                                | NA                   | 0                      | No Intervention      | NA                                | NA                   | 0                      | No Intervention      | NA                                | NA                   | 0                      | No Intervention      | NA                                | NA                   | 0                      | No Intervention      | NA                                | NA                   | 0                      | 2023-05-10        | Homeless                 | NA                                                 | NA                         | Data Not Collected                   | NA                               | 0                                             | NA                                                    | Data Not Collected            | Data Not Collected                      | Data Not Collected                  | 0                                                | NA       | 2023-03-06             | 101251_7 |        2 |
| 101251 | 6    | Riverside        | 72.80 | 2022-10-25      | Murrieta                  | Male            | White                          | Data Not Collected | Non-Hispanic/Latin(A)(O)(X) | Not Married/Living With Partner | Straight/Heterosexual | English            | Data Not Collected | Data Not Collected | Data Not Collected | Data Not Collected                  | Homeless                    | 0                                  | Data Not Collected                          | Data Not Collected                                            | Data Not Collected             | Within The Last Three Years | No                               | No                                 | No                                                | 2022-10-25      | Murrieta            | No                       | No                           | Yes          | Social Worker    | Yes                      |                  960 | NA           | NA           | Data Not Collected     | Other                    | NA                                | 2022-11-02           | 0                      | Housing Navigation   | NA                                | 2022-11-02           | 0                      | No Intervention      | NA                                | NA                   | 0                      | No Intervention      | NA                                | NA                   | 0                      | No Intervention      | NA                                | NA                   | 0                      | No Intervention      | NA                                | NA                   | 0                      | 2023-01-13        | Other                    | NA                                                 | NA                         | Data Not Collected                   | NA                               | 1                                             | NA                                                    | Data Not Collected            | Data Not Collected                      | Data Not Collected                  | 1                                                | NA       | 2022-10-25             | 101251_6 |        2 |

</div>

``` r
dat_subset <- dat %>% 
  group_by(id) %>% 
  arrange(id, desc(case_start_date_recode)) %>% 
  slice(1) %>% 
  ungroup()
```

Evaluate `100315` to double check that the correct one was selected.

``` r
dat_subset %>% 
  filter(id == 100315)
```

<div class="kable-table">

| id     | case | reporting_agency |   age | case_start_date | location_of_participation | gender_identity | race_1 | race_2             | ethnicity                   | current_marital_status | sexual_orientation  | preferred_language | veteran_status     | medi_cal           | medicare           | representative_payee_or_conservator | living_situation_upon_entry | monthly_rent_mortgage_contribution | client_homeless_within_the_last_three_years | number_of_times_homelessness_occurred_in_the_last_three_years | total_duration_of_homelessness | last_period_of_homelessness | current_eviction_or_foreclosures | previous_evictions_or_foreclosures | discharge_from_institution_in_the_last_six_months | aps_report_date | aps_report_location | abuse_by_other_financial | abuse_by_other_non_financial | self_neglect | reporting_source | previous_aps_involvement | income_from_benefits | work_for_pay | other_income | client_referred_to_ces | intervention_1\_type     | intervention_1\_other_explanation | intervention_1\_date | intervention_1\_amount | intervention_2\_type | intervention_2\_other_explanation | intervention_2\_date | intervention_2\_amount | intervention_3\_type | intervention_3\_other_explanation | intervention_3\_date | intervention_3\_amount | intervention_4\_type | intervention_4\_other_explanation | intervention_4\_date | intervention_4\_amount | intervention_5\_type | intervention_5\_other_explanation | intervention_5\_date | intervention_5\_amount | intervention_6\_type | intervention_6\_other_explanation | intervention_6\_date | intervention_6\_amount | case_closure_date | living_situation_at_exit | six_month_follow_up_living_situation_verified_date | six_month_follow_up_method | six_month_follow_up_living_situation | six_month_follow_up_homelessness | six_month_follow_up_substantiated_aps_reports | twelve_month_follow_up_living_situation_verified_date | twelve_month_follow_up_method | twelve_month_follow_up_living_situation | twelve_month_follow_up_homelessness | twelve_month_follow_up_substantiated_aps_reports | comments | case_start_date_recode | id_case  | id_count |
|:-------|:-----|:-----------------|------:|:----------------|:--------------------------|:----------------|:-------|:-------------------|:----------------------------|:-----------------------|:--------------------|:-------------------|:-------------------|:-------------------|:-------------------|:------------------------------------|:----------------------------|:-----------------------------------|:--------------------------------------------|:--------------------------------------------------------------|:-------------------------------|:----------------------------|:---------------------------------|:-----------------------------------|:--------------------------------------------------|:----------------|:--------------------|:-------------------------|:-----------------------------|:-------------|:-----------------|:-------------------------|---------------------:|:-------------|:-------------|:-----------------------|:-------------------------|:----------------------------------|:---------------------|:-----------------------|:---------------------|:----------------------------------|:---------------------|:-----------------------|:---------------------|:----------------------------------|:---------------------|:-----------------------|:---------------------|:----------------------------------|:---------------------|:-----------------------|:---------------------|:----------------------------------|:---------------------|:-----------------------|:---------------------|:----------------------------------|:---------------------|:-----------------------|:------------------|:-------------------------|:---------------------------------------------------|:---------------------------|:-------------------------------------|:---------------------------------|:----------------------------------------------|:------------------------------------------------------|:------------------------------|:----------------------------------------|:------------------------------------|:-------------------------------------------------|:---------|:-----------------------|:---------|---------:|
| 100315 | 3    | Riverside        | 65.37 | 2023-01-11      | Riverside                 | Female          | White  | Data Not Collected | Non-Hispanic/Latin(A)(O)(X) | Divorced               | Client Doesn’t Know | English            | Data Not Collected | Data Not Collected | Data Not Collected | Data Not Collected                  | Homeless                    | 0                                  | Data Not Collected                          | Data Not Collected                                            | Data Not Collected             | Currently Homeless          | No                               | No                                 | No                                                | 2023-01-11      | Riverside           | No                       | No                           | Yes          | Self             | Yes                      |                 1308 | NA           | NA           | No                     | Enhanced Case Management | NA                                | 2023-01-11           | 0                      | Emergency Shelter    | NA                                | 2023-01-23           | 1190                   | Security Deposit     | NA                                | 2023-02-03           | 644                    | No Intervention      | NA                                | NA                   | 0                      | No Intervention      | NA                                | NA                   | 0                      | No Intervention      | NA                                | NA                   | 0                      | 2023-03-20        | Rent Leaseholder         | NA                                                 | NA                         | Data Not Collected                   | NA                               | 0                                             | NA                                                    | Data Not Collected            | Data Not Collected                      | Data Not Collected                  | 0                                                | NA       | 2023-01-11             | 100315_3 |        2 |

</div>

Looks good.

# Random sample

First quickly glimpse at what the N is for reporting agencies w/ the
smallest sizes.

``` r
dat_subset %>% 
  count(reporting_agency) %>% 
  arrange(n) %>% 
  head()
```

<div class="kable-table">

| reporting_agency |   n |
|:-----------------|----:|
| El Dorado        |   1 |
| Sutter           |   2 |
| Calaveras        |   3 |
| Modoc            |   3 |
| Inyo             |   4 |
| San Mateo        |   6 |

</div>

Some reporting agencies have less than 10 cases.

Assign agency as a `small_agency` (10 cases or less) or `not_small`
(more than 10 cases)

``` r
dat_subset <- dat_subset %>% 
  add_count(reporting_agency) %>% 
  mutate(`agency_size` = if_else(n <= 10, "small_agency", "not_small"))

small_agencies <- dat_subset %>% 
  filter(agency_size == "small_agency") %>% 
  droplevels()

all_other_agencies <- dat_subset %>% 
  anti_join(small_agencies) %>% 
  droplevels()
```

# Alternative I.

If N \< 10 capture
`10%  across all SMALL agencies or just capture 1 per agency  Anything more we capture`10%\`

``` r
set.seed(123)

random_sample_small_agencies_i <- small_agencies %>% 
  group_by(reporting_agency) %>% 
  sample_n(1)
```

# Sample all other agencies

``` r
all_other_agencies <- all_other_agencies %>% 
  mutate(sample_n = ceiling(n*0.1))

set.seed(123)

random_sample_other_agencies <- all_other_agencies %>% 
  group_by(reporting_agency) %>% 
  group_modify(~sample_n(.x, sample_n[1])) %>% 
  ungroup()
```

# Distribution of samples

The smaller agencies

``` r
dist_small <- small_agencies %>% 
  ungroup() %>% 
  select(race_1, ethnicity, reporting_agency) %>% 
  pivot_longer(everything(), names_to = "variable") %>% 
  group_by(variable, value) %>% 
  summarise(n = n()) %>% 
  ungroup(value) %>% 
  mutate(statistic = n/sum(n)) %>% 
  bind_rows(
    small_agencies %>% 
    summarise(variable = "age",
              sd = sd(age, na.rm = T),
              statistic = mean(age, na.rm = T))
  ) %>% 
  mutate(across(where(is.numeric), ~round(.x, 2))) %>% 
  mutate(sample = "Full Sample") %>% 
  mutate(statistic = case_when(variable == "age" ~ paste(statistic, " (", sd, ")", sep = ""),
                               is.na(statistic) ~ NA,
                               TRUE ~ paste(n, " ", "(", statistic*100, "%",")", sep = ""))) %>% 
  select(variable, value, statistic, sample)
```

    ## `summarise()` has grouped output by 'variable'. You can override using the
    ## `.groups` argument.

``` r
dist_small_i <- random_sample_small_agencies_i %>% 
  select(race_1, ethnicity, reporting_agency) %>% 
  pivot_longer(everything(), names_to = "variable") %>% 
  group_by(variable, value) %>% 
  summarise(n = n()) %>% 
  ungroup(value) %>% 
  mutate(statistic = n/sum(n)) %>% 
  bind_rows(
    small_agencies %>% 
    summarise(variable = "age",
              sd = sd(age, na.rm = T),
              statistic = mean(age, na.rm = T))
  ) %>% 
  mutate(across(where(is.numeric), ~round(.x, 2))) %>% 
  mutate(sample = "Random Sample I") %>% 
  mutate(statistic = case_when(variable == "age" ~ paste(statistic, " (", sd, ")", sep = ""),
                               is.na(statistic) ~ NA,
                               TRUE ~ paste(n, " ", "(", statistic*100, "%",")", sep = ""))) %>% 
  select(variable, value, statistic, sample)
```

    ## `summarise()` has grouped output by 'variable'. You can override using the
    ## `.groups` argument.

``` r
full_stats_small_agency <- dist_small %>% 
  bind_rows(dist_small_i) %>% 
  pivot_wider(names_from = sample, values_from = statistic)

full_stats_small_agency
```

<div class="kable-table">

| variable         | value                                     | Full Sample  | Random Sample I |
|:-----------------|:------------------------------------------|:-------------|:----------------|
| ethnicity        | American Indian/Alaskan Native            | 1 (1%)       | NA              |
| ethnicity        | Client Doesn’t Know                       | 2 (3%)       | NA              |
| ethnicity        | Data Not Collected                        | 6 (9%)       | 1 (8%)          |
| ethnicity        | Hispanic/Latin(A)(O)(X)                   | 8 (11%)      | NA              |
| ethnicity        | Hispanic/Latino                           | 1 (1%)       | NA              |
| ethnicity        | Non-Hispanic/Latin(A)(O)(X)               | 47 (67%)     | 10 (83%)        |
| ethnicity        | Not Hispanic/Latino                       | 3 (4%)       | NA              |
| ethnicity        | Unknown                                   | 2 (3%)       | 1 (8%)          |
| race_1           | American Indian/Alaskan Native/Indigenous | 2 (3%)       | NA              |
| race_1           | Asian/Asian American                      | 3 (4%)       | 1 (8%)          |
| race_1           | Black/African American                    | 1 (1%)       | NA              |
| race_1           | Client Doesn’t Know                       | 2 (3%)       | NA              |
| race_1           | Unknown/Not Provided                      | 1 (1%)       | 1 (8%)          |
| race_1           | White                                     | 61 (87%)     | 10 (83%)        |
| reporting_agency | Amador                                    | 10 (14%)     | 1 (8%)          |
| reporting_agency | Calaveras                                 | 3 (4%)       | 1 (8%)          |
| reporting_agency | Del Norte                                 | 9 (13%)      | 1 (8%)          |
| reporting_agency | El Dorado                                 | 1 (1%)       | 1 (8%)          |
| reporting_agency | Inyo                                      | 4 (6%)       | 1 (8%)          |
| reporting_agency | Madera                                    | 7 (10%)      | 1 (8%)          |
| reporting_agency | Modoc                                     | 3 (4%)       | 1 (8%)          |
| reporting_agency | Mono                                      | 9 (13%)      | 1 (8%)          |
| reporting_agency | San Mateo                                 | 6 (9%)       | 1 (8%)          |
| reporting_agency | Siskiyou                                  | 7 (10%)      | 1 (8%)          |
| reporting_agency | Sutter                                    | 2 (3%)       | 1 (8%)          |
| reporting_agency | Yuba                                      | 9 (13%)      | 1 (8%)          |
| age              | NA                                        | 68.67 (9.77) | 68.67 (9.77)    |

</div>

All other agencies

``` r
dist_other <- all_other_agencies %>% 
  ungroup() %>% 
  select(race_1, ethnicity, reporting_agency) %>% 
  pivot_longer(everything(), names_to = "variable") %>% 
  group_by(variable, value) %>% 
  summarise(n = n()) %>% 
  ungroup(value) %>% 
  mutate(statistic = n/sum(n)) %>% 
  bind_rows(
    all_other_agencies %>% 
    summarise(variable = "age",
              sd = sd(age, na.rm = T),
              statistic = mean(age, na.rm = T))
  ) %>% 
  mutate(across(where(is.numeric), ~round(.x, 2))) %>% 
  mutate(sample = "Full Sample") %>% 
  mutate(statistic = case_when(variable == "age" ~ paste(statistic, " (", sd, ")", sep = ""),
                               is.na(statistic) ~ NA,
                               TRUE ~ paste(n, " ", "(", statistic*100, "%",")", sep = ""))) %>% 
  select(variable, value, statistic, sample)
```

    ## `summarise()` has grouped output by 'variable'. You can override using the
    ## `.groups` argument.

``` r
dist_other_sample <- random_sample_other_agencies %>% 
  select(race_1, ethnicity, reporting_agency) %>% 
  pivot_longer(everything(), names_to = "variable") %>% 
  group_by(variable, value) %>% 
  summarise(n = n()) %>% 
  ungroup(value) %>% 
  mutate(statistic = n/sum(n)) %>% 
  bind_rows(
    random_sample_other_agencies %>% 
    summarise(variable = "age",
              sd = sd(age, na.rm = T),
              statistic = mean(age, na.rm = T))
  ) %>% 
  mutate(across(where(is.numeric), ~round(.x, 2))) %>% 
  mutate(sample = "Random Sample I") %>% 
  mutate(statistic = case_when(variable == "age" ~ paste(statistic, " (", sd, ")", sep = ""),
                               is.na(statistic) ~ NA,
                               TRUE ~ paste(n, " ", "(", statistic*100, "%",")", sep = ""))) %>% 
  select(variable, value, statistic, sample)
```

    ## `summarise()` has grouped output by 'variable'. You can override using the
    ## `.groups` argument.

``` r
full_stats_other_agency <- dist_other %>% 
  bind_rows(dist_other_sample) %>% 
  pivot_wider(names_from = sample, values_from = statistic)

full_stats_other_agency
```

<div class="kable-table">

| variable         | value                                     | Full Sample   | Random Sample I |
|:-----------------|:------------------------------------------|:--------------|:----------------|
| ethnicity        | Client Doesn’t Know                       | 181 (8%)      | 17 (7%)         |
| ethnicity        | Client Refused                            | 2 (0%)        | NA              |
| ethnicity        | Cuban                                     | 2 (0%)        | NA              |
| ethnicity        | Data Not Collected                        | 211 (9%)      | 30 (12%)        |
| ethnicity        | Hispanic/Latin(A)(O)(X)                   | 352 (15%)     | 36 (15%)        |
| ethnicity        | Mexican/Chicano                           | 9 (0%)        | 1 (0%)          |
| ethnicity        | Non-Hispanic/Latin(A)(O)(X)               | 1230 (54%)    | 130 (53%)       |
| ethnicity        | Not Hispanic/Latino                       | 175 (8%)      | 16 (7%)         |
| ethnicity        | Other Hispanic/Latino                     | 48 (2%)       | 9 (4%)          |
| ethnicity        | Unknown/Not Provided                      | 69 (3%)       | 6 (2%)          |
| ethnicity        | NA                                        | 4 (0%)        | NA              |
| race_1           | American Indian/Alaskan Native            | 3 (0%)        | NA              |
| race_1           | American Indian/Alaskan Native/Indigenous | 34 (1%)       | 6 (2%)          |
| race_1           | Asian                                     | 5 (0%)        | NA              |
| race_1           | Asian/Asian American                      | 48 (2%)       | 4 (2%)          |
| race_1           | Black/African American                    | 65 (3%)       | 6 (2%)          |
| race_1           | Black/African American/African            | 300 (13%)     | 31 (13%)        |
| race_1           | Client Doesn’t Know                       | 4 (0%)        | NA              |
| race_1           | Client Refused                            | 20 (1%)       | 1 (0%)          |
| race_1           | Data Not Collected                        | 152 (7%)      | 18 (7%)         |
| race_1           | Native Hawaiian/Pacific Islander          | 15 (1%)       | 1 (0%)          |
| race_1           | Other                                     | 133 (6%)      | 12 (5%)         |
| race_1           | Unknown/Not Provided                      | 55 (2%)       | 10 (4%)         |
| race_1           | White                                     | 1448 (63%)    | 156 (64%)       |
| race_1           | NA                                        | 1 (0%)        | NA              |
| reporting_agency | Alameda                                   | 97 (4%)       | 10 (4%)         |
| reporting_agency | Butte                                     | 50 (2%)       | 5 (2%)          |
| reporting_agency | Colusa                                    | 16 (1%)       | 2 (1%)          |
| reporting_agency | Contra Costa                              | 23 (1%)       | 3 (1%)          |
| reporting_agency | Fresno                                    | 16 (1%)       | 2 (1%)          |
| reporting_agency | Glenn                                     | 25 (1%)       | 3 (1%)          |
| reporting_agency | Humboldt                                  | 28 (1%)       | 3 (1%)          |
| reporting_agency | Kern                                      | 102 (4%)      | 11 (4%)         |
| reporting_agency | Kings                                     | 12 (1%)       | 2 (1%)          |
| reporting_agency | Los Angeles                               | 44 (2%)       | 5 (2%)          |
| reporting_agency | Marin                                     | 14 (1%)       | 2 (1%)          |
| reporting_agency | Mendocino                                 | 17 (1%)       | 2 (1%)          |
| reporting_agency | Merced                                    | 23 (1%)       | 3 (1%)          |
| reporting_agency | Napa                                      | 24 (1%)       | 3 (1%)          |
| reporting_agency | Nevada                                    | 37 (2%)       | 4 (2%)          |
| reporting_agency | Orange                                    | 102 (4%)      | 11 (4%)         |
| reporting_agency | Placer                                    | 20 (1%)       | 2 (1%)          |
| reporting_agency | Plumas                                    | 11 (0%)       | 2 (1%)          |
| reporting_agency | Riverside                                 | 763 (33%)     | 77 (31%)        |
| reporting_agency | Sacramento                                | 28 (1%)       | 3 (1%)          |
| reporting_agency | San Bernardino                            | 247 (11%)     | 25 (10%)        |
| reporting_agency | San Diego                                 | 76 (3%)       | 8 (3%)          |
| reporting_agency | San Francisco                             | 91 (4%)       | 10 (4%)         |
| reporting_agency | San Joaquin                               | 22 (1%)       | 3 (1%)          |
| reporting_agency | San Luis Obispo                           | 37 (2%)       | 4 (2%)          |
| reporting_agency | Santa Barbara                             | 18 (1%)       | 2 (1%)          |
| reporting_agency | Santa Clara                               | 58 (3%)       | 6 (2%)          |
| reporting_agency | Santa Cruz                                | 11 (0%)       | 2 (1%)          |
| reporting_agency | Shasta                                    | 19 (1%)       | 2 (1%)          |
| reporting_agency | Solano                                    | 11 (0%)       | 2 (1%)          |
| reporting_agency | Sonoma                                    | 78 (3%)       | 8 (3%)          |
| reporting_agency | Stanislaus                                | 54 (2%)       | 6 (2%)          |
| reporting_agency | Tulare                                    | 17 (1%)       | 2 (1%)          |
| reporting_agency | Tuolumne                                  | 13 (1%)       | 2 (1%)          |
| reporting_agency | Ventura                                   | 59 (3%)       | 6 (2%)          |
| reporting_agency | Yolo                                      | 20 (1%)       | 2 (1%)          |
| age              | NA                                        | 65.18 (63.11) | 65.73 (11.2)    |

</div>

# Total N sampled

``` r
random_sample_other_agencies %>% 
  nrow()
```

    ## [1] 245

``` r
random_sample_small_agencies_i %>% 
  nrow()
```

    ## [1] 12

Now–I will use the combination of `id` and `case` number to subset the
first random sample from the `raw` (original) data.

``` r
small_agencies_id <- paste(random_sample_small_agencies_i$id, random_sample_small_agencies_i$case, sep = "_")
  
other_agencies <- paste(random_sample_other_agencies$id, random_sample_other_agencies$case, sep = "_")  
 
raw_sample <- raw %>% 
  filter(paste(id, case, sep = "_") %in% c(small_agencies_id, other_agencies))
```

Make sure that only ONE `id` is selected in the raw sample.

``` r
raw_sample %>% 
  count(id) %>% 
  arrange(desc(n)) %>% 
  head()
```

<div class="kable-table">

| id     |   n |
|:-------|----:|
| 100164 |   1 |
| 100221 |   1 |
| 100531 |   1 |
| 100555 |   1 |
| 100615 |   1 |
| 100714 |   1 |

</div>

Save the `id` and `case` selected in this iteration as a `tibble`

``` r
selected_id_case <- tibble(
  id_case = c(other_agencies, small_agencies_id)
)
```

# Export samples and stats

``` r
output <- list(id_case_selected = selected_id_case,
               stats_small_agency = full_stats_small_agency, 
               stats_all_other_agency = full_stats_other_agency)

writexl::write_xlsx(output, paste(path_to_proj, "/Quantitative/data/random_sample_iteration_1/stats_randomized_sample.xlsx", sep = ""))

write.csv(raw_sample, paste(path_to_proj, "/Quantitative/data/random_sample_iteration_1/randomized_sample.csv", sep = ""), row.names = F)

write.csv(raw, paste(path_to_proj, "/Quantitative/data/random_sample_iteration_1/og_file_used.csv", sep = ""))
```
