Random sample
================
Sara Colom
2023-10-02

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

Filter for cases with `case_start_date` on or after `10/01/2022`

``` r
dat <- raw %>% 
  mutate(case_start_date_recode = case_when(str_detect(case_start_date, ".427") ~ "2019-04-16",
                                            str_detect(case_start_date, ".527") ~ "2022-01-14",
                                            TRUE ~ case_start_date)) %>% 
  mutate(case_start_date_recode = lubridate::ymd(case_start_date_recode)) %>% 
  
  filter(case_start_date_recode >= lubridate::ymd("2022-10-01"))
```

Do any `ids` occur more than once?

``` r
dat <- dat %>% 
  add_count(id, name = "id_count")

dat %>% 
  count(id) %>% 
  filter(n > 1) %>% 
  nrow()
```

    ## [1] 105

It appears there are 105 `id`’s with more than once case.

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

| id     | case | reporting_agency |   age | case_start_date | location_of_participation | gender_identity | race_1                         | race_2             | ethnicity                   | current_marital_status | sexual_orientation    | preferred_language | veteran_status     | medi_cal           | medicare           | representative_payee_or_conservator | living_situation_upon_entry | monthly_rent_mortgage_contribution | client_homeless_within_the_last_three_years | number_of_times_homelessness_occurred_in_the_last_three_years | total_duration_of_homelessness | last_period_of_homelessness | current_eviction_or_foreclosures | previous_evictions_or_foreclosures | discharge_from_institution_in_the_last_six_months | aps_report_date | aps_report_location | abuse_by_other_financial | abuse_by_other_non_financial | self_neglect | reporting_source | previous_aps_involvement | income_from_benefits | work_for_pay | other_income | client_referred_to_ces | intervention_1\_type     | intervention_1\_other_explanation | intervention_1\_date | intervention_1\_amount | intervention_2\_type | intervention_2\_other_explanation                                                           | intervention_2\_date | intervention_2\_amount | intervention_3\_type | intervention_3\_other_explanation | intervention_3\_date | intervention_3\_amount | intervention_4\_type | intervention_4\_other_explanation | intervention_4\_date | intervention_4\_amount | intervention_5\_type | intervention_5\_other_explanation | intervention_5\_date | intervention_5\_amount | intervention_6\_type | intervention_6\_other_explanation | intervention_6\_date | intervention_6\_amount | case_closure_date | living_situation_at_exit | six_month_follow_up_living_situation_verified_date | six_month_follow_up_method | six_month_follow_up_living_situation | six_month_follow_up_homelessness | six_month_follow_up_substantiated_aps_reports | twelve_month_follow_up_living_situation_verified_date | twelve_month_follow_up_method | twelve_month_follow_up_living_situation | twelve_month_follow_up_homelessness | twelve_month_follow_up_substantiated_aps_reports | comments | case_start_date_recode | id_count |
|:-------|:-----|:-----------------|------:|:----------------|:--------------------------|:----------------|:-------------------------------|:-------------------|:----------------------------|:-----------------------|:----------------------|:-------------------|:-------------------|:-------------------|:-------------------|:------------------------------------|:----------------------------|:-----------------------------------|:--------------------------------------------|:--------------------------------------------------------------|:-------------------------------|:----------------------------|:---------------------------------|:-----------------------------------|:--------------------------------------------------|:----------------|:--------------------|:-------------------------|:-----------------------------|:-------------|:-----------------|:-------------------------|---------------------:|:-------------|:-------------|:-----------------------|:-------------------------|:----------------------------------|:---------------------|:-----------------------|:---------------------|:--------------------------------------------------------------------------------------------|:---------------------|:-----------------------|:---------------------|:----------------------------------|:---------------------|:-----------------------|:---------------------|:----------------------------------|:---------------------|:-----------------------|:---------------------|:----------------------------------|:---------------------|:-----------------------|:---------------------|:----------------------------------|:---------------------|:-----------------------|:------------------|:-------------------------|:---------------------------------------------------|:---------------------------|:-------------------------------------|:---------------------------------|:----------------------------------------------|:------------------------------------------------------|:------------------------------|:----------------------------------------|:------------------------------------|:-------------------------------------------------|:---------|:-----------------------|---------:|
| 100251 | 2    | Placer           | 62.99 | 2022-12-14      | Foresthill                | Female          | White                          | White              | Non-Hispanic/Latin(A)(O)(X) | Widowed                | Straight/Heterosexual | English            | No                 | Yes                | No                 | Yes                                 | Owner Lives Alone           | 400                                | Yes                                         | Three Times                                                   | More Than A Year               | Within The Last Three Years | No                               | No                                 | Yes                                               | NA              | Foresthill          | No                       | No                           | Yes          | NA               | NA                       |                 1321 | 0            | 0            | No                     | Home Habitability        | NA                                | 2022-12-01           | 154.34                 | Other                | Paid for Her Birth Certificate. Client is on the EHV list with Roseville Housing Authority. | 2023-03-17           | 41.95                  | No Intervention      | NA                                | NA                   | 0                      | No Intervention      | NA                                | NA                   | 0                      | No Intervention      | NA                                | NA                   | 0                      | No Intervention      | NA                                | NA                   | 0                      | NA                | Rent Leaseholder         | NA                                                 | NA                         | NA                                   | NA                               | NA                                            | NA                                                    | NA                            | NA                                      | NA                                  | NA                                               | NA       | 2022-12-14             |        2 |
| 100251 | 1    | Placer           | 62.83 | 2022-10-14      | Foresthill                | Female          | White                          | White              | Not Hispanic/Latino         | Widowed                | Straight/Heterosexual | English            | No                 | Yes                | Yes                | Yes                                 | Owner Lives Alone           | 400                                | Yes                                         | 1                                                             | More Than A Year               | Within The Last Three Years | No                               | No                                 | No                                                | NA              | NA                  | NA                       | NA                           | NA           | NA               | NA                       |                   NA | NA           | NA           | NA                     | No Intervention          | NA                                | NA                   | 0                      | No Intervention      | NA                                                                                          | NA                   | 0                      | No Intervention      | NA                                | NA                   | 0                      | No Intervention      | NA                                | NA                   | 0                      | No Intervention      | NA                                | NA                   | 0                      | No Intervention      | NA                                | NA                   | 0                      | NA                | NA                       | NA                                                 | NA                         | NA                                   | NA                               | NA                                            | NA                                                    | NA                            | NA                                      | NA                                  | NA                                               | NA       | 2022-10-14             |        2 |
| 100315 | 3    | Riverside        | 65.37 | 2023-01-11      | Riverside                 | Female          | White                          | Data Not Collected | Non-Hispanic/Latin(A)(O)(X) | Divorced               | Client Doesn’t Know   | English            | Data Not Collected | Data Not Collected | Data Not Collected | Data Not Collected                  | Homeless                    | 0                                  | Data Not Collected                          | Data Not Collected                                            | Data Not Collected             | Currently Homeless          | No                               | No                                 | No                                                | 2023-01-11      | Riverside           | No                       | No                           | Yes          | Self             | Yes                      |                 1308 | NA           | NA           | No                     | Enhanced Case Management | NA                                | 2023-01-11           | 0                      | Emergency Shelter    | NA                                                                                          | 2023-01-23           | 1190                   | Security Deposit     | NA                                | 2023-02-03           | 644                    | No Intervention      | NA                                | NA                   | 0                      | No Intervention      | NA                                | NA                   | 0                      | No Intervention      | NA                                | NA                   | 0                      | 2023-03-20        | Rent Leaseholder         | NA                                                 | NA                         | Data Not Collected                   | NA                               | 0                                             | NA                                                    | Data Not Collected            | Data Not Collected                      | Data Not Collected                  | 0                                                | NA       | 2023-01-11             |        2 |
| 100315 | 2    | Riverside        | 65.15 | 2022-10-24      | Riverside                 | Female          | White                          | Data Not Collected | Non-Hispanic/Latin(A)(O)(X) | Divorced               | Client Doesn’t Know   | English            | Data Not Collected | Data Not Collected | Data Not Collected | Data Not Collected                  | Temporary Housing           | NA                                 | Data Not Collected                          | Data Not Collected                                            | Data Not Collected             | Within The Last Month       | No                               | No                                 | No                                                | 2022-10-24      | Riverside           | No                       | No                           | Yes          | Self             | Yes                      |                 1258 | NA           | NA           | No                     | Enhanced Case Management | NA                                | 2022-11-01           | 0                      | Emergency Shelter    | NA                                                                                          | 2022-11-01           | 0                      | No Intervention      | NA                                | NA                   | 0                      | No Intervention      | NA                                | NA                   | 0                      | No Intervention      | NA                                | NA                   | 0                      | No Intervention      | NA                                | NA                   | 0                      | 2022-11-17        | Temporary Housing        | NA                                                 | NA                         | Data Not Collected                   | NA                               | 1                                             | NA                                                    | Data Not Collected            | Data Not Collected                      | Data Not Collected                  | 1                                                | NA       | 2022-10-24             |        2 |
| 101080 | 2    | Riverside        | 71.42 | 2023-05-03      | Corona                    | Female          | Black/African American/African | Data Not Collected | Non-Hispanic/Latin(A)(O)(X) | Divorced               | Straight/Heterosexual | English            | Data Not Collected | Data Not Collected | Data Not Collected | Data Not Collected                  | Data Not Collected          | NA                                 | Data Not Collected                          | Data Not Collected                                            | Data Not Collected             | Data Not Collected          | Data Not Collected               | Data Not Collected                 | Data Not Collected                                | 2023-05-03      | Corona              | No                       | No                           | Yes          | Unknown          | Yes                      |                   NA | NA           | NA           | Data Not Collected     | Other                    | NA                                | 2023-05-03           | 0                      | Temporary Housing    | NA                                                                                          | 2023-05-03           | 7470                   | Rent Payment         | NA                                | 2023-05-03           | 0                      | Housing Navigation   | NA                                | 2023-06-01           | 2450                   | No Intervention      | NA                                | NA                   | 0                      | No Intervention      | NA                                | NA                   | 0                      | NA                | Data Not Collected       | NA                                                 | NA                         | NA                                   | NA                               | NA                                            | NA                                                    | NA                            | NA                                      | NA                                  | NA                                               | NA       | 2023-05-03             |        2 |
| 101080 | 1    | Riverside        | 71.29 | 2023-03-16      | Corona                    | Female          | Black/African American/African | Data Not Collected | Non-Hispanic/Latin(A)(O)(X) | Divorced               | Straight/Heterosexual | English            | Data Not Collected | Data Not Collected | Yes                | Data Not Collected                  | Data Not Collected          | NA                                 | Data Not Collected                          | Data Not Collected                                            | Data Not Collected             | Data Not Collected          | Data Not Collected               | Data Not Collected                 | Data Not Collected                                | 2023-03-16      | Corona              | No                       | No                           | Yes          | Unknown          | Yes                      |                  210 | NA           | NA           | Data Not Collected     | Other                    | NA                                | 2023-03-17           | 0                      | Housing Navigation   | NA                                                                                          | 2023-03-17           | 0                      | Temporary Housing    | NA                                | 2023-03-17           | 0                      | No Intervention      | NA                                | NA                   | 0                      | No Intervention      | NA                                | NA                   | 0                      | No Intervention      | NA                                | NA                   | 0                      | 2023-04-10        | Homeless                 | NA                                                 | NA                         | Data Not Collected                   | NA                               | 0                                             | NA                                                    | Data Not Collected            | Data Not Collected                      | Data Not Collected                  | 0                                                | NA       | 2023-03-16             |        2 |

</div>

``` r
dat_subset <- dat %>% 
  group_by(id) %>% 
  arrange(id, desc(case_start_date_recode)) %>% 
  slice(1) %>% 
  ungroup()
```

Evaluate `100251` to double check that the correct one was selected.

``` r
dat_subset %>% 
  filter(id == 100251)
```

<div class="kable-table">

| id     | case | reporting_agency |   age | case_start_date | location_of_participation | gender_identity | race_1 | race_2 | ethnicity                   | current_marital_status | sexual_orientation    | preferred_language | veteran_status | medi_cal | medicare | representative_payee_or_conservator | living_situation_upon_entry | monthly_rent_mortgage_contribution | client_homeless_within_the_last_three_years | number_of_times_homelessness_occurred_in_the_last_three_years | total_duration_of_homelessness | last_period_of_homelessness | current_eviction_or_foreclosures | previous_evictions_or_foreclosures | discharge_from_institution_in_the_last_six_months | aps_report_date | aps_report_location | abuse_by_other_financial | abuse_by_other_non_financial | self_neglect | reporting_source | previous_aps_involvement | income_from_benefits | work_for_pay | other_income | client_referred_to_ces | intervention_1\_type | intervention_1\_other_explanation | intervention_1\_date | intervention_1\_amount | intervention_2\_type | intervention_2\_other_explanation                                                           | intervention_2\_date | intervention_2\_amount | intervention_3\_type | intervention_3\_other_explanation | intervention_3\_date | intervention_3\_amount | intervention_4\_type | intervention_4\_other_explanation | intervention_4\_date | intervention_4\_amount | intervention_5\_type | intervention_5\_other_explanation | intervention_5\_date | intervention_5\_amount | intervention_6\_type | intervention_6\_other_explanation | intervention_6\_date | intervention_6\_amount | case_closure_date | living_situation_at_exit | six_month_follow_up_living_situation_verified_date | six_month_follow_up_method | six_month_follow_up_living_situation | six_month_follow_up_homelessness | six_month_follow_up_substantiated_aps_reports | twelve_month_follow_up_living_situation_verified_date | twelve_month_follow_up_method | twelve_month_follow_up_living_situation | twelve_month_follow_up_homelessness | twelve_month_follow_up_substantiated_aps_reports | comments | case_start_date_recode | id_count |
|:-------|:-----|:-----------------|------:|:----------------|:--------------------------|:----------------|:-------|:-------|:----------------------------|:-----------------------|:----------------------|:-------------------|:---------------|:---------|:---------|:------------------------------------|:----------------------------|:-----------------------------------|:--------------------------------------------|:--------------------------------------------------------------|:-------------------------------|:----------------------------|:---------------------------------|:-----------------------------------|:--------------------------------------------------|:----------------|:--------------------|:-------------------------|:-----------------------------|:-------------|:-----------------|:-------------------------|---------------------:|:-------------|:-------------|:-----------------------|:---------------------|:----------------------------------|:---------------------|:-----------------------|:---------------------|:--------------------------------------------------------------------------------------------|:---------------------|:-----------------------|:---------------------|:----------------------------------|:---------------------|:-----------------------|:---------------------|:----------------------------------|:---------------------|:-----------------------|:---------------------|:----------------------------------|:---------------------|:-----------------------|:---------------------|:----------------------------------|:---------------------|:-----------------------|:------------------|:-------------------------|:---------------------------------------------------|:---------------------------|:-------------------------------------|:---------------------------------|:----------------------------------------------|:------------------------------------------------------|:------------------------------|:----------------------------------------|:------------------------------------|:-------------------------------------------------|:---------|:-----------------------|---------:|
| 100251 | 2    | Placer           | 62.99 | 2022-12-14      | Foresthill                | Female          | White  | White  | Non-Hispanic/Latin(A)(O)(X) | Widowed                | Straight/Heterosexual | English            | No             | Yes      | No       | Yes                                 | Owner Lives Alone           | 400                                | Yes                                         | Three Times                                                   | More Than A Year               | Within The Last Three Years | No                               | No                                 | Yes                                               | NA              | Foresthill          | No                       | No                           | Yes          | NA               | NA                       |                 1321 | 0            | 0            | No                     | Home Habitability    | NA                                | 2022-12-01           | 154.34                 | Other                | Paid for Her Birth Certificate. Client is on the EHV list with Roseville Housing Authority. | 2023-03-17           | 41.95                  | No Intervention      | NA                                | NA                   | 0                      | No Intervention      | NA                                | NA                   | 0                      | No Intervention      | NA                                | NA                   | 0                      | No Intervention      | NA                                | NA                   | 0                      | NA                | Rent Leaseholder         | NA                                                 | NA                         | NA                                   | NA                               | NA                                            | NA                                                    | NA                            | NA                                      | NA                                  | NA                                               | NA       | 2022-12-14             |        2 |

</div>

Looks good.

# Random sample

Group by agency and pull 15% of case numbers w/o replacement.

``` r
dat_subset %>% 
  count(reporting_agency) %>% 
  arrange(n) %>% 
  head()
```

<div class="kable-table">

| reporting_agency |   n |
|:-----------------|----:|
| Monterey         |   1 |
| El Dorado        |   2 |
| Sutter           |   2 |
| Calaveras        |   3 |
| Inyo             |   4 |
| Modoc            |   4 |

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

| variable         | value                                     | Full Sample   | Random Sample I |
|:-----------------|:------------------------------------------|:--------------|:----------------|
| ethnicity        | Client Doesn’t Know                       | 5 (6%)        | NA              |
| ethnicity        | Data Not Collected                        | 24 (27%)      | 4 (27%)         |
| ethnicity        | Hispanic/Latin(A)(O)(X)                   | 10 (11%)      | 2 (13%)         |
| ethnicity        | Non-Hispanic/Latin(A)(O)(X)               | 51 (57%)      | 9 (60%)         |
| race_1           | American Indian/Alaskan Native/Indigenous | 2 (2%)        | 1 (7%)          |
| race_1           | Asian/Asian American                      | 3 (3%)        | 1 (7%)          |
| race_1           | Client Doesn’t Know                       | 2 (2%)        | NA              |
| race_1           | Data Not Collected                        | 6 (7%)        | 2 (13%)         |
| race_1           | Other                                     | 1 (1%)        | 1 (7%)          |
| race_1           | White                                     | 76 (84%)      | 10 (67%)        |
| reporting_agency | Calaveras                                 | 3 (3%)        | 1 (7%)          |
| reporting_agency | Del Norte                                 | 9 (10%)       | 1 (7%)          |
| reporting_agency | El Dorado                                 | 2 (2%)        | 1 (7%)          |
| reporting_agency | Inyo                                      | 4 (4%)        | 1 (7%)          |
| reporting_agency | Lake                                      | 5 (6%)        | 1 (7%)          |
| reporting_agency | Mariposa                                  | 7 (8%)        | 1 (7%)          |
| reporting_agency | Modoc                                     | 4 (4%)        | 1 (7%)          |
| reporting_agency | Mono                                      | 9 (10%)       | 1 (7%)          |
| reporting_agency | Monterey                                  | 1 (1%)        | 1 (7%)          |
| reporting_agency | San Mateo                                 | 9 (10%)       | 1 (7%)          |
| reporting_agency | Siskiyou                                  | 7 (8%)        | 1 (7%)          |
| reporting_agency | Sutter                                    | 2 (2%)        | 1 (7%)          |
| reporting_agency | Tehama                                    | 10 (11%)      | 1 (7%)          |
| reporting_agency | Trinity                                   | 9 (10%)       | 1 (7%)          |
| reporting_agency | Yuba                                      | 9 (10%)       | 1 (7%)          |
| age              | NA                                        | 67.14 (12.34) | 67.14 (12.34)   |

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
| ethnicity        | American Indian/Alaskan Native            | 1 (0%)        | NA              |
| ethnicity        | Client Doesn’t Know                       | 208 (7%)      | 16 (5%)         |
| ethnicity        | Client Refused                            | 7 (0%)        | 1 (0%)          |
| ethnicity        | Cuban                                     | 3 (0%)        | NA              |
| ethnicity        | Data Not Collected                        | 338 (12%)     | 34 (11%)        |
| ethnicity        | Hispanic/Latin(A)(O)(X)                   | 443 (16%)     | 59 (20%)        |
| ethnicity        | Hispanic/Latino                           | 1 (0%)        | NA              |
| ethnicity        | Mexican/Chicano                           | 11 (0%)       | 1 (0%)          |
| ethnicity        | Non-Hispanic/Latin(A)(O)(X)               | 1497 (53%)    | 153 (51%)       |
| ethnicity        | Not Hispanic/Latino                       | 202 (7%)      | 24 (8%)         |
| ethnicity        | Other Hispanic/Latino                     | 52 (2%)       | 8 (3%)          |
| ethnicity        | Unknown                                   | 3 (0%)        | 1 (0%)          |
| ethnicity        | Unknown/Not Provided                      | 78 (3%)       | 5 (2%)          |
| ethnicity        | NA                                        | 4 (0%)        | NA              |
| race_1           | American Indian/Alaskan Native            | 3 (0%)        | NA              |
| race_1           | American Indian/Alaskan Native/Indigenous | 38 (1%)       | 5 (2%)          |
| race_1           | Asian                                     | 5 (0%)        | NA              |
| race_1           | Asian/Asian American                      | 68 (2%)       | 8 (3%)          |
| race_1           | Black/African American                    | 74 (3%)       | 8 (3%)          |
| race_1           | Black/African American/African            | 349 (12%)     | 30 (10%)        |
| race_1           | Client Doesn’t Know                       | 6 (0%)        | 1 (0%)          |
| race_1           | Client Refused                            | 26 (1%)       | 6 (2%)          |
| race_1           | Data Not Collected                        | 237 (8%)      | 22 (7%)         |
| race_1           | Native Hawaiian/Pacific Islander          | 21 (1%)       | 1 (0%)          |
| race_1           | Other                                     | 164 (6%)      | 26 (9%)         |
| race_1           | Unknown/Not Provided                      | 69 (2%)       | 4 (1%)          |
| race_1           | White                                     | 1786 (63%)    | 191 (63%)       |
| race_1           | Whte                                      | 1 (0%)        | NA              |
| race_1           | NA                                        | 1 (0%)        | NA              |
| reporting_agency | Alameda                                   | 97 (3%)       | 10 (3%)         |
| reporting_agency | Amador                                    | 14 (0%)       | 2 (1%)          |
| reporting_agency | Butte                                     | 50 (2%)       | 5 (2%)          |
| reporting_agency | Colusa                                    | 16 (1%)       | 2 (1%)          |
| reporting_agency | Contra Costa                              | 59 (2%)       | 6 (2%)          |
| reporting_agency | Fresno                                    | 63 (2%)       | 7 (2%)          |
| reporting_agency | Glenn                                     | 34 (1%)       | 4 (1%)          |
| reporting_agency | Humboldt                                  | 41 (1%)       | 5 (2%)          |
| reporting_agency | Kern                                      | 117 (4%)      | 12 (4%)         |
| reporting_agency | Kings                                     | 20 (1%)       | 2 (1%)          |
| reporting_agency | Los Angeles                               | 54 (2%)       | 6 (2%)          |
| reporting_agency | Madera                                    | 13 (0%)       | 2 (1%)          |
| reporting_agency | Marin                                     | 30 (1%)       | 3 (1%)          |
| reporting_agency | Mendocino                                 | 18 (1%)       | 2 (1%)          |
| reporting_agency | Merced                                    | 28 (1%)       | 3 (1%)          |
| reporting_agency | Napa                                      | 25 (1%)       | 3 (1%)          |
| reporting_agency | Nevada                                    | 50 (2%)       | 5 (2%)          |
| reporting_agency | Orange                                    | 158 (6%)      | 16 (5%)         |
| reporting_agency | Placer                                    | 22 (1%)       | 3 (1%)          |
| reporting_agency | Plumas                                    | 11 (0%)       | 2 (1%)          |
| reporting_agency | Riverside                                 | 936 (33%)     | 94 (31%)        |
| reporting_agency | Sacramento                                | 28 (1%)       | 3 (1%)          |
| reporting_agency | San Bernardino                            | 272 (10%)     | 28 (9%)         |
| reporting_agency | San Diego                                 | 120 (4%)      | 12 (4%)         |
| reporting_agency | San Francisco                             | 94 (3%)       | 10 (3%)         |
| reporting_agency | San Joaquin                               | 22 (1%)       | 3 (1%)          |
| reporting_agency | San Luis Obispo                           | 42 (1%)       | 5 (2%)          |
| reporting_agency | Santa Barbara                             | 23 (1%)       | 3 (1%)          |
| reporting_agency | Santa Clara                               | 58 (2%)       | 6 (2%)          |
| reporting_agency | Santa Cruz                                | 17 (1%)       | 2 (1%)          |
| reporting_agency | Shasta                                    | 23 (1%)       | 3 (1%)          |
| reporting_agency | Solano                                    | 16 (1%)       | 2 (1%)          |
| reporting_agency | Sonoma                                    | 83 (3%)       | 9 (3%)          |
| reporting_agency | Stanislaus                                | 66 (2%)       | 7 (2%)          |
| reporting_agency | Tulare                                    | 34 (1%)       | 4 (1%)          |
| reporting_agency | Tuolumne                                  | 13 (0%)       | 2 (1%)          |
| reporting_agency | Ventura                                   | 59 (2%)       | 6 (2%)          |
| reporting_agency | Yolo                                      | 22 (1%)       | 3 (1%)          |
| age              | NA                                        | 65.52 (56.82) | 66.56 (10.25)   |

</div>

# Total N sampled

``` r
random_sample_other_agencies %>% 
  nrow()
```

    ## [1] 302

``` r
random_sample_small_agencies_i %>% 
  nrow()
```

    ## [1] 15

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
| 100142 |   1 |
| 100156 |   1 |
| 100180 |   1 |
| 100221 |   1 |
| 100343 |   1 |
| 100443 |   1 |

</div>

Save the `id` and `case` selected in this iteration as a `tibble`

``` r
selected_id_case <- tibble(
  id_case = c(other_agencies, small_agencies_id)
)
```

# Export samples and stats

``` r
output <- list(random_sample_w_raw_data = raw_sample, 
               id_case_selected = selected_id_case,
               stats_small_agency = full_stats_small_agency, 
               stats_all_other_agency = full_stats_other_agency)

writexl::write_xlsx(output, paste(path_to_proj, "/Quantitative/data/random_sample_iteration_1/randomized_sample.xlsx", sep = ""))

#write.csv(dat, paste(path_to_proj, "/Quantitative/data/random_sample_iteration_1/og_file_used.csv", sep = ""))
```
