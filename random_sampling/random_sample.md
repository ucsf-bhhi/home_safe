Random sample
================
Sara Colom
2023-10-11

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

Raw data size.

``` r
raw %>% 
  dim()
```

    ## [1] 13087    74

Number of unique ID’s in the data.

``` r
raw %>% 
  count(id) %>% 
  nrow()
```

    ## [1] 10980

# Filters

- Option 1: Filter for cases with `case_start_date` on or after
  `10/01/2022` with at least ONE intervention **OR**
- Option 2: Has at least one intervention with date 10/1/22 or later
  (regardless of case start date or blank start date)

``` r
dim(raw)
```

    ## [1] 13087    74

``` r
dat <- raw %>% 
  mutate(case_start_date_recode = case_when(str_detect(case_start_date, ".427") ~ "2019-04-16",
                                            str_detect(case_start_date, ".527") ~ "2022-01-14",
                                            TRUE ~ case_start_date)) %>% 
  mutate(case_start_date_recode = lubridate::ymd(case_start_date_recode)) 

dat <- dat %>% 
  mutate(across(ends_with("_date") & starts_with("intervention"), lubridate::ymd))
```

    ## Warning: There were 5 warnings in `mutate()`.
    ## The first warning was:
    ## ℹ In argument: `across(ends_with("_date") & starts_with("intervention"),
    ##   lubridate::ymd)`.
    ## Caused by warning:
    ## !  268 failed to parse.
    ## ℹ Run `dplyr::last_dplyr_warnings()` to see the 4 remaining warnings.

``` r
op_1 <- dat %>% 
    filter(case_start_date_recode >= lubridate::ymd("2022-10-01"))

dim(op_1)
```

    ## [1] 3051   75

Filter those where ALL intervention types are either `NA` or state
`No Intervention`

``` r
interv <- op_1 %>% 
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
dim(op_1)
```

    ## [1] 3051   75

``` r
op_1 <- op_1 %>% 
  mutate(id_case = paste(id, case, sep = "_")) %>% 
  filter(!id_case %in% exclude_these)

dim(op_1)
```

    ## [1] 2421   76

Now–let’s exclude those selected already from the `op_1` pool to run our
`op_2` pool, then apply the filters as above, and lastly combine them
for sampling.

- Option 2: Has at least one intervention with date 10/1/22 or later
  (regardless of case start date or blank start date)

``` r
op_2 <- dat %>% 
  anti_join(op_1)
```

    ## Joining with `by = join_by(id, case, reporting_agency, age, case_start_date,
    ## location_of_participation, gender_identity, race_1, race_2, ethnicity,
    ## current_marital_status, sexual_orientation, preferred_language, veteran_status,
    ## medi_cal, medicare, representative_payee_or_conservator,
    ## living_situation_upon_entry, monthly_rent_mortgage_contribution,
    ## client_homeless_within_the_last_three_years,
    ## number_of_times_homelessness_occurred_in_the_last_three_years,
    ## total_duration_of_homelessness, last_period_of_homelessness,
    ## current_eviction_or_foreclosures, previous_evictions_or_foreclosures,
    ## discharge_from_institution_in_the_last_six_months, aps_report_date,
    ## aps_report_location, abuse_by_other_financial, abuse_by_other_non_financial,
    ## self_neglect, reporting_source, previous_aps_involvement, income_from_benefits,
    ## work_for_pay, other_income, client_referred_to_ces, intervention_1_type,
    ## intervention_1_other_explanation, intervention_1_date, intervention_1_amount,
    ## intervention_2_type, intervention_2_other_explanation, intervention_2_date,
    ## intervention_2_amount, intervention_3_type, intervention_3_other_explanation,
    ## intervention_3_date, intervention_3_amount, intervention_4_type,
    ## intervention_4_other_explanation, intervention_4_date, intervention_4_amount,
    ## intervention_5_type, intervention_5_other_explanation, intervention_5_date,
    ## intervention_5_amount, intervention_6_type, intervention_6_other_explanation,
    ## intervention_6_date, intervention_6_amount, case_closure_date,
    ## living_situation_at_exit, six_month_follow_up_living_situation_verified_date,
    ## six_month_follow_up_method, six_month_follow_up_living_situation,
    ## six_month_follow_up_homelessness,
    ## six_month_follow_up_substantiated_aps_reports,
    ## twelve_month_follow_up_living_situation_verified_date,
    ## twelve_month_follow_up_method, twelve_month_follow_up_living_situation,
    ## twelve_month_follow_up_homelessness,
    ## twelve_month_follow_up_substantiated_aps_reports, comments,
    ## case_start_date_recode)`

``` r
op_2_inver_dates <- op_2 %>%
  select(id, starts_with("intervention") & ends_with("date")) %>% 
  pivot_longer(starts_with("intervention") & ends_with("date"), names_to = "intervention", values_to = "date")

op_2_inver_dates %>% 
  head()
```

<div class="kable-table">

| id     | intervention         | date       |
|:-------|:---------------------|:-----------|
| 110343 | intervention_1\_date | 2021-02-24 |
| 110343 | intervention_2\_date | NA         |
| 110343 | intervention_3\_date | NA         |
| 110343 | intervention_4\_date | NA         |
| 110343 | intervention_5\_date | NA         |
| 110343 | intervention_6\_date | NA         |

</div>

``` r
op_2_inver_dates <- op_2_inver_dates %>% 
  filter(date >= lubridate::ymd("2022-10-01") & date <= lubridate::ymd("2023-10-01"))
```

``` r
summary(op_2_inver_dates$date)
```

    ##         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
    ## "2022-10-01" "2022-10-27" "2022-12-06" "2022-12-27" "2023-03-01" "2023-09-29"

``` r
include_these <- op_2_inver_dates %>% 
  select(id) %>% 
  unique() %>% 
  pull(id)
```

``` r
op_2 <- dat %>% 
  filter(id %in% include_these)
```

Combine `id`s from option 1 and 2.

``` r
sample_subset <- op_1  %>% 
  bind_rows(op_2)
```

Percent of those from original data who made the cut-off.

``` r
nrow(sample_subset)/nrow(raw)
```

    ## [1] 0.2521586

# Data cleaning

Do any `ids` occur more than once?

``` r
sample_subset <- sample_subset %>% 
  add_count(id, name = "id_count")

sample_subset %>% 
  count(id) %>% 
  filter(n > 1) %>% 
  nrow()
```

    ## [1] 205

It appears there are 205 `id`’s with more than once case.

In order to limit the sampling to the most **recent** case per ID, I
will first, `group_by` `id`, then sort descending by `case_start_date`,
and finally `slice` for the first occurrence (i.e. most recent case by
`id`).

``` r
# Quick sanity check before running below

sample_subset %>% 
    filter(id_count > 1) %>% 
    group_by(id) %>% 
    arrange(id, desc(case_start_date_recode)) %>% 
    head()
```

<div class="kable-table">

| id     | case | reporting_agency |   age | case_start_date | location_of_participation | gender_identity | race_1                           | race_2             | ethnicity                   | current_marital_status | sexual_orientation    | preferred_language | veteran_status     | medi_cal           | medicare           | representative_payee_or_conservator | living_situation_upon_entry | monthly_rent_mortgage_contribution | client_homeless_within_the_last_three_years | number_of_times_homelessness_occurred_in_the_last_three_years | total_duration_of_homelessness | last_period_of_homelessness | current_eviction_or_foreclosures | previous_evictions_or_foreclosures | discharge_from_institution_in_the_last_six_months | aps_report_date | aps_report_location | abuse_by_other_financial | abuse_by_other_non_financial | self_neglect | reporting_source       | previous_aps_involvement | income_from_benefits | work_for_pay | other_income | client_referred_to_ces | intervention_1\_type     | intervention_1\_other_explanation | intervention_1\_date | intervention_1\_amount | intervention_2\_type     | intervention_2\_other_explanation | intervention_2\_date | intervention_2\_amount | intervention_3\_type     | intervention_3\_other_explanation | intervention_3\_date | intervention_3\_amount | intervention_4\_type | intervention_4\_other_explanation | intervention_4\_date | intervention_4\_amount | intervention_5\_type | intervention_5\_other_explanation | intervention_5\_date | intervention_5\_amount | intervention_6\_type | intervention_6\_other_explanation | intervention_6\_date | intervention_6\_amount | case_closure_date | living_situation_at_exit | six_month_follow_up_living_situation_verified_date | six_month_follow_up_method | six_month_follow_up_living_situation | six_month_follow_up_homelessness | six_month_follow_up_substantiated_aps_reports | twelve_month_follow_up_living_situation_verified_date | twelve_month_follow_up_method | twelve_month_follow_up_living_situation | twelve_month_follow_up_homelessness | twelve_month_follow_up_substantiated_aps_reports | comments | case_start_date_recode | id_case  | id_count |
|:-------|:-----|:-----------------|------:|:----------------|:--------------------------|:----------------|:---------------------------------|:-------------------|:----------------------------|:-----------------------|:----------------------|:-------------------|:-------------------|:-------------------|:-------------------|:------------------------------------|:----------------------------|:-----------------------------------|:--------------------------------------------|:--------------------------------------------------------------|:-------------------------------|:----------------------------|:---------------------------------|:-----------------------------------|:--------------------------------------------------|:----------------|:--------------------|:-------------------------|:-----------------------------|:-------------|:-----------------------|:-------------------------|---------------------:|:-------------|:-------------|:-----------------------|:-------------------------|:----------------------------------|:---------------------|:-----------------------|:-------------------------|:----------------------------------|:---------------------|:-----------------------|:-------------------------|:----------------------------------|:---------------------|:-----------------------|:---------------------|:----------------------------------|:---------------------|:-----------------------|:---------------------|:----------------------------------|:---------------------|:-----------------------|:---------------------|:----------------------------------|:---------------------|:-----------------------|:------------------|:-------------------------|:---------------------------------------------------|:---------------------------|:-------------------------------------|:---------------------------------|:----------------------------------------------|:------------------------------------------------------|:------------------------------|:----------------------------------------|:------------------------------------|:-------------------------------------------------|:---------|:-----------------------|:---------|---------:|
| 100149 | 2    | Riverside        | 75.38 | 2022-06-23      | Temecula                  | Male            | White                            | Data Not Collected | Non-Hispanic/Latin(A)(O)(X) | Never Married          | Straight/Heterosexual | English            | Data Not Collected | Data Not Collected | Data Not Collected | Data Not Collected                  | Homeless                    | NA                                 | Data Not Collected                          | Data Not Collected                                            | Data Not Collected             | Currently Homeless          | No                               | No                                 | No                                                | 2022-06-23      | Temecula            | No                       | No                           | Yes          | No Relationship        | Yes                      |                 1359 | NA           | NA           | No                     | Temporary Housing        | NA                                | 2022-06-29           | 0                      | Enhanced Case Management | NA                                | 2022-07-29           | 0                      | Other                    | NA                                | 2022-12-08           | 0                      | Housing Navigation   | NA                                | 2023-01-05           | 0                      | No Intervention      | NA                                | NA                   | 0                      | No Intervention      | NA                                | NA                   | 0                      | 2023-03-02        | Homeless                 | NA                                                 | NA                         | Data Not Collected                   | NA                               | 0                                             | NA                                                    | Data Not Collected            | Data Not Collected                      | Data Not Collected                  | 0                                                | NA       | 2022-06-23             | NA       |        2 |
| 100149 | 1    | Riverside        | 73.23 | 2020-04-28      | Temecula                  | Male            | White                            | Data Not Collected | Non-Hispanic/Latin(A)(O)(X) | Never Married          | Straight/Heterosexual | English            | Data Not Collected | Data Not Collected | Data Not Collected | Data Not Collected                  | Data Not Collected          | NA                                 | Data Not Collected                          | Data Not Collected                                            | Data Not Collected             | Data Not Collected          | Data Not Collected               | Data Not Collected                 | Data Not Collected                                | 2020-04-28      | Temecula            | No                       | No                           | Yes          | Social Worker          | Yes                      |                   NA | NA           | NA           | Data Not Collected     | Emergency Shelter        | NA                                | 2020-04-30           | 0                      | Other                    | NA                                | 2020-04-30           | 0                      | Enhanced Case Management | NA                                | 2020-05-18           | 0                      | Housing Navigation   | NA                                | 2020-08-21           | 0                      | No Intervention      | NA                                | NA                   | 0                      | No Intervention      | NA                                | NA                   | 0                      | 2021-12-13        | Homeless                 | NA                                                 | NA                         | Data Not Collected                   | NA                               | 0                                             | 2023-03-09                                            | Aps System                    | Homeless                                | Yes                                 | 0                                                | NA       | 2020-04-28             | NA       |        2 |
| 100315 | 3    | Riverside        | 65.37 | 2023-01-11      | Riverside                 | Female          | White                            | Data Not Collected | Non-Hispanic/Latin(A)(O)(X) | Divorced               | Client Doesn’t Know   | English            | Data Not Collected | Data Not Collected | Data Not Collected | Data Not Collected                  | Homeless                    | 0                                  | Data Not Collected                          | Data Not Collected                                            | Data Not Collected             | Currently Homeless          | No                               | No                                 | No                                                | 2023-01-11      | Riverside           | No                       | No                           | Yes          | Self                   | Yes                      |                 1308 | NA           | NA           | No                     | Enhanced Case Management | NA                                | 2023-01-11           | 0                      | Emergency Shelter        | NA                                | 2023-01-23           | 1190                   | Security Deposit         | NA                                | 2023-02-03           | 644                    | No Intervention      | NA                                | NA                   | 0                      | No Intervention      | NA                                | NA                   | 0                      | No Intervention      | NA                                | NA                   | 0                      | 2023-03-20        | Rent Leaseholder         | NA                                                 | NA                         | Data Not Collected                   | NA                               | 0                                             | NA                                                    | Data Not Collected            | Data Not Collected                      | Data Not Collected                  | 0                                                | NA       | 2023-01-11             | 100315_3 |        2 |
| 100315 | 2    | Riverside        | 65.15 | 2022-10-24      | Riverside                 | Female          | White                            | Data Not Collected | Non-Hispanic/Latin(A)(O)(X) | Divorced               | Client Doesn’t Know   | English            | Data Not Collected | Data Not Collected | Data Not Collected | Data Not Collected                  | Temporary Housing           | NA                                 | Data Not Collected                          | Data Not Collected                                            | Data Not Collected             | Within The Last Month       | No                               | No                                 | No                                                | 2022-10-24      | Riverside           | No                       | No                           | Yes          | Self                   | Yes                      |                 1258 | NA           | NA           | No                     | Enhanced Case Management | NA                                | 2022-11-01           | 0                      | Emergency Shelter        | NA                                | 2022-11-01           | 0                      | No Intervention          | NA                                | NA                   | 0                      | No Intervention      | NA                                | NA                   | 0                      | No Intervention      | NA                                | NA                   | 0                      | No Intervention      | NA                                | NA                   | 0                      | 2022-11-17        | Temporary Housing        | NA                                                 | NA                         | Data Not Collected                   | NA                               | 1                                             | NA                                                    | Data Not Collected            | Data Not Collected                      | Data Not Collected                  | 1                                                | NA       | 2022-10-24             | 100315_2 |        2 |
| 100343 | 2    | Stanislaus       | 64.19 | 2022-11-03      | Turlock                   | Female          | Native Hawaiian/Pacific Islander | Data Not Collected | Non-Hispanic/Latin(A)(O)(X) | Data Not Collected     | Straight/Heterosexual | English            | Data Not Collected | Data Not Collected | Data Not Collected | Data Not Collected                  | Data Not Collected          | 0.99                               | Data Not Collected                          | Data Not Collected                                            | Data Not Collected             | Data Not Collected          | Data Not Collected               | Data Not Collected                 | Data Not Collected                                | 2022-07-12      | Turlock             | No                       | No                           | Yes          | Community Professional | No                       |                    0 | 0            | 0            | Data Not Collected     | Enhanced Case Management | NA                                | NA                   | NA                     | Housing Navigation       | NA                                | NA                   | NA                     | NA                       | NA                                | NA                   | NA                     | NA                   | NA                                | NA                   | NA                     | NA                   | NA                                | NA                   | NA                     | NA                   | NA                                | NA                   | NA                     | NA                | NA                       | NA                                                 | NA                         | NA                                   | NA                               | NA                                            | NA                                                    | NA                            | NA                                      | NA                                  | NA                                               | NA       | 2022-11-03             | 100343_2 |        3 |
| 100343 | 2    | Stanislaus       | 64.19 | 2022-11-03      | Turlock                   | Female          | Native Hawaiian/Pacific Islander | Data Not Collected | Non-Hispanic/Latin(A)(O)(X) | Data Not Collected     | Straight/Heterosexual | English            | Data Not Collected | Data Not Collected | Data Not Collected | Data Not Collected                  | Data Not Collected          | 0.99                               | Data Not Collected                          | Data Not Collected                                            | Data Not Collected             | Data Not Collected          | Data Not Collected               | Data Not Collected                 | Data Not Collected                                | 2022-07-12      | Turlock             | No                       | No                           | Yes          | Community Professional | No                       |                    0 | 0            | 0            | Data Not Collected     | Enhanced Case Management | NA                                | NA                   | NA                     | Housing Navigation       | NA                                | NA                   | NA                     | NA                       | NA                                | NA                   | NA                     | NA                   | NA                                | NA                   | NA                     | NA                   | NA                                | NA                   | NA                     | NA                   | NA                                | NA                   | NA                     | NA                | NA                       | NA                                                 | NA                         | NA                                   | NA                               | NA                                            | NA                                                    | NA                            | NA                                      | NA                                  | NA                                               | NA       | 2022-11-03             | NA       |        3 |

</div>

``` r
sample_subset <- sample_subset %>% 
  group_by(id) %>% 
  arrange(id, desc(case_start_date_recode)) %>% 
  slice(1) %>% 
  ungroup()
```

Evaluate `100315` to double check that the correct one was selected.

``` r
sample_subset %>% 
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
sample_subset %>% 
  count(reporting_agency) %>% 
  arrange(n) %>% 
  head()
```

<div class="kable-table">

| reporting_agency |   n |
|:-----------------|----:|
| El Dorado        |   1 |
| Tehama           |   1 |
| Trinity          |   1 |
| Sutter           |   2 |
| Calaveras        |   3 |
| Modoc            |   3 |

</div>

Some reporting agencies have less than 10 cases.

Assign agency as a `small_agency` (10 cases or less) or `not_small`
(more than 10 cases)

``` r
sample_subset <- sample_subset %>% 
  add_count(reporting_agency) %>% 
  mutate(`agency_size` = if_else(n <= 10, "small_agency", "not_small"))

small_agencies <- sample_subset %>% 
  filter(agency_size == "small_agency") %>% 
  droplevels()

all_other_agencies <- sample_subset %>% 
  anti_join(small_agencies) %>% 
  droplevels()
```

# Alternative I.

If N \< 10 capture
`10%  across all SMALL agencies or just capture 1 per agency  Anything more we capture`15%\`

``` r
set.seed(123)

random_sample_small_agencies_i <- small_agencies %>% 
  group_by(reporting_agency) %>% 
  sample_n(1)
```

# Sample all other agencies

``` r
all_other_agencies <- all_other_agencies %>% 
  mutate(sample_n = ceiling(n*0.15))

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
| ethnicity        | Client Doesn’t Know                       | 1 (2%)       | NA              |
| ethnicity        | Data Not Collected                        | 7 (16%)      | 3 (30%)         |
| ethnicity        | Hispanic/Latin(A)(O)(X)                   | 7 (16%)      | 1 (10%)         |
| ethnicity        | Non-Hispanic/Latin(A)(O)(X)               | 28 (65%)     | 6 (60%)         |
| race_1           | American Indian/Alaskan Native/Indigenous | 2 (5%)       | NA              |
| race_1           | Asian/Asian American                      | 2 (5%)       | 1 (10%)         |
| race_1           | Black/African American/African            | 1 (2%)       | NA              |
| race_1           | Client Doesn’t Know                       | 2 (5%)       | NA              |
| race_1           | Data Not Collected                        | 1 (2%)       | NA              |
| race_1           | White                                     | 35 (81%)     | 9 (90%)         |
| reporting_agency | Calaveras                                 | 3 (7%)       | 1 (10%)         |
| reporting_agency | Del Norte                                 | 9 (21%)      | 1 (10%)         |
| reporting_agency | El Dorado                                 | 1 (2%)       | 1 (10%)         |
| reporting_agency | Inyo                                      | 5 (12%)      | 1 (10%)         |
| reporting_agency | Modoc                                     | 3 (7%)       | 1 (10%)         |
| reporting_agency | San Mateo                                 | 10 (23%)     | 1 (10%)         |
| reporting_agency | Siskiyou                                  | 8 (19%)      | 1 (10%)         |
| reporting_agency | Sutter                                    | 2 (5%)       | 1 (10%)         |
| reporting_agency | Tehama                                    | 1 (2%)       | 1 (10%)         |
| reporting_agency | Trinity                                   | 1 (2%)       | 1 (10%)         |
| age              | NA                                        | 68.06 (9.07) | 68.06 (9.07)    |

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
| ethnicity        | Client Doesn’t Know                       | 219 (7%)      | 31 (7%)         |
| ethnicity        | Client Refused                            | 2 (0%)        | 1 (0%)          |
| ethnicity        | Cuban                                     | 2 (0%)        | 1 (0%)          |
| ethnicity        | Data Not Collected                        | 253 (8%)      | 47 (10%)        |
| ethnicity        | Hispanic/Latin(A)(O)(X)                   | 431 (14%)     | 70 (15%)        |
| ethnicity        | Hispanic/Latino                           | 1 (0%)        | 1 (0%)          |
| ethnicity        | Mexican/Chicano                           | 11 (0%)       | 4 (1%)          |
| ethnicity        | Non-Hispanic/Latin(A)(O)(X)               | 1490 (50%)    | 230 (49%)       |
| ethnicity        | Not Hispanic/Latino                       | 310 (10%)     | 47 (10%)        |
| ethnicity        | Other Hispanic/Latino                     | 96 (3%)       | 14 (3%)         |
| ethnicity        | Unknown                                   | 2 (0%)        | NA              |
| ethnicity        | Unknown/Not Provided                      | 160 (5%)      | 22 (5%)         |
| ethnicity        | NA                                        | 7 (0%)        | 1 (0%)          |
| race_1           | American Indian/Alaskan Native            | 8 (0%)        | 2 (0%)          |
| race_1           | American Indian/Alaskan Native/Indigenous | 42 (1%)       | 4 (1%)          |
| race_1           | Asian                                     | 12 (0%)       | NA              |
| race_1           | Asian/Asian American                      | 63 (2%)       | 12 (3%)         |
| race_1           | Black/African American                    | 98 (3%)       | 19 (4%)         |
| race_1           | Black/African American/African            | 364 (12%)     | 60 (13%)        |
| race_1           | Client Doesn’t Know                       | 5 (0%)        | 1 (0%)          |
| race_1           | Client Refused                            | 24 (1%)       | 1 (0%)          |
| race_1           | Data Not Collected                        | 177 (6%)      | 37 (8%)         |
| race_1           | Native Hawaiian/Pacific Islander          | 16 (1%)       | 6 (1%)          |
| race_1           | Other                                     | 192 (6%)      | 33 (7%)         |
| race_1           | Unknown/Not Provided                      | 136 (5%)      | 20 (4%)         |
| race_1           | White                                     | 1845 (62%)    | 273 (58%)       |
| race_1           | NA                                        | 3 (0%)        | 1 (0%)          |
| reporting_agency | Alameda                                   | 107 (4%)      | 17 (4%)         |
| reporting_agency | Amador                                    | 14 (0%)       | 3 (1%)          |
| reporting_agency | Butte                                     | 55 (2%)       | 9 (2%)          |
| reporting_agency | Colusa                                    | 17 (1%)       | 3 (1%)          |
| reporting_agency | Contra Costa                              | 38 (1%)       | 6 (1%)          |
| reporting_agency | Fresno                                    | 24 (1%)       | 4 (1%)          |
| reporting_agency | Glenn                                     | 26 (1%)       | 4 (1%)          |
| reporting_agency | Humboldt                                  | 35 (1%)       | 6 (1%)          |
| reporting_agency | Kern                                      | 135 (5%)      | 21 (4%)         |
| reporting_agency | Kings                                     | 17 (1%)       | 3 (1%)          |
| reporting_agency | Los Angeles                               | 172 (6%)      | 26 (6%)         |
| reporting_agency | Madera                                    | 14 (0%)       | 3 (1%)          |
| reporting_agency | Marin                                     | 18 (1%)       | 3 (1%)          |
| reporting_agency | Mendocino                                 | 18 (1%)       | 3 (1%)          |
| reporting_agency | Merced                                    | 35 (1%)       | 6 (1%)          |
| reporting_agency | Mono                                      | 14 (0%)       | 3 (1%)          |
| reporting_agency | Napa                                      | 26 (1%)       | 4 (1%)          |
| reporting_agency | Nevada                                    | 42 (1%)       | 7 (1%)          |
| reporting_agency | Orange                                    | 149 (5%)      | 23 (5%)         |
| reporting_agency | Placer                                    | 24 (1%)       | 4 (1%)          |
| reporting_agency | Plumas                                    | 11 (0%)       | 2 (0%)          |
| reporting_agency | Riverside                                 | 916 (31%)     | 138 (29%)       |
| reporting_agency | Sacramento                                | 28 (1%)       | 5 (1%)          |
| reporting_agency | San Bernardino                            | 372 (12%)     | 56 (12%)        |
| reporting_agency | San Diego                                 | 82 (3%)       | 13 (3%)         |
| reporting_agency | San Francisco                             | 110 (4%)      | 17 (4%)         |
| reporting_agency | San Joaquin                               | 30 (1%)       | 5 (1%)          |
| reporting_agency | San Luis Obispo                           | 44 (1%)       | 7 (1%)          |
| reporting_agency | Santa Barbara                             | 22 (1%)       | 4 (1%)          |
| reporting_agency | Santa Clara                               | 67 (2%)       | 11 (2%)         |
| reporting_agency | Santa Cruz                                | 11 (0%)       | 2 (0%)          |
| reporting_agency | Shasta                                    | 25 (1%)       | 4 (1%)          |
| reporting_agency | Solano                                    | 11 (0%)       | 2 (0%)          |
| reporting_agency | Sonoma                                    | 84 (3%)       | 13 (3%)         |
| reporting_agency | Stanislaus                                | 60 (2%)       | 9 (2%)          |
| reporting_agency | Tulare                                    | 17 (1%)       | 3 (1%)          |
| reporting_agency | Tuolumne                                  | 14 (0%)       | 3 (1%)          |
| reporting_agency | Ventura                                   | 67 (2%)       | 11 (2%)         |
| reporting_agency | Yolo                                      | 22 (1%)       | 4 (1%)          |
| reporting_agency | Yuba                                      | 12 (0%)       | 2 (0%)          |
| age              | NA                                        | 65.86 (55.65) | 60.45 (138.07)  |

</div>

# Total N sampled

``` r
random_sample_other_agencies %>% 
  nrow()
```

    ## [1] 469

``` r
random_sample_small_agencies_i %>% 
  nrow()
```

    ## [1] 10

Now–I will use the combination of `id` and `case` number to subset the
first random sample from the `raw` (original) sample_subseta.

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
| 100034 |   1 |
| 100036 |   1 |
| 100376 |   1 |
| 100543 |   1 |
| 100615 |   1 |
| 100617 |   1 |

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

writexl::write_xlsx(output, "stats_randomized_sample_v2.xlsx")

write.csv(raw_sample, "randomized_sample_v2.csv", row.names = F)

write.csv(raw, "og_file_used_v2.csv")
```
