Random sample
================
Sara Colom
2023-09-08

# Read libraries and data

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 4.2.3

``` r
library(tidyr)
```

    ## Warning: package 'tidyr' was built under R version 4.2.3

``` r
library(stringr)
```

    ## Warning: package 'stringr' was built under R version 4.2.3

``` r
path_to_proj <- Sys.getenv("HOME_SAFE")

dat <- readxl::read_excel(paste(path_to_proj, "/Quantitative/data/report_1_data/home_safe_data_upload_090623.xlsx", sep = "")) %>% 
  janitor::clean_names()
```

# Random sample

Group by agency and pull 15% of case numbers w/o replacement.

``` r
dat %>% 
  count(reporting_agency) %>% 
  arrange(n) %>% 
  head()
```

<div class="kable-table">

| reporting_agency |   n |
|:-----------------|----:|
| Monterey         |   1 |
| El Dorado        |   2 |
| Calaveras        |   3 |
| Modoc            |   4 |
| Inyo             |   6 |
| Lake             |   6 |

</div>

Some reporting agencies have less than 10 cases.

Assign agency as a `small_agency` (10 cases or less) or `not_small`
(more than 10 cases)

``` r
dat <- dat %>% 
  add_count(reporting_agency) %>% 
  mutate(`agency_size` = if_else(n <= 10, "small_agency", "not_small"))

small_agencies <- dat %>% 
  filter(agency_size == "small_agency") %>% 
  droplevels()

all_other_agencies <- dat %>% 
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

# Alternative II

We pool all those with 10 or less cases as one giant ‘small agency’
pool, and select 10%, then for those \> 10 cases randomly pool 10%.

``` r
sample_n <- nrow(small_agencies)*.1 %>% 
  round(1)

set.seed(123)

random_sample_small_agencies_ii <- small_agencies %>% 
  sample_n(sample_n)
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
dist_small_ii <- random_sample_small_agencies_ii %>% 
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
  mutate(sample = "Random Sample II") %>% 
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
  bind_rows(dist_small_ii) %>% 
  pivot_wider(names_from = sample, values_from = statistic)

full_stats_small_agency
```

<div class="kable-table">

| variable         | value                                     | Full Sample   | Random Sample I | Random Sample II |
|:-----------------|:------------------------------------------|:--------------|:----------------|:-----------------|
| ethnicity        | Client Doesn’t Know                       | 1 (3%)        | NA              | NA               |
| ethnicity        | Data Not Collected                        | 8 (20%)       | 1 (12%)         | 1 (25%)          |
| ethnicity        | Hispanic/Latin(A)(O)(X)                   | 4 (10%)       | NA              | NA               |
| ethnicity        | Non-Hispanic/Latin(A)(O)(X)               | 27 (68%)      | 7 (88%)         | 3 (75%)          |
| race_1           | American Indian/Alaskan Native/Indigenous | 3 (7%)        | NA              | NA               |
| race_1           | Other                                     | 2 (5%)        | 1 (12%)         | NA               |
| race_1           | White                                     | 35 (88%)      | 7 (88%)         | 4 (100%)         |
| reporting_agency | Calaveras                                 | 3 (7%)        | 1 (12%)         | 1 (25%)          |
| reporting_agency | Del Norte                                 | 10 (25%)      | 1 (12%)         | NA               |
| reporting_agency | El Dorado                                 | 2 (5%)        | 1 (12%)         | 2 (50%)          |
| reporting_agency | Inyo                                      | 6 (15%)       | 1 (12%)         | NA               |
| reporting_agency | Lake                                      | 6 (15%)       | 1 (12%)         | NA               |
| reporting_agency | Modoc                                     | 4 (10%)       | 1 (12%)         | 1 (25%)          |
| reporting_agency | Monterey                                  | 1 (3%)        | 1 (12%)         | NA               |
| reporting_agency | Sutter                                    | 8 (20%)       | 1 (12%)         | NA               |
| age              | NA                                        | 67.48 (10.98) | 67.48 (10.98)   | 67.48 (10.98)    |

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

| variable         | value                                     | Full Sample  | Random Sample I |
|:-----------------|:------------------------------------------|:-------------|:----------------|
| ethnicity        | 2                                         | 1 (0%)       | 1 (0%)          |
| ethnicity        | 6                                         | 1 (0%)       | NA              |
| ethnicity        | American Indian/Alaskan Native            | 1 (0%)       | NA              |
| ethnicity        | Client Doesn’t Know                       | 907 (7%)     | 99 (7%)         |
| ethnicity        | Client Refused                            | 11 (0%)      | 1 (0%)          |
| ethnicity        | Cuban                                     | 4 (0%)       | NA              |
| ethnicity        | Data Not Collected                        | 1254 (10%)   | 129 (10%)       |
| ethnicity        | Hispanic/Latin(A)(O)(X)                   | 1502 (12%)   | 152 (11%)       |
| ethnicity        | Hispanic/Latino                           | 1 (0%)       | NA              |
| ethnicity        | Hispanic/Latino/A/Spanish origin          | 6 (0%)       | 1 (0%)          |
| ethnicity        | Mexican/Chicano                           | 130 (1%)     | 19 (1%)         |
| ethnicity        | Mexican/Mexican American/Chicano          | 3 (0%)       | NA              |
| ethnicity        | Non-Hispanic/Latin(A)(O)(X)               | 5223 (40%)   | 543 (41%)       |
| ethnicity        | Not Hispanic/Latino                       | 2066 (16%)   | 184 (14%)       |
| ethnicity        | Other Hispanic/Latin                      | 1 (0%)       | NA              |
| ethnicity        | Other Hispanic/Latino                     | 588 (5%)     | 67 (5%)         |
| ethnicity        | Puerto Rican                              | 5 (0%)       | NA              |
| ethnicity        | Unknown                                   | 3 (0%)       | NA              |
| ethnicity        | Unknown/Not Provided                      | 1248 (10%)   | 124 (9%)        |
| ethnicity        | NA                                        | 92 (1%)      | 6 (0%)          |
| race_1           | 0                                         | 2 (0%)       | 1 (0%)          |
| race_1           | American Indian/Alaskan Native            | 33 (0%)      | 3 (0%)          |
| race_1           | American Indian/Alaskan Native/Indigenous | 107 (1%)     | 15 (1%)         |
| race_1           | Asian                                     | 82 (1%)      | 5 (0%)          |
| race_1           | Asian/Asian American                      | 217 (2%)     | 20 (2%)         |
| race_1           | Black/African American                    | 531 (4%)     | 41 (3%)         |
| race_1           | Black/African American/African            | 1232 (9%)    | 137 (10%)       |
| race_1           | Client Doesn’t Know                       | 42 (0%)      | 6 (0%)          |
| race_1           | Client Refused                            | 55 (0%)      | 6 (0%)          |
| race_1           | Data Not Collected                        | 713 (5%)     | 66 (5%)         |
| race_1           | Native Hawaiian/Pacific Islander          | 43 (0%)      | 6 (0%)          |
| race_1           | Other                                     | 1110 (9%)    | 120 (9%)        |
| race_1           | Pacific Islander/Native Hawaiian          | 36 (0%)      | 5 (0%)          |
| race_1           | Unknown/Not Provided                      | 1127 (9%)    | 113 (9%)        |
| race_1           | White                                     | 7665 (59%)   | 779 (59%)       |
| race_1           | Whte                                      | 1 (0%)       | NA              |
| race_1           | NA                                        | 51 (0%)      | 3 (0%)          |
| reporting_agency | Alameda                                   | 306 (2%)     | 31 (2%)         |
| reporting_agency | Amador                                    | 25 (0%)      | 3 (0%)          |
| reporting_agency | Butte                                     | 128 (1%)     | 13 (1%)         |
| reporting_agency | Colusa                                    | 24 (0%)      | 3 (0%)          |
| reporting_agency | Contra Costa                              | 370 (3%)     | 37 (3%)         |
| reporting_agency | Fresno                                    | 172 (1%)     | 18 (1%)         |
| reporting_agency | Glenn                                     | 39 (0%)      | 4 (0%)          |
| reporting_agency | Humboldt                                  | 222 (2%)     | 23 (2%)         |
| reporting_agency | Kern                                      | 1009 (8%)    | 101 (8%)        |
| reporting_agency | Kings                                     | 210 (2%)     | 21 (2%)         |
| reporting_agency | Los Angeles                               | 1486 (11%)   | 149 (11%)       |
| reporting_agency | Madera                                    | 62 (0%)      | 7 (1%)          |
| reporting_agency | Marin                                     | 43 (0%)      | 5 (0%)          |
| reporting_agency | Mariposa                                  | 81 (1%)      | 9 (1%)          |
| reporting_agency | Mendocino                                 | 109 (1%)     | 11 (1%)         |
| reporting_agency | Merced                                    | 201 (2%)     | 21 (2%)         |
| reporting_agency | Mono                                      | 25 (0%)      | 3 (0%)          |
| reporting_agency | Napa                                      | 33 (0%)      | 4 (0%)          |
| reporting_agency | Nevada                                    | 236 (2%)     | 24 (2%)         |
| reporting_agency | Orange                                    | 268 (2%)     | 27 (2%)         |
| reporting_agency | Placer                                    | 178 (1%)     | 18 (1%)         |
| reporting_agency | Plumas                                    | 11 (0%)      | 2 (0%)          |
| reporting_agency | Riverside                                 | 3661 (28%)   | 367 (28%)       |
| reporting_agency | Sacramento                                | 440 (3%)     | 44 (3%)         |
| reporting_agency | San Bernardino                            | 1336 (10%)   | 134 (10%)       |
| reporting_agency | San Diego                                 | 465 (4%)     | 47 (4%)         |
| reporting_agency | San Francisco                             | 308 (2%)     | 31 (2%)         |
| reporting_agency | San Joaquin                               | 33 (0%)      | 4 (0%)          |
| reporting_agency | San Luis Obispo                           | 56 (0%)      | 6 (0%)          |
| reporting_agency | San Mateo                                 | 25 (0%)      | 3 (0%)          |
| reporting_agency | Santa Barbara                             | 30 (0%)      | 3 (0%)          |
| reporting_agency | Santa Clara                               | 235 (2%)     | 24 (2%)         |
| reporting_agency | Santa Cruz                                | 277 (2%)     | 28 (2%)         |
| reporting_agency | Shasta                                    | 63 (0%)      | 7 (1%)          |
| reporting_agency | Siskiyou                                  | 24 (0%)      | 3 (0%)          |
| reporting_agency | Solano                                    | 22 (0%)      | 3 (0%)          |
| reporting_agency | Sonoma                                    | 286 (2%)     | 29 (2%)         |
| reporting_agency | Stanislaus                                | 99 (1%)      | 10 (1%)         |
| reporting_agency | Tehama                                    | 123 (1%)     | 13 (1%)         |
| reporting_agency | Trinity                                   | 11 (0%)      | 2 (0%)          |
| reporting_agency | Tulare                                    | 34 (0%)      | 4 (0%)          |
| reporting_agency | Tuolumne                                  | 14 (0%)      | 2 (0%)          |
| reporting_agency | Ventura                                   | 176 (1%)     | 18 (1%)         |
| reporting_agency | Yolo                                      | 32 (0%)      | 4 (0%)          |
| reporting_agency | Yuba                                      | 59 (0%)      | 6 (0%)          |
| age              | NA                                        | 66.06 (29.6) | 66.3 (13.29)    |

</div>

# Total N sampled

``` r
random_sample_other_agencies %>% 
  nrow()
```

    ## [1] 1326

``` r
random_sample_small_agencies_i %>% 
  nrow()
```

    ## [1] 8

``` r
random_sample_small_agencies_ii %>% 
  nrow()
```

    ## [1] 4

# Export samples and stats

``` r
output <- list(random_sample_small_agencies_i = random_sample_small_agencies_i, random_sample_small_agencies_ii = random_sample_small_agencies_ii,
               random_sample_other_agencies = random_sample_other_agencies, stats_small_agency = full_stats_small_agency, stats_all_other_agency = full_stats_other_agency)

writexl::write_xlsx(output, paste(path_to_proj, "/Quantitative/data/random_sample_iteration_1/randomized_sample.xlsx", sep = ""))

write.csv(dat, paste(path_to_proj, "/Quantitative/data/random_sample_iteration_1/og_file_used.csv", sep = ""))
```
