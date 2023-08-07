Data Exploration
================

# Read in data & libraries

``` r
library(dplyr)
library(janitor)
library(stringr)
library(SummaryTable)

full_dat <- readxl::read_excel("V:/Home Safe Eval 23_25/June 2023 Exploratory Data/7.12.23.Home Safe Data-FY 22-23 Q3.xlsx", 
    sheet = "Full Dataset")

full_dat <- full_dat %>% 
  janitor::clean_names()
```

Break data out by

- Demographics
- Intervention data
- Follow-up data

Demographic data

``` r
demo_dat <- full_dat %>% 
  select(reporting_agency:other_income)
```

Case data

- Intervention binary (yes/no)
- Case start date
- Client referred to CES

``` r
case_dat <- full_dat %>% 
  select(id, case_start_date:client_referred_to_ces)
```

Intervention data

``` r
int_dat <- full_dat %>% 
  select(id,
         starts_with("intervention"))
```

``` r
int_dat_long <- int_dat %>% 
  tidyr::pivot_longer(matches("type"))
```

Follow-up data

``` r
follow_dat <- full_dat %>% 
  select(id, case_closure_date:twelve_month_follow_up_substantiated_aps_reports)
```

# Clean up demographic data

Make all variables of `character` lower case, trim trailing and starting
white spaces and ‘extra’ inner spaces.

``` r
clean_characters <- function(x){
  str_to_lower(x) %>% 
    str_trim() %>% 
    str_squish()
}

demo_dat <- demo_dat %>% 
  mutate(across(where(is.character), clean_characters))
```

Clean APS Report Location indicator, remove “’”,“, ca” and ” ca” from
strings

``` r
demo_dat <- demo_dat %>% 
  mutate(aps_report_location = str_replace(aps_report_location, "'", "")) %>% 
  mutate(aps_report_location = str_replace(aps_report_location, ", ca| ca|,ca", ""))
```

Examine APS Report Location

``` r
demo_dat %>% 
  count(aps_report_location)
```

    # A tibble: 758 × 2
       aps_report_location                n
       <chr>                          <int>
     1 "#"                                1
     2 "--please select--"                1
     3 "0"                               42
     4 "1"                               11
     5 "109 sterling way roseville"       1
     6 "1167 plumas st"                   1
     7 "136 kaseberg lane roseville"      2
     8 "136 kaseberg lane roseville "     2
     9 "2"                                1
    10 "2 houses from corner tx"          2
    # ℹ 748 more rows

## Question for Gina

Should we drop everything from ” -” (i.e. space and then dash)

``` r
demo_dat %>%  
  mutate(aps_report_location = str_replace(aps_report_location, "'", "")) %>% 
  mutate(aps_report_location = str_replace(aps_report_location, ", ca|\\ ca|,ca", "")) %>%        count(aps_report_location) %>% filter(str_detect(aps_report_location, "-")) %>% 
  filter(str_detect(aps_report_location, "-"))
```

    # A tibble: 6 × 2
      aps_report_location                             n
      <chr>                                       <int>
    1 --please select--                               1
    2 auburn -95603                                   1
    3 lincoln 6/9/22- second home safe enrollment     2
    4 lincoln- home- grave disability hold            1
    5 rocklin-                                        1
    6 twenty-nine palms                               1

Counts of `gender`

``` r
demo_dat %>% 
  count(race_1, gender_identity)
```

    # A tibble: 181 × 3
       race_1                         gender_identity        n
       <chr>                          <chr>              <int>
     1 #                              #                      1
     2 44252                          rent leaseholder       1
     3 44376                          data not collected     1
     4 44515                          <NA>                   1
     5 44519                          <NA>                   1
     6 44600                          <NA>                   1
     7 44769                          <NA>                   2
     8 44774                          <NA>                   1
     9 american indian/alaskan native female                20
    10 american indian/alaskan native male                   8
    # ℹ 171 more rows

- Ask Gina, is the data transposition between `race` variables and
  `gender` being corrected prior to getting to us?

What does the `gender` data look like when `race_1` equals`transgender`
,`female` or `male`

``` r
demo_dat %>% 
  filter(race_1 %in% c("female", "male", "transgender")) %>% 
  count(gender_identity, race_1)
```

    # A tibble: 102 × 3
       gender_identity race_1     n
       <chr>           <chr>  <int>
     1 29 palms        female     6
     2 29 palms        male       2
     3 adelanto        female     6
     4 adelanto        male       1
     5 alta loma       female     4
     6 alta loma       male       2
     7 apple valley    female    11
     8 apple valley    male       2
     9 barstow         female    22
    10 barstow         male      23
    # ℹ 92 more rows

Was all the data with this issue just a matter of a column shift???

One solution (remove these–re-shift and then re add)

``` r
shifted_data <- demo_dat %>% 
    filter(race_1 %in% c("female", "male", "transgender")) 

# Anti join, remove shifted data
demo_dat <- demo_dat %>% 
      filter(!race_1 %in% c("female", "male", "transgender")) 


# Fix the shifted data

shifted_data <- shifted_data %>% 
  mutate(
    location_of_participation = gender_identity,
    gender_identity = race_1,
    race_1 = race_2,
    race_2 = ethnicity
  )
```

Glimpse shifted data

``` r
shifted_data %>% 
  count(gender_identity)
```

    # A tibble: 3 × 2
      gender_identity     n
      <chr>           <int>
    1 female            586
    2 male              401
    3 transgender         1

``` r
shifted_data %>% 
  count(race_1)
```

    # A tibble: 8 × 2
      race_1                               n
      <chr>                            <int>
    1 american indian/alaskan native      12
    2 asian                               16
    3 black/african american             193
    4 data not collected                   5
    5 other                               21
    6 pacific islander/native hawaiian     5
    7 unknown/not provided               143
    8 white                              593

``` r
shifted_data %>% 
  count(ethnicity)
```

    # A tibble: 1 × 2
      ethnicity              n
      <chr>              <int>
    1 data not collected   988

Looks good, now re-add the shifted data BACK to the main demo_dat.

``` r
demo_dat <- demo_dat %>% 
  bind_rows(shifted_data)

# Remove the shifted data subset from RAM

rm(shifted_data)
```

Check data dimensions.

``` r
demo_dat %>% 
  dim()
```

    [1] 14690    33

# Explore data quickly

## Data types

``` r
demo_dat %>% 
  glimpse()
```

    Rows: 14,690
    Columns: 33
    $ reporting_agency                                              <chr> "san ber…
    $ id                                                            <dbl> 165223, …
    $ location_of_participation                                     <chr> NA, NA, …
    $ gender_identity                                               <chr> "barstow…
    $ race_1                                                        <chr> "unknown…
    $ race_2                                                        <chr> "white",…
    $ ethnicity                                                     <chr> "data no…
    $ current_marital_status                                        <chr> "widowed…
    $ sexual_orientation                                            <chr> "english…
    $ preferred_language                                            <chr> "no", "n…
    $ veteran_status                                                <chr> "yes", "…
    $ medi_cal                                                      <chr> "no", "n…
    $ medicare                                                      <chr> "no", "n…
    $ representative_payee_or_conservator                           <chr> "with ot…
    $ living_situation_upon_entry                                   <chr> "0", "0"…
    $ monthly_rent_mortgage_contribution                            <dbl> NA, NA, …
    $ client_homeless_within_the_last_three_years                   <chr> "client …
    $ number_of_times_homelessness_occurred_in_the_last_three_years <chr> "yes", "…
    $ total_duration_of_homelessness                                <chr> "no", "n…
    $ last_period_of_homelessness                                   <chr> "no", "n…
    $ current_eviction_or_foreclosures                              <chr> "no", "n…
    $ previous_evictions_or_foreclosures                            <chr> "43906.5…
    $ discharge_from_institution_in_the_last_six_months             <chr> "no", "n…
    $ aps_report_date                                               <dttm> NA, 190…
    $ aps_report_location                                           <chr> "no inte…
    $ abuse_by_other_financial                                      <chr> NA, NA, …
    $ abuse_by_other_non_financial                                  <chr> NA, NA, …
    $ self_neglect                                                  <chr> NA, NA, …
    $ reporting_source                                              <chr> NA, NA, …
    $ previous_aps_involvement                                      <chr> NA, "app…
    $ income_from_benefits                                          <dbl> NA, NA, …
    $ work_for_pay                                                  <dbl> NA, NA, …
    $ other_income                                                  <dbl> NA, NA, …

# Quick descriptives

``` r
demo_dat %>% 
  select(-id, -aps_report_date, -aps_report_location) %>% 
  var_summarise()
```

    No variables specified, all applicable variables will be used.

    # A tibble: 2,806 × 3
       Variable                                    Label               statistic    
       <chr>                                       <chr>               <chr>        
     1 abuse_by_other_financial                    unknown             134 (1.16%)  
     2 abuse_by_other_financial                    no                  9652 (83.75%)
     3 abuse_by_other_financial                    yes                 1739 (15.09%)
     4 abuse_by_other_non_financial                unknown             109 (0.95%)  
     5 abuse_by_other_non_financial                no                  9007 (78.15%)
     6 abuse_by_other_non_financial                yes                 2409 (20.9%) 
     7 client_homeless_within_the_last_three_years 0                   28 (0.2%)    
     8 client_homeless_within_the_last_three_years data not collected  5115 (35.84%)
     9 client_homeless_within_the_last_three_years unknown             685 (4.8%)   
    10 client_homeless_within_the_last_three_years client doesn't know 3 (0.02%)    
    # ℹ 2,796 more rows
