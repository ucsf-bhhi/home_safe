Cleaning Demographic Data
================
Sara Colom
2023-09-01

# Read in data & libraries

``` r
source("R/helpers.R")
```

``` r
demo_dat %>% 
  glimpse()
```

    ## Rows: 14,690
    ## Columns: 33
    ## $ id                                                            <dbl> 116726, …
    ## $ reporting_agency                                              <chr> "San Ber…
    ## $ location_of_participation                                     <chr> NA, NA, …
    ## $ gender_identity                                               <chr> "29 Palm…
    ## $ race_1                                                        <chr> "Female"…
    ## $ race_2                                                        <chr> "Unknown…
    ## $ ethnicity                                                     <chr> "Data No…
    ## $ current_marital_status                                        <chr> "Unknown…
    ## $ sexual_orientation                                            <chr> "English…
    ## $ preferred_language                                            <chr> "No", "N…
    ## $ veteran_status                                                <chr> "Yes", "…
    ## $ medi_cal                                                      <chr> "No", "Y…
    ## $ medicare                                                      <chr> "No", "N…
    ## $ representative_payee_or_conservator                           <chr> "Homeles…
    ## $ living_situation_upon_entry                                   <chr> "0", "0"…
    ## $ monthly_rent_mortgage_contribution                            <dbl> NA, NA, …
    ## $ client_homeless_within_the_last_three_years                   <chr> "Current…
    ## $ number_of_times_homelessness_occurred_in_the_last_three_years <chr> "No", "Y…
    ## $ total_duration_of_homelessness                                <chr> "Yes", "…
    ## $ last_period_of_homelessness                                   <chr> "No", "N…
    ## $ current_eviction_or_foreclosures                              <chr> "No", "N…
    ## $ previous_evictions_or_foreclosures                            <chr> "44789.7…
    ## $ discharge_from_institution_in_the_last_six_months             <chr> "No", "N…
    ## $ aps_report_date                                               <dttm> 1902-09…
    ## $ aps_report_location                                           <chr> "Tempora…
    ## $ abuse_by_other_financial                                      <chr> NA, NA, …
    ## $ abuse_by_other_non_financial                                  <chr> NA, NA, …
    ## $ self_neglect                                                  <chr> NA, NA, …
    ## $ reporting_source                                              <chr> NA, NA, …
    ## $ previous_aps_involvement                                      <chr> NA, NA, …
    ## $ income_from_benefits                                          <dbl> NA, NA, …
    ## $ work_for_pay                                                  <dbl> NA, NA, …
    ## $ other_income                                                  <dbl> NA, NA, …

# Clean up demographic data

Make all variables of `character` lower case, trim trailing and starting
white spaces and ‘extra’ inner spaces.

``` r
demo_dat <- demo_dat %>% 
  mutate(across(where(is.character), clean_characters))
```

## Location of Participation:

remove ” ca” “, ca” and “california” from the end of responses. remove
special characters like “:” before laguna beach. Combine and correct
misspellings. Leaving combined cities or ones that seem similar
(i.e. big bear and big bear valley) alone for now, also not verifying
that all of these are in fact cities (I only verified for ones I
suspected having misspellings). Remove Zip codes from the end of cities
listed. Combining all unknown’s into one (data not collected, refused,
doesn’t know etc).

### Questions:

ca - perhaps best to change this to unknown? “cal city”, “california
city” - do we think these are the same? (Made the change in the recode
for now) responses i.e. “coulterville/la grange” (so combo of two
cities) - leave as is? “north side of parking lot” - ? “sin city” -
might be sun city? (there is no sin city in california it seems) “ylp” -
? what to do with zipcodes? (shall I go look up the cities for those
codes?) home, homeless, female, male - are in the data so double check
after new data comes in

``` r
demo_dat <- demo_dat %>% 
  mutate(location_of_participation_recode = case_when(str_ends(location_of_participation, " ca") ~ str_replace_all(location_of_participation, "ca|, ca", ""),
                                                      str_ends(location_of_participation, "california") ~ str_replace_all(location_of_participation, " california", ""),
                                                      str_detect(location_of_participation, ":") ~ "laguna beach",
                                                      str_detect(location_of_participation, "bakersf|bakerf") ~ "bakersfield",
                                                      str_detect(location_of_participation, "bartso") ~ "barstow",
                                                      str_detect(location_of_participation, "beam|beum") ~ "beaumont",
                                                      str_detect(location_of_participation, "cal city") ~ "california city",
                                                      str_detect(location_of_participation, "cailm") ~ "calimesa",
                                                      str_detect(location_of_participation, "catherd") ~ "cathedral city",
                                                      str_detect(location_of_participation, "cathey") ~ "catheys valley",
                                                      str_detect(location_of_participation, "clairem") ~ "claremont",
                                                      str_detect(location_of_participation, "cochell") ~ "coachella", 
                                                      str_detect(location_of_participation, "desesrt|desrt") ~ "desert hot springs",
                                                      str_detect(location_of_participation, "frasier") ~ "frazier park",
                                                      str_detect(location_of_participation, "idlewild") ~ "idyllwild",
                                                      str_detect(location_of_participation, "jurupa") ~"jurupa valley",
                                                      str_detect(location_of_participation, "mills hospital in b") ~ "burlingame",
                                                      str_detect(location_of_participation, "mountainview") ~"mountain view",
                                                      str_detect(location_of_participation, "murreta") ~ "murrieta",
                                                      str_detect(location_of_participation, "pachecc") ~ "pacheco",
                                                      str_detect(location_of_participation, "palms sp|palm spr|palm dpr") ~ "palm springs",
                                                      str_detect(location_of_participation, "ranch mira") ~ "rancho mirage",
                                                      str_detect(location_of_participation, "rancho cucam") ~ "rancho cucamonga",
                                                      str_detect(location_of_participation, "richomo") ~ "richmond",
                                                      str_detect(location_of_participation, "ridegc") ~ "ridgecrest",
                                                      str_detect(location_of_participation, "riverisd|riveside") ~ "riverside",
                                                      str_detect(location_of_participation, "rough &|rough and re") ~ "rough and ready",
                                                      str_detect(location_of_participation, "sacrment|sacarament|sarcamen") ~ "sacramento",
                                                      str_detect(location_of_participation, "san berna|san bera|san bernr") ~ "san bernardino",
                                                      str_detect(location_of_participation, "san dieg") ~ "san diego",
                                                      str_detect(location_of_participation, "san fra|sf") ~ "san francisco",
                                                      str_detect(location_of_participation, "sugar l") ~ "sugarloaf",
                                                      str_detect(location_of_participation, "tehac") ~ "tehachapi",
                                                      str_detect(location_of_participation, "twenty|29 pal") ~ "twentynine palms",
                                                      str_detect(location_of_participation, "vict") ~ "victorville",
                                                      str_detect(location_of_participation, "weaverv") ~ "weaverville",
                                                      str_detect(location_of_participation, "wildom") ~ "wildomar", 
                                                      str_detect(location_of_participation, "yucca") ~ "yucca valley",
                                                      # the following had zipcodes at the end of the string
                                                      str_detect(location_of_participation, "indio") ~ "indio",
                                                      str_detect(location_of_participation, "inglewood") ~ "inglewood",
                                                      str_detect(location_of_participation, "marysvil") ~ "marysville",
                                                      str_detect(location_of_participation, "turlock") ~ "turlock",
                                                      str_detect(location_of_participation, "yuba") ~ "yuba city",
                                                      str_detect(location_of_participation, "yucaipa") ~ "yucaipa",
                                                      
                                                      str_detect(location_of_participation, "data not col|doesn't know|refused|missing|unk") ~ "unknown",
                                                      is.na(location_of_participation) ~ "unknown",
                                                      TRUE ~ location_of_participation))
```

Check on location of participation

``` r
demo_dat %>% 
  count(location_of_participation, location_of_participation_recode)
```

<div class="kable-table">

<table>
<thead>
<tr>
<th style="text-align:left;">
location_of_participation
</th>
<th style="text-align:left;">
location_of_participation_recode
</th>
<th style="text-align:right;">
n
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
–please select–
</td>
<td style="text-align:left;">
–please select–
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
2 houses from corner tx
</td>
<td style="text-align:left;">
2 houses from corner tx
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
29 palms
</td>
<td style="text-align:left;">
twentynine palms
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
4000 orange st
</td>
<td style="text-align:left;">
4000 orange st
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43920
</td>
<td style="text-align:left;">
43920
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43990
</td>
<td style="text-align:left;">
43990
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44763
</td>
<td style="text-align:left;">
44763
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
92543
</td>
<td style="text-align:left;">
92543
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
:laguna beach
</td>
<td style="text-align:left;">
laguna beach
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
acampo
</td>
<td style="text-align:left;">
acampo
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
acton
</td>
<td style="text-align:left;">
acton
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
adelanto
</td>
<td style="text-align:left;">
adelanto
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
alameda
</td>
<td style="text-align:left;">
alameda
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
alamo
</td>
<td style="text-align:left;">
alamo
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
albany
</td>
<td style="text-align:left;">
albany
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
alhambra
</td>
<td style="text-align:left;">
alhambra
</td>
<td style="text-align:right;">
13
</td>
</tr>
<tr>
<td style="text-align:left;">
alpine
</td>
<td style="text-align:left;">
alpine
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
alta loma
</td>
<td style="text-align:left;">
alta loma
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
altadena
</td>
<td style="text-align:left;">
altadena
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
american canyon
</td>
<td style="text-align:left;">
american canyon
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
anaheim
</td>
<td style="text-align:left;">
anaheim
</td>
<td style="text-align:right;">
46
</td>
</tr>
<tr>
<td style="text-align:left;">
anderson
</td>
<td style="text-align:left;">
anderson
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
antelope
</td>
<td style="text-align:left;">
antelope
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
antioch
</td>
<td style="text-align:left;">
antioch
</td>
<td style="text-align:right;">
45
</td>
</tr>
<tr>
<td style="text-align:left;">
antioch, ca
</td>
<td style="text-align:left;">
antioch
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
anza
</td>
<td style="text-align:left;">
anza
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
apple valley
</td>
<td style="text-align:left;">
apple valley
</td>
<td style="text-align:right;">
17
</td>
</tr>
<tr>
<td style="text-align:left;">
aptos
</td>
<td style="text-align:left;">
aptos
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
arcadia
</td>
<td style="text-align:left;">
arcadia
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
arcata
</td>
<td style="text-align:left;">
arcata
</td>
<td style="text-align:right;">
31
</td>
</tr>
<tr>
<td style="text-align:left;">
arleta
</td>
<td style="text-align:left;">
arleta
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
arroyo grande
</td>
<td style="text-align:left;">
arroyo grande
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
artesia
</td>
<td style="text-align:left;">
artesia
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
arvin
</td>
<td style="text-align:left;">
arvin
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
atascadero
</td>
<td style="text-align:left;">
atascadero
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
atwater
</td>
<td style="text-align:left;">
atwater
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
auberry
</td>
<td style="text-align:left;">
auberry
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
auburn
</td>
<td style="text-align:left;">
auburn
</td>
<td style="text-align:right;">
50
</td>
</tr>
<tr>
<td style="text-align:left;">
avila beach
</td>
<td style="text-align:left;">
avila beach
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
azusa
</td>
<td style="text-align:left;">
azusa
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
bakerfield
</td>
<td style="text-align:left;">
bakersfield
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
bakerfsield
</td>
<td style="text-align:left;">
bakersfield
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
bakersfiel
</td>
<td style="text-align:left;">
bakersfield
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
bakersfield
</td>
<td style="text-align:left;">
bakersfield
</td>
<td style="text-align:right;">
708
</td>
</tr>
<tr>
<td style="text-align:left;">
bakersfield ca
</td>
<td style="text-align:left;">
bakersfield
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
bakersfield, ca
</td>
<td style="text-align:left;">
bakersfield
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
bakersfiled
</td>
<td style="text-align:left;">
bakersfield
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
baldwin park
</td>
<td style="text-align:left;">
baldwin park
</td>
<td style="text-align:right;">
13
</td>
</tr>
<tr>
<td style="text-align:left;">
ballico
</td>
<td style="text-align:left;">
ballico
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
bangor
</td>
<td style="text-align:left;">
bangor
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
banning
</td>
<td style="text-align:left;">
banning
</td>
<td style="text-align:right;">
77
</td>
</tr>
<tr>
<td style="text-align:left;">
barstow
</td>
<td style="text-align:left;">
barstow
</td>
<td style="text-align:right;">
52
</td>
</tr>
<tr>
<td style="text-align:left;">
bartsow
</td>
<td style="text-align:left;">
barstow
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
bass lake
</td>
<td style="text-align:left;">
bass lake
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
bay point
</td>
<td style="text-align:left;">
bay point
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
beamount
</td>
<td style="text-align:left;">
beaumont
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
beaumont
</td>
<td style="text-align:left;">
beaumont
</td>
<td style="text-align:right;">
41
</td>
</tr>
<tr>
<td style="text-align:left;">
bell
</td>
<td style="text-align:left;">
bell
</td>
<td style="text-align:right;">
17
</td>
</tr>
<tr>
<td style="text-align:left;">
bell gardens
</td>
<td style="text-align:left;">
bell gardens
</td>
<td style="text-align:right;">
14
</td>
</tr>
<tr>
<td style="text-align:left;">
bellflower
</td>
<td style="text-align:left;">
bellflower
</td>
<td style="text-align:right;">
14
</td>
</tr>
<tr>
<td style="text-align:left;">
ben lomond
</td>
<td style="text-align:left;">
ben lomond
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
benton
</td>
<td style="text-align:left;">
benton
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
benton, ca
</td>
<td style="text-align:left;">
benton
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
berkeley
</td>
<td style="text-align:left;">
berkeley
</td>
<td style="text-align:right;">
16
</td>
</tr>
<tr>
<td style="text-align:left;">
bethel island
</td>
<td style="text-align:left;">
bethel island
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
beumont
</td>
<td style="text-align:left;">
beaumont
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
beverly hills
</td>
<td style="text-align:left;">
beverly hills
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
big bear
</td>
<td style="text-align:left;">
big bear
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
big bear lake
</td>
<td style="text-align:left;">
big bear lake
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
bishop
</td>
<td style="text-align:left;">
bishop
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
bloomington
</td>
<td style="text-align:left;">
bloomington
</td>
<td style="text-align:right;">
13
</td>
</tr>
<tr>
<td style="text-align:left;">
bloomington/colton
</td>
<td style="text-align:left;">
bloomington/colton
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
blue jay
</td>
<td style="text-align:left;">
blue jay
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
blue lake
</td>
<td style="text-align:left;">
blue lake
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
blythe
</td>
<td style="text-align:left;">
blythe
</td>
<td style="text-align:right;">
19
</td>
</tr>
<tr>
<td style="text-align:left;">
bodega bay
</td>
<td style="text-align:left;">
bodega bay
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
bodfish
</td>
<td style="text-align:left;">
bodfish
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
bolinas
</td>
<td style="text-align:left;">
bolinas
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
boron
</td>
<td style="text-align:left;">
boron
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
boulder creek
</td>
<td style="text-align:left;">
boulder creek
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
boulevard
</td>
<td style="text-align:left;">
boulevard
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
brea
</td>
<td style="text-align:left;">
brea
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
brentwood
</td>
<td style="text-align:left;">
brentwood
</td>
<td style="text-align:right;">
13
</td>
</tr>
<tr>
<td style="text-align:left;">
bridgeport
</td>
<td style="text-align:left;">
bridgeport
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
bridgeport, ca
</td>
<td style="text-align:left;">
bridgeport
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
bridgeville
</td>
<td style="text-align:left;">
bridgeville
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
brownsvalley
</td>
<td style="text-align:left;">
brownsvalley
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
brownsville
</td>
<td style="text-align:left;">
brownsville
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
buena park
</td>
<td style="text-align:left;">
buena park
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
burbank
</td>
<td style="text-align:left;">
burbank
</td>
<td style="text-align:right;">
29
</td>
</tr>
<tr>
<td style="text-align:left;">
burlingame
</td>
<td style="text-align:left;">
burlingame
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
burney
</td>
<td style="text-align:left;">
burney
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
buttonwillow
</td>
<td style="text-align:left;">
buttonwillow
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
byron
</td>
<td style="text-align:left;">
byron
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
ca
</td>
<td style="text-align:left;">
ca
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
ca 92284
</td>
<td style="text-align:left;">
ca 92284
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
cabazon
</td>
<td style="text-align:left;">
cabazon
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
cailmesa
</td>
<td style="text-align:left;">
calimesa
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
cal city
</td>
<td style="text-align:left;">
california city
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
calabasas
</td>
<td style="text-align:left;">
calabasas
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
california city
</td>
<td style="text-align:left;">
california city
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
calimesa
</td>
<td style="text-align:left;">
calimesa
</td>
<td style="text-align:right;">
14
</td>
</tr>
<tr>
<td style="text-align:left;">
camarillo
</td>
<td style="text-align:left;">
camarillo
</td>
<td style="text-align:right;">
15
</td>
</tr>
<tr>
<td style="text-align:left;">
cambria
</td>
<td style="text-align:left;">
cambria
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
campbell
</td>
<td style="text-align:left;">
campbell
</td>
<td style="text-align:right;">
16
</td>
</tr>
<tr>
<td style="text-align:left;">
campo
</td>
<td style="text-align:left;">
campo
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
canoga park
</td>
<td style="text-align:left;">
canoga park
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
canyon country
</td>
<td style="text-align:left;">
canyon country
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
canyon lake
</td>
<td style="text-align:left;">
canyon lake
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
capay
</td>
<td style="text-align:left;">
capay
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
capitola
</td>
<td style="text-align:left;">
capitola
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
carlotta
</td>
<td style="text-align:left;">
carlotta
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
carlsbad
</td>
<td style="text-align:left;">
carlsbad
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
carmichael
</td>
<td style="text-align:left;">
carmichael
</td>
<td style="text-align:right;">
14
</td>
</tr>
<tr>
<td style="text-align:left;">
carpinteria
</td>
<td style="text-align:left;">
carpinteria
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
carson
</td>
<td style="text-align:left;">
carson
</td>
<td style="text-align:right;">
33
</td>
</tr>
<tr>
<td style="text-align:left;">
castaic
</td>
<td style="text-align:left;">
castaic
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
castella
</td>
<td style="text-align:left;">
castella
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
castro valley
</td>
<td style="text-align:left;">
castro valley
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
cathedral city
</td>
<td style="text-align:left;">
cathedral city
</td>
<td style="text-align:right;">
77
</td>
</tr>
<tr>
<td style="text-align:left;">
catherdal city
</td>
<td style="text-align:left;">
cathedral city
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
cathey valley
</td>
<td style="text-align:left;">
catheys valley
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
cathey’s valley
</td>
<td style="text-align:left;">
catheys valley
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
catheys valley
</td>
<td style="text-align:left;">
catheys valley
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
cayucos
</td>
<td style="text-align:left;">
cayucos
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
cedar ridge
</td>
<td style="text-align:left;">
cedar ridge
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
ceres
</td>
<td style="text-align:left;">
ceres
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
cerritos
</td>
<td style="text-align:left;">
cerritos
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
chalfant
</td>
<td style="text-align:left;">
chalfant
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
chatsworth
</td>
<td style="text-align:left;">
chatsworth
</td>
<td style="text-align:right;">
18
</td>
</tr>
<tr>
<td style="text-align:left;">
cherry valley
</td>
<td style="text-align:left;">
cherry valley
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
chester, ca
</td>
<td style="text-align:left;">
chester
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
chico
</td>
<td style="text-align:left;">
chico
</td>
<td style="text-align:right;">
59
</td>
</tr>
<tr>
<td style="text-align:left;">
chino
</td>
<td style="text-align:left;">
chino
</td>
<td style="text-align:right;">
34
</td>
</tr>
<tr>
<td style="text-align:left;">
chino hills
</td>
<td style="text-align:left;">
chino hills
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
chowchilla
</td>
<td style="text-align:left;">
chowchilla
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
chula vista
</td>
<td style="text-align:left;">
chula vista
</td>
<td style="text-align:right;">
22
</td>
</tr>
<tr>
<td style="text-align:left;">
citrus heights
</td>
<td style="text-align:left;">
citrus heights
</td>
<td style="text-align:right;">
23
</td>
</tr>
<tr>
<td style="text-align:left;">
city of commerce
</td>
<td style="text-align:left;">
city of commerce
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
clairemont
</td>
<td style="text-align:left;">
claremont
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
claremont
</td>
<td style="text-align:left;">
claremont
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
clayton
</td>
<td style="text-align:left;">
clayton
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
clearlake
</td>
<td style="text-align:left;">
clearlake
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
cloverdale
</td>
<td style="text-align:left;">
cloverdale
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
clovis
</td>
<td style="text-align:left;">
clovis
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
coachella
</td>
<td style="text-align:left;">
coachella
</td>
<td style="text-align:right;">
24
</td>
</tr>
<tr>
<td style="text-align:left;">
coalinga
</td>
<td style="text-align:left;">
coalinga
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
coarsegold
</td>
<td style="text-align:left;">
coarsegold
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
cochella
</td>
<td style="text-align:left;">
coachella
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
cohasset
</td>
<td style="text-align:left;">
cohasset
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
coleville
</td>
<td style="text-align:left;">
coleville
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
coleville, ca
</td>
<td style="text-align:left;">
coleville
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
colfax
</td>
<td style="text-align:left;">
colfax
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
colton
</td>
<td style="text-align:left;">
colton
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
colusa
</td>
<td style="text-align:left;">
colusa
</td>
<td style="text-align:right;">
18
</td>
</tr>
<tr>
<td style="text-align:left;">
compton
</td>
<td style="text-align:left;">
compton
</td>
<td style="text-align:right;">
24
</td>
</tr>
<tr>
<td style="text-align:left;">
concord
</td>
<td style="text-align:left;">
concord
</td>
<td style="text-align:right;">
63
</td>
</tr>
<tr>
<td style="text-align:left;">
corcoran
</td>
<td style="text-align:left;">
corcoran
</td>
<td style="text-align:right;">
37
</td>
</tr>
<tr>
<td style="text-align:left;">
corning
</td>
<td style="text-align:left;">
corning
</td>
<td style="text-align:right;">
30
</td>
</tr>
<tr>
<td style="text-align:left;">
corona
</td>
<td style="text-align:left;">
corona
</td>
<td style="text-align:right;">
143
</td>
</tr>
<tr>
<td style="text-align:left;">
corona, ca
</td>
<td style="text-align:left;">
corona
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
corte madera
</td>
<td style="text-align:left;">
corte madera
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
costa mesa
</td>
<td style="text-align:left;">
costa mesa
</td>
<td style="text-align:right;">
22
</td>
</tr>
<tr>
<td style="text-align:left;">
cotati
</td>
<td style="text-align:left;">
cotati
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
cottonwood
</td>
<td style="text-align:left;">
cottonwood
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
coulterville
</td>
<td style="text-align:left;">
coulterville
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
coulterville/la grange
</td>
<td style="text-align:left;">
coulterville/la grange
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
covina
</td>
<td style="text-align:left;">
covina
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
crescent city
</td>
<td style="text-align:left;">
crescent city
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
crestline
</td>
<td style="text-align:left;">
crestline
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
crockett
</td>
<td style="text-align:left;">
crockett
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
crowslanding
</td>
<td style="text-align:left;">
crowslanding
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
cudahy
</td>
<td style="text-align:left;">
cudahy
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
culver city
</td>
<td style="text-align:left;">
culver city
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
cupertino
</td>
<td style="text-align:left;">
cupertino
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
cutten
</td>
<td style="text-align:left;">
cutten
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
cypress
</td>
<td style="text-align:left;">
cypress
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
daly city
</td>
<td style="text-align:left;">
daly city
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
dana point
</td>
<td style="text-align:left;">
dana point
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
davenport
</td>
<td style="text-align:left;">
davenport
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
davis
</td>
<td style="text-align:left;">
davis
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
davis ca
</td>
<td style="text-align:left;">
davis
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
delano
</td>
<td style="text-align:left;">
delano
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
delhi
</td>
<td style="text-align:left;">
delhi
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
desert hot springs
</td>
<td style="text-align:left;">
desert hot springs
</td>
<td style="text-align:right;">
130
</td>
</tr>
<tr>
<td style="text-align:left;">
desesrt hot springs
</td>
<td style="text-align:left;">
desert hot springs
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
desrt hot springs
</td>
<td style="text-align:left;">
desert hot springs
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
dinuba
</td>
<td style="text-align:left;">
dinuba
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
discovery bay
</td>
<td style="text-align:left;">
discovery bay
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
dos palos
</td>
<td style="text-align:left;">
dos palos
</td>
<td style="text-align:right;">
17
</td>
</tr>
<tr>
<td style="text-align:left;">
douglas city
</td>
<td style="text-align:left;">
douglas city
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
downey
</td>
<td style="text-align:left;">
downey
</td>
<td style="text-align:right;">
30
</td>
</tr>
<tr>
<td style="text-align:left;">
doyle
</td>
<td style="text-align:left;">
doyle
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
duarte
</td>
<td style="text-align:left;">
duarte
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
dunlap
</td>
<td style="text-align:left;">
dunlap
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
dunnigan
</td>
<td style="text-align:left;">
dunnigan
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
east los angeles
</td>
<td style="text-align:left;">
east los angeles
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
east palo alto
</td>
<td style="text-align:left;">
east palo alto
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
eastvale
</td>
<td style="text-align:left;">
eastvale
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
el cajon
</td>
<td style="text-align:left;">
el cajon
</td>
<td style="text-align:right;">
40
</td>
</tr>
<tr>
<td style="text-align:left;">
el cerrito
</td>
<td style="text-align:left;">
el cerrito
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
el monte
</td>
<td style="text-align:left;">
el monte
</td>
<td style="text-align:right;">
14
</td>
</tr>
<tr>
<td style="text-align:left;">
el nido
</td>
<td style="text-align:left;">
el nido
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
el portal
</td>
<td style="text-align:left;">
el portal
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
el segundo
</td>
<td style="text-align:left;">
el segundo
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
el sobrante
</td>
<td style="text-align:left;">
el sobrante
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
elk grove
</td>
<td style="text-align:left;">
elk grove
</td>
<td style="text-align:right;">
27
</td>
</tr>
<tr>
<td style="text-align:left;">
elverta
</td>
<td style="text-align:left;">
elverta
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
emeryville
</td>
<td style="text-align:left;">
emeryville
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
encinitas
</td>
<td style="text-align:left;">
encinitas
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
encino
</td>
<td style="text-align:left;">
encino
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
escondido
</td>
<td style="text-align:left;">
escondido
</td>
<td style="text-align:right;">
31
</td>
</tr>
<tr>
<td style="text-align:left;">
eureka
</td>
<td style="text-align:left;">
eureka
</td>
<td style="text-align:right;">
107
</td>
</tr>
<tr>
<td style="text-align:left;">
exeter
</td>
<td style="text-align:left;">
exeter
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
exeter (tulle ville)
</td>
<td style="text-align:left;">
exeter (tulle ville)
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
fair oaks
</td>
<td style="text-align:left;">
fair oaks
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
fairfax
</td>
<td style="text-align:left;">
fairfax
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
fairfield
</td>
<td style="text-align:left;">
fairfield
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
fallbrook
</td>
<td style="text-align:left;">
fallbrook
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
farmersville
</td>
<td style="text-align:left;">
farmersville
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
felton
</td>
<td style="text-align:left;">
felton
</td>
<td style="text-align:right;">
14
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
39
</td>
</tr>
<tr>
<td style="text-align:left;">
ferndale
</td>
<td style="text-align:left;">
ferndale
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
fillmore
</td>
<td style="text-align:left;">
fillmore
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
folsom
</td>
<td style="text-align:left;">
folsom
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
fontana
</td>
<td style="text-align:left;">
fontana
</td>
<td style="text-align:right;">
62
</td>
</tr>
<tr>
<td style="text-align:left;">
foresthill
</td>
<td style="text-align:left;">
foresthill
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
forestville
</td>
<td style="text-align:left;">
forestville
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
fort bragg
</td>
<td style="text-align:left;">
fort bragg
</td>
<td style="text-align:right;">
35
</td>
</tr>
<tr>
<td style="text-align:left;">
fortuna
</td>
<td style="text-align:left;">
fortuna
</td>
<td style="text-align:right;">
15
</td>
</tr>
<tr>
<td style="text-align:left;">
fountain valley
</td>
<td style="text-align:left;">
fountain valley
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
frasier park
</td>
<td style="text-align:left;">
frazier park
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
frazier park
</td>
<td style="text-align:left;">
frazier park
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
freedom
</td>
<td style="text-align:left;">
freedom
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
fremont
</td>
<td style="text-align:left;">
fremont
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
french camp
</td>
<td style="text-align:left;">
french camp
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
fresno
</td>
<td style="text-align:left;">
fresno
</td>
<td style="text-align:right;">
107
</td>
</tr>
<tr>
<td style="text-align:left;">
fullerton
</td>
<td style="text-align:left;">
fullerton
</td>
<td style="text-align:right;">
14
</td>
</tr>
<tr>
<td style="text-align:left;">
fulton
</td>
<td style="text-align:left;">
fulton
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
garberville
</td>
<td style="text-align:left;">
garberville
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
garden grove
</td>
<td style="text-align:left;">
garden grove
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
gardena
</td>
<td style="text-align:left;">
gardena
</td>
<td style="text-align:right;">
51
</td>
</tr>
<tr>
<td style="text-align:left;">
gardena ca
</td>
<td style="text-align:left;">
gardena
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
gerber
</td>
<td style="text-align:left;">
gerber
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
gilroy
</td>
<td style="text-align:left;">
gilroy
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
glen ellen
</td>
<td style="text-align:left;">
glen ellen
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
glendale
</td>
<td style="text-align:left;">
glendale
</td>
<td style="text-align:right;">
23
</td>
</tr>
<tr>
<td style="text-align:left;">
glendora
</td>
<td style="text-align:left;">
glendora
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
goleta
</td>
<td style="text-align:left;">
goleta
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
granada hills
</td>
<td style="text-align:left;">
granada hills
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
grand terrace
</td>
<td style="text-align:left;">
grand terrace
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
granite bay
</td>
<td style="text-align:left;">
granite bay
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
grass valley
</td>
<td style="text-align:left;">
grass valley
</td>
<td style="text-align:right;">
185
</td>
</tr>
<tr>
<td style="text-align:left;">
gridley
</td>
<td style="text-align:left;">
gridley
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
grover beach
</td>
<td style="text-align:left;">
grover beach
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
gualala
</td>
<td style="text-align:left;">
gualala
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
guerneville
</td>
<td style="text-align:left;">
guerneville
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
gustine
</td>
<td style="text-align:left;">
gustine
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
hacienda heights
</td>
<td style="text-align:left;">
hacienda heights
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
hamilton city
</td>
<td style="text-align:left;">
hamilton city
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
hammil valley, ca
</td>
<td style="text-align:left;">
hammil valley
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
hanford
</td>
<td style="text-align:left;">
hanford
</td>
<td style="text-align:right;">
118
</td>
</tr>
<tr>
<td style="text-align:left;">
harbor city
</td>
<td style="text-align:left;">
harbor city
</td>
<td style="text-align:right;">
20
</td>
</tr>
<tr>
<td style="text-align:left;">
hawthorne
</td>
<td style="text-align:left;">
hawthorne
</td>
<td style="text-align:right;">
16
</td>
</tr>
<tr>
<td style="text-align:left;">
hayfork
</td>
<td style="text-align:left;">
hayfork
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
hayward
</td>
<td style="text-align:left;">
hayward
</td>
<td style="text-align:right;">
25
</td>
</tr>
<tr>
<td style="text-align:left;">
healdsburg
</td>
<td style="text-align:left;">
healdsburg
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
helendale
</td>
<td style="text-align:left;">
helendale
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
hemet
</td>
<td style="text-align:left;">
hemet
</td>
<td style="text-align:right;">
491
</td>
</tr>
<tr>
<td style="text-align:left;">
hercules
</td>
<td style="text-align:left;">
hercules
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
hermosa beach
</td>
<td style="text-align:left;">
hermosa beach
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
hesperia
</td>
<td style="text-align:left;">
hesperia
</td>
<td style="text-align:right;">
28
</td>
</tr>
<tr>
<td style="text-align:left;">
highland
</td>
<td style="text-align:left;">
highland
</td>
<td style="text-align:right;">
20
</td>
</tr>
<tr>
<td style="text-align:left;">
hilmar
</td>
<td style="text-align:left;">
hilmar
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
hollywood
</td>
<td style="text-align:left;">
hollywood
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
holmes
</td>
<td style="text-align:left;">
holmes
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
home
</td>
<td style="text-align:left;">
home
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
homeland
</td>
<td style="text-align:left;">
homeland
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
homeless
</td>
<td style="text-align:left;">
homeless
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
hoopa
</td>
<td style="text-align:left;">
hoopa
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
huntington beach
</td>
<td style="text-align:left;">
huntington beach
</td>
<td style="text-align:right;">
18
</td>
</tr>
<tr>
<td style="text-align:left;">
huntington park
</td>
<td style="text-align:left;">
huntington park
</td>
<td style="text-align:right;">
23
</td>
</tr>
<tr>
<td style="text-align:left;">
hyampom
</td>
<td style="text-align:left;">
hyampom
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
idlewild
</td>
<td style="text-align:left;">
idyllwild
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
idyllwild
</td>
<td style="text-align:left;">
idyllwild
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
imperial beach
</td>
<td style="text-align:left;">
imperial beach
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
indian wells
</td>
<td style="text-align:left;">
indian wells
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
indio
</td>
<td style="text-align:left;">
indio
</td>
<td style="text-align:right;">
170
</td>
</tr>
<tr>
<td style="text-align:left;">
indio, ca
</td>
<td style="text-align:left;">
indio
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
indio92203
</td>
<td style="text-align:left;">
indio
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
inglewood
</td>
<td style="text-align:left;">
inglewood
</td>
<td style="text-align:right;">
61
</td>
</tr>
<tr>
<td style="text-align:left;">
inglewood 90305
</td>
<td style="text-align:left;">
inglewood
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
ione
</td>
<td style="text-align:left;">
ione
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
irvine
</td>
<td style="text-align:left;">
irvine
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
isleton
</td>
<td style="text-align:left;">
isleton
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
ivanhoe
</td>
<td style="text-align:left;">
ivanhoe
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
jackson
</td>
<td style="text-align:left;">
jackson
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
jamestown
</td>
<td style="text-align:left;">
jamestown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
jamul
</td>
<td style="text-align:left;">
jamul
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
joshua tree
</td>
<td style="text-align:left;">
joshua tree
</td>
<td style="text-align:right;">
25
</td>
</tr>
<tr>
<td style="text-align:left;">
julian
</td>
<td style="text-align:left;">
julian
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
june lake
</td>
<td style="text-align:left;">
june lake
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
jurupa
</td>
<td style="text-align:left;">
jurupa valley
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
jurupa valley
</td>
<td style="text-align:left;">
jurupa valley
</td>
<td style="text-align:right;">
57
</td>
</tr>
<tr>
<td style="text-align:left;">
kelseyville
</td>
<td style="text-align:left;">
kelseyville
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
kensington
</td>
<td style="text-align:left;">
kensington
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
kentfield
</td>
<td style="text-align:left;">
kentfield
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
kerman
</td>
<td style="text-align:left;">
kerman
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
kernville
</td>
<td style="text-align:left;">
kernville
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
kettleman city
</td>
<td style="text-align:left;">
kettleman city
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
kingsburg
</td>
<td style="text-align:left;">
kingsburg
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
klamath
</td>
<td style="text-align:left;">
klamath
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
la crescenta
</td>
<td style="text-align:left;">
la crescenta
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
la grange
</td>
<td style="text-align:left;">
la grange
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
la habra
</td>
<td style="text-align:left;">
la habra
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
la jolla
</td>
<td style="text-align:left;">
la jolla
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
la mesa
</td>
<td style="text-align:left;">
la mesa
</td>
<td style="text-align:right;">
12
</td>
</tr>
<tr>
<td style="text-align:left;">
la mirada
</td>
<td style="text-align:left;">
la mirada
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
la puente
</td>
<td style="text-align:left;">
la puente
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
la quinta
</td>
<td style="text-align:left;">
la quinta
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
la verne
</td>
<td style="text-align:left;">
la verne
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
lafayette
</td>
<td style="text-align:left;">
lafayette
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
laguna beach
</td>
<td style="text-align:left;">
laguna beach
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
laguna hills
</td>
<td style="text-align:left;">
laguna hills
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
laguna niguel
</td>
<td style="text-align:left;">
laguna niguel
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
laguna woods
</td>
<td style="text-align:left;">
laguna woods
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
lake arrowhead
</td>
<td style="text-align:left;">
lake arrowhead
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
lake elsinore
</td>
<td style="text-align:left;">
lake elsinore
</td>
<td style="text-align:right;">
122
</td>
</tr>
<tr>
<td style="text-align:left;">
lake forest
</td>
<td style="text-align:left;">
lake forest
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
lake isabella
</td>
<td style="text-align:left;">
lake isabella
</td>
<td style="text-align:right;">
13
</td>
</tr>
<tr>
<td style="text-align:left;">
lakeport
</td>
<td style="text-align:left;">
lakeport
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
lakeside
</td>
<td style="text-align:left;">
lakeside
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
lakeview
</td>
<td style="text-align:left;">
lakeview
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
lakewood
</td>
<td style="text-align:left;">
lakewood
</td>
<td style="text-align:right;">
14
</td>
</tr>
<tr>
<td style="text-align:left;">
lamont
</td>
<td style="text-align:left;">
lamont
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
lancaster
</td>
<td style="text-align:left;">
lancaster
</td>
<td style="text-align:right;">
91
</td>
</tr>
<tr>
<td style="text-align:left;">
landers
</td>
<td style="text-align:left;">
landers
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
larkspur
</td>
<td style="text-align:left;">
larkspur
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
lawndale
</td>
<td style="text-align:left;">
lawndale
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
le grand
</td>
<td style="text-align:left;">
le grand
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
lebec
</td>
<td style="text-align:left;">
lebec
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
lee vining
</td>
<td style="text-align:left;">
lee vining
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
lemon grove
</td>
<td style="text-align:left;">
lemon grove
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
lemoore
</td>
<td style="text-align:left;">
lemoore
</td>
<td style="text-align:right;">
34
</td>
</tr>
<tr>
<td style="text-align:left;">
lewiston
</td>
<td style="text-align:left;">
lewiston
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
lincoln
</td>
<td style="text-align:left;">
lincoln
</td>
<td style="text-align:right;">
17
</td>
</tr>
<tr>
<td style="text-align:left;">
lincoln heights
</td>
<td style="text-align:left;">
lincoln heights
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
littlerock
</td>
<td style="text-align:left;">
littlerock
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
livermore
</td>
<td style="text-align:left;">
livermore
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
lodi
</td>
<td style="text-align:left;">
lodi
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
loleta
</td>
<td style="text-align:left;">
loleta
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
loma linda
</td>
<td style="text-align:left;">
loma linda
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
lomita
</td>
<td style="text-align:left;">
lomita
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
lompoc
</td>
<td style="text-align:left;">
lompoc
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
lone pine
</td>
<td style="text-align:left;">
lone pine
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
long beach
</td>
<td style="text-align:left;">
long beach
</td>
<td style="text-align:right;">
205
</td>
</tr>
<tr>
<td style="text-align:left;">
loomis
</td>
<td style="text-align:left;">
loomis
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
los angeles
</td>
<td style="text-align:left;">
los angeles
</td>
<td style="text-align:right;">
851
</td>
</tr>
<tr>
<td style="text-align:left;">
los angeles ca
</td>
<td style="text-align:left;">
los angeles
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
los angeles california
</td>
<td style="text-align:left;">
los angeles
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
los banos
</td>
<td style="text-align:left;">
los banos
</td>
<td style="text-align:right;">
19
</td>
</tr>
<tr>
<td style="text-align:left;">
los gatos
</td>
<td style="text-align:left;">
los gatos
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
los molinos
</td>
<td style="text-align:left;">
los molinos
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
los osos
</td>
<td style="text-align:left;">
los osos
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
lost hills
</td>
<td style="text-align:left;">
lost hills
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
lower lake
</td>
<td style="text-align:left;">
lower lake
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
lucerne valley
</td>
<td style="text-align:left;">
lucerne valley
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
lynwood
</td>
<td style="text-align:left;">
lynwood
</td>
<td style="text-align:right;">
20
</td>
</tr>
<tr>
<td style="text-align:left;">
madera
</td>
<td style="text-align:left;">
madera
</td>
<td style="text-align:right;">
47
</td>
</tr>
<tr>
<td style="text-align:left;">
madera ranchos
</td>
<td style="text-align:left;">
madera ranchos
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
magalia
</td>
<td style="text-align:left;">
magalia
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
male
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
35
</td>
</tr>
<tr>
<td style="text-align:left;">
malibu
</td>
<td style="text-align:left;">
malibu
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
mammoth lakes
</td>
<td style="text-align:left;">
mammoth lakes
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
mammoth lakes, ca
</td>
<td style="text-align:left;">
mammoth lakes
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
maricopa
</td>
<td style="text-align:left;">
maricopa
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
marina del rey
</td>
<td style="text-align:left;">
marina del rey
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
mariposa
</td>
<td style="text-align:left;">
mariposa
</td>
<td style="text-align:right;">
77
</td>
</tr>
<tr>
<td style="text-align:left;">
martell
</td>
<td style="text-align:left;">
martell
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
martinez
</td>
<td style="text-align:left;">
martinez
</td>
<td style="text-align:right;">
17
</td>
</tr>
<tr>
<td style="text-align:left;">
marysville
</td>
<td style="text-align:left;">
marysville
</td>
<td style="text-align:right;">
21
</td>
</tr>
<tr>
<td style="text-align:left;">
marysville, ca 95901
</td>
<td style="text-align:left;">
marysville
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
maxwell
</td>
<td style="text-align:left;">
maxwell
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
maywood
</td>
<td style="text-align:left;">
maywood
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
mcfarland
</td>
<td style="text-align:left;">
mcfarland
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
mcgill
</td>
<td style="text-align:left;">
mcgill
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
mckinleyville
</td>
<td style="text-align:left;">
mckinleyville
</td>
<td style="text-align:right;">
17
</td>
</tr>
<tr>
<td style="text-align:left;">
mecca
</td>
<td style="text-align:left;">
mecca
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
menifee
</td>
<td style="text-align:left;">
menifee
</td>
<td style="text-align:right;">
66
</td>
</tr>
<tr>
<td style="text-align:left;">
mentone
</td>
<td style="text-align:left;">
mentone
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
merced
</td>
<td style="text-align:left;">
merced
</td>
<td style="text-align:right;">
87
</td>
</tr>
<tr>
<td style="text-align:left;">
midpines
</td>
<td style="text-align:left;">
midpines
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
mill valley
</td>
<td style="text-align:left;">
mill valley
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
millbrae
</td>
<td style="text-align:left;">
millbrae
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
mills hospital in burlingame
</td>
<td style="text-align:left;">
burlingame
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
milpitas
</td>
<td style="text-align:left;">
milpitas
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
mira loma
</td>
<td style="text-align:left;">
mira loma
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
missing
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
mission hills
</td>
<td style="text-align:left;">
mission hills
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
modesto
</td>
<td style="text-align:left;">
modesto
</td>
<td style="text-align:right;">
61
</td>
</tr>
<tr>
<td style="text-align:left;">
mojave
</td>
<td style="text-align:left;">
mojave
</td>
<td style="text-align:right;">
14
</td>
</tr>
<tr>
<td style="text-align:left;">
monrovia
</td>
<td style="text-align:left;">
monrovia
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
montclair
</td>
<td style="text-align:left;">
montclair
</td>
<td style="text-align:right;">
23
</td>
</tr>
<tr>
<td style="text-align:left;">
monte rio
</td>
<td style="text-align:left;">
monte rio
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
montebello
</td>
<td style="text-align:left;">
montebello
</td>
<td style="text-align:right;">
16
</td>
</tr>
<tr>
<td style="text-align:left;">
monterey
</td>
<td style="text-align:left;">
monterey
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
monterey park
</td>
<td style="text-align:left;">
monterey park
</td>
<td style="text-align:right;">
12
</td>
</tr>
<tr>
<td style="text-align:left;">
monti rio
</td>
<td style="text-align:left;">
monti rio
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
moorpark
</td>
<td style="text-align:left;">
moorpark
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
moren
</td>
<td style="text-align:left;">
moren
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
moreno valley
</td>
<td style="text-align:left;">
moreno valley
</td>
<td style="text-align:right;">
200
</td>
</tr>
<tr>
<td style="text-align:left;">
morgan hill
</td>
<td style="text-align:left;">
morgan hill
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
morongo valley
</td>
<td style="text-align:left;">
morongo valley
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
morro bay
</td>
<td style="text-align:left;">
morro bay
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
mount mesa
</td>
<td style="text-align:left;">
mount mesa
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
mountain center
</td>
<td style="text-align:left;">
mountain center
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
mountain view
</td>
<td style="text-align:left;">
mountain view
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
mountianview
</td>
<td style="text-align:left;">
mountianview
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
murreta
</td>
<td style="text-align:left;">
murrieta
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
murrieta
</td>
<td style="text-align:left;">
murrieta
</td>
<td style="text-align:right;">
67
</td>
</tr>
<tr>
<td style="text-align:left;">
muscoy
</td>
<td style="text-align:left;">
muscoy
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
napa
</td>
<td style="text-align:left;">
napa
</td>
<td style="text-align:right;">
16
</td>
</tr>
<tr>
<td style="text-align:left;">
national city
</td>
<td style="text-align:left;">
national city
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
needles
</td>
<td style="text-align:left;">
needles
</td>
<td style="text-align:right;">
29
</td>
</tr>
<tr>
<td style="text-align:left;">
nevada city
</td>
<td style="text-align:left;">
nevada city
</td>
<td style="text-align:right;">
56
</td>
</tr>
<tr>
<td style="text-align:left;">
newark
</td>
<td style="text-align:left;">
newark
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
newberry springs
</td>
<td style="text-align:left;">
newberry springs
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
newbury park
</td>
<td style="text-align:left;">
newbury park
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
newcastle
</td>
<td style="text-align:left;">
newcastle
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
newhall
</td>
<td style="text-align:left;">
newhall
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
newman
</td>
<td style="text-align:left;">
newman
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
newport beach
</td>
<td style="text-align:left;">
newport beach
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
nicasio
</td>
<td style="text-align:left;">
nicasio
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
nipomo
</td>
<td style="text-align:left;">
nipomo
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
norco
</td>
<td style="text-align:left;">
norco
</td>
<td style="text-align:right;">
28
</td>
</tr>
<tr>
<td style="text-align:left;">
north highlands
</td>
<td style="text-align:left;">
north highlands
</td>
<td style="text-align:right;">
29
</td>
</tr>
<tr>
<td style="text-align:left;">
north hills
</td>
<td style="text-align:left;">
north hills
</td>
<td style="text-align:right;">
14
</td>
</tr>
<tr>
<td style="text-align:left;">
north hollywood
</td>
<td style="text-align:left;">
north hollywood
</td>
<td style="text-align:right;">
32
</td>
</tr>
<tr>
<td style="text-align:left;">
north palm springs
</td>
<td style="text-align:left;">
palm springs
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
north san juan
</td>
<td style="text-align:left;">
north san juan
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
north side of parking lot
</td>
<td style="text-align:left;">
north side of parking lot
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
northridge
</td>
<td style="text-align:left;">
northridge
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
norwalk
</td>
<td style="text-align:left;">
norwalk
</td>
<td style="text-align:right;">
29
</td>
</tr>
<tr>
<td style="text-align:left;">
novato
</td>
<td style="text-align:left;">
novato
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
nuevo
</td>
<td style="text-align:left;">
nuevo
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
oak view
</td>
<td style="text-align:left;">
oak view
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
oakdale
</td>
<td style="text-align:left;">
oakdale
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
oakhurst
</td>
<td style="text-align:left;">
oakhurst
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
oakland
</td>
<td style="text-align:left;">
oakland
</td>
<td style="text-align:right;">
84
</td>
</tr>
<tr>
<td style="text-align:left;">
oakley
</td>
<td style="text-align:left;">
oakley
</td>
<td style="text-align:right;">
13
</td>
</tr>
<tr>
<td style="text-align:left;">
oceanside
</td>
<td style="text-align:left;">
oceanside
</td>
<td style="text-align:right;">
19
</td>
</tr>
<tr>
<td style="text-align:left;">
ojai
</td>
<td style="text-align:left;">
ojai
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
olema
</td>
<td style="text-align:left;">
olema
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
olivehurst
</td>
<td style="text-align:left;">
olivehurst
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
ontario
</td>
<td style="text-align:left;">
ontario
</td>
<td style="text-align:right;">
74
</td>
</tr>
<tr>
<td style="text-align:left;">
onyx
</td>
<td style="text-align:left;">
onyx
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
orange
</td>
<td style="text-align:left;">
orange
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
orange cove
</td>
<td style="text-align:left;">
orange cove
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
oregon house
</td>
<td style="text-align:left;">
oregon house
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
orick
</td>
<td style="text-align:left;">
orick
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
orinda
</td>
<td style="text-align:left;">
orinda
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
orland
</td>
<td style="text-align:left;">
orland
</td>
<td style="text-align:right;">
12
</td>
</tr>
<tr>
<td style="text-align:left;">
oroville
</td>
<td style="text-align:left;">
oroville
</td>
<td style="text-align:right;">
24
</td>
</tr>
<tr>
<td style="text-align:left;">
oxnard
</td>
<td style="text-align:left;">
oxnard
</td>
<td style="text-align:right;">
31
</td>
</tr>
<tr>
<td style="text-align:left;">
pachecco
</td>
<td style="text-align:left;">
pacheco
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
pacheco
</td>
<td style="text-align:left;">
pacheco
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
pacoima
</td>
<td style="text-align:left;">
pacoima
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
pala
</td>
<td style="text-align:left;">
pala
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
palm desert
</td>
<td style="text-align:left;">
palm desert
</td>
<td style="text-align:right;">
79
</td>
</tr>
<tr>
<td style="text-align:left;">
palm dprings
</td>
<td style="text-align:left;">
palm springs
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
palm spring
</td>
<td style="text-align:left;">
palm springs
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
palm springs
</td>
<td style="text-align:left;">
palm springs
</td>
<td style="text-align:right;">
380
</td>
</tr>
<tr>
<td style="text-align:left;">
palmdale
</td>
<td style="text-align:left;">
palmdale
</td>
<td style="text-align:right;">
40
</td>
</tr>
<tr>
<td style="text-align:left;">
palms springs
</td>
<td style="text-align:left;">
palm springs
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
palo alto
</td>
<td style="text-align:left;">
palo alto
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
panorama city
</td>
<td style="text-align:left;">
panorama city
</td>
<td style="text-align:right;">
16
</td>
</tr>
<tr>
<td style="text-align:left;">
paradise
</td>
<td style="text-align:left;">
paradise
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
paramount
</td>
<td style="text-align:left;">
paramount
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
parlier
</td>
<td style="text-align:left;">
parlier
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
pasadena
</td>
<td style="text-align:left;">
pasadena
</td>
<td style="text-align:right;">
19
</td>
</tr>
<tr>
<td style="text-align:left;">
paso robles
</td>
<td style="text-align:left;">
paso robles
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
patterson
</td>
<td style="text-align:left;">
patterson
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
pauma valley
</td>
<td style="text-align:left;">
pauma valley
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
pearblossom
</td>
<td style="text-align:left;">
pearblossom
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
penn valley
</td>
<td style="text-align:left;">
penn valley
</td>
<td style="text-align:right;">
16
</td>
</tr>
<tr>
<td style="text-align:left;">
perris
</td>
<td style="text-align:left;">
perris
</td>
<td style="text-align:right;">
108
</td>
</tr>
<tr>
<td style="text-align:left;">
petaluma
</td>
<td style="text-align:left;">
petaluma
</td>
<td style="text-align:right;">
28
</td>
</tr>
<tr>
<td style="text-align:left;">
phillipsville
</td>
<td style="text-align:left;">
phillipsville
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
pico rivera
</td>
<td style="text-align:left;">
pico rivera
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
piedmont
</td>
<td style="text-align:left;">
piedmont
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
pine grove
</td>
<td style="text-align:left;">
pine grove
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
pinole
</td>
<td style="text-align:left;">
pinole
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
pinon hills
</td>
<td style="text-align:left;">
pinon hills
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
pioneer
</td>
<td style="text-align:left;">
pioneer
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
pismo beach
</td>
<td style="text-align:left;">
pismo beach
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
pittsburg
</td>
<td style="text-align:left;">
pittsburg
</td>
<td style="text-align:right;">
37
</td>
</tr>
<tr>
<td style="text-align:left;">
pittsburg, ca
</td>
<td style="text-align:left;">
pittsburg
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
placentia
</td>
<td style="text-align:left;">
placentia
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
planada
</td>
<td style="text-align:left;">
planada
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
playa del rey
</td>
<td style="text-align:left;">
playa del rey
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
playa vista
</td>
<td style="text-align:left;">
playa vista
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
pleasant hill
</td>
<td style="text-align:left;">
pleasant hill
</td>
<td style="text-align:right;">
16
</td>
</tr>
<tr>
<td style="text-align:left;">
pleasanton
</td>
<td style="text-align:left;">
pleasanton
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
point arena
</td>
<td style="text-align:left;">
point arena
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
pomona
</td>
<td style="text-align:left;">
pomona
</td>
<td style="text-align:right;">
44
</td>
</tr>
<tr>
<td style="text-align:left;">
port hueneme
</td>
<td style="text-align:left;">
port hueneme
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
porterville
</td>
<td style="text-align:left;">
porterville
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
portola, ca
</td>
<td style="text-align:left;">
portola
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
poway
</td>
<td style="text-align:left;">
poway
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
pt. reyes
</td>
<td style="text-align:left;">
pt. reyes
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
quincy, ca
</td>
<td style="text-align:left;">
quincy
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
ramona
</td>
<td style="text-align:left;">
ramona
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
ranch mirage
</td>
<td style="text-align:left;">
rancho mirage
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
rancho
</td>
<td style="text-align:left;">
rancho
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
rancho bernardo
</td>
<td style="text-align:left;">
rancho bernardo
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
rancho cordova
</td>
<td style="text-align:left;">
rancho cordova
</td>
<td style="text-align:right;">
19
</td>
</tr>
<tr>
<td style="text-align:left;">
rancho cucamong
</td>
<td style="text-align:left;">
rancho cucamonga
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
rancho cucamonga
</td>
<td style="text-align:left;">
rancho cucamonga
</td>
<td style="text-align:right;">
52
</td>
</tr>
<tr>
<td style="text-align:left;">
rancho mirage
</td>
<td style="text-align:left;">
rancho mirage
</td>
<td style="text-align:right;">
41
</td>
</tr>
<tr>
<td style="text-align:left;">
rancho palos verdes
</td>
<td style="text-align:left;">
rancho palos verdes
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
rancho santa fe
</td>
<td style="text-align:left;">
rancho santa fe
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
rancho santa margarita
</td>
<td style="text-align:left;">
rancho santa margarita
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
red bluff
</td>
<td style="text-align:left;">
red bluff
</td>
<td style="text-align:right;">
71
</td>
</tr>
<tr>
<td style="text-align:left;">
redcrest
</td>
<td style="text-align:left;">
redcrest
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
redding
</td>
<td style="text-align:left;">
redding
</td>
<td style="text-align:right;">
40
</td>
</tr>
<tr>
<td style="text-align:left;">
redlands
</td>
<td style="text-align:left;">
redlands
</td>
<td style="text-align:right;">
27
</td>
</tr>
<tr>
<td style="text-align:left;">
redondo beach
</td>
<td style="text-align:left;">
redondo beach
</td>
<td style="text-align:right;">
27
</td>
</tr>
<tr>
<td style="text-align:left;">
redway
</td>
<td style="text-align:left;">
redway
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
redwood city
</td>
<td style="text-align:left;">
redwood city
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
redwood valley
</td>
<td style="text-align:left;">
redwood valley
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
reedley
</td>
<td style="text-align:left;">
reedley
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
reseda
</td>
<td style="text-align:left;">
reseda
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
rialto
</td>
<td style="text-align:left;">
rialto
</td>
<td style="text-align:right;">
58
</td>
</tr>
<tr>
<td style="text-align:left;">
richmond
</td>
<td style="text-align:left;">
richmond
</td>
<td style="text-align:right;">
56
</td>
</tr>
<tr>
<td style="text-align:left;">
richomond
</td>
<td style="text-align:left;">
richmond
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
ridegcrest
</td>
<td style="text-align:left;">
ridgecrest
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
ridgecrest
</td>
<td style="text-align:left;">
ridgecrest
</td>
<td style="text-align:right;">
15
</td>
</tr>
<tr>
<td style="text-align:left;">
rio dell
</td>
<td style="text-align:left;">
rio dell
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
rio linda
</td>
<td style="text-align:left;">
rio linda
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
rio vista
</td>
<td style="text-align:left;">
rio vista
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
river pines
</td>
<td style="text-align:left;">
river pines
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
riverbank
</td>
<td style="text-align:left;">
riverbank
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
riverdale
</td>
<td style="text-align:left;">
riverdale
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
riverisde
</td>
<td style="text-align:left;">
riverside
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
riverside
</td>
<td style="text-align:left;">
riverside
</td>
<td style="text-align:right;">
797
</td>
</tr>
<tr>
<td style="text-align:left;">
riverside ca
</td>
<td style="text-align:left;">
riverside
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
riveside
</td>
<td style="text-align:left;">
riverside
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
rocklin
</td>
<td style="text-align:left;">
rocklin
</td>
<td style="text-align:right;">
14
</td>
</tr>
<tr>
<td style="text-align:left;">
rodeo
</td>
<td style="text-align:left;">
rodeo
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
rohnert park
</td>
<td style="text-align:left;">
rohnert park
</td>
<td style="text-align:right;">
41
</td>
</tr>
<tr>
<td style="text-align:left;">
romoland
</td>
<td style="text-align:left;">
romoland
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
rosamond
</td>
<td style="text-align:left;">
rosamond
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
rosemead
</td>
<td style="text-align:left;">
rosemead
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
roseville
</td>
<td style="text-align:left;">
roseville
</td>
<td style="text-align:right;">
86
</td>
</tr>
<tr>
<td style="text-align:left;">
rough & ready
</td>
<td style="text-align:left;">
rough and ready
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
rough and ready
</td>
<td style="text-align:left;">
rough and ready
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
rough and ready, ca 95975
</td>
<td style="text-align:left;">
rough and ready
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
rowland heights
</td>
<td style="text-align:left;">
rowland heights
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
rubidoux
</td>
<td style="text-align:left;">
rubidoux
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
running springs
</td>
<td style="text-align:left;">
running springs
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
ruth
</td>
<td style="text-align:left;">
ruth
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
sacramento
</td>
<td style="text-align:left;">
sacramento
</td>
<td style="text-align:right;">
223
</td>
</tr>
<tr>
<td style="text-align:left;">
sacrmento
</td>
<td style="text-align:left;">
sacramento
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
salton sea
</td>
<td style="text-align:left;">
salton sea
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
samoa
</td>
<td style="text-align:left;">
samoa
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
san andreas
</td>
<td style="text-align:left;">
san andreas
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
san anselmo
</td>
<td style="text-align:left;">
san anselmo
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
san berandino
</td>
<td style="text-align:left;">
san bernardino
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
san beranrdino
</td>
<td style="text-align:left;">
san bernardino
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
san bernadino
</td>
<td style="text-align:left;">
san bernardino
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
san bernardino
</td>
<td style="text-align:left;">
san bernardino
</td>
<td style="text-align:right;">
162
</td>
</tr>
<tr>
<td style="text-align:left;">
san bernardino.
</td>
<td style="text-align:left;">
san bernardino
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
san bernrdino
</td>
<td style="text-align:left;">
san bernardino
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
san bruno
</td>
<td style="text-align:left;">
san bruno
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
san clemente
</td>
<td style="text-align:left;">
san clemente
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
san diego
</td>
<td style="text-align:left;">
san diego
</td>
<td style="text-align:right;">
171
</td>
</tr>
<tr>
<td style="text-align:left;">
san diegp
</td>
<td style="text-align:left;">
san diego
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
san dimas
</td>
<td style="text-align:left;">
san dimas
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
san fernando
</td>
<td style="text-align:left;">
san fernando
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
san fracnsico
</td>
<td style="text-align:left;">
san francisco
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
san francisco
</td>
<td style="text-align:left;">
san francisco
</td>
<td style="text-align:right;">
233
</td>
</tr>
<tr>
<td style="text-align:left;">
san francisco, ca
</td>
<td style="text-align:left;">
san francisco
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
san francsico
</td>
<td style="text-align:left;">
san francisco
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
san franicsco
</td>
<td style="text-align:left;">
san francisco
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
san gabriel
</td>
<td style="text-align:left;">
san gabriel
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
san jacinto
</td>
<td style="text-align:left;">
san jacinto
</td>
<td style="text-align:right;">
104
</td>
</tr>
<tr>
<td style="text-align:left;">
san jacinto, ca
</td>
<td style="text-align:left;">
san jacinto
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
san jose
</td>
<td style="text-align:left;">
san jose
</td>
<td style="text-align:right;">
114
</td>
</tr>
<tr>
<td style="text-align:left;">
san juan capistrano
</td>
<td style="text-align:left;">
san juan capistrano
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
san leandro
</td>
<td style="text-align:left;">
san leandro
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
san lorenzo
</td>
<td style="text-align:left;">
san lorenzo
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
san luis obispo
</td>
<td style="text-align:left;">
san luis obispo
</td>
<td style="text-align:right;">
14
</td>
</tr>
<tr>
<td style="text-align:left;">
san marcos
</td>
<td style="text-align:left;">
san marcos
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
san martin
</td>
<td style="text-align:left;">
san martin
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
san mateo
</td>
<td style="text-align:left;">
san mateo
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
san pablo
</td>
<td style="text-align:left;">
san pablo
</td>
<td style="text-align:right;">
29
</td>
</tr>
<tr>
<td style="text-align:left;">
san pedro
</td>
<td style="text-align:left;">
san pedro
</td>
<td style="text-align:right;">
48
</td>
</tr>
<tr>
<td style="text-align:left;">
san rafael
</td>
<td style="text-align:left;">
san rafael
</td>
<td style="text-align:right;">
16
</td>
</tr>
<tr>
<td style="text-align:left;">
san ramon
</td>
<td style="text-align:left;">
san ramon
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
san ysidro
</td>
<td style="text-align:left;">
san ysidro
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
santa ana
</td>
<td style="text-align:left;">
santa ana
</td>
<td style="text-align:right;">
29
</td>
</tr>
<tr>
<td style="text-align:left;">
santa barbara
</td>
<td style="text-align:left;">
santa barbara
</td>
<td style="text-align:right;">
19
</td>
</tr>
<tr>
<td style="text-align:left;">
santa clara
</td>
<td style="text-align:left;">
santa clara
</td>
<td style="text-align:right;">
22
</td>
</tr>
<tr>
<td style="text-align:left;">
santa clarita
</td>
<td style="text-align:left;">
santa clarita
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
santa cruz
</td>
<td style="text-align:left;">
santa cruz
</td>
<td style="text-align:right;">
117
</td>
</tr>
<tr>
<td style="text-align:left;">
santa fe springs
</td>
<td style="text-align:left;">
santa fe springs
</td>
<td style="text-align:right;">
12
</td>
</tr>
<tr>
<td style="text-align:left;">
santa maria
</td>
<td style="text-align:left;">
santa maria
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
santa monica
</td>
<td style="text-align:left;">
santa monica
</td>
<td style="text-align:right;">
20
</td>
</tr>
<tr>
<td style="text-align:left;">
santa nella
</td>
<td style="text-align:left;">
santa nella
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
santa paula
</td>
<td style="text-align:left;">
santa paula
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
santa rosa
</td>
<td style="text-align:left;">
santa rosa
</td>
<td style="text-align:right;">
130
</td>
</tr>
<tr>
<td style="text-align:left;">
santee
</td>
<td style="text-align:left;">
santee
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
saratoga
</td>
<td style="text-align:left;">
saratoga
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
saugus
</td>
<td style="text-align:left;">
saugus
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
sausalito
</td>
<td style="text-align:left;">
sausalito
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
scotia
</td>
<td style="text-align:left;">
scotia
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
scotts valley
</td>
<td style="text-align:left;">
scotts valley
</td>
<td style="text-align:right;">
14
</td>
</tr>
<tr>
<td style="text-align:left;">
seal beach
</td>
<td style="text-align:left;">
seal beach
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
sebastopol
</td>
<td style="text-align:left;">
sebastopol
</td>
<td style="text-align:right;">
14
</td>
</tr>
<tr>
<td style="text-align:left;">
selma
</td>
<td style="text-align:left;">
selma
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
sf
</td>
<td style="text-align:left;">
san francisco
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
shafter
</td>
<td style="text-align:left;">
shafter
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
shasta lake
</td>
<td style="text-align:left;">
shasta lake
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
shelter cove
</td>
<td style="text-align:left;">
shelter cove
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
sherman oaks
</td>
<td style="text-align:left;">
sherman oaks
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
signal hill
</td>
<td style="text-align:left;">
signal hill
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
silverado
</td>
<td style="text-align:left;">
silverado
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
simi valley
</td>
<td style="text-align:left;">
simi valley
</td>
<td style="text-align:right;">
16
</td>
</tr>
<tr>
<td style="text-align:left;">
sin city
</td>
<td style="text-align:left;">
sin city
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
smartsville
</td>
<td style="text-align:left;">
smartsville
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
soda springs
</td>
<td style="text-align:left;">
soda springs
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
sonoma
</td>
<td style="text-align:left;">
sonoma
</td>
<td style="text-align:right;">
22
</td>
</tr>
<tr>
<td style="text-align:left;">
soquel
</td>
<td style="text-align:left;">
soquel
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
south dos palos
</td>
<td style="text-align:left;">
south dos palos
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
south gate
</td>
<td style="text-align:left;">
south gate
</td>
<td style="text-align:right;">
24
</td>
</tr>
<tr>
<td style="text-align:left;">
south pasadena
</td>
<td style="text-align:left;">
south pasadena
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
south san francisco
</td>
<td style="text-align:left;">
san francisco
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
spring valley
</td>
<td style="text-align:left;">
spring valley
</td>
<td style="text-align:right;">
17
</td>
</tr>
<tr>
<td style="text-align:left;">
sratford
</td>
<td style="text-align:left;">
sratford
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
stanton
</td>
<td style="text-align:left;">
stanton
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
stevinson
</td>
<td style="text-align:left;">
stevinson
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
stockton
</td>
<td style="text-align:left;">
stockton
</td>
<td style="text-align:right;">
23
</td>
</tr>
<tr>
<td style="text-align:left;">
stonyford
</td>
<td style="text-align:left;">
stonyford
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
stratford
</td>
<td style="text-align:left;">
stratford
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
strathmore
</td>
<td style="text-align:left;">
strathmore
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
studio city
</td>
<td style="text-align:left;">
studio city
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
sugar loaf
</td>
<td style="text-align:left;">
sugarloaf
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
sugarloaf
</td>
<td style="text-align:left;">
sugarloaf
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
suisun city
</td>
<td style="text-align:left;">
suisun city
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
sun city
</td>
<td style="text-align:left;">
sun city
</td>
<td style="text-align:right;">
21
</td>
</tr>
<tr>
<td style="text-align:left;">
sun valley
</td>
<td style="text-align:left;">
sun valley
</td>
<td style="text-align:right;">
12
</td>
</tr>
<tr>
<td style="text-align:left;">
sunland
</td>
<td style="text-align:left;">
sunland
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
sunnyvale
</td>
<td style="text-align:left;">
sunnyvale
</td>
<td style="text-align:right;">
28
</td>
</tr>
<tr>
<td style="text-align:left;">
susanville
</td>
<td style="text-align:left;">
susanville
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
sylmar
</td>
<td style="text-align:left;">
sylmar
</td>
<td style="text-align:right;">
18
</td>
</tr>
<tr>
<td style="text-align:left;">
taft
</td>
<td style="text-align:left;">
taft
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
tarzana
</td>
<td style="text-align:left;">
tarzana
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
tehacahpi
</td>
<td style="text-align:left;">
tehachapi
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
tehachapi
</td>
<td style="text-align:left;">
tehachapi
</td>
<td style="text-align:right;">
25
</td>
</tr>
<tr>
<td style="text-align:left;">
tehachipi ca
</td>
<td style="text-align:left;">
tehachipi
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
tehama
</td>
<td style="text-align:left;">
tehama
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
temecula
</td>
<td style="text-align:left;">
temecula
</td>
<td style="text-align:right;">
121
</td>
</tr>
<tr>
<td style="text-align:left;">
temecula, ca
</td>
<td style="text-align:left;">
temecula
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
temple city
</td>
<td style="text-align:left;">
temple city
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
the sea ranch
</td>
<td style="text-align:left;">
the sea ranch
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
thermal
</td>
<td style="text-align:left;">
thermal
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
thousand oaks
</td>
<td style="text-align:left;">
thousand oaks
</td>
<td style="text-align:right;">
13
</td>
</tr>
<tr>
<td style="text-align:left;">
thousand palms
</td>
<td style="text-align:left;">
thousand palms
</td>
<td style="text-align:right;">
13
</td>
</tr>
<tr>
<td style="text-align:left;">
tiburon
</td>
<td style="text-align:left;">
tiburon
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
tipton
</td>
<td style="text-align:left;">
tipton
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
topanga
</td>
<td style="text-align:left;">
topanga
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
topaz
</td>
<td style="text-align:left;">
topaz
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
torrance
</td>
<td style="text-align:left;">
torrance
</td>
<td style="text-align:right;">
82
</td>
</tr>
<tr>
<td style="text-align:left;">
trinidad
</td>
<td style="text-align:left;">
trinidad
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
trona
</td>
<td style="text-align:left;">
trona
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
truckee
</td>
<td style="text-align:left;">
truckee
</td>
<td style="text-align:right;">
13
</td>
</tr>
<tr>
<td style="text-align:left;">
truckee/tahoe
</td>
<td style="text-align:left;">
truckee/tahoe
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
tujunga
</td>
<td style="text-align:left;">
tujunga
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
tulare
</td>
<td style="text-align:left;">
tulare
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
turlock
</td>
<td style="text-align:left;">
turlock
</td>
<td style="text-align:right;">
18
</td>
</tr>
<tr>
<td style="text-align:left;">
turlock, ca 95380
</td>
<td style="text-align:left;">
turlock
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
tustin
</td>
<td style="text-align:left;">
tustin
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
twenty nine palms
</td>
<td style="text-align:left;">
twentynine palms
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
twenty-nine palms
</td>
<td style="text-align:left;">
twentynine palms
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
twentynine palms
</td>
<td style="text-align:left;">
twentynine palms
</td>
<td style="text-align:right;">
48
</td>
</tr>
<tr>
<td style="text-align:left;">
ukiah
</td>
<td style="text-align:left;">
ukiah
</td>
<td style="text-align:right;">
36
</td>
</tr>
<tr>
<td style="text-align:left;">
unincorporated placer county
</td>
<td style="text-align:left;">
unincorporated placer county
</td>
<td style="text-align:right;">
13
</td>
</tr>
<tr>
<td style="text-align:left;">
union city
</td>
<td style="text-align:left;">
union city
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
unk
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
upland
</td>
<td style="text-align:left;">
upland
</td>
<td style="text-align:right;">
20
</td>
</tr>
<tr>
<td style="text-align:left;">
vacaville
</td>
<td style="text-align:left;">
vacaville
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
valencia
</td>
<td style="text-align:left;">
valencia
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
vallejo
</td>
<td style="text-align:left;">
vallejo
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
valley center
</td>
<td style="text-align:left;">
valley center
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
valley village
</td>
<td style="text-align:left;">
valley village
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
van nuys
</td>
<td style="text-align:left;">
van nuys
</td>
<td style="text-align:right;">
38
</td>
</tr>
<tr>
<td style="text-align:left;">
venice
</td>
<td style="text-align:left;">
venice
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
ventura
</td>
<td style="text-align:left;">
ventura
</td>
<td style="text-align:right;">
61
</td>
</tr>
<tr>
<td style="text-align:left;">
victirville
</td>
<td style="text-align:left;">
victorville
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
victorville
</td>
<td style="text-align:left;">
victorville
</td>
<td style="text-align:right;">
72
</td>
</tr>
<tr>
<td style="text-align:left;">
vina
</td>
<td style="text-align:left;">
vina
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
visalia
</td>
<td style="text-align:left;">
visalia
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
vista
</td>
<td style="text-align:left;">
vista
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
volcano
</td>
<td style="text-align:left;">
volcano
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
walnut
</td>
<td style="text-align:left;">
walnut
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
walnut creek
</td>
<td style="text-align:left;">
walnut creek
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
walnut park
</td>
<td style="text-align:left;">
walnut park
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
wasco
</td>
<td style="text-align:left;">
wasco
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
wasco, ca
</td>
<td style="text-align:left;">
wasco
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
watsonville
</td>
<td style="text-align:left;">
watsonville
</td>
<td style="text-align:right;">
53
</td>
</tr>
<tr>
<td style="text-align:left;">
weavervile
</td>
<td style="text-align:left;">
weaverville
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
weaverville
</td>
<td style="text-align:left;">
weaverville
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
weed
</td>
<td style="text-align:left;">
weed
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
weldon
</td>
<td style="text-align:left;">
weldon
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
west covina
</td>
<td style="text-align:left;">
west covina
</td>
<td style="text-align:right;">
24
</td>
</tr>
<tr>
<td style="text-align:left;">
west hills
</td>
<td style="text-align:left;">
west hills
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
west hollywood
</td>
<td style="text-align:left;">
west hollywood
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
west sacramento
</td>
<td style="text-align:left;">
west sacramento
</td>
<td style="text-align:right;">
19
</td>
</tr>
<tr>
<td style="text-align:left;">
westminster
</td>
<td style="text-align:left;">
westminster
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
westwood
</td>
<td style="text-align:left;">
westwood
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
wheatland
</td>
<td style="text-align:left;">
wheatland
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
whitethorn
</td>
<td style="text-align:left;">
whitethorn
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
whitewater
</td>
<td style="text-align:left;">
whitewater
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
whittier
</td>
<td style="text-align:left;">
whittier
</td>
<td style="text-align:right;">
30
</td>
</tr>
<tr>
<td style="text-align:left;">
wildomar
</td>
<td style="text-align:left;">
wildomar
</td>
<td style="text-align:right;">
29
</td>
</tr>
<tr>
<td style="text-align:left;">
wildomor
</td>
<td style="text-align:left;">
wildomar
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
williams
</td>
<td style="text-align:left;">
williams
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
willits
</td>
<td style="text-align:left;">
willits
</td>
<td style="text-align:right;">
22
</td>
</tr>
<tr>
<td style="text-align:left;">
willow creek
</td>
<td style="text-align:left;">
willow creek
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
willowbrook
</td>
<td style="text-align:left;">
willowbrook
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
willows
</td>
<td style="text-align:left;">
willows
</td>
<td style="text-align:right;">
15
</td>
</tr>
<tr>
<td style="text-align:left;">
wilmington
</td>
<td style="text-align:left;">
wilmington
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
winchester
</td>
<td style="text-align:left;">
winchester
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
windsor
</td>
<td style="text-align:left;">
windsor
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
windsor hills
</td>
<td style="text-align:left;">
windsor hills
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
winnetka
</td>
<td style="text-align:left;">
winnetka
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
winters
</td>
<td style="text-align:left;">
winters
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
winton
</td>
<td style="text-align:left;">
winton
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
wofford heights
</td>
<td style="text-align:left;">
wofford heights
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
wonder valley
</td>
<td style="text-align:left;">
wonder valley
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
woodland
</td>
<td style="text-align:left;">
woodland
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
woodland hills
</td>
<td style="text-align:left;">
woodland hills
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
yermo
</td>
<td style="text-align:left;">
yermo
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
ylp
</td>
<td style="text-align:left;">
ylp
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
yorba linda
</td>
<td style="text-align:left;">
yorba linda
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
yountville
</td>
<td style="text-align:left;">
yountville
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
yreka
</td>
<td style="text-align:left;">
yreka
</td>
<td style="text-align:right;">
23
</td>
</tr>
<tr>
<td style="text-align:left;">
yuba city
</td>
<td style="text-align:left;">
yuba city
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
yuba city, ca 95992
</td>
<td style="text-align:left;">
yuba city
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
yucaipa
</td>
<td style="text-align:left;">
yucaipa
</td>
<td style="text-align:right;">
40
</td>
</tr>
<tr>
<td style="text-align:left;">
yucaipa ca 92399
</td>
<td style="text-align:left;">
yucaipa
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
yucca
</td>
<td style="text-align:left;">
yucca valley
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
yucca valley
</td>
<td style="text-align:left;">
yucca valley
</td>
<td style="text-align:right;">
92
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1500
</td>
</tr>
</tbody>
</table>

</div>

For just a check on cutting out ca:

``` r
demo_dat %>% 
  filter(str_detect(location_of_participation, ", ca| ca|,|california")) %>% 
  count(location_of_participation, location_of_participation_recode)
```

<div class="kable-table">

<table>
<thead>
<tr>
<th style="text-align:left;">
location_of_participation
</th>
<th style="text-align:left;">
location_of_participation_recode
</th>
<th style="text-align:right;">
n
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
american canyon
</td>
<td style="text-align:left;">
american canyon
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
antioch, ca
</td>
<td style="text-align:left;">
antioch
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
bakersfield ca
</td>
<td style="text-align:left;">
bakersfield
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
bakersfield, ca
</td>
<td style="text-align:left;">
bakersfield
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
benton, ca
</td>
<td style="text-align:left;">
benton
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
bridgeport, ca
</td>
<td style="text-align:left;">
bridgeport
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
california city
</td>
<td style="text-align:left;">
california city
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
chester, ca
</td>
<td style="text-align:left;">
chester
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
coleville, ca
</td>
<td style="text-align:left;">
coleville
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
corona, ca
</td>
<td style="text-align:left;">
corona
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
davis ca
</td>
<td style="text-align:left;">
davis
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
el cajon
</td>
<td style="text-align:left;">
el cajon
</td>
<td style="text-align:right;">
40
</td>
</tr>
<tr>
<td style="text-align:left;">
french camp
</td>
<td style="text-align:left;">
french camp
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
gardena ca
</td>
<td style="text-align:left;">
gardena
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
hammil valley, ca
</td>
<td style="text-align:left;">
hammil valley
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
indio, ca
</td>
<td style="text-align:left;">
indio
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
los angeles ca
</td>
<td style="text-align:left;">
los angeles
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
los angeles california
</td>
<td style="text-align:left;">
los angeles
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
mammoth lakes, ca
</td>
<td style="text-align:left;">
mammoth lakes
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
marysville, ca 95901
</td>
<td style="text-align:left;">
marysville
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
pittsburg, ca
</td>
<td style="text-align:left;">
pittsburg
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
portola, ca
</td>
<td style="text-align:left;">
portola
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
quincy, ca
</td>
<td style="text-align:left;">
quincy
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
riverside ca
</td>
<td style="text-align:left;">
riverside
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
rough and ready, ca 95975
</td>
<td style="text-align:left;">
rough and ready
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
san francisco, ca
</td>
<td style="text-align:left;">
san francisco
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
san jacinto, ca
</td>
<td style="text-align:left;">
san jacinto
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
san juan capistrano
</td>
<td style="text-align:left;">
san juan capistrano
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
tehachipi ca
</td>
<td style="text-align:left;">
tehachipi
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
temecula, ca
</td>
<td style="text-align:left;">
temecula
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
turlock, ca 95380
</td>
<td style="text-align:left;">
turlock
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
wasco, ca
</td>
<td style="text-align:left;">
wasco
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
yuba city, ca 95992
</td>
<td style="text-align:left;">
yuba city
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
yucaipa ca 92399
</td>
<td style="text-align:left;">
yucaipa
</td>
<td style="text-align:right;">
1
</td>
</tr>
</tbody>
</table>

</div>

Race_1 - replace american indian/alaskan native with american
indian/alaskan native/indigenous. Replace asian with asian/asian
american. Replace black/african american with black/african
american/african. Replace pacific islander/native hawaiian with native
hawaiian/pacific islander. Replace whte with white. Change all unknowns,
including data not collected and n/a to “unknown”.

``` r
demo_dat <- demo_dat %>% 
  mutate(race_1_recode = case_when(str_detect(race_1,"americ") ~ "american indian/alaskan native/indigenous", 
                                      str_starts(race_1, "asian") ~ "asian/asian american",
                                      str_starts(race_1, "bla") ~ "black/african american/african",
                                      str_starts(race_1, "pacific i") ~ "native hawaiian/pacific islander",
                                      str_starts(race_1, "wh") ~ "white",
                                   str_detect(race_1, "unkn|#|client|collected") ~ "unknown",
                                   is.na(race_1) ~ "unknown",
                                      
                                         TRUE ~ race_1))
```

check

``` r
demo_dat %>% 
  count(race_1, race_1_recode)
```

<div class="kable-table">

<table>
<thead>
<tr>
<th style="text-align:left;">
race_1
</th>
<th style="text-align:left;">
race_1\_recode
</th>
<th style="text-align:right;">
n
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
\#
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44252
</td>
<td style="text-align:left;">
44252
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44376
</td>
<td style="text-align:left;">
44376
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44515
</td>
<td style="text-align:left;">
44515
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44519
</td>
<td style="text-align:left;">
44519
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44600
</td>
<td style="text-align:left;">
44600
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44769
</td>
<td style="text-align:left;">
44769
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
44774
</td>
<td style="text-align:left;">
44774
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
american indian/alaskan native
</td>
<td style="text-align:left;">
american indian/alaskan native/indigenous
</td>
<td style="text-align:right;">
28
</td>
</tr>
<tr>
<td style="text-align:left;">
american indian/alaskan native/indigenous
</td>
<td style="text-align:left;">
american indian/alaskan native/indigenous
</td>
<td style="text-align:right;">
109
</td>
</tr>
<tr>
<td style="text-align:left;">
asian
</td>
<td style="text-align:left;">
asian/asian american
</td>
<td style="text-align:right;">
94
</td>
</tr>
<tr>
<td style="text-align:left;">
asian/asian american
</td>
<td style="text-align:left;">
american indian/alaskan native/indigenous
</td>
<td style="text-align:right;">
195
</td>
</tr>
<tr>
<td style="text-align:left;">
black/african american
</td>
<td style="text-align:left;">
american indian/alaskan native/indigenous
</td>
<td style="text-align:right;">
585
</td>
</tr>
<tr>
<td style="text-align:left;">
black/african american/african
</td>
<td style="text-align:left;">
american indian/alaskan native/indigenous
</td>
<td style="text-align:right;">
1109
</td>
</tr>
<tr>
<td style="text-align:left;">
client doesn’t know
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
42
</td>
</tr>
<tr>
<td style="text-align:left;">
client refused
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
34
</td>
</tr>
<tr>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
814
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
586
</td>
</tr>
<tr>
<td style="text-align:left;">
male
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
401
</td>
</tr>
<tr>
<td style="text-align:left;">
native hawaiian/pacific islander
</td>
<td style="text-align:left;">
native hawaiian/pacific islander
</td>
<td style="text-align:right;">
33
</td>
</tr>
<tr>
<td style="text-align:left;">
other
</td>
<td style="text-align:left;">
other
</td>
<td style="text-align:right;">
1292
</td>
</tr>
<tr>
<td style="text-align:left;">
pacific islander/native hawaiian
</td>
<td style="text-align:left;">
native hawaiian/pacific islander
</td>
<td style="text-align:right;">
29
</td>
</tr>
<tr>
<td style="text-align:left;">
transgender
</td>
<td style="text-align:left;">
transgender
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
unknown/not provided
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1819
</td>
</tr>
<tr>
<td style="text-align:left;">
white
</td>
<td style="text-align:left;">
white
</td>
<td style="text-align:right;">
7311
</td>
</tr>
<tr>
<td style="text-align:left;">
whte
</td>
<td style="text-align:left;">
white
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
198
</td>
</tr>
</tbody>
</table>

</div>

race_2 - doing the same as for race_1, and replacing no applicable with
not applicable, updating “unknown”. For those who put an ethnicity
response in race_2, replacing with “unknown” and updating those in the
ethnicity variable.

``` r
demo_dat <- demo_dat %>% 
  mutate(race_2_recode = case_when(str_starts(race_2,"americ") ~ "american indian/alaskan native/indigenous", 
                                      str_starts(race_2, "asian") ~ "asian/asian american",
                                      str_starts(race_2, "bla") ~ "black/african american/african",
                                      str_starts(race_2, "pacific i") ~ "native hawaiian/pacific islander",
                                      str_starts(race_2, "wh") ~ "white",
                                   str_detect(race_2, "hisp") ~ "unknown",
                                   str_detect(race_2, "unknown|unable|refused|not collected|applicable|verified|#|client|mex") ~ "unknown",
                                   is.na(race_2) ~ "unknown",
                                      
                                         TRUE ~ race_2))
```

check for race_2

``` r
demo_dat %>% 
  count(race_2, race_2_recode)
```

<div class="kable-table">

<table>
<thead>
<tr>
<th style="text-align:left;">
race_2
</th>
<th style="text-align:left;">
race_2\_recode
</th>
<th style="text-align:right;">
n
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
\#
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
american indian/alaskan native
</td>
<td style="text-align:left;">
american indian/alaskan native/indigenous
</td>
<td style="text-align:right;">
13
</td>
</tr>
<tr>
<td style="text-align:left;">
american indian/alaskan native/indigenous
</td>
<td style="text-align:left;">
american indian/alaskan native/indigenous
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
asian
</td>
<td style="text-align:left;">
asian/asian american
</td>
<td style="text-align:right;">
17
</td>
</tr>
<tr>
<td style="text-align:left;">
black/african american
</td>
<td style="text-align:left;">
black/african american/african
</td>
<td style="text-align:right;">
203
</td>
</tr>
<tr>
<td style="text-align:left;">
client doesn’t know
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
9780
</td>
</tr>
<tr>
<td style="text-align:left;">
hispanic/latino/a/spanish origin
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
mexican/chicano
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
no applicable
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
12
</td>
</tr>
<tr>
<td style="text-align:left;">
not applicable
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
458
</td>
</tr>
<tr>
<td style="text-align:left;">
not hispanic/latino
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
98
</td>
</tr>
<tr>
<td style="text-align:left;">
other
</td>
<td style="text-align:left;">
other
</td>
<td style="text-align:right;">
23
</td>
</tr>
<tr>
<td style="text-align:left;">
other hispanic/latino
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
pacific islander/native hawaiian
</td>
<td style="text-align:left;">
native hawaiian/pacific islander
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
unable to verify
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
unknown/not provided
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
912
</td>
</tr>
<tr>
<td style="text-align:left;">
verified - program staff
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
white
</td>
<td style="text-align:left;">
white
</td>
<td style="text-align:right;">
773
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
2376
</td>
</tr>
</tbody>
</table>

</div>

Ethnicity - replace all instances of different spellings or orders of
hispanic/latino/a/x to hispanic/latinx. Changing mexican/chicano, cuban,
other hispanic/latino, and puerto rican to hispanic/latinx. Also,
replace all instances of different spellings or orders of not
hispanic/latino to non-hispanic/latinx

``` r
demo_dat <- demo_dat %>% 
  mutate(ethnicity_recode = case_when(str_starts(ethnicity, "no") ~ "non-hispanic/latinx",
                                      str_detect(ethnicity,"hispa") ~ "hispanic/latinx", 
                                      str_detect(race_2, "not hispanic|no hispanic") ~ "non-hispanic/latinx",
                                      str_detect(race_2, "hispanic") ~ "hispanic/latinx",
                                      str_detect(race_2, "mex") ~ "hispanic/latinx",
                                      str_detect(ethnicity, "mex|puerto|cuban") ~ "hispanic/latinx",
                                      str_detect(ethnicity, "unknown|#|applicable|data not|client|missing|rent") ~ "unknown",
                                      is.na(ethnicity) ~ "unknown",
                                      
                                      
                                         TRUE ~ ethnicity))
```

check for ethnicity

``` r
demo_dat %>% 
  count(ethnicity, race_2, ethnicity_recode) 
```

<div class="kable-table">

<table>
<thead>
<tr>
<th style="text-align:left;">
ethnicity
</th>
<th style="text-align:left;">
race_2
</th>
<th style="text-align:left;">
ethnicity_recode
</th>
<th style="text-align:right;">
n
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
\#
</td>
<td style="text-align:left;">
\#
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
client doesn’t know
</td>
<td style="text-align:left;">
client doesn’t know
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
client doesn’t know
</td>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
836
</td>
</tr>
<tr>
<td style="text-align:left;">
client doesn’t know
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
client refused
</td>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
client refused
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
cuban
</td>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:left;">
hispanic/latinx
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
cuban
</td>
<td style="text-align:left;">
unknown/not provided
</td>
<td style="text-align:left;">
hispanic/latinx
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:left;">
american indian/alaskan native
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
12
</td>
</tr>
<tr>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:left;">
asian
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
16
</td>
</tr>
<tr>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:left;">
black/african american
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
194
</td>
</tr>
<tr>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1231
</td>
</tr>
<tr>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:left;">
not hispanic/latino
</td>
<td style="text-align:left;">
non-hispanic/latinx
</td>
<td style="text-align:right;">
54
</td>
</tr>
<tr>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:left;">
other
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
21
</td>
</tr>
<tr>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:left;">
other hispanic/latino
</td>
<td style="text-align:left;">
hispanic/latinx
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:left;">
pacific islander/native hawaiian
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:left;">
unknown/not provided
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
148
</td>
</tr>
<tr>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:left;">
white
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
597
</td>
</tr>
<tr>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
87
</td>
</tr>
<tr>
<td style="text-align:left;">
hispanic/latin(a)(o)(x)
</td>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:left;">
hispanic/latinx
</td>
<td style="text-align:right;">
1332
</td>
</tr>
<tr>
<td style="text-align:left;">
hispanic/latin(a)(o)(x)
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
hispanic/latinx
</td>
<td style="text-align:right;">
70
</td>
</tr>
<tr>
<td style="text-align:left;">
hispanic/latin(o)(a)(x)
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
hispanic/latinx
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
hispanic/latino
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
hispanic/latinx
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
hispanic/latino/a/spanish origin
</td>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:left;">
hispanic/latinx
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
hispanic/latino/a/spanish origin
</td>
<td style="text-align:left;">
unknown/not provided
</td>
<td style="text-align:left;">
hispanic/latinx
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
hispanic/latino/a/spanish origin
</td>
<td style="text-align:left;">
white
</td>
<td style="text-align:left;">
hispanic/latinx
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
mexican/chicano
</td>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:left;">
hispanic/latinx
</td>
<td style="text-align:right;">
63
</td>
</tr>
<tr>
<td style="text-align:left;">
mexican/chicano
</td>
<td style="text-align:left;">
not applicable
</td>
<td style="text-align:left;">
hispanic/latinx
</td>
<td style="text-align:right;">
14
</td>
</tr>
<tr>
<td style="text-align:left;">
mexican/chicano
</td>
<td style="text-align:left;">
unknown/not provided
</td>
<td style="text-align:left;">
hispanic/latinx
</td>
<td style="text-align:right;">
32
</td>
</tr>
<tr>
<td style="text-align:left;">
mexican/chicano
</td>
<td style="text-align:left;">
white
</td>
<td style="text-align:left;">
hispanic/latinx
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
mexican/chicano
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
hispanic/latinx
</td>
<td style="text-align:right;">
14
</td>
</tr>
<tr>
<td style="text-align:left;">
missing
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
non-hispanic/latin(a)(o)(x)
</td>
<td style="text-align:left;">
american indian/alaskan native/indigenous
</td>
<td style="text-align:left;">
non-hispanic/latinx
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
non-hispanic/latin(a)(o)(x)
</td>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:left;">
non-hispanic/latinx
</td>
<td style="text-align:right;">
4454
</td>
</tr>
<tr>
<td style="text-align:left;">
non-hispanic/latin(a)(o)(x)
</td>
<td style="text-align:left;">
white
</td>
<td style="text-align:left;">
non-hispanic/latinx
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
non-hispanic/latin(a)(o)(x)
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
non-hispanic/latinx
</td>
<td style="text-align:right;">
305
</td>
</tr>
<tr>
<td style="text-align:left;">
not hispanic/latino
</td>
<td style="text-align:left;">
american indian/alaskan native
</td>
<td style="text-align:left;">
non-hispanic/latinx
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
not hispanic/latino
</td>
<td style="text-align:left;">
asian
</td>
<td style="text-align:left;">
non-hispanic/latinx
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
not hispanic/latino
</td>
<td style="text-align:left;">
black/african american
</td>
<td style="text-align:left;">
non-hispanic/latinx
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
not hispanic/latino
</td>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:left;">
non-hispanic/latinx
</td>
<td style="text-align:right;">
1208
</td>
</tr>
<tr>
<td style="text-align:left;">
not hispanic/latino
</td>
<td style="text-align:left;">
no applicable
</td>
<td style="text-align:left;">
non-hispanic/latinx
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
not hispanic/latino
</td>
<td style="text-align:left;">
not applicable
</td>
<td style="text-align:left;">
non-hispanic/latinx
</td>
<td style="text-align:right;">
182
</td>
</tr>
<tr>
<td style="text-align:left;">
not hispanic/latino
</td>
<td style="text-align:left;">
unknown/not provided
</td>
<td style="text-align:left;">
non-hispanic/latinx
</td>
<td style="text-align:right;">
124
</td>
</tr>
<tr>
<td style="text-align:left;">
not hispanic/latino
</td>
<td style="text-align:left;">
white
</td>
<td style="text-align:left;">
non-hispanic/latinx
</td>
<td style="text-align:right;">
146
</td>
</tr>
<tr>
<td style="text-align:left;">
not hispanic/latino
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
non-hispanic/latinx
</td>
<td style="text-align:right;">
449
</td>
</tr>
<tr>
<td style="text-align:left;">
other hispanci/latino
</td>
<td style="text-align:left;">
not applicable
</td>
<td style="text-align:left;">
hispanic/latinx
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
other hispanic/latino
</td>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:left;">
hispanic/latinx
</td>
<td style="text-align:right;">
299
</td>
</tr>
<tr>
<td style="text-align:left;">
other hispanic/latino
</td>
<td style="text-align:left;">
no applicable
</td>
<td style="text-align:left;">
hispanic/latinx
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
other hispanic/latino
</td>
<td style="text-align:left;">
not applicable
</td>
<td style="text-align:left;">
hispanic/latinx
</td>
<td style="text-align:right;">
101
</td>
</tr>
<tr>
<td style="text-align:left;">
other hispanic/latino
</td>
<td style="text-align:left;">
other
</td>
<td style="text-align:left;">
hispanic/latinx
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
other hispanic/latino
</td>
<td style="text-align:left;">
unknown/not provided
</td>
<td style="text-align:left;">
hispanic/latinx
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
other hispanic/latino
</td>
<td style="text-align:left;">
white
</td>
<td style="text-align:left;">
hispanic/latinx
</td>
<td style="text-align:right;">
18
</td>
</tr>
<tr>
<td style="text-align:left;">
other hispanic/latino
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
hispanic/latinx
</td>
<td style="text-align:right;">
252
</td>
</tr>
<tr>
<td style="text-align:left;">
puerto rican
</td>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:left;">
hispanic/latinx
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
puerto rican
</td>
<td style="text-align:left;">
unknown/not provided
</td>
<td style="text-align:left;">
hispanic/latinx
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
rent leaseholder
</td>
<td style="text-align:left;">
verified - program staff
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:left;">
unable to verify
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
unknown/not provided
</td>
<td style="text-align:left;">
black/african american
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
unknown/not provided
</td>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
329
</td>
</tr>
<tr>
<td style="text-align:left;">
unknown/not provided
</td>
<td style="text-align:left;">
not applicable
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
37
</td>
</tr>
<tr>
<td style="text-align:left;">
unknown/not provided
</td>
<td style="text-align:left;">
other
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
unknown/not provided
</td>
<td style="text-align:left;">
unknown/not provided
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
385
</td>
</tr>
<tr>
<td style="text-align:left;">
unknown/not provided
</td>
<td style="text-align:left;">
white
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
unknown/not provided
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
989
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
hispanic/latino/a/spanish origin
</td>
<td style="text-align:left;">
hispanic/latinx
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
mexican/chicano
</td>
<td style="text-align:left;">
hispanic/latinx
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
no applicable
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
not applicable
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
123
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
not hispanic/latino
</td>
<td style="text-align:left;">
non-hispanic/latinx
</td>
<td style="text-align:right;">
44
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
other hispanic/latino
</td>
<td style="text-align:left;">
hispanic/latinx
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
unknown/not provided
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
210
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
white
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
197
</td>
</tr>
</tbody>
</table>

</div>

current_martial_status - replace unknowns and data not collected with
“unknown”.

### Question: How would we like to recode domestic partnership? (Lump with married?) What to do with single? (100/103 records with single are from LA) ALSO, what to do with “no”? (both from Merced, might get fixed when column shift issue is solved)

``` r
demo_dat <- demo_dat %>% 
  mutate(current_marital_recode = case_when(str_detect(current_marital_status, "unknown|#|applicable|data not") ~ "unknown",
                                            str_detect(current_marital_status, "domestic|single") ~ "flag",
                                      is.na(current_marital_status) ~ "unknown",
                                         TRUE ~ current_marital_status))
```

check for marital status

``` r
demo_dat %>% 
  count(current_marital_status, current_marital_recode)
```

<div class="kable-table">

<table>
<thead>
<tr>
<th style="text-align:left;">
current_marital_status
</th>
<th style="text-align:left;">
current_marital_recode
</th>
<th style="text-align:right;">
n
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
\#
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:right;">
25
</td>
</tr>
<tr>
<td style="text-align:left;">
1
</td>
<td style="text-align:left;">
1
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
2759
</td>
</tr>
<tr>
<td style="text-align:left;">
divorced
</td>
<td style="text-align:left;">
divorced
</td>
<td style="text-align:right;">
1909
</td>
</tr>
<tr>
<td style="text-align:left;">
domestic partnership
</td>
<td style="text-align:left;">
flag
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
gay/lesbian
</td>
<td style="text-align:left;">
gay/lesbian
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
married
</td>
<td style="text-align:left;">
married
</td>
<td style="text-align:right;">
1395
</td>
</tr>
<tr>
<td style="text-align:left;">
never married
</td>
<td style="text-align:left;">
never married
</td>
<td style="text-align:right;">
1802
</td>
</tr>
<tr>
<td style="text-align:left;">
no
</td>
<td style="text-align:left;">
no
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
not married/living with partner
</td>
<td style="text-align:left;">
not married/living with partner
</td>
<td style="text-align:right;">
1557
</td>
</tr>
<tr>
<td style="text-align:left;">
separated
</td>
<td style="text-align:left;">
separated
</td>
<td style="text-align:right;">
453
</td>
</tr>
<tr>
<td style="text-align:left;">
single
</td>
<td style="text-align:left;">
flag
</td>
<td style="text-align:right;">
104
</td>
</tr>
<tr>
<td style="text-align:left;">
straight/heterosexual
</td>
<td style="text-align:left;">
straight/heterosexual
</td>
<td style="text-align:right;">
92
</td>
</tr>
<tr>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
295
</td>
</tr>
<tr>
<td style="text-align:left;">
unknown/not provided
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
2337
</td>
</tr>
<tr>
<td style="text-align:left;">
widowed
</td>
<td style="text-align:left;">
widowed
</td>
<td style="text-align:right;">
1612
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
312
</td>
</tr>
</tbody>
</table>

</div>

sexual_orientation - changed straight to “straight/heterosexual” to
match CDSS variable. Changed all unknown or declined or missing
responses to “unknown”

### Question: do we want to lump “another sexual orientation” or “other” into something? Perhaps an “other sexual orientation” bucket?

``` r
demo_dat <- demo_dat %>% 
  mutate(sexual_orientation_recode = case_when(str_detect(sexual_orientation, "strai") ~ "straight/heterosexual",
                                               str_detect(sexual_orientation, "unknown|#|client|decline|data not") ~ "unknown",
                                               str_detect(sexual_orientation, "other") ~ "flag",
                                      is.na(sexual_orientation) ~ "unknown",
                                         TRUE ~ sexual_orientation))
```

check for sexual orientation

``` r
demo_dat %>% 
  count(sexual_orientation, sexual_orientation_recode)
```

<div class="kable-table">

<table>
<thead>
<tr>
<th style="text-align:left;">
sexual_orientation
</th>
<th style="text-align:left;">
sexual_orientation_recode
</th>
<th style="text-align:right;">
n
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
\#
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:right;">
30
</td>
</tr>
<tr>
<td style="text-align:left;">
1
</td>
<td style="text-align:left;">
1
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
99
</td>
<td style="text-align:left;">
99
</td>
<td style="text-align:right;">
21
</td>
</tr>
<tr>
<td style="text-align:left;">
another sexual orientation
</td>
<td style="text-align:left;">
flag
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
bisexual
</td>
<td style="text-align:left;">
bisexual
</td>
<td style="text-align:right;">
28
</td>
</tr>
<tr>
<td style="text-align:left;">
client doesn’t know
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1449
</td>
</tr>
<tr>
<td style="text-align:left;">
client refused
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
147
</td>
</tr>
<tr>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1939
</td>
</tr>
<tr>
<td style="text-align:left;">
decline to state
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
12
</td>
</tr>
<tr>
<td style="text-align:left;">
english
</td>
<td style="text-align:left;">
english
</td>
<td style="text-align:right;">
971
</td>
</tr>
<tr>
<td style="text-align:left;">
gay/lesbian
</td>
<td style="text-align:left;">
gay/lesbian
</td>
<td style="text-align:right;">
166
</td>
</tr>
<tr>
<td style="text-align:left;">
mandarin/cantonese
</td>
<td style="text-align:left;">
mandarin/cantonese
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
other
</td>
<td style="text-align:left;">
flag
</td>
<td style="text-align:right;">
27
</td>
</tr>
<tr>
<td style="text-align:left;">
questioning
</td>
<td style="text-align:left;">
questioning
</td>
<td style="text-align:right;">
33
</td>
</tr>
<tr>
<td style="text-align:left;">
spanish
</td>
<td style="text-align:left;">
spanish
</td>
<td style="text-align:right;">
49
</td>
</tr>
<tr>
<td style="text-align:left;">
straight
</td>
<td style="text-align:left;">
straight/heterosexual
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
straight/heterosexual
</td>
<td style="text-align:left;">
straight/heterosexual
</td>
<td style="text-align:right;">
6407
</td>
</tr>
<tr>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
253
</td>
</tr>
<tr>
<td style="text-align:left;">
unknown/not provided
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
2902
</td>
</tr>
<tr>
<td style="text-align:left;">
unknown/not provided/not provided
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
211
</td>
</tr>
</tbody>
</table>

</div>

preferred_language - leaving alone the responses that don’t appear to be
languages for now. Combining the mandarin/cantonese responses. Changing
unknowns and missings to “unknown”.

``` r
demo_dat <- demo_dat %>% 
  mutate(preferred_language_recode = case_when(str_detect(preferred_language, "mandarin") ~ "mandarin/cantonese",
                                               str_detect(preferred_language, "unknown|#|data not") ~ "unknown",
                                               is.na(preferred_language) ~ "unknown",
                                               TRUE ~ preferred_language))
```

check for preferred language

``` r
demo_dat %>% 
  count(preferred_language, preferred_language_recode)
```

<div class="kable-table">

<table>
<thead>
<tr>
<th style="text-align:left;">
preferred_language
</th>
<th style="text-align:left;">
preferred_language_recode
</th>
<th style="text-align:right;">
n
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
\#
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44252
</td>
<td style="text-align:left;">
44252
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44376
</td>
<td style="text-align:left;">
44376
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44519
</td>
<td style="text-align:left;">
44519
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44600
</td>
<td style="text-align:left;">
44600
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44769
</td>
<td style="text-align:left;">
44769
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
44774
</td>
<td style="text-align:left;">
44774
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
214
</td>
</tr>
<tr>
<td style="text-align:left;">
english
</td>
<td style="text-align:left;">
english
</td>
<td style="text-align:right;">
11996
</td>
</tr>
<tr>
<td style="text-align:left;">
gay/lesbian
</td>
<td style="text-align:left;">
gay/lesbian
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
korean
</td>
<td style="text-align:left;">
korean
</td>
<td style="text-align:right;">
16
</td>
</tr>
<tr>
<td style="text-align:left;">
mandarin-cantonese
</td>
<td style="text-align:left;">
mandarin/cantonese
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
mandarin/cantonese
</td>
<td style="text-align:left;">
mandarin/cantonese
</td>
<td style="text-align:right;">
17
</td>
</tr>
<tr>
<td style="text-align:left;">
no
</td>
<td style="text-align:left;">
no
</td>
<td style="text-align:right;">
944
</td>
</tr>
<tr>
<td style="text-align:left;">
other
</td>
<td style="text-align:left;">
other
</td>
<td style="text-align:right;">
193
</td>
</tr>
<tr>
<td style="text-align:left;">
spanish
</td>
<td style="text-align:left;">
spanish
</td>
<td style="text-align:right;">
892
</td>
</tr>
<tr>
<td style="text-align:left;">
straight/heterosexual
</td>
<td style="text-align:left;">
straight/heterosexual
</td>
<td style="text-align:right;">
41
</td>
</tr>
<tr>
<td style="text-align:left;">
tagalog
</td>
<td style="text-align:left;">
tagalog
</td>
<td style="text-align:right;">
17
</td>
</tr>
<tr>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
30
</td>
</tr>
<tr>
<td style="text-align:left;">
unknown/not provided
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
18
</td>
</tr>
<tr>
<td style="text-align:left;">
vietnamese
</td>
<td style="text-align:left;">
vietnamese
</td>
<td style="text-align:right;">
12
</td>
</tr>
<tr>
<td style="text-align:left;">
yes
</td>
<td style="text-align:left;">
yes
</td>
<td style="text-align:right;">
74
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
215
</td>
</tr>
</tbody>
</table>

</div>

veteran_status - combining client refused, unknown, missing, unable to
verify, verified by staff, and “b” into “unknown”. Leaving the response
“none” for now to see if it’s a column shift issue, will revisit.

### Question: I’m guessing “none” should be lumped with “no”?

``` r
demo_dat <- demo_dat %>% 
  mutate(veteran_status_recode = case_when(str_detect(veteran_status, "client|unknown|#|verif|b|data not|n/a") ~ "unknown",
                                           is.na(veteran_status) ~ "unknown",
                                           TRUE ~ veteran_status))
```

check for veteran status

``` r
demo_dat %>% 
  count(veteran_status, veteran_status_recode)
```

<div class="kable-table">

<table>
<thead>
<tr>
<th style="text-align:left;">
veteran_status
</th>
<th style="text-align:left;">
veteran_status_recode
</th>
<th style="text-align:right;">
n
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
b
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
client doesn’t know
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
766
</td>
</tr>
<tr>
<td style="text-align:left;">
client refused
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1803
</td>
</tr>
<tr>
<td style="text-align:left;">
english
</td>
<td style="text-align:left;">
english
</td>
<td style="text-align:right;">
70
</td>
</tr>
<tr>
<td style="text-align:left;">
n/a
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
no
</td>
<td style="text-align:left;">
no
</td>
<td style="text-align:right;">
8195
</td>
</tr>
<tr>
<td style="text-align:left;">
none
</td>
<td style="text-align:left;">
none
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
unable to verify
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1634
</td>
</tr>
<tr>
<td style="text-align:left;">
verified - program staff
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
yes
</td>
<td style="text-align:left;">
yes
</td>
<td style="text-align:right;">
1452
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
755
</td>
</tr>
</tbody>
</table>

</div>

medi_cal - combining the unknowns into one “unknown”

``` r
demo_dat <- demo_dat %>% 
  mutate(medi_cal_recode = case_when(str_detect(medi_cal, "client|unknown|#|data not") ~ "unknown",
                                           is.na(medi_cal) ~ "unknown",
                                           TRUE ~ medi_cal))
```

check medi-cal

``` r
demo_dat %>% 
  count(medi_cal, medi_cal_recode)
```

<div class="kable-table">

<table>
<thead>
<tr>
<th style="text-align:left;">
medi_cal
</th>
<th style="text-align:left;">
medi_cal_recode
</th>
<th style="text-align:right;">
n
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
client doesn’t know
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
5982
</td>
</tr>
<tr>
<td style="text-align:left;">
no
</td>
<td style="text-align:left;">
no
</td>
<td style="text-align:right;">
1216
</td>
</tr>
<tr>
<td style="text-align:left;">
rent leaseholder
</td>
<td style="text-align:left;">
rent leaseholder
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1289
</td>
</tr>
<tr>
<td style="text-align:left;">
yes
</td>
<td style="text-align:left;">
yes
</td>
<td style="text-align:right;">
4084
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
2113
</td>
</tr>
</tbody>
</table>

</div>

doing the same for medicare

``` r
demo_dat <- demo_dat %>% 
  mutate(medicare_recode = case_when(str_detect(medicare, "client|unknown|#|data not|n/a") ~ "unknown",
                                           is.na(medicare) ~ "unknown",
                                           TRUE ~ medicare))
```

check for medicare

``` r
demo_dat %>% 
  count(medicare, medicare_recode)
```

<div class="kable-table">

<table>
<thead>
<tr>
<th style="text-align:left;">
medicare
</th>
<th style="text-align:left;">
medicare_recode
</th>
<th style="text-align:right;">
n
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
client doesn’t know
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
6504
</td>
</tr>
<tr>
<td style="text-align:left;">
n/a
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
no
</td>
<td style="text-align:left;">
no
</td>
<td style="text-align:right;">
1873
</td>
</tr>
<tr>
<td style="text-align:left;">
rent leaseholder
</td>
<td style="text-align:left;">
rent leaseholder
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1273
</td>
</tr>
<tr>
<td style="text-align:left;">
yes
</td>
<td style="text-align:left;">
yes
</td>
<td style="text-align:right;">
2900
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
2136
</td>
</tr>
</tbody>
</table>

</div>

representative payee or conservator: data is real messy, seems to have
many values that shouldn’t be in this column. Will hold off until new
dataset comes in to assess how much is a column shift issue.

``` r
demo_dat %>% 
  count(representative_payee_or_conservator)
```

<div class="kable-table">

<table>
<thead>
<tr>
<th style="text-align:left;">
representative_payee_or_conservator
</th>
<th style="text-align:right;">
n
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
0
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
0.0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
22.3
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
23.2
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
24.5
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
25.8
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
26.1
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
26.3
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
26.4
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
28.0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
28.1
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
28.6
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
29.3
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
29.4
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
29.9
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
2no5no
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
30.0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
30.6
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
32.4
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
32.6
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
32.9
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
33.2
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
33.4
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
33.9
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
34.0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
34.4
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
35.0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
35.1
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
35.4
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
36.4
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
36.6
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
36.7
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
37.8
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
38.0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
38.3
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
38.5
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
39.1
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
39.4
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
39.8
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
39.9
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
40.0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
40.4
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
40.8
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
40.9
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
41.0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
41.2
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
41.4
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
41.5
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
41.6
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
41.9
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
42.7
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
42.9
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43.2
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43.7
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44.1
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
46.4
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
46.5
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
46.7
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
46.8
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
46.9
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
47.4
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
47.6
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
47.7
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
48.1
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
48.2
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
48.9
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
49.1
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
49.3
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
49.5
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
49.8
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
49.9
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
50.0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
50.2
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
51.0
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
51.3
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
51.4
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
51.5
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
51.6
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
52.0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
52.2
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
52.3
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
52.6
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
52.7
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
52.8
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
52.9
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
53.0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
53.2
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
53.3
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
53.4
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
53.5
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
53.6
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
53.7
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
54.0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
54.2
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
54.5
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
54.7
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
54.9
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
55.0
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
55.3
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
55.4
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
55.5
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
55.7
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
55.8
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
55.9
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
56.0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
56.2
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
56.4
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
56.5
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
56.6
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
56.7
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
57.1
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
57.3
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
57.5
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
57.6
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
57.7
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
57.8
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
58.0
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
58.1
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
58.3
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
58.5
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
58.8
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
59.1
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
59.2
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
59.5
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
59.6
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
59.7
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
59.8
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
59.9
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
60.0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
60.1
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
60.2
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
60.4
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
60.7
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
60.8
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
61.0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
61.2
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
61.4
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
61.5
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
61.6
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
61.7
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
61.8
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
62.0
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
62.1
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
62.2
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
62.3
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
62.4
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
62.5
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
62.7
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
62.8
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
63.0
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
63.1
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
63.2
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
63.4
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
63.5
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
63.6
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
63.8
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
63.9
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
64.0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
64.1
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
64.2
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
64.3
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
64.4
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
64.5
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
64.6
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
64.7
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
64.8
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
64.9
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
65.0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
65.1
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
65.2
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
65.3
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
65.4
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
65.5
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
65.6
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
65.7
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
65.9
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
66.1
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
66.3
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
66.4
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
66.5
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
66.6
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
66.7
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
66.8
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
66.9
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
67.0
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
67.1
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
67.2
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
67.3
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
67.5
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
67.6
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
67.8
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
67.9
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
68.0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
68.1
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
68.2
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
68.3
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
68.4
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
68.5
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
68.6
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
68.7
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
68.8
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
69.1
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
69.2
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
69.3
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
69.5
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
69.6
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
69.7
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
69.8
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
69.9
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
70.1
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
70.2
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
70.3
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
70.6
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
70.7
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
70.8
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
70.9
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
71.0
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
71.1
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
71.2
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
71.3
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
71.4
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
71.5
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
71.6
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
71.7
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
71.8
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
72.2
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
72.5
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
72.6
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
72.8
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
73.1
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
73.3
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
73.4
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
73.5
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
73.9
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
74.1
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
74.4
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
74.6
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
75.0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
75.4
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
75.6
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
75.7
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
75.9
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
76.3
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
76.4
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
76.5
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
76.6
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
76.8
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
77.2
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
77.6
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
78.0
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
78.1
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
78.6
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
78.7
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
78.9
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
79.0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
79.6
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
79.7
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
80.0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
80.3
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
80.4
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
80.5
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
80.8
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
81.6
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
82.0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
82.2
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
82.3
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
82.5
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
82.9
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
83.1
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
84.6
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
85.1
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
85.5
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
85.8
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
86.7
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
86.8
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
87.4
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:right;">
7146
</td>
</tr>
<tr>
<td style="text-align:left;">
homeless sheltered
</td>
<td style="text-align:right;">
129
</td>
</tr>
<tr>
<td style="text-align:left;">
homeless unsheltered
</td>
<td style="text-align:right;">
157
</td>
</tr>
<tr>
<td style="text-align:left;">
hotel no rights
</td>
<td style="text-align:right;">
95
</td>
</tr>
<tr>
<td style="text-align:left;">
hotel with rights
</td>
<td style="text-align:right;">
19
</td>
</tr>
<tr>
<td style="text-align:left;">
n/a
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
no
</td>
<td style="text-align:right;">
2850
</td>
</tr>
<tr>
<td style="text-align:left;">
none
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
owner lives alone
</td>
<td style="text-align:right;">
112
</td>
</tr>
<tr>
<td style="text-align:left;">
owner with others no rent
</td>
<td style="text-align:right;">
51
</td>
</tr>
<tr>
<td style="text-align:left;">
owner with others rent
</td>
<td style="text-align:right;">
30
</td>
</tr>
<tr>
<td style="text-align:left;">
rent leaseholder
</td>
<td style="text-align:right;">
201
</td>
</tr>
<tr>
<td style="text-align:left;">
residential care facility
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
skilled nursing facility
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
854
</td>
</tr>
<tr>
<td style="text-align:left;">
with others no rent
</td>
<td style="text-align:right;">
63
</td>
</tr>
<tr>
<td style="text-align:left;">
with others rent
</td>
<td style="text-align:right;">
186
</td>
</tr>
<tr>
<td style="text-align:left;">
yes
</td>
<td style="text-align:right;">
195
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
2158
</td>
</tr>
</tbody>
</table>

</div>

living situation upon entry - changing the numbers to unknown for now,
I’m guessing many of them are rent amount, but will wait for column
shift fix to verify. Matching responses with dropdown options: combining
all “homeless” responses into “homeless.” Recoding “hotel” as temporary
housing. Changing rent by client, rental by client, etc and “lease
holder” into “rent leaseholder”. Recoding descriptions of owner, whether
living alone or with others, as “owner” (excluding the records where it
says rental by owner).Changing “programpermanent-residential program” to
“permanent-residential program” as that seems like a typo. Recoding
“hospital facility” to “other”. Combining unknown, n/a, data not
collected as unknown.

\###Questions for the group: “hotel with rights” - I’m thinking this
shouldn’t be temporary housing, what do you all think? “with others no
rent”/“with others” - Is this akin to “staying with friends/family on a
temp basis (temporary housing), is it”other permanent housing”? “board
and care facility”/“residential care facility”/SNF - in the dropdown,
these are in both temporary- residential program and permanent-
residential program options. “rent by owner” - is this rent leaseholder?
(that is where I put it for now) “shared housing”/“living in shared
house”/“living alone” - rent leaseholder? other permanent housing? Could
also be owner I suppose? “living with family”/“living with relative” -
this could be either temporary housing or other permanent housing..
“with others rent” - my guess is this should go under “other permanent
housing” which includes in () “renting a room without a lease?

``` r
demo_dat <- demo_dat %>% 
  mutate(living_sit_entry_recode = case_when(str_detect(living_situation_upon_entry, "1|2|3|4|5|6|7|8|9|0") ~ "unknown",
                                            str_detect(living_situation_upon_entry, "homeless|homless|unsheltered") ~ "homeless",
                                             str_detect(living_situation_upon_entry, "hotel") ~ "temporary housing",
                                             str_detect(living_situation_upon_entry, "rent by|rental by|lease holder") ~ "rent leaseholder",
                                             str_detect(living_situation_upon_entry, "owned|owner") ~ "owner",
                                             str_detect(living_situation_upon_entry, "programperm") ~ "permanent- residential program",
                                             str_detect(living_situation_upon_entry, "hospital") ~ "other",
                                            str_detect(living_situation_upon_entry, "hotel with rights|facility|shared hous|living with|live alone|with others") ~ "flag",
                                             str_detect(living_situation_upon_entry, "data not|unknown") ~ "unknown",
                                             is.na(living_situation_upon_entry) ~ "unknown",
                                          TRUE ~ living_situation_upon_entry))
```

check for living situation upon entry

``` r
demo_dat %>% 
  count(living_situation_upon_entry, living_sit_entry_recode)
```

<div class="kable-table">

<table>
<thead>
<tr>
<th style="text-align:left;">
living_situation_upon_entry
</th>
<th style="text-align:left;">
living_sit_entry_recode
</th>
<th style="text-align:right;">
n
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
345
</td>
</tr>
<tr>
<td style="text-align:left;">
1
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
1000
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
16
</td>
</tr>
<tr>
<td style="text-align:left;">
1006
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1015
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1020
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1023
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1027
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1030
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
1036
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1040
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
1050
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1058
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1070
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
1075
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1080
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1085
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1090
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1093
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1095
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1100
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
1133
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1134
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1140
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1145
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1188
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1193
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1195
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
1200
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
15
</td>
</tr>
<tr>
<td style="text-align:left;">
1210
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1211
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1230
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1238
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
1249
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1250
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
1251.78
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1263
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1275
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1293
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1295
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
1300
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
1306
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1313
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1317
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1320
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1321
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1364
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1368
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1375
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1380
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1400
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
1440
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1441
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1453
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1500
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
1506
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1553
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
156
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1570
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1600
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
1650
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
1668
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1685
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1700
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
175
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1750
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1775
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1800
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
1850
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
1862
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1900
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
1925
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1937
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1980
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1988
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
200
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
2000
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
2066
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
2100
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
215
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
216
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
224
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
2300
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
234
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
2346
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
2359
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
2400
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
247
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
250
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
2500
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
254
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
255
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
2550
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
2600
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
2700
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
280
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
2800
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
2835
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
289
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
290
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
2900
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
298
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
300
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
13
</td>
</tr>
<tr>
<td style="text-align:left;">
3000
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
305
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
308
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
3140
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
342
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
350
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
360
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
368
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
371
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
375
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
377
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
3776
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
384
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
390
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
400
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
22
</td>
</tr>
<tr>
<td style="text-align:left;">
408
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
42
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
420
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
423
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
429
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
438
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
440
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
448
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
45
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
450
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
12
</td>
</tr>
<tr>
<td style="text-align:left;">
451
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
459
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
460
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
462.5
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
470
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
473
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
475
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
482
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
485
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
490
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
495
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
496
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
50
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
500
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
66
</td>
</tr>
<tr>
<td style="text-align:left;">
532
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
543
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
550
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
13
</td>
</tr>
<tr>
<td style="text-align:left;">
560
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
561
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
567
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
568
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
572
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
575
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
577
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
580
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
581
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
590
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
596
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
598
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
600
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
30
</td>
</tr>
<tr>
<td style="text-align:left;">
607
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
610
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
614
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
616
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
618
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
621
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
625
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
635
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
637
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
639
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
650
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
15
</td>
</tr>
<tr>
<td style="text-align:left;">
652
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
654
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
657
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
658
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
660
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
668
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
675
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
680
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
682
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
685
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
687
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
689
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
690
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
694
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
695
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
696
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
700
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
51
</td>
</tr>
<tr>
<td style="text-align:left;">
705
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
712
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
723
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
725
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
750
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
31
</td>
</tr>
<tr>
<td style="text-align:left;">
756
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
757
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
760
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
765
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
768
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
775
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
776
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
780
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
786
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
788
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
791
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
795
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
798
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
800
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
47
</td>
</tr>
<tr>
<td style="text-align:left;">
817
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
823
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
825
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
830
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
841
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
844
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
850
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
875
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
880
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
896
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
900
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
22
</td>
</tr>
<tr>
<td style="text-align:left;">
909
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
919
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
925
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
935
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
940
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
950
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
954
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
955
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
975
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
978
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
board and care facility
</td>
<td style="text-align:left;">
flag
</td>
<td style="text-align:right;">
15
</td>
</tr>
<tr>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
3713
</td>
</tr>
<tr>
<td style="text-align:left;">
home owner- lives alone
</td>
<td style="text-align:left;">
owner
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
home owner- with others no rent
</td>
<td style="text-align:left;">
owner
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
homeless
</td>
<td style="text-align:left;">
homeless
</td>
<td style="text-align:right;">
1933
</td>
</tr>
<tr>
<td style="text-align:left;">
homeless sheltered
</td>
<td style="text-align:left;">
homeless
</td>
<td style="text-align:right;">
465
</td>
</tr>
<tr>
<td style="text-align:left;">
homeless unsheltered
</td>
<td style="text-align:left;">
homeless
</td>
<td style="text-align:right;">
1508
</td>
</tr>
<tr>
<td style="text-align:left;">
homless
</td>
<td style="text-align:left;">
homeless
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
hospital facility
</td>
<td style="text-align:left;">
other
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
hotel
</td>
<td style="text-align:left;">
temporary housing
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
hotel no rights
</td>
<td style="text-align:left;">
temporary housing
</td>
<td style="text-align:right;">
202
</td>
</tr>
<tr>
<td style="text-align:left;">
hotel with rights
</td>
<td style="text-align:left;">
temporary housing
</td>
<td style="text-align:right;">
40
</td>
</tr>
<tr>
<td style="text-align:left;">
lease holder
</td>
<td style="text-align:left;">
rent leaseholder
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
live alone
</td>
<td style="text-align:left;">
flag
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
living in a shared house
</td>
<td style="text-align:left;">
flag
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
living with family
</td>
<td style="text-align:left;">
flag
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
living with relative
</td>
<td style="text-align:left;">
flag
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
no
</td>
<td style="text-align:left;">
no
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
other
</td>
<td style="text-align:left;">
other
</td>
<td style="text-align:right;">
69
</td>
</tr>
<tr>
<td style="text-align:left;">
other permanent housing
</td>
<td style="text-align:left;">
other permanent housing
</td>
<td style="text-align:right;">
108
</td>
</tr>
<tr>
<td style="text-align:left;">
owned by client
</td>
<td style="text-align:left;">
owner
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
owner
</td>
<td style="text-align:left;">
owner
</td>
<td style="text-align:right;">
450
</td>
</tr>
<tr>
<td style="text-align:left;">
owner lives alone
</td>
<td style="text-align:left;">
owner
</td>
<td style="text-align:right;">
226
</td>
</tr>
<tr>
<td style="text-align:left;">
owner with others
</td>
<td style="text-align:left;">
owner
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
owner with others no rent
</td>
<td style="text-align:left;">
owner
</td>
<td style="text-align:right;">
91
</td>
</tr>
<tr>
<td style="text-align:left;">
owner with others rent
</td>
<td style="text-align:left;">
owner
</td>
<td style="text-align:right;">
62
</td>
</tr>
<tr>
<td style="text-align:left;">
permanent- residential program
</td>
<td style="text-align:left;">
permanent- residential program
</td>
<td style="text-align:right;">
18
</td>
</tr>
<tr>
<td style="text-align:left;">
programpermanent-residential program
</td>
<td style="text-align:left;">
permanent- residential program
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
rent by client
</td>
<td style="text-align:left;">
rent leaseholder
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
rent by owner
</td>
<td style="text-align:left;">
rent leaseholder
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
rent leaseholder
</td>
<td style="text-align:left;">
rent leaseholder
</td>
<td style="text-align:right;">
2217
</td>
</tr>
<tr>
<td style="text-align:left;">
rental by client
</td>
<td style="text-align:left;">
rent leaseholder
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
rental by client with ongoing subsidy
</td>
<td style="text-align:left;">
rent leaseholder
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
residential care facility
</td>
<td style="text-align:left;">
flag
</td>
<td style="text-align:right;">
44
</td>
</tr>
<tr>
<td style="text-align:left;">
shared housing
</td>
<td style="text-align:left;">
flag
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
skilled nursing facility
</td>
<td style="text-align:left;">
flag
</td>
<td style="text-align:right;">
19
</td>
</tr>
<tr>
<td style="text-align:left;">
temporary housing
</td>
<td style="text-align:left;">
temporary housing
</td>
<td style="text-align:right;">
529
</td>
</tr>
<tr>
<td style="text-align:left;">
temporary- residential program
</td>
<td style="text-align:left;">
temporary- residential program
</td>
<td style="text-align:right;">
70
</td>
</tr>
<tr>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
71
</td>
</tr>
<tr>
<td style="text-align:left;">
unsheltered
</td>
<td style="text-align:left;">
homeless
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
with others
</td>
<td style="text-align:left;">
flag
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
with others no rent
</td>
<td style="text-align:left;">
flag
</td>
<td style="text-align:right;">
408
</td>
</tr>
<tr>
<td style="text-align:left;">
with others rent
</td>
<td style="text-align:left;">
flag
</td>
<td style="text-align:right;">
816
</td>
</tr>
<tr>
<td style="text-align:left;">
yes
</td>
<td style="text-align:left;">
yes
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
554
</td>
</tr>
</tbody>
</table>

</div>

And! for a breakdown of which counties these hard to classify ones are
coming up (it’s many different ones it seems):

``` r
demo_dat %>% 
  filter(str_detect(living_sit_entry_recode, "flag")) %>% 
  count(living_situation_upon_entry, reporting_agency)
```

<div class="kable-table">

<table>
<thead>
<tr>
<th style="text-align:left;">
living_situation_upon_entry
</th>
<th style="text-align:left;">
reporting_agency
</th>
<th style="text-align:right;">
n
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
board and care facility
</td>
<td style="text-align:left;">
alameda
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
board and care facility
</td>
<td style="text-align:left;">
los angeles
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
board and care facility
</td>
<td style="text-align:left;">
placer
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
board and care facility
</td>
<td style="text-align:left;">
sacramento
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
board and care facility
</td>
<td style="text-align:left;">
san diego
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
board and care facility
</td>
<td style="text-align:left;">
santa cruz
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
board and care facility
</td>
<td style="text-align:left;">
sonoma
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
board and care facility
</td>
<td style="text-align:left;">
ventura
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
board and care facility
</td>
<td style="text-align:left;">
yuba
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
live alone
</td>
<td style="text-align:left;">
los angeles
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
living in a shared house
</td>
<td style="text-align:left;">
los angeles
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
living with family
</td>
<td style="text-align:left;">
los angeles
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
living with relative
</td>
<td style="text-align:left;">
los angeles
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
residential care facility
</td>
<td style="text-align:left;">
fresno
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
residential care facility
</td>
<td style="text-align:left;">
kern
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
residential care facility
</td>
<td style="text-align:left;">
los angeles
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
residential care facility
</td>
<td style="text-align:left;">
mendocino
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
residential care facility
</td>
<td style="text-align:left;">
merced
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
residential care facility
</td>
<td style="text-align:left;">
riverside
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
residential care facility
</td>
<td style="text-align:left;">
san bernardino
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
residential care facility
</td>
<td style="text-align:left;">
san diego
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
residential care facility
</td>
<td style="text-align:left;">
san francisco
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
residential care facility
</td>
<td style="text-align:left;">
santa clara
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
residential care facility
</td>
<td style="text-align:left;">
sonoma
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
residential care facility
</td>
<td style="text-align:left;">
tehama
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
residential care facility
</td>
<td style="text-align:left;">
ventura
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
residential care facility
</td>
<td style="text-align:left;">
yuba
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
shared housing
</td>
<td style="text-align:left;">
los angeles
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
skilled nursing facility
</td>
<td style="text-align:left;">
alameda
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
skilled nursing facility
</td>
<td style="text-align:left;">
kern
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
skilled nursing facility
</td>
<td style="text-align:left;">
madera
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
skilled nursing facility
</td>
<td style="text-align:left;">
placer
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
skilled nursing facility
</td>
<td style="text-align:left;">
riverside
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
skilled nursing facility
</td>
<td style="text-align:left;">
san bernardino
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
skilled nursing facility
</td>
<td style="text-align:left;">
san diego
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
skilled nursing facility
</td>
<td style="text-align:left;">
santa cruz
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
skilled nursing facility
</td>
<td style="text-align:left;">
ventura
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
with others
</td>
<td style="text-align:left;">
los angeles
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
with others no rent
</td>
<td style="text-align:left;">
alameda
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
with others no rent
</td>
<td style="text-align:left;">
contra costa
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
with others no rent
</td>
<td style="text-align:left;">
fresno
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
with others no rent
</td>
<td style="text-align:left;">
humboldt
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
with others no rent
</td>
<td style="text-align:left;">
kern
</td>
<td style="text-align:right;">
20
</td>
</tr>
<tr>
<td style="text-align:left;">
with others no rent
</td>
<td style="text-align:left;">
kings
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
with others no rent
</td>
<td style="text-align:left;">
los angeles
</td>
<td style="text-align:right;">
108
</td>
</tr>
<tr>
<td style="text-align:left;">
with others no rent
</td>
<td style="text-align:left;">
madera
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
with others no rent
</td>
<td style="text-align:left;">
mariposa
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
with others no rent
</td>
<td style="text-align:left;">
mendocino
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
with others no rent
</td>
<td style="text-align:left;">
merced
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
with others no rent
</td>
<td style="text-align:left;">
orange
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
with others no rent
</td>
<td style="text-align:left;">
placer
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
with others no rent
</td>
<td style="text-align:left;">
riverside
</td>
<td style="text-align:right;">
60
</td>
</tr>
<tr>
<td style="text-align:left;">
with others no rent
</td>
<td style="text-align:left;">
sacramento
</td>
<td style="text-align:right;">
43
</td>
</tr>
<tr>
<td style="text-align:left;">
with others no rent
</td>
<td style="text-align:left;">
san bernardino
</td>
<td style="text-align:right;">
64
</td>
</tr>
<tr>
<td style="text-align:left;">
with others no rent
</td>
<td style="text-align:left;">
san diego
</td>
<td style="text-align:right;">
23
</td>
</tr>
<tr>
<td style="text-align:left;">
with others no rent
</td>
<td style="text-align:left;">
san francisco
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
with others no rent
</td>
<td style="text-align:left;">
santa clara
</td>
<td style="text-align:right;">
14
</td>
</tr>
<tr>
<td style="text-align:left;">
with others no rent
</td>
<td style="text-align:left;">
santa cruz
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
with others no rent
</td>
<td style="text-align:left;">
shasta
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
with others no rent
</td>
<td style="text-align:left;">
sonoma
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
with others no rent
</td>
<td style="text-align:left;">
tehama
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
with others no rent
</td>
<td style="text-align:left;">
ventura
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
with others no rent
</td>
<td style="text-align:left;">
yuba
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
with others rent
</td>
<td style="text-align:left;">
alameda
</td>
<td style="text-align:right;">
17
</td>
</tr>
<tr>
<td style="text-align:left;">
with others rent
</td>
<td style="text-align:left;">
contra costa
</td>
<td style="text-align:right;">
35
</td>
</tr>
<tr>
<td style="text-align:left;">
with others rent
</td>
<td style="text-align:left;">
fresno
</td>
<td style="text-align:right;">
28
</td>
</tr>
<tr>
<td style="text-align:left;">
with others rent
</td>
<td style="text-align:left;">
humboldt
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
with others rent
</td>
<td style="text-align:left;">
kern
</td>
<td style="text-align:right;">
68
</td>
</tr>
<tr>
<td style="text-align:left;">
with others rent
</td>
<td style="text-align:left;">
kings
</td>
<td style="text-align:right;">
15
</td>
</tr>
<tr>
<td style="text-align:left;">
with others rent
</td>
<td style="text-align:left;">
los angeles
</td>
<td style="text-align:right;">
82
</td>
</tr>
<tr>
<td style="text-align:left;">
with others rent
</td>
<td style="text-align:left;">
madera
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
with others rent
</td>
<td style="text-align:left;">
mariposa
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
with others rent
</td>
<td style="text-align:left;">
mendocino
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
with others rent
</td>
<td style="text-align:left;">
merced
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
with others rent
</td>
<td style="text-align:left;">
nevada
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
with others rent
</td>
<td style="text-align:left;">
orange
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
with others rent
</td>
<td style="text-align:left;">
placer
</td>
<td style="text-align:right;">
14
</td>
</tr>
<tr>
<td style="text-align:left;">
with others rent
</td>
<td style="text-align:left;">
riverside
</td>
<td style="text-align:right;">
65
</td>
</tr>
<tr>
<td style="text-align:left;">
with others rent
</td>
<td style="text-align:left;">
sacramento
</td>
<td style="text-align:right;">
91
</td>
</tr>
<tr>
<td style="text-align:left;">
with others rent
</td>
<td style="text-align:left;">
san bernardino
</td>
<td style="text-align:right;">
206
</td>
</tr>
<tr>
<td style="text-align:left;">
with others rent
</td>
<td style="text-align:left;">
san diego
</td>
<td style="text-align:right;">
63
</td>
</tr>
<tr>
<td style="text-align:left;">
with others rent
</td>
<td style="text-align:left;">
san francisco
</td>
<td style="text-align:right;">
17
</td>
</tr>
<tr>
<td style="text-align:left;">
with others rent
</td>
<td style="text-align:left;">
san mateo
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
with others rent
</td>
<td style="text-align:left;">
santa clara
</td>
<td style="text-align:right;">
21
</td>
</tr>
<tr>
<td style="text-align:left;">
with others rent
</td>
<td style="text-align:left;">
santa cruz
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
with others rent
</td>
<td style="text-align:left;">
shasta
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
with others rent
</td>
<td style="text-align:left;">
sonoma
</td>
<td style="text-align:right;">
36
</td>
</tr>
<tr>
<td style="text-align:left;">
with others rent
</td>
<td style="text-align:left;">
tehama
</td>
<td style="text-align:right;">
14
</td>
</tr>
<tr>
<td style="text-align:left;">
with others rent
</td>
<td style="text-align:left;">
ventura
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
with others rent
</td>
<td style="text-align:left;">
yuba
</td>
<td style="text-align:right;">
2
</td>
</tr>
</tbody>
</table>

</div>

client homeless within the last 3 years - answers should be yes or no.
Leaving numbers alone for now. Changing 3 years or longer, within the
last year, within the last 3 years, currently homeless to “yes”.
Changing client was not homeless to “no”. Combining doesn’t know,
refused, data not collected, unknown, blank as “unknown”.

Question: one response that is “n” - I’m guessing this might be no? But
there is another response with n/a, which is what they might’ve been
going for? (changed to no for now)

``` r
demo_dat <- demo_dat %>% 
  mutate(client_homeless_last_three_years_recode = case_when(str_detect(client_homeless_within_the_last_three_years, "three years or|within the last|currently homeles") ~ "yes",
                                                  str_detect(client_homeless_within_the_last_three_years, "client was not homeles") ~ "no",
                                                  str_equal(client_homeless_within_the_last_three_years, "n") ~ "no",
                                                  str_detect(client_homeless_within_the_last_three_years, "doesn't know|refused|n/a|data not") ~ "unknown",
                                                  is.na(client_homeless_within_the_last_three_years) ~ "unknown",
                                                  TRUE ~ client_homeless_within_the_last_three_years))
```

check for homeless in last 3 years

``` r
demo_dat %>% 
  count(client_homeless_within_the_last_three_years, client_homeless_last_three_years_recode)
```

<div class="kable-table">

<table>
<thead>
<tr>
<th style="text-align:left;">
client_homeless_within_the_last_three_years
</th>
<th style="text-align:left;">
client_homeless_last_three_years_recode
</th>
<th style="text-align:right;">
n
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:right;">
28
</td>
</tr>
<tr>
<td style="text-align:left;">
1
</td>
<td style="text-align:left;">
1
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
1000
</td>
<td style="text-align:left;">
1000
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1001
</td>
<td style="text-align:left;">
1001
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1100
</td>
<td style="text-align:left;">
1100
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1200
</td>
<td style="text-align:left;">
1200
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1500
</td>
<td style="text-align:left;">
1500
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1692
</td>
<td style="text-align:left;">
1692
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
175
</td>
<td style="text-align:left;">
175
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
186
</td>
<td style="text-align:left;">
186
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
200
</td>
<td style="text-align:left;">
200
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
225
</td>
<td style="text-align:left;">
225
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
275
</td>
<td style="text-align:left;">
275
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
288
</td>
<td style="text-align:left;">
288
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
300
</td>
<td style="text-align:left;">
300
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
325
</td>
<td style="text-align:left;">
325
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
350
</td>
<td style="text-align:left;">
350
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
375
</td>
<td style="text-align:left;">
375
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
400
</td>
<td style="text-align:left;">
400
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
447
</td>
<td style="text-align:left;">
447
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
4495
</td>
<td style="text-align:left;">
4495
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
450
</td>
<td style="text-align:left;">
450
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
500
</td>
<td style="text-align:left;">
500
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
540
</td>
<td style="text-align:left;">
540
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
550
</td>
<td style="text-align:left;">
550
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
556
</td>
<td style="text-align:left;">
556
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
575
</td>
<td style="text-align:left;">
575
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
60
</td>
<td style="text-align:left;">
60
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
600
</td>
<td style="text-align:left;">
600
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
650
</td>
<td style="text-align:left;">
650
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
675
</td>
<td style="text-align:left;">
675
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
725
</td>
<td style="text-align:left;">
725
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
750
</td>
<td style="text-align:left;">
750
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
800
</td>
<td style="text-align:left;">
800
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
850
</td>
<td style="text-align:left;">
850
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
930
</td>
<td style="text-align:left;">
930
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
950
</td>
<td style="text-align:left;">
950
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
client doesn’t know
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
client refused
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
59
</td>
</tr>
<tr>
<td style="text-align:left;">
client was not homeless
</td>
<td style="text-align:left;">
no
</td>
<td style="text-align:right;">
546
</td>
</tr>
<tr>
<td style="text-align:left;">
currently homeless
</td>
<td style="text-align:left;">
yes
</td>
<td style="text-align:right;">
276
</td>
</tr>
<tr>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
5115
</td>
</tr>
<tr>
<td style="text-align:left;">
n
</td>
<td style="text-align:left;">
no
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
n/a
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
no
</td>
<td style="text-align:left;">
no
</td>
<td style="text-align:right;">
3842
</td>
</tr>
<tr>
<td style="text-align:left;">
three years or longer
</td>
<td style="text-align:left;">
yes
</td>
<td style="text-align:right;">
15
</td>
</tr>
<tr>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
685
</td>
</tr>
<tr>
<td style="text-align:left;">
within the last month
</td>
<td style="text-align:left;">
yes
</td>
<td style="text-align:right;">
48
</td>
</tr>
<tr>
<td style="text-align:left;">
within the last three years
</td>
<td style="text-align:left;">
yes
</td>
<td style="text-align:right;">
42
</td>
</tr>
<tr>
<td style="text-align:left;">
within the last year
</td>
<td style="text-align:left;">
yes
</td>
<td style="text-align:right;">
52
</td>
</tr>
<tr>
<td style="text-align:left;">
yes
</td>
<td style="text-align:left;">
yes
</td>
<td style="text-align:right;">
3499
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
417
</td>
</tr>
</tbody>
</table>

</div>

Number of times homeless in the past 3 years - changing 0 to “client was
not homeless”, 1 to “one time”, 2 to “two times”, 3 to “three times” 4
and 4+ to “four or more times”. Changing any “0-x” responses to unknown,
since it seems the exact number is not known.. Changing refused, doesn’t
know, n/a, blank to “unknown”.

### Question (Kenny, Angelica, Sara):

99 is a response with 1366 answers, I’m wondering if this is the same as
unknown? (like people were just using a very big number as a
placeholder?) Or just a very large number of times someone (many people)
were unhoused? (Putting in unknown for now) 0-3 - where to put this one?
(putting in unknown for now) “currently homeless” - perhaps should just
change to unknown? (unsure also if this is a column shift issue, same
with “yes” and “no”, leaving alone for now)

``` r
demo_dat <- demo_dat %>% 
  mutate(number_times_homeless_recode = case_when(str_equal(number_of_times_homelessness_occurred_in_the_last_three_years, "0") ~ "client was not homeless",
                                                  str_detect(number_of_times_homelessness_occurred_in_the_last_three_years, "0-|doesn't know|refused|data not|free form te|99") ~ "unknown",
                                                  str_equal(number_of_times_homelessness_occurred_in_the_last_three_years, "1") ~ "one time",
                                                  str_equal(number_of_times_homelessness_occurred_in_the_last_three_years, "2") ~ "two times",
                                                  str_equal(number_of_times_homelessness_occurred_in_the_last_three_years, "3") ~ "three times",
                                                  str_detect(number_of_times_homelessness_occurred_in_the_last_three_years, "0|1|2|3|4|5|6|7|8|9|four") ~ "four or more times",
                                                  is.na(number_of_times_homelessness_occurred_in_the_last_three_years) ~ "unknown",
                                                  TRUE ~ number_of_times_homelessness_occurred_in_the_last_three_years))
```

check for number of times homeless in last 3 years

``` r
demo_dat %>% 
  count(number_of_times_homelessness_occurred_in_the_last_three_years, number_times_homeless_recode)
```

<div class="kable-table">

<table>
<thead>
<tr>
<th style="text-align:left;">
number_of_times_homelessness_occurred_in_the_last_three_years
</th>
<th style="text-align:left;">
number_times_homeless_recode
</th>
<th style="text-align:right;">
n
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
client was not homeless
</td>
<td style="text-align:right;">
1303
</td>
</tr>
<tr>
<td style="text-align:left;">
0-3
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1
</td>
<td style="text-align:left;">
one time
</td>
<td style="text-align:right;">
631
</td>
</tr>
<tr>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
four or more times
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
15
</td>
<td style="text-align:left;">
four or more times
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
two times
</td>
<td style="text-align:right;">
116
</td>
</tr>
<tr>
<td style="text-align:left;">
20
</td>
<td style="text-align:left;">
four or more times
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
three times
</td>
<td style="text-align:right;">
60
</td>
</tr>
<tr>
<td style="text-align:left;">
30
</td>
<td style="text-align:left;">
four or more times
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
36
</td>
<td style="text-align:left;">
four or more times
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
four or more times
</td>
<td style="text-align:right;">
42
</td>
</tr>
<tr>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
four or more times
</td>
<td style="text-align:right;">
29
</td>
</tr>
<tr>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
four or more times
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
four or more times
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
99
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1366
</td>
</tr>
<tr>
<td style="text-align:left;">
client doesn’t know
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
25
</td>
</tr>
<tr>
<td style="text-align:left;">
client refused
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
60
</td>
</tr>
<tr>
<td style="text-align:left;">
client was not homeless
</td>
<td style="text-align:left;">
client was not homeless
</td>
<td style="text-align:right;">
1717
</td>
</tr>
<tr>
<td style="text-align:left;">
currently homeless
</td>
<td style="text-align:left;">
currently homeless
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
5139
</td>
</tr>
<tr>
<td style="text-align:left;">
four/more times
</td>
<td style="text-align:left;">
four or more times
</td>
<td style="text-align:right;">
246
</td>
</tr>
<tr>
<td style="text-align:left;">
free form text
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
45
</td>
</tr>
<tr>
<td style="text-align:left;">
no
</td>
<td style="text-align:left;">
no
</td>
<td style="text-align:right;">
817
</td>
</tr>
<tr>
<td style="text-align:left;">
one time
</td>
<td style="text-align:left;">
one time
</td>
<td style="text-align:right;">
725
</td>
</tr>
<tr>
<td style="text-align:left;">
three times
</td>
<td style="text-align:left;">
three times
</td>
<td style="text-align:right;">
27
</td>
</tr>
<tr>
<td style="text-align:left;">
two times
</td>
<td style="text-align:left;">
two times
</td>
<td style="text-align:right;">
52
</td>
</tr>
<tr>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
yes
</td>
<td style="text-align:left;">
yes
</td>
<td style="text-align:right;">
175
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
2086
</td>
</tr>
</tbody>
</table>

</div>

Total duration of homelessness (in last 3 years) - Changing “0” to
“client was not homeless”. Leaving “yes” and “no” responses for now, as
I think they’re a column shift issue. Combining unknown
responses.Changing responses that are more than a year to all be “more
than a year”. changing “n” to “unknown”.

### Questions:

99 - same question as number of times homeless responses like “1”, “2”
“24” - no units.. so not sure. My guess would be those are the number of
months? (left as is for now) “within the last 3 years” - can’t really
take a guess at duration, besides saying it’s not more than 3 years?
“six months to one year” “7 months to one year” “less than 12 months”
“two to six months” - what to do? “within the last year” “within the
last 3 years” “currently homeless” - seems a response to the following
question (last period of homelessness). Looking at what those records
have for last period, both don’t really make sense, so double check
after column issue fix? (Sara) - there must be a better way to code
replacing numbers with the number written out? (i.e. 5 months –\> five
months)

``` r
demo_dat <- demo_dat %>% 
  mutate(total_duration_homeless_recode = case_when(str_equal(total_duration_of_homelessness, "0") ~ "client was not homeless",
                                                    str_detect(total_duration_of_homelessness, "not homeless") ~ "client was not homeless",
                                                    str_detect(total_duration_of_homelessness, "blank|doesn't know|data no|refused|none|n/a|99") ~ "unknown",
                                                    str_detect(total_duration_of_homelessness, "2 year|3 year|6 year|few year|more than a year|years or longer") ~ "more than a year",
                                                    str_detect(total_duration_of_homelessness, "1 day to one month") ~"one day to one month",
                                                    str_detect(total_duration_of_homelessness, "5 mon") ~ "five months",
                                                    str_detect(total_duration_of_homelessness, "6 mon") ~ "six months",
                                                    str_detect(total_duration_of_homelessness, "to one year|less than 12 months|within the last|currently homeless") ~ "flag",
                                                    str_equal(total_duration_of_homelessness, "n") ~ "unknown",
                                                    is.na(total_duration_of_homelessness) ~ "unknown",
                                                    
                                                    TRUE ~ total_duration_of_homelessness))
```

Check for total duration of homelessness in last 3 years

``` r
demo_dat %>% 
  count(total_duration_of_homelessness, total_duration_homeless_recode)
```

<div class="kable-table">

<table>
<thead>
<tr>
<th style="text-align:left;">
total_duration_of_homelessness
</th>
<th style="text-align:left;">
total_duration_homeless_recode
</th>
<th style="text-align:right;">
n
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
client was not homeless
</td>
<td style="text-align:right;">
172
</td>
</tr>
<tr>
<td style="text-align:left;">
1
</td>
<td style="text-align:left;">
1
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
1 day to one month
</td>
<td style="text-align:left;">
one day to one month
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
2 years
</td>
<td style="text-align:left;">
more than a year
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
24
</td>
<td style="text-align:left;">
24
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
3 years
</td>
<td style="text-align:left;">
more than a year
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
5 months
</td>
<td style="text-align:left;">
five months
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
6 months
</td>
<td style="text-align:left;">
six months
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
6 years
</td>
<td style="text-align:left;">
more than a year
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
99
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
15
</td>
</tr>
<tr>
<td style="text-align:left;">
a few years
</td>
<td style="text-align:left;">
more than a year
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
blank
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
client doesn’t know
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
client not homeless
</td>
<td style="text-align:left;">
client was not homeless
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
client refused
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
client was client was not homelesst homeless
</td>
<td style="text-align:left;">
client was not homeless
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
client was not homeless
</td>
<td style="text-align:left;">
client was not homeless
</td>
<td style="text-align:right;">
2939
</td>
</tr>
<tr>
<td style="text-align:left;">
currently homeless
</td>
<td style="text-align:left;">
flag
</td>
<td style="text-align:right;">
18
</td>
</tr>
<tr>
<td style="text-align:left;">
data client was not homelesst collected
</td>
<td style="text-align:left;">
client was not homeless
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
data no collected
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
6747
</td>
</tr>
<tr>
<td style="text-align:left;">
eight months
</td>
<td style="text-align:left;">
eight months
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
eleven months
</td>
<td style="text-align:left;">
eleven months
</td>
<td style="text-align:right;">
19
</td>
</tr>
<tr>
<td style="text-align:left;">
five months
</td>
<td style="text-align:left;">
five months
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
four months
</td>
<td style="text-align:left;">
four months
</td>
<td style="text-align:right;">
13
</td>
</tr>
<tr>
<td style="text-align:left;">
less than 12 months
</td>
<td style="text-align:left;">
flag
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
more than a year
</td>
<td style="text-align:left;">
more than a year
</td>
<td style="text-align:right;">
611
</td>
</tr>
<tr>
<td style="text-align:left;">
more than a
year/lookat:=xlwhole/searchorder:=xlbyrows/matchcase:=false/searchformat:=false/replaceformat:=false
</td>
<td style="text-align:left;">
more than a year
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
n
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
n/a
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
nine months
</td>
<td style="text-align:left;">
nine months
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
no
</td>
<td style="text-align:left;">
no
</td>
<td style="text-align:right;">
758
</td>
</tr>
<tr>
<td style="text-align:left;">
none
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
one day to one month
</td>
<td style="text-align:left;">
one day to one month
</td>
<td style="text-align:right;">
547
</td>
</tr>
<tr>
<td style="text-align:left;">
one year
</td>
<td style="text-align:left;">
one year
</td>
<td style="text-align:right;">
36
</td>
</tr>
<tr>
<td style="text-align:left;">
seven months
</td>
<td style="text-align:left;">
seven months
</td>
<td style="text-align:right;">
58
</td>
</tr>
<tr>
<td style="text-align:left;">
seven months to one year
</td>
<td style="text-align:left;">
flag
</td>
<td style="text-align:right;">
142
</td>
</tr>
<tr>
<td style="text-align:left;">
six months
</td>
<td style="text-align:left;">
six months
</td>
<td style="text-align:right;">
42
</td>
</tr>
<tr>
<td style="text-align:left;">
six months to one year
</td>
<td style="text-align:left;">
flag
</td>
<td style="text-align:right;">
17
</td>
</tr>
<tr>
<td style="text-align:left;">
ten months
</td>
<td style="text-align:left;">
ten months
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
three months
</td>
<td style="text-align:left;">
three months
</td>
<td style="text-align:right;">
31
</td>
</tr>
<tr>
<td style="text-align:left;">
three years or longer
</td>
<td style="text-align:left;">
more than a year
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
two months
</td>
<td style="text-align:left;">
two months
</td>
<td style="text-align:right;">
123
</td>
</tr>
<tr>
<td style="text-align:left;">
two to six months
</td>
<td style="text-align:left;">
two to six months
</td>
<td style="text-align:right;">
214
</td>
</tr>
<tr>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
91
</td>
</tr>
<tr>
<td style="text-align:left;">
within the last three years
</td>
<td style="text-align:left;">
flag
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
within the last year
</td>
<td style="text-align:left;">
flag
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
yes
</td>
<td style="text-align:left;">
yes
</td>
<td style="text-align:right;">
151
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1853
</td>
</tr>
</tbody>
</table>

</div>

Current eviction or foreclosure - combining unknown responses, and
making “none” “no”. Leaving the rest for now.

### Question:

is 99 unknown here as well? (this one isn’t a drop down menu, so
wondering if I should treat the same)

``` r
demo_dat <- demo_dat %>% 
  mutate(current_eviction_recode = case_when(str_detect(current_eviction_or_foreclosures, "doesn't know|refused|data not|n/a") ~ "unknown",
                                             str_detect(current_eviction_or_foreclosures, "none") ~ "no",
                                             is.na(current_eviction_or_foreclosures) ~ "unknown",
                                            TRUE ~ current_eviction_or_foreclosures ))
```

check for current eviction/foreclosure

``` r
demo_dat %>% 
  count(current_eviction_or_foreclosures, current_eviction_recode)
```

<div class="kable-table">

<table>
<thead>
<tr>
<th style="text-align:left;">
current_eviction_or_foreclosures
</th>
<th style="text-align:left;">
current_eviction_recode
</th>
<th style="text-align:right;">
n
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:right;">
43
</td>
</tr>
<tr>
<td style="text-align:left;">
1
</td>
<td style="text-align:left;">
1
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
100
</td>
<td style="text-align:left;">
100
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
15
</td>
<td style="text-align:left;">
15
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
20
</td>
<td style="text-align:left;">
20
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
730
</td>
<td style="text-align:left;">
730
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
99
</td>
<td style="text-align:left;">
99
</td>
<td style="text-align:right;">
12
</td>
</tr>
<tr>
<td style="text-align:left;">
client doesn’t know
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
44
</td>
</tr>
<tr>
<td style="text-align:left;">
client refused
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
24
</td>
</tr>
<tr>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
3921
</td>
</tr>
<tr>
<td style="text-align:left;">
n/a
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
no
</td>
<td style="text-align:left;">
no
</td>
<td style="text-align:right;">
7048
</td>
</tr>
<tr>
<td style="text-align:left;">
none
</td>
<td style="text-align:left;">
no
</td>
<td style="text-align:right;">
17
</td>
</tr>
<tr>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1513
</td>
</tr>
<tr>
<td style="text-align:left;">
yes
</td>
<td style="text-align:left;">
yes
</td>
<td style="text-align:right;">
1722
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
325
</td>
</tr>
</tbody>
</table>

</div>

Previous Evictions or Foreclosures - this one is truly so messy I will
hold off until we see the new data with column issue fix

``` r
demo_dat %>% 
  count(previous_evictions_or_foreclosures)
```

<div class="kable-table">

<table>
<thead>
<tr>
<th style="text-align:left;">
previous_evictions_or_foreclosures
</th>
<th style="text-align:right;">
n
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
0
</td>
<td style="text-align:right;">
49
</td>
</tr>
<tr>
<td style="text-align:left;">
2
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43585.447222222225
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43635.388888888891
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43636.57708333333
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43647.558333333334
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43658.507638888892
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43663.599305555559
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43665.365277777775
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43665.489583333336
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43665.763194444444
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43682.470138888886
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43682.481249999997
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43683.523611111108
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43685.658333333333
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43685.723611111112
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43697.348611111112
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43700.531944444447
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43702.052083333336
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43711.425000000003
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43712.35833333333
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43714.407638888886
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43718.323611111111
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43719.581250000003
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43720.838194444441
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43720.848611111112
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43724.432638888888
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43727
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43727.535416666666
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43728.416666666664
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43739
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43742.657638888886
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43743
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43746.435416666667
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43747.317361111112
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43748
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43748.4375
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43751.536111111112
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43752.588194444441
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43753.756944444445
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43754.40902777778
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43754.648611111108
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43755.622916666667
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43755.629166666666
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43759.502083333333
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43759.660416666666
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43760.714583333334
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43760.719444444447
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43762.371527777781
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43762.896527777775
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43763.56527777778
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43764.385416666664
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43766
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43766.637499999997
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43766.679861111108
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43767.668055555558
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43768.527777777781
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43768.663194444445
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43769.417361111111
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43770.42291666667
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43770.712500000001
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43773.257638888892
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43776.552083333336
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43776.570833333331
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43777.347222222219
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43781
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43782.479166666664
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43783.490277777775
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43783.678472222222
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43783.688888888886
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43784.381249999999
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43787.505555555559
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43787.665972222225
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43788.441666666666
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43789.679166666669
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43791.662499999999
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43794.722916666666
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43795.432638888888
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43801.431944444441
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43804.568055555559
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43804.701388888891
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43804.71875
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43805.619444444441
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43805.716666666667
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43808.550694444442
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43809.795138888891
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43811.645138888889
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43815.373611111114
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43816.603472222225
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43819.395833333336
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43819.439583333333
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43819.655555555553
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43822.347222222219
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43825.342361111114
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43826.634722222225
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43826.680555555555
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43832.675000000003
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43833.387499999997
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43837.465277777781
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43837.619444444441
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43837.675694444442
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43838.423611111109
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43838.738194444442
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43839.34652777778
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43841.594444444447
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43842.602083333331
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43843.625
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43844.511111111111
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43844.61041666667
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43844.620833333334
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43846.705555555556
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43847.613194444442
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43851.529861111114
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43851.640277777777
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43852.432638888888
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43852.436805555553
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43852.634722222225
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43852.657638888886
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43854.568055555559
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43857.401388888888
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43857.581250000003
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43858.441666666666
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43858.713194444441
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43858.71597222222
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43859.416666666664
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43859.543055555558
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43860.493055555555
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43860.581250000003
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43860.647222222222
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43860.997916666667
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43861.545138888891
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43861.553472222222
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43861.598611111112
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43861.609027777777
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43865.35833333333
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43865.511111111111
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43866.494444444441
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43867.732638888891
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43872.658333333333
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43873.607638888891
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43875.36041666667
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43875.402083333334
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43875.70208333333
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43877.671527777777
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43878.50277777778
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43880.324305555558
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43880.699305555558
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43881.456944444442
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43883.464583333334
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43883.469444444447
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43885.491666666669
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43885.625694444447
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43886.477083333331
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43886.659722222219
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43886.87777777778
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43886.95
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43887.638194444444
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43889.462500000001
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43892.45
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43895.425694444442
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43895.693749999999
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43896.522916666669
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43899
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43899.493750000001
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43899.512499999997
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43899.73333333333
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43900.463194444441
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43901.357638888891
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43902.401388888888
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43902.697916666664
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43903.486805555556
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43906.55
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43906.667361111111
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43907.337500000001
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43909.702777777777
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43910.510416666664
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43910.652083333334
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43910.667361111111
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43920.624305555553
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43921.408333333333
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43921.561111111114
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43921.568749999999
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43922.713888888888
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43924.649305555555
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43928.392361111109
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43928.724305555559
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43929.578472222223
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43935.668749999997
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43937.287499999999
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43941.415972222225
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43942.557638888888
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43943.538888888892
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43943.563194444447
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43948.783333333333
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43949
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43949.723611111112
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43950.640277777777
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43951.340277777781
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43951.406944444447
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43951.459722222222
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43955.609722222223
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43957
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43957.665277777778
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43957.701388888891
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43959
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43959.493055555555
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43962.469444444447
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43963.297222222223
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43964
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43967.855555555558
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43971.570138888892
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43971.609027777777
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43972
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43973.445833333331
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43977.5625
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43977.663888888892
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43979
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43979.480555555558
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43984.327777777777
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43984.840277777781
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43985.588194444441
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43986.571527777778
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43986.636805555558
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43987.578472222223
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43989.580555555556
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43989.898611111108
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43990.506249999999
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43990.618055555555
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43992.722916666666
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43993.600694444445
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43994
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43997.599305555559
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43998.511805555558
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43998.709027777775
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
43999.736111111109
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44000.693749999999
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44000.713888888888
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44000.959027777775
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44001.490277777775
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44006.477777777778
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44012.364583333336
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44012.554166666669
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44018.571527777778
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44018.581944444442
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44018.695138888892
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44019.536111111112
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44021.54791666667
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44023.568055555559
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44023.59652777778
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44025.570138888892
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44026
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44033.631249999999
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44035.307638888888
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44036.617361111108
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44041.428472222222
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44041.579861111109
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44042.394444444442
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44046.426388888889
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44046.684027777781
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44049.533333333333
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44050.603472222225
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44053.494444444441
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44054.425694444442
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44054.586111111108
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44054.728472222225
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44055.504861111112
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44061.445833333331
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44062.521527777775
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44063.522222222222
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44069.465277777781
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44069.709027777775
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44071.702777777777
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44074.609722222223
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44078.434027777781
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44078.629861111112
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44081.523611111108
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44083.563194444447
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44086.579861111109
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44089.329861111109
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44089.404861111114
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44089.551388888889
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44091.436111111114
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44094.711111111108
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44094.953472222223
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44096.644444444442
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44105.677083333336
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44106.420138888891
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44107.48541666667
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44107.507638888892
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44110.679166666669
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44110.682638888888
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44112.559027777781
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44116.377083333333
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44117.551388888889
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44119.458333333336
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44125.4375
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44126.429166666669
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44128.698611111111
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44130.368750000001
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44130.37777777778
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44130.385416666664
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44130.393055555556
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44130.594444444447
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44137.795138888891
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44138.55
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44138.724999999999
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44145.344444444447
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44145.599305555559
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44146.480555555558
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44146.486111111109
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44147.839583333334
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44152.647222222222
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44152.652777777781
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44152.659722222219
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44152.660416666666
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44152.663888888892
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44153.638888888891
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44154.410416666666
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44167.67083333333
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44167.675694444442
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44167.679861111108
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44167.683333333334
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44168.298611111109
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44168.511805555558
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44168.561805555553
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44172.368750000001
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44172.618750000001
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44173.397222222222
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44173.556944444441
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44173.566666666666
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44175.6875
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44179.449305555558
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44180.47152777778
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44180.649305555555
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44183.461111111108
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44203.459027777775
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44203.493750000001
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44208.363194444442
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44210.556250000001
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44216
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44217.671527777777
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44218.647222222222
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44221.924305555556
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44225
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44225.426388888889
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44225.747916666667
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44227.734027777777
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44230.432638888888
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44232.513888888891
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44235.658333333333
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44235.697222222225
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44236.698611111111
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44236.701388888891
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44237.398611111108
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44239.413888888892
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44239.425000000003
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44239.502083333333
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44243.543749999997
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44243.643750000003
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44246.423611111109
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44246.510416666664
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44249.378472222219
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44249.447222222225
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44249.521527777775
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44249.586111111108
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44251.615972222222
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44251.671527777777
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44252.638888888891
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44253.634027777778
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44254.765277777777
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44257.708333333336
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44258.543055555558
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44259.422222222223
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44260.43472222222
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44260.644444444442
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44260.697222222225
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44262.424305555556
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44263.393055555556
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44263.540972222225
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44263.726388888892
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44264.461805555555
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44265.376388888886
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44265.623611111114
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44265.804861111108
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44266.013888888891
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
44266.393055555556
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44266.473611111112
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44266.541666666664
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44266.698611111111
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44267.393055555556
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44268.080555555556
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44270.458333333336
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44270.609722222223
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44270.652083333334
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44270.679861111108
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44272.352083333331
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44272.416666666664
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44272.54583333333
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44272.615277777775
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44274.216666666667
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44277.546527777777
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44277.582638888889
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44278.496527777781
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44279.590277777781
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44280.497916666667
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44280.550694444442
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44281.03125
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44284
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44284.595833333333
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44284.618750000001
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44285.606249999997
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44287.319444444445
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44287.368750000001
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44289.461111111108
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44291.941666666666
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44292.363194444442
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44293.488888888889
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44294.544444444444
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44294.75
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44299.531944444447
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44299.53402777778
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44299.561111111114
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44300.060416666667
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44300.57708333333
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44300.614583333336
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44303.509027777778
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44305.493750000001
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44305.580555555556
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44305.675694444442
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44306.371527777781
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44306.472916666666
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44306.643750000003
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44306.661111111112
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44308.54791666667
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44309.409722222219
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44312.38958333333
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44313.497916666667
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44313.543749999997
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44315.697222222225
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44316.702777777777
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44319.48333333333
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44319.506944444445
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44319.61041666667
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44320
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
44320.411805555559
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44321.414583333331
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44322.991666666669
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44323.440972222219
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44323.57916666667
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44326.777083333334
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44327.456250000003
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44327.602777777778
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44328.776388888888
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44328.830555555556
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44329.40347222222
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44330.55
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44334.302083333336
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44334.355555555558
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44334.387499999997
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44335.322222222225
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44335.383333333331
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44335.685416666667
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44335.693055555559
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44336.424305555556
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44336.44027777778
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44336.504861111112
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44337.611111111109
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44340.560416666667
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44340.599305555559
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44340.684027777781
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44340.811111111114
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44341.679166666669
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44342
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44342.832638888889
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44343.54791666667
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44348.555555555555
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44349.648611111108
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44349.650694444441
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44349.70208333333
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44350.588888888888
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44350.654166666667
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44350.660416666666
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44350.669444444444
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44350.675000000003
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44350.686805555553
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44350.727777777778
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44350.736111111109
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44350.748611111114
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44350.76458333333
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44350.782638888886
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44350.787499999999
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44350.793749999997
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44351.470138888886
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44354.44027777778
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44354.554861111108
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44355.026388888888
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44355.50277777778
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44355.719444444447
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44356.448611111111
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44356.465277777781
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44356.540972222225
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44356.673611111109
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44357.509722222225
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44358.343055555553
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44358.477777777778
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44364.370833333334
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44371.480555555558
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44375.605555555558
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44376.669444444444
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44378.055555555555
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44379.540972222225
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44384.601388888892
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44385.446527777778
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44386.690972222219
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44389.473611111112
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44389.564583333333
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44390.364583333336
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44395.70208333333
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44397
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44398.402777777781
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44398.626388888886
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44399.582638888889
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44404.395833333336
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44404.720833333333
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44405.629861111112
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44410.522916666669
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44412.462500000001
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44412.631249999999
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44414.45
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44414.631249999999
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44418
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
44419.448611111111
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44419.488888888889
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44419.512499999997
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44420.401388888888
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44420.422222222223
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44420.518055555556
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44420.583333333336
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44420.760416666664
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44425.715277777781
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44426.615972222222
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44426.754861111112
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44428.443749999999
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44431.425694444442
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44433.55
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44446.34652777778
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44446.572916666664
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44446.650694444441
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44447
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44449.515972222223
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44454.47152777778
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44455
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44455.400694444441
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44456
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44456.472222222219
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44459.415972222225
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44459.61041666667
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44462.465277777781
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44466.423611111109
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44467.377083333333
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44467.505555555559
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44467.506249999999
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44468.45416666667
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44469.505555555559
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44475.435416666667
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44475.489583333336
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44476.478472222225
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44476.640972222223
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44476.645833333336
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44477.602777777778
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44478.862500000003
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44481
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44481.484027777777
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44482
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44482.238194444442
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44482.649305555555
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44483.347916666666
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44483.40902777778
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44483.486111111109
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44484.582638888889
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44484.597916666666
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44487.46875
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44488.530555555553
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44495.56527777778
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44497
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44503.545138888891
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44503.595138888886
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44504.441666666666
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44508
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44509.370833333334
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44509.594444444447
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44510.336111111108
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44510.402083333334
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44512.597222222219
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44513.552083333336
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44515.649305555555
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44515.709027777775
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44517.600694444445
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44519.559027777781
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44521.549305555556
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44523.377083333333
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44523.523611111108
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44524.609027777777
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44529.379166666666
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44529.470833333333
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44530.570138888892
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44530.615277777775
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44533
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44536.40347222222
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44538.486111111109
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44539.424305555556
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44543.502083333333
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44544.564583333333
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44545.335416666669
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44546.495138888888
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44551.697916666664
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44552.555555555555
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44559.523611111108
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44566.507638888892
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44566.527083333334
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44566.683333333334
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44567.578472222223
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44567.609722222223
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44571.363194444442
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44572.402083333334
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44572.457638888889
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44573
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
44573.383333333331
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44573.418055555558
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44573.446527777778
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44573.699305555558
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44574.793749999997
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44576
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44578.8125
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44579.405555555553
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44579.544444444444
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44581.460416666669
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44581.600694444445
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44585.631249999999
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44586.440972222219
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44588.378472222219
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44590
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44592.334722222222
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44593.565972222219
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44593.626388888886
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44594.306250000001
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44594.685416666667
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44595.367361111108
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44601.40902777778
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44601.520138888889
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44602.567361111112
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44607.535416666666
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44608.356249999997
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44610.432638888888
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44613
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44614.361805555556
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44614.600694444445
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44614.680555555555
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44615.586111111108
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44616
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44616.394444444442
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44616.719444444447
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44620.460416666669
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44621.609027777777
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44622.629166666666
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44628
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44630.595833333333
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44634.386111111111
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44634.388194444444
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44634.49722222222
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44634.51666666667
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44634.56527777778
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44634.636111111111
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44636.473611111112
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44637.408333333333
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44638.478472222225
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44638.496527777781
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44638.518750000003
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44638.525000000001
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44642
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44642.522222222222
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44644.340277777781
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44645.385416666664
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44647.932638888888
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44648.628472222219
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44650.558333333334
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44650.806250000001
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44651.463194444441
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44651.582638888889
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44651.727083333331
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44654.277777777781
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44655.493055555555
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44656.353472222225
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44656.556250000001
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44659.693055555559
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44660
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44661.462500000001
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44664.368055555555
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44666.557638888888
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44669.61041666667
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44671.652777777781
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44672.464583333334
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44676.4
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44677.673611111109
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44677.683333333334
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44679.45416666667
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44680.40625
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44683.347222222219
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44683.458333333336
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44684
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44684.650694444441
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44684.665277777778
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44685.68472222222
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44686.412499999999
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44686.487500000003
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44687.612500000003
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44687.65347222222
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44690.445138888892
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44690.555555555555
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44691.554861111108
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44691.620833333334
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44694.506944444445
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44694.752083333333
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44698.330555555556
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44698.336111111108
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44698.5625
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
44699.4
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44699.509722222225
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44700.45416666667
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44700.691666666666
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44700.824999999997
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44701.690972222219
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44701.691666666666
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44704.35833333333
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44707.414583333331
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44707.677083333336
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44707.732638888891
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44708.584027777775
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44709.601388888892
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44712.713888888888
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44712.724305555559
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44714.609722222223
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44714.618750000001
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44714.622916666667
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44714.625
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44714.627083333333
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44718.609722222223
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44719.399305555555
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44720.617361111108
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44720.622916666667
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44721
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44721.396527777775
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44721.482638888891
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44721.554166666669
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44725.63958333333
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44727.476388888892
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44727.521527777775
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44728.368750000001
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44728.6
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44728.648611111108
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44733.399305555555
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44733.425694444442
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44734.353472222225
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44739.399305555555
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44739.409722222219
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44739.431250000001
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44739.449305555558
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44739.7
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44740.395138888889
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44741.395138888889
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44742.526388888888
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44742.666666666664
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44742.670138888891
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44743
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44747.629861111112
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44748.56527777778
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44749.463888888888
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44749.476388888892
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44750
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44750.584027777775
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44752.336805555555
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44754.472222222219
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44754.572916666664
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44756.549305555556
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44756.625
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44756.678472222222
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44756.695833333331
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44757.600694444445
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44760.474999999999
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44761.564583333333
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44761.71875
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44763.626388888886
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44764.359027777777
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44764.688888888886
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44764.692361111112
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44767.545138888891
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44767.656944444447
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44769.643750000003
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44771.404166666667
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44773
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44775.651388888888
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44776.543055555558
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44778.743750000001
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44781.395833333336
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44781.685416666667
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44781.701388888891
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44782.351388888892
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44783.48541666667
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44783.609722222223
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44783.668749999997
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44784.353472222225
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44784.361111111109
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44784.37222222222
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44784.375
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44784.472222222219
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44784.574305555558
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44784.578472222223
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44784.60833333333
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44785.417361111111
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44785.429166666669
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44785.704861111109
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44785.793749999997
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44788.695138888892
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44789.703472222223
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44790
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44790.581250000003
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44791
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
44791.436111111114
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44791.5
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44791.644444444442
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44792.584722222222
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44795.363888888889
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44795.384027777778
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44795.491666666669
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44795.579861111109
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44795.700694444444
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44795.707638888889
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44796
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
44796.36041666667
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44797
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44797.474999999999
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44797.59375
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44797.652777777781
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44797.676388888889
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44797.678472222222
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44798.463888888888
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44798.477777777778
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44798.601388888892
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44798.668749999997
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44799.65902777778
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44799.663888888892
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44799.676388888889
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44799.701388888891
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44799.707638888889
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44802.650694444441
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44803.32916666667
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44803.378472222219
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44803.387499999997
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44803.581944444442
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44803.675694444442
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44803.676388888889
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44804.594444444447
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44804.643055555556
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44804.699305555558
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44805.604861111111
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44805.616666666669
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44806.45416666667
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44810.497916666667
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44811.45208333333
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44811.511805555558
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44811.545138888891
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44812.343055555553
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44812.348611111112
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44812.353472222225
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44812.381944444445
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44812.451388888891
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44812.503472222219
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44812.536805555559
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44813.428472222222
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44813.691666666666
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44816.492361111108
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44816.556944444441
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44817.463888888888
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44817.657638888886
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
44818.336111111108
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44818.594444444447
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44818.599305555559
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44821
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44823.4375
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44823.650694444441
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44823.677083333336
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44824.545138888891
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44824.56527777778
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44824.631249999999
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44824.643750000003
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44825.636805555558
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44826.388194444444
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44826.470833333333
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44827
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44827.02847222222
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44827.348611111112
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44827.409722222219
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44827.479861111111
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44827.51458333333
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44827.61041666667
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44827.715277777781
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44827.731249999997
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44830.486805555556
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44830.605555555558
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44830.643055555556
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44830.710416666669
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44831.561805555553
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44831.607638888891
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44831.704861111109
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44832.417361111111
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44832.531944444447
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44832.537499999999
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44832.585416666669
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44832.713888888888
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44833.693749999999
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44834
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44836.350694444445
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44838.370833333334
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44838.506944444445
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44838.568749999999
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44838.573611111111
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44838.62222222222
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44838.627083333333
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44838.697916666664
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44839.505555555559
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44839.552777777775
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44839.987500000003
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44840.443055555559
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44840.443749999999
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44840.822916666664
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44846.324305555558
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44846.474999999999
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44847.377083333333
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44847.456944444442
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44848
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44851
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44851.584722222222
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44852.439583333333
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44853.380555555559
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44854.527083333334
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44854.632638888892
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44855.468055555553
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44859.411111111112
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44859.677777777775
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44859.684027777781
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44862.488888888889
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44865.381944444445
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44866.564583333333
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44867.35833333333
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44868.684027777781
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44869.427083333336
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44872
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44872.477083333331
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44872.542361111111
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44874.588888888888
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44875.631249999999
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44875.676388888889
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44877.473611111112
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44881.563888888886
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44883
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44883.498611111114
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44883.69027777778
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44886.445833333331
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44888.550694444442
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44892
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44893.722916666666
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44894
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
44894.382638888892
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44894.681250000001
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44899.581944444442
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44900.415277777778
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44901.426388888889
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44901.605555555558
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44901.609722222223
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44901.688194444447
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44902.342361111114
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44903.704861111109
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44908.597222222219
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44914.62777777778
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44915.440972222219
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44917
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44917.436111111114
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44922.711111111108
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
99
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
blank
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
client doesn’t know
</td>
<td style="text-align:right;">
25
</td>
</tr>
<tr>
<td style="text-align:left;">
client refused
</td>
<td style="text-align:right;">
19
</td>
</tr>
<tr>
<td style="text-align:left;">
client was not homeless
</td>
<td style="text-align:right;">
43
</td>
</tr>
<tr>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:right;">
4046
</td>
</tr>
<tr>
<td style="text-align:left;">
more than a year
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
n/a
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
no
</td>
<td style="text-align:right;">
5370
</td>
</tr>
<tr>
<td style="text-align:left;">
none
</td>
<td style="text-align:right;">
13
</td>
</tr>
<tr>
<td style="text-align:left;">
one day to one month
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
seven months to one year
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
two to six months
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
2755
</td>
</tr>
<tr>
<td style="text-align:left;">
yes
</td>
<td style="text-align:right;">
1126
</td>
</tr>
<tr>
<td style="text-align:left;">
yese
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
218
</td>
</tr>
</tbody>
</table>

</div>

Discharge from institution in the last 6 months - combining all unknown,
leaving much of the rest for post column issue fix

``` r
demo_dat <- demo_dat %>% 
  mutate(discharge_institution_recode = case_when(str_detect(discharge_from_institution_in_the_last_six_months, "data not|refused|doesn't know|n/a") ~ "unknown",
                                                  is.na(discharge_from_institution_in_the_last_six_months) ~ "unknown",
                                                  TRUE ~ discharge_from_institution_in_the_last_six_months))
```

Check for discharge from institution

``` r
demo_dat %>% 
  count(discharge_from_institution_in_the_last_six_months, discharge_institution_recode)
```

<div class="kable-table">

<table>
<thead>
<tr>
<th style="text-align:left;">
discharge_from_institution_in_the_last_six_months
</th>
<th style="text-align:left;">
discharge_institution_recode
</th>
<th style="text-align:right;">
n
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
client doesn’t know
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
client refused
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
22
</td>
</tr>
<tr>
<td style="text-align:left;">
client was not homeless
</td>
<td style="text-align:left;">
client was not homeless
</td>
<td style="text-align:right;">
24
</td>
</tr>
<tr>
<td style="text-align:left;">
currently homeless
</td>
<td style="text-align:left;">
currently homeless
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
4721
</td>
</tr>
<tr>
<td style="text-align:left;">
n
</td>
<td style="text-align:left;">
n
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
n/a
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
no
</td>
<td style="text-align:left;">
no
</td>
<td style="text-align:right;">
6503
</td>
</tr>
<tr>
<td style="text-align:left;">
none
</td>
<td style="text-align:left;">
none
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
2297
</td>
</tr>
<tr>
<td style="text-align:left;">
within the last month
</td>
<td style="text-align:left;">
within the last month
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
within the last three years
</td>
<td style="text-align:left;">
within the last three years
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
within the last year
</td>
<td style="text-align:left;">
within the last year
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
yes
</td>
<td style="text-align:left;">
yes
</td>
<td style="text-align:right;">
814
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
271
</td>
</tr>
</tbody>
</table>

</div>

APS report date -

### Question (for Sara): How do I code that we will want to check that the date is something between start of the program and the present?

Reporting Source - comining unknown responses into a “unknown”
(including not applicable). Correcting spelling of “self”, changing
“client” to “self”. Recoding “relative” and “family” as “family
member”.Changing “community professional” to “other community
professional” to match their drop down. Changing “financial institution”
and “financial” to “financial service provider”. Combining mental health
and mental health personnel.Changing “no relation” to “no relationship”.

### Question:

mandated reporter - ? (could be many, i.e. social worker, medical
personnel, mh personnel) paid caregiver - another where I am unsure,
maybe professional service provider? no APS referral, not applicable,
no - (no might be column shift) but these, change to unknown? or is it
interesting that some didn’t get referred?

``` r
demo_dat <- demo_dat %>% 
  mutate(reporting_source_recode = case_when(str_detect(reporting_source, "data not|refused|doesn't know|#|not applicable") ~ "unknown",
                                             str_detect(reporting_source, "sel|client") ~"self",
                                             str_detect(reporting_source, "relative|family") ~ "family member",
                                             str_detect(reporting_source, "community pro") ~"other community professional",
                                             str_detect(reporting_source, "financial") ~ "financial service provider",
                                             str_detect(reporting_source, "mental health") ~ "mental health personnel",
                                             str_detect(reporting_source, "no relatio") ~"no relationship",
                                             str_detect(reporting_source, "workshop|mandated report") ~ "flag",
                                             is.na(reporting_source) ~ "unknown",
                                             
                                             TRUE ~ reporting_source))
```

Check for reporting source

``` r
demo_dat %>% 
  count(reporting_source, reporting_source_recode)
```

<div class="kable-table">

<table>
<thead>
<tr>
<th style="text-align:left;">
reporting_source
</th>
<th style="text-align:left;">
reporting_source_recode
</th>
<th style="text-align:right;">
n
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
\#
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
anonymous
</td>
<td style="text-align:left;">
anonymous
</td>
<td style="text-align:right;">
167
</td>
</tr>
<tr>
<td style="text-align:left;">
clergy
</td>
<td style="text-align:left;">
clergy
</td>
<td style="text-align:right;">
13
</td>
</tr>
<tr>
<td style="text-align:left;">
client
</td>
<td style="text-align:left;">
self
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
community professional
</td>
<td style="text-align:left;">
other community professional
</td>
<td style="text-align:right;">
990
</td>
</tr>
<tr>
<td style="text-align:left;">
educator
</td>
<td style="text-align:left;">
educator
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
family
</td>
<td style="text-align:left;">
family member
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
family member
</td>
<td style="text-align:left;">
family member
</td>
<td style="text-align:right;">
339
</td>
</tr>
<tr>
<td style="text-align:left;">
financial
</td>
<td style="text-align:left;">
financial service provider
</td>
<td style="text-align:right;">
16
</td>
</tr>
<tr>
<td style="text-align:left;">
financial institution
</td>
<td style="text-align:left;">
financial service provider
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
financial service provider
</td>
<td style="text-align:left;">
financial service provider
</td>
<td style="text-align:right;">
121
</td>
</tr>
<tr>
<td style="text-align:left;">
institutional employee
</td>
<td style="text-align:left;">
institutional employee
</td>
<td style="text-align:right;">
29
</td>
</tr>
<tr>
<td style="text-align:left;">
law enforcement
</td>
<td style="text-align:left;">
law enforcement
</td>
<td style="text-align:right;">
409
</td>
</tr>
<tr>
<td style="text-align:left;">
mandated reporter
</td>
<td style="text-align:left;">
flag
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
medical personnel
</td>
<td style="text-align:left;">
medical personnel
</td>
<td style="text-align:right;">
661
</td>
</tr>
<tr>
<td style="text-align:left;">
mental health
</td>
<td style="text-align:left;">
mental health personnel
</td>
<td style="text-align:right;">
321
</td>
</tr>
<tr>
<td style="text-align:left;">
mental health personnel
</td>
<td style="text-align:left;">
mental health personnel
</td>
<td style="text-align:right;">
33
</td>
</tr>
<tr>
<td style="text-align:left;">
no
</td>
<td style="text-align:left;">
no
</td>
<td style="text-align:right;">
76
</td>
</tr>
<tr>
<td style="text-align:left;">
no aps referral
</td>
<td style="text-align:left;">
no aps referral
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
no relation
</td>
<td style="text-align:left;">
no relationship
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
no relationship
</td>
<td style="text-align:left;">
no relationship
</td>
<td style="text-align:right;">
897
</td>
</tr>
<tr>
<td style="text-align:left;">
not applicable
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
other community professional
</td>
<td style="text-align:left;">
other community professional
</td>
<td style="text-align:right;">
129
</td>
</tr>
<tr>
<td style="text-align:left;">
paid caregiver
</td>
<td style="text-align:left;">
paid caregiver
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
professional service provider
</td>
<td style="text-align:left;">
professional service provider
</td>
<td style="text-align:right;">
689
</td>
</tr>
<tr>
<td style="text-align:left;">
regional center/sheltered workshop
</td>
<td style="text-align:left;">
flag
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
relative
</td>
<td style="text-align:left;">
family member
</td>
<td style="text-align:right;">
15
</td>
</tr>
<tr>
<td style="text-align:left;">
self
</td>
<td style="text-align:left;">
self
</td>
<td style="text-align:right;">
2131
</td>
</tr>
<tr>
<td style="text-align:left;">
sellf
</td>
<td style="text-align:left;">
self
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
social worker
</td>
<td style="text-align:left;">
social worker
</td>
<td style="text-align:right;">
1959
</td>
</tr>
<tr>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
4340
</td>
</tr>
<tr>
<td style="text-align:left;">
yes
</td>
<td style="text-align:left;">
yes
</td>
<td style="text-align:right;">
16
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1303
</td>
</tr>
</tbody>
</table>

</div>

### Question: for previous APS involvement - seeing some responses that appear to fit in intervention, will this affect processing (given these are now subsets of data?) Hopefully this is just a column issue, but want to flag.

### Question: anything we want to do with processing the income variables? (income fro benefits, work for pay, other income)

## To come back to if needed:

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

<div class="kable-table">

<table>
<thead>
<tr>
<th style="text-align:left;">
aps_report_location
</th>
<th style="text-align:right;">
n
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
\#
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
–please select–
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
0
</td>
<td style="text-align:right;">
42
</td>
</tr>
<tr>
<td style="text-align:left;">
1
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
109 sterling way roseville
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1167 plumas st
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
136 kaseberg lane roseville
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
136 kaseberg lane roseville
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
2
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
2 houses from corner tx
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
29 palms
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
4000 orange st
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
5636 cold springs dr forest hill clientsresiden
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
92543
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
99
</td>
<td style="text-align:right;">
21
</td>
</tr>
<tr>
<td style="text-align:left;">
acampo
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
adelanto
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
alameda
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
alamo
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
albany
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
alhambra
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
alpine
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
alta loma
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
alturas
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
americannyon
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
anaheim
</td>
<td style="text-align:right;">
44
</td>
</tr>
<tr>
<td style="text-align:left;">
anderson
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
angelsmp
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
antelope
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
antioch
</td>
<td style="text-align:right;">
48
</td>
</tr>
<tr>
<td style="text-align:left;">
anza
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
apple valley
</td>
<td style="text-align:right;">
17
</td>
</tr>
<tr>
<td style="text-align:left;">
applegate
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
aptos
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
arcadia
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
arcata
</td>
<td style="text-align:right;">
30
</td>
</tr>
<tr>
<td style="text-align:left;">
arroyo grande
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
artesia
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
arvin
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
atascadero
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
atwater
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
auberry
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
auburn
</td>
<td style="text-align:right;">
38
</td>
</tr>
<tr>
<td style="text-align:left;">
auburn -95603
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
avila beach
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
azusa
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
bakerfield
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
bakerfsield
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
bakersfiel
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
bakersfield
</td>
<td style="text-align:right;">
705
</td>
</tr>
<tr>
<td style="text-align:left;">
bakersfiled
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
baldwin park
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
ballico
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
bangor
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
banning
</td>
<td style="text-align:right;">
78
</td>
</tr>
<tr>
<td style="text-align:left;">
barstow
</td>
<td style="text-align:right;">
52
</td>
</tr>
<tr>
<td style="text-align:left;">
bartsow
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
bay point
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
beamount
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
beaumont
</td>
<td style="text-align:right;">
43
</td>
</tr>
<tr>
<td style="text-align:left;">
bell
</td>
<td style="text-align:right;">
16
</td>
</tr>
<tr>
<td style="text-align:left;">
bell gardens
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
bellflower
</td>
<td style="text-align:right;">
12
</td>
</tr>
<tr>
<td style="text-align:left;">
ben lomond
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
benton
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
berkeley
</td>
<td style="text-align:right;">
17
</td>
</tr>
<tr>
<td style="text-align:left;">
bethel island
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
beumont
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
beverly hills
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
big bear
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
big bear lake
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
bishop
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
bloomington
</td>
<td style="text-align:right;">
13
</td>
</tr>
<tr>
<td style="text-align:left;">
bloomington/colton
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
blue jay
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
blue lake
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
blythe
</td>
<td style="text-align:right;">
19
</td>
</tr>
<tr>
<td style="text-align:left;">
bodega bay
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
bodfish
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
bolinas
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
bonny doon
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
boron
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
boulder creek
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
boulevard
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
brea
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
brentwood
</td>
<td style="text-align:right;">
14
</td>
</tr>
<tr>
<td style="text-align:left;">
bridgeport
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
bridgeville
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
brownsvalley
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
brownsville
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
buena park
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
burbank
</td>
<td style="text-align:right;">
16
</td>
</tr>
<tr>
<td style="text-align:left;">
burlingame
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
burney
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
buttonwillow
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
byron
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
ca
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
ca 92284
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
cabazon
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
cailmesa
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
cal city
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
calabasas
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
california city
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
calimesa
</td>
<td style="text-align:right;">
14
</td>
</tr>
<tr>
<td style="text-align:left;">
camarillo
</td>
<td style="text-align:right;">
15
</td>
</tr>
<tr>
<td style="text-align:left;">
cambria
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
campbell
</td>
<td style="text-align:right;">
17
</td>
</tr>
<tr>
<td style="text-align:left;">
campo
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
canoga park
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
canyon country
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
canyon lake
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
capay
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
capitola
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
carlotta
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
carlsbad
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
carmichael
</td>
<td style="text-align:right;">
14
</td>
</tr>
<tr>
<td style="text-align:left;">
carpinteria
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
carson
</td>
<td style="text-align:right;">
20
</td>
</tr>
<tr>
<td style="text-align:left;">
castaic
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
castella
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
castro valley
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
cathedral city
</td>
<td style="text-align:right;">
74
</td>
</tr>
<tr>
<td style="text-align:left;">
catherdal city
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
cathey valley
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
catheys valley
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
cayucos
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
cedar ridge
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
cedarville
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
ceres
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
cerritos
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
chalfant
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
chatsworth
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
cherry valley
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
chester
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
chico
</td>
<td style="text-align:right;">
58
</td>
</tr>
<tr>
<td style="text-align:left;">
chino
</td>
<td style="text-align:right;">
34
</td>
</tr>
<tr>
<td style="text-align:left;">
chino hills
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
chowchilla
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
chula vista
</td>
<td style="text-align:right;">
22
</td>
</tr>
<tr>
<td style="text-align:left;">
citrus heights
</td>
<td style="text-align:right;">
23
</td>
</tr>
<tr>
<td style="text-align:left;">
city of commerce
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
city of industry
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
clairemont
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
claremont
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
clayton
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
clearlake
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
clients home
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
clients home 745 crothers dr meadow vista
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
cloverdale
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
clovis
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
coachella
</td>
<td style="text-align:right;">
23
</td>
</tr>
<tr>
<td style="text-align:left;">
coalinga
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
cochella
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
cohasset
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
coleville
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
colfax
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
colton
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
colusa
</td>
<td style="text-align:right;">
22
</td>
</tr>
<tr>
<td style="text-align:left;">
commerce
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
compton
</td>
<td style="text-align:right;">
14
</td>
</tr>
<tr>
<td style="text-align:left;">
concord
</td>
<td style="text-align:right;">
63
</td>
</tr>
<tr>
<td style="text-align:left;">
corcoran
</td>
<td style="text-align:right;">
38
</td>
</tr>
<tr>
<td style="text-align:left;">
corning
</td>
<td style="text-align:right;">
30
</td>
</tr>
<tr>
<td style="text-align:left;">
corona
</td>
<td style="text-align:right;">
143
</td>
</tr>
<tr>
<td style="text-align:left;">
corte madera
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
costa mesa
</td>
<td style="text-align:right;">
23
</td>
</tr>
<tr>
<td style="text-align:left;">
cotati
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
cottonwood
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
coulterville
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
coulterville/la grange
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
covina
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
crescent city
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
crestline
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
crockett
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
crowslanding
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
cudahy
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
culver city
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
cupertino
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
cutten
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
cypress
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
daly city
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
dana point
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
davenport
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
davis
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
deep cleaning
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
delano
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
delhi
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
desert hot springs
</td>
<td style="text-align:right;">
129
</td>
</tr>
<tr>
<td style="text-align:left;">
desesrt hot springs
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
desrt hot springs
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
dinuba
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
discovery bay
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
dos palos
</td>
<td style="text-align:right;">
18
</td>
</tr>
<tr>
<td style="text-align:left;">
douglas city
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
downey
</td>
<td style="text-align:right;">
15
</td>
</tr>
<tr>
<td style="text-align:left;">
doyle
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
duarte
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
dunlap
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
dunnigan
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
east los angeles
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
east palo alto
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
eastvale
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
el cerrito
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
el monte
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
el nido
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
el portal
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
el salvador
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
el segundo
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
el sobrante
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
elcajon
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
eljon
</td>
<td style="text-align:right;">
38
</td>
</tr>
<tr>
<td style="text-align:left;">
elk grove
</td>
<td style="text-align:right;">
27
</td>
</tr>
<tr>
<td style="text-align:left;">
emeryville
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
encinitas
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
encino
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
escondido
</td>
<td style="text-align:right;">
28
</td>
</tr>
<tr>
<td style="text-align:left;">
eureka
</td>
<td style="text-align:right;">
107
</td>
</tr>
<tr>
<td style="text-align:left;">
exeter
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
exeter (tulle ville)
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
fairfax
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
fairfield
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
fallbrook
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
farmersville
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
felton
</td>
<td style="text-align:right;">
13
</td>
</tr>
<tr>
<td style="text-align:left;">
ferndale
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
fillmore
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
folsom
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
fontana
</td>
<td style="text-align:right;">
62
</td>
</tr>
<tr>
<td style="text-align:left;">
foresthill
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
forestville
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
fort bragg
</td>
<td style="text-align:right;">
35
</td>
</tr>
<tr>
<td style="text-align:left;">
fortuna
</td>
<td style="text-align:right;">
15
</td>
</tr>
<tr>
<td style="text-align:left;">
foster city
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
fountain valley
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
frasier park
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
frazier park
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
freedom
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
fremont
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
frenchmp
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
fresno
</td>
<td style="text-align:right;">
107
</td>
</tr>
<tr>
<td style="text-align:left;">
fullerton
</td>
<td style="text-align:right;">
15
</td>
</tr>
<tr>
<td style="text-align:left;">
fulton
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
garberville
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
garden grove
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
gardena
</td>
<td style="text-align:right;">
34
</td>
</tr>
<tr>
<td style="text-align:left;">
gazelle
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
gerber
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
gilroy
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
glen ellen
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
glendale
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
glendora
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
goleta
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
granada hills
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
grand terrace
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
granite bay
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
grass valley
</td>
<td style="text-align:right;">
191
</td>
</tr>
<tr>
<td style="text-align:left;">
gridley
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
grover beach
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
gualala
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
guerneville
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
gustine
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
hacienda heights
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
hamilton city
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
hammil valley
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
hanford
</td>
<td style="text-align:right;">
114
</td>
</tr>
<tr>
<td style="text-align:left;">
harbor city
</td>
<td style="text-align:right;">
16
</td>
</tr>
<tr>
<td style="text-align:left;">
hawthorne
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
hayfork
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
hayward
</td>
<td style="text-align:right;">
25
</td>
</tr>
<tr>
<td style="text-align:left;">
healdsburg
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
helendale
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
hemet
</td>
<td style="text-align:right;">
490
</td>
</tr>
<tr>
<td style="text-align:left;">
hercules
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
hesperia
</td>
<td style="text-align:right;">
28
</td>
</tr>
<tr>
<td style="text-align:left;">
highland
</td>
<td style="text-align:right;">
20
</td>
</tr>
<tr>
<td style="text-align:left;">
hilmar
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
hollywood
</td>
<td style="text-align:right;">
12
</td>
</tr>
<tr>
<td style="text-align:left;">
holmes
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
home of client
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
home repair
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
homeland
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
homeless
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
hoopa
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
huntington beach
</td>
<td style="text-align:right;">
19
</td>
</tr>
<tr>
<td style="text-align:left;">
huntington park
</td>
<td style="text-align:right;">
19
</td>
</tr>
<tr>
<td style="text-align:left;">
hyampom
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
idlewild
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
idyllwild
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
imperial beach
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
indian wells
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
indio
</td>
<td style="text-align:right;">
175
</td>
</tr>
<tr>
<td style="text-align:left;">
indio92203
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
inglewood
</td>
<td style="text-align:right;">
55
</td>
</tr>
<tr>
<td style="text-align:left;">
ione
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
iraq
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
irvine
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
isleton
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
ivanhoe
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
jackson
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
jamul
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
joshua tree
</td>
<td style="text-align:right;">
25
</td>
</tr>
<tr>
<td style="text-align:left;">
julian
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
june lake
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
jurupa
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
jurupa valley
</td>
<td style="text-align:right;">
57
</td>
</tr>
<tr>
<td style="text-align:left;">
kelseyville
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
kensington
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
kentfield
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
kerman
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
kernville
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
kettleman city
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
kingsburg
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
klamath
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
la crescenta
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
la grange
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
la habra
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
la jolla
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
la mesa
</td>
<td style="text-align:right;">
13
</td>
</tr>
<tr>
<td style="text-align:left;">
la mirada
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
la puente
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
la quinta
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
lafayette
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
laguna beach
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
laguna hills
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
laguna niguel
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
laguna woods
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
lake arrowhead
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
lake county
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
lake elsinore
</td>
<td style="text-align:right;">
121
</td>
</tr>
<tr>
<td style="text-align:left;">
lake forest
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
lake isabella
</td>
<td style="text-align:right;">
14
</td>
</tr>
<tr>
<td style="text-align:left;">
lakeport
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
lakeside
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
lakeview
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
lakewood
</td>
<td style="text-align:right;">
14
</td>
</tr>
<tr>
<td style="text-align:left;">
lamont
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
lancaster
</td>
<td style="text-align:right;">
56
</td>
</tr>
<tr>
<td style="text-align:left;">
landers
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
larkspur
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
lawndale
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
le grand
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
lebec
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
lee vining
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
lemon grove
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
lemoore
</td>
<td style="text-align:right;">
33
</td>
</tr>
<tr>
<td style="text-align:left;">
lewiston
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
lincoln
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
lincoln 3 referrals
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
lincoln 6/9/22- second home safe enrollment
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
lincoln heights
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
lincoln- home- grave disability hold
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
littlerock
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
livermore
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
lodi
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
loleta
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
loma linda
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
lomita
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
lompoc
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
lone pine
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
long beach
</td>
<td style="text-align:right;">
122
</td>
</tr>
<tr>
<td style="text-align:left;">
loomis
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
los altos
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
los angeles
</td>
<td style="text-align:right;">
567
</td>
</tr>
<tr>
<td style="text-align:left;">
los banos
</td>
<td style="text-align:right;">
21
</td>
</tr>
<tr>
<td style="text-align:left;">
los gatos
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
los molinos
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
los osos
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
lost hills
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
lower lake
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
lucerne valley
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
lynwood
</td>
<td style="text-align:right;">
13
</td>
</tr>
<tr>
<td style="text-align:left;">
macdoel
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
madera
</td>
<td style="text-align:right;">
52
</td>
</tr>
<tr>
<td style="text-align:left;">
madera ranchos
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
magalia
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
mammoth lakes
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
maricopa
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
marina del rey
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
mariposa
</td>
<td style="text-align:right;">
77
</td>
</tr>
<tr>
<td style="text-align:left;">
martell
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
martinez
</td>
<td style="text-align:right;">
17
</td>
</tr>
<tr>
<td style="text-align:left;">
marysville
</td>
<td style="text-align:right;">
15
</td>
</tr>
<tr>
<td style="text-align:left;">
maywood
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
mcfarland
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
mcgill
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
mckinleyville
</td>
<td style="text-align:right;">
17
</td>
</tr>
<tr>
<td style="text-align:left;">
mecca
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
menifee
</td>
<td style="text-align:right;">
66
</td>
</tr>
<tr>
<td style="text-align:left;">
mentone
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
merced
</td>
<td style="text-align:right;">
92
</td>
</tr>
<tr>
<td style="text-align:left;">
midpines
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
mill valley
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
millbrae
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
milpitas
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
mira loma
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
missing
</td>
<td style="text-align:right;">
110
</td>
</tr>
<tr>
<td style="text-align:left;">
mission hills
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
mission hlls
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
modesto
</td>
<td style="text-align:right;">
60
</td>
</tr>
<tr>
<td style="text-align:left;">
mojave
</td>
<td style="text-align:right;">
14
</td>
</tr>
<tr>
<td style="text-align:left;">
monrovia
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
montclair
</td>
<td style="text-align:right;">
23
</td>
</tr>
<tr>
<td style="text-align:left;">
monte rio
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
montebello
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
monterey
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
monterey park
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
monti rio
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
moorpark
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
moren
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
moreno valley
</td>
<td style="text-align:right;">
201
</td>
</tr>
<tr>
<td style="text-align:left;">
morgan hill
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
morongo valley
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
morro bay
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
mount mesa
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
mount shasta
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
mountain center
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
mountain view
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
mountianview
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
mt. view
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
murreta
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
murrieta
</td>
<td style="text-align:right;">
70
</td>
</tr>
<tr>
<td style="text-align:left;">
muscoy
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
napa
</td>
<td style="text-align:right;">
17
</td>
</tr>
<tr>
<td style="text-align:left;">
national city
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
needles
</td>
<td style="text-align:right;">
29
</td>
</tr>
<tr>
<td style="text-align:left;">
nevada city
</td>
<td style="text-align:right;">
51
</td>
</tr>
<tr>
<td style="text-align:left;">
newberry springs
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
newbury park
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
newcastle
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
newhall
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
newman
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
newport beach
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
nicasio
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
nipomo
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
no
</td>
<td style="text-align:right;">
28
</td>
</tr>
<tr>
<td style="text-align:left;">
no intervention
</td>
<td style="text-align:right;">
929
</td>
</tr>
<tr>
<td style="text-align:left;">
none
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
norco
</td>
<td style="text-align:right;">
28
</td>
</tr>
<tr>
<td style="text-align:left;">
north highlands
</td>
<td style="text-align:right;">
29
</td>
</tr>
<tr>
<td style="text-align:left;">
north hills
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
north hollywood
</td>
<td style="text-align:right;">
20
</td>
</tr>
<tr>
<td style="text-align:left;">
north palm springs
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
north san juan
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
north side of parking lot
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
northridge
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
norwalk
</td>
<td style="text-align:right;">
20
</td>
</tr>
<tr>
<td style="text-align:left;">
novato
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
nuevo
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
oak view
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
oakdale
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
oakhurst
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
oakland
</td>
<td style="text-align:right;">
84
</td>
</tr>
<tr>
<td style="text-align:left;">
oakley
</td>
<td style="text-align:right;">
12
</td>
</tr>
<tr>
<td style="text-align:left;">
oceanside
</td>
<td style="text-align:right;">
18
</td>
</tr>
<tr>
<td style="text-align:left;">
ojai
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
olema
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
olivehurst
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
ontario
</td>
<td style="text-align:right;">
74
</td>
</tr>
<tr>
<td style="text-align:left;">
onyx
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
orange
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
orange cove
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
orick
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
orinda
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
orland
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
oroville
</td>
<td style="text-align:right;">
25
</td>
</tr>
<tr>
<td style="text-align:left;">
other
</td>
<td style="text-align:right;">
36
</td>
</tr>
<tr>
<td style="text-align:left;">
oxnard
</td>
<td style="text-align:right;">
31
</td>
</tr>
<tr>
<td style="text-align:left;">
pachecco
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
pacheco
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
pacoima
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
pala
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
palm desert
</td>
<td style="text-align:right;">
79
</td>
</tr>
<tr>
<td style="text-align:left;">
palm dprings
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
palm spring
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
palm springs
</td>
<td style="text-align:right;">
379
</td>
</tr>
<tr>
<td style="text-align:left;">
palmdale
</td>
<td style="text-align:right;">
22
</td>
</tr>
<tr>
<td style="text-align:left;">
palms springs
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
palo alto
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
panorama city
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
paradise
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
paramount
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
parlier
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
pasadena
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
paso robles
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
patterson
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
pauma valley
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
penn valley
</td>
<td style="text-align:right;">
16
</td>
</tr>
<tr>
<td style="text-align:left;">
perris
</td>
<td style="text-align:right;">
108
</td>
</tr>
<tr>
<td style="text-align:left;">
petaluma
</td>
<td style="text-align:right;">
28
</td>
</tr>
<tr>
<td style="text-align:left;">
phillipsville
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
pico rivera
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
piedmont
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
pine grove
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
pinole
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
pinon hills
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
pioneer
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
pismo beach
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
pittsburg
</td>
<td style="text-align:right;">
36
</td>
</tr>
<tr>
<td style="text-align:left;">
placentia
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
placer county
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
planada
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
playa del rey
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
playa vista
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
pleasant hill
</td>
<td style="text-align:right;">
16
</td>
</tr>
<tr>
<td style="text-align:left;">
pleasanton
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
point arena
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
pomona
</td>
<td style="text-align:right;">
34
</td>
</tr>
<tr>
<td style="text-align:left;">
port hueneme
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
porterville
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
portola
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
poway
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
pt. reyes
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
quincy
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
ramona
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
ranch mirage
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
rancho
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
rancho bernardo
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
rancho cordova
</td>
<td style="text-align:right;">
20
</td>
</tr>
<tr>
<td style="text-align:left;">
rancho cucamong
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
rancho cucamonga
</td>
<td style="text-align:right;">
52
</td>
</tr>
<tr>
<td style="text-align:left;">
rancho mirage
</td>
<td style="text-align:right;">
42
</td>
</tr>
<tr>
<td style="text-align:left;">
rancho palos verdes
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
rancho santa fe
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
rancho santa margarita
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
red bluff
</td>
<td style="text-align:right;">
71
</td>
</tr>
<tr>
<td style="text-align:left;">
redcrest
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
redding
</td>
<td style="text-align:right;">
38
</td>
</tr>
<tr>
<td style="text-align:left;">
redlands
</td>
<td style="text-align:right;">
27
</td>
</tr>
<tr>
<td style="text-align:left;">
redondo beach
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
redway
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
redwood city
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
redwood valley
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
reedley
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
rent payment
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
renting a room at: 1437 stone hearth ,lincoln
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
reseda
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
rialto
</td>
<td style="text-align:right;">
58
</td>
</tr>
<tr>
<td style="text-align:left;">
richmond
</td>
<td style="text-align:right;">
56
</td>
</tr>
<tr>
<td style="text-align:left;">
richomond
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
ridegcrest
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
ridgecrest
</td>
<td style="text-align:right;">
18
</td>
</tr>
<tr>
<td style="text-align:left;">
rio dell
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
rio linda
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
rio vista
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
river pines
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
riverbank
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
riverdale
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
riverisde
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
riverside
</td>
<td style="text-align:right;">
795
</td>
</tr>
<tr>
<td style="text-align:left;">
riveside
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
rocklin
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
rocklin 95677
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
rocklin-
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
rodeo
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
rohnert park
</td>
<td style="text-align:right;">
41
</td>
</tr>
<tr>
<td style="text-align:left;">
romoland
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
rosamond
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
rosemead
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
rosevile snf oakridgere center
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
roseville
</td>
<td style="text-align:right;">
44
</td>
</tr>
<tr>
<td style="text-align:left;">
roseville 95661
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
roseville 95661 second home safe intervention 22
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
roseville 95678
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
rough & ready
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
rough and ready
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
rough and ready 95975
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
rowland heights
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
rubidoux
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
running springs
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
ruth
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
sacramento
</td>
<td style="text-align:right;">
225
</td>
</tr>
<tr>
<td style="text-align:left;">
sacrmento
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
salton sea
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
samoa
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
san andreas
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
san anselmo
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
san berandino
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
san beranrdino
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
san bernadino
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
san bernardino
</td>
<td style="text-align:right;">
162
</td>
</tr>
<tr>
<td style="text-align:left;">
san bernardino.
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
san bernrdino
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
san bruno
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
san clemente
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
san diego
</td>
<td style="text-align:right;">
175
</td>
</tr>
<tr>
<td style="text-align:left;">
san dimas
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
san fernando
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
san francisco
</td>
<td style="text-align:right;">
245
</td>
</tr>
<tr>
<td style="text-align:left;">
san francsico
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
san gabriel
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
san jacinto
</td>
<td style="text-align:right;">
103
</td>
</tr>
<tr>
<td style="text-align:left;">
san jose
</td>
<td style="text-align:right;">
108
</td>
</tr>
<tr>
<td style="text-align:left;">
san juanpistrano
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
san leandro
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
san lorenzo
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
san luis obispo
</td>
<td style="text-align:right;">
14
</td>
</tr>
<tr>
<td style="text-align:left;">
san marcos
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
san martin
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
san mateo
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
san pablo
</td>
<td style="text-align:right;">
29
</td>
</tr>
<tr>
<td style="text-align:left;">
san pedro
</td>
<td style="text-align:right;">
37
</td>
</tr>
<tr>
<td style="text-align:left;">
san rafael
</td>
<td style="text-align:right;">
16
</td>
</tr>
<tr>
<td style="text-align:left;">
san ramon
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
san ysidro
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
sanjose
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
santa ana
</td>
<td style="text-align:right;">
30
</td>
</tr>
<tr>
<td style="text-align:left;">
santa barbara
</td>
<td style="text-align:right;">
19
</td>
</tr>
<tr>
<td style="text-align:left;">
santa clara
</td>
<td style="text-align:right;">
19
</td>
</tr>
<tr>
<td style="text-align:left;">
santa clarita
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
santa cruz
</td>
<td style="text-align:right;">
120
</td>
</tr>
<tr>
<td style="text-align:left;">
santa fe springs
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
santa maria
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
santa monica
</td>
<td style="text-align:right;">
14
</td>
</tr>
<tr>
<td style="text-align:left;">
santa nella
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
santa paula
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
santa rosa
</td>
<td style="text-align:right;">
129
</td>
</tr>
<tr>
<td style="text-align:left;">
santee
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
saratoga
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
saugus
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
sausalito
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
scotia
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
scotts valley
</td>
<td style="text-align:right;">
13
</td>
</tr>
<tr>
<td style="text-align:left;">
seal beach
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
sebastopol
</td>
<td style="text-align:right;">
14
</td>
</tr>
<tr>
<td style="text-align:left;">
security deposit
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
selma
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
sf
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
shafter
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
shelter cove
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
sheridan
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
sherman oaks
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
signal hill
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
silver lake
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
silverado
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
simi valley
</td>
<td style="text-align:right;">
16
</td>
</tr>
<tr>
<td style="text-align:left;">
sin city
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
smartsville
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
soda springs
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
sonoma
</td>
<td style="text-align:right;">
21
</td>
</tr>
<tr>
<td style="text-align:left;">
soquel
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
south dos palos
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
south el montelifornia
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
south gate
</td>
<td style="text-align:right;">
18
</td>
</tr>
<tr>
<td style="text-align:left;">
south pasadena
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
south san francisco
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
spring valley
</td>
<td style="text-align:right;">
18
</td>
</tr>
<tr>
<td style="text-align:left;">
stanton
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
stevinson
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
stockton
</td>
<td style="text-align:right;">
23
</td>
</tr>
<tr>
<td style="text-align:left;">
stratford
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
strathmore
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
studio city
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
sugar loaf
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
sugarloaf
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
suisun city
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
sun city
</td>
<td style="text-align:right;">
22
</td>
</tr>
<tr>
<td style="text-align:left;">
sun valley
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
sunland
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
sunnyvale
</td>
<td style="text-align:right;">
31
</td>
</tr>
<tr>
<td style="text-align:left;">
susanville
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
sylmar
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
taft
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
tahoe
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
tahoe city
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
tarzana
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
tehacahpi
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
tehachapi
</td>
<td style="text-align:right;">
25
</td>
</tr>
<tr>
<td style="text-align:left;">
tehachipi
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
tehama
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
temecula
</td>
<td style="text-align:right;">
122
</td>
</tr>
<tr>
<td style="text-align:left;">
temple city
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
temporary housing
</td>
<td style="text-align:right;">
12
</td>
</tr>
<tr>
<td style="text-align:left;">
the sea ranch
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
thermal
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
thousand oaks
</td>
<td style="text-align:right;">
13
</td>
</tr>
<tr>
<td style="text-align:left;">
thousand palms
</td>
<td style="text-align:right;">
13
</td>
</tr>
<tr>
<td style="text-align:left;">
tiburon
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
tipton
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
topanga
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
topaz
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
torrance
</td>
<td style="text-align:right;">
51
</td>
</tr>
<tr>
<td style="text-align:left;">
trinidad
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
trona
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
truckee
</td>
<td style="text-align:right;">
12
</td>
</tr>
<tr>
<td style="text-align:left;">
tujunga
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
tulare
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
turlock
</td>
<td style="text-align:right;">
18
</td>
</tr>
<tr>
<td style="text-align:left;">
turlock 95380
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
tustin
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
twenty nine palms
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
twenty-nine palms
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
twentynine palms
</td>
<td style="text-align:right;">
48
</td>
</tr>
<tr>
<td style="text-align:left;">
ukiah
</td>
<td style="text-align:right;">
36
</td>
</tr>
<tr>
<td style="text-align:left;">
union city
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
unk
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
554
</td>
</tr>
<tr>
<td style="text-align:left;">
upland
</td>
<td style="text-align:right;">
20
</td>
</tr>
<tr>
<td style="text-align:left;">
utilities
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
vacaville
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
valencia
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
vallejo
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
valley center
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
van nuys
</td>
<td style="text-align:right;">
26
</td>
</tr>
<tr>
<td style="text-align:left;">
venice
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
ventura
</td>
<td style="text-align:right;">
60
</td>
</tr>
<tr>
<td style="text-align:left;">
victirville
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
victorville
</td>
<td style="text-align:right;">
72
</td>
</tr>
<tr>
<td style="text-align:left;">
vina
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
visalia
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
vista
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
volcano
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
walnut
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
walnut creek
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
walnut park
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
wasco
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
watsonville
</td>
<td style="text-align:right;">
53
</td>
</tr>
<tr>
<td style="text-align:left;">
weavervile
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
weaverville
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
weldon
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
west covina
</td>
<td style="text-align:right;">
14
</td>
</tr>
<tr>
<td style="text-align:left;">
west hills
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
west hollywood
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
west sacramento
</td>
<td style="text-align:right;">
19
</td>
</tr>
<tr>
<td style="text-align:left;">
westminster
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
westwood
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
wheatland
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
whitethorn
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
whitewater
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
whittier
</td>
<td style="text-align:right;">
13
</td>
</tr>
<tr>
<td style="text-align:left;">
wildomar
</td>
<td style="text-align:right;">
29
</td>
</tr>
<tr>
<td style="text-align:left;">
wildomor
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
willits
</td>
<td style="text-align:right;">
22
</td>
</tr>
<tr>
<td style="text-align:left;">
willow creek
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
willowbrook
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
willows
</td>
<td style="text-align:right;">
17
</td>
</tr>
<tr>
<td style="text-align:left;">
wilmington
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
winchester
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
windsor
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
winnetka
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
winters
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
winton
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
wofford heights
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
wonder valley
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
woodland
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
woodland hills
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
yermo
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
yes
</td>
<td style="text-align:right;">
29
</td>
</tr>
<tr>
<td style="text-align:left;">
ylp
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
yorba linda
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
yountville
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
yreka
</td>
<td style="text-align:right;">
18
</td>
</tr>
<tr>
<td style="text-align:left;">
yuba county
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
yuba county jail
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
yucaipa
</td>
<td style="text-align:right;">
40
</td>
</tr>
<tr>
<td style="text-align:left;">
yucaipa 92399
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
yucca
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
yucca valley
</td>
<td style="text-align:right;">
92
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
702
</td>
</tr>
</tbody>
</table>

</div>

## Question for Gina

Should we drop everything from ” -” (i.e. space and then dash)

``` r
demo_dat %>%  
  count(aps_report_location) %>% 
  filter(str_detect(aps_report_location, "-")) %>% 
  filter(str_detect(aps_report_location, "-"))
```

<div class="kable-table">

<table>
<thead>
<tr>
<th style="text-align:left;">
aps_report_location
</th>
<th style="text-align:right;">
n
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
–please select–
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
auburn -95603
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
lincoln 6/9/22- second home safe enrollment
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
lincoln- home- grave disability hold
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
rocklin-
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
twenty-nine palms
</td>
<td style="text-align:right;">
1
</td>
</tr>
</tbody>
</table>

</div>

replace all locations that contain Roseville in the name to just
Roseville as a new location variable

``` r
demo_dat <- demo_dat %>% 
  mutate(aps_location_recode = case_when(str_detect(aps_report_location,"rosev") ~ "roseville",
                                      
                                         TRUE ~ aps_report_location))
```

check

``` r
demo_dat %>% 
  count(aps_report_location, aps_location_recode) %>% 
  filter(str_starts(aps_report_location, "r"))
```

<div class="kable-table">

<table>
<thead>
<tr>
<th style="text-align:left;">
aps_report_location
</th>
<th style="text-align:left;">
aps_location_recode
</th>
<th style="text-align:right;">
n
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
ramona
</td>
<td style="text-align:left;">
ramona
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
ranch mirage
</td>
<td style="text-align:left;">
ranch mirage
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
rancho
</td>
<td style="text-align:left;">
rancho
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
rancho bernardo
</td>
<td style="text-align:left;">
rancho bernardo
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
rancho cordova
</td>
<td style="text-align:left;">
rancho cordova
</td>
<td style="text-align:right;">
20
</td>
</tr>
<tr>
<td style="text-align:left;">
rancho cucamong
</td>
<td style="text-align:left;">
rancho cucamong
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
rancho cucamonga
</td>
<td style="text-align:left;">
rancho cucamonga
</td>
<td style="text-align:right;">
52
</td>
</tr>
<tr>
<td style="text-align:left;">
rancho mirage
</td>
<td style="text-align:left;">
rancho mirage
</td>
<td style="text-align:right;">
42
</td>
</tr>
<tr>
<td style="text-align:left;">
rancho palos verdes
</td>
<td style="text-align:left;">
rancho palos verdes
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
rancho santa fe
</td>
<td style="text-align:left;">
rancho santa fe
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
rancho santa margarita
</td>
<td style="text-align:left;">
rancho santa margarita
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
red bluff
</td>
<td style="text-align:left;">
red bluff
</td>
<td style="text-align:right;">
71
</td>
</tr>
<tr>
<td style="text-align:left;">
redcrest
</td>
<td style="text-align:left;">
redcrest
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
redding
</td>
<td style="text-align:left;">
redding
</td>
<td style="text-align:right;">
38
</td>
</tr>
<tr>
<td style="text-align:left;">
redlands
</td>
<td style="text-align:left;">
redlands
</td>
<td style="text-align:right;">
27
</td>
</tr>
<tr>
<td style="text-align:left;">
redondo beach
</td>
<td style="text-align:left;">
redondo beach
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
redway
</td>
<td style="text-align:left;">
redway
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
redwood city
</td>
<td style="text-align:left;">
redwood city
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
redwood valley
</td>
<td style="text-align:left;">
redwood valley
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
reedley
</td>
<td style="text-align:left;">
reedley
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
rent payment
</td>
<td style="text-align:left;">
rent payment
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
renting a room at: 1437 stone hearth ,lincoln
</td>
<td style="text-align:left;">
renting a room at: 1437 stone hearth ,lincoln
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
reseda
</td>
<td style="text-align:left;">
reseda
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
rialto
</td>
<td style="text-align:left;">
rialto
</td>
<td style="text-align:right;">
58
</td>
</tr>
<tr>
<td style="text-align:left;">
richmond
</td>
<td style="text-align:left;">
richmond
</td>
<td style="text-align:right;">
56
</td>
</tr>
<tr>
<td style="text-align:left;">
richomond
</td>
<td style="text-align:left;">
richomond
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
ridegcrest
</td>
<td style="text-align:left;">
ridegcrest
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
ridgecrest
</td>
<td style="text-align:left;">
ridgecrest
</td>
<td style="text-align:right;">
18
</td>
</tr>
<tr>
<td style="text-align:left;">
rio dell
</td>
<td style="text-align:left;">
rio dell
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
rio linda
</td>
<td style="text-align:left;">
rio linda
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
rio vista
</td>
<td style="text-align:left;">
rio vista
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
river pines
</td>
<td style="text-align:left;">
river pines
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
riverbank
</td>
<td style="text-align:left;">
riverbank
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
riverdale
</td>
<td style="text-align:left;">
riverdale
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
riverisde
</td>
<td style="text-align:left;">
riverisde
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
riverside
</td>
<td style="text-align:left;">
riverside
</td>
<td style="text-align:right;">
795
</td>
</tr>
<tr>
<td style="text-align:left;">
riveside
</td>
<td style="text-align:left;">
riveside
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
rocklin
</td>
<td style="text-align:left;">
rocklin
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
rocklin 95677
</td>
<td style="text-align:left;">
rocklin 95677
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
rocklin-
</td>
<td style="text-align:left;">
rocklin-
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
rodeo
</td>
<td style="text-align:left;">
rodeo
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
rohnert park
</td>
<td style="text-align:left;">
rohnert park
</td>
<td style="text-align:right;">
41
</td>
</tr>
<tr>
<td style="text-align:left;">
romoland
</td>
<td style="text-align:left;">
romoland
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
rosamond
</td>
<td style="text-align:left;">
rosamond
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
rosemead
</td>
<td style="text-align:left;">
rosemead
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
rosevile snf oakridgere center
</td>
<td style="text-align:left;">
roseville
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
roseville
</td>
<td style="text-align:left;">
roseville
</td>
<td style="text-align:right;">
44
</td>
</tr>
<tr>
<td style="text-align:left;">
roseville 95661
</td>
<td style="text-align:left;">
roseville
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
roseville 95661 second home safe intervention 22
</td>
<td style="text-align:left;">
roseville
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
roseville 95678
</td>
<td style="text-align:left;">
roseville
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
rough & ready
</td>
<td style="text-align:left;">
rough & ready
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
rough and ready
</td>
<td style="text-align:left;">
rough and ready
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
rough and ready 95975
</td>
<td style="text-align:left;">
rough and ready 95975
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
rowland heights
</td>
<td style="text-align:left;">
rowland heights
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
rubidoux
</td>
<td style="text-align:left;">
rubidoux
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
running springs
</td>
<td style="text-align:left;">
running springs
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
ruth
</td>
<td style="text-align:left;">
ruth
</td>
<td style="text-align:right;">
1
</td>
</tr>
</tbody>
</table>

</div>

Counts of `gender`

``` r
demo_dat %>% 
  count(race_1, gender_identity)
```

<div class="kable-table">

<table>
<thead>
<tr>
<th style="text-align:left;">
race_1
</th>
<th style="text-align:left;">
gender_identity
</th>
<th style="text-align:right;">
n
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
\#
</td>
<td style="text-align:left;">
\#
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44252
</td>
<td style="text-align:left;">
rent leaseholder
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44376
</td>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44515
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44519
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44600
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
44769
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
44774
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
american indian/alaskan native
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
20
</td>
</tr>
<tr>
<td style="text-align:left;">
american indian/alaskan native
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
american indian/alaskan native/indigenous
</td>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
american indian/alaskan native/indigenous
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
71
</td>
</tr>
<tr>
<td style="text-align:left;">
american indian/alaskan native/indigenous
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
37
</td>
</tr>
<tr>
<td style="text-align:left;">
asian
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
61
</td>
</tr>
<tr>
<td style="text-align:left;">
asian
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
32
</td>
</tr>
<tr>
<td style="text-align:left;">
asian
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
asian/asian american
</td>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
asian/asian american
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
100
</td>
</tr>
<tr>
<td style="text-align:left;">
asian/asian american
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
94
</td>
</tr>
<tr>
<td style="text-align:left;">
black/african american
</td>
<td style="text-align:left;">
black/african american
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
black/african american
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
337
</td>
</tr>
<tr>
<td style="text-align:left;">
black/african american
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
244
</td>
</tr>
<tr>
<td style="text-align:left;">
black/african american
</td>
<td style="text-align:left;">
unknown/not provided
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
black/african american
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
black/african american/african
</td>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
black/african american/african
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
552
</td>
</tr>
<tr>
<td style="text-align:left;">
black/african american/african
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
551
</td>
</tr>
<tr>
<td style="text-align:left;">
black/african american/african
</td>
<td style="text-align:left;">
transgender
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
black/african american/african
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
client doesn’t know
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
23
</td>
</tr>
<tr>
<td style="text-align:left;">
client doesn’t know
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
19
</td>
</tr>
<tr>
<td style="text-align:left;">
client refused
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
18
</td>
</tr>
<tr>
<td style="text-align:left;">
client refused
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
15
</td>
</tr>
<tr>
<td style="text-align:left;">
client refused
</td>
<td style="text-align:left;">
transgender
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:left;">
american indian/alaskan native
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:right;">
38
</td>
</tr>
<tr>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
374
</td>
</tr>
<tr>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
330
</td>
</tr>
<tr>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:left;">
transgender
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:left;">
unknown/not provided
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:left;">
white
</td>
<td style="text-align:right;">
66
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
29 palms
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
adelanto
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
alta loma
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
apple valley
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
barstow
</td>
<td style="text-align:right;">
22
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
bartsow
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
big bear lake
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
bloomington
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
bloomington/colton
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
blue jay
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
ca
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
ca 92284
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
chino
</td>
<td style="text-align:right;">
21
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
chino hills
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
colton
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
crestline
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
fontana
</td>
<td style="text-align:right;">
28
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
grand terrace
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
helendale
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
hemet
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
hesperia
</td>
<td style="text-align:right;">
18
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
highland
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
homeless
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
joshua tree
</td>
<td style="text-align:right;">
14
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
lake arrowhead
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
landers
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
loma linda
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
lucerne valley
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
montclair
</td>
<td style="text-align:right;">
12
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
morongo valley
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
muscoy
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
needles
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
ontario
</td>
<td style="text-align:right;">
42
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
pinon hills
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
rancho
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
rancho cucamonga
</td>
<td style="text-align:right;">
34
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
redlands
</td>
<td style="text-align:right;">
15
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
rialto
</td>
<td style="text-align:right;">
30
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
riverside
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
san bernadino
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
san bernardino
</td>
<td style="text-align:right;">
78
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
san bernrdino
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
sugarloaf
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
trona
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
twenty-nine palms
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
twentynine palms
</td>
<td style="text-align:right;">
25
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
unk
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
upland
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
victorville
</td>
<td style="text-align:right;">
29
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
wonder valley
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
yucaipa
</td>
<td style="text-align:right;">
18
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
yucaipa ca 92399
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
yucca valley
</td>
<td style="text-align:right;">
61
</td>
</tr>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
14
</td>
</tr>
<tr>
<td style="text-align:left;">
male
</td>
<td style="text-align:left;">
29 palms
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
male
</td>
<td style="text-align:left;">
adelanto
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
male
</td>
<td style="text-align:left;">
alta loma
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
male
</td>
<td style="text-align:left;">
apple valley
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
male
</td>
<td style="text-align:left;">
barstow
</td>
<td style="text-align:right;">
23
</td>
</tr>
<tr>
<td style="text-align:left;">
male
</td>
<td style="text-align:left;">
big bear
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
male
</td>
<td style="text-align:left;">
bloomington
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
male
</td>
<td style="text-align:left;">
chino
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
male
</td>
<td style="text-align:left;">
chino hills
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
male
</td>
<td style="text-align:left;">
colton
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
male
</td>
<td style="text-align:left;">
crestline
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
male
</td>
<td style="text-align:left;">
fontana
</td>
<td style="text-align:right;">
27
</td>
</tr>
<tr>
<td style="text-align:left;">
male
</td>
<td style="text-align:left;">
grand terrace
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
male
</td>
<td style="text-align:left;">
hesperia
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
male
</td>
<td style="text-align:left;">
highland
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
male
</td>
<td style="text-align:left;">
homeless
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
male
</td>
<td style="text-align:left;">
joshua tree
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
male
</td>
<td style="text-align:left;">
landers
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
male
</td>
<td style="text-align:left;">
loma linda
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
male
</td>
<td style="text-align:left;">
los angeles
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
male
</td>
<td style="text-align:left;">
lucerne valley
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
male
</td>
<td style="text-align:left;">
mentone
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
male
</td>
<td style="text-align:left;">
montclair
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
male
</td>
<td style="text-align:left;">
morongo valley
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
male
</td>
<td style="text-align:left;">
needles
</td>
<td style="text-align:right;">
12
</td>
</tr>
<tr>
<td style="text-align:left;">
male
</td>
<td style="text-align:left;">
newberry springs
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
male
</td>
<td style="text-align:left;">
ontario
</td>
<td style="text-align:right;">
24
</td>
</tr>
<tr>
<td style="text-align:left;">
male
</td>
<td style="text-align:left;">
rancho cucamong
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
male
</td>
<td style="text-align:left;">
rancho cucamonga
</td>
<td style="text-align:right;">
18
</td>
</tr>
<tr>
<td style="text-align:left;">
male
</td>
<td style="text-align:left;">
redlands
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
male
</td>
<td style="text-align:left;">
rialto
</td>
<td style="text-align:right;">
19
</td>
</tr>
<tr>
<td style="text-align:left;">
male
</td>
<td style="text-align:left;">
running springs
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
male
</td>
<td style="text-align:left;">
san beranrdino
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
male
</td>
<td style="text-align:left;">
san bernardino
</td>
<td style="text-align:right;">
67
</td>
</tr>
<tr>
<td style="text-align:left;">
male
</td>
<td style="text-align:left;">
sugar loaf
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
male
</td>
<td style="text-align:left;">
sugarloaf
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
male
</td>
<td style="text-align:left;">
twenty nine palms
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
male
</td>
<td style="text-align:left;">
twentynine palms
</td>
<td style="text-align:right;">
22
</td>
</tr>
<tr>
<td style="text-align:left;">
male
</td>
<td style="text-align:left;">
upland
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
male
</td>
<td style="text-align:left;">
victirville
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
male
</td>
<td style="text-align:left;">
victorville
</td>
<td style="text-align:right;">
37
</td>
</tr>
<tr>
<td style="text-align:left;">
male
</td>
<td style="text-align:left;">
yermo
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
male
</td>
<td style="text-align:left;">
yucaipa
</td>
<td style="text-align:right;">
16
</td>
</tr>
<tr>
<td style="text-align:left;">
male
</td>
<td style="text-align:left;">
yucca
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
male
</td>
<td style="text-align:left;">
yucca valley
</td>
<td style="text-align:right;">
24
</td>
</tr>
<tr>
<td style="text-align:left;">
male
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
native hawaiian/pacific islander
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
15
</td>
</tr>
<tr>
<td style="text-align:left;">
native hawaiian/pacific islander
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
17
</td>
</tr>
<tr>
<td style="text-align:left;">
native hawaiian/pacific islander
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
other
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
681
</td>
</tr>
<tr>
<td style="text-align:left;">
other
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
600
</td>
</tr>
<tr>
<td style="text-align:left;">
other
</td>
<td style="text-align:left;">
transgender
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
other
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
other
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
pacific islander/native hawaiian
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
21
</td>
</tr>
<tr>
<td style="text-align:left;">
pacific islander/native hawaiian
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
transgender
</td>
<td style="text-align:left;">
barstow
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
unknown/not provided
</td>
<td style="text-align:left;">
barstow
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
unknown/not provided
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
921
</td>
</tr>
<tr>
<td style="text-align:left;">
unknown/not provided
</td>
<td style="text-align:left;">
highland
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
unknown/not provided
</td>
<td style="text-align:left;">
joshua tree
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
unknown/not provided
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
726
</td>
</tr>
<tr>
<td style="text-align:left;">
unknown/not provided
</td>
<td style="text-align:left;">
needles
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
unknown/not provided
</td>
<td style="text-align:left;">
redlands
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
unknown/not provided
</td>
<td style="text-align:left;">
san bernardino
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
unknown/not provided
</td>
<td style="text-align:left;">
transgender
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
unknown/not provided
</td>
<td style="text-align:left;">
unknown/not provided
</td>
<td style="text-align:right;">
142
</td>
</tr>
<tr>
<td style="text-align:left;">
unknown/not provided
</td>
<td style="text-align:left;">
white
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
unknown/not provided
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
15
</td>
</tr>
<tr>
<td style="text-align:left;">
white
</td>
<td style="text-align:left;">
american indian/alaskan native
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
white
</td>
<td style="text-align:left;">
asian
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
white
</td>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:right;">
15
</td>
</tr>
<tr>
<td style="text-align:left;">
white
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
3906
</td>
</tr>
<tr>
<td style="text-align:left;">
white
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
3296
</td>
</tr>
<tr>
<td style="text-align:left;">
white
</td>
<td style="text-align:left;">
other/non-binary
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
white
</td>
<td style="text-align:left;">
pacific islander/native hawaiian
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
white
</td>
<td style="text-align:left;">
transgender
</td>
<td style="text-align:right;">
13
</td>
</tr>
<tr>
<td style="text-align:left;">
white
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
white
</td>
<td style="text-align:left;">
unknown/not provided
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
white
</td>
<td style="text-align:left;">
white
</td>
<td style="text-align:right;">
41
</td>
</tr>
<tr>
<td style="text-align:left;">
white
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
22
</td>
</tr>
<tr>
<td style="text-align:left;">
whte
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
homeless
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
196
</td>
</tr>
</tbody>
</table>

</div>

- Ask Gina, is the data transposition between `race` variables and
  `gender` being corrected prior to getting to us?

What does the `gender` data look like when `race_1` equals`transgender`
,`female` or `male`

``` r
demo_dat %>% 
  filter(race_1 %in% c("female", "male", "transgender")) %>% 
  count(gender_identity, race_1)
```

<div class="kable-table">

<table>
<thead>
<tr>
<th style="text-align:left;">
gender_identity
</th>
<th style="text-align:left;">
race_1
</th>
<th style="text-align:right;">
n
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
29 palms
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
29 palms
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
adelanto
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
adelanto
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
alta loma
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
alta loma
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
apple valley
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
apple valley
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
barstow
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
22
</td>
</tr>
<tr>
<td style="text-align:left;">
barstow
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
23
</td>
</tr>
<tr>
<td style="text-align:left;">
barstow
</td>
<td style="text-align:left;">
transgender
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
bartsow
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
big bear
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
big bear lake
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
bloomington
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
bloomington
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
bloomington/colton
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
blue jay
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
ca
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
ca 92284
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
chino
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
21
</td>
</tr>
<tr>
<td style="text-align:left;">
chino
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
chino hills
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
chino hills
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
colton
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
colton
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
crestline
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
crestline
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
fontana
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
28
</td>
</tr>
<tr>
<td style="text-align:left;">
fontana
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
27
</td>
</tr>
<tr>
<td style="text-align:left;">
grand terrace
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
grand terrace
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
helendale
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
hemet
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
hesperia
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
18
</td>
</tr>
<tr>
<td style="text-align:left;">
hesperia
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
highland
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
highland
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
homeless
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
homeless
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
joshua tree
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
14
</td>
</tr>
<tr>
<td style="text-align:left;">
joshua tree
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
lake arrowhead
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
landers
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
landers
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
loma linda
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
loma linda
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
los angeles
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
lucerne valley
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
lucerne valley
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
mentone
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
montclair
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
12
</td>
</tr>
<tr>
<td style="text-align:left;">
montclair
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
morongo valley
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
morongo valley
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
muscoy
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
needles
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
needles
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
12
</td>
</tr>
<tr>
<td style="text-align:left;">
newberry springs
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
ontario
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
42
</td>
</tr>
<tr>
<td style="text-align:left;">
ontario
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
24
</td>
</tr>
<tr>
<td style="text-align:left;">
pinon hills
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
rancho
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
rancho cucamong
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
rancho cucamonga
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
34
</td>
</tr>
<tr>
<td style="text-align:left;">
rancho cucamonga
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
18
</td>
</tr>
<tr>
<td style="text-align:left;">
redlands
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
15
</td>
</tr>
<tr>
<td style="text-align:left;">
redlands
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
rialto
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
30
</td>
</tr>
<tr>
<td style="text-align:left;">
rialto
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
19
</td>
</tr>
<tr>
<td style="text-align:left;">
riverside
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
running springs
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
san beranrdino
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
san bernadino
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
san bernardino
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
78
</td>
</tr>
<tr>
<td style="text-align:left;">
san bernardino
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
67
</td>
</tr>
<tr>
<td style="text-align:left;">
san bernrdino
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
sugar loaf
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
sugarloaf
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
sugarloaf
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
trona
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
twenty nine palms
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
twenty-nine palms
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
twentynine palms
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
25
</td>
</tr>
<tr>
<td style="text-align:left;">
twentynine palms
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
22
</td>
</tr>
<tr>
<td style="text-align:left;">
unk
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
upland
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
upland
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
victirville
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
victorville
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
29
</td>
</tr>
<tr>
<td style="text-align:left;">
victorville
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
37
</td>
</tr>
<tr>
<td style="text-align:left;">
wonder valley
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
yermo
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
yucaipa
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
18
</td>
</tr>
<tr>
<td style="text-align:left;">
yucaipa
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
16
</td>
</tr>
<tr>
<td style="text-align:left;">
yucaipa ca 92399
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
yucca
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
yucca valley
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
61
</td>
</tr>
<tr>
<td style="text-align:left;">
yucca valley
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
24
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
14
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
9
</td>
</tr>
</tbody>
</table>

</div>

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

<div class="kable-table">

<table>
<thead>
<tr>
<th style="text-align:left;">
gender_identity
</th>
<th style="text-align:right;">
n
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
586
</td>
</tr>
<tr>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
401
</td>
</tr>
<tr>
<td style="text-align:left;">
transgender
</td>
<td style="text-align:right;">
1
</td>
</tr>
</tbody>
</table>

</div>

``` r
shifted_data %>% 
  count(race_1)
```

<div class="kable-table">

<table>
<thead>
<tr>
<th style="text-align:left;">
race_1
</th>
<th style="text-align:right;">
n
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
american indian/alaskan native
</td>
<td style="text-align:right;">
12
</td>
</tr>
<tr>
<td style="text-align:left;">
asian
</td>
<td style="text-align:right;">
16
</td>
</tr>
<tr>
<td style="text-align:left;">
black/african american
</td>
<td style="text-align:right;">
193
</td>
</tr>
<tr>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
other
</td>
<td style="text-align:right;">
21
</td>
</tr>
<tr>
<td style="text-align:left;">
pacific islander/native hawaiian
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
unknown/not provided
</td>
<td style="text-align:right;">
143
</td>
</tr>
<tr>
<td style="text-align:left;">
white
</td>
<td style="text-align:right;">
593
</td>
</tr>
</tbody>
</table>

</div>

``` r
shifted_data %>% 
  count(ethnicity)
```

<div class="kable-table">

<table>
<thead>
<tr>
<th style="text-align:left;">
ethnicity
</th>
<th style="text-align:right;">
n
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:right;">
988
</td>
</tr>
</tbody>
</table>

</div>

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

    ## [1] 14690    51

# Re-coding variables
