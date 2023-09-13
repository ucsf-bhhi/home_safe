Cleaning Demographic Data
================
Sara Colom
2023-09-06

# Read in data & libraries

``` r
source("R/helpers.R")
```

``` r
demo_dat %>% 
  glimpse()
```

    ## Rows: 13,087
    ## Columns: 35
    ## $ id                                                            <chr> "113443"…
    ## $ reporting_agency                                              <chr> "Alameda…
    ## $ age                                                           <dbl> 54.76, 7…
    ## $ case_start_date                                               <chr> "2022-10…
    ## $ location_of_participation                                     <chr> "Castro …
    ## $ gender_identity                                               <chr> "Female"…
    ## $ race_1                                                        <chr> "Client …
    ## $ race_2                                                        <chr> "Data No…
    ## $ ethnicity                                                     <chr> "Hispani…
    ## $ current_marital_status                                        <chr> "Widowed…
    ## $ sexual_orientation                                            <chr> "Straigh…
    ## $ preferred_language                                            <chr> "Spanish…
    ## $ veteran_status                                                <chr> "Data No…
    ## $ medi_cal                                                      <chr> "Data No…
    ## $ medicare                                                      <chr> "Data No…
    ## $ representative_payee_or_conservator                           <chr> "Data No…
    ## $ living_situation_upon_entry                                   <chr> "Other P…
    ## $ monthly_rent_mortgage_contribution                            <chr> NA, "130…
    ## $ client_homeless_within_the_last_three_years                   <chr> "No", "N…
    ## $ number_of_times_homelessness_occurred_in_the_last_three_years <chr> "Client …
    ## $ total_duration_of_homelessness                                <chr> "Data No…
    ## $ last_period_of_homelessness                                   <chr> "Client …
    ## $ current_eviction_or_foreclosures                              <chr> "Client …
    ## $ previous_evictions_or_foreclosures                            <chr> "No", "N…
    ## $ discharge_from_institution_in_the_last_six_months             <chr> "No", "N…
    ## $ aps_report_date                                               <chr> "2022-09…
    ## $ aps_report_location                                           <chr> "Castro …
    ## $ abuse_by_other_financial                                      <chr> "No", "Y…
    ## $ abuse_by_other_non_financial                                  <chr> "No", "N…
    ## $ self_neglect                                                  <chr> "Yes", "…
    ## $ reporting_source                                              <chr> "Unknown…
    ## $ previous_aps_involvement                                      <chr> "No", "N…
    ## $ income_from_benefits                                          <dbl> NA, 3000…
    ## $ work_for_pay                                                  <chr> NA, "0",…
    ## $ other_income                                                  <chr> NA, "0",…

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
doesn’t know etc). Changing responses “ca” to “unknown”. Flagging , “2
houses from corner tx”, “north side of parking lot”, “home” and
“homeless”, “outside california”, not sure what to do with these.

### Questions:

ca - perhaps best to change this to unknown? “cal city”, “california
city” - do we think these are the same? (Made the change in the recode
for now) responses i.e. “coulterville/la grange” (so combo of two
cities) - leave as is? “north side of parking lot” - ? “sin city” -
might be sun city? (there is no sin city in california it seems) “ylp” -
? what to do with zipcodes? (shall I go look up the cities for those
codes?) home, homeless - are in the data still outside california - ?
exclude?

``` r
demo_dat <- demo_dat %>% 
  mutate(location_of_participation_recode = case_when(str_detect(location_of_participation, "outside california") ~ "flag",
                                                      str_ends(location_of_participation, " ca") ~ str_replace_all(location_of_participation, " ca|, ca", ""),
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
                                                      str_detect(location_of_participation, "eurek") ~ "eureka",
                                                      str_detect(location_of_participation, "frasier") ~ "frazier park",
                                                      str_detect(location_of_participation, "idlewild") ~ "idyllwild",
                                                      str_detect(location_of_participation, "jurupa") ~"jurupa valley",
                                                      str_detect(location_of_participation, "mills hospital in b") ~ "burlingame",
                                                      str_detect(location_of_participation, "monti r") ~ "monte rio",
                                                      str_detect(location_of_participation, "moren") ~ "moreno valley",
                                                      str_detect(location_of_participation, "mountianview|mountainview|moutain v") ~"mountain view",
                                                      str_detect(location_of_participation, "murreta") ~ "murrieta",
                                                      str_detect(location_of_participation, "pachecc") ~ "pacheco",
                                                      str_detect(location_of_participation, "north palm sp") ~ "north palm springs",
                                                      str_detect(location_of_participation, "palms sp|palm spr|palm dpr") ~ "palm springs",
                                                      str_detect(location_of_participation, "palo aal") ~ "palo alto",
                                                      str_detect(location_of_participation, "ranch mira") ~ "rancho mirage",
                                                      str_detect(location_of_participation, "rancho cucam") ~ "rancho cucamonga",
                                                      str_detect(location_of_participation, "richomo") ~ "richmond",
                                                      str_detect(location_of_participation, "ridegc") ~ "ridgecrest",
                                                      str_detect(location_of_participation, "riverisd|riveside") ~ "riverside",
                                                      str_detect(location_of_participation, "rough &|rough and re") ~ "rough and ready",
                                                      str_detect(location_of_participation, "sacrment|sacarament|sarcamen") ~ "sacramento",
                                                      str_detect(location_of_participation, "san berna|san bera|san bernr") ~ "san bernardino",
                                                      str_detect(location_of_participation, "san dieg") ~ "san diego",
                                                      str_detect(location_of_participation, "south san fran") ~ "south san francisco",
                                                      str_detect(location_of_participation, "san fra|sf") ~ "san francisco",
                                                      str_detect(location_of_participation, "slo") ~ "san luis obispo",
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
                                                      str_equal(location_of_participation, "ca") ~ "unknown",
                                                      str_equal(location_of_participation, "0") ~ "unknown",
                                                      str_equal(location_of_participation, "home") ~ "flag",
                                                      str_detect(location_of_participation, "homeless|north side of parking|2 houses") ~ "flag",
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
unknown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
2 houses from corner tx
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
29 palms
</td>
<td style="text-align:left;">
twentynine palms
</td>
<td style="text-align:right;">
12
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
92509
</td>
<td style="text-align:left;">
92509
</td>
<td style="text-align:right;">
2
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
adelanto
</td>
<td style="text-align:left;">
adelanto
</td>
<td style="text-align:right;">
10
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
9
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
7
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
2
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
12
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
2
</td>
</tr>
<tr>
<td style="text-align:left;">
alturas, ca
</td>
<td style="text-align:left;">
alturas
</td>
<td style="text-align:right;">
1
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
5
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
57
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
10
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
6
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
43
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
26
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
6
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
34
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
1
</td>
</tr>
<tr>
<td style="text-align:left;">
armona
</td>
<td style="text-align:left;">
armona
</td>
<td style="text-align:right;">
1
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
4
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
1
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
13
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
3
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
47
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
4
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
667
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
5
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
5
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
10
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
3
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
71
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
64
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
bastow
</td>
<td style="text-align:left;">
bastow
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
5
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
1
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
39
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
6
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
7
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
9
</td>
</tr>
<tr>
<td style="text-align:left;">
belmont
</td>
<td style="text-align:left;">
belmont
</td>
<td style="text-align:right;">
1
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
24
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
6
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
1
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
6
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
18
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
2
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
12
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
2
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
9
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
12
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
2
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
2
</td>
</tr>
<tr>
<td style="text-align:left;">
ca
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
ca 91737
</td>
<td style="text-align:left;">
ca 91737
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
8
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
1
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
7
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
13
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
16
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
camel valley
</td>
<td style="text-align:left;">
camel valley
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
3
</td>
</tr>
<tr>
<td style="text-align:left;">
cantua creek
</td>
<td style="text-align:left;">
cantua creek
</td>
<td style="text-align:right;">
1
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
4
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
5
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
11
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
9
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
23
</td>
</tr>
<tr>
<td style="text-align:left;">
caruthers
</td>
<td style="text-align:left;">
caruthers
</td>
<td style="text-align:right;">
1
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
3
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
1
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
7
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
80
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
cedar pines park
</td>
<td style="text-align:left;">
cedar pines park
</td>
<td style="text-align:right;">
1
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
5
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
1
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
8
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
11
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
71
</td>
</tr>
<tr>
<td style="text-align:left;">
chico ca
</td>
<td style="text-align:left;">
chico
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
chino
</td>
<td style="text-align:right;">
37
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
8
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
27
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
32
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
4
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
4
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
1
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
4
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
12
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
26
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
9
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
15
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
20
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
10
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
61
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
36
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
31
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
141
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
coronado
</td>
<td style="text-align:left;">
coronado
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
26
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
2
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
3
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
8
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
6
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
5
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
9
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
3
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
7
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
2
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
133
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
15
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
13
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
1
</td>
</tr>
<tr>
<td style="text-align:left;">
dublin
</td>
<td style="text-align:left;">
dublin
</td>
<td style="text-align:right;">
2
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
3
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
8
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
45
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
7
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
9
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
2
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
6
</td>
</tr>
<tr>
<td style="text-align:left;">
elk creek
</td>
<td style="text-align:left;">
elk creek
</td>
<td style="text-align:right;">
1
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
26
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
7
</td>
</tr>
<tr>
<td style="text-align:left;">
empire
</td>
<td style="text-align:left;">
empire
</td>
<td style="text-align:right;">
1
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
6
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
1
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
38
</td>
</tr>
<tr>
<td style="text-align:left;">
eurek
</td>
<td style="text-align:left;">
eureka
</td>
<td style="text-align:right;">
1
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
106
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
2
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
3
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
2
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
7
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
firebaugh
</td>
<td style="text-align:left;">
firebaugh
</td>
<td style="text-align:right;">
2
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
11
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
73
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
6
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
37
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
16
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
4
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
5
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
9
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
18
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
121
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
16
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
7
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
30
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
1
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
7
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
1
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
goleta-isla vista
</td>
<td style="text-align:left;">
goleta-isla vista
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
7
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
2
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
121
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
5
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
3
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
4
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
2
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
122
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
15
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
14
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
41
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
3
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
493
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
4
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
29
</td>
</tr>
<tr>
<td style="text-align:left;">
hidden valley lake
</td>
<td style="text-align:left;">
hidden valley lake
</td>
<td style="text-align:right;">
1
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
26
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
4
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
flag
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
13
</td>
</tr>
<tr>
<td style="text-align:left;">
homeless
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
22
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
9
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
2
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
164
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
37
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
12
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
12
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
3
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
johnson valley
</td>
<td style="text-align:left;">
johnson valley
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
32
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
53
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
kentfield
</td>
<td style="text-align:left;">
kentfield
</td>
<td style="text-align:right;">
2
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
1
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
2
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
2
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
1
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
14
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
1
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
5
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
11
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
1
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
4
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
123
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
16
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
4
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
6
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
3
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
41
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
6
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
8
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
1
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
37
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
1
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
16
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
2
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
1
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
8
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
2
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
11
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
4
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
127
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
2
</td>
</tr>
<tr>
<td style="text-align:left;">
los alamitos
</td>
<td style="text-align:left;">
los alamitos
</td>
<td style="text-align:right;">
1
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
456
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
1
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
24
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
4
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
4
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
6
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
10
</td>
</tr>
<tr>
<td style="text-align:left;">
lytle creek
</td>
<td style="text-align:left;">
lytle creek
</td>
<td style="text-align:right;">
1
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
49
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
5
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
3
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
3
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
56
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
3
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
29
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
5
</td>
</tr>
<tr>
<td style="text-align:left;">
mcclellan
</td>
<td style="text-align:left;">
mcclellan
</td>
<td style="text-align:right;">
1
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
2
</td>
</tr>
<tr>
<td style="text-align:left;">
mendota
</td>
<td style="text-align:left;">
mendota
</td>
<td style="text-align:right;">
2
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
65
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
3
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
110
</td>
</tr>
<tr>
<td style="text-align:left;">
mi wuk village
</td>
<td style="text-align:left;">
mi wuk village
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
middletown
</td>
<td style="text-align:left;">
middletown
</td>
<td style="text-align:right;">
1
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
5
</td>
</tr>
<tr>
<td style="text-align:left;">
midway city
</td>
<td style="text-align:left;">
midway city
</td>
<td style="text-align:right;">
1
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
3
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
6
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
7
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
mission viejo
</td>
<td style="text-align:left;">
mission viejo
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
62
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
10
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
6
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
10
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
9
</td>
</tr>
<tr>
<td style="text-align:left;">
monti rio
</td>
<td style="text-align:left;">
monte rio
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
moreno valley
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
206
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
11
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
9
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
mountain view
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
moutain view
</td>
<td style="text-align:left;">
mountain view
</td>
<td style="text-align:right;">
1
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
70
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
26
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
11
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
37
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
35
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
2
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
7
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
4
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
2
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
8
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
14
</td>
</tr>
<tr>
<td style="text-align:left;">
north palm springs
</td>
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
<td style="text-align:left;">
north san juan
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
north side of parking lot
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
northridge
</td>
<td style="text-align:left;">
northridge
</td>
<td style="text-align:right;">
5
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
15
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
10
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
3
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
3
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
148
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
12
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
23
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
15
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
83
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
2
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
1
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
16
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
37
</td>
</tr>
<tr>
<td style="text-align:left;">
outside california
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
oxnard
</td>
<td style="text-align:left;">
oxnard
</td>
<td style="text-align:right;">
32
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
8
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
4
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
palermo
</td>
<td style="text-align:left;">
palermo
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
74
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
368
</td>
</tr>
<tr>
<td style="text-align:left;">
palm springs,
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
palmdale
</td>
<td style="text-align:left;">
palmdale
</td>
<td style="text-align:right;">
23
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
palo aalto
</td>
<td style="text-align:left;">
palo alto
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
5
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
5
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
11
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
5
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
paynes creek
</td>
<td style="text-align:left;">
paynes creek
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
1
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
9
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
104
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
30
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
3
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
pine mountain club
</td>
<td style="text-align:left;">
pine mountain club
</td>
<td style="text-align:right;">
4
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
9
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
5
</td>
</tr>
<tr>
<td style="text-align:left;">
piru
</td>
<td style="text-align:left;">
piru
</td>
<td style="text-align:right;">
1
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
31
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
placerville
</td>
<td style="text-align:left;">
placerville
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
2
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
2
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
4
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
12
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
8
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
pollock pines
</td>
<td style="text-align:left;">
pollock pines
</td>
<td style="text-align:right;">
1
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
26
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
7
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
3
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
2
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
7
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
62
</td>
</tr>
<tr>
<td style="text-align:left;">
rancho cucamongo
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
rancho mirage
</td>
<td style="text-align:left;">
rancho mirage
</td>
<td style="text-align:right;">
38
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
3
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
75
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
45
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
34
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
20
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
6
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
5
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
71
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
49
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
7
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
3
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
5
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
16
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
40
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
4
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
3
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
68
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
2
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
6
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
260
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
5
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
188
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
192
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
4
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
3
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
278
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
2
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
97
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
125
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
3
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
21
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
3
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
17
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
5
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
34
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
13
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
8
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
sanger
</td>
<td style="text-align:left;">
sanger
</td>
<td style="text-align:right;">
1
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
36
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
25
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
2
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
6
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
1
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
9
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
133
</td>
</tr>
<tr>
<td style="text-align:left;">
santa rosa ca
</td>
<td style="text-align:left;">
santa rosa
</td>
<td style="text-align:right;">
1
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
6
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
15
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
2
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
13
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
3
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
4
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
6
</td>
</tr>
<tr>
<td style="text-align:left;">
shingletown
</td>
<td style="text-align:left;">
shingletown
</td>
<td style="text-align:right;">
1
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
1
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
slo
</td>
<td style="text-align:left;">
san luis obispo
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
1
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
21
</td>
</tr>
<tr>
<td style="text-align:left;">
sonora
</td>
<td style="text-align:left;">
sonora
</td>
<td style="text-align:right;">
10
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
15
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
2
</td>
</tr>
<tr>
<td style="text-align:left;">
south san francisco
</td>
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
<td style="text-align:left;">
spring valley
</td>
<td style="text-align:right;">
18
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
24
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
8
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
1
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
23
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
6
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
2
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
30
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
11
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
13
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
4
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
24
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
114
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
1
</td>
</tr>
<tr>
<td style="text-align:left;">
templeton
</td>
<td style="text-align:left;">
templeton
</td>
<td style="text-align:right;">
1
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
12
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
2
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
54
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
9
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
4
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
5
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
4
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
17
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
7
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
4
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
63
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
38
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
10
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
4
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
25
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
7
</td>
</tr>
<tr>
<td style="text-align:left;">
vacaville ca
</td>
<td style="text-align:left;">
vacaville
</td>
<td style="text-align:right;">
1
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
2
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
4
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
2
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
20
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
5
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
70
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
85
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
14
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
14
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
1
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
8
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
washington
</td>
<td style="text-align:left;">
washington
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
58
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
3
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
8
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
13
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
4
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
2
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
18
</td>
</tr>
<tr>
<td style="text-align:left;">
westlake village
</td>
<td style="text-align:left;">
westlake village
</td>
<td style="text-align:right;">
1
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
3
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
17
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
36
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
23
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
3
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
19
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
3
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
7
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
1
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
7
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
5
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
2
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
woodcrest
</td>
<td style="text-align:left;">
woodcrest
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
5
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
2
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
2
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
20
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
8
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
48
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
120
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
595
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
alturas, ca
</td>
<td style="text-align:left;">
alturas
</td>
<td style="text-align:right;">
1
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
5
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
5
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
7
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
chico ca
</td>
<td style="text-align:left;">
chico
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
45
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
1
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
los angeles california
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
outside california
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
palm springs,
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
3
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
7
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
2
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
3
</td>
</tr>
<tr>
<td style="text-align:left;">
santa rosa ca
</td>
<td style="text-align:left;">
santa rosa
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
vacaville ca
</td>
<td style="text-align:left;">
vacaville
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
                                   str_detect(race_1, "unkn|#|client|collected|1|2|3|4|5|6|7|8|9|0") ~ "unknown",
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
0
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
american indian/alaskan native
</td>
<td style="text-align:left;">
american indian/alaskan native/indigenous
</td>
<td style="text-align:right;">
33
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
110
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
82
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
217
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
531
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
1232
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
55
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
713
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
43
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
1112
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
36
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
1127
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
7700
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
51
</td>
</tr>
</tbody>
</table>

</div>

race_2 - doing the same as for race_1, and replacing no applicable with
not applicable, updating “unknown”. For those who put an ethnicity
response in race_2, replacing with “unknown” and updating those in the
ethnicity variable. There was someone with race information in
ethnicity, moving that to race_2\_recode.

``` r
demo_dat <- demo_dat %>% 
  mutate(race_2_recode = case_when(str_starts(race_2,"americ") ~ "american indian/alaskan native/indigenous", 
                                      str_starts(race_2, "asian") ~ "asian/asian american",
                                      str_starts(race_2, "bla") ~ "black/african american/african",
                                      str_starts(race_2, "pacific i") ~ "native hawaiian/pacific islander",
                                      str_starts(race_2, "wh") ~ "white",
                                   str_detect(race_2, "hisp") ~ "unknown",
                                   str_detect(ethnicity, "american indian") ~ "american indian/alaskan native/indigenous",
                                   str_detect(race_2, "unknown|unable|refused|not collected|applicable|verified|#|client|mex|1|2|3|4|5|6|7|8|9|0") ~ "unknown",
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
2
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
american indian/alaskan native
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
american indian/alaskan native/indigenous
</td>
<td style="text-align:left;">
american indian/alaskan native/indigenous
</td>
<td style="text-align:right;">
4
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
2
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
6
</td>
</tr>
<tr>
<td style="text-align:left;">
black/african american/african
</td>
<td style="text-align:left;">
black/african american/african
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
18
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
10390
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
9
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
382
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
545
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
202
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
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
NA
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1518
</td>
</tr>
</tbody>
</table>

</div>

Check that the race_2 variable was fixed for the one with response in
ethnicity var

``` r
demo_dat %>% 
  filter(str_starts(ethnicity, "american")) %>% 
  count(ethnicity, race_1, race_2, race_2_recode)
```

<div class="kable-table">

<table>
<thead>
<tr>
<th style="text-align:left;">
ethnicity
</th>
<th style="text-align:left;">
race_1
</th>
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
american indian/alaskan native
</td>
<td style="text-align:left;">
white
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
american indian/alaskan native/indigenous
</td>
<td style="text-align:right;">
1
</td>
</tr>
</tbody>
</table>

</div>

Ethnicity - replace all instances of different spellings or orders of
hispanic/latino/a/x to hispanic/latinx. Changing mexican/chicano, cuban,
other hispanic/latino, and puerto rican to hispanic/latinx. Also,
replace all instances of different spellings or orders of not
hispanic/latino to non-hispanic/latinx. Moving “american indian/alaskan
native” to race_2 from ethnicity.

``` r
demo_dat <- demo_dat %>% 
  mutate(ethnicity_recode = case_when(str_starts(ethnicity, "no") ~ "non-hispanic/latinx",
                                      str_detect(ethnicity,"hispa") ~ "hispanic/latinx", 
                                      str_detect(race_2, "not hispanic|no hispanic") ~ "non-hispanic/latinx",
                                      str_detect(race_2, "hispanic") ~ "hispanic/latinx",
                                      str_detect(race_2, "mex") ~ "hispanic/latinx",
                                      str_detect(ethnicity, "mex|puerto|cuban") ~ "hispanic/latinx",
                                      str_detect(ethnicity, "american ind") ~ "unknown",
                                      str_detect(ethnicity, "unknown|#|applicable|data not|client|missing|rent|1|2|3|4|5|6|7|8|9|0") ~ "unknown",
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
2
</td>
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
6
</td>
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
american indian/alaskan native
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
client doesn’t know
</td>
<td style="text-align:left;">
black/african american/african
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
client doesn’t know
</td>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
894
</td>
</tr>
<tr>
<td style="text-align:left;">
client doesn’t know
</td>
<td style="text-align:left;">
white
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
NA
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
client refused
</td>
<td style="text-align:left;">
data not collected
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
data not collected
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1190
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
2
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
70
</td>
</tr>
<tr>
<td style="text-align:left;">
hispanic/latin(a)(o)(x)
</td>
<td style="text-align:left;">
american indian/alaskan native/indigenous
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
hispanic/latin(a)(o)(x)
</td>
<td style="text-align:left;">
client doesn’t know
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
hispanic/latin(a)(o)(x)
</td>
<td style="text-align:left;">
data not collected
</td>
<td style="text-align:left;">
hispanic/latinx
</td>
<td style="text-align:right;">
1408
</td>
</tr>
<tr>
<td style="text-align:left;">
hispanic/latin(a)(o)(x)
</td>
<td style="text-align:left;">
white
</td>
<td style="text-align:left;">
hispanic/latinx
</td>
<td style="text-align:right;">
7
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
89
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
client refused
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
4
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
client doesn’t know
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
74
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
13
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
9
</td>
</tr>
<tr>
<td style="text-align:left;">
mexican/mexican american/chicano
</td>
<td style="text-align:left;">
not applicable
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
non-hispanic/latin(a)(o)(x)
</td>
<td style="text-align:left;">
american indian/alaskan native/indigenous
</td>
<td style="text-align:left;">
non-hispanic/latinx
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
non-hispanic/latin(a)(o)(x)
</td>
<td style="text-align:left;">
black/african american
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
black/african american/african
</td>
<td style="text-align:left;">
non-hispanic/latinx
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
non-hispanic/latin(a)(o)(x)
</td>
<td style="text-align:left;">
client refused
</td>
<td style="text-align:left;">
non-hispanic/latinx
</td>
<td style="text-align:right;">
11
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
4700
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
99
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
434
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
2
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
5
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
1388
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
5
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
217
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
109
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
75
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
264
</td>
</tr>
<tr>
<td style="text-align:left;">
other hispanic/latin
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
334
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
3
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
unknown/not provided
</td>
<td style="text-align:left;">
hispanic/latinx
</td>
<td style="text-align:right;">
11
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
13
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
126
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
4
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
data not collected
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
372
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
40
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
368
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
2
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
465
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
11
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
1
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
7
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
23
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
50
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
  mutate(current_marital_recode = case_when(str_detect(current_marital_status, "unknown|#|applicable|data not|1|2|3|4|5|6|7|8|9|0") ~ "unknown",
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
5
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
6
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
2223
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
1894
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
4
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
1280
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
1729
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
1494
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
451
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
87
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
814
</td>
</tr>
<tr>
<td style="text-align:left;">
unknown/no provided
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
unknown/not provided
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1286
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
1521
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
205
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
0
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:right;">
1
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
33
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
1320
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
178
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
1957
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
15
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
169
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
37
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
25
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
6844
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
306
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
2137
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
62
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
data not collected
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
141
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
11855
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
12
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
14
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
198
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
764
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
16
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
8
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
15
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
63
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
0
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:right;">
2
</td>
</tr>
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
331
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
4619
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
6389
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
944
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
606
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
193
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
1
</td>
<td style="text-align:left;">
1
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
unknown
</td>
<td style="text-align:right;">
5
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
6011
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
1019
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
1221
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
3840
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
988
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
1
</td>
<td style="text-align:left;">
1
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
6603
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
986
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
1258
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
3223
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
1009
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
16
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
28
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
30
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
33.200000000000003
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
34
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
35
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
36.700000000000003
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
37.799999999999997
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
38
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
38.299999999999997
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
39.799999999999997
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
40
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
40.799999999999997
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
41
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
50
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
51
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
52
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
53
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
54
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
55
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
56
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
58
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
60
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
61
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
62
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
63
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
64
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
64.099999999999994
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
64.400000000000006
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
64.599999999999994
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
64.900000000000006
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
65
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
65.099999999999994
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
65.400000000000006
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
65.599999999999994
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
65.900000000000006
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
66.099999999999994
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
66.400000000000006
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
66.599999999999994
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
66.900000000000006
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
67
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
67.099999999999994
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
67.599999999999994
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
67.900000000000006
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
68
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
68.099999999999994
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
68.400000000000006
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
68.599999999999994
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
69.099999999999994
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
69.599999999999994
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
69.900000000000006
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
70.099999999999994
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
70.599999999999994
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
70.900000000000006
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
71
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
71.099999999999994
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
71.400000000000006
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
71.599999999999994
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
72.599999999999994
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
73.099999999999994
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
73.400000000000006
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
73.900000000000006
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
74.099999999999994
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
74.400000000000006
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
74.599999999999994
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
75
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
75.400000000000006
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
75.599999999999994
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
75.900000000000006
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
76.400000000000006
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
76.599999999999994
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
77.599999999999994
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
78
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
78.099999999999994
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
78.599999999999994
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
78.900000000000006
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
79
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
79.599999999999994
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
80
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
80.400000000000006
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
81.599999999999994
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
82
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
client refused
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
7316
</td>
</tr>
<tr>
<td style="text-align:left;">
n/a
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
no
</td>
<td style="text-align:right;">
3277
</td>
</tr>
<tr>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
835
</td>
</tr>
<tr>
<td style="text-align:left;">
yes
</td>
<td style="text-align:right;">
207
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
1010
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
7
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
2
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
14
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
3510
</td>
</tr>
<tr>
<td style="text-align:left;">
fleeing dv
</td>
<td style="text-align:left;">
fleeing dv
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
1
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
2246
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
355
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
711
</td>
</tr>
<tr>
<td style="text-align:left;">
homeless- sheltered
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
homeowner- alone
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
hotel
</td>
<td style="text-align:left;">
temporary housing
</td>
<td style="text-align:right;">
1
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
217
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
living in a shared house
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
living with others rent
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
1
</td>
</tr>
<tr>
<td style="text-align:left;">
not answered
</td>
<td style="text-align:left;">
not answered
</td>
<td style="text-align:right;">
5
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
148
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
151
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
543
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
259
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
105
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
70
</td>
</tr>
<tr>
<td style="text-align:left;">
permanent housing
</td>
<td style="text-align:left;">
permanent housing
</td>
<td style="text-align:right;">
1
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
25
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
4
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
2439
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
2
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
rental housing lease holder
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
rental housing- lease holder
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
46
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
1
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
18
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
1045
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
79
</td>
</tr>
<tr>
<td style="text-align:left;">
temporary-residential program
</td>
<td style="text-align:left;">
temporary-residential program
</td>
<td style="text-align:right;">
1
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
279
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
629
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
113
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
los angeles
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
placer
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
sacramento
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
san bernardino
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
living in a shared house
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
living with others rent
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
4
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
2
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
placer
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
7
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
shared housing
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
2
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
tehama
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
ventura
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
contra costa
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
fresno
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
humboldt
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
kern
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
kings
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
los angeles
</td>
<td style="text-align:right;">
62
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
4
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
1
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
5
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
9
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
4
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
33
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
78
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
22
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
4
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
10
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
13
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
19
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
1
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
11
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
15
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
9
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
42
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
1
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
8
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
3
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
66
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
253
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
62
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
6
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
22
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
43
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
4
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
6
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
58
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
4925
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
no
</td>
<td style="text-align:left;">
no
</td>
<td style="text-align:right;">
4277
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
752
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
2913
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
151
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
1023
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
635
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
11
</td>
</tr>
<tr>
<td style="text-align:left;">
100
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
15
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
2
</td>
<td style="text-align:left;">
two times
</td>
<td style="text-align:right;">
121
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
4
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
44
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
31
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
7
</td>
</tr>
<tr>
<td style="text-align:left;">
730
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
1063
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
39
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
55
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
2129
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
4946
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
306
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
one time
</td>
<td style="text-align:left;">
one time
</td>
<td style="text-align:right;">
900
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
35
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
81
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
NA
</td>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
1537
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
73
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
1
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
a few years
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
client doesn’t know
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
client not homeless
</td>
<td style="text-align:left;">
client was not homeless
</td>
<td style="text-align:right;">
2
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
7
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
7
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
3189
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
1
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
6370
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
21
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
14
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
17
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
more than 12 months
</td>
<td style="text-align:left;">
more than 12 months
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
more than a a year
</td>
<td style="text-align:left;">
more than a a year
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
730
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
nine months
</td>
<td style="text-align:left;">
nine months
</td>
<td style="text-align:right;">
7
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
not homeless
</td>
<td style="text-align:left;">
client was not homeless
</td>
<td style="text-align:right;">
2
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
624
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
44
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
46
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
156
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
54
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
19
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
7
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
49
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
155
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
223
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
7
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
1213
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
4
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
72
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
31
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
3603
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
6677
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
12
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
688
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
1852
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
144
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
2
</td>
</tr>
<tr>
<td style="text-align:left;">
1
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
7
</td>
</tr>
<tr>
<td style="text-align:left;">
client doesn’t know
</td>
<td style="text-align:right;">
37
</td>
</tr>
<tr>
<td style="text-align:left;">
client refused
</td>
<td style="text-align:right;">
26
</td>
</tr>
<tr>
<td style="text-align:left;">
client was not homeless
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
3727
</td>
</tr>
<tr>
<td style="text-align:left;">
n/a
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
no
</td>
<td style="text-align:right;">
6041
</td>
</tr>
<tr>
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
<td style="text-align:right;">
1853
</td>
</tr>
<tr>
<td style="text-align:left;">
yes
</td>
<td style="text-align:right;">
1291
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
91
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
0
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:right;">
2
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
11
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
28
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
4327
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
6415
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
1352
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
791
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
148
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
0
</td>
<td style="text-align:left;">
0
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
152
</td>
</tr>
<tr>
<td style="text-align:left;">
aps
</td>
<td style="text-align:left;">
aps
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
bhs
</td>
<td style="text-align:left;">
bhs
</td>
<td style="text-align:right;">
1
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
14
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
950
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
3
</td>
</tr>
<tr>
<td style="text-align:left;">
eligibility worker
</td>
<td style="text-align:left;">
eligibility worker
</td>
<td style="text-align:right;">
1
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
354
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
20
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
6
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
130
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
33
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
435
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
9
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
648
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
336
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
32
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
884
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
156
</td>
</tr>
<tr>
<td style="text-align:left;">
other public agency
</td>
<td style="text-align:left;">
other public agency
</td>
<td style="text-align:right;">
3
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
710
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
2070
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
2078
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
3842
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
185
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
3
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
1
</td>
</tr>
<tr>
<td style="text-align:left;">
136 kaseberg lane roseville
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
12
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
92509
</td>
<td style="text-align:right;">
2
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
10
</td>
</tr>
<tr>
<td style="text-align:left;">
alameda
</td>
<td style="text-align:right;">
9
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
6
</td>
</tr>
<tr>
<td style="text-align:left;">
alpine
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
alta loma
</td>
<td style="text-align:right;">
12
</td>
</tr>
<tr>
<td style="text-align:left;">
alturas
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
americannyon
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
anaheim
</td>
<td style="text-align:right;">
54
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
46
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
26
</td>
</tr>
<tr>
<td style="text-align:left;">
applegate
</td>
<td style="text-align:right;">
2
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
5
</td>
</tr>
<tr>
<td style="text-align:left;">
arcata
</td>
<td style="text-align:right;">
33
</td>
</tr>
<tr>
<td style="text-align:left;">
armona
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
arroyo grande
</td>
<td style="text-align:right;">
4
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
13
</td>
</tr>
<tr>
<td style="text-align:left;">
auberry
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
auburn
</td>
<td style="text-align:right;">
42
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
4
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
737
</td>
</tr>
<tr>
<td style="text-align:left;">
bakersfiled
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
baldwin park
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
ballico
</td>
<td style="text-align:right;">
3
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
74
</td>
</tr>
<tr>
<td style="text-align:left;">
barstow
</td>
<td style="text-align:right;">
64
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
bastow
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
5
</td>
</tr>
<tr>
<td style="text-align:left;">
beamount
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
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
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
bell gardens
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
bellflower
</td>
<td style="text-align:right;">
7
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
25
</td>
</tr>
<tr>
<td style="text-align:left;">
bethel island
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
beumont
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
beverly hills
</td>
<td style="text-align:right;">
6
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
18
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
2
</td>
</tr>
<tr>
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
2
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
9
</td>
</tr>
<tr>
<td style="text-align:left;">
burbank
</td>
<td style="text-align:right;">
6
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
2
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
ca 91737
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
8
</td>
</tr>
<tr>
<td style="text-align:left;">
cailmesa
</td>
<td style="text-align:right;">
1
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
1
</td>
</tr>
<tr>
<td style="text-align:left;">
california city
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
calimesa
</td>
<td style="text-align:right;">
13
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
camel valley
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
16
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
1
</td>
</tr>
<tr>
<td style="text-align:left;">
cantua creek
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
canyon country
</td>
<td style="text-align:right;">
2
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
11
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
9
</td>
</tr>
<tr>
<td style="text-align:left;">
carmichael
</td>
<td style="text-align:right;">
15
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
14
</td>
</tr>
<tr>
<td style="text-align:left;">
caruthers
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
castaic
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
castella
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
castro valley
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
cathedral city
</td>
<td style="text-align:right;">
78
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
cedar pines park
</td>
<td style="text-align:right;">
1
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
5
</td>
</tr>
<tr>
<td style="text-align:left;">
cerritos
</td>
<td style="text-align:right;">
1
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
6
</td>
</tr>
<tr>
<td style="text-align:left;">
cherry valley
</td>
<td style="text-align:right;">
11
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
70
</td>
</tr>
<tr>
<td style="text-align:left;">
chino
</td>
<td style="text-align:right;">
37
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
27
</td>
</tr>
<tr>
<td style="text-align:left;">
citrus height
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
citrus heights
</td>
<td style="text-align:right;">
31
</td>
</tr>
<tr>
<td style="text-align:left;">
city of commerce
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
city of industry
</td>
<td style="text-align:right;">
1
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
4
</td>
</tr>
<tr>
<td style="text-align:left;">
clearlake
</td>
<td style="text-align:right;">
1
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
2
</td>
</tr>
<tr>
<td style="text-align:left;">
cloverdale
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
clovis
</td>
<td style="text-align:right;">
13
</td>
</tr>
<tr>
<td style="text-align:left;">
coachella
</td>
<td style="text-align:right;">
28
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
8
</td>
</tr>
<tr>
<td style="text-align:left;">
colton
</td>
<td style="text-align:right;">
15
</td>
</tr>
<tr>
<td style="text-align:left;">
colusa
</td>
<td style="text-align:right;">
24
</td>
</tr>
<tr>
<td style="text-align:left;">
compton
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
concord
</td>
<td style="text-align:right;">
61
</td>
</tr>
<tr>
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
<td style="text-align:right;">
33
</td>
</tr>
<tr>
<td style="text-align:left;">
corona
</td>
<td style="text-align:right;">
146
</td>
</tr>
<tr>
<td style="text-align:left;">
coronado
</td>
<td style="text-align:right;">
1
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
27
</td>
</tr>
<tr>
<td style="text-align:left;">
cotati
</td>
<td style="text-align:right;">
2
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
4
</td>
</tr>
<tr>
<td style="text-align:left;">
crescent city
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
crestline
</td>
<td style="text-align:right;">
9
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
4
</td>
</tr>
<tr>
<td style="text-align:left;">
culver city
</td>
<td style="text-align:right;">
5
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
6
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
2
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
7
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
137
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
16
</td>
</tr>
<tr>
<td style="text-align:left;">
downey
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
duarte
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
dublin
</td>
<td style="text-align:right;">
2
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
2
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
8
</td>
</tr>
<tr>
<td style="text-align:left;">
el cerrito
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
el monte
</td>
<td style="text-align:right;">
7
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
2
</td>
</tr>
<tr>
<td style="text-align:left;">
el sobrante
</td>
<td style="text-align:right;">
6
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
42
</td>
</tr>
<tr>
<td style="text-align:left;">
elk creek
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
elk grove
</td>
<td style="text-align:right;">
29
</td>
</tr>
<tr>
<td style="text-align:left;">
emeryville
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
empire
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
encinitas
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
escondido
</td>
<td style="text-align:right;">
34
</td>
</tr>
<tr>
<td style="text-align:left;">
eurek
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
eureka
</td>
<td style="text-align:right;">
108
</td>
</tr>
<tr>
<td style="text-align:left;">
exeter
</td>
<td style="text-align:right;">
2
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
<td style="text-align:right;">
2
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
firebaugh
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
folsom
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
fontana
</td>
<td style="text-align:right;">
73
</td>
</tr>
<tr>
<td style="text-align:left;">
forest hill
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
foresthill
</td>
<td style="text-align:right;">
3
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
37
</td>
</tr>
<tr>
<td style="text-align:left;">
fortuna
</td>
<td style="text-align:right;">
16
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
4
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
19
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
127
</td>
</tr>
<tr>
<td style="text-align:left;">
fullerton
</td>
<td style="text-align:right;">
17
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
9
</td>
</tr>
<tr>
<td style="text-align:left;">
gardena
</td>
<td style="text-align:right;">
22
</td>
</tr>
<tr>
<td style="text-align:left;">
gazelle
</td>
<td style="text-align:right;">
2
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
5
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
3
</td>
</tr>
<tr>
<td style="text-align:left;">
glendora
</td>
<td style="text-align:right;">
1
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
goleta-isla vista
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
6
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
2
</td>
</tr>
<tr>
<td style="text-align:left;">
grass valley
</td>
<td style="text-align:right;">
143
</td>
</tr>
<tr>
<td style="text-align:left;">
gridley
</td>
<td style="text-align:right;">
5
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
4
</td>
</tr>
<tr>
<td style="text-align:left;">
guerneville
</td>
<td style="text-align:right;">
4
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
2
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
119
</td>
</tr>
<tr>
<td style="text-align:left;">
harbor city
</td>
<td style="text-align:right;">
12
</td>
</tr>
<tr>
<td style="text-align:left;">
hawkins bar
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
hawthorne
</td>
<td style="text-align:right;">
9
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
40
</td>
</tr>
<tr>
<td style="text-align:left;">
hayward 94541
</td>
<td style="text-align:right;">
1
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
499
</td>
</tr>
<tr>
<td style="text-align:left;">
hercules
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
hesperia
</td>
<td style="text-align:right;">
29
</td>
</tr>
<tr>
<td style="text-align:left;">
hidden valley lake
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
highland
</td>
<td style="text-align:right;">
26
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
6
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
9
</td>
</tr>
<tr>
<td style="text-align:left;">
homeland
</td>
<td style="text-align:right;">
13
</td>
</tr>
<tr>
<td style="text-align:left;">
homeless
</td>
<td style="text-align:right;">
11
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
23
</td>
</tr>
<tr>
<td style="text-align:left;">
huntington park
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
hyampom
</td>
<td style="text-align:right;">
2
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
170
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
34
</td>
</tr>
<tr>
<td style="text-align:left;">
inyokern
</td>
<td style="text-align:right;">
1
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
14
</td>
</tr>
<tr>
<td style="text-align:left;">
isleton
</td>
<td style="text-align:right;">
3
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
10
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
johnson valley
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
32
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
53
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
kentfield
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
kerman
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
kernville
</td>
<td style="text-align:right;">
6
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
2
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
1
</td>
</tr>
<tr>
<td style="text-align:left;">
la grange
</td>
<td style="text-align:right;">
1
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
15
</td>
</tr>
<tr>
<td style="text-align:left;">
la mirada
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
la puente
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
la quinta
</td>
<td style="text-align:right;">
12
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
5
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
123
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
18
</td>
</tr>
<tr>
<td style="text-align:left;">
lakeside
</td>
<td style="text-align:right;">
4
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
6
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
23
</td>
</tr>
<tr>
<td style="text-align:left;">
landers
</td>
<td style="text-align:right;">
6
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
5
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
2
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
36
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
11
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
1
</td>
</tr>
<tr>
<td style="text-align:left;">
lincoln- home- grave disability hold
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
littlerock
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
livermore
</td>
<td style="text-align:right;">
8
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
2
</td>
</tr>
<tr>
<td style="text-align:left;">
loma linda
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
lomita
</td>
<td style="text-align:right;">
3
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
81
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
2
</td>
</tr>
<tr>
<td style="text-align:left;">
los angeles
</td>
<td style="text-align:right;">
305
</td>
</tr>
<tr>
<td style="text-align:left;">
los banos
</td>
<td style="text-align:right;">
25
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
4
</td>
</tr>
<tr>
<td style="text-align:left;">
lost hills
</td>
<td style="text-align:right;">
1
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
6
</td>
</tr>
<tr>
<td style="text-align:left;">
lynwood
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
lytle creek
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
macdoel
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
madera
</td>
<td style="text-align:right;">
54
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
5
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
3
</td>
</tr>
<tr>
<td style="text-align:left;">
marina del rey
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
mariposa
</td>
<td style="text-align:right;">
59
</td>
</tr>
<tr>
<td style="text-align:left;">
martell
</td>
<td style="text-align:right;">
2
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
22
</td>
</tr>
<tr>
<td style="text-align:left;">
maywood
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
mcclellan
</td>
<td style="text-align:right;">
1
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
meadow vista
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
mecca
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
mendota
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
menifee
</td>
<td style="text-align:right;">
65
</td>
</tr>
<tr>
<td style="text-align:left;">
mentone
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
merced
</td>
<td style="text-align:right;">
111
</td>
</tr>
<tr>
<td style="text-align:left;">
middletown
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
midpines
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
midway city
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
mill valley
</td>
<td style="text-align:right;">
4
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
5
</td>
</tr>
<tr>
<td style="text-align:left;">
mira loma
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
missing
</td>
<td style="text-align:right;">
154
</td>
</tr>
<tr>
<td style="text-align:left;">
mission hills
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
mission viejo
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
modesto
</td>
<td style="text-align:right;">
63
</td>
</tr>
<tr>
<td style="text-align:left;">
mojave
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
monrovia
</td>
<td style="text-align:right;">
1
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
7
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
8
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
213
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
11
</td>
</tr>
<tr>
<td style="text-align:left;">
morro bay
</td>
<td style="text-align:right;">
9
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
2
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
moutain view
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
mt shasta
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
mt. shasta
</td>
<td style="text-align:right;">
1
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
77
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
26
</td>
</tr>
<tr>
<td style="text-align:left;">
national city
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
needles
</td>
<td style="text-align:right;">
37
</td>
</tr>
<tr>
<td style="text-align:left;">
nevada city
</td>
<td style="text-align:right;">
34
</td>
</tr>
<tr>
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
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
newbury park
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
newcastle
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
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
30
</td>
</tr>
<tr>
<td style="text-align:left;">
north hills
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
north hollywood
</td>
<td style="text-align:right;">
9
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
2
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
5
</td>
</tr>
<tr>
<td style="text-align:left;">
norwalk
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
novato
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
nuevo
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
oak view
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
oakdale
</td>
<td style="text-align:right;">
3
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
147
</td>
</tr>
<tr>
<td style="text-align:left;">
oakley
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:left;">
oaklland
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
oceanside
</td>
<td style="text-align:right;">
22
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
7
</td>
</tr>
<tr>
<td style="text-align:left;">
ontario
</td>
<td style="text-align:right;">
83
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
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
orinda
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
orland
</td>
<td style="text-align:right;">
14
</td>
</tr>
<tr>
<td style="text-align:left;">
oroville
</td>
<td style="text-align:right;">
37
</td>
</tr>
<tr>
<td style="text-align:left;">
oxnard
</td>
<td style="text-align:right;">
32
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
8
</td>
</tr>
<tr>
<td style="text-align:left;">
pacoima
</td>
<td style="text-align:right;">
4
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
palermo
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
74
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
368
</td>
</tr>
<tr>
<td style="text-align:left;">
palm springs,
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
palmdale
</td>
<td style="text-align:right;">
12
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
3
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
5
</td>
</tr>
<tr>
<td style="text-align:left;">
pasadena
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
paso robles
</td>
<td style="text-align:right;">
5
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
paynes creek
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
10
</td>
</tr>
<tr>
<td style="text-align:left;">
perris
</td>
<td style="text-align:right;">
109
</td>
</tr>
<tr>
<td style="text-align:left;">
petaluma
</td>
<td style="text-align:right;">
31
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
4
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
pine mountain club
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
pinole
</td>
<td style="text-align:right;">
9
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
piru
</td>
<td style="text-align:right;">
1
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
30
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
placerville
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
2
</td>
</tr>
<tr>
<td style="text-align:left;">
playa del rey
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
playa vista
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
pleasant hill
</td>
<td style="text-align:right;">
12
</td>
</tr>
<tr>
<td style="text-align:left;">
pleasanton
</td>
<td style="text-align:right;">
9
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
pollock pines
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
pomona
</td>
<td style="text-align:right;">
20
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
7
</td>
</tr>
<tr>
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
<td style="text-align:right;">
2
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
8
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
62
</td>
</tr>
<tr>
<td style="text-align:left;">
rancho cucamongo
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
rancho mirage
</td>
<td style="text-align:right;">
40
</td>
</tr>
<tr>
<td style="text-align:left;">
rancho palos verdes
</td>
<td style="text-align:right;">
3
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
72
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
34
</td>
</tr>
<tr>
<td style="text-align:left;">
redondo beach
</td>
<td style="text-align:right;">
8
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
3
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
1
</td>
</tr>
<tr>
<td style="text-align:left;">
renting a room at: 1437 stone hearth ,lincoln
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
reseda
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
rialto
</td>
<td style="text-align:right;">
71
</td>
</tr>
<tr>
<td style="text-align:left;">
richmond
</td>
<td style="text-align:right;">
50
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
17
</td>
</tr>
<tr>
<td style="text-align:left;">
rio dell
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
rio linda
</td>
<td style="text-align:right;">
7
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
3
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
5
</td>
</tr>
<tr>
<td style="text-align:left;">
riverside
</td>
<td style="text-align:right;">
810
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
12
</td>
</tr>
<tr>
<td style="text-align:left;">
rocklin 95677
</td>
<td style="text-align:right;">
2
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
43
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
rosevile snf oakridgere center
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
roseville
</td>
<td style="text-align:right;">
51
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
3
</td>
</tr>
<tr>
<td style="text-align:left;">
rough and ready
</td>
<td style="text-align:right;">
7
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
3
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
271
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
5
</td>
</tr>
<tr>
<td style="text-align:left;">
san bernardino
</td>
<td style="text-align:right;">
188
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
198
</td>
</tr>
<tr>
<td style="text-align:left;">
san dimas
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
san fernando
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
san francisco
</td>
<td style="text-align:right;">
290
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
104
</td>
</tr>
<tr>
<td style="text-align:left;">
san jose
</td>
<td style="text-align:right;">
117
</td>
</tr>
<tr>
<td style="text-align:left;">
san juanpistrano
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
san leandro
</td>
<td style="text-align:right;">
18
</td>
</tr>
<tr>
<td style="text-align:left;">
san lorenzo
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
san luis obispo
</td>
<td style="text-align:right;">
17
</td>
</tr>
<tr>
<td style="text-align:left;">
san marcos
</td>
<td style="text-align:right;">
5
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
3
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
28
</td>
</tr>
<tr>
<td style="text-align:left;">
san rafael
</td>
<td style="text-align:right;">
13
</td>
</tr>
<tr>
<td style="text-align:left;">
san ramon
</td>
<td style="text-align:right;">
8
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
sanger
</td>
<td style="text-align:right;">
1
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
36
</td>
</tr>
<tr>
<td style="text-align:left;">
santa barbara
</td>
<td style="text-align:right;">
26
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
3
</td>
</tr>
<tr>
<td style="text-align:left;">
santa cruz
</td>
<td style="text-align:right;">
121
</td>
</tr>
<tr>
<td style="text-align:left;">
santa fe springs
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
santa maria
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
santa monica
</td>
<td style="text-align:right;">
5
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
139
</td>
</tr>
<tr>
<td style="text-align:left;">
santee
</td>
<td style="text-align:right;">
6
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
14
</td>
</tr>
<tr>
<td style="text-align:left;">
seal beach
</td>
<td style="text-align:right;">
2
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
selma
</td>
<td style="text-align:right;">
3
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
5
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
4
</td>
</tr>
<tr>
<td style="text-align:left;">
signal hill
</td>
<td style="text-align:right;">
2
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
slo
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
1
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
sonora
</td>
<td style="text-align:right;">
14
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
1
</td>
</tr>
<tr>
<td style="text-align:left;">
south gate
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
south pasadena
</td>
<td style="text-align:right;">
3
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
19
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
24
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
8
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
1
</td>
</tr>
<tr>
<td style="text-align:left;">
sun city
</td>
<td style="text-align:right;">
25
</td>
</tr>
<tr>
<td style="text-align:left;">
sun valley
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
sunland
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
sunnyvale
</td>
<td style="text-align:right;">
33
</td>
</tr>
<tr>
<td style="text-align:left;">
sutter creek
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
suttercreek
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
sylmar
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
taft
</td>
<td style="text-align:right;">
14
</td>
</tr>
<tr>
<td style="text-align:left;">
tahoe
</td>
<td style="text-align:right;">
2
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
2
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
118
</td>
</tr>
<tr>
<td style="text-align:left;">
temple city
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
templeton
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
temporary housing
</td>
<td style="text-align:right;">
1
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
14
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
2
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
31
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
10
</td>
</tr>
<tr>
<td style="text-align:left;">
tujunga
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
tulare
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
turlock
</td>
<td style="text-align:right;">
17
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
4
</td>
</tr>
<tr>
<td style="text-align:left;">
twentynine palms
</td>
<td style="text-align:right;">
63
</td>
</tr>
<tr>
<td style="text-align:left;">
ukiah
</td>
<td style="text-align:right;">
38
</td>
</tr>
<tr>
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
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
unknown
</td>
<td style="text-align:right;">
134
</td>
</tr>
<tr>
<td style="text-align:left;">
upland
</td>
<td style="text-align:right;">
25
</td>
</tr>
<tr>
<td style="text-align:left;">
vacaville
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
valencia
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
vallejo
</td>
<td style="text-align:right;">
4
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
12
</td>
</tr>
<tr>
<td style="text-align:left;">
venice
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
ventura
</td>
<td style="text-align:right;">
69
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
85
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
14
</td>
</tr>
<tr>
<td style="text-align:left;">
vista
</td>
<td style="text-align:right;">
13
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
1
</td>
</tr>
<tr>
<td style="text-align:left;">
walnut creek
</td>
<td style="text-align:right;">
8
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
2
</td>
</tr>
<tr>
<td style="text-align:left;">
washington
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
watsonville
</td>
<td style="text-align:right;">
58
</td>
</tr>
<tr>
<td style="text-align:left;">
weaverville
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
weldon
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
west covina
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
west hills
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
west hollywood
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
west sacramento
</td>
<td style="text-align:right;">
18
</td>
</tr>
<tr>
<td style="text-align:left;">
westlake village
</td>
<td style="text-align:right;">
1
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
3
</td>
</tr>
<tr>
<td style="text-align:left;">
whittier
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
wildomar
</td>
<td style="text-align:right;">
35
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
23
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
willows
</td>
<td style="text-align:right;">
21
</td>
</tr>
<tr>
<td style="text-align:left;">
wilmington
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
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
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
winnetka
</td>
<td style="text-align:right;">
4
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
2
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
3
</td>
</tr>
<tr>
<td style="text-align:left;">
woodland hills
</td>
<td style="text-align:right;">
6
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
2
</td>
</tr>
<tr>
<td style="text-align:left;">
yreka
</td>
<td style="text-align:right;">
17
</td>
</tr>
<tr>
<td style="text-align:left;">
yuba county
</td>
<td style="text-align:right;">
8
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
48
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
121
</td>
</tr>
<tr>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
566
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
goleta-isla vista
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
2
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
4
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
62
</td>
</tr>
<tr>
<td style="text-align:left;">
rancho cucamongo
</td>
<td style="text-align:left;">
rancho cucamongo
</td>
<td style="text-align:right;">
1
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
40
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
3
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
72
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
34
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
8
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
3
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
1
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
3
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
71
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
50
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
17
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
9
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
7
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
3
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
5
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
810
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
12
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
2
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
43
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
rosevile snf oakridgere center
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
roseville
</td>
<td style="text-align:left;">
roseville
</td>
<td style="text-align:right;">
51
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
3
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
7
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
0
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
american indian/alaskan native
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
american indian/alaskan native
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
11
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
72
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
57
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
25
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
114
</td>
</tr>
<tr>
<td style="text-align:left;">
asian/asian american
</td>
<td style="text-align:left;">
gender other than female/male
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
male
</td>
<td style="text-align:right;">
102
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
319
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
209
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
3
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
622
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
604
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
31
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
24
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
30
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
373
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
307
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
native hawaiian/pacific islander
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
native hawaiian/pacific islander
</td>
<td style="text-align:left;">
male
</td>
<td style="text-align:right;">
20
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
data not collected
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
579
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
525
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
pacific islander/native hawaiian
</td>
<td style="text-align:left;">
female
</td>
<td style="text-align:right;">
26
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
10
</td>
</tr>
<tr>
<td style="text-align:left;">
unknown/not provided
</td>
<td style="text-align:left;">
decline to state
</td>
<td style="text-align:right;">
4
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
593
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
455
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
2
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
72
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
13
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
4175
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
3481
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
transgender
</td>
<td style="text-align:right;">
12
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
5
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
9
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
3
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
data not collected
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
49
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

    ## [1] 13087    53

# Re-coding variables
