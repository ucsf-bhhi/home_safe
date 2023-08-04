# Cleaning strings and recoding demographic data

## make all character variables lower case and remove trailing/starting white space, and extra inner white spaces

demo_dat <- demo_dat %>%
  mutate(across(where(is.character), clean_characters))


##
