#load the required packages
library(ggplot2)
library(dplyr)
library(tidyr)

#data tidying and transforming
oceanic_four <- DAISIETable %>%
  dplyr::filter(island_type == 'oceanic') %>%
  dplyr::filter(time == 4)

oceanic_ten <- DAISIETable %>%
  dplyr::filter(island_type == 'oceanic') %>%
  dplyr::filter(time == 10)

nonoceanic_four_one_nine <- DAISIETable %>%
  dplyr::filter(island_type == 'nonoceanic') %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(prop_mainland == 0.1) %>%
  dplyr::filter(prop_non_endemic == 0.9)

nonoceanic_four_four_nine <- DAISIETable %>%
  dplyr::filter(island_type == 'nonoceanic') %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(prop_mainland == 0.4) %>%
  dplyr::filter(prop_non_endemic == 0.9)

nonoceanic_four_one_five <- DAISIETable %>%
  dplyr::filter(island_type == 'nonoceanic') %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(prop_mainland == 0.1) %>%
  dplyr::filter(prop_non_endemic == 0.5)

nonoceanic_four_four_five <- DAISIETable %>%
  dplyr::filter(island_type == 'nonoceanic') %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(prop_mainland == 0.4) %>%
  dplyr::filter(prop_non_endemic == 0.5)

nonoceanic_four_one_seven <- DAISIETable %>%
  dplyr::filter(island_type == 'nonoceanic') %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(prop_mainland == 0.1) %>%
  dplyr::filter(prop_non_endemic == 0.7)

nonoceanic_four_four_sevem <- DAISIETable %>%
  dplyr::filter(island_type == 'nonoceanic') %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(prop_mainland == 0.4) %>%
  dplyr::filter(prop_non_endemic == 0.7)

nonoceanic_four_two_nine <- DAISIETable %>%
  dplyr::filter(island_type == 'nonoceanic') %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(prop_mainland == 0.25) %>%
  dplyr::filter(prop_non_endemic == 0.9)

nonoceanic_four_two_seven <- DAISIETable %>%
  dplyr::filter(island_type == 'nonoceanic') %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(prop_mainland == 0.25) %>%
  dplyr::filter(prop_non_endemic == 0.7)

nonoceanic_four_two_five <- DAISIETable %>%
  dplyr::filter(island_type == 'nonoceanic') %>%
  dplyr::filter(time == 4) %>%
  dplyr::filter(prop_mainland == 0.25) %>%
  dplyr::filter(prop_non_endemic == 0.5)  

nonoceanic_ten_one_nine <- DAISIETable %>%
  dplyr::filter(island_type == 'nonoceanic') %>%
  dplyr::filter(time == 10) %>%
  dplyr::filter(prop_mainland == 0.1) %>%
  dplyr::filter(prop_non_endemic == 0.9)

nonoceanic_ten_four_nine <- DAISIETable %>%
  dplyr::filter(island_type == 'nonoceanic') %>%
  dplyr::filter(time == 10) %>%
  dplyr::filter(prop_mainland == 0.4) %>%
  dplyr::filter(prop_non_endemic == 0.9)

nonoceanic_ten_one_five <- DAISIETable %>%
  dplyr::filter(island_type == 'nonoceanic') %>%
  dplyr::filter(time == 10) %>%
  dplyr::filter(prop_mainland == 0.1) %>%
  dplyr::filter(prop_non_endemic == 0.5)

nonoceanic_ten_four_five <- DAISIETable %>%
  dplyr::filter(island_type == 'nonoceanic') %>%
  dplyr::filter(time == 10) %>%
  dplyr::filter(prop_mainland == 0.4) %>%
  dplyr::filter(prop_non_endemic == 0.5)

nonoceanic_ten_one_seven <- DAISIETable %>%
  dplyr::filter(island_type == 'nonoceanic') %>%
  dplyr::filter(time == 10) %>%
  dplyr::filter(prop_mainland == 0.1) %>%
  dplyr::filter(prop_non_endemic == 0.7)

nonoceanic_ten_four_sevem <- DAISIETable %>%
  dplyr::filter(island_type == 'nonoceanic') %>%
  dplyr::filter(time == 10) %>%
  dplyr::filter(prop_mainland == 0.4) %>%
  dplyr::filter(prop_non_endemic == 0.7)

nonoceanic_ten_two_nine <- DAISIETable %>%
  dplyr::filter(island_type == 'nonoceanic') %>%
  dplyr::filter(time == 10) %>%
  dplyr::filter(prop_mainland == 0.25) %>%
  dplyr::filter(prop_non_endemic == 0.9)

nonoceanic_ten_two_seven <- DAISIETable %>%
  dplyr::filter(island_type == 'nonoceanic') %>%
  dplyr::filter(time == 10) %>%
  dplyr::filter(prop_mainland == 0.25) %>%
  dplyr::filter(prop_non_endemic == 0.7)

nonoceanic_ten_two_five <- DAISIETable %>%
  dplyr::filter(island_type == 'nonoceanic') %>%
  dplyr::filter(time == 10) %>%
  dplyr::filter(prop_mainland == 0.25) %>%
  dplyr::filter(prop_non_endemic == 0.5) %>%
  tidyr::drop_na()
