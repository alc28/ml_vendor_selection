# load libraries
library(tidyverse)
library(stringr)
library(readr)
library(tidytext)

# import tidy data

import_filename <- "data/experiment1/tidy_mlsv1.txt"
export_filename <- "data/experiment1/out1.txt"

tidy_marc <- read_delim(import_filename, 
                        "\t", escape_double = FALSE, trim_ws = TRUE)

tidy_marc <- tidy_marc %>%
  mutate(num_dollarzero = str_count(rawfield, "\\$0"))

count_mrcfield <- tidy_marc %>%
  count(mrcfield)

sum_num_dz <- tidy_marc %>%
  group_by(mrcfield) %>%
  summarise(sum_num_zd = sum(num_dollarzero))

tidy_nest <- tidy_marc %>%
  mutate(dz = str_extract_all(rawfield, "\\$0.+") ) %>%
  unnest(.drop = FALSE)
  

tidy_split <- tidy_marc %>%
  mutate(dz = str_split(rawfield, "\\$")) %>%
  unnest() %>%
  filter(str_detect(dz, pattern = "0http")) %>%
  mutate(dz = str_replace(dz, "0http", "http"))

tidy_split <- tidy_split[c(1,2,3,6,4)]
write_delim(tidy_split, "data/tidy_split_oclc_masters.txt", delim = "\t")

tidy_sample <- sample_n(tidy_split, 100, replace = FALSE)
write_delim(tidy_sample, "data/dz_sample_oclc_masters.txt", delim = "\t")

#############


tidy_split %>% 
  glimpse()

tidy_split %>% 
  count(mrcfield) %>%
  arrange(desc(n)) 

tidy_split %>% 
  count(mrcfield) %>%
  arrange(desc(n)) %>%
  ggplot() + geom_col(aes(x=mrcfield, y = n)) +
  labs(x = "marc tag", 
       y = "total $0 uris", 
       title = "$0 URI frequency following MarcEdit enrichment",
       subtitle = paste("Total marc records in set: ", length(unique(tidy_marc$bib_id))),
       caption = "Input file: OCLC_Masters_matching_Voyager_with_MARCEdit_URIs.mrk")
  
ggsave(plot = last_plot(), "output/dz_frequency_in_OCLC_Masters_matching_Voyager_with_MARCEdit_URIs.png", width = 10, height = 6)


sum_tidy_marc <- tidy_marc %>% 
  group_by(bib_id) %>%
  summarise(total_dz_in_record = sum(num_dollarzero)) %>%
  arrange( desc(total_dz_in_record)  )

f050 <- tidy_marc %>%
  filter(mrcfield == '050') %>%
  mutate(callnumber = str_sub(rawfield, str_locate(rawfield, "\\$")[1] + 2, str_length(rawfield))) %>%
  arrange(callnumber)
