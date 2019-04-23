#sv1.R

library(tidyverse)
library(stringr)

# load file with correct vendor assignments

dfwithfinalvendor <- read_csv("data/experiment1/vendors_selected_201801_201806.txt")


# labels

labelcounts <- dfwithfinalvendor %>%
  filter(!is.na(Get980z)) %>%
  select(BIB_ID, VENDOR_CODE) %>%
  count(VENDOR_CODE)

labels <- dfwithfinalvendor %>%
  filter(!is.na(Get980z)) %>%
  select(BIB_ID, VENDOR_CODE)

# load in tidy marc of keylist job

dfraw <- read_delim("data/experiment1/delim.txt", 
                    "\t", escape_double = FALSE, trim_ws = TRUE)

# Publisher 260/264$b
# Bib. Type/Level ldr/6,2
# Form of Item 008/23,1 
# Place of Publication 008/15,3
# Language 008/35,3
# Govt. Doc. 008/28,1
# Location 980$-h
# Requestor 981$n
# Date <= 008/7,4

df <- dfraw %>%
  rename(BIB_ID = `001`,
         leader = `000`,
         f008 = `008`,
         f260 = `260`,
         f264 = `264`,
         f980 = `980`,
         f981 = `981`) %>%
    mutate(bibtype = str_sub(leader,7,8),
           date = str_sub(f008, 8,11),
           form = str_sub(f008, 25,25),
           place = str_sub(f008, 16,18),
           language = str_sub(f008, 36,38),
           fund = str_extract(f980, "\\$h.+?\\$"),
           fund = str_remove(fund, "\\$h"),
           fund = str_remove_all(fund, "\\$"),
           poofid = str_extract(f980, "\\$z.+?\\$"),
           poofid = str_remove(poofid, "\\$zPOOF;http://poof2.library.cornell.edu/orders/"),
           poofid = str_remove(poofid, ";Cornell\\$"),
           govdoc = str_sub(f008, 29,29)) %>%
  select(-leader, -f008) %>%
  filter(!is.na(f980))

# join labels and available features

dfcomplete <- labels %>%
  inner_join(df)

df264 <- dfcomplete %>%
  select(BIB_ID, f264) %>%
  filter(str_detect(f264, "\\$b")) %>%
  mutate(f264b = str_extract(f264, "\\$b.+,"))

df260 <- dfcomplete %>%
  select(BIB_ID, f260) %>%
  filter(str_detect(f260, "\\$b")) %>%
  mutate(f260b = str_extract(f260, "\\$b.+,"))

dfready4ml <- dfcomplete %>%
  filter(str_detect(f260, "\\$b") | str_detect(f264, "\\$b")) %>%
  mutate(f260b = str_extract(f260, "\\$b.+,"),
         f264b = str_extract(f264, "\\$b.+,")) %>%
  mutate(publisher = ifelse(!is.na(f260b), f260b, f264b)) %>%
  mutate(publisher = str_remove(publisher, "\\$b"),
         publisher = str_remove(publisher, ",$")) %>%
  select(BIB_ID, VENDOR_CODE, bibtype, date, form, place, language, govdoc, publisher, fund, poofid) 

write_tsv(dfready4ml, "data/experiment1/dfready4ml.txt")


# export bibids for harvest keyjob

mlsv1_bibids <- df %>%
  select(BIB_ID)
write_delim(mlsv1_bibids, "data/experiment1/mlsv1_bibids.txt")
