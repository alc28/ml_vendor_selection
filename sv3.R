# sv3.R

# load libraries

library(tidyverse)
library(stringr)
library(readxl)

# load and prep datasets

# First:
# this dataset was pulled from Voyager.  Critical piece of info in this 
# dataset is the final vendor selection for the order

df_raw_vendor_labels <- read_excel("data/experiment3/lean_vendor_labels.xlsx")

# pull out the poofid

clean_poofid <- function(s) {
  poofid = str_remove(s, "POOF;http://poof2.library.cornell.edu/orders/")
  poofid = str_remove(poofid, ";Cornell")
  return(poofid)
}

df_vendor_labels <- df_raw_vendor_labels %>%
  filter(!is.na(Get980z)) %>%
  mutate(poofid = clean_poofid(Get980z),
         poofid = as.integer(poofid)) %>%
  select(-Get980z)

# Second:
# load dataset pulled from the poof2 server. Key field it adds
# is the rush status

dfalmostready4ml <- read_delim("data/experiment3/dfalmostready4ml.txt", 
                               "\t", escape_double = FALSE, trim_ws = TRUE)

df <- dfalmostready4ml %>%
  inner_join(df_vendor_labels, by = "poofid")

write_tsv(df, "data/experiment3/ready4ml.txt")


