# sv2.R

library(tidyverse)
library(stringr)

# load pre-matrix data

dfraw <- read_delim("data/experiment2/delim.txt", 
                    "\t", escape_double = FALSE, trim_ws = TRUE)

clean_rush <- function(s) {
  #print(s)
  rush = str_extract(s, "\\$q.+?\\$")
  rush = str_remove(rush, "\\$q")
  rush = str_remove(rush, ";Rush:")
  rush = str_remove_all(rush, "\\$")
  rush = ifelse(rush == "Yes", 1, 0)
  return(rush)
}
  

df <- dfraw %>%
  rename(leader = `000`,
         poofid = `001`,
         f008 = `008`,
         f260 = `260`,
         f980 = `980`,
         f981 = `981`) %>%
  mutate(bibtype = str_sub(leader,7,8),
         date = str_sub(f008, 8,11),
         form = str_sub(f008, 25,25),
         place = str_sub(f008, 16,18),
         language = str_sub(f008, 36,38),
         fundloc = str_extract(f980, "\\$h.+?\\$"),
         fundloc = str_remove(fundloc, "\\$h"),
         fundloc = str_remove_all(fundloc, "\\$"),
         rush = clean_rush(f981)) %>%
  separate(fundloc, into = c("fund", "location"), sep = "-") %>%
  select(-leader, -f008)

write_tsv(df, "data/experiment2/dfalmostready4ml.txt")
