# random_sampling

library(dplyr)
library(stringr)

# read_data
dataset <- read.table("BCDCM4318.fam", header = FALSE)

# extract populations which are over 40 individuals
populations <- dataset %>%
  count(V1) %>%
  filter(n >= 40 #set individuals) %>%
  pull(V1)

#sampling results in sampled_list
sampled_list <- list()

for (pop in populations) {
  filtered <- dataset %>% filter(str_detect(V1, fixed(pop)))
  sampled <- sample_n(filtered, 40)
  sampled_list[[pop]] <- sampled
}

#filter
dataset2 <- dataset %>% filter(!V1 %in% populations)

# merge the data
sampled_all <- bind_rows(sampled_list)
dataset3 <- bind_rows(dataset2, sampled_all)

# output
write.table(dataset3, "BCDCM1747", row.names = FALSE, col.names = FALSE, quote = FALSE)

#plink --keep
  system(sprintf("plink --bfile BCDCM4318 --cow --keep BCDCM1747 --make-bed --out BCDCM1747"))
}