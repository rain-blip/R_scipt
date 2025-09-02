# CambodiaM Randomly sample CambodiaM group and merge with other data

library(stringr)
library(dplyr)

dataset <- read.table("BCDCM1747.fam", header = FALSE)
dataset2 <- dataset[!(dataset$V1 == "CambodiaM"),]
CambodiaM <- filter(dataset, V1 == "CambodiaM")
sorted <- unique(dataset$V1)

# 100 times sampling
for (i in 1:100) {
  sampled <- CambodiaM[sample(nrow(CambodiaM), 3), ]
  combined <- bind_rows(dataset2, sampled)
  
  data <- combined %>%
    mutate(pop2 = factor(V1, levels = sorted)) %>%
    unique() %>%
    arrange(pop2) %>%
    select(-pop2)
  
  # Output data
  file.name <- sprintf("pop%03d", i)
  write.table(data, file.name, row.names = FALSE, col.names = FALSE, quote = FALSE)
  
  # Run PLINK -- use data for --keep as stdin
  keep.name <- sprintf("pop%03d", i)
  out.name <- sprintf("pop%03d", i)
  system(sprintf("plink --bfile BCDCM1747 --cow --keep %s --make-bed --out %s", keep.name, out.name))
}

##RRAA_average##
library(dplyr)
#The Q folder, which contains combined pop files and Q files, must be in the same directory


# Load the common pop file (only the first column) and rename it to "pop"
pop <- read.table("pop1_100", sep = "\t", header = FALSE) |>
  select(1) |>
  rename(pop = V1)

# Process Q (choose the value of K yourself)
for (k in 2:15) {
  folder <- paste0("Q", k)
  q_file <- file.path(folder, paste0("pop1_100_Q", k))
  
  if (file.exists(q_file)) {
    Q <- read.table(q_file, sep = " ", header = FALSE)
    
    # Merge Q and pop
    calculate_data <- cbind(Q, pop)
    Q_cols <- names(Q)
    each_pop <- unique(calculate_data$pop)
    
    # Calculate average Q for each population
    result <- lapply(each_pop, function(pop_name) {
      group_data <- filter(calculate_data, pop == pop_name) |>
        select(all_of(Q_cols))
      avg <- colMeans(group_data|>mutate_all(as.numeric),na.rm = TRUE)
      cbind(as.data.frame(t(avg)), pop = pop_name)
    }) |> bind_rows()
    
    # Output file name
    output_file <- paste0("Q_average_K", k, ".Q")
    write.table(result, output_file, row.names = FALSE, quote = FALSE, sep = "\t")
    cat("✅ Output complete:", output_file, "\n")
  } else {
    warning(paste("⚠ Q file not found →", q_file))
  }
}
