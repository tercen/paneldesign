library(tidyr)
library(dplyr)

generate_names <- function(a_letter, a_number) {
  return(paste0(a_letter, seq(1:a_number)))
}

num = 8
marker_names <- generate_names("m", num)
filter_names <- generate_names("f", num)

mf_combs <- expand.grid(marker_names,
                        filter_names,
                        marker_names,
                        filter_names,
                        stringsAsFactors = FALSE)

is_valid_m <- mf_combs[[1]] != mf_combs[[3]]
is_valid_f <- mf_combs[[2]] != mf_combs[[4]]
is_valid <- is_valid_m & is_valid_f

mf_combs <- mf_combs[is_valid,]

mfcomb1 <- mf_combs %>%
  select(Var1, Var2) %>%
  unite("mfcomb1", Var1, Var2)


mfcomb2 <- mf_combs %>%
  select(Var3, Var4) %>%
  unite("mfcomb2", Var3, Var4)

mfcomb12 <- cbind(mfcomb1, mfcomb2)

is_duplicate <- function(a_row1_df, a_row2_df) {
  if (a_row1_df[[1]] == a_row2_df[[2]])
  {
    if (a_row1_df[[2]] == a_row2_df[[1]]) {
      return(TRUE)
    }
  }
  return(FALSE)
}

table <- mfcomb12

for (j in 1:((nrow(table)) / 2)) {
  duplicates <- vector("logical", nrow(table))
  
  for (i in 1:nrow(table)) {
    duplicates[i] <- is_duplicate(table[j, ], table[i, ])
  }
  table <- table[!duplicates,]
}

file_name <- paste0("table", "_", num, ".tsv")
write.table(
  table,
  file = file.path("output", file_name) ,
  sep = "\t",
  row.names = FALSE
)

# table_split_1 <- table %>%
#   select(mfcomb1) %>%
#   separate(mfcomb1, c("m", "f"), sep = "_")
#
#
# table_split_2 <- table %>%
#   select(mfcomb2) %>%
#   separate(mfcomb2, c("m", "f"), sep = "_")
