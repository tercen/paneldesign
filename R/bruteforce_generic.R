non_dupl_val <- function(df_row) {
  comb <- as.character(df_row)
  comb[!(comb %in% comb[duplicated(comb)])]
}

find_combinations <- function(combinations_df,
                              combination_list,
                              max_depth,
                              current_depth = 0) {
  print(paste0('max_depth=', max_depth))
  print(paste0('cur_depth=', current_depth))
  
  if (current_depth == 0) {
    for (i in 1:nrow(combinations_df)) {
      c1 = combinations_df[i,]$c1
      c2 = combinations_df[i,]$c2
      # first iteration
      combinations_match <- combination_list %>%
        filter(
          (mfcomb1 == c1 & mfcomb2 != c2) |
            (mfcomb1 == c2 & mfcomb2 != c1) |
            (mfcomb1 != c1 & mfcomb2 == c2) |
            (mfcomb1 != c2 & mfcomb2 == c1)
        )
      
      names(combinations_match) <- c(paste0("c", current_depth + 2, "_a"),
                                     paste0("c", current_depth + 2, "_b"))
      
      find_combinations(
        cbind(data.frame(c1_a = c1, c1_b = c2), combinations_match),
        combination_list,
        max_depth,
        current_depth + 1
      )
      
      exit()
    }
  } else if (current_depth + 1 < max_depth) {
    for (i in 1:nrow(combinations_df)) {
      unique_vals <- non_dupl_val(combinations_df[i,])
      
      combinations_match <- combination_list %>%
        filter((mfcomb1 %in% unique_vals & !(mfcomb2 %in% combinations_df[i,])) |
                 (!(mfcomb1 %in% combinations_df[i, ]) & mfcomb2 %in% unique_vals))
      
      names(combinations_match) <- c(paste0("c", current_depth + 2, "_a"),
                                     paste0("c", current_depth + 2, "_b"))
      
      if (nrow(combinations_match) > 0) {
        #print('original')
        #print(combinations_df[i, ])
        #print('new')
        #print(cbind(combinations_df[i,], combinations_match))
        find_combinations(
          cbind(combinations_df[i,], combinations_match),
          combination_list,
          max_depth,
          current_depth + 1
        )
      }
    }
  } else if (current_depth + 1 == max_depth) {
    print('final stage')
    
    for (i in 1:nrow(combinations_df)) {
      unique_vals <- non_dupl_val(combinations_df[i,])
      
      #print('original')
      #print(combinations_df[i, ])
      #print('unique')
      #print(unique_vals)
      
      if (length(unique_vals) == 2) {
        print(unique_vals[1])
        print(unique_vals[2])
        combinations_match <- combination_list %>%
          filter((mfcomb1 == unique_vals[1] & mfcomb2 == unique_vals[2]) |
                  (mfcomb1 == unique_vals[2] & mfcomb2 == unique_vals[1]))
        
        if (nrow(combinations_match) == 1) {
          print('wooo found')
        }
        
      } else {
        print('something went wrong')
      }
      
    }
      
  }
}
