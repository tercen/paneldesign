m_filename <- file.path("examples", "3c", "m.tsv")
f_filename <- file.path("examples", "3c", "f.tsv")
m_x_m_filename <- file.path("examples", "3c", "m_x_m.tsv")
f_x_f_filename <- file.path("examples", "3c", "f_x_f.tsv")

mf_combs_filename <- file.path("output", "table_3.tsv")

m <- read.delim(m_filename, header = FALSE)
f <- read.delim(f_filename, header = FALSE)

m_x_m <- read.delim(m_x_m_filename, row.names = 1)
f_x_f <- read.delim(f_x_f_filename, row.names = 1)

mf_combs <- read.delim(mf_combs_filename)

m_x_m[1, 2]
f_x_f[1, 2]
f_x_f[2, 1]


rscore = function(mA, mB,
                  fA, fB,
                  SAB, SBA,
                  mAB) {
  delta = abs(mA * fA - mB * fB)
  spread = (mA * fA * SAB + mB * fB * SBA) * mAB
  total = spread + delta
}



calculate_score <- function(a_row) {
  mfcomb1 <- unlist(strsplit(a_row[[1]], split = "_"))
  mfcomb2 <- unlist(strsplit(a_row[[2]], split = "_"))
  
  m_index_1 <- as.numeric(substr(mfcomb1, start = 2, stop = 2))[1]
  f_index_1 <- as.numeric(substr(mfcomb1, start = 2, stop = 2))[2]
  
  m_index_2 <- as.numeric(substr(mfcomb2, start = 2, stop = 2))[1]
  f_index_2 <- as.numeric(substr(mfcomb2, start = 2, stop = 2))[2]
  
  s = rscore(m[m_index_1, 2],
             m[m_index_2, 2],
             f[f_index_1, 2],
             f[f_index_2, 2],
             f_x_f[f_index_1, f_index_2],
             f_x_f[f_index_2, f_index_1],
             m_x_m[m_index_1, m_index_2])
  return(s)
}

calculate_score(mf_combs[1,])

score_list <- vector("numeric", nrow(mf_combs))
for (i in 1:nrow(mf_combs)) {
  score_list[i] <- calculate_score(mf_combs[i, ])
}

score_result <- data.frame(mf_combs, score_list)

num = 3
file_name <- paste0("scores_output", "_", num, ".tsv")

write.table(
  score_result,
  file = file.path("output", file_name) ,
  sep = "\t",
  row.names = FALSE
)
