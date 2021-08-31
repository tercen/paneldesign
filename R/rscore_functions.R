# rscore_functions.R
# this R module contains only the rscore functions to seperate
# functionality from other moduless

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
