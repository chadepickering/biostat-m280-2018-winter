# Collect simulation results from files and store them in table.
nVals <- seq(100, 500, by=100)
dist <- c("gaussian", "t1", "t5")
filenames <- c()
for (i in 1:3) {
  for (n in nVals) {
    oFile <- paste(dist[i], '_n', n, '.txt', sep='')
    filenames <- append(filenames, oFile)
  }
}

means_raw <- lapply(filenames, function(x) read.table(x, header=FALSE))
means_df_raw <- do.call("rbind", means_raw) 
means_df_raw <- means_df_raw[ ,-1]
means_df_raw




