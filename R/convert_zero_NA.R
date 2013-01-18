convert_zero_NA <-
function(data,vars){
  for (var in vars){
    data[,var]<<-ifelse(data[,var]==0,NA,data[,var])
  }
}
