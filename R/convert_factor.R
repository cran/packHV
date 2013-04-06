convert_factor <-
function(data,vars){
  for (var in vars){
    data[,var]=factor(data[,var])
  }
  data
}
