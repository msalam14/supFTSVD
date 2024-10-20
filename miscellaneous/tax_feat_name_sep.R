tax_feat_sep<-function(feat_name,pattern_vec){
  taxINFO<-NULL
  res<-str_split_1(feat_name,paste(pattern_vec[1],"__",sep=""))[2]
  for(i in 1:(length(pattern_vec)-1)){
    res1<-str_split_1(res,paste(pattern_vec[i+1],"__",sep=""))
    res<-res1[2]
    taxINFO<-c(taxINFO,res1[1])
  }
  drpI<-nchar(taxINFO)
  c(sapply(seq_len(length(drpI)), function(u){
    substr(taxINFO[u],1,drpI[u]-1)
  }),res)
}
