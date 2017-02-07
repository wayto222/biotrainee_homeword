require(data.table)
options(stringsAsFactors = F)

#download url "ftp://ftp.ncbi.nlm.nih.gov/pub/CCDS/current_human/CCDS.current.txt"

ccds = fread("pdata/bio-info-trainee/CCDS-current.txt")
#把所有的exon 按染色体分组拼接
cc2 = ccds[,.(exos=paste0(cds_locations,collapse="")),by=`#chromosome`]
cc2[,ll:=gg(exos),`#chromosome`]
sum(cc2$ll)
#36048075

gg = function(x){
  #输入为 包含一些列exon坐标的的string
  #去重
  x =unique( stringr::str_extract_all(x,"\\d+-\\d+")[[1]])
  # mm = sum(sapply(xx3,function(x){abs(eval(parse(text = x)))}),na.rm = T) #much slower
  #提取exon位置
  x2 =stringi::stri_extract_all_regex(x,"\\d+")
  #计算单个exon长度
  x3 =lapply(x2,function(x){as.integer(x[1])-as.integer(x[2])})
  #计算总的exon长度
  sum(abs(unlist(x3)))
}
