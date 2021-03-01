#' @title Interdaily Statbility
#' @description This function calcualte interdaily stability, a nonparametric metric of circadian rhtymicity
#'
#' @param count.data  \code{data.frame} of dimension p by 1440, representing the p days of activity data for one subjectect
#' @param level \code{character} level="hour" or not
#' @return IS
#'
#'
#' @export
#'
#'
#'
#'

 


IS_long2 = function(count.data,
  level = "hour"
){
  # subject level feature since input has to be a n*p matrix.
  # library(dplyr) 

  idlist = unique(count.data[,"ID"])
  y<-NULL
  for (i in 1:length(idlist))
     y[i]<-IS2(count.data[which(count.data[,"ID"]==idlist[i]),-c(1,2)],level=level) 
  ext_all =  data.frame(ID=idlist,IS=y) 
  row.names(ext_all) = c(1:length(idlist))
  return(ext_all) 
}

 
 
 

IS2 = function(x, level = "hour"
){  
  if (level=="hour") {
      xc<- array(NA,dim=c(nrow(x),ncol(x)/10)) 
     for (j in 1:ncol(xc)) xc[,j]<-rowMeans(x[,(j-1)*10+1:10],na.rm=TRUE) } else xc<-x
 
  p = ncol(xc)
  xh = colMeans(xc)

  v = c(t(xc))
  n = length(v)

  numerator = sum((xh - mean(v))^2)/p
  denominator = sum((v - mean(v))^2)/n

  return(numerator/denominator)
}

 
 