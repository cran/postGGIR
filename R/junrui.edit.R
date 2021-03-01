#' @title Total Volumen of Activity for Whole Dataset
#' @description Calculate total volume of activity level, which includes
#' \code{TLAC} (total log transfored activity counts),
#' \code{TAC} (total activity counts).
#'
#' @param count.data \code{data.frame} of dimension n*1442 containing the 1440 minute activity data for all n subject days.
#' The first two columns have to be ID and Day.
#' @param weartime \code{data.frame} with dimension of \code{count.data}.
#' The first two columns have to be ID and Day.
#' @param logtransform if \code{TRUE}, then calcualte \code{TLAC}. Or calculate \code{TAC}.
#' @param log.multiplier  \code{number} The coefficient used in the log transformation of the ENMO data, i.e. log( log.multiplier * ENMO + 1). Defaut is 9250. 
#'
#'
#' @return A dataframe with some of the following columns
#' \item{ID}{identifier of the person}
#' \item{Day}{indicator of which day of activity it is, can be a numeric vector of sequence 1,2,... or a string of date}
#' \item{TAC}{total activity count}
#' \item{TLAC}{total log activity count}
#'
#'
#' @export
#' @details log transormation is defined as log(x+1).
#'
#'
 


Tvol2 = function(
  count.data,
  weartime,
  logtransform = FALSE, 
  log.multiplier=9250 
){
  if(missing(weartime)){
    print("No weartime supplied, calculated based on defualt from 05:00 to 23:00")
    weartime = wear_flag(count.data =  count.data)
  }

  uwear = unique(unlist(c(weartime[,-c(1,2)])))
  uwear = as.integer(uwear)
  if (!all(uwear %in% c(0, 1))) {
    stop("weartime matrix has non 0-1 data! Set NA to 0 if missing")
  }

  count.mat = as.matrix(count.data[,-c(1:2)])
  wear.mat = as.matrix(weartime[,-c(1:2)])

  if(logtransform){
    count.mat = log(count.mat*log.multiplier + 1)
  }

  adj.ct = count.mat * wear.mat
  tvol = as.data.frame(cbind(count.data[,c(1:2)],apply(adj.ct, 1, sum)))
  if(logtransform){
    names(tvol) = c("ID","Day","TLAC")
  }
  if(!logtransform){
    names(tvol) = c("ID","Day","TAC")
  }
  return(tvol = tvol)
}


################################################################################################################################

#' @title Fragmentation Metrics for Whole Dataset
#' @description Fragmentation methods to study the transition between two states, e.g.
#' sedentary v.s. active.This function is a whole dataset wrapper for \code{fragmentation}
#'
#' @param count.data \code{data.frame} of dimension n*1442 containing the 1440 minute activity data for all n subject days.
#' The first two columns have to be ID and Day.
#' @param weartime \code{data.frame} with dimension of \code{count.data}.
#' The first two columns have to be ID and Day.
#'
#' @param thresh threshold to binarize the data.
#' @param bout.length minimum duration of defining an active bout; defaults to 1.
#' @param metrics What is the fragmentation metrics to exract. Can be
#' "mean_bout","TP","Gini","power","hazard",or all the above metrics "all".
#' @param by Determine whether fragmentation is calcualted by day or by subjects (i.e. aggregate bouts across days).
#' by-subject is recommended to gain more power.
#'
#'
#'
#'
#' @return A dataframe with some of the following columns
#' \item{ID}{identifier of the person}
#' \item{Day}{indicator of which day of activity it is, can be a numeric vector of sequence 1,2,... or a string of date}
#' \item{mean_r}{mean sedentary bout duration}
#' \item{mean_a}{mean active bout duration}
#' \item{SATP}{sedentary to active transition probability}
#' \item{ASTP}{bactive to sedentary transition probability}
#' \item{Gini_r}{Gini index for active bout}
#' \item{Gini_a}{Gini index for sedentary bout}
#' \item{h_r}{hazard function for sedentary bout}
#' \item{h_a}{hazard function for active bout}
#' \item{alpha_r}{power law parameter for sedentary bout}
#' \item{alpha_a}{power law parameter for active bout}
#'
#'
#' @import  tidyr
#' @importFrom stats na.omit reshape
#' @importFrom dplyr group_by %>%
#' @importFrom dplyr do as_data_frame filter
#' @importFrom accelerometry bouts rle2
#' @importFrom survival survfit Surv
#' @importFrom ineq Gini
#'
#' @export
#' @details Metrics include
#' mean_bout (mean bout duration),
#' TP (between states transition probability),
#' Gini (gini index),
#' power (alapha parameter for power law distribution)
#' hazard (average hazard function)
#'
#'
 

 


fragmentation_long2 = function(
  count.data,
  weartime,
  thresh,
  bout.length = 1,
  metrics = c("mean_bout","TP","Gini","power","hazard","all"),
  by = c("day","subject")
){
  ID = value = . = NULL
  rm(list = c("ID", "value", "."))


  metrics = match.arg(metrics)
  by = match.arg(by)


  if(missing(weartime)){
    print("No weartime supplied, calculated based on defualt from 05:00 to 23:00")
    weartime = wear_flag(count.data =  count.data)
  }

  n1440<-ncol(count.data)-2
  n1441<- n1440+1
  n1442<- n1440+2
  n2880<- 2*n1440 

  if(by == "day"){
    mat = cbind(as.matrix(count.data[,-c(1:2)]),as.matrix(weartime[,-c(1:2)]))

    result.list =  apply(mat,1,function(x){
      fragmentation2(x[1:n1440],x[n1441:n2880],thresh = thresh,bout.length = bout.length, metrics = metrics)
    })

    vfrag = unlist(result.list)

    if(metrics == "all"){
      frag_all = as.data.frame(cbind(count.data[,c(1,2)],
                                     vfrag[seq(1,length(vfrag),10)],
                                     vfrag[seq(2,length(vfrag),10)],
                                     vfrag[seq(3,length(vfrag),10)],
                                     vfrag[seq(4,length(vfrag),10)],
                                     vfrag[seq(5,length(vfrag),10)],
                                     vfrag[seq(6,length(vfrag),10)],
                                     vfrag[seq(7,length(vfrag),10)],
                                     vfrag[seq(8,length(vfrag),10)],
                                     vfrag[seq(9,length(vfrag),10)],
                                     vfrag[seq(10,length(vfrag),10)]))
    }

    if(metrics != "all"){
      frag_all = as.data.frame(cbind(count.data[,c(1,2)],
                                     vfrag[seq(1,length(vfrag),2)],
                                     vfrag[seq(2,length(vfrag),2)]))
    }

    if(metrics == "mean_bout"){
      names(frag_all) = c("ID","Day","mean_r","mean_a")
    }

    if(metrics == "TP"){
      names(frag_all) = c("ID","Day","SATP","ASTP")
    }

    if(metrics == "Gini"){
      names(frag_all) = c("ID","Day","Gini_r","Gini_a")
    }


    if(metrics == "power"){
      names(frag_all) = c("ID","Day","alpha_r","alpha_a")
    }

    if(metrics == "hazard"){
      names(frag_all) = c("ID","Day","h_r","h_a")
    }

    if(metrics == "all"){
      names(frag_all) = c("ID","Day","mean_r","mean_a","SATP","ASTP",
                          "Gini_r","Gini_a","alpha_r","alpha_a","h_r","h_a")
    }
  }

  if(by == "subject"){

    long.count = reshape(count.data, varying = names(count.data)[3:n1442],direction = "long",
                         timevar = "MIN",idvar = c("ID","Day"),v.names = "values",new.row.names = c(1:(n1440*nrow(count.data))))
    long.count = long.count[
      with(long.count, order(ID, Day,MIN)),
      ]


    long.wear = reshape(weartime, varying = names(weartime)[3:n1442],direction = "long",
                         timevar = "MIN",idvar = c("ID","Day"),v.names = "values",new.row.names = c(1:(n1440*nrow(count.data))))
    long.wear= long.wear[
      with(long.wear, order(ID, Day,MIN)),
      ]


    longdata = data.frame(ID = long.count$ID, count = long.count$values, wear = long.wear$values)

    result= longdata  %>% group_by(ID) %>% do(out = fragmentation2(.$count,.$wear,thresh = thresh,
     bout.length = bout.length, metrics = metrics))

    # idlist = as.numeric(as.character(result$ID))  # bug for nccr-wg
    idlist = as.character(result$ID)   # bug for nccr

    result.list = result$out

    vfrag = unlist(result.list)

    if(metrics == "all"){
      frag_all = as.data.frame(cbind(idlist,
                                     vfrag[seq(1,length(vfrag),10)],
                                     vfrag[seq(2,length(vfrag),10)],
                                     vfrag[seq(3,length(vfrag),10)],
                                     vfrag[seq(4,length(vfrag),10)],
                                     vfrag[seq(5,length(vfrag),10)],
                                     vfrag[seq(6,length(vfrag),10)],
                                     vfrag[seq(7,length(vfrag),10)],
                                     vfrag[seq(8,length(vfrag),10)],
                                     vfrag[seq(9,length(vfrag),10)],
                                     vfrag[seq(10,length(vfrag),10)]))
    }
    if(metrics != "all"){
      frag_all = as.data.frame(cbind(idlist[,1],
                                     vfrag[seq(1,length(vfrag),2)],
                                     vfrag[seq(2,length(vfrag),2)]))
    }



    if(metrics == "mean_bout"){
      names(frag_all) = c("ID","mean_r","mean_a")
    }

    if(metrics == "TP"){
      names(frag_all) = c("ID","SATP","ASTP")
    }

    if(metrics == "Gini"){
      names(frag_all) = c("ID","Gini_r","Gini_a")
    }


    if(metrics == "power"){
      names(frag_all) = c("ID","alpha_r","alpha_a")
    }

    if(metrics == "hazard"){
      names(frag_all) = c("ID","h_r","h_a")
    }

    if(metrics == "all"){
      names(frag_all) = c("ID", "mean_r","mean_a","SATP","ASTP",
                          "Gini_r","Gini_a","alpha_r","alpha_a","h_r","h_a")
    }

    row.names(frag_all) = c(1:length(idlist))
  }

  return(frag_all)

}



 


#' @title Fragmentation Metrics
#' @description Fragmentation methods to study the transition between two states, e.g.
#' sedentary v.s. active.
#'
#' @param x \code{integer} \code{vector} of activity data.
#' @param w \code{vector} of wear flag data with same dimension as \code{x}.
#' @param thresh threshold to binarize the data.
#' @param bout.length minimum duration of defining an active bout; defaults to 1.
#' @param metrics What is the fragmentation metrics to exract. Can be
#' "mean_bout","TP","Gini","power","hazard",or all the above metrics "all".
#'
#' @return A list with elements
#' \item{mean_r}{mean sedentary bout duration}
#' \item{mean_a}{mean active bout duration}
#' \item{SATP}{sedentary to active transition probability}
#' \item{ASTP}{bactive to sedentary transition probability}
#' \item{Gini_r}{Gini index for active bout}
#' \item{Gini_a}{Gini index for sedentary bout}
#' \item{h_r}{hazard function for sedentary bout}
#' \item{h_a}{hazard function for active bout}
#' \item{alpha_r}{power law parameter for sedentary bout}
#' \item{alpha_a}{power law parameter for active bout}
#'
#' @importFrom stats na.omit reshape
#' @importFrom dplyr %>% as_data_frame filter
#' @importFrom accelerometry bouts rle2
#' @importFrom survival survfit Surv
#' @importFrom ineq Gini
#'
#' @export
#'
#' @references Di et al. Patterns of sedentary and active time accumulation are associated with mortality in US adults: The NHANES study.
#'
#' @details Metrics include
#' mean_bout (mean bout duration),
#' TP (between states transition probability),
#' Gini (gini index),
#' power (alapha parameter for power law distribution)
#' hazard (average hazard function)
#'
#' 
#'
#'


fragmentation2 = function(
  x,
  w,
  thresh ,
  bout.length = 1,
  metrics = c("mean_bout","TP","Gini","power","hazard","all")
){
 
  value = NULL
  rm(list = c("value"))

  metrics = match.arg(metrics)

  if(missing(w)){
    stop("Please input weartime flag vector w with same dimension")
  }


  if(length(x) != length(w)){
    stop("x and w should have the same length")
  }

  uwear = unique(c(w))
  uwear = as.integer(uwear)
  if (!all(uwear %in% c(0, 1, NA))) {
    stop("w has non 0-1 data!")
  }

  x = na.omit( x ) #enmo is not integer
  w = na.omit(w)
 

  w[w == 0] = NA
 

  y = create.bouts(counts = x, thresh_lower  = thresh, bout_length = bout.length) #y=0/1 for bouts
 

  yw = y * w
  
 

  uy = unique(na.omit(yw))
  if (length(uy) == 1) {
    #stop("Only one state found in the activity, no transition defined.")

      if(metrics == "mean_bout"){
       frag = list(mean_r = NA, mean_a = NA)
      }

      if(metrics == "TP"){
       frag = list(SATP = NA, ASTP = NA)
      }

      if(metrics == "Gini"){
        frag = list(Gini_r = NA, Gini_a = NA)
      }

      if(metrics == "power"){
        frag = list(alpha_r = NA, alpha_a = NA)
      }

      if(metrics == "hazard"){
        frag = list(h_r = NA, h_a = NA)
      }

      if (metrics == "all"){
      frag = list(mean_r = NA, mean_a = NA,
                  SATP = NA, ASTP = NA,
                  Gini_r = NA,
                  Gini_a = NA,
                  alpha_r = NA,
                  alpha_a = NA,
                  h_r =  NA,
                  h_a = NA
      )
      }
  }


  if (length(uy) > 1) {
  mat = as_data_frame(rle2(yw)) %>%
    filter(!is.na(value))

  A = mat$length[which(mat$value == 1)]
  R = mat$length[which(mat$value == 0)]

  if(metrics == "mean_bout"){
    frag = list(mean_r = mean(R), mean_a = mean(A))
  }

  if(metrics == "TP"){
    frag = list(SATP = 1/mean(R), ASTP = 1/mean(A))
  }

  if(metrics == "Gini"){
    frag = list(Gini_r = Gini(R,corr = T),
                Gini_a = Gini(A,corr = T))
  }


  if(metrics == "power"){
    nr = length(R)
    na = length(A)

    rmin = min(R)
    amin = min(A)

    frag = list(alpha_r = 1+ nr/sum(log(R/(rmin-0.5))),
                alpha_a = 1+ na/sum(log(A/(amin-0.5))))

  }

  if(metrics == "hazard"){
    fitr = survfit(Surv(R,rep(1,length(R)))~1)
    fita = survfit(Surv(A,rep(1,length(A)))~1)

    frag = list(h_r =  mean(fitr$n.event/fitr$n.risk),
                h_a = mean(fita$n.event/fita$n.risk))
  }

  if(metrics == "all"){

    nr = length(R)
    na = length(A)

    rmin = min(R)
    amin = min(A)

    fitr = survfit(Surv(R,rep(1,length(R)))~1)
    fita = survfit(Surv(A,rep(1,length(A)))~1)

    frag = list(mean_r = mean(R), mean_a = mean(A),
                SATP = 1/mean(R), ASTP = 1/mean(A),
                Gini_r = Gini(R,corr = T),
                Gini_a = Gini(A,corr = T),
                alpha_r = 1+ nr/sum(log(R/(rmin-0.5))),
                alpha_a = 1+ na/sum(log(A/(amin-0.5))),
                h_r =  mean(fitr$n.event/fitr$n.risk),
                h_a = mean(fita$n.event/fita$n.risk)
                )
  }}

  return(frag)
}


##########################################################################
# Without considering nonwear for imputed data
##########################################################################
create.bouts<-function(counts, thresh_lower, bout_length = 1){

S1<-which(counts>=thresh_lower)
S0<-which(counts<thresh_lower)
bouts<-rep(NA,length(counts))
bouts[S1]<-1
bouts[S0]<-0
if (bout_length>1){
   
  for (i in 2:length(S0)){
      W<-S0[i]-S0[i-1]
      if (W-1<bout_length) bouts[ S0[i-1]:S0[i] ]<-0 #kill 1 in this interval
  }
}

return(bouts)
}


 
#' @title Intradaily Variability
#' @description This function calcualte intradaily variability, a nonparametric metric
#' reprsenting fragmentation of circadian rhtymicity
#'
#' @param count.data \code{data.frame} of dimension n * 1442 containing the 1440 minute activity data for all n subject days.
#' @param level \code{character} the number of seconds in each window. level= 600 seconds (10 minutes) by default. 
#'  
#' @return IV 
#'
#'
#'
#' @export
#' 
#'
#'


IV_long2 = function(
  count.data,
  level = 600
){
  x = count.data[,-c(1:2)] 
  
     n10<-(ncol(count.data)-2)/(24*3600/level)
     if (n10==1) xc<- x else {
      xc<- array(NA,dim=c(nrow(x),ncol(x)/n10)) 
     for (j in 1:ncol(xc)) xc[,j]<-rowMeans(x[,(j-1)*n10+1:n10],na.rm=TRUE) } 
  
  result.list =  apply(xc,1,IV)
  iv_all = as.data.frame(cbind(count.data[,c(1,2)],result.list))

  names(iv_all) = c("ID","Day","IV")
  return(iv= iv_all)
}


##########################################################################
#   end
##########################################################################
#' @title Time of A Certain activity State
#' @description Calculate the total time of being in certain state, e.g. sedentary, active, MVPA, etc.
#'
#' @param x \code{vector} of activity data.
#' @param w \code{vector} of wear flag data with same dimension as \code{x}.
#' @param thresh threshold to binarize the data.
#' @param bout.length minimum duration of defining an active bout; defaults to 1.
#' @param smallerthan Find a state that is smaller than a threshold, or greater than or equal to.
#'
#' @return Time
#' @importFrom accelerometry bouts
#'
#' @export
#'
#'
#'

Time2 = function(
  x,
  w,
  thresh,
  smallerthan = TRUE,
  bout.length = 1
){
  if(missing(w)){
    stop("Please input weartime flag vector w with same dimension")
  }

  if(length(x) != length(w)){
    stop("count x and weartime w should have the same length")
  }

  uwear = unique(c(w))
  uwear = as.integer(uwear)
  if (!all(uwear %in% c(0, 1, NA))) {
    stop("weartime w has non 0-1 data")
  }

  x = na.omit(x)
  w = na.omit(w)


  w[w == 0] = NA
  y = create.bouts(counts = x, thresh_lower = thresh, bout_length = bout.length)
  yw = y * w

  if(smallerthan){
    time = sum(yw == 0, na.rm = T)
  }
  if(!smallerthan){
    time = sum(yw == 1, na.rm = T)
  }

  return(time = time)
}

 



#' @title Timne Metrics for Whole Dataset
#' @description This function is a whole dataset wrapper for \code{Time}
#'
#' @param count.data \code{data.frame} of dimension n*1442 containing the 1440 minute activity data for all n subject days.
#' The first two columns have to be ID and Day.
#' @param weartime \code{data.frame} with dimension of \code{count.data}.
#' The first two columns have to be ID and Day.
#'
#' @param thresh threshold to binarize the data.
#' @param bout.length minimum duration of defining an active bout; defaults to 1.
#' @param smallerthan Find a state that is smaller than a threshold, or greater than or equal to.
#'
#' @importFrom accelerometry bouts
#'
#' @return A dataframe with some of the following columns
#' \item{ID}{identifier of the person}
#' \item{Day}{indicator of which day of activity it is, can be a numeric vector of sequence 1,2,... or a string of date}
#' \item{time}{time of certain state}
#'
#' @export
#'
#'


Time_long2 = function(
  count.data,
  weartime,
  thresh,
  smallerthan = TRUE,
  bout.length = 1
){

  n1440<-ncol(count.data)-2
  n1441<- n1440+1
  n1442<- n1440+2
  n2880<- 2*n1440 
  minuteUnit<-n1440/1440

  if(missing(weartime)){
    print("No weartime supplied, calculated based on defualt from 05:00 to 23:00")
    weartime = wear_flag(count.data =  count.data)
  } else {
  if (length(which(count.data[,1]!= weartime[,1] | count.data[,2]!= weartime[,2]))>=1) stop ("Checking IDs between count.data and weartime in Time_long2 function.") 
  }



  mat = cbind(as.matrix(count.data[,-c(1:2)]),as.matrix(weartime[,-c(1:2)]))

  result.list =  apply(mat,1,function(x){
    Time2(x[1:n1440],x[n1441:n2880],thresh = thresh,bout.length = bout.length, smallerthan = smallerthan)
  })

  time_all = as.data.frame(cbind(count.data[,c(1,2)],result.list/minuteUnit))
  names(time_all) = c("ID","Day","time")


  return(time_all = time_all)
}


###########################################################################################
#  ExtCos_long
###########################################################################################
#' @title Extended Cosinor Model for Circadian Rhythmicity for Whole Dataset
#' @description A parametric approach to study circadian rhythmicity assuming cosinor shape.
#' Here we used the anti logistic transformed cosine curves, which provided 5 parameters.
#' This function is a wrapper for \code{ExtCos}.
#'
#' @param count.data \code{data.frame} of dimension n*1442 containing the 1440 minute activity data for all n subject days.
#' The first two columns have to be ID and Day.
#' @param logtransform Conduct log transfomation before. Default is \code{TRUE}.
#'
#' @importFrom minpack.lm nls.lm nls.lm.control
#' @importFrom stats coef reshape
#' @importFrom dplyr group_by %>% do
#'
#' @return A \code{data.frame} with following columns
#' \item{ID}{identifier of the person}
#' \item{min}{minimum}
#' \item{amp}{amplitude}
#' \item{alpha}{alpha parameter}
#' \item{beta}{beta parameter}
#' \item{acro}{acrophase}
#' \item{F_imp}{pseudo-F statistics}
#'
#' @export
#'


#library(dplyr)  # %>%
#library(cosinor)  
 


ExtCos_long2 = function(
  count.data,
  logtransform = TRUE
){

  # stupid NSE problem with dplyr
  ID = values = . = NULL
  rm(list = c("ID", "values", "."))
  n1440<-ncol(count.data)-2
  n1441<- n1440+1
  n1442<- n1440+2
  n2880<- 2*n1440 


  long.count = reshape(count.data, varying = names(count.data)[3:n1442],direction = "long",
                       timevar = "MIN",idvar = c("ID","Day"),v.names = "values",
                       new.row.names = c(1:(n1440*nrow(count.data))))
  
  long.count = long.count[
    with(long.count, order(ID, Day,MIN)),
    ]

  result= long.count  %>% group_by(ID) %>% do(out = ExtCos2(.$values,logtransform = logtransform))

  idlist = result$ID
  result.list = result$out

  vext= unlist(result.list)
  ext_all = as.data.frame(cbind(idlist,
                                vext[seq(1,length(vext),2)],
                                vext[seq(2,length(vext),2)] ))
  names(ext_all) = c("ID", "amp", "acro" )
  row.names(ext_all) = c(1:length(idlist))
  return(ext_all)
}

###########################################################################################
#  ExtCos
###########################################################################################

#' @title Extended Cosinor Model for Circadian Rhythmicity
#' @description A parametric approach to study circadian rhythmicity assuming cosinor shape.
#' Here we used the anti logistic transformed cosine curves, which provided 5 parameters.
#'
#'
#' @param x \code{vector} vector of dimension n*1440 which reprsents n days of 1440 minute activity data
#' @param logtransform Conduct log transfomation before. Default is \code{TRUE}.
#'
#' @importFrom minpack.lm nls.lm nls.lm.control
#' @importFrom stats coef residuals
#'
#' @return A list with elements
#' \item{min}{minimum}
#' \item{amp}{amplitude}
#' \item{alpha}{alpha parameter:determines whether the peaks of the curve are wider than the troughs}
#' \item{beta}{beta parameter: determines whether the function rises and falls more steeply than the cosine curve}
#' \item{acro}{acrophase}
#' \item{F_imp}{pseudo-F statistics}
#'
#' @export
#'



ExtCos2 <- function(
  x,
  logtransform = TRUE){ 
  
  n1440=length(x)
  n60=n1440/24 #window size of one hour

  if(logtransform){
    x = log(x + 1)
  }
  n.days <- length(x)/n1440
  tmp.dat <- data.frame(time = rep(1:n1440, n.days) / n60, Y = x)
  fit <- cosinor.lm(Y ~ time(time) + 1, data = tmp.dat, period = 24)
  #####stage 1#####
  #par <- c(1, 1, 1)
  #fit.nls <- nls.lm(par = par, fn = fn.res1, tmp.dat = tmp.dat)
  #coef.nls <- coef(fit.nls)
  #####stage 2#######
  #newpar <- c(coef.nls[1] - abs(coef.nls[2]), 2 * abs(coef.nls[2]), 0, 2, coef.nls[3])
  #fit2.nls <- nls.lm(newpar, fn = fn.res2, tmp.dat = tmp.dat, control = nls.lm.control(maxiter = 1000))
  #####stage 2#######

  cosinor.stat = as.numeric(coef(fit))  #acrophase/amplitude,
 
  return(  list(amp = abs(cosinor.stat[2]), acro = cosinor.stat[3] ))
}

###########################################################################################
# 1) Tvol2      2)fragmentation2    3) fragmentation_long2  
# 4) create.bouts 
# IV_long2 (no IV)
# Time2
# Extcos RA2 RA_long2
###########################################################################################



#' @title Relative Amplitude
#' @description This function calcualte relative amplitude, a nonparametric metric
#' reprsenting fragmentation of circadian rhtymicity
#'
#' @param x  \code{vector} vector of dimension 440 which reprsents 1440 minute activity data
#' @return RA
#'
#' @importFrom zoo rollapplyr
#'
#' @export
#'
#'
#'
#'

RA2 = function(
  x
){
  # x = count.data[1,-c(1:2)] 
  day.counts = as.numeric(x)
  n60=length(x)/24 #window size of one hour
  n600=10*n60 
  n300=n600/2
 
  M10TIME_num_index= which.max(roll(day.counts,n600) ) 
  L5TIME_num_index = which.min(roll(day.counts,n300) )  

  M10TIME_num= M10TIME_num_index/n60
  L5TIME_num = L5TIME_num_index/n60
  M10 = max(roll(day.counts,n600),na.rm=TRUE)
  L5 = min(roll(day.counts,n300),na.rm=TRUE) 
  RA<-(M10 - L5)/(M10 + L5)
  output<-c(M10TIME_num,L5TIME_num,M10,L5,RA) 
   
  return(output)
}

roll = function(day.counts,k){
  kvec = rollapplyr(day.counts, k, function(x) mean(x,na.rm = T), fill = NA)  # rollapplyr (R package zoo)
  kvec = kvec[!is.na(kvec)]
  return(kvec)
}



#' @title Relative Amplitude for Whole Dataset
#' @description Relative Amplitude is a nonparametric metric
#' reprsenting fragmentation of circadian rhtymicity. this function is a whole data wrapper for
#' \code{RA}.
#'
#' @param count.data \code{data.frame} of dimension n * 1442 containing the 1440 minute activity data for all n subject days.
#' The first two columns have to be ID and Day.
#' @return A \code{data.frame} with following columns
#' \item{ID}{identifier of the person}
#' \item{Day}{indicator of which day of activity it is, can be a numeric vector of sequence 1,2,... or a string of date}
#' \item{RA}{RA}
#'
#'#' @importFrom zoo rollapplyr
#'
#' @export
#'
#'
#'
RA_long2 = function(
  count.data
){
  x = count.data[,-c(1:2)]
  result.list =  t(apply(x,1,RA2)) #run RA2 for each row
  ra_all = as.data.frame(cbind(count.data[,c(1,2)],result.list))

  names(ra_all) = c("ID","Day","M10TIME","L5TIME","M10","L5","RA")
  return(ra = ra_all)
}

 





############################################
##   Fit cosinor model on multiple days   ##
##   and extract parameters               ##
##   Di, Junrui 4/27/2020                 ##
############################################
#' @title  Cosinor Model for Circadian Rhythmicity for Whole Dataset
#' @description A parametric approach to study circadian rhythmicity assuming cosinor shape.
#' Here we used the anti logistic transformed cosine curves, which provided 3 parameters. 
#'
#' @param  data \code{data.frame} of dimension n*1442 containing the 1440 minute activity data for all n subject days.
#' The first two columns have to be ID and Day.
#' @param window  \code{number} 1440/window is the dimension of the data. Default is 1.
#'
#' @importFrom minpack.lm nls.lm nls.lm.control 
#'
#' @return A \code{data.frame} with following columns
#' \item{ID}{identifier of the person}
#' \item{mesor}{mesor}
#' \item{amp}{amplitude} 
#' \item{acro}{acrophase} 
#'
#' @export
#'




ActCosinor = function(data, window=1){

  C1=which(colnames(data)=="ID")
  if (length(C1)==0) stop("No ID column found in the input data")
  IDlist = unique(data[,C1])
  cors = NULL
  n1440<-ncol(data)-2 
  n60=n1440/24          # window size of one hour
  newdim = n1440/window # dimension of the data, if window = 1, the dimension of the data is
                        # 1440, i.e there is no binning. If window = 10, we bin the data into
                        # 10 minute window, and the dimension of the data is 144.
  
  for(i in 1:length(IDlist)){
    
    id.i = IDlist[i]
    data.i = data[which(data[,C1]==id.i),-c(1:2)]
    x = c(t(data.i)) # a long vecotr of couple of days of data
    n.days = length(x)/newdim
    
    # Prep the data for cosinor model
    tmp.dat = data.frame(time = rep(1:newdim, n.days) / (n60/window), Y = x) #convert 0-24hour data, so make sure data columns are MIN1,MIN2...
    fit = cosinor.lm(Y ~ time(time) + 1, data = tmp.dat, period = 24)
    
    mesor = fit$coefficients[1]
    amp = fit$coefficients[2]
    acr = fit$coefficients[3]
    acr_fix = correct.acrophase(fit)          ### added
    acr_time = (-1) * acr_fix * 24/(2 * pi)   ### added
    
    
    
    cors = rbind(cors,c(mesor,amp,acr_time)) 
  }
  cors = cbind(IDlist, cors)
  colnames(cors) = c("ID", "mesor","amp","acro") 
  return(cors)
}

 


correct.acrophase = function (x) 
{
  # x is the fit from cosinor.lm
  coefs =  data.frame(t(x$fit$coefficients))
  if (coefs$rrr > 0 & coefs$sss > 0) {
    acrophase = 0 + (-1 * atan(abs(coefs$sss/coefs$rrr)))
  }
  else if (coefs$rrr > 0 & coefs$sss < 0) {
    acrophase = 2 * -1 * pi + (1 * atan(abs(coefs$sss/coefs$rrr)))
  }
  else if (coefs$rrr < 0 & coefs$sss > 0) {
    acrophase = pi * -1 + (1 * atan(abs(coefs$sss/coefs$rrr)))
  }
  else {
    acrophase = pi * -1 + (-1 * atan(abs(coefs$sss/coefs$rrr)))
  }
  return(acrophase)
}

