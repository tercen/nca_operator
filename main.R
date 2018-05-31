library(tercen)
library(dplyr)
library(NonCompart)

do.nca = function(df, dose, R2ADJ) {

  iAUC = data.frame(Name=c("AUC[0-8h]","AUC[0-24h]"), Start=c(0,0), End=c(8,24))
  
  pk <-NonCompart::sNCA(df$.x,df$.y, dose = dose, concUnit="mg/mL",iAUC=iAUC, down = "Log", R2ADJ = R2ADJ)
  TMAX <- round(pk["TMAX"])
  CMAX <- round(pk["CMAX"])
  LAMZHL <- round(pk["LAMZHL"])
  AUC_0_8h <- round(pk["AUC[0-8h]"])
  AUC_0_24h <-round(pk["AUC[0-24h]"])
  
  result_df <-  data.frame(
    .ri = df$.ri[1],
    .ci = df$.ci[1],
    TMAX_h  = TMAX,
    CMAX_ng_per_ml  = CMAX,
    LAMZHL_h= LAMZHL,
    AUC_0_8h_ng_per_ml = AUC_0_8h,
    AUC_0_24h_ng_per_ml = AUC_0_24h)
  
  return(result_df)
}

ctx = tercenCtx()

if (!ctx$hasXAxis)
  stop("An x-axis factor is required.")
# 
dose=as.double(ctx$op.value('dose'))
R2ADJ=as.double(ctx$op.value('R2ADJ'))

ctx %>%
  select(.ci, .ri, .y, .x) %>%
  group_by(.ci, .ri) %>%
  do(do.nca(., dose, R2ADJ)) %>%
  ctx$addNamespace() %>%
  ctx$save()
