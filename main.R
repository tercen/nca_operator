library(tercen)
library(dplyr)
library(NonCompart)

do.nca = function(df) {
  ci <- df$.ci[1]
  ri <- df$.ri[1]
  col_annot <- ctx$cselect()
  dose <- col_annot[[1]][ci+1]
  
  iAUC = data.frame(Name=c("AUC[0-4h]","AUC[0-8h]","AUC[0-12h]","AUC[4h-24h]","AUC[0-24h]"), Start=c(0,0,0,4,0), End=c(4,8,12,24,24))
  
  pk <-pkr::sNCA(df$.x,(df$.y), dose = dose, concUnit="mg/L",iAUC=iAUC, adm = "Extravascular", down = "Linear", returnNA = TRUE )
  
  TMAX <- round(pk["TMAX"])
  CMAX <- round(pk["CMAX"])
  LAMZHL <- round(pk["LAMZHL"])
  AUC_0_4h <- round(pk["AUC[0-4h]"])
  AUC_0_8h <- round(pk["AUC[0-8h]"])
  AUC_0_12h <- round(pk["AUC[0-12h]"])
  AUC_4_24h <- round(pk["AUC[4-24h]"])
  AUC_0_24h <-round(pk["AUC[0-24h]"])
  
  result_df <-  data.frame(
    .ri = df$.ri[1],
    .ci = df$.ci[1],
    TMAX_h             = TMAX,
    CMAX_ng_per_ml     = CMAX,
    LAMZHL_h           = LAMZHL,
    AUC_0_4h_ng_per_ml = AUC_0_4h,
    AUC_0_8h_ng_per_ml = AUC_0_8h,
    AUC_0_12h_ng_per_ml = AUC_0_12h,
    AUC_4_24h_ng_per_ml = AUC_4_24h,
    AUC_0_24h_ng_per_ml = AUC_0_24h)
  return(result_df)
}

ctx = tercenCtx()

if (!ctx$hasXAxis)
  stop("An x-axis factor is required.")

if (ncol(ctx$cselect()) < 1) stop ("Require dose as a first factor in column zone")
dose <- ctx$cselect()[[1]]

check_dose_in_range <- (all(unlist(lapply(dose, function(x) x %in% 1:1000))))

if (!check_dose_in_range)  stop ("dose need to be in range from 1mg to 1000mg")

ctx %>%
  select(.ci, .ri, .y, .x) %>%
  group_by(.ci, .ri) %>%
  do(do.nca(., R2ADJ)) %>%
  ctx$addNamespace() %>%
  ctx$save()
