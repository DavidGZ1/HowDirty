
# Attempt to use cut instead of merging dfs
## merging is faster
pivot_conta_tshd <- function(df_threshold){
  #pivot conta_tshd so that it can be used with cut for get_risk_level_analyte_full
  df_threshold  %>%
    mutate(Tshd_Area_TICA_perc0 = 0, Tshd_Area_TICA_perc100 = Inf) %>%
    pivot_longer(cols = starts_with("Tshd"), names_to = "Quantile", values_to = "Abundance", names_prefix = "Tshd_Area_TICA_perc") %>%
    mutate(Quantile = factor(Quantile)) %>%
    mutate(RiskLevel = fct_recode(Quantile,
                                  "0" = "0", "1" = "25", "2" = "50",
                                  "3"  = "75", "4" = "90", "5" = "100"),
           RiskLevel = fct_relevel(RiskLevel, "0", "1", "2", "3", "4", "5")) %>%
    # mutate(RiskLevel = as.numeric(RiskLevel)) %>%
    arrange(AnalyteFull, RiskLevel)

}

get_risk_level_analyte_full <- function(abundance, df_threshold, analyte_full){
  # assessing risk levels based on threshold, requires conta_tshd in long format from  pivot_conta_tshd
  if(analyte_full %in% df_threshold$AnalyteFull){
    output <- cut(abundance, breaks = c(-1,  df_threshold[df_threshold$AnalyteFull == analyte_full, ]$Abundance),
                  labels = df_threshold[df_threshold$AnalyteFull == analyte_full, ]$RiskLevel)
  }else{ #if AnalyteFull not found in df_threshold, assign a level 6 unless Abundance = 0
    output <- rep("6", length(abundance))
    output[abundance == 0] <- 0
  }
  return(output)
}

get_risk_level_df <-  function(df_conta, df_threshold){
  # assessing risk levels based on threshold for the whole df_conta
  output<-
    df_conta %>%
    rowwise() %>%
    mutate(RiskLevel = get_risk_level_analyte_full(Abundance, analyte_full = as.character(AnalyteFull), df_threshold = df_threshold)) %>%
    ungroup()
  return(output)
}


## pivot
ref_conta_tshd_long <-  ref_conta_tshd %>% pivot_conta_tshd()



start_time <- Sys.time()
conta <- get_risk_level_df(conta_annotated, ref_conta_tshd_long)
end_time <- Sys.time()
end_time - start_time



start_time <- Sys.time()
conta2 <- annotate_conta_thresholds(conta_annotated, ref_conta_tshd)
end_time <- Sys.time()
end_time - start_time
