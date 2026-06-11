setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# bal_harg_85 <- subset(bal_harg_85, select=-c(1))
# pet_harg_85 <- subset(pet_harg_85, select=-c(1))

listofindicators <- list(fracsnow_85, 
                     # datemaxswe_85,
                     bal_harg_85,
                     gslen_85,
                     tmax_mx_85,
                     maximumswe_85,
                     pet_harg_85,
                     pr_max_85,
                     scdann_85,
                     smelt_jjapr_85,
                     smrann_85,
                     # smfirstdate_85,
                     tmax_fa_85,
                     tmax_gs_85,
                     tmax_jul_85,
                     tmax_jun_85,
                     tmax_win_85,
                     tmean_ann_85,
                     tmin_fa_85,
                     tmin_gs_85,
                     tmin_jul_85,
                     tmin_jun_85,
                     tmin_win_85
                     )

names(listofindicators) <- c("Fraction of Precip as Snow RCP 8.5", 
                             # "Date of Maximum SWE RCP 8.5",
                             "Annual Water Balance RCP 8.5",
                             "Frost Free Season Length RCP 8.5",
                             "Hottest Max Temperature RCP 8.5",
                             "Maximum SWE",
                             "PET",
                             "Maximum One Day Precip",
                             "Snow Cover Duration",
                             "Snowmelt:JJA precip Ratio",
                             "Average Snowmelt Rate (mm/d)",
                             # "Snowmelt Start Date",
                             "Max Temp SON",
                             "Frost-free Maximum Temperature",
                             "July Maximum Temperature",
                             "June Maximum Temperature",
                             "Winter Maximum Temperature",
                             "Mean Annual Temperature",
                             "Fall Minimum Temperature",
                             "Frost-free season Minimum Temperature",
                             "July Minimum Temperature",
                             "June Minimum Temperature",
                             "Minimum DJF Temperature"
                             )


listofindicators <- lapply(listofindicators,function(i){
  colnames(i)[3] <- "indicator"; return(i)
  # i$period <- cut(i$year, breaks = c(1976,2005,2040,2070,2099), 
    # labels = c('1976-2005','2005-2040','2040-2069','2070-2099'), right = TRUE); return(i)
})

library(magrittr)
library(purrr)

for (i in (1:21)){
listofindicators[[i]]$period <- cut(listofindicators[[i]]$year, breaks = c(1976,2005,2040,2070,2099), 
               labels = c('1976-2005','2005-2040','2040-2069','2070-2099'), right = TRUE)
}

df <- data.frame() 

indicatorperiodmeans <-lapply(listofindicators,function(i){
  i %>%
    group_by(period) %>%
    summarise(mean = mean(indicator))
})

indicatorperiodmeans <-lapply(indicatorperiodmeans,function(i){
  i <- i[-c(2,5),] 
})

a <- map_dfr(indicatorperiodmeans, bind_rows, .id = "tib") %>%
  pivot_wider(names_from = period, values_from = mean)
write.csv(a,"Indicators.csv")



listofDATEindicators <- list( 
                         datemaxswe_85,
                         smfirstdate_85

)
names(listofDATEindicators) <- c(
                             "Date of Maximum SWE RCP 8.5",
                             "Snowmelt Start Date"
)

DATEindicatorperiodmeans <- lapply(listofDATEindicators,function(i){
  i$period <- cut(i$year, breaks = c(1976,2005,2040,2070,2099), 
                  labels = c('1976-2005','2005-2040','2040-2069','2070-2099'), right = TRUE)
  i$indicator <- i[,3]
  i %>%
    group_by(period) %>%
    summarise(mean = mean(indicator))
})
names(DATEindicatorperiodmeans) <- names(listofDATEindicators)

DATEindicatorperiodmeans <-lapply(DATEindicatorperiodmeans,function(i){
  i <- i[-c(2,5),] 
})

b <- map_dfr(DATEindicatorperiodmeans, bind_rows, .id = "tib") %>%
  pivot_wider(names_from = period, values_from = mean)

write.csv(b,"DateIndicators.csv")
