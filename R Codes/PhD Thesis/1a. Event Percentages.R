
file <- file.choose() # load the "Additional Data" file
data <-read.csv(file)

BFC_time = 0
FFC_time = data$`Stride (ms)` / 1000
MAW_time = FFC_time + (data$`Transition (ms)` / 1000)
BR_time = MAW_time + (data$`Acceleration (ms)` / 1000)
EFT_time = data$`Total (ms)` / 1000


phase_perc <- data.frame("BFC" = BFC_time,
                         "FFC" = FFC_time,
                         "MAW" = MAW_time,
                         "BR" = BR_time,
                         "EFT" = EFT_time)

phase_perc$`File Name` <- data$`File Name`
  

phase_perc$`BFC %` = abs(phase_perc$BFC / phase_perc$EFT) * 100
phase_perc$`FFC %` = abs(phase_perc$FFC / phase_perc$EFT) * 100
phase_perc$`MAW %` = abs(phase_perc$MAW / phase_perc$EFT) * 100
phase_perc$`BR %` = abs(phase_perc$BR / phase_perc$EFT) * 100
phase_perc$`EFT %` = abs(phase_perc$EFT / phase_perc$EFT) * 100

library(dplyr)
phase_perc <- phase_perc %>%
  select(`File Name`, BFC, FFC, MAW, BR, EFT, `BFC %`, `FFC %`, `MAW %`, `BR %`, `EFT %`)


write.csv(phase_perc, file = "Event Percentages.csv", row.names = FALSE)
