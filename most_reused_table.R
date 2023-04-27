most_reused_table_data <- merge(data.frame(number = c(1:635)), noPureNumbersFiltered, by = "number") %>%
  arrange(desc(number_hits)) %>% head(10)

