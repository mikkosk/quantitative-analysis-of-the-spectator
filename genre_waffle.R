
genre_data <- read.csv("C:/Users/mikko/OneDrive/Desktop/Gradu/genres_with_predicted.csv", stringsAsFactors = FALSE, sep = ",")


genre_for_reuses <- spectatorReuseFiltered %>% left_join(final_work_fields, by="estc_id") %>%
  rename(work_id = finalWorkField) %>%
  left_join(genre_data, by="work_id") %>%
  filter(!is.na(work_id) & !is.na(main_category))

all_genre <- genre_for_reuses %>% group_by(main_category) %>% summarise(count = n(), sum = sum(length), unique = length(unique(estc_id)))


## Go through numbers and calculate genres
## Not a great way to do this so takes a moment
numbers_most_genre_list <- list()
for(i in 1:635) {
  print(i)
  filtered_data <- genre_for_reuses %>% filter(startSpec <= i & endSpec >= i)
  most_popular <- filtered_data %>% count(main_category, sort = TRUE)
  numbers_most_genre_list[[i]] <- most_popular[1,1]
}

waffle_data <- data.frame(number = 1:635, category = unlist(numbers_most_genre_list))

  
genre_waffle_fig <- ggplot(waffle_data, aes(x = ((number-1) %% 20), y = 20 -(number-1)%/%20, fill = category)) + 
  geom_tile(color = "black") + scale_fill_manual(values=c("aliceblue", "yellow", "cyan", "darkolivegreen2", "darkorange", "deepskyblue", "brown1", "blueviolet", "blue", "red", "darkseagreen1"))

file <- paste("C:/Users/mikko/OneDrive/Desktop/Gradu/cleaned_thesis_code/graphs/used/genre_waffle.png", sep="")
png(file=file,
    width=1200, height=700)
print(genre_waffle_fig)
dev.off()
