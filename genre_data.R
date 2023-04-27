library(janitor)
library(reshape2)

genre_data <- read.csv("C:/Users/mikko/OneDrive/Desktop/Gradu/genres_with_predicted.csv", stringsAsFactors = FALSE, sep = ",")


genre_for_reuses <- spectatorReuseFiltered %>% left_join(final_work_fields, by="estc_id") %>%
  rename(work_id = finalWorkField) %>%
  left_join(genre_data, by="work_id") %>%
  filter(!is.na(work_id) & !is.na(main_category))

all_genre <- genre_for_reuses %>% group_by(main_category) %>% summarise(count = n(), sum = sum(length), avg_reuse = sum(length)/n(), median=median(length),unique = length(unique(estc_id)))


## Go through numbers and calculate genres
## Not a great way to do this so takes a moment
numbers_genre_list <- list()
for(i in 1:635) {
  print(i)
  filtered_data <- genre_for_reuses %>% filter(startSpec <= i & endSpec >= i)
  new_row <- filtered_data %>% group_by(main_category) %>% summarise(Num = (n()/nrow(filtered_data))) %>% t() %>% janitor::row_to_names(1)
  rowname <- paste0(i)
  rownames(new_row) <- c(rowname)
  numbers_genre_list[[i]] <- data.frame(new_row)
  
}

genre_by_numbers <- do.call(bind_rows, numbers_genre_list) %>% mutate_if(is.character,as.numeric) %>% mutate_if(is.numeric, ~replace_na(., 0))
genre_by_numbers <- cbind(rownames(genre_by_numbers), data.frame(genre_by_numbers, row.names=NULL)) %>% mutate_if(is.character,as.numeric)

genre_by_numbers_only_some <- genre_by_numbers[, c("rownames(genre_by_numbers)", "history", "literature", "religion", "education")]
  
melted_data <- reshape2::melt(genre_by_numbers_only_some, id.vars = "rownames(genre_by_numbers)", variable.name = "category",
           value.name="count") %>% rename(specNum = "rownames(genre_by_numbers)")
genre_plot <- ggplot(melted_data, aes(x=specNum, y=count, fill=category)) +
  geom_area()

print(genre_plot)

reuse_summarise <- genre_for_reuses %>% group_by(estc_id) %>% summarise(count = n())
distinct_genre_data <- genre_for_reuses %>% distinct(estc_id, .keep_all = TRUE)

all_categories <- genre_data %>% distinct(main_category)
cleaned_category_names <- c(
  "scientificimprovement"="scientific improvement", 
  "salecataloguesalmanacsdirectoriesetc"="sale catalogues, almanacs, directories and etc")
