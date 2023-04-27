### Run genre data file first

old_clusters <- spectatorReuse %>% distinct(estc_id, .keep_all = TRUE) %>% group_by(clus) %>% filter(any(publication_year< 1711)) %>% ungroup()
old_with_genre <- old_clusters  %>% left_join(final_work_fields, by="estc_id") %>%
  rename(work_id = finalWorkField) %>%
  left_join(genre_data, by="work_id") %>%
  filter(!is.na(work_id) & !is.na(main_category))

grouped_cluster_genre <- old_with_genre %>% mutate(is_old = publication_year < 1711) %>% 
  group_by(main_category, is_old) %>% summarise(count = n()) %>% ungroup()

old_clusters_percentages <- grouped_cluster_genre %>%
  filter(is_old == TRUE)

old_clusters_percentages <- old_clusters_percentages %>% mutate(percentage = count / sum(old_clusters_percentages$count))

new_clusters_percentages <- grouped_cluster_genre %>%
  filter(is_old == FALSE)

new_clusters_percentages <- new_clusters_percentages %>% 
  mutate(percentage = count / sum(new_clusters_percentages$count)) %>%
  filter(main_category != "salecataloguesalmanacsdirectoriesetc") %>%
  mutate(main_category = str_replace_all(main_category, cleaned_category_names))


old_clusters_pie <- ggplot(old_clusters_percentages, aes(x="", y=percentage, fill=main_category)) +
  geom_bar(stat="identity") + coord_polar("y", start=0)  + scale_fill_manual(values=c("aliceblue", "yellow", "cyan", "darkolivegreen2", "darkorange", "deepskyblue", "brown1", "blueviolet", "blue", "red", "darkseagreen1"))

new_clusters_pie <- ggplot(new_clusters_percentages, aes(x="", y=percentage, fill=main_category)) +
  geom_bar(stat="identity") + coord_polar("y", start=0)  + scale_fill_manual(values=c("aliceblue", "yellow", "cyan", "darkolivegreen2", "darkorange", "deepskyblue", "brown1", "blueviolet", "blue", "red", "darkseagreen1"))

genre_change_fig <- plot_grid(old_clusters_pie, new_clusters_pie)


old_clusters_bar <- ggplot(old_clusters_percentages, aes(x=main_category, y=count, fill=main_category)) +
  geom_bar(stat="identity") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text = element_text(size = 20))

new_clusters_bar <- ggplot(new_clusters_percentages, aes(x=reorder(main_category, count), y=count)) +
  geom_bar(stat="identity", show.legend = FALSE, fill="#5a5a5a") +
  theme(text = element_text(size = 20)) + coord_flip() +
  scale_y_continuous(name="Count") +
  scale_x_discrete(name="Main category")



file <- paste("C:/Users/mikko/OneDrive/Desktop/Gradu/cleaned_thesis_code/graphs/used/genre_bar.png", sep="")
png(file=file,
    width=1200, height=700)
print(new_clusters_bar)
dev.off()
