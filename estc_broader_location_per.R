draw4 <- function(data, title) {
  
  data <- broader_location(data) %>%
    filter(publication_decade > 1700) %>%
    filter(publication_decade <  1800) %>%
    distinct(id, .keep_all = TRUE) %>%
    group_by(broader_location, publication_decade) %>%
    dplyr::summarise(n = n()) %>%
    arrange(n)
  
  return(ggplot(data = data, aes(fill=broader_location, y = n, x = publication_decade)) +
           geom_bar(position="fill", stat="identity") +
           scale_fill_manual(values = palette)+
           theme(text = element_text(size = 30), legend.title=element_blank()) + 
           scale_x_continuous(name="Decade", breaks=seq(1710,1790,20)) +
           scale_y_continuous(name="Percentage of publishing", labels = scales::percent))
  
}

draw4greyscale <- function(data, title) {
  
  data <- broader_location(data) %>%
    filter(publication_decade > 1700) %>%
    filter(publication_decade <  1800) %>%
    distinct(id, .keep_all = TRUE) %>%
    filter(broader_location %in% c("Ireland", "London", "Provincial", "Scotland")) %>%
    group_by(broader_location, publication_decade) %>%
    dplyr::summarise(n = n()) %>%
    arrange(n)
  
  data$broader_location <- factor(data$broader_location, levels=c("London", "Ireland", "Scotland", "Provincial"), ordered = TRUE)
  
  return(ggplot(data = data, aes(fill=broader_location, y = n, x = publication_decade)) +
           geom_bar(position=position_dodge2(width = 0.9, preserve = "single"), stat="identity") +
           theme(text = element_text(size = 30), legend.title=element_blank()) + scale_fill_grey(start = 0, end = .9) + theme_bw() + 
           scale_x_continuous(name="Decade", breaks=seq(1710,1790,20)) +
           scale_y_continuous(name="Entries published"))
  
}


final4 <- draw4(spectator)

file <- paste("C:/Users/mikko/OneDrive/Desktop/Gradu/cleaned_thesis_code/graphs/used/fig4_broader_location.png", sep="")
png(file=file,
    width=1200, height=700)

print(final4)

dev.off()

final4greyscale <- draw4greyscale(no_added_bernard)

file <- paste("C:/Users/mikko/OneDrive/Desktop/Gradu/cleaned_thesis_code/graphs/used/article_fig4_broader_location.png", sep="")
png(file=file,
    width=1200, height=700)

print(final4greyscale)

dev.off()

spectator_only_distinct <- no_added_bernard %>% distinct(id, .keep_all = TRUE)

provincal_locations <- no_added_bernard %>%
  filter(country_752 == "England") %>%
  filter(publication_decade > 1700) %>%
  filter(publication_decade <  1800) %>%
  distinct(id, .keep_all = TRUE) %>%
  group_by(publication_place_752, publication_decade) %>%
  dplyr::summarise(n = n()) %>%
  arrange(n)

provincal_locations_ecco <- allData %>% distinct(id, .keep_all = TRUE) %>% 
  rename(estc_id = id) %>% dplyr::right_join(unique_spectatorReuse, by="estc_id") %>%
  distinct(estc_id, .keep_all = TRUE) %>%
  filter(country_752 == "England") %>%
  filter(publication_place_752 != "London") %>%
  filter(publication_decade > 1700) %>%
  filter(publication_decade <  1800) %>% group_by(publication_decade) %>% dplyr::summarise(n = n())

provincal_distinct_locations_ecco <- allData %>% distinct(id, .keep_all = TRUE) %>% 
  rename(estc_id = id) %>% dplyr::right_join(unique_spectatorReuse, by="estc_id") %>%
  distinct(estc_id, .keep_all = TRUE) %>%
  filter(country_752 == "England") %>%
  filter(publication_place_752 != "London") %>%
  filter(publication_decade > 1700) %>%
  filter(publication_decade <  1800) %>% group_by(publication_decade) %>% distinct(publication_place_752) %>% dplyr::summarise(n = n())


provincal_locations_all <- allData %>% distinct(id, .keep_all = TRUE) %>% 
  filter(country_752 == "England") %>%
  filter(publication_place_752 != "London") %>%
  filter(publication_decade > 1700) %>%
  filter(publication_decade <  1800) %>% group_by(publication_decade) %>% dplyr::summarise(n = n())

provincal_distinct_locations_all <- allData %>% distinct(id, .keep_all = TRUE) %>% 
  filter(country_752 == "England") %>%
  filter(publication_place_752 != "London") %>%
  filter(publication_decade > 1700) %>%
  filter(publication_decade <  1800) %>% group_by(publication_decade) %>% distinct(publication_place_752) %>% dplyr::summarise(n = n())
