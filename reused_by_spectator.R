unfiltered_ecco_data <- bind_rows(vol1, vol2, vol3, vol4, vol5, vol6, vol7, vol8)


with_clus_info <- unfiltered_ecco_data %>% group_by(clus) %>% mutate(first_date = min(publication_year)) %>% ungroup() %>% mutate(before1711 = first_date < 1711) %>%
  mutate(possible_first = first_date == publication_year)

possible_firsts <- with_clus_info %>% filter(possible_first == TRUE) %>% filter(before1711 == TRUE)
final_work_fields <- select(allData, c("id", "finalWorkField")) %>% distinct(id, .keep_all = TRUE) %>% rename(estc_id = "id") %>% filter(estc_id %in% possible_firsts$estc_id)

with_work_fields <- possible_firsts %>% left_join(final_work_fields, by="estc_id")

most_used_works <- with_work_fields %>% group_by(finalWorkField) %>% summarise(n = n())

cluster_count <- possible_firsts %>% distinct(clus) %>% nrow() 

contains_old_cluster <- c()

is_number <- function(num) {
  return (!is.na(num) & is.integer(num))   
}

for (row in 1:nrow(with_work_fields)) {
  start <- as.integer(with_work_fields[row, "startSpec"])
  end  <- as.integer(with_work_fields[row, "endSpec"])
  
  if(!is_number(start) | !is_number(start)) {
    if(is_number(start)) {
      end <- start
      
    }
    
    else if(is_number(end)) {
      start <- end
    }
    
    else {
      next;
    }
  }
  
  
  
  contains_old_cluster <- c(contains_old_cluster, seq.int(start, end))
}

contains_old_cluster <- unique(contains_old_cluster)

most_reused_with_old_cluster_info <- noPureNumbersFiltered %>% mutate(contains_old = number %in% contains_old_cluster)

most_averages <- most_reused_with_old_cluster_info %>% group_by(contains_old) %>% dplyr::summarise(n = n(), avg = mean(number_hits), med = median(number_hits))

most_cluster_plot <- ggplot(most_reused_with_old_cluster_info, (aes(x = number, y = number_hits, fill=contains_old))) + geom_col()
print(most_cluster_plot)
