pureShortComp<- merge(data.frame(number = c(1:635)), noPureNumbersFiltered, by = "number") %>%
  rename("noPure" = number_hits) %>%
  merge(under2500NumbersFiltered, by = "number") %>%
  rename("under2500" = number_hits)

top50data <- pureShortComp %>%
  arrange(desc(noPure)) %>%
  mutate(posNo = row_number()) %>%
  mutate(posNo = ifelse(noPure == 0, 0, posNo)) %>%
  mutate(top50 = ifelse((row_number() < 50), TRUE, FALSE)) %>%
  arrange(desc(under2500)) %>%
  mutate(posUnd = row_number()) %>%
  mutate(posUnd = ifelse(under2500 == 0, 635, posUnd)) %>%
  mutate(top50 = ifelse((row_number() < 50 | top50 == TRUE), TRUE, FALSE)) %>%
  mutate(difference = abs(posNo - posUnd))

checkPosBoth <- function (no, und) {
  print(no)
  print(und)
  if(no < 51 & und < 51) {
    return("Top 50 both")
  }
  if(no < 51 & und > 50) {
    return("Top 50 only on data with no length restrictions")
  }
  if(no > 50 & und < 51) {
    return("Top 50 only on data with under 2500 character reuse")
  }
  return("Not in top 50 in either data")
}

numberHitDistribution <- top50data %>% arrange(desc(noPure)) %>%
  mutate(row = row_number()) %>%
  rowwise() %>%
  mutate(top50 = checkPosBoth(posNo, posUnd))

colors <- c("Top 50 both" = "#00308F",
  "Top 50 only on data with no length restrictions" = "#187DE9", 
  "Top 50 only on data with under 2500 character reuse" = "#BBEEFF",
  "Not in top 50 in either data" = "#555555")



distributionFig <- ggplot(numberHitDistribution) +
  geom_col(aes(x=row, y=noPure, fill=top50), width = 1) +
  theme(text = element_text(size = 30)) + 
  scale_x_continuous(name="Numbers of the Spectator in order of reuse", limits=c(0, 635), breaks=seq(0, 635, 100)) +
  scale_y_continuous(name="Number of reuse cases (Data with pure editions filtered)") +
  scale_fill_manual(values = colors) +
  theme(legend.title=element_blank(), legend.position="bottom") + guides(fill=guide_legend(nrow=4,byrow=TRUE))

file <- paste("C:/Users/mikko/OneDrive/Ty�p�yt�/Gradu/cleaned_thesis_code/graphs/used/fig6_distribution.png", sep="")
png(file=file,
    width=1200, height=700)
print(distributionFig)
dev.off()


distributionOrderByNumberFig <- ggplot(numberHitDistribution) +
  geom_col(aes(x=number, y=noPure, fill=top50), width = 1) +
  theme(text = element_text(size = 30)) + 
  scale_x_continuous(name="Numbers of the Spectator in order of reuse", limits=c(0, 635), breaks=seq(0, 635, 100)) +
  scale_y_continuous(name="Number of reuse cases (Data with pure editions filtered)") +
  scale_fill_manual(values = colors) +
  theme(legend.title=element_blank(), legend.position="bottom") + guides(fill=guide_legend(nrow=4,byrow=TRUE))

file <- paste("C:/Users/mikko/OneDrive/Desktop/Gradu/cleaned_thesis_code/graphs/unused/distribution_number_order.png", sep="")
png(file=file,
    width=1200, height=700)
print(distributionOrderByNumberFig)
dev.off()

file <- paste("C:/Users/mikko/OneDrive/Desktop/Gradu/cleaned_thesis_code/csv/spectator_distiribution.csv", sep="")
write.csv(numberHitDistribution,file, row.names=FALSE)


simple_distribution_fig <- ggplot(pureShortComp) + geom_col(aes(x=number, y=noPure)) +
  scale_x_continuous(name = "Issue number") +
  scale_y_continuous(name = "Number of reuse cases")
print(simple_distribution_fig)

file <- paste("C:/Users/mikko/OneDrive/Desktop/Gradu/article_code/graphs/used/article_fig3_simple_distribution.tiff", sep="")
tiff(file=file,
    width=2400, height=1400, res=300)
print(simple_distribution_fig)
dev.off()

histogram_distribution_fig <- pureShortComp %>% ggplot() + geom_histogram(aes(noPure), binwidth=20) +
  scale_x_continuous(name = "Number of reuse cases") +
  scale_y_continuous(name = "Count")
print(histogram_distribution_fig)

file <- paste("C:/Users/mikko/OneDrive/Desktop/Gradu/article_code/graphs/used/article_fig4_histogram.tiff", sep="")
tiff(file=file,
     width=2400, height=1400, res=300)
print(histogram_distribution_fig)
dev.off()
