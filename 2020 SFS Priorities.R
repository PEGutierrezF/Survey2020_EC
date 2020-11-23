




##################
### SFS's priorities supporting ECRs###
##################


priorities_2020<-read.csv("data/prioties_full_2020.csv", header=TRUE)
head(priorities_2020)


#1
priorities_2020$Providing.conference.travel.grants = factor(priorities_2020$Providing.conference.travel.grants,
                                                            levels = c("1","2", "3","4", "5","6", "7"),
                                                            ordered = TRUE, exclude="NA")
#2
priorities_2020$Providing.competitive.research.awards = factor(priorities_2020$Providing.competitive.research.awards,
                                                               levels = c("1","2", "3","4", "5","6", "7"),
                                                               ordered = TRUE, exclude="NA")
#3
priorities_2020$Hosting.workshops = factor(priorities_2020$Hosting.workshops,
                                           levels = c("1","2", "3","4", "5","6", "7"),
                                           ordered = TRUE, exclude="NA")

#4
priorities_2020$Providing.networking.opportunities = factor(priorities_2020$Providing.networking.opportunities,
                                                            levels = c("1","2", "3","4", "5","6", "7"),
                                                            ordered = TRUE, exclude="NA")

#5
priorities_2020$Hosting.ECD.conference.events = factor(priorities_2020$Hosting.ECD.conference.events,
                                                       levels = c("1","2", "3","4", "5","6", "7"),
                                                       ordered = TRUE, exclude="NA")

#6
priorities_2020$Promoting.diversity..equity..and.inclusion = factor(priorities_2020$Promoting.diversity..equity..and.inclusion,
                                                                    levels = c("1","2", "3","4", "5","6", "7"),
                                                                    ordered = TRUE, exclude="NA")

#7
priorities_2020$Curating.web.resources = factor(priorities_2020$Curating.web.resources,
                                                levels = c("1","2", "3","4", "5","6", "7"),
                                                ordered = TRUE, exclude="NA")

priorities_2020 <- rename(priorities_2020, c(Providing.conference.travel.grants = "Providing conference travel grants", 
                                             Providing.competitive.research.awards= "Providing competitive research awards",
                                             Hosting.workshops= "Hosting workshops",
                                             Providing.networking.opportunities= "Providing networking opportunities",
                                             Hosting.ECD.conference.events= "Hosting ECD conference events",
                                             Promoting.diversity..equity..and.inclusion= "Promoting diversity, equity and inclusion",
                                             Curating.web.resources= "Curating web resources"))




priorities_2020_plot = likert(priorities_2020)
print(priorities_2020_plot)


p<- as.data.frame(priorities_2020_plot$results)
pr<-melt(p)
pr

mycolors <- c( '#d0d1e6','#a6bddb','#74a9cf','#3690c0','#0570b0','#034e7b','#023858')

labels <- c("Curating web resources",
            "Hosting ECD conference events","Hosting workshops",
            "Providing competitive research awards",
            "Promoting diversity, equity and inclusion",
            "Providing conference travel grants",
            "Providing networking opportunities")

priority = pr %>% 
  ungroup() %>%
  arrange(fct_relevel(variable, "7"), value) %>%
  mutate(Item = fct_inorder(Item))
priority



pt <- ggplot(data = priority, aes(x =Item, y = value, fill = variable)) +
  labs(y="Percentage", x = "",fill="Response") +
  geom_col(width = 0.7, position = position_stack(reverse = F)) +
  # geom_bar(stat="identity", width = 0.7) +
  scale_fill_manual (values=mycolors) +
  scale_x_discrete(labels = labels) +
  geom_text(aes(label = round(value, digits = 0)), position = position_stack(vjust = 0.5,reverse = F), 
            size = 5, colour = "gray15") +
  coord_flip() +
  theme_bw() +
  theme(text = element_text(size = rel(4), colour = "black"), # x-label
        axis.text.y = element_text(size = rel(4), colour = "black"),
        axis.text.x = element_text(size = rel(4), colour = "black")) + 
  theme(legend.text = element_text(size = rel(3))) + #legend size
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position="bottom") +
  guides(fill = guide_legend(reverse=TRUE))

pt
pt + ggsave("Priorities.jpeg", width = 25, height = 22, units = "cm")  


