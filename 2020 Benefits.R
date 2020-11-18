




benefits_2020<-read.csv("data/benefits_full_2020.csv", header=TRUE)
head(benefits_2020)

#1
benefits_2020$Networking = factor(benefits_2020$Networking,
                                  levels = c("Don't know/Unsure", "Not important", "Somewhat important", "Important", 
                                             "Very important", "Extremely important"),ordered = TRUE,exclude="NA")

#2
benefits_2020$International.travel.and.experience = factor(benefits_2020$International.travel.and.experience,
                                                           levels = c("Don't know/Unsure", "Not important", "Somewhat important", "Important", 
                                                                      "Very important", "Extremely important"),ordered = TRUE,exclude="NA")
#3
benefits_2020$Workshops = factor(benefits_2020$Workshops,
                                 levels = c("Don't know/Unsure", "Not important", "Somewhat important", "Important", 
                                            "Very important", "Extremely important"),ordered = TRUE,exclude="NA")
#4
benefits_2020$Field.excursions.during.conferences = factor(benefits_2020$Field.excursions.during.conferences,
                                                           levels = c("Don't know/Unsure", "Not important", "Somewhat important", "Important", 
                                                                      "Very important", "Extremely important"),ordered = TRUE,exclude="NA")

#5
benefits_2020$Special.sessions = factor(benefits_2020$Special.sessions,
                                        levels = c("Don't know/Unsure", "Not important", "Somewhat important", "Important", 
                                                   "Very important", "Extremely important"),ordered = TRUE,exclude="NA")
#6
benefits_2020$Service.opportunities = factor(benefits_2020$Service.opportunities,
                                             levels = c("Don't know/Unsure", "Not important", "Somewhat important", "Important", 
                                                        "Very important", "Extremely important"),ordered = TRUE,exclude="NA")
#7
benefits_2020$Opportunity.to.present.your.science = factor(benefits_2020$Opportunity.to.present.your.science,
                                                           levels = c("Don't know/Unsure", "Not important", "Somewhat important", "Important", 
                                                                      "Very important", "Extremely important"),ordered = TRUE,exclude="NA")
#8
benefits_2020$Presentations.on.science.in.your.field = factor(benefits_2020$Presentations.on.science.in.your.field,
                                                              levels = c("Don't know/Unsure", "Not important", "Somewhat important", "Important", 
                                                                         "Very important", "Extremely important"),ordered = TRUE,exclude="NA")
#9
benefits_2020$  Providing.travel.grants.to.attend.meetings = factor(benefits_2020$  Providing.travel.grants.to.attend.meetings,
                                                                    levels = c("Don't know/Unsure", "Not important", "Somewhat important", "Important", 
                                                                               "Very important", "Extremely important"),ordered = TRUE,exclude="NA")
#10
benefits_2020$Providing.competitive.research.and.recognition.awards = factor(benefits_2020$Providing.competitive.research.and.recognition.awards,
                                                                             levels = c("Don't know/Unsure", "Not important", "Somewhat important", "Important", 
                                                                                        "Very important", "Extremely important"),ordered = TRUE,exclude="NA")
#11
benefits_2020$FWS.article.access = factor(benefits_2020$FWS.article.access,
                                          levels = c("Don't know/Unsure", "Not important", "Somewhat important", "Important", 
                                                     "Very important", "Extremely important"),ordered = TRUE,exclude="NA")
#12
benefits_2020$Web.resources = factor(benefits_2020$Web.resources,
                                     levels = c("Don't know/Unsure", "Not important", "Somewhat important", "Important", 
                                                "Very important", "Extremely important"),ordered = TRUE,exclude="NA")


benefits_2020 <- rename(benefits_2020, c(Networking = "Networking", 
                                         International.travel.and.experience="International travel and experience",
                                         Workshops = "Workshops", Field.excursions.during.conferences="Field excursions during conferences",
                                         Special.sessions = "Special sessions", Service.opportunities = "Service opportunities", 
                                         Opportunity.to.present.your.science = "Opportunity to present your research", 
                                         Presentations.on.science.in.your.field = "Presentations on science in your field",
                                         Providing.travel.grants.to.attend.meetings = "Travel grants", 
                                         Providing.competitive.research.and.recognition.awards = "Providing competitive research / recognition awards", 
                                         FWS.article.access="FWS article access", Web.resources = "Web resources"))

benefits_2020_full = likert(benefits_2020)

summary(benefits_2020_full)
print(benefits_2020_full)


b <- as.data.frame(benefits_2020_full$results)
benefits<-melt(b)

benefits20 = benefits %>% 
  ungroup() %>%
  arrange(fct_relevel(variable, "Extremely important"), value) %>%
  mutate(Item = fct_inorder(Item))
benefits20


mycolors <- c( '#d0d1e6','#a6bddb','#74a9cf','#3690c0','#0570b0','#034e7b','#023858')
labels <- c("Field excursions during conferences","Service opportunities",
  "International travel and experience","Web resources",
  "Special sessions","Workshops", "FWS article access",
  "Providing competitive research \n recognition awards",
  "Travel grants","Presentations on science in your field",
  "Opportunity to present your research", "Networking")
  

benefits2020 <- ggplot(data = benefits20, aes(x =Item, y = value, fill = variable, order= value)) +
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

benefits2020
benefits2020 + ggsave("Attending.jpeg", width = 25, height = 22, units = "cm")  

