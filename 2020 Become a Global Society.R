



become_global_full_2020<-read.csv("data/become_global_full_2020.csv", header=TRUE)
head(become_global_full_2020)

#1
become_global_full_2020$Hosting.SFS.annual.meetings.at.international.locations. = factor(become_global_full_2020$Hosting.SFS.annual.meetings.at.international.locations.,
                                                                                         levels = c("Don't know/Unsure", "Not important", "Somewhat important", "Important", 
                                                                                                    "Very important", "Extremely important"),ordered = TRUE,exclude="NA")

#2
become_global_full_2020$Providing.travel.awards.for.attendees.traveling.internationally. = factor(become_global_full_2020$Providing.travel.awards.for.attendees.traveling.internationally.,
                                                                                                  levels = c("Don't know/Unsure", "Not important", "Somewhat important", "Important", 
                                                                                                             "Very important", "Extremely important"),ordered = TRUE,exclude="NA")

#3
become_global_full_2020$Creating.initiatives.to.offset.the.carbon.footprint.incurred.from.international.travel.= 
  factor(become_global_full_2020$Creating.initiatives.to.offset.the.carbon.footprint.incurred.from.international.travel.,
         levels = c("Don't know/Unsure", "Not important", "Somewhat important", "Important", 
                    "Very important", "Extremely important"),ordered = TRUE,exclude="NA")    

#4
become_global_full_2020$Proving.alternative.ways.of.attending.annual.meetings.virtually.= 
  factor(become_global_full_2020$Proving.alternative.ways.of.attending.annual.meetings.virtually.,
         levels = c("Don't know/Unsure", "Not important", "Somewhat important", "Important", 
                    "Very important", "Extremely important"),ordered = TRUE,exclude="NA") 

#5
become_global_full_2020$  Providing.more.webinars.and.online.activities.throughout.the.year.that.can.be.attended.remotely.= 
  factor(become_global_full_2020$  Providing.more.webinars.and.online.activities.throughout.the.year.that.can.be.attended.remotely.,
         levels = c("Don't know/Unsure", "Not important", "Somewhat important", "Important", 
                    "Very important", "Extremely important"),ordered = TRUE,exclude="NA") 


become_global_full_20201 <- rename(become_global_full_2020, c(
  Hosting.SFS.annual.meetings.at.international.locations.=  "Hosting SFS annual meetings at international locations",
  Providing.travel.awards.for.attendees.traveling.internationally.=  "Providing travel awards for attendees traveling internationally",
  Creating.initiatives.to.offset.the.carbon.footprint.incurred.from.international.travel.=   "Creating initiatives to offset the carbon footprint incurred from international travel",
  Proving.alternative.ways.of.attending.annual.meetings.virtually. =  "Proving alternative ways of attending annual meetings virtually",
  Providing.more.webinars.and.online.activities.throughout.the.year.that.can.be.attended.remotely. =  "Providing more webinars and online activities throughout the year that can be attended remotely"))



become_global_full_20201 = likert(become_global_full_2020)
print(become_global_full_20201)

become_global <- as.data.frame(become_global_full_20201$results)
become_global


Global_Soc<-melt(become_global)
Global_Soc

mycolors <- c( '#d0d1e6','#a6bddb','#74a9cf','#3690c0','#0570b0','#034e7b','#023858')

labels <- c("Hosting SFS annual meetings at\n international locations",
            "Creating initiatives to offset the carbon footprint\n incurred from international travel",
            "Providing travel awards for attendees\n traveling internationally",
            "Providing alternative ways of attending\n annual meetings virtually",
            "Providing more webinars and online activities\n throughout the year that can be attended\n remotely")

Global_Society = Global_Soc %>% 
  ungroup() %>%
  arrange(fct_relevel(variable, "Extremely important"), value) %>%
  mutate(Item = fct_inorder(Item))


GS <- ggplot(data = Global_Society, aes(x =Item, y = value, fill = variable, order= value)) +
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

GS
GS + ggsave("Global Society.jpeg", width = 25, height = 22, units = "cm")  



