



# Activity ----------------------------------------------------------------
a <- read.csv("activities_in_EC.csv")
a


activity<-melt(a)
head(activity)

activity1 = activity %>% 
  ungroup() %>%
  arrange(variable, value) %>%
  mutate(Category = fct_inorder(Category ) )

labels <- c("Applied for SFS awards (e.g., Hynes)",
            "Others", "Accessed SFS's online ECC resources 
            (e.g., list of books, blogs, resources)", "Applied for support for ECC resources 
            from SFS (e.g., travel grants)", "Participated in the ECC planning 
            committee","Attended an ECC webinar", "I wasn't aware of one or more of the above 
            events","Attended an ECC workshop", "I wasn't aware of the above online 
            resources", "Attended an ECC mixer")


a <- ggplot(activity1, aes(x =variable, y = Category)) +
  geom_point( aes(size=value), shape=21, fill="steelblue") +
  labs(x="", y="Activity", size="Percentage") +
  scale_size_area(max_size=20) +
   scale_y_discrete(labels = labels) +
  
  theme(axis.title.y = element_text(size = 14, angle = 90)) + # axis y 
  theme(axis.title.x = element_text(size = 14, angle = 00)) + # axis x
  theme(axis.text.x=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis y
  
  #Legend   
  theme(legend.title=element_text(color = "black", size=12),
        legend.text =element_text(color = "black", size=12),
        legend.key=element_blank(), # gray background
        legend.key.size = unit(1, 'lines'),
        legend.text.align = 0.5) +  # text align left legend
  
  # Panel
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) 
a

a + ggsave("ECactivity.jpeg", width = 25, height = 22, units = "cm")  




# Society Activities ------------------------------------------------------


b<- read.csv("activities_in_Soc.csv")
b

Socactivity<-melt(b)
Socactivity
head(Socactivity)

Socactivity1 = Socactivity %>% 
  ungroup() %>%
  arrange(variable, value) %>%
  mutate(Activity = fct_inorder(Activity ) )

labels <- c("Assisted fundraising efforts","Others","Served on a conference planning committee",
  "Served as a mentor (e.g., with the Instars program)",
  "Helped organize conference events (e.g., mixers, 
  workshops, field trips, 5k, etc.)",
  "Participated in an SFS governance committee 
  (e.g., Long Range Planning, Board of Directors, 
  Diversity & Inclusivity, etc.)","Hosted a special session",
  "Volunteered at a conference (e.g., registration 
  desk, moderator, judge, etc.)", "Attended the Early Career mixer at a conference", 
  "Gave a poster at a conference",
  "Gave a talk at a conference",
  "Attended a conference")


b <- ggplot(Socactivity1, aes(x =variable, y = Activity)) +
  geom_point( aes(size=value), shape=21, fill="steelblue") +
  labs(x="", y="Activity", size="Percentage") +
  scale_size_area(max_size=20) +
 scale_y_discrete(labels = labels) +
  
  theme(axis.title.y = element_text(size = 14, angle = 90)) + # axis y 
  theme(axis.title.x = element_text(size = 14, angle = 00)) + # axis x
  theme(axis.text.x=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis y
  
  #Legend   
  theme(legend.title=element_text(color = "black", size=12),
        legend.text =element_text(color = "black", size=12),
        legend.key=element_blank(), # gray background
        legend.key.size = unit(1, 'lines'),
        legend.text.align = 0.5) +  # text align left legend
  
  # Panel
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) 
b

b + ggsave("Socactivity.jpeg", width = 25, height = 22, units = "cm")  




# Research topic ----------------------------------------------------------

category <- read.csv("focus_by_category.csv")
category

focus<-melt(category)
head(focus)


focus1 = focus %>% 
  ungroup() %>%
  arrange(variable, value) %>%
  mutate(Category  = fct_inorder(Category ) )

labels <- c("Algae", "Anthropogenic materials (litter, plastics, 
            nanoparticles, pharmaceuticals",
            "Biology and biology education 
            research", "Management", "Pollution","Fish", "Climate change",
            "Lakes", "Conservation", "Food webs", "Nutrients",
            "Biogeochemistry", "Community ecology",
            "Invertebrates", "Streams/Rivers")


b <- ggplot(focus1, aes(x =variable, y = Category)) +
  geom_point( aes(size=value), shape=21, fill="steelblue") +
  labs(x="", y="Research focus", size="Percentage") +
  scale_size_area(max_size=20) +
  scale_y_discrete(labels = labels) +
  
  theme(axis.title.y = element_text(size = 14, angle = 90)) + # axis y 
  theme(axis.title.x = element_text(size = 14, angle = 00)) + # axis x
  theme(axis.text.x=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=14, vjust=0.5, color="black")) + #subaxis y
 
#Legend   
  theme(legend.title=element_text(color = "black", size=12),
        legend.text =element_text(color = "black", size=12),
        legend.key=element_blank(), # gray background
        legend.key.size = unit(1, 'lines'),
        legend.text.align = 0.5) +  # text align left legend
  
  # Panel
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) 
b

b + ggsave("Research.jpeg", width = 25, height = 22, units = "cm")  
  
