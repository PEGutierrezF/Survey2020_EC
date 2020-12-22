

category <- c('Assistant Professor','Associate or Full Professor',
            'Postdoc ', 'Consulting', 'State or provincial agency', 'Federal agency',
            'NGO','Others')

responses <- c(26.8,18.2,14,8.6,6,6,6,15.8)

perfil <- data.frame(category, responses)
perfil

perfil$category <- factor(perfil$category, levels = perfil$category)

p1 <- ggplot(data=perfil, aes(x=reorder(category, responses), y=responses)) +
  labs(x= "Category", y = "Responses (%)") +
  geom_bar(stat="identity",fill="steelblue") + 
  coord_flip() +
  
  theme(axis.title.y = element_text(size = 14, angle = 90)) + # axis y 
  theme(axis.title.x = element_text(size = 14, angle = 00)) + # axis x
  theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=12, vjust=0.5, color="black")) + #subaxis y
  
  # Panel
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  
  scale_x_discrete(labels=c('State or\n provincial agency', 'Federal agency',
                            'NGO','Consulting','Postdoc*','Others',
                            'Associate or\n Full Professor*','Assistant Professor*'))
p1


# Optional ----------------------------------------------------------------


category <- c('Academia', 'Consulting', 'State or provincial agency', 'Federal agency',
              'NGO','Unemployed')

responses <- c(73,8.6,6.1,6.1,6.1,1.2)

perfil <- data.frame(category, responses)
perfil

perfil$category <- factor(perfil$category, levels = perfil$category)

a1 <- ggplot(data=perfil, aes(x=reorder(category, responses), y=responses)) +
  labs(x= "Category", y = "Responses (%)") +
  geom_bar(stat="identity",fill="steelblue") + 
  coord_flip() +
  
  theme(axis.title.y = element_text(size = 14, angle = 90)) + # axis y 
  theme(axis.title.x = element_text(size = 14, angle = 00)) + # axis x
  theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=12, vjust=0.5, color="black")) + #subaxis y
  
  # Panel
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  
  scale_x_discrete(labels=c('Unemployed','State or\n provincial\n agency', 'Federal\n agency',
                            'NGO','Consulting','Academia'))
a1
a1 + ggsave("work.jpeg", width = 20, height = 20, units = "cm") 

# Academy -----------------------------------------------------------------

category1 <- c('Assistant Professor','Associate or Full Professor',
              'Postdoc ', 'Lecurer', 'Research', 'Others')

responses1 <- c(36.6,25,20,6.6,6.6,5)

perfil1 <- data.frame(category1, responses1)
perfil1

perfil1$category1 <- factor(perfil1$category1, levels = perfil1$category1)

a2 <- ggplot(data=perfil1, aes(x=reorder(category1, responses1), y=responses1)) +
  labs(x= "", y = "Responses (%)") +
  geom_bar(stat="identity",fill="steelblue") + 
  coord_flip() +
  
  theme(axis.title.y = element_text(size = 14, angle = 90)) + # axis y 
  theme(axis.title.x = element_text(size = 14, angle = 00)) + # axis x
  theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=12, vjust=0.5, color="black")) + #subaxis y
  
  # Panel
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  
  scale_x_discrete(labels=c('Others', 'Researcher', 'Lecturer','Postdoc',
                            'Associate or\n Full Professor','Assistant\n Professor'))
a2
a2 + ggsave("academy.jpeg", width = 20, height = 20, units = "cm") 

# Degree ------------------------------------------------------------------

degree <- c('PhD','MSc','BSc')
number <- c(70,20,10)

degree<- data.frame(degree, number)
degree


p2 <- ggplot(data=degree, aes(x=reorder(degree, -number), y=number)) +
  labs(x= "Highest level of education", y = "% of respondents") +
  geom_bar(stat="identity", fill="steelblue") + 

  
  theme(axis.title.y = element_text(size = 14, angle = 90)) + # axis y 
  theme(axis.title.x = element_text(size = 14, angle = 00)) + # axis x
  theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=12, vjust=0.5, color="black")) + #subaxis y
  
  # Panel
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))
p2  
p2 + ggsave("degree.jpeg", width = 20, height = 20, units = "cm") 
 
Fig1a <- (a1 + a2) / (p2 +plot_spacer())
Fig1a + plot_annotation(tag_levels = 'A')
Fig1a + ggsave("Figure_1a.jpeg", width = 20, height = 20, units = "cm") 


# Continually attending SFS meetings ---------------------------------------

continually <- c('Yes','Not','Not applicable (not yet graduated)')
number <- c(51,29,20)

continually <- data.frame(continually, number)
continually

p3 <- ggplot(data=continually, aes(x=reorder(continually,-number), y=number)) +
  labs(x= "Continuously member of SFS", y = "% of respondents") +
  geom_bar(stat="identity",fill="steelblue") + 
  
  
  theme(axis.title.y = element_text(size = 14, angle = 90)) + # axis y 
  theme(axis.title.x = element_text(size = 14, angle = 00)) + # axis x
  theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=12, vjust=0.5, color="black")) + #subaxis y
  
  # Panel
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  
  scale_x_discrete(labels=c('Yes',
                            'No',
                            'Not applicable\n (not yet\n graduated)'))

p3
p3 + ggsave("participation.jpeg", width = 20, height = 20, units = "cm") 

# Why no continually  ---------------------------------------

continually <- c('Did not have funds for membership fee',
                 'Did not plan to attend the annual meeting',
                 'Other reasons')

number <- c(55,21,24)

continually <- data.frame(continually, number)
continually

levels(continually$continually) <- gsub(" ", "\n", levels(continually$continually))

p4 <- ggplot(data=continually, aes(x=reorder(continually, number), y=number)) +
  labs(x= "Category", y = "Responses (%)") +
  geom_bar(stat="identity",fill="steelblue") + 
  coord_flip() +
  
  theme(axis.title.y = element_text(size = 12, angle = 90)) + # axis y 
  theme(axis.title.x = element_text(size = 12, angle = 00)) + # axis x
  theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis y
  
  # Panel
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  
  scale_x_discrete(labels=c('Did not plan to attend\n the annual meeting',
                            'Other reasons',
                            'Did not have funds for\n membership fee'))

p4
p4 + ggsave("notMembershipRenew.jpeg", width = 20, height = 20, units = "cm") 

Fig2 <- p3 + p4
Fig2 + plot_annotation(tag_levels = 'A')
Fig2 + ggsave("Figure_2.jpeg") 


# Interest ----------------------------------------------------------------

option <- c(rep("Option 1" , 5) , rep("Option 2" , 5) , rep("Option 3" , 5) ,
            rep("Option 4" , 5), rep("Option 5" , 5))

category <- c('Community Ecology','Biogeochemistry','Conservation','Streams/Rivers','Hydrology',
              'Streams/Rivers', 'Conservation', 'Biogeochemistry', 'Management', 'Hydrology',
              'Streams/Rivers', 'Invertebrates','Lakes', 'Management', 'Conservation',
              'Invertebrates','Streams/Rivers','Wetlands','Nutrients','Sediments',
              'Fish', 'Climate Change', 'Food Webs', 'Nutrients', 'Streams/Rivers')
numbers <- c(48,24,7,5,4,
             32.3,12.5,8.3,8.3,8.3,
             30.1,10.8,9.7,6.7,5.4,
             19.3,12.5,5.7,4.6,4.6,
             10.3,7.7,7.7,7.7,6.4
             )

interest <- data.frame(option,category, numbers)
interest


cbp1 <- c("#AA7744", "#774411", "#DDDD77","#AAAA44", 
          "#777711", "#88CCAA", "#44AA77", "#117744", 
          "#77CCCC", "#44AAAA","#117777",  "#77AADD", 
          "#4477AA", "#114477")
                                          #azul


p <- ggplot(interest, 
       aes(x=reorder(option,numbers), y=numbers, fill=reorder(category, numbers))) + 
  geom_bar(stat="identity") +
  labs(x= "", y = "Percentage", fill= "Topic") +
  coord_flip()+
  scale_fill_manual(values = cbp1)+
   guides(fill = guide_legend(reverse=TRUE)) +
   theme(axis.title.y = element_text(size = 12, angle = 90)) + # axis y 
   theme(axis.title.x = element_text(size = 12, angle = 00)) + # axis x
   theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis x
   theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis y
   
   # Panel
   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
         panel.background = element_blank(), axis.line = element_line(colour = "black")) +
   theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))
p  

p + ggsave("Figure_3.jpeg")



# Frequenly ---------------------------------------------------------------

frequence <- c('> 75% of the years since','51-75% of the years since',
                  '26-50% of the years since','<25% of the years since',
                 'Only one time')

number <- c(55,18,11,4,12)

frequence <- data.frame(frequence, number)
frequence

frequence$frequence <- factor(frequence$frequence, levels = frequence$frequence)

f <- ggplot(data=frequence, aes(x=frequence, y=number)) +
  labs(x= "", y = "Responses (%)") +
  geom_bar(stat="identity",fill="steelblue") + 
  coord_flip() +
  
  theme(axis.title.y = element_text(size = 14, angle = 90)) + # axis y 
  theme(axis.title.x = element_text(size = 14, angle = 00)) + # axis x
  theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=12, vjust=0.5, color="black")) + #subaxis y
  
  # Panel
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) 
  

f
f + ggsave("frequency.jpeg", width = 20, height = 20, units = "cm")
