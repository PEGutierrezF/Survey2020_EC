

library('ggplot2')
library('patchwork')
library(dplyr)


category <- c('Assistant Professor','Associate or Full Professor',
            'Postdoc ', 'Consulting', 'State or provincial agency', 'Federal agency',
            'NGO','Others')

responses <- c(25.6,18,14.6,7,6,6,6,15.8)

perfil <- data.frame(category, responses)
perfil

perfil$category <- factor(perfil$category, levels = perfil$category)

p1 <- ggplot(data=perfil, aes(x=reorder(category, responses), y=responses)) +
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
  
  scale_x_discrete(labels=c('State or\n provincial agency', 'Federal agency',
                            'NGO','Consulting','Postdoc ','Others',
                            'Associate or\n Full Professor','Assistant Professor'))
p1


# Degree ------------------------------------------------------------------

degree <- c('PhD','MSc','BSc')
number <- c(70,20,10)

degree<- data.frame(degree, number)
degree


p2 <- ggplot(data=degree, aes(x=reorder(degree, -number), y=number)) +
  labs(x= "Degree", y = "% of respondents") +
  geom_bar(stat="identity", fill="steelblue") + 

  
  theme(axis.title.y = element_text(size = 12, angle = 90)) + # axis y 
  theme(axis.title.x = element_text(size = 12, angle = 00)) + # axis x
  theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis y
  
  # Panel
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))
p2  
 
Fig1 <- p1 + p2
Fig1 + plot_annotation(tag_levels = 'A')
Fig1 + ggsave("Figure_1.jpeg") 


# Continually attending SFS meetings ---------------------------------------

continually <- c('Yes','Not','Not applicable (not yet graduated)')
number <- c(51,29,20)

continually <- data.frame(continually, number)
continually

p3 <- ggplot(data=continually, aes(x=reorder(continually,-number), y=number)) +
  labs(x= "Attending continually", y = "% of respondents") +
  geom_bar(stat="identity",fill="steelblue") + 
  
  
  theme(axis.title.y = element_text(size = 12, angle = 90)) + # axis y 
  theme(axis.title.x = element_text(size = 12, angle = 00)) + # axis x
  theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis y
  
  # Panel
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  
  scale_x_discrete(labels=c('Yes',
                            'No',
                            'Not applicable\n (not yet\n graduated'))

p3

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
  
  scale_x_discrete(labels=c('Other reasons',
                            'Did not plan to attend\n the annual meeting',
                            'Did not have funds for\n membership fee'))

p4

Fig2 <- p3 + p4
Fig2 + plot_annotation(tag_levels = 'A')
Fig2 + ggsave("Figure_2.jpeg") 


# Interest ----------------------------------------------------------------

option <- c(rep("option1" , 5) , rep("option2" , 5) , rep("option3" , 5) )

category <- c('Community Ecology','Biogeochemistry','Conservation','Streams/Rivers','Hydrology',
              'Streams/Rivers', 'Conservation', 'Biogeochemistry', 'Management', 'Hydrology',
              'Streams/Rivers', 'Invertebrates','Lakes', 'Management', 'Conservation')
numbers <- c(48,24,7,5,4,
             32.29,12.5,8.33,8.33,8.33,
             29.17,10.42,9.38,6.26,5.21
             )

interest <- data.frame(option,category, numbers)
interest

plotorder = c('Community Ecology','Biogeochemistry','Conservation','Streams/Rivers',
              'Hydrology','Invertebrates','Lakes', 'Management')

p <- ggplot(interest, 
       aes(x=reorder(option,numbers), y=numbers, fill=reorder(category,numbers))) + 
  geom_bar(stat="identity") +
  labs(x= "", y = "Percentage", fill= "Topic") +
   scale_fill_brewer(palette="Blues")+
  coord_flip()+
   guides(fill = guide_legend(reverse=TRUE)) +
   theme(axis.title.y = element_text(size = 12, angle = 90)) + # axis y 
   theme(axis.title.x = element_text(size = 12, angle = 00)) + # axis x
   theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis x
   theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis y
   
   # Panel
   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
         panel.background = element_blank(), axis.line = element_line(colour = "black")) +
   theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))
p + ggsave("Figure_3.jpeg") 

