





# Degree ------------------------------------------------------------------

year <- c(2017,2017,2017,2017,2020,2020,2020,2020)
degree <- c('PhD','MSc','BSc','Other degrees','PhD','MSc','BSc','Other degrees')
number <- c(43,28,26,3,70,20,10,0)

degree<- data.frame(year, degree, number)
degree


ggplot(data=degree, aes(x=reorder(degree, -number), y=number, fill=as.factor(year))) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(x= "Degree", y = "% of respondents", fill='Survey year') +
  
  geom_text(aes(label=number), vjust=1.6, color="white",
          position = position_dodge(0.9), size=4)+
  scale_fill_brewer(palette="Paired") +
  
  theme(axis.title.y = element_text(size = 12, angle = 90)) + # axis y 
  theme(axis.title.x = element_text(size = 12, angle = 00)) + # axis x
  theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis y
  
  # Panel
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))

