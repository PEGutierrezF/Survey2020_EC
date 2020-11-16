


category <- read.csv("category.csv")
category

focus<-melt(category)
focus

b <- ggplot(focus, aes(x =as.numeric(variable), y = Category)) +
  geom_point( aes(size=value), shape=21, fill="steelblue") +
  labs(x="Option", y="Research focus", size="Percentage") +
  scale_size_area(max_size=20) +
  scale_y_discrete(limits=rev)+
  
  theme(axis.title.y = element_text(size = 12, angle = 90)) + # axis y 
  theme(axis.title.x = element_text(size = 12, angle = 00)) + # axis x
  theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis y
  
  # Panel
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  theme(legend.key=element_blank()) + theme(legend.title.align=0.5) 
b

b + ggsave("Figure_4.jpeg", width = 25, height = 22, units = "cm")  
  
