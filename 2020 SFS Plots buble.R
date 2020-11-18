


category <- read.csv("category1.csv")
category


focus<-melt(category)
head(focus)

not_a1 = focus %>% 
  ungroup() %>%
  arrange(fct_relevel(variable, "Extremely important"), value) %>%
  mutate(Item = fct_inorder(Item))
not_a1

focus1 = focus %>% 
  ungroup() %>%
  arrange(variable, value) %>%
  mutate(Category  = fct_inorder(Category ) )

labels <- c("Algae", "Anthropogenic materials (litter plastics, 
            nanoparticles, pharmaceuticals",
            "Biology and biologu education 
            research", "Managment", "Pollution","Fish", "Climate changes",
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
  
