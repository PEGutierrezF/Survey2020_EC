






##################
### Workshops ###
##################


workshop_2020<-read.csv("data/workshop_full_2020.csv", header=TRUE)
head(workshop_2020)

#1
workshop_2020$Grant.writing = factor(workshop_2020$Grant.writing,
                                     levels = c("Don't know/Unsure", "Will not participate", "Unlikely to participate", 
                                                "Might participate", "Likely to participate", "Will definitely participate"),
                                     ordered = TRUE, exclude="NA")

#2
workshop_2020$Mentoring.training.and.mentoring.graduate.and.undergraduate.students = factor(workshop_2020$Mentoring.training.and.mentoring.graduate.and.undergraduate.students,
                                                                                            levels = c("Don't know/Unsure", "Will not participate", "Unlikely to participate", 
                                                                                                       "Might participate", "Likely to participate", "Will definitely participate"),
                                                                                            ordered = TRUE, exclude="NA")

#3
workshop_2020$Lab.personnel.management = factor(workshop_2020$Lab.personnel.management,
                                                levels = c("Don't know/Unsure", "Will not participate", "Unlikely to participate", 
                                                           "Might participate", "Likely to participate", "Will definitely participate"),
                                                ordered = TRUE, exclude="NA")


#4
workshop_2020$Teaching = factor(workshop_2020$Teaching,
                                levels = c("Don't know/Unsure", "Will not participate", "Unlikely to participate", 
                                           "Might participate", "Likely to participate", "Will definitely participate"),
                                ordered = TRUE, exclude="NA")

#5
workshop_2020$Negotiation = factor(workshop_2020$Negotiation,
                                   levels = c("Don't know/Unsure", "Will not participate", "Unlikely to participate", 
                                              "Might participate", "Likely to participate", "Will definitely participate"),
                                   ordered = TRUE, exclude="NA")

#6
workshop_2020$Tenure.process = factor(workshop_2020$Tenure.process,
                                      levels = c("Don't know/Unsure", "Will not participate", "Unlikely to participate", 
                                                 "Might participate", "Likely to participate", "Will definitely participate"),
                                      ordered = TRUE, exclude="NA")

#7
workshop_2020$Stakeholder.engagement.and.participatory.research = factor(workshop_2020$Stakeholder.engagement.and.participatory.research,
                                                                         levels = c("Don't know/Unsure", "Will not participate", "Unlikely to participate", 
                                                                                    "Might participate", "Likely to participate", "Will definitely participate"),
                                                                         ordered = TRUE, exclude="NA")

#8
workshop_2020$The.publishing.and.peer.review.process = factor(workshop_2020$The.publishing.and.peer.review.process,
                                                              levels = c("Don't know/Unsure", "Will not participate", "Unlikely to participate", 
                                                                         "Might participate", "Likely to participate", "Will definitely participate"),
                                                              ordered = TRUE, exclude="NA")

#9
workshop_2020$Family.and.work.balance = factor(workshop_2020$Family.and.work.balance,
                                               levels = c("Don't know/Unsure", "Will not participate", "Unlikely to participate", 
                                                          "Might participate", "Likely to participate", "Will definitely participate"),
                                               ordered = TRUE, exclude="NA")

#10
workshop_2020$Workplace.gender.equality = factor(workshop_2020$Workplace.gender.equality,
                                                 levels = c("Don't know/Unsure", "Will not participate", "Unlikely to participate", 
                                                            "Might participate", "Likely to participate", "Will definitely participate"),
                                                 ordered = TRUE, exclude="NA")
#11
workshop_2020$Equal.opportunity = factor(workshop_2020$Equal.opportunity,
                                         levels = c("Don't know/Unsure", "Will not participate", "Unlikely to participate", 
                                                    "Might participate", "Likely to participate", "Will definitely participate"),
                                         ordered = TRUE, exclude="NA")

#12
workshop_2020$Power.and.privilege = factor(workshop_2020$Power.and.privilege,
                                           levels = c("Don't know/Unsure", "Will not participate", "Unlikely to participate", 
                                                      "Might participate", "Likely to participate", "Will definitely participate"),
                                           ordered = TRUE, exclude="NA")
#13
workshop_2020$Diversity.equity.and.inclusivity.in.the.workplace = factor(workshop_2020$Diversity.equity.and.inclusivity.in.the.workplace,
                                                                         levels = c("Don't know/Unsure", "Will not participate", "Unlikely to participate", 
                                                                                    "Might participate", "Likely to participate", "Will definitely participate"),
                                                                         ordered = TRUE, exclude="NA")

#14
workshop_2020$Building.an.inclusive.workplace = factor(workshop_2020$Building.an.inclusive.workplace,
                                                       levels = c("Don't know/Unsure", "Will not participate", "Unlikely to participate", 
                                                                  "Might participate", "Likely to participate", "Will definitely participate"),
                                                       ordered = TRUE, exclude="NA")


workshop_2020 <- rename(workshop_2020, c(Grant.writing = "Grant writing", 
                                         Mentoring.training.and.mentoring.graduate.and.undergraduate.students= "Training and mentoring grad and undergrad students", 
                                         Lab.personnel.management="Lab personnel management", Teaching = "Teaching", Negotiation = "Negotiation", 
                                         Tenure.process = "Tenure process", 
                                         Stakeholder.engagement.and.participatory.research= "Stakeholder engagement and participatory research", 
                                         The.publishing.and.peer.review.process = "The publishing and peer-review process",
                                         Family.and.work.balance = "Family and work balance",
                                         Workplace.gender.equality="Workplace gender equality", Equal.opportunity="Equal opportunity",
                                         Power.and.privilege= "Power and privilege",
                                         Diversity.equity.and.inclusivity.in.the.workplace="Diversity equity and inclusivity in the workplace",
                                         Building.an.inclusive.workplace= "Building an inclusive workplace"))


workshop_20201 = likert(workshop_2020)
print(workshop_20201)

w<- as.data.frame(workshop_20201$results)
w<-melt(w)
w

mycolors <- c( '#d0d1e6','#a6bddb','#74a9cf','#3690c0','#0570b0','#034e7b','#023858')

labels <- c("The publishing and peer-review
            process","Lab personnel management",
  "Equal opportunity",
  "Family and work balance",
  "Grant writing",
  "Tenure process","Teaching",
  "Stakeholder engagement and 
  participatory research",
  "Workplace gender equality",
  "Negotiation","Power and privilege",
  "Training and mentoring grad and 
  undergrad students", 
  "Diversity equity and inclusivity 
  in the workplace", "Building an inclusive workplace")


workshop_2020 = w %>% 
  ungroup() %>%
  arrange(fct_relevel(variable, "Will definitely participate"), value) %>%
  mutate(Item = fct_inorder(Item))
workshop_2020

ws <- ggplot(data = workshop_2020, aes(x =Item, y = value, fill = variable, order= value)) +
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

ws
ws + ggsave("Workshop.jpeg", width = 25, height = 22, units = "cm")  

