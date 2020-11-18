


### If you have missed a SFS meeting, Why? ####

attendance_2020<-read.csv("data/attendance_Why_not_2020.csv", header=TRUE)
head(attendance_2020)

attendance_2020$Registration.cost = factor(attendance_2020$Registration.cost,
                                           levels = c("Don't know/Unsure", "Not important", "Somewhat important", "Important", 
                                                      "Very important", "Extremely important"),ordered = TRUE,exclude="NA")

attendance_2020$Travel.and.lodging.costs = factor(attendance_2020$Travel.and.lodging.costs,
                                                  levels = c("Don't know/Unsure", "Not important", "Somewhat important", "Important", 
                                                             "Very important", "Extremely important"),ordered = TRUE,exclude="NA")


attendance_2020$Time.expense.associated.with.travel.visas.or.other.international.travel.constraints = factor(attendance_2020$Time.expense.associated.with.travel.visas.or.other.international.travel.constraints,
                                                                                                             levels = c("Don't know/Unsure", "Not important", "Somewhat important", "Important", 
                                                                                                                        "Very important", "Extremely important"),ordered = TRUE,exclude="NA")


attendance_2020$Availability.of.outside.funding = factor(attendance_2020$Availability.of.outside.funding,
                                                         levels = c("Don't know/Unsure", "Not important", "Somewhat important", "Important", 
                                                                    "Very important", "Extremely important"),ordered = TRUE,exclude="NA")

attendance_2020$Time.commitment = factor(attendance_2020$Time.commitment,
                                         levels = c("Don't know/Unsure", "Not important", "Somewhat important", "Important", 
                                                    "Very important", "Extremely important"),ordered = TRUE,exclude="NA")

attendance_2020$Meeting.location.convenience = factor(attendance_2020$Meeting.location.convenience,
                                                      levels = c("Don't know/Unsure", "Not important", "Somewhat important", "Important", 
                                                                 "Very important", "Extremely important"),ordered = TRUE,exclude="NA")


attendance_2020$Meeting.location.desirability = factor(attendance_2020$Meeting.location.desirability,
                                                       levels = c("Don't know/Unsure", "Not important", "Somewhat important", "Important", 
                                                                  "Very important", "Extremely important"),ordered = TRUE,exclude="NA")

attendance_2020$Time.of.year = factor(attendance_2020$Time.of.year,
                                      levels = c("Don't know/Unsure", "Not important", "Somewhat important", "Important", 
                                                 "Very important", "Extremely important"),ordered = TRUE,exclude="NA")

attendance_2020$Keynote.speakers.or.other.distinguished.scientists = factor(attendance_2020$Keynote.speakers.or.other.distinguished.scientists,
                                                                            levels = c("Don't know/Unsure", "Not important", "Somewhat important", "Important", 
                                                                                       "Very important", "Extremely important"),
                                                                            ordered = TRUE,exclude="NA")

attendance_2020$Attendance.of.others.from.your.institution = factor(attendance_2020$Attendance.of.others.from.your.institution,
                                                                    levels = c("Don't know/Unsure", "Not important", "Somewhat important", "Important", 
                                                                               "Very important", "Extremely important"),ordered = TRUE,exclude="NA")

attendance_2020$Meeting.theme.and.science.focus = factor(attendance_2020$Meeting.theme.and.science.focus,
                                                         levels = c("Don't know/Unsure", "Not important", "Somewhat important", "Important", 
                                                                    "Very important", "Extremely important"),ordered = TRUE,exclude="NA")

attendance_2020$Special.session.topic.and.invitation.to.present = factor(attendance_2020$Special.session.topic.and.invitation.to.present,
                                                                         levels = c("Don't know/Unsure", "Not important", "Somewhat important", "Important", 
                                                                                    "Very important", "Extremely important"),ordered = TRUE,exclude="NA")

attendance_2020$Networking.opportunities = factor(attendance_2020$Networking.opportunities,
                                                  levels = c("Don't know/Unsure", "Not important", "Somewhat important", "Important", 
                                                             "Very important", "Extremely important"),ordered = TRUE,exclude="NA")


attendance_2020$Conflict.with.other.travel.plans = factor(attendance_2020$Conflict.with.other.travel.plans,
                                                          levels = c("Don't know/Unsure", "Not important", "Somewhat important", "Important", 
                                                                     "Very important", "Extremely important"),ordered = TRUE,exclude="NA")


attendance_2020$Other.personal.or.family.events.and.commitments = factor(attendance_2020$Other.personal.or.family.events.and.commitments,
                                                                         levels = c("Don't know/Unsure", "Not important", "Somewhat important", "Important", 
                                                                                    "Very important", "Extremely important"),ordered = TRUE,exclude="NA")

attendance_2020P <- rename(attendance_2020, c(Registration.cost = "Registration cost", 
                                              Travel.and.lodging.costs = "Travel and lodging costs",
                                              Time.expense.associated.with.travel.visas.or.other.international.travel.constraints = "Time expense by visa or international travel",
                                              Availability.of.outside.funding= "Availability of outside funding", Time.commitment= "Time commitment",
                                              Meeting.location.convenience= "Meeting location convenience", Meeting.location.desirability= "Meeting location desirability",
                                              Time.of.year= "Time of year", Keynote.speakers.or.other.distinguished.scientists= "Keynote speakers or other distinguished scientists",
                                              Attendance.of.others.from.your.institution="Attendance of others from your institution", Meeting.theme.and.science.focus="Meeting theme and science focus",
                                              Special.session.topic.and.invitation.to.present="Special session topic and invitation to present",
                                              Networking.opportunities= "Networking opportunities", Conflict.with.other.travel.plans="Conflict with other travel plans",
                                              Other.personal.or.family.events.and.commitments="Other personal or family events and commitments"))

attendance_2020P
attendance_missed = likert(attendance_2020P)

summary(attendance_missed)
print(attendance_missed)

not_a<- as.data.frame(attendance_missed$results)
not_a<-melt(not_a)
not_a

mycolors <- c( '#d0d1e6','#a6bddb','#74a9cf','#3690c0','#0570b0','#034e7b','#023858')

labels <- c( "Meeting location desirability",
              "Keynote speakers or other\n distinguished scientists",
              "Meeting theme and science focus", 
              "Attendance of others from your\n institution",
              "Special session topic and invitation\n to present",
              "Networking opportunities",            
              "Time expense by visa or\n international travel",
              "Meeting location convenience","Time of year", 
              "Time commitment","Registration cost",
              "Conflict with other travel plans",
              "Other personal or family events\n and commitments",
              "Travel and lodging costs", "Availability of outside funding")

library(dplyr)
library(forcats)

not_a1 = not_a %>% 
  ungroup() %>%
  arrange(fct_relevel(variable, "Extremely important"), value) %>%
  mutate(Item = fct_inorder(Item))
not_a1


n_a <- ggplot(data = not_a1, aes(x =Item, y = value, fill = variable, order= value)) +
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

n_a
n_a + ggsave("not_Attendance.jpeg", width = 25, height = 22, units = "cm")  



