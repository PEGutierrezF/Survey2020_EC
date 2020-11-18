


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

not_attendance<- as.data.frame(attendance_missed$results)
not_attendance<-melt(not_attendance)

mycolors <- c( '#d0d1e6','#a6bddb','#74a9cf','#3690c0','#0570b0','#034e7b','#023858')
labels <- c("Attendance of others from your\n institution", 
            "Keynote speakers or other\n distinguished scientists",
            "Meeting location convenience","Meeting location desirability",
            "Meeting theme and science focus", "Networking opportunities",
            "Special session topic and invitation\n to present","Time commitment",
            "Time expense by visa or\n international travel",  
            "Time of year", "Availability of outside funding",
            "Registration cost", "Travel and lodging costs")

not_attendance <- ggplot(data = not_attendance, aes(x =reorder(Item,-value), y = value, fill = variable, order= value)) +
  labs(y="Percentage", x = "",fill="Response") +
  geom_col(width = 0.7, position = position_stack(reverse = F)) +
  # geom_bar(stat="identity", width = 0.7) +
  scale_fill_manual (values=mycolors) +
#  scale_x_discrete(labels = labels) +
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

not_attendance
not_attendance + ggsave("not_Attendance.jpeg", width = 25, height = 22, units = "cm")  



