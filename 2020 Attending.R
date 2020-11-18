

libraries <- c("xtable", "ggplot2", "likert", "plyr", "psych",
               "vctrs", "sjmisc", "pkgload", "sjPlot", "xlsx", "writexl")
lapply(libraries, require, character.only = TRUE)

##################
### ATTENDANCE ###
##################

### Attendance since becoming a member ####
attendance_Yes_2020<-read.csv("data/attendance_Since_becoming_2020.csv", header=TRUE)
head(attendance_Yes_2020)

#1
attendance_Yes_2020$Registration.cost = factor(attendance_Yes_2020$Registration.cost,
                                               levels = c("Don't know/Unsure", "Not important", "Somewhat important", "Important", 
                                                          "Very important", "Extremely important"),ordered = TRUE,exclude="NA")
#2
attendance_Yes_2020$Travel.and.lodging.costs = factor(attendance_Yes_2020$Travel.and.lodging.costs,
                                                      levels = c("Don't know/Unsure", "Not important", "Somewhat important", "Important", 
                                                                 "Very important", "Extremely important"),ordered = TRUE,exclude="NA")

#3
attendance_Yes_2020$Time.expense.associated.with.travel.visas.or.other.international.travel.constraints = factor(attendance_Yes_2020$Time.expense.associated.with.travel.visas.or.other.international.travel.constraints,
                                                                                                                 levels = c("Don't know/Unsure", "Not important", "Somewhat important", "Important", 
                                                                                                                            "Very important", "Extremely important"),
                                                                                                                 ordered = TRUE,exclude="NA")

#4
attendance_Yes_2020$Availability.of.outside.funding = factor(attendance_Yes_2020$Availability.of.outside.funding,
                                                             levels = c("Don't know/Unsure", "Not important", "Somewhat important", "Important", 
                                                                        "Very important", "Extremely important"),ordered = TRUE,exclude="NA")
#5
attendance_Yes_2020$Time.commitment = factor(attendance_Yes_2020$Time.commitment,
                                             levels = c("Don't know/Unsure", "Not important", "Somewhat important", "Important", 
                                                        "Very important", "Extremely important"),ordered = TRUE,exclude="NA")
#6
attendance_Yes_2020$Meeting.location.convenience = factor(attendance_Yes_2020$Meeting.location.convenience,
                                                          levels = c("Don't know/Unsure", "Not important", "Somewhat important", "Important", 
                                                                     "Very important", "Extremely important"),ordered = TRUE,exclude="NA")

#7
attendance_Yes_2020$Meeting.location.desirability = factor(attendance_Yes_2020$Meeting.location.desirability,
                                                           levels = c("Don't know/Unsure", "Not important", "Somewhat important", "Important", 
                                                                      "Very important", "Extremely important"),ordered = TRUE,exclude="NA")
#8
attendance_Yes_2020$Time.of.year = factor(attendance_Yes_2020$Time.of.year,
                                          levels = c("Don't know/Unsure", "Not important", "Somewhat important", "Important", 
                                                     "Very important", "Extremely important"),ordered = TRUE,exclude="NA")
#9
attendance_Yes_2020$Keynote.speakers.or.other.distinguished.scientists = factor(attendance_Yes_2020$Keynote.speakers.or.other.distinguished.scientists,
                                                                                levels = c("Don't know/Unsure", "Not important", "Somewhat important", "Important", 
                                                                                           "Very important", "Extremely important"),ordered = TRUE,exclude="NA")
#10
attendance_Yes_2020$Attendance.of.others.from.your.institution = factor(attendance_Yes_2020$Attendance.of.others.from.your.institution,
                                                                        levels = c("Don't know/Unsure", "Not important", "Somewhat important", "Important", 
                                                                                   "Very important", "Extremely important"),ordered = TRUE,exclude="NA")
#11
attendance_Yes_2020$Meeting.theme.and.science.focus = factor(attendance_Yes_2020$Meeting.theme.and.science.focus,
                                                             levels = c("Don't know/Unsure", "Not important", "Somewhat important", "Important", 
                                                                        "Very important", "Extremely important"),ordered = TRUE,exclude="NA")
#12
attendance_Yes_2020$Special.session.topic.and.invitation.to.present = factor(attendance_Yes_2020$Special.session.topic.and.invitation.to.present,
                                                                             levels = c("Don't know/Unsure", "Not important", "Somewhat important", "Important", 
                                                                                        "Very important", "Extremely important"),ordered = TRUE,exclude="NA")
#13
attendance_Yes_2020$Networking.opportunities = factor(attendance_Yes_2020$Networking.opportunities,
                                                      levels = c("Don't know/Unsure", "Not important", "Somewhat important", "Important", 
                                                                 "Very important", "Extremely important"),ordered = TRUE,exclude="NA")


attendance_Yes_2020 <- rename(attendance_Yes_2020, c(Registration.cost = "Registration cost", 
                                                     Travel.and.lodging.costs = "Travel and lodging costs",
                                                     Time.expense.associated.with.travel.visas.or.other.international.travel.constraints = "Time expense by visa or international travel",
                                                     Availability.of.outside.funding= "Availability of outside funding", Time.commitment= "Time commitment",
                                                     Meeting.location.convenience= "Meeting location convenience", Meeting.location.desirability= "Meeting location desirability",
                                                     Time.of.year= "Time of year", Keynote.speakers.or.other.distinguished.scientists= "Keynote speakers or other distinguished scientists",
                                                     Attendance.of.others.from.your.institution="Attendance of others from your institution", Meeting.theme.and.science.focus="Meeting theme and science focus",
                                                     Special.session.topic.and.invitation.to.present="Special session topic and invitation to present",
                                                     Networking.opportunities= "Networking opportunities"))

attendance_Yes_2020
attendance_Since_Member = likert(attendance_Yes_2020)

summary(attendance_Since_Member)
print(attendance_Since_Member, row.names=FALSE)

a <- as.data.frame(attendance_Since_Member$results)
class(a)
#write.xlsx(a, "/mydata.xlsx")
attending<-melt(a)

attending


mycolors <- c( '#d0d1e6','#a6bddb','#74a9cf','#3690c0','#0570b0','#034e7b','#023858')
labels <- c("Attendance of others from your\n institution", 
  "Keynote speakers or other\n distinguished scientists",
  "Meeting location convenience","Meeting location desirability",
  "Meeting theme and science focus", "Networking opportunities",
"Special session topic and invitation\n to present","Time commitment",
"Time expense by visa or\n international travel",  
"Time of year", "Availability of outside funding",
"Registration cost", "Travel and lodging costs")
  
a <- ggplot(data = attending, aes(x =reorder(Item,-value), y = value, fill = variable, order= value)) +
  labs(y="Percentage", x = "",fill="Category") +
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
  theme(legend.position="bottom")
a
a + ggsave("Attending.jpeg", width = 25, height = 22, units = "cm")  
