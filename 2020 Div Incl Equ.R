




# Changes in Diverity -----------------------------------------------------

response<- c('Increased in diversity','No change',"Don't know / Unsure")
number <- c(36,21,43)

diversity<- data.frame(response, number)

d <- ggplot(data=diversity, aes(x=response, y=number)) +
  labs(x= "Responses", y = "% of respondents") +
  geom_bar(stat="identity", fill="steelblue") + 
  
    theme(axis.title.y = element_text(size = 14, angle = 90)) + # axis y 
  theme(axis.title.x = element_text(size = 14, angle = 00)) + # axis x
  theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=12, vjust=0.5, color="black")) + #subaxis y
  
  # Panel
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))
d  
d + ggsave("diversity.jpeg", width = 20, height = 20, units = "cm")




#######################
####### thoughts on diversity equity and inclusion 2020 ########
#######################


thoughts_div_eq_inc_full_2020<-read.csv("data/thoughts_div_eq_inc_full_2020.csv", header=TRUE)
head(thoughts_div_eq_inc_full_2020)

#1
thoughts_div_eq_inc_full_2020$The.atmosphere.at.SFS.is.very.welcoming.and.inclusive = 
  factor(thoughts_div_eq_inc_full_2020$The.atmosphere.at.SFS.is.very.welcoming.and.inclusive,
         levels = c("Don't know/Unsure", "Strongly disagree", "Somewhat disagree", "Agree", 
                    "Neutral", "Strongly agree"),ordered = TRUE, exclude="NA")

#2
thoughts_div_eq_inc_full_2020$Diverse.ideas.and.perspectives.are.heard.and.respected.without.fear.of.negative.responses.or.reactions = 
  factor(thoughts_div_eq_inc_full_2020$Diverse.ideas.and.perspectives.are.heard.and.respected.without.fear.of.negative.responses.or.reactions,
         levels = c("Don't know/Unsure", "Strongly disagree", "Somewhat disagree", "Agree", 
                    "Neutral", "Strongly agree"),ordered = TRUE, exclude="NA")

#3
thoughts_div_eq_inc_full_2020$I.feel.adequately.supported.by.SFS.in.reporting.incidents.or.problematic.behaviors.observed.within.the.society = 
  factor(thoughts_div_eq_inc_full_2020$I.feel.adequately.supported.by.SFS.in.reporting.incidents.or.problematic.behaviors.observed.within.the.society,
         levels = c("Don't know/Unsure", "Strongly disagree", "Somewhat disagree", "Agree", 
                    "Neutral", "Strongly agree"),ordered = TRUE, exclude="NA")

#4
thoughts_div_eq_inc_full_2020$Diversity.and.inclusion.resources.are.transparent.and.discoverable.within.the.SFS.ecosystem = 
  factor(thoughts_div_eq_inc_full_2020$Diversity.and.inclusion.resources.are.transparent.and.discoverable.within.the.SFS.ecosystem,
         levels = c("Don't know/Unsure", "Strongly disagree", "Somewhat disagree", "Agree", 
                    "Neutral", "Strongly agree"),ordered = TRUE, exclude="NA")
#5
thoughts_div_eq_inc_full_2020$The.balance.of.volunteer.and.administrative.work.among.SFS.members.is.adequately.and.equally.divided.such.that.no.one.group.or.person.is.responsible.for.more.than.their.fair.share.of.work = 
  factor(thoughts_div_eq_inc_full_2020$The.balance.of.volunteer.and.administrative.work.among.SFS.members.is.adequately.and.equally.divided.such.that.no.one.group.or.person.is.responsible.for.more.than.their.fair.share.of.work,
         levels = c("Don't know/Unsure", "Strongly disagree", "Somewhat disagree", "Agree", 
                    "Neutral", "Strongly agree"),ordered = TRUE, exclude="NA")

thoughts_div_eq_inc_full_2020 <- rename(thoughts_div_eq_inc_full_2020, c(
  The.atmosphere.at.SFS.is.very.welcoming.and.inclusive=  "The.atmosphere at SFS is very welcoming and inclusive",
  Diverse.ideas.and.perspectives.are.heard.and.respected.without.fear.of.negative.responses.or.reactions=  "Diverse ideas and perspectives are heard and respected without fear of negative responses or reactions",
  I.feel.adequately.supported.by.SFS.in.reporting.incidents.or.problematic.behaviors.observed.within.the.society ="I feel adequately supported by SFS in reporting incidents or problematic behaviors observed within the society",
  Diversity.and.inclusion.resources.are.transparent.and.discoverable.within.the.SFS.ecosystem= "Diversity and inclusion resources are transparent and discoverable within the SFS ecosystem",
  The.balance.of.volunteer.and.administrative.work.among.SFS.members.is.adequately.and.equally.divided.such.that.no.one.group.or.person.is.responsible.for.more.than.their.fair.share.of.work="The balance of volunteer and administrative work among SFS members is adequately and equally divided such that no one group or person is responsible for more than their fair share of work"))




thoughts_div_eq_inc_full_2020 = likert(thoughts_div_eq_inc_full_2020)
print(thoughts_div_eq_inc_full_2020)

thoughts_div_eq_inc_full_2020 <- as.data.frame(thoughts_div_eq_inc_full_2020$results)
thoughts_div_eq_inc_full_2020

DEI<-melt(thoughts_div_eq_inc_full_2020)
DEI

mycolors <- c( '#d0d1e6','#a6bddb','#74a9cf','#3690c0','#0570b0','#034e7b','#023858')

labels <- c("The balance of volunteer and administrative work among
            SFS members is adequately and equally divided such that no
            one group or person is responsible for more than their fair share of work",
  "Diverse ideas and perspectives are heard and respected
  without fear of negative responses or reactions",
  "I feel adequately supported by SFS in reporting incidents or
  problematic behaviors observed within the society",
  "Diverse ideas and perspectives are heard and respected without
  fear of negative responses or reactions",
  "The atmosphere at SFS is very welcoming and inclusive")

DEI1 = DEI %>% 
  ungroup() %>%
  arrange(fct_relevel(variable, "Strongly agree"), value) %>%
  mutate(Item = fct_inorder(Item))
DEI1

DEI_2020 <- ggplot(data = DEI1, aes(x =Item, y = value, fill = variable, order= value)) +
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

DEI_2020
DEI_2020 + ggsave("Div Equ Inc.jpeg", width = 25, height = 22, units = "cm")  









#######################
####### Invest on diversity equity and inclusion 2020 ########
#######################


invest_div_eq_inc_full_2020<-read.csv("data/invest_div_eq_inc_full_2020.csv", header=TRUE)
head(invest_div_eq_inc_full_2020)

#1
invest_div_eq_inc_full_2020$Work.with.the.Education.and.DIversity.Committee.on.initiatives.specifically.for.Early.Career.members = 
  factor(invest_div_eq_inc_full_2020$Work.with.the.Education.and.DIversity.Committee.on.initiatives.specifically.for.Early.Career.members,
         levels = c("Don't know/Unsure", "Not important", "Somewhat important", "Important", 
                    "Very important", "Extremely important"),ordered = TRUE,exclude="NA")

#2
invest_div_eq_inc_full_2020$Provide.workshops.or.networking.activities.focused.on.diversity..representation..and.inclusivity.in.STEM.fields = 
  factor(invest_div_eq_inc_full_2020$Provide.workshops.or.networking.activities.focused.on.diversity..representation..and.inclusivity.in.STEM.fields,
         levels = c("Don't know/Unsure", "Not important", "Somewhat important", "Important", 
                    "Very important", "Extremely important"),ordered = TRUE,exclude="NA")

#3
invest_div_eq_inc_full_2020$Actively.promote.the.work.contributions.of.minoritized.EC.members = 
  factor(invest_div_eq_inc_full_2020$Actively.promote.the.work.contributions.of.minoritized.EC.members,
         levels = c("Don't know/Unsure", "Not important", "Somewhat important", "Important", 
                    "Very important", "Extremely important"),ordered = TRUE,exclude="NA")


#3
invest_div_eq_inc_full_2020$Provide.travel.grants.to.members.who.have.to.travel.internationally.to.attend.an.SFS.meeting = 
  factor(invest_div_eq_inc_full_2020$Provide.travel.grants.to.members.who.have.to.travel.internationally.to.attend.an.SFS.meeting,
         levels = c("Don't know/Unsure", "Not important", "Somewhat important", "Important", 
                    "Very important", "Extremely important"),ordered = TRUE,exclude="NA")


invest_div_eq_inc_full_2020 <- rename(invest_div_eq_inc_full_2020, c(
  Work.with.the.Education.and.DIversity.Committee.on.initiatives.specifically.for.Early.Career.members=
    "Work with the Education and DIversity Committee on initiatives specifically for Early Career members",
  Provide.workshops.or.networking.activities.focused.on.diversity..representation..and.inclusivity.in.STEM.fields=
    "Provide workshops or networking activities focused on diversity representation and inclusivity in STEM fields",
  Actively.promote.the.work.contributions.of.minoritized.EC.members =
    "Actively promote the work contributions of minoritized EC members",
  Provide.travel.grants.to.members.who.have.to.travel.internationally.to.attend.an.SFS.meeting=
    "Provide travel grants to members who have to travel internationally to attend an SFS meeting"))



invest_div_eq_inc_full_2020 = likert(invest_div_eq_inc_full_2020)
print(invest_div_eq_inc_full_2020)

invest_div_eq_inc_full_2020 <- as.data.frame(invest_div_eq_inc_full_2020$results)
invest_div_eq_inc_full_2020


I_DEI<-melt(invest_div_eq_inc_full_2020)
I_DEI

mycolors <- c( '#d0d1e6','#a6bddb','#74a9cf','#3690c0','#0570b0','#034e7b','#023858')

labels <- c( "Provide workshops or networking
            activities focused on diversity 
             representation and inclusivity
             in STEM fields",
             "Work with the Education and Diversity
             Committee on initiatives specifically 
             for Early Career members",
             "Actively promote the work contributions 
             of minoritized EC members",
             "Provide travel grants to members
             who have to travel internationally 
             to attend an SFS meeting")


Inv_DEI = I_DEI %>% 
  ungroup() %>%
  arrange(fct_relevel(variable, "Extremely important"), value) %>%
  mutate(Item = fct_inorder(Item))
Inv_DEI

ID <- ggplot(data = Inv_DEI, aes(x =Item, y = value, fill = variable, order= value)) +
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

ID
ID + ggsave("Invest_DEI.jpeg", width = 25, height = 22, units = "cm")  


