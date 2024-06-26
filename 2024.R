library(tidyverse)
library(ggplot2)
library(forcats)
library(scales)
#install.packages("ggstats")
library(ggstats)

##Out of the Fall 2023 or Spring 2024 Muser projects you were accepted to, how many did you end up participating in across both semesters?
results %>%
  ggplot(aes(x=numproj)) +
  geom_bar(stat = "count") +
  ggtitle("Out of the Fall 2023 or Spring 2024 Muser projects you were accepted to,
  how many did you end up participating in across both semesters?") +
  xlab("Number of Projects") + ylab("Number of Responses") + 
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.4),
        axis.title.x = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(size = 18, face = "bold"),
        axis.text.y = element_text(size = 18))

length(which(results$numproj == " 1 project "))
length(which(results$numproj == " 2 projects "))
length(which(results$numproj == " 3 projects "))
length(results$numproj)
33/40
38/40

##Did you and your mentor sign a mentor-student contract at the start of your research experience?
results %>%
  filter(Q10 != "") %>%
  ggplot(aes(x=Q10)) +
  geom_bar(stat = "count") +
  ggtitle("Did you and your mentor sign a mentor-student contract 
      at the start of your research experience?") +
  xlab("") + ylab("Number of Responses") +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.4),
        axis.title.x = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(size = 18, face = "bold"),
        axis.text.y = element_text(size = 18))

length(which(results$Q10 == " Yes "))
length(which(results$Q10 == " No "))
length(results$Q10)
11/40
26/40


##LIKERT GRAPH
#Please rate the following statements about your Muser project's mentor-student contract
contract <- results %>%
  filter(Q12_1 != "") %>%
  filter(Q12_2 != "") %>%
  filter(Q12_3 != "") %>%
  filter(Q12_4 != "") %>%
  filter(Q12_5 != "") %>%
  mutate(across(8:12, ~ factor(.x, levels = c("Strongly disagree",
                                              "Somewhat disagree",
                                              "Neither agree nor disagree",
                                              "Somewhat agree",
                                              "Strongly agree"))))
  
gglikert(contract, labels_size = 7, include = Q12_1:Q12_5, sort = "ascending", 
         sort_method = "mean", y_label_wrap = 25,
         variable_labels = c(
           Q12_1 = "The contents of the mentor-student contract were clear",
           Q12_2 = "The process of writing the mentor-student contract was collaborative",
           Q12_3 = "My mentor and I honored the terms of the mentor-student contract",
           Q12_4 = "The mentor-student contract was useful",
           Q12_5 = "The mentor-student contract enhanced my research experience"
         )) +
  ggtitle("Please rate the following statements about your Muser project's mentor-student contract") +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 20),
  )

##If you were compensated using Federal or Duke work-study funds, did you apply for an Undergraduate Research Support Assistantship Grant?
results %>%
  filter(Q7 != "") %>%
  ggplot(aes(x=Q7)) +
  geom_bar(stat = "count") +
  ggtitle("If you were compensated using Federal or Duke work-study funds, 
  did you apply for an Undergraduate Research Support Assistantship Grant?") +
  xlab("") + ylab("Number of Responses") +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(size = 18, face = "bold"),
        axis.text.y = element_text(size = 18))

length(which(results$Q7 == " I was not compensated using work-study "))
length(which(results$Q7 == " No "))
length(which(results$Q7 == " Yes "))
length(results$Q10)

17/40
11/40
9/40

##Who did you work with over the course of your Muser project? You may select more than one.
Q8 <- results %>%
  separate_rows(Q8, sep = ",")

Q8 %>%
  filter(Q8 != "") %>%
  ggplot(aes(x=factor(Q8, levels = 
                        c("Faculty/staff", "Graduate students", 
                          "Postdoctoral scholars", "Undergraduate students",
                          "Other")))) + 
  geom_bar(stat = "count") +
  ggtitle("Who did you work with over the course of your Muser project?") +
  xlab("") + ylab("Number of Responses") +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(size = 18, face = "bold"),
        axis.text.y = element_text(size = 18))

length(which(Q8$Q8 == "Faculty/staff"))
length(which(Q8$Q8 == "Graduate students"))
length(which(Q8$Q8 == "Postdoctoral scholars"))
length(which(Q8$Q8 == "Undergraduate students"))
length(Q8$Q8)
28/40
18/40

length(unique(Q8$uniqueid))

##What were the outcomes of your research experience? You may select more than one.
Q9 <- results %>%
  separate_rows(Q9, sep = ",")

Q9 %>%
  filter(Q9 != "") %>%
  count(Q9) %>%
  ggplot(aes(reorder(Q9, n), n)) +
  geom_col() +
  ggtitle("What were the outcomes of your research experience?") +
  xlab("") + ylab("Number of Responses") +
  theme(plot.title = element_text(hjust = 0)) +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.475),
        axis.title.x = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18, face = "bold")) + 
  coord_flip()

length(which(Q9$Q9 == "Research experience"))
length(unique(Q9$uniqueid))
30/40

##In general, what qualities do you most value in a research mentor?
Q13 <- results %>%
  separate_rows(Q13, sep = ",")

Q13 %>%
  filter(Q13 != "") %>%
  count(Q13) %>%
  ggplot(aes(reorder(Q13, n), n)) +
  geom_col() +
  scale_x_discrete(labels = label_wrap(30)) +
  ggtitle("In general, what qualities do you most value in a research mentor?") +
  xlab("") + ylab("Number of Responses") +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.2),
        axis.title.x = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18, face = "bold")) +
  coord_flip()

length(which(Q13$Q13 == "My mentor is willing to teach me new concepts or skills"))
length(which(Q13$Q13 == "My mentor communicates with me in a clear manner"))
length(which(Q13$Q13 == "My mentor is willing to work closely with me"))

28/40
25/40
20/40

##Please rate your satisfaction with how well your Muser mentor fulfilled the top qualities you value in a mentor generally.
mentorsatis <- results %>%
  mutate(across(21:29, ~ factor(.x, levels = c ("Very dissatisfied",
                                                  "Dissatisfied",
                                                  "Neither dissatisfied nor satisfied",
                                                  "Satisfied",
                                                  "Very satisfied"))))

gglikert(mentorsatis, labels_size = 7, include = Q14_1:Q14_8, sort = "ascending", 
         sort_method = "mean", y_label_wrap = 25,
         variable_labels = c(
           Q14_1 = "My mentor communicates with me in a clear manner",
           Q14_2 = "My mentor is willing to work closely with me",
           Q14_3 = "My mentor is clear about my work expectations",
           Q14_4 = "My mentor is willing to teach me new concepts or skills",
           Q14_5 = "My mentor supports my professional development",
           Q14_6 = "My mentor shares similar identities to my own",
           Q14_7 = "My mentor supports me as a whole person",
           Q14_8= "My mentor is culturally competent"
         )) +
  ggtitle("Rate your satisfaction with how well your Muser mentor fulfilled the top qualities you value in a mentor.") +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 20),
  )

##In general, what do you most want to get out of a research project?
Q15 <- results %>%
  separate_rows(Q15, sep = ",")

Q15 %>%
  filter(Q15 != "") %>%
  count(Q15) %>%
  ggplot(aes(reorder(Q15, n), n)) +
  geom_col() +
  ggtitle("What do you most want to get out of a research project?") +
  xlab("") + ylab("Number of Responses") +
  scale_x_discrete(labels = label_wrap(40)) +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18, face = "bold")) +
  coord_flip()

length(which(Q15$Q15 == "Research skills"))
length(which(Q15$Q15 == "Experience working in the field I'm interested in"))
length(which(Q15$Q15 == "A close connection with my mentor"))

25/40
21/40
12/40

##Please rate your satisfaction with how well your Muser project fulfilled what you most want to get out of a research project generally.
projsatis <- results %>%
  mutate(across(32:42, ~ factor(.x, levels = c("Very dissatisfied",
                                               "Dissatisfied",
                                               "Neither dissatisfied nor satisfied",
                                               "Satisfied",
                                               "Very satisfied"))))

gglikert(projsatis, labels_size = 7, include = Q16_1:Q16_11, sort = "ascending", 
         sort_method = "mean", y_label_wrap = 25,
         variable_labels = c(
           Q16_1 = "Experience working in the field I’m interested in",
           Q16_2 = "Experience working on academic topics I’m interested in",
           Q16_3 = "Research skills",
           Q16_4 = "Learning something new",
           Q16_5 = "Feeling like I’m solving a problem",
           Q16_6 = "A close connection with my mentor",
           Q16_7 = "A close connection with other student researchers",
           Q16_8 = "Independent study/academic credit",
           Q16_9 = "Financial compensation",
           Q16_10 = "Professional connections and support in the field I’m interested in",
           Q16_11 = "A letter of reference or recommendation"
         )) +
  ggtitle("Rate your satisfaction with how well your Muser project fulfilled what you most want to get out of a research project.") +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 20),
  )

##Mentor satisfaction - with student-mentor contract vs. without
mentcontract <- mentorsatis %>%
  filter(Q10 == " Yes ") %>%
  select(-Q14_3)

gglikert(mentcontract, labels_size = 7, include = Q14_1:Q14_8, sort = "ascending", 
         sort_method = "mean", y_label_wrap = 25,
         variable_labels = c(
           Q14_1 = "My mentor communicates with me in a clear manner",
           Q14_2 = "My mentor is willing to work closely with me",
           Q14_4 = "My mentor is willing to teach me new concepts or skills",
           Q14_5 = "My mentor supports my professional development",
           Q14_6 = "My mentor shares similar identities to my own",
           Q14_7 = "My mentor supports me as a whole person",
           Q14_8= "My mentor is culturally competent"
         )) +
  ggtitle("With contract: Rate your satisfaction with how well your Muser mentor fulfilled the top qualities you value in a mentor.") +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 20),
  )

mentnocontract <- mentorsatis %>%
  filter(Q10 == " No ")

gglikert(mentnocontract, labels_size = 7, include = Q14_1:Q14_8, sort = "ascending", 
         sort_method = "mean", y_label_wrap = 25,
         variable_labels = c(
           Q14_1 = "My mentor communicates with me in a clear manner",
           Q14_2 = "My mentor is willing to work closely with me",
           Q14_3 = "My mentor is clear about my work expectations",
           Q14_4 = "My mentor is willing to teach me new concepts or skills",
           Q14_5 = "My mentor supports my professional development",
           Q14_6 = "My mentor shares similar identities to my own",
           Q14_7 = "My mentor supports me as a whole person",
           Q14_8= "My mentor is culturally competent"
         )) +
  ggtitle("Without contract: Rate your satisfaction with how well your Muser mentor fulfilled the top qualities you value in a mentor.") +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 20),
  )

##Project satisfaction - with student-mentor contract vs. without
projcontract <- projsatis %>%
  filter(Q10 == " Yes ") %>%
  select(-Q16_7) %>%
  select(-Q16_8) %>%
  select(-Q16_11)

gglikert(projcontract, labels_size = 7, include = Q16_1:Q16_10, sort = "ascending", 
         sort_method = "mean", y_label_wrap = 25,
         variable_labels = c(
           Q16_1 = "Experience working in the field I’m interested in",
           Q16_2 = "Experience working on academic topics I’m interested in",
           Q16_3 = "Research skills",
           Q16_4 = "Learning something new",
           Q16_5 = "Feeling like I’m solving a problem",
           Q16_6 = "A close connection with my mentor",
           Q16_9 = "Financial compensation",
           Q16_10 = "Professional connections and support in the field I’m interested in"
         )) +
  ggtitle("With contract: Rate your satisfaction with how well your Muser project fulfilled what you most want to get out of a project.") +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 20),
  )

projnocontract <- projsatis %>%
  filter(Q10 == " No ")


gglikert(projnocontract, labels_size = 7, include = Q16_1:Q16_11, sort = "ascending", 
         sort_method = "mean", y_label_wrap = 25,
         variable_labels = c(
           Q16_1 = "Experience working in the field I’m interested in",
           Q16_2 = "Experience working on academic topics I’m interested in",
           Q16_3 = "Research skills",
           Q16_4 = "Learning something new",
           Q16_5 = "Feeling like I’m solving a problem",
           Q16_6 = "A close connection with my mentor",
           Q16_7 = "A close connection with other student researchers",
           Q16_8 = "Independent study/academic credit",
           Q16_9 = "Financial compensation",
           Q16_10 = "Professional connections and support in the field I’m interested in",
           Q16_11 = "A letter of reference or recommendation"
         )) +
  ggtitle("Without contract: Rate your satisfaction with how well your Muser project fulfilled what you most want to get out of a project.") +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 20),
  )
