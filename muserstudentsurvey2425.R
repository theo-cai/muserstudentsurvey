library(tidyverse)
library(ggplot2)
library(forcats)
library(scales)
#install.packages("ggstats")
library(ggstats)

##Out of the Summer 2024, Fall 2024, or Spring 2025 Muser projects you were accepted to, how many did you end up participating in across both semesters?
results %>%
  ggplot(aes(x=numproj)) +
  geom_bar(stat = "count") +
  ggtitle("Out of the Summer 2025, Fall 2024, or Spring 2025 Muser projects you were accepted to,
  how many did you end up participating in across both semesters?") +
  xlab("Number of Projects") + ylab("Number of Responses") + 
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.4),
        axis.title.x = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(size = 18, face = "bold"),
        axis.text.y = element_text(size = 18))

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

##LIKERT GRAPH
#Please rate the following statements about your Muser project's mentor-student contract
contract <- results %>%
  filter(Q12_1 != "") %>%
  filter(Q12_2 != "") %>%
  filter(Q12_3 != "") %>%
  filter(Q12_4 != "") %>%
  filter(Q12_5 != "") %>%
  mutate(across(12:16, ~ factor(.x, levels = c("Strongly disagree",
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

##Who did you work with over the course of your Muser project? You may select more than one.
Q8 <- results %>%
  separate_rows(Q8, sep = ",")

Q8 %>%
  filter(Q8 != "") %>%
  ggplot(aes(x=factor(Q8, levels = 
                        c("Faculty/staff", "Graduate students ", 
                          "Postdoctoral scholars ", "Undergraduate students ",
                          "Other")))) + 
  geom_bar(stat = "count") +
  ggtitle("Who did you work with over the course of your Muser project?") +
  xlab("") + ylab("Number of Responses") +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(size = 18, face = "bold"),
        axis.text.y = element_text(size = 18))

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

##In general, what qualities do you most value in a research mentor?
Q13 <- results %>%
  separate_rows(Q13, sep = ",")

Q13 %>%
  filter(Q13 != "") %>%
  count(Q13) %>%
  ggplot(aes(reorder(Q13, n), n)) +
  geom_col() +
  scale_x_discrete(labels = label_wrap(30)) +
  scale_y_continuous(breaks = breaks_pretty()) +
  ggtitle("In general, what qualities do you most value in a research mentor?") +
  xlab("") + ylab("Number of Responses") +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.2),
        axis.title.x = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18, face = "bold")) +
  coord_flip()

##Please rate your satisfaction with how well your Muser mentor fulfilled the top qualities you value in a mentor generally.
mentorsatis <- results %>%
  mutate(across(25:31, ~ factor(.x, levels = c ("Very dissatisfied",
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
  scale_y_continuous(breaks = breaks_pretty()) + # Fixes the 0.5 increment issue noted below
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18, face = "bold")) +
  coord_flip()


##Please rate your satisfaction with how well your Muser project fulfilled what you most want to get out of a research project generally.
projsatis <- results %>%
  mutate(across(36:47, ~ factor(.x, levels = c("Very dissatisfied",
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


