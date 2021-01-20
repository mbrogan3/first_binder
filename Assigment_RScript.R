library(extrafont)
loadfonts(device = "win", quiet = TRUE) 
library(tidyverse)
library(ggpattern)
library(ggthemes)
library(gridExtra)



#opened data
raw_data <- read.csv("AssignmentRawData.csv")
view(raw_data)


#converted it to long
long_data <- raw_data %>%
  pivot_longer(cols = c(Condition1, Condition2, Condition3, Condition4), 
               names_to = "Condition", 
               values_to = "RT") 


#changed the names of the condition and split into two columns
tidy_data <- long_data %>% 
  mutate(Condition = recode(Condition,
                            "Condition1" = "Number_Number",
                            "Condition2" = "Number_Letter", 
                            "Condition3" = "Letter_Letter", 
                            "Condition4" = "Letter_Number")) %>%
  separate(col = "Condition", into = c("Prime", "Target"), sep = "_") %>%
  mutate(Prime = factor(Prime), Target = factor(Target)) 


#statistics for each condition
tidy_data %>%
  distinct(ID)

tidy_data %>%
  filter(Prime == "Number" & Target == "Number") %>%
  summarise(mean_RT = mean(RT), sd_RT = sd(RT), median_RT = median(RT), number = n(), min_RT = min(RT),
            max_RT = max(RT), IQR_RT = IQR(RT))

tidy_data %>%
  filter(Prime == "Number" & Target == "Letter") %>%
  summarise(mean_RT = mean(RT), sd_RT = sd(RT), median_RT = median(RT), number = n(), min_RT = min(RT),
            max_RT = max(RT), IQR_RT = IQR(RT))

tidy_data %>%
  filter(Prime == "Letter" & Target == "Letter") %>%
  summarise(mean_RT = mean(RT), sd_RT = sd(RT), median_RT = median(RT), number = n(), min_RT = min(RT),
            max_RT = max(RT), IQR_RT = IQR(RT))

tidy_data %>%
  filter(Prime == "Letter" & Target == "Number") %>%
  summarise(mean_RT = mean(RT), sd_RT = sd(RT), median_RT = median(RT),  number = n(), min_RT = min(RT),
            max_RT = max(RT), IQR_RT = IQR(RT))


#scatter plot
tidy_data %>%
  ggplot(aes(y = RT, x = Prime:Target, colour = Prime:Target)) +
  geom_jitter(alpha = .25) +
  stat_summary(fun.data = mean_cl_boot, colour = "grey", size = .8) +
  guides(colour = FALSE) +
  theme_classic() +
  ggtitle("Examing the Effect of Priming",) +
  labs(x = NULL, y = "Reaction Time",
       caption = "Paticipants reaction times to a target, number or letter, 
       after a display of a congruent or incongruent prime, number or letter. 
       The first word in the graph indicates the prime and the second 
       indicates the target e.g. Letter:Number indicates the prime was a letter
       and the target was a number.") +
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 0.5, hjust = .5)) +
  theme(axis.text.x = element_text(size = 10)) +
  theme(plot.title = element_text(face = "bold",
                                  size = 14, hjust = .5), 
        plot.caption = element_text(hjust = .5, face = "italic")) +
  theme(text = element_text( family = "Times New Roman"))

  

#boxplot
tidy_data%>% 
  ggplot(aes(x = Prime:Target, y = RT, fill = Prime:Target)) + 
  geom_boxplot(alpha = .5) +
  theme_classic() +
  theme(legend.position = "none") +
  geom_boxplot_pattern(pattern_color = "white",
                       pattern_fill = "black",
                       aes(pattern = Prime:Target)) +
  theme(axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5, hjust = .5)) +
  labs(title = "Examining the Effect of Priming", 
       y = "Reaction Time", 
       x = NULL) + 
  theme(plot.title = element_text(face = "bold",
                                  size = 14, hjust = .5)) +
  theme(text = element_text( family = "Times New Roman"))
 
#violin plot
tidy_data %>% 
  ggplot(aes(y = RT, x = fct_reorder(Prime:Target, .fun = mean, RT), 
             colour = Prime:Target)) + 
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  geom_boxplot(alpha = .3) +
  theme_clean() +
  theme(axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5, hjust = .5)) +
  guides(colour = FALSE) +
  labs(title = "Examining the Effect of Priming", 
       y = "Reaction Time", 
       x = NULL) +
  theme(plot.title = element_text(face = "bold",
                                  size = 14, hjust = .5)) +
  theme(text = element_text( family = "Times New Roman")) 
  

#density plot
tidy_data %>%
  ggplot(aes(x = RT, fill = Prime:Target)) +
  geom_density_pattern(
                       pattern_fill = "black",
                       alpha = 0.5,
                       pattern_alpha = 0.5,
                       aes(pattern = Prime:Target)) +
  theme_classic() +
  theme(legend.key.size = unit(.4, 'cm')) +
  labs(x = "Reaction Time", y = "Density") +
  theme(text = element_text( family = "Times New Roman"))

#density plot, facet wrap
tidy_data %>%
  ggplot(aes(x = RT, fill = Prime:Target)) +
  geom_density_pattern(pattern_fill = "black",
                       aes(pattern = Prime:Target)) +
  theme(legend.key.size = unit(.5, 'cm')) +
  guides(pattern = FALSE, fill = FALSE) +
  theme_clean() +
  facet_wrap(~ Prime:Target, scales = "free") +
  labs(x = "Reaction Time", y = "Density") +
  theme(text = element_text( family = "Times New Roman"))
 
  view(tidy_data)
  view(long_data)
  
#line graph
tidy_data %>%
  ggplot(aes(y = RT, x = ID, colour = Prime:Target)) + 
  geom_line() +
  geom_point(alpha = .2) +
  coord_polar() +
  theme_classic() +
  theme(text = element_text( family = "Times New Roman")) +
  theme(legend.key.size = unit(.4, 'cm')) +
  labs(x = "Participant", y = "Reaction Time") 


#looking at the four conditions individually
p1 <- tidy_data %>%
  filter(Prime == "Letter" & Target == "Letter") %>%
  ggplot(aes(x = RT, y = ID, colour = Target)) +
  geom_point() +
  theme_classic() +
  scale_colour_manual(values=c("magenta")) +
  guides(colour = FALSE) +
  ggtitle("Letter to Letter") +
  labs(x = "", y = "") +
  theme(plot.title = element_text(face = "bold.italic",
                                  size = 11, hjust = .5)) +
  scale_x_continuous(label = function(x) {return(paste(x, "ms"))}) +
  coord_flip() +
  theme(text = element_text( family = "Times New Roman"))

p2 <- tidy_data %>%
  filter(Prime == "Letter" & Target == "Number") %>%
  ggplot(aes(x = RT, y = ID, colour = Target)) +
  geom_point() +
  theme_classic() +
  scale_color_manual(values=c("skyblue")) +
  guides(colour = FALSE) +
  ggtitle("Letter to Number") +
  labs(x = "", y = "") +
  theme(plot.title = element_text(face = "bold.italic",
                                  size = 11, hjust = .5)) +
  scale_x_continuous(label = function(x) {return(paste(x, "ms"))}) +
  coord_flip() +
  theme(text = element_text( family = "Times New Roman"))

p3 <- tidy_data %>%
  filter(Prime == "Number" & Target == "Number") %>%
  ggplot(aes(x = RT, y = ID, colour = Target)) +
  geom_point() +
  theme_classic() +
  scale_color_manual(values=c("darkorchid")) +
  guides(colour = FALSE) +
  ggtitle("Number to Number") +
  labs(x = "", y = "") +
  theme(plot.title = element_text(face = "bold.italic",
                                  size = 11, hjust = .5)) +
  scale_x_continuous(label = function(x) {return(paste(x, "ms"))}) +
  coord_flip() +
  theme(text = element_text( family = "Times New Roman"))

p4 <- tidy_data %>%
  filter(Prime == "Number" & Target == "Letter") %>%
  ggplot(aes(x = RT, y = ID)) +
  geom_point(aes(colour = Target)) +
  theme_classic() +
  scale_color_manual(values=c("orangered")) +
  guides(colour = FALSE) +
  ggtitle("Number to Letter",) +
  labs(x = "", y = "") +
  theme(plot.title = element_text(face = "bold.italic",
                                  size = 11, hjust = .5)) +
  scale_x_continuous(label = function(x) {return(paste(x, "ms"))} ) +
  coord_flip() +
  theme(text = element_text( family = "Times New Roman"))

#displaying the four plot together
grid.arrange(p1, p2, p3, p4, ncol=2)







#### dont think really needed, dont show anything else?
tidy_data %>%
  filter(Prime == "Letter") %>%
  ggplot(aes(x = RT, y = ID, colour = Target)) +
  geom_point() +
  theme_classic() +
  labs(title = "Influence of Priming",
       x = "Reaction Time", 
       y = "Participant",
       caption = "Paticipants reaction times to both targets, letter and number, 
       with the prime as a letter.") +
  theme(plot.title = element_text(hjust = .5, size = 14, face = "bold"),
        plot.caption = element_text(hjust = .5, face = "italic"),
        axis.title = element_text(size = 10)) +
  theme(text = element_text( family = "Times New Roman")) +
  scale_color_manual(values=c("magenta", "skyblue"))

tidy_data %>%
  filter(Prime == "Number") %>%
  ggplot(aes(x = RT, y = ID, colour = Target)) +
  geom_point() +
  theme_classic() +
  labs(title = "Influence of Priming", 
       x = "Reaction Time", 
       y = "Participant",
       caption = "Participants reaction times to both targets, letter and number,
       with the prime as a number.") +
  theme(plot.title = element_text(hjust = .5, size = 14, face = "bold"),
        plot.caption = element_text(hjust = .5, face = "italic"),
        axis.title = element_text(size = 10)) +
  theme(text = element_text( family = "Times New Roman")) +
  scale_color_manual(values=c("orangered", "orchid")) 



### radar chart attempt
long_data %>%
  ggplot(aes(x = Condition, y = RT, group = ID, colour = Condition)) + 
  geom_point(size=5) + 
  geom_line() + 
  xlab("Decils") + 
  ylab("% difference in nº Pk") + 
  ylim(-50,25) + ggtitle("CL")  + 
  geom_hline(aes(yintercept=0), lwd=1, lty=2) + 
  scale_x_discrete(limits = c("RT")) +
  coord_polar()


tidy_data %>%
  ggplot(aes(x = RT, y = fct_reorder(Prime, .fun = mean, RT))) +
  geom_density_ridges(height = .5, aes(fill = Prime)) +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  guides(fill = FALSE) + 
  labs(x = "Engine Displacement (litres)",
       y = NULL)

tidy_data %>% #??????????????????????????????????///
  ggplot(aes(x = RT, y = fct_reorder(Prime, .fun = mean, RT))) +
  geom_density_ridges(height = .5, aes(fill = Target)) +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(x = "Engine Displacement (litres)",
       y = "Prime")




  
 

  