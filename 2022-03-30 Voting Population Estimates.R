# Analysis of Estimates of Voting Population for 2021
# Japbir Gill

# Sources ----
# Estimates of Voting Population: https://www.federalregister.gov/documents/2022/03/30/2022-06654/estimates-of-the-voting-age-population-for-2021
# 2020 Election Data: https://www.nbcnews.com/politics/2020-elections/president-results

# Clearing Evironment ----
rm(list = ls())

# Setting Working Drives ----
datawd <- "C:/Users/japbi/Dropbox/Data Projects/Datasets/Voting"
outputwd <- "C:/Users/japbi/Dropbox/Data Projects/Daily Practice"

# Libraries ----
library(ggplot2)
library(psych)
library(readxl)
library(usmap)

# Loading Datset ----
setwd(datawd)
voting <- read_excel("Elections Data.xlsx", sheet = "Data", col_names = TRUE) 
head(voting)

# Creating Variables----
names(voting)[1] <- 'state'
names(voting)[6] <- 'pct_GOP'
names(voting)[5] <- 'pct_DEM'
names(voting)[4] <- 'voter_turnout'
names(voting)[7] <- 'party_winner'
names(voting)[2] <- 'voting_pop'
names(voting)[3] <- 'EV_votes'
voting$pct_pop <- voting$voting_pop / sum(voting$voting_pop) 
voting$expectedvote <- voting$voting_pop * voting$voter_turnout
voting$pct_expvote <- voting$expectedvote / sum(voting$expectedvote)
voting$pct_EV <- voting$EV_votes / sum(voting$EV_votes)
voting$voter_pwr <- voting$pct_EV/voting$pct_pop
voting$adjvoter_pwr <- voting$pct_EV/voting$pct_expvote
voting$absval <- abs(voting$voter_pwr-voting$adjvoter_pwr)

# Graphing ----

## Scatterplot of Voter Power Index and % Vote for Republican Party
ggplot(voting, aes(x=adjvoter_pwr, y=voter_pwr, group = party_winner)) +
  geom_point(aes(shape=party_winner, color=party_winner)) +
  labs(title = "Relationship of Estimated State Voter Power Indices \nby State and 2020 Election Winner", 
       subtitle = "Proportional Electoral Votes Over Proporitonal Voter Population Estimate" , 
       caption = "\n Source: US Department of Commerce Population Estimates,\n Electoral College 2020 Election Results\n Labeled states have absolute differences > 0.25") +
  ylab("Estimated Voter Power") +
  xlab("Estimated Voter Power Adjusted for Voter Turnout") +
  scale_color_manual(values = c("darkblue","darkred")) +
  geom_text(data = subset(voting, absval > 0.25), aes(adjvoter_pwr, voter_pwr, label=state),position = position_jitter(width = 0.15, height = 0.15)) +
  theme(legend.title = element_blank(), legend.position = c(0.8,0.1))
setwd(outputwd)
ggsave(filename = "2022-03-30 Voting Pop Est Indices Scatter.png", scale = 1.25)
  
## Voter Power Map
plot_usmap(data = voting, values="voter_pwr", color="black") +
  labs(title = "Expected Voter Power Index", 
       subtitle = "Proportional Electoral Votes Over Proporitonal Voter Population Estimate" , 
       caption = "Source: US Department of Commerce Population Estimates, Electoral College 2020 Election Results") +
  scale_fill_gradient2(name = " ", low="red", mid="green", high="blue", limits=c(0.7,3.5)) +
  theme(legend.position = "right", legend.direction = "vertical",
        plot.title = element_text(hjust = 0, size = 24, face = "bold.italic")) 
ggsave(filename = "2022-03-30 Voter Power Index Map.png", bg="white")

## Adjusted Voter Power Map 
plot_usmap(data = voting, values="adjvoter_pwr", color="darkgreen") +
  labs(title = "Adjusted Expected Voter Power Index", 
       subtitle = "Proportional Electoral Votes Over Proporitonal Voter Population Estimate \n(Adjusted for Voter Turnout)" , 
       caption = "Source: US Department of Commerce Population Estimates, Electoral College 2020 Election Results") +
  scale_fill_gradient2(name = " ", low="red", mid="green", high="blue", limits=c(0.7,3.5)) +
  theme(legend.position = "right", legend.direction = "vertical",
        plot.title = element_text(hjust = 0, size = 24, face = "bold.italic")) 
ggsave(filename = "2022-03-30 Adj Voter Power Index Map.png", bg="white")

## Bar Chart of Voter Power by State and Election
ggplot(data = voting, aes(x=reorder(state, voter_pwr), y=voter_pwr, fill = party_winner)) +
  geom_bar(stat = "identity") +
  labs(title = "Voter Power Index\n by Sate and 2020 Election Result", 
       subtitle = "Proportional Electoral Votes Over Voter Population Estimate" , 
       caption = "Source: US Department of Commerce Population Estimates,
                  Electoral College 2020 Election Results") +
  theme_minimal() + 
  scale_fill_manual(values = c("darkblue","darkred")) +
  xlab("State") + ylab("Voter Power Index") +
  theme(legend.title = element_blank(), legend.position = c(0.8,0.2),
        axis.text.x = element_text(size = 7, hjust = 0.5)) +
  coord_flip()
ggsave(filename = "2022-03-30 Voter Turnout by State and Election Result.png", bg="white", height = 7, width = 6)

## Bar Chart of Adjusted  Voter Power by State and Election
ggplot(data = voting, aes(x=reorder(state, adjvoter_pwr), y=adjvoter_pwr, fill = party_winner)) +
  geom_bar(stat = "identity") +
  labs(title = "Voter Power Index by Sate and 2020 Election Result \n Adjusted for Voter Turnout", 
       subtitle = "Proportional Electoral Votes Over Voter Population Estimate" , 
       caption = "Source: US Department of Commerce Population Estimates, Electoral College 2020 Election Results") +
  theme_minimal() + 
  scale_fill_manual(values = c("darkblue","darkred")) +
  xlab("State") + ylab("Adj Voter Power Index") +
  theme(legend.title = element_blank(), legend.position = c(0.8,0.2),
        axis.text.x = element_text(size = 7, hjust = 0.5)) +
  coord_flip()
ggsave(filename = "2022-03-30 Adj Voter Turnout by State and Election Result.png", bg="white", height = 7, width = 6)
