
######################### File Name : STAT.R ################################################
## Description : This file applies various statistical analysis techniques for initial  
##               exploratory analysis of data.
#############################################################################################

rm(list=ls())
setwd("C:\\Users\\DELL\\Google Drive\\DPA Project\\Project")
load("Data_Cleaning.RData")

library(grid)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(gtable)
library(compare)
library(maps)
library(wordcloud2)

#Finding correlations between top 20 drugs Where opiod there is Opioid Claim
drugs <- names(which(sapply(Prescriber, is.numeric)))
drugs<-drugs[drugs!="Opioid_Claims"]
n <- 20

Prescriber.Opiod.Claim<- Prescriber[Prescriber$Opioid_Claims != 0,]

corMatrix.Opiod.Claim <- cor(Prescriber.Opiod.Claim[drugs])
corMatrix.Opiod.Claim[lower.tri(corMatrix.Opiod.Claim,diag=TRUE)]=NA
corMatrix.Opiod.Claim=as.data.frame(as.table(corMatrix.Opiod.Claim))
corMatrix.Opiod.Claim=na.omit(corMatrix.Opiod.Claim)
corMatrix.Opiod.Claim=corMatrix.Opiod.Claim[order(-abs(corMatrix.Opiod.Claim$Freq)),]

corplot<-ggplot(data = corMatrix.Opiod.Claim[1:n,], aes(x=Var1, y=Var2, fill = Freq)) + geom_tile() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab('Drugs') + ylab('Drugs')

corplot
# There is a high correlation between Potassium.Chloride and Furosemide.
# This combination is usually prescribed to treat fluid retention (edema)
# in people with congestive heart failure, liver disease.
# Similarly other drugs too are prescribed in critical situations only, as they have big side-effects.


#Bar and Line Graph shoing reationship between States and #of Presriptions and States and
#   Opioid Related Deaths.
#Grouping the data by State to match the Demographic dataset format 
StateDeathData <- Prescriber
StateDeathData$Opioid.Prescriber<-as.numeric(as.character(StateDeathData$Opioid.Prescriber))
StateDeathData <- StateDeathData %>%
   group_by(State) %>%
   dplyr::summarise(Count = n(), Opioid.Prescriber = sum(Opioid.Prescriber))

#Merging the Demographic Death Data with the grouped data.
StateDeathData <- DemogDrugState %>%
  select(State =`Postal Code`, Deaths=Deaths) %>%
  merge(StateDeathData, by = "State") %>%
  select(State, Count, Opioid.Prescriber, Deaths)

StateDeathData=na.omit(StateDeathData)

#Arranging StateData it in increasing order of #of opioid prescriptions.
StateOpioidData<-Prescriber
StateOpioidData <- within(StateOpioidData, 
                   State <- factor(State, 
                                      levels=names(sort(table(State), 
                                                        decreasing=FALSE))))

#Filling the Bar plot with #of Opioid prescribers and non opioid prescribers, to understand if there 
#   exists a relation between Opioid related deaths and opioid prescription.

OpioidPlot <- ggplot(StateOpioidData, aes(x = State, fill = Opioid.Prescriber,width=.15)) + 
  geom_bar() + theme(panel.background = element_rect(fill = NA)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme(legend.position = c(0.25, 0.75))

DeathPlot <- ggplot(StateDeathData,aes(x = reorder(State, Count))) +
  geom_line(aes(y=Deaths,  group=1), linetype="dashed", color="black", size=1.2) +
  geom_point(aes(y=Deaths), color="steelblue", size=3) +
  theme_bw() %+replace% 
  theme(panel.background = element_rect(fill = NA)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("State")

#Overlapping the  two plots with 2 y-axes.
g1 <- ggplot_gtable(ggplot_build(DeathPlot))
g2 <- ggplot_gtable(ggplot_build(OpioidPlot))

pp <- c(subset(g1$layout, name == "panel", se = t:r))
g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                     pp$l, pp$b, pp$l)

ia <- which(g2$layout$name == "axis-l")
ga <- g2$grobs[[ia]]
ax <- ga$children[[2]]
ax$widths <- rev(ax$widths)
ax$grobs <- rev(ax$grobs)
ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
grid.draw(g) 
# It can be seen from the plot that there exists a strong relationship between
# Opioid  related deaths and Opioid Prescription in a State, with higher correlation in
# California, Texas, New Mexico Florida but lesser in New York.


#Barplot to understand if there is any relationship between Opioid Prescription and Gender of Prescriber
GenderPlot <- ggplot(Prescriber, aes(x = Gender, fill = Opioid.Prescriber)) + 
  geom_bar(width = 0.5) + ylab('No. of Prescribers')
GenderPlot
#The ratio of No. of Opioid Prescribers to non opioid prescribers seems to be higher in females.

#Plotting the death per capita on the state to analyse if there exists a relationship between the
#   the geographical location of state and opioid related deaths
all_states <- map_data("state")
DemogDrugState<-DemogDrugState[DemogDrugState$State != c("District of Columbia"),]
od <- DemogDrugState
od$State <- as.factor(DemogDrugState$State)
od$DeathsPerCapita<- as.numeric(DemogDrugState$Deaths)/as.numeric(DemogDrugState$Population)
od$Abbrev<-DemogDrugState$`Postal Code`
StateMap <- mutate(od,state.lower=tolower(State), Population=as.numeric(Population)) %>%
  merge(all_states,by.x="state.lower",by.y="region") %>%
  select(-subregion,-order) %>% 
  ggplot() + geom_map(map=all_states, aes(x=long, y=lat, map_id=state.lower,fill=DeathsPerCapita) )  + ggtitle("U.S. Opiate O.D. Rate") +
  geom_text(data=data.frame(state.center,od$Abbrev),aes(x=x, y=y,label=od.Abbrev),size=3) +
  scale_fill_continuous(low='gray85', high='steelblue',guide=guide_colorbar(ticks=FALSE,barheight=1,barwidth=15,title.vjust=.8,values=c(0.2,0.3)),name="# of Deaths") +
  theme(panel.background = element_rect(fill = NA))+theme(axis.text=element_blank(),axis.title=element_blank(),axis.ticks=element_blank(),legend.position="bottom",plot.title=element_text(size=20))

StateMap
# There seems to be higher Opioid related deaths percent in the boundary and closer to boundary states
# with exeption to NorthDakota. Also there seems to be stronger correlation in the state of New Mexico and West Virginia.


#Plot to understand the relationship between age, race and opioid related deaths
df<-DemogDrugStateRaceAge
DemographicPlot <- ggplot(DemogDrugStateRaceAge, aes(x = Ten.Year.Age.Groups, fill = Race)) + 
  geom_bar(aes(y=100*as.numeric(as.character(df$Deaths))/as.numeric(as.character(df$Population))), stat = 'identity')+ xlab("Age Groups") +
  ylab("Percent Deaths") +theme(axis.text.x = element_text(angle = 90, hjust = 1))
DemographicPlot
#Across all age groups there seems to be higher opioid related deaths in white people. 


# Create wordcloud of frequencies
# wordcloud of State freq -- Count State for 1/Count State for total
OpioidPrescriptionRatio_per_State <- Prescriber %>%
  group_by(State) %>%
  summarise(opioid_count = sum(Opioid.Prescriber == 1),
            total_count = n(),
            Opioid_Total = opioid_count / total_count) %>%
  select(State, Opioid_Total)


# wordcloud of Specialty freq -- Count Specialty for 1/Count Speciality for total
OpioidPrescriptionRatio_per_Specialty <- Prescriber %>%
  group_by(Specialty) %>%
  summarise(opioid_count = sum(Opioid.Prescriber == 1),
            total_count = n(),
            Opioid_Total = opioid_count / total_count) %>%
  select(Specialty, Opioid_Total)


# Print wordcloud for Opioid Prescriptions per state
wordcloud2(OpioidPrescriptionRatio_per_State, 
           color='random-dark',
           size = 0.5, shape = 'circle',
           minRotation = -pi/6, maxRotation = -pi/6, rotateRatio = 1)


# Print wordcloud for Opioid Prescriptions per specialty
wordcloud2(OpioidPrescriptionRatio_per_Specialty, 
           color='random-dark',
           size = 0.5, shape = 'circle')

################################### Save and Cleanup ###############################

# Save Image File
save.image(file="STAT.R")

# Detach libraries
detach("package:grid", unload = TRUE)
detach("package:corrplot", unload = TRUE)
detach("package:ggplot2", unload = TRUE)
detach("package:gridExtra", unload = TRUE)
detach("package:gtable", unload = TRUE)
detach("package:dplyr", unload = TRUE)
detach("package:compare", unload = TRUE)
detach("package:maps", unload = TRUE)
detach("package:wordcloud2", unload = TRUE)

# Remove all
rm(list=ls())