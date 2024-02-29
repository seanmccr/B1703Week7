# ----- B1703 Week 7 | Advanced Plots in R | 28.02.2024 -----
# ----- NHL Data -----
# ----- 1. Loading Data and Libraries -----
NHL <- readxl::read_xlsx("/Users/seanmccrone/Desktop/MASTERS DEGREE/Course Material/B1703/Week 7 /Practical Files/NHL data.xlsx")

library(ggplot2)
library(tidyverse)

# ----- 2. Creating Correlation Plot -----
Correlationplot <- NHL %>%
  ggplot(aes(x=G, y=A))+
  geom_point()
Correlationplot  

# ----- 3. What can we see about it? -----

# There is a large cloud of points not really indicating a correlation
# There is one extreme outlier with one player scoring almost 2000 assists and 900 goals
# The majority of players record more assists than goals

# ----- 4. Adjusting Correelation Plot 

Correlationplot <- Correlationplot + 
  geom_point(aes(colour=GP, size=P))
Correlationplot 

# ----- 5. Changing the colour scheme -----

Correlationplot <- Correlationplot + 
  scale_color_gradient2(low = "darkblue", mid = "white", high = "orange", midpoint = 1250)
Correlationplot 

# ----- 6. Adding player names -----

Correlationplot2 <- Correlationplot +
  geom_text(aes(label = Player))  # Adjust vjust for vertical positioning
Correlationplot2

# ----- 7. Dataframe based on best performing assists -----

TopPlayer <- NHL %>%
  slice_max(A)

# ----- 8. Updated Correlation Plot to include top player name only -----

Correlationplot <- Correlationplot +
  geom_text(data=TopPlayer, aes(label = Player), vjust = 0.2, hjust=1.1)  # Adjust vjust and hjust for vertical and horizontal positioning
Correlationplot

# ----- 9. Renaming legend and axis titles -----

Correlationplot <- Correlationplot +
  labs(x= "Goals",
       y= "Assists",
       size = "Points",
       color= "Games Played")
Correlationplot

# ----- 10. Adding variables -----

NHL <- NHL %>%
  mutate(APG = A/GP,
         GPG = G/GP,
         PPG = P/GP)

# ----- 11. Pivoting variables -----
# We will need to pivot our data so they are seen as one variable with two categories (i.e. APG and GPG)
NHL_Pivot <- NHL %>% 
  pivot_longer(19:20, names_to="Variable", values_to="PerGameRatio")

# ----- 12. Stacked Bar Chart -----
NHL_Pivot_Sub <- NHL_Pivot %>%
  arrange(desc(PPG)) %>%
  slice_head(n=40) #remember each player has 2 entries so to get the top 20 players we will need to select the first 40 rows.

StackedBar <- NHL_Pivot_Sub %>%
  ggplot(aes(y=reorder(Player,PPG), x=PerGameRatio, fill=Variable),position='fill')+
  geom_col()+
  labs(y="",x="Per Game Ratio")
StackedBar

# ----- 13. Stacked Point Chart -----
# Use reorder(Player,PPG) so that we re-order the players descindingly from player with most PPG to player with lowest PPG ratio
Pointchart <- NHL_Pivot_Sub %>%
  ggplot(aes(y=reorder(Player,PPG), x=PerGameRatio, color=Variable))+
  geom_point()+
  labs(y="",x="Per Game Ratio")
Pointchart


# ----- 14. Regression/Association Plot -----
# Use expland_limits to provide room to annotate the graph if necesary 
Corr <- NHL %>%
  ggplot(aes(Shots,G,color=Pos))+
  geom_point()+
  labs(y="Goals")+
  scale_color_discrete(name = "Position", labels = c("Center", "Defense", "Left Wing", "Right Wing"))+
  expand_limits(x=c(0,8000),y=c(0,1000))
Corr

# ----- 15. Add Regression Lines -----

CorrReg <- Corr+
  geom_smooth(method = "lm", formula= y ~ 0+x, se = FALSE)
CorrReg

# ----- 16. Annotating scatter plot -----

Corrquad <- Corr+
  geom_hline(yintercept=mean(NHL$G, na.rm=TRUE), linetype="dashed",color="grey")+
  geom_vline(xintercept=mean(NHL$Shots, na.rm=TRUE),linetype="dashed",color="grey")+
  annotate("text", x=c(1000,6500, 6500, 1000), y=c(900,900, 100, 100), label= c("High accuracy","High accuracy
             and efficiency", "High efficiency", "Low accuracy
             and efficiency"), colour="darkblue")
Corrquad


# ----- 10K Data -----
TenK <- readxl::read_xlsx("/Users/seanmccrone/Desktop/MASTERS DEGREE/Course Material/B1703/Week 7 /Practical Files/TenK.xlsx")

# ----- 17. Create Line Chart of Time w/Trendlines -----
#
TenK$Year <-  as.numeric(str_sub(TenK$Olympics, start=-4)) #I want to extract the year from the string variable `Olympics` first.

TimePlot <- TenK %>%
  ggplot(aes(Year, Men))+
  geom_line(colour="darkblue")+
  xlim(1912,2020)+
  scale_x_continuous(breaks=seq(1912,2020,16), minor_breaks = waiver())+
  labs(x="Olympic Year", y="Winning Time (minutes)")+
  theme_minimal()
TimePlot

# Plot with Trendline 
TimePlot2 <- TimePlot +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth",
              se=FALSE, 
              colour="black",
              linetype=2) 
TimePlot2

# ----- 18. Takeaways -----

# There is a downward trend in finishing times (i.e. athletes are getting faster)
# 1968 (Mexico City) very clear outlier
# 2012 and 2020 also seem to break the trend

# ----- 19. Create Altitude Plot & Combine w/Patchwork -----

AltitudePlot <- TenK %>%
  ggplot(aes(Year, Altitude))+
  geom_line(colour="darkblue")+
  xlim(1912,2020)+
  scale_x_continuous(breaks=seq(1912,2020,16), minor_breaks = waiver())+
  labs(x="Olympic Year", y="Altitude")+
  theme_minimal()
AltitudePlot

library(patchwork)
TimePlot + AltitudePlot #this will only work if you have patchwork installed


# ----- 20. Overlap -----

coeff<-max(TenK$Altitude)/(max(TenK$Men)-min(TenK$Men)) # we first need to create a coefficient to scale the values to the same axis (if we don't do this we will lose all detail in the Race time graph)

CombinedPlot <- TenK %>%
  ggplot(aes(Year))+
  geom_line(aes(y=Men),colour="darkblue")+
  geom_line(aes(y=(Altitude/coeff)+min(Men)),colour="orange")+ # I divide the altitude value by the coefficient and add the fastest race time to the division. This ensures our altitude values fall between 27 and 31.
  xlim(1912,2020)+
  scale_x_continuous(breaks=seq(1912,2020,16), minor_breaks = waiver())+
  scale_y_continuous(name = "Winning race time (minutes)",
                     sec.axis = sec_axis(~((.-min(TenK$Men))*coeff),name="Altitude (meters)"))+    # Here I add the second axis and ensure we show the actual altitude values not the adjusted ones. I do this by reversing the previous calculation (i.e. (the adjusted value - the fastest time) * coefficient)
  theme_minimal()+
  theme(
    axis.title.y = element_text(color = "darkblue", size=13),
    axis.title.y.right = element_text(color = "orange", size=13)
  )+geom_label(data = TenK,aes(x=1980, y =(max(Altitude)/coeff)+min(Men)), label = paste("Altitude: ",max(TenK$Altitude),"M"))

CombinedPlot
