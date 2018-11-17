# 1. WHO dataset
# 

WHO = read.csv("WHO.csv")
# d. country with the lowest literacy
Lowest_LitRate<- which.min(WHO$LiteracyRate)
  WHO$Country[Lowest_LitRate]
  
# e. Richest country in Europe based on GNI
  WHO.Europe = subset(WHO, Region == "Europe")
    maxGNI_Country <- which.max(WHO.Europe$GNI)
       WHO.Europe$Country[maxGNI_Country]

# f. Mean Life expectancy of countries in Africa
       WHO.Africa<- subset(WHO, Region == "Africa")
            mean(WHO.Africa$LifeExpectancy)

# g. Number of countries with population greater than 10,000
            Ten_K_plus_pop<- WHO$Population > 10000
            table(Ten_K_plus_pop) ["TRUE"]
          
# h. Top 5 countries in the Americas with the highest child mortality
  WHO.Americas <- subset(WHO, Region == "Americas")
           Child_mortAM <- order(WHO.Americas$ChildMortality, decreasing = TRUE)
# 
           Top5_Child_mortAM <- Child_mortAM[1:5]
           
           WHO.Americas$Country[Top5_Child_mortAM]
           
           
# 2. NBA dataset (Historical NBA Performance.xlsx)
# 
NBA = read.csv("Historical NBA Performance.csv")
# a. The year Bulls has the highest winning percentage 
#R script involving Team == "Bulls" and the Winning Percentage column,
#that when execued will show 1995-96 as the answer
Bulls <- subset(NBA, Team == "Bulls")
    Bulls_WP_order <- order(Bulls$Winning.Percentage, decreasing = TRUE)
         Bulls$X...Year[Bulls_WP_order][1]

# b. Teams with an even win-loss record in a year
         winrate_par <- subset(NBA, Winning.Percentage == 0.5)
              Unq_winrate_par <- unique(winrate_par$Team)
        Unq_winrate_par
# 
# 3. Seasons_Stats.csv
# 
STAT = read.csv("Seasons_Stats.csv")

# a. Player with the highest 3-pt attempt rate in a season.
      Three_pt.AttemptRate<- order(STAT$X3P., decreasing= TRUE)
      
          STAT$Player[Three_pt.AttemptRate][1]
      
# b. Player with the highest free throw rate in a season.
#aggregation of data values within a seasons is needed since
#some players play in more than one team in a season.
          
          
          # ag_STAT<- aggregate(STAT, by=list() FUN, simplify = TRUE, drop= TRUE)
          # 
          # 
          # 
          # 1) agregate dataframe, file in a variable
          # 2) get highest freethorw rate, file in a var
          # 3) call player name, [1]

# c. What year/season does Lebron James scored the highest? 
        LBJ_Data<- subset(STAT, Player == 'LeBron James' )
            LBJ_ScoringMax  <-  max(LBJ_Data$PTS, na.rm=TRUE)
        LBJ_MaxScoreYR <- subset(LBJ_Data, PTS== LBJ_ScoringMax)$Year
        
LBJ_MaxScoreYR
      

#   d. What year/season does Michael Jordan scored the highest? 

    MJ_Stats<- subset(STAT, Player == 'Michael Jordan*')
      MJ_ScoringMax <- max(MJ_Stats$PTS, na.rm=TRUE)
    MJ_MaxScoreYR <- subset(MJ_Stats, PTS == MJ_ScoringMax)$Year
    
    MJ_MaxScoreYR
      
  

#   e. Player efficiency rating of Kobe Bryant in the year where his MP is the lowest?
#   
    
    Kobe_Stats <- subset(STAT, Player == "Kobe Bryant")
        subset(Kobe_Stats, MP == min(Kobe_Stats$MP))$PER
  
  
#   4. National Universities Rankings.csv
NatU = read.csv("National Universities Rankings.csv")
# 
# a. University with the most number of undergrads 

NatU[which.max(NatU$Undergrad.Enrollment),]$Name

# b. Average Tuition in the Top 10 University

Top_10_uni_rank <- order(NatU$Rank, decreasing = FALSE)[1:10]
      Top10_Unifees_final <- gsub(pattern = "\\$|\\,", replacement = "", NatU$Tuition.and.fees)[Top_10_uni_rank]
          mean(as.numeric(Top10_Unifees_final))

          