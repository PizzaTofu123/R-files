###############################################################
# Limiting the teams for the filter function for the Line Chart
###############################################################
setwd("D:/Desktop/R files/3154 Files")
nba_team = read.csv("Team Stats Per Game.csv")
new_data = nba_team[nba_team$season >= 1990, ]
# print data
write.csv(new_data, "D:\\Desktop\\R files\\3154 Files\\Team Stats Per Game 1990-2022.csv", row.names=FALSE)

###############################################################
# Making the Appropriate data for the making of the Radar Chart
###############################################################
setwd("D:/Desktop/R files/3154 Files")
players = read.csv("Player Per Game.csv")
players[is.na(players)] <- 0
name = c(
  "Kobe Bryant",
  "Magic Johnson",
  "Kareem Abdul-Jabbar",
  "Shaquille O'Neal",
"Jerry West",
"James Worthy",
"LeBron James",
"Pau Gasol",
"Gail Goodrich",
"Michael Cooper",
"Byron Scott",
"Bob McAdoo",
"Robert Horry",
"Jamaal Wilkes",
"Derek Fisher"
)
columns = c('name', 'metric', 'value')
Top_15 = data.frame(matrix(nrow = 0, ncol = length(columns)))
colnames(Top_15) = columns
for (i in 1:15){
  p_name = name[i]
  p_data = players[players$player == p_name,]
  Top_15[nrow(Top_15)+1, ] = c(name[i], 'Points Per Game', 
                               (sum(p_data$pts_per_game)/nrow(p_data)))
  Top_15[nrow(Top_15)+1, ] = c(name[i], 'Field Goal Percentage', 
                               (sum(p_data$fg_percent)/nrow(p_data)))
  Top_15[nrow(Top_15)+1, ] = c(name[i], '3 Point Percentage', 
                               (sum(p_data$x3p_percent)/nrow(p_data)))
  Top_15[nrow(Top_15)+1, ] = c(name[i], 'Rebounds Per Game', 
                               (sum(p_data$trb_per_game)/nrow(p_data)))
  Top_15[nrow(Top_15)+1, ] = c(name[i], 'Assists Per Game', 
                               (sum(p_data$ast_per_game)/nrow(p_data)))
  Top_15[nrow(Top_15)+1, ] = c(name[i], 'Steals Per Game', 
                               (sum(p_data$stl_per_game)/nrow(p_data)))
}
write.csv(Top_15, "D:\\Desktop\\R files\\3154 Files\\Ranked Players Long.csv", row.names=FALSE)

setwd("D:/Desktop/R files/3154 Files")
life = read.csv("life expectancy.csv",header=TRUE, check.names=FALSE)
life = life[life$Year == 2019, ]
life[is.na(life)] <- 0
write.csv(life, "D:\\Desktop\\R files\\3154 Files\\life_expectancy_2019.csv", row.names=FALSE)

setwd("D:/Desktop/R files")
dev = read.csv("world_bank_development_indicators.csv",header=TRUE, check.names=FALSE)
dev$year = as.numeric(substr(dev$date,1,4))
dev$year
dev[is.na(dev)] <- 0
dev_no_na = read.csv("world_bank_development_indicators_cleaned_No_NA.csv",header=TRUE, check.names=FALSE)
dev_no_na = dev_no_na[dev_no_na$country == "Indonesia" 
          | dev_no_na$country == "World" 
          | dev_no_na$country == "Vietnam" 
          | dev_no_na$country == "Thailand" 
          | dev_no_na$country == "Malaysia" 
          | dev_no_na$country == "Philippines" 
          | dev_no_na$country == "Brunei Darussalam" 
          | dev_no_na$country == "Myanmar" 
          | dev_no_na$country == "Thailand" 
          | dev_no_na$country == "Cambodia"
          | dev_no_na$country == "Singapore"
          | dev_no_na$country == "Laos"
          | dev_no_na$country == "Timor-Leste"
          | dev_no_na$country == "Brunei"
          ,]
dev_no_na[dev_no_na$country == 'Brunei',]
write.csv(dev_no_na, "D:\\Desktop\\R files\\world_bank_development_indicators_cleaned_No_NA_SEA.csv", row.names=FALSE)



dev$
  dev = dev[dev$country != "Africa Eastern and Southern" 
            & dev$country != "Africa Western and Central" 
            & dev$country != "Arab World" 
            & dev$country != "Early-demographic dividend" 
            & dev$country != "East Asia & Pacific" 
            & dev$country != "East Asia & Pacific (IDA & IBRD countries)" 
            & dev$country != "East Asia & Pacific (excluding high income)" 
            & dev$country != "Euro Area" 
            & dev$country != "Europe & Central Asia" 
            & dev$country != "Europe & Central Asia (IDA & IBRD countries)" 
            & dev$country != "Europe & Central Asia (excluding high income countries)" 
            & dev$country != "European Union" 
            & dev$country != "Fragile and conflict affected situations" 
            & dev$country != "Heavily indebted poor countries (HIPC)" 
            & dev$country != "High income" 
            & dev$country != "IBRD only" 
            & dev$country != "IDA & IBRD total" 
            & dev$country != "IDA blend" 
            & dev$country != "IDA only" 
            & dev$country != "IDA total"
            & dev$country != "Late-demographic dividend" 
            & dev$country != "Latin America & Caribbean" 
            & dev$country != "Low & middle income" 
            & dev$country != "Lower middle income" 
            & dev$country != "Upper middle income" 
            & dev$country != "Latin America & Caribbean (excluding high income)" 
            & dev$country != "Latin America & the Caribbean (IDA & IBRD countries)" &dev$country != "Least developed countries: UN classification" &
              dev$country != "Middle East & North Africa" &dev$country != "Middle East & North Africa (IDA & IBRD countries)" &dev$country != "Middle East & North Africa (excluding high income)	" &
              dev$country != "Middle income" & dev$country != "North America" &dev$country != "Not classified" &dev$country != "OECD members" &dev$country != "Other small states" &dev$country != "Pacific island small states" &
              dev$country != "Post-demographic dividend" & dev$country != "Pre-demographic dividend" &dev$country != "South Asia" & dev$country != "South Asia (IDA & IBRD)" &dev$country != "Sub-Saharan Africa" &
              dev$country != "Sub-Saharan Africa (IDA & IBRD countries)" & dev$country != "Sub-Saharan Africa (excluding high income)" & dev$country != "West Bank and Gaza",]




