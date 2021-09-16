

# 1- Packages : 

library(tidyverse) 
library(magrittr)
library(DataExplorer)
library(maps)
library(plotly)
library(DT)
library(tidytext)
library(gridExtra)
library(factoextra)
library(scales)
library(caret)
library(gbm)
library(xgboost)
library(Metrics)


# 2-Data Structure:
## Loading Data:
  df <- read.csv("data.csv", encoding = "UTF-8")[-1]
## Data structure :
dim(df)   #size of the dataset

introduce(df) #overall description

plot_intro(df)  #overall visually description 

plot_missing(df)  #Spot the missing value 

# 3- Data Preparation :
## Create League 
bundesliga <- c("1. FC NÃ¼rnberg", "1. FSV Mainz 05", "Bayer 04 Leverkusen", "FC Bayern MÃ¼nchen",
"Borussia Dortmund", "Borussia MÃ¶nchengladbach", "Eintracht Frankfurt",
"FC Augsburg", "FC Schalke 04", "Fortuna DÃ¼sseldorf", "Hannover 96",
"Hertha BSC", "RB Leipzig", "SC Freiburg", "TSG 1899 Hoffenheim",
"VfB Stuttgart", "VfL Wolfsburg", "SV Werder Bremen")
premierLeague <- c(
"Arsenal", "Bournemouth", "Brighton & Hove Albion", "Burnley",
"Cardiff City", "Chelsea", "Crystal Palace", "Everton", "Fulham",
"Huddersfield Town", "Leicester City", "Liverpool", "Manchester City",
"Manchester United", "Newcastle United", "Southampton", 
"Tottenham Hotspur", "Watford", "West Ham United", "Wolverhampton Wanderers")
laliga <- c(
"Athletic Club de Bilbao", "AtlÃ©tico Madrid", "CD LeganÃ©s",
"Deportivo AlavÃ©s", "FC Barcelona", "Getafe CF", "Girona FC", 
"Levante UD", "Rayo Vallecano", "RC Celta", "RCD Espanyol", 
"Real Betis", "Real Madrid", "Real Sociedad", "Real Valladolid CF",
"SD Eibar", "SD Huesca", "Sevilla FC", "Valencia CF", "Villarreal CF")
seriea <- c(
"Atalanta","Bologna","Cagliari","Chievo Verona","Empoli", "Fiorentina","Frosinone","Genoa",
"Inter","Juventus","Lazio","Milan","Napoli","Parma","Roma","Sampdoria","Sassuolo","SPAL",
"Torino","Udinese")
superlig <- c(
  "Akhisar Belediyespor","Alanyaspor", "Antalyaspor","Medipol Basaksehir FK","BB Erzurumspor","Besiktas JK",
  "Bursaspor","Çaykur Rizespor","Fenerbahçe SK", "Galatasaray SK","Göztepe SK","Kasimpasa SK",
  "Kayserispor","Atiker Konyaspor","MKE Ankaragücü", "Sivasspor","Trabzonspor","Yeni Malatyaspor"
)

ligue1 <- c(
  "Amiens SC", "Angers SCO", "AS Monaco", "AS Saint-Étienne", "Dijon FCO", "En Avant de Guingamp",
  "FC Nantes", "FC Girondins de Bordeaux", "LOSC Lille", "Montpellier HSC", "Nîmes Olympique", 
  "OGC Nice", "Olympique Lyonnais","Olympique de Marseille", "Paris Saint-Germain", 
  "RC Strasbourg Alsace", "Stade Malherbe Caen", "Stade de Reims", "Stade Rennais FC", "Toulouse Football Club"
)

eredivisie <- c(
  "ADO Den Haag","Ajax", "AZ Alkmaar", "De Graafschap","Excelsior","FC Emmen","FC Groningen",
  "FC Utrecht", "Feyenoord","Fortuna Sittard", "Heracles Almelo","NAC Breda",
  "PEC Zwolle", "PSV","SC Heerenveen","Vitesse","VVV-Venlo","Willem II"
)

liganos <- c(
  "Os Belenenses", "Boavista FC", "CD Feirense", "CD Tondela", "CD Aves", "FC Porto",
  "CD Nacional", "GD Chaves", "Clube Sport Marítimo", "Moreirense FC", "Portimonense SC", "Rio Ave FC",
  "Santa Clara", "SC Braga", "SL Benfica", "Sporting CP", "Vitória Guimarães", "Vitória de Setúbal"
)

df %<>% mutate(
League = case_when(
Club %in% bundesliga ~ "Bundesliga",
Club %in% premierLeague ~ "Premier League",
Club %in% laliga ~ "La Liga",
Club %in% seriea ~ "Serie A",
Club %in% superlig ~ "SÃ¼per Lig",
Club %in% ligue1 ~ "Ligue 1",
Club %in% liganos ~ "Liga Nos",
Club %in% eredivisie ~ "Eredivisie"),
Country = case_when(
League == "Bundesliga" ~ "Germany",
League == "Premier League" ~ "UK",
League == "La Liga" ~ "Spain",
League == "Serie A" ~ "Italy",
League == "SÃ¼per Lig" ~ "Turkey",
League == "Ligue 1" ~ "France",
League == "Liga Nos" ~ "Portugal", 
League == "Eredivisie" ~ "Netherlands")
) %>% filter(!is.na(League)) %>% mutate_if(is.factor, as.character)
rm(bundesliga, premierLeague, laliga, seriea, superlig, ligue1, eredivisie, liganos)

## Units Conversion: 
head(df$Value)

### Player Value
df$Values <- str_remove_all(df$Value,"???")
df$Values <- str_replace_all(df$Values,"K", "000")
df$Values <- str_remove_all(df$Values,"M")
df$Values <- as.numeric(df$Values)
df <- df  %>% mutate(Values = if_else(df$Values < 1000 , Values * 1000000, Values))
head(df$Values)

### Player Wage
df$Wages <- str_remove_all(df$Wage,"???")
df$Wages <- str_replace_all(df$Wages,"K", "000")

df$Wages <- as.numeric(df$Wages)
head(df$Wages)

### Height & Weight units conversion : 
df %<>%
  mutate(Height = round((as.numeric(str_sub(Height, start=1,end = 1))*30.48) + (as.numeric(str_sub(Height, start = 3, end = 5))* 2.54)),
         Weight = round(as.numeric(str_sub(Weight, start = 1, end = 3)) / 2.204623))

## Variables Manipulation :

### Rename variables :
df %<>% 
  rename(
    "Heading.Accuracy"= HeadingAccuracy, "Short.Passing"= ShortPassing, "FK.Accuracy" = FKAccuracy,
    "Long.Passing"= LongPassing, "Ball.Control"= BallControl, "Sprint.Speed"= SprintSpeed, "Shot.Power"= ShotPower, 
    "Standing.Tackle"= StandingTackle, "Sliding.Tackle"= SlidingTackle, "GK.Diving"= GKDiving, "GK.Handling"= GKHandling,
    "GK.Kicking"= GKKicking, "GK.Positioning"= GKPositioning, "GK.Reflexes"= GKReflexes )

### Create Position Class: 
unique(df$Position) 
defence <- c("CB", "RB", "LB", "LWB", "RWB", "LCB", "RCB")
midfielder <- c("CM", "CDM","CAM","LM","RM", "LAM", "RAM", "LCM", "RCM", "LDM", "RDM")
df %<>% mutate(Class = if_else(Position %in% "GK", "Goal Keeper",
                               if_else(Position %in% defence, "Defender",
                                       if_else(Position %in% midfielder, "Midfielder", "Forward"))))
rm(defence, midfielder)

### Add the Preferred Foot Variable:
df %<>% filter(Preferred.Foot %in% c("Left", "Right")) 
df$Preferred.Foot <- as.factor(as.character(df$Preferred.Foot))

### Remove Unnecessary Variables : 
df %<>% select(-Real.Face, -Joined, -Loaned.From, -Release.Clause, -Photo, -Flag, -Special, -Work.Rate)

### Tidy Data :
introduce(df)
plot_missing(df)


# 4- DATA ANALYSIS & VISUALIZATIONS:

## Age effect :

### Distribution and the Average of The Players Age in each League:

summ <- df %>% 
group_by(League) %>% 
summarise(age = mean(Age))

options(repr.plot.width = 12, repr.plot.height = 8)
ggplot()+
geom_histogram(df, mapping = aes(Age, fill = League))+
geom_vline(summ, mapping = aes(xintercept = age), color = "red", size = 1.5)+
geom_text(summ, mapping = aes(x = age+3, y = 65, label = round(age,digits = 2)))+
facet_wrap(League~.)+
theme_minimal()+
theme(legend.position = "bottom")+
labs(y = "Frequency", title = "Distribution & The Average Age of The Players in each League", caption = "@EA Sports - FIFA 19")

### Wage & Age : 
  #### in All Leagues : 
ggplot() +
geom_point(data = df, aes(Age, Wages)) +
geom_point(data = df, aes(Age, Wages), colour = 'purple', size = 3)
  #### Per league :
options(repr.plot.width = 12, repr.plot.height = 8)
ggplot(df, aes(Age, Wages))+
geom_hex()+
facet_wrap(League~., scales = "free")+
scale_fill_viridis_c()+
theme_minimal()

### Overall rating & Age : 
  #### in All Leagues : 
ggplot() +
geom_point(data = df, aes(Age, Overall)) +
geom_point(data = df, aes(Age, Overall), colour = 'purple', size = 3) 
  #### Per league :
options(repr.plot.width = 12, repr.plot.height = 8)
ggplot(df, aes(Age, Overall))+
geom_hex()+
facet_wrap(League~., scales = "free")+
scale_fill_viridis_c()+
theme_minimal()

### Clustering of Leagues by Age and Overall:
df %>%
group_by(League)%>%
summarise(Age = mean(Age), Overall = mean(Overall))%>%
ungroup()%>%
ggplot(aes(Age, Overall, label = League))+
geom_rect(aes(xmin =22, xmax=28, ymin=65, ymax=70), fill = "red", alpha = 0.1)+
geom_rect(aes(xmin =22, xmax=28, ymin=72, ymax=74), fill = "green", alpha = 0.1)+
geom_rect(aes(xmin =22, xmax=28, ymin=70, ymax=72), fill = "yellow", alpha = 0.1)+
geom_vline(aes(xintercept = 24), alpha = 0.5, color = "navy")+
geom_vline(aes(xintercept = 26), alpha = 0.5, color = "navy")+
geom_hline(aes(yintercept = 72), alpha = 0.5, color = "navy")+
geom_hline(aes(yintercept = 70), alpha = 0.5, color = "navy")+
geom_point(alpha = 0.5)+
geom_text(size = 5)+
ylim(65,74)+xlim(22,28)+
theme(
plot.background = element_rect(fill = "gray"),
panel.background = element_rect(fill = "gray"),
panel.grid = element_blank(),
axis.ticks = element_blank(),
plot.title = element_text(hjust = 0.5, size = 25, color = "navy", face = "bold")
)+
labs(title = "Average Overall and Age")


## Preferred Foot Vs Overall Rating : 

table(df$Preferred.Foot)

### Distribution of Players by Preferred Foot :
df %>% count(Preferred.Foot) %>% 
ggplot(aes(Preferred.Foot,n))+
geom_col(fill="navy")+
theme_minimal()+
labs(title="Preferred Foot",x = NULL, y = NULL)

### Distribution of Preferred Foot Per League :
options(repr.plot.width = 12, repr.plot.height = 8)
df %>% 
  group_by(League)%>% 
  count(Preferred.Foot) %>% 
  ggplot(aes(League, n, fill = Preferred.Foot)) +
  geom_col()+
  coord_polar()+
  scale_fill_ordinal()+
  theme_minimal()+
  labs(title="Preferred Foot Per League",x = NULL, y = NULL)

### Density of Preferred Foot in Function of Overall Rating :
ggplot() +
  geom_point(data = df, aes(Preferred.Foot, Overall)) +
  geom_point(data = df, aes(Preferred.Foot, Overall), colour = 'purple', size = 1) +
  theme_minimal()

### With Boxplots:
kor1 <- df %>% 
filter(League == "La Liga") %>% 
select(Name, Preferred.Foot, Overall)
shapiro.test(kor1$Overall)
xt1 <- kor1 %>% filter(Preferred.Foot == "Left") %>% select(Overall) %>% pull()
xt2 <- kor1 %>% filter(Preferred.Foot == "Right") %>% select(Overall) %>% pull()
xht <- wilcox.test(xt1, xt2, alternative = "two.sided")
xht
options(repr.plot.width = 25, repr.plot.height = 8)
grid.arrange(ncol = 2,
         ggplot(kor1, aes(x = Preferred.Foot, y = Overall, fill = Preferred.Foot))+
           geom_boxplot(show.legend = FALSE, color="darkgray")+
           scale_fill_manual(values = c("pink", "purple"))+
           ylim(30,100)+
           labs(title = "Overall Rating Vs Preferred Foot "))

### Preferred Foot Vs Shot Power and Finishing:
xt1 <- kor %>% filter(Preferred.Foot == "Left") %>% select(Shot.Power) %>% pull()
xt2 <- kor %>% filter(Preferred.Foot == "Right") %>% select(Shot.Power) %>% pull()
yt1 <- kor %>% filter(Preferred.Foot == "Left") %>% select(Finishing) %>% pull()
yt2 <- kor %>% filter(Preferred.Foot == "Right") %>% select(Finishing) %>% pull()
xht <- wilcox.test(xt1, xt2, alternative = "two.sided")
yht <- wilcox.test(yt1, yt2, alternative = "two.sided")

xht; yht

options(repr.plot.width = 12, repr.plot.height = 8)

grid.arrange(ncol = 2,
             ggplot(kor, aes(x = Preferred.Foot, y = Shot.Power, fill = Preferred.Foot))+
               geom_boxplot(show.legend = FALSE)+
               theme_minimal()+
               scale_fill_manual(values = c("pink", "purple"))+
               ylim(30,100)+
               labs(title = "Shot Power"),
             
             ggplot(kor, aes(x = Preferred.Foot, y = Finishing, fill = Preferred.Foot))+
               geom_boxplot(show.legend = FALSE)+
               theme_minimal()+
               scale_fill_manual(values = c("pink", "purple"))+
               ylim(30,100)+
               labs(title = "Finishing"))
### The Best Player by Preferred Foot:
kor <- df %>% 
  filter(League == "La Liga", Class == "Forward") %>% 
  select(Name, Preferred.Foot, Finishing, Shot.Power)
shapiro.test(kor$Finishing); shapiro.test(kor$Shot.Power)
cor.test(kor$Shot.Power, kor$Finishing, method = "pearson")
cor.test(kor$Shot.Power, kor$Finishing, method = "kendall")
hypo <- cor.test(kor$Shot.Power, kor$Finishing, method = "spearman")
hypo
options(repr.plot.width = 15, repr.plot.height = 8)
ggplot(kor, aes(Shot.Power, Finishing, label = Name, color = Preferred.Foot))+
  geom_text()+
  theme_minimal()+
  theme(legend.position = "bottom")+
  geom_jitter(alpha = 0.3, size = 2.5, width = 0.3, height = 0.3)+
  geom_smooth(method = "lm", color = "gray40", lty = 2, se = FALSE, size = 0.6)+
  scale_color_manual(values = c("orangered","steelblue"))+
  labs(title = "Best Player by Preferred Foot")

### Average summary statistics of players by Preferred Foot in Premier League :
df %>% 
filter(League == "Premier League") %>% 
select(Preferred.Foot, Sprint.Speed, Dribbling, Shot.Power, Finishing, Balance, Short.Passing) %>% 
group_by(Preferred.Foot) %>% 
summarise_at(vars(Sprint.Speed:Short.Passing), funs(mean)) %>% 
gather(variables, values, -Preferred.Foot) %>% 
ggplot(aes(variables, values, fill = Preferred.Foot))+
geom_col(position = "dodge")+
coord_polar()+
scale_fill_ordinal()+
theme_minimal()+
labs(x = NULL, y = NULL)

###Average summary statistics of players by Preferred Foot in the all leagues : 
options(repr.plot.width = 12, repr.plot.height = 8)
df %>% 
select(Preferred.Foot, Sprint.Speed, Dribbling, Shot.Power, Finishing, Balance, Short.Passing) %>% 
group_by(Preferred.Foot) %>% 
summarise_at(vars(Sprint.Speed:Short.Passing), funs(mean)) %>% 
gather(variables, values, -Preferred.Foot) %>% 
ggplot(aes(variables, values, fill = Preferred.Foot))+
geom_col(position = "dodge")+
coord_polar()+
scale_fill_ordinal()+
theme_minimal()+
labs(x = NULL, y = NULL)

### Preferred Foot and Wages :
  #### Density of Preferred Foot in Function of Wages :
ggplot() +
geom_point(data = df, aes(Preferred.Foot, Wages)) +
geom_point(data = df, aes(Preferred.Foot, Wages), colour = 'purple', size = 1) +
theme_minimal()
  #### with barplot :
options(repr.plot.width = 12, repr.plot.height = 8)
df %>% 
group_by(League) %>% 
ggplot(aes(Preferred.Foot, fill = Wages),color='pink')+
geom_col(aes(Preferred.Foot,Wages),position = "dodge")+
scale_fill_gradient(low="navy", high="pink")
labs(x = NULL, y = NULL)


## Nationality effect :

### Distribution of Players by Nationality : 
table(df$Nationality)

### World Map & Number of Player:
options(repr.plot.width = 12, repr.plot.height = 8)
world_map <- map_data("world")
numofplayers <- world_map %>% 
mutate(region = as.character(region)) %>% 
left_join((df %>% mutate(Nationality = as.character(Nationality),
                       Nationality = if_else(Nationality %in% "England", 
                                             "UK", Nationality)) %>%
           filter(League == "Bundesliga") %>%
           count(Nationality, name = "Number of Player") %>%
           rename(region = Nationality) %>%
           mutate(region = as.character(region))), by = "region")
ggplot(numofplayers, aes(long, lat, group = group))+
geom_polygon(aes(fill = `Number of Player` ), color = "white")+
scale_fill_viridis_c(option = "C")+
theme_void()+
labs(fill = "Number of Player",
   title = "Number of Player by Nationality")

### Barplot: 
df %>% 
  group_by(Nationality) %>% 
  count(Nationality) %>% 
  summarise(Total.Value = n) %>% 
  ggplot(aes(reorder(Nationality, Total.Value), Total.Value, fill = Total.Value))+
  geom_col(show.legend = FALSE)+
  coord_flip()+
  theme_minimal()+
  labs(x = NULL, y = "Number of players by Country")+
  scale_fill_gradient(low = "pink", high = "purple")+
  theme(axis.line.y = element_line(colour = "darkslategray"),
        axis.ticks.x = element_line(colour = "darkslategray"))

### Top 20 Nationalities:
nations = table(df$Nationality)
nations1 = sort(nations, decreasing = TRUE)
nations1= as.data.frame(nations1)
colnames(nations1)=c("Nations", "Counts")
top_20_nation = nations1[1:20,]  
top_20_nation
top_20_nation %>% 
ggplot(aes(Nations, Counts), fill = Counts)+
geom_col(show.legend = FALSE)+
coord_flip()+
theme_minimal()+
labs(x = NULL, y = "Number of players by Country")+
scale_fill_hue( "purple")+
theme_minimal()

### Distribution of Nationalities by Leagues:
options(repr.plot.width = 12, repr.plot.height = 8)
df %>% group_by(League) %>% count(Nationality) %>% 
ggplot(aes(League, n, fill = Nationality)) +
geom_col()+
coord_polar()+
theme_minimal()+
labs(x = NULL, y = NULL)


## BMI & Body Type effect :

### Club Example: Juventus - Serie A
      # Calculate BMI
      bmi <- df %>% 
      filter(Club == "Juventus") %>%
      mutate(BMI = round(Weight/(Height/100)^2, digits = 4))%>%
      arrange(-BMI)%>%
      select(Name, Age, Position, Class, Height, Weight, BMI)
      
      options(repr.plot.width = 12, repr.plot.height = 8)
      
      # Head & Tail Observations
      bmi2  <- rbind(
      bmi %>% head(5) %>% mutate(BMI = BMI * -1),
      bmi %>% tail(5)
      ) %>% mutate(Type = if_else(BMI < 0, "Head", "Tail"))
      
      # BMI Visual
      bmi2 %>% 
      ggplot(aes(fct_reorder(paste(Name,",", Position), desc(BMI)), BMI))+
      geom_col(aes(fill = Type))+
      geom_text(aes(y = c(rep(-2,5), rep(2,5)),label = round(abs(BMI),digits = 2)), 
              color = "white", fontface = "bold", size = 4)+
      coord_flip()+
      theme_minimal()+
      theme(axis.text.x = element_blank(),
          legend.position = "top",
          panel.background = element_rect(fill = "lightgray"),
          panel.grid.minor = element_blank(),
          axis.text = element_text(color = "slategray", face = "bold.italic",size = 12),
          title = element_text(color = "slategray", face = "bold.italic",size = 20),
          legend.box.background = element_rect(linetype = 2))+
      labs(x = NULL, y = NULL, fill = NULL, title = "BMI Index")+
      scale_fill_manual(values = c("pink", "purple")) 

### All Leagues: 
      bmi0 <- df %>% 
      mutate(BMI = round(Weight/(Height/100)^2, digits = 4))%>%
      arrange(-BMI)%>%
      select(Name, Age, Position, Class, Height, Weight, BMI)
      
      options(repr.plot.width = 12, repr.plot.height = 8)
      
      # Head & Tail Observations
      bmi3  <- rbind(
      bmi0 %>% head(5) %>% mutate(BMI = BMI * -1),
      bmi0 %>% tail(5)
      ) %>% mutate(Type = if_else(BMI < 0, "Head", "Tail"))
      
      # BMI Visual
      bmi3 %>% 
      ggplot(aes(fct_reorder(paste(Name,",", Position), desc(BMI)), BMI))+
      geom_col(aes(fill = Type))+
      geom_text(aes(y = c(rep(-2,5), rep(2,5)),label = round(abs(BMI),digits = 2)), 
              color = "white", fontface = "bold", size = 4)+
      coord_flip()+
      theme_minimal()+
      theme(axis.text.x = element_blank(),
          legend.position = "top",
          panel.background = element_rect(fill = "lightgray"),
          panel.grid.minor = element_blank(),
          axis.text = element_text(color = "slategray", face = "bold.italic",size = 12),
          title = element_text(color = "slategray", face = "bold.italic",size = 20),
          legend.box.background = element_rect(linetype = 2))+
      labs(x = NULL, y = NULL, fill = NULL, title = "BMI Index")+
      scale_fill_manual(values = c("orange", "navy")) 

### Body.type:
table(df$Body.Type)
df$Body.Type[1] = "Normal"
df$Body.Type[2] = "Stocky"
df$Body.Type[3] = "Lean"
df$Body.Type[20] = "Normal"
df$Body.Type[27] = "Stocky"
df$Body.Type[346] = "Stocky"

### Distribution of Players Body type by league : 
df %>% group_by(League) %>% count(Body.Type) %>% 
ggplot(aes(League, n, fill = Body.Type)) +
geom_col()+
coord_polar()+
scale_fill_ordinal()+
theme_minimal()+
labs(x = NULL, y = NULL)

###Average summary statistics of players by Preferred Foot in Serie A : 
df %>% 
filter(League == "Serie A") %>% 
select(Body.Type, Sprint.Speed, Dribbling, Shot.Power, Finishing, Balance, Short.Passing) %>% 
group_by(Body.Type) %>% 
summarise_at(vars(Sprint.Speed:Short.Passing), funs(mean)) %>% 
gather(variables, values, -Body.Type) %>% 
ggplot(aes(variables, values, fill = Body.Type))+
geom_col(position = "dodge")+
coord_polar()+
scale_fill_ordinal()+
theme_minimal()+
labs(x = NULL, y = NULL)

### Average summary statistics of players by Preferred Foot in the all leagues : 
df %>% 
select(Body.Type, Sprint.Speed, Dribbling, Shot.Power, Finishing, Balance, Short.Passing) %>% 
group_by(Body.Type) %>% 
summarise_at(vars(Sprint.Speed:Short.Passing), funs(mean)) %>% 
gather(variables, values, -Body.Type) %>% 
ggplot(aes(variables, values, fill = Body.Type))+
geom_col(position = "dodge")+
coord_polar()+
scale_fill_ordinal()+
theme_minimal()+
labs(x = NULL, y = NULL)

### Body Type Vs Wages :
### Average of players Wages by Body type in the all leagues : 
df %>% 
select(Body.Type, Wages) %>% 
group_by(Body.Type) %>% 
summarise_at(vars(Wages), funs(mean)) %>% 
gather(variables, values, -Body.Type) %>% 
ggplot(aes(variables, values, fill = Body.Type))+
geom_col(position = "dodge")+
scale_fill_ordinal()+
theme_minimal()+
labs(x = NULL, y = NULL)

###Average of players Wages by Body type in Serie A : 
df %>%
filter(League == "Serie A") %>% 
select(Body.Type, Wages) %>% 
group_by(Body.Type) %>% 
summarise_at(vars(Wages), funs(mean)) %>% 
gather(variables, values, -Body.Type) %>% 
ggplot(aes(variables, values, fill = Body.Type))+
geom_col(position = "dodge")+
scale_fill_ordinal()+
theme_minimal()+
labs(x = NULL, y = NULL)

### Body Type Vs Overall rating : 
### Average of players Overall rating by Body type in Serie A : 
df %>%filter(League == "Serie A") %>% 
select(Body.Type, Overall) %>% 
group_by(Body.Type) %>% 
summarise_at(vars(Overall), funs(mean)) %>% 
gather(variables, values, -Body.Type) %>% 
ggplot(aes(variables, values, fill = Body.Type))+
geom_col(position = "dodge")+
scale_fill_ordinal()+
theme_minimal()+
labs(x = NULL, y = NULL)

### Average of players Overall rating by Body type for all players : 
df %>%
select(Body.Type, Overall) %>% 
group_by(Body.Type) %>% 
summarise_at(vars(Overall), funs(mean)) %>% 
gather(variables, values, -Body.Type) %>% 
ggplot(aes(variables, values, fill = Body.Type))+
geom_col(position = "dodge")+
scale_fill_ordinal()+
theme_minimal()+
labs(x = NULL, y = NULL)

### Boxplots : 
#### for Serie A
kor11 <- df %>% 
filter(League == "Serie A") %>% 
select(Name, Body.Type, Overall)
shapiro.test(kor1$Overall)
xbt1 <- kor11 %>% filter(Body.Type == "Normal") %>% select(Overall) %>% pull()
xbt2 <- kor11 %>% filter(Body.Type == "Lean") %>% select(Overall) %>% pull()
xbt3 <- kor11 %>% filter(Body.Type == "Stocky") %>% select(Overall) %>% pull()
xbht <- wilcox.test(xbt1, xbt2, xbt3, alternative = "no type")
xbht
options(repr.plot.width = 120, repr.plot.height = 80)
grid.arrange(ncol = 3,
         ggplot(kor11, aes(x = Body.Type, y = Overall, fill = Body.Type))+
           geom_boxplot(show.legend = FALSE, color="gray")+
           scale_fill_ordinal()+
           ylim(30,100)+
           theme_minimal()+
           labs(title = "Overall Rating Vs Body Type in Serie A "))

#### for all Leagues : 
kor110 <- df %>%
select(Name, Body.Type, Overall)
shapiro.test(kor1$Overall)
xbt10 <- kor110 %>% filter(Body.Type == "Normal") %>% select(Overall) %>% pull()
xbt20 <- kor110%>% filter(Body.Type == "Lean") %>% select(Overall) %>% pull()
xbt30<- kor110 %>% filter(Body.Type == "Stocky") %>% select(Overall) %>% pull()
xbht0 <- wilcox.test(xbt10, xbt20, xbt30, alternative = "no type")
xbht0
options(repr.plot.width = 120, repr.plot.height = 80)
grid.arrange(ncol = 3,
         ggplot(kor110, aes(x = Body.Type, y = Overall, fill = Body.Type))+
           geom_boxplot(show.legend = FALSE, color="GRAY")+
           scale_fill_ordinal()+
           ylim(30,100)+
           theme_minimal()+
           labs(title = "Overall Rating Vs Body Type "))


## Position effect:
### Distribution of Players Position Class by league  
df %>% group_by(League) %>% count(Class) %>% 
ggplot(aes(League, n, fill = Class)) +
geom_col()+
coord_polar()+
scale_fill_ordinal()+
theme_minimal()+
labs(x = NULL, y = NULL)
options(repr.plot.width = 10, repr.plot.height = 7)

### Player Position Visualization :
      # Select a player
      player <- df %>% filter(Name == "Cristiano Ronaldo")%>%  select(Position, LS:RB)
      # Transform from column to observation
      player <- as.data.frame(t(player)) %>% 
      rownames_to_column("Pos") %>% 
      mutate(V1 = as.numeric(str_sub(V1, end = 2)),
           Pos = as.factor(Pos))
      
      # Create pitch & positions
      pos <- data.frame(
      Pos = as.character(c("LB","LCB","CB", "RCB","RB",
                         "LWB", "LDM", "CDM", "RDM", "RWB",
                         "LM", "LCM", "CM", "RCM", "RM",
                         "LAM", "CAM", "RAM",
                         "LW","LF","CF","RF","RW",
                         "LS","ST","RS")), 
      x = c(1:5, 1:5,1:5, 2:4, 1:5,2:4),
      y = c(rep(1,5), rep(1.5,5), rep(2,5), rep(2.5,3), rep(3,5), rep(3.5,3)))
      
      # Join player data & pitch
      player <- left_join(player, pos, by = 'Pos')
      
      # Remove an unnecessary observation
      player <- na.omit(player)
      
      # Visual
      ggplot(player, aes(x, y, fill = if_else(V1 < 50, "orangered", 
                                          if_else(V1 <60, "orange",
                                                  if_else(V1 < 70, "goldenrod1", 
                                                          if_else(V1 <80, "palegreen4",
                                                                  if_else(V1 < 90, "forestgreen",
                                                                          if_else(V1 == 0,
                                                                                  "orangered","darkgreen"))))))
      ))+
      geom_point(shape = 22, size = 20, color = "white", show.legend = FALSE,position = "identity")+
      theme(panel.background = element_rect(fill = "steelblue"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          title = element_text(color = "navy",face="bold",size = 23) 
      )+
      geom_text(aes(label = Pos), vjust= -0.5, color = "white", size = 4.5, fontface = "bold")+
      geom_text(aes(label = V1), vjust = 1.5, fontface = "bold", color = "white")+
      scale_fill_identity()+
      ylim(0.8, 4)+
      labs(title = "Cristiano Ronaldo Vs  Positions")
      
      df %>%filter(League == "Serie A") %>% 
      select(Position, Overall) %>% 
      group_by(Position) %>% 
      summarise_at(vars(Overall), funs(mean)) %>% 
      gather(variables, values, -Position) %>% 
      ggplot(aes(variables, values, fill = Position))+
      geom_col(position = "dodge")+
      scale_fill_ordinal()+
      theme_minimal()+
      labs(x = NULL, y = NULL)
      
### Position Vs Overall rating :
###Average of players Overall rating by position type for all players : 
df %>%
select(Position, Overall) %>% 
group_by(Position) %>% 
summarise_at(vars(Overall), funs(mean)) %>% 
gather(variables, values, -Position) %>% 
ggplot(aes(variables, values, fill = Position))+
geom_col(position = "dodge")+
scale_fill_ordinal()+
theme_minimal()+
labs(x = NULL, y = NULL)

###Average of players Overall rating by Position Class in Serie A : 
df %>%filter(League == "Serie A") %>% 
select(Class, Overall) %>% 
group_by(Class) %>% 
summarise_at(vars(Overall), funs(mean)) %>% 
gather(variables, values, -Class) %>% 
ggplot(aes(variables, values, fill = Class))+
geom_col(position = "dodge")+
scale_fill_ordinal()+
theme_minimal()+
labs(x = NULL, y = NULL)

###Average of players Overall rating by Position Class for all players : 
df  %>% 
select(Class, Overall) %>% 
group_by(Class) %>% 
summarise_at(vars(Overall), funs(mean)) %>% 
gather(variables, values, -Class) %>% 
ggplot(aes(variables, values, fill = Class))+
geom_col(position = "dodge")+
scale_fill_ordinal()+
theme_minimal()+
labs(x = NULL, y = NULL)

### Position Vs Performance :
###Average summary statistics of players by Preferred Foot in Serie A : 
df %>% 
filter(League == "Serie A") %>% 
select(Class, Sprint.Speed, Dribbling, Shot.Power, Finishing, Balance, Short.Passing) %>% 
group_by(Class) %>% 
summarise_at(vars(Sprint.Speed:Short.Passing), funs(mean)) %>% 
gather(variables, values, -Class) %>% 
ggplot(aes(variables, values, fill = Class))+
geom_col(position = "dodge")+
coord_polar()+
scale_fill_ordinal()+
theme_minimal()+
labs(x = NULL, y = NULL)


### Position Vs Physical characteristics:
### Average of players Height and weight by Preferred Foot in Serie A : 
df %>% 
filter(League == "Serie A") %>% 
select(Class, Height, Weight) %>% 
group_by(Class) %>% 
summarise_at(vars(Height:Weight), funs(mean)) %>% 
gather(variables, values, -Class) %>% 
ggplot(aes(variables, values, fill = Class))+
geom_col(position = "dodge")+
scale_fill_ordinal()+
theme_minimal()+
labs(x = NULL, y = NULL)


### Position Vs Wages :
###Average of players Wages by Position Class for all players : 
df  %>% 
select(Class, Wages) %>% 
group_by(Class) %>% 
summarise_at(vars(Wages), funs(mean)) %>% 
gather(variables, values, -Class) %>% 
ggplot(aes(variables, values, fill = Class))+
geom_col(position = "dodge")+
scale_fill_ordinal()+
theme_minimal()+
labs(x = NULL, y = NULL)

### Players Comparison :
#### Comparison of the Two Players:
      # Selection of the players
      players <- df %>% 
        filter(Name %in% c("Cristiano Ronaldo", "L. Messi")) %>% 
        # Unite Name & Club variables
        mutate(Name = paste0(Name, ", ", Club)) %>%
        # Selection abilities of the players
        select(Name,Crossing:Sliding.Tackle) %>% 
        # Correction of the punctuation
        rename_all(funs(gsub("[[:punct:]]", " ", .))) %>% 
        # Tranform from Variable to observation
        gather(Skill, Exp, Crossing:`Sliding Tackle`, -Name)
      head(players  )
      ## Visually :
      options(repr.plot.width = 15, repr.plot.height = 8)
      ggplot(players, aes(Skill, Exp, fill = Name))+
        geom_col(show.legend = FALSE)+
        coord_flip()+
        facet_wrap(Name~.)+
        scale_fill_manual(values = c("orange", "navy"))+
        theme_minimal()
      ## Let's make more interpertable : 
      options(repr.plot.width = 15, repr.plot.height = 8)
      ggplot(players, aes(Skill, Exp, fill = Name))+
        geom_col(position = "fill")+
        coord_flip()+
        scale_fill_manual(values = c("orange", "navy"))+
        theme_minimal()+
        geom_hline(yintercept = 0.5, color = "white", size = 1, linetype = 2)+
        theme(legend.position = "top", axis.text.x=element_blank())+
        labs(title = "Messi Vs Ronaldo", 
             fill = NULL,x = NULL, y = NULL)

## The Most Powerful Clubs in the World : 

power <- df %>% 
  group_by(Club) %>% 
  summarise(mean = mean(Overall)) %>% 
  arrange(-mean) %>% 
  head(20)

df %>% 
  group_by(Club, Class) %>% 
  summarise(mean = mean(Overall)) %>% 
  ungroup() %>% 
  filter(Club %in% power$Club) %>% 
  ggplot(aes(reorder(Club, mean), mean, fill = Class))+
  geom_col(position = "fill")+
  geom_text(aes(label = round(mean,digits = 2)), position = position_fill(0.5))+
  coord_flip()+
  scale_fill_ordinal()+
  theme_minimal()+
  theme(legend.position = "top")+
  labs(x = NULL, y = NULL, title = "Team Power in every Position Class")


## The Most Vallued Clubs in the World : 
powerf <- df %>% 
  group_by(Club) %>% 
  summarise(mean = mean(Values)) %>% 
  arrange(-mean) %>% 
  head(20)

df %>% 
  group_by(Club, Class) %>% 
  summarise(mean = mean(Values)/1000000) %>% 
  ungroup() %>% 
  filter(Club %in% powerf$Club) %>% 
  ggplot(aes(reorder(Club, mean), mean, fill = Class))+
  geom_col(position = "fill")+
  geom_text(aes(label = round(mean,digits = 2)), position = position_fill(0.5))+
  coord_flip()+
  scale_fill_ordinal()+
  theme_minimal()+
  theme(legend.position = "top")+
  labs(x = NULL, y = NULL, title = "Team Value in every Position Class in Mdâ‚¬")

## Models : 
fifa<-df
fifa %<>%
  select(Name, Club, League, Position, Class, Nationality, Preferred.Foot, Work.Rate,
         International.Reputation, Weak.Foot, Skill.Moves, Values, Wages, Age, Overall, Potential, Preferred.Foot,
         Crossing:GK.Reflexes) 

# Data Types
fifa %<>%
  mutate_if(is.integer, as.numeric) %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate_if(is.character, as.factor)
# ID-Name
fifa %<>% mutate(Name = paste0(1:nrow(fifa), "-", Name)) 

# Column to Row
fifa %<>% column_to_rownames("Name") 
str(fifa)
# **TRAIN TEST SPLIT**
train_indx <- createDataPartition(fifa$Value,
                                  p = 0.7,
                                  list = FALSE,
                                  times = 1)

train <- fifa[train_indx,]
test <- fifa[-train_indx,]

rm(train_indx)

train_x <- train %>% dplyr::select(-Values)
train_y <- train$Values

test_x <- test %>% dplyr::select(-Values)
test_y <- test$Values
#####FIRST MODEL GBM

gbm <- gbm(sqrt(Values)~., data = train, 
           n.trees = 500, # Default: 100 
           n.cores = parallel::detectCores(),
           distribution = "gaussian",
           interaction.depth = 1, # Default: 1 
           n.minobsinnode = 10, # Default: 10 
           shrinkage = 0.1, # Default: 0.1 # Learning Rate
           bag.fraction = 0.5 # Default: 0.5 
)

###################FIRST MODEL XGB#################################
xgb <- xgboost(data = as.matrix(train_x %>% mutate_if(is.factor, as.integer)),label = sqrt(train_y),
               nrounds = 1, # MAx Iterasyon Sayisi
               print_every_n = 1, # 
               params = list(
                 max_depth = 6, # Default: 6
                 eta = 0.3, # Default:0.3 # Learning Rate
                 min_child_weight = 1, # Default: 1 
                 subsample = 1, # Default: 1 
                 colsubsample_bytree = 1, # Default: 1 
                 num_parallel_tree = 1, # Default: 1
                 lambda = 0, # Default: 0 
                 lambda_bias = 0, # Default: 0 
                 alpha = 0 # Default: 0 
                 
               )
)
################### MODELS SUMMARY AND RESULTS PLOT #################################
res <- rbind( 
  defaultSummary(data.frame(
    obs = sqrt(train_y),
    pred = gbm$fit
  )),
  defaultSummary(data.frame(
    obs = sqrt(test_y),
    pred = predict(gbm, test_x, n.trees = 1)
  )),
  defaultSummary(data.frame(
    obs = sqrt(train_y),
    pred = predict(xgb, as.matrix(train_x %>% mutate_if(is.factor, as.integer)))
  )),
  defaultSummary(data.frame(
    obs = sqrt(test_y),
    pred = predict(xgb, as.matrix(test_x %>% mutate_if(is.factor, as.integer)))
  ))
) %>% as.data.frame()

rownames(res) <- c("GBM Train", "GBM Test", "XGBoost Train", "XGBoost Test")

res




res1 <- data.frame(PLAYER = rownames(test_x),
                   ACTUAL = sqrt(test_y), 
                   PRED = predict(gbm, test_x, n.trees = 1)
) %>% 
  mutate(ERROR = abs(ACTUAL - PRED),
         ERROR2 = ERROR^2,
         RATIO = if_else(PRED > ACTUAL, ACTUAL / PRED, PRED / ACTUAL)) %>% 
  separate(PLAYER, c("Num", "PLAYER"), sep = "-") %>% 
  select(-Num)

res2 <- data.frame(PLAYER = rownames(test_x),
                   ACTUAL = sqrt(test_y), 
                   PRED = predict(xgb, as.matrix(test_x  %>% mutate_if(is.factor, as.integer)))
) %>% 
  mutate(ERROR = abs(ACTUAL - PRED),
         ERROR2 = ERROR^2,
         RATIO = if_else(PRED > ACTUAL, ACTUAL / PRED, PRED / ACTUAL)) %>% 
  separate(PLAYER, c("Num", "PLAYER"), sep = "-") %>% 
  select(-Num)

p1 <- ggplot(res1, aes(ACTUAL^2, PRED^2, label = PLAYER))+
  geom_point(alpha = 0.5, color = "darkslategray")+
  geom_text(color = "orchid")+
  geom_smooth(method = "lm", se = FALSE)+
  ggtitle("GBM Test: Player Value Prediction")+
  theme_minimal()+
  scale_x_continuous(labels = scales::comma)+
  scale_y_continuous(labels = scales::comma)+
  labs(x = "Actual", y = "Prediction")


p2 <- ggplot(res2, aes(ACTUAL^2, PRED^2, label = PLAYER))+
  geom_point(alpha = 0.5, color = "darkslategray")+
  geom_text(color = "seagreen")+
  geom_smooth(method = "lm", se = FALSE)+
  ggtitle("XGBoost Test: Player Value Prediction")+
  theme_minimal()+
  scale_x_continuous(labels = scales::comma)+
  scale_y_continuous(labels = scales::comma)+
  labs(x = "Actual", y = "Prediction")

grid.arrange(p1, p2, ncol = 2)
################### TUNING MODELS #################################

########    TUNE GBM    ########
gbm.perf(gbm) ###########TuneNtrees
hyper_grid <- expand.grid(
  shrinkage = c(.02, .1, .3),
  interaction.depth = c(1, 3, 5),
  n.minobsinnode = c(5, 10, 15),
  bag.fraction = c(.65, .8, 1), 
  min_MAE=0,
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)
hyper_grid 
for(i in 1:nrow(hyper_grid)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbm.tune <- gbm(
    sqrt(Values)~., data = train,
    distribution = "gaussian",
    n.trees = 400,
    interaction.depth = hyper_grid$interaction.depth[i],
    shrinkage = hyper_grid$shrinkage[i],
    n.minobsinnode = hyper_grid$n.minobsinnode[i],
    bag.fraction = hyper_grid$bag.fraction[i],
    n.cores = parallel::detectCores()
  )
  # add min training error  to grid
  hyper_grid$min_RMSE[i] <-RMSE(obs = sqrt(train_y),pred = gbm.tune$fit)
  hyper_grid$min_MAE[i]<-MAE(obs = sqrt(train_y),pred = gbm.tune$fit)
}
hyper_grid %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)
########TUNE XGB ########
hyper_xgb <- expand.grid(
  nrounds = c(1,10, 20,30, 40),
  lambda =  c(0,0.3, 0.5,0.8, 1),
  alpha = c(0,0.3, 0.5,0.8, 1),           
  min_RMSE1 = 0                   
)
for(i in 1:nrow(hyper_xgb)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  xgb_Tune <- xgboost(data = as.matrix(train_x %>% mutate_if(is.factor, as.integer)),label = sqrt(train_y),
                      nrounds =hyper_xgb$nrounds[i], # MAx Iterasyon Sayisi
                      print_every_n = 5, # 
                      params = list(
                        max_depth = 6, # Default: 6
                        eta = 0.3, # Default:0.3 # Learning Rate
                        min_child_weight = 1, # Default: 1 
                        subsample = 1, # Default: 1 
                        colsubsample_bytree = 1, # Default: 1 
                        num_parallel_tree = 1, # Default: 1
                        lambda = hyper_xgb$lambda[i], # Default: 0 
                        lambda_bias = 0, # Default: 0 
                        alpha = hyper_xgb$alpha[i] # Default: 0 
                        
                      )
  )
  
  # add min training error to grid
  hyper_xgb$min_RMSE1[i] <-RMSE(obs = sqrt(train_y),pred =  predict(xgb_Tune, as.matrix(train_x %>% mutate_if(is.factor, as.integer))))
}

hyper_xgb %>% 
  dplyr::arrange(min_RMSE1) %>%
  head(10)
############################################## **FINAL MODELS**####################################################
gbm <- gbm(sqrt(Values)~., data = train, 
           n.trees = 200, # Default: 100 
           n.cores = parallel::detectCores(),
           distribution = "gaussian",
           interaction.depth = 5, # Default: 1 
           n.minobsinnode = 10, # Default: 10 
           shrinkage = 0.3, # Default: 0.1 # Learning Rate
           bag.fraction = 1 # Default: 0.5 
)




xgb <- xgboost(data = as.matrix(train_x %>% mutate_if(is.factor, as.integer)),label = sqrt(train_y),
               nrounds = 40, 
               print_every_n = 5, 
               params = list(
                 max_depth = 3, # Default: 6
                 eta = 0.3, # Default:0.3 # Learning Rate
                 min_child_weight = 1, # Default: 1 
                 subsample = 1, # Default: 1 
                 colsubsample_bytree = 1, # Default: 1 
                 num_parallel_tree = 1, # Default: 1
                 lambda = 0, # Default: 0 
                 lambda_bias = 0, # Default: 0 
                 alpha = 0.5 # Default: 0  
                 
               )
)

res <- rbind(
  defaultSummary(data.frame(
    obs = sqrt(train_y),
    pred = gbm$fit
  )),
  defaultSummary(data.frame(
    obs = sqrt(test_y),
    pred = predict(gbm, test_x, n.trees = 600)
  )),
  defaultSummary(data.frame(
    obs = sqrt(train_y),
    pred = predict(xgb, as.matrix(train_x %>% mutate_if(is.factor, as.integer)))
  )),
  
  defaultSummary(data.frame(
    obs = sqrt(test_y),
    pred = predict(xgb, as.matrix(test_x %>% mutate_if(is.factor, as.integer)))
  ))
  
  
) %>% as.data.frame()

rownames(res) <- c("GBM Train", "GBM Test", "XGBoost Train", "XGBoost Test")

res

res1 <- data.frame(PLAYER = rownames(test_x),
                   ACTUAL = sqrt(test_y), 
                   PRED = predict(gbm, test_x, n.trees = 600)
) %>% 
  mutate(ERROR = abs(ACTUAL - PRED),
         ERROR2 = ERROR^2,
         RATIO = if_else(PRED > ACTUAL, ACTUAL / PRED, PRED / ACTUAL)) %>% 
  separate(PLAYER, c("Num", "PLAYER"), sep = "-") %>% 
  select(-Num)

res2 <- data.frame(PLAYER = rownames(test_x),
                   ACTUAL = sqrt(test_y), 
                   PRED = predict(xgb, as.matrix(test_x  %>% mutate_if(is.factor, as.integer)))
) %>% 
  mutate(ERROR = abs(ACTUAL - PRED),
         ERROR2 = ERROR^2,
         RATIO = if_else(PRED > ACTUAL, ACTUAL / PRED, PRED / ACTUAL)) %>% 
  separate(PLAYER, c("Num", "PLAYER"), sep = "-") %>% 
  select(-Num)

p1 <- ggplot(res1, aes(ACTUAL^2, PRED^2, label = PLAYER))+
  geom_point(alpha = 0.5, color = "darkslategray")+
  geom_text(color = "orchid")+
  geom_smooth(method = "lm", se = FALSE)+
  ggtitle("GBM Test: Player Value Prediction")+
  theme_minimal()+
  scale_x_continuous(labels = scales::comma)+
  scale_y_continuous(labels = scales::comma)+
  labs(x = "Actual", y = "Prediction")


p2 <- ggplot(res2, aes(ACTUAL^2, PRED^2, label = PLAYER))+
  geom_point(alpha = 0.5, color = "darkslategray")+
  geom_text(color = "seagreen")+
  geom_smooth(method = "lm", se = FALSE)+
  ggtitle("XGBoost Test: Player Value Prediction")+
  theme_minimal()+
  scale_x_continuous(labels = scales::comma)+
  scale_y_continuous(labels = scales::comma)+
  labs(x = "Actual", y = "Prediction")

grid.arrange(p1, p2, ncol = 2)









