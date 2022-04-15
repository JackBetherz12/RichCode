
# Your task is to develop, analyse and validate an appropriate statistical model for the data you have selected. 
# Your project will include evidence of extensive data exploration, hypothesis testing, model development, interpretation and validation. 

# This is a methods module so we are not expecting a project write up on an ecological or modelling problem. Instead, present the R code 
# from your statistical and mechanistic models and an analysis/interpretation of the results. The R code must be fully annotated so as to be 
# clearly and easily understandable to a reader who is moderately proficient in R but who has no knowledge of the work you are presenting. 
# Integrate appropriate figures and tables and code snippets into each part of the project to show a narrative of how and why you did what you 
# did, what the models mean (interpret them) and how and why you validated them.

# PLEASE upload your R script and associated datafiles as appendices to the project. We will run the script to ensure it works!

# The fish data derive from a longitudinal sample of fish species from a river, which runs near France-Switzerland border in the Jura Mountains. 

# The first matrix contains coded abundances of 27 fish species, fish overall abundance and fish species richness, the second matrix contains 11
# environmental variables related to the hydrology, geomorphology, and chemistry of the river of the sites. You will need to figure out what you 
# want to test as response variables (e.g. species richness, species abundance, combine them in 'vegan' package to create a diversity metric 
# [e.g. Shannon] or use individual species or a combination of both). Whatever, you'll need a little reorganisation of the data, either in R, if 
# you think your wrangling skills are up to the challenge, or in excel if not.

rm(list=ls()) # This clears R's brain such that no problems with variables names are encountered

library(MuMIn) # Used for model averaging coupled with model selection
library(tidyverse)  # Contains components ggplot, dplyr, tidyr etc - this is the full suite
library(tidyr) # Used for tidying data - can change the shape and hierachy of a dataset
library(ggplot2) # Used for creating graphical plots
library(ggfortify) # Used as a plotting tool for statistics - is an extension for ggplot2 
library(dplyr) # Transforms and summarises data i.e. used for data manipulation
library(alr4) # Data to accompany an applied linear Regression book
library(lattice) # To display multivariate relationships - improving base R graphics
library(ez) # Needs a test of sphericity - for Random (within variation) designs
library(car) # For regression - gets the levene test!
library(coin) # For conditional infererence procedures - including non-parametric anova
library(vegan) # Provides tools for descriptive community ecology - Specifically diversity analysis
library(moments) # Provides functions to carry out skewness and kurtosis
library(cowplot) # Used to separate ggplot into a grid - essentially the ggplot equivalent of par(mfrow)
# The library function loads a library or package - here we have chosen to load all the packages we will require at the start for simplicity, to save
# loading them as we go which can be a bit more confusing!

getwd() #Check where R is looking. Then set the working directory (through the session tab) if R is not looking in the correct folder.

Fish <- read.csv(file.choose()) # Load fish species, abundance and richness data sheet, copied and pasted from original file to a csv file

FishV <- read.csv(file.choose()) # Load geochemical data impacting the fish, copied and pasted from original file to a csv file

# **********************************************************
# Data Exploration                                  *
# **********************************************************

names(Fish)        # Lists the column (or variable names)
str(Fish)          # Indicates the STRUCTURE of the data frame (i.e. variable types etc)
head(Fish)         # Lists the first 6 rows of data
tail(Fish)         # Lists the last 6 rows of data
dim(Fish)          # Lists the dimensions of the data frame 

# ***********************************************************
# Scatterplots    
# ***********************************************************    

# Plotting abundance of fish and species richness against environmental variables on scatterplots. Choose to use Base R as easier to plot variables in different files. 

par(mfrow = c(1,2)) # Splits the plot window into two - the par function allows this while the mfrow is the function that allows the screen to be split. 1,2 implies 1 row and 2 columns.
  plot(x = FishV$dbo, y = Fish$fish_abun, #Data that's being plot on each axis - Explanatory Variable (Environmental variable) on the X axis and Response variable (Fish abundance etc) on the Y
     xlab = "Biological Oxygen Demand", #Labels for x axis
     ylab = "Fish Abundance Count", main = "Scatter plot of abundance 
     count with oxygen demand", # labels for y axis and title. Second half of the title is on the second line such that the title on the plot is on two lines, this allows it to not cover up the
                                # second plot within the plot space.
     xlim = c(0, 17), ylim = c(5, 90), cex = 1.5, cex.axis = 0.8, cex.main = 0.9, pch = 20) # numerical limits of the axis, size of symbols and symbol type. Numbers on axis font 
      # size decreased by the use of cex.axis. cex.main decreases title size

  plot(x = FishV$dbo, y = Fish$fish_SR, #Data that's being plot on each axis - Explanatory Variable (Environmental variable) on the X axis and Response variable (Fish abundance etc) on the Y
     xlab = "Biological Oxygen Demand", #Labels for x axis
     ylab = "Species Richness", main = "Scatter plot of species richness 
     change with oxygen demand", #labels for y axis and title
     xlim = c(0, 20), ylim = c(0, 30), cex = 1.2, cex.axis = 0.8, cex.main = 0.9, pch = 18, col = "dark red") # numerical limits of the axis, size of symbols, symbol type and colour

par(mfrow = c(1,2)) # Splits the plot window into two - the par function allows this while the mfrow is the function that allows the screen to be split. 1,2 implies 1 row and 2 columns.
  plot(x = FishV$das, y = Fish$fish_SR, #Data that's being plot on each axis - Explanatory Variable (Environmental variable) on the X axis and Response variable (Fish abundance etc) on the Y
     xlab = "Distance From Source", #Labels for x axis
     ylab = "Species Richness", main = "Scatter plot of species richness change 
     with distance from the source", #labels for y axis and title
     xlim = c(0, 500), ylim = c(0, 30), cex = 1.2, cex.axis = 0.8, cex.main = 0.9, pch = 18, col = "dark green") # numerical limits of the axis, size of symbols, symbol type and colour. 
          M0 <- lm(Fish$fish_SR ~ FishV$das, data = Fish) # Linear best fit line added to capture the overall pattern - to do such a linear model is created and then an abline used to add this to the plot.
              abline(M0)

  plot(x = FishV$das, y = Fish$fish_abun, #Data that's being plot on each axis - Explanatory Variable (Environmental variable) on the X axis and Response variable (Fish abundance etc) on the Y
     xlab = "Distance From Source", #Labels for x axis
     ylab = "Fish Abundance Count", main = "Scatter plot of species richness change 
     with distance from the source", #labels for y axis and title
     xlim = c(0, 500), ylim = c(0, 100), cex = 1.2, cex.axis = 0.8, cex.main = 0.9, pch = 18, col = "dark blue") # numerical limits of the axis, size of symbols, symbol type and colour
        M1 <- lm(Fish$fish_abun ~ FishV$das, data = Fish) # Same as above, just for abundance instead
            abline(M1)
            
par(mfrow = c(1,1)) # Reverts the plot window to the original one screen format
  plot(x = FishV$dur, y = Fish$fish_abun, #Data that's being plot on each axis - Explanatory Variable (Environmental variable) on the X axis and Response variable (Fish abundance etc) on the Y
      xlab = "Distance From Source", #Labels for x axis
      ylab = "Fish Abundance Count", main = "Scatter plot of species richness change 
     with distance from the source", #labels for y axis and title
      xlim = c(40, 120), ylim = c(0, 100), cex = 1.2, cex.axis = 0.8, cex.main = 0.9, pch = 18, col = "brown") # numerical limits of the axis, size of symbols, symbol type and colour
        M1 <- lm(Fish$fish_abun ~ FishV$dur, data = Fish) # Same as above, just for abundance instead
           abline(M1)
           cor.test(FishV$dur, Fish$fish_abun) # Pearson's correlation coefficient - where it states sample estimates: cor, this is the Pearson's coefficient
           legend(x='right', legend='Correlation = 0.41', cex = 0.8) # Here the Pearson's value is 0.41 and hence is entered into the legend. x = right determines
           # the position of the legend, here it is positioned to the right so it overlaps the abline of best fit. cex is used to decrease the text size here.
 
# ***********************************************************
# Histogram, Skewness, Kurtosis, Means and Standard Deviation    
# ***********************************************************           
           
      
hist(FishV$pH, xlab = "pH", main = "pH throughout the river system", 
     breaks = 8, xlim = c(7.7, 8.3), col = "black", border = "dark grey") 
     # Hist is the histogram function. Here the frequency of different pH values is plotted, with the labels carried out using xlab and main.
     # The amount of breaks is given by the break function, 8 breaks means 6 boxes as there's always two more boxes than breaks. The xlim are set
     # as the pH ranges within the data set while the colours are changed. Although one can estimate how skewed the ph data is from the histogram
     # (as well as the level of kurtosis), it is better to quantify this through calculating these values:
skewness(FishV$pH) # 0.92 indicates moderate skewness - it moderately skews from a normal distribution.
kurtosis(FishV$pH) # Kurtosis is a measure of how heavily tailed the data is relative to a normal distribution.
     # As found from the below website, values above 3 indicate a high level of kurtosis - with a big peak and 'fatter' tails - given the reading
     # is ~4.9 it implies this is the case and is therefore described as leptokurtic. 
     # https://www.macroption.com/kurtosis-values/#:~:text=Kurtosis%20can%20reach%20values%20from%201%20to%20positive%20infinite.&text=A%20distribution%20that%20is%20more,more%20peaked%20and%20fatter%20tails).
     # A property of leptokurtic distribution is the data is concentrated about the mean. We can test this by investigating the standard deviation
     # and mean of the data.
mean(FishV$pH)  
sd(FishV$pH) # A standard deviation of < 1 is often deemed low, and hence the standard deviation of ~0.17 is therefore very low and shows the 
# variation about the mean is minimal

var(FishV$dur)
var(FishV$pH)
# The var function allows the sample variability within a data set to be seen - it is a measure of how much value something is from the mean value.
# As one would expect from looking at the data, variation of the pH within ther river is alot less than the variation of the calcium concentration.

# ***********************************************************
# Scatterplot Matrix       
# ***********************************************************           
           
          
       
scatterplotMatrix(~pho + nit + amm + oxy + dbo, data = FishV, diagonal = list(method = "qqplot"), col = "black", lower.panel=NULL)

panel.cor <- function(x, y, cex.cor = 0.8, method = "pearson", ...) {
  options(warn = -1)                   # Turn of warnings (e.g. tied ranks)
  usr <- par("usr"); on.exit(par(usr)) # Saves current "usr" and resets on exit
  par(usr = c(0, 1, 0, 1))             # Set plot size to 1 x 1
  r <- cor(x, y, method = method, use = "pair")               # correlation coef
  p <- cor.test(x, y, method = method)$p.val                  # p-value
  n <- sum(complete.cases(x, y))                              # How many data pairs
  txt <- format(r, digits = 3)                                # Format r-value
  txt1 <- format(p, digits = 3)                                 # Format p-value
  txt2 <- paste0("r= ", txt, '\n', "p= ", txt1, '\n', 'n= ', n) # Make panel text
  text(0.5, 0.5, txt2, cex = cex.cor, ...)                      # Place panel text
  options(warn = 0)                                             # Reset warning
}

  
  # Default method ("pearson")
  pairs(trees, lower.panel = panel.cor, cex.cor = 2)


scatterplotMatrix(~pho + log10(nit) + amm + oxy + dbo, data = FishV, diagonal = list(method = "boxplot"), col = "dark blue")

scatterplotMatrix(~pho + nit + amm + oxy + dbo, data = FishV, diagonal = list(method = "boxplot"), col = "dark blue")

# ***********************************************************
# Barcharts      
# ***********************************************************


require(dplyr)
require(ggplot2)

library(Hmisc)




Ammplot <- ggplot(Errors, aes(x = factor(FishV$das), y = FishV$amm)) +  
  geom_bar(stat = "identity", width = 1, colour = "black", fill = "steelblue") + # We need this command ggplot is trying to bin and summarize the beaches (remember, geom_bar defaults to stat = stat_count); obviously this will not work.
  xlab("Distance from source") + # add x label
  ylab("Ammonia Levels") + #add y label
  ggtitle("Ammonia levels with distance downstream") + #add main title
  theme(plot.title = element_text(size = 8, face = "bold")) #+ # Change size of title such that it fits on plot screen

Ammplot
  
  

Phosplot <- ggplot(Errors, aes(x = factor(FishV$das), y = FishV$pho)) +  
  geom_bar(stat = "identity", width = 1, colour = "black", fill = "red") + # We need this command ggplot is trying to bin and summarize the beaches (remember, geom_bar defaults to stat = stat_count); obviously this will not work.
  xlab("Distance from source") + # add x label
  ylab("Phosphate Levels") + #add y label
  ggtitle("Phosphate levels with distance downstream") +
  theme(plot.title = element_text(size = 8, face = "bold")) #+
  

Nitplot <- ggplot(Errors, aes(x = factor(FishV$das), y = FishV$nit)) +  
  geom_bar(stat = "identity", width = 1, colour = "black", fill = "green") + # We need this command ggplot is trying to bin and summarize the beaches (remember, geom_bar defaults to stat = stat_count); obviously this will not work.
  xlab("Distance from source") + # add x label
  ylab("Nitrogen Levels") + #add y label
  ggtitle("Nitrogen levels with distance downstream")+ #add main title
  theme(plot.title = element_text(size = 8, face = "bold")) #+
  

Oxyplot <- ggplot(Errors, aes(x = factor(FishV$das), y = FishV$oxy)) +  
  geom_bar(stat = "identity", width = 1, colour = "black", fill = "yellow") + # We need this command ggplot is trying to bin and summarize the beaches (remember, geom_bar defaults to stat = stat_count); obviously this will not work.
  xlab("Distance from source") + # add x label
  ylab("Dissolved Oxygen Levels") + #add y label
  ggtitle("Oxygen levels with distance downstream") +   #add main title
  theme(plot.title = element_text(size = 8, face = "bold")) +
  scale_x_discrete(breaks=NULL)


#plot_grid(Ammplot, Phosplot, Nitplot, Oxyplot, labels = "AUTO") # Utilises cowplot library - plots all of these 4 barcharts on the same screen -
# essentially the ggplot equivalent of par(mfrow). Labels auto indicates use what has already been coded above for the labels.



require(gridExtra)
plot1 <- qplot(1)
plot2 <- qplot(1)
grid.arrange(plot1, plot2, ncol=2)

<- 



# ***********************************************************
# Bubble chart to illustrate longitudinal variation        
# ***********************************************************
par(mfrow = c(1,1)) #

Fish_ord <- FishV[order(FishV$das), ]

Radius <- sqrt(Fish$fish_abun/ pi) 

par(las= 1) 
symbols(FishV$das, Fish$fish_SR, circles = Radius, inches = 0.25,
        xlab = "Distance from start of sampling", 
        ylab = "Species Richness", 
        xlim = c(0, 475), ylim = c(0, 30),
        fg = "black", bg = "orange")
M0 <- lm(Fish$fish_SR ~ FishV$das, data = Fish) # Linear best fit line added to capture the overall pattern - to do such a linear model is created and then an abline used to add this to the plot.
abline(M0)
abline(12.5, 0:400, col = "red")

mean(Fish$fish_SR)
sd(Fish$fish_SR)
mean()


ggplot(Fish_ord, aes(x=FishV$das, y=Fish$fish_SR, size=Radius, label=Fish$Site_no)) + # sets graph parameters
  geom_point(shape = 21, colour = "black", fill = "orange") + # sets circle size and fill colours
  scale_size_area("size") +
  theme_bw(base_size = 18) + # override default theme to make it B/W
  ylim(0, 26) + xlim(0, 460) + # set axis limits
  ylab(expression(paste("Species Richness ")))+ # y axis labels
  xlab(expression(paste("Distance from start of sampling", "(", m, )", sep=")))+ # x axis labels
  labs(size=(expression(paste("Catchment Area ", "(",km^2,")")))) 
M0 <- lm(Fish$fish_SR ~ FishV$das, data = Fish) # Linear best fit line added to capture the overall pattern - to do such a linear model is created and then an abline used to add this to the plot.
abline(M0)
abline(12.5, 0:400, col = "red")# Caption label
# NOTE we removed the text in the bubbles because it obscures the patterns




# **********************************************************
# Shannon's Diversity Index                                  
# **********************************************************

str(Fish) # Check the structure of the fish species data
FishSpOnly <- Fish[-1] # Remove the first column as it is site numbers
FishSpOnly2 <- FishSpOnly[-29] # Remove the 29th row as it is species richness
FishSpOnly3 <- FishSpOnly2[-28] # Remove the 28th row as it is species abundance
FishSpOnly3 # This gives us purely the species such that a Shannon's species diversity index can be carried out

# From here on, this section uses the vegan package downloaded earlier.

speciescurve <- specaccum(comm = FishSpOnly3, method = "random", permutations = 1000)
plot(speciescurve) 
# Species accumulation plot allows one to see if the sampling was extensive enough. This can be seen by the graph flattening out, something of which 
# can be seen from between 15-20 on-wards. It uses a reshuffling method by essentially resampling the data in a random manner, the amount of times of 
# which is 1000. The plot function is used to plot this curve. The bars or hatch marks, are confidence levels, the smaller the bar the greater the 
# increase in confidence. Hence when no bars are present, one can be completely confidnent that the sampling has provided accurate results. 

Shannon<- diversity(FishSpOnly3, index = "shannon")
Shannon
mean(Shannon)
sd(Shannon)
# Shannon's index tells us how diverse the species sampling is. The higher the number, the greater the diversity - this is controlled by the number
# of species and the evenness of their abundance. A mean of this gives us an overall species diversity reading, while individual species diversity 
# readings for each column can be seen by just typing Shannon again (species are in the order of the data frame)
# According to a source below the highest diversity reading for 100 species would be ~4.6 and hence 2.1 is a relatively high diversity reading for 
# 27 species, hence species diversity is high. 
# https://www.omnicalculator.com/ecology/shannon-index#:~:text=There's%20no%20upper%20limit%20to%20the%20index.&text=To%20give%20you%20some%20perspective,%2C%20for%201%2C000%2C000%20species%3A%2013.816%20.

even map longitudinal patterns in particular species
