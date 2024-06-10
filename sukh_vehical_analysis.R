install.packages("ggvis")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("plm")

library('ggvis')
library('tidyverse')
library('ggplot2')
library('plm')


scooty_buyers = read.csv('D:/RProject/scooty_buyers.csv', header=T, na.strings='')
head(vehical_buyers)
--------------------------------------------
  class(vehical_buyers)
---------------------------------------------
  str(vehical_buyers)
-----------------------------------------------
  summary(vehical_buyers)
------------------------------------------------
  levels(vehical_buyers$Gender)
----------------------------------------------
  vehical_buyers$Marital.Status <- as.factor(vehical_buyers$Marital.Status)
vehical_buyers$Gender <- as.factor(vehical_buyers$Gender)
vehical_buyers$Home.Owner <- as.factor(vehical_buyers$Home.Owner)
vehical_buyers$Purchased.vehical <- as.factor(scooty_buyers$Purchased.vehical)
--------------------------------------------------
  str(vehical_buyers)
------------------------------------------------
  colSums(is.na(vehical_buyers))
----------------------------------------------------
  summary(vehical_buyers)
----------------------------------------------
  hist(vehical_buyers$Income)
----------------------------------------------
  hist(vehical_buyers$Children, breaks = 20)
------------------------------------------------
  hist(vehical_buyers$Cars, breaks = 15)
-------------------------------------------------
  hist(vehical_buyers$Age)
---------------------------------------------
  median(na.omit((vehical_buyers$Income)))
median(na.omit((vehical_buyers$Age)))
----------------------------------------------
  vehical_buyers_clean <- vehical_buyers
colSums(is.na(vehical_buyers_clean))
-------------------------------------------------
  # Income replaced with Median
  vehical_buyers_clean$Income[is.na(vehical_buyers_clean$Income)] <- 
  median(na.omit((vehical_buyers$Income)))

# Age replaced with Median
vehical_buyers_clean$Age[is.na(vehical_buyers_clean$Age)] <- 
  median(na.omit((vehical_buyers$Age)))

colSums(is.na(vehical_buyers_clean))
------------------------------------------------
  
  get_mode <- function(x) {                 
    unique_x <- unique(x)
    tabulate_x <- tabulate(match(x, unique_x))
    unique_x[tabulate_x == max(tabulate_x)]
  }
-----------------------------------------------
  # Marital Status replaced with Mode
  vehical_buyers_clean$Marital.Status[is.na(vehical_buyers_clean$Marital.Status)] <- 
  get_mode(vehical_buyers$Marital.Status)

# Gender replaced with Mode
vehical_buyers_clean$Gender[is.na(vehical_buyers_clean$Gender)] <- 
  get_mode(scooty_buyers$Gender)

# Children replaced with Mode
vehical_buyers_clean$Children[is.na(vehical_buyers_clean$Children)] <- 
  get_mode(vehical_buyers$Children)

# Home Owner replaced with Mode
vehical_buyers_clean$Home.Owner[is.na(vehical_buyers_clean$Home.Owner)] <- 
  get_mode(scooty_buyers$Home.Owner)

colSums(is.na(vehical_buyers_clean))

---------------------------------------------------
  
  # Cars replaced with Mean
 vehical_buyers_clean$Cars[is.na(vehical_buyers_clean$Cars)] <- 
  mean(vehical_buyers$Cars, na.rm = TRUE)

colSums(is.na(vehical_buyers_clean))
------------------------------------------------------
  write.csv(scooty_buyers_clean,"vehical_buyers_clean.csv", quote = FALSE, row.names = TRUE)
------------------------------------------------------
  vehical_buyers <- vehical_buyers_clean
-------------------------------------------------------
  
  counts <- table(vehical_buyers$Cars, vehical_buyers$Gender)
barplot(counts, main = '',
        xlab="Number of Gears",
        legend = rownames(counts))
----------------------------------------------------
  
  plot(vehical_buyers$Income, type= "p", col="blue")
---------------------------------------------------
  
  ggplot(vehical_buyers, aes(x = Age)) +
  geom_histogram()
---------------------------------------------------
  
  plot(density(vehical_buyers$Income), main='Income Density Spread', col="red")
------------------------------------------------
  ggplot(vehical_buyers,
         aes(y = Age, x = Gender)) +
  geom_point()
-----------------------------------------------
  ggplot(vehical_buyers,
         aes(y = Age, x = Income)) +
  geom_point()
-------------------------------------------------
  
  p3 <- ggplot(vehical_buyers,
               aes(x = Age,
                   y = Income)) + 
  theme(legend.position="top",
        axis.text=element_text(size = 6))
p4 <- p3 + geom_point(aes(color = Age),
                      alpha = 0.5,
                      size = 1.5,
                      position = position_jitter(width = 0.25, height = 0))
p4 +
  scale_x_discrete(name="Income") +
  scale_color_continuous(name="", low = "blue", high = "red")

------------------------------------------------
  
  p5 <- ggplot(vehical_buyers, aes(x = Age, y = Occupation))
p5 + geom_line(aes(color = Age))

-------------------------------------------------
  (p5 <- p5 + geom_line() +
     facet_wrap(~Gender, ncol = 10))
--------------------------------------------------
  
  boxplot(vehical_buyers$Income, main = 'Income Boxplot')
boxplot(vehical_buyers[,c(1,4)], main='Multiple Box plots')
---------------------------------------------------
  
  OutVals = boxplot(vehical_buyers$Income)$out
print(OutVals)

which(vehical_buyers$Income %in% OutVals)

x = vehical_buyers$Income [!(vehical_buyers$Income %in% OutVals) ]
boxplot(x)
-----------------------------------------------------
  summary(vehical_buyers)


------------------------------------------------------
  
  columns_to_correlate <- c("Income", "Children", "Age", "Cars")

# Subset the data based on the selected columns
subset_data <- vehical_buyers[, columns_to_correlate]

# Calculate the correlation matrix
correlation_matrix <- cor(subset_data)

# Print correlation matrix
print(correlation_matrix)

