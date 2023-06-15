#a)
data(iris)
install.packages("ggplot2")
library(ggplot2)
ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_boxplot() +
  labs(x = "Species", y = "Sepal Length") +
  ggtitle("Boxplot of Sepal Length by Species")

ggplot(iris, aes(x = Species, y = Petal.Length, fill = Species)) +
  geom_boxplot() +
  labs(x = "Species", y = "Petal Length") +
  ggtitle("Boxplot of Petal Length by Species")
ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
  geom_point() +
  labs(x = "Sepal Length", y = "Petal Length") +
  ggtitle("Scatterplot of Sepal Length and Petal Length by Species")
#b)
library(imager)

flip <- function(image) {
 
  width <- dim(image)[2]
  height <- dim(image)[1]
  
 
  flipped_image <- image(height, width)
  
 
  for (y in 1:height) {
    for (x in 1:width) {
      
      pixel <- image[y, x, ]
      
      
      flipped_x <- width - x + 1
      
    
      flipped_image[y, flipped_x, ] <- pixel
    }
  }
  return(flipped_image)
}

image <- load.image("PXL_20230610_183347161.jpg")
flipped_image <- flip(image)
par(mfrow = c(1, 2))
plot(image, main = "Original Image")
plot(flipped_image, main = "Flipped Image")

#c)
library(MASS)

data(ships)

incident_counts <- tapply(ships$incidents, ships$type, sum)

barplot(incident_counts, 
        main = "Total Number of Incidents by Ship Type",
        xlab = "Ship Type",
        ylab = "Total Number of Incidents",
        col = "steelblue")
