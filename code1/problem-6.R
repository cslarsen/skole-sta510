rm(list=ls())

x <- c(13.89, 13.39, 12.20, 14.35, 14.10, 13.39, 13.96, 14.15, 13.69, 12.57)
y <- c(13.99, 13.39, 12.65, 14.25, 13.99, 13.09, 13.66, 14.25, 13.36, 12.57)

plot(x, y, main="Gas prices scatterplot", pch=19)

correlation <- cor(x, y)
cat("Correlation:", correlation, "\n")