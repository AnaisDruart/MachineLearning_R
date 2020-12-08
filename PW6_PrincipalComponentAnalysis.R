#'## Loading Data
#'### 1. Download the iris dataset 

iris = read.table("iris.data",sep = ",", header = T)

#'## Exploratory analysis
#'### 2. Compare the means and the quartiles of the 3 different flower classes for the 4 different features (Plot 4 boxplots into the same figure).
par(mfrow=c(2,2))

boxplot(iris$sepal_length~iris$class,
        xlab = "iris$class",
        ylab = "sepal_length")

boxplot(iris$sepal_width~iris$class,
        xlab = "iris$class",
        ylab = "sepal_width")
boxplot(iris$petal_length~iris$class,
        xlab = "iris$class",
        ylab = "petal_length")
boxplot(iris$petal_width~iris$class,
        xlab = "iris$class",
        ylab = "petal_width")

#'### 3. To explore how the 3 different flower classes are distributed along the 4 different features, visualize them via histograms using the following code.

# Let's use the ggplot2 library
# ggplot2 is the most advanced package for data visualization
# gg corresponds to The Grammar of Graphics.
library(ggplot2) #of course you must install it first if you don't have it already
library(gridExtra)   # Side-by-side ggplot graph

# histogram of sepal_length
box1=ggplot(iris, aes(x=sepal_length, fill=class)) +
  geom_histogram(binwidth=.2, alpha=.5)
# histogram of sepal_width
box2=ggplot(iris, aes(x=sepal_width, fill=class)) +
  geom_histogram(binwidth=.2, alpha=.5)
# histogram of petal_length
box3=ggplot(iris, aes(x=petal_length, fill=class)) +
  geom_histogram(binwidth=.2, alpha=.5)
# histogram of petal_width
box4=ggplot(iris, aes(x=petal_width, fill=class)) +
  geom_histogram(binwidth=.2, alpha=.5)

grid.arrange(box1,box2,box3,box4,ncol=2,nrow=2)

#'## PCA using princomp()
#'### 4. Apply a PCA on the Iris dataset using the princomp() function and interpret the results.

pcairis=princomp(iris[,-5], cor=T) 
# Note that we take only the numerical columns to apply PCA.
# now pcairis is a R object of type princomp
# To display the internal structure of pcairis
str(pcairis)
#ans> List of 7
#ans>  $ sdev    : Named num [1:4] 1.706 0.96 0.384 0.144
#ans>   ..- attr(*, "names")= chr [1:4] "Comp.1" "Comp.2" "Comp.3" "Comp.4"
#ans>  $ loadings: 'loadings' num [1:4, 1:4] 0.522 -0.263 0.581 0.566 0.372 ...
#ans>   ..- attr(*, "dimnames")=List of 2
#ans>   .. ..$ : chr [1:4] "sepal_length" "sepal_width" "petal_length" "petal_width"
#ans>   .. ..$ : chr [1:4] "Comp.1" "Comp.2" "Comp.3" "Comp.4"
#ans>  $ center  : Named num [1:4] 5.84 3.05 3.76 1.2
#ans>   ..- attr(*, "names")= chr [1:4] "sepal_length" "sepal_width" "petal_length" "petal_width"
#ans>  $ scale   : Named num [1:4] 0.825 0.432 1.759 0.761
#ans>   ..- attr(*, "names")= chr [1:4] "sepal_length" "sepal_width" "petal_length" "petal_width"
#ans>  $ n.obs   : int 150
#ans>  $ scores  : num [1:150, 1:4] -2.26 -2.09 -2.37 -2.3 -2.39 ...
#ans>   ..- attr(*, "dimnames")=List of 2
#ans>   .. ..$ : NULL
#ans>   .. ..$ : chr [1:4] "Comp.1" "Comp.2" "Comp.3" "Comp.4"
#ans>  $ call    : language princomp(x = iris[, -5], cor = T)
#ans>  - attr(*, "class")= chr "princomp"

# To see the variance explained by the the pcs
summary(pcairis) 
#ans> Importance of components:
#ans>                        Comp.1 Comp.2 Comp.3  Comp.4
#ans> Standard deviation      1.706  0.960 0.3839 0.14355
#ans> Proportion of Variance  0.728  0.230 0.0368 0.00515
#ans> Cumulative Proportion   0.728  0.958 0.9948 1.00000

# To plot the variance explained by each pc
plot(pcairis) 

# To plot together the scores for PC1 and PC2 and the 
# variables expressed in terms of PC1 and PC2.
biplot(pcairis) 

#'## Deeper PCA using factoextra package
#'
#'### 5 Using factoextra packag plot the following:
#'
#'#### The scree plot.
#'#### The graph of individuals.
#'#### The graph of variables.
#'#### The biplot graph.
#'#### The contributions of the variables to the first 2 principal components.
#'

library(factoextra)

# the scree plot
fviz_eig(pcairis, addlabels = TRUE)
# the graph of individuals
fviz_pca_ind(pcairis,
             col.ind = "contrib", # Color by their contribution to axes
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     
)
# the graph of variables
fviz_pca_var(pcairis,
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     
)
# the biplot graph
fviz_pca_biplot(pcairis, repel = TRUE,
                col.var = "#2E9FDF", 
                col.ind = "#696969"  
)
# the contributions of the variables to the first 2 principal components
fviz_pca_biplot (pcairis, select.ind = list (contrib = 2),
                 select.var = list (contrib = 2),
                 ggtheme = theme_minimal())

#'## Step-by-step PCA
#'
#'### 6. First step, split the iris dataset into data X and class labels y.
X <- iris[,-5]
y <- iris[,5]


#'### 7. Scale the 4 features. Store the scaled matrix into a new one (for example, name it X_scaled).
X_scaled <- scale(X, center=TRUE, scale=TRUE)


#'### 8. The classic approach to PCA is to perform the eigendecomposition on the covariance matrix Σ, which is a  p×p matrix where each element represents the covariance between two features. Compute the Covariance Matrix of the scaled features (Print the results).
X_covmat <- cov(X_scaled)
X_covmat

#'### 9. Perform an eigendecomposition on the covariance matrix. Compute the Eigenvectors and the Eigenvalues (you can use the eigen() function). What do you obtain?
X_eig <- eigen(X_covmat)
X_eig

#'### 10. Perform an eigendecomposition of the standardized data based on the correlation matrix
X_cormat <- cor(X_scaled)
eigen(X_cormat)
# same results as the eigendecomposition on the covariance matrix
# it's normal since the correlation matrix can be understood as the normalized covariance matrix

#'### 11. Perform an eigendecomposition of the raw data based on the correlation matrix. Compare the obtained results with the previous question.
X_raw_cormat <- cor(X)
eigen(X_raw_cormat)
# all three approaches yield the same eigenvectors and eigenvalue pairs

#'### 12. Calculate the individual explained variation and the cumulative explained variation of each principal component. Show the results.

# short solution :
summary(pcairis)

#long solution
ind_var <- function(index){
  return(X_eig$values[index] / sum(X_eig$values))
  }

# Comp1
comp1 <- ind_var(1)
comp1_cum <- comp1
# Comp2
comp2 <- ind_var(2)
comp2_cum <- comp2 + comp1_cum
# Comp3
comp3 <- ind_var(3)
comp3_cum <- comp3 + comp2_cum
# Comp4
comp4 <- ind_var(4)
comp4_cum <- comp4 + comp3_cum

comp1
# account for 72,8% of the variance
comp1_cum
comp2
# account for 23% of the variance
comp2_cum
comp3
# account for 3,7% of the variance
comp3_cum
comp4
# account for 0,5% of the variance
comp4_cum

ind_exp_var <- c(comp1,comp2,comp3,comp4)

#'### 13. Plot the individual explained variation.(scree plot)
#'#### Version with the plot function
plot(ind_exp_var)
lines(ind_exp_var)

#'### 14. Construct the projection matrix that will be used to transform the Iris data onto the new feature subspace.
mat_proj <- X_eig$vectors[,c(1,2)]

#'### 15. Compute Y (Recall the Y is the matrix of scores, A is the matrix of loadings).
Y_final <- X_scaled %*% mat_proj
Y_final

#'### 16. Plot the observations on the new feature space. Name the axis PC1 and PC2.
#'### 17. On the same plot, color the observations (the flowers) with respect to their flower classes.
plot(Y_final, xlab="PC1",ylab="PC2", col="red", pch=20)
points(Y_final[51:100,], col = "blue", pch = 20)
points(Y_final[101:150,], col = "green", pch = 20)



