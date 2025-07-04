set.seed(42)
n <- 100  # number of points
d <- 10000  # dimensions
X <- matrix(runif(n * d), nrow = n, ncol = d)

# compute pairwise distances
D <- dist(X)
summary(as.vector(D))
# Min.  1st Qu.   Median  Mean  3rd Qu.     Max. 
# 40.01   40.71   40.87   40.87   41.03   41.67
# Тут отклонения от среднего = 0.16

n <- 100  # number of points
d <- 2  # dimensions
X <- matrix(runif(n * d), nrow = n, ncol = d)

# compute pairwise distances
D <- dist(X)
summary(as.vector(D))
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.007394 0.335639 0.520105 0.529813 0.714546 1.333944 
# Тут отклонение от среднего = 0.184733
# т.е при меньшей размерности расстояние между точками боллее разбросанное и есть
# хоть какая-то разница между ними 