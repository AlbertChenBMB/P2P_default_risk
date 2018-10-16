# create initial values for theta
theta <- rep(0.5, 100)

# initialize other needed parameters
alpha <- 0.9
threshold <- 1
max_iter <- 100

# generate random matrix of predictors
train_x <- lapply(1:100, function(elt) sample(1:10000, 100, replace = TRUE))
train_x <- Reduce(cbind, x)

# set seed for reproducibility
set.seed(2000)

# generate random labels
train_y <- sample(c(1, -1), 100, replace = TRUE)

# get trained theta
train_theta_result <- get_final_theta(theta, train_x , y, 1, .9, max_iterations = 100)

# run trained model on dataset
train_result_predictions <- lr(train_x, train_theta_result)