## This code block is to re-install a particular version of H2O
# START
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }
#install.packages("h2o", repos=(c("file:///Users/arno/h2o/target/R", getOption("repos"))))
install.packages("h2o", repos=(c("http://s3.amazonaws.com/h2o-release/h2o/master/1545/R", getOption("repos")))) #choose a build here
# END
library(statmod)
library(h2o)
library(stringr)

# Launch H2O directly on localhost
h2o.init(ip = "localhost", port = 54321, startH2O = TRUE)

# Import data
train_hex = h2o.uploadFile("training.csv", destination_frame = "train_hex")
test_hex = h2o.uploadFile("sorted_test.csv", destination_frame = "test_hex")

# Group variables
vars <- colnames(train_hex)
spectra <- vars[seq(2,3579,by=20)] # "poor man's dimensionality reduction": take every N-th column of spectral data
extra <- vars[3580:3595]
targets <- vars[3596:3600]
predictors <- c(spectra, extra)

for( resp in 1:length(targets) )
{
  model <- h2o.deeplearning(x = predictors, y = targets[resp], training_frame = train_hex)
  test_pred <- h2o.predict(model, test_hex)
  names(test_pred) = targets[resp]
  if (resp == 1) {
    test_preds_blend <- test_pred[,1]
  } else {
    test_preds_blend <- cbind(as.matrix(test_preds_blend), as.matrix(test_pred[,1]))
  }
}

write.csv(test_preds_blend, "test_preds.csv")