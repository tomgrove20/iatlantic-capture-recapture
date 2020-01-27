### CJS model applied to Bermuda data ###
############## Tom Grove ################
############# 05/01/2020 ################

## Using package marked: https://cran.r-project.org/web/packages/marked/marked.pdf

library(marked)
library(RMark)
library(Rcapture)

## Primary samples: years
## Secondary samples: days

## starting by using RMark on a dipper example

require(RMark)
data(dipper)
head(dipper)

dipper.processed <- process.data(dipper,model="CJS",begin.time=1980,groups="sex")
dipper.ddl <- make.design.data(dipper.processed)
my_first_model <- mark(dipper.processed,dipper.ddl)

# So that was just using a simple cjs model (that was specified and is indicated in the output by 'Name : Phi(~1)p(~1)'. Now let's try to apply this simple model to our Bermuda stuff

hb.processed <- process.data(capture_history, model="CJS", begin.time=2005)
hb.ddl <- make.design.data(hb.processed)
hb_first_model <- mark(hb.processed, hb.ddl)

# This worked! No errors. Now I just have to figure out what this means ...
# Next, let's try out this link (uses RMark) to get abundance estimation: https://rstudio-pubs-static.s3.amazonaws.com/201392_14ed145562b34fc785f5ff2ca00ba011.html