# Create a REST API using Plumber ----

# This requires the "plumber" package that was installed and loaded earlier in
# STEP 1. The commenting below makes R recognize the code as the definition of
# an API, i.e., #* comments.

# Install and Load the Required Packages ----
if (require("languageserver")) {
  require("languageserver")
} else {
  install.packages("languageserver", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
## plumber ----
if (require("plumber")) {
  require("plumber")
} else {
  install.packages("plumber", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
## caret ----
if (require("caret")) {
  require("caret")
} else {
  install.packages("caret", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

loaded_farming_caret_model_lda <- readRDS("./models/saved_farming_caret_model_lda.rds")

#* @apiTitle Crop Selection Model API

#* @apiDescription Used to predict the crop suitable based on the inputted soil parameters

#* @param arg_N The Nitrogen level in the soil
#* @param arg_P The Phosphorous level in the soil
#* @param arg_K The Potassium level in the soil
#* @param arg_temperature The temperature in degree Celcius
#* @param arg_humidity The humidity level
#* @param arg_ph The soil's pH level
#* @param arg_rainfall The rainfall level in mm

#* @get /label

predict_label <-
  function(arg_N, arg_P, arg_K, arg_temperature, arg_humidity,
           arg_ph, arg_rainfall) {
    # Create a data frame using the arguments
    to_be_predicted <-
      data.frame(N = as.numeric(arg_N),
                 P = as.numeric(arg_P),
                 K = as.numeric(arg_K),
                 temperature = as.numeric(arg_temperature),
                 humidity = as.numeric(arg_humidity),
                 ph = as.numeric(arg_ph),
                 rainfall = as.numeric(arg_rainfall))
    # Make a prediction based on the data frame
    predict(loaded_farming_caret_model_lda, to_be_predicted)
  }