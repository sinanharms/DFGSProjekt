library(R6)
library(rjson)

ParseError = R6Class("ParseError",
                     inherit = Exception)