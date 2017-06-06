#' @title Extract values from objects or list of objects
#' @description  Extrac objects from lists returned by function \code{\link[mopa]{mopaPredict}}.
#' 
#' 
#' @param predictions Listed lists of objects (e.g. as returned by \code{\link[mopa]{mopaPredict}})
#' @param value Character pointing to the name of component/s in the list
#' 

#' 
#' 
#' @author M. Iturbide 
#' 
#' @examples
#' ## Load and prepare presence data
#' data(Q_pubescens)
#' presences <- Q_pubescens[sample(1:300, size = 100),]
#' 
#' ## Load climate data
#' destfile <- tempfile()
#' data.url <- "https://raw.githubusercontent.com/SantanderMetGroup/mopa/master/data/biostack.rda"
#' download.file(data.url, destfile)
#' load(destfile, verbose = TRUE)
#' 
#' ## Create the background of the whole study area
#' bg <- backgroundGrid(biostack$baseline$bio1)
#' 
#' ## Generate pseudo-absences
#' RS_random <-pseudoAbsences(xy = presences, background = bg$xy, 
#'                            exclusion.buffer = 0.083*5, prevalence = -0.5, kmeans = FALSE)
#' ## Model training
#' fittedRS <- mopaTrain(y = RS_random, x = biostack$baseline, 
#'                       k = 10, algorithm = "glm", weighting = TRUE)
#' ## Extract fitted models
#' mods <- extractFromModel(models = fittedRS, value = "model")
#' 
#' ## Model prediction
#' preds <- mopaPredict(models = mods, newClim = biostack$future)
#' predsMPI <- extractFromPrediction(predictions = preds, value = "MPI")
#' spplot(predsMPI)
#' 
#' @export
#' 
extractFromPrediction <- function(predictions, value){
  if(class(predictions) == "list")  predictions <- stack(unlist(predictions))
  rasters <- predictions
  nm <- names(rasters)
  ind <- grep(value, nm)
  r <- rasters[[ind]]
  if(length(ind) > 1) names(r) <- nm[ind]
  
  return(r)
}

#end