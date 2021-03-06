% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mopaTrain.R
\name{mopaTrain}
\alias{mopaTrain}
\title{Easy species distribution modeling and cross validation}
\usage{
mopaTrain(y, x, k = 10, algorithm = c("glm", "svm", "maxent", "mars", "rf",
  "cart.rpart", "cart.tree"), algorithm.args = NULL, weighting = FALSE,
  threshold = NULL, diagrams = FALSE, tuneRF.args = NULL)
}
\arguments{
\item{y}{Object returned by function \code{\link[mopa]{pseudoAbsences}} or data frame or list/s of data 
frames with coordinates
 in the first two columns and presence/absence (1=presence, 0=absence) in the third column.}

\item{x}{RasterStack ot list of RasterStacks of variables for modeling, a.k.a baseline environment/climatology}

\item{k}{Integer. Number of folds for cross validation. Default is 10}

\item{algorithm}{Character string of the algorithms for modeling. Options are the following: 
"glm", "svm", "maxent", "mars", "rf", "cart.rpart" and "cart.tree" (see details)}

\item{algorithm.args}{Further arguments to be passed to the selected algorithm for modeling (functions involved 
are described in details).}

\item{weighting}{Logical for model fitting with weighted presence/absences. Applicable for algorithms "glm", "mars", 
"rf", cart.tree and "cart.rpart". Default is FALSE.
The processing time is considerably increased if weighting option is selected when the  
"mars" algorithm (see \code{\link[earth]{earth}} is applied.}

\item{threshold}{Cut value between 0 and 1 to calculate the confusion matrix. Default is NULL (see Details).}

\item{diagrams}{Logical. Only applied if \code{x} contains data for different background extents 
(see \code{\link[mopa]{backgroundRadius}} and \code{\link[mopa]{pseudoAbsences}}). Should diagrams of 
AUC extent fitting be printed? default is FALSE.}

\item{tuneRF.args}{list of arguments from function \code{\link[randomForest]{tuneRF}}. Only used when algorihm = "rf"}
}
\value{
A list of six components is returned for each species in \code{x}:
\itemize{
 \item{\code{$model} }{fitted model using all data for training}
 \item{\code{$auc} }{AUC statistic in the cross validation}
 \item{\code{$kappa} }{kappa statistic in the cross validation}
 \item{\code{$tss} }{true skill statistic in the cross validation }
 \item{\code{$fold.models} }{fitted models of each data partition for cross validation}
 \item{\code{$ObsPred} }{cross model prediction (e.g. for further assessment of model accuracy)}
 }
}
\description{
Species distribution modeling and k-fold cross validation 
for a set of presence/absence data per species, also considering different background 
extents (optional). Algorithms supported are "glm", "svm", "maxent", "mars", "rf", "cart.rpart" 
and "cart.tree"
}
\details{
This function calculates the AUC with the function \code{\link[PresenceAbsence]{auc}} from package 
\pkg{PresenceAbsence}. \strong{Note:} Package \pkg{SDMTools} must be detached. 

If \code{threshold} is not specified the value that maximisez the TSS (true skill statistic) is 
used to calculate the confusion matrix.



If \code{y} contains data for different background extents (see \code{\link[mopa]{backgroundRadius}} and
\code{\link[mopa]{pseudoAbsences}}), \code{\link[mopa]{mopaTrain}} performs the species distribution modeling for 
each different background extent, and fits obtained AUCs (corresponding to different background extents) 
to three non linear models (Michaelis-Menten, exponential2 and exponential3). 
The model that scores the lowest error is automatically selected to extract the Vm coefficient (equation 1 in 
Iturbide et al., 2015). Then, the minimum extent at which the AUC surpasses the Vm value is selected 
as the threshold extent (see Figure 3 in Iturbide et al., 2015), being the corresponding fitted SDM the 
one returned by \code{mopaFitting}. If argument \code{diagrams} is set to TRUE, A fitted model plot 
(as in Fig. 3 in Iturbide et al., 2015) is printed in the plotting environment.

  
\code{mopaTrain} uses the algorithm implementations of the following functions and R packages: 
\itemize{
\item{"mars" }{function \code{\link[earth]{earth}} from package \pkg{earth}} 
\item{"rf" }{function \code{\link[ranger]{ranger}} from package \pkg{ranger}} 
\item{"maxent" }{function \code{\link[dismo]{maxent}} from package \pkg{dismo}} 
\item{"cart.rpart" }{function \code{\link[rpart]{rpart}} from package \pkg{rpart}}
\item{"svm" }{function \code{\link[e1071]{best.svm}} from package \pkg{e1071}}
\item{"cart.tree" }{function \code{\link[tree]{tree}} from package \pkg{tree}} 
\item{"glm" }{function \code{\link[stats]{glm}} from  package \pkg{stats}}
}

For example, when appying "glm", further arguments from function \code{\link[stats]{glm}} can be 
passed to \code{mopaTrain} by using \code{algorithm.args}.
}
\examples{
## Load presence data
data(Oak_phylo2)

## Load climate data
destfile <- tempfile()
data.url <- "https://raw.githubusercontent.com/SantanderMetGroup/mopa/master/data/biostack.rda"
download.file(data.url, destfile)
load(destfile, verbose = TRUE)

## Spatial reference
r <- biostack$baseline[[1]]
## Create background grid
bg <- backgroundGrid(r)

## Generate pseudo-absences
RS_random <-pseudoAbsences(xy = Oak_phylo2, background = bg$xy, 
                           exclusion.buffer = 0.083*5, prevalence = -0.5, kmeans = FALSE)
## Model training
fittedRS <- mopaTrain(y = RS_random, x = biostack$baseline, 
                      k = 10, algorithm = "glm", weighting = TRUE)
## Extract fitted models
mods <- extractFromModel(models = fittedRS, value = "model")

}
\references{
Iturbide, M., Bedia, J., Herrera, S., del Hierro, O., Pinto, M., Gutierrez, J.M., 2015. 
A framework for species distribution modelling with improved pseudo-absence generation. Ecological 
Modelling. DOI:10.1016/j.ecolmodel.2015.05.018.
}
\seealso{
\code{\link[mopa]{mopaPredict}}, \code{\link[mopa]{pseudoAbsences}}, \code{\link[mopa]{backgroundGrid}}, 
\code{\link[mopa]{OCSVMprofiling}}, \code{\link[mopa]{backgroundRadius}}, \code{\link[mopa]{extractFromModel}}
}
\author{
M. Iturbide
}
