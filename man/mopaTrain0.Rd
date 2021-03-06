% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mopaTrain.R
\name{mopaTrain0}
\alias{mopaTrain0}
\title{Easy species distribution modeling and cross validation}
\usage{
mopaTrain0(y, x, k = 10, algorithm = c("glm", "svm", "maxent", "mars", "rf",
  "cart.rpart", "cart.tree"), algorithm.args = NULL, weighting = FALSE,
  threshold = NULL, diagrams = FALSE, tuneRF.args = NULL,
  plotnames = "unnamed")
}
\arguments{
\item{y}{RasterStack of variables for modelling}

\item{x}{Object returned by function \code{link[mopa]{pseudoAbsences}} or list/s of data frames with coordinates
in the first two columns and presence/absence (1=presence, 0=absence) in the third column.}

\item{k}{Integer. Number of folds for cross validation. Default is 10}

\item{algorithm}{Any character of the following: "glm", "svm", "maxent", "mars", "rf", "cart.rpart" 
or "cart.tree"}

\item{algorithm.args}{Further arguments to be passed to the selected algorithm for modeling (functions involved 
are described in details)}

\item{weighting}{Logical for "glm", "mars" and "rf" fitting with weighted presence/absences-s. Default is FALSE.}

\item{threshold}{Cut value between 0 and 1 to calculate the confusion matrix. Default is NULL (see Details).}

\item{diagrams}{logical. Only applied if \code{x} contains data for different background extents 
(see \code{\link[mopa]{backgroundRadius}} and \code{\link[mopa]{pseudoAbsences}}). Should diagrams of 
AUC extent fitting be printed? default is FALSE.}

\item{tuneRF.args}{list of arguments from function \code{\link[randomForest]{tuneRF}}. Only used when algorihm = "rf"}

\item{plotnames}{names to be printed in the diagrams}
}
\value{
A list of six components is returned for each species in \code{x}:

 \item{model }{fitted model using all data for training}
 \item{auc }{AUC statistic in the cross validation}
 \item{kappa }{kappa statistic in the cross validation}
 \item{tss }{true skill statistic in the cross validation }
 \item{fold.models }{fitted model with partitioned data}
 \item{ObsPred }{cross model prediction}
}
\description{
Species distribution modeling and k-fold cross validation 
for a set of presence/absence data per species, also considering different background 
extents (optional). Algorithms supported are "glm", "svm", "maxent", "mars", "rf", "cart.rpart" 
and "cart.tree"
}
\references{
Iturbide, M., Bedia, J., Herrera, S., del Hierro, O., Pinto, M., Gutierrez, J.M., 2015. 
A framework for species distribution modelling with improved pseudo-absence generation. Ecological 
Modelling. DOI:10.1016/j.ecolmodel.2015.05.018.
}
\seealso{
\code{\link[mopa]{mopaPredict}}, \code{\link[mopa]{pseudoAbsences}}, \code{\link[mopa]{backgroundGrid}}, 
\code{\link[mopa]{OCSVMprofiling}}, \code{\link[mopa]{backgroundRadius}}
}
\author{
M. Iturbide
}
\keyword{internal}
