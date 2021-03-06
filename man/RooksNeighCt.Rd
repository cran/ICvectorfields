% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RooksNeighCt.R
\name{RooksNeighCt}
\alias{RooksNeighCt}
\title{Define a subset of grid locations with non-overlapping rook neighborhoods}
\usage{
RooksNeighCt(vfdf)
}
\arguments{
\item{vfdf}{A data frame as returned by \code{\link{PatternDetect}}}
}
\value{
A data frame similar to vfdf except that it includes only grid
locations with speed estimates in all four adjacent grid locations in their
rook's neighborhood. An additional column called IndPatternCt is appended
which contains NA values for locations that are overlapping other locations
and ones for all non-overlapping locations that have speed estimates in all
four adjacent cells.
}
\description{
This function prunes the data frame returned by the
\code{\link{PatternDetect}} function such that it includes only rook's
neighborhoods that do not overlap. Pruning is done by sequential removal of
observations that are too near one another and as a result are overlapping.
Locations that are the most highly connected are removed first.
}
\details{
The reason for this function's existence is to facilitate probabilistic
calculations regarding whether certain patterns are occurring more or less
often that would be expected by chance. If rook's neighborhoods in which
patterns are observed overlap, then the assumption of probabilistic
independence is necessarily incorrect. Thus, overlap invalidates any
calculation of the probability of occurrence of a particular pattern if that
calculation assumes independence. The pruning actions of this function enable
the user to more safely assume probabilistic independence.
}
\examples{

# creating convergence/divergence patterns
Mat1 <- matrix(rep(0,9*9), nrow = 9)
Mat1[3, c(4, 6)] <- 1
Mat1[7, c(4, 6)] <- 1
Mat1[c(4, 6), 3] <- 1
Mat1[c(4, 6), 7] <- 1
Mat1

Mat2 <- matrix(rep(0,9*9), nrow = 9)
Mat2[2, c(4, 6)] <- 1
Mat2[8, c(4, 6)] <- 1
Mat2[c(4, 6), 2] <- 1
Mat2[c(4, 6), 8] <- 1
Mat2

Mat1 <- cbind(Mat1, Mat1)
Mat1 <- rbind(Mat1, Mat1)

Mat2 <- cbind(Mat2, Mat2)
Mat2 <- rbind(Mat2, Mat2)

# rasterizing
rast1 <- terra::rast(Mat1)
terra::plot(rast1)
rast2 <- terra::rast(Mat2)
terra::plot(rast2)

# Detecting a divergence
(VFdf1 <- DispField(rast1, rast2, factv1 = 3, facth1 = 3, restricted = TRUE))
(patdf1 <- PatternDetect(VFdf1))
(subdf1 <- RooksNeighCt(patdf1))
# The last call should print a data table with four rows, each with a one under
# the column header of IndPatternCt
}
