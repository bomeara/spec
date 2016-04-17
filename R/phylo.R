#TODO
#use Rphylopars::phylopars and then get result$anc_recon to reconstruct tips. Replace code below with that
#also note use of this for regression

#' Predict the spectral signature of a taxon
#'
#' @param taxon Taxon to estimate (character format)
#' @param phy Phylo object with all taxa (including ones with missing data)
#' @param spectra.df A data.frame with one row of spectrum observations per species
#'
#' @return A data.frame with the median, upper, and lower estimates for each value for the focal taxon
#' @export
PredictSignature <- function(taxon, phy, spectra.df) {
  phy <- ape::root(phy, taxon)
  spectra.tmp <- rbind(spectra.df, rep(NA, dim(spectra.df)[2]))
  rownames(spectra.tmp) <- c(rownames(spectra.df), taxon)
  pruned <- geiger::treedata(phy, spectra.tmp, sort=TRUE, warnings=FALSE)
  phy.pruned <- pruned$phy
  phy.pruned <- ape::drop.tip(phy.pruned, taxon)
  spectra.pruned <- pruned$data
  spectra.pruned <- spectra.pruned[-which(rownames(spectra.pruned)==taxon),]

  anc.recon <- phylocurve::phylocurve(y~x,tree = phy.pruned,data = SpectraToPhylocurveData(spectra.pruned))

  #TODO: Get the estimate and CI at the root, return this
}

#' Convert a data.frame of spectral values (one row per species) to a format suitable for phylocurve
#'
#' @param spectra.df A data.frame with one row of spectrum observations per species
#'
#' @return A data.frame with a column for species, wavelength, and measurement at that wavelength
#' @export
SpectraToPhylocurveData <- function(spectra.df) {
  xval <- as.numeric(colnames(spectra.df))
  result <- data.frame(species=sapply(rownames(spectra.df), rep, times=length(xval)), x=rep(xval, dim(spectra.df)[1], y=as.vector(t(spectra.df))))
  return(result)
}
