###########################################################
#
#   spec
#
###########################################################


###########################################################
# spectra
###########################################################


#' Internal use only. Constructor for spectra object
#'
#' @param n_spectrum How many spectrum will this spectra hold
#'
#' @return TODO
.spectra = function(n_spectrum = 0){
    object        = vector(mode = "list", n_spectrum)
    class(object) = "spectra"
    return(object)
}


#' Converts a spectra object into a list of data.frames
#'
#' Note that there is no analogous function to this conversion. That is, you
#' cannot convert a list of tables or a data frame back to a spectra object.
#'
#' * 1) The target data.table, with one row per target measurement.
#' * 2) The reference data.table, with the __unique__ reference measurements.
#' In this case, the data returned is either "radiance", "irradiance" or "counts"
#'
#' @param this TODO
#' @param ... TODO
#'
#' @export
convert_to_data_tables = function(this, policy_duplicate_bands = "ADD_SMALL_N", ...) {
    n               = length(this)
    meta_cols_dat   = c("type", "file_name", "instrument_model", "sensor_integ_time",
                        "scan_method", "sensor_scan_coadds", "scan_time", "sensor_temperature",
                        "battery_voltage", "error_code", "units", "date_time", "longitude",
                        "latitude", "gps_scan_time")

    meta_cols_mat   = names(unlist(this[[1]][["target"]][meta_cols_dat]))
    meta_cols_mat_t = c(meta_cols_mat, "reference_id")
    data_cols       = as.character(this[[1]][["target"]][["data"]][ , "wavelength_nm"])

    if(policy_duplicate_bands == "ADD_SMALL_N") {
        mat_dat_cols = as.numeric(data_cols)
        mat_dat_cols[duplicated(mat_dat_cols)] = mat_dat_cols[duplicated(mat_dat_cols)] + 0.001
        mat_dat_cols = as.character(mat_dat_cols)
    } else {
        stop("Data cannot have duplicated column names! Please set policy_duplicate_bands = 'ADD_SMALL_N'.")
    }

    mat_cols_mat    = c(meta_cols_mat, mat_dat_cols)
    mat_cols_mat_t  = c(meta_cols_mat_t, mat_dat_cols)


    target        = matrix(nrow     = n,
                           ncol     = length(mat_cols_mat_t),    # added column for ref id
                           dimnames = list(NULL, mat_cols_mat_t))

    reference     = matrix(nrow     = n,
                           ncol     = length(mat_cols_mat),
                           dimnames = list(NULL, mat_cols_mat))

    reflectance   = matrix(nrow     = n,
                           ncol     = length(mat_dat_cols),
                           dimnames = list(NULL, mat_dat_cols))


      for(i in 1:n) {
            target[i, meta_cols_mat]      = unlist(this[[i]][["target"]][meta_cols_dat], use.names = F)
            target[i, mat_dat_cols]       = this[[i]][["target"]][["data"]][ , "target_values"]
            reference[i, meta_cols_mat]   = unlist(this[[i]][["reference"]][meta_cols_dat], use.names = F)
            reference[i, mat_dat_cols]    = this[[i]][["reference"]][["data"]][ , "reference_values"]
            reflectance[i, mat_dat_cols]  = this[[i]][["target"]][["data"]][ , "reflectance_percent"]
      }


    ref_id                    = reference[ , "date_time"]
    unique_ref_id             = !duplicated(ref_id)
    target[ , "reference_id"] = ref_id
    reference                 = reference[unique_ref_id, ]

    target      = as.data.frame(target, stringsAsFactors = F)
    reference   = as.data.frame(reference, stringsAsFactors = F)
    reflectance = as.data.frame(reflectance, stringsAsFactors = F)

    for(i in 1:ncol(target))
        target[[i]] = type.convert(target[[i]], as.is = T)

    for(i in 1:ncol(reference))
        reference[[i]] = type.convert(reference[[i]], as.is = T)


    ls = list("target" = target, "reference" = reference, "reflectance" = reflectance)


    return(ls)
}



#' Plot spectra
#' For now it assumes that all spetra have the the same n bands
#' @export
plot.spectra = function(this,
                        xlab = "wavelength",
                        ylab = "reflectance",
                        xlim = NULL,
                        ylim = NULL,
                        ...) {
    if(is.null(xlim))
        xlim = range(
            unlist(lapply(this, function(x){range(x, type = "wavelength")}), recursive = F)
        )

    if(is.null(ylim))
        ylim = range(
            unlist(lapply(this, function(x){range(x, type = "reflectance")}), recursive = F)
        )

    plot(x = 0, y = 0, type = "n", xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, ...)

    for(i in seq_along(this)){
        plot(this[[i]], add = T, ...) #xlim = xlim, ylim = ylim,
    }
}

#' Print spectra
#' @export
print.spectra = function(this, ...){
    print.default(this)
}


#' Subset operator overload for spectra
#' TODO! HACK! Learn about this thing ASAP
#' @export
`[.spectra` = function(this, i) {
    this = unclass(this)
    this = this[i]
    class(this) = "spectra"
    return(this)
}
