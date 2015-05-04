###########################################################
#
#   spec
#
###########################################################


###########################################################
# import packages
###########################################################

devtools::use_package("lubridate")


###########################################################
# spectrum base
###########################################################


#' Internal use only! Constructor for a basic spectrum measurement object
#'
#' @param type Type of spectrum measueremt. Can be "reference", "target: blank" or "target: leaf"
#' @param file_name
#' @param instrument_model
#' @param sensor_integ_time Integration time for each sensor in milliseconds
#' @param scan_method
#' @param sensor_scan_coadds Count of measurements per for each sensor
#' @param scan_time Total scan time in seconds
#' @param sensor_temperature Temperature of each sensor in degrees celcius
#' @param battery_voltage Voltage of the battery at measurement time in volts
#' @param error_code
#' @param units In what units the data are. Options are "Radiance", "Irradiance" and "Counts"
#' @param date_time
#' @param longitude Format DDD.mm.mmmmC
#' @param latitude Format DDmm.mmmmC
#' @param gps_scan_time Format HHmmSS.SSS
#' @param data
#'
#' @return Object of class ".spectrum_base"
.spectrum_base = function(type,
                          file_name,
                          instrument_model,
                          sensor_integ_time,
                          scan_method,
                          sensor_scan_coadds,
                          scan_time,
                          sensor_temperature,
                          battery_voltage,
                          error_code,
                          units,
                          date_time,
                          longitude,
                          latitude,
                          gps_scan_time,
                          data
                          ) {

    #------------------------#
    # Convenience typedefs
    #------------------------#

    ._char    = as.character
    ._double  = as.numeric
    ._int     = as.integer

    #------------------------#
    # Coord conversion HACK!
    #------------------------#

    # TODO!
    # These two methods should not even be here! The lat/long fields should have
    # a documented format, say decimal degrees, and the parser should take care
    # of converting from the input format.

    ._hack_long_do_decimal = function(DDD.mm.mmmmC, warn = F){
        if(warn)
            message("This coord conversion is an ugly ass hack! It WILL break!")

        deg   = ._double(substr(DDD.mm.mmmmC, 1, 3))
        dec_m = ._double(substr(DDD.mm.mmmmC, 4, 10)) / 60
        quad  = ifelse(substr(DDD.mm.mmmmC, 11, 11) == "W", -1, 1)

        quad * {deg + dec_m}
    }

    ._hack_lat_do_decimal = function(DDmm.mmmmC, warn = F){
        if(warn)
            message("This coord conversion is an ugly ass hack! It WILL break!")

        deg   = ._double(substr(DDmm.mmmmC, 1, 3))
        dec_m = ._double(substr(DDmm.mmmmC, 4, 9)) / 60
        quad  = ifelse(substr(DDmm.mmmmC, 10, 10) == "S", -1, 1)

        quad * {deg + dec_m}
    }

    #------------------------#
    # Real deal
    #------------------------#

    object = list(
        type                = ._char(type),
        file_name           = ._char(file_name),
        instrument_model    = ._char(instrument_model),
        sensor_integ_time   = ._double(sensor_integ_time),
        scan_method         = ._char(scan_method),
        sensor_scan_coadds  = ._int(sensor_scan_coadds),
        scan_time           = ._double(scan_time),
        sensor_temperature  = ._double(sensor_temperature),
        battery_voltage     = ._double(battery_voltage),
        error_code          = ._int(error_code),
        units               = ._char(units),
        date_time           = ._char(lubridate::mdy_hms(date_time)),       #MAYBE I SHOULDN'T BE HERE?
        longitude           = ._hack_long_do_decimal(._char(longitude)),   #FIXME!
        latitude            = ._hack_lat_do_decimal(._char(latitude)),     #FIXME!
        gps_scan_time       = ._double(gps_scan_time),
        data                = {mode(data)  = "numeric"; data}
    )
    class(object) = c(".spectrum_base", class(object))
    return(object)
}



###########################################################
# spectrum
###########################################################

#' Constructor for a spectrum object
#'
#' @param target_object "spectrum_base" object that represents a target measurement
#' @param reference_object "spectrum_base" object that represents a reference measurement
#'
#' @return Object of class "spectrum"
.spectrum = function(target_object, reference_object) {
    # if(! ".spectrum_base" %in% class(target_object))
      if(!any(".spectrum_base" == class(target_object)))
         stop("target_object must be of class '.spectrum_base!'")

    # if(! ".spectrum_base" %in% class(reference_object) )
      if(!any(".spectrum_base" == class(reference_object)))
          stop("reference_object must be of class '.spectrum_base!'")

    object = list("target" = target_object, "reference" = reference_object)
    class(object) = c("spectrum", class(object))
    return(object)
}



###########################################################
# spectrum "methods"
###########################################################

#' Plots the reflectance of a spectrum
#' To "zoom" in part of the spectrum, use xlim = c(min, max)
#'
#' @param this
#' @param type What type of plot, e.g. "l" or "p"? Passed to "plot.default(type = )".
#' If add = T, then the only types available are lines and points
#' @param add Plot onto an existing plot window? Bool.
#' @param ... Additional arguments passed to plot.default
#'
#' @export
plot.spectrum = function(this, type = "l", add = FALSE, ...) {
    if (add) {
        if (type == "l")
            fun = lines
        if (type == "p")
            fun = points
        else
            fun = lines       # defaults to lines
    } else
        fun = plot.default

    fun(x    = this$target$data[ , "wavelength_nm"],
        y    = this$target$data[ , "reflectance_percent"],
        type = type,
        xlab = "wavelength",
        ylab = "reflectance",
        ...
    )
}


#' range
#'
#' Without this generic the range.spectrum method doesn't work
#' @export
range = function(self, ...){
    UseMethod("range", self)
}


#' Get the range of spectrum data "wavelength", "reflectance" or "both"
#' type:
#'
#' @param this
#' @param type Choice of range to return: "wavelength", "reflectance" or "both".
#' @param data Choice of data to query: "target" or "reference"
#' @return TODO
#' @export
range.spectrum = function(this, type = "wavelength", data = "target") {
    if(data == "target"){
        y = "target"
    } else {
        y = "reference"
    }

    if (type == "wavelength")
        return(range(this[[y]]$data[, "wavelength_nm"]))
    if (type == "reflectance")
        return(range(this[[y]]$data[, "reflectance_percent"]))
    if (type == "both")
        return(list(
            "wavelength"  = range(this[[y]]$data[, "wavelength_nm"]),
            "reflectance" = range(this[[y]]$data[, "reflectance_percent"])
        ))
}


#' Print basic spectrum info
#'
#' @param self
#' @param full Plot the full object? Bool.
#'
#' @return nothing. Called for its side effect
#' @export
print.spectrum = function(self, full = FALSE) {
    if (full)
        print.default(self)
    else
        cat(
            c(  "spectrum object",
                "\n",
                "target: type ",
                self$target$type,
                " and reference type: ",
                self$reference$type,
                "\n",
                "file name = ",
                self$target$file_name,
                "\n",
                "data = ",
                nrow(self$target$data),
                " bands in the range [",
                min(self$target$data[,"wavelength_nm"]),
                ", ",
                max(self$target$data[, "wavelength_nm"]),
                "]"
            ), sep = ""
        )
}
