###########################################################
#
#   spec
#
###########################################################


###########################################################
# .sig
###########################################################

#----------------------------------
# Sensors
#----------------------------------

# Silicon array         : 512 channels
# InGaAs array          : 256 channels
# Extended InGaAs array : 256 channels

#----------------------------------
# Config: modifiable by the user
#----------------------------------

## Default overlap controls

# Si     -> Swirl1 transition wavelength: 1000
# Swirl1 -> Swirl2 transition wavelength: 1910

## Detector Matching controls
# beginning wavelength: 990
# ending    wavelength: 1006

## Matching Factor Limits
# min: 0.90
# max: 1.10

#----------------------------------
# Units
#----------------------------------
# Temp  : Degres C
# Error : 0 (no error)
# Sensor integration time: milliseconds

# Longitude DDD.mm.mmmmC
# Latitude  DDmm.mmmmC
# GpsTime   HHmmSS.SSS

# D = degrees
# m = decimal minutes
# C = quadrant
# H = hours
# S = seconds
#----------------------------------

#' Reads sig files
#'
#' @param sig_files
#' @param types
#'
#' @return TODO
#' @export
read_spectra_sig  = function(sig_files, types = "leaf") {

    #------------------------------#
    # check files
    #------------------------------#
    exist   = file.exists(sig_files)
    files   = sig_files[exist]
    n_files = length(files)

    if(any(!exist))
        message("These files were not found:", sig_files[!exist], appendLF = TRUE)

    #------------------------------#
    # sig format details
    #------------------------------#

    assumed_header    = "/*** Spectra Vista SIG Data ***/"
    assumed_max_lines = 1100    # Should actualy be 1050
    assumed_min_band  = 340.5
    assumed_max_band  = 2522.8

    keywords_interest = c("name",
                          "instrument",
                          "integration",
                          "scan method",
                          "scan coadds",
                          "scan time",
                          "temp",
                          "battery",
                          "error",
                          "units",
                          "time",
                          "longitude",
                          "latitude",
                          "gpstime"
                          )

    data_colnames     = c("wavelength_nm", "reference_values",
                          "target_values", "reflectance_percent")

    #------------------------------#
    # create spectra object
    #------------------------------#

    spectra = .spectra(n_files)

    #------------------------------#
    # Create a vector of "type" if
    # less than length(files) values
    # were provided
    #------------------------------#

    type    = rep(types, length.out = n_files)

    #------------------------------#
    # functions
    #------------------------------#

    # trims trailing and leading white spaces in a string
    trim_string = function(x) {
        gsub("^\\s+|\\s+$", "", x)
    }

    #------------------------------#
    # parse each spectrum
    #------------------------------#

    for(i in seq_along(files)) {
        message("reading ", i, " out of ", n_files)

        raw  = readLines(files[i], n = assumed_max_lines)

        # Format test 1: Does the file have the expected header?
        if(raw[[1]] != assumed_header)
            stop(files[i], "missing header!")

        #------------------------------#
        # data
        #------------------------------#
        data_mark      = grep("data= ", raw)
        data           = do.call("rbind", strsplit(raw[(data_mark + 1) : length(raw)], "  "))
        colnames(data) = data_colnames
        mode(data)     = "numeric"

        #------------------------------#
        # meta data
        #------------------------------#
        split_lines = sapply(raw[1 : data_mark], function(x) strsplit(x, split = "= ") )
        match_lines = match(keywords_interest, sapply(split_lines, function(x) x[[1]]) )
        meta        = sapply(split_lines[match_lines], function(x) x[[2]] )


        #------------------------------#
        # construct
        #------------------------------#

        spectra[[i]] = .spectrum(
            target_object   = .spectrum_base(
                type               = type[i],
                file_name          = trim_string(meta[[1]]),
                instrument_model   = trim_string(meta[[2]]),
                sensor_integ_time  = strsplit(meta[[3]], ", ")[[1]][4:6],
                scan_method        = strsplit(meta[[4]], ", ")[[1]][2],
                sensor_scan_coadds = strsplit(meta[[5]], ", ")[[1]][4:6],
                scan_time          = strsplit(meta[[6]], ", ")[[1]][2],
                sensor_temperature = strsplit(meta[[7]], ", ")[[1]][4:6],
                battery_voltage    = strsplit(meta[[8]], ", ")[[1]][2],
                error_code         = strsplit(meta[[9]], ", ")[[1]][2],
                units              = strsplit(meta[[10]], ", ")[[1]][2],
                date_time          = strsplit(meta[[11]], ", ")[[1]][2],
                longitude          = trim_string(strsplit(meta[[12]], ", ")[[1]][2]),
                latitude           = trim_string(strsplit(meta[[13]], ", ")[[1]][2]),
                gps_scan_time      = strsplit(meta[[14]], ", ")[[1]][2],
                data               = data[ , c("wavelength_nm",
                                               "target_values",
                                               "reflectance_percent") ]
            )
            ,
            reference_object = .spectrum_base(
                type               = "reference",
                file_name          = trim_string(meta[[1]]),
                instrument_model   = trim_string(meta[[2]]),
                sensor_integ_time  = strsplit(meta[[3]], ", ")[[1]][1:3],
                scan_method        = strsplit(meta[[4]], ", ")[[1]][1],
                sensor_scan_coadds = strsplit(meta[[5]], ", ")[[1]][1:3],
                scan_time          = strsplit(meta[[6]], ", ")[[1]][1],
                sensor_temperature = strsplit(meta[[7]], ", ")[[1]][1:3],
                battery_voltage    = strsplit(meta[[8]], ", ")[[1]][1],
                error_code         = strsplit(meta[[9]], ", ")[[1]][1],
                units              = strsplit(meta[[10]], ", ")[[1]][1],
                date_time          = strsplit(meta[[11]], ", ")[[1]][1],
                longitude          = trim_string(strsplit(meta[[12]], ", ")[[1]][1]),
                latitude           = trim_string(strsplit(meta[[13]], ", ")[[1]][1]),
                gps_scan_time      = strsplit(meta[[14]], ", ")[[1]][1],
                data               = data[ , c("wavelength_nm", "reference_values") ]
            )
        )
    }
    return(spectra)
}








