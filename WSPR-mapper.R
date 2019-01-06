#!/usr/local/bin/Rscript

## version 1.0.3  2017-09-19 Fixed incorrect highlighting when starting up in Unknown mode
## version 1.0.2  2017-07-27 Added new argument parser, modified timing to 15 second intervals to accommodate FT8 mode
## version 1.0.1, 2017-05-03
## version 1.0.0, 2017-05-01

#############################################################################
## JT-Mapper                                                               ##
##                                                                         ##
## When you run WSPR using WSJT-X, have you ever wondered where    ##
## in the world all those stations you are hearing are?                    ##
##                                                                         ##
## I certainly have, which is why I wrote a little R program to map them   ##
## all in real-time. That tool is called JT-Mapper, and if                 ##
## you're running WSJT-X on a Mac or Linux, I'd love for you to give it a  ##
## try and let me know what you think of it.                               ##
##                                                                         ##
## First a few caveats for those brave souls who want to give this a       ##
## shot:                                                                   ##
##                                                                         ##
## * You have to be running WSJT-X on a Mac or Linux machine. Windows      ##
##   machines lack an essential utility for the program to work, so        ##
##   I'm deferring Windows support.                                        ##
##                                                                         ##
## * You have to install the open source language R on your computer and   ##
##   download a bunch of libraries for it. This is no big deal, but I don't##
##   want people to think this is all just download and go. R language     ##
##   installers are available at r-project.org.                            ##
##                                                                         ##
## * You have to be comfortable typing at the command line to start up     ##
##   the program (or be comfortable sourcing it from R itself).            ##
##                                                                         ##
## For anyone wanting to see what this looks like, you can check out my    ##
## public repository on github. The README.md document there has screen    ##
## shots and many more details about the program. You can read about it    ##
## and download the program (cleverly named JT-Mapper.R) at:               ##
##                                                                         ##
## http://github.com/cdhowe/J-Mapper                                       ##
##                                                                         ##
## I have a full time job and travel a fair amount for work, so while I    ##
## am happy to answer the occasional question, please be patient -- it     ##
## can be days before I'm able to answer.                                  ##
##                                                                         ##
## Finally, I would like to thank Joe Taylor, whose WSJT-X program         ##
## inspired me to write this code. I write a lot of visualizations as      ##
## part of my big data work, and it was a treat to be able to apply this   ##
## to ham radio through his software. Thank you Joe.                       ##
##                                                                         ##
## Have fun, Carl WG1V FN42                                                ##
#############################################################################

## Usage: ./WSPR-mapper numberlines callsign location

suppressWarnings(suppressMessages(library(getopt)))
suppressWarnings(suppressMessages(library(readr)))
suppressWarnings(suppressMessages(library(stringr)))
# suppressWarnings(suppressMessages(library(sp)))
# suppressWarnings(suppressMessages(library(rgdal)))
suppressWarnings(suppressMessages(library(ggplot2)))
# suppressWarnings(suppressMessages(library(ggmap)))
suppressWarnings(suppressMessages(library(maptools)))
suppressWarnings(suppressMessages(library(lubridate)))
suppressWarnings(suppressMessages(library(rworldmap)))
suppressWarnings(suppressMessages(library(tidyr, warn.conflicts=FALSE)))
suppressWarnings(suppressMessages(library(dplyr)))

# These are the default values for the program. All of these can be overridden with
# command line arguments

loglocation <- "/Users/chowe/Library/Application\ Support/WSJT-X/ALL_WSPR.TXT"
mycall        <- "WG1V"                      # my station callsign
mygrid        <- "FN42fk"                    # my station location
wsjtxlines    <- "100"                    # how many recent log lines to plot
cqcolor       <- "green4"
heardcolor    <- "palegreen"
callingcolor  <- "red"
oldcolor      <- "gray90"
debugflag     <- FALSE

bands <- data.frame("MHz" =       c( "3",     "7",    "10",    "14", 
                                    "18",    "21",    "28",    "50"),
                    "band" =      c("80m",   "40m",   "30m",   "20m", 
                                    "17m",   "15m",   "10m",   "06m"),
                    "fillcolor" = c("#d53e4f", "#f46d43", "#fdae61", "#fee08b", 
                                    "#ffffbf", "#e6f598", "#66c2a5", "#3288bd"),
                    "textcolor" = c("#ffffff", "#111111", "#111111", "#111111",
                                    "#111111", "#111111", "#111111", "#ffffff"))

###################################################################################
###################################################################################
## The following are a collection of helper functions for dealing with the logs. ##
## Not all are necessarily used.                                                 ##
###################################################################################
###################################################################################

new_window <- function(width, height, dpi) {
    ## for Mac OS X
    quartz(width=width, height=height, dpi=dpi)
    ## for Linux
#    x11(width=width, height=height, dpi=dpi)
}

# returns the new offset of the terminator in the string starting at 
gettoken <- function(string, offset, terminator)
{
  newoffset <- str_locate(str_sub(string, offset, nchar(string)), terminator)
  if (is.na(newoffset[1]) | is.na(newoffset[2]))
    return(NA)
  if (as.numeric(newoffset[1]) <= as.numeric(newoffset[2]))
    return(as.numeric(newoffset[1]) + as.numeric(offset) - 1)
  else
    return(NA)
}

MakeDescription <- function(df) {
  len <- length(df[,1])
  cols <- length(df[1,])
  nms <- names(df)
  description <- NULL
  for (i in 1:len) {                    # for each row
    vals = df[i,1:cols]
    desc <- paste0('<tr><td>', nms[!is.na(vals) & vals != "NA%"], '</td><td>', vals[!is.na(vals) & vals != "NA%"], '</td></tr>', collapse=' ')
    desc2 <- paste0("<table>", desc, '</table>', collapse='')
    description <- c(description, desc2)
  }
  return(description)
}

debug <- function(x) {
  if (debugflag) {
       print(x)
  }
}

# Read in ADIF file
# This is really a pain because of all the fixed width junk

# returns a data frame with all the keys that appear as columns and
# values assigned (when they are included) as appropriate for each row

# The assumption here is that the file is small enough to fit in memory

# Sadly, it's a two-pass algorithm: first pass to determine all the column
# names, second one to assign the values to the columns

adif_to_df <- function(bigstring) {
    rows <- grep("<call", bigstring, value=TRUE) # just rows with calls
    uniquecolnames <- NULL
    for (line in rows) {
       colstrings <- unlist(str_split(line, "<"))
       cols = str_replace_all(colstrings, ":.*$", "")
       cols = cols[-1]
       cols = cols[-length(cols)]
       uniquecolnames <- unique(c(uniquecolnames, cols))
    }

    frame <- data.frame(matrix(nrow=1, ncol=length(uniquecolnames)))
    names(frame) <- uniquecolnames
    
    for (rownum in 1:length(rows)) {
        offset <- 1
        line <- rows[rownum]
        while(offset < nchar(line)) {
          offset <- gettoken(line, offset, "<")
          # now accumulate the column name          
          if (is.na(offset)) break
          endoffset <- gettoken(line, offset+1, ":")              
          colname <- str_sub(line, offset+1, endoffset-1)
          offset <- endoffset + 1
          fieldlenoffset <- gettoken(line, offset, ">")
          if (is.na(fieldlenoffset)) break
          fieldlen <- as.numeric(str_sub(line, offset, fieldlenoffset-1))
          debug(sprintf("offset=%g, fieldlenoffset=%g, fieldlen=%s",
                        offset, fieldlenoffset, fieldlen))
          value <- str_sub(line, fieldlenoffset+1, fieldlenoffset+fieldlen)
          offset <- fieldlenoffset + fieldlen + 1
          frame[rownum, colname] <- value
          debug(sprintf("frame[%d, %s] <- %s", rownum, colname, value))
        }
    }
    return(frame)
}

charToInt <- function(x) {
  return (as.numeric(charToRaw(x)))
}

# Convert grid square to latitude and longitude
grid_to_latlon <- function(grid)
{
  grid <- as.character(grid)
  if (is.na(grid) | nchar(grid) < 4) {
       return(data.frame("lat" = NA, "lon" = NA))
  }
  grid <- toupper(grid)
  if (grepl("^[S-Z]", grid) | grepl("^\\w[S-Z]", grid)) {
       return(data.frame("lat" = NA, "lon" = NA))
  }
  lon <- (charToInt(substr(grid, 1, 1)) - charToInt('A')) * 20 - 180
  lat <- (charToInt(substr(grid, 2, 2)) - charToInt('A')) * 10 - 90
  lon <- lon + (charToInt(substr(grid, 3, 3)) - charToInt('0')) * 2
  lat <- lat + (charToInt(substr(grid, 4, 4)) - charToInt('0')) * 1
  
  if (nchar(grid) >= 5) {
    ## We have subsquares
    if (nchar(grid) > 6) {     # I don't care about sub sub squares
      grid <- substr(grid, 1, 6)     
    }
    if (grepl("^[Y-Z]", grid)) {
       return(data.frame("lat" = NA, "lon" = NA))
    }
    lon <- lon + (charToInt(substr(grid, 5, 5)) - charToInt('A')) * 5/60
    lat <- lat + (charToInt(substr(grid, 6, 6)) - charToInt('A')) * 2.5/60
    ## Move to center of subsquare
    lon <- lon + 2.5/60
    lat <- lat + 1.25/60
    formatter = "%.5f"
  } else {
    lon = lon + 1
    lat = lat + 0.5
    formatter = "%.1f"
  }
    df <- data.frame("lat" = lat, "lon" = lon)
    return(df)
}


#################################################################################################
## Loglines_to_df parses the ALL.TXT log put out by WSJT-X for grid locators and call signs ## ##
## loglines is a vector of log lines,                                                          ##
## return value log.df is a dataframe of calls, times, grids, and labelcolors                  ##
#################################################################################################

loglines_to_df <- function(lines) {
    lines <- str_trim(lines)
    loglines <- grep("Transmitting", lines, invert=TRUE, value=TRUE) # Transmitting lines are different formate
    loglines_df <- data.frame(loglines)
    
    columns <- c("date", "time", "unk", "dB", "drift", "MHz", "fracMHz", "call", "grid", "pwr")
    parsestring <- "([0-9]+)\\s+([0-9]+)\\s+([\\.\\+\\-0-9]+)\\s+([\\.\\+\\-0-9]+)\\s+([\\.\\+\\-0-9]+)\\s+([0-9]+)\\.([0-9]+)\\s+([/<>\\.[[:alnum:]]]+)\\s+([/[[:alnum:]]]+)\\s+([0-9]+)"

    parsed_df <- loglines_df %>% extract(loglines, columns, parsestring)
    df <- merge(parsed_df, bands, by="MHz")
#    df$band <- factor(df$band, levels=bands$band)
    return(df)
}

##############################
## Main Program starts here ##
##############################


# Parse the arguments given on the command line using the getopt package
argument_spec <- matrix(c(
    'help',        'h', 0, "logical",
    'debug',       'd', 0, "logical",
    'mycall',      'c', 1, "character",
    'mygrid',      'g', 1, "character",
    'logfile',     'l', 1, "character",
    'numlines',    'n', 1, "integer",
    'cqcolor',     'q', 1, "character",
    'heardcolor',  'r', 1, "character",
    'callingcolor','a', 1, "character",
    'oldcolor',    'o', 1, "character"
), byrow=TRUE, ncol=4)

opt <- getopt(argument_spec)

# process the help command
if ( !is.null(opt$help) ) {
  cat(getopt(argument_spec, usage=TRUE));
  q(status=1);
}

# Now we reset our parameters for arguments given
if (!is.null(opt$mycall)       ) { mycall       = opt$mycall       }
if (!is.null(opt$mygrid)       ) { mygrid       = opt$mygrid       }
if (!is.null(opt$logfile)      ) { loglocation  = opt$logfile      }
if (!is.null(opt$numlines)     ) { wsjtxlines   = opt$numlines     }
if (!is.null(opt$cqcolor)      ) { cqcolor      = opt$cqcolor      }
if (!is.null(opt$heardcolor)   ) { heardcolor   = opt$heardcolor   }
if (!is.null(opt$callingcolor) ) { callingcolor = opt$callingcolor }
if (!is.null(opt$oldcolor)     ) { oldcolor     = opt$oldcolor     }
if (!is.null(opt$debug   )     ) { debugflag        = TRUE             }

call_grid_df <- data.frame("grid"=mygrid, "call"=mycall) #initialize


theme_set(theme_bw())
new_window(width=11, height=11, dpi=82) ## create window to plot in

mystation.df <- data.frame("call" = mycall, "grid" = mygrid, stringsAsFactors=FALSE)
xy <- grid_to_latlon(mygrid)
mystation.df$lat <- xy$lat
mystation.df$lon <- xy$lon

map.world <- map_data("world")
wm <- ggplot(map.world, aes(x = long, y = lat, group = group))
wm <- wm + geom_polygon(fill = "wheat", color="gray70", size=0.3) # fill areas
wm <- wm + theme(axis.line=element_blank(),axis.text.x=element_blank(),
                 panel.background = element_rect(fill="lightblue"),
                 panel.grid.major = element_line(color="gray40", size=0.3, linetype="dotted"),
                 panel.grid.minor = element_line(color="gray50", size=0.2, linetype="dotted"),
                 axis.text.y=element_blank(),axis.ticks=element_blank(),
                 axis.title.x=element_blank(),
                 axis.title.y=element_blank(),legend.position="none")
wm <- wm + geom_point(data=mystation.df, 
                      aes(x=lon, y=lat), 
                      shape=23, color="white", fill="gold4",
                      group=NA, size=3) 


map.states <- map_data("state")
sm <- ggplot(map.states, aes(x = long, y = lat, group = group))
sm <- sm + geom_polygon(fill = "wheat", color="gray70", size=0.3) # fill areas
sm <- sm + theme(axis.line=element_blank(),axis.text.x=element_blank(),
                 panel.background = element_rect(fill="lightblue"),
                 panel.grid.major = element_line(color="gray40", size=0.3, linetype="dotted"),
                 panel.grid.minor = element_line(color="gray50", size=0.2, linetype="dotted"),
                 axis.text.y=element_blank(),axis.ticks=element_blank(),
                 axis.title.x=element_blank(),
                 axis.title.y=element_blank(),legend.position="none")
sm <- sm + geom_point(data=mystation.df, 
                      aes(x=lon, y=lat), 
                      shape=23, color="white", fill="gold4",
                      group=NA, size=3)  

oldlog = ""
clockseconds <- c(57)       # second times we should wake up and refresh if needed
mode = "WSPR"

while (1) {
  timeofnow       <- now(tzone="GMT")
  datetimenow     <- parse_date_time(timeofnow, "%y-%m-%d %H:%M:%S", tz="UTC")
  ## Read the last N lines of the full log, usually ALL.TXT
  logread <- system(paste0('tail -', wsjtxlines, ' "', loglocation, '"'), 
                    intern=TRUE)
  logread <- logread[logread != ""]  # eliminate null strings read
  ## Has the last entry for the log changed since last time we read it?
    debug(paste0(timeofnow, "----- LOG READ ------"))
    debug(sprintf("noldlog last entry = %s", oldlog[length(oldlog)]))
    debug(sprintf("logread last entry = %s", logread[length(logread)]))
    if (is.na(logread[length(logread)]) | logread[length(logread)] == "") {
      debug("Read null log entry")
      print("Logread dump: ***************")
      print(logread)
      break();
    }
    if (oldlog[length(oldlog)] == logread[length(logread)]) {
      currentseconds <- floor(second(datetimenow))  # if not, wait until our next target time
      targetclockseconds <- clockseconds
      timediff       <- targetclockseconds - currentseconds
      timesremaining <- timediff[timediff > 0]
      debug(sprintf("Times remaining = %d", timesremaining))
      sleeptime <- ifelse(length(timesremaining) > 0, min(timesremaining), 15)
      if (is.na(sleeptime) | is.infinite(sleeptime) | sleeptime == 0) { sleeptime <- 1 }
      debug(sprintf("Current seconds = %d, Sleeping for %d", currentseconds, sleeptime))
      Sys.sleep(sleeptime) 
      next
    }

################################################################################################
## We have something in the log to process. Now we need to see if we have band and           ##
## mode information. We're going to search our log backwards for a mode change.              ##
##                                                                                           ##
## We have a couple of challenges to deal with though. We don't get any indication in the    ##
## log when the user QSYs to 6 meters, and there may be no mode change within the last       ##
## N lines we read. So if we don't find a mode change we'll have to assume mode and frequency ##
## until we do find one. That means we get incorrect results sometims.                        ##
################################################################################################

  log <- str_trim(logread)              # Get rid of trailing spaces

  ##########################################################################################
  ## Process the log to generate a data frame of all the contacts we want to plot         ##
  ## Locations is a character vector containing all the lines with an ending grid locator ##
  ## Resulting log dataframe is named log.df                                              ##
  ##########################################################################################

  if (length(log) > 0) {     
      log.df <- loglines_to_df(log)     # convert to dataframe
      numstations <- length(unique(log.df$call))
      if (nrow(log.df) > 0) {
           xy <- sapply(log.df$grid, grid_to_latlon)
           log.df$lat <- as.numeric(xy[1,])
           log.df$lon <- as.numeric(xy[2,])

           maxage          <- 1                  # max age in hours to be displayed
           log.df$datetime <- parse_date_time(paste(log.df$date, log.df$time, sep=" "), "%y%m%d %H%M", tz="UTC")

           debug(sprintf("Current datetime: %s", datetimenow))
#           debug(sprintf("DateTime parsing: %s", paste(log.df$date, log.df$time, sep=" ")))

           log.df$age      <-  timeofnow - log.df$datetime
           log.df$age      <-  abs(log.df$age)
           
           ratio = 2.6      # longitude should be 3x latitude in cartesian
           minlat <- min(log.df$lat, na.rm=TRUE)
           minlon <- min(log.df$lon, na.rm=TRUE)
           maxlat <- max(log.df$lat, na.rm=TRUE)
           maxlon <- max(log.df$lon, na.rm=TRUE)
#          debug(sprintf("orig bounding box =[(%.1f, %.1f), (%.1f, %.1f)]",
#                        minlon, minlat,maxlon, maxlat))

           debug("**************LOG.DF*************")
           debug(log.df)
           debug("**************END****************")

           lowlat  <- max(minlat - 1, -90)
           highlat <- min(maxlat + 1, 85)
           lowlon  <- max(minlon - 1, -180)
           highlon <- min(maxlon + 1, 180)
#           debug(sprintf("low lat, lowlon, highlat, highlon = [(%.1f, %.1f), (%.1f, %.1f)]",
#                         lowlat, lowlon, highlat, highlon))
           
           lonrange0 <- highlon - lowlon
           latrange0 <- highlat - lowlat
           lonrange <- lonrange0
           latrange <- latrange0
           
           ## Here we are trying to correct the aspect ratio of the map
           minmapdegrees <- 5
           minlonrange <- minmapdegrees * ratio
           minlatrange <- minmapdegrees
           debug(sprintf("lonrange=%s, minlonrange=%s", lonrange, minlonrange))
           if (lonrange < minlonrange) {
               lowlon <- max(minlon - (minlonrange - lonrange)/2, -180)
               highlon <- min(maxlon + (minlonrange + lonrange)/2, 180)
               lonrange <- minlonrange
           }
           if (latrange < minlatrange) {
               lowlat <- max(minlat - (minlatrange - latrange)/2, -90)
               highlat <- min(maxlat + (minlatrange + latrange)/2, 85)
               latrange <- minlatrange
               }

#           debug(sprintf("plot bounding box [%.1f, %.1f], [%.1f, %.1f]", lowlon, lowlat, highlon, highlat))
#           debug(sprintf("lonrange0=%.1f, latrange0=%.1f, lonrange=%.1f, latrange=%.1f",
#                         lonrange0, latrange0, lonrange, latrange))

           ## We really have two cases here:
           ## When we adjust for our desired aspect ratio, we want our latitude and longitude
           ## ranges to be equal.
           ## if latitude range >> longitude range, we need to adjust the longitude values
           ## if the latitude range << longitude rante, we must adjust the latitude ranges
           ## in both cases, we have to make sure we don't go overboard.
           
           ## Aspect ratio correction
           if (latrange >= lonrange / ratio) {          # must make y axis larger
               debug("targetlatrange >= targetlonrange")
               mapminlat <- lowlat
               mapmaxlat <- highlat
               mapminlon <- max(lowlon - (latrange * ratio - lonrange)/2, -180)
               mapmaxlon <- min(highlon + (latrange * ratio - lonrange)/2, 180)
           } else {                     # in this case, we have to expand x axis
               debug("targetlatrange < targetlonrange")
               mapminlat <- max(lowlat - (lonrange / ratio - latrange)/2, -90)
               mapmaxlat <- min(highlat + (lonrange / ratio - latrange)/2, 85)
               mapminlon <- lowlon
               mapmaxlon <- highlon
            }

           maplonrange <- mapmaxlon - mapminlon
           maplatrange <- mapmaxlat - mapminlat
#           debug(sprintf("map bounding box [%.1f, %.1f], [%.1f, %.1f]", mapminlon, mapminlat, mapmaxlon, mapmaxlat))
#           debug(sprintf("transform from [(%.1f, %.1f), (%.1f, %.1f)] TO [(%.1f, %.1f), (%.1f, %.1f)]",
#                         lowlat, lowlon, highlat, highlon,
#                         mapminlat, mapminlon, mapmaxlat, mapmaxlon))

           ## We're now going to create a new data frame that 
           ## only contains contacts in the latest reception period.
           ## The reception period is last 15 seconds for FT8
           ## 60 seconds for all else.
           ## We'll only display the call signs for the latest signals
           
           ##           latest_time <- max(log.df$datetime)
           timestring        <- as.character(timeofnow)
           latest_time       <- datetimenow      # use Date object for math
           debug(sprintf("Latest time = %s", timestring))
           
           log.df$numeric_age <- as.double(log.df$age)
           log.df <- log.df %>% filter(numeric_age < 60)
           log.df <- log.df %>% arrange(-age)

           ## First, let's get rid of duplicates by only keeping the most recent one
           
           minagelog.df <- log.df %>% group_by(call, band) %>% summarize(minage = min(age))
           log.df <- log.df %>% inner_join(minagelog.df, by = c("call" = "call", "band" = "band", "age" = "minage"))
                      
           minages <- log.df %>% group_by(band) %>% summarize(minage = min(numeric_age))
           log.df <- log.df %>% left_join(minages, by="band") #still doesn't include all bands
           log.df$bandtext <- ifelse(is.na(log.df$minage), 
                                     as.character(log.df$band), 
                                     sprintf("%s: %3.0f minutes old", as.character(log.df$band), log.df$minage))
           log.df$bandtext <- factor(log.df$bandtext)
           
           ## Decide if we should use a world or state map
           p <- wm                      # plot defaults to world map
           if (!is.na(lowlat) & !is.na(highlat) &
               !is.na(lowlon) & !is.na(highlon)) {
             if (lowlat > 30 & highlat < 45 & 
                 lowlon > -125 & highlon < -63) {
               p <- sm          # can use the states map for the US
             }
           }
  
           log.df$band <- factor(log.df$band, levels=bands$band)         
           ## Compute how many seconds to subtract to derive the
           ## beginning of the reception period.
           ## OK, this is our bounding box
           p <- p + geom_point(data=log.df, 
                                 aes(x=lon, y=lat, color=fillcolor),
                                 group=NA, size=2) 

           ###################################
           ## OK, it's time to plot the map ##
           ###################################
           p <- p + geom_label(data=log.df, 
                                aes(x=lon, y=lat+maplatrange*.1,
                                    label=paste0(call, "\n", round(age), " min ", pwr, " ", dB, "dB"),
                                    fill=fillcolor, color=textcolor,
                                    alpha=-numeric_age),
                               group=NA, size=2.7)
#           p <- p + scale_color_gradient(low = "darkgreen", high="skyblue")
                                        # mp <- mp + coord_map("ortho")
                                        # mp <- mp + facet_wrap(~ band)
           p <- p + scale_color_identity()
           p <- p + scale_fill_identity()
                                        # mp <- mp + coord_map("ortho")
                                        # mp <- mp + facet_wrap(~ band)
           p <- p + scale_alpha_continuous(range = c(0.1, 0.9))
           p <- p + labs(title = paste(numstations, "WSPR Stations Located From ", mycall, "Since", min(log.df$datetime), sep=" "))
           p <- p + scale_y_continuous(NULL, breaks = (-2:2) * 30, labels = NULL)
           p <- p + scale_x_continuous(NULL, breaks = (-4:4) * 45, labels = NULL)
           p <- p + coord_cartesian(xlim = c(mapminlon, mapmaxlon), ylim=c(mapminlat, mapmaxlat))
#           we would need to limit the polygons for the following to work           
#           p <- p + coord_map("mercator", xlim = c(mapminlon, mapmaxlon), ylim=c(mapminlat, mapmaxlat))
           p <- p + facet_wrap(~ band, ncol=2, nrow=4, drop=FALSE)
           print(p)
       }
   }
  oldlog <- logread
}
