# Parameters for JT-Mapper
mycall       <- "WG1V"    # *Your* station callsign
mygrid       <- "FN42fk"  # *Your* station location

# WSJT-X log file.
loglocation  <- file.path(Sys.getenv("HOME"),"/Library/Application Support/WSJT-X/ALL.TXT")
# How many recent log lines to plot
wsjtxlines   <- "50"

# Colors
cqcolor      <- "green4"
heardcolor   <- "palegreen"
callingcolor <- "red"
oldcolor     <- "gray90"
