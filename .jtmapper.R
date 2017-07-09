# Parameters for JT-Mapper
mycall       <- "WG1V"    # *Your* station callsign
mygrid       <- "FN42fk"  # *Your* station location


# How many recent log lines to plot
wsjtxlines   <- "50"

# WSJT-X log file.
# Attempt to be OS independent.  
# NOTE: I am guessing at what the Linux path is.

# Get the operating system.
os = Sys.info()[['sysname']]

if (os == "Darwin") {
   wsjtpath = { file.path(path.expand("~"),"Library/Application Support/WSJT-X") }
} else if (os == "Windows") { 
   # deal with backslashes in environment variables.  Not sure how to map system
   # drive, so this will fail for people who install on other drives.
   wsjtpath = { 
    file.path(
      chartr("\\","/",
        file.path(# Sys.getenv("HOMEDRIVE"),
		  Sys.getenv("HOMEPATH"),"AppData/Local/WSJT-X")
      )
    ) 
   }
   # wsjtpath = { file.path(chartr("\\","/",path.expand("~")),"AppData/Local/WSJT-X") }

   # On my virtual machine, HOME is set to \\Mac\Home\Documents; however, on a true 
} else  { # linux, guessing.
   wsjtpath = { file.path(path.expand("~"),"wsjt-x") } 
}
loglocation = file.path(wsjtpath,"ALL.TXT")


# Colors
cqcolor      <- "green4"
heardcolor   <- "palegreen"
callingcolor <- "red"
oldcolor     <- "gray90"
