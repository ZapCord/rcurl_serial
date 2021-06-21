# rcurl_serial
This is a pre-release version of grabbing serial force data from an Exsurgo device.

For running existing .csv, all you'll need in the csv is a "Time" and "Force" column. You can run this R app with the shiny library. Assuming Shiny is installed with all required packages: use:
```
library(shiny)
runGitHub("rcurl_serial","ZapCord",ref="v0.3.0")
```
Alternatively, download the released version and use:
```
library(shiny)
runApp('rcurl_serial-0.3.0')
```
Required R packages: shiny, shinyjs, lubridate, serial, stringr, dplyr, DT, ggplot2

To install required packages:
```
install.packages(c("shiny", "shinyjs", "lubridate", "serial", "stringr", "dplyr", "DT", "ggplot2"))
```
