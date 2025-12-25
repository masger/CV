install.packages("nlmixr2",dependencies = TRUE)


install.packages(c("xpose.nlmixr2", # Additional goodness of fit plots
                   # baesd on xpose
                   "nlmixr2targets", # Simplify work with the
                   # `targets` package
                   "babelmixr2", # Convert/run from nlmixr2-based
                   # models to NONMEM, Monolix, and
                   # initialize models with PKNCA
                   "nonmem2rx", # Convert from NONMEM to
                   # rxode2/nlmixr2-based models
                   "nlmixr2lib", # a model library and model
                   # modification functions that
                   # complement model piping
                   "nlmixr2rpt" # Automated Microsoft Word and
                   # PowerPoint reporting for nlmixr2
),
repos = c('https://nlmixr2.r-universe.dev',
          'https://cloud.r-project.org'))

# Some additional packages outside of the `nlmixr2.r-univers.dev`
# install.packages("remotes")
remotes::install_github("ggPMXdevelopment/ggPMX") # Goodness of fit plots
remotes::install_github("RichardHooijmaijers/shinyMixR") # Shiny run manager (like Piranha)



library(nlmixr2)
## The basic model consists of an ini block that has initial estimates
one.compartment <- function() {
  ini({
    tka <- log(1.57); label("Ka")
    tcl <- log(2.72); label("Cl")
    tv <- log(31.5); label("V")
    eta.ka ~ 0.6
    eta.cl ~ 0.3
    eta.v ~ 0.1
    add.sd <- 0.7
  })
  # and a model block with the error specification and model specification
  model({
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl)
    v <- exp(tv + eta.v)
    d/dt(depot) <- -ka * depot
    d/dt(center) <- ka * depot - cl / v * center
    cp <- center / v
    cp ~ add(add.sd)
  })
}
## The fit is performed by the function nlmixr/nlmixr2 specifying the model, data and estimate
fit <- nlmixr2(one.compartment, theo_sd, est="foce")
fit$objDf$OBJF
