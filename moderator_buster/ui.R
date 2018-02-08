library(shiny)


shinyUI(fluidPage(
    mainPanel(
      img(src="buster.png", width = 300, height = 150)
    ),
    tabsetPanel(
      tabPanel("Abtract Prediction",
               fluidRow(
                 column(4,
                        textAreaInput("text", "Abstract:", "WMAP precision data enables accurate testing of cosmological models. We find that the emerging standard model of cosmology, a flat Lambda-dominated universe seeded by nearly scale-invariant adiabatic Gaussian fluctuations, fits the WMAP data. With parameters fixed only by WMAP data, we can fit finer scale CMB measurements and measurements of large scle structure (galaxy surveys and the Lyman alpha forest). This simple model is also consistent with a host of other astronomical measurements. We then fit the model parameters to a combination of WMAP data with other finer scale CMB experiments (ACBAR and CBI), 2dFGRS measurements and Lyman alpha forest data to find the model's best fit cosmological parameters: h=0.71+0.04-0.03, Omega_b h^2=0.0224+-0.0009, Omega_m h^2=0.135+0.008-0.009, tau=0.17+-0.06, n_s(0.05/Mpc)=0.93+-0.03, and sigma_8=0.84+-0.04. WMAP's best determination of tau=0.17+-0.04 arises directly from the TE data and not from this model fit, but they are consistent. These parameters imply that the age of the universe is 13.7+-0.2 Gyr. The data favors but does not require a slowly varying spectral index. By combining WMAP data with other astronomical data sets, we constrain the geometry of the universe, Omega_tot = 1.02 +- 0.02, the equation of state of the dark energy w < -0.78 (95% confidence limit assuming w >= -1), and the energy density in stable neutrinos, Omega_nu h^2 < 0.0076 (95% confidence limit). For 3 degenerate neutrino species, this limit implies that their mass is less than 0.23 eV (95% confidence limit). The WMAP detection of early reionization rules out warm dark matter.", width = 400, height = 400),
                   actionButton("Submit","Submit")
                 ),
                 column(4,
                        plotOutput("plot1", width = 500, height = 450),
                        verbatimTextOutput("results_text")
                  ),
                 column(4,
                        h5("Current Out-of-Sample Accuracy"),
                        tableOutput("tab1"),
                        plotOutput("plot2")
                 )
               )
      )
    )
))