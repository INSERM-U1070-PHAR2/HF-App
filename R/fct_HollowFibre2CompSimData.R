#' HollowFibre2CompSimData
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

HollowFibre2CompSimData  <- function(model,
                                     halfLifeHoursA = 10,
                                     halfLifeHoursB = 16,
                                     Vcentral = 200,
                                     Vcartridge = 50,
                                     initialConcentrationA = 2,
                                     initialConcentrationB = 10,
                                     lastTimePointHours = 24,
                                     VinjectBolusA =  NA,
                                     VinjectBolusB = NA,
                                     VinjectBolusExtraB = NA,
                                     dosingIntervalHoursBolusA = NA,
                                     dosingIntervalHoursBolusB = NA,
                                     numberOfDosesBolusA = NA,
                                     numberOfDosesBolusB = NA,
                                     drugNameA = NA,
                                     drugNameB = NA,
                                     admType = "Bolus",
                                     tinfuseHoursA = NA,
                                     tinfuseHoursB = NA,
                                     VinjectInfA =  NA,
                                     VinjectInfB = NA,
                                     VinjectInfExtraB = NA,
                                     dosingIntervalHoursInfA = NA,
                                     dosingIntervalHoursInfB = NA,
                                     numberOfDosesInfA = NA,
                                     numberOfDosesInfB = NA,
                                     constantVolume = T,
                                     debitCentralCartridge)
{
  library(mrgsolve)
  library(dplyr)
  halfLifeAmin <- halfLifeHoursA * 60 #hours * minutes/hours
  halfLifeBmin <- halfLifeHoursB * 60 #hours * minutes/hours

  clearanceA <-
    log(2) * (Vcentral + Vcartridge) / halfLifeAmin #ml/min
  clearanceB <-
    log(2) * (Vcentral + Vcartridge) / halfLifeBmin #ml/min
  Vextra <- ifelse(clearanceA-clearanceB != 0, ((clearanceA - clearanceB) / clearanceB) * (Vcentral + Vcartridge) ,1)
  #mL protection for same halflife

  debit_pompe_extra_central <- clearanceA - clearanceB #ml/min
  debit_pompe_central_waste <- clearanceA #ml/min

  #Declare last time point to be simulated
  lastTimePointMin <-
    lastTimePointHours * 60 #in minutes = durée de la manip

  parameterValues <-
    c(
      Q1 = debit_pompe_extra_central   ,
      V1 = Vextra ,
      Q2 = debit_pompe_central_waste ,
      V2 = Vcentral,
      Q3 = debitCentralCartridge,
      V3 = Vcartridge
    )



  if (admType == "Bolus")
  {
    dosingIntervalMin_ABolus <- dosingIntervalHoursBolusA * 60
    dosingIntervalMin_BBolus <- dosingIntervalHoursBolusB * 60
    doseA_bolus_central <-
      initialConcentrationA * (Vcentral + Vcartridge) #µg
    doseB_bolus_central <-
      initialConcentrationB * (Vcentral + Vcartridge) #µg
    doseB_bolus_extra <-
      doseB_bolus_central * ((clearanceA - clearanceB) / clearanceB) #µg

    #Dose events for each compartments
    e1_bolus <-
      ev(
        amt = doseA_bolus_central,
        cmt = 1,
        ii = dosingIntervalMin_ABolus,
        addl = numberOfDosesBolusA - 1
      )
    e2_bolus <-
      ev(
        amt = doseB_bolus_extra,
        cmt = 3,
        ii = dosingIntervalMin_BBolus,
        addl = numberOfDosesBolusB - 1
      )
    e3_bolus <-
      ev(
        amt = doseB_bolus_central,
        cmt = 4,
        ii = dosingIntervalMin_BBolus,
        addl = numberOfDosesBolusB - 1
      )
    dose <- e1_bolus + e2_bolus + e3_bolus
    dose <- realize_addl(dose)
    if (constantVolume == FALSE)
    {
      Vadd_bolus_central_A <-
        ev(
          amt = VinjectBolusA,
          cmt = 6,
          ii = dosingIntervalMin_ABolus,
          addl = numberOfDosesBolusA - 1
        )
      Vadd_bolus_central_B <-
        ev(
          amt = VinjectBolusB,
          cmt = 7,
          ii = dosingIntervalMin_BBolus,
          addl = numberOfDosesBolusB - 1
        )
      Vadd_bolus_extra_B <-
        ev(
          amt = VinjectBolusExtraB,
          cmt = 8,
          ii = dosingIntervalMin_BBolus,
          addl = numberOfDosesBolusB - 1
        )
      dose <-
        e1_bolus + e2_bolus + e3_bolus + Vadd_bolus_central_A + Vadd_bolus_central_B +
        Vadd_bolus_extra_B
      dose <- realize_addl(dose)
    }



    simulated_data <- model %>%
      ev(dose) %>%
      param(parameterValues) %>%
      mrgsim(end = lastTimePointMin) %>%
      as_tibble

    #Red is drug A

  }
  if (admType == "Infusion")
  {
    tinfuseMin_A <- tinfuseHoursA * 60
    tinfuseMin_B <- tinfuseHoursB * 60

    dosingIntervalMin_AInf <-
      dosingIntervalHoursInfA * 60
    dosingIntervalMin_BInf <-
      dosingIntervalHoursInfB * 60

    # IV infusion
    keHours_A <- log(2) / halfLifeHoursA
    keHours_B <- log(2) / halfLifeHoursB
    doseA_infuse_central <-
      (
        initialConcentrationA * (Vcentral + Vcartridge) * keHours_A * tinfuseHoursA
      ) / (1 - exp(-keHours_A *
                     tinfuseHoursA))#µg
    doseB_infuse_central <-
      (
        initialConcentrationB * (Vcentral + Vcartridge) * keHours_B * tinfuseHoursB
      ) / (1 - exp(-keHours_B *
                     tinfuseHoursB))#µg
    doseB_infuse_extra <-
      doseB_infuse_central * ((clearanceA - clearanceB) / clearanceB) #µg
    #Dose events for each compartments
    e1_infuse <-
      ev(
        amt = doseA_infuse_central,
        rate = doseA_infuse_central / tinfuseMin_A,
        cmt = 1,
        ii = dosingIntervalMin_AInf,
        addl = numberOfDosesInfA - 1
      )
    e2_infuse <-
      ev(
        amt = doseB_infuse_extra,
        rate = doseB_infuse_extra / tinfuseMin_B,
        cmt = 3,
        ii = dosingIntervalMin_BInf,
        addl = numberOfDosesInfB - 1
      )
    e3_infuse <-
      ev(
        amt = doseB_infuse_central,
        rate = doseB_infuse_central / tinfuseMin_B,
        cmt = 4,
        ii = dosingIntervalMin_BInf,
        addl = numberOfDosesInfB - 1
      )
    dose <- e1_infuse + e2_infuse + e3_infuse
    dose <- realize_addl(dose)
    if (constantVolume == FALSE)
    {
      Vadd_infusion_A <-
        ev(
          amt = VinjectInfA,
          rate = VinjectInfA / tinfuseMin_A,
          cmt = 6,
          ii = dosingIntervalMin_AInf,
          addl = numberOfDosesInfA - 1
        )
      Vadd_infusion_B_central <-
        ev(
          amt = VinjectInfB,
          rate = Vinject_B_central / tinfuseMin_B,
          cmt = 7,
          ii = dosingIntervalMin_BInf,
          addl = numberOfDosesInfB - 1
        )
      Vadd_infusion_B_extra <-
        ev(
          amt = VinjectInfExtraB,
          rate = Vinject_B_extra / tinfuseMin_B,
          cmt = 8,
          ii = dosingIntervalMin_BInf,
          addl = numberOfDosesInfB - 1
        )

      dose <-
        e1_infuse + e2_infuse + e3_infuse + Vadd_infusion_A + Vadd_infusion_B_extra + Vadd_infusion_B_central
      dose <- realize_addl(dose)
    }


    simulated_data <- model %>%
      ev(dose) %>%
      param(parameterValues) %>%
      mrgsim(end = lastTimePointMin) %>%
      as_tibble


  }

  return(simulated_data)
}