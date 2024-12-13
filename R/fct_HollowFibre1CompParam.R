HollowFibre1CompParam <- function(halfLifeHours=7.22,
                                  Vcentral=200,
                                  Vcartridge=50,
                                  initial_concentration=2,
                                  lastTimePointHours=24,
                                  drugName="Drug",
                                  VinjectBolus =  10,
                                  dosingIntervalHoursBolus = 12,
                                  numberOfDosesBolus = 2,
                                  tinfuseHours = 1,
                                  VinjectInf =  10,
                                  dosingIntervalHoursInf = 12,
                                  numberOfDosesInf = 2,
                                  adm.type="Bolus",
                                  debit_central_cartridge=120,
                                  Css = 1,
                                  Cinfusemaintenance = 1000,
                                  VinjectLoadingDose = 10)
{
  library(plyr)
  library(dplyr)
        halfLifeMin <-   halfLifeHours * 60
        lastTimePointMin <- lastTimePointHours * 60
        keMin <- log(2) / halfLifeMin

        clearance <-
                log(2) * (Vcentral + Vcartridge) / halfLifeMin #ml/min

        debit_pompe_central_waste <- clearance #ml/min
        debit_pompe_dil_central <- clearance #ml/min


        volume_waste_produced <-
                debit_pompe_central_waste * lastTimePointMin #mL
        volume_diluant_to_central <-
                debit_pompe_dil_central * lastTimePointMin #mL
        # Declare parameter values
        if (adm.type == "Bolus")
        {
                # IV bolus
                dose_bolus <-
                        initial_concentration * (Vcentral + Vcartridge)

                conc_bolus <- dose_bolus / VinjectBolus
                total_injected_volume <- VinjectBolus * numberOfDosesBolus


                #Create a table that summarizes everything
                Parameters <- tibble(
                  Group = c(rep("Drug",4),
                            rep("Volume",4),
                            rep("Bolus",4),
                            rep("Experiment",4)),
                        Parameter = c(
                                "Drug name",
                                "Cmax central after 1 dose (µg/mL)",
                                "Bolus solution concentration (µg/mL)",
                                "Half life central (h)",
                                "Central volume (mL)",
                                "Cartridge volume (mL)",
                                "Spent volume diluant to central (mL)",
                                "Waste volume (mL)",
                                "Bolus volume for 1 dose (mL)",
                                "Total number of doses",
                                "Dosing interval (h)",
                                "Total injected volume (mL)",
                                "Duration of experiment (h)",
                                "Flow pump diluant to central (mL/min)",
                                "Flow pump central to waste (mL/min)",
                                "Flow pump central to cartridge (mL/min)"
                        ),
                        Value = c(drugName,
                                  round(
                                          c(
                                                  initial_concentration,
                                                  conc_bolus,
                                                  halfLifeHours,
                                                  Vcentral,
                                                  Vcartridge,
                                                  volume_diluant_to_central,
                                                  volume_waste_produced,
                                                  VinjectBolus,
                                                  numberOfDosesBolus,
                                                  dosingIntervalHoursBolus,
                                                  total_injected_volume,
                                                  lastTimePointHours,
                                                  debit_pompe_dil_central,
                                                  debit_pompe_central_waste,
                                                  debit_central_cartridge
                                          ),
                                          3
                                  )),
                        ID =        c(
                                "drugName",
                                "initial_concentration",
                                "conc_bolus",
                                "halfLifeHours",
                                "Vcentral",
                                "Vcartridge",
                                "volume_diluant_to_central",
                                "volume_waste_produced",
                                "Vinject",
                                "numberOfDoses",
                                "dosingIntervalHours",
                                "total_injected_volume",
                                "lastTimePointHours",
                                "debit_pompe_dil_central",
                                "debit_pompe_central_waste",
                                "debit_central_cartridge"
                        )
                )

        }
        if (adm.type == "Infusion")
        {
                tinfuseMin <- tinfuseHours * 60
                debit_infuse <- VinjectInf / tinfuseHours
                keHours <- log(2) / halfLifeHours
                dose_infuse <-
                        (
                                initial_concentration * (Vcentral + Vcartridge) * keHours * tinfuseHours
                        ) / (1 - exp(-keHours *
                                             tinfuseHours))#µg
                conc_infuse <- dose_infuse / VinjectInf

                total_infused_volume <- VinjectInf * numberOfDosesInf

                #Create a table that summarizes everything
                Parameters <- tibble(
                        Group = c(rep("Drug",4),
                                  rep("Volume",4),
                                  rep("Infusion",4),
                                  rep("Experiment",5)),
                        Parameter = c(
                                "Drug Name",
                                "Cmax central after 1 dose (µg/mL)",
                                "Infusion solution concentration (µg/mL)",
                                "Half life central (h)",
                                "Central volume (mL)",
                                "Cartridge volume (mL)",
                                "Spent volume diluant to central (mL)",
                                "Waste volume (mL)",
                                "Infusion volume for 1 dose (mL)",
                                "Total number of doses",
                                "Dosing interval (h)",
                                "Total infused volume (mL)",
                                "Duration of experiment (h)",
                                "Flow pump diluant to central (mL/min)",
                                "Flow pump central to waste (mL/min)",
                                "Flow pump central to cartridge (mL/min)",
                                "Flow infusion pump (mL/h)"
                        ),
                        Value = c(drugName,
                                  round(
                                          c(
                                                  initial_concentration,
                                                  conc_infuse,
                                                  halfLifeHours,
                                                  Vcentral,
                                                  Vcartridge,
                                                  volume_diluant_to_central,
                                                  volume_waste_produced,
                                                  VinjectInf,
                                                  numberOfDosesInf,
                                                  dosingIntervalHoursInf,
                                                  total_infused_volume,
                                                  lastTimePointHours,
                                                  debit_pompe_dil_central,
                                                  debit_pompe_central_waste,
                                                  debit_central_cartridge,
                                                  debit_infuse
                                          ),
                                          3
                                  )),
                        ID =        c(
                                "drugName",
                                "initial_concentration",
                                "conc_infuse",
                                "halfLifeHours",
                                "Vcentral",
                                "Vcartridge",
                                "volume_diluant_to_central",
                                "volume_waste_produced",
                                "Vinject",
                                "numberOfDoses",
                                "dosingIntervalHours",
                                "total_infused_volume",
                                "lastTimePointHours",
                                "debit_pompe_dil_central",
                                "debit_pompe_central_waste",
                                "debit_central_cartridge",
                                "debit_infuse"
                        )
                )

        }
        if (adm.type == "Loading dose + Infusion")
        {
          tinfuseMin <- tinfuseHours * 60
          keHours <- log(2) / halfLifeHours
          rate_infuse <- keHours * Css * (Vcentral + Vcartridge)
          debit_infuse <- rate_infuse/Cinfusemaintenance
          total_infused_volume <- debit_infuse*lastTimePointMin
          dose_loading <- Css * (Vcentral + Vcartridge)
          CLoadingDose <- dose_loading/VinjectLoadingDose

          #Create a table that summarizes everything
          Parameters <- tibble(
            Group = c(rep("Drug",3),
                      rep("Volume",4),
                      rep("Loading dose and infusion",4),
                      rep("Experiment",5)),
            Parameter = c(
              "Drug Name",
              "Steady state concentration (µg/mL)",
              "Half life central (h)",
              "Central volume (mL)",
              "Cartridge volume (mL)",
              "Spent volume diluant to central (mL)",
              "Waste volume (mL)",
              "Loading dose concentration (µg/mL)",
              "Loading dose volume (mL)",
              "Infusion solution concentration (µg/mL)",
              "Total infused volume (mL)",
              "Duration of experiment (h)",
              "Flow pump diluant to central (mL/min)",
              "Flow pump central to waste (mL/min)",
              "Flow pump central to cartridge (mL/min)",
              "Flow infusion pump (mL/h)"
            ),
            Value = c(drugName,
                      round(
                        c(
                          Css,
                          halfLifeHours,
                          Vcentral,
                          Vcartridge,
                          volume_diluant_to_central,
                          volume_waste_produced,
                          CLoadingDose,
                          VinjectLoadingDose,
                          Cinfusemaintenance,
                          total_infused_volume,
                          lastTimePointHours,
                          debit_pompe_dil_central,
                          debit_pompe_central_waste,
                          debit_central_cartridge,
                          debit_infuse
                        ),
                        3
                      )),
            ID =        c(
              "drugName",
              "Css",
              "halfLifeHours",
              "Vcentral",
              "Vcartridge",
              "volume_diluant_to_central",
              "volume_waste_produced",
              "CLoadingDose",
              "VinjectLoadingDose",
              "Cinfusemaintenance",
              "total_infused_volume",
              "lastTimePointHours",
              "debit_pompe_dil_central",
              "debit_pompe_central_waste",
              "debit_central_cartridge",
              "debit_infuse"
            )
          )

        }
        return(Parameters)
}
