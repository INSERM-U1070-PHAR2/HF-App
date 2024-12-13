HollowFibre1CompDiagram <- function(Parameters,
                                    adm.type,
                                    drugName,
                                    imgFilePath)
{
  library(png)
  library(ggplot2)

  HFimg <- png::readPNG(file.path(imgFilePath,"1CompApp.png"))

  h <- dim(HFimg)[1] # image height
  w <- dim(HFimg)[2] # image width

  df <-
    data.frame(x = rnorm(10, w / 1.99, w / 100),
               y = rnorm(10, h / 2.01, h / 97))
  if (adm.type == "Bolus")
  {
    plot <- ggplot(df, aes(x, y)) +
      annotation_custom(grid::rasterGrob(
        HFimg,
        width = unit(1, "npc"),
        height = unit(1, "npc")
      ),
      0,
      w,
      0,
      -h) + # The minus is needed to get the y scale reversed
      scale_x_continuous(expand = c(0, 0), limits = c(0, w)) +
      scale_y_reverse(expand = c(0, 0), limits = c(h, 0)) + # The y scale is reversed because in image the vertical positive direction is typically downward
      # Also note the limits where h>0 is the first parameter.
      coord_equal() + # To keep the aspect ratio of the image.
      geom_text(size=4,mapping=aes(
        x = 220,
        y = 1110,
        label = paste0(
          filter(Parameters, ID == "volume_waste_produced") %>% select(Value) %>% as.numeric() %>% round(0),
          " mL"
        )
      ), color = "#0ed145") + # volume waste
      geom_text(size=4,mapping=aes(
        x = 875,
        y = 1230,
        label = paste0(filter(Parameters, ID == "Vcentral") %>% select(Value) %>% as.numeric() %>% round(0), " mL")
      ), color = "#0ed145") + # volume central
      geom_text(
        size = 4,
        mapping = aes(
          x = 1275,
          y = 375,
          label = paste0(
            filter(Parameters, ID == "debit_central_cartridge") %>% select(Value),
            " mL/min"
          )
        ),
        color = "#ff7f27"
      ) +
      geom_text(size=4,mapping=aes(
        x = 1275,
        y = 575,
        label = paste0(
          filter(Parameters, ID == "debit_pompe_dil_central") %>% select(Value),
          " mL/min"
        )
      ), color = "#ff7f27") + # flow diluant pump
      geom_text(size=4,mapping=aes(
        x = 1462.5,
        y = 140,
        label = paste0(
          filter(Parameters, ID == "Vcartridge") %>% select(Value) %>% as.numeric() %>% round(0),
          " mL"
        )
      ), color =
        "#0ed145") + # volume cartridge
      geom_text(size=4,mapping=aes(
        x = 475,
        y = 575,
        label = paste0(
          filter(Parameters, ID == "debit_pompe_central_waste") %>% select(Value),
          " mL/min"
        )
      ), color = "#ff7f27") + #flow central to waste
      geom_text(size=4,mapping=aes(
        x = 550,
        y = 825,
        label = paste0(filter(Parameters, ID == "drugName") %>% select(Value))
      ), color = "black") + # drugnam
      geom_text(size=4,mapping=aes(
        x = 550,
        y = 875,
        label = paste0(
          "Every ",
          filter(Parameters, ID == "dosingIntervalHours") %>% select(Value),
          " h"
        )
      ), color = "black") + # time between doses
      geom_text(size=4,mapping=aes(
        x = 550,
        y = 925,
        label = paste0(filter(Parameters, ID == "Vinject") %>% select(Value), " mL")
      ), color = "#0ed145") + #volume bolus
      geom_text(size=4,mapping=aes(
        x = 550,
        y = 975,
        label = paste0(
          filter(Parameters, ID == "conc_bolus") %>% select(Value),
          " µg/mL"
        )
      ), color = "blue") + #concentration bolus
      theme(
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        text=element_text(family = "sans",size=28),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank()
      )
  }
  if (adm.type == "Infusion")
  {
    plot <- ggplot(df, aes(x, y)) +
      annotation_custom(grid::rasterGrob(
        HFimg,
        width = unit(1, "npc"),
        height = unit(1, "npc")
      ),
      0,
      w,
      0,
      -h) + # The minus is needed to get the y scale reversed
      scale_x_continuous(expand = c(0, 0), limits = c(0, w)) +
      scale_y_reverse(expand = c(0, 0), limits = c(h, 0)) + # The y scale is reversed because in image the vertical positive direction is typically downward
      # Also note the limits where h>0 is the first parameter.
      coord_equal() + # To keep the aspect ratio of the image.
      geom_text(size=4,mapping=aes(
        x = 220,
        y = 1110,
        label = paste0(
          filter(Parameters, ID == "volume_waste_produced") %>% select(Value),
          " mL"
        )
      ), color = "#0ed145") + # volume waste
      geom_text(size=4,mapping=aes(
        x = 875,
        y = 1230,
        label = paste0(filter(Parameters, ID == "Vcentral") %>% select(Value), " mL")
      ), color = "#0ed145") + # volume central
      geom_text(size=4,mapping=aes(
        x = 1510,
        y = 1100,
        label = paste0(
          filter(Parameters, ID == "volume_diluant_to_central") %>% select(Value),
          " mL"
        )
      ), color = "#0ed145") + # volume diluent
      geom_text(
        size = 4,
        mapping = aes(
          x = 1275,
          y = 375,
          label = paste0(
            filter(Parameters, ID == "debit_central_cartridge") %>% select(Value),
            " mL/min"
          )
        ),
        color = "#ff7f27"
      ) + # flow cartridge pump
      geom_text(size=4,mapping=aes(
        x = 1275,
        y = 575,
        label = paste0(
          filter(Parameters, ID == "debit_pompe_dil_central") %>% select(Value),
          " mL/min"
        )
      ), color = "#ff7f27") + # flow diluant pump
      geom_text(size=4,mapping=aes(
        x = 1462.5,
        y = 140,
        label = paste0(
          filter(Parameters, ID == "Vcartridge") %>% select(Value),
          " mL"
        )
      ), color =
        "#0ed145") + # volume cartridge
      geom_text(size=4,mapping=aes(
        x = 475,
        y = 575,
        label = paste0(
          filter(Parameters, ID == "debit_pompe_central_waste") %>% select(Value),
          " mL/min"
        )
      ), color = "#ff7f27") + #flow central to waste
      geom_text(size=4,mapping=aes(
        x = 550,
        y = 825,
        label = paste0(filter(Parameters, ID == "drugName") %>% select(Value))
      ), color = "black") + # drugnam
      geom_text(size=4,mapping=aes(
        x = 550,
        y = 875,
        label = paste0(
          "Every ",
          filter(Parameters, ID == "dosingIntervalHours") %>% select(Value),
          " h"
        )
      ), color = "black") + # time between doses infusion
      geom_text(size=4,mapping=aes(
        x = 550,
        y = 925,
        label = paste0(filter(Parameters, ID == "Vinject") %>% select(Value), " mL")
      ), color = "#0ed145") + #volume infusion
      geom_text(size=4,mapping=aes(
        x = 550,
        y = 975,
        label = paste0(
          filter(Parameters, ID == "conc_infuse") %>% select(Value),
          " µg/mL"
        )
      ), color = "blue") + #concentration infusion
      geom_text(size=4,mapping=aes(
        x = 550,
        y = 1025,
        label = paste0(
          filter(Parameters, ID == "debit_infuse") %>% select(Value),
          " mL/h"
        )
      ), color = "#ff7f27") + #flow infusion
      theme(
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        text=element_text(family = "sans",size=28),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank()
      )
  }
  if (adm.type == "Loading dose + Infusion")
  {
    plot <- ggplot(df, aes(x, y)) +
      annotation_custom(grid::rasterGrob(
        HFimg,
        width = unit(1, "npc"),
        height = unit(1, "npc")
      ),
      0,
      w,
      0,
      -h) + # The minus is needed to get the y scale reversed
      scale_x_continuous(expand = c(0, 0), limits = c(0, w)) +
      scale_y_reverse(expand = c(0, 0), limits = c(h, 0)) + # The y scale is reversed because in image the vertical positive direction is typically downward
      # Also note the limits where h>0 is the first parameter.
      coord_equal() + # To keep the aspect ratio of the image.
      geom_text(size=4,mapping=aes(
        x = 220,
        y = 1110,
        label = paste0(
          filter(Parameters, ID == "volume_waste_produced") %>% select(Value),
          " mL"
        )
      ), color = "#0ed145") + # volume waste
      geom_text(size=4,mapping=aes(
        x = 875,
        y = 1230,
        label = paste0(filter(Parameters, ID == "Vcentral") %>% select(Value), " mL")
      ), color = "#0ed145") + # volume central
      geom_text(size=4,mapping=aes(
        x = 1510,
        y = 1100,
        label = paste0(
          filter(Parameters, ID == "volume_diluant_to_central") %>% select(Value),
          " mL"
        )
      ), color = "#0ed145") + # volume diluent
      geom_text(
        size = 4,
        mapping = aes(
          x = 1275,
          y = 375,
          label = paste0(
            filter(Parameters, ID == "debit_central_cartridge") %>% select(Value),
            " mL/min"
          )
        ),
        color = "#ff7f27"
      ) + # flow cartridge pump
      geom_text(size=4,mapping=aes(
        x = 1275,
        y = 575,
        label = paste0(
          filter(Parameters, ID == "debit_pompe_dil_central") %>% select(Value),
          " mL/min"
        )
      ), color = "#ff7f27") + # flow diluant pump
      geom_text(size=4,mapping=aes(
        x = 1462.5,
        y = 140,
        label = paste0(
          filter(Parameters, ID == "Vcartridge") %>% select(Value),
          " mL"
        )
      ), color =
        "#0ed145") + # volume cartridge
      geom_text(size=4,mapping=aes(
        x = 475,
        y = 575,
        label = paste0(
          filter(Parameters, ID == "debit_pompe_central_waste") %>% select(Value),
          " mL/min"
        )
      ), color = "#ff7f27") + #flow central to waste
      geom_text(size=4,mapping=aes(
        x = 550,
        y = 825,
        label = paste0(filter(Parameters, ID == "drugName") %>% select(Value))
      ), color = "black") + # drugnam
      geom_text(size=4,mapping=aes(
        x = 550,
        y = 875,
        label = paste0(
          "Loading dose ",
          filter(Parameters, ID == "CLoadingDose") %>% select(Value),
          "µg/mL"
        )
      ), color = "black") + # time between doses infusion
      geom_text(size=4,mapping=aes(
        x = 550,
        y = 925,
        label = paste0(filter(Parameters, ID == "VinjectLoadingDose") %>% select(Value), " mL")
      ), color = "#0ed145") + #volume infusion
      geom_text(size=4,mapping=aes(
        x = 550,
        y = 975,
        label = paste0(
          filter(Parameters, ID == "Cinfusemaintenance") %>% select(Value),
          " µg/mL"
        )
      ), color = "blue") + #concentration infusion
      geom_text(size=4,mapping=aes(
        x = 550,
        y = 1025,
        label = paste0(
          filter(Parameters, ID == "debit_infuse") %>% select(Value),
          " mL/h"
        )
      ), color = "#ff7f27") + #flow infusion
      theme(
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        text=element_text(family = "sans",size=28),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank()
      )
  }
  return(plot)
}
