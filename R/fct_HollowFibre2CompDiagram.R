#' HollowFibre2CompDiagram
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
HollowFibre2CompDiagram <- function(Parameters,
                                    admType,
                                    imgFilePath)
{
  library(png)
  library(ggplot2)

  HFimg <- png::readPNG(file.path(imgFilePath,"2CompApp.png"))

  h <- dim(HFimg)[1] # image height
  w <- dim(HFimg)[2] # image width

  df <-
    data.frame(x = rnorm(10, w / 1.99, w / 100),
               y = rnorm(10, h / 2.01, h / 97))
  if (admType == "Bolus")
  {
    plot <- ggplot(df, aes(x, y)) +
      annotation_custom(grid::rasterGrob(
        HFimg,
        width = unit(1, "npc"),
        height = unit(1, "npc")
      ),
      0,
      w,
      0,-h) + # The minus is needed to get the y scale reversed
      scale_x_continuous(expand = c(0, 0), limits = c(0, w)) +
      scale_y_reverse(expand = c(0, 0), limits = c(h, 0)) + # The y scale is reversed because in image the vertical positive direction is typically downward
      # Also note the limits where h>0 is the first parameter.
      coord_equal() + # To keep the aspect ratio of the image.
      geom_text(
        size = 3,
        mapping = aes(
          x = 200,
          y = 1310,
          label = paste0(
            filter(Parameters, ID == "volume_waste_produced") %>% select(Value) %>% as.numeric() %>% round(0),
            " mL"
          )
        ),
        color = "#0ed145"
      ) + # volume waste
      geom_text(
        size = 3,
        mapping = aes(
          x = 1775,
          y = 1290,
          label = paste0(filter(Parameters, ID == "Vextra") %>% select(Value)  %>% as.numeric() %>% round(0),
                         " mL")
        ),
        color = "#0ed145"
      ) + # volume extra
      geom_text(
        size = 3,
        mapping = aes(
          x = 2450,
          y = 1290,
          label = paste0(
            filter(Parameters, ID == "volume_diluant_to_extra") %>% select(Value)  %>% as.numeric() %>% round(0),
            " mL"
          )
        ),
        color = "#0ed145"
      ) + # volume dil extra
      geom_text(
        size = 3,
        mapping = aes(
          x = 975,
          y = 1500,
          label = paste0(filter(Parameters, ID == "Vcentral") %>% select(Value)  %>% as.numeric() %>% round(0), " mL")
        ),
        color = "#0ed145"
      ) + # volume central
      geom_text(
        size = 3,
        mapping = aes(
          x = 1475,
          y = 1935,
          label = paste0(
            filter(Parameters, ID == "volume_diluant_to_central") %>% select(Value)  %>% as.numeric() %>% round(0),
            " mL"
          )
        ),
        color = "#0ed145"
      ) + # volume diluent central
      geom_text(
        size = 3,
        mapping = aes(x = 1550,
                      y = 400,
                      label = paste0(
                        filter(Parameters, ID == "debit_central_cartridge") %>% select(Value),
                        " mL/min"
                      )),
        color = "#ff7f27"
      ) + # flow cartridge pump
      geom_text(
        size = 3,
        mapping = aes(
          x = 1235,
          y = 950,
          label = paste0(
            filter(Parameters, ID == "debit_pompe_dil_central") %>% select(Value),
            "\n mL/min"
          )
        ),
        color = "#ff7f27"
      ) + # flow diluant pump
      geom_text(
        size = 3,
        mapping = aes(
          x = 1700,
          y = 150,
          label = paste0(
            filter(Parameters, ID == "Vcartridge") %>% select(Value),
            " mL"
          )
        ),
        color =
          "#0ed145"
      ) + # volume cartridge
      geom_text(
        size = 3,
        mapping = aes(
          x = 475,
          y = 675,
          label = paste0(
            filter(Parameters, ID == "debit_pompe_central_waste") %>% select(Value),
            " mL/min"
          )
        ),
        color = "#ff7f27"
      ) + #flow central to waste
      geom_text(
        size = 3,
        mapping = aes(
          x = 1525,
          y = 675,
          label = paste0(
            filter(Parameters, ID == "debit_pompe_extra_central") %>% select(Value),
            " mL/min"
          )
        ),
        color = "#ff7f27"
      ) + #flow extra to central
      geom_text(
        size = 3,
        mapping = aes(
          x = 2175,
          y = 675,
          label = paste0(
            filter(Parameters, ID == "debit_pompe_dil_extra") %>% select(Value),
            " mL/min"
          )
        ),
        color = "#ff7f27"
      ) + #flow diluant to extra
      geom_text(
        size = 3,
        mapping = aes(
          x = 550,
          y = 925,
          label = paste0(filter(Parameters, ID == "drugNameA") %>% select(Value))
        ),
        color = "black"
      ) + # drugnameA
      geom_text(
        size = 3,
        mapping = aes(
          x = 550,
          y = 975,
          label = paste0(
            "Every ",
            filter(Parameters, ID == "dosingIntervalHours_A") %>% select(Value),
            " h"
          )
        ),
        color = "black"
      ) + # time between doses
      geom_text(
        size = 3,
        mapping = aes(
          x = 550,
          y = 1025,
          label = paste0(
            filter(Parameters, ID == "Vinject_A") %>% select(Value),
            " mL"
          )
        ),
        color = "#0ed145"
      ) + #volume bolus
      geom_text(
        size = 3,
        mapping = aes(
          x = 550,
          y = 1075,
          label = paste0(
            filter(Parameters, ID == "conc_bolus_A") %>% select(Value),
            " µg/mL"
          )
        ),
        color = "blue"
      ) + #concentration bolus
      geom_text(
        size = 3,
        mapping = aes(
          x = 550,
          y = 1275,
          label = paste0(filter(Parameters, ID == "drugNameB") %>% select(Value))
        ),
        color = "black"
      ) + # drugnameB
      geom_text(
        size = 3,
        mapping = aes(
          x = 550,
          y = 1325,
          label = paste0(
            "Every ",
            filter(Parameters, ID == "dosingIntervalHours_B") %>% select(Value),
            " h"
          )
        ),
        color = "black"
      ) + # time between doses
      geom_text(
        size = 3,
        mapping = aes(
          x = 550,
          y = 1375,
          label = paste0(
            filter(Parameters, ID == "Vinject_B_central") %>% select(Value),
            " mL"
          )
        ),
        color = "#0ed145"
      ) + #volume bolus
      geom_text(
        size = 3,
        mapping = aes(
          x = 550,
          y = 1425,
          label = paste0(
            filter(Parameters, ID == "conc_bolus_B_central") %>% select(Value),
            " µg/mL"
          )
        ),
        color = "blue"
      ) + #concentration bolus
      geom_text(
        size = 3,
        mapping = aes(
          x = 2100,
          y = 860,
          label = paste0(filter(Parameters, ID == "drugNameB") %>% select(Value))
        ),
        color = "black"
      ) + # drugnameB
      geom_text(
        size = 3,
        mapping = aes(
          x = 2100,
          y = 910,
          label = paste0(
            "Every ",
            filter(Parameters, ID == "dosingIntervalHours_B") %>% select(Value),
            " h"
          )
        ),
        color = "black"
      ) + # time between doses B
      geom_text(
        size = 3,
        mapping = aes(
          x = 2100,
          y = 960,
          label = paste0(
            filter(Parameters, ID == "Vinject_B_extra") %>% select(Value),
            " mL"
          )
        ),
        color = "#0ed145"
      ) + #volume bolus B extra
      geom_text(
        size = 3,
        mapping = aes(
          x = 2100,
          y = 1010,
          label = paste0(
            filter(Parameters, ID == "conc_bolus_B_extra") %>% select(Value),
            " µg/mL"
          )
        ),
        color = "blue"
      ) + #concentration bolus B extra
      theme(
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        text = element_text(family = "sans", size = 28),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank()
      )
  }
  if (admType == "Infusion")
  {
    plot <- ggplot(df, aes(x, y)) +
      annotation_custom(grid::rasterGrob(
        HFimg,
        width = unit(1, "npc"),
        height = unit(1, "npc")
      ),
      0,
      w,
      0,-h) + # The minus is needed to get the y scale reversed
      scale_x_continuous(expand = c(0, 0), limits = c(0, w)) +
      scale_y_reverse(expand = c(0, 0), limits = c(h, 0)) + # The y scale is reversed because in image the vertical positive direction is typically downward
      # Also note the limits where h>0 is the first parameter.
      coord_equal() + # To keep the aspect ratio of the image.
      geom_text(
        size = 3,
        mapping = aes(
          x = 200,
          y = 1310,
          label = paste0(
            filter(Parameters, ID == "volume_waste_produced") %>% select(Value) %>% as.numeric() %>% round(0),
            " mL"
          )
        ),
        color = "#0ed145"
      ) + # volume waste
      geom_text(
        size = 3,
        mapping = aes(
          x = 1775,
          y = 1290,
          label = paste0(
            filter(Parameters, ID == "Vextra") %>% select(Value) %>% as.numeric() %>% round(0),
            " mL"
          )
        ),
        color = "#0ed145"
      ) + # volume extra
      geom_text(
        size = 3,
        mapping = aes(
          x = 2450,
          y = 1290,
          label = paste0(
            filter(Parameters, ID == "volume_diluant_to_extra") %>% select(Value) %>% as.numeric() %>% round(0),
            " mL"
          )
        ),
        color = "#0ed145"
      ) + # volume dil extra
      geom_text(
        size = 3,
        mapping = aes(
          x = 975,
          y = 1500,
          label = paste0(
            filter(Parameters, ID == "Vcentral") %>% select(Value) %>% as.numeric() %>% round(0),
            " mL"
          )
        ),
        color = "#0ed145"
      ) + # volume central
      geom_text(
        size = 3,
        mapping = aes(
          x = 1475,
          y = 1935,
          label = paste0(
            filter(Parameters, ID == "volume_diluant_to_central") %>% select(Value) %>% as.numeric() %>% round(0) , #
            " mL"
          )
        ),
        color = "#0ed145"
      ) + # volume diluent central
      geom_text(
        size = 3,
        mapping = aes(x = 1550,
                      y = 400,
                      label = "50 to 120 mL/min"),
        color = "#ff7f27"
      ) + # flow cartridge pump
      geom_text(
        size = 3,
        mapping = aes(
          x = 1235,
          y = 950,
          label = paste0(
            filter(Parameters, ID == "debit_pompe_dil_central") %>% select(Value),
            "\n mL/min"
          )
        ),
        color = "#ff7f27"
      ) + # flow diluant pump
      geom_text(
        size = 3,
        mapping = aes(
          x = 1700,
          y = 150,
          label = paste0(
            filter(Parameters, ID == "Vcartridge") %>% select(Value),
            " mL"
          )
        ),
        color =
          "#0ed145"
      ) + # volume cartridge
      geom_text(
        size = 3,
        mapping = aes(
          x = 475,
          y = 675,
          label = paste0(
            filter(Parameters, ID == "debit_pompe_central_waste") %>% select(Value),
            " mL/min"
          )
        ),
        color = "#ff7f27"
      ) + #flow central to waste
      geom_text(
        size = 3,
        mapping = aes(
          x = 1525,
          y = 675,
          label = paste0(
            filter(Parameters, ID == "debit_pompe_extra_central") %>% select(Value),
            " mL/min"
          )
        ),
        color = "#ff7f27"
      ) + #flow extra to central
      geom_text(
        size = 3,
        mapping = aes(
          x = 2175,
          y = 675,
          label = paste0(
            filter(Parameters, ID == "debit_pompe_dil_extra") %>% select(Value),
            " mL/min"
          )
        ),
        color = "#ff7f27"
      ) + #flow diluant to extra
      geom_text(
        size = 3,
        mapping = aes(
          x = 550,
          y = 925,
          label = paste0(filter(Parameters, ID == "drugNameA") %>% select(Value))
        ),
        color = "black"
      ) + # drugnameA
      geom_text(
        size = 3,
        mapping = aes(
          x = 550,
          y = 975,
          label = paste0(
            "Every ",
            filter(Parameters, ID == "dosingIntervalHours_A") %>% select(Value),
            " h"
          )
        ),
        color = "black"
      ) + # time between doses
      geom_text(
        size = 3,
        mapping = aes(
          x = 550,
          y = 1025,
          label = paste0(
            filter(Parameters, ID == "Vinject_A") %>% select(Value),
            " mL"
          )
        ),
        color = "#0ed145"
      ) + #volume bolus
      geom_text(
        size = 3,
        mapping = aes(
          x = 550,
          y = 1075,
          label = paste0(
            filter(Parameters, ID == "conc_infuse_A") %>% select(Value),
            " µg/mL"
          )
        ),
        color = "blue"
      ) + #concentration bolus
      geom_text(
        size = 3,
        mapping = aes(
          x = 550,
          y = 1125,
          label = paste0(
            filter(Parameters, ID == "debit_infuse_A") %>% select(Value),
            " mL/min"
          )
        ),
        color = "#ff7f27"
      ) + #flow infusion B central
      geom_text(
        size = 3,
        mapping = aes(
          x = 550,
          y = 1275,
          label = paste0(filter(Parameters, ID == "drugNameB") %>% select(Value))
        ),
        color = "black"
      ) + # drugnameB
      geom_text(
        size = 3,
        mapping = aes(
          x = 550,
          y = 1325,
          label = paste0(
            "Every ",
            filter(Parameters, ID == "dosingIntervalHours_B") %>% select(Value),
            " h"
          )
        ),
        color = "black"
      ) + # time between doses
      geom_text(
        size = 3,
        mapping = aes(
          x = 550,
          y = 1375,
          label = paste0(
            filter(Parameters, ID == "Vinject_B_central") %>% select(Value),
            " mL"
          )
        ),
        color = "#0ed145"
      ) + #volume bolus
      geom_text(
        size = 3,
        mapping = aes(
          x = 550,
          y = 1425,
          label = paste0(
            filter(Parameters, ID == "conc_infuse_B_central") %>% select(Value),
            " µg/mL"
          )
        ),
        color = "blue"
      ) + #concentration bolus
      geom_text(
        size = 3,
        mapping = aes(
          x = 550,
          y = 1475,
          label = paste0(
            filter(Parameters, ID == "debit_infuse_B_central") %>% select(Value),
            " mL/min"
          )
        ),
        color = "#ff7f27"
      ) + #flow infusion B central
      geom_text(
        size = 3,
        mapping = aes(
          x = 2100,
          y = 860,
          label = paste0(filter(Parameters, ID == "drugNameB") %>% select(Value))
        ),
        color = "black"
      ) + # drugnameB
      geom_text(
        size = 3,
        mapping = aes(
          x = 2100,
          y = 910,
          label = paste0(
            "Every ",
            filter(Parameters, ID == "dosingIntervalHours_B") %>% select(Value),
            " h"
          )
        ),
        color = "black"
      ) + # time between doses B
      geom_text(
        size = 3,
        mapping = aes(
          x = 2100,
          y = 960,
          label = paste0(
            filter(Parameters, ID == "Vinject_B_extra") %>% select(Value),
            " mL"
          )
        ),
        color = "#0ed145"
      ) + #volume bolus B extra
      geom_text(
        size = 3,
        mapping = aes(
          x = 2100,
          y = 1010,
          label = paste0(
            filter(Parameters, ID == "conc_infuse_B_extra") %>% select(Value),
            " µg/mL"
          )
        ),
        color = "blue"
      ) + #concentration bolus B extra
      geom_text(
        size = 3,
        mapping = aes(
          x = 2100,
          y = 1060,
          label = paste0(
            filter(Parameters, ID == "debit_infuse_B_extra") %>% select(Value),
            " mL/min"
          )
        ),
        color = "#ff7f27"
      ) + #flow infusion B extra

      theme(
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        text = element_text(size = 400),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank()
      )
  }
  return(plot)
}
