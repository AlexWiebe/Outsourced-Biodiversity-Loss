
### ODBL_fig2.R

### Alex Wiebe
### Princeton University
### 4/22/23

### DESCRIPTION:
### Creates supplementary figure reporting median
### contributions to species rather than total biodiversity loss metrics.

### Libraries
setwd("")
library(ggplot2)
library(grid)
library(shadowtext)
library(terra)
library(tidyverse)

### Read data
### Read global data
{
  dir_rept = list.files (path = paste0(getwd(), "/IDBL_outputs/species_data/Reptile"),
                         pattern = "*", full.names = T)
  rept = lapply(dir_rept, read.csv, header = T)
  dir_bird = list.files (path = paste0(getwd(), "/IDBL_outputs/species_data/Bird"),
                         pattern = "*", full.names = T)
  bird = lapply(dir_bird, read.csv, header = T)
  dir_mammal = list.files (path = paste0(getwd(), "/IDBL_outputs/species_data/Mammal"),
                           pattern = "*", full.names = T)
  mammal = lapply(dir_mammal, read.csv, header = T)
}
### Read domestic data
{
  dir_rept = list.files (path = paste0(getwd(), "/IDBL_outputs/species_data/Reptile_Domestic"),
                         pattern = "*", full.names = T)
  rept.dom = lapply(dir_rept, read.csv, header = T)
  dir_bird = list.files (path = paste0(getwd(), "/IDBL_outputs/species_data/Bird_Domestic"),
                         pattern = "*", full.names = T)
  bird.dom = lapply(dir_bird, read.csv, header = T)
  dir_mammal = list.files (path = paste0(getwd(), "/IDBL_outputs/species_data/Mammal_Domestic"),
                           pattern = "*", full.names = T)
  mammal.dom = lapply(dir_mammal, read.csv, header = T)
}

country.names = c("ARG", "AUS", "BRA", "CAN", "CHN", "DEU", "FRA",
                  "GBR", "IDN", "IND", "IRN", "ITA", "JPN",
                  "KOR", "MEX", "NOR", "RUS", "SAU", "SGP",
                  "SWE", "TUR", "UKR", "USA", "ZAF")
full.country.names = c("Argentina", "Australia", "Brazil", "Canada", "China", "Germany",
                       "France", "United Kingdom", "Indonesia", "India", "Iran",
                       "Italy", "Japan", "South Korea", "Mexico", "Norway",
                       "Russia", "Saudi Arabia", "Singapore", "Sweden",
                       "Turkey", "Ukraine", "USA",
                       "South Africa")


###--------------Data manipulation

### Create one dataframe with each country's global data
{
  global = data.frame(country.abbrev = country.names,
                      country.name = full.country.names,
                      global.median = rep(NA, length(country.names)),
                      int.median = rep(NA, length(country.names)),
                      dom.median = rep(NA, length(country.names)))
}

### Populate global.median (median global contribution to species' range loss)
{
  for (c in 1:length(global$country.name)){
    r = rept[[c]]
    b = bird[[c]]
    m = mammal[[c]]
    
    # Remove all species without coverage
    r = r[r$aoh2000 > 0,]; b = b[b$aoh2000 > 0,]; m = m[m$aoh2000 > 0,]
    
    # Remove extinct species
    ex = read.csv("IUCN_extinctspecies.csv", header = T)
    r = r[!(r$id_no %in% ex$taxonid),]
    b = b[!(b$id_no %in% ex$taxonid),]
    m = m[!(m$id_no %in% ex$taxonid),]
    
    r$rangeeq = r$drivenloss / r$rangeloss
    b$rangeeq = b$drivenloss / b$rangeloss
    m$rangeeq = m$drivenloss / m$rangeloss

    
    loss.median = median(c(r$rangeeq, b$rangeeq, m$rangeeq), na.rm = T)
    
    global$global.median[c] = loss.median
  }
}

### Populate dom.median (median domestic contribution to species' range loss)
{
  for (c in 1:length(global$country.name)){
    r = rept[[c]]
    b = bird[[c]]
    m = mammal[[c]]
    r.dom = rept.dom[[c]]
    b.dom = bird.dom[[c]]
    m.dom = mammal.dom[[c]]
    
    # We need global AOH2000 for subsequent calcs, not domestic AOH2000
    if(all(r$id_no == r.dom$id_no) & all(b$id_no == b.dom$id_no) &
       all(m$id_no == m.dom$id_no)){
      r.dom$aoh2000 = r$aoh2000
      b.dom$aoh2000 = b$aoh2000
      m.dom$aoh2000 = m$aoh2000
    } else{
      print("Something wrong here.")
    }
    
    # Remove all species without coverage
    r.dom = r.dom[r.dom$aoh2000 > 0,]
    b.dom = b.dom[b.dom$aoh2000 > 0,]
    m.dom = m.dom[m.dom$aoh2000 > 0,]
    
    # Remove extinct species
    ex = read.csv("IUCN_extinctspecies.csv", header = T)
    r.dom = r.dom[!(r.dom$id_no %in% ex$taxonid),]
    b.dom = b.dom[!(b.dom$id_no %in% ex$taxonid),]
    m.dom = m.dom[!(m.dom$id_no %in% ex$taxonid),]
    
    r.dom$rangeeq = r.dom$drivenloss / r.dom$rangeloss
    b.dom$rangeeq = b.dom$drivenloss / b.dom$rangeloss
    m.dom$rangeeq = m.dom$drivenloss / m.dom$rangeloss

    
    loss.median = median(c(r.dom$rangeeq, b.dom$rangeeq, m.dom$rangeeq), na.rm = T)
    
    if(is.na(loss.median)) # Iran exception
      loss.median = 0
    
    global$dom.median[c] = loss.median
  }
}

### Populate int.rangeeqs (international range equivalents of habitat loss)
{
  for (c in 1:length(global$country.name)){
    r = rept[[c]]
    b = bird[[c]]
    m = mammal[[c]]
    r.dom = rept.dom[[c]]
    b.dom = bird.dom[[c]]
    m.dom = mammal.dom[[c]]
    r.int = data.frame(binomial = r$binomial,
                       aoh2000 = r$aoh2000 - r.dom$aoh2000,
                       rangeloss = r$rangeloss - r.dom$rangeloss,
                       drivenloss = r$drivenloss - r.dom$drivenloss)
    b.int = data.frame(binomial = b$binomial,
                       aoh2000 = b$aoh2000 - b.dom$aoh2000,
                       rangeloss = b$rangeloss - b.dom$rangeloss,
                       drivenloss = b$drivenloss - b.dom$drivenloss)
    m.int = data.frame(binomial = m$binomial,
                       aoh2000 = m$aoh2000 - m.dom$aoh2000,
                       rangeloss = m$rangeloss - m.dom$rangeloss,
                       drivenloss = m$drivenloss - m.dom$drivenloss)
    
    
    # Remove all species without coverage
    r.int = r.int[r.int$aoh2000 > 0,]
    b.int = b.int[b.int$aoh2000 > 0,]
    m.int = m.int[m.int$aoh2000 > 0,]
    
    r.int$rangeeq = r.int$drivenloss / r.int$rangeloss
    b.int$rangeeq = b.int$drivenloss / b.int$rangeloss
    m.int$rangeeq = m.int$drivenloss / m.int$rangeloss

    loss.median = median(c(r.int$rangeeq, b.int$rangeeq, m.int$rangeeq), na.rm = T)
    
    global$int.median[c] = loss.median
  }
}


###-------Plotting the figure

d <- global
d$global.median = 100 * d$global.median
d$dom.median = 100 * d$dom.median
d$int.median = 100 * d$int.median

BLUE <- "#25325C"
  RED <- "#ab0000"
    # BLUE <- "#076fa2"
  # RED <- "#E3120B"
  BLACK <- "#202020"
    GREY <- "grey50"
      
    ### Separate plots of total domestic and international contributions
    # Individual plots saved as 400 x 585
    ## Domestic plot
    {
      d <- d %>%
        mutate(country.name = fct_reorder(country.name, dom.median))
      plt <- ggplot(data = d, aes(x = dom.median, y = country.name,
                                  fill = BLUE)) +
        geom_col(width = 0.6, position = position_stack(reverse = T)) +
        scale_x_continuous(
          limits = c(0, 100),
          breaks = seq(0, 100, by = 10), 
          expand = c(0, 0), # The horizontal axis does not extend to either side
          position = "top"  # Labels are located on the top
        ) +
        scale_y_discrete(expand = expansion(add = c(0, 0.5))) +
        theme(
          # Set background color to white
          panel.background = element_rect(fill = "white"),
          # Set the color and the width of the grid lines for the horizontal axis
          panel.grid.major.x = element_line(color = "#A8BAC4", size = 0.3),
          # Remove tick marks by setting their length to 0
          axis.ticks.length = unit(0, "mm"),
          # Remove the title for both axes
          axis.title = element_blank(),
          # Only left line of the vertical axis is painted in black
          axis.line.y.left = element_line(color = "black"),
          # Remove labels from the vertical axis
          axis.text.y = element_blank(),
          # But customize labels for the horizontal axis
          axis.text.x = element_text(family = "Econ Sans Cnd", size = 8)
        ) +
        scale_fill_manual(values = c(BLUE, RED)) +
        geom_shadowtext(
          data = subset(d, dom.median < 10),
          aes(dom.median, y = country.name, label = country.name),
          hjust = 0,
          nudge_x = 1,
          color = BLUE,
          bg.color = "white",
          bg.r = 0.2,
          family = "Econ Sans Cnd",
          size = 2.5,
          fontface = "bold"
        ) + 
        geom_text(
          data = subset(d, dom.median >= 10),
          aes(0, y = country.name, label = country.name),
          hjust = 0,
          nudge_x = 2,
          colour = "white",
          family = "Econ Sans Cnd",
          size = 2.5,
          fontface = "bold"
        ) +
        labs(
          title = "Domestic Contributions to \nGlobal Forest Biodiversity Loss",
          subtitle = str_wrap("Median % contribution to habitat loss 2001-2015",
                              width = 70)
        ) + 
        theme(
          plot.title = element_text(hjust = 0.5,
                                    family = "Econ Sans Cnd", 
                                    face = "bold",
                                    size = 14
          ),
          plot.subtitle = element_text(hjust = 0.5,
                                       family = "Econ Sans Cnd",
                                       size = 8
          )
        ) +
        theme(
          plot.margin = margin(0.01, 0.05, 0.01, 0.05, "npc")
        ) +
        theme(
          legend.title = element_blank(),
          legend.position = "none"
        )
      
      
      plt
    }
    
    # To save:
    {
      png("supp_fig_median_domestic.png", units = "px", height=4200, width=3000, res=600)
      plt
      dev.off()
    }
    ## International plot
    {
      d <- d %>%
        mutate(country.name = fct_reorder(country.name, int.median))
      plt <- ggplot(data = d, aes(x = int.median, y = country.name,
                                  fill = RED)) +
        geom_col(width = 0.6, position = position_stack(reverse = T)) +
        scale_x_continuous(
          limits = c(0, 4),
          breaks = seq(0, 4, by = 0.5), 
          expand = c(0, 0), # The horizontal axis does not extend to either side
          position = "top"  # Labels are located on the top
        ) +
        scale_y_discrete(expand = expansion(add = c(0, 0.5))) +
        theme(
          # Set background color to white
          panel.background = element_rect(fill = "white"),
          # Set the color and the width of the grid lines for the horizontal axis
          panel.grid.major.x = element_line(color = "#A8BAC4", size = 0.3),
          # Remove tick marks by setting their length to 0
          axis.ticks.length = unit(0, "mm"),
          # Remove the title for both axes
          axis.title = element_blank(),
          # Only left line of the vertical axis is painted in black
          axis.line.y.left = element_line(color = "black"),
          # Remove labels from the vertical axis
          axis.text.y = element_blank(),
          # But customize labels for the horizontal axis
          axis.text.x = element_text(family = "Econ Sans Cnd", size = 8)
        ) +
        scale_fill_manual(values = c(RED)) +
        geom_shadowtext(
          data = subset(d, int.median < 0.4 | country.name == "United Kingdom"),
          aes(int.median, y = country.name, label = country.name),
          hjust = 0,
          nudge_x = 0.05,
          color = BLUE,
          bg.color = "white",
          bg.r = 0.2,
          family = "Econ Sans Cnd",
          size = 2.5,
          fontface = "bold"
        ) + 
        geom_text(
          data = subset(d, int.median >= 0.4 & country.name != "United Kingdom"),
          aes(0, y = country.name, label = country.name),
          hjust = 0,
          nudge_x = 0.05,
          colour = "white",
          family = "Econ Sans Cnd",
          size = 2.5,
          fontface = "bold"
        ) +
        labs(
          title = "International Contributions to \nGlobal Forest Biodiversity Loss",
          subtitle = str_wrap("Median % contribution to habitat loss 2001-2015",
                              width = 70)
        ) + 
        theme(
          plot.title = element_text(hjust = 0.5,
                                    family = "Econ Sans Cnd", 
                                    face = "bold",
                                    size = 14
          ),
          plot.subtitle = element_text(hjust = 0.5,
                                       family = "Econ Sans Cnd",
                                       size = 8
          )
        ) +
        theme(
          plot.margin = margin(0.01, 0.05, 0.01, 0.05, "npc")
        ) +
        theme(
          legend.title = element_blank(),
          legend.position = "none"
        )
      
      
      plt
    }
    
    # To save:
    {
      png("supp_fig_median_international.png", units = "px", height=4200, width=3000, res=600)
      plt
      dev.off()
    }
    