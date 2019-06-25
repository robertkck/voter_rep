
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(plotly)
library(countrycode)
library(DT)
library(scales)
library(SciencesPo)
library(tidyverse)
# library(ggvis)
source('funk/voting_gini.R')
source('funk/camcom.R')
source('funk/powcom.R')
source('funk/parabolic.R')
source('funk/limitloss.R')
source('funk/noloss.R')
source('funk/malapportionment.R')

scenarios_list <- c(
  "Status quo",
  "Current Proposal and Amendments",
  "Simple Brexit Scenarios",
  "Minimising inequality within the Treaty",
  "Treaty change"
)
brexit_list <- c(
  "Drop 73 MEPs",
  "Equally distribute 73 MEPs",
  "Distribute 73 seats at current proportions",
  "Distribute 73 seats to increase representativeness"
  # "Distribute 73 seats to increase representativeness (no maximum)",
  # "Allocate seats to transnational list"
)

brexit_props <- c(
  "AFCO Proposal",
  "Amendment 140: No reallocation",
  "Amendment 141: Guy Verhofstadt",
  "Amendment 142: György Schöpflin",
  "Amendment 143: Pascal Durand",
  "Amendment 144: Martina Anderson",
  "Amendment 145: Jerome Lavrilleux",
  "Amendment 146: Michal Boni et al."
  # "Drop 73 MEPs",
  # "Equally distribute 73 MEPs",
  # "Distribute 73 seats at current proportions",
  # "Distribute 73 seats to increase representativeness"
  # "Distribute 73 seats to increase representativeness (no maximum)",
  # "Allocate seats to transnational list"
)

treaty_list <- c(
  # "Cambridge Compromise (total 751)",
  "Cambridge Compromise (total 637) - minimise Gini",
  "Cambridge Compromise (total 736) - minimise malapportionment"
)
brexit_num <- c("1", "7", "5", "2") #  "4" "9"
brexit_prop_num <- c(2, 3, 4, 5, 6, 7, 8, 9) #  "4" "9"
treaty_num <- c( "_gini", "6") #  "3"

scenarios_num <- c("", "1", "7", "5", "2", "9", "4", "3", "6" ) # Status quo wrong

colors <- c(
    "#d8d506",# ALDE
    "#104E8B",# ECR
    "#40E0D0",# EFDD
    "#1E90FF",# ENF
    '#030E40',# EPP
    "#00CD66",# Greens/EFA
    "#CD2626",# GUE/NGL
    '#D3D3D3',# NI
    "#FF3030" # S&D
  )

shinyServer(function(input, output, session) {

###########################################################################################################
# Preamble
  # Load data
  # eu_brexit <- read.csv("data/eu_brexit.csv")
  # eu_brexit$ctry <- countrycode(eu_brexit$GEO, "country.name", "iso3c")
  # eu <- read.csv("data/eu.csv")
  # eu$ctry <- countrycode(eu$GEO, "country.name", "iso3c")
  # meps <- read.csv("data/meps.csv")
  # # meps_mat <- table(meps$country, meps$grp)
  # meps_table <- count(meps, country, grp)
  # meps_table[(meps_table$country == "Spain") & (meps_table$grp == "EPP"), 'n'] <- 17
  # meps_table <- spread(meps_table, grp, n, fill=0)
  # meps_table[,-1] <- meps_table[,-1] / rowSums(meps_table[,-1])
  # meps_group <- count(meps, grp, group)
  # meps_group[(meps_group$grp == "EPP"), 'n'] <- 217
  # # m_short <- meps_group$grp
  # # m_long <- meps_group$group
  # m_labels <- data.frame(meps_group$grp, meps_group$group, colors)
  # rep_scen_comp <- NULL
  #
  # # Merge data sets
  # eu <- merge(meps_table, eu, by.x="country", by.y = "GEO")
  # eu <- eu[order(-eu$pop),]
  # eu_brexit <- merge(meps_table, eu_brexit, by.x="country", by.y = "GEO")
  # eu_brexit <- eu_brexit[order(-eu_brexit$pop),]
  #
  # # Load proposals
  # props <- read_csv("data/proposals.csv")
  # props$ctry <- countrycode(props$country, "country.name", "iso3c")
  #
  # # Defaults
  # data <- eu_brexit
  # data <- select(
  #   data, country, ALDE, ECR, EFDD, ENF, EPP,
  #   contains("Greens"), contains("GUE/NGL"), NI, contains("S&D"),
  #   ctry, pop, rep,  pop_share, pop_rep, rep_share
  # )
  # data$rep_scen <- data$rep
  # m <- 6
  # M <- 96
  # H <- 751

  load("data/data.RData")
############################################################################################################
# Reactive data generation

  # Filter data
  filteredScenario <- reactive({
    if (input$scen=="Simple Brexit Scenarios"){
      brexit_num[brexit_list == input$brexit]
    } else if (input$scen=="Current Proposal and Amendments"){
      brexit_prop_num[brexit_props == input$brexit_props]
    } else if (input$scen=="Minimising inequality within the Treaty") {
      treaty_num[treaty_list == input$treaty]
    }
  })

  filteredData <- reactive({
    scenario <- filteredScenario()
    if (input$scen=="Status quo") {
      data <- eu
      data <- rename(data, rep_share_scen = rep_share ,
                     pop_rep_scen =  pop_rep)
      data$mal_scen <- mal(data$pop_share, data$rep_share_scen)
      data$rep_scen <- data$rep
    } else if (input$scen == "Treaty change") {
      data <- myScenData()
    } else if (scenario == "_gini") {
      if (input$uk == TRUE) {data <- eu} else {data <- eu_brexit}
      data <- select(
        data, country, ALDE, ECR, EFDD, ENF, EPP,
        contains("Greens"), contains("GUE/NGL"), NI, contains("S&D"),
        ctry, pop, rep,  pop_share, pop_rep, rep_share
      )
      m <- 6
      M <- 96
      H <- 637
      out <- alloc.camcom(data$pop, m, M, H)
      data$rep_scen <- out$rep
      data$rep_share_scen <- (data$rep_scen) / sum(data$rep_scen)
      data$pop_rep_scen <- data$pop / data$rep_scen
      data$diffs_rep_scen <- data$rep_scen - data$rep
      data$diffs_pop_rep_scen <-data$pop_rep_scen -  data$pop_rep
      data$diffs_rep_share_scen <- data$rep_share_scen - data$rep_share
      # data$mal_scen <- 0.5 * sum(abs(data$rep_share_scen - data$pop_share))
      data$mal_scen <- mal(data$pop_share, data$rep_share_scen)
      # data$gini_scen <- voting_gini(data$pop_share, data$rep_share_scen)
    } else if (is.numeric(scenario)) {
    data <- select(props, country, rep_scen = scenario, pop, pop_share, ctry, rep, rep_share, pop_rep)
    # data$rep_scen <- data$rep
    data$rep_share_scen <- (data$rep_scen) / sum(data$rep_scen)
    data$pop_rep_scen <- data$pop / data$rep_scen
    data$diffs_rep_scen <- round(data$rep_scen - data$rep,2)
    # data$mal_scen <- 0.5 * sum(abs(data$rep_share_scen - data$pop_share))
    data$mal_scen <- mal(data$pop_share, data$rep_share_scen)
    }  else {
      data <- select(
        eu_brexit, country, ALDE, ECR, EFDD, ENF, EPP,
        contains("Greens"), contains("GUE/NGL"), NI, contains("S&D"),
        ctry, pop, rep, pop_share, pop_rep, rep_share,
        ends_with(scenario)
      )
      names(data) <- gsub(scenario, "_scen", names(data))
    }
    # Insert proportional benchmark
    # data$rep_prop <- 3 + data$pop * 3 / min(data$pop)
    # data$pop_rep_prop <- data$pop / data$rep_prop

    # Print
    data$gini_scen <- voting_gini(data$pop_share, data$rep_share_scen)
    data$mal_scen <- mal(data$pop_share, data$rep_share_scen)
    data
  })

  # Calculate my scenario
  myScenData <- reactive({
      if (input$uk == TRUE) {data <- eu} else {data <- eu_brexit}
      data <- select(
        data, country, ALDE, ECR, EFDD, ENF, EPP,
        contains("Greens"), contains("GUE/NGL"), NI, contains("S&D"),
        ctry, pop, rep,  pop_share, pop_rep, rep_share
      )
      m <- input$m
      M <- input$M
      H <- input$H

      data$rep_scen <- data$rep

      if (input$myscenario == "Cambridge Compromise (Base + Prop)"){
        out <- alloc.camcom(data$pop, m, M, H)
        data$rep_scen <- out$rep
      } else if (input$myscenario == "Parabolic"){
        out <- alloc.parabolic(data$pop, m, M, H)
        data$rep_scen <- out$rep
        c <- out$c
        # print(c)
        # print(sum(data$rep_scen))
        # print(M_par >= -c/(2*b))
        if ((max(data$rep_scen) > M) | (c > 0)) {
          showNotification(paste("Parabolic method not applicable for this specifications."), type = "warning", duration = 2)
        }
        validate(
          # need(M_par <= -b/(2*c), "Please select a data set")
          need(max(data$rep_scen) <= M, ""),
          need(c <= 0, "")
        )
      } else if (input$myscenario == "Limited loss"){
        allocation <- alloc.camcom(data$pop, m, M, H)
        out <- limitloss(data$rep, allocation$rep, allocation$rep_exact, m, M)
        data$rep_scen <- out$rep_scen
      } else if (input$myscenario == "No loss of seats"){
        allocation <- alloc.camcom(data$pop, m, M, H)
        out <- noloss(data$rep, allocation$rep, allocation$rep_exact, m, M)
        data$rep_scen <- out$rep_scen
      } else if (input$myscenario == "Transnational list"){
        data$rep_scen <- data$rep + input$t * data$pop_share
      } else if (input$myscenario == "Power Compromise"){
        out <- alloc.powcom(data$pop, m, M, H)
        data$rep_scen <- out$rep
        if (max(out$rep)< M) {
          showNotification(paste("Upper limit is not effective for this specification."), type = "warning", duration = 2)
        }
      }


      data$rep_share_scen <- (data$rep_scen) / sum(data$rep_scen)
      data$pop_rep_scen <- data$pop / data$rep_scen
      data$diffs_rep_scen <- round(data$rep_scen - data$rep,2)
      data$diffs_pop_rep_scen <-data$pop_rep_scen -  data$pop_rep
      data$diffs_rep_share_scen <- data$rep_share_scen - data$rep_share
      # data$mal_scen <- 0.5 * sum(abs(data$rep_share_scen - data$pop_share))
      data$mal_scen <- mal(data$pop_share, data$rep_share_scen)
      data$gini_scen <- voting_gini(data$pop_share, data$rep_share_scen)
      data$rep_scen <- round(data$rep_scen,2)
      data
  })

  # Comparison button
  rep_scen_comp <- reactiveValues()

  comparison_button <- observeEvent(input$compare, {
    data <- filteredData()
    rep_scen_comp$country <- data$country
    rep_scen_comp$comp1 <- data$rep_scen
    rep_scen_comp$comp1_share <- data$rep_share_scen
    rep_scen_comp$pop_rep_comp <- data$pop_rep_scen
  })

  clear_button <- observeEvent(input$clear, {
    rep_scen_comp$comp1 <- NULL
  })



  #############################################################################################
  ### Plots

  # Plot Representation
    output$represent <- renderPlotly({
    data <- filteredData()
    # scenario <- filteredScenario()

    # data$rep_prop <- 3 + data$pop * (sum(data$rep_scen - 3))/ sum(data$pop)

    p <- plot_ly(data, x = ~pop, hoverinfo = 'text') %>%
      config(collaborate = FALSE, displaylogo = FALSE) %>%
      # add_lines(
      #   y = ~rep_prop, name = paste0("Proportional <br> (min 3, total ", round(sum(data$rep_scen)),")" ),
      #   visible = "legendonly"
      # ) %>%
      add_markers(
        y = ~rep_scen, name = "Selected scenario",
        text = ~paste0(country, "<br>Seats: ", rep_scen, "<br>Population: ", format(pop, big.mark = ",")),
        marker = list(
          size = 8,
          color = "#a21636"
        )
      ) %>%
      # add_bars( y = ~pop_share, name = "Share in population") %>%
      layout(
        legend = list(x = 0, y = 1),
        # legend = list(showlegend = FALSE),
        paper_bgcolor='transparent',
        plot_bgcolor='transparent',
        yaxis = list(
          title = 'Seats in the EP',
          range = c(0, 140)
        ),
        xaxis = list(
          title = 'Population',
          range = c(0, 83000000)
        )
      )
    # if (input$scen != "Status quo"){
    #   p <- add_markers(
    #     p,
    #     y = ~rep, name = "Current",
    #     text = ~paste0(country, "<br>Seats: ", rep, "<br>Population: ", pop),
    #     opacity = 0.6,
    #     visible = "legendonly"
    #   )
    # }

    # print(!is.null(rep_scen_comp$comp1))
    if (!is.null(rep_scen_comp$comp1)){
      comparison_data <- data.frame(country = rep_scen_comp$country, comp1 = rep_scen_comp$comp1, comp1_share = rep_scen_comp$comp1_share)
      data <- left_join(data, comparison_data, by = "country")
      # print(data$comp1)
      p <- add_markers(
        p,
        y = data$comp1, name = "Comparison scenario",
        text = ~paste0(country, "<br>Seats: ", data$comp1, "<br>Population: ", pop),
        opacity = 0.8,
        marker = list(color = "#89B440")
        # visible = "legendonly"
      )
    }
    p
    })

  # Plot Representation with ggvis
  # vis <- reactive({
  #   ggvis(filteredData, x = ~pop, y = ~rep_scen) %>%
  #     layer_points(size := 50, size.hover := 200,
  #                  fillOpacity := 0.8, fillOpacity.hover := 1,
  #                  stroke := "#d6cfd0", fill := "#a21636",
  #                  key := ~country) %>%
  #     add_tooltip(function(x){paste0(x$country, "<br>Seats: ", x$rep_scen, "<br>Population: ", format(x$pop, big.mark = ","))}) %>%
  #     set_options(width = "auto") %>%
  #     add_axis("x", title = 'Population', format = '.2s'
  #     ) %>%
  #     add_axis("y", title = 'Seats in the EP', values = (1:5)*20)
  # })
  # vis %>% bind_shiny("plot1")


  # Plot Shares
  output$shares <- renderPlotly({
    scenario <- filteredScenario()
    data <- filteredData()
    p <- plot_ly(data, x = ~reorder(ctry, pop_share), hoverinfo = 'text') %>%
      add_bars(
        y = ~pop_share,
        name = "Share in population",
        marker = list(color = "#60bbce"),
        text = ~paste0("Population: ", percent(pop_share))
      ) %>%
      add_lines(
        y = ~rep_share_scen,
        name = "Share in EP",
        line = list(color = "#a21636"),
        text = ~paste0("Seats: ", percent(rep_share_scen))
      ) %>%
      layout(
          legend = list(orientation = 'h'),
          paper_bgcolor='transparent',
          plot_bgcolor='transparent',
          yaxis = list(
            title = 'Share in Population and Seats',
            range = c(0,0.2)
            ),
          xaxis = list(
              title = '',
              tickangle = 45
          )  # Fix this
      ) %>%
      config(collaborate = FALSE, displaylogo = FALSE)

    if (!is.null(rep_scen_comp$comp1)){
      comparison_data <- data.frame(country = rep_scen_comp$country, comp1 = rep_scen_comp$comp1, comp1_share = rep_scen_comp$comp1_share)
      data <- left_join(data, comparison_data, by = "country")
      p <- add_lines(
        p,
        y = data$comp1_share, name = "Comparison",
        text = ~paste0("Seats: ", percent(data$comp1_share)),
        opacity = 0.8,
        line = list(color = "#89B440")
        # visible = "legendonly"
      )
    }
    p
  })

  # Plot Degressive Proportionality
  output$degprop <- renderPlotly({
    data <- filteredData()
    scenario <- filteredScenario()

    # data$rep_prop <- 3 + data$pop * (sum(data$rep_scen - 3))/ sum(data$pop)
    # data$pop_rep_prop <- data$pop / data$rep_prop

    p <- plot_ly(data, x = ~log(pop), hoverinfo = 'text') %>%
      config(collaborate = FALSE, displaylogo = FALSE) %>%
      add_trace(
        y = ~pop_rep_scen, name = "Selected scenario",
        type = 'scatter',
        mode = 'lines+markers',
        text = ~paste0(country, "<br>Pop./MEP: ", round(pop_rep_scen), "<br>Seats: ", rep_scen),
        marker = list(color = "#a21636"),
        line = list(color = "#a21636")
      ) %>%
      # add_lines(
      #   y = ~pop_rep_prop, name = "Proportional <br> (min 3)",
      #   text = ~country,
      #   visible = "legendonly"
      # ) %>%
      layout(
        legend = list(x = 0, y = 1),
        # legend = list(showlegend = FALSE),
        paper_bgcolor='transparent',
        plot_bgcolor='transparent',
        yaxis = list(
          title = 'Population per seat',
          range = c(0, 901000)
        )
      )

    # if (input$scen != "Status quo"){
    #   p <- add_trace(
    #     p,
    #     y = ~pop_rep, name = "Current",
    #     text = ~paste0(country, "<br>Pop./MEP: ", round(pop_rep), "<br>Seats: ", rep),
    #     mode = 'lines+markers',
    #     opacity = 0.6,
    #     visible = "legendonly",
    #     type = 'scatter'
    #   )
    # }
    if (!is.null(rep_scen_comp$comp1)){
      comparison_data <- data.frame(country = rep_scen_comp$country, comp1 = rep_scen_comp$comp1, comp1_share = rep_scen_comp$comp1_share, pop_rep_comp = rep_scen_comp$pop_rep_comp)
      data <- left_join(data, comparison_data, by = "country")
      p <- add_trace(
        p,
        y = data$pop_rep_comp, name = "Comparison scenario",
        type = 'scatter',
        mode = 'lines+markers',
        opacity = 0.8,
        text = ~paste0(country, "<br>Pop./MEP: ", round(data$pop_rep_comp), "<br>Seats: ", data$comp1),
        marker = list(color = "#89B440"),
        line = list(color = "#89B440")
      )
    }
    p

  })


  #################################################################################################
  ### Tables

  # Render Details table
  observe({
    data <- filteredData()
    data$rep_share_scen <- percent(data$rep_share_scen)
    output$table <- DT::renderDataTable(
        if (input$scen=="Status quo") {
          datatable({
            data[,c("country", "pop_share", "rep_scen", "rep_share_scen", "pop_rep_scen" )]
          },
          rownames = FALSE,
          colnames = c("Member State", "Population %", "Seats", "Seat %", "Population/Seat")
          ) %>%
          formatRound(c('pop_rep_scen'),0) %>%
            formatPercentage(c('pop_share'),1)
        } else {
          datatable({
            data[,c("country", "pop_share", "rep_scen", "diffs_rep_scen", "rep_share_scen", "pop_rep_scen" )] #  "diffs_rep_share_scen",
          },
          rownames = FALSE,
          colnames = c("Member State", "Pop. %", "Seats", "Diff.", "Seats %", "Pop. / Seat") # "Difference in seat share",
          ) %>%
          # formatRound(c('diffs_rep_share_scen'),2) %>%
          formatRound(c('pop_rep_scen'),0) %>%
          formatPercentage(c('pop_share'),1)
        }
    )
  })

  # Render Scenario comparison table
  # observe({
  #   output$comp <- renderDataTable(datatable({
  #     m <- paste("mal", scenarios_num[c(-1)], sep="")
  #     g <- paste("gini", scenarios_num[c(-1)], sep="")
  #     s <- paste("rep", scenarios_num[c(-1)], sep="")
  #     mal <- round(as.numeric(eu$mal[1]), digits = 4)
  #     mal <-  percent(c(mal, round(as.numeric(eu_brexit[1, m]), digits=4)))
  #     gini <- round(as.numeric(eu$gini[1]), digits = 3)
  #     gini <-  c(gini, round(as.numeric(eu_brexit[1,g]), digits=3))
  #     seats <- as.numeric(sum(eu$rep))
  #     seats <- c(seats, round(as.numeric(colSums(eu_brexit[,s])), digits = 0))
  #     t <- cbind(scenarios_list, seats, mal, gini)
  #     # t[c(-11,-12),]
  #     # t <- cbind(scenarios_list[-11], seats, mal, gini)
  #     t
  #     }, options = list(paging = FALSE, searching = FALSE), selection = list(mode='single', selected = 1, target = 'row')))
  # })

  # proxy = dataTableProxy('comp')
  # observe({
  #   scenario <- filteredScenario()
  #   r <- which(scenarios_num == scenario)
  #   proxy %>% selectRows(r)
  # })



  #########################################################################################################
  ### Other info

  # Render scenario description
  output$text_scen <- renderText({
    t <- paste("Scenario:", input$scen)
    t
  })
  output$text_method <- renderText({
    t <- "Method: "
    if (input$scen=="Current Proposal and Amendments"){
      t <- paste0(t, input$brexit_props)
    } else if (input$scen=="Simple Brexit Scenarios"){
      t <- paste0(t, input$brexit)
    } else if (input$scen=="Minimising inequality within the Treaty") {
      t <- paste0(t, input$treaty)
    } else if (input$scen=="Treaty change"){
      t <- paste0(t, input$myscenario)
    } else if (input$scen=="Status quo"){
      t <- ""
    }
    t
  })
  output$text_gini <- renderText({
    data <- filteredData()
    paste("Gini:", percent(round(data$gini_scen[1], digits = 3)))
  })
  output$text_mal <- renderText({
    data <- filteredData()
    paste("Malapportionment:", percent(round(data$mal_scen[1], digits = 5)))
  })
  output$text_specs <- renderText({
    data <- filteredData()
    paste("Seats:", round(sum(data$rep_scen), digits = 1), ", smallest: ", round(min(data$rep_scen), digits = 1), ", largest: ", round(max(data$rep_scen), digits = 1))
  })
  output$text_desc <- renderText({
    t <- descScenario()
    t
  })

  # Prepare download table
  output$downloadData <- downloadHandler(
    filename = function() {
        data <- filteredData()
        m <- round(min(data$rep_scen),0)
        M <- round(max(data$rep_scen),0)
        H <- round(sum(data$rep_scen),0)
        paste0("EP_", m, "_", M, "_", H, ".csv")
    },
    content = function(file) {
      data <- filteredData()
      # data[, 2:10] <- data[, 2:10] * data$rep_scen
      if (input$scen=="Status quo"){
        data <- select(
          data, country, pop, pop_share, seats_current = rep, seats_share_current = rep_share_scen,
          pop_seats_current = pop_rep_scen, malapportionment = mal_scen, gini = gini_scen
        )
      } else {
        data <- select(
          data, country, pop, pop_share, seats_current = rep, seats_share_current = rep_share,
          pop_seats_current = pop_rep, seats_scenario = rep_scen, seats_share_scenario = rep_share_scen,
          pop_seats_scenario = pop_rep_scen, diff_seats = diffs_rep_scen, malapportionment = mal_scen,
          gini = gini_scen
        )
      }
      write.csv(data, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )




  # My Scenario
  # reactive()

  # output$downloadData <- downloadHandler(
  #   filename = function() { paste(input$dataset, '.csv', sep='') },
  #   content = function(file) {
  #     write.csv(datasetInput(), file)
  #   }
  # )

  # Scenario descriptions
  descScenario <- reactive({
    t <- ""
    if (input$scen=="Status quo"){
      t <- paste0(t, "Allocation of seats in the EP, 2014 - 2019. The distribution of seats is part of secondary law and determined in the European Council's decision from 28 June 2013 (2013/312/EU)")
    } else if(input$scen == "Current Proposal and Amendments") {
      t <- "Proposal discussed in the European Parliament in January 2018"
    } else if (input$scen == "Simple Brexit Scenarios") {
        if (input$brexit == "Drop 73 MEPs") {
          t <- paste0(t, "The size of the Parliament shrinks by the number of British MEPs.")
        } else if (input$brexit == "Equally distribute 73 MEPs") {
          t <- paste0(t, "73 are distributed equally over the remaining 27 member states while maintaining the upper limit of 96 seats. Each state receives three additional seats, except for Malta, Luxembourg, Cyrpus, Estonia and Latvia, which receive two seats. Germany does not receive additional seats as the upper limit would be exceeded.")
        } else if (input$brexit == "Distribute 73 seats at current proportions") {
          t <- paste0(t, "Seats are distributed following current proportions in the Parliament while maintaining the upper limit of 96 seats. Additional seats are computed using the Hamilton largest-remainder method.")
        } else if (input$brexit == "Distribute 73 seats to increase representativeness") {
          t <- paste0(t, "The vacant seats are step-wise allocated to the most under-represented member state while maintaing the upper limit of 96 seats.")
        }
    } else if (input$scen == "Minimising inequality within the Treaty"){
      if (input$treaty=="Cambridge Compromise (total 736) - minimise malapportionment") {
        t <- paste0(t, "Following the recommendations of the Cambridge Compromise, this method allocates seats based on a fixed and a proportional part. First, every Member State receives 5 seats. Second, the remaining seats are linearly distributed according to population sizes with upwards rounding. A total parliament size of 736 minimises malapportionment")
      }
      if (input$treaty=="Cambridge Compromise (total 637) - minimise Gini") {
        t <- paste0(t, "Following the recommendations of the Cambridge Compromise, this method allocates seats based on a fixed and a proportional part. First, every Member State receives 5 seats. Second, the remaining seats are linearly distributed according to population sizes with upwards rounding. A total parliament size of 637 minimises the Gini coefficient")
      }
    } else if (input$scen == "Treaty change"){
      if (input$myscenario=="Cambridge Compromise (Base + Prop)") {
        t <- paste0(t, "Following the recommendations of the Cambridge Compromise, this method allocates seats based on a fixed and a proportional part. First, every Member State receives 5 seats. Second, the remaining seats are linearly distributed according to population sizes with upwards rounding.")
      } else if (input$myscenario=="Parabolic") {
        t <- paste0(t, "The ‘parabolic’ method produces one of the most degressively proportional allocation of seats. In its 2013 report, the AFCO committee suggests this mode of distribution to be used as a benchmark, although the implied redistribution would be ''too drastic to be politically sustainable in a single step'' (Gualtieri and Trzaskowski, 2013).")
      } else if (input$myscenario=="Limited loss") {
        t <- paste0(t, "Although this is not strictly a method, but rather an ad-hoc criterion for distribution of seats, it can be modelled by following the decision-making process behind the allocation for the 2014-2019 parliamentary cycle. The allocation proceeds in two steps. First, a degressively proportional distribution is drawn up . In the second step, seats are redistributed so that no Member State loses more than one seat.")
      } else if (input$myscenario=="No loss of seats") {
        t <- paste0(t, "Although this is not strictly a method, but rather an ad-hoc criterion for distribution of seats, it can be modelled by following the decision-making process behind the allocation for the 2014-2019 parliamentary cycle. The allocation proceeds in two steps. First, a degressively proportional distribution is drawn up . In the second step, seats are redistributed so that no Member State loses a seat.")
      } else if (input$myscenario=="Transnational list") {
        t <- paste0(t, "The selected number of seats are allocated to a transnational list while keeping the current number of seats per member state. This allocation is approximated by adding a fraction of the seats from the transnational list to member states depending on their population size.")
      } else if (input$myscenario=="Power Compromise") {
      t <- paste0(t, "The ''Power Compromise'' is based on the Cambridge Compromise, but uses weighted population numbers. In a first step population numbers are raised to the power t (0<t<1). In the second step the 'base + prop' formula is applied on the weighted numbers. ")
      }
    } else {
      t <- ""
    }
    t
  })

})





