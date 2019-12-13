################################
#                              #
#   MEDIAN RENT VIEWER SERVER  #         
#                              #
################################

################
# HOUSEKEEPING #
################

suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(htmltools))
suppressPackageStartupMessages(library(shinyjs))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(leaflet))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(shinyWidgets))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(shinythemes))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(leaflet.extras))


server <- function(input, output, session) {
  
  options(stringsAsFactors = F, scipen = 9999)
  
  ################
  # READ IN DATA #
  ################
  
  # state boundary map (projected to 4326)
  states_all <- st_read("data/state boundary/WesternStates_4326.shp", quiet = T)
  
  # all rent data 
  rent_files <- list.files("data/rent", pattern = ".csv")
  
  rent_list <- list()
  for (i in 1:length(rent_files)) {
    
    print(paste0("Reading in rent file ", i, " of ", length(rent_files), "..."))
    curr_rent <- fread(paste0("data/rent/", rent_files[i]))
    curr_rent$YEAR <- as.integer(substr(rent_files[i], start = 3, stop = 6))
    rent_list[[i]] <- curr_rent
    
  } # end for
  
  rent_all <- do.call("rbind", rent_list)  
  colnames(rent_all)[colnames(rent_all)=="state_alpha"] <- "state"
  rent_all$CountyID <- as.character(rent_all$CountyID)
  
  # counties (projected to 4326)
  print("Reading in county data")
  counties_all <- readRDS("data/county/counties_western_4326.RDS")
  counties_all$CountyID <- as.character(counties_all$CountyID)
  
  
  #############################
  #   CREATE REACTIVE VALUES  #
  #############################
  
  # reactive variables for spatial data
  react_rent <- reactiveVal()
  react_counties <- reactiveVal()
  react_state <- reactiveVal()
  
  # selected state code (intialized with CA)
  state_code <- reactiveVal("CA")
  
  # selected counties list 
  myselection <- reactiveVal(c())
  
  # map groups 
  basemap <-reactiveVal("Current Rent")
  
  # min and max values of time period
  years <- unique(rent_all$YEAR)
  timeperiod <- reactiveVal(c(years[1], years[length(years)]))
  
  
  ###################
  #   CREATE DATA   #
  ###################
  
  # updates the spatial data when state code is updated
  observeEvent(state_code(), {
    print(paste("Reading data for", state_code()))
    
    rent <- rent_all %>% filter(state == state_code()) %>% mutate(SelecRent = rent50_1)
    react_rent(rent)
    
    # county map (with CountyID as character)
    counties <- counties_all %>% filter(Code == state_code())
    react_counties(counties)
    
    # select this state
    state <- states_all[states_all$STATE_ABBR == state_code(), ]
    react_state(state)
    
  })
  
  # Calculates the current rent and current county values when the update button is pressed, the
  # change view button is pressed, or the state selected is updated. The argument
  # ignoreNull allows them to be intialized when app start up.
  # NOTE: this observeEvent is included here to initialize data that reactive values rely on
  observeEvent(c(input$update, input$changeView, state_code()), {
    
    hideElement(id = "mymap")
    
    # clear any existing selection
    myselection(c())
    
    showElement(id = "mymap")
    
  }, ignoreNULL = F)
  
  
  # CURRENT RENT DATA 
  # - RECALCULATES ON CHANGE OF: score ranges, form choice, or react_rent() 
  currRent_data <- eventReactive(c(input$changeView, input$update, state_code()), {
    rent <- react_rent()
    
    # filters the rent by the score range 
    rent$SelecRent <- rent[, input$numRooms]
    currRent <- rent %>% filter(SelecRent >= input$rentScoreRange[1] & SelecRent <= input$rentScoreRange[2])
    
    return(currRent)
  })
  
  
  # CURRENT COUNTY DATA
  # - RECALCULATES ON CHANGE OF: currrent_data 
  currCnty_data <- eventReactive(currRent_data(), {
    
    currRent <- currRent_data()
    
    # adds the rent and associated rent bin for each Cnty based on the filters 
    bins_rent <- currRent %>% filter(YEAR == max(YEAR)) 
    cntys_with_bin <- left_join(react_counties(), bins_rent, by = "CountyID")
    cntys_with_bin <- cntys_with_bin  %>% 
      mutate(bin = cut(cntys_with_bin$SelecRent,
                       breaks = c(-Inf, 0, 250, 500, 1000, 2000, Inf),
                       labels = c("$0", "$0.01 - $250", "$250 - $500",
                                  "$500 - $1000", "$1000 - $2000", "> $2000"),
                       right = T))
    
    # makes sure all NA values are assigned appropriate value
    cntys_with_bin[is.na(cntys_with_bin$SelecRent), "SelecRent"] <- 0
    cntys_with_bin[is.na(cntys_with_bin$bin), "bin"] <- "$0"
    
    initial_rent <- currRent %>% filter(YEAR == timeperiod()[1]) %>% mutate(iRent = SelecRent)
    final_rent <- currRent %>% filter(YEAR == timeperiod()[2]) %>% mutate(fRent = SelecRent)
    second_bin <- left_join(final_rent[, c("fRent", "CountyID")], initial_rent[, c("iRent", "CountyID")], by = "CountyID")
    second_bin$rent_change <- second_bin$fRent - second_bin$iRent
    second_bin <- second_bin  %>% 
      mutate(change_bin = cut(second_bin$rent_change,
                              breaks = c(-Inf, 0, 25, 50, 100, 150, Inf),
                              labels = c("< $0", "$0.1 - $25", "$25 - $50",
                                         "$50 - $100", "$100 - $150", "> $150"),
                              right = T))
    cntys_with_bin <- left_join(cntys_with_bin, second_bin, by = "CountyID")
    
    return(st_as_sf(cntys_with_bin, crs = 4326))
  })
  
  
  ###############################
  #  CREATE REACTIVE VARIABLES  #
  ###############################
  
  # labels for each county on the map when being viewed by current rent
  # - RECALCULATES ON CHANGE OF: currCnty_data
  getRentLabels <- reactive({
    currCntys <- currCnty_data()
    currCntys <- currCntys[currCntys$bin != "$0",]
    
    # creates label with rent
    labs <- lapply(seq(nrow(currCntys)), function(i) {
      paste0('County: ', currCntys$NAME[i], '<br>', 
             'Rent: ',paste0("$", format( currCntys$SelecRent[i], format = "f", digits = 2, big.mark = ",")))
    })
  })
  
  # labels for each Cnty on the map when being viewed by rent growth
  # - RECALCULATES ON CHANGE OF: currCnty_data
  getGrowthLabels <- reactive({
    currCntys <- currCnty_data()
    currCntys <- currCntys[!(is.na(currCntys$change_bin)), ]
    
    labs <- lapply(seq(nrow(currCntys)), function(i) {
      paste0( 'County: ', currCntys$NAME[i], '<br>',
              'Growth: ', paste0("$", format(currCntys$rent_change[i], format = "f", digits = 2, big.mark = ",")))
    })
  })
  
  
  
  
  # updates the historical table 
  # - RECALCULATES ON CHANGE OF: currRent_data
  histTbl <- reactive({
    
    # filters the rent data to only get rent in selection
    histTbl <- currRent_data() %>% filter(CountyID %in% myselection())
    
    # finds the rent, TIV, and changes for each YEAR    
    histTbl <- histTbl %>%
      group_by(YEAR) %>%
      summarise(AvgRent = mean(as.numeric(SelecRent))) %>%
      mutate(AvgRent_Change = AvgRent - lag(AvgRent)) %>%
      arrange(desc(YEAR)) %>%
      select(Year = YEAR, AverageRent = AvgRent, AverageRentChange = AvgRent_Change)
    
    return(histTbl)
  })
  
  
  # updates the most recent table
  # - RECALCULATES ON CHANGE OF: histTbl
  currTbl <- reactive({
    
    # filters the history table to just get the rent and rent for the most recent date
    currTbl <- histTbl() %>% 
      filter(Year == max(Year)) %>%
      select(Year, AverageRent) %>%
      mutate(AverageRent = paste0("$", format(as.numeric(AverageRent), format = "f", digits = 2, big.mark = ",")))
    
    
    return(currTbl)
  })
  
  
  # updates the top counties table
  # - RECALCULATES ON CHANGE OF: currCnty_data
  topTbl <- reactive({
    
    # gets the top 50 TIV counties based on the current filters
    topTbl <- as.data.frame(currCnty_data())
    topTbl <- topTbl %>% arrange(desc(SelecRent)) %>% select(CountyID, Name = NAME, SelectedRent = SelecRent) 
    
    # adds column with row names to make selections later easier
    topTbl$rowNames <- 1:nrow(topTbl)
    
    return(topTbl)
  })
  
  
  ################
  #  CREATE MAP  #
  ################
  
  # The main map to be displayed to the user. Contains Cnty, historic perimeters, moratorium areas, state border.
  # - RECALCULATES ON CHANGE OF: currCnty_data
  
  output$mymap <- renderLeaflet({
    
    # gets the current Cnty data (contains bin information for coloring)
    currCntys <- currCnty_data()
    currCntys$currID <- 1:nrow(currCntys)
    growthCntys <- currCnty_data()
    growthCntys$growthID <- 1:nrow(growthCntys)
    
    # filter out zero or NA values so those counties can be "transparent" on map
    currCntys <- currCntys[currCntys$bin != "$0",]
    growthCntys <- growthCntys[!is.na(growthCntys$change_bin),]
    
    # give Cnty unique ID so can map as Cnty outline 
    CntyOutline <- currCnty_data()
    CntyOutline$CntyOutId <- 1:nrow(CntyOutline)
    
    # creates the color palette for the Cntys 
    colorsRent <- colorFactor(palette = c("#dbdbdb", "#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c"),
                              domain = currCnty_data()$bin)
    colorsGrowth <- colorFactor(palette =  c("#477ead", "#dbdbdb", "#fcae91", "#fb6a4a", "#de2d26", "#a50f15"),
                                domain = currCnty_data()$change_bin, na.color = NULL)
    
    Rentlabs <- getRentLabels()
    Growthlabs <- getGrowthLabels()
    
    THE_MAP <- leaflet() %>%
      
      #map panes
      addMapPane("basemap", 410) %>%
      addMapPane("polygons", 420) %>%
      addMapPane("labels", 430) %>%
      addMapPane("highlighted", 440) %>%
      
      # basemap
      addProviderTiles(providers$Esri.WorldImagery,
                       c(providerTileOptions(noWrap = TRUE), leafletOptions(pane = "basemap"))) %>%
      
      # colored counties by rent
      addPolygons(data = currCntys, group = "Current Rent", layerId = ~currID, color = "#949494", weight = 1,
                  smoothFactor = 0.5, opacity = 0.5, fillOpacity = 0.7, label = lapply(Rentlabs, htmltools::HTML),
                  options = leafletOptions(pane = "polygons"), fillColor = ~colorsRent(bin),
                  highlightOptions = highlightOptions(color = "black", weight = 3)) %>%
      
      # colored counties by rent Growth
      addPolygons(data = growthCntys, group = "Rent Growth", layerId = ~growthID, color = "#949494", weight = 1,
                  smoothFactor = 0.5, opacity = 0.5, fillOpacity = 0.7, label = lapply(Growthlabs, htmltools::HTML),
                  options = leafletOptions(pane = "polygons"), fillColor = ~colorsGrowth(change_bin),
                  highlightOptions = highlightOptions(color = "black", weight = 3)) %>%
      
      # county outlines
      addPolygons(data = CntyOutline, layerId = ~CountyID, color = "#949494", weight = 1.3,
                  smoothFactor = 0.5, opacity = 0.8, fillOpacity = 0.0, 
                  options = c(leafletOptions(pane = "basemap"), pathOptions(clickable = F))) %>%
      
      # state border
      addPolylines(data = react_state(), group = "border", color = "black", opacity = 1.0, weight = 1.3, 
                   options = leafletOptions(pane = "polygons")) %>%
      
      # map labels
      addProviderTiles(providers$CartoDB.PositronOnlyLabels,
                       options = c(providerTileOptions(noWrap = TRUE), leafletOptions(pane = "labels")),
                       group = "labels") %>%
      
      # button for returning to initial zoom
      addResetMapButton()  %>%
      addLegend(position = "bottomleft", labels = c(""), colors = c("white"), opacity = 0.1)
    
    
    # determines how counties should be displayed based on the current basemap
    if (identical(basemap(), "Current Rent")) {
      THE_MAP <- THE_MAP %>%
        addLegend(data = currCntys, position = "bottomleft", pal = colorsRent,
                  values = ~bin, title = "Current Rent", opacity = 0.5) %>%
        hideGroup("Rent Growth")
    } else {
      THE_MAP <- THE_MAP %>%
        addLegend(data = currCntys, position = "bottomleft", pal = colorsGrowth,
                  values = ~change_bin, title = "Rent Growth", opacity = 0.5) %>%
        hideGroup("Current Rent")
    }
    
    
    THE_MAP
  })
  
  
  #########################
  # SWITCH TO INTERACTION #
  #########################
  
  # go from loading data to interacting with the map
  hide(id = 'loading-content', anim = TRUE, animType = 'slide')
  shinyjs::show(id = "app-content")
  
  output$maxYEAR <- renderText({
    paste("Top counties by Rent as of", currRent_data()[currRent_data()$YEAR == max(currRent_data()$YEAR), ]$YEAR[1])
  })
  
  output$currPeriod <- renderText({
    paste("Selected:", timeperiod()[1], "to", timeperiod()[2])
  })
  
  #################
  # RENDER TABLES #
  #################
  
  # These tables will update on changes to when current county data, the rent data, or the selection 
  
  # the table containing data over time
  output$historicalData <- renderDT({
    data <- histTbl() 
    table <- datatable(data, options = list(searching = F, lengthMenu = c(5, 10)), selection = "none", rownames = F)
    table <- formatCurrency(table, c("AverageRent", "AverageRentChange"), digits = 0)
  })
  
  # the table containing the rent for the most recent year
  output$mydata <- renderTable({
    currTbl()
  })
  
  # the table containing the top counties with the highest rent according to the current filters
  output$topCntyData <- renderDT({
    table <- datatable(topTbl()[,1:3], rownames = T, selection = "multiple",
                       options = list(pageLength = 10, searching = F, lengthChange = F))
    table <- formatCurrency(table, c("SelectedRent"), digits = 0)
  })
  
  
  ##################
  # OBSERVE EVENTS #
  ##################
  
  # shows the select time period button only if rent growth is selected
  observeEvent(input$viewChoice, {
    if(input$viewChoice == "Rent Growth") {
      show(id = "selectPeriod")
      show(id = "currPeriod")
    } else {
      hide(id = "selectPeriod")
      hide(id = "currPeriod")
    }
  })
  
  
  # when the select period button is clicked call the popup window
  observeEvent(input$selectPeriod, {
    showModal(modalPeriod())
  })
  
  
  # the pop up window when the choose time period button is clicked
  modalPeriod <- function() {
    years <- unique(rent_all$YEAR)
    modalDialog(
      h4(strong("Select period to view growth:")),
      br(),
      sliderTextInput(inputId = "dateRange", label = NULL,
                      choices = years, grid = TRUE,
                      selected = c(timeperiod()[1], timeperiod()[2])),
      br(), 
      footer = tagList(
        modalButton("Cancel"),
        actionButton("modalOk", "OK")
      ), size = "s"
    )
  }
  
  
  # when the modal ok button is pressed close the window
  # Note: the inputs retrieved from the modal will not be used until change basemap is pressed
  observeEvent(input$modalOk, {
    removeModal()
    if(!is.null(input$dateRange[1]) & !is.null(input$dateRange[2])) {
      timeperiod(c(as.integer(input$dateRange[1]), as.integer(input$dateRange[2])))
    }
  })
  
  
  # if the basemap has actually changed, change it and clear selection
  observeEvent(input$changeView, {
    if(basemap() != input$viewChoice) {
      basemap(input$viewChoice)
      myselection(c())
    }
  })
  
  # when the state selected changes
  observeEvent(input$selectState, {
    
    shinyjs::show(id = 'loading-content')
    
    state_code(input$selectState)
    
    hide(id = 'loading-content', anim = TRUE, animType = 'slide')
    shinyjs::show(id = 'app-content')
    
  }, ignoreInit = TRUE)
  
  
  
  # highlights the counties currently selected on the map and in top grids table
  observeEvent(c(myselection(), input$updateFilters), {
    
    # clear highlights on map
    proxy <- leafletProxy("mymap") %>% clearGroup("highlighted")
    
    # update topTbl selection
    dataTableProxy("topCntyData") %>% selectRows(which(topTbl()$CountyID %in% myselection()))
    
    req(myselection())
    
    # pull out the counties and rent that have an CountyID in selection vector
    mycounties <- react_counties()[react_counties()$CountyID %in% myselection(),]
    
    # add the counties as white polylines and rent as circles
    proxy %>% addPolylines(data = mycounties, group = "highlighted", stroke = T, weight = 3, opacity = 1, color = "white",
                           options = leafletOptions(pane = "highlighted"))
    
  })
  
  
  
  # when the map is clicked add/remove counties from myselection
  observeEvent(input$mymap_shape_click, {
    
    # make sure id isn't null
    if(!is.null(input$mymap_shape_clicked$id)) {
      
      # if id is in selection, remove it
      if (input$mymap_shape_clicked$id %in% myselection()) {
        myselection(myselection()[myselection() != input$mymap_shape_click$id])
        
      } else { # add id to selection
        myselection(c(myselection(), input$mymap_shape_click$id))
      }
    }
    
  })
  
  
  # see if the top counties tab is selected and if so, match the rows selected with the counties selected manually via map
  # NOTE: this makes sure that you do not lose the selection the first time you click on top counties tab
  observeEvent({identical(input$tableTabs, "Top Counties")}, {
    dataTableProxy('topCntyData') %>% selectRows(topTbl()[topTbl()$CountyID %in% myselection(), "rowNames"])
  })
  
  
  # applies selections from top counties table to map
  observeEvent(input$topCntyData_rows_selected, {
  
    topTblSelection <- topTbl()[input$topCntyData_rows_selected, "CountyID"]
    
    # if the rows selected don't match the current displayed selection
    if (!setequal(topTblSelection, myselection())) {
      
      # determine if the selection is an item to add to myselection
      newItem <- topTblSelection[!(topTblSelection %in% myselection())]
      
      # only zoom to grid if new and zoom switch is true
      if (length(newItem) == 1 && input$zoomSwitch) {
        new_county <- state_data$grid[state_data$grid$CountyID == newItem, ]
        zoom_coords <- suppressWarnings(st_coordinates(st_centroid(new_county)))
        
        leafletProxy("mymap") %>% setView(zoom_coords[1], zoom_coords[2], zoom = 11)
      }
      
      # set myselection to the rows currently selected in table
      myselection(topTblSelection)
    }
    
  }, ignoreNULL = F)
  
  
  # when the clear selection button is pressed
  observeEvent(input$clearSelection, {
    myselection(c())
  })
  
  
  # downloads the selected rent to user's downloads folder 
  output$downloadData <- downloadHandler(
    
    # creates the string that will be the file name
    filename = function() {
      return(paste0("MedianRent", "_", input$selectState, "rooms.csv"))
    },
    
    # exports the file to the users download folder
    content = function(file) {
      data <- currRent_data() %>% filter(YEAR == max(YEAR))
      data <- data[data$CountyID %in% myselection()]
      write.csv(data, file, row.names = F)
    }
  )
  
  
  # ends the app when the tab is closed
  session$onSessionEnded(function(){
    stopApp()
  })
  
}