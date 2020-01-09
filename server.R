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
  
  
  #########################
  # SWITCH TO INTERACTION #
  #########################
  
  # go from loading data to interacting with the map
  hide(id = 'loading-content', anim = TRUE, animType = 'slide')
  shinyjs::show(id = "app-content")
  
  
  
  #############################
  #   CREATE REACTIVE VALUES  #
  #############################

  # selected counties list 
  myselection <- reactiveVal(c())

  
  
  ###################
  #   CREATE DATA   #
  ###################
  
  # update selected rent when selected state is updated
  react_rent <- eventReactive(input$selectState, {
    rent <- rent_all %>% filter(state == input$selectState) %>% mutate(SelecRent = rent50_1)
    return(rent)
  })
  
  # update selected counties when selected state is updated
  react_counties <- eventReactive(input$selectState, {
    # county map (with CountyID as character)
    counties <- counties_all %>% filter(Code == input$selectState)
    return(counties)
  })
  
  # update the selected state map when selected state is updated
  react_state <- eventReactive(input$selectState, {
    # select this state
    state <- states_all[states_all$STATE_ABBR == input$selectState, ]
    return(state)
  })
  
  # when select state is changed, clear the selection
  observeEvent(input$selectState, {
    myselection(c())
  })
  
  
  # CURRENT RENT DATA 
  # - RECALCULATES ON CHANGE OF: score ranges, form choice, or react_rent() 
  currRent_data <- eventReactive(c(input$changeView, input$update, input$selectState), {
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
  
  # viewType (chr): updates whether the map should be current TIV or TIV growth when change view is pressed
  viewType <- eventReactive(input$changeView, {
    input$viewChoice
  }, ignoreNULL = FALSE)
  
  # the time period to be used (updated on changeView)
  timeperiod <- eventReactive(c(input$changeView, input$periodOk), {
    
    # if the dateRange inputs (from modal dialogue) are null, set them to min/max values of PIF
    if(is.null(input$dateRange[1]) & is.null(input$dateRange[2])) {
      
      years <- unique(rent_all$YEAR)
      return(c(years[1], years[length(years)]))
      
    } else { # else get the input values
      return(c(input$dateRange[1], input$dateRange[2]))
    }
    
  })
  
  ###############################
  #  CREATE REACTIVE VARIABLES  #
  ###############################
  
  # getRentLabels (list): the labels for the counties based on current rent 
  getRentLabels <- reactive({
    currCntys <- currCnty_data()
    currCntys <- currCntys[currCntys$bin != "$0",]
    
    # creates label with rent
    labs <- lapply(seq(nrow(currCntys)), function(i) {
      paste0('County: ', currCntys$NAME[i], '<br>', 
             'Rent: ',paste0("$", format( currCntys$SelecRent[i], format = "f", digits = 2, big.mark = ",")))
    })
  })
  
  # getGrowthLabels (list): the labels for the counties based on rent growth
  getGrowthLabels <- reactive({
    currCntys <- currCnty_data()
    currCntys <- currCntys[!(is.na(currCntys$change_bin)), ]
    
    labs <- lapply(seq(nrow(currCntys)), function(i) {
      paste0( 'County: ', currCntys$NAME[i], '<br>',
              'Growth: ', paste0("$", format(currCntys$rent_change[i], format = "f", digits = 2, big.mark = ",")))
    })
  })
  
  
  # histTbl (data.frame) : the table with the rent change over time for the current selection
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
  
  
  # currTabl (data.frame): the table with the most recent rent for the current selection
  currTbl <- reactive({
    
    # filters the history table to just get the rent and rent for the most recent date
    currTbl <- histTbl() %>% 
      filter(Year == max(Year)) %>%
      select(Year, AverageRent) %>%
      mutate(AverageRent = paste0("$", format(as.numeric(AverageRent), format = "f", digits = 2, big.mark = ",")))
    
    return(currTbl)
  })
  
  
  # topTbl (data.frame): the table with the counties ordered from highest to lowest rent
  topTbl <- reactive({
    
    topTbl <- as.data.frame(currCnty_data())
    topTbl <- topTbl %>% arrange(desc(SelecRent)) %>% select(CountyID, Name = NAME, SelectedRent = SelecRent) 
    
    return(topTbl)
  })
  
  
  ################
  #  CREATE MAP  #
  ################
  
  # The main map to be displayed to the user. Contains basemap, labels, state/county outlines.
  output$mymap <- renderLeaflet({
    
    # give Cnty unique ID so can map as Cnty outline 
    CntyOutline <- currCnty_data()
    CntyOutline$CntyOutId <- 1:nrow(CntyOutline)
    
  
    THE_MAP <- leaflet() %>%
      
      #map panes
      addMapPane("basemap", 410) %>%
      addMapPane("polygons", 420) %>%
      addMapPane("labels", 430) %>%
      addMapPane("highlighted", 440) %>%
      
      # basemap
      addProviderTiles(providers$Esri.WorldImagery,
                       c(providerTileOptions(noWrap = TRUE), leafletOptions(pane = "basemap"))) %>%
      
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
      addResetMapButton() 
    
    
    THE_MAP
  })
  

  #######################
  # RENDER TEXT OUTPUTS #
  #######################
  
  output$maxYEAR <- renderText({
    paste("Top counties by Rent as of", currRent_data()[currRent_data()$YEAR == max(currRent_data()$YEAR), ]$YEAR[1])
  })
  
  output$currPeriod <- renderText({
    paste("Selected:", timeperiod()[1], "to", timeperiod()[2])
  })
  
  #################
  # RENDER TABLES #
  #################
  
  # displays histTbl (rent change over time)
  output$historicalData <- renderDT({
    data <- histTbl() 
    table <- datatable(data, options = list(searching = F, lengthMenu = c(5, 10)), selection = "none", rownames = F)
    table <- formatCurrency(table, c("AverageRent", "AverageRentChange"), digits = 0)
  })
  
  # displays currTbl (the most recent rent)
  output$currData <- renderTable({
    currTbl()
  })

  # displays topCntyTbl (the counties with the highest to lowest rent)  
  output$topCntyData <- renderDT({
    table <- datatable(topTbl()[,1:3], rownames = T, selection = "multiple",
                       options = list(pageLength = 10, searching = F, lengthChange = F))
    table <- formatCurrency(table, c("SelectedRent"), digits = 0)
  })
  
  
  ##################
  # OBSERVE EVENTS #
  ##################
  
  
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
        actionButton("periodOk", "OK")
      ), size = "s"
    )
  }
  
  
  # when the modal ok button is pressed close the window
  # Note: the inputs retrieved from the modal will not be used until change basemap is pressed
  observeEvent(input$periodOk, {
    removeModal()
  })
  
  
  # when the county data changes, update the map to display counties accordingly
  observeEvent(currCnty_data(), {
    
    # clear old counties and legends
    THE_MAP <- leafletProxy("mymap") %>%
      clearGroup("Current Rent") %>% removeControl("Current Rent") %>%
      clearGroup("Rent Growth") %>% removeControl("Rent Growth")
    
    # determines how the counties should be displayed based on the current basemap
    if (viewType() == "Current Rent") {
      
      # county data for viewing by current rent
      currCntys <- currCnty_data()
      currCntys <- currCntys[currCntys$bin != "$0",]
      
      # color palette for viewing by current rent 
      colorsRent <- colorFactor(palette = c("#dbdbdb", "#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c"),
                                domain = currCnty_data()$bin)
      
      THE_MAP <- THE_MAP %>%
        # colored counties by rent
        addPolygons(data = currCntys, group = "Current Rent", layerId = ~CountyID, color = "#949494", weight = 1,
                    smoothFactor = 0.5, opacity = 0.5, fillOpacity = 0.7, label = lapply(getRentLabels(), htmltools::HTML),
                    options = leafletOptions(pane = "polygons"), fillColor = ~colorsRent(bin),
                    highlightOptions = highlightOptions(color = "black", weight = 3)) %>%
        addLegend(data = currCntys, position = "bottomleft", pal = colorsRent,
                  values = ~bin, title = "Current Rent", opacity = 0.5, layerId = "Current Rent") 
    } else { # view by rent growth
      
      # data for viewing the counties by rent growth
      growthCntys <- currCnty_data()
      growthCntys <- growthCntys[!is.na(growthCntys$change_bin),]
      
      # color palette for viewing by rent growth
      colorsGrowth <- colorFactor(palette =  c("#477ead", "#dbdbdb", "#fcae91", "#fb6a4a", "#de2d26", "#a50f15"),
                                  domain = currCnty_data()$change_bin, na.color = NULL)
      
      THE_MAP <- THE_MAP %>%
        # colored counties by rent Growth
        addPolygons(data = growthCntys, group = "Rent Growth", layerId = ~CountyID, color = "#949494", weight = 1,
                    smoothFactor = 0.5, opacity = 0.5, fillOpacity = 0.7, label = lapply(getGrowthLabels(), htmltools::HTML),
                    options = leafletOptions(pane = "polygons"), fillColor = ~colorsGrowth(change_bin),
                    highlightOptions = highlightOptions(color = "black", weight = 3)) %>%
        addLegend(data = growthCntys, position = "bottomleft", pal = colorsGrowth,
                  values = ~change_bin, title = "Rent Growth", opacity = 0.5)
    }
    
    THE_MAP
  })
  
  # when the map is clicked add/remove grids selection
  observeEvent(input$mymap_shape_click, {
    print(input$mymap_shape_click$id)
    
    # make sure id isn't null
    if(!is.null(input$mymap_shape_click$id)) {
      
      # if id is in selection remove it
      if (input$mymap_shape_click$id %in% myselection()) {
        myselection(myselection()[myselection() != input$mymap_shape_click$id])
        
      } else { # else add id to selection
        myselection(c(myselection(), input$mymap_shape_click$id))
      }
    }
    
    print(myselection())
  })
  
  

  # when myselection changes or filters are updated, update highlights on map and in top tbl
  observeEvent(c(myselection(), input$updateFilters), {
    
    # clear highlights on map
    proxy <- leafletProxy("mymap") %>% clearGroup("highlighted")
    
    # update topTbl selection
    dataTableProxy("topCntyData") %>% selectRows(which(topTbl()$CountyID %in% myselection()))
    print(paste("which rows:", which(topTbl()$CountyID %in% myselection())))
    
    req(myselection())
    
    # pull out the counties and rent that have an CountyID in selection vector
    mycounties <- currCnty_data()[currCnty_data()$CountyID %in% myselection(),]
    
    # add the counties as white polylines and rent as circles
    proxy %>% addPolylines(data = mycounties, group = "highlighted", stroke = T, weight = 3, opacity = 1, color = "white",
                           options = leafletOptions(pane = "highlighted"))
    
  }, ignoreNULL = FALSE)
  
  
  

  
  
  # see if the top counties tab is selected and if so, match the rows selected with the counties selected manually via map
  # NOTE: this makes sure that you do not lose the selection the first time you click on top counties tab
  observeEvent({identical(input$tableTabs, "Top Counties")}, {
    dataTableProxy('topCntyData') %>% selectRows(which(topTbl()$CountyID %in% myselection()))
  })
  
  
  # when you click on the top counties table, add/remove county to selection and zoom to county if adding
  observeEvent(input$topCntyData_rows_selected, {
  
    topTblSelection <- topTbl()[input$topCntyData_rows_selected, "CountyID"]
    
    # if the rows selected don't match the current displayed selection
    if (!setequal(topTblSelection, myselection())) {
      
      # determine if the selection is an item to add to myselection
      newItem <- topTblSelection[!(topTblSelection %in% myselection())]
      
      # only zoom to grid if new and zoom switch is true
      if (length(newItem) == 1 && input$zoomSwitch) {
        new_county <- currCnty_data()[currCnty_data()$CountyID == newItem, ]
        zoom_coords <- suppressWarnings(st_coordinates(st_centroid(new_county)))
        
        leafletProxy("mymap") %>% setView(zoom_coords[1], zoom_coords[2], zoom = 7)
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