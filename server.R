shinyServer(function(input, output) {
  plotInput1 <- function() {
    #scale
    if (input$scale == 1) {
      scale_value <-TRUE
    } else if (input$scale == 2) {
      scale_value <-FALSE
    } else {
      scale_value <-FALSE
    }
    
    #fancy
    if (input$fancy == 1) {
      fancy_value <-TRUE
    } else if (input$fancy == 2) {
      fancy_value <-FALSE
    } else {
      fancy_value <-FALSE
    }
    
    #col
    if (input$col == 0) {
      col_value <-0
    } else if (input$col == 1) {
      col_value <-1
    } else if (input$col == 2) {
      col_value <-2
    } else if (input$col == 3) {
      col_value <-3
    } else if (input$col == 4) {
      col_value <-4
    } else if (input$col == 5) {
      col_value <-5
    } else if (input$col == 6) {
      col_value <-6
    } else if (input$col == 7) {
      col_value <-7
    } else if (input$col == 8) {
      col_value <-8
    } else if (input$col == 9) {
      col_value <-NULL
    } else {
      col_value <-NULL
    }
    
    #radius
    if (input$radius == 0.750) {
      radius_value <-0.750
    } else if (input$radius == 0.875) {
      radius_value <-0.875
    } else if (input$radius == 1) {
      radius_value <-1
    } else if (input$radius == 1.17) {
      radius_value <-1.17
    } else if (input$radius == 1.33) {
      radius_value <-1.33
    } else if (input$radius == 1.50) {
      radius_value <-1.50
    } else if (input$radius == 1.67) {
      radius_value <-1.67
    } else if (input$radius == 1.83) {
      radius_value <-1.83
    } else if (input$radius == 2.00) {
      radius_value <-2.00
    } else if (input$radius == 2.33) {
      radius_value <-2.33
    } else {
      radius <-1
    }
    
    #legend
    if (input$legend == 0) {
      legend_value <-"topleft"
    } else if (input$legend == 1) {
      legend_value <-"topright"
    } else if (input$legend == 2) {
      legend_value <-"top"
    } else if (input$legend == 3) {
      legend_value <-"bottomleft"
    } else if (input$legend == 4) {
      legend_value <-"bottomright"
    } else if (input$legend == 5) {
      legend_value <-"bottom"
    } else if (input$legend == 6) {
      legend_value <-NULL
    } else {
      legend_value <-"topleft"
    }
    
    #　csvファイルのテーブル読み込み
    inFile <- reactive(read.csv(input$file1$datapath, header=TRUE, sep = ','))
    inFile <-inFile()[]
    
    inFile=data.frame("inFile"= inFile)
    compiled_inFile<-inFile[input$column_start:input$column_end]
    pca <-prcomp(compiled_inFile, scale= scale_value)
    
    pca2d(pca,
          group = inFile[,input$label],
          show.ellipses = TRUE,
          ellipse.ci = input$ellipse_ci,
          show.plane = TRUE,
          fancy = fancy_value,
          bg = input$bg,
          col = col_value,
          radius = radius_value,
          legend = legend_value
    )
  }
  
  plotInput2 <- function() {
    #scale
    if (input$scale_2 == 1) {
      scale_value_2 <-TRUE
    } else if (input$scale_2 == 2) {
      scale_value_2 <-FALSE
    } else {
      scale_value_2 <-FALSE
    }
    #var.axes
    if (input$var_axes == 1) {
      var_axes_value <-TRUE
    } else if (input$var_axes == 2) {
      var_axes_value <-FALSE
    } else {
      var_axes_value <-FALSE
    }
    #varname.abbrev
    if (input$varname_abbrev == 1) {
      varname_abbrev_value <-TRUE
    } else if (input$varname_abbrev == 2) {
      varname_abbrev_value <-FALSE
    } else {
      varname_abbrev_value <-FALSE
    }
    #　csvファイルのテーブル読み込み
    #　csvファイルのテーブル読み込み
    inFile1 <- reactive(read.csv(input$file1$datapath, header=TRUE, sep = ','))
    inFile1 <-inFile1()[]
    inFile1=data.frame("inFile1"= inFile1)
    inFile1<-inFile1[input$column_start:input$column_end]
    inFile1[input$column_start1:input$column_end1,] <- inFile1[input$column_start1:input$column_end1,] + 1
    inFile1[input$column_start2:input$column_end2,] <- inFile1[input$column_start2:input$column_end2,] + 2
    inFile1[input$column_start3:input$column_end3,] <- inFile1[input$column_start3:input$column_end3,] + 3
    inFile1[input$column_start4:input$column_end4,] <- inFile1[input$column_start4:input$column_end4,,] + 4
    
    component_number2 <- input$column_end2 - input$column_end1
    component_number3 <- input$column_end3 - input$column_end2
    component_number4 <- input$column_end4 - input$column_end3
    
    inFile1<- cbind(c(rep(input$component1, input$column_end1),
                      rep(input$component2, component_number2),
                      rep(input$component3, component_number3),
                      rep(input$component4, component_number4)),
                    inFile1)
    #colnames(inFile1) <- c("types","L1","L2","L3","L4","L1/L2","L1/L4" )
    pca2 <- prcomp(inFile1[, input$column_start:input$column_end], scale = scale_value_2)
    
    ggbiplot(pcobj = pca2, choices = 1:2, obs.scale = 0.5, var.scale = 2,
             groups = inFile1[, input$label], ellipse = TRUE, ellipse.prob = input$ellipse_prob, alpha = input$alpha,
             labels.size = 3, var.axes = var_axes_value, varname.size = input$varname_size, varname.adjust = input$varname_adjust,
             varname.abbrev = varname_abbrev_value) +
      scale_colour_manual(values = c(input$color1, input$color2, input$color3, input$color4)) +
      theme(legend.direction = "horizontal", legend.position = "top")
  }
  
  output$plot2d_1 <- renderPlot ({
    plotInput1()
  })
  
  output$downloadPlot = downloadHandler(
    filename = "pca.png",
    content = function(file) {
      png(file, width = 1000)
      print(plotInput1())
      dev.off()
    },
    contentType = "image/png",
  )
  #######################################################################
  output$summary_1 <- renderPrint({
    #scale
    if (input$scale == 1) {
      scale_value <-TRUE
    } else if (input$scale == 2) {
      scale_value <-FALSE
    } else {
      scale_value <-FALSE
    }
    
    #fancy
    if (input$fancy == 1) {
      fancy_value <-TRUE
    } else if (input$fancy == 2) {
      fancy_value <-FALSE
    } else {
      fancy_value <-FALSE
    }
    
    #col
    if (input$col == 0) {
      col_value <-0
    } else if (input$col == 1) {
      col_value <-1
    } else if (input$col == 2) {
      col_value <-2
    } else if (input$col == 3) {
      col_value <-3
    } else if (input$col == 4) {
      col_value <-4
    } else if (input$col == 5) {
      col_value <-5
    } else if (input$col == 6) {
      col_value <-6
    } else if (input$col == 7) {
      col_value <-7
    } else if (input$col == 8) {
      col_value <-8
    } else if (input$col == 9) {
      col_value <-NULL
    } else {
      col_value <-NULL
    }
    
    #radius
    if (input$radius == 0.750) {
      radius_value <-0.750
    } else if (input$radius == 0.875) {
      radius_value <-0.875
    } else if (input$radius == 1) {
      radius_value <-1
    } else if (input$radius == 1.17) {
      radius_value <-1.17
    } else if (input$radius == 1.33) {
      radius_value <-1.33
    } else if (input$radius == 1.50) {
      radius_value <-1.50
    } else if (input$radius == 1.67) {
      radius_value <-1.67
    } else if (input$radius == 1.83) {
      radius_value <-1.83
    } else if (input$radius == 2.00) {
      radius_value <-2.00
    } else if (input$radius == 2.33) {
      radius_value <-2.33
    } else {
      radius <-1
    }
    
    #legend
    if (input$legend == 0) {
      legend_value <-"topleft"
    } else if (input$legend == 1) {
      legend_value <-"topright"
    } else if (input$legend == 2) {
      legend_value <-"top"
    } else if (input$legend == 3) {
      legend_value <-"bottomleft"
    } else if (input$legend == 4) {
      legend_value <-"bottomright"
    } else if (input$legend == 5) {
      legend_value <-"bottom"
    } else if (input$legend == 6) {
      legend_value <-NULL
    } else {
      legend_value <-"topleft"
    }
    
    #　csvファイルのテーブル読み込み
    inFile <- reactive(read.csv(input$file1$datapath, header=TRUE, sep = ','))
    inFile <-inFile()[]
    
    inFile=data.frame("inFile"= inFile)
    compiled_inFile<-inFile[input$column_start:input$column_end]
    pca <-prcomp(compiled_inFile, scale= scale_value)
    summary(pca)
  })
  output$round_1 <- renderPrint({
    #scale
    if (input$scale == 1) {
      scale_value <-TRUE
    } else if (input$scale == 2) {
      scale_value <-FALSE
    } else {
      scale_value <-FALSE
    }
    
    #fancy
    if (input$fancy == 1) {
      fancy_value <-TRUE
    } else if (input$fancy == 2) {
      fancy_value <-FALSE
    } else {
      fancy_value <-FALSE
    }
    
    #col
    if (input$col == 0) {
      col_value <-0
    } else if (input$col == 1) {
      col_value <-1
    } else if (input$col == 2) {
      col_value <-2
    } else if (input$col == 3) {
      col_value <-3
    } else if (input$col == 4) {
      col_value <-4
    } else if (input$col == 5) {
      col_value <-5
    } else if (input$col == 6) {
      col_value <-6
    } else if (input$col == 7) {
      col_value <-7
    } else if (input$col == 8) {
      col_value <-8
    } else if (input$col == 9) {
      col_value <-NULL
    } else {
      col_value <-NULL
    }
    
    #radius
    if (input$radius == 0.750) {
      radius_value <-0.750
    } else if (input$radius == 0.875) {
      radius_value <-0.875
    } else if (input$radius == 1) {
      radius_value <-1
    } else if (input$radius == 1.17) {
      radius_value <-1.17
    } else if (input$radius == 1.33) {
      radius_value <-1.33
    } else if (input$radius == 1.50) {
      radius_value <-1.50
    } else if (input$radius == 1.67) {
      radius_value <-1.67
    } else if (input$radius == 1.83) {
      radius_value <-1.83
    } else if (input$radius == 2.00) {
      radius_value <-2.00
    } else if (input$radius == 2.33) {
      radius_value <-2.33
    } else {
      radius <-1
    }
    
    #legend
    if (input$legend == 0) {
      legend_value <-"topleft"
    } else if (input$legend == 1) {
      legend_value <-"topright"
    } else if (input$legend == 2) {
      legend_value <-"top"
    } else if (input$legend == 3) {
      legend_value <-"bottomleft"
    } else if (input$legend == 4) {
      legend_value <-"bottomright"
    } else if (input$legend == 5) {
      legend_value <-"bottom"
    } else if (input$legend == 6) {
      legend_value <-NULL
    } else {
      legend_value <-"topleft"
    }
    
    #　csvファイルのテーブル読み込み
    inFile <- reactive(read.csv(input$file1$datapath, header=TRUE, sep = ','))
    inFile <-inFile()[]
    
    inFile=data.frame("inFile"= inFile)
    compiled_inFile<-inFile[input$column_start:input$column_end]
    pca <-prcomp(compiled_inFile, scale= scale_value)
    round(pca$rotation, 4)
  })
  ##################################################################
  output$plot2d_2 <- renderPlot ({
    plotInput2()
  })
  
  output$downloadPlot2 = downloadHandler(
    filename = "pca2.png",
    content = function(file) {
      png(file, width = 1000)
      print(plotInput2())
      dev.off()
    },
    contentType = "image/png",
  )

  output$summary_2 <- renderPrint({
    #scale
    if (input$scale_2 == 1) {
      scale_value_2 <-TRUE
    } else if (input$scale_2 == 2) {
      scale_value_2 <-FALSE
    } else {
      scale_value_2 <-FALSE
    }
    #var.axes
    if (input$var_axes == 1) {
      var_axes_value <-TRUE
    } else if (input$var_axes == 2) {
      var_axes_value <-FALSE
    } else {
      var_axes_value <-FALSE
    }
    #varname.abbrev
    if (input$varname_abbrev == 1) {
      varname_abbrev_value <-TRUE
    } else if (input$varname_abbrev == 2) {
      varname_abbrev_value <-FALSE
    } else {
      varname_abbrev_value <-FALSE
    }
    #　csvファイルのテーブル読み込み
    #　csvファイルのテーブル読み込み
    inFile1 <- reactive(read.csv(input$file1$datapath, header=TRUE, sep = ','))
    inFile1 <-inFile1()[]
    inFile1=data.frame("inFile1"= inFile1)
    inFile1<-inFile1[input$column_start:input$column_end]
    inFile1[input$column_start1:input$column_end1,] <- inFile1[input$column_start1:input$column_end1,] + 1
    inFile1[input$column_start2:input$column_end2,] <- inFile1[input$column_start2:input$column_end2,] + 2
    inFile1[input$column_start3:input$column_end3,] <- inFile1[input$column_start3:input$column_end3,] + 3
    inFile1[input$column_start4:input$column_end4,] <- inFile1[input$column_start4:input$column_end4,,] + 4
    
    component_number2 <- input$column_end2 - input$column_end1
    component_number3 <- input$column_end3 - input$column_end2
    component_number4 <- input$column_end4 - input$column_end3
    
    inFile1<- cbind(c(rep(input$component1, input$column_end1),
                      rep(input$component2, component_number2),
                      rep(input$component3, component_number3),
                      rep(input$component4, component_number4)),
                    inFile1)
    
    pca2 <- prcomp(inFile1[, input$column_start:input$column_end], scale = scale_value_2)
    summary(pca2)
  })
  
  output$round_2 <- renderPrint({
    #scale
    if (input$scale_2 == 1) {
      scale_value_2 <-TRUE
    } else if (input$scale_2 == 2) {
      scale_value_2 <-FALSE
    } else {
      scale_value_2 <-FALSE
    }
    #var.axes
    if (input$var_axes == 1) {
      var_axes_value <-TRUE
    } else if (input$var_axes == 2) {
      var_axes_value <-FALSE
    } else {
      var_axes_value <-FALSE
    }
    #varname.abbrev
    if (input$varname_abbrev == 1) {
      varname_abbrev_value <-TRUE
    } else if (input$varname_abbrev == 2) {
      varname_abbrev_value <-FALSE
    } else {
      varname_abbrev_value <-FALSE
    }
    #　csvファイルのテーブル読み込み
    #　csvファイルのテーブル読み込み
    inFile1 <- reactive(read.csv(input$file1$datapath, header=TRUE, sep = ','))
    inFile1 <-inFile1()[]
    inFile1=data.frame("inFile1"= inFile1)
    inFile1<-inFile1[input$column_start:input$column_end]
    inFile1[input$column_start1:input$column_end1,] <- inFile1[input$column_start1:input$column_end1,] + 1
    inFile1[input$column_start2:input$column_end2,] <- inFile1[input$column_start2:input$column_end2,] + 2
    inFile1[input$column_start3:input$column_end3,] <- inFile1[input$column_start3:input$column_end3,] + 3
    inFile1[input$column_start4:input$column_end4,] <- inFile1[input$column_start4:input$column_end4,,] + 4
    
    component_number2 <- input$column_end2 - input$column_end1
    component_number3 <- input$column_end3 - input$column_end2
    component_number4 <- input$column_end4 - input$column_end3
    
    inFile1<- cbind(c(rep(input$component1, input$column_end1),
                      rep(input$component2, component_number2),
                      rep(input$component3, component_number3),
                      rep(input$component4, component_number4)),
                    inFile1)

    pca2 <- prcomp(inFile1[, input$column_start:input$column_end], scale = scale_value_2)
    round(pca2$rotation, 4)
  })
})