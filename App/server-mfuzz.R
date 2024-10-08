#server-mfuzz.R

MFEnrichRun <- reactiveValues(MFEnrichRunValue = FALSE) # to precise the run button has not been clicked

observeEvent(input$mfuzzCountData, {   # when a table is being uploaded 
  tryCatch({
    var$mfuzzTable <-    # assign the table as a data frame to this variable 
      data.frame(fread(input$mfuzzCountData$datapath), row.names = 1)
    v$importActionValue = FALSE # precise no button has been clicked yet
  },
  error = function(e) {  # error messages about the input table 
    sendSweetAlert(
      session = session,
      title = "Input data error!",
      text = as.character(message(e)),
      type = "error"
    )
    return()
  },
  warning = function(w) {
    sendSweetAlert(
      session = session,
      title = "Input data warning!",
      text = "Error in dataset",
      type = "warning"
    )
    return()
  })
  
  var$mfuzzTable <- var$mfuzzTable[rowSums(var$mfuzzTable >= 1) > 0 , ]
  var$timepoints <- as.vector(colnames(var$mfuzzTable))
  
  output$inertia_plot <-renderPlotly({
    hierdata <- as.matrix((var$mfuzzTable))
    # Euclidean distance
    dist <- dist(hierdata, diag=TRUE)
    # Hierarchical Clustering with hclust
    hc <- hclust(dist)
    inertia <- sort(hc$height, decreasing = TRUE)
    max <- as.numeric(input$maxclass)
    inertia <- inertia[1:max]
    az <- c(1:max)
    df <- as.data.frame(inertia, row.names = c(1:max))
    df["class"] <- az
    fig <- plot_ly(
      df,
      x = ~az,
      y = ~inertia,
      type = "scatter",
      mode = "markers"
      
    )%>% add_lines(y = df$az, line = list(shape = "vh"))
    fig <- fig %>% layout(
      title = "Inertia drops",
      xaxis = list(title = "Clusters"),
      yaxis = list(title = "Inertia"),
      showlegend = F
    )
  })
  



observeEvent(input$inertiaclass,{   # when a filter of low count genes is set 
  if (input$inertiaclass != 0) {
    mfmat <- as.matrix((var$mfuzzTable))
    mfmat <- DGEList(counts = mfmat, group=colnames(var$mfuzzTable))
    mfmat <- calcNormFactors(mfmat)
    mfcpm <- cpm(mfmat, normalized.lib.size=TRUE)
    
    
    timepoint <- colnames(var$mfuzzTable)
    test_data <- rbind(timepoint, mfcpm)
    row.names(test_data)[1]<-"time"
    
    #save it to a temp file so ti doesn't clutter up the blog directory
    tmp <- tempfile()
    write.table(test_data,file=tmp, sep='\t', col.names=NA)
    #read it back in as an expression set
    
    mfdata <- table2eset(file=tmp)
    mfdata.s <- standardise(mfdata)
    m1 <- mestimate(mfdata.s)
    cent <- input$inertiaclass
    i=0
    for (i in 0:9){
      N_cl<- mfuzz(mfdata.s, centers=cent, m = m1)
      i = i + 1
    }
    ov <- overlap(N_cl)
    
  }
  
  output$elbow_plot <- renderPlot({
    data.s <- as.matrix(mfdata.s)
    scaledata <- t(scale(t(data.s))) # Centers and scales data.
    scaledata <- scaledata[complete.cases(scaledata),]
    
    
    #helper function for the within sum of squared error
    sumsqr <- function(x, clusters){
      sumsqr <- function(x) sum(scale(x, scale = FALSE)^2)
      wss <- sapply(split(as.data.frame(x), clusters), sumsqr)
      return(wss)
    }
    
    #get the wss for repeated clustering
    iterate_fcm_WSS <- function(df,m){
      totss <- numeric()
      for (i in 2:20){
        FCMresults <- cmeans(df,centers=i,m=m)
        totss[i] <- sum(sumsqr(df,FCMresults$cluster))
      }
      return(totss)
    }
    wss_2to20 <- iterate_fcm_WSS(scaledata,m1)
    max <- as.numeric(input$maxclass)
    elb <- plot(1:max, wss_2to20[1:max], type="b", xlab="Number of Clusters", ylab="WSS")
    elb
  })
  
  output$overlap_plot <- renderPlot({
    Ptmp<- overlap.plot(N_cl,over=ov, thres=as.numeric(input$ov_threshold))
    Ptmp
  })
  
  

  output$mfuzz_plots <- renderPlot({
    
    if(input$inertiaclass < 10){
      mfrow <- c(4,3)
      var$heightplot <- 1200
    }else{
      mfrow <- c(10,3)
      var$heightplot <- 3600
    }
    fuzz <- mfuzz.plot(mfdata.s,cl=N_cl,mfrow = mfrow, time.labels = var$timepoints,new.window = F)
    fuzz

  })
  
  output$mfuzzbutton <- renderUI({
    actionButton(
      "dlmfuzz",
      "Download clusters lists"
    )
  })
  output$mfuzzcorebutton <- renderUI({
    actionButton(
      "dlcoremfuzz",
      "Download core clusters lists"
    )
  })
  
  observeEvent(input$dlmfuzz,{
    clusters_list<- acore(mfdata.s,N_cl, min.acore = 0)
    for (i in 1:length(clusters_list)){
      write.table(clusters_list[i],
                  file = paste0("~/Desktop/", paste("cluster", i, "txt", sep = ".")), sep = "\t", row.names = F)
    }
    
  })
  
  observeEvent(input$dlcoremfuzz,{
    core_clusters_list <- acore(mfdata.s, N_cl, min.acore = 0.7)
    for (i in 1:length(core_clusters_list)){
      write.table(core_clusters_list[i],
                  file = paste0("~/Desktop/", paste("core_cluster", i, "txt", sep = ".")), sep = "\t", row.names = F)
    }
    
  })

  

  output$clus_enrich <- renderUI({
                      box(
                        title = tagList(icon("cogs"),"Parameters"),
                        solidHeader = T,
                        status = "primary",
                        width = NULL,
                        sliderInput(
                          "whichclust",
                          "Cluster to enrich",
                          min = 1,
                          max = as.numeric(input$inertiaclass),
                          value = 1,
                          step = 1
                      ),
                      sliderInput(
                        "topres",
                        "Top results to show",
                        min = 1,
                        max = 50,
                        value = 10,
                        step = 1
                      ),
                      selectInput(
                        "mforg",
                        "Choose your Organism",
                        c("Drosophila melanogaster" = "dmelanogaster",
                          "Mus musculus" = "mmusculus",
                          "Homo sapiens" = "hsapiens", 
                          "Caenorhabditis elegans" = "celegans",
                          "Zebrafish" = "drerio",
                          "Aspergillus fumigatus Af293" = "afumigatus",
                          "Bonobo" = "ppaniscus",
                          "Cat" = "fcatus",
                          "Chicken" = "ggallus",
                          "Chimpanzee" = "ptroglodytes",
                          "Common Carp" = "ccarpio",
                          "Cow" = "btaurus",
                          "Dog" = "clfamiliaris",
                          "Dolphin" = "ttruncatus",
                          "Goat" = "chircus",
                          "Gorilla" = "ggorilla",
                          "Guppy" = "preticulata",
                          "Horse" = "ecaballus",
                          "Pig" = "sscrofa",
                          "Platypus" = "oanatinus",
                          "Rabbit" = "ocuniculus")
                      ),
                      checkboxGroupInput(
                        "mfEnrich",
                        "Choose your enrichment",
                        c("GO : Biological Processes" = "GO:BP",
                          "GO : Molecular Functions" = "GO:MF",
                          "GO : Cellular Components" = "GO:CC",
                          "KEGG Pathways" = "KEGG",
                          "Reactome" = "REAC",
                          "WikiPathways" = "WP",
                          "TRANSFAC" = "TF",
                          "MirTarBase" = "MIRNA",
                          "Human Phenotype Ontology" = "HP",
                          "Human Protein Atlas" = "HPA",
                          "CORUM" = "CORUM")
                      ),
                      do.call(actionBttn, c(          # run button 
                        list(
                          inputId = "mfenrichmentgo",
                          label = "Enrich",
                          icon = icon("play")
                        )))
                      )
  })
  

  
  
  observeEvent(input$mfenrichmentgo,{
      clusters_list<- acore(mfdata.s,N_cl, min.acore = 0)
      geneset <-as.character(sapply(clusters_list[as.numeric(input$whichclust)],"[[", 1 )) # takes the gene set of the chosen cluster
      enrichement <- unlist(strsplit(input$mfEnrich, split = '\n'))
      res <- gost(geneset, 
                  organism = input$mforg,
                  user_threshold = 0.05,
                  correction_method = "g_SCS",
                  domain_scope = "annotated", 
                  sources = enrichement,
                  numeric_ns = "ENTREZGENE",
                  significant = T)
      res_mf_enrich <- as.data.frame(res$result)# result as data frame
      res_mf_enrich <- res_mf_enrich[,-1]
      res_mf_enrich <- res_mf_enrich[order(res_mf_enrich[,2]),]
      res_mf_enrich <- res_mf_enrich[1:as.numeric(input$topres),]
      res_mf_enrich <- res_mf_enrich[,-1]
      res_mf_enrich <- res_mf_enrich[,-2]
      res_mf_enrich <- res_mf_enrich[,-2]
      res_mf_enrich <- res_mf_enrich[,-2]
      res_mf_enrich <- res_mf_enrich[,-2]
      res_mf_enrich <- res_mf_enrich[,-2]
      res_mf_enrich <- res_mf_enrich[,-5]
      res_mf_enrich <- res_mf_enrich[,-5]

      
      MFEnrichRun$MFEnrichRunValue <- input$mfenrichmentgo   # precise the run button has been clicked
      
      output$mf_enrichdt <-  DT::renderDataTable({   # result table
        data <- res_mf_enrich
        colnames(data) <- c("P Value","Term_id", "Enrichment","Term_name", "Parents")
        DT::datatable(
          data,        
          extensions = 'Buttons',    # download button 
          option = list(
            paging = TRUE,
            searching = TRUE,
            fixedColumns = TRUE,
            autoWidth = TRUE,
            ordering = TRUE,
            dom = 'Bfrtip',
            buttons = list(list(
              extend = 'collection',
              buttons = list(extend='csv',
                             filename = "cluster_enrichment"),
              text = 'Download')),
            scrollX = TRUE,
            pageLength = 10,
            searchHighlight = TRUE,     # search bar 
            orderClasses = TRUE
            
          ),
          
          class = "display")
      }, server = FALSE)
      
      output$mf_manhattan <- renderPlotly({
        data <- res
        fig <- gostplot(
          data,
          capped = T,
          interactive = T
        )
        fig
      })
      
      output$mf_barplot <- renderPlotly({
        data <- res_mf_enrich
        fig <- plot_ly(
          data,
          x = ~(-log10(p_value)),
          y = ~term_name,
          type = "bar",
          color = ~factor(source)
          
        )
        fig
      })
      
      

      
  } 
               )
  
  
  
  

  
  })

})




output$inertia_elbowUI <- renderUI({
  if (nrow(var$mfuzzTable) != 0){
  tabsetPanel(
    # render plots 
    tabPanel(title = "Inertia", plotlyOutput("inertia_plot") %>% withSpinner()),
    tabPanel(title = "Elbow", plotOutput("elbow_plot") %>% withSpinner())
  )}else{
    helpText("input a count matrix first.")
  }
})
output$overlap <- renderUI({
  if (nrow(var$mfuzzTable) != 0){   
    tagList(
      fluidRow(column(
        12, plotOutput('overlap_plot',height = 700) %>% withSpinner()
      )))} else {                   
        helpText("input a count matrix first.")
      }
})




output$mfuzz <- renderUI({
  if (nrow(var$mfuzzTable) != 0){    
    tagList(
      fluidRow(column(12, uiOutput("mfuzzbutton"),
                      uiOutput("mfuzzcorebutton"),
                      plotOutput('mfuzz_plots',height = var$heightplot) %>% withSpinner()
      )))} else {                      
        helpText("input a count matrix first.")
      }
})

output$mf_enrich <- renderUI({
  if(MFEnrichRun$MFEnrichRunValue){
    tagList(
      fluidRow(
        column(12, plotlyOutput("mf_manhattan") %>% withSpinner()),
        column(12, dataTableOutput("mf_enrichdt") %>% withSpinner())

      )
    )} else {                      
      helpText("Compute Mfuzz plots before, and select your parameters")
    }
})

output$mfenrichement <- renderUI({
  navbarPage("Cluster Enrichment", 
             tabPanel(
               title = tagList(icon("dice-one"), "Enrichment"),
               width = NULL,
               solidHeader = T,
               status = "primary",
               tagList(
                  fluidRow(
                    column(3, uiOutput("clus_enrich")),
                    column(9,uiOutput("mf_enrich") %>% withSpinner())
))),
tabPanel(title = tagList(icon("dice-two"), "Bar Plot"),
         width = NULL,
         solidHeader = T,
         status = "primary",
         plotlyOutput("mf_barplot"))

)
})