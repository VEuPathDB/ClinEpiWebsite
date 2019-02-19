    observeEvent(plotData(), {
      plotData <- as.data.table(plotData())
      if (is.null(plotData)) {
        return()
      }
      if (input$facetType == "none") {
        myFacet <- "none"
      } else {
        myFacet <- getMyFacet$val
      }
      if (input$facet2Type == "none") {
        myFacet2 <- "none"
      } else {
        myFacet2 <- getMyFacet2$val
      }
      if ("FACET" %in% colnames(plotData)) {
        myFacet <- "FACET"
      }
      if ("FACET2" %in% colnames(plotData)) {
        myFacet2 <- "FACET2"
      }
      myPrtcpntView <- prtcpntView$val

      if (myFacet2 != "none") {
        if (myFacet != "none") {
          dt_len <- uniqueN(plotData[, c(myFacet, myFacet2), with=FALSE])
          dt_list <- unique(plotData[, c(myFacet, myFacet2), with=FALSE])
        } else {
          dt_len <- uniqueN(plotData[, myFacet2, with=FALSE])
          dt_list <- unique(plotData[, myFacet2, with=FALSE])
          myFacet <- myFacet2
        }
      } else {
        if (myFacet != "none") {
          dt_len <- uniqueN(plotData[, myFacet, with=FALSE])
          dt_list <- unique(plotData[, myFacet, with=FALSE])
        } else {
          dt_len <- 1
          dt_list <- NULL
        }
      }

      createUI <- function(id, data, facets) {

        print(data)
        #get or
        #TODO double check i've pulled these out right !!!!
        a <- data$Proportion[data$Variable1 == "Attribute+" & data$Variable2 == "Outcome+"]
        b <- data$Proportion[data$Variable1 == "Attribute-" & data$Variable2 == "Outcome+"]
        c <- data$Proportion[data$Variable1 == "Attribute+" & data$Variable2 == "Outcome-"]
        d <- data$Proportion[data$Variable1 == "Attribute-" & data$Variable2 == "Outcome-"]
        OR <- (a*d)/(b*c)
        #get rr
        RR <- (a/(a+b)) / (c/(c+d))
        #if 10 digits or fewer dont use exponential notation
        options("scipen"=10)
        #round to 4 digits and for p-value show "<0.0001" when appropriate
        OR <- round(OR, digits=4)
        RR <- round(RR, digits=4)

        #calc conf interval. later install epitools and let it do the work for you.
        alpha <- 0.05
        siglog <- sqrt((1/a) + (1/b) + (1/c) + (1/d))
        zalph <- qnorm(1 - alpha/2)
        logOR <- log(OR)
        logRR <- log(RR)
        logloOR <- logOR - zalph * siglog
        logloRR <- logRR - zalph * siglog
        loghiOR <- logOR + zalph * siglog
        loghiRR <- logRR + zalph * siglog

        ORlo <- round(exp(logloOR), digits=4)
        RRlo <- round(exp(logloRR), digits=4)
        ORhi <- round(exp(loghiOR), digits=4)
        RRhi <- round(exp(loghiRR), digits=4)

        #get pval
        df <- data.frame(col1 = c(a,c), col2 =c(b,d))
        p <- chisq.test(df)
        p <- p$p.value
        p <- round(p, digits=4)
        if (p != "NaN" & p < 0.0001) {
          p <- "<0.0001"
        }
        #make stats table
        odds.ratio <- c(OR, paste(ORlo, "-", ORhi))
        relative.risk <- c(RR, paste(RRlo, "-", RRhi))
        p.val <- c(p, "N/A")
        stats <- data.table("p-value" = p.val, "Odds Ratio" = as.character(odds.ratio), "Relative Risk" = as.character(relative.risk))
        rownames(stats) <- c("Statistics", "95% Confidence Interval")

        if (length(facets) > 0) {
          myCaption <- paste("Strata:",paste(facets, collapse = " and "))
        } else {
          myCaption <- ""
        }

        output[[id]] <- DT::renderDataTable(datatable(stats,
                                                      caption = myCaption,
                                                      width = '100%',
                                                      options = list(
                                                      sDom = '<"top"><"bottom">',
                                                      autoWidth = TRUE,
                                                      columnDefs = list(list(width = '250px', className = 'dt-right', targets = c(1,2,3)))
                                                      )
        ))
      }

      output$statsTable <- renderUI({
        lapply(1:dt_len, function(i) {
          id <- paste0("statsTable", i)
          facets <- c()
          if (!is.null(dt_list)) {
            if (length(dt_list) == 1) {
              keep <- c(plotData[, myFacet, with=FALSE] == c(dt_list[i]))
              facets <- c(dt_list[i])
              data <- plotData[keep,]
            } else {
              keep1 <- c(plotData[, myFacet, with=FALSE] == c(dt_list[i][, myFacet, with=FALSE]))
              keep2 <- c(plotData[, myFacet2, with=FALSE] == c(dt_list[i][, myFacet2, with=FALSE]))
              keep <- keep1 & keep2
              facets <- c(c(dt_list[i][, myFacet, with=FALSE]), c(dt_list[i][, myFacet2, with=FALSE]))
              data <- plotData[keep,]
            }
          } else {
            data <- plotData
            facets <- c()
          }
          createUI(id, data, facets)
        })
      })
    })
