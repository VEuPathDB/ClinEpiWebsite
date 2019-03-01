  staticDataFetcher = function(){

    datasetList <- list()
    metadataList <- list()


    #TEMPLATE_ANCHOR shinyDataLoad

    if (length(metadataList[["ISASimple_Gates_MAL-ED_phase2_RSRC"]]) > 1) {
      myPrtcpntCols <- metadataList[["ISASimple_Gates_MAL-ED_phase2_RSRC"]]$source_id[metadataList[["ISASimple_Gates_MAL-ED_phase2_RSRC"]]$category == "Participant"]
      prtcpnt <- unique(datasetList[["ISASimple_Gates_MAL-ED_phase2_RSRC"]][, c("Participant_Id", myPrtcpntCols), with=FALSE])
      temp <- datasetList[["ISASimple_Gates_MAL-ED_phase2_RSRC"]][, !myPrtcpntCols, with=FALSE]
      obs <- temp[!is.na(temp$EUPATH_0000579),]
      obs <- obs[,which(unlist(lapply(obs, function(x)!all(is.na(x))))),with=F]
      obs$EUPATH_0004991 <- ifelse(is.na(obs$EUPATH_0004991), obs$EUPATH_0011650, obs$EUPATH_0004991)
      obs$EUPATH_0004991 <- ifelse(is.na(obs$EUPATH_0004991), obs$EUPATH_0011056, obs$EUPATH_0004991)
      obs$EUPATH_0004991 <- ifelse(is.na(obs$EUPATH_0004991), obs$EUPATH_0011901, obs$EUPATH_0004991)
      houseObs <- temp[is.na(temp$EUPATH_0000579),]
      houseObs$EUPATH_0004991 <- houseObs$EUPATH_0011134
      houseObs$EUPATH_0004991 <- ifelse(is.na(houseObs$EUPATH_0004991), houseObs$EUPATH_0011587, houseObs$EUPATH_0004991)
      houseObs$EUPATH_0000579 <- houseObs$EUPATH_0011931
      houseObs <- houseObs[,which(unlist(lapply(houseObs, function(x)!all(is.na(x))))),with=F]
      myStaticCols <- c("Participant_Id", "EUPATH_0011928", "OBI_0001627")
      static <- houseObs[, myStaticCols, with=FALSE]
      static <- unique(static[!is.na(static$OBI_0001627),])
      houseObs <- houseObs[, !c("EUPATH_0011928", "OBI_0001627"), with=FALSE]
      obs <- merge(obs, houseObs, by = c("Participant_Id", "EUPATH_0004991", "EUPATH_0000579"), all=TRUE)
      obs <- merge(obs, prtcpnt, by = "Participant_Id")
      datasetList[["ISASimple_Gates_MAL-ED_phase2_RSRC"]] <- merge(static, obs, by = "Participant_Id")
    }
  
    assign("datasetList", datasetList, .GlobalEnv)
    assign("metadataList", metadataList, .GlobalEnv)
  
  }
