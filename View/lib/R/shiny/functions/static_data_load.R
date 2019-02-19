  staticDataFetcher = function(){

    datasetList <- list()
    metadataList <- list()


    #TEMPLATE_ANCHOR shinyDataLoad

      obs <- datasetList[["ISASimple_Gates_MAL-ED_phase2_RSRC"]][!is.na(datasetList[["ISASimple_Gates_MAL-ED_phase2_RSRC"]]$EUPATH_0000579),]
      obs <- obs[,which(unlist(lapply(obs, function(x)!all(is.na(x))))),with=F]
      obs$EUPATH_0004991 <- ifelse(is.na(obs$EUPATH_0004991), obs$EUPATH_0011650, obs$EUPATH_0004991)
      obs$EUPATH_0004991 <- ifelse(is.na(obs$EUPATH_0004991), obs$EUPATH_0011056, obs$EUPATH_0004991)
      obs$EUPATH_0004991 <- ifelse(is.na(obs$EUPATH_0004991), obs$EUPATH_0011901, obs$EUPATH_0004991)
      houseObs <- datasetList[["ISASimple_Gates_MAL-ED_phase2_RSRC"]][is.na(datasetList[["ISASimple_Gates_MAL-ED_phase2_RSRC"]]$EUPATH_0000579),]
      houseObs$EUPATH_0004991 <- houseObs$EUPATH_0011134
      houseObs$EUPATH_0004991 <- ifelse(is.na(houseObs$EUPATH_0004991), houseObs$EUPATH_0011587, houseObs$EUPATH_0004991)
      houseObs$EUPATH_0000579 <- houseObs$EUPATH_0011931
      houseObs <- houseObs[,which(unlist(lapply(houseObs, function(x)!all(is.na(x))))),with=F]
      myCols <- colnames(obs)[colnames(obs) %in% colnames(houseObs) & !colnames(obs) %in% c("Participant_Id", "EUPATH_0004991", "EUPATH_0000579")]
      houseObs <- houseObs[, !myCols, with=FALSE]
      myStaticCols <- c("Participant_Id", "EUPATH_0011928", "OBI_0001627")
      #myStaticCols <- c("Participant_Id", "OBI_0001627")
      static <- houseObs[, myStaticCols, with=FALSE]
      static <- unique(static[!is.na(static$OBI_0001627),])
      houseObs <- houseObs[, !c("EUPATH_0011928", "OBI_0001627"), with=FALSE]
      #houseObs <- houseObs[, !c("OBI_0001627"), with=FALSE]
      obs <- merge(obs, houseObs, by = c("Participant_Id", "EUPATH_0004991", "EUPATH_0000579"), all=TRUE)
      datasetList[["ISASimple_Gates_MAL-ED_phase2_RSRC"]] <- merge(static, obs, by = "Participant_Id")

    assign("datasetList", datasetList, .GlobalEnv)
    assign("metadataList", metadataList, .GlobalEnv)

  }
