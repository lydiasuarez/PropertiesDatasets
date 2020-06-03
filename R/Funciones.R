#' @import graphics
#' @import stats
#'
NULL

#' Datasets List
#' @return Returns a vector of strings with the data sets included
#' @export
getDataSetList = function(){
  return(unique(basic_dataframe[,"Dataset"]))
}


#' Cell Types in Dataset
#' @return Returns the cell types considered in the data set identified by the string in dataset
#' @export
getCellTypesInDataSet <- function(dataset){
  if (dataset == "Zheisel"){
    cat("Cell types considered for this dataset are: ","\n")
    return(unique(basic_dataframe[,"CellType"]))
  }
  else
    print("There are not available cell types for this dataset")
}

#' Brain Regions in Dataset
#' @return Returns the brain regions considered in the data set identified by the string in dataset
#' @export
getBrainregionsDataSet <- function(dataset){
  if (dataset == "Zheisel"){
    cat("Brain regions from", dataset,"are: ","\n")
    return(unique(basic_dataframe[,"BrainRegion"]))
  }
  else
    print("There are not brain regions for this dataset")
}

#' Dataset Properties
#' @return Returns a data frame of properties describing the dataset
#' @export
getDataSetInfo <- function(dataset){
  if (dataset == "Zheisel"){
    load(basic_dataframe)
    dataset_info <- basic_dataframe[]
    return(dataset_info)
  }
  else
    print("There are not information for this dataset")
}


#' Active Gene Methods
#' @return Returns the available methods data for availability
#' @export
getActiveGeneMethod <- function(dataset){
  methods= c("rawexpression", "variationcoefficient")
  if (dataset == "Zheisel"){
    cat("The available methods data for availability are: ","\n")
    return(unique(basic_dataframe[,"Methods"]))
  }
  else
    print("There are not available methods for this dataset")
}

#' Active Genes In Cell Type
#' @return Returns the gene ids of genes active in the brain region, cell type, for a given calculus method.
#' @export
getActiveGenesInCellType <- function(dataset,brainregion,celltype,method){
  datos_filtrados <-  datos[datos$Dataset == dataset & datos$BrainRegion == brainregion & datos$CellType == celltype & datos$Methods == method,]
  datos_filtrados <- datos_filtrados[rowSums(is.na(datos_filtrados)) != ncol(datos_filtrados),]
  genes_activos <-grep("TRUE",datos_filtrados[,])
  return(names(datos_filtrados)[genes_activos])
}

