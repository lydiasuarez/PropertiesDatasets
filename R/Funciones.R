#' @import graphics
#' @import stats
#'
NULL

#' Datasets List
#' @return Returns a vector of strings with the data sets included
#' @export
getDataSetList = function(){
  dataset <- c("Zheisel")
  cat("Available Datasets: ", "\n", dataset)
}

#' Cell Types in Dataset
#' @return Returns the cell types considered in the data set identified by the string in dataset
#' @export
getCellTypesInDataSet <- function(dataset){
  if (dataset == "Zheisel"){
    cat("Cell types considered for this dataset are: ","\n")
    gsub("ClassProbability_","",names(sympathetic$col.attrs)[grep("ClassProbability",names(sympathetic$col.attrs))])
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
    gsub(".loom","",listloom)
  }
  else
    print("There are not brain regions for this dataset")
}

#' Active Gene Methods
#' @return Returns the available methods data for availability
#' @export
getActiveGeneMethod <- function(dataset){
  methods= c("rawexpression", "variationcoefficient")
  if (dataset == "Zheisel"){
    cat("The available methods data for availability are: ","\n")
    methods
  }
  else
    print("There are not available methods for this dataset")
}

#' Active Genes In Cell Type
#' @return Returns the gene ids of genes active in the brain region, cell type, for a given calculus method.
#' @export
getActiveGenesInCellType <- function(dataset,brainregion,celltype,method){
  if (dataset == "Zheisel"){
    mymatrix = brainregion$matrix[,]
    colnames(mymatrix) = brainregion$row.attrs$Gene[]
    rownames(mymatrix) = brainregion$col.attrs$CellID[]
    tgtmatrix = mymatrix[brainregion$col.attrs$Class[] == celltype,]
    tissue = deparse(substitute(x))
    saveRDS(tgtmatrix,paste0(mypath,tissue,celltype,".rds"))
    expr.data = readRDS(paste0(mypath,tissue,celltype,".rds"))

    if (method == "rawexpression"){
      visibility = seq(0,0.1,0.01)
      genes = colnames(expr.data)
      pdata = unlist(lapply(visibility,function(x){
        mask = colSums(expr.data > 0.5)  > (x * nrow(expr.data))
        cat("Genes in this tissue for visibility",x,"are",sum(mask),"\n")
        cat("First 5 visible genes for this threshold of visibility are",
            paste0(colnames(expr.data)[mask][1:5],collapse=", "),"\n")
        sum(mask)
      }))
      plot_rawexpression <- plot(x=visibility,
           y=pdata,
           main=paste0("Genes available for ",celltype),
           type="l",
           xlab="Visibility of the genes in cell number ratio")
    }
    if (method == "variationcoefficient"){
      cv = function (x, na.rm = FALSE)
      {
        sd(x, na.rm = na.rm)/mean(x, na.rm = na.rm)
      }
      cvs = apply(expr.data,2,cv,na.rm=T)
      plot_variationcoefficient <- plot(density(na.omit(cvs)),
                 main=paste0("Variation coeff. for ",length(na.omit(cvs)),
                             " mouse genes"),
                 xlab="5000 probes selected (right of the vertical line)",
                 ylab="variation coefficient")
    }

  }
}















