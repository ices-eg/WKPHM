#### Code for creating spatial blocks for cross validation
###copied from CSAS SDM Framework (Nephin et al. 2020)
##2020-02-14



# Create spatial blocks for CV
Spatial_CV <- function(  sp, folds, obs, env ){
  # message
  cat("\n",sp,"folds:\n")
  # Convert pts to spPts
  spPts <- obs
 #   SpatialPointsDataFrame( coordinates(obs[["spatDat"]]),
  #                                 data = obs[["sppDat"]],
   #                                proj4string = CRS(geoCRS))
  # investigate spatial autocorrelation in raster covariates
  autorange <- spatialAutoRange(rasterLayer = env, # raster stack
                                sampleNumber = 10000, # number of cells to sample
                                doParallel = TRUE,
                                showPlots = FALSE)
  # spatial blocking by specified range and random assignment for each species
  set.seed(42) # to ensure reproducibility
  sp.blocks <- spatialBlock(speciesData = obs,
                            species = sp,
                            theRange = autorange$range, # distance by which blocks are created
                            k = folds,
                            selection = 'random',
                            iteration = 2000,
                            numLimit = 2, # min points in each category of data
                            biomod2Format = FALSE,
                            xOffset = 0, # shift the blocks horizontally
                            yOffset = 0, # shift the blocks vertically
                            progress = TRUE,
                            showBlocks = TRUE)
  # Create cv list to store train and test row indices for each CV run
  cv <- list()
  for ( f in 1:folds ){
    foldname <- paste0("fold",f)
    cv[[foldname]][['train']] <- which(sp.blocks$foldID!=f)
    cv[[foldname]][['test']] <- which(sp.blocks$foldID==f)
  }
  #return blockpoly, foldID and CV list
  return( list(foldID=sp.blocks$foldID, blockpolys=sp.blocks$blocks, cv=cv, records=sp.blocks$records) )
}
