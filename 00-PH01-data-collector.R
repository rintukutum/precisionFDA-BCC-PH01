rm(list=ls())
sc1.files <- list.files('../SC1/data/',full.names = TRUE)
sc1.phenotype <- read.table(
  sc1.files[grep('Phenotype',sc1.files)],
  header = TRUE,
  sep = '\t',
  stringsAsFactors = FALSE
)
sc1.outcome <- read.table(
  sc1.files[grep('Outcome',sc1.files)],
  header = TRUE,
  sep = '\t',
  stringsAsFactors = FALSE
)
sc1.FeatureMatrix <- read.table(
  sc1.files[grep('FeatureMatrix',sc1.files)],
  header = TRUE,
  sep = '\t',
  stringsAsFactors = FALSE
)
sc1.data <- list(
  phenotype = sc1.phenotype,
  outcome = sc1.outcome,
  feature = sc1.FeatureMatrix
)
dir.create('./data',showWarnings = FALSE)
save(sc1.data, file= './data/sc1.data.RData')
#------------------
rm(list=ls())
sc2.files <- list.files('../SC2/data/',full.names = TRUE)
sc2.phenotype <- read.table(
  sc2.files[grep('Phenotype',sc2.files)],
  header = TRUE,
  sep = '\t',
  stringsAsFactors = FALSE
)
sc2.outcome <- read.table(
  sc2.files[grep('Outcome',sc2.files)],
  header = TRUE,
  sep = '\t',
  stringsAsFactors = FALSE
)
sc2.FeatureMatrix <- read.table(
  sc2.files[grep('FeatureMatrix',sc2.files)],
  header = TRUE,
  sep = '\t',
  stringsAsFactors = FALSE
)
sc2.data <- list(
  phenotype = sc2.phenotype,
  outcome = sc2.outcome,
  feature = sc2.FeatureMatrix
)
dir.create('./data',showWarnings = FALSE)
save(sc2.data, file= './data/sc2.data.RData')
#------------------
rm(list=ls())
sc3.files <- list.files('../SC3/data/',full.names = TRUE)
sc3.phenotype <- read.table(
  sc3.files[grep('Phenotype',sc3.files)],
  header = TRUE,
  sep = '\t',
  stringsAsFactors = FALSE
)
sc3.outcome <- read.table(
  sc3.files[grep('Outcome',sc3.files)],
  header = TRUE,
  sep = '\t',
  stringsAsFactors = FALSE
)
sc3.FeatureMatrix <- read.table(
  sc3.files[grep('FeatureMatrix',sc3.files)],
  header = TRUE,
  sep = '\t',
  stringsAsFactors = FALSE
)
sc3.data <- list(
  phenotype = sc3.phenotype,
  outcome = sc3.outcome,
  feature = sc3.FeatureMatrix
)
dir.create('./data',showWarnings = FALSE)
save(sc3.data, file= './data/sc3.data.RData')