# This file contains the common first steps for every code: loading the data, filtering exp so that we only have the IDH-WT primary tumors & the expression of the first 15 genes. Note that the data is internally normalized

# First load the data

pheno <- read.delim('Data/Pheno_conSS.txt') # Contains phenotypic information of the samples
exp <- read.delim('Data/2021-02-08_TCGA_GBM_expression.txt', row.names = 1) # Measures expression levels for different genes in each sample
genes <- read.delim('Data/Genes.txt') # Contains the 20 genes required for classification

# We only want to study the 15 genes of interest, so filter exp accordingly

goi <- match(genes$x[1:15], colnames(exp))
exp <- exp[, goi]

all(colnames(exp) == genes$x[1:15])

# We only want those samples that are IDH-WT and primary tumors, so subset pheno accordingly. Additionally, we will work only with samples with simplicity score > 0.95. The reason behind this is that we want to make sure that the samples we are working with correspond to a single tumor type (main tumor) and are not a mix of different subtypes (which will make it more difficult for the alg to see the patterns behind the data)

samples <- subset.data.frame(pheno, subset = pheno$IDH1_status == 'Wild-type' & pheno$Recurrence == 'Primary' & pheno$Simplicity.score > 0.05)

# Now filter exp to select these samples

filter <- match(samples$Sample, rownames(exp))
exp_f <- exp[filter, ]

# Normalize data internally

exp_fn <- apply(exp_f, 1, function(x) (rank(x)-1)/14)

# Assign exp_fn to exp so that we can work with it. Transpose the df so that it's easier to work with

exp <- t(exp_fn)

# Add the subtype col to the exp matrix. Make sure to use Subtype as a factor

Subtype <- as.factor(samples$Wang)
exp <- data.frame(exp, Subtype = Subtype)