# Ecological disturbance reduces genomic diversity across an Alpine whitefish adaptive radiation


All scripts to generate the results (starting with processing of raw sequence data, mapping, calculation of genotype likelihoods and the analyses done) can be found in the folder lsf_submit_scripts. Scripts were run according to the sequence described in the file on the ETHZ cluster Euler.

All scripts to visualize and finalize the data are in the folder R_scripts.


## lsf_submit_scripts

#### fastp.lsf
Used to trim poly-G tails from raw reads.


#### Seqprep.lsf
Used to merge overlapping forward and reverse reads.


#### BWA.lsf
Used to map the data to the Alpine whitefish reference genome (De-Kayne et al. 2020).


#### Processing_bam.lsf
Used to process the mapped bam files.


#### Angsd_per_chromosom.lsf
Used to calculate genotypelikelihoods (per chromosome).


#### merging_single_beagle.lsf
Used to merge the single beagle files of each chromosomes into one genome-wide file with genotype likelihoods.


#### PCAngsd.lsf
Used to do a genomic PCA.

#### SAF.lsf
Used to calculate site allele frequencies for the FST analysis.

#### FST.lsf
FST analysis based on site allele frequency spectra.

#### maf_at_position.lsf
Used to calculate allele frequencies based on genotype likelihoods.




## R_scripts

#### Figure_1.R
Script to produce first figure of the paper.

#### FST.R
FST analysis to generate bed files with most differentiated positions (used as input to maf_at_position.lsf).

#### PCA.R
Used to plot output of PCAngsd.lsf.

#### frequencies.R
Used to plot allele frequency results of maf_at_position.lsf.

#### manhattan_custom.R
R script to generate custom manhattan plot, modified from package qqman (Turner, 2018).


