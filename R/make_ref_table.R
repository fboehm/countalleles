#' Convert A/C/T/G genotype vectors (for a single locus) to 0,1,2 allele count genotypes.
#'
#' @param gv_actg A genotype vector (coded as A/C/T/G) for a single SNP locus.
#' @return a genotype vector with entries 0,1, or 2 for each subject
#' @examples
#' count_alleles(c("TT", "CT", "CC", "TC"))
#' @export
count_alleles <- function(gv_actg)
    {
    labeled <- label_ref_snp(gv_actg)
    # labeled is a list with two components; first is labeled alleles; second is vector of length 2*length(gv_actg) where each entry is a single nucleotide
    f2<- matrix(ncol = 2, data=labeled[[2]], byrow=TRUE)
    o1  <- apply(FUN = function(x)sum(x==labeled[[1]][1]), X = f2, MARGIN=1)
    names(o1)<- names(gv_actg)
    return(o1)
    }

#' Label one allele (A/C/T/G) as reference and the other as other for use in determining numeric count genotypes.
#'
#' @param gv_actg A genotype vector (coded as A/C/T/G) for a single SNP locus.
#' @return a character vector of length 2 with names reference and other
#' @examples
#' label_ref_snp(c("TT", "CT", "CC", "TC"))
#' @export
label_ref_snp<- function(gv_actg)
    {
    foo<- strsplit(gv_actg, "")
    # define two alleles, "other" & "ref"
    ufoo<- unlist(foo)
    ref <- ufoo[1] #arbitrarily set first entry to be 'ref' allele
    other <- ufoo[ufoo != ref][1] # choose (arbitrarily) the first entry in the vector of those characters that aren't ref.
    #* we should write a test to verify that the input has only two alleles present.
    o2 <- c(ref, other)
    names(o2)<- c("reference", "other")
    gv_unlist <- ufoo
    return(list(o2, gv_unlist))
}


#' Create a reference table for the conversion of A/C/T/G genotype vectors (for a single locus) to 0,1,2 allele count genotypes.
#'
#' @param gv_actg A genotype vector (coded as A/C/T/G) for a single SNP locus.
#' @return a reference table with 1) inputted vector, 2) labeled alleles and 3) numeric count genotypes (0,1,2)
#' @examples
#' make_ref_table(c("TT", "CT", "CC", "TC"))
#' @export
make_ref_table<- function(gv_actg)
{
    all_num <- count_alleles(gv_actg)
    snp_labels <- label_ref_snp(gv_actg)[[1]]
    data.frame(gv_actg, all_num, refallele = snp_labels[1], otherallele= snp_labels[2])
}

