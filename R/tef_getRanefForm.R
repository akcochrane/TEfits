#' Extract group-level regressions from a multilevel model formula
#'
#' Returns the groupingVar-level regression formulas. For example, the
#' formula \code{yVar ~ aVar \* bVar \* cVar + (aVar \* cVar | groupingVar)}
#' will return the group-level formula \code{groupingVar ~ aVar \* cVar}. Note that
#' All random effects of \code{groupingVar} must be within a single set of (parentheses).
#'
#' @param formIn multilevel model formula
#' @param groupingVar grouping variable that will be the left-hand-side of the returned formula
#'
#' @export
#'
tef_getRanefForm <- function(formIn,groupingVar){

  failed <- T ; try({
  groupTerm <- grep(groupingVar,attr(terms(formIn),'term.labels'),fixed=T,value=T)

  groupRHS <- substr(groupTerm,1,
                     min(gregexpr('|',groupTerm,fixed = T)[[1]])-1
  )
  groupForm <- as.formula(paste(formIn[[2]],'~',groupRHS))
failed <- F
},silent=T)
if(failed){cat('Parsing the multilevel model for',groupingVar,
          '- level models failed.\nPlease ensure your formula includes all random effects of',
          groupingVar,'in a single set of (), i.e., `yVar ~ aVar * bVar + (bVar |',groupingVar,')`\n')
}else{return(groupForm)}

}
