library(magrittr)

#' Create a glossary with supporting functions
#' 
#' Depends on dplyr, magrittr, glue, reactable
#' 
#' @param glt a data.frame with 2 columns: "Term" and "Definition".
#' if NULL (default), an empty glossary is created
#' @param style default style for html definitions
#' 
create_glossary <- function(
   glt=NULL, style="text-decoration: underline dashed;"
){
   
   if(is.null(glt)){
      glt <- dplyr::tibble(
         Term=character(), Definition=character()
      )
   }
   
   ##
   stopifnot(
      is.data.frame(glt),
      all(sort(colnames(glt))==sort(c("Term", "Definition"))),
      is.character(glt$Term), all(!is.na(glt$Term)),
      is.character(glt$Definition), all(!is.na(glt$Definition)),
      is.character(style), length(style)==1, !is.na(style)
   )
   glt <- glt %>%
      dplyr::as_tibble() %>% 
      dplyr::mutate(Synonyms=lapply(Term, function(x) x))
   stopifnot(sum(duplicated(toupper(unlist(glt$Synonyms))))==0)
   default_style <- style
   
   ##
   get_definitions <- function(terms, include_plurals=TRUE){
      stopifnot(length(terms)>0)
      matches <- lapply(
         terms, function(x)
            which(unlist(lapply(
               glt$Synonyms,
               function(y){
                  if(include_plurals){
                     toupper(x) %in% c(toupper(y), paste0(toupper(y), "S"))
                  }else{
                     toupper(x) %in% toupper(y)
                  }
               }
            )))
      )
      issues <- terms[which(lengths(matches)!=1)]
      if(length(issues)>0){
         stop(glue::glue(
            'Issue with the following glossary term(s): ',
            '{glue_collapse(issues, sep=", ", last=" and ")}'
         ))
      }
      mgloss <- glt %>% 
         dplyr::slice(unlist(matches)) %>% 
         dplyr::mutate(Term=terms)
      return(mgloss)
   }
   
   ##
   return(list(
      
      ##
      get_table=function(){return(glt)},
      
      ##
      view=function(){
         glt %>%
            dplyr::mutate(
               Terms=unlist(lapply(Synonyms, glue::glue_collapse, sep=", "))
            ) %>% 
            dplyr::select(Terms, Definition) %>% 
            reactable::reactable(
               columns=list(
                  Terms=reactable::colDef(minWidth=50),
                  Definition=reactable::colDef(minWidth=200)
               ),
               defaultSorted=c("Terms"),
               searchable=TRUE
            )
      },
      
      ##
      add_terms=function(nglt){
         stopifnot(
            is.data.frame(nglt),
            all(sort(colnames(nglt))==sort(c("Term", "Definition"))),
            is.character(nglt$Term), all(!is.na(nglt$Term)),
            is.character(nglt$Definition), all(!is.na(nglt$Definition))
         )
         glt <- dplyr::bind_rows(
            glt,
            nglt %>%
               dplyr::as_tibble() %>% 
               dplyr::mutate(Synonyms=lapply(Term, function(x) x))
         )
         stopifnot(sum(duplicated(toupper(unlist(glt$Synonyms))))==0)
         glt <<- glt
      },
      
      ##
      add_term=function(term, definition){
         stopifnot(
            is.character(term), length(term)==1, !is.na(term),
            is.character(definition), length(definition)==1, !is.na(definition)
         )
         nglt <- dplyr::tibble(Term=term, Definition=definition) %>% 
            dplyr::mutate(Synonyms=lapply(Term, function(x) x))
         glt <- dplyr::bind_rows(glt, nglt)
         stopifnot(sum(duplicated(toupper(unlist(glt$Synonyms))))==0)
         glt <<- glt
      },
      
      ##
      add_synonyms=function(term, synonyms){
         toAdd <- c(
            glt$Synonyms[[which(glt$Term==term)]],
            synonyms
         )
         toAdd <- toAdd[which(!duplicated(toupper(toAdd)))]
         glt$Synonyms[which(glt$Term==term)] <- list(toAdd)
         stopifnot(sum(duplicated(toupper(unlist(glt$Synonyms))))==0)
         glt <<- glt
      },
      
      ##
      remove_terms=function(terms){
         glt <<- glt %>% dplyr::filter(!Term %in% terms)
      },
      
      ##
      get_definitions=get_definitions,
      
      ##
      get_html_definitions=function(terms, include_plurals=TRUE, style=NULL){
         if(is.null(style)){
            style <- default_style
         }
         stopifnot(is.character(style), length(style)==1, !is.na(style))
         mgloss <- get_definitions(terms, include_plurals)
         glue::glue(
            '<span title="{mgloss$Definition}" style="{style}">',
            "{mgloss$Term}",
            "</span>"
         ) %>% 
            as.character() %>% 
            return()
      }
   ))
   
}

