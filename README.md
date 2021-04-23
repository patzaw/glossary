-   [Introduction](#introduction)
    -   [Dependencies](#dependencies)
-   [Use](#use)
    -   [Load and create a glossary](#load-and-create-a-glossary)
    -   [Add terms in the glossary](#add-terms-in-the-glossary)
    -   [Showing the glossary](#showing-the-glossary)
    -   [Get definitions](#get-definitions)

# Introduction

This small script allows the management of simple glossaries.

## Dependencies

This script depends on the following packages:

``` r
library(magrittr)
library(dplyr)
library(glue)
library(reactable)
```

# Use

## Load and create a glossary

``` r
source("glossary.R")
glossary <- create_glossary()
```

## Add terms in the glossary

``` r
glossary$add_term(
   "UMI",
   "Unique Molecular Identifier. Each PCR primer is associated to a UMI to identify PCR duplicates when quantifying the transcripts. Therefore the number of counts corresponds to the number of UMI which is the quantification measure per feature."
)
```

Synonyms can also be provided:

``` r
glossary$add_term(
   "MT",
   "Genes coded by mitochondrial genome. MT (%) corresponds to the percentage of UMI corresponding to mitochondrial genes."
)
glossary$add_synonyms("MT", "MT (%)")
```

## Showing the glossary

-   `glossary$get_table()` returns the glossary tibble
-   `glossary$view()` shows the glossary in a reactable

## Get definitions

The following function is used to get the definitions of terms of
interest. It’s case insensitive and support plurals by default.

``` r
glossary$get_definitions(c("mt", "UMIs"))
```

    ## # A tibble: 2 x 3
    ##   Term  Definition                                                      Synonyms
    ##   <chr> <chr>                                                           <list>  
    ## 1 mt    Genes coded by mitochondrial genome. MT (%) corresponds to the… <chr [2…
    ## 2 UMIs  Unique Molecular Identifier. Each PCR primer is associated to … <chr [1…

In an markdown or html document, the definitions can be shown in
tooltips using the `glossary$get_html_definitions()` function. For
example <span
title="Unique Molecular Identifier. Each PCR primer is associated to a UMI to identify PCR duplicates when quantifying the transcripts. Therefore the number of counts corresponds to the number of UMI which is the quantification measure per feature."
style="text-decoration: underline dashed;">UMIs</span>.

To make it easier to use in an Rmd document in RStudio this function can
be copied in a function with a shorter name such as:

``` r
td <- glossary$get_html_definitions
```

And then the following markdown
[snippet](https://support.rstudio.com/hc/en-us/articles/204463668-Code-Snippets)
can be added in RStudio (Edit Snippets button in Global Options -&gt;
Code):

    snippet td
        `r${2} td(${1})`
