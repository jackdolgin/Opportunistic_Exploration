if (!require(devtools)) install.packages("pacman")
pacman::p_load(here, bibtex, RefManageR, knitr)

pubs <- GetBibEntryWithDOI(
  c(
    "10.1002/bdm.2226",
    "10.1037/dec0000074",
    "10.1016/j.cogpsych.2016.01.001",
    "10.1111/cogs.12817",
    "10.1037/a0038199",
    "10.1016/j.cognition.2017.12.014",
    "10.1016/j.cobeha.2020.10.001",
    "10.1037/10039-000",
    "10.1177/0956797610387443",
    "10.1207/s15516709cog0304_1",
    "10.2307/1914185",
    "10.1007/s42113-018-0010-8",
    "10.3758/s13428-014-0458-y",
    "10.1126/science.143.3611.1190",
    "10.1007/s00426-017-0943-x",
    "10.1037/a0020198",
    "10.1371/journal.pone.0068210",
    "10.7554/eLife.27430",
    "10.1038/s41598-021-82530-8",
    "10.1177/1073858408317417",
    "10.3389/fnhum.2017.00238",
    "10.1037/xge0000546",
    "10.1016/j.conb.2018.11.003",
    "10.3758/s13415-020-00837-x"
  )
)


BibEntry(
  bibtype = "manual",
  key = "deep-exploration",
  title = "Deep exploration as a unifying account of explore-exploit behavior",
  author = c(person("Wilson, R. C."), person("Wang, S."), person("Sadeghiyeh, H."), person("Cohen, J. D")),
  date = "2020",
  url = "https://doi.org/10.31234/osf.io/uj85c"
)


annyang_citation <- BibEntry(
  "manual",
  key = "js-annyang",
  author = "Tal Ater",
  title = "annyang! Easily add speech recognition to your site",
  year = "2017",
  url = "https://github.com/TalAter/annyang"
)

WriteBib(
  c(pubs, annyang_citation),
  here("manuscript", "article_refs.bib")
)



write_bib(
  c(
    "arrow",
    "base",
    "cowplot",
    "gghalves",
    "patchwork",
    "RcppDE",
    "tidyverse"
  ),
  here("manuscript", "r_package_refs.bib")
)




Wilson, Wang, Sadeghiyeh, and Cohen, 2020 preprint https://psyarxiv.com/uj85c

Wu, H., Guo, X. &amp; Liu, X.. (2018). Adaptive Exploration-Exploitation Tradeoff for Opportunistic Bandits. <i>Proceedings of the 35th International Conference on Machine Learning</i>, in <i>Proceedings of Machine Learning Research</i> 80:5306-5314 Available from https://proceedings.mlr.press/v80/wu18b.html.

Lashley, K. S. (1951). The problem of serial order in behavior. In L. A. Jeffress (Ed.), Cerebral mechanisms in behavior: The Hixon symposium (pp. 112–146). Wiley.


Brändle, Stocks, Tenenbaum, Gershman, & Schulz, 2021
https://doi.org/10.31234/osf.io/ybs7g


Schulz, Klenske, Bramley, and Speekenbrink, 2017
"10.1101/110486"