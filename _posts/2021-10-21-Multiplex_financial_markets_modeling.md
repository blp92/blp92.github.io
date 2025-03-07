---
layout: single
title: "Multiplex Network Financial Models"
categories:
  - Finance
tags:
  - content
  - Finance
  - statistical analysis
  - network dynamics
  - graph theory
  - markup
---
Multiplex Financial Models
================

# Background

This post will outline the background and first pass at the scope of
examining global financial markets (using country data –not involving
private corporations) as dynamic multiplex netowrks. I’ll start mostly
by taking inspiration and techniques from the Maria del Rio-Chanona et
al. paper that [can be found at this
link](https://appliednetsci.springeropen.com/articles/10.1007/s41109-020-00301-2).
This paper looks at the 2008 global financial crises by modeling the
global financial system as a multiplex network (a graph with nodes
representing countries, edges encoding cross-country financial assets,
and layers as asset types).

The paper seeks to identify “systemacially important countries”, in the
hopes of understandign the structure (or thresholds) of critically
important nodes (countries)in the global financial system. I hope to
understand their methodologies to apply a similar analysis on a
different form of multiplex networks. At the outset, my hope is to
represent the global financial network – ideally involving a more
granular node set incorporating private sector firms and corporations –
where the layers correspond to the posture of an entities portfolio
according to Hyman Minsky’s income-debt relation classification of
hedge, speculative, and Ponzi finance positions. This framing can be
found in his 1992 paper “The Financial Instability Hypothesis” published
by the Jerome Levy Economics Institute Working Paper No. 74 [at this
link.](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=161024)

## Dataset and beginnings of paper/model

The paper uses “annual data from the IMF Coordinated Portfolio
Investment Survey on countries’ cross-border equity and debt holdings”,
as well as “quarterly data from the BIS International Banking Statistics
on countries’ cross-border bank loans and deposits” (Maria del
Rio-Chanona et al. 5). From there they construct a directed and weighted
multiplex network consisting of three layers and 131 countries. Before
we can begin constructing a similar model for our purposes, we must
obtain, import, and clean the data. It’s time to do some plumbing and
wrangling.

### BIS data

Following the reference at the end of the BIS data quote from above we
are taken to [a BIS statistics
page](https://www.bis.org/statistics/rppb1807.htm), a page containing
international banking statistics. If you are interested in other BIS
datasets, they have a consolidated link [found
here](https://www.bis.org/statistics/full_data_sets.htm). From the
latter page I downloaded the locational banking statistics and extracted
the zip to a local datasets directory. Time to open up the CSV and see
if there are any formatting peccadilloes to work through before
importing into Rstudio. Scanning through, it seems well formatted and
straightforward. Note there are both regions and countries, and multiple
different asset types as expected.

![a preview of the data in
libreCalc](img/posts/multiplex_financial_markets/BIS_data_csv_preview.gif)
