# Excuse my R

A catalogue of R behaviours that surprise even seasoned users. Every pitfall runs in the visitor's browser thanks to [webR](https://docs.r-wasm.org/webr/), on the R version of their choice (4.1.3 to 4.6.0).

Live site: <https://excuse-my-r.github.io>, in English, and in French under [`/fr/`](https://excuse-my-r.github.io/fr/).

## Build the site

```sh
quarto render     # static site in _site/
quarto preview    # local preview with live reload
```

You need R with {knitr}, {rmarkdown} and the packages the pages execute: {cli}, {data.table}, {dplyr}, {ggplot2}, {here}, {httr}, {lubridate}, {shiny}, {stringr}, {testthat}, {tibble} and {tidyr}.

The `.github/workflows/publish.yml` workflow builds the site and publishes it to GitHub Pages on every push to `main`.

## How it works

- Each pitfall is one `.qmd` page, in English in `pitfalls/` and in French in `fr/pieges/`. Both versions share the same `ref` (`NUM-01`, `TDY-08`...), which links them together.
- The `::: {.piege}` blocks are executed by real R at build time: that output is the reference answer.
- `assets/site.js` turns these blocks into an Rgui-style console and reruns them with webR in the browser. Before running a block, it silently replays the blocks above it on the same page, so each block finds its variables.
- A block that needs a package declares it: `::: {.piege packages="dplyr"}`.
- A pitfall that cannot run in a browser (Shiny, renv, CI, OS) uses `::: {.piege .piege-statique}` with plain fenced code, shown without a console.
- The home page catalogue is a Quarto listing (`assets/listing.ejs`), filtered in the browser.
- The interface language follows `<html lang>`: `lang: en` in `_quarto.yml`, `lang: fr` in `fr/_metadata.yml`, which also translates the page labels.
- R 4.1.3 requires cross-origin isolation (COOP and COEP headers). On GitHub Pages, `coi-serviceworker.js` adds them; on Cloudflare, `_headers` does.

## Add a pitfall

Copy an existing page in both languages and fill in the front matter:

```yaml
title: "What the pitfall does, in one sentence"
ref: NUM-14
categories: [Numbers]       # [Numérique] in the French page
impact: silencieux          # silencieux, cryptique or incoherent (internal keys)
extrait: 'round(2.5)'       # one line shown in the catalogue
reponse: '2'                # what R prints after [1]
attendu: "`3`"              # what you would expect
versions: "All"
source: "Slack ThinkR (2026)"
```

Then write the pitfall block, a `## Why` section, and a `## The fix` section with a second block. Each block must run on its own after the blocks above it, without randomness that is not seeded, and without output that depends on the machine or the date.
