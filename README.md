# Excuse my R

Catalogue des comportements surprenants de R, où chaque piège s'exécute dans le navigateur du visiteur grâce à [webR](https://docs.r-wasm.org/webr/).

## Construire le site

```sh
quarto render     # site statique dans _site/
quarto preview    # aperçu local avec rechargement
```

Il faut R avec {knitr}, {rmarkdown} et les paquets que les fiches exécutent : {cli}, {data.table}, {dplyr}, {ggplot2}, {here}, {httr}, {lubridate}, {shiny}, {stringr}, {testthat}, {tibble} et {tidyr}. Le workflow `.github/workflows/publish.yml` construit et publie le site sur GitHub Pages à chaque push sur `main`.

Aperçu de partage sur Cloudflare (config dans `wrangler.jsonc`) :

```sh
quarto render && npx wrangler deploy
```

## Fonctionnement

- Chaque fiche est un `.qmd` dans `pieges/`. Les blocs `::: {.piege}` sont exécutés par le vrai R au build : c'est la réponse de référence.
- `assets/site.js` rend ces blocs modifiables et les relance avec webR dans le navigateur. Un bloc qui a besoin d'un paquet le déclare : `::: {.piege packages="dplyr"}`.
- Sur GitHub Pages, `coi-serviceworker.js` ajoute les en-têtes COOP et COEP que R 4.1.3 exige (sur Cloudflare, `_headers` s'en charge).
- Le catalogue de l'accueil est un listing Quarto (`assets/listing.ejs`), filtré côté navigateur.

## Ajouter une fiche

Copier une fiche existante et renseigner l'en-tête :

```yaml
title: "Ce que fait le piège, en une phrase"
ref: NUM-05
categories: [Numérique]
impact: silencieux   # silencieux, cryptique ou incoherent
attendu: "`TRUE`"
versions: "Toutes"
source: "Slack ThinkR (2026)"
```
