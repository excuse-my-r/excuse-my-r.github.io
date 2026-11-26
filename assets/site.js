// Chaque version de webR embarque une version de R ; une par version de R suffit.
// webR 0.1.x (R 4.1.3) exige l'isolation cross-origin (en-têtes COOP/COEP) et
// son dépôt de paquets n'est plus complet. Aucune version de webR n'a embarqué R 4.2.
const VERSIONS_R = [
  { r: "4.6.0", webr: "0.6.0", paquets: true },
  { r: "4.5.1", webr: "0.5.9", paquets: true },
  { r: "4.5.0", webr: "0.5.0", paquets: true },
  { r: "4.4.2", webr: "0.4.4", paquets: true },
  { r: "4.4.1", webr: "0.4.2", paquets: true },
  { r: "4.3.3", webr: "0.3.3", paquets: true },
  { r: "4.3.0", webr: "0.2.2", paquets: true },
  { r: "4.1.3", webr: "0.1.1", paquets: false, isolation: true }
].filter(x => !x.isolation || window.crossOriginIsolated);
const CLE_VERSION = "batard-de-r.version";
const offset = document.querySelector('meta[name="quarto:offset"]')?.content ?? "./";

// Le site existe en anglais (racine) et en français (fr/) ; la langue vient de <html lang>
const LANGUE = (document.documentElement.lang || "en").startsWith("fr") ? "fr" : "en";
const RACINE_LANGUE = LANGUE === "fr" ? offset + "fr/" : offset;
const TEXTES = {
  en: {
    verifier: "Run the code",
    executer: "Run",
    modifier: "Edit the code",
    retablir: "Reset",
    raccourci: "Ctrl + Enter to run",
    versionR: "R version",
    codeModifiable: "Editable R code",
    execution: "Running...",
    demarrage: v => `Starting R ${v}. The first time, allow a few seconds.`,
    sansPaquet: (paquets, v) => `This block needs {${paquets.join("}, {")}}, which is no longer available for R ${v} in the browser. Pick R 4.3.0 or a more recent version.`,
    installation: p => `Installing package {${p}}...`,
    duree: s => `Ran in ${s.toFixed(1)} s.`,
    echec: m => `R could not start in this browser (${m}).`,
    prochaine: v => `R ${v} will load on the next run.`,
    voisines: "Neighbouring pages",
    precedent: "Previous pitfall",
    suivant: "Next pitfall",
    toutCatalogue: "The whole catalogue",
    hasard: "Discover an oddity",
    autreHasard: "Another oddity, at random",
    impacts: { silencieux: "Silently wrong", cryptique: "Obscure error", incoherent: "Inconsistency" },
    rechercher: "Search a pitfall",
    exemple: "ifelse, NA, factor...",
    categorie: "Category",
    toutes: "All",
    gravite: "Severity",
    controle: "Controlled",
    effacer: "Clear filters",
    aucun: "No pitfall matches these filters.",
    compte: (n, total) => n === total ? `${n} pitfalls` : `${n} ${n > 1 ? "pitfalls" : "pitfall"} out of ${total}`,
    autreLangue: "Français",
    autreLangueTitre: "Lire en français",
    navigation: "Site menu",
    menuCatalogue: "Catalogue",
    contribuer: "How to contribute",
    categories: "Categories",
    source: "Source on GitHub"
  },
  fr: {
    verifier: "Exécuter le code",
    executer: "Exécuter",
    modifier: "Modifier le code",
    retablir: "Rétablir",
    raccourci: "Ctrl + Entrée pour exécuter",
    versionR: "Version de R",
    codeModifiable: "Code R modifiable",
    execution: "Exécution...",
    demarrage: v => `Démarrage de R ${v}. La première fois, comptez quelques secondes.`,
    sansPaquet: (paquets, v) => `Ce bloc a besoin de {${paquets.join("}, {")}}, qui n'existe plus pour R ${v} dans le navigateur. Choisissez R 4.3.0 ou une version plus récente.`,
    installation: p => `Installation du paquet {${p}}...`,
    duree: s => `Exécuté en ${s.toFixed(1).replace(".", ",")} s.`,
    echec: m => `R n'a pas pu démarrer dans ce navigateur (${m}).`,
    prochaine: v => `R ${v} sera chargé à la prochaine exécution.`,
    voisines: "Fiches voisines",
    precedent: "Piège précédent",
    suivant: "Piège suivant",
    toutCatalogue: "Tout le catalogue",
    hasard: "Découvrir une bizarrerie",
    autreHasard: "Une autre bizarrerie, au hasard",
    impacts: { silencieux: "Faux en silence", cryptique: "Erreur obscure", incoherent: "Incohérence" },
    rechercher: "Rechercher un piège",
    exemple: "ifelse, NA, facteur...",
    categorie: "Catégorie",
    toutes: "Toutes",
    gravite: "Gravité",
    controle: "Contrôlé",
    effacer: "Effacer les filtres",
    aucun: "Aucun piège ne correspond à ces filtres.",
    compte: (n, total) => n === total ? `${n} pièges` : `${n} ${n > 1 ? "pièges" : "piège"} sur ${total}`,
    autreLangue: "English",
    autreLangueTitre: "Read in English",
    navigation: "Menu du site",
    menuCatalogue: "Catalogue",
    contribuer: "Comment contribuer",
    categories: "Catégories",
    source: "Source sur GitHub"
  }
};
const T = TEXTES[LANGUE];

// Reproduit la console R : le code est lu ligne par ligne comme au clavier,
// chaque expression complète est recopiée avec ses invites (lignes marquées
// par \001), puis évaluée à part, une erreur, même de syntaxe, n'arrêtant pas
// la suite, avec autoprint, warnings et messages dans l'ordre. Comme au build,
// les blocs précédents de la page sont d'abord rejoués en silence, dans un
// environnement neuf, pour que chaque bloc retrouve ses variables.
const RUNNER = `
.batard_decouper <- function(texte) {
  morceaux <- list()
  tampon <- character()
  ajouter <- function(lignes, exprs) {
    morceaux[[length(morceaux) + 1L]] <<- list(lignes = lignes, exprs = exprs)
  }
  for (ligne in strsplit(texte, "\\n", fixed = TRUE)[[1]]) {
    if (!length(tampon) && !nzchar(trimws(ligne))) {
      next
    }
    tampon <- c(tampon, ligne)
    exprs <- tryCatch(parse(text = tampon, keep.source = FALSE), error = function(err) {
      err
    })
    if (inherits(exprs, "error") && grepl("unexpected (end of input|INCOMPLETE_STRING)", conditionMessage(exprs))) {
      next
    }
    ajouter(tampon, exprs = exprs)
    tampon <- character()
  }
  if (length(tampon)) {
    ajouter(tampon, exprs = tryCatch(parse(text = tampon, keep.source = FALSE), error = function(err) {
      err
    }))
  }
  morceaux
}

.batard_erreur_syntaxe <- function(err, lignes) {
  premiere <- strsplit(conditionMessage(err), "\\n", fixed = TRUE)[[1]][1]
  position <- regmatches(premiere, regexec("^<text>:(\\\\d+):(\\\\d+): (.*)$", premiere))[[1]]
  if (length(position) != 4L) {
    return(paste0("Error: ", premiere))
  }
  extrait <- substr(lignes[as.integer(position[2])], start = 1L, stop = as.integer(position[3]))
  paste0("Error: ", position[4], " in \\"", extrait, "\\"")
}

# À la console, dt[, x := ...] ne s'affiche pas ; {data.table} le détecte par la
# profondeur de pile, que ce runner fausse
.batard_affectation_dt <- function(e) {
  is.call(e) && identical(e[[1]], as.name("[")) &&
    grepl(":=", paste(deparse(e), collapse = " "), fixed = TRUE)
}

.batard_run <- function(code, prelude = "") {
  options_avant <- options()
  # Avant R 4.2, ces options absentes de options() gardent leur effet si on les remet à NULL
  drapeaux <- c("warnPartialMatchDollar", "warnPartialMatchArgs", "warnPartialMatchAttr")
  drapeaux_avant <- lapply(drapeaux, getOption, default = FALSE)
  on.exit({
    options(options_avant)
    options(stats::setNames(drapeaux_avant, drapeaux))
  }, add = TRUE)
  # Chaque exécution repart d'un environnement global vide, comme au build
  env <- globalenv()
  rm(list = grep("^\\\\.batard_", ls(env, all.names = TRUE), value = TRUE, invert = TRUE), envir = env)
  invisible(utils::capture.output({
    for (morceau in .batard_decouper(prelude)) {
      if (!inherits(morceau$exprs, "error")) {
        for (e in morceau$exprs) {
          try(suppressWarnings(suppressMessages(eval(e, envir = env))), silent = TRUE)
        }
      }
    }
  }))
  # L'appel fautif tel que la console l'affiche : rien pour une erreur au niveau
  # supérieur, le seul nom de fonction pour une erreur {rlang}
  decrire_appel <- function(condition) {
    appel <- conditionCall(condition)
    if (!is.call(appel) || identical(appel, quote(eval(e, envir = env)))) {
      return(NULL)
    }
    if (inherits(condition, "rlang_error") && is.name(appel[[1]])) {
      return(paste0(as.character(appel[[1]]), "()"))
    }
    deparse(appel, nlines = 1L)
  }
  afficher_erreur <- function(err) {
    appel <- decrire_appel(err)
    if (is.null(appel)) {
      cat("Error: ", conditionMessage(err), "\\n", sep = "", file = stderr())
    } else {
      cat("Error in ", appel, " : ", conditionMessage(err), "\\n", sep = "", file = stderr())
    }
  }
  # Sortie et messages vont dans la même transcription, mais les messages
  # restent sur leur propre flux : capture.output() ne les voit pas, comme dans R
  transcription <- character()
  connexion <- textConnection("transcription", open = "w", local = TRUE)
  sink(connexion)
  sink(connexion, type = "message")
  on.exit({
    sink(type = "message")
    sink()
    close(connexion)
  }, add = TRUE, after = FALSE)
  local({
    for (morceau in .batard_decouper(code)) {
      lignes <- morceau$lignes
      invites <- c("> ", rep("+ ", length(lignes) - 1L))
      cat(paste0("\\001", invites, lignes, "\\n"), sep = "")
      if (inherits(morceau$exprs, "error")) {
        cat(.batard_erreur_syntaxe(morceau$exprs, lignes = lignes), "\\n", sep = "", file = stderr())
        next
      }
      for (e in morceau$exprs) {
        withCallingHandlers(
          tryCatch({
            v <- withVisible(eval(e, envir = env))
            if (v$visible && !.batard_affectation_dt(e)) {
              print(v$value)
            }
          }, error = afficher_erreur),
          warning = function(w) {
            appel <- decrire_appel(w)
            if (is.null(appel)) {
              cat("Warning message:\\n", conditionMessage(w), "\\n", sep = "", file = stderr())
            } else {
              cat("Warning message:\\nIn ", appel, " : ", conditionMessage(w), "\\n", sep = "", file = stderr())
            }
            invokeRestart("muffleWarning")
          },
          # webR intercepte les messages avant le flux d'erreur : on les écrit
          # nous-mêmes dans la transcription, hors de portée de capture.output()
          message = function(m) {
            cat(conditionMessage(m), sep = "", file = connexion)
            invokeRestart("muffleMessage")
          }
        )
      }
    }
  })
  sink(type = "message")
  sink()
  close(connexion)
  on.exit({
    options(options_avant)
    options(stats::setNames(drapeaux_avant, drapeaux))
  })
  paste(transcription, collapse = "\\n")
}

# Guillemets droits, comme dans la sortie du build
options(useFancyQuotes = FALSE)
`;

function lireVersion() {
  try {
    const v = localStorage.getItem(CLE_VERSION);
    return VERSIONS_R.some(x => x.r === v) ? v : VERSIONS_R[0].r;
  } catch {
    return VERSIONS_R[0].r;
  }
}

let versionChoisie = lireVersion();

// Une seule session R a la fois : changer de version arrete la precedente
let moteur = null;
function demarrerR() {
  if (moteur && moteur.r !== versionChoisie) {
    arreterR();
  }
  if (!moteur) {
    const cible = VERSIONS_R.find(x => x.r === versionChoisie);
    const session = {
      r: cible.r,
      promesse: (async () => {
        const { WebR } = await import(`https://webr.r-wasm.org/v${cible.webr}/webr.mjs`);
        const webR = new WebR();
        await webR.init();
        await webR.evalRVoid(RUNNER);
        const version = await webR.evalRString("paste(R.version$major, R.version$minor, sep = '.')");
        return { webR, version, paquets: new Set() };
      })()
    };
    session.promesse.catch(() => {
      if (moteur === session) {
        moteur = null;
      }
    });
    moteur = session;
  }
  return moteur.promesse;
}

function arreterR() {
  if (!moteur) {
    return;
  }
  moteur.promesse.then(r => { r.webR.close(); }).catch(() => {});
  moteur = null;
}

const selecteurs = new Set();

function choisirVersion(v) {
  versionChoisie = v;
  try {
    localStorage.setItem(CLE_VERSION, v);
  } catch {
    // Stockage indisponible : le choix vaut pour la page en cours.
  }
  selecteurs.forEach(s => { s.value = v; });
}

function el(tag, attrs = {}, texte) {
  const n = document.createElement(tag);
  for (const [k, v] of Object.entries(attrs)) {
    n.setAttribute(k, v);
  }
  if (texte !== undefined) {
    n.textContent = texte;
  }
  return n;
}

// Invites de la console (> pour une nouvelle expression, + pour la suite),
// déduites des parenthèses ouvertes et des opérateurs en fin de ligne.
function avecInvites(code) {
  let profondeur = 0;
  let ouvert = false;
  let chaine = null;
  return code.split("\n").flatMap(ligne => {
    if (!ouvert && ligne.trim() === "") {
      return [];
    }
    const invite = ouvert ? "+ " : "> ";
    let finCode = ligne.length;
    for (let i = 0; i < ligne.length; i++) {
      const c = ligne[i];
      if (chaine) {
        if (c === "\\") {
          i++;
        } else if (c === chaine) {
          chaine = null;
        }
      } else if (c === '"' || c === "'" || c === "`") {
        chaine = c;
      } else if (c === "#") {
        finCode = i;
        break;
      } else if ("([{".includes(c)) {
        profondeur++;
      } else if (")]}".includes(c)) {
        profondeur--;
      }
    }
    const fin = ligne.slice(0, finCode).trim();
    const suite = /(\|>|%[^%]*%|<-|[-+*/^,&|~=<>!])$/.test(fin);
    ouvert = profondeur > 0 || chaine !== null || suite || (ouvert && fin === "");
    return [invite + ligne];
  }).join("\n");
}

function transcriptBuild(cell) {
  const segments = [];
  for (const enfant of cell.children) {
    if (enfant.matches("div.sourceCode")) {
      segments.push({ type: "entree", texte: avecInvites(enfant.querySelector("pre code").textContent.replace(/\n+$/, "")) });
    } else if (enfant.matches(".cell-output, .cell-output-display")) {
      // knitr écrit les erreurs en style moderne ; la console R les affiche « Error in appel : message »
      const texte = enfant.textContent
        .replace(/^\n+|\n+$/g, "")
        .replace(/^Error in `([^`\n]*)`:\n(! )?/gm, "Error in $1 : ")
        .replace(/^Error:\n! /gm, "Error: ")
        .replace(/^Warning in (.*?): /gm, "Warning message:\nIn $1 : ")
        .replace(/^Warning: /gm, "Warning message:\n");
      if (texte.trim() !== "") {
        segments.push({ type: "sortie", texte });
      }
    }
  }
  return segments;
}

function transcriptWebR(texte) {
  const segments = [];
  for (const ligne of texte.split("\n")) {
    const type = ligne.startsWith("\u0001") ? "entree" : "sortie";
    const contenu = type === "entree" ? ligne.slice(1) : ligne;
    const dernier = segments[segments.length - 1];
    if (dernier && dernier.type === type) {
      dernier.texte += "\n" + contenu;
    } else {
      segments.push({ type, texte: contenu });
    }
  }
  return segments.filter(s => s.type === "entree" || s.texte.trim() !== "");
}

// Fenêtre « R Console » de Rgui : saisie en rouge, réponses en bleu marine
function rendreConsole(cible, segments) {
  const code = el("code");
  for (const s of segments) {
    code.append(el("span", { class: s.type === "entree" ? "rgui-entree" : "rgui-sortie" }, s.texte + "\n"));
  }
  code.append(el("span", { class: "rgui-entree" }, "> "));
  const pre = el("pre", { class: "rgui-texte" });
  pre.append(code);
  cible.replaceChildren(pre);
}

function rendreHero(cible, segments) {
  const texte = segments.filter(s => s.type === "sortie").map(s => s.texte).join("\n");
  const pre = el("pre");
  pre.append(el("code", {}, texte));
  cible.replaceChildren(pre);
}

function codeSource(cell) {
  return [...cell.querySelectorAll(":scope > div.sourceCode pre code")]
    .map(c => c.textContent.replace(/\n$/, ""))
    .join("\n");
}

function preparerBloc(bloc, index) {
  const cell = bloc.querySelector(".cell");
  if (!cell) {
    return;
  }
  const original = codeSource(cell);
  // La transcription se lit avant de regrouper le code, pour garder l'alternance commande, réponse
  const segmentsBuild = transcriptBuild(cell);
  const [sourceDiv, ...autresSources] = cell.querySelectorAll(":scope > div.sourceCode");
  autresSources.forEach(s => {
    sourceDiv.querySelector("pre code").append("\n", ...s.querySelector("pre code").childNodes);
    s.remove();
  });
  const paquets = (bloc.dataset.packages || "").split(/[\s,]+/).filter(Boolean);
  const hero = bloc.classList.contains("piege-hero");

  cell.querySelectorAll(":scope > .cell-output, :scope > .cell-output-display").forEach(s => s.remove());

  let sortie;
  let label;
  let contenu;
  if (hero) {
    sortie = el("div", { class: "piege-sortie", "aria-live": "polite" });
    label = el("p", { class: "piege-sortie-label", hidden: "" });
    contenu = el("div", { class: "piege-sortie-contenu" });
    sortie.append(label, contenu);
  } else {
    sortie = el("div", { class: "piege-sortie rgui", "aria-live": "polite" });
    const barreTitre = el("div", { class: "rgui-titre" });
    const boutons = el("span", { class: "rgui-boutons", "aria-hidden": "true" });
    boutons.append(el("span", { class: "rgui-bouton rgui-reduire" }), el("span", { class: "rgui-bouton rgui-agrandir" }), el("span", { class: "rgui-bouton rgui-fermer" }));
    label = el("span", { class: "rgui-nom" }, "R Console");
    barreTitre.append(el("span", { class: "rgui-icone", "aria-hidden": "true" }, "R"), label, boutons);
    contenu = el("div", { class: "rgui-client" });
    sortie.append(barreTitre, contenu);
  }

  function afficher(segments) {
    if (hero) {
      rendreHero(contenu, segments);
    } else {
      rendreConsole(contenu, segments);
    }
  }

  function titrer(texte) {
    if (hero) {
      label.textContent = texte ?? "";
      label.hidden = !texte;
    } else {
      label.textContent = texte ? `R Console (${texte})` : "R Console";
    }
  }

  afficher(segmentsBuild);

  const barre = el("div", { class: "piege-barre" });
  const executer = el("button", { type: "button", class: "bouton" }, hero ? T.verifier : T.executer);
  const modifier = el("button", { type: "button", class: "bouton bouton-discret" }, T.modifier);
  const retablir = el("button", { type: "button", class: "bouton bouton-discret", hidden: "" }, T.retablir);
  const version = el("select", { class: "choix-version", "aria-label": T.versionR });
  for (const x of VERSIONS_R) {
    version.append(el("option", { value: x.r }, `R ${x.r}`));
  }
  version.value = versionChoisie;
  selecteurs.add(version);
  const etat = el("span", { class: "piege-etat", role: "status" });
  barre.append(executer, version, modifier, retablir, etat);
  if (!hero) {
    barre.append(el("span", { class: "piege-raccourci" }, T.raccourci));
  }

  let editeur = null;

  function code() {
    return editeur ? editeur.value : original;
  }
  const precedents = [...blocsPage];
  blocsPage.push({ code, paquets });

  function ajusterHauteur() {
    editeur.style.height = "auto";
    editeur.style.height = editeur.scrollHeight + 2 + "px";
  }

  function ouvrirEditeur() {
    if (!editeur) {
      editeur = el("textarea", {
        class: "piege-editeur",
        spellcheck: "false",
        autocapitalize: "off",
        autocomplete: "off",
        "aria-label": T.codeModifiable,
        id: `piege-editeur-${index}`
      });
      editeur.value = original;
      editeur.addEventListener("input", ajusterHauteur);
      editeur.addEventListener("keydown", e => {
        if (e.key === "Enter" && (e.ctrlKey || e.metaKey)) {
          e.preventDefault();
          lancer();
        }
      });
      sourceDiv.replaceWith(editeur);
    }
    modifier.hidden = true;
    retablir.hidden = false;
    ajusterHauteur();
    editeur.focus();
  }

  function retablirBloc() {
    if (editeur) {
      editeur.replaceWith(sourceDiv);
      editeur = null;
    }
    afficher(segmentsBuild);
    titrer(null);
    sortie.classList.remove("est-navigateur");
    etat.textContent = "";
    modifier.hidden = false;
    retablir.hidden = true;
    modifier.focus();
  }

  async function lancer() {
    executer.disabled = true;
    selecteurs.forEach(s => { s.disabled = true; });
    const t0 = performance.now();
    try {
      etat.textContent = moteur && moteur.r === versionChoisie
        ? T.execution
        : T.demarrage(versionChoisie);
      const cible = VERSIONS_R.find(x => x.r === versionChoisie);
      const requis = [...new Set([...precedents.flatMap(b => b.paquets), ...paquets])];
      if (requis.length && !cible.paquets) {
        afficher([{ type: "sortie", texte: T.sansPaquet(requis, cible.r) }]);
        titrer(`R ${cible.r}`);
        sortie.classList.add("est-navigateur");
        retablir.hidden = false;
        etat.textContent = "";
        return;
      }
      const r = await demarrerR();
      for (const p of requis) {
        if (!r.paquets.has(p)) {
          etat.textContent = T.installation(p);
          await r.webR.installPackages([p], { quiet: true });
          r.paquets.add(p);
        }
      }
      etat.textContent = T.execution;
      await r.webR.objs.globalEnv.bind(".batard_code", code());
      await r.webR.objs.globalEnv.bind(".batard_prelude", precedents.map(b => b.code()).join("\n"));
      const texte = await r.webR.evalRString(".batard_run(.batard_code, prelude = .batard_prelude)");
      afficher(transcriptWebR(texte));
      titrer(`R ${r.version}`);
      sortie.classList.add("est-navigateur");
      retablir.hidden = false;
      etat.textContent = T.duree((performance.now() - t0) / 1000);
    } catch (e) {
      etat.textContent = T.echec(e.message);
    } finally {
      executer.disabled = false;
      selecteurs.forEach(s => { s.disabled = false; });
    }
  }

  version.addEventListener("change", () => {
    choisirVersion(version.value);
    arreterR();
    if (sortie.classList.contains("est-navigateur")) {
      lancer();
    } else {
      etat.textContent = T.prochaine(versionChoisie);
    }
  });
  executer.addEventListener("pointerenter", () => { demarrerR(); }, { once: true });
  executer.addEventListener("focus", () => { demarrerR(); }, { once: true });
  executer.addEventListener("click", lancer);
  modifier.addEventListener("click", ouvrirEditeur);
  retablir.addEventListener("click", retablirBloc);

  if (hero) {
    cell.append(sortie, barre);
  } else {
    cell.append(barre, sortie);
  }
}

const blocsPage = [];
document.querySelectorAll(".piege:not(.piege-statique)").forEach(preparerBloc);

// ------------------------------------------------------------------
// Catalogue de l'accueil : source du menu de gauche et des fiches voisines
// ------------------------------------------------------------------

// Certains hébergeurs servent les pages sans l'extension .html, et l'accueil sans index.html
function cheminNormalise(adresse) {
  return new URL(adresse, location.href).pathname
    .replace(/\.html$/, "")
    .replace(/\/index$/, "/")
    .replace(/\/+$/, "") || "/";
}
const ICI = cheminNormalise(location.href);

// Lu une seule fois par page : déjà là sur l'accueil, chargé ailleurs
const adresseAccueil = new URL(RACINE_LANGUE + "index.html", location.href);
const accueilPret = document.querySelector(".catalogue")
  ? Promise.resolve(document)
  : fetch(adresseAccueil).then(r => r.text()).then(html => new DOMParser().parseFromString(html, "text/html"));

const enteteFiche = document.querySelector(".fiche-entete");
if (enteteFiche) {
  ajouterSuite();
}

async function ajouterSuite() {
  try {
    // L'ordre de lecture est celui du catalogue de l'accueil
    const accueil = await accueilPret;
    const fiches = [...accueil.querySelectorAll(".catalogue-lien")].map(a => ({
      adresse: new URL(a.getAttribute("href"), adresseAccueil).href,
      titre: a.querySelector(".catalogue-titre")?.textContent ?? ""
    }));
    const position = fiches.findIndex(f => cheminNormalise(f.adresse) === ICI);
    if (position === -1 || fiches.length < 2) {
      return;
    }
    const precedent = fiches[(position - 1 + fiches.length) % fiches.length];
    const suivant = fiches[(position + 1) % fiches.length];
    const nav = el("nav", { class: "suite", "aria-label": T.voisines });

    // Boutons « Précédent » et « Suivant » de Read the Docs, avec le titre de la fiche
    const versPrecedent = el("a", { class: "suite-bouton suite-precedent", href: precedent.adresse, rel: "prev" });
    const labelPrecedent = el("span", { class: "suite-label" });
    labelPrecedent.append(el("i", { class: "bi bi-arrow-left-circle-fill", "aria-hidden": "true" }), " ", T.precedent);
    versPrecedent.append(labelPrecedent, el("span", { class: "suite-titre" }, precedent.titre));
    const versSuivant = el("a", { class: "suite-bouton suite-suivant", href: suivant.adresse, rel: "next" });
    const labelSuivant = el("span", { class: "suite-label" });
    labelSuivant.append(T.suivant, " ", el("i", { class: "bi bi-arrow-right-circle-fill", "aria-hidden": "true" }));
    versSuivant.append(labelSuivant, el("span", { class: "suite-titre" }, suivant.titre));
    const boutons = el("div", { class: "suite-boutons" });
    boutons.append(versPrecedent, versSuivant);

    const autres = fiches.filter((f, i) => i !== position);
    const hasard = el("a", { href: autres[Math.floor(Math.random() * autres.length)].adresse }, T.autreHasard);
    const retour = el("a", { href: RACINE_LANGUE + "index.html#catalogue" }, T.toutCatalogue);
    const liens = el("div", { class: "suite-liens" });
    liens.append(hasard, retour);
    nav.append(boutons, liens);
    document.getElementById("quarto-content").append(nav);
  } catch {
    // Sans accès à l'accueil, la fiche reste lisible sans navigation.
  }
}

// ------------------------------------------------------------------
// Filtres du catalogue
// ------------------------------------------------------------------

const IMPACTS = Object.entries(T.impacts);
// Outils qui détectent ou empêchent le piège (champ controle des fiches)
const CONTROLES = ["safer-r", "strict"];

function normaliser(s) {
  return s.normalize("NFD").replace(/[̀-ͯ]/g, "").toLowerCase();
}

const liste = document.querySelector(".catalogue");
const zoneFiltres = document.getElementById("filtres");

// Sur l'accueil, un piège tiré au hasard dans tout le catalogue
const chapo = document.querySelector(".accueil-chapo");
if (liste && chapo) {
  const bouton = el("button", { type: "button", class: "bouton bouton-hasard" }, T.hasard);
  bouton.addEventListener("click", () => {
    const liens = [...liste.querySelectorAll(".catalogue-lien")];
    location.href = liens[Math.floor(Math.random() * liens.length)].href;
  });
  chapo.after(bouton);
}

if (liste && zoneFiltres) {
  const lignes = [...liste.querySelectorAll(".catalogue-ligne")];
  const categories = [...new Set(lignes.map(l => l.dataset.categorie).filter(Boolean))];
  const controlesLigne = l => (l.dataset.controle || "").split(" ").filter(Boolean);
  const etatFiltre = { texte: "", categorie: null, impacts: new Set(), controles: new Set() };

  const recherche = el("div", { class: "recherche" });
  const champ = el("input", { type: "search", id: "recherche-piege", placeholder: T.exemple, autocomplete: "off" });
  recherche.append(el("label", { for: "recherche-piege" }, T.rechercher), champ);

  function groupe(titre) {
    const g = el("div", { class: "puces", role: "group", "aria-label": titre });
    g.append(el("span", { class: "puces-titre", "aria-hidden": "true" }, titre));
    return g;
  }

  const groupeCat = groupe(T.categorie);
  const toutes = el("button", { type: "button", class: "puce", "aria-pressed": "true" }, T.toutes);
  groupeCat.append(toutes);
  const boutonsCat = categories.map(c => {
    const b = el("button", { type: "button", class: "puce", "aria-pressed": "false" }, c);
    b.dataset.valeur = c;
    groupeCat.append(b);
    return b;
  });

  const groupeImpact = groupe(T.gravite);
  const boutonsImpact = IMPACTS
    .filter(([cle]) => lignes.some(l => l.dataset.impact === cle))
    .map(([cle, libelle]) => {
      const b = el("button", { type: "button", class: "puce", "aria-pressed": "false" }, libelle);
      b.dataset.valeur = cle;
      groupeImpact.append(b);
      return b;
    });

  const groupeControle = groupe(T.controle);
  const boutonsControle = CONTROLES
    .filter(outil => lignes.some(l => controlesLigne(l).includes(outil)))
    .map(outil => {
      const b = el("button", { type: "button", class: "puce", "aria-pressed": "false" }, outil);
      b.dataset.valeur = outil;
      groupeControle.append(b);
      return b;
    });

  const compteur = el("p", { class: "compteur", role: "status" });
  const vide = el("div", { class: "catalogue-vide", hidden: "" });
  const effacer = el("button", { type: "button", class: "bouton bouton-discret" }, T.effacer);
  vide.append(el("p", {}, T.aucun), effacer);

  zoneFiltres.append(recherche, groupeCat);
  if (boutonsImpact.length > 1) {
    zoneFiltres.append(groupeImpact);
  }
  if (boutonsControle.length) {
    zoneFiltres.append(groupeControle);
  }
  zoneFiltres.classList.add("filtres");
  liste.before(compteur);
  liste.after(vide);

  function appliquer() {
    const q = normaliser(etatFiltre.texte.trim());
    let n = 0;
    for (const l of lignes) {
      // La recherche porte sur la référence, le titre et l'extrait : « str » ne doit pas trouver l'étiquette strict
      const ok = (!q || normaliser(`${l.dataset.ref} ${l.querySelector(".catalogue-lien").textContent}`).includes(q))
        && (!etatFiltre.categorie || l.dataset.categorie === etatFiltre.categorie)
        && (!etatFiltre.impacts.size || etatFiltre.impacts.has(l.dataset.impact))
        && (!etatFiltre.controles.size || controlesLigne(l).some(c => etatFiltre.controles.has(c)));
      l.hidden = !ok;
      if (ok) {
        n += 1;
      }
    }
    liste.querySelectorAll(".catalogue-groupe").forEach(g => {
      g.hidden = !g.querySelector(".catalogue-ligne:not([hidden])");
    });
    compteur.textContent = T.compte(n, lignes.length);
    compteur.hidden = n === 0;
    vide.hidden = n !== 0;
    toutes.setAttribute("aria-pressed", String(!etatFiltre.categorie));
    boutonsCat.forEach(b => b.setAttribute("aria-pressed", String(b.dataset.valeur === etatFiltre.categorie)));
    boutonsImpact.forEach(b => b.setAttribute("aria-pressed", String(etatFiltre.impacts.has(b.dataset.valeur))));
    boutonsControle.forEach(b => b.setAttribute("aria-pressed", String(etatFiltre.controles.has(b.dataset.valeur))));
  }

  function basculer(ensemble, valeur) {
    if (ensemble.has(valeur)) {
      ensemble.delete(valeur);
    } else {
      ensemble.add(valeur);
    }
    appliquer();
  }

  champ.addEventListener("input", () => { etatFiltre.texte = champ.value; appliquer(); });
  toutes.addEventListener("click", () => { etatFiltre.categorie = null; appliquer(); });
  boutonsCat.forEach(b => b.addEventListener("click", () => {
    etatFiltre.categorie = etatFiltre.categorie === b.dataset.valeur ? null : b.dataset.valeur;
    appliquer();
  }));
  boutonsImpact.forEach(b => b.addEventListener("click", () => basculer(etatFiltre.impacts, b.dataset.valeur)));
  boutonsControle.forEach(b => b.addEventListener("click", () => basculer(etatFiltre.controles, b.dataset.valeur)));
  effacer.addEventListener("click", () => {
    etatFiltre.texte = "";
    etatFiltre.categorie = null;
    etatFiltre.impacts.clear();
    etatFiltre.controles.clear();
    champ.value = "";
    appliquer();
    champ.focus();
  });

  appliquer();
}

// ------------------------------------------------------------------
// Menu de gauche, à la Read the Docs (cadre dans assets/before-body.html)
// ------------------------------------------------------------------

const cote = document.getElementById("rtd-cote");
cote?.setAttribute("aria-label", T.navigation);

// Le titre ramène à l'accueil de la langue courante
document.querySelectorAll(".rtd-accueil").forEach(a => { a.href = RACINE_LANGUE + "index.html"; });

// Petits écrans : le bouton de la barre bleue ouvre et ferme le menu
const boutonMenu = document.querySelector(".rtd-menu-bouton");
const voile = document.querySelector(".rtd-voile");

function ouvrirMenu(ouvert) {
  document.body.classList.toggle("rtd-menu-ouvert", ouvert);
  boutonMenu?.setAttribute("aria-expanded", String(ouvert));
  if (voile) {
    voile.hidden = !ouvert;
  }
}

function fermerMenu(rendreFocus) {
  if (!document.body.classList.contains("rtd-menu-ouvert")) {
    return;
  }
  ouvrirMenu(false);
  if (rendreFocus) {
    boutonMenu?.focus();
  }
}

boutonMenu?.addEventListener("click", () => {
  const ouvert = !document.body.classList.contains("rtd-menu-ouvert");
  ouvrirMenu(ouvert);
  if (ouvert) {
    cote?.querySelector(".rtd-site")?.focus();
  }
});
voile?.addEventListener("click", () => fermerMenu(true));
document.addEventListener("keydown", e => {
  if (e.key === "Escape") {
    fermerMenu(true);
  }
});

// Titres de niveau 2 de la page en cours, sous son entrée du menu
function sectionsDeLaPage() {
  const liste = el("ul", { class: "rtd-toc" });
  document.querySelectorAll("#quarto-content section.level2[id] > h2").forEach(h => {
    const li = el("li", { class: "rtd-l3" });
    li.append(el("a", { href: "#" + h.parentElement.id }, h.textContent.trim()));
    liste.append(li);
  });
  return liste.children.length ? liste : null;
}

function marquerCourant(li, a) {
  li.classList.add("courant");
  a.setAttribute("aria-current", "page");
}

// Pages du site, puis les catégories du catalogue, dépliées sur la fiche en cours
async function construireMenu() {
  const menu = document.getElementById("rtd-menu");
  if (!menu) {
    return;
  }
  // Un clic sur une entrée referme le menu des petits écrans
  menu.addEventListener("click", e => {
    if (e.target.closest("a")) {
      fermerMenu(false);
    }
  });

  const pages = el("ul", { class: "rtd-liste" });
  const pagesSite = [
    { texte: T.menuCatalogue, href: RACINE_LANGUE + "index.html#catalogue", accueil: true },
    { texte: T.contribuer, href: RACINE_LANGUE + (LANGUE === "fr" ? "contribuer.html" : "contributing.html") }
  ];
  for (const page of pagesSite) {
    const li = el("li", { class: "rtd-l1" });
    const a = el("a", { href: page.href }, page.texte);
    li.append(a);
    if (cheminNormalise(page.href) === ICI) {
      marquerCourant(li, a);
      const sections = page.accueil ? null : sectionsDeLaPage();
      if (sections) {
        li.append(sections);
      }
    }
    pages.append(li);
  }
  menu.replaceChildren(pages);

  let accueil;
  try {
    accueil = await accueilPret;
  } catch {
    // Sans le catalogue, le menu garde les pages du site
    return;
  }

  const liste = el("ul", { class: "rtd-liste" });
  accueil.querySelectorAll(".catalogue-groupe").forEach((groupe, i) => {
    const lignes = [...groupe.querySelectorAll(".catalogue-ligne")];
    const li = el("li", { class: "rtd-l1" });
    const id = `rtd-categorie-${i + 1}`;
    const bouton = el("button", { type: "button", class: "rtd-categorie", "aria-expanded": "false", "aria-controls": id });
    bouton.append(el("span", { class: "rtd-libelle" }, groupe.dataset.categorie), el("span", { class: "rtd-compte" }, String(lignes.length)));
    const sous = el("ul", { class: "rtd-sous", id, hidden: "" });
    for (const ligne of lignes) {
      const source = ligne.querySelector(".catalogue-lien");
      const adresse = new URL(source.getAttribute("href"), adresseAccueil).href;
      const item = el("li", { class: "rtd-l2" });
      const a = el("a", { href: adresse });
      a.append(el("span", { class: "rtd-num" }, ligne.dataset.ref), el("span", { class: "rtd-libelle" }, source.querySelector(".catalogue-titre")?.textContent ?? ""));
      item.append(a);
      if (cheminNormalise(adresse) === ICI) {
        marquerCourant(item, a);
        li.classList.add("courant");
        const sections = sectionsDeLaPage();
        if (sections) {
          item.append(sections);
        }
      }
      sous.append(item);
    }
    const deplier = ouvert => {
      bouton.setAttribute("aria-expanded", String(ouvert));
      sous.hidden = !ouvert;
    };
    bouton.addEventListener("click", () => deplier(sous.hidden));
    deplier(li.classList.contains("courant"));
    li.append(bouton, sous);
    liste.append(li);
  });
  if (liste.children.length) {
    menu.append(el("p", { class: "rtd-legende", role: "heading", "aria-level": "2" }, T.categories), liste);
  }

  // La fiche en cours reste visible dans le menu, avec sa catégorie si la place le permet
  const courant = menu.querySelector(".rtd-l2.courant") ?? menu.querySelector(".rtd-l1.courant");
  const defilement = cote?.querySelector(".rtd-cote-defilement");
  if (courant && defilement) {
    const hauteur = defilement.clientHeight;
    const bas = courant.offsetTop + courant.offsetHeight;
    const categorie = courant.closest(".rtd-l1").offsetTop;
    if (bas > hauteur - 20) {
      defilement.scrollTop = bas - categorie <= hauteur - 40
        ? categorie - 8
        : courant.offsetTop - hauteur / 3;
    }
  }
}

construireMenu();

// Lien vers la source de la page sur GitHub, à droite du fil d'Ariane
function ajouterLienSource() {
  const fil = document.querySelector(".rtd-fil-liste");
  const depot = document.querySelector(".rtd-depot")?.href;
  if (!fil || !depot) {
    return;
  }
  const racine = new URL(offset, location.href).pathname;
  let chemin = location.pathname.startsWith(racine) ? location.pathname.slice(racine.length) : "";
  if (chemin === "" || chemin.endsWith("/")) {
    chemin += "index";
  }
  chemin = chemin.replace(/\.html$/, "") + ".qmd";
  const li = el("li", { class: "rtd-fil-source" });
  const a = el("a", { href: `${depot.replace(/\/$/, "")}/blob/main/${chemin}` });
  a.append(el("i", { class: "bi bi-github", "aria-hidden": "true" }), T.source);
  li.append(a);
  fil.append(li);
}

ajouterLienSource();

// ------------------------------------------------------------------
// Choix de la langue
// ------------------------------------------------------------------

ajouterChoixLangue();

// Drapeau de la langue visée, en SVG : les émojis drapeaux ne s'affichent pas sous Windows
function drapeau(langue) {
  const span = el("span", { class: "drapeau", "aria-hidden": "true" });
  span.innerHTML = langue === "fr"
    ? '<svg viewBox="0 0 3 2" preserveAspectRatio="none"><path fill="#002395" d="M0 0h1v2H0z"/><path fill="#fff" d="M1 0h1v2H1z"/><path fill="#ed2939" d="M2 0h1v2H2z"/></svg>'
    : '<svg viewBox="0 0 60 30" preserveAspectRatio="none"><clipPath id="drapeau-uk"><path d="M30 15h30v15zv15H0zH0V0zV0h30z"/></clipPath><path fill="#012169" d="M0 0v30h60V0z"/><path stroke="#fff" stroke-width="6" d="M0 0l60 30m0-30L0 30"/><path stroke="#c8102e" stroke-width="4" clip-path="url(#drapeau-uk)" d="M0 0l60 30m0-30L0 30"/><path stroke="#fff" stroke-width="10" d="M30 0v30M0 15h60"/><path stroke="#c8102e" stroke-width="6" d="M30 0v30M0 15h60"/></svg>';
  return span;
}

async function ajouterChoixLangue() {
  const autre = LANGUE === "fr" ? offset : offset + "fr/";
  const lien = el("a", { class: "choix-langue", href: autre + "index.html", lang: LANGUE === "fr" ? "en" : "fr", hreflang: LANGUE === "fr" ? "en" : "fr", title: T.autreLangueTitre }, T.autreLangue);
  lien.prepend(drapeau(LANGUE === "fr" ? "en" : "fr"));
  const zone = document.querySelector(".rtd-version");
  if (!zone) {
    return;
  }
  zone.append(lien);
  try {
    localStorage.setItem("excuse-my-r.langue", LANGUE);
  } catch {
    // Sans stockage, le choix n'est simplement pas retenu
  }
  // Sur une fiche, viser la même fiche dans l'autre langue, retrouvée par sa référence
  if (/\/(contributing|contribuer)(\.html)?$/.test(location.pathname)) {
    lien.href = autre + (LANGUE === "fr" ? "contributing.html" : "contribuer.html");
    return;
  }
  const ref = document.querySelector(".fiche-entete")?.dataset.ref;
  if (!ref) {
    return;
  }
  try {
    const adresseAutre = new URL(autre + "index.html", location.href);
    const html = await fetch(adresseAutre).then(r => r.text());
    const accueil = new DOMParser().parseFromString(html, "text/html");
    const ligne = accueil.querySelector(`.catalogue-ligne[data-ref="${CSS.escape(ref)}"] .catalogue-lien`);
    if (ligne) {
      lien.href = new URL(ligne.getAttribute("href"), adresseAutre).href;
    }
  } catch {
    // Le lien vers l'accueil de l'autre langue reste valable
  }
}
