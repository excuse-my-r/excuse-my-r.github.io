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

.batard_run <- function(code, prelude = "") {
  options_avant <- options()
  # Avant R 4.2, ces options absentes de options() gardent leur effet si on les remet à NULL
  drapeaux <- c("warnPartialMatchDollar", "warnPartialMatchArgs", "warnPartialMatchAttr")
  drapeaux_avant <- lapply(drapeaux, getOption, default = FALSE)
  on.exit({
    nouvelles <- setdiff(names(options()), names(options_avant))
    options(stats::setNames(vector("list", length(nouvelles)), nouvelles))
    options(options_avant)
    options(stats::setNames(drapeaux_avant, drapeaux))
  }, add = TRUE)
  env <- new.env(parent = globalenv())
  invisible(utils::capture.output({
    for (morceau in .batard_decouper(prelude)) {
      if (!inherits(morceau$exprs, "error")) {
        for (e in morceau$exprs) {
          try(suppressWarnings(suppressMessages(eval(e, envir = env))), silent = TRUE)
        }
      }
    }
  }))
  afficher_erreur <- function(err) {
    appel <- conditionCall(err)
    if (is.null(appel)) {
      cat("Error: ", conditionMessage(err), "\\n", sep = "")
    } else {
      cat("Error in ", deparse(appel, nlines = 1L), " : ", conditionMessage(err), "\\n", sep = "")
    }
  }
  sortie <- utils::capture.output({
    for (morceau in .batard_decouper(code)) {
      lignes <- morceau$lignes
      invites <- c("> ", rep("+ ", length(lignes) - 1L))
      cat(paste0("\\001", invites, lignes, "\\n"), sep = "")
      if (inherits(morceau$exprs, "error")) {
        cat(.batard_erreur_syntaxe(morceau$exprs, lignes = lignes), "\\n", sep = "")
        next
      }
      for (e in morceau$exprs) {
        withCallingHandlers(
          tryCatch({
            v <- withVisible(eval(e, envir = env))
            if (v$visible) {
              print(v$value)
            }
          }, error = afficher_erreur),
          warning = function(w) {
            appel <- conditionCall(w)
            if (is.null(appel)) {
              cat("Warning message:\\n", conditionMessage(w), "\\n", sep = "")
            } else {
              cat("Warning message:\\nIn ", deparse(appel, nlines = 1L), " : ", conditionMessage(w), "\\n", sep = "")
            }
            invokeRestart("muffleWarning")
          },
          message = function(m) {
            cat(conditionMessage(m), sep = "")
            invokeRestart("muffleMessage")
          }
        )
      }
    }
  }, type = "output")
  paste(sortie, collapse = "\\n")
}
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
  return code.split("\n").flatMap(ligne => {
    if (!ouvert && ligne.trim() === "") {
      return [];
    }
    const invite = ouvert ? "+ " : "> ";
    let chaine = null;
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
        .replace(/^Error in `([\s\S]*?)`:\n! /gm, "Error in $1 : ")
        .replace(/^Error:\n! /gm, "Error: ")
        .replace(/^Warning in (.*?): /gm, "Warning message:\nIn $1 : ")
        .replace(/^Warning: /gm, "Warning message:\n");
      segments.push({ type: "sortie", texte });
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
  const executer = el("button", { type: "button", class: "bouton" }, hero ? "Vérifier dans votre navigateur" : "Exécuter");
  const modifier = el("button", { type: "button", class: "bouton bouton-discret" }, "Modifier le code");
  const retablir = el("button", { type: "button", class: "bouton bouton-discret", hidden: "" }, "Rétablir");
  const version = el("select", { class: "choix-version", "aria-label": "Version de R" });
  for (const x of VERSIONS_R) {
    version.append(el("option", { value: x.r }, `R ${x.r}`));
  }
  version.value = versionChoisie;
  selecteurs.add(version);
  const etat = el("span", { class: "piege-etat", role: "status" });
  barre.append(executer, version, modifier, retablir, etat);
  if (!hero) {
    barre.append(el("span", { class: "piege-raccourci" }, "Ctrl + Entrée pour exécuter"));
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
        "aria-label": "Code R modifiable",
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
        ? "Exécution..."
        : `Démarrage de R ${versionChoisie}. La première fois, comptez quelques secondes.`;
      const cible = VERSIONS_R.find(x => x.r === versionChoisie);
      const requis = [...new Set([...precedents.flatMap(b => b.paquets), ...paquets])];
      if (requis.length && !cible.paquets) {
        afficher([{ type: "sortie", texte: `Ce bloc a besoin de {${requis.join("}, {")}}, qui n'existe plus pour R ${cible.r} dans le navigateur. Choisissez R 4.3.0 ou une version plus récente.` }]);
        titrer(`R ${cible.r}`);
        sortie.classList.add("est-navigateur");
        retablir.hidden = false;
        etat.textContent = "";
        return;
      }
      const r = await demarrerR();
      for (const p of requis) {
        if (!r.paquets.has(p)) {
          etat.textContent = `Installation du paquet {${p}}...`;
          await r.webR.installPackages([p], { quiet: true });
          r.paquets.add(p);
        }
      }
      etat.textContent = "Exécution...";
      await r.webR.objs.globalEnv.bind(".batard_code", code());
      await r.webR.objs.globalEnv.bind(".batard_prelude", precedents.map(b => b.code()).join("\n"));
      const texte = await r.webR.evalRString(".batard_run(.batard_code, prelude = .batard_prelude)");
      afficher(transcriptWebR(texte));
      titrer(`R ${r.version}`);
      sortie.classList.add("est-navigateur");
      retablir.hidden = false;
      etat.textContent = `Exécuté en ${((performance.now() - t0) / 1000).toFixed(1).replace(".", ",")} s.`;
    } catch (e) {
      etat.textContent = `R n'a pas pu démarrer dans ce navigateur (${e.message}).`;
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
      etat.textContent = `R ${versionChoisie} sera chargé à la prochaine exécution.`;
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

// Sur une fiche, le premier bloc est le piege lui-meme : sa reponse est marquee
const enteteFiche = document.querySelector(".fiche-entete");
if (enteteFiche) {
  ajouterSuite();
}

async function ajouterSuite() {
  try {
    // L'ordre de lecture est celui du catalogue de l'accueil
    const html = await fetch(offset + "index.html").then(r => r.text());
    const accueil = new DOMParser().parseFromString(html, "text/html");
    const fiches = [...accueil.querySelectorAll(".catalogue-lien")].map(a => ({
      chemin: a.getAttribute("href").replace(/^\.\//, ""),
      titre: a.querySelector(".catalogue-titre")?.textContent ?? ""
    }));
    // Certains hébergeurs servent les pages sans l'extension .html
    const sansExtension = chemin => chemin.replace(/\.html$/, "");
    const ici = sansExtension(location.pathname.split("/").slice(-2).join("/"));
    const position = fiches.findIndex(f => sansExtension(f.chemin) === ici);
    if (position === -1) {
      return;
    }
    const suivant = fiches[(position + 1) % fiches.length];
    const nav = el("nav", { class: "suite", "aria-label": "Fiches voisines" });
    const lien = el("a", { class: "suite-lien", href: offset + suivant.chemin });
    lien.append(el("span", { class: "suite-label" }, "Piège suivant"), el("span", { class: "suite-titre" }, suivant.titre));
    const retour = el("a", { class: "suite-retour", href: offset + "index.html#catalogue" }, "Tout le catalogue");
    nav.append(lien, retour);
    document.getElementById("quarto-content").append(nav);
  } catch {
    // Sans accès à l'accueil, la fiche reste lisible sans navigation.
  }
}

// ------------------------------------------------------------------
// Filtres du catalogue
// ------------------------------------------------------------------

const IMPACTS = [
  ["silencieux", "Faux en silence"],
  ["cryptique", "Erreur obscure"],
  ["incoherent", "Incohérence"]
];

function normaliser(s) {
  return s.normalize("NFD").replace(/[̀-ͯ]/g, "").toLowerCase();
}

const liste = document.querySelector(".catalogue");
const zoneFiltres = document.getElementById("filtres");

if (liste && zoneFiltres) {
  const lignes = [...liste.querySelectorAll(".catalogue-ligne")];
  const categories = [...new Set(lignes.map(l => l.dataset.categorie).filter(Boolean))];
  const etatFiltre = { texte: "", categorie: null, impacts: new Set() };

  const recherche = el("div", { class: "recherche" });
  const champ = el("input", { type: "search", id: "recherche-piege", placeholder: "ifelse, NA, facteur...", autocomplete: "off" });
  recherche.append(el("label", { for: "recherche-piege" }, "Rechercher un piège"), champ);

  function groupe(titre) {
    const g = el("div", { class: "puces", role: "group", "aria-label": titre });
    g.append(el("span", { class: "puces-titre", "aria-hidden": "true" }, titre));
    return g;
  }

  const groupeCat = groupe("Catégorie");
  const toutes = el("button", { type: "button", class: "puce", "aria-pressed": "true" }, "Toutes");
  groupeCat.append(toutes);
  const boutonsCat = categories.map(c => {
    const b = el("button", { type: "button", class: "puce", "aria-pressed": "false" }, c);
    b.dataset.valeur = c;
    groupeCat.append(b);
    return b;
  });

  const groupeImpact = groupe("Gravité");
  const boutonsImpact = IMPACTS
    .filter(([cle]) => lignes.some(l => l.dataset.impact === cle))
    .map(([cle, libelle]) => {
      const b = el("button", { type: "button", class: "puce", "aria-pressed": "false" }, libelle);
      b.dataset.valeur = cle;
      groupeImpact.append(b);
      return b;
    });

  const compteur = el("p", { class: "compteur", role: "status" });
  const vide = el("div", { class: "catalogue-vide", hidden: "" });
  const effacer = el("button", { type: "button", class: "bouton bouton-discret" }, "Effacer les filtres");
  vide.append(el("p", {}, "Aucun piège ne correspond à ces filtres."), effacer);

  zoneFiltres.append(recherche, groupeCat);
  if (boutonsImpact.length > 1) {
    zoneFiltres.append(groupeImpact);
  }
  zoneFiltres.classList.add("filtres");
  liste.before(compteur);
  liste.after(vide);

  function appliquer() {
    const q = normaliser(etatFiltre.texte.trim());
    let n = 0;
    for (const l of lignes) {
      const ok = (!q || normaliser(l.textContent).includes(q))
        && (!etatFiltre.categorie || l.dataset.categorie === etatFiltre.categorie)
        && (!etatFiltre.impacts.size || etatFiltre.impacts.has(l.dataset.impact));
      l.hidden = !ok;
      if (ok) {
        n += 1;
      }
    }
    liste.querySelectorAll(".catalogue-groupe").forEach(g => {
      g.hidden = !g.querySelector(".catalogue-ligne:not([hidden])");
    });
    compteur.textContent = n === lignes.length
      ? `${n} pièges`
      : `${n} ${n > 1 ? "pièges" : "piège"} sur ${lignes.length}`;
    compteur.hidden = n === 0;
    vide.hidden = n !== 0;
    toutes.setAttribute("aria-pressed", String(!etatFiltre.categorie));
    boutonsCat.forEach(b => b.setAttribute("aria-pressed", String(b.dataset.valeur === etatFiltre.categorie)));
    boutonsImpact.forEach(b => b.setAttribute("aria-pressed", String(etatFiltre.impacts.has(b.dataset.valeur))));
  }

  champ.addEventListener("input", () => { etatFiltre.texte = champ.value; appliquer(); });
  toutes.addEventListener("click", () => { etatFiltre.categorie = null; appliquer(); });
  boutonsCat.forEach(b => b.addEventListener("click", () => {
    etatFiltre.categorie = etatFiltre.categorie === b.dataset.valeur ? null : b.dataset.valeur;
    appliquer();
  }));
  boutonsImpact.forEach(b => b.addEventListener("click", () => {
    const v = b.dataset.valeur;
    if (etatFiltre.impacts.has(v)) {
      etatFiltre.impacts.delete(v);
    } else {
      etatFiltre.impacts.add(v);
    }
    appliquer();
  }));
  effacer.addEventListener("click", () => {
    etatFiltre.texte = "";
    etatFiltre.categorie = null;
    etatFiltre.impacts.clear();
    champ.value = "";
    appliquer();
    champ.focus();
  });

  appliquer();
}
