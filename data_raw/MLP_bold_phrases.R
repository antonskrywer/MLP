# data_raw/MLP_bold_phrases.R
# Schlüsselwörter je Antwortstufe (L3) für die Fettung der MLP-Antwortkarten.
# Wird von prepare_MLP_from_xlsx.R eingelesen (source) und über mlp_apply_bold()
# als **...** in die Dictionary-Texte geschrieben. Reihenfolge: Stufe 1 bis 5.
# Deutsche Wortgruppen:
P_de <- list(
"0000" = c("kaum musikalisches Verständnis","nur ansatzweise ein musikalisches Verständnis","teilweise ein musikalisches Verständnis","weitgehend ein musikalisches Verständnis","durchgehend ein tiefes musikalisches Verständnis"),
"0001" = c("kein erkennbares Verständnis","nur ansatzweise ein Verständnis","ein grundlegendes Verständnis","weitgehend ein Verständnis","durchgehend ein differenziertes und sicheres Verständnis"),
"0002" = c("nicht authentisch oder emotional","nur ansatzweise authentisch und emotional","teilweise authentisch und emotional","weitgehend authentisch und emotional","durchgehend authentisch und emotional berührend"),
"0003" = c("überwiegend nicht","nur teilweise","überwiegend","dem für dieses Leistungsniveau definierten Standard","durchgehend dem für dieses Leistungsniveau definierten Standard"),
"0004" = c("überzeugt überwiegend nicht","überzeugt nur eingeschränkt","überzeugt teilweise","überzeugt insgesamt","überzeugt durchgehend auf hohem Niveau"),
"0005" = c("durchgehend unsicher","häufige Abweichungen","überwiegend treffsicher","weitgehend treffsicher","durchgehend treffsicher"),
"0006" = c("meist unklar oder ungenau","häufig ungenau","überwiegend erkennbar","weitgehend klar und präzise","durchgehend klar und präzise"),
"0007" = c("durchgehend inkonsistent","schwankt häufig","teilweise konsistent","weitgehend konsistent und angemessen","durchgehend konsistent"),
"0009" = c("überwiegend uneinheitlich","weicht häufig","teilweise","weitgehend","durchgehend einheitlich"),
"0010" = c("überwiegend dünn, wenig tragfähig","häufig wenig tragfähig","teilweise voll und charakteristisch","weitgehend voll und charakteristisch","durchgehend voll, tragfähig und charakteristisch"),
"0011" = c("überwiegend nicht","häufig nicht","teilweise","weitgehend angemessen","durchgehend angemessen"),
"0012" = c("überwiegend unökonomisch","häufig unökonomisch","teilweise ökonomisch","weitgehend ökonomisch","durchgehend ökonomisch"),
"0013" = c("überwiegend unkontrolliert","häufig unkontrolliert","teilweise kontrolliert","weitgehend kontrolliert","durchgehend kontrolliert"),
"0014" = c("überwiegend unsauber","häufig unsauber","teilweise klar","weitgehend klar und sauber","durchgehend klar und sauber"),
"0015" = c("überwiegend ungenau und geräuschbehaftet","häufig ungenau oder geräuschbehaftet","teilweise präzise","weitgehend präzise und geräuschlos","durchgehend präzise und geräuschlos"),
"0016" = c("durchgehend instabil","schwanken häufig","in der mittleren Lage stabil","weitgehend stabil","durchgehend in allen Lagen"),
"0017" = c("überwiegend dünn und nicht resonant","häufig dünn oder wenig resonant","teilweise voll und resonant","weitgehend voll und resonant","durchgehend voll"),
"0018" = c("überwiegend unkoordiniert und spannungsbehaftet","häufig unkoordiniert oder verspannt","teilweise koordiniert","weitgehend koordiniert und spannungsfrei","durchgehend koordiniert und spannungsfrei"),
"0019" = c("überwiegend unsauber und geräuschbehaftet","häufig unsauber oder geräuschbehaftet","teilweise sauber","weitgehend sauber","durchgehend sauber und geräuschlos"),
"0020" = c("kaum eine Differenzierung","häufig unklar","teilweise Differenzierung","weitgehend eine gute Differenzierung","durchgehend eine klare Differenzierung"),
"0021" = c("überwiegend ungenau und unsauber","häufig ungenau oder unsauber","teilweise sauber und präzise","weitgehend saubere und präzise","durchgehend eine saubere und präzise"),
"0022" = c("überwiegend instabil","häufig instabil","teilweise konstant","weitgehend konstant","durchgehend konstant"),
"0023" = c("überwiegend instabil","häufig instabil","teilweise stabil","weitgehend stabil","durchgehend stabil"),
"0024" = c("überwiegend unsynchron","häufig unsynchron","teilweise synchronisiert","weitgehend synchronisiert","durchgehend präzise synchronisiert"),
"0025" = c("durchgehend deutlich ab","häufig deutlich ab","teilweise konstant","weitgehend konstant","durchgehend konstant"),
"0026" = c("überwiegend unausgeglichen","häufig unausgeglichen","teilweise angemessen","weitgehend angemessen","durchgehend mit angemessenem Gewicht"),
"0027" = c("überwiegend mit hörbaren Unterbrechungen","häufig mit hörbaren Unterbrechungen","teilweise fließend","weitgehend fließend","durchgehend fließend"),
"0028" = c("überwiegend unsauber","häufig unsauber","teilweise sauber","weitgehend sauber","durchgehend sauber"),
"0029" = c("überwiegend ungenau","häufige Ungenauigkeiten","teilweise präzise","weitgehend präzise","durchgehend präzise"),
"0030" = c("überwiegend ungleichmäßig oder fehlt","häufig ungleichmäßig","teilweise gleichmäßig","weitgehend gleichmäßig und ausdrucksvoll","durchgehend gleichmäßig und ausdrucksvoll"),
"0031" = c("überwiegend nicht aufeinander abgestimmt","häufig nicht gut aufeinander abgestimmt","teilweise aufeinander abgestimmt","weitgehend aufeinander abgestimmt","durchgehend präzise aufeinander abgestimmt"),
"0032" = c("überwiegend ineffizient","häufig ineffizient","teilweise effizient","weitgehend effizient","durchgehend effizient"),
"0033" = c("überwiegend nicht","häufig nicht klar","teilweise","weitgehend","durchgehend klar"),
"0035" = c("durchgehend deutlich unter","häufig deutlich unter","teilweise dem vorgegebenen Tempo","weitgehend dem vorgegebenen Tempo","durchgehend dem vorgegebenen Tempo"),
"0036" = c("durchgehend nach Noten","nur in kurzen Abschnitten auswendig","teilweise auswendig","weitgehend auswendig","durchgehend sicher und vollständig auswendig"),
"0037" = c("durchgehend zu Fehlern","häufig zu Fehlern","teilweise beeinträchtigt","weitgehend stabil","durchgehend stabil"),
"0038" = c("vollständig auf dem Treffen der richtigen Töne","überwiegend auf dem Treffen der richtigen Töne","teilweise auf musikalischer Gestaltung","weitgehend auf musikalischer Gestaltung","durchgehend auf musikalischer Gestaltung")
)

# Englische Entsprechungen (gleiche Reihenfolge der Stufen 1-5)
P_en <- list(
"0000" = c("hardly any musical understanding","only a rudimentary musical understanding","a partial musical understanding","largely conveys a musical understanding","consistently conveys a deep musical understanding"),
"0001" = c("no recognisable understanding","only a rudimentary understanding","a basic understanding","largely shows an understanding","consistently shows a nuanced and confident understanding"),
"0002" = c("does not seem authentic or emotional","only slightly authentic and emotional","partly authentic and emotional","largely authentic and emotional","consistently authentic and emotionally moving"),
"0003" = c("predominantly does not meet","only partially meets","predominantly meets","meets the standard defined for this level of achievement","consistently meets the standard defined for this level of achievement"),
"0004" = c("is predominantly not convincing","is only somewhat convincing","is partially convincing","is convincing overall","is consistently convincing at a high level"),
"0005" = c("consistently inaccurate","frequent deviations","predominantly accurate","largely accurate","consistently accurate"),
"0006" = c("mostly played unclearly or imprecisely","often imprecise","predominantly recognisable","largely clear and precise","consistently played clearly and precisely"),
"0007" = c("consistently inconsistent","fluctuates frequently","partially consistent","largely consistent and appropriate","consistently consistent"),
"0009" = c("predominantly inconsistent","frequently deviates","partially","largely","consistently uniform"),
"0010" = c("predominantly thin, lacks resonance","often lacking in resonance","partially full and characteristic","largely full and characteristic","consistently full, resonant and characteristic"),
"0011" = c("predominantly insufficient","often insufficient","partially","largely appropriate","consistently appropriate"),
"0012" = c("predominantly uneconomical","often uneconomical","partially economical","largely economical","consistently economical"),
"0013" = c("predominantly used without control","often used without control","partially used with control","largely used with control","consistently used with control"),
"0014" = c("predominantly unclean","often unclean","partially clear","largely clear and clean","consistently clear and clean"),
"0015" = c("predominantly imprecise and audible","often imprecise or audible","partially precise","largely precise and silent","consistently precise and silent"),
"0016" = c("consistently unstable","frequently fluctuate","stable in the middle register","largely remain stable","consistently remain even and stable across all registers"),
"0017" = c("predominantly thin and lacking resonance","often thin or lacks resonance","partially full and resonant","largely full and resonant","consistently full"),
"0018" = c("predominantly uncoordinated and tense","often uncoordinated or tense","partially coordinated","largely coordinated and relaxed","consistently coordinated and relaxed"),
"0019" = c("predominantly unclean and audible","often unclean or audible","partially performed cleanly","largely performed cleanly","consistently performed cleanly and silently"),
"0020" = c("hardly any distinction","often unclear","a partial distinction","largely a good distinction","consistently a clear distinction"),
"0021" = c("predominantly imprecise and unclean","often imprecise or unclean","partially clean and precise","largely has a clean and precise","consistently has a clean and precise"),
"0022" = c("predominantly unstable","often unstable","partially consistent","largely consistent","consistently steady"),
"0023" = c("predominantly unstable","often unstable","partially stable","largely stable","consistently stable"),
"0024" = c("predominantly unsynchronised","often unsynchronised","partially synchronised","largely synchronised","consistently precisely synchronised"),
"0025" = c("consistently decline noticeably","often decline noticeably","partially remain consistent","largely remain consistent","consistently steady"),
"0026" = c("predominantly uneven","often uneven","partially guided appropriately","largely guided appropriately","consistently guided with the right amount of pressure"),
"0027" = c("predominantly associated with audible breaks","often associated with audible breaks","partially performed smoothly","largely performed smoothly","consistently performed smoothly"),
"0028" = c("predominantly uncleanly","often works uncleanly","partially cleanly","largely works cleanly","consistently works cleanly"),
"0029" = c("predominantly imprecise","frequently shows inaccuracies","partially precise","largely precise","consistently precise"),
"0030" = c("predominantly uneven or absent","often uneven","partially even","largely even and expressive","consistently even and expressive"),
"0031" = c("predominantly uncoordinated","often not well coordinated","partially coordinated","largely coordinated","consistently perfectly coordinated"),
"0032" = c("predominantly inefficient","often inefficient","partially efficient","largely efficient","consistently efficient"),
"0033" = c("predominantly does not stand out","often does not clearly stand out","partially stands out","largely stands out","consistently stands out clearly"),
"0035" = c("consistently well below","often well below","partially matches the indicated tempo","largely matches the indicated tempo","consistently matches the tempo indicated by the piece"),
"0036" = c("consistently played from sheet music","played from memory only in short sections","partially played from memory","largely played from memory","consistently played confidently and entirely from memory"),
"0037" = c("consistently lead to mistakes","often lead to mistakes","partially impaired","largely remains stable","consistently remains stable"),
"0038" = c("entirely on hitting the right notes","predominantly on hitting the right notes","partially on musical shaping","largely on musical shaping","consistently on musical shaping")
)

# L1/L2: Vergleichswort am Satzanfang (für alle Items gleich)
P_cmp <- list(
  de = c("Deutlich schlechter", "Schlechter", "Gleich", "Besser", "Viel besser"),
  en = c("Clearly worse", "Worse", "The same", "Better", "Much better")
)

# Sprachlich geänderte Texte (Quelle: Abstimmung Oktober 2026; die xlsx bleibt unverändert)
text_overrides <- list(
  TMLP_0025_L3_CHOICE5 = c(
    de = "Klangqualität und Intonation bleiben durchgehend konstant, auch in der oberen Lage und gegen Ende der Darbietung.",
    en = "Sound quality and intonation remain consistently steady, also in the upper register and towards the end of the performance."
  )
)

# Setzt Overrides ein und markiert pro Antwort die Stufen-Schlüsselwörter mit **...**
# (mlp.js wandelt das auf den Antwortkarten in Fettdruck um).
mlp_apply_bold <- function(dict) {
  wrap <- function(txt, phrase, key) {
    p <- regexpr(phrase, txt, fixed = TRUE)
    if (p < 0) stop("Phrase nicht gefunden: ", key, " -> ", phrase)
    paste0(substr(txt, 1, p - 1), "**", phrase, "**", substr(txt, p + nchar(phrase), nchar(txt)))
  }
  for (k in names(text_overrides)) {
    i <- which(dict$key == k)
    dict$de[i] <- text_overrides[[k]][["de"]]
    dict$en[i] <- text_overrides[[k]][["en"]]
  }
  for (i in seq_len(nrow(dict))) {
    m <- regmatches(dict$key[i], regexec("^TMLP_([0-9]{4})_(L[123])_CHOICE([1-5])$", dict$key[i]))[[1]]
    if (length(m) == 0) next
    item <- m[2]; lvl <- m[3]; k <- as.integer(m[4])
    if (lvl == "L3") {
      dict$de[i] <- wrap(dict$de[i], P_de[[item]][k], dict$key[i])
      dict$en[i] <- wrap(dict$en[i], P_en[[item]][k], dict$key[i])
    } else {
      dict$de[i] <- wrap(dict$de[i], P_cmp$de[k], dict$key[i])
      dict$en[i] <- wrap(dict$en[i], P_cmp$en[k], dict$key[i])
    }
  }
  dict
}
