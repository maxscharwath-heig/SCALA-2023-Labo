// SCALA - Labo 2
// Nicolas Crausaz & Maxime Scharwath

package Utils

/** Contains the dictionary of the application, which is used to validate,
  * correct and normalize words entered by the user.
  */
object Dictionary:
  // This dictionary is a Map object that contains valid words as keys and their normalized equivalents as values (e.g.
  // we want to normalize the words "veux" and "aimerais" in one unique term: "vouloir").
  val dictionary: Map[String, String] = Map(
    "bonjour" -> "bonjour",
    "hello" -> "bonjour",
    "yo" -> "bonjour",
    "salut" -> "bonjour",
    "je" -> "je",
    "j" -> "je",
    "suis" -> "etre",
    "est" -> "etre",
    "ai" -> "etre",
    "veux" -> "vouloir",
    "voudrais" -> "vouloir",
    "aimerais" -> "vouloir",
    "assoiffé" -> "assoiffe",
    "assoiffée" -> "assoiffe",
    "soif" -> "assoiffe",
    "affamé" -> "affame",
    "affamée" -> "affame",
    "faim" -> "affame",
    "bière" -> "biere",
    "bières" -> "biere",
    "croissant" -> "croissant",
    "croissants" -> "croissant",
    "et" -> "et",
    "ou" -> "ou",
    "svp" -> "svp",
    "stp" -> "svp",
    "quel" -> "quel",
    "quelle" -> "quel",
    "quels" -> "quel",
    "quelles" -> "quel",
    "le" -> "le",
    "la" -> "le",
    "les" -> "le",
    "prix" -> "prix",
    "de" -> "de",
    "combien" -> "combien",
    "coute" -> "couter",
    "coutent" -> "couter",
    "commander" -> "commander",
    "commandez" -> "commander",
    "connaître" -> "connaître",
    "connais" -> "connaître",
    "mon" -> "mon",
    "ma" -> "mon",
    "mes" -> "mon",
    "me" -> "me",
    "m" -> "me",
    "solde" -> "solde",
    "appeler" -> "appeler",
    "appelle" -> "appeler",
    "appelles" -> "appeler",
    "maison" -> "maison",
    "maisons" -> "maison",
    "cailler" -> "cailler",
    "farmer" -> "farmer",
    "boxer" -> "boxer",
    "wittekop" -> "wittekop",
    "punkipa" -> "punkipa",
    "jackhammer" -> "jackhammer",
    "ténébreuse" -> "tenebreuse"
  )
end Dictionary
