// SCALA - Labo 2
// Nicolas Crausaz & Maxime Scharwath

package Chat

enum Token:
  case
    BONJOUR,
    JE,
    SVP,
    ASSOIFFE,
    AFFAME,
    QUEL,
    LE,
    PRIX,
    DE,
    COMBIEN,
    COUTER,
    COMMANDER,
    CONNAITRE,
    MON,
    ME,
    SOLDE,
    APPELLER,
    // Actions
    ETRE,
    VOULOIR,
    // Logic Operators
    ET,
    OU,
    // Products
    PRODUIT,
    MARQUE,
    // Util
    PSEUDO,
    NUM,
    EOL,
    UNKNOWN,
    BAD
end Token
