# SCALA - Labo 4 - Bot-Tender : Future

> Nicolas Crausaz & Maxime Scharwath

# Informations

Dans les itérations précèdentes de Bot-tender, on supposait une commande prête immédiatement. L’objectif
de ce labo est d’intégrer une gestion de commandes asynchrones en utilisant la classe Future de
l’API Scala.

# Choix architecturaux

TODO: En plus du code source, il faut rendre un fichier README contenant une explication de vos choix
architecturaux et d’implémentation (pas plus que 2 pages).

N’utilisez pas Future.onComplete ou Await.


Pour gérer les réponses immédiates et les réponses asynchrones, nous avons décidé de retourner un tuple dans la fonction reply (réponse immédiate, Réponse future)