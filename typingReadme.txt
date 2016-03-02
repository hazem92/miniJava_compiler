typing: 

les fichiers Compiling/typeEnv.ml et Compiling/typeExec.ml utilisés pour construire, à partir de l'ast, l'environnement de typages.
- un environnement global qui contient les classes et vérifie les noms et l'héritage
- un environnement local pour chaque classe qui verifie les attributs les constructeurs et les méthodes

les fichiers Typing/ASTType.ml, Typing/ASTTypeEnv.ml et Typing/ASTSolver.ml permette de continuer le travail de compilation en se basant sur l'environnement pour verfier l'exactitude du contenu des classes et des methodes. 
en sortie on obtient un ast typé.

on a ajouté un autre argument au programme "-e" (execution only) pour contourner la compilation. 
-> cette option est ajoutée parce qu'on traite une classe particulière dans la partie exécution ("System_out") pour simuler l'affichage en java 
si non il faut commenter les ligne contenat cette classe dans le fichier test "test_execution.java"

les autres fichiers dans le dossier tests sont des tests de compilation.
certains lèvent une erreur d'exécution puisqu'on definit pas le main dans ces fichiers