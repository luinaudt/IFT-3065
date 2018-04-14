# belize
LA BRANCHE POUR LA REMISE DE L'ÉTAPE 2 EST LA BRANCHE intermediateUnstable

belize : Compilateur Scheme à x86-64
Thomas Luinaud
Francis de Ladurantaye

todo Parseur : pour define macro
-support des , dans le parseur
-support des @ dans le parseur
-support de #!unbound (pour autogene plutot)
-support de `

voir pour transformer les (define (nom var)) en (define nom (lambda (var)...))

#todo :
-read (en cours)
-parametre reste (test unitaire ajoute)
	   Il va falloir modifier closure-conv (toutes les fonctions)
-gc (code c du cours disponible)
-define macro
-write (reste les vecteurs)
-vecteurs



