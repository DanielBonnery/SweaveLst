#' Affiche le contenu d'un fichier R dans un environnement LaTeX lstlisting
#'
#' Cette fonction lit un fichier R, échappe les caractères spéciaux (comme \#)
#' et génère une chaîne de caractères LaTeX prête à être insérée dans un document.
#' Le résultat est formaté dans un environnement \verb|lstlisting| avec un titre
#' correspondant au nom du fichier.
#'
#' @param fichier Une chaîne de caractères indiquant le chemin vers le fichier R à afficher.
#' @return Une chaîne de caractères contenant le code LaTeX pour afficher le code R.
#' @examples
#' # Supposons que le fichier "exo1.r" existe dans le répertoire courant
#' \Sexpr{affiche_contenu_fichier_r("exo1.r")}
#' @export
affiche_contenu_fichier_r <- function(fichier) {
    lines <- readLines(fichier)
    escaped_lines <- gsub("\\", "\\\\", lines, fixed = TRUE)
    escaped_lines <- gsub("#", "\\\\#", lines, fixed = TRUE)
    escaped_lines <- gsub("_", "\\\\_", escaped_lines, fixed = TRUE)
    paste0("\\textbf{Code R}\n",
           "\\begin{lstlisting}[style=Rinput,title={", fichier, "},captionpos=tb]\n",
           paste(escaped_lines, collapse = "\n"),
           "\\end{lstlisting}\n")
}
affiche<-function (fichier) {
    lines <- readLines(fichier)
    escaped_lines <- gsub("#", "\\#", lines, fixed = TRUE)
    paste0("\\\\textbf{Code R}\n", 
        "\\\\begin{lstlisting}[style=Rinput,title={", 
        fichier,"},captionpos=tb]\n", paste(escaped_lines, collapse = "\n"), 
        "\\\\end{lstlisting}\n")
}
