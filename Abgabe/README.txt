Projektarbeit statistisches Praktikum Justin König

Im github Verzeichnis Abgabe befindet sich eine Ordner Namens Abgabe_statistisches_Praktikum_Justin_König_Mtr-Nr._2340677. Dieser kann heruntergeladen 
und entpackt werden. 

Darin sind Ordner mit Bilddateien der Flow Chars, Diagramme und Tabellen enthalten. Zudem ist ein Ordner mit den RDS Files der 
wichtigsten data frames und Listen enthalten. (Der Diagramm Ordner ist Projektarbeit_statisches_Praktikum_Justin_Koenig_Abgabe_files)

Im Ordner Abgabe_statistisches_Praktikum_Justin_König_Mtr-Nr._2340677 sind ausserdem zwei bibfiles enthalten mit den Quellenangaben 
zu den packages und der weiteren Literatur und Internetquellen.

Die benötigte .Rdata_File ist über die Datei data1 vorhanden. Desweiteren sind eine RMD und MD Datei vorhanden.

Der Ordner functions muss sich für einen reibungslosen Ablauf im gleichen Ordner befinden wie die RMD-Datei, da die darin enthaltenen Funktion 
gesourced werden und für die Berechnungen und das Scraping benötigt werden. 
Durch das im RMD-File enthaltene setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) sollte es keine Rolle spielen, wo der 
Ordener Abgabe gespeichert wird bzw in was dieser umbenannt wird, solange sich der Inhalt nicht verändert.


R Version: 4.1.1

Verwendete packages:

	tidyverse
	rvest
	quantmod
	tidyquant
	DiagrammeR
	reshape2
	gt
	glue
	gridExtra