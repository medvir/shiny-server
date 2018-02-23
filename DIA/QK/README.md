# Qualitätskontrolle Diagnostik

## Shiny App
http://172.23.175.116:3838/DIA/QK/

Link von Resultat zu Master via “Panel” und Datum.
Link von Massnahmen zu Resultat via “Distribution Nr.” und “Specimen ID”.

Es können mehrer Resultat Tabellen gleichzeitig hochgeladen werden.

Die Selektion ist hierarchisch aufgebaut: Es werden nur die Panels der ausgewählten Anbieter angezeigt, nur die Erreger der ausgewählten Panels und nur die Parameter der ausgewählten Erreger.

### Mastertabelle
Spalten: Erreger, Technologiebereich, QK_Anbieter, Panel, Frequenz
Enthält die Übersicht, welche Qualitätskontrollen im laufenden Jahr geplant ist. Wird sich von Jahr zu Jahr nicht gross verändern. Die Frequenz (Anzahl Testserien pro Jahr) wird in der Shiny App in die Testserien 1, 2, etc. umgesetzt.

### Resultat Tabellen
Spalten: Panel, Datum, Distribution, Specimen, Parameter, Resultat, Score, Type
Vorlage Resultat Erfassung: QK_Vorlage.xltx
Jedes eingehende Resultat (pdf) wird als Tabelle mit vordefinierten Spalten abgespeichert. Panel, Parameter und Type sind mittels Dropdown Liste vordefiniert.


### Massnahmentabelle
Spalten: Distribution, Specimen, Bewertung, Bemerkungen, Massnahmen
Für jedes Sample mit Fehler werden die Massnahmen fortlaufend erfasst.
