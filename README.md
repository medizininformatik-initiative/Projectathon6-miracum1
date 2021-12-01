# Selectanfrage für den 6. Projectathon der MII: MIRACUM "WE-STORM"
Datum: 30.11.21

Autoren: [Nandhini.Santhanam@medma.uni-heidelberg.de](mailto:nandhini.santhanam@medma.uni-heidelberg.de) & [Maros@uni-heidelberg.de]

Dieses Project führt die Select-Anfrage für das MIRACUM ("WE-STORM") Projekt im Rahmen des 6. Projectathons aus. Hier ist eine dezentrale Analyse (distributed On-Site and Federated Learning)vorgesehen. Dieses Skript (Step 1) erzeugt mehreren Tabellen mit aggregierten Daten, die für die Planung der statistischen Analysen (Step 2) benötigt werden. Diese Tabellen sollen zuerst zentral evaluiert werden und somit an die datenauswertendende Stelle (MIRACUM, Mannheim) übergeben werden.

Das Readme beschreibt zunächst die technischen Details der Verwendung. Darunter sind die verwendeten CodeSysteme/Ressourcen/Profile und der konzeptionelle Ablauf der Abfrage beschrieben.

## Verwendung
Es gibt zwei Möglichkeiten diese R-Skripte auszuführen: Direkt in R oder in einem Docker Container. Beide werden im folgenden beschrieben.

### Ausführung in R
#### Vor der ersten Nutzung
1. Um die Selectanfrage durchzuführen, muss der Inhalt des Git-Repository auf einen Rechner (PC, Server) gezogen werden, von dem aus der REST-Endpunkt des gewünschten FHIR-Servers (z.B. FHIR-Server der Clinical Domain im DIZ) erreichbar ist. 

2. Auf diesem Rechner muss R (aber nicht notwendigerweise RStudio) als genutzte Laufzeitumgebung installiert sein.

3. Die mitgelieferte Datei `./config_default.yml` muss nach `./config.yml` kopiert werden und lokal angepasst werden (serverbase, ggf. Authentifizierung - Username and password); Erklärungen dazu finden sich direkt in dieser Datei. Eine Authentifizierung mit Basic Authentication. Dafür müssen in `config.yml` die Variable `authentication` und die zugehörigen Zugangsdaten (`password`/`username`) angepasst werden.
  

4. Wenn die App über `runMiracum_select.bat` (unter Windows) gestartet soll, muss in dieser der Pfad zur Datei `Rscript.exe` geprüft und ggf. angepasst werden (z.B. `C:\Program Files\R\R-4.0.4\bin\Rscript.exe`).


#### Start des Skripts
Beim ersten Start des Skripts wird überprüft, ob die zur Ausführung notwendigen R-Pakete ("rprojroot","fhircrackr","config","dplyr","zoo","stringr","tidyr") vorhanden sind. Ist dies nicht der Fall, werden diese Pakete nachinstalliert – dieser Prozess kann einige Zeit in Anspruch nehmen.

##### Batch-Datei/Shell-Skript
**Unter Windows**: Mit der Batch-Datei `runMIRACUM_select.bat`.
Beim ersten Ausführen sollte diese ggf. als Administrator gestartet werden (über Eingabeaufforderung oder Rechtsklick), wenn die ggf. notwendigen Berechtigungen zum Nachinstallieren der R-Pakete sonst nicht vorhanden sind. Nach der ersten Installation reicht dann ein Doppelklick zum Starten.

**Unter Linux**: Mit dem Shell-Skript `runMIRACUM_select.sh`. Das Shell-Skript muss ausführbar sein und ggf. beim ersten Ausführen mittels `sudo` gestartet werden, wenn ein Nachinstallieren der R-Pakete außerhalb des User-Kontexts erforderlich ist.

_Debugging/Error:_ Im Falle eines Berechtigungsfehlers soll der folgende Befehl vor dem ausführen des o.b. Shell-Skripts noch zusätzlich ausgeführt werden: `chmod -R 777 ./` 

#### R/RStudio
Durch Öffnen des R-Projektes (`Projectathon6-miracum1.Rproj`) mit anschließendem Ausführen der Datei `miracum_select.R` innerhalb von R/RStudio. Auch hier werden beim ersten Ausführen ggf. notwendige R-Pakete nachinstalliert.

## Ausführung im Docker Container
Um die Abfrage in einem Docker Container laufen zu lassen gibt es zwei Möglichkeiten:

<!--- 
DockerHub option will be updated
**A) Image von DockerHub ziehen:**
1. Git-Respository klonen: `git clone https://github.com/NandhiniS08/Projectathon6-miracum1.git`
2. Verzeichniswechsel in das lokale Repository: `cd Projectathon6-miracum1`
3. Konfiguration lokal anpassen: `./config_default.yml` nach `./config.yml` kopieren und anpassen 
4. Image downloaden und Container starten: `docker run --name projectathon6-miracum1 -v "$(pwd)/errors:/errors" -v "$(pwd)/Bundles:/Bundles" -v "$(pwd)/Summary:/Summary" -v "$(pwd)/Ergebnisse:/Ergebnisse" -v "$(pwd)/config.yml:/config.yml" NandhiniS08/projectathon6-miracum1`
--->

**A) Image bauen mit Docker Compose:**
1. Git-Respository klonen: `git clone https://github.com/NandhiniS08/Projectathon6-miracum1.git`
2. Verzeichniswechsel in das lokale Repository: `cd Projectathon6-miracum1`
3. Konfiguration lokal anpassen: `./config_default.yml` nach `./config.yml` kopieren und anpassen
4. Image bauen und Container starten: `docker compose up -d`

Zum Stoppen des Containers `docker compose stop`. Um ihn erneut zu starten, `docker compose start`.

**B) Image bauen ohne Docker Compose**
1. Git-Respository klonen: `git clone https://github.com/NandhiniS08/Projectathon6-miracum1.git`
2. Verzeichniswechsel in das lokale Repository: `cd Projectathon6-miracum1`
3. Image bauen: `docker build -t projectathon6-miracum1 .` 
4. Konfiguration lokal anpassen:  `./config_default.yml` nach `./config.yml` kopieren und anpassen
5. Container starten: `docker run --name projectathon6-miracum1 -v "$(pwd)/errors:/errors" -v "$(pwd)/Bundles:/Bundles" -v "$(pwd)/Ergebnisse:/Ergebnisse" -v "$(pwd)/config.yml:/config.yml" projectathon6-miracum1`

Erklärung:

-  `-v "$(pwd)/config.yml:/config.yml""` bindet die lokal veränderte Variante des config-Files ein. Wenn dieses geändert wird, reicht es, den Container neu zu stoppen und starten (`docker stop Projectathon6-miracum1`, `config.yml` ändern, dann `docker start Projectathon6-miracum1`), ein erneutes `docker build` ist nicht nötig.


## Output 
Das Skript erzeugt mehrere Ordner im Projekt-Directory. Um für den Projectathon eine möglichst einfache übersichtliche Lösung zu bekommen, werden alle files, die darin erzeugt werden bei mehrmaligem Ausführen ggf. einfach überschrieben.

### Ergebnisse
Wenn die Abfrage erfolgreich durchgeführt wurde, sind hier zwei Gruppen von csv-Dateien zu finden.
In der ersten Gruppe befinden sich 3 `.csv` Dateien mit den orignalen Quelldaten:
- `Kohorte.csv` inkl. alle Patienten mit den Pflichtdatenfelder(patient_id, birth_date, gender, patient_zip)
- `Medication.csv` inkl. alle Resourcen bzgl. Patientenaufnahmen (encouter_id) und die erhaltene Medikation (code)
- `Observations.csv` inkl. patient_id und encounter_id sowie LOINC-Codes (value & unit)

Analog dazu befinden sich in der zweiten Gruppe die zusammengefasste/aggregierte Count-Daten der obigen Tabellen: 
- `Cohort_Summary.csv` gruppiert für die Quartale (z.B. 2020/Q1, ...)
- `Medication_Summary.csv` Anzahl der Fälle gruppierte nach Medikationstyp  
- `Observation_Summary.csv` Anzahl der Fälle gruppierte entsprechend der verfügbaren Laborwerte

Diese sind benötigt um die möglichste größte und feature-reicshte homogene Kohrote über alle Standorten hinweg für die statistische Auswertung selektieren zu können. 
