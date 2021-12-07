# Selectanfrage für den 6. Projectathon der MII: MIRACUM "WE-STORM"
Datum: 01.12.21

Autoren: [Nandhini.Santhanam@medma.uni-heidelberg.de](mailto:nandhini.santhanam@medma.uni-heidelberg.de) & [Maros@uni-heidelberg.de](mailto:Maros@uni-heidelberg.de)

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
- `Kohorte.csv` inkl. alle Patienten mit den Pflichtdatenfelder(patient_id, birth_date, gender, patient_zip) Und Informationen über den Besuch des Patienten im Krankenhaus - Aufnahmedatum, ICD, Rang (Haupt-/Nebendiagnose) und verschiedene damit zusammenhängende Merkmale (intravenous lyse therapy (IVT) , Admission to the ICU, Admission to the stroke unit, Neurosurgery, Thrombectomy, Intrakraniell Stent)  und Kardiovaskuläre Risikofaktoren und metabolische Komorbiditäten
- `Medication.csv` inkl. alle Resourcen bzgl. Patientenaufnahmen (encouter_id) und die erhaltene Medikation (code)
- `Observations.csv` inkl. patient_id und encounter_id sowie LOINC-Codes (value & unit)
### Summary
Analog dazu befinden sich in der zweiten Gruppe die zusammengefasste/aggregierte Count-Daten der obigen Tabellen: 
- `Cohort_Summary.csv` gruppiert für die Quartale (z.B. 2020/Q1, ...)
- `Medication_Summary.csv` Anzahl der Fälle gruppierte nach Medikationstyp  
- `Observation_Summary.csv` Anzahl der Fälle gruppierte entsprechend der verfügbaren Laborwerte
- `Procedure_Summary.csv` Anzahl der Fälle gruppierte entsprechend der verfügbaren Procedures
- `StrokeDiagnosis_Summary.csv` Anzahl der Fälle gruppierte entsprechend der verfügbaren Stroke diagnosen ICD

Diese sind benötigt um die möglichste größte und feature-reicshte homogene Kohrote über alle Standorten hinweg für die statistische Auswertung selektieren zu können. 

## Verwendete Codesysteme
  Dieses System wird für den Download per FHIR Search verwendet
- http://fhir.de/CodeSystem/dimdi/icd-10-gm für Condition.code.coding.system
- http://fhir.de/CodeSystem/dimdi/ops für Procedure.code.coding.system
- http://loinc.org für Observation.code.coding.system
- http://fhir.de/CodeSystem/dimdi/atc für Medication.code.coding.system


## Verwendete Profile/Datenelemente
Die Abfragen werden auf der Grundlage der MII-Profile für die entsprechenden Ressourcen geschrieben. Die Skripte sind mit der neuesten Version der verfügbaren Hauptversionen kompatibel. Im Folgenden wird für jeden verwendeten Ressourcentyp beschrieben, welche Elemente für die FHIR-Suchanfrage an den Server verwendet werden (diese Elemente müssen vorhanden sein, damit kein Fehler ausgelöst wird) und welche Elemente im Skript extrahiert und in die Ergebnistabellen geschrieben werden.

<!--- [ENG] The queries are written based on the MII profiles for the corresponding resources. The scripts are compatible with the latest release of the available major versions. The following describes, for each resource type used, which elements are used for the FHIR search query to the server (these elements must be present to avoid throwing an error) and which elements are extracted in the script and written to the results tables.
--->

### Modul Person: Patient
Profil: https://www.medizininformatik-initiative.de/fhir/core/modul-person/StructureDefinition/Patient

Version: 2.0.0-alpha3 bzw. 1.0.14

Für Servabfrage verwendete Elemente:
Extrahierte Elemente:

* Patient.id
* Patient.gender
* Patient.birthDate
* Patient.address.postalCode

### Modul Fall: Encounter
Profil: https://www.medizininformatik-initiative.de/fhir/core/modul-fall/StructureDefinition/KontaktGesundheitseinrichtung

Version: 1.0.1

Extrahierte Elemente:

* Encounter.id
* Encounter.subject.reference
* Encounter.period.start
* Encounter.diagnosis.condition.reference
* Encounter.diagnosis.rank
* Encounter.hospitalization.dischargeDisposition.coding.code
### Modul Diagnose: Condition
Profil: https://www.medizininformatik-initiative.de/fhir/core/modul-diagnose/StructureDefinition/Diagnose

Version: 2.0.0-alpha3 bzw. 1.0.4

Für Servabfrage verwendete Elemente:

* Condition.subject.reference

Extrahierte Elemente:

* Condition.id
* Condition.recordedDate
* Condition.code.coding.code
* Condition.code.coding.system
* Condition.encounter.reference
* Condition.subject.reference

### Modul Prozedur: Procedure
Profil: https://www.medizininformatik-initiative.de/fhir/core/modul-prozedur/StructureDefinition/Procedure

Version: 2.0.0-alpha3 bzw. 1.0.4

Für Servabfrage verwendete Elemente:

* Procedure.subject.reference

Extrahierte Elemente:

* Procedure.id
* Procedure.performedDateTime
* Procedure.code.coding.code
* Procedure.code.coding.system
* Procedure.encounter.reference
* Procedure.subject.reference


### Modul Labor: Observation
Profil: https://www.medizininformatik-initiative.de/fhir/core/modul-labor/StructureDefinition/ObservationLab

Version: 1.0.6

Extrahierte Elemente:

* Observation.id
* Observation.effectiveDateTime
* Observation.code.coding.code
* Observation.code.coding.system
* Observation.subject.reference
* Observation.valueQuantity.value
* Observation.valueQuantity.unit
* Observation.subject.reference
* Observation.encounter.reference


### Modul Medikation and MedicationStatement
Profil: https://www.medizininformatik-initiative.de/fhir/core/modul-medikation/StructureDefinition/MedicationStatement

Version: 1.0.6

Extrahierte Elemente:
* MedicationStatement.medication
* Medication.code.coding.code
* Medication.code.coding.system


## Konzeptioneller Ablauf der Abfrage
Im Prinzip läuft das Drehbuch wie folgt ab:
 1. Es verbindet sich mit dem FHIR-Server, um alle **Encounter**-Ressourcen herunterzuladen, die die unten genannten Schlaganfalldiagnosen aus dem Zeitraum vom 2015-01-01 bis zum aktuellen Datum haben.
 
        ICD10: I60.0,I60.1,I60.2,I60.3,I60.4,I60.5,I60.6,I60.7,I60.8,I60.9,I61.0,I61.1,I61.2,I61.3,I61.4,I61.5,I61.6,I61.8,I61.9,I63.0,I63.1,I63.2,I63.3,I63.4,I63.5,I63.6,I63.8,I63.9,I67.80!
          
 2. Es lädt auch alle referenzierten **Patienten-**, **Condition-** Ressourcen durch die erhaltenen **Encounter**-Ressourcen herunter bei bei denen in *Schritt 1.* **Encounters** erhaltenen worden sind.
  
        Request: [base]/Encounter?date=ge2015-01-01&_has:diagnosis.code&_include=Encounter:patient&_include=Encounter:diagnosis
 
 3. Nachdem diese Ressourcen heruntergeladen wurden, wird die notwendige Verarbeitung mit dem `FHIRCrackR` Paket durchgeführt und in einen Datenframe mit relevanten Merkmalen für die Encounter mit der entsprechenden Diagnose umgewandelt.
 
 4. Die Liste der **Encounter-** und **Patienten-IDs** wird aus den extrahierten Ressourcen extrahiert und wird für das Herunterladen weiterer Ressourcen wie **Observation** und **Medikation** verwendet.
 
 5. Die **Observation-** Ressources werden für die Liste der **Patient-IDS** und **LOINC-Codes** heruntergeladen. Zusätzlich werden die **Observation-** Ressources basierend auf das **Aufnahme-** und **Entlassdatum** miteinander gematcht.
 
        Request: [base]Observation?subject=xx&code=777-3,6301-6,3173-2,2160-0,2089-1,2085-9,7799-0,4548-4,2345-7,2093-3,74201-5
        *Note: xx indicates a placeholder for list of patient ids*
               
6. Ähnlich werden die **Procedure-** Ressources für die Liste der **Patient-IDS** heruntergeladen und basierend auf das **Aufnahme-** und **Entlassdatum** zusätzlich gematcht.
 
        Request: [base]Procedure?subject=xx
        *Note: xx indicates a placeholder for list of patient ids*  
              
 7. Das **medicationStatement** wird für die Liste der **Encounters** heruntergeladen, aus der die relevante **Medikamenten-ID** gewonnen wird, die dann zur Extraktion der eigentlichen **Medikamenten-**Ressourcen verwendet wird:

        Request: [Base]/Medication?id=xx
        *Note: xx indicates a placeholder for list of encounter ids*
        
8. Um die früheren Komorbiditäten im Zusammenhang mit dem kardiovaskulären Risiko und den metabolischen Risiken zu erhalten, wird die **Condition**-Ressource für die Liste der Patienten extrahiert und die relevanten Merkmale werden auf der Grundlage der ICD10-Codes erstellt.

        Request: [Base]/Condition?subject=xx        
        *Note: xx indicates a placeholder for list of patient ids*

 9. Wann alle diese Ressourcen heruntergeladen worden sind, werden in R verschiedene Data-Frames für die gesamten aggregierten Daten und auch verschiedene *Summaries* erstellt, und als `.csv` gespeichert werden. Die Einzelheiten dazu sind im obigen Abschnitt über die Ausgabe aufgeführt. 
         





