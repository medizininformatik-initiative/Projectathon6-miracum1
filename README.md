# 6. Projectathon der MII: MIRACUM - "WEather-based Stroke event and Outcome Risk Modeling" (WE-STORM)
Datum: 28.01.2023

Autoren: [Nandhini.Santhanam@medma.uni-heidelberg.de](mailto:nandhini.santhanam@medma.uni-heidelberg.de) & [Maros@uni-heidelberg.de](mailto:Maros@uni-heidelberg.de)

Dieses Project führtdas MIRACUM (["WE-STORM"](https://forschen-fuer-gesundheit.de/projekt2.php)) Projekt vom [6. Projectathons](https://www.medizininformatik-initiative.de/datennutzung) aus. Hier ist eine dezentrale Analyse (distributed On-Site and Federated Learning)vorgesehen. 
Dieses Repository beinhaltet Skripte für das zweistufige Verfahren: 
+ Skript-1 (Step 1 - Abfrage) erzeugt mehreren Tabellen mit (anonymen, grob)aggregierten Daten, die für die Planung der statistischen Analysen (Step 2) benötigt werden. Diese Tabellen wurden zentral evaluiert und somit an die datenauswertendende Stelle (MIRACUM, Mannheim) übergeben.
+ Skript-2 (Step 2 Analysen) **neues Update vom 13-01-2023**, im Folder [step2](https://github.com/medizininformatik-initiative/Projectathon6-miracum1/tree/master/step2) des master Branches, führt die basierend der Step 1 erstellten statistische Modelle und Maschinelle Lernalgorithmen aus. Die Modelle werden _ausschließlich lokal_ (im jeweiligen DIZ) trainiert und nur die Modellobjekte werden i.R. des Federated Learning Verfahrens zusammengeführt. 

Das Readme beschreibt zunächst die technischen Details der Verwendung. Darunter sind die verwendeten CodeSysteme/Ressourcen/Profile und der konzeptionelle Ablauf der Abfrage beschrieben.

### BREAKING NEWS
+ Freigabe [13.01.2023](#jan-01-2023) der [Step2 Skripte](https://github.com/medizininformatik-initiative/Projectathon6-miracum1/tree/master/step2) für die statistischen Analysen mittels Federated Learning. 

+ In der MII Weekly Projectathon Webkonferenz am [29.04.2022](#apr-29-2022) haben wir die Scripts für die **Select-Abfrage (Step 1)** freigegeben. Details zur Ausführung der Scripte finden Sie im [Changelog](#apr-29-2022). 

#### Häufige Probleme
Den [Quick-Fix](#may-05-2022) für die häufigste Error Meldung `HTTP code 500` (bisher nur HAPI) aufgrund der Timeout der Abfrage (für Observations Modul) finden sie [hier](#may-05-2022) und für Condition Resources [hier](may-05-2022).

---

## Table of Contents 

  * [2023-0. UPDATE - Step2 Analysen](#update-2023)
  * [2023-1. Verwendung (Step 2)](#verwendung-step2)
    * [1.1. Ausführung in R Step2](#ausführung-in-R-step2)
  * [2023-2 Ablauf und Logik der Analyse Pipeline](#ablauf-und-logik-der-analyse-pipeline)
  * [2023-3. Ausführung im Docker Container (Step 2)](#ausführung-im-docker-container-step2)
    * [3.2. Docker Compose](#b-image-bauen-mit-docker-compose-step2)
  * [2023-4. Changelog (Step 2)](#changelog-step2)
  * [2022-0. Step 1](#2022-step1)
  * [1. Verwendung (Step 1)](#verwendung-step1)
    * [1.1. Ausführung in R](#ausführung-in-R)
     * [1.1.1. Vor der ersten Nutzung](#vor-der-ersten-utzung)
      * [1.1.1.1. Batch-Datei/Shell-Skript](#batch-dateishell-skript)
      * [1.1.1.2. R/RStudio](#rrstudio)
  * [2. Ausführung im Docker Container (Step 1)](#ausführung-im-docker-container)
    * [2.1. DockerHub](#a-image-von-dockerhub-ziehen)
    * [2.2. Docker Compose](#b-image-bauen-mit-docker-compose)
    * [2.3. Ohne Docker Compose](#c-image-bauen-ohne-docker-compose)
    * [2.4. DockerHub Direkt Download](#d-direkt-download-vom-dockerHub)
  * [3. Output (Step 1)](#output)
    * [3.1. Summary](#summary)
  * [4. Verwendete Codesysteme (Step 1)](#verwendete-codesysteme)
  * [5. Verwendete Profile/Datenelemente (Step 1)](#verwendete-profiledatenelemente)
  * [6. Konzeptioneller Ablauf der Abfrage (Step 1)](#konzeptioneller-ablauf-der-abfrage)
  * [7. Datentransfer (Step 1)](#data-transfer)
  * [8. Changelog (Step 1)](#changelog-step1)
    * [Apr 29, 2022 - Freigabe der Scripte für die STEP 1-Select Abfrage](#apr-29-2022)
    * [May 05, 2022 - Frequent Errors & Quick fixes - HTTP 500 Observation Module](#may-05-2022)
    * [May 06, 2022 - Frequent Errors & Quick fixes - HTTP 500 Condition Resources](#may-06-2022)

---
## UPDATE 2023

## Verwendung Step2 
Es gibt zwei Möglichkeiten die R-Skripte für die "Step 2 - Analysen" auszuführen: 
+ Direkt in R oder in einem Docker Container. Beide werden im folgenden beschrieben.

_Wichtig:_ nach dem Cloning des Repos bitte ins [step2](https://github.com/medizininformatik-initiative/Projectathon6-miracum1/tree/master/step2) Folder navigieren und als working/reference director einstellen.

### Ausführung in R Step2

1. Um die Analysen auszuführen (Analog zu den Step1-Skripten), muss der Inhalt des Git-Repository auf einen Rechner (PC, Server) gezogen werden, von dem aus der REST-Endpunkt des gewünschten FHIR-Servers (z.B. FHIR-Server der Clinical Domain im DIZ) erreichbar ist. 

2. Auf diesem Rechner muss R (aber nicht notwendigerweise RStudio) als genutzte Laufzeitumgebung installiert sein.

3. Im `master` branch des Repos soll im Folder [step2](https://github.com/medizininformatik-initiative/Projectathon6-miracum1/tree/master/step2) befindliche Datei `./config_default.yml` muss nach `./config.yml` kopiert werden und lokal angepasst werden (serverbase, ggf. Authentifizierung - Username and password, proxy configs); Erklärungen dazu finden sich direkt in dieser Datei. Eine Authentifizierung mit Basic Authentication. Dafür müssen in `config.yml` die Variable `authentication` und die zugehörigen Zugangsdaten (`password`/`username`) angepasst werden.
  
4. Wenn die App über `runMiracum_select.bat` (unter Windows) gestartet wird, sollte in dieser der Pfad zur Datei `Rscript.exe` geprüft und ggf. angepasst werden (z.B. `C:\Program Files\R\R-4.0.4\bin\Rscript.exe`).

#### Start des Skripts
Beim ersten Start des Skripts wird überprüft, ob die zur Ausführung notwendigen R-Pakete vorhanden sind. Ist dies nicht der Fall, werden diese Pakete nachinstalliert – dieser Prozess kann einige Zeit in Anspruch nehmen.

#### Batch-Datei/Shell-Skript
**Unter Windows**: Mit der Batch-Datei `runMIRACUM_select_step2.bat`.
Beim ersten Ausführen sollte diese ggf. als Administrator gestartet werden (über Eingabeaufforderung oder Rechtsklick), wenn die ggf. notwendigen Berechtigungen zum Nachinstallieren der R-Pakete sonst nicht vorhanden sind. Nach der ersten Installation reicht dann ein Doppelklick zum Starten.

**Unter Linux**: Mit dem Shell-Skript `runMIRACUM_select_step2.sh`. Das Shell-Skript muss ausführbar sein und ggf. beim ersten Ausführen mittels `sudo` gestartet werden, wenn ein Nachinstallieren der R-Pakete außerhalb des User-Kontexts erforderlich ist.

### Ablauf und Logik der Analyse Pipeline
Beide Skripte (`runMIRACUM_select_step2.bat` und `runMIRACUM_select_step2.sh`) führen die Datei `execution.R` aus. 

Diese Skript (`execution.R`) führt sequenziell 6 weitere R Skripte aus, wie folgt: 
* `1_install_r_packages.R`, Load & Install notwendige R Packete
* `2_data_selection.R`, Selektion der Daten vom FHIR Server zum Wetterdaten-Matching
* `3_feature_extraction.R`, Extraktion der Wetterparameter entsprechend der WStationen. 
* `4_modeling1_fixedwindow_false.R`, Modeling 1: Kreuzvalidierung (fixedWindow = F) 
* `5_modeling2_fixedwindow_true.R`, Modeling 2: Kreuzvalidierung (fixedWindow = T) 
* `6_modeling3_gamboost.R`, Modeling 3: genestete-Kreuzvalidierung 

_Hinweis:_ Alle notwendige präprozessierte Wetterdaten sind im Folder [`data`](https://github.com/medizininformatik-initiative/Projectathon6-miracum1/tree/master/step2/data) vorhanden. 
Im Rahmen der Ausführung der Skripte werden __keine Daten__ von externen Quellen herunterladen, keine Ports geöffnet. 

### Ausführung im Docker Container Step2

#### A) Image von DockerHub ziehen:
[Link to DockerHub](nandhinis08/projectathon6-miracum1-step2) or by using `docker pull nandhinis08/projectathon6-miracum1-step2`

#### B) Image bauen mit Docker Compose Step2: 
1. Git-Respository klonen: `git clone https://github.com/medizininformatik-initiative/Projectathon6-miracum1.git`
2. Verzeichniswechsel in das lokale Repository und in den neuen Folder **step2**: `cd Projectathon6-miracum1/step2`
3. Konfiguration lokal anpassen: `./config_default.yml` nach `./config.yml` kopieren und anpassen
4. Image bauen und Container starten: `docker compose up -d`

Zum Stoppen des Containers `docker compose stop`. Um ihn erneut zu starten, `docker compose start`.

#### C) Image bauen ohne Docker Compose:
Ergänzung folgt.


## Changelog Step2

#### Jan 27, 2023
_MII Projektathon Weekly-Sprechstunde:_ WE-STORM Step2 Update Darstellung. 

The branch [blaze_update](https://github.com/medizininformatik-initiative/Projectathon6-miracum1/tree/blaze_update) has also been updated with `step2` scripts.

Currently, _UKH_ and _UKJ_ successfully ran the scripts in their current forms. 

[Link to DockerHub](https://hub.docker.com/r/nandhinis08/projectathon6-miracum1-step2)

_Minor:_ the number of model objects in the `results` folder is coming soon. 

#### Jan 26, 2023 
GitHub Issue [#11](https://github.com/medizininformatik-initiative/Projectathon6-miracum1/issues/11) regarding missing support vectors in SVM fit is fixed. 

__TL;DR Explanation__
This issue originated from the hardcoded import time range of `2015-01-01 to 2021-12-31`, which might not neccessarily be available at all sites, resulting in (potentially years of) 0 values in the target vector (count). Hence, no decision boundary could be found. 
Additionally, `try() | tryCatch()` wrappers have been added to assure that the script runs even if certain model fits would fail.
Thanks for raising and helping to solve the issue [@pdi-uk](https://github.com/pdi-uk).

#### Jan 25, 2023 
GitHub Issue [#10](https://github.com/medizininformatik-initiative/Projectathon6-miracum1/issues/10) regarding package dependency is now fixed. 

#### Jan 23, 2023 
GitHub Issue [#9](https://github.com/medizininformatik-initiative/Projectathon6-miracum1/issues/9) regarding `predict.glm(..., type = "response")` is now fixed. Thanks for raising the issue [@KutSaleh](https://github.com/KutSaleh).


#### Jan 13, 2023
_Major Update:_ Analyses Skripts for Step 2 including statistical (baseline) and machine learning models. 

#### Jan 23, 2023
Uploaded the fix for the issue (Fehler bei der Ausführung (step2) #9). Updated Poisson-glm object with type = "response"

#### Jan 26, 2023
Uploaded the fix for the issue (Error for SVM model #11). Added try catch blocks, Changed dataframe timerange based on local min and max

#### Feb 02, 2023
Custom Random Forest function to tune mtry and ntree parameter. Made the max_bundles as configurable parameter in config.yml

#### Feb 06, 2023
SVM linear kernal models added and source path error fixed by changing source(file.path(getwd(), "customRF.R")) to
source(file.path(getwd(), "customRF.R") , local = TRUE) (cannot open file '//customRF.R': No such file or directory #16)


## 2022 Step 1 - Select Abfrage

## Verwendung Step1
Es gibt zwei Möglichkeiten diese R-Skripte auszuführen: Direkt in R oder in einem Docker Container. Beide werden im folgenden beschrieben.

### Ausführung in R
#### Vor der ersten Nutzung
1. Um die Selectanfrage durchzuführen, muss der Inhalt des Git-Repository auf einen Rechner (PC, Server) gezogen werden, von dem aus der REST-Endpunkt des gewünschten FHIR-Servers (z.B. FHIR-Server der Clinical Domain im DIZ) erreichbar ist. 

2. Auf diesem Rechner muss R (aber nicht notwendigerweise RStudio) als genutzte Laufzeitumgebung installiert sein.

3. Die mitgelieferte Datei `./config_default.yml` muss nach `./config.yml` kopiert werden und lokal angepasst werden (serverbase, ggf. Authentifizierung - Username and password, proxy configs); Erklärungen dazu finden sich direkt in dieser Datei. Eine Authentifizierung mit Basic Authentication. Dafür müssen in `config.yml` die Variable `authentication` und die zugehörigen Zugangsdaten (`password`/`username`) angepasst werden.
  
4. Wenn die App über `runMiracum_select.bat` (unter Windows) gestartet soll, muss in dieser der Pfad zur Datei `Rscript.exe` geprüft und ggf. angepasst werden (z.B. `C:\Program Files\R\R-4.0.4\bin\Rscript.exe`).

##### Start des Skripts
Beim ersten Start des Skripts wird überprüft, ob die zur Ausführung notwendigen R-Pakete ("rprojroot","fhircrackr","config","dplyr","zoo","stringr","tidyr") vorhanden sind. Ist dies nicht der Fall, werden diese Pakete nachinstalliert – dieser Prozess kann einige Zeit in Anspruch nehmen.

##### Batch-Datei/Shell-Skript
**Unter Windows**: Mit der Batch-Datei `runMIRACUM_select.bat`.
Beim ersten Ausführen sollte diese ggf. als Administrator gestartet werden (über Eingabeaufforderung oder Rechtsklick), wenn die ggf. notwendigen Berechtigungen zum Nachinstallieren der R-Pakete sonst nicht vorhanden sind. Nach der ersten Installation reicht dann ein Doppelklick zum Starten.

**Unter Linux**: Mit dem Shell-Skript `runMIRACUM_select.sh`. Das Shell-Skript muss ausführbar sein und ggf. beim ersten Ausführen mittels `sudo` gestartet werden, wenn ein Nachinstallieren der R-Pakete außerhalb des User-Kontexts erforderlich ist.

_Debugging/Error:_ Im Falle eines Berechtigungsfehlers soll der folgende Befehl vor dem ausführen des o.b. Shell-Skripts noch zusätzlich ausgeführt werden: `chmod -R 777 ./` 

#### R/RStudio
Durch Öffnen des R-Projektes (`Projectathon6-miracum1.Rproj`) mit anschließendem Ausführen der Datei `miracum_select.R` innerhalb von R/RStudio. Auch hier werden beim ersten Ausführen ggf. notwendige R-Pakete nachinstalliert.

### Ausführung im Docker Container
Um die Abfrage in einem Docker Container laufen zu lassen gibt es zwei Möglichkeiten:

#### A) Image von DockerHub ziehen:
1. Git-Repository klonen: `git clone https://github.com/medizininformatik-initiative/Projectathon6-miracum1.git Projectathon6-miracum1`
2. Verzeichniswechsel in das lokale Repository: `cd Projectathon6-miracum1`
3. Konfiguration lokal anpassen: `./config_default.yml` nach `./config.yml` kopieren und anpassen
4. Image downloaden und Container starten:

```bash
docker run --name projectathon6-miracum1 \
       -v "$(pwd)/errors:/errors" \
       -v "$(pwd)/Bundles:/Bundles" \
       -v "$(pwd)/Summary:/Summary" \
       -v "$(pwd)/Ergebnisse:/Ergebnisse" \
       -v "$(pwd)/config.yml:/config.yml" \
       nandhinis08/projectathon6-miracum1
```

#### B) Image bauen mit Docker Compose: 
1. Git-Respository klonen: `git clone https://github.com/medizininformatik-initiative/Projectathon6-miracum1.git`
2. Verzeichniswechsel in das lokale Repository: `cd Projectathon6-miracum1`
3. Konfiguration lokal anpassen: `./config_default.yml` nach `./config.yml` kopieren und anpassen
4. Image bauen und Container starten: `docker compose up -d`

Zum Stoppen des Containers `docker compose stop`. Um ihn erneut zu starten, `docker compose start`.

#### C) Image bauen ohne Docker Compose:
1. Git-Respository klonen: `git clone https://github.com/medizininformatik-initiative/Projectathon6-miracum1.git`
2. Verzeichniswechsel in das lokale Repository: `cd Projectathon6-miracum1`
3. Image bauen: `docker build -t projectathon6-miracum1 .` 
4. Konfiguration lokal anpassen:  `./config_default.yml` nach `./config.yml` kopieren und anpassen
5. Container starten: `docker run --name projectathon6-miracum1 -v "$(pwd)/errors:/errors" -v "$(pwd)/Bundles:/Bundles" -v "$(pwd)/Ergebnisse:/Ergebnisse" -v "$(pwd)/config.yml:/config.yml" projectathon6-miracum1`

Erklärung:

-  `-v "$(pwd)/config.yml:/config.yml""` bindet die lokal veränderte Variante des config-Files ein. Wenn dieses geändert wird, reicht es, den Container neu zu stoppen und starten (`docker stop Projectathon6-miracum1`, `config.yml` ändern, dann `docker start Projectathon6-miracum1`), ein erneutes `docker build` ist nicht nötig.

#### D) Direkt Download vom DockerHub:
Falls ein Error beim lokalen Builden des Containers auftreten soll (e.g. `RUN install2.r --error   --deps TRUE   fhircrackr ---> Running in 34cdad0afa40`), bitte entsprechend des Changelogs [Feb 17](#feb-17-2022) und [Feb 18](#feb-18-2022) den neusten pre-built container vom `dockerhub` herunterladen. 
Der DockerHub Link wurde angepasst, bitte entsprechend des Changelogs die letzte Version [Apr 12](#apr-12-2022) nutzen. 

---

## Output 
Das Skript erzeugt mehrere Ordner im Projekt-Directory. Um für den Projectathon eine möglichst einfache übersichtliche Lösung zu bekommen, werden alle files, die darin erzeugt werden bei mehrmaligem Ausführen ggf. einfach überschrieben.

Wenn die Abfrage erfolgreich durchgeführt wurde, wird die folgende Zusammenfassung in Excel erstellt 

### Summary
- `Cohort_Feature_availability` Dieses Blatt zeigt den Prozentsatz der verfügbaren Merkmale für jeden Monat in der Kohorte. Diese Tabelle enthält die wichtigsten Merkmale, die für das Modell benötigt werden
- `MultiplePatientVisit: ` Hier wird die Zuordnung zwischen der Anzahl der Patienten und ihrer Besuche dargestellt
- `PLZ` Hier wird die Anzahl der Begegnungen in jeder PLZ angegeben (gefiltert größer oder gleich 5). Dies hilft uns bei der Identifizierung der wichtigsten PLZ in der Kohorte für den Abgleich mit den Wetterstationen.
- `Stroke_ICD_Summary` Dies gibt die Anzahl der Begegnungen mit einem entsprechenden ICD an und auch den Prozentsatz, den dieser in der Gesamtkohorte ausmacht, der ebenfalls mit etwas Rauschen addiert wird.
- `Different Procedures` Für die identifizierte Kohorte werden hier die verschiedenen Verfahren, die für die verschiedenen Begegnungen durchgeführt wurden, und ihr prozentualer Anteil an der Gesamtkohorte angegeben.
- `Previous comorbidities` Anzahl der Fälle gruppierte entsprechend der verfügbaren Stroke diagnosen ICDFür die identifizierte Kohorte werden hier die früheren Komorbiditäten für die verschiedenen Begegnungen und ihr prozentualer Anteil an der Gesamtkohorte angegeben.
- `LabValues` Für die identifizierte Kohorte werden hier die Labormethoden angegeben, die bei verschiedenen Untersuchungen gemessen wurden (basierend auf LOINC-Code), sowie deren prozentualer Anteil an der Gesamtkohorte.
- `Medication` Für die identifizierte Kohorte werden hier die bei verschiedenen Untersuchungen verabreichten Medikamente und ihr prozentualer Anteil an der Gesamtkohorte angegeben.
Diese sind benötigt um die Different Procedures größte und feature-reicshte homogene Kohrote über alle Standorten hinweg für die statistische Auswertung selektieren zu können. 

---

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

---

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
       
---

## Datentransfer 

**Datentransfer:**
Für den zentralen Datentransfer der Ergebnisse der SELECT-Abfrage (`Summary/Summary_Step1_MIRACUM_WESTORM.xlsx`) soll der Prozess-Skript für den Dateityp (".xlsx") angepasst werden. 
Der Hintergrund dafür ist, dass der Projectathon Prozess prüft den tatsächlichen `MimeType` der Base64 codierten Datei gegen den deklarierten `MimeType. Daher muss der deklarierte MimeType in den `DocumentReference` und `Binary Ressourcen` dazu passen bzw. identisch sein. Für eine `.xlsx` Datei wie folgt: `<contentType value="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"/>`.

Die Repository für das **MII Projectathon Data Transfer process** befindet sich unter [MII DSF Processes](https://github.com/medizininformatik-initiative/mii-dsf-processes). 

Als Beispiel, [@wetret](https://github.com/wetret) (Reto Wettstein) hat bereits einen erfolgreichen Test mit dem [folgenden Skript (`DicFhirStore_WE-STORM.xml`; lines 23 & 38)](https://github.com/medizininformatik-initiative/mii-dsf-processes/blob/main/mii-dsf-process-projectathon-data-transfer/src/test/resources/fhir/Bundle/DicFhirStore_WE-STORM.xml) durchgeführt. 

Vielen Dank an [@wetret](https://github.com/wetret) (Reto Wettstein) und [@hhund](https://github.com/hhund) (Hauke Hund) sowie Christian Gierschner für den Support. 

---

## Changelog Step1

### Major changes

#### May 06, 2022

Siehe das [Issue](https://github.com/medizininformatik-initiative/Projectathon6-miracum1/issues/7) was auch ein `HTTP code 500` error geliefert hat. Das Problem scheint hier mit der HAPI Server Version (5.3.0) gebunden zu sein. Die länge der Condition Resources und Anzahl der gebundelten Fälle (zuvor `max_bundles = 100`) kann ggf. zu lang werden. 

Der simple Fix in [line 131](https://github.com/medizininformatik-initiative/Projectathon6-miracum1/blob/c4c219bca9a9343392d203113727a4fb20601cb5/miracum_select.R#L131) mit der Reduktion auf `, max_bundles = 40` hat das Problem in Halle gelöst. Vielen Dank [@Diana Pietzner](https://github.com/pdi-uk).

#### May 05, 2022 

Die häufigste Fehlermeldung beim Ausführen des Scripts (`miracum_select.R`) war bisher das folgende `Your request generated a server error, HTTP code 500.` siehe auch das [Issue](https://github.com/medizininformatik-initiative/Projectathon6-miracum1/issues/7) - vielen Dank [@pdi-uk](https://github.com/pdi-uk) und @Abel Stolz (MII Slack). Der Fehler trat bisher nur beim [HAPI FHIR Servers](https://hapifhir.io) auf. 

Das Problem wird von der langen Query im Rahmen der Abfrage vom Labor Modul ausgelöst. Die kombinierte Länge der multiplen LOINC Codes von multiplen Patienten überschreiten die erlaubte URL Charakter Länge. 
Diese wird in [Zeile 352](https://github.com/medizininformatik-initiative/Projectathon6-miracum1/blob/2a795679d2827165564decf9c58da07f2b17363f/miracum_select.R#L352) definiert. In Erlangen, Jena, Mannheim lief das Script mit 1800 durch. In Leipzig kam der HTTP 500 Error.

* Zuvor `nchar_for_ids <- 1800 - (nchar(conf$serverbase)+nchar_loincs)`
* Lösung `nchar_for_ids <- 900 - (nchar(conf$serverbase)+nchar_loincs)`
   * Dadurch wird die Anzahl der Patienten pro Anfrage reduziert, für die LOINC-kodierte Beobachtungen heruntergeladen werden (z.B. in Leipzig hat es das Problem gelöst).

Wir haben den  `master` [Branch](https://github.com/medizininformatik-initiative/Projectathon6-miracum1) dem entsprechend ([Observation_length_update](https://github.com/medizininformatik-initiative/Projectathon6-miracum1/commit/388a470dcf1149498909725dc82b7357d791f71f)) aktualisiert.

#### Apr 29, 2022

##### Freigabe der Step 1, SELECT-Query
In der heutigen MII Weekly Projectathon Webkonferenz haben wir die **Step 1, Select-Abfrage** des WE-STORM Projektes freigegeben. Die Skripte wurden erfolgreich an den folgenden Standorten getestet (e.g. Erlangen, Jena, Mannheim, TU-München, Tübingen). 

Vielen Dank für die gute Zusammenarbeit, Eure Zeit und Unterstützung sowie die Anregungen und Verbesserungs Vorschläge, insbesondere an:  
 
* [Julia Palm](https://github.com/palmjulia), UK Jena (HAPI) 
* [Jonathan Mang](https://github.com/joundso), UK Erlangen (HAPI) 
* [Thomas Ganslandt], UK Erlangen (HAPI)
* [Noemi Deppenwiese], UK Erlangen (HAPI)
* [Raffael Bild](https://github.com/RaffaelBild), TUM (IBM HAPI & BLAZE)
* [Stephanie Biergans], UK Tübingen (IBM HAPI & Blaze) 
* [Alexander Kiel](https://github.com/alexanderkiel), Uni Leipzig ([blaze](https://github.com/samply/blaze) [v0.17.0](https://github.com/samply/blaze/releases/tag/v0.17.0) update with chaining)

##### Github Pull
1. Für die [HAPI](https://hapifhir.io) erstellte Scripte können vom `master` [Branch](https://github.com/medizininformatik-initiative/Projectathon6-miracum1) ge-pullt werden. 
2. Für [blaze](https://github.com/samply/blaze) angepasste Scripte können vom `blaze_update` [Branch](https://github.com/medizininformatik-initiative/Projectathon6-miracum1/tree/blaze_update) ge-pullt werden. 
    * Diese wurden für `v0.16.x`(noch ohne chained search parameters) angepasst.

##### Dockerhub
Die [dockerhub](https://hub.docker.com/) Images sind für jeweils:
* [HAPI-type](https://hub.docker.com/r/nandhinis08/projectathon6-miracum1) und 
* [blaze](https://hub.docker.com/r/nandhinis08/projectathon6-miracum1-blaze) FHIR Server über die Links verfügbar. Diese können je nach Server/Proxy Settings eine einfachere Lösung bieten (siehe Changelogs [Apr 12](#apr-12-2022) & [Feb 18](#feb-18-2022)). 

##### Performance
Der geschwindigkeitslimitierende Faktor ist der Download der Observations ([Labor Module](#modul-labor-observation). Für Standorte mit vielen Labordaten erhöht sich die Zeit (vor allem auf HAPI FHIR). 
  + Time ranges (HAPI): 4h-16h (cohort sizes: ~2-9k)
  + Time range (blaze): 46 mins (cohort sizes: ~6-9k)

---

#### Apr 27, 2022

Für blaze Servers wird der Download von Resourcen angepasst, um die Verkettung (chaining) aus dem Filtering-Process der HAPI Scripte zu entfernen ([siehe Issue hier](https://github.com/medizininformatik-initiative/Projectathon6-miracum1/issues/5)). 

#### Apr 21, 2022

Wir haben die Scripts updated um stationäre und ambulante Fälle, je nach vorliegende FHIR-Elemente (`.class`, `.rank`, `.use`) bzw. Kondierung des jeweiligen DIZ besser unterscheiden zu können. Aktuell basieren diese Scripts auf lokale (Mannheimer DIZ-Daten) somit müssen diese noch vor dem finalen Einsatz (Step 1, SELECT-Abfrage), extern getestet werden. Diese Tests sind noch aussstehend, daher bitte den Script noch nicht nutzen.  

#### Apr 12, 2022
Änderung: Zusätzlich zu den Ergebnis-Tabellen wird nun ein Textfile `"Summary/miracum_select.log"` erzeugt, welches die Anzahl der extrahierten Fälle, Patienten und die Laufzeit des R-Skriptes dokumentiert. Das log-file muss nicht geteilt werden, es dient den DIZen nur als Hilfestellung für die Einschätzung von Laufzeiten und Ergebnismengen. 

**Updated LOINC Codes:**
 Die LOINC Codes für die Laborparameter mussten aufgrund der unterschiedlichen Kodierung der Laborparameter zwischen den Standorten erweitert werden. Hierfür haben wir die Referenztabelle mit Top 300 LOINC Codes entsprechend der Liste im KDS Basismodule, 03 Modul Laborbefunde `2021-08-08_MII_TOP_300_LOINC.xlsx` verwendet.

**Updated DockerHub Link:**
 Ein neues [dockerhub](https://hub.docker.com/r/nandhinis08/projectathon6-miracum1) Image wurde mit den neuen Updates vom 12.04.2022 erstellt. 
Wenn dieses Image lokal nachgebaut wird soll dies mit `--deps TRUE` Flag passieren, somit alle R Pakete bereits miteingebaut werden. 

#### Mar 10, 2022
Proxy-Konfigurationsoptionen in config_default.yml hinzugefügt und in `miracum_select.R` eingefügt, um es in R-Session zu verwenden


#### Mar 08, 2022
Maxbundles-Argument aus dem Encounter-Bundle-Downlaod entfernt, das am 25. Januar versehentlich hinzugefügt wurde (Überbleibsel aus einer Debug-Session)

#### Feb 18, 2022
Rückmeldung von Leipzing: Fehlermeldung bei R package install `Warning: unable to access index for repository https://packagemanager.rstudio.com/cran/__linux__/focal/latest/src/contrib: cannot open URL ...`

Bei WE-STORM werden zur Run time des Containers durch das R-Skript (`install2.r`) weitere R-Pakete (`fhircrackr, config, dplyr, zoo, didyr, data.table, openxlsx`) nachinstalliert, deise benötigen weitere Pakete als Abhängigkeiten, welche standardgemäß von CRAN geladen werden.  

Zwischenlösung: [@joundso](https://github.com/joundso) docker image auf [dockerhub](https://hub.docker.com/r/joundso/projectathon6-miracum1) nachgebaut mit `--deps TRUE` flag somit alle R Pakete bereits miteingebaut sind. 

Falls der Skript ausführende Server keine Internetverbindung hat [siehe Feb 17](#feb-17-2022) kann der o.g. updated container vom dockerhub `docker pull joundso/projectathon6-miracum1:latest` benutzt werden.

#### Feb 17, 2022
Rückmeldung von Leipzig: Fehler am ehesten aufgrund von Firewall beim Download vom R-Packete & Dependencies (Abel Stolz; `RUN install2.r --error   --deps TRUE   fhircrackr ---> Running in 34cdad0afa40`), weil wir aktuell den Docker-Container lokal selbst bauen. 

Anmerkung: Das Docker-Image sollte für Sites verfügbar sein, die nicht in der Lage sind, selbst zu bauen. Siehe auch [Issue](https://github.com/medizininformatik-initiative/Projectathon6-miracum1/issues/3). 

Zwischenlösung: @joundso (Jonathan Mang) hat netterweise das Image unter seinem [dockerhub-Konto](https://hub.docker.com/r/joundso/projectathon6-miracum1) hochgeladen. Es wird noch ein Konto des Maintainers ([@nandhiniS08] | [@mematt]) erstellt.

#### Jan 27, 2022
Änderung: Fälle mit fehlenden Aufnahme- und Aufzeichnungsdaten wurden entfernt.

#### Jan 25, 2022
Änderung: Logik beim herunterladen von Conditions geändert. Es werden jetzt alle Conditions zu den untersuchten Patienten gezogen und anschließend so gefiltert, dass nur Conditions übrig bleiben, die zu den gewünschten Encountern gehören. 

Erklärung: Damit ist es jetzt irrelevant, ob der Encounter auf die Condition verlinkt oder die Condition auf den Encounter verlinkt. Das Skript funktioniert, solange mindestens eine der Richtungen gegeben ist. Diese Änderung wurde implementiert, weil es sich herausgestellt hat, dass die Linkrichtung in den verschiedenen DIZen heterogen und unterschiedlich gelöst ist. 

#### Jan 18, 2022
Änderung: Problem, wenn multiple Medikamenten-IDs gefiltert werden, wird die URL-Länge der FHIR Query zu lang. Dieses Problem hat Erlangen gemeldet. Die Medikamenten-IDs werden aufgeteilt und die Ressourcen werden in Teilen heruntergeladen.

Erklärung: Es wurde versäumt, die Logik der Aufteilung von IDs anzupassen (die für andere Ressourcen-Downloads implementiert ist). Der Fehler trat auf der lokalen Seite nicht auf, da die Medikationsdaten sehr gering sind.

##### Jan 11, 2022
Änderung: Anpassung des Formats und des Inhalts der Excel-Zusammenfassung gemäß einem Vorschlag der UAC.

##### Dec 10, 2021
Änderung: Anpassung der FHIR-Suchparameter für den Diagnosecode 

##### Dec 7, 2021
Änderung: Extraction based on subject_id and admission and discharge date

##### Dec 6, 2021
Änderung: Adapted SSL option in config and adapted the readme

##### Dec 2,3, 2021
Änderung: minor changes on readme and removed saving bundles 
