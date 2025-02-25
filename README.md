# README - Analisi Statistica-Temporale dei Tweet francofoni sulla guerra Russo-Ucraina

## Descrizione del Progetto
L’obiettivo di questo lavoro è condurre un’analisi statistica approfondita sui tweet in lingua francese pubblicati tra il _24 febbraio 2023_ e il _4 marzo 2023_, riguardanti la guerra Russo-Ucraina. Lo studio si concentrerà inizialmente sull’esplorazione delle variabili quanti tative presenti nel dataset, con particolare attenzione all’individuazione di eventuali relazioni tra di esse. L’obiettivo è sviluppare modelli statistici capaci di descrivere e spiegare le interazioni tra le variabili, analizzando come si influenzano a vicenda. Successivamente, l’attenzione si sposterà sull’analisi temporale dei dati. Non solo verrà descritto l’andamento dei tweet nel tempo, ma si cercherà anche di costruire modelli statistici in grado di **rappresentare le serie storiche**, valutando la possibilità dei modelli di effettuare previsioni (**forecasting**) sui dati. I modelli sviluppati quindi saranno testati e confrontati tra loro al fine di individuare il più efficace. Infine, nell’ultima parte il progetto esplorerà l’utilizzo dei modelli di linguaggio (LLM) per la generazione di **dataset sintetici**. L’obiettivo sarà verificare se tali modelli possano produrre dataset _gemelli_ che mantengano la stessa struttura e le stesse informazioni di quelli originali, ma con un numero ridotto di osservazioni. Questo per verificare se il loro utilizzo potrebbe essere utile al fine di trarre conclusioni senza dover elaborare un numero enorme di dati.

## Struttura del Progetto
Il progetto è suddiviso nelle seguenti sezioni:

1. **Introduzione**: Panoramica dell'analisi e degli obiettivi.
2. **Analisi del Dataset**:
   - Descrizione delle variabili.
   - Pulizia dei dati e trattamento degli outliers.
   - Analisi delle distribuzioni e trasformazioni dei dati.
3. **Relazioni tra Variabili**:
   - Calcolo delle correlazioni.
   - Regressioni lineari per la previsione di followers e score.
   - Regressione multinomiale per la previsione del sentiment.
4. **Analisi Temporale**:
   - Identificazione di pattern stagionali e trend nei tweet.
   - Decomposizione delle serie temporali.
5. **Previsioni con Modelli di Machine Learning**:
   - Modelli _ARIMA_ per la previsione dello score e del numero di tweet.
   - _Auto-Regressive Neural Network_ per l'analisi predittiva.
   - Confronto tra modelli e valutazione delle prestazioni.
6. **Generazione di Dataset Sintetici**:
   - Utilizzo di modelli di linguaggio per creare dataset alternativi.
   - Valutazione dell'affidabilità dei dati sintetici rispetto a quelli reali.

## Requisiti Tecnici
Per eseguire il codice del progetto, sono necessari i seguenti strumenti:

- **Linguaggio di programmazione**: R
- **Librerie principali**:
  - `ggplot2`, `GGally` per la visualizzazione dei dati.
  - `forecast`, `tseries` per l'analisi delle serie temporali.
  - `nnet`, `caret` per le reti neurali e le regressioni.
  - `dplyr`, `tidyverse` per la manipolazione dei dati.

## Istruzioni per l'Esecuzione
1. **Scaricare il dataset**: Il dataset è presente nella repository.
2. **Installare le librerie necessarie**:
   ```r
   install.packages(c("ggplot2", "GGally", "forecast", "tseries", "nnet", "caret", "dplyr", "tidyverse"))
   ```
3. **Eseguire gli script**: Ogni analisi ha il proprio file `.RMD` in cui sono contenuti i codicie qualche commento.
   -  `descrizione-dataset.RMD`: contiene i codice della prima sezione del lavoro;
   -  `ts_analysis.RMD` e `ts_analysis2.RMD`: contengono le analisi temporali dei dati, con il secondo file che si concentra sul forecast;
   -  `LLMs.RMD`: raccoglie i codici per l'analisi del dataset sintetico. 

## Contatti
Per ulteriori informazioni: rosariopiognazzo@gmail.com
---

