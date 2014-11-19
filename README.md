# OPAVaD-Grid
### Outil de Prospection, d'Analyse et de Valorisation de la Donnée
----
OPAVaD-Grid is a fork of the [OPAVaD](https://github.com/thidiff/OPAVaD) repository of my Work realized during my 6 month internship from June to December 2014 at the **Innovation Lab** of the French leading bank **Crédit Agricole**.

My mission consists in developping a prospecting tool to the merchants affiliated to the Crédit Agricole bank. This tool is based on the data of the credit card transactions (data not provided due to privacy). This project serves as a Proof of Concept in adding value to the data. Value which is highly sought in order to improve services, products and creates new ones.

The specifications are as follows :

* login module to propose merchant to access its personnel account 
* geographical comparison with other merchants in the same category (eg, restaurant) in terms of turnover during a certain period
* visualization of its sales
* profiling of its customers
* prospecting module for determining customers potentially interested in promotional offers

Technically, the prospecting tool is mainly developped in R, using the framework [Shiny](https://github.com/rstudio/shiny) and the [ShinyDash](https://github.com/trestletech/ShinyDash) project which is an implementation of the Gridster JavaScript library.
