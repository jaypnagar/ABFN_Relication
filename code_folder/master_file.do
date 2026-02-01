*Set working director with following files in the code and data folder

/* Set Directory */

/*Elia  cd "C:\Users\ef122\Duke University\Jay Prakash Nagar - ABF_PatValue_Research"*/
/*Jay*/  cd "C:\Users\jayho\OneDrive - Duke University\ABF_Replication" 


cd "C:\Users\jayho\OneDrive - Duke University\ABF_Replication"

gl OUT "C:\Users\jayho\OneDrive - Duke University\ABF_Replication\Output"

gl IN "C:\Users\jayho\OneDrive - Duke University\ABF_Replication\Data"

clear all


/*code
1. A0_add_citations_to_patents.do: Merge DISCERN Data set patent with Matt Marx Reliance on Science data (output: patents_1980_2021_UO_with_citations_to_science.dta)
2. A1_patent_indicator_construction_variable.do: Create patent level indicators (output: patent_ipc.dta, Patent_Science_Indicator.dta, patent_quality_oursample.dta, CPI.dta, Firm_patent_invt_date.dta")
3. A2_Do_SampleCreation.do: create sample with patent data, stock market data to calculate the KPSS values (output:analyses.dta )
*/


/*data
input: A0_add_citations_to_patents.do
1. DISCERN Data (patents_1980_2021_UO.dta)
2. Reliance on science (_pcs_mag_doi_pmid.tsv)

input: A1_patent_indicator_construction_variable.do
3. USPTO PatentsView IPC Class data (g_ipc_at_issue.tsv)
4. DISCERN Patent with NPL Citation (patents_1980_2021_UO_with_citations_to_science.dta)
5. OECD Patent Quality indicators (202401_OECD_PATENT_QUALITY_USPTO_INDIC.txt)
6. Consumer Price Index for All Urban Consumers: All Items in U.S. City Average (CPIAUCSL.csv)
7. Patent Invt indicators (Number of Inventor, Number of Foreign Inventor) (patent_invt_indicator.csv) use python code - A0_PatentLevelIndicator_TeamForeignInvt.ipynb to create patent level data using Patstat.

input: A2_Do_SampleCreation.do
8. DISCERN Patent with NPL Citation (patents_1980_2021_UO_with_citations_to_science.dta)
9. SAS Code to calculate the stock market return (ret_patents.dta)

## SAS Code: Input:patentawards.xlsx ; output: ret_patents.dta

10. hoberg-phillips vertical relatedness data library (VertInteg.txt)
*/



**run A1_patent_indicator_construction_variable.do
do "code\A1_patent_indicator_construction_variable.do"

**run A1_patent_indicator_construction_variable.do (please run the SAS code use to create the market return)

do "code\A2_Do_SampleCreation.do"

**Estimate Baseline KPSS using single signal-to-noise ratio
do "code\A3_Kpss_baseline.do"

**Estimate Foreign-Domestic using two signal-to-noise ratio
do "code\A3_Kpss_Estimation_ForeignInvt.do"

**Estimate teamsize using two signal-to-noise ratio
do "code\A3_Kpss_Estimation_TeamSize.do"

**Estimate Vertical Integration using two signal-to-noise ratio
do "code\A3_Kpss_Estimation_VerticalInteg.do"
 
**Estimate using IID assumption
do "code\Appendix_A3_Do_Analyses_KPSS_NewAssumption.do"

**Creating Patent Level sample
do "code\A4_Do_Regression_Sample_Construction.do"

**Regression - Signal to noise ratio
do "code\A5_Table_SignalToNoiseRatio.do"

**Summary Statistics Tables
do "code\A6_SummaryStatsTables_PatentLevel.do"

**Patent Level Regression: Group Mean Difference- Teamsize, R&D offshoring, Science and VI
do "code\A7_Table_Regression_baseline.do"

**Abnormal Return as Dep variables
do "code\A8_AbnormalReturnAsDepVar_Regression.do"


**KPSS as explanatory variables
do "code\A9_KPSSasIndepVar_standarized_bootstrap.do"

