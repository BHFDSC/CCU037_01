#CCU037_01-v1_all-ethnicity_codes_patients
#Description Summarize in one table the avaliable ethnicity codes in GDPPR table from DAE. The output of this is a global temp View which is then used by CCU037_01-include_name_following_code
#Author(s) Marta Pineda
#Reviewer(s)

#Date last updated 2022-02-15
#Date last reviewed
#Data last run 2022-03-24

#Data input: gdppr_data = 'gdppr_dars_nic_391419_j3w9t' -- ETHNIC = as NHS dictionary (A, B ...) -- CODE = SNOMED code
#Parameters collabdatabase_name = 'dars_nic_391419_j3w9t_collab' project_prefix = 'ccu037_01'
#Data output global_temp.ccu037_01_snomed_ethnicity_codes_patients_gdppr

#Software and versions SQL, Python

#This syntax followed the following work as an initial template:
#https://db.core.data.digital.nhs.uk/#notebook/2694567/command/2694589 # Skinny unassembled (not decisions yet)
#https://db.core.data.digital.nhs.uk/#notebook/4196839/command/5511846 # Tom Bolton code

#Temporary: tables with Ethnic fields in TRE
#gdppr_data = 'gdppr_dars_nic_391419_j3w9t' -- ETHNIC = as NHS dictionary (A, B ...)
#chess_data = 'chess_dars_nic_391419_j3w9t' -- Ethnicity = acording to ONS (see above)
#hes_apc_data = 'hes_apc_all_years'
#-- ETHNOS = This field contains a code that specifies some ethnic groups and some nationalities. It was introduced from the 1995-96 data year. From April 2001 the codes were changed to conform to the 2001 census classification.
#hes_op_data = 'hes_op_all_years'
#-- ETHNOS = This field contains a code that specifies some ethnic groups and some nationalities. It was introduced from the 1995-96 data year. From April 2001 the codes were changed to conform to the 2001 census classification.
#hes_ae_data = 'hes_ae_all_years'
#-- ETHNOS = TThis field contains a code that specifies some ethnic groups and some nationalities. It was introduced from the 1995-96 data year. From April 2001 the codes were changed to conform to the 2001 census classification.
#lowlat_apc_data = 'lowlat_apc_all_years'
#-- ETHNOS = This field contains a code that specifies some ethnic groups and some nationalities. It was introduced from the 1995-96 data year. From April 2001 the codes were changed to conform to the 2001 census classification. -- ETHRAW = Ethnic category is supplied as a two-character field. ETHRAW contains the raw left-hand character. The right hand character is optional for use locally. A copy of the raw data found in the right hand character is held in ETHRAWL. This field represents the raw code as submitted by the provider; it can contain codes that do not represent valid ethnic category codes. See the field ETHNOS for a validated and cleaned version of this field.
#lowlat_op_data = 'lowlat_op_all_years'
#-- ethnos = This field contains a code that specifies some ethnic groups and some nationalities. It was introduced from the 1995-96 data year. From April 2001 the codes were changed to conform to the 2001 census classification. -- ethrawl= Ethnic category is supplied by the trusts as a two-character field. Ethrawl contains the right-hand character. The left-hand character should contain the national code. A copy of the raw data found in the left hand character is held in ethraw
#nicor_congenital_data = "nicor_congenital_dars_nic_391419_j3w9t"
#-- 1_08_ETHNIC_GROUP = 1.08 EthnicGroup Please refer to the data manual for information.
#nicor_hf_data = "nicor_hf_dars_nic_391419_j3w9t"
#-- 1_08_ETHNIC_CATEGORY = 1.08a The self-determined ethnicity of the patient.
#nicor_pci_data = "nicor_pci_dars__nic_391419_j3w9t"
#-- ETHNIC_GROUP = The patient's ethnic group as perceived by the clinician.
#sus_data = "sus_dars_nic_391419_j3w9t"
#-- ETHNIC_CATEGORY_CODE = The coded value for the ethnicity of a person recorded

#This notebook will have 1 record for each patient with the most recent ethnicity snomed code (for now, when conflict, picks the 1st)

#FROM: ------------------
#Column	Content
#NHS_NUMBER_DEID	Patient NHS Number
#CODE	Explanation Ethnicity CODE by SNOMED
#conceptID	Patient Ethnicity conceptID by SNOMED
#PrimaryCode	Mapped concept in NHS/ONS
#record_id	The id of the record from which the data was drawn
#To: --------------------
#Column	Content
#CODE	Patient Ethnicity code by SNOMED
#SNOMED_conceptId_description	Explanation Ethnicity CODE by SNOMED
#PrimaryCode	Mapped concept in NHS/ONS
#n_id_distinct	Number of patients who had a the CODE record

##Set the values for the widgets and import common functions
#Parameters 
collab_database_name = 'dars_nic_391419_j3w9t_collab'
project_prefix = 'ccu037_01_'
 
#Data output name
table_GDPPR = 'ccu037_01_GDPPR_unique_ethnicity_patients' 
table_GDPPR_short = 'ccu037_01_GDPPR_unique_ethnicity_patients_short' 
dbutils.widgets.removeAll();

##Define functions
# Define create table function by Sam Hollings
# Source: Workspaces/dars_nic_391419_j3w9t_collab/DATA_CURATION_wrang000_functions
# Second source were pasted from: https://db.core.data.digital.nhs.uk/#notebook/2317231/command/2452227
def create_table(table_name:str, database_name:str='dars_nic_391419_j3w9t_collab', select_sql_script:str=None) -> None:
  """Will save to table from a global_temp view of the same name as the supplied table name (if no SQL script is supplied)
  Otherwise, can supply a SQL script and this will be used to make the table with the specificed name, in the specifcied database."""
  
  spark.conf.set("spark.sql.legacy.allowCreatingManagedTableUsingNonemptyLocation","true")
  
  if select_sql_script is None:
    select_sql_script = f"SELECT * FROM global_temp.{table_name}"
  
  spark.sql(f"""CREATE TABLE {database_name}.{table_name} AS
                {select_sql_script}
             """)
  spark.sql(f"ALTER TABLE {database_name}.{table_name} OWNER TO {database_name}")
  
def drop_table(table_name:str, database_name:str='dars_nic_391419_j3w9t_collab', if_exists=True):
  if if_exists:
    IF_EXISTS = 'IF EXISTS'
  else: 
    IF_EXISTS = ''
  spark.sql(f"DROP TABLE {IF_EXISTS} {database_name}.{table_name}")
GET SNOMED codes using Tom code
1 Get Data
import pyspark.sql.functions as f
import pyspark.sql.types as t
from pyspark.sql import Window
import pandas as pd
import numpy as np
import re
import seaborn as sns
import matplotlib.pyplot as plt
import io
from matplotlib import dates as mdates
%run "/Workspaces/dars_nic_391419_j3w9t_collab/SHDS/common/functions"
#Parameters 
db = 'dars_nic_391419_j3w9t'
dbc = f'{db}_collab'
 
path_gdppr_alt = f'{db}.gdppr_dars_nic_391419_j3w9t'
 
id_name = 'NHS_NUMBER_DEID'
date_name = 'DATE'
# get gdppr
# gdppr = spark.table(path_gdppr_archive)\
#   .where(f.substring(f.col('ProductionDate'), 0, 10) == production_date)
gdppr = spark.table(path_gdppr_alt)#\
#  .select('NHS_NUMBER_DEID', 'CODE') 
# 'YEAR_OF_BIRTH', 'SEX', 'YEAR_OF_DEATH', 'LSOA', 'ETHNIC', 'REPORTING_PERIOD_END_DATE', 'DATE', )
 
# cache
gdppr.cache()
print(f'{gdppr.count():,}')
 
#Ethnicity mapping (i.e., CODE snowmed and ONS letter associated to it )
map_gdppr_eth = spark.table('dss_corporate.gdppr_ethnicity_mappings')
display(map_gdppr_eth)
count_var(map_gdppr_eth, 'ConceptId')
print()
tmpt = tab(map_gdppr_eth, 'DSS_SYSTEM_CREATED_DATE')
print()
tmpt = tab(map_gdppr_eth, 'PrimaryCode')
eth_codelist = list(
  map_gdppr_eth\
    .select('ConceptId')\
    .toPandas()['ConceptId']\
  )
print(f'{len(eth_codelist):,}')
#print(eth_codelist)
#1.3 SNOMED refset (i.e., where the concept of th snomed CODE is explained )
snomed_refset = spark.table('dss_corporate.gpdata_snomed_refset_full')
display(snomed_refset)
# merge SNOMED refset and map_gdppr_eth to obtain description of snomed code
tmp1 = map_gdppr_eth\
  .select('ConceptId', 'PrimaryCode')\
  .withColumnRenamed('ConceptId', 'SNOMED_conceptId')
tmp2 = merge(snomed_refset, tmp1, ['SNOMED_conceptId'])
 
assert all(tmp2.toPandas()['_merge'].isin(['both', 'left_only']))
 
snomed_desc = tmp2\
  .where(f.col('_merge') == 'both')\
  .select('SNOMED_conceptId', 'SNOMED_conceptId_description', 'PrimaryCode')\
  .withColumnRenamed('SNOMED_conceptId', 'CODE')\
  .distinct()
 
assert snomed_desc.count() == map_gdppr_eth.count()
2 COUNTS
# counts
tmp1 = gdppr\
  .where(f.col('CODE').isin(*eth_codelist))\
  .groupBy('CODE')\
  .agg(\
    f.count(f.lit(1)).alias('n')\
    , f.count(f.col('NHS_NUMBER_DEID')).alias('n_id')\
    , f.countDistinct(f.col('NHS_NUMBER_DEID')).alias('n_id_distinct')\
  )
  
tmp1.cache()
#print(f'{tmp1.count():,}')
 
# add description of snomed code
tmp2 = merge(tmp1, snomed_desc, ['CODE'])\
  .drop('_merge')\
  .na.fill(value=0, subset=['n', 'n_id', 'n_id_distinct'])
# extract records with ethnicity codes and summarise
tmp1 = gdppr\
  .where(f.col('CODE').isin(*eth_codelist))\
  .groupBy('CODE')\
  .agg(\
    f.count(f.lit(1)).alias('n')\
    , f.count(f.col('NHS_NUMBER_DEID')).alias('n_id')\
    , f.countDistinct(f.col('NHS_NUMBER_DEID')).alias('n_id_distinct')\
  )
  
tmp1.cache()
print(f'{tmp1.count():,}')
 
# add descriptions
tmp2 = merge(tmp1, snomed_desc, ['CODE'])\
  .drop('_merge')\
  .na.fill(value=0, subset=['n', 'n_id', 'n_id_distinct'])
 
#display(tmp2)
# reorder and sort
tmp3 = tmp2\
  .select('CODE', 'SNOMED_conceptId_description', 'PrimaryCode', 'n', 'n_id', 'n_id_distinct')\
  .orderBy(f.desc('n_id_distinct'))
display(tmp3)
# NOTE: individuals may have multiple distinct ethnicity codes. The below simply counts the number of records, records with an ID, and individuals with each code. 
Export SNOMED codes with concept Id description:
tmp3ab = tmp3\
  .select('CODE', 'SNOMED_conceptId_description', 'PrimaryCode')\
  .orderBy(f.asc('PrimaryCode'))
display(tmp3ab)
#ccu037_snomed_conceptid_description=spark.createDataFrame(tmp3ab) #if no data base format
ccu037_snomed_conceptid_description=tmp3ab                         #if database format
ccu037_snomed_conceptid_description.createOrReplaceGlobalTempView("ccu037_snomed_conceptid_description")
 
drop_table("ccu037_snomed_conceptid_description")
create_table("ccu037_snomed_conceptid_description")
3 Skinny
I.e., get the latest ethnicity record

From "gdppr_ethnicity_mappings (2) - NEW updated with" --> https://db.core.data.digital.nhs.uk/#notebook/5574101/command/5574136

 
# define window
_win = Window\
  .partitionBy('PERSON_ID')\
  .orderBy(f.desc('RECORD_DATE'))
 
# extract records with ethnicity codes
# create a row number for each individual sorted on descending date, as per the window
# cretae a dense rank number, used to identify tied records
tmp31 = gdppr\
  .select('NHS_NUMBER_DEID', 'RECORD_DATE', 'CODE', 'ETHNIC')\
  .where(f.col('NHS_NUMBER_DEID').isNotNull())\
  .where(f.col('CODE').isin(*eth_codelist))\
  .dropDuplicates()\
  .withColumnRenamed('NHS_NUMBER_DEID', 'PERSON_ID')\
  .withColumn('_rownum', f.row_number().over(_win))\
  .withColumn('_denrank', f.dense_rank().over(_win))
 
tmp31.cache()
count_var(tmp31, 'PERSON_ID')
 
# identify tied records
tmp32 = tmp31\
  .where(f.col('_denrank') == 1)\
  .groupBy('PERSON_ID')\
  .agg(f.countDistinct(f.col('CODE')).alias('_n_distinct_CODE'))\
  .withColumn('_tie', f.when(f.col('_n_distinct_CODE') > 1, 1).otherwise(0))\
  .select('PERSON_ID', '_tie')  
count_var(tmp32, 'PERSON_ID')
tmp33 = tmp31\
  .join(tmp32, on=['PERSON_ID'], how='left')
count_var(tmp33, 'PERSON_ID')
 
# select first record
tmp34 = tmp33\
  .where(f.col('_rownum') == 1)\
  .select('PERSON_ID', 'RECORD_DATE', 'CODE', 'ETHNIC', '_tie')  
count_var(tmp34, 'PERSON_ID')
display(tmp31)
# investigate ties
tmpt = tab(tmp34, '_tie')
 
tmp33a = tmp33\
  .where((f.col('_denrank') == 1) & (f.col('_tie') == 1))\
  .orderBy('PERSON_ID', '_rownum')
print()
count_var(tmp33a, 'PERSON_ID')
 
tmp33b = tmp33a\
  .join(snomed_desc, on=['CODE'], how='inner')\
  .orderBy('PERSON_ID', '_rownum')
print()
count_var(tmp33b, 'PERSON_ID')
display(tmp33b)
 
# ---------------------------------------------------------------------
# COMMENTS
# ---------------------------------------------------------------------
# - work may be needed to combine categories (where appropriate) to reduce the number of tied records e.g.,
#     494131000000105 - White British - ethnic category 2001 census (finding)
#     315236000 - White British (ethnic group)
#     ,
#     92531000000104 - Ethnic category not stated - 2001 census (finding)
#     415226007 - Race not stated (racial group)
#     ,
#     92461000000105 - Pakistani or British Pakistani - ethnic category 2001 census (finding)
#     186002003 - Pakistani (ethnic group)
#
# - work may be needed to construct a heirarchy of ethnic codes that can be applied e.g.,
#     92521000000101 - Other - ethnic category 2001 census (finding)
#     88971000000106 - Albanian - ethnic category 2001 census (finding)
#   where the above are tied, the more informative record (i.e., Albanian) should be prioritised and selected.
#   
# - work may be needed to construct a null indicator i.e., for the "not stated" categories above, so that we can prioritise earlier records with more 
#   information
#
# - hopefully, after the above, only a small number of conflicting record ties would remain, 
#   these could be resolved by a set of rules or random selection accepted (with work undertaken to ensure results can be regenerated if needed).
4 Extra
#import numpy as np
#import pandas as pd
#import databricks.koalas as ks
count_var(tmp33, 'CODE') #no unique ID
print()
count_var(tmp34, 'CODE') #unique ID (only the 1st code)
print()
display(tmp33)
display(tmp34)
#Marta addition: see how many different id_name = 'NHS_NUMBER_DEID' are in GDPPR 
#res = spark.sql(f""" SELECT COUNT(DISTINCT PERSON_ID)
 #              FROM temp34""")
 
#Reminder:
#gdppr_data = 'gdppr_dars_nic_391419_j3w9t'
#{gdppr_data} OR gdppr_dars_nic_391419_j3w9t 
 
# Print
#display(res)
 
display(tmp34
        .groupby("_tie")
        .count()
       )
#47261472 + 4664325 = 51,925,797 (distinct Primary Codes (including transformed SNOMED CODES))
%sql
 
DESCRIBE TABLE tmp34
#From the first record table in Skinny (temp4), see frequency of codes.
tmp35 = tmp34\
  .groupBy('CODE')\
  .agg(\
    f.count(f.lit(1)).alias('n')\
    , f.count(f.col('PERSON_ID')).alias('n_id')\
    , f.countDistinct(f.col('PERSON_ID')).alias('n_id_distinct')\
  )
 
  # add descriptions
tmp35b = merge(tmp35, snomed_desc, ['CODE'])\
  .drop('_merge')\
  .na.fill(value=0, subset=['n', 'n_id', 'n_id_distinct'])
 
display(tmp35b)
# NOTE: we only captured the first record when individuals had conflicts (some may have multiple equal/distinct ethnicity codes). 
# Hence, the below counts of the number of records (n), records with an ID (n), and individuals with each code (n) have the same number. 
# reorder and sort
 
# .select('CODE', 'SNOMED_conceptId_description', 'PrimaryCode', 'n', 'n_id', 'n_id_distinct')\
 
tmp36 = tmp35b\
  .select('CODE','SNOMED_conceptId_description','PrimaryCode','n_id_distinct')\
  .where(f.col('n_id_distinct') > 0)\
  .orderBy(f.desc('n_id_distinct'))
display(tmp36)
# NOTE: individuals may have multiple distinct ethnicity codes. We only captured the first record when individuals had conflicts in the following table/plot.
Save tables to Import in R
 
CU037_01_table_GDPPR = tmp35b #All 489 snomed ethnicity codes
 
CU037_01_table_GDPPR_short = tmp36  #256 snomed ethnicity codes: those who had at least one patient record, and sorted from most to the lowest frequent.
#create_table()
#dataFrame.write.saveAsTable("tmp35b")
 
#create_table(table_GDPPR_short, tmp36) 
 
# Save table
drop_table('CU037_01_table_GDPPR')
drop_table('CU037_01_table_GDPPR_short')
create_table('CU037_01_table_GDPPR')
create_table('CU037_01_table_GDPPR_short')
Select from temp table into table
Set source database and tables, these are the 'core' tables, some ancillary tables may also be used source_table = 'hes_apc_all_years' source_database = 'dars_nic_391419_j3w9t_collab' source_name = 'hes_apc_diag'

source_df = spark.table(f'{source_database}.{source_table}')

outputtable = f'curated{source_name}' output_database = 'dars_nic_391419_j3w9t_collab'

Mine
Parameters
collabdatabase_name = 'dars_nic_391419_j3w9t_collab' project_prefix = 'ccu037_01'

#Data output name
table_GDPPR = 'ccu037_01_GDPPR_unique_ethnicity_patients' table_GDPPR_short = 'ccu037_01_GDPPR_unique_ethnicity_patients_short'

#Original from https://db.core.data.digital.nhs.uk/#notebook/4151107/command/4151114
#spark.table(f'{source_name}_curated_cleaned').write.mode("overwrite").saveAsTable(f"{output_database}.{output_table}")
#spark.sql(f'ALTER TABLE {output_database}.{output_table} OWNER TO {output_database}')
#Modified
spark.table(f'CU037_01_table_GDPPR').write.mode("overwrite").saveAsTable(f"{collab_database_name}.{table_GDPPR}")
spark.sql(f'ALTER TABLE {collab_database_name}.{table_GDPPR} OWNER TO {collab_database_name}')
 
#END of the SCRIPT
