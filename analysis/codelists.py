from cohortextractor import (
    codelist_from_csv,
    codelist,
)

enzyme_replace = codelist_from_csv(
    "codelists/user-agleman-pancreatic-enzyme-replacement-therapy.csv",
    system="snomed",
    column="code",
)

pan_cancer_codes = codelist_from_csv(
    "codelists/user-agleman-pancreatic_cancer_snomed.csv",
    system="snomed",
    column="code",
)

pa_ca_icd10 = codelist_from_csv(
    "codelists/user-agleman-pancreatic-cancer-icd10.csv",
    system="icd10",
    column="code",
)

# Pancreatic resection
pancreatic_resection_OPCS4 = codelist_from_csv(
    "codelists/user-agleman-resection-of-pancreatic-cancer-opcs-4.csv",
    system="opcs4",
    column="Code",
)
