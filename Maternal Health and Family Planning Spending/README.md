

# Estimating domestic spending on maternal health and family planning

## Project repository structure:  

- `src`: for helper functions/libraries used by analysis pipelines
- `data`: for storing all types of data and outputs  
    - `meta`: metadata
    - `outputs`: analysis outputs, like figures, tables, or data products
    - `data`: input, intermediate, and output data files used/created by analyses
- `analysis`: pipelines for processing data and generating estimates  
    - `0_auxiliary`: scripts for generating auxiliary data
    - `1_retro/domestic`: pipeline for estimating domestic spending on maternal health and family planning
    - `1_retro/donor`: pipeline for estimating donor spending on prevention of harmful practices

## Useful Acronyms/Terminology

- ODA: Official development assistance  
- DAH: Development Assistance for Health  
- Domestic: typically refers to domestic health spending, which can be out-of-pocket, government, or prepaid private.
- OOP: Out-of-pocket expenditures  
- Public: Public/government expenditures  
- PPP: Prepaid-private expenditures  
- RH: Reproductive health  
- MH: Maternal health  
- FP: Family planning  
- HP: Harmful practice(s), which include GBV, ECM, and FGM  
- GBV: Gender-based violence  
- ECM: Early and child marriages (sometimes referred to as "CM")  
- GFM: Female genital mutilation  
- SHA: System of Health Accounts
- Dis2: Reproductive health (based on the coding used in SHA 2011) 
- Dis2.1: Maternal health (based on the coding used in SHA 2011)  
- Dis2.3: Family planning (based on the coding used in SHA 2011)  
- CM: Contraceptive Method(s)  
- Repr. pop: reproductive population  
