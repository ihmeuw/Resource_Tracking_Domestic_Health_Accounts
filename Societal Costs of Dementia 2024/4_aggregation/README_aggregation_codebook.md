---
title: "README aggregation"
output: html_document
date: '2023-12-06'
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Aggregation variable codebook

*Population variables* (number of individuals in each category):

 1. **dx_comm** - diagnosed in the community
 2. **no_dx_comm** - undiagnosed in the community
 3. **dx_inst** - diagnosed in SNF (skill nursing facility)
 4. **no_dx_inst** - undiagnosed in SNF
 
 
*Attributable direct costs per-case* (medical spending) for each category: 
 (Note: These are PER-CASE not total.)
 
 1. **comm_attr** - attributable per-case cost for those diagnosed in the community
 2. (no variable for this group) - we assume no direct costs for undiagnosed in community
 3. **fac_attr_dx** - attributable per-case cost for those diagnosed in SNF (skill nursing facility)
 4. **fac_attr_no_dx** - attributable per-case cost for those undiagnosed in SNF
 
 *Total direct costs per-case* (medical spending) for each category: 

 1. **comm_total**- attributable per-case cost for those diagnosed in the community
 2. (no variable for this group) - we assume no direct costs for undiagnosed in community
 3. **fac_total_dx** - attributable per-case cost for those diagnosed in SNF (skill nursing facility)
 4. **fac_total_no_dx** - attributable per-case cost for those undiagnosed in SNF
 
 
 *Attributable indirect (caregiving) total costs* for each category:
 
 1. **attributable_caregiving_dx** - attributable cost for all diagnosed individuals in the community
 2. **attributable_caregiving_no_dx** - attributable cost for all undiagnosed individuals in the community
 3. no variable - assuming no informal care in SNF
 4. no variable - assuming no informal care in SNF
 
 *Other variables*
 
  **annual_hours** - total (not attributable) annual hours per-case. 
  
  **pct_female_cg** - the percent of caregivers that are female
  
  **female_annual_hours** - the annual caregiving hours provided by female caregivers (annual_hours*pct_female_cg)
  
  **male_annual_hours** - the annual caregiving hours provided by male caregivers
  
  **cg_af** - caregiving attributable fraction
    
  **attributable_direct_all** - the attributable direct costs for the entire population, calculated as: comm_attr * dx_comm + fac_attr_dx * dx_inst + fac_attr_no_dx * no_dx_inst
  
  **attributable_caregiving_all** - attributable indirect costs for the entire population, calculated as: attributable_caregiving_dx + attributable_caregiving_no_dx
    
    
    
 
