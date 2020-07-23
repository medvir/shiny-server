# SARS-CoV-2 serology - Version 1
## measure
**net_mfi_foc = net_mfi/(cutoff*net_mfi_neg)**

foc = fold over cutoff  
cutoff = 3  
net_mfi_neg = mean of net_mfi of all negative samples (name has to contain "neg", case insensitive)

## interpretation
### S1
positive if net_mfi_foc > 1

### S2
positive if net_mfi_foc > 1 AND at least one S1 result is positive

### NP
positive if net_mfi_foc > 1 AND at least one S1 result is positive

### overall isotype result
IgG, IgA, IgM positive if at least one target (S1, S2, NP) is positive

### Serokonversion
"fs (fortgeschritten)" if IgG_Resultat_S1 is positive  
"ps (partiell)" if either IgA_Resultat_S1 or IgM_Resultat_S1 is positive  
"ks (keine)" if none of IgG_Resultat_S1, IgA_Resultat_S1, IgM_Resultat_S1 is positive

## Kommentar
"\*sarsk (Kreuzrkt whs)" if Serokonversion is "ks (keine)" AND at least one target is positive  
"S1 Reaktivität grenzwertig, bitte Verlaufsprobe einsenden" if Serokonversion is "ks (keine)" AND at least one S1_net_mfi_foc of the three isotypes is >= 0.9

## flags
Fehler_count if count of any target is below a set threshold of 20  
Fehler_empty if Empty_net_mfi_foc of any isotype is above a set threshold of 1


## changelog
- 1.0.0
  - first version used for diagnostics
