# SARS-CoV-2 serology - Version 2
## measure
**net_mfi_soc = net_mfi/(cutoff*empty)**

soc = signal over cutoff  
empty = signal of the empty beads

### cutoff
There's a different cutoff for each of the 12 parameter.

|       ~target     |  ~cutoff |                |
|-------------------|----------|----------------|
|         "IgG_NP"  | 7.7      |  # mean + 6*sd |
|         "IgG_RBD" | 3.5      |  # mean + 4*sd |
|         "IgG_S1"  | 2.3      |  # mean + 3*sd |
|         "IgG_S2"  | 62.8     |  # mean + 6*sd |
|                   |          |                |
|         "IgA_NP"  | 27.7     |  # mean + 6*sd |
|         "IgA_RBD" | 13.9     |  # mean + 6*sd |
|         "IgA_S1"  | 6.7      |  # mean + 6*sd |
|         "IgA_S2"  | 35.8     |  # mean + 6*sd |
|                   |          |                |
|         "IgM_NP"  | 49.2     |  # mean + 4*sd |
|         "IgM_RBD" | 66.8     |  # mean + 4*sd |
|         "IgM_S1"  | 22.9     |  # mean + 4*sd |
|         "IgM_S2"  | 25.2     |  # mean + 4*sd |

## interpretation
In addition to positive and negative, there's something in between (gw). The net_mfi_soc thresholds are:  
- IgG_S1: 0.895  
- IgG_NP: 0.884  
- IgG_RBD: 0.714  

### S1
positive if net_mfi_soc > 1  
gw only for IgG if net_mfi_soc > 0.895

### S2
positive if net_mfi_soc > 1

### NP
positive if net_mfi_soc > 1  
gw only for IgG if net_mfi_soc > 0.884

### RBD
positive if net_mfi_soc > 1  
gw only for IgG if net_mfi_soc > 0.714

### overall isotype result
there's no overall result anymore

### Serokonversion
It's evaluated in the order listed here, first condition which is TRUE is reported.

"Positiv, fortgeschritten" if at least two parameter were tested positive (OR one positive and two borderline (gw)) AND (at least one IgG parameter's borderline (gw) OR positive)  
"Positiv, partiell" if at least two parameter were tested positive AND all IgG parameter were negative  
"Schwach reaktiv" if one of IgG_Resultat_S1, IgG_Resultat_NP, IgG_Resultat_RBD is positive OR (at least one IgG parameter's borderline AND at least one IgA, IgM parameter's positive)  
"Indeterminat" at least one parameter of the following IgA (N, RBD, S1, S2), IgM (N, RBD, S1, S2), IgG (S2) was tested positive  
"Negativ" all other cases (no parameter positive)  

## Kommentar
there's no additional comment anymore

## flags
Fehler_count if count of any target is below a set threshold of 20  
Fehler_empty if Empty_net_mfi of any isotype is above a set threshold of:  
- IgG: 40.05
- IgA: 55.26
- IgM: 539.74


## changelog
- 2.0.0
  - updated version for diagnostics
  - normalising on empty beads signal
  - including measurements of additional parameters
  - updated interpretation
  - removal of overall isotype result
  - removal of additional comment
  - different specifications for Fehler_empty

- 1.0.0
  - first version used for diagnostics
  - normalising on neg control(s) signal
