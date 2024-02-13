## High-Level Pipeline

```{mermaid}
graph TB;
    A[External Sources] --> |Geospatial Modeling| B[External Exposure];
    B[External Exposure] --> |Behaviorial and Physiological Modeling| C[Internal Exposure];
    C[Internal Exposure] --> |PBTK| D[Target Organ Dose];
    D[Target Organ Dose] --> |IVIVE| E[In vitro Equivalent Concentration];
    E[In vitro Equivalent Concentration] --> |Mixtures Modeling| F[Concentration Response];
   
```
