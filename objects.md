## Proposed Objects and Methods
```mermaid
classDiagram
    class GeoTox {
        +id: data.frame
        +chems: list
    }
  
      class Group {
        +definition: string
        +geometry: sf
        +population: list
        +chems: list
    }

    class Person {
        +age: numeric
        +obesity: character
        +ir: numeric
    }

    class Chemical {
        +casn: character
        +smiles: list
        +c-r: data.frame
        +resp: data.frame
    }

    class Assay {
        +casn: list
        +smiles: list
        +aeid: data.frame
    }

    GeoTox <-- Group
    Group <-- Person
    Chemical <--> Assay
    Chemical --> Group
    Chemical --> Person

```

## NOTES
- Person method using httk to generate C_ss
- Person method fetch Chemical.params to compute risk
-  Region method input chemical_info to set its Person's C_ext values
