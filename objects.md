## Proposed Objects and Methods
```mermaid
classDiagram
    class GeoTox {
        +id: character, numeric
        +chems: list
        +aeid: numeric
    }
  
      class Group {
        +geometry: sf
        +crs: sf
        +population: sf
        +chems: sf
        +aeid: sf
        + ...  sf
        +sample_*()
        +simulate_*
    }

    class Person {
        +age: numeric
        +obesity: character
        +ir: numeric
        +calc_internal_dose()
        +calc_invitro_concentration()
        +calc_Css()
    }

    class Chemical {
        +casn: character
        +smiles: list
        +c-r: data.frame: logc, resp
        +resp: data.frame
        +fit_hill()
        +calc_concentration_response()
        ~obj_*()
        ~tcpl*()
    }

    class Assay {
        +properties: data.frame: casn, dsstox, aeid, resp_units
    }

    GeoTox <-- Group
    Group <-- Person
    Chemical <--> Assay
    Chemical --> Person

```

## NOTES
- Person method using httk to generate C_ss
- Person method fetch Chemical.params to compute risk
-  Region method input chemical_info to set its Person's C_ext values
-  sf objects are data.frames or data.tables, hence the additional information for `Group` can all be contained in the `sf` object
