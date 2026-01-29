# sensitivity analysis

    Code
      res
    Output
      $risk_sensitivity_age
      # A tibble: 3 x 6
        assay_id sample_id GCA.Eff IA.Eff GCA.HQ.10 IA.HQ.10
           <int>     <int>   <dbl>  <dbl>     <dbl>    <dbl>
      1       NA         1    1.94   1.94      9.27     9.27
      2       NA         2    2.06   2.06     10.4     10.4 
      3       NA         3    2.13   2.13     11.3     11.3 
      
      $risk_sensitivity_weight
      # A tibble: 3 x 6
        assay_id sample_id GCA.Eff IA.Eff GCA.HQ.10 IA.HQ.10
           <int>     <int>   <dbl>  <dbl>     <dbl>    <dbl>
      1       NA         1   0.663  0.663      1.89     1.89
      2       NA         2   0.812  0.812      2.42     2.42
      3       NA         3   2.96   2.96      30.7     30.7 
      
      $risk_sensitivity_css_params
      # A tibble: 3 x 6
        assay_id sample_id GCA.Eff IA.Eff GCA.HQ.10 IA.HQ.10
           <int>     <int>   <dbl>  <dbl>     <dbl>    <dbl>
      1       NA         1   0.591  0.591      1.64     1.64
      2       NA         2   0.479  0.479      1.29     1.29
      3       NA         3   1.14   1.14       3.82     3.82
      
      $risk_sensitivity_fit_params
      # A tibble: 3 x 6
        assay_id sample_id GCA.Eff IA.Eff GCA.HQ.10 IA.HQ.10
           <int>     <int>   <dbl>  <dbl>     <dbl>    <dbl>
      1       NA         1   1.47   1.47    320.     320.   
      2       NA         2   0.376  0.376     0.899    0.899
      3       NA         3   0.610  0.610     1.31     1.31 
      
      $risk_sensitivity_C_ext
      # A tibble: 3 x 6
        assay_id sample_id GCA.Eff IA.Eff GCA.HQ.10 IA.HQ.10
           <int>     <int>   <dbl>  <dbl>     <dbl>    <dbl>
      1       NA         1   1.00   1.00       3.20     3.20
      2       NA         2   0.829  0.829      2.49     2.49
      3       NA         3   2.38   2.38      14.8     14.8 
      

