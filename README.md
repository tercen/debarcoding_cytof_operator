# Debarcoding CyTOF

##### Description

The `Debarcoding CyTOF operator` is an operator to perform the Debarcoding of mass
cytometry data.

##### Usage

Input projection|.
---|---
`row`        | channels 
`column`     | observations (rowId) + filename
`y-axis`     | measurement values
`label`     | documentId of sample_key matrix file


Input parameters|.
---|---
`Separation Cutoff`| Cutoff to be applied to the data (-1 for population specific cutoff)

Output relations|.
---|---
`Scaled`        | Normalised measurements
`Barcode`        | Flags indicating whether a measurement was assigned to a barcode. One column per barcode.
`Diagnostic plot`        | Computed tables include a graph of the estimated compensation matrix.

##### Details

This operator uses the debarcoding approach described in the [CATALYST R package](https://www.bioconductor.org/packages/devel/bioc/vignettes/CATALYST/inst/doc/preprocessing.html#debarcoding-workflow).

##### See Also

[normalise_cytof_operator](https://github.com/tercen/normalise_cytof_operator)
, [compensate_cytof_operator](https://github.com/tercen/compensate_cytof_operator)