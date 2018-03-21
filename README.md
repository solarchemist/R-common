## A collection of general functions and data

Includes common numerical functions, unit converters,
some LaTeX-specific functions, as well as reference data.


## Known bugs

`as.SHE()` and `from.SHE()` will fail to return any results if a vector of potentials is supplied along with a vector containing more than one reference scale (e.g., `as.SHE(potentials = c(0.24, 0.46, -0.15), scale = c("AgCl", "SCE", "AVS"))`).
This bug was first confirmed for vesion `0.0.0.9011`.
