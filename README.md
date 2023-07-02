# My collection of R utility functions

Includes common utilities and converters as well as some functions tailored
heavily towards my own use cases.

If you find something you like, feel free to copy it and use it for your own work.
If you spot a bug, open an issue or [let me know](https://solarchemist.se/contact)
(with my appreciation!).

Note that the package is named `common`, despite the repository being name `R-common`.
Sorry about that (this was one of the very first packages I created, and I haven't
gotten around to fixing the naming mismatch yet).

[This repository is mirrored on git.solarchemist.se](https://git.solarchemist.se/taha/R-common).


## Install this package

To use this package, install it from this repo:

```
install.packages("remotes")
remotes::install_github("solarchemist/R-common")
```


## Develop this package

Check out the source code from this repo:
```
git clone https://github.com/solarchemist/R-common.git
```

I suggest the following package rebuild procedure:

+ Run `devtools::check()`.
  Should complete with no warnings, errors or notes:
```
── R CMD check results ─────────────────────────────────────── common 0.1.0 ────
Duration: 8.1s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔
```
+ If necessary, run `devtools::document()` to update the documentation.

Contributions are welcome, no matter whether code, bug reports or suggestions!
