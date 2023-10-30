# Instant
To build the compilers use `make`, via either `make` or `make all`. To clean files created via compilation use `make clean.
## Structure
Standard cabal project structure:
- `app` contains sources for executables
- `src` contains sources for library used by these executables
- `lib` contains `jasmin.jar` used to turn generated `.j` files into `.class` files.
## Dependencies
All used Haskell packages are outlined in `Instant.cabal` file. There is one external dependency - sources inside `src/Instant/Grammar` were created using BNFC from `src/Instant/Grammar/Instant.cf` file.