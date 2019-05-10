# insert-molecule

Installation with Stack
-----------------------
Stackage is a stable package archive. Stackage builds are supposed to
be reproducible. Stackage also provides Long Term Support releases.
To build Moo with Stackage dependencies, use the `stack` tool:

  * install [`stack`](https://docs.haskellstack.org/)
  * if necessary, install GHC: run `stack setup`
  * run: `stack update`
  * in the project source directory run: `stack build`

### Build Status

[![Build Status](https://travis-ci.org/wurthel/insert-molecule.svg?branch=master)](https://travis-ci.org/wurthel/insert-molecule)

To run
------
To run the programm: 

`stack exec insert-molecule-exe config`

where `config` is configuration file.
