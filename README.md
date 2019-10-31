```

   A      ...:'....:'':...':......
   R    :''   ._   .  `.  \   ,   '':
   B    ':  .   \" .|    \  `>/   _.-':
   O   .:'  .`'.   `-.  '. /'  ,..  .:
   G  :'        `.    `\| \./   ' :
   E  :. ,,-'''''  \"-.  |   | ....:
   N   '.      ..'''  `\ :   |
         ''''''''       \'   |
*fast* uniform random    |  =|
       tree generator    |   |
                         |-  |
'''''''''''''''''''''''''''''''''''''''
(C) F. Peschanski et al. under the GPL
```

Arbogen is a fast uniform random generator of tree structures.
The tool reads a grammar file describing a tree structure
(e.g. binary trees, 2-3-4 trees, etc.) and a size interval
(eg. trees of size 1000+-100). From the grammar one or
many trees satisfying the structure and the size interval
are produced (following simple file formats).
The generated tree are generated with a guarantee of uniformity,
which means that it is the "average" tree for the given size.

----

![Tree example](https://github.com/fredokun/arbogen/wiki/images/tree_nary_seq_big.png)

Note: this example is a plane rooted tree of more than 100.000 nodes obtained in a few seconds on a standard PC.

----

Internally, the tool is based on recent advances in analytic
combinatorics and Boltzmann generators. The generation is
roughly linear in the size of the trees, which means huge
trees (e.g. of more than 1 millions nodes) can be generated
in a few seconds, memory management being the main
performance bottleneck.

Arbogen can be used in the very many situations when tree-shaped
data structures must be manipulated, e.g. in random testing.

----

This software was inspired by the research work of Alexis Darasse.
It is distributed under the GNU Public license v.2 (cf. `LICENSE.txt`).

----

## Build & install arbogen

Arbogen is packaged as a library and command line tool. The library exposes the
internal grammar representation, our oracle implementation and the random
generator for use as part of other projects. Note that the library should be
considered rather unstable and *will* change in the future.

Arbogen is packaged for the `opam` package manager. It can be installed in your
current opam switch by running one of this two commands from the root of the
repository:

- For **Opam 2**: `opam install .`
- For **Opam 1**: `opam pin add arbogen . && opam install arbogen`. (Not tested
  but should work, source: https://opam.ocaml.org/blog/opam-1-2-pin/).

In both cases, opam will resolve all the dependencies and install them in your
current switch. In case you do not want to use opam, you should first install
the dependencies listed in `arbogen.opam` and then run `dune build @install` or
`make build`. This will build both the executable and the library and expose
them under `bin/` and `lib/` at the root of the repository.

### Documentation

A rather sparse documentation can be build using `dune build @doc` or `make
doc`. Please feel free to open an issue if you need some parts of the
documentation to be clarified.
