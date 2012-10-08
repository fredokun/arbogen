<pre>

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

</pre>
 
Arbogen is a fast uniform random generator of tree structures.
The tool reads a grammar file describing a tree structure
(e.g. binary trees, 2-3-4 trees, etc.) and a size interval
 (eg. trees of size 1000+-100). From the grammar one or
many trees satisfying the structure and the size interval
are produced (following simple file formats).
The generated tree are generated with a guarantee of uniformity,
 which means that it is the "average" tree for the given size.

Internally, the tool is based on recent advances in analytic
combinatorics and Boltzman generators.  The generation is
roughly linear in the size of the trees, which means huge
trees (e.g. of more than 1 millions nodes) can be generated
 in a few seconds, memory management being the main
performance bottleneck.

Arbogen can be used in the very many situations when tree-shaped
datastructures must be manipulated, e.g. in random testing.

----
This software was inspired by the research work of Alexis Darasse.
It is distributed under the GNU Public license v.2 (cf. LICENSE.txt).

