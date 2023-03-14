# sicp-picture-language

An internal DSL written in Racket Scheme.
It is the implementation of a DSL for an "algebra of pictures", published by Peter Henderson in 2002[^1].
The publication is named "Functional Geometry" and the original version of the paper was published in 1982[^2].

> **Abstract.** An algebra of pictures is described that is sufficiently powerful to denote the structure of a well-known Escher woodcut, Square Limit. A decomposition of the picture that is reasonably faithful to Escher's original design is given. This illustrates how a suitably chosen algebraic specification can be both a clear description and a practical implementation method. It also allows us to address some of the criteria that make a good algebraic description.

> [...] computers, computer science and functional programming were all a lot less powerful than they are today. The idea that one could write an algebraic description, embed it in a functional program, and execute it directly was not new. But it was not then considered a practical programming technique. Now we know better and many examples exist of **how practical it can be to simply write denotations of what is to be constructed, rather than to write algorithmic descriptions of how to perform the construction.** (emphasis mine)

[^1]: Henderson, P. (2002). Functional geometry. *Higher-order and symbolic computation* 15 (2002): 349-365.

[^2]: Henderson, P. (1982, August). Functional geometry. In *Proceedings of the 1982 ACM Symposium on LISP and Functional Programming* (pp. 179-187).
