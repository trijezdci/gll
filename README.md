# gll
Grammar Tool for LL(1) Grammars

GLL is a tool to verify and visualise LL(1) grammars.  It parses EBNF
specifications of grammars, calculates FIRST and FOLLOW sets, reports
any LL(1) violations and generates (railroad) syntax diagrams.

#### Future Outlook

Eventually it is intended to add further capabilities:

(1) a compiler front-end generator

that will generate lexers and parsers from an LL(1) grammar;

(2) a compiler mid-end generator

that will generate ASTs and AST walkers from a formal AST specification;

(3) a compiler back-end generator

that will generate a code generator from a formal target specification.
