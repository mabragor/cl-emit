time-liquid
===========

TIME-LIQUID is a framework for specifying, how to serialize objects.

It is largely inspired by the great ESRAP package, and mine fork of it, ESRAP-LIQUID.
Hence, key features are:
  - memoization
  - non-determinism
  - rule-based top-down specification of serialization
  - convenient S-exp-context reader macros via CL-READ-MACRO-TOKENS
  - full CL is available when defining rules

See tests for examples. Also, CL-YACLYAML package uses this package to dump Lisp structures as YAML, so
see code in CL-YACLYAML (file presenting.lisp) for some real-life examples.

Bugreports, fixes and suggestions are highly welcome!