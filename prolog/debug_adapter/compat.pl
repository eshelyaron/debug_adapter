:- module(
       da_compat,
       [
       ]
   ).

:- if(\+current_predicate(det/1)).
user:det(_).
:- endif.
