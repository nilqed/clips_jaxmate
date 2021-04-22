Section 4: Deffacts Construct
=============================

With the **deffacts** construct, a list of facts can be defined which
are automatically asserted whenever the **reset** command is performed.
Facts as­serted through deffacts may be retracted or pattern-matched
like any other fact. The initial fact-list, including any defined
deffacts, is always reconstructed after a **reset** command.

``Syntax`` ::

  (deffacts <deffacts-name> [<comment>] | <RHS-pattern>*)

Redefining a currently existing deffacts causes the previous deffacts
with the same name to be removed even if the new definition has errors
in it. There may be multiple deffacts constructs and any number of facts
(either ordered or deftemplate) may be asserted into the initial
fact-list by each deffacts construct.

Dynamic expressions may be included in a fact by embedding the
expression directly within the fact. All such expressions are evaluated
when CLIPS is reset.

``Example`` ::

  (deffacts startup "Refrigerator Status"
    (refrigerator light on)
    (refrigerator door open)
    (refrigerator temp (get-temp)))

Upon startup and after a **clear** command, CLIPS automatically
constructs the following deftemplate and deffacts.
::

  (deftemplate initial-fact)
  (deffacts initial-fact
  (initial-fact))

In prior versions of CLIPS, the initial-fact deffacts provided a
convenient method for starting the execution of a system: rules without
conditional elements had an (initial-fact) pattern automatically added
to their LHS. When a **reset** command was issued, the assertion of the
(intial-fact) would activate these rules. Starting with version 6.3 of
CLIPS, rules without conditional elements are automatically matched
without the need for the (initial-fact) assertion. Usage of the
initial-fact is now deprecated. Programs should not rely on the
assertion of this fact when a reset is performed.

.. _section-2:
