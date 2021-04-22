Section 7: Deffunction Construct
================================

With the **deffunction** construct, new functions may be defined
directly in CLIPS. Deffunctions are equivalent in use to other
functions; see section 2.3.2 for more detail on calling functions in
CLIPS. The only differences between user-defined external functions and
deffunctions are that deffunctions are written in CLIPS and executed by
CLIPS interpretively and user-defined external functions are written in
an external language, such as C, and executed by CLIPS directly. Also,
deffunctions allow the addition of new functions without having to
recompile and relink CLIPS.

A deffunction is comprised of five elements: 1) a name, 2) an optional
comment, 3) a list of zero or more required parameters, 4) an optional
wildcard parameter to handle a variable number of arguments and 5) a
sequence of actions, or expressions, which will be executed in order
when the deffunction is called.

``Syntax`` ::

  (deffunction <name> [<comment>]
    (<regular-parameter>\* [<wildcard-parameter>])
     <action>*)

   <regular-parameter> ::= <single-field-variable>
   <wildcard-parameter> ::= <multifield-variable>

A deffunction must have a unique name different from all other system
functions and generic functions. In particular, a deffunction cannot be
overloaded like a system function (see section 8 for more detail). A
deffunction must be declared prior to being called from another
deffunction, generic function method, object message-handler, rule, or
the top level. The only exception is a self recursive deffunction.

A deffunction may accept *exactly* or *at least* a specified number of
arguments, depending on whether a wildcard parameter is used or not. The
regular parameters specify the minimum number of arguments that must be
passed to the deffunction. Each of these parameters may be referenced
like a normal single-field variable within the actions of the
deffunction. If a wildcard parameter is present, the deffunction may be
passed any number of arguments greater than or equal to the minimum. If
no wildcard parameter is present, then the deffunction must be passed
exactly the number of arguments specified by the regular parameters. All
arguments to a deffunction that do not correspond to a regular parameter
are grouped into a multifield value that can be referenced by the
wildcard parameter. The standard CLIPS multifield functions, such as
length and nth, can be applied to the wildcard parameter.

Example
::

	CLIPS> (clear)
	CLIPS>
	(deffunction print-args (?a ?b $?c)
	  (printout t ?a " " ?b " and " (length ?c) " extras: " ?c crlf))
	CLIPS> (print-args 1 2)
	1 2 and 0 extras: ()
	CLIPS> (print-args a b c d)
	a b and 2 extras: (c d)
	CLIPS>


When a deffunction is called, its actions are executed in order. The
return value of a deffunction is the evaluation of the last action. If a
deffunction has no actions, its return value is the symbol FALSE. If an
error occurs while the deffunction is executing, any actions not yet
executed will be aborted, and the deffunction will return the symbol
FALSE.

Deffunctions may be self and mutually recursive. Self recursion is
accomplished simply by calling the deffunction from within its own
actions.

Example
::

  (deffunction factorial (?a)
    (if (or (not (integerp ?a)) (< ?a 0)) then
      (printout t "Factorial Error!" crlf)
     else
      (if (= ?a 0) then
         1
       else
         (* ?a (factorial (- ?a 1))))))

Mutual recursion between two deffunctions requires a forward declaration
of one of the deffunctions. A forward declaration is simply a
declaration of the deffunction without any actions. In the following
example, the deffunction foo is forward declared so that it may be
called by the deffunction bar. Then the deffunction foo is redefined
with actions that call the deffunction bar.

Example
::

  (deffunction foo ())

  (deffunction bar ()
    (foo))

  (deffunction foo ()
    (bar))

Care should be taken with recursive deffunctions; too many levels of
recursion can lead to an overflow of stack memory (especially on PC-type
machines).

.. _section-5: