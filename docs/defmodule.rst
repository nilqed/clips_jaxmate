Section 10: Defmodule Construct
===============================

CLIPS provides support for the modular development and execution of
knowledge bases with the **defmodule** construct. CLIPS modules allow a
set of constructs to be grouped together such that explicit control can
be maintained over restricting the access of the constructs by other
modules. This type of control is similar to global and local scoping
used in languages such as C or Ada (note, however, that the global
scoping used by CLIPS is strictly hierarchical and in one direction
only—if module A can see constructs from module B, then it is not
possible for module B to see any of module A’s constructs). By
restricting access to deftemplate and defclass constructs, modules can
function as blackboards, permitting only certain facts and instances to
be seen by other modules. Modules are also used by rules to provide
execution control.

10.1 Defining Modules
---------------------

Modules are defined using the defmodule construct.

``Syntax`` ::

	(defmodule <module-name> [<comment>] <port-specification>*)
	
	<port-specification> ::= (export <port-item>) |
	  (import <module-name> <port-item>)
	  <port-item> ::= ?ALL |
	  ?NONE |
	  <port-construct> ?ALL |
	  <port-construct> ?NONE |
	  <port-construct> <construct-name>+
	  <port-construct> ::= deftemplate | defclass |
	  defglobal | deffunction |
	  defgeneric


A defmodule cannot be redefined or even deleted once it is defined (with
the exception of the MAIN module which can be redefined once). The only
way to delete a module is with the **clear** command. Upon startup and
after a **clear** command, CLIPS automatically constructs the following
defmodule.
::

  (defmodule MAIN)

All of the predefined system classes (see section 9.2) belong to the
MAIN module. However, it is not necessary to import or export the system
classes; they are always in scope. Discounting the previous exception,
the predefined MAIN module does not import or export any constructs.
However, unlike other modules, the MAIN module can be redefined once
after startup or a **clear** command.

Example
::

	(defmodule FOO
	  (import BAR ?ALL)
	  (import YAK deftemplate ?ALL)
	  (import GOZ defglobal x y z)
	  (export defgeneric +)
	  (export defclass ?ALL))


10.2 Specifying a Construct’s Module
------------------------------------

The module in which a construct is placed can be specified when the
construct is defined. The deffacts, deftemplate, defrule, deffunction,
defgeneric, defclass, and definstances constructs all specify the module
for the construct by including it as part of the name. The module of a
defglobal construct is indicated by specifying the module name after the
defglobal keyword. The module of a defmessage-handler is specified as
part of the class specifier. The module of a defmethod is specified as
part of the generic function specifier. For example, the following
constructs would be placed in the DETECTION module.
::

	(defrule DETECTION::Find-Fault
	  (sensor (name ?name) (value bad))
	  =>
	  (assert (fault (name ?name))))
	  (defglobal DETECTION ?*count* = 0)
	  (defmessage-handler DETECTION::COMPONENT get-charge ()
	  (* ?self:flux ?self:flow))
	  (defmethod DETECTION::+ ((?x STRING) (?y STRING))
	  (str-cat ?x ?y))


Example
::

	CLIPS> (clear)
	CLIPS> (defmodule A)
	CLIPS> (defmodule B)
	CLIPS> (defrule foo =>)
	CLIPS> (defrule A::bar =>)
	CLIPS> (list-defrules)
	bar
	For a total of 1 defrule.
	CLIPS> (set-current-module B)
	A
	CLIPS> (list-defrules)
	foo
	For a total of 1 defrule.
	CLIPS>


10.3 Specifying Modules
-----------------------

Commands such as **undefrule** and **ppdefrule** require the name of a
construct on which to operate. In previous versions of CLIPS, constructs
were always referred to by their name only, so it was sufficient just to
pass the name of the construct to these commands. With modules, however,
it is possible to have a construct with the same name in two different
modules. The modules associated with a name can be specified either
explicitly or implicitly. To explicitly specify a name’s module the
module name (a symbol) is listed followed by two colons, ::, and then
the name is listed. The module name followed by :: is referred to as a
**module specifier**. For example, MAIN::find-stuff, refers to the
find-stuff construct in the MAIN module. A module can also be implicitly
specified since there is always a “current” module. The current module
is changed whenever a defmodule construct is defined or the
**set-current-module** function is used. The MAIN module is
automatically defined by CLIPS and by default is the current module when
CLIPS is started or after a **clear** command is issued. Thus the name
find-stuff would implicitly have the MAIN module as its module when
CLIPS is first started.
::

	CLIPS> (clear)
	CLIPS> (defmodule A)
	CLIPS> (defglobal A ?*x* = 0)
	CLIPS> (defmodule B)
	CLIPS> (defglobal B ?*y* = 1)
	CLIPS> (ppdefglobal y)
	(defglobal B ?*y* = 1)
	CLIPS> (ppdefglobal B::y)
	(defglobal B ?*y* = 1)
	CLIPS> (ppdefglobal x)
	[PRNTUTIL1] Unable to find defglobal x.
	CLIPS> (ppdefglobal A::x)
	(defglobal A ?*x* = 0)
	CLIPS>


10.4 Importing and Exporting Constructs
---------------------------------------

Unless specifically **exported** and **imported**, the constructs of one
module may not be used by another module. A construct is said to be
visible or within scope of a module if that construct can be used by the
module. For example, if module *B* wants to use the *foo* deftemplate
defined in module *A*, then module *A* must export the *foo* deftemplate
and module *B* must import the *foo* deftemplate from module *A*.
::

	CLIPS> (clear)
	CLIPS> (defmodule A)
	CLIPS> (deftemplate A::foo (slot x))
	CLIPS> (defmodule B)
	CLIPS> (defrule B::bar (foo (x 3)) =>)
	[PRNTUTIL2] Syntax Error: Check appropriate syntax for defrule
	ERROR:
	(defrule B::bar
	(foo (
	CLIPS> (clear)
	CLIPS> (defmodule A (export deftemplate foo))
	CLIPS> (deftemplate A::foo (slot x))
	CLIPS> (defmodule B (import A deftemplate foo))
	CLIPS> (defrule B::bar (foo (x 3)) =>)
	CLIPS>

CLIPS will **not** allow a module or other construct to be defined that
causes two constructs with the same name to be visible within the same
module.

10.4.1 Exporting Constructs
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The export specification in a defmodule definition is used to indicate
which constructs will be accessible to other modules importing from the
module being defined. Only deftemplates, defclasses, defglobals,
deffunctions, and defgenerics may be exported. A module may export any
valid constructs that are visible to it (not just constructs that it
defines).

There are three different types of export specifications. First, a
module may export all valid constructs that are visible to it. This
accomplished by following the *export* keyword with the *?ALL* keyword.
Second, a module may export all valid constructs of a particular type
that are visible to it. This accomplished by following the *export*
keyword with the name of the construct type followed by the *?ALL*
keyword. Third, a module may export specific constructs of a particular
type that are visible to it. This accomplished by following the *export*
keyword with the name of the construct type followed by the name of one
or more visible constructs of the specified type. In the following code,
defmodule *A* exports all of its constructs; defmodule *B* exports all
of its deftemplates; and defmodule *C* exports the *foo*, *bar*, and
*yak* defglobals.
::

  (defmodule A (export ?ALL))

  (defmodule B (export deftemplate ?ALL))

  (defmodule C (export defglobal foo bar yak))

The ?NONE keyword may be used in place of the ?ALL keyword to indicate
either that no constructs are exported from a module or that no
constructs of a particular type are exported from a module.

Defmethods and defmessage-handlers cannot be explicitly exported.
Exporting a defgeneric automatically exports all associated defmethods.
Exporting a defclass automatically exports all associated
defmessage-handlers. Deffacts, definstances, and defrules cannot be
exported.

10.4.2 Importing Constructs
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The import specification in a defmodule definition is used to indicate
which constructs the module being defined will use from other modules.
Only deftemplates, defclasses, defglobals, deffunctions, and defgenerics
may be imported.

There are three different types of import specifications. First, a
module may import all valid constructs that are visible to a specified
module. This accomplished by following the *import* keyword with a
module name followed by the *?ALL* keyword. Second, a module may import
all valid constructs of a particular type that are visible to a
specified module. This accomplished by following the *import* keyword
with a module name followed by the name of the construct type followed
by the *?ALL* keyword. Third, a module may import specific constructs of
a particular type that are visible to it. This accomplished by following
the *import* keyword with a module name followed by the name of the
construct type followed by the name of one or more visible constructs of
the specified type. In the following code, defmodule *A* imports all of
module *D*\ ’s constructs; defmodule *B* imports all of module *D*\ ’s
deftemplates; and defmodule *C* imports the foo, bar, and *yak*
defglobals from module *D*.
::

  (defmodule A (import D ?ALL))

  (defmodule B (import D deftemplate ?ALL))

  (defmodule C (import D defglobal foo bar yak))

The ?NONE keyword may be used in place of the ?ALL keyword to indicate
either that no constructs are imported from a module or that no
constructs of a particular type are imported from a module.

Defmethods and defmessage-handlers cannot be explicitly imported.
Importing a defgeneric automatically imports all associated defmethods.
Importing a defclass automatically imports all associated
defmessage-handlers. Deffacts, definstances, and defrules cannot be
imported.

A module must be defined before it is used in an import specification.
In addition, if specific constructs are listed in the import
specification, they must already be defined in the module exporting
them. It is not necessary to import a construct from the module in which
it is defined in order to use it. A construct can be indirectly imported
from a module that directly imports and then exports the module to be
used.

10.5 Importing and Exporting Facts and Instances
------------------------------------------------

Facts and instances are “owned” by the module in which their
corresponding deftemplate or defclass is defined, *not* by the module
which creates them. Facts and instances are thus visible only to those
modules that import the corresponding deftemplate or defclass. This
allows a knowledge base to be partitioned such that rules and other
constructs can only “see” those facts and instances that are of interest
to them. Instance names, however, are global in scope, so it is still
possible to send messages to an instance of a class that is not in
scope.

Example
::

	CLIPS> (clear)
	CLIPS> (defmodule A (export deftemplate foo bar))
	CLIPS> (deftemplate A::foo (slot x))
	CLIPS> (deftemplate A::bar (slot y))
	CLIPS> (deffacts A::info (foo (x 3)) (bar (y 4)))
	CLIPS> (defmodule B (import A deftemplate foo))
	CLIPS> (reset)
	CLIPS> (facts A)
	f-1 (foo (x 3))
	f-2 (bar (y 4))
	For a total of 2 facts.
	CLIPS> (facts B)
	f-1 (foo (x 3))
	For a total of 1 fact.
	CLIPS>


10.5.1 Specifying Instance-Names
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Instance-names are required to be unique regardless of the module that
owns them. The syntax of instance-names has been extended to allow
module specifications (note that the left and right brackets in bold are
to be typed and do not indicate an optional part of the syntax).

``Syntax`` ::

	<instance-name> ::= [ <symbol>] | [::<symbol>] | [ <module>::symbol>]

Specifying just a symbol as the instance-name, such as [Rolls-Royce],
will search for the instance in all modules. Specifying only the ``::``
before the name, such as ``[::Rolls-Royce]``, will search for the instance
first in the current module and then recursively in the imported modules
as defined in the module definition. Specifying both a symbol and a
module name, such as ``[CARS::Rolls-Royce]``, searches for the instance only
in the specified module.

10.6 Modules and Rule Execution
-------------------------------

Each module has its own pattern-matching network for its rules and its
own agenda. When a **run** command is given, the agenda of the module
that is the current focus is executed (note that the **reset** and
**clear** commands make the MAIN module the current focus). Rule
execution continues until another module becomes the current focus, no
rules are left on the agenda, or the **return** function is used from
the RHS of a rule. Whenever a module that was focused on runs out of
rules on its agenda, the current focus is removed from the focus stack
and the next module on the focus stack becomes the current focus. Before
a rule executes, the current module is changed to the module in which
the executing rule is defined (the current focus). The current focus can
be changed by using the **focus** command. See sections 5.2, 5.4.10.2,
12.12, and 13.12 for more details.

Example
::

	CLIPS> (clear)
	CLIPS> (defmodule MAIN)
	CLIPS>
	(defrule MAIN::focus-example
	  =>
	  (printout t "Firing rule in module MAIN." crlf)
	  (focus A B))
	CLIPS> (defmodule A)
	CLIPS>
	(defrule A::example-rule
	  =>
	  (printout t "Firing rule in module A." crlf))
	CLIPS> (defmodule B)
	CLIPS>
	(defrule B::example-rule
	  =>
	  (printout t "Firing rule in module B." crlf))
	CLIPS> (reset)
	CLIPS> (run)
	Firing rule in module MAIN.
	Firing rule in module A.
	Firing rule in module B.
	CLIPS>


.. _section-8:
