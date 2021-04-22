Section 11: Constraint Attributes
=================================

This section describes the constraint attributes that can be associated
with deftemplates and defclasses so that type checking can be performed
on slot values when template facts and instances are created. The
constraint information is also analyzed for the patterns on the LHS of a
rule to determine if the specified constraints prevent the rule from
ever firing.

Two types of constraint checking are supported: static and dynamic. When
static constraint checking is enabled, constraint violations are checked
when function calls and constructs are parsed. This includes constraint
checking between patterns on the LHS of a rule when variables are used
in more than one slot. When dynamic constraint checking is enabled,
newly created data objects (such as deftemplate facts and instances)
have their slot values checked for constraint violations. Essentially,
static constraint checking occurs when a CLIPS program is loaded and
dynamic constraint checking occurs when a CLIPS program is running. By
default, static constraint checking is enabled and dynamic constraint
checking is disabled. The default behavior can be changed by using the
**set-static-constraint-checking** and
**set-dynamic-constraint-checking** functions.

Unless dynamic constraint checking is enabled, constraint information
associated with constructs is not saved when a binary image is created
using the **bsave** command.

The general syntax for constraint attributes is shown following.

``Syntax`` ::

	<constraint-attribute> ::= <type-attribute> |
	  <allowed-constant-attribute> |
	  <range-attribute> |
	  <cardinality-attribute>

11.1 Type Attribute
-------------------

The type attribute allows the types of values to be stored in a slot to
be restricted.

``Syntax`` ::

	<type-attribute> ::= (type <type-specification>)
	
	<type-specification> ::= <allowed-type>+ | ?VARIABLE
	
	<allowed-type> ::= SYMBOL | STRING | LEXEME |
	  INTEGER | FLOAT | NUMBER |
	  INSTANCE-NAME | INSTANCE-ADDRESS | INSTANCE |
	  EXTERNAL-ADDRESS | FACT-ADDRESS

Using NUMBER for this attribute is equivalent to using both INTEGER and
FLOAT. Using LEXEME for this attribute is equivalent to using both
SYMBOL and STRING. Using INSTANCE for this attribute is equivalent to
using both INSTANCE-NAME and INSTANCE-ADDRESS. ?VARIABLE allows any type
to be stored.

11.2 Allowed Constant Attributes
--------------------------------

The allowed constant attributes allow the constant values of a specific
type that can be stored in a slot to be restricted. The list of values
provided should either be a list of constants of the specified type or
the keyword ?VARIABLE which means any constant of that type is allowed.
The allowed-values attribute allows the slot to be restricted to a
specific set of values (encompassing all types). Note the difference
between using the attribute (allowed-symbols red green blue) and
(allowed-values red green blue). The allowed-symbols attribute states
that if the value is of type symbol, then its value must be one of the
listed symbols. The allowed-values attribute completely restricts the
allowed values to the listed values. The allowed-classes attribute does
not restrict the slot value in the same manner as the other allowed
constant attributes. Instead, if this attribute is specified and the
slot value is either an instance address or instance name, then the
class to which the instance belongs must be a class specified in the
allowed-classes attribute or be a subclass of one of the specified
classes.

``Syntax`` ::

	<allowed-constant-attribute> ::= (allowed-symbols <symbol-list>) |	
	  (allowed-strings <string-list>) |
	  (allowed-lexemes <lexeme-list> |
	  (allowed-integers <integer-list>) |
	  (allowed-floats <float-list>) |
	  (allowed-numbers <number-list>) |
	  (allowed-instance-names <instance-list>) |
	  (allowed-classes <class-name-list>)
	  (allowed-values <value-list>)
	
	
	  <symbol-list> ::= <symbol>+ | ?VARIABLE
	  
	  <string-list> ::= <string>+ | ?VARIABLE
	  
	  <lexeme-list> ::= <lexeme>+ | ?VARIABLE
	  
	  <integer-list> ::= <integer>+ | ?VARIABLE
	  
	  <float-list> ::= <float>+ | ?VARIABLE
	  
	  <number-list> ::= <number>+ | ?VARIABLE
	  
	  <instance-name-list> ::= <instance-name>+ | ?VARIABLE
	  
	  <class-name-list> ::= <class-name>+ | ?VARIABLE
	  
	  <value-list> ::= <constant>+ | ?VARIABLE


Specifying the allowed-lexemes attribute is equivalent to specifying
constant restrictions on both symbols and strings. A string or symbol
must match one of the constants in the attribute list. Type conversion
from symbols to strings and strings to symbols is not performed.
Similarly, specifying the allowed-numbers attribute is equivalent to
specifying constant restrictions on both integers and floats. In CLIPS
5.1, type conversion of integers to floats and floats to integers was
performed when using the allowed-numbers attribute (thus using
allowed-numbers was not equivalent to using both the allowed-integers
and allowed-floats attributes together). In CLIPS 6.0, this type
conversion is no longer performed. The allowed-instances attribute found
in CLIPS 5.1 is no longer supported. The allowed-instance-names
attribute should be used in its place.

11.3 Range Attribute
--------------------

The range attribute allows a numeric range to be specified for a slot
when a numeric value is used in that slot. If a numeric value is not
used in that slot, then no checking is performed.

``Syntax`` ::

	<range-attribute> ::= (range <range-specification> <range-specification>)
	
	<range-specification> ::= <number> | ?VARIABLE

Either integers or floats can be used in the range specification with
the first value to the range attribute signifying the minimum allowed
value and the second value signifying the maximum value. Integers will
be temporarily converted to floats when necessary to perform range
comparisons. If the keyword ?VARIABLE is used for the minimum value,
then the minimum value is negative infinity (-8). If the keyword
?VARIABLE is used for the maximum value, then the maximum value is
positive infinity (+8). The range attribute cannot be used in
conjunction with the allowed-values, allowed-numbers, allowed-integers,
or allowed-floats attributes.

11.4 Cardinality Attribute
--------------------------

The cardinality attribute restricts the number of fields that can be
stored in a multifield slot. This attribute can not be used with a
single field slot.

``Syntax`` ::

	<cardinality-attribute> ::= (cardinality <cardinality-specification>
	                                <cardinality-specification>)
	
	<cardinality-specification> ::= <integer> | ?VARIABLE

Only integers can be used in the cardinality specification with the
first value to the cardinality attribute signifying the minimum number
of fields that can be stored in the slot and the second value signifying
the maximum number of fields which can be stored in the slot. If the
keyword ?VARIABLE is used for the minimum value, then the minimum
cardinality is zero. If the keyword ?VARIABLE is used for the maximum
value, then the maximum cardinality is positive infinity (+8). If the
cardinality is not specified for a multifield slot, then it is assumed
to be zero to infinity.

The min-number-of-elements and max-number-of-elements attributes found
in CLIPS 5.1 are no longer supported. The cardinality attribute should
be used in their place.

11.5 Deriving a Default Value From Constraints
----------------------------------------------

Default values for deftemplate and instance slots are automatically
derived from the constraints for the slots if an explicit default value
is not specified. The following rules are used (in order) to determine
the default value for a slot with an unspecified default value.

1) The default type for the slot is chosen from the list of allowed
types for the slot in the following order of precedence: SYMBOL, STRING,
INTEGER, FLOAT, INSTANCE-NAME, INSTANCE-ADDRESS, FACT-ADDRESS,
EXTERNAL-ADDRESS.

2) If the default type has an allowed constant restriction specified
(such as the allowed-integers attribute for the INTEGER type), then the
first value specified in the allowed constant attribute is chosen as the
default value.

3) If the default value was not specified by step 2 and the default type
is INTEGER or FLOAT and the range attribute is specified, then the
minimum range value is used as the default value if it is not ?VARIABLE,
otherwise, the maximum range value is used if it is not ?VARIABLE.

4) If the default value was not specified by step 2 or 3, then the
default default value is used. This value is nil for type SYMBOL, "" for
type STRING, 0 for type INTEGER, 0.0 for type FLOAT, [nil] for type
INSTANCE-NAME, a pointer to a dummy instance for type INSTANCE-ADDRESS,
a pointer to a dummy fact for type FACT-ADDRESS, and the NULL pointer
for type EXTERNAL-ADDRESS.

5) If the default value is being derived for a single field slot, then
the default value derived from steps 1 through 4 is used. The default
value for a multifield slot is a multifield value of length zero.
However, if the multifield slot has a minimum cardinality greater than
zero, then a multifield value with a length of the minimum cardinality
is created and the default value that would be used for a single field
slot is stored in each field of the multifield value.

11.6 Constraint Violation Examples
----------------------------------

The following examples illustrate some of the types of constraint
violations that CLIPS can detect.

Example 1
::

	CLIPS>
	(deftemplate bar
	(slot a (type SYMBOL INTEGER))
	(slot b (type INTEGER FLOAT))
	(slot c (type SYMBOL STRING)))
	CLIPS>
	(defrule error
	(bar (a ?x))
	(bar (b ?x))
	(bar (c ?x))
	=>)
	[RULECSTR1] Variable ?x in CE #3 slot c
	has constraint conflicts which make the pattern unmatchable
	ERROR:
	(defrule MAIN::error-4
	(bar (a ?x))
	(bar (b ?x))
	(bar (c ?x))
	=>)
	CLIPS>

The first occurrence of the variable ?x in slot a of the first pattern
restricts its allowed types to either a symbol or integer. The second
occurrence of ?x in slot b of the second pattern further restricts its
allowed types to only integers. The final occurence of ?x in the third
pattern generates an error because slot c expects ?x to be either a
symbol or a string, but its only allowed type is an integer.

Example 2
::

	CLIPS>
	(deftemplate foo (multislot x (cardinality ?VARIABLE 2)))
	CLIPS>
	(deftemplate bar (multislot y (cardinality ?VARIABLE 3)))
	CLIPS>
	(deftemplate woz (multislot z (cardinality 7 ?VARIABLE)))
	CLIPS>
	(defrule MAIN::error
	(foo (x $?x))
	(bar (y $?y))
	(woz (z $?x $?y))
	=>)
	[CSTRNCHK1] The group of restrictions found in CE #3
	do not satisfy the cardinality restrictions for slot z
	ERROR:
	(defrule MAIN::error
	(foo (x $?x))
	(bar (y $?y))
	(woz (z $?x $?y))
	=>)
	CLIPS>


The variable ?x, found in the first pattern, can have a maximum of two
fields. The variable ?y, found in the second pattern, can have a maximum
of three fields. Added together, both variables have a maximum of five
fields. Since slot z in the the third pattern has a minimum cardinality
of seven, the variables ?x and ?y cannot satisfy the minimum cardinality
restriction for this slot.

Example 3
::

	CLIPS> (clear)
	CLIPS> (deftemplate foo (slot x (type SYMBOL)))
	CLIPS>
	(defrule error
	(foo (x ?x))
	(test (> ?x 10))
	=>)
	[RULECSTR2] Previous variable bindings of ?x caused the type
	restrictions for argument #1 of the expression (> ?x 10)
	found in CE #2 to be violated
	ERROR:
	(defrule MAIN::error
	(foo (x ?x))
	(test (> ?x 10))
	=>)
	CLIPS>

The variable ?x, found in slot x of the first pattern, must be a symbol.
Since the **>** function expects numeric values for its arguments, an
error occurs.

.. _section-9:
