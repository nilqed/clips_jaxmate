Section 9: CLIPS Object Oriented Language
=========================================

This section provides the comprehensive details of the CLIPS
Object-Oriented Language (COOL). Sections 2.3.1, 2.4.2 and 2.5.2.3
explain object references and structure. Section 2.6 gives a high level
overview of COOL. This section assumes a complete understanding of the
material given in the listed sections.

9.1 Background
--------------

COOL is a hybrid of features from many different OOP systems as well as
new ideas. For example, object encapsulation concepts are similar to
those in Smalltalk, and the Common Lisp Object System (CLOS) provides
the basis for multiple inheritance rules. A mixture of ideas from
Smalltalk, CLOS and other systems form the foundation of messages.
Section 8.1 explains an important contrast between the terms **method**
and **message-handler** in CLIPS.

9.2 Predefined System Classes
-----------------------------

COOL provides seventeen system classes: OBJECT, USER, INITIAL-OBJECT,
PRIMITIVE, NUMBER, INTEGER, FLOAT, INSTANCE, INSTANCE-NAME,
INSTANCE-ADDRESS, ADDRESS, FACT-ADDRESS, EXTERNAL-ADDRESS, MULTIFIELD,
LEXEME, SYMBOL and STRING. The user may not delete or modify any of
these classes. The diagram illustrates the inheritance relationships
between these classes.

|image2|

All of these system classes except INITIAL-OBJECT are **abstract**
classes, which means that their only use is for inheritance (**direct**
instances of this class are illegal). None of these classes have slots,
and, except for the class USER, none of them have message-handlers.
However, the user may explicitly attach message-handlers to all of the
system classes except for INSTANCE, INSTANCE-ADDRESS and INSTANCE-NAME.
The OBJECT class is a superclass of all other classes, including
user-defined classes. All user-defined classes should (but are not
required to) inherit directly or indirectly from the class USER, since
this class has all of the standard system message-handlers, such as
initialization and deletion, attached to it. Section 9.4 describes these
system message-handlers.

The PRIMITIVE system class and all of its subclasses are provided mostly
for use in generic function method restrictions, but message-handlers
and new subclasses may be attached if desired. However, the three
primitive system classes INSTANCE, INSTANCE-ADDRESS and INSTANCE-NAME
are provided strictly for use in methods (particularly in forming
implicit methods for overloaded system functions - see section 8.5.1)
and as such cannot have subclasses or message-handlers attached to them.

The INITIAL-OBJECT class is provided for use by the default
**definstances** initial-object in creating the default instance
[initial-object] during the **reset** command. This system class is
**concrete** and **reactive** to pattern-matching on the LHS of rules
but is in other respects exactly like the system class USER. The
instance [initial-object] is for use by the initial-object pattern (see
section 5.4.9).

9.3 Defclass Construct
----------------------

A **defclass** is a construct for specifying the properties (slots) and
behavior (message-handlers) of a class of objects. A defclass consists
of five elements: 1) a name, 2) a list of superclasses from which the
new class inherits slots and message-handlers, 3) a specifier saying
whether or not the creation of direct instances of the new class is
allowed, 4) a specifier saying whether or not instances of this class
can match object patterns on the LHS of rules and 5) a list of slots
specific to the new class. All user-defined classes must inherit from at
least one class, and to this end COOL provides predefined system classes
for use as a base in the derivation of new classes.

Any slots explicitly given in the defclass override those gotten from
inheritance. COOL applies rules to the list of superclasses to generate
a class precedence list (see section 9.3.1) for the new class. Facets
(see section 9.3.3) further describe slots. Some examples of facets
include: default value, cardinality, and types of access allowed.

``Syntax`` ::

  Defaults are in **xxxxx**.

  (defclass <name> [<comment>]
    (is-a <superclass-name>+)
    [<role>]
    [<pattern-match-role>]
    <slot>*
    <handler-documentation>*)

  <role> ::= (role concrete | abstract)

  <pattern-match-role> ::= (pattern-match reactive | non-reactive)

  <slot> ::= (slot <name> <facet>*) | (single-slot <name> <facet>*) |
             (multislot <name> <facet>*)

  <facet> ::= <default-facet> | <storage-facet> |
              <access-facet> | <propagation-facet> |
              <source-facet> | <pattern-match-facet> |
              <visibility-facet> | <create-accessor-facet>
              <override-message-facet> | <constraint-attributes>

  <default-facet> ::= (default **?DERIVE** | ?NONE | <expression>*) |
                      (default-dynamic <expression>*)

  <storage-facet> ::= (storage **local** | shared)

  <access-facet> ::= (access **read-write** | read-only | initialize-only)

  <propagation-facet> ::= (propagation **inherit** | no-inherit)

  <source-facet> ::= (source **exclusive** | composite)

  <pattern-match-facet> ::= (pattern-match **reactive** | non-reactive)

  <visibility-facet> ::= (visibility **private** | public)

  <create-accessor-facet> ::= (create-accessor ?NONE | read | write | 
                               **read-write**)

  <override-message-facet> ::= (override-message **?DEFAULT** | <message-name>)

  <handler-documentation> ::= (message-handler <name> [<handler-type>])

  <handler-type> ::= **primary** | around | before | after

Redefining an existing class deletes the current subclasses and all
associated message-handlers. An error will occur if instances of the
class or any of its subclasses exist.

9.3.1 Multiple Inheritance
~~~~~~~~~~~~~~~~~~~~~~~~~~

If one class inherits from another class, the first class is a
**subclass** of the second class, and the second class is a
**superclass** of the first class. Every user-defined class must have at
least one direct superclass, i.e. at least one class must appear in the
*is-a* portion of the defclass. Multiple inheritance occurs when a class
has more than one direct superclass. COOL examines the direct superclass
list for a new class to establish a linear ordering called the **class
precedence list**. The new class inherits slots and message-handlers
from each of the classes in the class precedence list. The word
precedence implies that slots and message-handlers of a class in the
list override conflicting definitions of another class found later in
the list. A class that comes before another class in the list is said to
be more **specific**. All class precedence lists will terminate in the
system class OBJECT, and most (if not all) class precedence lists for
user-defined classes will terminate in the system classes USER and
OBJECT. The class precedence list can be listed using the
**describe-class** function (see section 13.11.1.4).

9.3.1.1 Multiple Inheritance Rules
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

COOL uses the inheritance hierarchy of the direct superclasses to
determine the class precedence list for a new class. COOL recursively
applies the following two rules to the direct superclasses:

 1. A class has higher precedence than any of its superclasses.

 2. A class specifies the precedence between its direct superclasses.

If more than one class precedence list would satisfy these rules, COOL
chooses the one most similar to a strict preorder depth-first traversal.
This heuristic attempts to preserve “family trees” to the greatest
extent possible. For example, if a child inherited genetic traits from a
mother and father, and the mother and father each inherited traits from
their parents, the child’s class precedence list would be: child mother
maternal-grandmother maternal-grandfather father paternal-grandmother
paternal-grandfather. There are other orderings which would satisfy the
rules (such as child mother father paternal-grandfather
maternal-grandmother paternal-grandmother maternal-grandfather), but
COOL chooses the one which keeps the family trees together as much as
possible.

Example 1
::

  (defclass A (is-a USER))

Class A directly inherits information from the class USER. The class
precedence list for A is: A USER OBJECT.

Example 2
::

  (defclass B (is-a USER))

Class B directly inherits information from the class USER. The class
precedence list for B is: B USER OBJECT.

Example 3
::

  (defclass C (is-a A B))

Class C directly inherits information from the classes A and B. The
class precedence list for C is: C A B USER OBJECT.

Example 4
::
  (defclass D (is-a B A))

Class D directly inherits information from the classes B and A. The
class precedence list for D is: D B A USER OBJECT.

Example 5
::

  (defclass E (is-a A C))

By rule #2, A must precede C. However, C is a subclass of A and cannot
succeed A in a precedence list without violating rule #1. Thus, this is
an error.

Example 6
::

  (defclass E (is-a C A))

Specifying that E inherits from A is extraneous, since C inherits from
A. However, this definition does not violate any rules and is
acceptable. The class precedence list for E is: E C A B USER OBJECT.

Example 7
::

  (defclass F (is-a C B))

Specifying that F inherits from B is extraneous, since C inherits from
B. The class precedence list for F is: F C A B USER OBJECT. The
superclass list says B must follow C in F’s class precedence list but
*not* that B must *immediately* follow C.

Example 8
::

  (defclass G (is-a C D))

This is an error, for it violates rule #2. The class precedence of C
says that A should precede B, but the class precedence list of D says
the opposite.

Example 9
::

  (defclass H (is-a A))

  (defclass I (is-a B))

  (defclass J (is-a H I A B))

The respective class precedence lists of H and I are: ``H A USER OBJECT``
and ``I B USER OBJECT``. If J did not have A and B as direct superclasses, J
could have one of three possible class precedence lists: ``J H A I B USER
OBJECT``, ``J H I A B USER OBJECT`` or ``J H I B A USER OBJECT``. COOL would
normally pick the first list since it preserves the family trees (H A
and I B) to the greatest extent possible. However, since J inherits
directly from A and B, rule #2 dictates that the class precedence list
must be ``J H I A B USER OBJECT``.

Usage Note

For most practical applications of multiple inheritance, the order in
which the superclasses are specified should not matter. If you create a
class using multiple inheritance and the order of the classes specified
in the *is-a* attribute effects the behavior of the class, you should
consider whether your program design is needlessly complex.

9.3.2 Class Specifiers
~~~~~~~~~~~~~~~~~~~~~~

9.3.2.1 Abstract and Concrete Classes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

An **abstract** class is intended for inheritance only, and no direct
instances of this class can be created. A **concrete** class can have
direct instances. Using the abstract role specifier in a defclass will
cause COOL to generate an error if **make-instance** is ever called for
this class. If the abstract or concrete descriptor for a class is not
specified, it is determined by inheritance. For the purpose of role
inheritance, system defined classes behave as concrete classes. Thus a
class which inherits from USER will be concrete if no role is specified.

9.3.2.2 Reactive and Non-Reactive Classes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Objects of a **reactive** class can match object patterns in a rule.
Objects of a **non-reactive** class cannot match object patterns in a
rule and are not considered when the list of applicable classes are
determined for an object pattern. An **abstract** class cannot be
**reactive**. If the reactive or non-reactive descriptor for a class is
not specified, it is determined by inheritance. For the purpose of
pattern-match inheritance, system defined classes behave as reactive
classes unless the inheriting class is **abstract**.

9.3.3 Slots
~~~~~~~~~~~

Slots are placeholders for values associated with instances of a
user-defined class. Each instance has a copy of the set of slots
specified by the immediate class as well as any obtained from
inheritance. Only available memory limits the number of slots. The name
of a slot may be any symbol with the exception of the keywords *is-a*
and *name* which are reserved for use in object patterns.

To determine the set of slots for an instance, the class precedence list
for the instance’s class is examined in order from most specific to most
general (left to right). A class is more specific than its superclasses.
Slots specified in any of the classes in the class precedence list are
given to the instance, with the exception of no-inherit slots (see
section 9.3.3.5). If a slot is inherited from more than one class, the
definition given by the more specific class takes precedence, with the
exception of composite slots (see section 9.3.3.6).

Example
::

  (defclass A (is-a USER)
    (slot fooA)
    (slot barA))

  (defclass B (is-a A)
    (slot fooB)
    (slot barB))

The class precedence list of A is: A USER OBJECT. Instances of A will
have two slots: fooA and barA. The class precedence list of B is: B A
USER OBJECT. Instances of B will have four slots: fooB, barB, fooA and
barA.

Just as slots make up classes, **facets** make up slots. Facets describe
various features of a slot that hold true for all objects which have the
slot: default value, storage, access, inheritance propagation, source of
other facets, pattern-matching reactivity, visibility to subclass
message-handlers, the automatic creation of message-handlers to access
the slot, the name of the message to send to set the slot and constraint
information. Each object can still have its own value for a slot, with
the exception of shared slots (see section 9.3.3.3).

9.3.3.1 Slot Field Type
^^^^^^^^^^^^^^^^^^^^^^^

A slot can hold either a single-field or multifield value. By default, a
slot is single-field. The keyword **multislot** specifies that a slot
can hold a multifield value comprised of zero or more fields, and the
keywords **slot** or **single-slot** specify that the slot can hold one
value. Multifield slot values are stored as multifield values and can be
manipulated with the standard multifield functions, such as **nth$** and
**length$**, once they are retrieved via messages. COOL also provides
functions for setting multifield slots, such as **slot-insert$** (see
section 12.16.4.12.2). Single-field slots are stored as a CLIPS
primitive type, such as integer or string.

Example
::

	CLIPS> (clear)
	CLIPS>
	(defclass A (is-a USER)
	  (multislot foo
	  (default abc def ghi)))
	CLIPS> (make-instance a of A)
	[a]
	CLIPS> (nth$ 2 (send [a] get-foo))
	def
	CLIPS>


9.3.3.2 Default Value Facet
^^^^^^^^^^^^^^^^^^^^^^^^^^^

The **default** and **default-dynamic** facets can be used to specify an
initial value given to a slot when an instance of the class is created
or initialized. By default, a slot will have a default value that is
derived from the slot’s constraint facets (see sections 9.3.3.11 and
11.5). Default values are directly assigned to slots without the use of
messages, unlike slot overrides in a **make-instance** call (see section
9.6.1).

The **default** facet is a static default: the specified expression is
evaluated once when the class is defined, and the result is stored with
the class. This result is assigned to the appropriate slot when a new
instance is created. If the keyword ?DERIVE is used for the default
value, then a default value is derived from the constraints for the slot
(see section 11.5 for more details). By default, the default attribute
for a slot is (default ?DERIVE). If the keyword ?NONE is used for the
default value, then the slot is not assigned a default value. Using this
keyword causes make-instance to require a slot-override for that slot
when an instance is created. Note that in CLIPS 6.0, a slot now has a
default even if one is not explicitly specified (unlike CLIPS 5.1). This
could cause different behavior for CLIPS 5.1 programs using the
initialize-instance function. The ?NONE keyword can be used to recover
the original behavior for classes.

The **default-dynamic** facet is a dynamic default: the specified
expression is evaluated every time an instance is created, and the
result is assigned to the appropriate slot.

Example
::

	CLIPS> (clear)
	CLIPS> (setgen 1)
	1
	CLIPS>
	(defclass A (is-a USER)
	  (slot foo (default-dynamic (gensym))))
	CLIPS> (make-instance a1 of A)
	[a1]
	CLIPS> (make-instance a2 of A)
	[a2]
	CLIPS> (send [a1] get-foo)
	gen1
	CLIPS> (send [a2] get-foo)
	gen2
	CLIPS>


9.3.3.3 Storage Facet
^^^^^^^^^^^^^^^^^^^^^

The actual value of an instance’s copy of a slot can either be stored
with the instance or with the class. The **local** facet specifies that
the value be stored with the instance, and this is the default. The
**shared** facet specifies that the value be stored with the class. If
the slot value is locally stored, then each instance can have a separate
value for the slot. However, if the slot value is stored with the class,
all instances will have the same value for the slot. Anytime the value
is changed for a shared slot, it will be changed for all instances with
that slot.

A shared slot will always pick up a dynamic default value from a
defclass when an instance is created or initialized, but the shared slot
will ignore a static default value unless it does not currently have a
value. Any changes to a shared slot will cause pattern-matching for
rules to be updated for all reactive instances containing that slot.

Example
::

	CLIPS> (clear)
	CLIPS>
	(defclass A (is-a USER)
	  (slot foo (storage shared)
	  (default 1))
	  (slot bar (storage shared)
	  (default-dynamic 2))
	  (slot woz (storage local)))
	CLIPS> (make-instance a of A)
	[a]
	CLIPS> (send [a] print)
	[a] of A
	(foo 1)
	(bar 2)
	(woz nil)
	CLIPS> (send [a] put-foo 56)
	56
	CLIPS> (send [a] put-bar 104)
	104
	CLIPS> (make-instance b of A)
	[b]
	CLIPS> (send [b] print)
	[b] of A
	(foo 56)
	(bar 2)
	(woz nil)
	CLIPS> (send [b] put-foo 34)
	34
	CLIPS> (send [b] put-woz 68)
	68
	CLIPS> (send [a] print)
	[a] of A
	(foo 34)
	(bar 2)
	(woz nil)
	CLIPS> (send [b] print)
	[b] of A
	(foo 34)
	(bar 2)
	(woz 68)
	CLIPS>

9.3.3.4 Access Facet
^^^^^^^^^^^^^^^^^^^^

There are three types of access facets which can be specified for a
slot: **read-write**, **read-only**, and **initialize-only**. The
**read-write** facet is the default and says that a slot can be both
written and read. The **read-only** facet says the slot can only be
read; the only way to set this slot is with default facets in the class
definition. The **initialize-only** facet is like **read-only** except
that the slot can also be set by slot overrides in a **make-instance**
call (see section 9.6.1) and **init** message-handlers (see section
9.4). These privileges apply to indirect access via messages as well as
direct access within message-handler bodies (see section 9.4). Note: a
**read-only** slot that has a static default value will implicitly have
the **shared** storage facet.

Example
::

	CLIPS> (clear)
	CLIPS>
	(defclass A (is-a USER)
	  (slot foo (access read-write))
	  (slot bar (access read-only) (default abc))
	  (slot woz (access initialize-only)))
	CLIPS>
	(defmessage-handler A put-bar (?value)
	  (dynamic-put (sym-cat bar) ?value))
	CLIPS> (make-instance a of A (bar 34))
	[MSGFUN3] bar slot in [a] of A: write access denied.
	[PRCCODE4] Execution halted during the actions of message-handler
	put-bar primary in class A
	FALSE
	CLIPS> (make-instance a of A (foo 34) (woz 65))
	[a]
	CLIPS> (send [a] put-bar 1)
	[MSGFUN3] bar slot in [a] of A: write access denied.
	[PRCCODE4] Execution halted during the actions of message-handler
	put-bar primary in class A
	FALSE
	CLIPS> (send [a] put-woz 1)
	[MSGFUN3] woz slot in [a] of A: write access denied.
	[PRCCODE4] Execution halted during the actions of message-handler
	put-bar primary in class A
	FALSE
	CLIPS> (send [a] print)
	[a] of A
	(foo 34)
	(bar abc)
	(woz 65)
	CLIPS>

9.3.3.5 Inheritance Propagation Facet
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

An **inherit** facet says that a slot in a class can be given to
instances of other classes that inherit from the first class. This is
the default. The **no-inherit** facet says that only direct instances of
this class will get the slot.

Example
::

	CLIPS> (clear)
	CLIPS>
	(defclass A (is-a USER)
	  (slot foo (propagation inherit))
	  (slot bar (propagation no-inherit)))
	CLIPS> (defclass B (is-a A))
	CLIPS> (make-instance a of A)
	[a]
	CLIPS> (make-instance b of B)
	[b]
	CLIPS> (send [a] print)
	[a] of A
	(foo nil)
	(bar nil)
	CLIPS> (send [b] print)
	[b] of B
	(foo nil)
	CLIPS>

9.3.3.6 Source Facet
^^^^^^^^^^^^^^^^^^^^

When obtaining slots from the class precedence list during instance
creation, the default behavior is to take the facets from the most
specific class that gives the slot and give default values to any
unspecified facets. This is the behavior specified by the **exclusive**
facet. The **composite** facet causes facets which are not explicitly
specified by the most specific class to be taken from the next most
specific class. Thus, in an overlay fashion, the facets of an instance’s
slot can be specified by more than one class. Note that even though
facets may be taken from superclasses, the slot is still considered to
reside in the new class for purposes of visibility (see section
9.3.3.8). One good example of a use of this feature is to pick up a slot
definition and change only its default value for a new derived class.

Example
::

	CLIPS> (clear)
	CLIPS>
	(defclass A (is-a USER)
	  (multislot foo (access read-only)
	  (default a b c)))
	CLIPS>
	(defclass B (is-a A)
	  (slot foo (source composite) ; multiple and read-only
	  ; from class A
	  (default d e f)))
	CLIPS> (describe-class B)
	================================================================================
	********************************************************************************
	Concrete: direct instances of this class can be created.
	Reactive: direct instances of this class can match defrule patterns.
	Direct Superclasses: A
	Inheritance Precedence: B A USER OBJECT
	Direct Subclasses:
	--------------------------------------------------------------------------------
	SLOTS : FLD DEF PRP ACC STO MCH SRC VIS CRT OVRD-MSG SOURCE(S)
	foo : MLT STC INH R SHR RCT CMP PRV R NIL A B
	Constraint information for slots:
	SLOTS : SYM STR INN INA EXA FTA INT FLT
	foo : + + + + + + + + RNG:[-oo..+oo] CRD:[0..+oo]
	--------------------------------------------------------------------------------
	Recognized message-handlers:
	init primary in class USER
	delete primary in class USER
	create primary in class USER
	print primary in class USER
	direct-modify primary in class USER
	message-modify primary in class USER
	direct-duplicate primary in class USER
	message-duplicate primary in class USER
	get-foo primary in class A
	get-foo primary in class B
	********************************************************************************
	================================================================================
	CLIPS>


9.3.3.7 Pattern-Match Reactivity Facet
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Normally, any change to a slot of an instance will be considered as a
change to the instance for purposes of pattern-matching. However, it is
possible to indicate that changes to a slot of an instance should not
cause pattern-matching. The **reactive** facet specifies that changes to
a slot trigger pattern-matching, and this is the default. The
**non-reactive** facet specifies that changes to a slot do not affect
pattern-matching.

Example
::

	CLIPS> (clear)
	CLIPS>
	(defclass A (is-a USER)
	  (slot foo (pattern-match non-reactive)))
	CLIPS>
	(defclass B (is-a USER)
	  (slot foo))
	CLIPS>
	(defrule Create
	  ?ins<-(object (is-a A | B))
	  =>
	  (printout t "Create " (instance-name ?ins) crlf))
	CLIPS>
	(defrule Foo-Access
	  ?ins<-(object (is-a A | B) (foo ?))
	  =>
	  (printout t "Foo-Access " (instance-name ?ins) crlf))
	CLIPS> (make-instance a of A)
	[a]
	CLIPS> (make-instance b of B)
	[b]
	CLIPS> (run)
	Create [b]
	Foo-Access [b]
	Create [a]
	CLIPS> (send [a] put-foo 1)
	1
	CLIPS> (send [b] put-foo 1)
	1
	CLIPS> (run)
	Foo-Access [b]
	CLIPS>

9.3.3.8 Visibility Facet
^^^^^^^^^^^^^^^^^^^^^^^^

Normally, only message-handlers attached to the class in which a slot is
defined may directly access the slot. However, it is possible to allow
message-handlers attached to superclasses or subclasses which inherit
the slot to directly access the slot as well. Declaring the
**visibility** facet to be **private** specifies that only the
message-handlers of the defining class may directly access the slot, and
this is the default. Declaring the **visibility** facet to be **public**
specifies that the message-handlers and subclasses that inherit the slot
and superclasses may also directly access the slot.

Example
::

	CLIPS> (clear)
	CLIPS>
	(defclass A (is-a USER)
	  (slot foo (visibility private)))
	CLIPS>
	(defclass B (is-a A))
	CLIPS>
	(defmessage-handler B get-foo ()
	  ?self:foo)
	[MSGFUN6] Private slot foo of class A cannot be accessed directly by
	handlers attached to class B
	[PRCCODE3] Undefined variable self:foo referenced in message-handler.
	ERROR:
	(defmessage-handler MAIN::B get-foo
	()
	?self:foo)
	CLIPS>

9.3.3.9 Create-Accessor Facet
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The **create-accessor** facet instructs CLIPS to automatically create
*explicit* message-handlers for reading and/or writing a slot. By
default, implicit slot-accessor message-handlers are created for every
slot. While these message-handlers are real message-handlers and can be
manipulated as such, they have no pretty-print form and cannot be
directly modified by the user.

If the value **?NONE** is specified for the facet, no message-handlers
are created.

If the value **read** is specified for the facet, CLIPS creates the
following message-handler:
::

  (defmessage-handler <class> get-<slot-name> primary ()
    ?self:<slot-name>)

If the value **write** is specified for the facet, CLIPS creates the
following message-handler for single-field slots:
::

  (defmessage-handler <class> put-<slot-name> primary (?value)
    (bind ?self:<slot-name> ?value)

or the following message-handler for multifield slots:
::

  (defmessage-handler <class> put-<slot-name> primary ($?value)
    (bind ?self:<slot-name> ?value)

If the value **read-write** is specified for the facet, both the get-
and one of the put- message-handlers are created.

If accessors are required that do not use static slot references (see
sections 9.4.2, 9.6.3 and 9.6.4), then user must define them explicitly
with the defmessage-handler construct.

The **access** facet affects the default value for the
**create-accessor** facet. If the **access** facet is **read-write**,
then the default value for the **create-accessor** facet is
**read-write**. If the **access** facet is **read-only**, then the
default value is **read**. If the access facet is **initialize-only**,
then the default is **?NONE**.

Example
::
	CLIPS> (clear)
	CLIPS>
	(defclass A (is-a USER)
	  (slot foo (create-accessor write))
	  (slot bar (create-accessor read))
	CLIPS> (make-instance a of A (foo 36))
	[a]
	CLIPS> (make-instance b of A (bar 45))
	[MSGFUN1] No applicable primary message-handlers found for put-bar.
	FALSE
	CLIPS>


9.3.3.10 Override-Message Facet
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

There are several COOL support functions that set slots via use of
message-passing, e.g., **make-instance**, **initialize-instance**,
**message-modify-instance** and **message-duplicate-instance**. By
default, all these functions attempt to set a slot with the message
called **put-<slot-name>**. However, if the user has elected not to use
standard slot-accessors and wishes these functions to be able to perform
slot-overrides, then the **override-message** facet can be used to
indicate what message to send instead.

Example
::

	CLIPS> (clear)
	CLIPS>
	(defclass A (is-a USER)
	  (slot special (override-message special-put)))
	CLIPS>
	(defmessage-handler A special-put primary (?value)
	  (bind ?self:special ?value))
	CLIPS> (watch messages)
	CLIPS> (make-instance a of A (special 65))
	MSG >> create ED:1 (<Instance-a>)
	MSG << create ED:1 (<Instance-a>)
	MSG >> special-put ED:1 (<Instance-a> 65)
	MSG << special-put ED:1 (<Instance-a> 65)
	MSG >> init ED:1 (<Instance-a>)
	MSG << init ED:1 (<Instance-a>)
	[a]
	CLIPS> (unwatch messages)
	CLIPS>

9.3.3.11 Constraint Facets
^^^^^^^^^^^^^^^^^^^^^^^^^^

The syntax and functionality of single and multifield constraint facets
(attributes) are described in detail in Section 11. Static and dynamic
constraint checking for classes and their instances is supported. Static
checking is performed when constructs or commands that specify slot
information are being parsed. Object patterns used on the LHS of a rule
are also checked to determine if constraint conflicts exist among
variables used in more that one slot. Errors for inappropriate values
are immediately signaled. Static checking is enabled by default. This
behavior can be changed using the **set-static-constraint-checking**
function. Dynamic checking is also supported. If dynamic checking is
enabled, then new instances have their values checked whenever they are
set (e.g. initialization, slot-overrides, and put- access). This dynamic
checking is disabled by default. This behavior can be changed using the
**set-dynamic-constraint-checking** function. If an violation occurs
when dynamic checking is being performed, then execution will be halted.

Regardless of whether static or dynamic checking is enabled, multifield
values can never be stored in single-field slots. Single-field values
are converted to a multifield value of length one when storing in a
multifield slot. In addition, the evaluation of a function that has no
return value is always illegal as a slot value.

Example
::

	CLIPS> (clear)
	CLIPS>
	(defclass A (is-a USER)
	  (multislot foo (type SYMBOL)
	  (cardinality 2 3)))
	CLIPS> (make-instance a of A (foo 45))
	[a]
	CLIPS> (set-dynamic-constraint-checking TRUE)
	FALSE
	CLIPS> (make-instance a of A (foo red 5.0))
	[CSTRNCHK1] (red 5.0) for slot foo of instance [a] found in put-foo
	primary in class A does not match the allowed types.
	[PRCCODE4] Execution halted during the actions of message-handler
	put-foo primary in class A
	FALSE
	CLIPS> (make-instance a of A (foo red))
	[CSTRNCHK1] (red) for slot foo of instance [a] found in put-foo primary
	in class A does not satisfy the cardinality restrictions.
	[PRCCODE4] Execution halted during the actions of message-handler
	put-foo primary in class A
	FALSE
	CLIPS>

9.3.4 Message-handler Documentation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

COOL allows the user to forward declare the message-handlers for a class
within the defclass statement. These declarations are for documentation
only and are ignored by CLIPS. The defmessage-handler construct must be
used to actually add message-handlers to a class. Message-handlers can
later be added which are not documented in the defclass.

Example
::

	CLIPS> (clear)
	CLIPS>
	(defclass rectangle (is-a USER)
	  (slot side-a (default 1))
	  (slot side-b (default 1))
	  (message-handler find-area))
	CLIPS>
	(defmessage-handler rectangle find-area ()
	  (* ?self:side-a ?self:side-b))
	CLIPS>
	(defmessage-handler rectangle print-area ()
	  (printout t (send ?self find-area) crlf))
	CLIPS>


9.4 Defmessage-handler Construct
--------------------------------

Objects are manipulated by sending them messages via the function
**send**. The result of a message is a useful return-value or
side-effect. A **defmessage-handler** is a construct for specifying the
behavior of a class of objects in response to a particular message. The
implementation of a message is made up of pieces of procedural code
called message-handlers (or handlers for short). Each class in the class
precedence list of an object’s class can have handlers for a message. In
this way, the object’s class and all its superclasses share the labor of
handling the message. Each class’s handlers handle the part of the
message that is appropriate to that class. Within a class, the handlers
for a particular message can be further subdivided into four types or
categories: **primary**, **before**, **after** and **around**. The
intended purposes of each type are summarized in the chart below:

======== =====================================================================
**Type** **Role for the Class**
======== =====================================================================
primary  Performs the majority of the work for the message
before   Does auxiliary work for a message before the primary handler executes
after    Does auxiliary work for a message after the primary handler executes
around   Sets up an environment for the execution of the rest of the handlers
======== =====================================================================

Before and after handlers are for side-effects only; their return values
are always ignored. Before handlers execute before the primary ones, and
after message-handlers execute after the primary ones. The return value
of a message is generally given by the primary message-handlers, but
around handlers can also return a value. Around message-handlers allow
the user to wrap code around the rest of the handlers. They begin
execution before the other handlers and pick up again after all the
other message-handlers have finished.

A primary handler provides the part of the message implementation which
is most specific to an object, and thus the primary handler attached to
the class closest to the immediate class of the object overrides other
primary handlers. Before and after handlers provide the ability to pick
up behavior from classes that are more general than the immediate class
of the object, thus the message implementation uses all handlers of this
type from all the classes of an object. When only the roles of the
handlers specify which handlers get executed and in what order, the
message is said to be **declaratively** implemented. However, some
message implementations may not fit this model well. For example, the
results of more than one primary handler may be needed. In cases like
this, the handlers themselves must take part in deciding which handlers
get executed and in what order. This is called the **imperative**
technique. Around handlers provide imperative control over all other
types of handlers except more specific around handlers. Around handlers
can change the environment in which other handlers execute and modify
the return value for the entire message. A message implementation should
use the declarative technique if at all possible because this allows the
handlers to be more independent and modular.

A defmessage-handler is comprised of seven elements: 1) a class name to
which to attach the handler (the class must have been previously
defined), 2) a message name to which the handler will respond, 3) an
optional type (the default is primary), 4) an optional comment, 5) a
list of parameters that will be passed to the handler during execution,
6) an optional wildcard parameter and 7) a series of expressions which
are executed in order when the handler is called. The return-value of a
message-handler is the evaluation of the last expression in the body.

``Syntax`` ::

  Defaults are in **xxxx**.

  (defmessage-handler <class-name> <message-name>
    [<handler-type>] [<comment>]
    (<parameter>* [<wildcard-parameter>])
    <action>*)

  <handler-type> ::= around | before | **primary** | after

  <parameter> ::= <single-field-variable>

  <wildcard-parameter> ::= <multifield-variable>

Message-handlers are uniquely identified by class, name and type.
Message-handlers are never called directly. When the user sends a
message to an object, CLIPS selects and orders the applicable
message-handlers attached to the object’s class(es) and then executes
them. This process is termed the **message dispatch**.

Example
::

	CLIPS> (clear)
	CLIPS> (defclass A (is-a USER))
	CLIPS>
	(defmessage-handler A delete before ()
	  (printout t "Deleting an instance of the class A..." crlf))
	CLIPS>
	(defmessage-handler USER delete after ()
	  (printout t "System completed deletion of an instance."
	crlf))
	CLIPS> (watch instances)
	CLIPS> (make-instance a of A)
	==> instance [a] of A
	[a]
	CLIPS> (send [a] delete)
	Deleting an instance of the class A...
	<== instance [a] of A
	System completed deletion of an instance.
	TRUE
	CLIPS> (unwatch instances)
	CLIPS>

9.4.1 Message-handler Parameters
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A message-handler may accept *exactly* or *at least* a specified number
of arguments, depending on whether a wildcard parameter is used or not.
The regular parameters specify the minimum number of arguments that must
be passed to the handler. Each of these parameters may be referenced
like a normal single-field variable within the actions of the handler.
If a wildcard parameter is present, the handler may be passed any number
of arguments greater than or equal to the minimum. If no wildcard
parameter is present, then the handler must be passed exactly the number
of arguments specified by the regular parameters. All arguments to a
handler that do not correspond to a regular parameter are grouped into a
multifield value that can be referenced by the wildcard parameter. The
standard CLIPS multifield functions, such as **length$** and
**expand$**, can be applied to the wildcard parameter.

Handler parameters have no bearing on the applicability of a handler to
a particular message (see section 9.5.1). However, if the number of
arguments is inappropriate, a message execution error (see section
9.5.4) will be generated when the handler is called. Thus, the number of
arguments accepted should be consistent for all message-handlers
applicable to a particular message.

Example
::

	CLIPS> (clear)
	CLIPS>
	(defclass CAR (is-a USER)
	  (slot front-seat)
	  (multislot trunk)
	  (slot trunk-count))
	CLIPS>
	(defmessage-handler CAR put-items-in-car (?item $?rest)
	  (bind ?self:front-seat ?item)
	  (bind ?self:trunk ?rest)
	  (bind ?self:trunk-count (length$ ?rest)))
	CLIPS> (make-instance Pinto of CAR)
	[Pinto]
	CLIPS> (send [Pinto] put-items-in-car bag-of-groceries tire suitcase)
	2
	CLIPS> (send [Pinto] print)
	[Pinto] of CAR
	(front-seat bag-of-groceries)
	(trunk tire suitcase)
	(trunk-count 2)
	CLIPS>


9.4.1.1 Active Instance Parameter
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The term **active instance** refers to an instance that is responding to
a message. All message-handlers have an implicit parameter called
**?self** which binds the active instance for a message. This parameter
name is reserved and cannot be explicitly listed in the
message-handler’s parameters, nor can it be rebound within the body of a
message-handler.

Example
::

	CLIPS> (clear)
	CLIPS> (defclass A (is-a USER))
	CLIPS> (make-instance a of A)
	[a]
	CLIPS>
	(defmessage-handler A print-args (?a ?b $?c)
	  (printout t (instance-name ?self) " " ?a " " ?b
	   " and " (length$ ?c) " extras: " ?c crlf))
	CLIPS> (send [a] print-args 1 2)
	[a] 1 2 and 0 extras: ()
	CLIPS> (send [a] print-args a b c d)
	[a] a b and 2 extras: (c d)
	CLIPS>


9.4.2 Message-handler Actions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The body of a message-handler is a sequence of expressions that are
executed in order when the handler is called. The return value of the
message-handler is the result of the evaluation of the last expression
in the body.

Handler actions may *directly* manipulate slots of the active instance.
Normally, slots can only be manipulated by sending the object
slot-accessor messages (see sections 9.3.3.9 and 9.4.3). However,
handlers are considered part of the encapsulation (see section 2.6.2) of
an object, and thus can directly view and change the slots of the
object. There are several functions which operate implicitly on the
active instance (without the use of messages) and can only be called
from within a message-handler. These functions are discussed in section
12.16.

A shorthand notation is provided for accessing slots of the active
instance from within a message-handler.

``Syntax`` ::

  ?self:<slot-name>


Example
::

	CLIPS> (clear)
	CLIPS>
	(defclass A (is-a USER)
	  (slot foo (default 1))
	  (slot bar (default 2)))
	CLIPS>
	(defmessage-handler A print-all-slots ()
	  (printout t ?self:foo " " ?self:bar crlf))
	CLIPS> (make-instance a of A)
	[a]
	CLIPS> (send [a] print-all-slots)
	1 2
	CLIPS>


The **bind** function can also take advantage of this shorthand notation
to set the value of a slot.

``Syntax`` ::

  (bind ?self:<slot-name> <value>*)

Example
::

  CLIPS>
  (defmessage-handler A set-foo (?value)
  (bind ?self:foo ?value))

  CLIPS> (send [a] set-foo 34)
  34
  CLIPS>

Direct slot accesses are statically bound to the appropriate slot in the
defclass when the message-handler is defined. Care must be taken when
these direct slot accesses can be executed as the result of a message
sent to an instance of a subclass of the class to which the
message-handler is attached. If the subclass has redefined the slot, the
direct slot access contained in the message-handler attached to the
superclass will fail. That message-handler accesses the slot in the
superclass, not the subclass.

Example
::

	CLIPS> (clear)
	CLIPS>
	(defclass A (is-a USER)
	  (slot foo (create-accessor read)))
	CLIPS>
	(defclass B (is-a A)
	  (slot foo (create-accessor ?NONE)))
	CLIPS> (make-instance b of B)
	[b]
	CLIPS> (send [b] get-foo)
	[MSGPASS3] Static reference to slot foo of class A does not apply to [b]
	of B
	[PRCCODE4] Execution halted during the actions of message-handler
	get-foo primary in class A
	FALSE
	CLIPS>


In order for direct slot accesses in a superclass message-handler to
apply to new versions of the slot in subclasses, the dynamic-put and
dynamic-get (see sections 12.16.4.10 and 12.16.4.11) must be used.
However, the subclass slot must have public visibility for this to work
(see section 9.3.3.8).

Example
::

	CLIPS> (clear)
	CLIPS>
	(defclass A (is-a USER)
	  (slot foo (create-accessor ?NONE)))
	CLIPS>
	(defmessage-handler A get-foo ()
	  (dynamic-get foo))
	CLIPS>
	(defclass B (is-a A)
	  (role concrete)
	  (slot foo (visibility public)))
	CLIPS> (make-instance b of B)
	[b]
	CLIPS> (send [b] get-foo)
	nil
	CLIPS>


9.4.3 Daemons
~~~~~~~~~~~~~

Daemons are pieces of code which execute implicitly whenever some basic
action is taken upon an instance, such as initialization, deletion, or
reading and writing of slots. All these basic actions are implemented
with primary handlers attached to the class of the instance. Daemons may
be easily implemented by defining other types of message-handlers, such
as before or after, which will recognize the same messages. These pieces
of code will then be executed whenever the basic actions are performed
on the instance.

Example
::

	CLIPS> (clear)
	CLIPS> (defclass A (is-a USER))
	CLIPS>
	(defmessage-handler A init before ()
	  (printout t "Initializing a new instance of class A..."
	crlf))
	CLIPS> (make-instance a of A)
	Initializing a new instance of class A...
	[a]
	CLIPS>

9.4.4 Predefined System Message-handlers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

CLIPS defines eight primary message-handlers that are attached to the
class USER. These handlers cannot be deleted or modified.

9.4.4.1 Instance Initialization
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

``Syntax`` ::

  (defmessage-handler USER init primary ())

This handler is responsible for initializing instances with class
default values after creation. The **make-instance** and
**initialize-instance** functions send the **init** message to an
instance (see sections 9.6.1 and 9.6.2); the user should never send this
message directly. This handler is implemented using the **init-slots**
function (see section 12.13). User-defined **init** handlers should not
prevent the system message-handler from responding to an **init**
message (see section 9.5.3).

Example
::

	CLIPS> (clear)
	CLIPS>
	(defclass CAR (is-a USER)
	  (slot price (default 75000))
	  (slot model (default Corniche)))
	CLIPS> (watch messages)
	CLIPS> (watch message-handlers)
	CLIPS> (make-instance Rolls-Royce of CAR)
	MSG >> create ED:1 (<Instance-Rolls-Royce>)
	HND >> create primary in class USER
	ED:1 (<Instance-Rolls-Royce>)
	HND << create primary in class USER
	ED:1 (<Instance-Rolls-Royce>)
	MSG << create ED:1 (<Instance-Rolls-Royce>)
	MSG >> init ED:1 (<Instance-Rolls-Royce>)
	HND >> init primary in class USER
	ED:1 (<Instance-Rolls-Royce>)
	HND << init primary in class USER
	ED:1 (<Instance-Rolls-Royce>)
	MSG << init ED:1 (<Instance-Rolls-Royce>)
	[Rolls-Royce]
	CLIPS>


9.4.4.2 Instance Deletion
^^^^^^^^^^^^^^^^^^^^^^^^^

``Syntax`` ::

  (defmessage-handler USER delete primary ())

This handler is responsible for deleting an instance from the system.
The user must directly send a **delete** message to an instance.
User-defined **delete** message-handlers should not prevent the system
message-handler from responding to a **delete** message (see section
9.5.3). The handler returns the symbol TRUE if the instance was
successfully deleted, otherwise it returns the symbol FALSE.

Example
::

	CLIPS> (send [Rolls-Royce] delete)
	MSG >> delete ED:1 (<Instance-Rolls-Royce>)
	HND >> delete primary in class USER
	ED:1 (<Instance-Rolls-Royce>)
	HND << delete primary in class USER
	ED:1 (<Stale Instance-Rolls-Royce>)
	MSG << delete ED:1 (<Stale Instance-Rolls-Royce>)
	TRUE
	CLIPS>

9.4.4.3 Instance Display
^^^^^^^^^^^^^^^^^^^^^^^^

``Syntax`` ::

  (defmessage-handler USER print primary ())

This handler prints out slots and their values for an instance.

Example
::

    CLIPS> (make-instance Rolls-Royce of CAR)

	MSG >> create ED:1 (<Instance-Rolls-Royce>)
	HND >> create primary in class USER
	ED:1 (<Instance-Rolls-Royce>)
	HND << create primary in class USER
	ED:1 (<Instance-Rolls-Royce>)
	MSG << create ED:1 (<Instance-Rolls-Royce>)
	MSG >> init ED:1 (<Instance-Rolls-Royce>)
	HND >> init primary in class USER
	ED:1 (<Instance-Rolls-Royce>)
	HND << init primary in class USER
	ED:1 (<Instance-Rolls-Royce>)
	MSG << init ED:1 (<Instance-Rolls-Royce>)
	[Rolls-Royce]
	CLIPS> (send [Rolls-Royce] print)
	MSG >> print ED:1 (<Instance-Rolls-Royce>)
	HND >> print primary in class USER
	ED:1 (<Instance-Rolls-Royce>)
	[Rolls-Royce] of CAR
	(price 75000)
	(model Corniche)
	HND << print primary in class USER
	ED:1 (<Instance-Rolls-Royce>)
	MSG << print ED:1 (<Instance-Rolls-Royce>)
	CLIPS> (unwatch messages)
	CLIPS. (unwatch message-handlers)
	CLIPS>


9.4.4.4 Directly Modifying an Instance
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

``Syntax`` ::

  (defmessage-handler USER direct-modify primary
    (?slot-override-expressions))

This handler modifies the slots of an instance directly rather than
using put- override messages to place the slot values. The slot-override
expressions are passed as an EXTERNAL_ADDRESS data object to the
direct-modify handler. This message is used by the functions
**modify-instance** and **active-modify-instance**.

Example

The following around message-handler could be used to insure that all
modify message slot-overrides are handled using put- messages.
::

  (defmessage-handler USER direct-modify around (?overrides)
    (send ?self message-modify ?overrides))
    

9.4.4.5 Modifying an Instance using Messages
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

``Syntax`` ::

  (defmessage-handler USER message-modify primary
    (?slot-override-expressions)

This handler modifies the slots of an instance using put- messages for
each slot update. The slot-override expressions are passed as an
EXTERNAL_ADDRESS data object to the message-modify handler. This message
is used by the functions **message-modify-instance** and
**active-message-modify-instance**.

9.4.4.6 Directly Duplicating an Instance
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

``Syntax`` ::

  (defmessage-handler USER direct-duplicate primary
    (?new-instance-name ?slot-override-expressions))

This handler duplicates an instance without using put- messages to
assign the slot-overrides. Slot values from the original instance and
slot overrides are directly copied. If the name of the new instance
created matches a currently existing instance-name, then the currently
existing instance is deleted without use of a message. The slot-override
expressions are passed as an EXTERNAL_ADDRESS data object to the
direct-duplicate handler. This message is used by the functions
**duplicate-instance** and **active-duplicate-instance**.

Example

The following around message-handler could be used to insure that all
duplicate message slot-overrides are handled using put- messages.
::

  (defmessage-handler USER direct-duplicate around (?new-name ?overrides)
    (send ?self message-duplicate ?new-name ?overrides))


9.4.4.7 Duplicating an Instance using Messages
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

``Syntax`` ::

  (defmessage-handler USER message-duplicate primary
    (?new-instance-name ?slot-override-expressions)

This handler duplicates an instance using messages. Slot values from the
original instance and slot overrides are copied using put- and get-
messages. If the name of the new instance created matches a currently
existing instance-name, then the currently existing instance is deleted
using a delete message. After creation, the new instance is sent a
create message and then an init message. The slot-override expressions
are passed as an EXTERNAL_ADDRESS data object to the message-duplicate
handler. This message is used by the functions
**message-duplicate-instance** and
**active-message-duplicate-instance**.

9.4.4.8 Instance Creation
^^^^^^^^^^^^^^^^^^^^^^^^^

``Syntax`` ::

  (defmessage-handler USER create primary ())

This handler is called after an instance is created , but before any
slot initialization has occurred. The newly created instance is sent a
**create** message. This handler performs no actions—It is provided so
that instance creation can be detected by user-defined message-handlers.
The handler returns the symbol TRUE if the instance was successfully
created, otherwise it returns the symbol FALSE.

9.5 Message Dispatch
--------------------

When a message is sent to an object using the **send** function, CLIPS
examines the class precedence list of the active instance’s class to
determine a complete set of message-handlers which are applicable to the
message. CLIPS uses the roles (around, before, primary or after) and
specificity of these message-handlers to establish an ordering and then
executes them. A handler that is attached to a subclass of another
message-handler’s class is said to be more specific. This entire process
is referred to as the **message dispatch**. Following is a flow diagram
summary:

|image3|

The solid arrows indicate automatic control transfer by the message
dispatch system. The dashed arrows indicate control transfer that can
only be accomplished by the use or lack of the use of
**call-next-handler** (or **override-next-handler**).

9.5.1 Applicability of Message-handlers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A message-handler is applicable to a message if its name matches the
message, and it is attached to a class which is in the class precedence
list of the class of the instance receiving the message.

9.5.2 Message-handler Precedence
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The set of all applicable message-handlers are sorted into four groups
according to role, and these four groups are further sorted by class
specificity. The around, before and primary handlers are ordered from
most specific to most general, whereas after handlers are ordered from
most general to most specific.

The order of execution is as follows: 1) around handlers begin execution
from most specific to most general (each around handler must explicitly
allow execution of other handlers), 2) before handlers execute (one
after the other) from most specific to most general 3) primary handlers
begin execution from most specific to most general (more specific
primary handlers must explicitly allow execution of more general ones),
4) primary handlers finish execution from most general to most specific,
5) after handlers execute (one after the other) from most general to
most specific and 6) around handlers finish execution from most general
to most specific.

There must be at least one applicable primary handler for a message, or
a message execution error will be generated (see section 9.5.4).

9.5.3 Shadowed Message-handlers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When one handler must be called by another handler in order to be
executed, the first handler is said to be **shadowed** by the second. An
around handler shadows all handlers except more specific around
handlers. A primary handler shadows all more general primary handlers.

Messages should be implemented using the declarative technique, if
possible. Only the handler roles will dictate which handlers get
executed; only before and after handlers and the most specific primary
handler are used. This allows each handler for a message to be
completely independent of the other message-handlers. However, if around
handlers or shadowed primary handlers are necessary, then the handlers
must explicitly take part in the message dispatch by calling other
handlers they are shadowing. This is called the imperative technique.
The functions **call-next-handler** and **override-next-handler** (see
section 12.16.2) allow a handler to execute the handler it is shadowing.
A handler can call the same shadowed handler multiple times.

Example
::

	(defmessage-handler USER my-message around ()
	  (call-next-handler))
	
	(defmessage-handler USER my-message before ())
	
	(defmessage-handler USER my-message ()
	  (call-next-handler))
	
	(defmessage-handler USER my-message after ())
	
	(defmessage-handler OBJECT my-message around ()
	  (call-next-handler))
	
	(defmessage-handler OBJECT my-message before ())
	
	(defmessage-handler OBJECT my-message ())
	
	(defmessage-handler OBJECT my-message after ())

For a message sent to an instance of a class which inherits from USER,
the following diagram illustrates the order of execution for the
handlers attached to the classes USER and OBJECT. The brackets indicate
where a particular handler begins and ends execution. Handlers enclosed
within a bracket are shadowed.

|image4|

9.5.4 Message Execution Errors
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If an error occurs at any time during the execution of a
message-handler, any currently executing handlers will be aborted, any
handlers which have not yet started execution will be ignored, and the
**send** function will return the symbol FALSE.

A lack of applicable of primary message-handlers and a handler being
called with the wrong number of arguments are common message execution
errors.

9.5.5 Message Return Value
~~~~~~~~~~~~~~~~~~~~~~~~~~

The return value of call to the **send** function is the return value of
the most specific around handler, or the most specific primary handler
if there are no around handlers. The return value of a handler is the
result of the evaluation of the last action in the handler.

The return values of the before and after handlers are ignored; they are
for side-effects only. An around handler can choose to ignore or capture
the return value of the next most specific around or primary handler. A
primary handler can choose to ignore or capture the return value of a
more general primary handler.

9.6 Manipulating Instances
--------------------------

Objects are manipulated by sending them messages. This is achieved by
using the **send** function, which takes as arguments the destination
object for the message, the message itself and any arguments which are
to be passed to handlers.

``Syntax`` ::

  (send <object-expression> <message-name-expression> <expression>*)

Section 2.4.2 explains object references. The return value of **send**
is the result of the message as explained in section 9.5.5.

The slots of an object may be read or set directly only within the body
of a message-handler that is executing on behalf of a message that was
sent to that object. This is how COOL implements the notion of
encapsulation (see Section 2.6.2). Any action performed on an object by
an external source, such as a rule or function, must be done with
messages. There are two major exceptions: 1) objects which are not
instances of user-defined classes (floating-point and integer numbers,
symbols, strings, multifield values, fact-addresses and
external-addresses) can be manipulated in the standard non-OOP manner of
previous versions of CLIPS as well and 2) creation and initialization of
an instance of a user-defined class are performed via the function
**make-instance**.

9.6.1 Creating Instances
~~~~~~~~~~~~~~~~~~~~~~~~

Like facts, instances of user-defined classes must be explicitly created
by the user. Likewise, all instances are deleted during the **reset**
command, and they can be loaded and saved similarly to facts. All
operations involving instances require message-passing using the
**send** function except for creation, since the object does not yet
exist. A function called **make-instance** is used to create and
initialize a new instance. This function implicitly sends first a create
message and then an initialization message to the new object after
allocation. The user can customize instance initialization with daemons.
**make-instance** also allows slot-overrides to change any predefined
initialization for a particular instance. **make-instance**
automatically delays all object pattern-matching activities for rules
until all slot overrides have been processed. The function
**active-make-instance** can be used if delayed pattern-matching is not
desired. **active-make-instance** remembers the current state of delayed
pattern-matching, explicitly turns delay on, and then restores it to its
previous state once all slot overrides have been processed.

``Syntax`` ::

  (make-instance <instance-definition>)
    (active-make-instance <instance-definition>)

  <instance-definition> ::= [<instance-name-expression>] of
    <class-name-expression>
    <slot-override>*

  <slot-override> ::= (<slot-name-expression> <expression>*)

The return value of **make-instance** is the name of the new instance on
success or the symbol FALSE on failure. The evaluation of
<instance-name-expression> can either be an instance-name or a symbol.
If <instance-name-expression> is not specified, then the function
**gensym*** will be called to generate the instance-name.

The **make-instance** function performs the following steps in order:

1. If an instance of the specified name already exists, that instance
   receives a **delete** message, e.g. (send <instance-name> delete). If
   this fails for any reason, the new instance creation is aborted.
   Normally, the handler attached to class USER will respond to this
   message (see section 9.4.5.2).

2. A new and uninitialized instance of the specified class is created
   with the specified name.

3. The new instance receives the **create** message, e.g. (send
   <instance-name> create). Normally, the handler attached to class USER
   will respond to this message (see section 9.4.4.8), although it
   performs no actions.

4. All slot-overrides are immediately evaluated and placed via **put-**
   messages (see section 9.3.3.10), e.g. (send <instance-name>
   put-<slot-name> <expression>*). If there are any errors, the new
   instance is deleted.

5. The new instance receives the **init** message, e.g. (send
   <instance-name> init). Normally, the handler attached to class USER will
   respond to this message (see section 9.4.4.1). This handler calls the
   **init-slots** function (see section 12.16.4.1). This function uses
   defaults from the class definition (if any) for any slots which do not
   have slot-overrides. The class defaults are placed directly without the
   use of messages. If there are any errors, the new instance is deleted.

Example
::

	CLIPS> (clear)
	CLIPS>
	(defclass A (is-a USER)
	  (slot x (default 34))
	  (slot y (default abc)))
	CLIPS>
	(defmessage-handler A put-x before (?value)
	  (printout t "Slot x set with message." crlf))
	CLIPS>
	(defmessage-handler A delete after ()
	  (printout t "Old instance deleted." crlf))
	CLIPS> (make-instance a of A)
	[a]
	CLIPS> (send [a] print)
	[a] of A
	(x 34)
	(y abc)
	CLIPS> (make-instance [a] of A (x 65))
	Old instance deleted.
	Slot x set with message.
	[a]
	CLIPS> (send [a] print)
	a of A
	(x 65)
	(y abc)
	CLIPS> (send [a] delete)
	Old instance deleted.
	TRUE
	CLIPS>

9.6.1.1 Definstances Construct
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Similar to deffacts, the **definstances** construct allows the
specification of instances which will be created every time the
**reset** command is executed. On every reset all current instances
receive a **delete** message, and the equivalent of a **make-instance**
function call is made for every instance specified in definstances
constructs.

``Syntax`` ::

  (definstances <definstances-name> [active] [<comment>] <instance-template>*)

  <instance-template> ::= (<instance-definition>)

A definstances cannot use classes that have not been previously defined.
The instances of a definstances are created in order, and if any
individual creation fails, the remainder of the definstances will be
aborted. Normally, definstances just use the **make-instance** function
(which means delayed Rete activity) to create the instances. However, if
this is not desired, then the *active* keyword can be specified after
the definstances name so that the **active-make-instance** function will
be used.

Example
::

	CLIPS> (clear)
	CLIPS>
	(defclass A (is-a USER)
	  (slot x (default 1)))
	CLIPS>
	(definstances A-OBJECTS
	  (a1 of A)
	  (of A (x 65)))
	CLIPS> (watch instances)
	CLIPS> (reset)
	<== instance [initial-object] of INITIAL-OBJECT
	==> instance [initial-object] of INITIAL-OBJECT
	==> instance [a1] of A
	==> instance [gen1] of A
	CLIPS> (reset)
	<== instance [initial-object] of INITIAL-OBJECT
	<== instance [a1] of A
	<== instance [gen1] of A
	==> instance [initial-object] of INITIAL-OBJECT
	==> instance [a1] of A
	==> instance [gen2] of A
	CLIPS> (unwatch instances)
	CLIPS>


Upon startup and after a **clear** command, CLIPS automatically
constructs the following definstances.
::

  (definstances initial-object (initial-object of INITIAL-OBJECT))

The class INITIAL-OBJECT is a predefined system class that is a direct
subclass of USER.
::

  (defclass INITIAL-OBJECT (is-a USER)
    (role concrete)
    (pattern-match reactive))

The initial-object definstances and the INITIAL-OBJECT class are only
defined if both the object system and defrules are enabled (see section
2 of the *Advanced Programming Guide*). The INITIAL-OBJECT class cannot
be deleted, but the *initial-object* definstances can. See section 5.4.9
for details on default patterns which pattern-match against the
*initial-object* instance.

9.6.2 Reinitializing Existing Instances
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The **initialize-instance** function provides the ability to
reinitialize an existing instance with class defaults and new
slot-overrides. The return value of **initialize-instance** is the name
of the instance on success or the symbol FALSE on failure. The
evaluation of <instance-name-expression> can either be an instance-name,
instance-address or a symbol. **initialize-instance** automatically
delays all object pattern-matching activities for rules until all slot
overrides have been processed. The function
**active-initialize-instance** can be used if delayed pattern-matching
is not desired.

``Syntax`` ::

  (initialize-instance <instance-name-expression> <slot-override>*)

The **initialize-instance** function performs the following steps in
order:

1. All slot-overrides are immediately evaluated and placed via **put-**
   messages (see section 9.3.3.10), e.g. (send <instance-name>
   put-<slot-name> <expression>*).

2. The instance receives the **init** message, e.g. (send
   <instance-name> init). Normally, the handler attached to class USER will
   respond to this message (see section 9.4.5.1). This handler calls the
   **init-slots** function (see section 12.16.4.1). This function uses
   defaults from the class definition (if any) for any slots that do not
   have slot-overrides. The class defaults are placed directly without the
   use of messages.

If no slot-override or class default specifies the value of a slot, that
value will remain the same. Empty class default values allow
**initialize-instance** to clear a slot.

If an error occurs, the instance will *not* be deleted, but the slot
values may be in an inconsistent state.

Example
::

	CLIPS> (clear)
	CLIPS>
	(defclass A (is-a USER)
	  (slot x (default 34))
	  (slot y (default abc))
	  (slot z))
	CLIPS> (make-instance a of A (y 100))
	[a]
	CLIPS> (send [a] print)
	[a] of A
	(x 34)
	(y 100)
	(z nil)
	CLIPS> (send [a] put-x 65)
	65
	CLIPS> (send [a] put-y abc)
	abc
	CLIPS> (send [a] put-z "Hello world.")
	“Hello world.”
	CLIPS> (send [a] print)
	[a] of A
	(x 65)
	(y abc)
	(z "Hello world.")
	CLIPS> (initialize-instance a)
	[a]
	CLIPS> (send [a] print)
	a of A
	(x 34)
	(y abc)
	(z nil)
	CLIPS>

9.6.3 Reading Slots
~~~~~~~~~~~~~~~~~~~

Sources external to an object, such as a rule or deffunction, can read
an object’s slots only by sending the object a message. Message-handlers
executing on the behalf of an object can either use messages or direct
access to read the object’s slots (see section 9.4.2). Several functions
also exist which operate implicitly on the active instance for a message
that can only be called by message-handlers, such as **dynamic-get**
(see section 12.16.4.10).

Section 12.16 describes ways of testing for the existence of slots and
their values.

Example
::

	CLIPS> (clear)
	CLIPS>
	(defclass A (is-a USER)
	(slot x (default abc)))
	CLIPS> (make-instance a of A)
	[a]
	CLIPS> (sym-cat (send [a] get-x) def)
	abcdef
	CLIPS>


9.6.4 Setting Slots
~~~~~~~~~~~~~~~~~~~

Sources external to an object, such as a rule or deffunction, can write
an object’s slots only by sending the object a message. Several
functions also exist which operate implicitly on the active instance for
a message that can only be called by message-handlers, such as
**dynamic-put** (see section 12.16.4.11). The **bind** function can also
be used to set a slot's value from within a message-handler (see section
9.4.2).

Example
::

	CLIPS> (clear)
	CLIPS>
	(defclass A (is-a USER)
	(slot x (default abc)))
	CLIPS> (make-instance a of A)
	[a]
	CLIPS> (send [a] put-x "New value.")
	“New value.”
	CLIPS>

9.6.5 Deleting Instances
~~~~~~~~~~~~~~~~~~~~~~~~

Sending the **delete** message to an instance removes it from the
system. Within a message-handler, the **delete-instance** function (see
section 12.16) can be used to delete the active instance for a message.

``Syntax`` ::

  (send <instance> delete)


9.6.6 Delayed Pattern-Matching When Manipulating Instances
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

While manipulating instances (either by creating, modifying, or
deleting), it is possible to delay pattern-matching activities for rules
until after all of the manipulations have been made. This can be
accomplished using the **object-pattern-match-delay** function. This
function acts identically to the **progn** function, however, any
actions that could affect object pattern-matching for rules are delayed
until the function is exited. This function’s primary purpose is to
provide some control over performance.

``Syntax`` ::

  (object-pattern-match-delay <action>*)

Example
::

	CLIPS> (clear)
	CLIPS>
	(defclass A (is-a USER))
	CLIPS>
	(defrule match-A
	  (object (is-a A))
	  =>)
	CLIPS> (make-instance a of A)
	[a]
	CLIPS> (agenda)
	0 match-A: [a]
	For a total of 1 activation.
	CLIPS> (make-instance b of A)
	[b]
	CLIPS> (agenda)
	0 match-A: [b]
	0 match-A: [a]
	For a total of 2 activations.
	CLIPS>
	(object-pattern-match-delay
	  (make-instance c of A)
	  (printout t "After c..." crlf)
	  (agenda)
	  (make-instance d of A)
	  (printout t "After d..." crlf)
	  (agenda))
	After c...
	0 match-A: [b]
	0 match-A: [a]
	For a total of 2 activations.
	After d...
	0 match-A: [b]
	0 match-A: [a]
	For a total of 2 activations.
	CLIPS> (agenda)
	0 match-A: [d]
	0 match-A: [c]
	0 match-A: [b]
	0 match-A: [a]
	For a total of 4 activations.
	CLIPS>


9.6.7 Modifying Instances
~~~~~~~~~~~~~~~~~~~~~~~~~

Four functions are provided for modifying instances. These functions
allow instance slot updates to be performed in blocks without requiring
a series of put- messages. Each of these functions returns the symbol
TRUE if successful, otherwise the symbol FALSE is returned.

9.6.7.1 Directly Modifying an Instance with Delayed Pattern-Matching
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The **modify-instance** function uses the **direct-modify** message to
change the values of the instance. Object pattern-matching is delayed
until all of the slot modifications have been performed.

``Syntax`` ::

  (modify-instance <instance> <slot-override>*)

Example
::

	CLIPS> (clear)
	CLIPS>
	(defclass A (is-a USER)
	  (slot foo)
	  (slot bar))
	CLIPS> (make-instance a of A)
	[a]
	CLIPS> (watch all)
	CLIPS> (modify-instance a (foo 0))
	MSG >> direct-modify ED:1 (<Instance-a> <Pointer-0019CD5A>)
	HND >> direct-modify primary in class USER.
	ED:1 (<Instance-a> <Pointer-0019CD5A>)
	::= local slot foo in instance a <- 0
	HND << direct-modify primary in class USER.
	ED:1 (<Instance-a> <Pointer-0019CD5A>)
	MSG << direct-modify ED:1 (<Instance-a> <Pointer-0019CD5A>)
	TRUE
	CLIPS> (unwatch all)
	CLIPS>


9.6.7.2 Directly Modifying an Instance with Immediate Pattern-Matching
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The **active-modify-instance** function uses the **direct-modify**
message to change the values of the instance. Object pattern-matching
occurs as slot modifications are being performed.

``Syntax`` ::

  (active-modify-instance <instance> <slot-override>*)


9.6.7.3 Modifying an Instance using Messages with Delayed Pattern-Matching
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The **message-modify-instance** function uses the **message-modify**
message to change the values of the instance. Object pattern-matching is
delayed until all of the slot modifications have been performed.

``Syntax`` ::

  (message-modify-instance <instance> <slot-override>*)

Example
::

	CLIPS> (clear)
	CLIPS>
	(defclass A (is-a USER)
	  (slot foo)
	  (slot bar))
	CLIPS> (make-instance a of A)
	[a]
	CLIPS> (watch all)
	CLIPS> (message-modify-instance a (bar 4))
	MSG >> message-modify ED:1 (<Instance-a> <Pointer-009F04A0>)
	HND >> message-modify primary in class USER
	ED:1 (<Instance-a> <Pointer-009F04A0>)
	MSG >> put-bar ED:2 (<Instance-a> 4)
	HND >> put-bar primary in class A
	ED:2 (<Instance-a> 4)
	::= local slot bar in instance a <- 4
	HND << put-bar primary in class A
	ED:2 (<Instance-a> 4)
	MSG << put-bar ED:2 (<Instance-a> 4)
	HND << message-modify primary in class USER
	ED:1 (<Instance-a> <Pointer-009F04A0>)
	MSG << message-modify ED:1 (<Instance-a> <Pointer-009F04A0>)
	TRUE
	CLIPS> (unwatch all)
	CLIPS>

9.6.7.4 Modifying an Instance using Messages with Immediate Pattern-Matching
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The **active-message-modify-instance** function uses the
**message-modify** message to change the values of the instance. Object
pattern-matching occurs as slot modifications are being performed.

``Syntax`` ::

  (active-message-modify-instance <instance> <slot-override>*)

9.6.8 Duplicating Instances
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Four functions are provided for duplicating instances. These functions
allow instance duplication and slot updates to be performed in blocks
without requiring a series of put- messages. Each of these functions
return the instance-name of the new duplicated instance if successful,
otherwise the symbol FALSE is returned.

Each of the duplicate functions can optionally specify the name of the
instance to which the old instance will be copied. If the name is not
specified, the function will generate the name using the (gensym*)
function. If the target instance already exists, it will be deleted
directly or with a delete message depending on which function was
called.

9.6.8.1 Directly Duplicating an Instance with Delayed Pattern-Matching
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The **duplicate-instance** function uses the **direct-duplicate**
message to change the values of the instance. Object pattern-matching is
delayed until all of the slot modifications have been performed.

``Syntax`` ::

  (duplicate-instance <instance> [to <instance-name>] <slot-override>*)

Example
::

	CLIPS> (clear)
	CLIPS> (setgen 1)
	1
	CLIPS>
	(defclass A (is-a USER)
	  (slot foo)
	  (slot bar))
	CLIPS> (make-instance a of A (foo 0) (bar 4))
	[a]
	CLIPS> (watch all)
	CLIPS> (duplicate-instance a)
	MSG >> direct-duplicate ED:1 (<Instance-a> [gen1] <Pointer-00000000>)
	HND >> direct-duplicate primary in class USER
	ED:1 (<Instance-a> [gen1] <Pointer-00000000>)
	==> instance [gen1] of A
	::= local slot foo in instance gen1 <- 0
	::= local slot bar in instance gen1 <- 4
	HND << direct-duplicate primary in class USER
	ED:1 (<Instance-a> [gen1] <Pointer-00000000>)
	MSG << direct-duplicate ED:1 (<Instance-a> [gen1] <Pointer-00000000>)
	[gen1]
	CLIPS> (unwatch all)
	CLIPS>


9.6.8.2 Directly Duplicating an Instance with Immediate Pattern-Matching
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The **active-duplicate-instance** function uses the **direct-duplicate**
message to change the values of the instance. Object pattern-matching
occurs as slot modifications are being performed.

``Syntax`` ::

  (active-duplicate-instance <instance> [to <instance-name>] <slot-override>*)

9.6.8.3 Duplicating an Instance using Messages with Delayed Pattern-Matching
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The **message-duplicate-instance** function uses the
**message-duplicate** message to change the values of the instance.
Object pattern-matching is delayed until all of the slot modifications
have been performed.

``Syntax`` ::

  (message-duplicate-instance <instance> [to <instance-name>] <slot-override>*)

Example
::

	CLIPS> (clear)
	CLIPS>
	(defclass A (is-a USER)
	  (slot foo)
	  (slot bar))
	CLIPS> (make-instance a of A (foo 0) (bar 4))
	[a]
	CLIPS> (make-instance b of A)
	[b]
	CLIPS> (watch all)
	CLIPS> (message-duplicate-instance a to b (bar 6))
	MSG >> message-duplicate ED:1 (<Instance-a> [b] <Pointer-009F04A0>)
	HND >> message-duplicate primary in class USER
	ED:1 (<Instance-a> [b] <Pointer-009F04A0>)
	MSG >> delete ED:2 (<Instance-b>)
	HND >> delete primary in class USER
	ED:2 (<Instance-b>)
	<== instance [b] of A
	HND << delete primary in class USER
	ED:2 (<Stale Instance-b>)
	MSG << delete ED:2 (<Stale Instance-b>)
	==> instance [b] of A
	MSG >> create ED:2 (<Instance-b>)
	HND >> create primary in class USER
	ED:2 (<Instance-b>)
	HND << create primary in class USER
	ED:2 (<Instance-b>)
	MSG << create ED:2 (<Instance-b>)
	MSG >> put-bar ED:2 (<Instance-b> 6)
	HND >> put-bar primary in class A
	ED:2 (<Instance-b> 6)
	::= local slot bar in instance b <- 6
	HND << put-bar primary in class A
	ED:2 (<Instance-b> 6)
	MSG << put-bar ED:2 (<Instance-b> 6)
	MSG >> put-foo ED:2 (<Instance-b> 0)
	HND >> put-foo primary in class A
	ED:2 (<Instance-b> 0)
	::= local slot foo in instance b <- 0
	HND << put-foo primary in class A
	ED:2 (<Instance-b> 0)
	MSG << put-foo ED:2 (<Instance-b> 0)
	MSG >> init ED:2 (<Instance-b>)
	HND >> init primary in class USER
	ED:2 (<Instance-b>)
	HND << init primary in class USER
	ED:2 (<Instance-b>)
	MSG << init ED:2 (<Instance-b>)
	HND << message-duplicate primary in class USER
	ED:1 (<Instance-a> [b] <Pointer-009F04A0>)
	MSG << message-duplicate ED:1 (<Instance-a> [b] <Pointer-009F04A0>)
	[b]
	CLIPS> (unwatch all)
	CLIPS>

9.6.8.4 Duplicating an Instance using Messages with Immediate Pattern-Matching
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The **active-message-duplicate-instance** function uses the
**message-duplicate** message to change the values of the instance.
Object pattern-matching occurs as slot modifications are being
performed.

``Syntax`` ::

  (active-message-duplicate-instance <instance> [to <instance-name>]
     <slot-override>*)

.. _instanceset-queries-and-distributed-actions-1:

9.7 Instance-set Queries and Distributed Actions
------------------------------------------------

COOL provides a useful query system for determining and performing
actions on sets of instances of user-defined classes that satisfy
user-defined queries. The instance query system in COOL provides six
functions, each of which operate on instance-sets determined by
user-defined criteria:

============================ =============================================================================================
**Function**                 **Purpose**
============================ =============================================================================================
any-instancep                   Determines if one or more instance-sets satisfy a query
find-instance                   Returns the first instance-set that satisfies a query
find-all-instances              Groups and returns all instance-sets which satisfy a query
do-for-instance                 Performs an action for the first instance-set which satisfies a query
do-for-all-instances            Performs an action for every instance-set which satisfies a query as they are found
delayed-do-for-all-instances    Groups all instance-sets which satisfy a query and then iterates an action over this group
============================ =============================================================================================

Explanations on how to form instance-set templates, queries and actions
immediately follow, for these definitions are common to all of the query
functions. The specific details of each query function will then be
given. The following is a complete example of an instance-set query
function:

Example

|image5|

For all of the examples in this section, assume that the commands below
have already been entered:

Example
::

	CLIPS>
	(defclass PERSON (is-a USER)
	  (role abstract)
	  (slot sex (access read-only)
	  (storage shared))
	  (slot age (type NUMBER)
	  (create-accessor ?NONE)
	  (visibility public)))
	CLIPS>
	(defmessage-handler PERSON put-age (?value)
	  (dynamic-put age ?value))
	CLIPS>
	(defclass FEMALE (is-a PERSON)
	  (role abstract)
	  (slot sex (source composite)
	  (default female)))
	CLIPS>
	(defclass MALE (is-a PERSON)
	  (role abstract)
	  (slot sex (source composite)
	  (default male)))
	CLIPS>
	(defclass GIRL (is-a FEMALE)
	  (role concrete)
	  (slot age (source composite)
	  (default 4)
	  (range 0.0 17.9)))
	CLIPS>
	(defclass WOMAN (is-a FEMALE)
	  (role concrete)
	  (slot age (source composite)
	  (default 25)
	  (range 18.0 100.0)))
	CLIPS>
	(defclass BOY (is-a MALE)
	  (role concrete)
	  (slot age (source composite)
	  (default 4)
	  (range 0.0 17.9)))
	CLIPS>
	(defclass MAN (is-a MALE)
	  (role concrete)
	  (slot age (source composite)
	  (default 25)
	  (range 18.0 100.0)))
	CLIPS>
	(definstances PEOPLE
	  (Man-1 of MAN (age 18))
	  (Man-2 of MAN (age 60))
	  (Woman-1 of WOMAN (age 18))
	  (Woman-2 of WOMAN (age 60))
	  (Woman-3 of WOMAN)
	  (Boy-1 of BOY (age 8))
	  (Boy-2 of BOY)
	  (Boy-3 of BOY)
	  (Boy-4 of BOY)
	  (Girl-1 of GIRL (age 8))
	  (Girl-2 of GIRL))
	CLIPS> (reset)
	CLIPS>

9.7.1 Instance-set Definition
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

An **instance-set** is an ordered collection of instances. Each
**instance-set member** is an instance of a set of classes, called
**class restrictions**, defined by the user. The class restrictions can
be different for each instance-set member. The query functions use
**instance-set templates** to generate instance-sets. An instance-set
template is a set of **instance-set member variables** and their
associated class restrictions. Instance-set member variables reference
the corresponding members in each instance-set that matches a template.
Variables may be used to specify the classes for the instance-set
template, but if the constant names of the classes are specified, the
classes must already be defined. Module specifiers may be included with
the class names; the classes need not be in scope of the current module.

``Syntax`` ::

  <instance-set-template> ::= (<instance-set-member-template>+)

  <instance-set-member-template> ::= (<instance-set-member-variable> 
     <class-restrictions>)

  <instance-set-member-variable> ::= <single-field-variable>

  <class-restrictions> ::= <class-name-expression>+

Example

One instance-set template might be the ordered pairs of boys or men and
girls or women.
::

  ((?man-or-boy BOY MAN) (?woman-or-girl GIRL WOMAN))

This instance-set template could have been written equivalently:
::

  ((?man-or-boy MALE) (?woman-or-girl FEMALE))

Instance-set member variables (e.g. ?man-or-boy) are bound to
instance-names.

9.7.2 Instance-set Determination
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

COOL uses straightforward permutations to generate instance-sets that
match an instance-set template from the actual instances in the system.
The rules are as follows:

1. When there is more than one member in an instance-set template, vary
   the rightmost members first.

2. When there is more than one class that an instance-set member can be,
   iterate through the classes from left to right.

3. Examine instances of a class in the order that they were defined.

4. Recursively examine instances of subclasses in the order that the
   subclasses were defined. If the specified query class was in scope of
   the current module, then only subclasses that are also in scope will be
   examined. Otherwise, only subclasses that are in scope of the module to
   which the query class belongs will be examined.

Example

For the instance-set template given in section 9.7.1, thirty
instance-sets would be generated in the following order:
::

	===================== =====================
	1. [Boy-1] [Girl-1]   16. [Boy-4] [Girl-1]
	2. [Boy-1] [Girl-2]   17. [Boy-4] [Girl-2]
	3. [Boy-1] [Woman-1]  18. [Boy-4] [Woman-1]
	4. [Boy-1] [Woman-2]  19. [Boy-4] [Woman-2]
	5. [Boy-1] [Woman-3]  20. [Boy-4] [Woman-3]
	6. [Boy-2] [Girl-1]   21. [Man-1] [Girl-1]
	7. [Boy-2] [Girl-2]   22. [Man-1] [Girl-2]
	8. [Boy-2] [Woman-1]  23. [Man-1] [Woman-1]
	9. [Boy-2] [Woman-2]  24. [Man-1] [Woman-2]
	10.[Boy-2] [Woman-3]  25. [Man-1] [Woman-3]
	11.[Boy-3] [Girl-1]   26. [Man-2] [Girl-1]
	12.[Boy-3] [Girl-2]   27. [Man-2] [Girl-2]
	13 [Boy-3] [Woman-1]  28. [Man-2] [Woman-1]
	14.[Boy-3] [Woman-2]  29. [Man-2] [Woman-2]
	15.[Boy-3] [Woman-3]  30. [Man-2] [Woman-3]
	===================== =====================


Example

Consider the following instance-set template:
::

  ((?f1 FEMALE) (?f2 FEMALE))


Twenty-five instance-sets would be generated in the following order:
::

	====================== ======================
	1. [Girl-1] [Girl-1]   14.[Woman-1] [Woman-2]
	2. [Girl-1] [Girl-2]   15.[Woman-1] [Woman-3]
	3. [Girl-1] [Woman-1]  16.[Woman-2] [Girl-1]
	4. [Girl-1] [Woman-2]  17.[Woman-2] [Girl-2]
	5. [Girl-1] [Woman-3]  18.[Woman-2] [Woman-1]
	6. [Girl-2] [Girl-1]   19.[Woman-2] [Woman-2]
	7. [Girl-2] [Girl-2]   20.[Woman-2] [Woman-3]
	8. [Girl-2] [Woman-1]  21.[Woman-3] [Girl-1]
	9. [Girl-2] [Woman-2]  22.[Woman-3] [Girl-2]
	10.[Girl-2] [Woman-3]  23.[Woman-3] [Woman-1]
	11.[Woman-1] [Girl-1]  24.[Woman-3] [Woman-2]
	12.[Woman-1] [Girl-2]  25.[Woman-3] [Woman-3]
	13.[Woman-1] [Woman-1]
	====================== ======================


The instances of class GIRL are examined before the instances of class
WOMAN because GIRL was defined before WOMAN.

9.7.3 Query Definition
~~~~~~~~~~~~~~~~~~~~~~

A **query** is a user-defined boolean expression applied to an
instance-set to determine if the instance-set meets further user-defined
restrictions. If the evaluation of this expression for an instance-set
is anything but the symbol FALSE, the instance-set is said to satisfy
the query.

``Syntax`` ::

  <query> ::= <boolean-expression>

Example

Continuing the previous example, one query might be that the two
instances in an ordered pair have the same age.
::

  (= (send ?man-or-boy get-age) (send ?woman-or-girl get-age))

Within a query, slots of instance-set members can be directly read with
a shorthand notation similar to that used in message-handlers (see
section 9.4.2). If message-passing is not explicitly required for
reading a slot (i.e. there are no accessor daemons for reads), then this
second method of slot access should be used, for it gives a significant
performance benefit.

``Syntax`` ::

  <instance-set-member-variable>:<slot-name>

Example

The previous example could be rewritten as:
::

  (= ?man-or-boy:age ?woman-or-girl:age)

Since only instance-sets that satisfy a query are of interest, and the
query is evaluated for all possible instance-sets, the query should not
have any side-effects.

9.7.4 Distributed Action Definition
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A **distributed action** is a user-defined expression evaluated for each
instance-set which satisfies a query. Unlike queries, distributed
actions must use messages to read slots of instance-set members. If more
than one action is required, use the **progn** function (see section
12.6.5) to group them.

Action ``Syntax`` ::

  <action> ::= <expression>

Example

Continuing the previous example, one distributed action might be to
simply print out the ordered pair to the screen.
::

  (printout t "(" ?man-or-boy "," ?woman-or-girl ")" crlf)

9.7.5 Scope in Instance-set Query Functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

An instance-set query function can be called from anywhere that a
regular function can be called. If a variable from an outer scope is not
masked by an instance-set member variable, then that variable may be
referenced within the query and action. In addition, rebinding variables
within an instance-set function action is allowed. However, attempts to
rebind instance-set member variables will generate errors. Binding
variables is not allowed within a query. Instance-set query functions
can be nested.

Example
::

	CLIPS>
	(deffunction count-instances (?class)
	  (bind ?count 0)
	  (do-for-all-instances ((?ins ?class)) TRUE
	  (bind ?count (+ ?count 1)))
	  ?count)
	CLIPS>
	(deffunction count-instances-2 (?class)
	  (length (find-all-instances ((?ins ?class)) TRUE)))
	CLIPS> (count-instances WOMAN)
	3
	CLIPS> (count-instances-2 BOY)
	4
	CLIPS>


Instance-set member variables are only in scope within the instance-set
query function. Attempting to use instance-set member variables in an
outer scope will generate an error.

Example
::

    CLIPS>
	(deffunction last-instance (?class)
	  (any-instancep ((?ins ?class)) TRUE)
	  ?ins)
	[PRCCODE3] Undefined variable ins referenced in deffunction.
	ERROR:
	(deffunction MAIN::last-instance
	  (?class)
	  (any-instancep ((?ins ?class))
	  TRUE)
	  ?ins)
	CLIPS>


9.7.6 Errors during Instance-set Query Functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If an error occurs during an instance-set query function, the function
will be immediately terminated and the return value will be the symbol
FALSE.

9.7.7 Halting and Returning Values from Query Functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The functions **break** and **return** are now valid inside the action
of the instance-set query functions **do-for-instance**,
**do-for-all-instances** and **delayed-do-for-all-instances**. The
**return** function is only valid if it is applicable in the outer
scope, whereas the **break** function actually halts the query.

9.7.8 Instance-set Query Functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The instance query system in COOL provides six functions. For a given
set of instances, all six query functions will iterate over these
instances in the same order (see section 9.7.2). However, if a
particular instance is deleted and recreated, the iteration order will
change.

9.7.8.1 Testing if Any Instance-set Satisfies a Query
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This function applies a query to each instance-set that matches the
template. If an instance-set satisfies the query, then the function is
immediately terminated, and the return value is the symbol TRUE.
Otherwise, the return value is the symbol FALSE.

``Syntax`` ::

  (any-instancep <instance-set-template> <query>)

Example

Are there any men over age 30?
::

  CLIPS> (any-instancep ((?man MAN)) (> ?man:age 30))
  TRUE
  CLIPS>

9.7.8.2 Determining the First Instance-set Satisfying a Query
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This function applies a query to each instance-set that matches the
template. If an instance-set satisfies the query, then the function is
immediately terminated, and the instance-set is returned in a multifield
value. Otherwise, the return value is a zero-length multifield value.
Each field of the multifield value is an instance-name representing an
instance-set member.

``Syntax`` ::

  (find-instance <instance-set-template> <query>)

Example

Find the first pair of a man and a woman who have the same age.
::
  CLIPS>
  (find-instance ((?m MAN) (?w WOMAN)) (= ?m:age ?w:age))

  ([Man-1] [Woman-1])
  CLIPS>

9.7.8.3 Determining All Instance-sets Satisfying a Query
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This function applies a query to each instance-set that matches the
template. Each instance-set that satisfies the query is stored in a
multifield value. This multifield value is returned when the query has
been applied to all possible instance-sets. If there are n instances in
each instance-set, and m instance-sets satisfied the query, then the
length of the returned multifield value will be n * m. The first n
fields correspond to the first instance-set, and so on. Each field of
the multifield value is an instance-name representing an instance-set
member. The multifield value can consume a large amount of memory due to
permutational explosion, so this function should be used judiciously.

``Syntax`` ::

  (find-all-instances <instance-set-template> <query>)

Example

Find all pairs of a man and a woman who have the same age.
::

  CLIPS>
  (find-all-instances ((?m MAN) (?w WOMAN)) (= ?m:age ?w:age))
  
  ([Man-1] [Woman-1] [Man-2] [Woman-2])
  CLIPS>

9.7.8.4 Executing an Action for the First Instance-set Satisfying a Query
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This function applies a query to each instance-set that matches the
template. If an instance-set satisfies the query, the specified action
is executed, and the function is immediately terminated. The return
value is the evaluation of the action. If no instance-set satisfied the
query, then the return value is the symbol FALSE.

``Syntax`` ::

  (do-for-instance <instance-set-template> <query> <action>*)

Example

Print out the first triplet of different people that have the same age.
The calls to **neq** in the query eliminate the permutations where two
or more members of the instance-set are identical.
::

	CLIPS>
	(do-for-instance ((?p1 PERSON) (?p2 PERSON) (?p3 PERSON))
	  (and (= ?p1:age ?p2:age ?p3:age)
	  (neq ?p1 ?p2)
	  (neq ?p1 ?p3)
	  (neq ?p2 ?p3))
	  (printout t ?p1 " " ?p2 " " ?p3 crlf))
	
	  [Girl-2] [Boy-2] [Boy-3]
	  CLIPS>

9.7.8.5 Executing an Action for All Instance-sets Satisfying a Query
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This function applies a query to each instance-set that matches the
template. If an instance-set satisfies the query, the specified action
is executed. The return value is the evaluation of the action for the
last instance-set that satisfied the query. If no instance-set satisfied
the query, then the return value is the symbol FALSE.

``Syntax`` ::

  (do-for-all-instances <instance-set-template> <query> <action>*)

Example

Print out all triplets of different people that have the same age. The
calls to **str-compare** limit the instance-sets that satisfy the query
to combinations instead of permutations. Without these restrictions, two
instance-sets that differed only in the order of their members would
both satisfy the query.
::

    CLIPS>

	(do-for-all-instances ((?p1 PERSON) (?p2 PERSON) (?p3 PERSON))
	  (and (= ?p1:age ?p2:age ?p3:age)
	  (> (str-compare ?p1 ?p2) 0)
	  (> (str-compare ?p2 ?p3) 0))
	  (printout t ?p1 " " ?p2 " " ?p3 crlf))

	[Girl-2] [Boy-3] [Boy-2]
	[Girl-2] [Boy-4] [Boy-2]
	[Girl-2] [Boy-4] [Boy-3]
	[Boy-4] [Boy-3] [Boy-2]

	CLIPS>


9.7.8.6 Executing a Delayed Action for All Instance-sets Satisfying a Query
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This function is similar to **do-for-all-instances** except that it
groups all instance-sets that satisfy the query into an intermediary
multifield value. If there are no instance-sets which satisfy the query,
then the function returns the symbol FALSE. Otherwise, the specified
action is executed for each instance-set in the multifield value, and
the return value is the evaluation of the action for the last
instance-set to satisfy the query. The intermediary multifield value is
discarded. This function can consume large amounts of memory in the same
fashion as **find-all-instances**. This function should be used in lieu
of **do-for-all-instances** when the action applied to one instance-set
would change the result of the query for another instance-set (unless
that is the desired effect).

``Syntax`` ::

  (delayed-do-for-all-instances <instance-set-template> <query> <action>*)

Example

Delete all boys with the greatest age. The test in this case is another
query function that determines if there are any older boys than the one
currently being examined. The action needs to be delayed until all boys
have been processed, or the greatest age will decrease as the older boys
are deleted.
::

	CLIPS> (watch instances)
	CLIPS>
	(delayed-do-for-all-instances ((?b1 BOY))
	  (not (any-instancep ((?b2 BOY))
	  (> ?b2:age ?b1:age)))
	  (send ?b1 delete))
	<== instance [Boy-1] of BOY
	TRUE
	CLIPS> (unwatch instances)
	CLIPS> (reset)
	CLIPS> (watch instances)
	CLIPS>
	(do-for-all-instances ((?b1 BOY))
	  (not (any-instancep ((?b2 BOY))
	  (> ?b2:age ?b1:age)))
	  (send ?b1 delete))
	<== instance [Boy-1] of BOY
	<== instance [Boy-2] of BOY
	<== instance [Boy-3] of BOY
	<== instance [Boy-4] of BOY
	TRUE
	CLIPS> (unwatch instances)
	CLIPS>

.. _section-7:
