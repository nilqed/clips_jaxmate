Appendix A: Support Information
===============================

A.1 Questions and Information
-----------------------------

The URL for the CLIPS Web page is http://clipsrules.sourceforge.net.

Questions regarding CLIPS can be posted to one of several online forums
including the CLIPS Expert System Group,
http://groups.google.com/group/CLIPSESG/, the SourceForge CLIPS Forums,
http://sourceforge.net/forum/?group_id=215471, and Stack Overflow,
http://stackoverflow.com/questions/tagged/clips.

Inquiries related to the use or installation of CLIPS can be sent via
electronic mail to clipssupport@secretsocietysoftware.com.

A.2 Documentation
-----------------

The CLIPS Reference Manuals and User痴 Guide are available in Portable
Document Format (PDF) at
http://clipsrules.sourceforge.net/OnlineDocs.html.

*Expert Systems: Principles and Programming*, 4th Edition, by Giarratano
and Riley comes with a CD-ROM containing CLIPS 6.22 executables (DOS,
Windows XP, and Mac OS), documentation, and source code. The first half
of the book is theory oriented and the second half covers rule-based,
procedural, and object-oriented programming using CLIPS.

A.3 CLIPS Source Code and Executables
-------------------------------------

CLIPS executables and source code are available on the SourceForge web
site at http://sourceforge.net/projects/clipsrules/files.

Appendix B:
Update Release Notes
====================

The following sections denote the changes and bug fixes for CLIPS
versions 6.2. 6.21, 6.22, 6.23, 6.24, and 6.3.

B.1 Version 6.30
----------------

・**Performance Improvements** ・Rule performance has been improved
particularly in situations with large numbers of fact/instances or
partial matches.

・**64-bit Integers** ・Integers in CLIPS are now represented using the
斗ong long・C data type which provides a minimum of 64 bits of
precision.

・**Reset after Clear** ・A reset command is now performed after a clear
command (which includes the clear command issued internally by CLIPS
when it is started). Since no user constructs will be present after a
clear, the primary effect of this behavior is to create the initial-fact
and initial-object.

・**Pattern Addition** ・The initial-fact and initial-object patterns
are no longer used in triggering rules. When printing partial matches,
the \* symbol is used to indicate a not or exists pattern that is
satisfied.

・**Module Specifiers** ・A module specifier can be used in expressions
to reference a deffunction or defgeneric that is exported by a module,
but not specifically imported by the module which is referencing it. For
example: (UTIL::my-function a 3).

・**Instance Name and Class Visibility** ・Instance names now have
global scope and must be unique regardless of their module. Instances of
classes that are not in scope can be created if the module name is
specified as part of the class name. Messages can be sent to instances
regardless of whether the instance class is in scope.

・**Command Prompt** ・Local variables bound at the command prompt using
the bind function persist until a reset or clear command is issued (see
section 2.1.1).

・**Printout Function** ・The deprecated use of the symbol *t* as a
substitute for the *crlf* symbol is no longer allowed.

・**MicroEMACS Editor** ・The built-in editor is no longer supported.

・**New Functions and Commands** - Several new functions and commands
have been added. They are:

・**foreach** (see section 12.6.10)

・**operating-system** (see section 12.7.12)

・**Command and Function Changes** - The following commands and
functions have been enhanced:

・**matches** (see section 13.6.4). This command now has a return value
indicating the number of matches, partial matches, and activations. The
amount of output can be controlled with a verbosity argument.

・**open** (see section 12.4.2.1). The r+ mode is no longer supported.
New modes ab and rb have been added.

・**Help Functions** ・The **help** and **help-path** funtions are no
longer supported

・**Behavior Changes** - The following changes have been made to
behavior:

・A defgeneric redefinition warning is no longer printed when a
defmethod is defined.

B.2 Version 6.24
----------------

・**Allowed Classes Constraint Attribute** ・The allowed-classes
constraint attribute allows a slot containing an instance value to be
restricted to the specified list of classes.

・**New Functions and Commands** - Several new functions and commands
have been added. They are:

・**deftemplate-slot-allowed-values**

・**deftemplate-slot-cardinality**

・**deftemplate-slot-defaultp**

・**deftemplate-slot-default-value**

・**deftemplate-slot-existp**

・**deftemplate-slot-multip**

・**deftemplate-slot-names**

・**deftemplate-slot-range**

・**deftemplate-slot-singlep**

・**deftemplate-slot-type**

・**get-char**

・**get-region**

・**ppfact**

・**read-number**

・**set-locale**

・**slot-allowed-classes**

・**Command and Function Changes** - The following commands and
functions have been enhanced:

・**format** The formatting of printed numbers can be changed to use a
native locale with the **set-locale** function. The documentation has
been updated to include the effect of the precision argument on the d,
g, o, and x format flags.

・**Behavior Changes** - The following changes have been made to
behavior:

・The message displayed when a construct is redefined and compilations
are being watched is now more prominent.

・**Bug Fixes** - The following bugs were fixed by the 6.24 release:

・The DescribeClass macros were incorrectly defined.

・The sort function leaks memory when called with a multifield value of
length zero.

・Link error occurred for the SlotExistError function when OBJECT_SYSTEM
is set to 0 in setup.h.

・An error when calling the Eval function causes a subsequent call to
DeallocateEnvironmentData to fail.

・Loading a binary instance file from a run-time program caused a bus
error.

・Incorrect activations could occur with the exists CE.

・Compilation errors occurred when compiling CLIPS source as C++ files.

・The AssignFactSlotDefaults function did not correctly handle defaults
for multifield slots.

・The slot-default-value function crashed when no default existed for a
slot (the ?NONE value was specified).

・CLIPS crashed on AMD64 processor in the function used to generate hash
values for integers.

・A syntax error was not generated for the last deffunction or defmethod
in a file if it was missing the final closing right parenthesis.

・**Compiler Support** - The following compilers are now supported. See
the *Interfaces Guide* for more details.

・Metrowerks CodeWarrior 9.6 for Mac OS X.

・Xcode 2.3 for Mac OS X.

・Microsoft Visual C++ .NET 2003 for Windows.

B.3 Version 6.23
----------------

・**Fact-Set Query Functions** ・Six new functions similar to the
instance set query functions have been added for determining and
performing actions on sets of facts that satisfy user-defined queries:
any-factp, find-fact, find-all-facts, do-for-fact, do-for-all-facts, and
delayed-do-for-all-facts. The GetNextFactInTemplate function allows
iteration from C over the facts belonging to a specific deftemplate.

・**Bug Fixes** - The following bugs were fixed by the 6.23 release:

・Passing the wrong number of arguments to a deffunction through the
funcall function could cause unpredictable behavior including memory
corruption.

・A large file name (at least 60 characters) passed into the fetch
command causes a buffer overrun.

・A large file name (at least 60 characters) passed into the
constructs-to-c command causes a buffer overrun.

・A large defclass or defgeneric name (at least 500 characters) causes a
buffer overrun when the profile-info command is called.

・A large module or construct name (at least 500 characters) causes a
buffer overrun when the get-<construct>-list command is called.

・The FalseSymbol and TrueSymbol constants were not defined as described
in the *Advanced Programming Guide*. These constants have have now been
defined as macros so that their corresponding environment companion
functions (EnvFalseSymbol and EnvTrueSymbol) could be defined. See the
*Advanced Programming Guide* for more details.

・The slot-writablep function returns TRUE for slots having
initialize-only access.

・Files created by the constructs-to-c function for use in a run-time
program generate compilation errors.

・**Command and Function Changes** - The following commands and
functions have been enhanced:

・**funcall** Multifield arguments are no longer expanded into multiple
arguments before being passed to the target function of the funcall. The
expand$ function can be placed around an argument to revert to the old
behavior.

・**Compiler Support** - The following compilers are now supported. See
the *Interfaces Guide* for more details.

・Metrowerks CodeWarrior 9.4 for Mac OS X and Windows.

・Xcode 1.2 for Mac OS X.

B.4 Version 6.22
----------------

・**Bug Fixes** - The following bugs were fixed by the 6.22 release:

・Numerous fixes for functions and macros that did not accept the
correct number of arguments as specified in the *Advanced Programming
Guide*. The following functions and macros were corrected: Agenda,
BatchStar, EnvGetActivationSalience, EnvBatchStar, EnvFactDeftemplate,
EnvFactExistp, EnvFactList, EnvFactSlotNames,
EnvGetNextInstanceInClassAndSubclasses, EnvLoadInstancesFromString,
EnvRestoreInstancesFromString, EnvSetOutOfMemoryFunction,
FactDeftemplate, FactExistp, FactList, FactSlotNames,
GetNextInstanceInClassAndSubclasses, LoadInstancesFromString,
RestoreInstancesFromString, SetOutOfMemoryFunction.

B.5 Version 6.21
----------------

・**Bug Fixes** - The following bugs were fixed by the 6.21 release:

・The C GetDefglobalValue macro did not have the correct number of
arguments.

・The C RtnArgCount macro did not have the correct number of arguments.

・Erroneous error generated for object pattern under some circumstances.

・The C Save macro did not have the correct number of arguments.

・The C Eval and Build functions did not have the correct number of
arguments.

・The progn$ index variable did not always return the correct value.

・The member$ function did not always return the correct value.

・C++ style comments in the code caused errors when using strict ANSI C
compilation.

・The C LoadFactsFromString function did not have the correct number of
arguments.

・Prior bug fix to the PutFactSlot C function prevented memory
associated with the fact to be garbage collected after the fact had been
retracted. The original bug is now fixed through a new API which allows
embedded programs to temporarily disable garbage collection. See section
1.4 of *The Advanced Programming Guide* for more details.

B.6 Version 6.2
---------------

**・Environments** ・It is now possible in an embedded application to
create multiple environments into which programs can be loaded. See
section 9 of *The Advanced Programming Guide* for more details

・**New Microsoft Windows and MacOS X Interfaces** ・An improved Windows
95/98/NT CLIPS interface is now available for PC compatible computers.
See section 2 of *The Interfaces Guide* for more details. An improved
MacOS X/9.0 CLIPS interface is now available for Macintosh computers.
See section 3 of *The Interfaces Guide* for more details.

・**COOL Default Behavior** ・For the purpose of role inheritance,
system defined classes behave as concrete classes. The default value for
the **create-accessor** facet is now **read-write**. For the purpose of
pattern-match inheritance, system defined classes behave as reactive
classes unless the inheriting class is **abstract**. This behavior can
be restored to CLIPS 6.1 behavior using the **set-class-defaults-mode**
function.

・**COOL Changes** - The following changes have been made to instance
behavior:

・Newly created instances are sent a **create** message.

・**New Functions and Commands** - Several new functions and commands
have been added. They are:

・**funcall**

・**get-class-defaults-mode**

・**set-class-defaults-mode**

・**string-to-field**

・**timer**

・**Command and Function Enhancements** - The following commands and
functions have been enhanced:

・**switch** The requirement of having at least two case statements has
been removed.

・**printout** The logical name **nil** now has special significance.

・**sort** Multifield as well as single-field values can now be sorted.

・**random** The integer value returned by the function can now be
restricted to a specified range of values.

・**do-for-instance** Multiple actions can be specified in the action
portion of the query (which previously required an explicit **progn**
call).

・**do-for-all-instances** Multiple actions can be specified in the
action portion of the query (which previously required an explicit
**progn** call).

・**delayed-do-for-all-instances** Multiple actions can be specified in
the action portion of the query (which previously required an explicit
**progn** call).

・**Bug Fixes** - The following bugs were fixed by the 6.20 release:

・Inner member variables in nested instance set queries could be
confused with member variables from an outer query. This would happen if
the inner member variable name shared all of its characters with the
first N characters of the outer variable name.

・Some data structures generated by object pattern matching were not
garbage collected until a run command was issued.

・Backspaces were not properly handled in the display window of the
Windows interface.

・The module name was not specified for deffunctions by the save
command.

・Garbage collection triggered by evaluating the arguments to
str-cat/sym-cat could change the value of the arguments already
retrieved.

・The PutFactSlot C function does not properly increment/decrement the
slot value that it changes.

・Defrule data structures could end up in an invalid state during a
retract if a fact/object matched two patterns in a rule and one pattern
was inside two nested not conditional elements and the other pattern was
inside one not conditional element.

・The implode$ function does not properly handle the \\ character in a
string.

・Garbage collection triggered by expression evaluation during an
assert-string call could corrupt the fact being asserted.

・Inappropriate fact syntax for the assert-string function causes a
crash.

・The evaluation error flag was not reset before calling the
AssertString C function.

・The FindDefclass C function crashes when the specified module is not
defined.

・The save command did not always properly save constructs when modules
using import/export specifications were used.

Appendix C:
Glossary
===========

This section defines some of the terminology used throughout this
manual.

========================= ======================================================================================================================================================================================================================================================================================================================================
**abstraction**           The definition of new classes to describe the common properties and behavior of a group of objects.
\                        
**action**                A function executed by a construct (such as the RHS of a rule) which typically has no return value, but performs some useful action (such as the printout action) (see section 12).
\                        
**activation**            A rule is activated if all of its conditional elements are satisfied and it has not yet fired based on a specific set of matching pattern entities that caused it to be activated. Note that a rule can be activated by more than one set of pattern entities. An activated rule that is placed on the agenda is called an activation.
\                        
**active instance**       The object responding to a message which can be referred to by ?self in the message痴 handlers.
\                        
**agenda**                A list of all rules that are presently ready to fire. It is sorted by salience values and the current conflict resolution strategy. The rule at the top of the agenda is the next rule that will fire.
\                        
**antecedent**            The LHS of a rule.
\                        
**bind**                  The action of storing a value in a variable.
\                        
**class**                 Template for describing the common properties (slots) and behavior (message-handlers) of a group of objects called instances of the class.
\                        
**class precedence list** A linear ordering of classes which describes the path of inheritance for a class.
\                        
**command**               A function executed at the top-level command prompt (such as the reset command) typically having no return value.
\                        
**command prompt**        In the interactive interface, the 鼎LIPS>・prompt which indicates that CLIPS is ready for a command to be entered.
\                        
**condition**             A conditional element.
\                        
**conditional**           A restriction on the LHS of a rule which must be satisfied in order for the rule to be applicable (also referred to as a CE).
                         
**element**              
========================= ======================================================================================================================================================================================================================================================================================================================================

======================= =========================================================================================================================================================================================================================
**conflict resolution** A method for determining the order in which rules should fire among rules with the same salience. There are seven different conflict resolution strategies: depth, breadth, simplicity, complexity, lex, mea, and random.
                       
**strategy**           
======================= =========================================================================================================================================================================================================================

====================================== ===============================================================================================================================================================================================================================================================================================================================================================================================================================
**consequent**                         The RHS of a rule.
\                                     
**constant**                           A non-varying single field value directly expressed as a series of characters.
\                                     
**constraint**                         In patterns, a constraint is a requirement that is placed on the value of a field from a fact or instance that must be satisified in order for the pattern to be satisfied. For example, the ~red constraint is satisfied if the field to which the constraint is applied is not the symbol *red*. The term constraint is also used to refer to the legal values allowed in the slots of facts and instances.
\                                     
**construct**                          A high level CLIPS abstraction used to add components to the knowledge base.
\                                     
**current focus**                      The module from which activations are selected to be fired.
\                                     
**current module**                     The module to which newly defined constructs that do not have a module specifier are added. Also is the default module for certain commands which accept as an optional argument a module name (such as list-defrules).
\                                     
**daemon**                             A message-handler which executes implicitly whenever some action is taken upon an object, such as initialization, deletion, or slot access.
\                                     
**deffunction**                        A non-overloaded function written directly in CLIPS.
\                                     
**deftemplate fact**                   A deftemplate name followed by a list of named fields (slots) and specific values used to represent a deftemplate object. Note that a deftemplate fact has no inheritance. Also called a non-ordered fact.
\                                     
**deftemplate object**                 An informal term for the entity described by a deftemplate. A deftemplate object is simply an informal term for the collections of slots (without specific values) which define a deftemplate. Deftemplate objects do not have inheritance
\                                     
**deftemplate pattern**                A list of named constraints (constrained slots). A deftemplate pattern describes the attributes and associated values of a deftemplate object. Also called a non-ordered pattern.
\                                     
**delimiter**                          A character which indicates the end of a symbol. The following characters act as delimiters: any non-printable ASCII character (including spaces, tabs, carriage returns, and line feeds), a double quote, opening and closing parenthesis ・・and ・・ an ampersand ・・ a vertical bar 倒・ a less than ・・ a semicolon ・・ and a tilde 冬・
\                                     
**dynamic binding**                    The deferral of which message-handlers will be called for a message until run-time.
\                                     
**encapsulation**                      The requirement that all manipulation of instances of user-defined classes be done with messages.
\                                     
**expression**                         A function call with arguments specified.
\                                     
**external-address**                   The address of an external data structure returned by a function (written in a language such as C or Ada) that has been integrated with CLIPS (see section 2.3.1 for more details).
\                                     
**external function**                  A function written in an external language (such as C or Ada) defined by the user or provided by CLIPS and called from within CLIPS rules.
\                                     
**facet**                              A component of a slot specification for a class, e.g. default value and cardinality.
\                                     
**fact**                               An ordered or deftemplate (non-ordered) fact. Facts are the data about which rules reaｭson and represent the current state of the world.
\                                     
**fact-address**                       A pointer to a fact obtained by binding a variable to the fact which matches a pattern on the LHS of a rule.
\                                     
**fact-identifier**                    A shorthand notation for referring to a fact. It consists of the character 吐・ followed by a dash, followed by the fact-index of the fact.
\                                     
**fact-index**                         A unique integer index used to identify a particular fact.
\                                     
**fact-list**                          The list of current facts.
\                                     
**field**                              A placeholder (named or unnamed) that has a value.
\                                     
**fire**                               A rule is said to have fired if all of its conditions are satisfied and the actions then are executed.
\                                     
**float**                              A number that begins with an optional sign followed optionally in order by zero or more digits, a decimal point, zero or more digits, and an exponent (consisting of an e or E followed by an integer). A floating point number must have at least one digit in it (not including the exponent) and must either contain a decimal point or an exponent (see section 2.3.1 for more details).
\                                     
**focus**                              As a verb, refers to changing the current focus. As a noun, refers to the current focus.
\                                     
**focus stack**                        The list of modules that have been focused upon. The module at the top of the focus stack is the current focus. When all the activations from the current focus have been fired, the current focus is removed from the focus stack and the next module on the stack becomes the current focus.
\                                     
**function**                           A piece of executable code identified by a specific name which returns a useful value or performs a useful side effect. Typically only used to refer to functions which do return a value (whereas commands and actions are used to refer to functions which do not return a value).
\                                     
**generic dispatch**                   The process whereby applicable methods are selected and executed for a particular generic function call.
\                                     
**generic function**                   A function written in CLIPS which can do different things depending on what the number and types of its arguments.
\                                     
**inference engine**                   The mechanism provided by CLIPS which automatically matches patterns against the current state of the fact-list and list of instances and determines which rules are applicable.
\                                     
**inheritance**                        The process whereby one class can be defined in terms of other class(es).
\                                     
**instance**                           An object is an instance of a class. Throughout the documentation, the term instance usually refers to objects which are instances of user-defined classes.
\                                     
**instance (of a user-defined class)** An object which can only be manipulated via messages, i.e all objects except symbols, strings, integers, floats, multifields and external-addresses.
\                                     
**instance-address**                   The address of an instance of a user-defined class (see section 2.3.1 for more details).
\                                     
**instance-name**                      A symbol enclosed within left and right brackets (see section 2.3.1 for more details). An instance-name refers to an object of the specified name which is an instance of a user-defined class.
\                                     
**instance-set**                       An ordered collection of instances of user-defined classes. Each member of an instance-set is an instance of a set of classes, where the set can be different for each member.
\                                     
**instance-set distributed action**    A user-defined expression which is evaluated for every instance-set which satisfies an instance-set query.
\                                     
**instance-set query**                 A user-defined boolean expression applied to an instance-set to see if it satisfies further user-defined criteria.
\                                     
**integer**                            A number that begins with an optional sign followed by one or more digits (see section 2.3.1 for more details).
\                                     
**LHS**                                Left-Hand Side. The set of conditional elements that must be satisfied for the acｭtions of the RHS of a rule to be performed.
\                                     
**list**                               A group of items with no implied order.
\                                     
**logical name**                       A symbolic name that is associated with an I/O source or destination.
\                                     
**message**                            The mechanism used to manipulate an object.
\                                     
**message dispatch**                   The process whereby applicable message-handlers are selected and executed for a particular message.
\                                     
**message-handler**                    An implementation of a message for a particular class of objects.
\                                     
**message-handler precedence**         The property used by the message dispatch to select between handlers when more than one is applicable to a particular message.
\                                     
**method**                             An implementation of a generic function for a particular set of argument restrictions.
\                                     
**method index**                       A shorthand notation for referring to a method with a particular set of parameter restrictions.
\                                     
**method precedence**                  The property used by the generic dispatch to select a method when more than one is applicable to a particular generic function call.
\                                     
**module**                             A workspace where a set of constructs can be grouped together such that explicit control can be maintained over restricting the access of the constructs by other modules. Also used to control the flow of execution of rules through the use of the focus command.
\                                     
**module specifier**                   A notation for specifying a module. It consists of a module name followed by two colons. When placed before a construct name, it痴 used to specify which module a newly defined construct is to be added to or to specify which construct a command will affect if that construct is not in the current module.
\                                     
**multifield**                         A sequence of unnamed placeholders each having a value.
\                                     
**multifield value**                   A sequence of zero or more single-field values.
\                                     
**non-ordered fact**                   A deftemplate fact.
\                                     
**number**                             An integer or float.
\                                     
**object**                             A symbol, a string, a floating-point or integer number, a multifield value, an external address or an instance of a user-defined class.
\                                     
**order**                              Position is significant.
\                                     
**ordered fact**                       A sequence of unnamed fields.
\                                     
**ordered pattern**                    A sequence of constraints.
\                                     
**overload**                           The process whereby a generic function can do different things depending on the types and number of its arguments, i.e. the generic function has multiple methods.
\                                     
**pattern**                            A conditional element on the LHS of a rule which is used to match facts in the fact-list.
\                                     
**pattern entity**                     An item that is capable of matching a pattern on the LHS of a rule. Facts and instances are the only types of pattern entities available.
\                                     
**pattern-matching**                   The process of matching facts or instances to patterns on the LHS of rules.
\                                     
**polymorphism**                       The ability of different objects to respond to the same message in a specialized manner.
\                                     
**primitive type object**              A symbol, string, integer, float, multifield or external-address.
\                                     
**relation**                           The first field in a fact or fact pattern. Synonomous with the associated deftemplate name.
\                                     
**RHS**                                Right-Hand Side. The actions to be performed when the LHS of a rule is satisfied.
\                                     
**rule**                               A collection of conditions and actions. When all patterns are satisfied, the actions will be taken.
\                                     
**salience**                           A priority number given to a rule. When multiple rules are ready for firing, they are fired in order of priority. The default salience is zero (0). Rules with the same salience are fired according to the current conflict resolution strategy.
\                                     
**sequence**                           An ordered list.
\                                     
**shadowed message-handler**           A message-handler that must be explicitly called by another message-handler in order to execute.
\                                     
**shadowed method**                    A method that must be explicitly called by another method in order to execute.
\                                     
**single-field value**                 One of the primitive data types: float, integer, symbol, string, external-address, instance-name, or instance-address.
\                                     
**slot**                               Named single-field or multifield. To write a slot give the field name (attribute) followed by the field value. A single-field slot has one value, while a multifield slot has zero or more values. Note that a multifield slot with one value is strictly not the same as a single field slot. However, the value of a single-field slot (or variable) may match a multifield slot (or multifield variable) that has one field.
\                                     
**slot-accessor**                      Implicit message-handlers which provide read and write access to slots of an object.
\                                     
**specificity (class)**                A class that precedes another class in a class precedence list is said to be more specific. A class is more specific than any of its superclasses.
\                                     
**specificity (rule)**                 A measure of how 都pecific・the LHS of a rule is in the pattern-matching process. The specificity is determined by the number of constants, variables, and function calls used within LHS conditional elements.
\                                     
**string**                             A set of characters that starts with double quotes (") and is followed by zero or more printable characters and ends with double quotes (see section 2.3.1 for more details).
\                                     
**subclass**                           If a class inherits from a second class, the first class is a subclass of the second class.
\                                     
**superclass**                         If a class inherits from a second class, the second class is a superclass of the first class.
\                                     
**symbol**                             Any sequence of characters that starts with any printable ASCII character and is followed by zero or more characters (see section 2.3.1 for more details).
\                                     
**top level**                          In the interactive interface, the 鼎LIPS>・prompt which indicates that CLIPS is ready for a command to be entered.
\                                     
**value**                              A single or multifield value.
\                                     
**variable**                           An symbolic location which can store a value.
====================================== ===============================================================================================================================================================================================================================================================================================================================================================================================================================

Appendix D:
Performance Considerations
==========================

This appendix explains various techniques that the user can apply to a
CLIPS program to maximize performance. Included are discussions of
pattern ordering in rules, use of deffunctions in lieu of non-overloaded
generic functions, parameter restriction ordering in generic function
methods, and various approaches to improving the speed of
message-passing and reading slots of instances.

D.1 Ordering of Patterns on the LHS
-----------------------------------

The issues which affect performance of a rule-based system are
considerably different from those which affect conventional programs.
This section discusses the single most important issue: the ordering of
patterns on the LHS of a rule.

CLIPS is a rule language based on the RETE algorithm. The RETE algorithm
was deｭsigned specifically to provide very efficient pattern-matching.
CLIPS has attempted to implement this algorithm in a manner that
combines efficient performance with powerful features. When used
properly, CLIPS can provide very reasonable performance, even on
microcomputers. However, to use CLIPS properly requires some
underｭstandｭing of how the pattern-matcher works.

Prior to initiating execution, each rule is loaded into the system and a
network of all patterns that appear on the LHS of any rule is
constructed. As facts and instances of reactive classes (referred to
collectively as pattern entities) are created, they are filtered through
the pattern network. If the pattern entities match any of the patterns
in the network, the rules associated with those patterns are partially
instantiated. When pattern entities exist that match all patterns on the
LHS of the rule, variable bindings (if any) are considered. They are
considered from the top to the bottom; i.e., the first pattern on the
LHS of a rule is conｭsidered, then the second, and so on. If the
variable bindings for all patterns are consisｭtent with the constraints
applied to the variables, the rules are activated and placed on the
agenda.

This is a very simple description of what occurs in CLIPS, but it gives
the basic idea. A number of important considerations come out of this.
Basic pattern-matching is done by filtering through the pattern network.
The time involved in doing this is fairly constant. The slow portion of
basic pattern-matching comes from comparing variable bindings across
patterns. Therefore, the single most important performance factor is the
ordering of patterns on the LHS of the rule. Unfortunately, there are no
hard and fast methods that will alｭways order the patterns properly. At
best, there seem to be three 賭uasi・methods for ordering the patterns.

1) Most specific to most general. The more wildcards or unbound
variables there are in a pattern, the lower it should go. If the rule
firing can be controlled by a single pattern, place that pattern first.
This technique often is used to provide control structure in an expert
system; e.g., some kind of 菟hase・fact. Putting this kind of pattern
first will *guarantee* that the rest of the rule will not be considered
until that pattern exists. This is most effective if the single pattern
consists only of literal constraints. If multiple patterns with variable
bindings control rule firing, arrange the patterns so the most important
variables are bound first and compared as soon as possible to the other
pattern constraints. The use of phase facts is not recommended for large
programs if they are used solely for controlling the flow of execution
(use modules instead).

2) Patterns with the lowest number of occurrences in the fact-list or
instance-list should go near the top. A large number of patterns of a
particular form in the fact-list or instance-list can cause numerous
partial instantiations of a rule that have to be 努eeded・out by
comparing the variable bindings, a slower operation.

3) Volatile patterns (ones that are retracted and asserted continuously)
should go last, particularly if the rest of the patterns are mostly
independent. Every time a pattern entity is created, it must be filtered
through the network. If a pattern entity causes a partial rule
instanｭtiation, the variable bindings must be considered. By putting
volatile patterns last, the variable bindings only will be checked if
all of the rest of the patterns already exist.

These rules are *not* independent and commonly conflict with each other.
At best, they provide some rough guidelines. Since all systems have
these characteristics in different proportions, at a glance the most
efficient manner of ordering patterns for a given system is not evident.
The best approach is to develop the rules with minimal consideration of
ordering. When the reasoning is fairly well verified, experiment with
the patterns until the optimum configuration is found.

Another performance issue is the use of multifield variables and
wildcards ($?). Although they provide a powerful capability, they must
be used very carefully. Since they can bind to zero or more fields, they
can cause multiple instantiations of a single rule. In particular, the
use of multiple multifield variables in one pattern can cause a very
large number of instantiations.

Some final notes on rule performance. Experience suggests that the user
should keep the expert system 斗ean and mean.・The list of pattern
entities should not be used as a data base for storage of exｭtraneous
information. Store and pattern-match only on that information necessary
for reasoning. Keep the pattern-matching to a minimum and be as specific
as possible. Many short, simple rules perform better than long, complex
rules and have the added benefit of being easier to understand and
maintain.

D.2 Deffunctions versus Generic Functions
-----------------------------------------

Deffunctions execute more quickly than generic function because generic
functions must first examine their arguments to determine which methods
are applicable. If a generic function has only one method, a deffunction
probably would be better. Care should be taken when determining if a
particular function truly needs to be overloaded. In addition, if
recompiling and relinking CLIPS is not prohibitive, user-defined
external functions are even more efficient than deffunctions. This is
because deffunction are interpreted whereas external functions are
directly executed. For more details, see sections 7 and 8.2.

D.3 Ordering of Method Parameter Restrictions
---------------------------------------------

When the generic dispatch examines a generic function痴 method to
determine if it is applicable to a particular set of arguments, it
examines that method痴 parameter restrictions from left to right. The
programmer can take advantage of this by placing parameter restrictions
which are less frequently satisfied than others first in the list. Thus,
the generic dispatch can conclude as quickly as possible when a method
is not applicable to a generic function call. If a group of restrictions
are all equally likely to be satisfied, placing the simpler restrictions
first, such as those without queries, will also allow the generic
dispatch to conclude more quickly for a method that is not applicable.
For more details, see section 8.4.3.

D.4 Instance-Addresses versus Instance-Names
--------------------------------------------

COOL allows instances of user-defined classes to be referenced either by
address or by name in functions which manipulate instances, such as
message-passing with the **send** function. However, when an instance is
referenced by name, CLIPS must perform an internal lookup to find the
instance-address anyway. If the same instance is going to be manipulated
many times, it might be advantageous to store the instance-address and
use that as a reference. This will allow CLIPS to always go directly to
the instance. For more details, see sections 2.4.2 and 12.16.4.6.

D.5 Reading Instance Slots Directly
-----------------------------------

Normally, message-passing must be used to read or set a slot of an
instance. However, slots can be read directly within instance-set
queries and message-handlers, and they can be set directly within
message-handlers. Accessing slots directly is significantly faster than
message-passing. Unless message-passing is required (because of slot
daemons), direct access should be used when allowed. For more details,
see sections 9.4.2, 9.4.3, 9.4.4, 9.6.3, 9.6.4 and 9.7.3.

Appendix E:
CLIPS Warning Messages
======================

CLIPS typically will display two kinds of warning messages: those
associated with executing constructs and those associated with loading
constructs. This appendix describes some of the more common warning
messages and what they mean. Each message begins with a unique
identifier enclosed in brackets followed by the keyword **WARNING**; the
messages are listed here in alphabetic order according to the
identifier.

[CONSCOMP1] WARNING: Base file name exceeds 3 characters.

This may cause files to be overwritten if file name length

is limited on your platform.

The constructs-to-c command generates file names using the file name
prefix supplied as an argument. If this base file name is longer than
three characters, then the possibility exists that files may be
overwritten if file name length is limited on your platform.

[CONSCOMP2] WARNING: Array name <arrayName> exceeds 6 characters in
length. This variable may be indistinguishable from another by the
linker.

The constructs-to-c command generates arrays for storing data
structures. If the SHORT_LINK_NAMES compiler flag is enabled, then this
warning message is displayed if generated array names exceed six
characters in length.

[CSTRCPSR1] WARNING: Redefining <constructType>: <constructName>

This indicates that a previously defined construct of the specified type
has been redefined.

[CSTRNBIN1] WARNING: Constraints are not saved with a binary image when
dynamic constraint checking is disabled

or

[CSTRNCMP1] WARNING: Constraints are not saved with a constructs-to-c
image when dynamic constraint checking is disabled

These warnings occur when dynamic constraint checking is disabled and
the **constructs-to-c** or **bsave** commands are executed. Constraints
attached to deftemplate and defclass slots will not be saved with the
runtime or binary image in these cases since it is assumed that dynamic
constraint checking is not required. Enable dynamic constraint checking
with the **set-dynamic-constraint-checking** function before calling
**constructs-to-c** or **bsave** in order to include constraints in the
runtime or binary image.

[DFFNXFUN1] WARNING: Deffunction <name> only partially deleted due to
usage by other constructs.

During a clear or deletion of all deffunctions, only the actions of a
deffunction were deleted because another construct which also could not
be deleted referenced the deffunction.

Example:

CLIPS>

(deffunction foo ()

(printout t "Hi there!" crlf))

CLIPS>

(deffunction bar ()

(foo)

(undeffunction \*))

CLIPS> (bar)

[GENRCBIN1] WARNING: COOL not installed! User-defined class in method
restriction substituted with OBJECT.

This warning occurs when a generic function method restricted by
defclasses is loaded using the **bload** command into a CLIPS
configuration where the object language is not enabled. The restriction
containing the defclass will match any of the primitive types.

[OBJBIN1] WARNING: Around message-handlers are not supported in this
environment.

This warning occurs when an **around** message-handler is loaded using
the **bload** command into a CLIPS configuration not supporting
imperative message-handlers.

[OBJBIN2] WARNING: Before and after message-handlers are not supported
in this environment.

This warning occurs when a **before** or an **after** message-handler is
loaded using the **bload** command into a CLIPS configuration not
supporting auxiliary message-handlers.

Appendix F:
CLIPS Error Messages
====================

CLIPS typically will display two kinds of error messages: those
associated with executing constructs and those associated with loading
constructs. This appendix describes some of the more common error
messages and what they mean. Each message begins with a unique
identifier enclosed in brackets; the messages are listed here in
alphabetic order according to the identifier.

[AGENDA1] Salience value must be an integer value.

Salience requires a integer argument and will otherwise result in this
error message.

Example:

CLIPS> (defrule error (declare (salience a)) =>)

[AGENDA2] Salience value out of range <min> to <max>

The range of allowed salience has an explicit limit; this error message
will result if the value is out of that range.

Example:

CLIPS> (defrule error (declare (salience 20000)) =>)

[AGENDA3] This error occurred while evaluating the salience [for rule
<name>]

When an error results from evaluating a salience value for a rule, this
error message is given.

[ANALYSIS1] Duplicate pattern-address <variable name> found in CE <CE
number>.

This message occurs when two facts or instances are bound to the same
pattern-address variable.

Example:

CLIPS> (defrule error ?f <- (a) ?f <- (b) =>)

[ANALYSIS2] Pattern-address <variable name> used in CE #2 was previously
bound within a pattern CE.

A variable first bound within a pattern cannot be later bound to a
fact-address.

Example:

CLIPS> (defrule error (a ?f) ?f <- (b) =>)

[ANALYSIS3] Variable <variable name> is used as both a single and
multifield variable.

Variables on the LHS of a rule cannot be bound to both single and
multifield variables.

Example:

CLIPS> (defrule error (a ?x $?x) =>)

[ANALYSIS4] Variable <variable name> [found in the expression
<expression>]

was referenced in CE <CE number> <field or slot identifier> before being
defined

A variable cannot be referenced before it is defined and, thus, results
in this error message.

Example:

CLIPS> (defrule foo (a ~?x) =>)

[ARGACCES1] Function <name> expected at least <minimum> and no more than
<maximum> argument(s)

This error occurs when a function receives less than the minimum number
or more than the maximum number of arguｭment(s) expected.

[ARGACCES2] Function <function-name> was unable to open file <file-name>

This error occurs when the specified function cannot open a file.

[ARGACCES3] Function <name1> received a request from function <name2>
for argument #<number> which is non-existent

This error occurs when a function is passed fewer arguments than were
expected.

[ARGACCES4] Function <name> expected exactly <number> argument(s)

This error occurs when a function that expects a precise number of
argument(s) reｭceives an incorrect number of arguments.

[ARGACCES4] Function <name> expected at least <number> argument(s)

This error occurs when a function does not receive the minimum number of
arguｭment(s) that it expected.

[ARGACCES4] Function <name> expected no more than <number> argument(s)

This error occurs when a function receives more than the maximum number
of arguｭment(s) expected.

[ARGACCES5] Function <name> expected argument #<number> to be of type
<data-type>

This error occurs when a function is passed the wrong type of argument.

[ARGACCES6] Function <name1> received a request from function <name2>
for argument #<number> which is not of type <data-type>

This error occurs when a function requests from another function the
wrong type of arｭgument, typically a **string** or **symbol**, when
expecting a **number** or vice versa.

[BLOAD1] Cannot load <construct type> construct with binary load in
effect.

If the bload command was used to load in a binary image, then the named
construct cannot be entered until a clear command has been performed to
remove the binary image.

[BLOAD2] File <file-name> is not a binary construct file

This error occurs when the bload command is used to load a file that was
not creｭated with the bsave command.

[BLOAD3] File <file-name> is an incompatible binary construct file

This error occurs when the bload command is used to load a file that was
creｭated with the bsave command using a different version of CLIPS.

[BLOAD4] The CLIPS environment could not be cleared.

Binary load cannot continue.

A binary load cannot be performed unless the current CLIPS environment
can be cleared.

| [BLOAD5] Some constructs are still in use by the current binary image:
| <construct-name 1>
| <construct-name 2>
| ...
| <construct-name N>

Binary <operation> cannot continue.

This error occurs when the current binary image cannot be cleared
because some constructs are still being used. The <operation> in
progress may either be a binary load or a binary clear.

| [BLOAD6] The following undefined functions are referenced by this
  binary image:
| <function-name 1>
| <function-name 2>
| ...
| <function-name N>

This error occurs when a binary image is loaded that calls functions
which were available in the CLIPS executable that originally created the
binary image, but which are not available in the CLIPS executable that
is loading the binary image.

[BSAVE1] Cannot perform a binary save while a binary load is in effect.

The bsave command does not work when a binary image is loaded.

[CLASSEXM1] Inherited slot <slot-name> from class <slot-name> is not
valid for function slot-publicp

This error message occurs when the function **slot-publicp** is given an
inherited slot. This function can only be used on slots defined in the
given class.

Example:

CLIPS>

(defclass FOO (is-a USER)

(slot woz (visibility private)))

CLIPS>

(defclass BAR (is-a FOO))

CLIPS> (slot-publicp BAR woz)

[CLASSFUN1] Unable to find class <class name> in function <function
name>.

This error message occurs when a function is given a non-existent class
name.

Example:

CLIPS> (class-slots FOO)

[CLASSFUN2] Maximum number of simultaneous class hierarchy traversals
exceeded <number>.

This error is usually caused by too many simultaneously active
instance-set queries, e.g., **do-for-all-instances**. The direct or
indirect nesting of instance-set query functions is limited in the
following way:

Ci is the number of members in an instance-set for the ith nested
instance-set query function.

N is the number of nested instance-set query functions.

======== ================================
|image7| <= 128 (the default upper limit)
======== ================================

Example:

CLIPS>

(deffunction my-func ()

(do-for-instance ((?a USER) (?b USER) (?c USER)) TRUE

(printout t ?a " " ?b " " ?c crlf))

; The sum here is C1 = 3 which is OK.

CLIPS>

(do-for-all-instances ((?a OBJECT) (?b OBJECT)) TRUE

(my-func))

; The sum here is C1 + C2 = 2 + 3 = 5 which is OK.

The default upper limit of 128 should be sufficient for most if not all
applications. However, the limit may be increased by editing the header
file OBJECT.H and recompiling CLIPS.

[CLASSPSR1] An abstract class cannot be reactive.

Only concrete classes can be reactive.

Example:

CLIPS>

(defclass FOO (is-a USER)

(role abstract)

(pattern-match reactive))

[CLASSPSR2] Cannot redefine a predefined system class.

Predefined system classes cannot be modified by the user.

Example:

CLIPS> (defclass STRING (is-a NUMBER))

[CLASSPSR3] <name> class cannot be redefined while outstanding
references to it still exist.

This error occurs when an attempt to redefine a class is made under one
or both of the following two circumstances:

1) The class (or any of its subclasses) has instances.

2) The class (or any of its subclasses) appear in the parameter
restrictions of any generic function method.

Before the class can be redefined, all such instances and methods must
be deleted.

Example:

CLIPS> (defclass A (is-a USER))

CLIPS> (defmethod foo ((?a A LEXEME)))

CLIPS> (defclass A (is-a OBJECT)))

[CLASSPSR4] Class <attribute> already declared.

Only one specification of a class attribute is allowed.

Example:

CLIPS>

(defclass A (is-a USER)

(role abstract)

(role concrete))

[CLSLTPSR1] Duplicate slots not allowed.

Slots in a defclass must be unique.

Example:

CLIPS>

(defclass A (is-a USER)

(slot foo)

(slot foo))

[CLSLTPSR2] <name> facet already specified.

Only one occurrence of a facet per slot is allowed.

Example:

CLIPS>

(defclass A (is-a USER)

(slot foo (access read-only)

(access read-write)))

[CLSLTPSR3] Cardinality facet can only be used with multifield slots

Single-field slots by definition have a cardinality of one.

Example:

CLIPS>

(defclass A (is-a USER)

(slot foo (cardinality 3 5)))

[CLSLTPSR4] read-only slots must have a default value

Since slots cannot be unbound and **read-only** slots cannot be set
after initial creation of the instance, **read-only** slots must have a
default value.

Example:

CLIPS>

(defclass A (is-a USER)

(slot foo (access read-only)

(default ?NONE)))

[CLSLTPSR5] read-only slots cannot have a write accessor

Since **read-only** slots cannot be changed after initializationof the
instance, a **write** accessor (**put-** message-handler) is not
allowed.

Example:

CLIPS>

(defclass A (is-a USER)

(slot foo (access read-only)

(create-accessor write)))

[CLSLTPSR6] no-inherit slots cannot also be public

**no-inherit** slots are by definition not accessible to subclasses and
thus only visible to the parent class.

Example:

CLIPS>

(defclass A (is-a USER)

(slot foo (propagation no-inherit)

(visibility public)))

[COMMLINE1] Expected a '(', constant, or global variable

This message occurs when a top-level command does not begin with a '(',
constant, or global variable.

Example:

CLIPS> )

[COMMLINE2] Expected a command.

This message occurs when a top-level command is not a symbol.

Example:

CLIPS> ("facts"

[CONSCOMP1] Invalid file name <fileName> contains '.'

A '.' cannot be used in the file name prefix that is passed to the
constructs-to-c command since this prefix is used to generate file names
and some operating systems do not allow more than one '.' to appear in a
file name.

[CONSTRCT1] Some constructs are still in use. Clear cannot continue.

This error occurs when the clear command is issued when a construct is
in use (such as a rule that is firing).

[CSTRCPSR1] Expected the beginning of a construct.

This error occurs when the load command expects a left parenthesis
followed a construct type and these token types are not found.

[CSTRCPSR2] Missing name for <construct-type> construct

This error occurs when the name is missing for a construct that requires
a name.

Example:

CLIPS> (defgeneric ())

[CSTRCPSR3] Cannot define <construct-type> <construct-name> because of
an import/export conflict.

or

[CSTRCPSR3] Cannot define defmodule <defmodule-name> because of an
import/export conflict cause by the <construct-type> <construct-name>.

A construct cannot be defined if defining the construct would allow two
different definitions of the same construct type and name to both be
visible to any module.

Example:

CLIPS> (defmodule MAIN (export ?ALL))

CLIPS> (deftemplate MAIN::foo)

CLIPS> (defmodule BAR (import MAIN ?ALL))

CLIPS> (deftemplate BAR::foo (slot x))

[CSTRCPSR4] Cannot redefine <construct-type> <construct-name> while it
is in use.

A construct cannot be redefined while it is being used by another
construct or other data structure (such as a fact or instance).

Example:

CLIPS> (clear)

CLIPS> (deftemplate bar)

CLIPS> (assert (bar))

<Fact-0>

CLIPS> (deftemplate (bar (slot x)))

[CSTRNCHK1] *Message Varies*

This error ID covers a range of messages indicating a type, value,
range, or cardinality violation.

Example:

CLIPS> (deftemplate foo (slot x (type SYMBOL)))

CLIPS> (assert (foo (x 3)))

[CSTRNPSR1] The <first attribute name> attribute conflicts with the
<second attribute name> attribute.

This error message occurs when two slot attributes conflict.

Example:

CLIPS> (deftemplate foo (slot x (type SYMBOL) (range 0 2)))

[CSTRNPSR2] Minimum <attribute> value must be less than

or equal to the maximum <attribute> value.

The minimum attribute value for the range and cardinality attributes
must be less than or equal to the maximum attribute value for the
attribute.

Example:

CLIPS> (deftemplate foo (slot x (range 8 1)))

[CSTRNPSR3] The <first attribute name> attribute cannot be used in
conjunction with the <second attribute name> attribute.

The use of some slot attributes excludes the use of other slot
attributes.

Example:

CLIPS> (deftemplate foo (slot x (allowed-values a)

(allowed-symbols b)))

[CSTRNPSR4] Value does not match the expected type for the <attribute
name> attribute.

The arguments to an attribute must match the type expected for that
attribute (e.g. integers must be used for the allowed-integers
attribute).

Example:

CLIPS> (deftemplate example (slot x (allowed-integers 3.0)))

[CSTRNPSR5] The cardinality attribute can only be used with multifield
slots.

The cardinality attribute can only be used for slots defined with the
multislot keyword.

Example:

CLIPS> (deftemplate foo (slot x (cardinality 1 1)))

[CSTRNPSR6] Minimum cardinality value must be greater than or equal to
zero.

A multislot with no value has a cardinality of 0. It is not possible to
have a lower cardinality.

Example:

CLIPS> (deftemplate foo (multislot x (cardinality -3 1)))

[DEFAULT1] The default value for a single field slot must be a single
field value.

This error occurs when the default or default-dynamic attribute for a
single-field slot does not contain a single value or an expression
returning a single value.

Example:

CLIPS> (deftemplate error (slot x (default)))

[DFFNXPSR1] Deffunctions are not allowed to replace constructs.

A deffunction cannot have the same name as any construct.

Example:

CLIPS> (deffunction defgeneric ())

[DFFNXPSR2] Deffunctions are not allowed to replace external functions.

A deffunction cannot have the same name as any system or user-defined
external function.

Example:

CLIPS> (deffunction + ())

[DFFNXPSR3] Deffunctions are not allowed to replace generic functions.

A deffunction cannot have the same name as any generic function.

Example:

CLIPS> (defgeneric foo)

CLIPS> (deffunction foo ())

[DFFNXPSR4] Deffunction <name> may not be redefined while it is
executing.

A deffunction can be loaded at any time except when a deffunction of the
same name is already executing.

Example:

CLIPS>

(deffunction foo ()

(build "(deffunction foo ())"))

CLIPS> (foo)

**[DFFNXPSR5] Defgeneric <name> imported from module <module name>
conflicts with this deffunction.**

A deffunction cannot have the same name as any generic function imported
from another module.

Example:

CLIPS> (defmodule MAIN (export ?ALL))

CLIPS> (defmethod foo ())

CLIPS> (defmodule FOO (import MAIN ?ALL))

CLIPS> (deffunction foo)

[DRIVE1] This error occurred in the join network

Problem resides in join #<pattern-number> in rule(s):

<problem-rules>+

This error pinpoints other evaluation errors associated with evaluating
an expression within the join network. The specific pattern of the
problem rules is identified.

[EMATHFUN1] Domain error for <function-name> function

This error occurs when an argument passed to a math function is not in
the domain of values for which a return value exists.

[EMATHFUN2] Argument overflow for <function-name> function

This error occurs when an argument to an extended math function would
cause a numeric overflow.

[EMATHFUN3] Singularity at asymptote in <function-name> function

This error occurs when an argument to a trigonometric math function
would cause a singularity.

[EVALUATN1] Variable <name> is unbound

This error occurs when a local variable not set by a previous call to
**bind** is accessed at the top level.

Example:

CLIPS> (progn ?error)

[EVALUATN2] No function, generic function or deffunction of name <name>
exists for external call.

IThis error occurs only when an invalid function name is passed to the
external C access routine CLIPSFunctionCall.

[EXPRNPSR1] A function name must be a symbol

In the following example, '**~**' is recognized by CLIPS as an operator,
not a function:

Example:

CLIPS> (+ (~ 3 4) 4)

[EXPRNPSR2] Expected a constant, variable, or expression

In the following example, '**~**' is an operator and is illegal as an
argument to a function call:

Example:

CLIPS> (<= ~ 4)

[EXPRNPSR3] Missing function declaration for <name>

CLIPS does not recognize <name> as a declared function and gives this
error message.

Example:

CLIPS> (xyz)

[EXPRNPSR4] $ Sequence operator not a valid argument for <name>.

The sequence expansion operator cannot be used with certain functions.

Example:

CLIPS> (set-sequence-operator-recognition TRUE)

FALSE

CLIPS> (defrule foo (x $?y) => (assert (x1 $?y)))

[FACTMCH1] This error occurred in the pattern network

Currently active fact: <newly assert fact>

Problem resides in slot <slot name>

Of pattern #<pattern-number> in rule(s):

<problem-rules>+

This error pinpoints other evaluation errors associated with evaluating
an expression within the pattern network. The specific pattern and field
of the problem rules are identified.

[FACTMNGR1] Facts may not be retracted during pattern-matching

or

[FACTMNGR2] Facts may not be retracted during pattern-matching

Functions used on the LHS of a rule should not have side effects (such
as the creation of a new instance or fact).

Example:

CLIPS>

(defrule error

(test (assert (blah)))

=>)

CLIPS> (reset)

[FACTRHS1] Template <name> does not exist for assert.

This error occurs when an assert is attempted for a deftemplate which
does not exist in a runtime or active **bload** image. In other
situations, CLIPS will create an implied deftemplate if one does not
already exist.

Example:

CLIPS> (clear)

CLIPS> (bsave error.bin)

TRUE

CLIPS> (bload error.bin)

TRUE

CLIPS> (assert (error))

[GENRCCOM1] No such generic function <name> in function undefmethod.

This error occurs when the generic function name passed to the
undefmethod function does not exist.

Example:

CLIPS> (clear)

CLIPS> (undefmethod yak 3)

[GENRCCOM2] Expected a valid method index in function undefmethod.

This error occurs when an invalid method index is passed to undefmethod
(e.g. a negative integer or a symbol other than \*).

Example:

CLIPS> (defmethod foo ())

CLIPS> (undefmethod foo a))

[GENRCCOM3] Incomplete method specification for deletion.

It is illegal to specify a non-wildcard method index when a wildcard is
given for the generic function in the function **undefmethod**.

Example:

CLIPS> (undefmethod \* 1)

[GENRCCOM4] Cannot remove implicit system function method for generic
function <name>.

A method corresponding to a system defined function cannot be deleted.

Example:

CLIPS> (defmethod integer ((?x SYMBOL)) 0)

CLIPS> (list-defmethods integer)

integer #SYS1 (NUMBER)

integer #2 (SYMBOL)

For a total of 2 methods.

CLIPS> (undefmethod integer 1)

[GENRCEXE1] No applicable methods for <name>.

The generic function call arguments do not satisfy any method痴
parameter restrictions.

Example:

CLIPS> (defmethod foo ())

CLIPS> (foo 1 2)

[GENRCEXE2] Shadowed methods not applicable in current context.

No shadowed method is available when the function **call-next-method**
is called.

Example:

CLIPS> (call-next-method)

[GENRCEXE3] Unable to determine class of <value> in generic function
<name>.

The class or type of a generic function argument could not be determined
for comparison to a method type restriction.

Example:

CLIPS> (defmethod foo ((?a INTEGER)))

CLIPS> (foo [bogus-instance])

[GENRCEXE4] Generic function <name> method #<index> is not applicable to
the given arguments.

This error occurs when **call-specific-method** is called with an
inappropriate set of arguments for the specified method.

Example:

CLIPS> (defmethod foo ())

CLIPS> (call-specific-method foo 1 abc)

[GENRCFUN1] Defgeneric <name> cannot be modified while one of its
methods is executing.

Defgenerics can稚 be redefined while one of their methods is currently
executing.

Example:

CLIPS> (defgeneric foo)

CLIPS> (defmethod foo () (build "(defgeneric foo)"))

CLIPS> (foo)

[GENRCFUN2] Unable to find method <name> #<index> in function <name>.

No generic function method of the specified index could be found by the
named function.

Example:

CLIPS> (defmethod foo 1 ())

CLIPS> (ppdefmethod foo 2)

[GENRCFUN3] Unable to find generic function <name> in function <name>.

No generic function method of the specified index could be found by the
named function.

Example:

CLIPS> (preview-generic balh)

[GENRCPSR1] Expected ')' to complete defgeneric.

A right parenthesis completes the definition of a generic function
header.

Example:

CLIPS> (defgeneric foo ())

[GENRCPSR2] New method #<index1> would be indistinguishable from method
#<index2>.

An explicit index has been specified for a new method that does not
match that of an older method which has identical parameter
restrictions.

Example:

CLIPS> (defmethod foo 1 ((?a INTEGER)))

CLIPS> (defmethod foo 2 ((?a INTEGER)))

[GENRCPSR3] Defgenerics are not allowed to replace constructs.

A generic function cannot have the same name as any construct.

**[GENRCPSR4] Deffunction <name> imported from module <module name>
conflicts with this defgeneric.**

A deffunction cannot have the same name as any generic function imported
from another module.

Example:

CLIPS> (defmodule MAIN (export ?ALL))

CLIPS> (deffunction foo ())

CLIPS> (defmodule FOO (import MAIN ?ALL))

CLIPS> (defmethod foo)

[GENRCPSR5] Generic functions are not allowed to replace deffunctions.

A generic function cannot have the same name as any deffunction.

[GENRCPSR6] Method index out of range.

A method index cannot be greater than the maximum value of an integer or
less than 1.

Example:

CLIPS> (defmethod foo 0 ())

[GENRCPSR7] Expected a '(' to begin method parameter restrictions.

A left parenthesis must begin a parameter restriction list for a method.

Example:

CLIPS> (defmethod foo)

[GENRCPSR8] Expected a variable for parameter specification.

A method parameter with restrictions must be a variable.

Example:

CLIPS> (defmethod foo ((abc)))

[GENRCPSR9] Expected a variable or '(' for parameter specification.

A method parameter must be a variable with or without restrictions.

Example:

CLIPS> (defmethod foo (abc))

[GENRCPSR10] Query must be last in parameter restriction.

A query parameter restriction must follow a type parameter restriction
(if any).

Example:

CLIPS> (defmethod foo ((?a (< ?a 1) INTEGER)))

[GENRCPSR11] Duplicate classes/types not allowed in parameter
restriction.

A method type parameter restriction may have only a single occurrence of
a particular class.

Example:

CLIPS> (defmethod foo ((?a INTEGER INTEGER)))

[GENRCPSR12] Binds are not allowed in query expressions.

Binding new variables in a method query parameter restriction is
illegal.

Example:

CLIPS> (defmethod foo ((?a (bind ?b 1))))

[GENRCPSR13] Expected a valid class/type name or query.

Method parameter restrictions consist of zero or more class names and an
optional query expression.

Example:

CLIPS> (defmethod foo ((?a 34)))

[GENRCPSR14] Unknown class/type in method.

Classes in method type parameter restrictions must already be defined.

Example:

CLIPS> (defmethod foo ((?a bogus-class)))

[GENRCPSR15] <name> class is redundant.

All classes in a method type parameter restriction should be unrelated.

Example:

CLIPS> (defmethod foo ((?a INTEGER NUMBER)))

[GENRCPSR16] The system function <name> cannot be overloaded.

Some system functions canot be overloaded.

Example:

CLIPS> (defmethod if ())

[GENRCPSR17] Cannot replace the implicit system method #<integer>.

A system function can not be overloaded with a method that has the exact
number and types of arguments.

Example:

CLIPS> (defmethod integer ((?x NUMBER)) (\* 2 ?x))

[GLOBLDEF1] Global variable <variable name> is unbound.

A global variable must be defined before it can be accessed at the
command prompt or elsewhere.

Example:

CLIPS> (clear)

CLIPS> ?*x\*

[GLOBLPSR1] Global variable <variable name> was referenced, but is not
defined.

A global variable must be defined before it can be accessed at the
command prompt or elsewhere.

Example:

CLIPS> (clear)

CLIPS> ?*x\*

[INCRRSET1] The incremental reset behavior cannot be changed with rules
loaded.

The incremental reset behaviour can only be changed when there are no
currently defined rules.

[INHERPSR1] A class may not have itself as a superclass.

A class may not inherit from itself.

Example:

CLIPS> (defclass A (is-a A))

[INHERPSR2] A class may inherit from a superclass only once.

All direct superclasses of a class must be unique.

Example:

CLIPS> (defclass A (is-a USER USER))

[INHERPSR3] A class must be defined after all its superclasses.

Subclasses must be defined last.

Example:

CLIPS> (defclass B (is-a A))

[INHERPSR4] Must have at least one superclass.

All user-defined classes must have at least one direct superclass.

Example:

CLIPS> (defclass A (is-a))

[INHERPSR5] Partial precedence list formed: <classa> <classb> ・<classc>

Precedence loop in superclasses: <class1> <class2> ・<classn> <class1>

No class precedence list satisfies the rules specified in section
9.3.1.1 for the given direct superclass list. The message shows a
conflict for <class1> because the precedence implies that <class1> must
both precede and succeed <class2> through <classn>. The full loop can be
used to help identify which particular classes are causing the problem.
This loop is not necessarily the only loop in the precedence list; it is
the first one detected. The part of the precedence list which was
successfully formed is also listed.

Example:

CLIPS> (defclass A (is-a MULTIFIELD FLOAT SYMBOL))

CLIPS> (defclass B (is-a SYMBOL FLOAT))

CLIPS> (defclass C (is-a A B))

[INHERPSR6] A user-defined class cannot be a subclass of <name>.

The INSTANCE, INSTANCE-NAME, and INSTANCE-ADDRESS classes cannot have
any subclasses.

Example:

CLIPS> (defclass A (is-a INSTANCE))

[INSCOM1] Undefined type in function <name>.

The evaluation of an expression yielded something other than a
recognized class or primitive type.

[INSFILE1] Function <function-name> could not completely process file
<name>.

This error occurs when an instance definition is improperly formed in
the input file for the **load-instances**, **restore-instances**, or
**bload-instances** command.

Example:

CLIPS> (load-instances bogus.txt)

[INSFILE2] <file-name> file is not a binary instances file.

or

[INSFILE3] <file-name> file is not a compatible binary instances file.

This error occurs when bload-instances attempts to load a file that was
not created with bsave-instances or when the file being loaded was
created by a different version of CLIPS.

Example:

CLIPS> (reset)

CLIPS> (save-instances foo.ins)

1

CLIPS> (bload-instances foo.ins)

[INSFILE4] Function bload-instances unable to load instance
<instance-name>.

This error occurs when an instance specification in the input file for
the **bload-instances** command could not be created.

Example:

CLIPS> (defclass A (is-a USER) (role concrete))

CLIPS> (make-instance of A)

[gen1]

CLIPS> (bsave-instances foo.bin)

1

CLIPS> (clear)

CLIPS> (defclass A (is-a USER))

CLIPS> (bload-instances foo.bin)

[INSFUN1] Expected a valid instance in function <name>.

The named function expected an instance-name or address as an argument.

Example:

CLIPS> (initialize-instance 34)

[INSFUN2] No such instance <name> in function <name>.

This error occurs when the named function cannot find the specified
instance.

Example:

CLIPS> (instance-address [bogus-instance])

[INSFUN3] No such slot <name> in function <name>.

This error occurs when the named function cannot find the specified slot
in an instance or class.

Example:

CLIPS> (defclass A (is-a USER))

CLIPS> (slot-writablep A b)

[INSFUN4] Invalid instance-address in function <name>.

This error occurs when an attempt is made to use the address of a
deleted instance.

Example:

CLIPS> (defclass A (is-a USER))

CLIPS> (make-instance a of A)

[a]

CLIPS> (defglobal ?*x\* = (instance-address a))

CLIPS> (make-instance a of A)

[a]

CLIPS> (class ?*x*)

[INSFUN5] Cannot modify reactive instance slots while pattern-matching
is in process.

CLIPS does not allow reactive instance slots to be changed while
pattern-matching is taking place. Functions used on the LHS of a rule
should not have side effects (such as the changing slot values).

Example:

CLIPS>

(defclass FOO (is-a USER)

(role concrete)

(pattern-match reactive)

(slot x (create-accessor read-write)))

CLIPS> (make-instance x of FOO)

[x]

CLIPS> (defrule BAR (x) (test (send [x] put-x 3)) =>)

CLIPS> (assert (x))

[INSFUN6] Unable to pattern-match on shared slot <name> in class <name>.

This error occurs when the number of simultaneous class hierarchy
traversals is exceeded while pattern-matching on a shared slot. See the
related error message [CLASSFUN2] for more details.

[INSFUN7] <multifield-value> illegal for single-field slot <name> of
instance <name> found in <function-call or message-handler>.

Single-field slots in an instance can hold only one atomic value.

Example:

CLIPS> (set-static-constraint-checking FALSE)

TRUE

CLIPS>

(defclass FOO (is-a USER)

(role concrete)

(slot foo))

CLIPS>

(defmessage-handler FOO error ()

(bind ?self:foo 1 2 3))

CLIPS> (make-instance foo of FOO)

[foo]

CLIPS> (send [foo] error)

[INSFUN8] Void function illegal value for slot <name> of instance <name>
found in <function-call or message-handler>.

Only functions which have a return value can be used to generate values
for an instance slot.

Example:

CLIPS> (set-static-constraint-checking FALSE)

TRUE

CLIPS>

(defclass FOO (is-a USER)

(role concrete)

(slot foo))

CLIPS>

(defmessage-handler FOO error ()

(bind ?self:foo (instances)))

CLIPS> (make-instance foo of FOO)

[foo]

CLIPS> (send [foo] error)

[INSMNGR1] Expected a valid name for new instance.

**make-instance** expects a symbol or an instance-name for the name of a
new instance.

Example:

CLIPS> (make-instance 34 of A)

[INSMNGR2] Expected a valid class name for new instance.

**make-instance** expects a symbol for the class of a new instance.

Example:

CLIPS> (make-instance a of 34)

[INSMNGR3] Cannot create instances of abstract class <name>.

Direct instances of abstract classes, such as the predefined system
classes, are illegal.

Example:

CLIPS> (make-instance [foo] of USER)

[INSMGNR4] The instance <name> has a slot-value which depends on the
instance definition.

The initialization of an instance is recursive in that a slot-override
or default-value tries to create or reinitialize the same instance.

Example:

CLIPS>

(defclass A (is-a USER)

(slot foo))

CLIPS>

(make-instance a of A (foo (make-instance a of A)))

[INSMNGR5] Unable to delete old instance <name>.

**make-instance** will attempt to delete an old instance of the same
name if it exists. This error occurs if that deletion fails.

Example:

CLIPS> (defclass A (is-a USER))

CLIPS>

(defmessage-handler A delete around ()

(if (neq (instance-name ?self) [a]) then

(call-next-handler)))

CLIPS> (make-instance a of A)

CLIPS> (make-instance a of A)

[INSMNGR6] Cannot delete instance <name> during initialization.

The evaluation of a slot-override in **make-instance** or
**initialize-instance** attempted to delete the instance.

Example:

CLIPS>

(defclass A (is-a USER)

(slot foo))

CLIPS>

(defmessage-handler A put-foo after ($?any)

(delete-instance))

CLIPS> (make-instance a of A (foo 2))

[INSMNGR7] Instance <name> is already being initialized.

An instance cannot be reinitialized during initialization.

Example:

CLIPS> (defclass A (is-a USER))

CLIPS> (make-instance a of A)

CLIPS>

(defmessage-handler A init after ()

(initialize-instance ?self))

CLIPS> (initialize-instance a)

CLIPS> (send [a] try)

[INSMNGR8] An error occurred during the initialization of instance
<name>.

This message is displayed when an evaluation error occurs while the
**init** message is executing for an instance.

[INSMNGR9] Expected a valid slot name for slot-override.

**make-instance** and **initialize-instance** expect symbols for slot
names.

Example:

CLIPS> (defclass A (is-a USER))

CLIPS> (make-instance a of A (34 override-value))

[INSMNGR10] Cannot create instances of reactive classes while
pattern-matching is in process.

CLIPS does not allow instances of reactive classes to be created while
pattern-matching is taking place. Functions used on the LHS of a rule
should not have side effects (such as the creation of a new instance or
fact).

Example:

CLIPS> (defclass FOO (is-a USER) (role concrete) (pattern-match
reactive))

CLIPS> (defrule BAR (x) (test (make-instance of FOO)) =>)

CLIPS> (assert (x))

[INSMNGR11] Invalid module specifier in new instance name.

This error occurs when the module specifier in the instance-name is
illegal (such as an undefined module name).

Example:

CLIPS> (defclass FOO (is-a USER) (role concrete))

CLIPS> (make-instance BOGUS::x of FOO)

[INSMNGR12] Cannot delete instances of reactive classes while
pattern-matching is in process.

CLIPS does not allow instances of reactive classes to be deleted while
pattern-matching is taking place. Functions used on the LHS of a rule
should not have side effects (such as the deletion of a new instance or
the retraction of a fact).

Example:

CLIPS> (defclass FOO (is-a USER) (role concrete) (pattern-match
reactive))

CLIPS> (make-instance x of FOO)

[x]

CLIPS> (defrule BAR (x) (test (send [x] delete)) =>)

CLIPS> (assert (x))

[INSMNGR13] Slot <slot-name> does not exist in instance <instance-name>.

This error occurs when the slot name of a slot override does not
correspond to any of the valid slot names for an instance.

Example:

CLIPS> (defclass FOO (is-a USER) (role concrete))

CLIPS> (make-instance of FOO (x 3))

[INSMNGR14] Override required for slot <slot-name> in instance
<instance-name>.

If the ?NONE keyword was specified with the default attribute for a
slot, then a slot override must be provided when an instance containing
that slot is created.

Example:

CLIPS> (defclass FOO (is-a USER)

(role concrete)

(slot x (default ?NONE)))

CLIPS> (make-instance of FOO)

[INSMNGR15] init-slots not valid in this context.

The special function **init-slots** (for initializing slots of an
instance to the class default values) can only be called during the
dispatch of an **init** message for an instance, i.e., in an **init**
message-handler.

Example:

CLIPS>

(defmessage-handler INITIAL-OBJECT error ()

(init-slots))

CLIPS> (reset)

CLIPS> (send [initial-object] error)

[INSMODDP1] Direct/message-modify message valid only in modify-instance.

The **direct-modify** and **message-modify** message-handlers attached
to the class **USER** can only be called as a result of the appropriate
message being sent.by the **modify-instance** or
**message-modify-instance** functions. Additional handlers may be
defined, but the message can only be sent in this context.

Example:

CLIPS> (reset)

CLIPS> (send [initial-object] direct-modify 0)

[INSMODDP2] Direct/message-duplicate message valid only in
duplicate-instance.

The **direct-duplicate** and **message-duplicate** message-handlers
attached to the class **USER** can only be called as a result of the
appropriate message being sent.by the **duplicate-instance** or
**message-duplicate-instance** functions. Additional handlers may be
defined, but the message can only be sent in this context.

Example:

CLIPS> (reset)

CLIPS> (send [initial-object] direct-duplicate 0 0)

[INSMODDP3] Instance copy must have a different name in
duplicate-instance.

If an instance-name is specified for the new instance in the call to
**duplicate-instance**, it must be different from the source instance痴
name.

Example:

CLIPS> (reset)

CLIPS> (duplicate-instance initial-object to initial-object)

[INSMULT1] Function <name> cannot be used on single-field slot <name> in
instance <name>.

The functions described in section 12.13.4.12, such as **slot-insert$**,
can only operate on multifield slots.

Example:

CLIPS>

(defclass A (is-a USER)

(slot foo))

CLIPS> (make-instance a of A)

[a]

CLIPS> (slot-insert$ a foo 1 abc def)

[INSQYPSR1] Duplicate instance-set member variable name in function
<name>.

Instance-set member variables in an instance-set query function must be
unique.

Example:

CLIPS> (any-instancep ((?a OBJECT) (?a OBJECT)) TRUE)

[INSQYPSR2] Binds are not allowed in instance-set query in function
<name>.

An instance-set query cannot bind variables.

Example:

CLIPS>

(any-instancep ((?a OBJECT) (?b OBJECT))

(bind ?c 1))

[INSQYPSR3] Cannot rebind instance-set member variable <name> in
function <name>.

Instance-set member variables cannot be changed within the actions of an
instance-set query function.

Example:

CLIPS>

(do-for-all-instances ((?a USER))

(if (slot-existp ?a age) then

(> ?a:age 30))

(bind ?a (send ?a get-brother)))

[IOFUN1] Illegal logical name used for <function name> function.

A logical name must be either a symbol, string, instance-name, float, or
integer.

Example:

(printout (create$ a b c) x)

[IOFUN2] Logical name <logical name> already in use.

A logical name cannot be associated with two different files.

Example:

CLIPS> (open "foo.txt" foo "w")

TRUE

CLIPS> (open "foo2.txt" foo "w")

[MEMORY1] Out of memory

This error indicates insufficient memory exists to expand internal
strucｭtures enough to allow continued operation (causing an exit to the
operating system).

[MEMORY2] Release error in genfree

This error indicates a problem in the memory management routines.

[MEMORY3] Unable to allocate memory block > 32K

This error occurs when the bload function attempts to allocate a block
of memory larger than 32K and the operating system does not permit
blocks greater than 32K to be allocated. This will only occur on
machines which have 2 byte integers (excluding the Macintosh and IBM PC
which have machine dependent code provided so that they can allocate
more than 32K). When this error occurs, CLIPS exits to the operating
system.

[MISCFUN1] expand$ must be used in the argument list of a function call.

The expand$ function may not be called unless it is within the argument
list of another function.

Example:

CLIPS> (expand$ (create$ a b c))

[MODULDEF1] Illegal use of the module specifier.

The module specifier can only be used as part of a defined construct痴
name or as an argument to a function.

Example:

CLIPS> (deffunction y ())

CLIPS> (MAIN::y)

[MODULPSR1] Module <module name> does not export any constructs.

or

[MODULPSR1] Module <module name> does not export any <construct type>
constructs.

or

[MODULPSR1] Module <module name> does not export the <construct type>
<construct name>.

A construct cannot be imported from a module unless the defmodule
exports that construct.

Example:

CLIPS> (clear)

CLIPS> (defmodule BAR)

CLIPS> (deftemplate BAR::bar)

CLIPS> (defmodule FOO (import BAR deftemplate bar)))

[MSGCOM1] Incomplete message-handler specification for deletion.

It is illegal to specify a non-wildcard handler index when a wildcard is
given for the class in the external C function
**UndefmessageHandler()**. This error can only be generated when a
user-defined external function linked with CLIPS calls this function
incorrectly.

[MSGCOM2] Unable to find message-handler <name> <type> for class <name>
in function <name>.

This error occurs when the named function cannot find the specified
message-handler.

Example:

CLIPS> (ppdefmessage-handler USER foo around)

[MSGCOM3] Unable to delete message-handlers.

This error occurs when a message-handler can稚 be deleted (such as when
a binary image is loaded).

Example:

CLIPS> (defclass FOO (is-a USER) (role concrete))

CLIPS> (defmessage-handler FOO bar ())

CLIPS> (bsave foo.bin)

TRUE

CLIPS> (bload foo.bin)

TRUE

CLIPS> (undefmessage-handler FOO bar)

[MSGFUN1] No applicable primary message-handlers found for <message>.

No primary message-handler attached to the object痴 classes matched the
name of the message.

Example:

CLIPS> (defclass A (is-a USER))

CLIPS> (make-instance a of A)

[a]

CLIPS> (send [a] bogus-message)

[MSGFUN2] Message-handler <name> <type> in class <name> expected
exactly/at least <number> argument(s).

The number of message arguments was inappropriate for one of the
applicable message-handlers.

Example:

CLIPS> (defclass A (is-a USER))

CLIPS> (defmessage-handler USER foo (?a ?b))

CLIPS> (make-instance a of A)

[a]

CLIPS> (send [a] foo)

[MSGFUN3] <name> slot in instance <name>: write access denied.

This error occurs when an attempt is made to change the value of a
read-only slot.

Example:

CLIPS>

(defclass A (is-a USER)

(slot foo (default 100)

(read-only)))

CLIPS> (make-instance a of A)

[a]

CLIPS> (send [a] put-foo)

[MSGFUN4] <function> may only be called from within message-handlers.

The named function operates on the active instance of a message and thus
can only be called by message-handlers.

Example:

CLIPS> (ppinstance)

[MSGFUN5] <function> operates only on instances.

The named function operates on the active instance of a message and can
only handle instances of user-defined classes (not primitive type
objects).

Example:

CLIPS>

(defmessage-handler INTEGER print ()

(ppinstance))

CLIPS> (send 34 print)

[MSGFUN6] Private slot <slot-name> of class <class-name> cannot be
accessed directly by handlers attached to class <class-name>

A subclass which inherits private slots from a superclass may not access
those slots using the ?self variable. This error can also occur when a
superclass tries to access via **dynamic-put** or **dynamic-get** a
private slot in a subclass.

Example:

CLIPS> (defclass FOO (is-a USER) (role concrete) (slot x))

CLIPS> (defclass BAR (is-a FOO))

CLIPS> (defmessage-handler BAR yak () ?self:x)

[MSGFUN7] Unrecognized message-handler type in defmessage-handler.

Allowed message-handler types include primary, before, after, and
around.

Example:

CLIPS> (defmessage-handler USER foo behind ())

[MSGFUN8] Unable to delete message-handler(s) from class <name>.

This error occurs when an attempt is made to delete a message-handler
attached to a class for which any of the message-handlers are executing.

Example:

CLIPS> (reset)

CLIPS>

(defmessage-handler INITIAL-OBJECT error ()

(undefmessage-handler INITIAL-OBJECT error primary))

CLIPS> (send [initial-object] error)

[MSGPASS1] Shadowed message-handlers not applicable in current context.

No shadowed message-handler is available when the function
**call-next-handler** or **override-next-handler** is called.

Example:

CLIPS> (call-next-handler)

[MSGPASS2] No such instance <name> in function <name>.

This error occurs when the named function cannot find the specified
instance.

Example:

CLIPS> (instance-address [bogus-instance])

[MSGPASS3] Static reference to slot <name> of class <name> does not
apply to <instance-name> of <class-name>.

This error occurs when a static reference to a slot in a superclass by a
message-handler attached to that superclass is incorrectly applied to an
instance of a subclass which redefines that slot. Static slot references
always refer to the slot defined in the class to which the
message-handler is attached.

Example:

CLIPS>

(defclass A (is-a USER)

(slot foo))

CLIPS>

(defclass B (is-a A)

(role concrete)

(slot foo))

CLIPS>

(defmessage-handler A access-foo ()

?self:foo)

CLIPS> (make-instance b of B)

[b]

CLIPS> (send [b] access-foo)

[MSGPSR1] A class must be defined before its message-handlers.

A message-handler can only be attached to an existing class.

Example:

CLIPS> (defmessage-handler bogus-class foo ())

[MSGPSR2] Cannot (re)define message-handlers during execution of other
message-handlers for the same class.

No message-handlers for a class can be loaded while any current
message-handlers attached to the class are executing.

Example:

CLIPS> (defclass A (is-a USER))

CLIPS> (make-instance a of A)

[a]

CLIPS>

(defmessage-handler A build-new ()

(build "(defmessage-handler A new ())"))

CLIPS> (send [a] build-new)

[MSGPSR3] System message-handlers may not be modified.

There are four primary message-handlers attached to the class USER which
cannot be modified: init, delete, create and print.

Example:

CLIPS> (defmessage-handler USER init ())

[MSGPSR4] Illegal slot reference in parameter list.

Direct slot references are allowed only within message-handler bodies.

Example:

CLIPS> (defmessage-handler USER foo (?self:bar))

[MSGPSR5] Active instance parameter cannot be changed.

?self is a reserved parameter for the active instance.

Example:

CLIPS>

(defmessage-handler USER foo ()

(bind ?self 1))

[MSGPSR6] No such slot <name> in class <name> for ?self reference.

The symbol following the ?self: reference must be a valid slot for the
class.

Example:

CLIPS> (defclass FOO (is-a USER) (role concrete) (slot x))

CLIPS> (defmessage-handler FOO bar () ?self:y)

[MSGPSR7] Illegal value for ?self reference.

The symbol following the ?self: reference must be a symbol.

Example:

CLIPS> (defclass FOO (is-a USER) (role concrete) (slot x))

CLIPS> (defmessage-handler FOO bar () ?self:7)

[MSGPSR8] Message-handlers cannot be attached to the class <name>.

Message-handlers cannot be attached to the INSTANCE, INSTANCE-ADDRESS,
or INSTANCE-NAME classes.

Example:

CLIPS> (defmessage-handler INSTANCE foo ())

[MULTIFUN1] Multifield index <index> out of range 1..<end range> in
function <name>

or

[MULTIFUN1] Multifield index range <start>...<end> out of range 1..<end
range> in function <name>

This error occurs when a multifield manipulation function is passed a
single index or range of indices that does not fall within the specified
range of allowed indices.

Example:

CLIPS> (delete$ (create$ a b c) 4 4)

[MULTIFUN2] Cannot rebind field variable in function progn$.

The field variable (if specified) cannot be rebound within the body of
the progn$ function.

Example:

CLIPS> (progn$ (?field (create$ a)) (bind ?field 3))

[OBJRTBLD1] No objects of existing classes can satisfy pattern.

No objects of existing classes could possibly satisfy the pattern. This
error usually occurs when a restriction placed on the is-a attribute is
incompatible with slot restrictions before it in the pattern.

Example:

CLIPS> (defclass A (is-a INITIAL-OBJECT) (slot foo))

CLIPS> (defrule error (object (foo ?) (is-a ~A)) =>)

[OBJRTBLD2] No objects of existing classes can satisfy <attribute-name>
restriction in object pattern.

The restrictions on <attribute> are such that no objects of existing
classes (which also satisfy preceding restrictions) could possibly
satisfy the pattern.

Example:

CLIPS> (defrule error (object (bad-slot ?)) =>)

[OBJRTBLD3] No objects of existing classes can satisfy pattern
#<pattern-num>.

No objects of existing classes could possibly satisfy the pattern. This
error occurs when the constraints for a slot as given in the
defclass(es) are incompatible with the constraints imposed by the
pattern.

Example:

CLIPS>

(defclass FOO (is-a INITIAL-OBJECT)

(slot bar (type INTEGER)))

CLIPS>

(defclass BAR (is-a INITIAL-OBJECT)

(slot bar (type SYMBOL))

(slot woz))

CLIPS>

(defrule error

(x abc)

(object (bar 100) (woz ?))

(y def)

=>)

[OBJRTBLD4] Multiple restrictions on attribute <attribute-name> not
allowed.

Only one restriction per attribute is allowed per object pattern.

Example:

CLIPS> (defrule error (object (is-a ?) (is-a ?)) =>)

[OBJRTBLD5] Undefined class in object pattern.

Object patterns are applicable only to classes of objects which are
already defined.

Example:

CLIPS> (defrule error (object (is-a BOGUS)) =>)

[OBJRTMCH1] This error occurred in the object pattern network

Currently active instance: <instance-name>

Problem resides in slot <slot name> field #<field-index>

Of pattern #<pattern-number> in rule(s):

<problem-rules>+

This error pinpoints other evaluation errors associated with evaluating
an expression within the object pattern network. The specific pattern
and field of the problem rules are identified.

[PATTERN1] The symbol <symbol name> has special meaning and may not be
used as a <use name>.

Certain keywords have special meaning to CLIPS and may not be used in
situations that would cause an ambiguity.

Example:

CLIPS> (deftemplate exists (slot x))

[PATTERN2] Single and multifield constraints cannot be mixed in a field
constraint

Single and multifield variable constraints cannot be mixed in a field
constraint (this restriction does not include variables passed to
functions with the predicate or return value constraints).

Example:

CLIPS> (defrule foo (a ?x $?y ?x&~$?y) =>)

[PRCCODE1] Attempted to call a <construct> which does not exist.

In a CLIPS configuration without deffunctions and/or generic functions,
an attempt was made to call a deffunction or generic function from a
binary image generated by the **bsave** command.

[PRCCODE2] Functions without a return value are illegal as <construct>
arguments.

An evaluation error occurred while examining the arguments for a
deffunction, generic function or message.

Example:

CLIPS> (defmethod foo (?a))

CLIPS> (foo (instances))

[PRCCODE3] Undefined variable <name> referenced in <where>.

Local variables in the actions of a deffunction, method,
message-handler, or defrule must reference parameters, variables bound
within the actions with the **bind** function, or variables bound on the
LHS of a rule.

Example:

CLIPS> (defrule foo => (+ ?a 3))

[PRCCODE4] Execution halted during the actions of <construct> <name>.

This error occurs when the actions of a rule, deffunction, generic
function method or message-handler are prematurely aborted due to an
error.

[PRCCODE5] Variable <name> unbound [in <construct> <name>].

This error occurs when local variables in the actions of a deffunction,
method, message-handler, or defrule becomes unbound during execution as
a result of calling the **bind** function with no arguments.

Example:

CLIPS> (deffunction foo () (bind ?a) ?a)

CLIPS> (foo)

[PRCCODE6] This error occurred while evaluating arguments for the
<construct> <name>.

An evaluation error occurred while examining the arguments for a
deffunction, generic function method or message-handler.

Example:

CLIPS> (deffunction foo (?a))

CLIPS> (foo (+ (eval "(gensym)") 2))

[PRCCODE7] Duplicate parameter names not allowed.

Deffunction, method or message-handler parameter names must be unique.

Example:

CLIPS> (defmethod foo ((?x INTEGER) (?x FLOAT)))

[PRCCODE8] No parameters allowed after wildcard parameter.

A wildcard parameter for a deffunction, method or message-handler must
be the last parameter.

Example:

CLIPS> (defmethod foo (($?x INTEGER) (?y SYMBOL)))

[PRCDRPSR1] Cannot rebind count variable in function loop-for-count.

The special variable ?count cannot be rebound within the body of the
loop-for-count function.

Example:

CLIPS> (loop-for-count (?count 10) (bind ?count 3))

[PRCDRPSR2] The return function is not valid in this context.

or

[PRCDRPSR2] The break function is not valid in this context.

The return and break functions can only be used within certain contexts
(e.g. the break function can only be used within a while loop and
certain instance set query functions).

Example:

CLIPS> (return 3)

[PRCDRPSR3] Duplicate case found in switch function.

A case may be specified only once in a switch statement.

Example:

CLIPS> (switch a (case a then 8) (case a then 9))

[PRNTUTIL1] Unable to find <item> <item-name>

This error occurs when CLIPS cannot find the named item (check for
typos).

[PRNTUTIL2] Syntax Error: Check appropriate syntax for <item>

This error occurs when the appropriate syntax is not used.

Example:

CLIPS> (if (> 3 4))

[PRNTUTIL3]

\**\* CLIPS SYSTEM ERROR \**\*

ID = <error-id>

CLIPS data structures are in an inconsistent or corrupted state.

This error may have occurred from errors in user defined code.

\*************************\*

This error indicates an internal problem within CLIPS (which may have
been caused by user defined functions or other user code). If the
problem cannot be located within user defined code, then the <error-id>
should be reported.

[PRNTUTIL4] Unable to delete <item> <item-name>

This error occurs when CLIPS cannot delete the named item (e.g. a
construct might be in use). One example which will cause this error is
an attempt to delete a deffunction or generic function which is used in
another construct (such as the RHS of a defrule or a default-dynamic
facet of a defclass slot).

[PRNTUTIL5] The <item> has already been parsed.

This error occurs when CLIPS has already parsed an attribute or
declaration.

[PRNTUTIL6] Local variables cannot be accessed by <function or
construct>.

This error occurs when a local variable is used by a function or
construct that cannot use global variables.

Example:

CLIPS> (deffacts info (fact ?x))

[PRNTUTIL7] Attempt to divide by zero in <function-name> function.

This error occurs when a function attempts to divide by zero.

Example:

CLIPS> (/ 3 0)

[ROUTER1] Logical name <logical_name> was not recognized by any routers

This error results because "Hello" is not recognized as a valid router
name.

Example:

CLIPS> (printout "Hello" crlf)

[RULECSTR1] Variable <variable name> in CE #<integer> slot <slot name>

has constraint conflicts which make the pattern unmatchable.

or

[RULECSTR1] Variable <variable name> in CE #<integer> field #<integer>

has constraint conflicts which make the pattern unmatchable.

or

[RULECSTR1] CE #<integer> slot <slot name>

has constraint conflicts which make the pattern unmatchable.

or

[RULECSTR1] CE #<integer> field #<integer>

has constraint conflicts which make the pattern unmatchable.

This error occurs when slot value constraints (such as allowed types)
prevents any value from matching the slot constraint for a pattern.

Example:

CLIPS> (deftemplate foo (slot x (type SYMBOL)))

CLIPS> (deftemplate bar (slot x (type FLOAT)))

CLIPS> (defrule yak (foo (x ?x)) (bar (x ?x)) =>)

[RULECSTR2] Previous variable bindings of <variable name> caused the
type restrictions

for argument #<integer> of the expression <expression>

found in CE#<integer> slot <slot name> to be violated.

This error occurs when previous variable bindings and constraints
prevent a variable from containing a value which satisfies the type
constraints for one of a function痴 parameters.

Example:

CLIPS> (deftemplate foo (slot x (type SYMBOL)))

CLIPS> (defrule bar (foo (x ?x&:(> ?x 3))) =>)

[RULECSTR3] Previous variable bindings of <variable name> caused the
type restrictions

for argument #<integer> of the expression <expression>

found in the rule's RHS to be violated.

This error occurs when previous variable bindings and constraints
prevent a variable from containing a value which satisfies the type
constraints for one of a function痴 parameters.

Example:

CLIPS> (deftemplate foo (slot x (type SYMBOL)))

CLIPS> (defrule bar (foo (x ?x)) => (printout t (+ ?x 1) crlf))

[RULELHS1] The logical CE cannot be used with a not/exists/forall CE.

Logical CEs can be placed outside, but not inside, a not/exists/forall
CE.

Example:

CLIPS> (defrule error (not (logical (x))) =>)

[RULELHS2] A pattern CE cannot be bound to a pattern-address within a
not CE

This is an illegal operation and results in an error message.

Example:

CLIPS> (defrule error (not ?f <- (fact)) =>)

[RULEPSR1] Logical CEs must be placed first in a rule

If logical CEs are used, then the first CE must be a logical CE.

Example:

CLIPS> (defrule error (a) (logical (b)) =>)

[RULEPSR2] Gaps may not exist between logical CEs

Logical CEs found within a rule must be contiguous.

Example:

CLIPS> (defrule error (logical (a)) (b) (logical (c)) =>)

[STRNGFUN1] Function build does not work in run time modules.

or

[STRNGFUN1] Function eval does not work in run time modules.

The build and eval functions do not work in run time modules because the
code required for parsing is not available.

[STRNGFUN2] Some variables could not be accessed by the eval function.

Local variables cannot be accessed by the eval function.

Example:

CLIPS> (eval "?x")

[SYSDEP1] No file found for -f option

This message occurs if the -f option is used when executing CLIPS, but
no arguments are provided.

Example:

clips -f

[TEXTPRO1] Could not open file <file-name>.

This error occurs when the external text-processing system command
**fetch** encounters an error when loading a file.

Example:

CLIPS> (fetch 澱ogus.txt・

[TMPLTDEF1] Invalid slot <slot name> not defined in corresponding
deftemplate <deftemplate name>

The slot name supplied does not correspond to a slot name defined in the
correｭsponding deftemplate

Example:

CLIPS> (deftemplate example (slot x))

CLIPS> (defrule error (example (z 3)) =>)

[TMPLTDEF2] The single field slot <slot name> can only contain a single
field value.

If a slot definition is specified in a template pattern or fact, the
contents of the slot must be capable of matching against or evaluating
to a single value.

Example:

CLIPS> (deftemplate example (slot x))

CLIPS> (assert (example (x)))

[TMPLTFUN1] Fact-indexes can only be used by <command name> as a top
level command.

Fact indexes may only be used with the modify and duplicate commands
when the command is issued from the top-level command prompt.

Example:

CLIPS> (defrule foo => (modify 1 (x 3))

[TMPLTFUN2] Attempted to assert a multifield value into the single field
slot <slot name> of deftemplate <deftemplate name>.

A multifield value cannot be stored in a single field slot.

Example:

CLIPS> (deftemplate foo (slot x))

CLIPS>

(defrule foo

=>

(bind ?x (create$ a b))

(assert (foo (x ?x))))

CLIPS> (reset)

CLIPS> (run)

[TMPLTRHS1] Slot <slot name> requires a value because of its (default
?NONE) attribute.

The (default ?NONE) attribute requires that a slot value be supplied
whenever a new fact is created.

Example:

CLIPS> (deftemplate foo (slot x (default ?NONE)))

CLIPS> (assert (foo))

Appendix G:
CLIPS BNF
===========

Data Types

<symbol> ::= **A valid symbol as specified**

**in section 2.3.1**

<string> ::= **A valid string as specified**

**in section 2.3.1**

<float> ::= **A valid float as specified**

**in section 2.3.1**

<integer> ::= **A valid integer as specified**

**in section 2.3.1**

<instance-name> ::= **A valid instance-name as specified**

**in section 2.3.1**

<number> ::= <float> \| <integer>

<lexeme> ::= <symbol> \| <string>

<constant> ::= <symbol> \| <string> \| <integer> \|

<float> \| <instance-name>

<comment> ::= <string>

<variable-symbol> ::= **A symbol beginning with an**

**alphabetic character**

<function-name> ::= **Any symbol which corresponds to a**

**system or user defined function, a**

**deffunction name, or a defgeneric**

**name**

<file-name> ::= **A symbol or string which is a valid**

**file name (including path**

**information) for the operating**

**system under which CLIPS is running**

<slot-name> ::= **A valid deftemplate slot name**

<...-name> ::= **A <symbol> where the ellipsis**

**indicate what the symbol represents.**

**For example, <rule-name> is a symbol**

**which represents the name of a rule.**

Variables and Expressions

<single-field-variable> ::= ?<variable-symbol>

<multifield-variable> ::= $?<variable-symbol>

<global-variable> ::= ?*<symbol>\*

<variable> ::= <single-field-variable> \|

<multifield-variable> \|

<global-variable>

<function-call> ::= (<function-name> <expression>*)

<expression> ::= <constant> \| <variable> \|

<function-call>

<action> ::= <expression>

<...-expression> ::= **An <expression> which returns**

**the type indicated by the**

**ellipsis. For example,**

**<integer-expression> should**

**return an integer.**

Constructs

<CLIPS-program> ::= <construct>\*

<construct> ::= <deffacts-construct> \|

<deftemplate-construct> \|

<defglobal-construct> \|

<defrule-construct> \|

<deffunction-construct> \|

<defgeneric-construct> \|

<defmethod-construct> \|

<defclass-construct> \|

<definstance-construct> \|

<defmessage-handler-construct> \|

<defmodule-construct>

Deffacts Construct

| <deffacts-construct> ::= (deffacts <deffacts-name> [<comment>]
| <RHS-pattern>*)

Deftemplate Construct

<deftemplate-construct>

::= (deftemplate <deftemplate-name>

[<comment>]

<slot-definition>*)

<slot-definition> ::= <single-slot-definition> \|

<multislot-definition>

<single-slot-definition>

::= (slot <slot-name> <template-attribute>*)

<multislot-definition>

::= (multislot <slot-name>

<template-attribute>*)

<template-attribute>

::= <default-attribute> \|

<constraint-attribute>

<default-attribute>

::= (default ?DERIVE \| ?NONE \| <expression>*) \|

(default-dynamic <expression>*)

Fact Specification

<RHS-pattern> ::= <ordered-RHS-pattern> \|

<template-RHS-pattern>

<ordered-RHS-pattern> ::= (<symbol> <RHS-field>+)

<template-RHS-pattern> ::= (<deftemplate-name> <RHS-slot>*)

<RHS-slot> ::= <single-field-RHS-slot> \|

<multifield-RHS-slot>

<single-field-RHS-slot> ::= (<slot-name> <RHS-field>)

<multifield-RHS-slot> ::= (<slot-name> <RHS-field>*)

<RHS-field> ::= <variable> \|

<constant> \|

<function-call>

Defrule Construct

<defrule-construct> ::= (defrule <rule-name> [<comment>]

[<declaration>]

| <conditional-element>\*
| =>
| <action>*)

<declaration> ::= (declare <rule-property>+)

<rule-property> ::= (salience <integer-expression>) \|

(auto-focus <boolean-symbol>)

<boolean-symbol> ::= TRUE \| FALSE

<conditional-element> ::= <pattern-CE> \|

<assigned-pattern-CE> \|

<not-CE> \| <and-CE> \| <or-CE> \|

<logical-CE> \| <test-CE> \|

<exists-CE> \| <forall-CE>

<pattern-CE> ::= <ordered-pattern-CE> \|

<template-pattern-CE> \|

<object-pattern-CE>

<assigned-pattern-CE> ::= <single-field-variable> <- <pattern-CE>

<not-CE> ::= (not <conditional-element>)

<and-CE> ::= (and <conditional-element>+)

<or-CE> ::= (or <conditional-element>+)

<logical-CE> ::= (logical <conditional-element>+)

<test-CE> ::= (test <function-call>)

<exists-CE> ::= (exists <conditional-element>+)

<forall-CE> ::= (forall <conditional-element>

<conditional-element>+)

<ordered-pattern-CE> ::= (<symbol> <constraint>*)

<template-pattern-CE> ::= (<deftemplate-name> <LHS-slot>*)

<object-pattern-CE> ::= (object <attribute-constraint>*)

<attribute-constraint> ::= (is-a <constraint>) \|

(name <constraint>) \|

(<slot-name> <constraint>*)

<LHS-slot> ::= <single-field-LHS-slot> \|

<multifield-LHS-slot>

<single-field-LHS-slot> ::= (<slot-name> <constraint>)

<multifield-LHS-slot> ::= (<slot-name> <constraint>*)

<constraint> ::= ? \| $? \| <connected-constraint>

<connected-constraint>

::= <single-constraint> \|

<single-constraint> **&** <connected-constraint> \|

<single-constraint> **\|** <connected-constraint>

<single-constraint> ::= <term> \| ~<term>

<term> ::= <constant> \|

<single-field-variable> \|

<multifield-variable> \|

:<function-call> \|

=<function-call>

Defglobal Construct

<defglobal-construct> ::= (defglobal [<defmodule-name>]

<global-assignment>*)

<global-assignment> ::= <global-variable> = <expression>

<global-variable> ::= ?*<symbol>\*

Deffunction Construct

<deffunction-construct>

::= (deffunction <name> [<comment>]

(<regular-parameter>\* [<wildcard-parameter>])

<action>*)

<regular-parameter> ::= <single-field-variable>

<wildcard-parameter> ::= <multifield-variable>

Defgeneric Construct

<defgeneric-construct> ::= (defgeneric <name> [<comment>])

Defmethod Construct

<defmethod-construct>

::= (defmethod <name> [<index>] [<comment>]

(<parameter-restriction>\*

[<wildcard-parameter-restriction>])

<action>*)

<parameter-restriction>

::= <single-field-variable> \|

(<single-field-variable> <type>\* [<query>])

<wildcard-parameter-restriction>

::= <multifield-variable> \|

(<multifield-variable> <type>\* [<query>])

<type> ::= <class-name>

<query> ::= <global-variable> \| <function-call>

Defclass Construct

<defclass-construct> ::= (defclass <name> [<comment>]

(is-a <superclass-name>+)

[<role>]

[<pattern-match-role>]

<slot>\*

<handler-documentation>*)

<role> ::= (role concrete \| abstract)

<pattern-match-role>

::= (pattern-match reactive \| non-reactive)

<slot> ::= (slot <name> <facet>*) \|

(single-slot <name> <facet>*) \|

(multislot <name> <facet>*)

<facet> ::= <default-facet> \| <storage-facet> \|

<access-facet> \| <propagation-facet> \|

<source-facet> \| <pattern-match-facet> \|

<visibility-facet> \| <create-accessor-facet>

<override-message-facet> \| <constraint-attribute>

<default-facet> ::=

(default ?DERIVE \| ?NONE \| <expression>*) \|

(default-dynamic <expression>*)

<storage-facet> ::= (storage local \| shared)

<access-facet>

::= (access read-write \| read-only \| initialize-only)

<propagation-facet> ::= (propagation inherit \| no-inherit)

<source-facet> ::= (source exclusive \| composite)

<pattern-match-facet>

::= (pattern-match reactive \| non-reactive)

<visibility-facet> ::= (visibility private \| public)

<create-accessor-facet>

::= (create-accessor ?NONE \| read \| write \| read-write)

<override-message-facet>

::= (override-message ?DEFAULT \| <message-name>)

<handler-documentation>

::= (message-handler <name> [<handler-type>])

<handler-type> ::= primary \| around \| before \| after

Defmessage-handler Construct

<defmessage-handler-construct>

::= (defmessage-handler <class-name>

<message-name> [<handler-type>] [<comment>]

(<parameter>\* [<wildcard-parameter>])

<action>*)

<handler-type> ::= around \| before \| primary \| after

<parameter> ::= <single-field-variable>

<wildcard-parameter> ::= <multifield-variable>

Definstances Construct

<definstances-construct>

::= (definstances <definstances-name>

[active] [<comment>]

<instance-template>*)

<instance-template> ::= (<instance-definition>)

| <instance-definition> ::= <instance-name-expression> of
| <class-name-expression>
| <slot-override>\*

<slot-override> ::= (<slot-name-expression> <expression>*)

Defmodule Construct

<defmodule-construct> ::= (defmodule <module-name> [<comment>]

<port-specification>*)

<port-specification> ::= (export <port-item>) \|

(import <module-name> <port-item>)

<port-item> ::= ?ALL \|

?NONE \|

<port-construct> ?ALL \|

<port-construct> ?NONE \|

<port-construct> <construct-name>+

<port-construct> ::= deftemplate \| defclass \|

defglobal \| deffunction \|

defgeneric

Constraint Attributes

<constraint-attribute>

::= <type-attribute>\|

<allowed-constant-attribute> \|

<range-attribute> \|

<cardinality-attribute>

<type-attribute> ::= (type <type-specification>)

<type-specification> ::= <allowed-type>+ \| ?VARIABLE

<allowed-type> ::= SYMBOL \| STRING \| LEXEME \|

INTEGER \| FLOAT \| NUMBER \|

INSTANCE-NAME \| INSTANCE-ADDRESS \|

INSTANCE \| EXTERNAL-ADDRESS \|

FACT-ADDRESS

<allowed-constant-attribute>

::= (allowed-symbols<symbol-list>) \|

(allowed-strings <string-list>) \|

(allowed-lexemes <lexeme-list> \|

(allowed-integers<integer-list>) \|

(allowed-floats<float-list>) \|

(allowed-numbers<number-list>) \|

(allowed-instance-names <instance-list>) \|

(allowed-classes <class-name-list>) \|

(allowed-values<value-list>)

<symbol-list> ::= <symbol>+ \| ?VARIABLE

<string-list> ::= <string>+ \| ?VARIABLE

<lexeme-list> ::= <lexeme>+ \| ?VARIABLE

<integer-list> ::= <integer>+ \| ?VARIABLE

<float-list> ::= <float>+ \| ?VARIABLE

<number-list> ::= <number>+ \| ?VARIABLE

<instance-name-list> ::= <instance-name>+ \| ?VARIABLE

<class-name-list> ::= <class-name>+ \| ?VARIABLE

<value-list> ::= <constant>+ \| ?VARIABLE

<range-attribute> ::= (range <range-specification>

<range-specification>)

<range-specification> ::= <number> \| ?VARIABLE

<cardinality-attribute>

::= (cardinality <cardinality-specification>

<cardinality-specification>)

<cardinality-specification>

::= <integer> \| ?VARIABLE

Appendix H:
Reserved Function Names
=======================

This appendix lists all of the functions provided by either standard
CLIPS or various CLIPS extensions. They should be considered reserved
function names, and users should not create user-defined functions with
any of these names.

!=

\*

\*\*

+

-

/

<

<=

<>

=

>

>=

abs

acos

acosh

acot

acoth

acsc

acsch

active-duplicate-instance

active-initialize-instance

active-make-instance

active-message-duplicate-instance

active-message-modify-instance

active-modify-instance

agenda

and

any-instancep

apropos

asec

asech

asin

asinh

assert

assert-string

atan

atanh

batch

batch\*

bind

bload

bload-instances

break

browse-classes

bsave

bsave-instances

build

call-next-handler

call-next-method

call-specific-method

class

class-abstractp

class-existp

class-reactivep

class-slots

class-subclasses

class-superclasses

clear

clear-focus-stack

close

conserve-mem

constructs-to-c

cos

cosh

cot

coth

create$

csc

csch

defclass-module

deffacts-module

deffunction-module

defgeneric-module

defglobal-module

definstances-module

defrule-module

deftemplate-module

deg-grad

deg-rad

delayed-do-for-all-instances

delete$

delete-instance

dependencies

dependents

describe-class

direct-mv-delete

direct-mv-insert

direct-mv-replace

div

do-for-all-instances

do-for-instance

dribble-off

dribble-on

duplicate

duplicate-instance

duplicate-instance

dynamic-get

dynamic-put

edit

eq

eval

evenp

exit

exp

expand$

explode$

fact-existp

fact-index

fact-relation

fact-slot-names

fact-slot-value

facts

fetch

find-all-instances

find-instance

first$

float

floatp

focus

format

gensym

gensym\*

get

get-auto-float-dividend

get-current-module

get-defclass-list

get-deffacts-list

get-deffunction-list

get-defgeneric-list

get-defglobal-list

get-definstances-list

get-defmessage-handler-list

get-defmethod-list

get-defmodule-list

get-defrule-list

get-deftemplate-list

get-dynamic-constraint-checking

get-fact-duplication

get-fact-list

get-focus

get-focus-stack

get-function-restrictions

get-incremental-reset

get-method-restrictions

get-reset-globals

get-salience-evaluation

get-sequence-operator-recognition

get-static-constraint-checking

get-strategy

grad-deg

halt

if

implode$

init-slots

initialize-instance

initialize-instance

insert$

instance-address

instance-addressp

instance-existp

instance-name

instance-name-to-symbol

instance-namep

instancep

instances

integer

integerp

length

length$

lexemep

list-defclasses

list-deffacts

list-deffunctions

list-defgenerics

list-defglobals

list-definstances

list-defmessage-handlers

list-defmethods

list-defmodules

list-defrules

list-deftemplates

list-focus-stack

list-watch-items

load

load\*

load-facts

load-instances

log

log10

loop-for-count

lowcase

make-instance

make-instance

matches

max

mem-requests

mem-used

member

member$

message-duplicate-instance

message-duplicate-instance

message-handler-existp

message-modify-instance

message-modify-instance

min

mod

modify

modify-instance

modify-instance

multifieldp

mv-append

mv-delete

mv-replace

mv-slot-delete

mv-slot-insert

mv-slot-replace

mv-subseq

neq

next-handlerp

next-methodp

not

nth

nth$

numberp

object-pattern-match-delay

oddp

open

options

or

override-next-handler

override-next-method

pi

pointerp

pop-focus

ppdefclass

ppdeffacts

ppdeffunction

ppdefgeneric

ppdefglobal

ppdefinstances

ppdefmessage-handler

ppdefmethod

ppdefmodule

ppdefrule

ppdeftemplate

ppinstance

preview-generic

preview-send

primitives-info

print-region

printout

progn

progn$

put

rad-deg

random

read

readline

refresh

refresh-agenda

release-mem

remove

remove-break

rename

replace$

reset

rest$

restore-instances

retract

return

round

rule-complexity

rules

run

save

save-facts

save-instances

sec

sech

seed

send

sequencep

set-auto-float-dividend

set-break

set-current-module

set-dynamic-constraint-checking

set-fact-duplication

set-incremental-reset

set-reset-globals

set-salience-evaluation

set-sequence-operator-recognition

set-static-constraint-checking

set-strategy

setgen

show-breaks

show-defglobals

show-fht

show-fpn

show-joins

show-opn

sin

sinh

slot-allowed-values

slot-cardinality

slot-delete$

slot-direct-accessp

slot-direct-delete$

slot-direct-insert$

slot-direct-replace$

slot-existp

slot-facets

slot-initablep

slot-insert$

slot-publicp

slot-range

slot-replace$

slot-sources

slot-types

slot-writablep

sqrt

str-assert

str-cat

str-compare

str-explode

str-implode

str-index

str-length

stringp

sub-string

subclassp

subseq$

subset

subsetp

superclassp

switch

sym-cat

symbol-to-instance-name

symbolp

system

tan

tanh

time

toss

type

type

undefclass

undeffacts

undeffunction

undefgeneric

undefglobal

undefinstances

undefmessage-handler

undefmethod

undefrule

undeftemplate

unmake-instance

unwatch

upcase

watch

while

wordp

Appendix I:
Bibliography of CLIPS Publications
==================================

Programming

Giarratano, J., and Riley, G. *Expert Systems: Principles and
Programming*, 3rd Edition, Boston, PWS Publishing Company, 1998.

Gonzalez, A. J., and Dankel, D. D. *Engineering of
Knowledge-basedSystems: Theory and Practice*, Prentice Hall, 1993.

Jackson, P. *Introduction to Expert Systems*, 3rd Edition, Reading,
Addison-Wesley, 1998.

Reviews

Brooke, T. 典he Art of Production Systems,・*AI Expert*, January 1992.

Brooke, T. 鉄oftware Review,・*AI Expert*, April 1988.

Golden, J. 鉄hell Review Monthly,・*AI Today*, March/April 1988.

Mettrey, W. 鄭 Comparative Evaluation of Expert System Tools,・
*Computer*, February 1991.

Popolizio, J. 鼎LIPS: NASA痴 COSMIC Shell,・*Artificial Intelligence
Research*, August 1, 1988.

Raeth, P. 典wo PC肪ased Expert System Shells for the First釦ime
Developer,・*Computer*, November 1988.

Overviews

Culbert, C., et al., 鄭 Solution to the Expert System Delivery Problem,・
*Proceedings of the ISA/88*, Houston, TX, October 1988.

Riley, G. 鼎 Language Integrated Production System,・in *Encyclopedia of
Computer Science and Technology*, Volume 37. Edited by A. Kent and J.G.
Williams. New York, Marcel Dekker, Inc., 1997.

Riley, G. 摘xpert Systems Methodology,・in *The Industrial Electronics
Handbook*. Edited by J.D. Irwin. Boca Raton, CRC Press, 1997.

Riley, G. 鼎LIPS: An Expert System Building Tool,・*Proceedings of the
Technology 2001 Conference*, San Jose, CA, December 1991.

Riley, G. 鼎LIPS: A Tool for the Development and Delivery of Expert
Systems,・*Proceedings of the Technology 2000 Conference*, Washington,
DC, November 1990.

Riley G., and Donnell, B. 鄭dvanced CLIPS Capabilities,・*Proceedings of
The Fourth Annual Workshop on Space Operations Applications and Research
(SOAR ・0)*, Albuquerque, NM, June 1990.

Riley, G., et al. 鼎LIPS: An Expert System Tool for Training,
Development, and Delivery,・in *Intelligent Systems Review*, Volume 1,
Number 1, Fall 1988.

Riley, G., et al., 鼎LIPS: An Expert System Tool for Delivery and
Training,・*Proceedings of the Third Conference on Artificial
Intelligence for Space Applications*, Huntsville, AL, November 1987.

Applications

Carreno, L. A. and Steel, R. A. 鏑ife Insurance Risk Assessment using a
Fuzzy Logic Expert System,・*Proceedings of the North American Fuzzy
Logic Processing Society (NAFIPS 1992)*, Houston, TX, December 1992.

Cheatham, J. B., et al. 鄭 Multi亡ensor System for Robotics Proximity
Operations,・*Proceedings of The Second Annual Workshop on Space
Operations Automation and Robotics (SOAR ・8)*, Albuquerque, NM, July
1988.

Chen, Y. 鄭pplying Knowledge烹ased Expert System to Meat Grading,・
*Proceedings of The Annual AI Systems in Government Conference*,
Washington, D.C., March 1989.

鼎LIPS: A NASA Developed Expert System Tool,・*NASA Tech Briefs*,
November/December 1987.

Dutton, T. 滴UB SIAASHING: A Knowledge烹ased System for Severe,
Temporary Airline Schedule Reduction,・*Innovative Applications of
Artificial Intelligence 4*, Klahr, Philip, and Scott, A. Carlisle ed.,
1992.

Ehler, G. B. 鄭 Multiple Knowledge-Based Spatial Decision Support System
for Industrial Site Selection,・M.S. Thesis, Department of Geography,
University of South Carolina, Columbia, SC.

Fink, P., and Herren, L. T. 鄭n Intelligent Tutoring System to Teach
Interdependent Cognitive and High Performance Skills,・*Proceedings of
Contributed Sessions 1991 Conference on Intelligent Computer Aided
Training*, Houston, TX, November 1991.

Fink, P. 哲ESSUS/EXPERT: Bridging the Gap between Artificial
Intelligence and FORTRAN,・*Proceedings of The Second Annual Workshop on
Space Operations Automation and Robotics (SOAR ・8)*, Albuquerque, NM,
July 1988.

Flamm, R. O., et al. 典he Integrated Southern Pine Beetle Expert System:
ISPBEX,・*Expert Systems with Applications*, Vol. 2, 1991.

Franier, R., et al. 撤I吠n紡烹ox: A Knowledge肪ased System for Space
Science Experimentation,・*Proceedings of the Fifth Innovative
Applications of Artificial Intelligence Conference*, July 11・5, 1993,
Washington, D.C.

Frainier, R., et al. 撤I-in-a-Box: A Knowledge-Based System for Space
Science Experimentation,・*AI magazine*, Volume 15, No. 1, Spring, 1994.

Frainier, R., et al. 撤I in the Sky: The Astronaut Science Advisor on
SLS-2,・*Proceedings of The Seventh Annual Workshop on Space Operations
Applications and Research (SOAR ・3)*, Houston, TX, August 1993.

Germain, D., and Desrosiers, S. 典urning Up the Heat on Space Station
Training: The Active Thermal Control System ICAT,・*Proceedings of
Contributed Sessions 1991 Conference on Intelligent Computer Aided
Training*, Houston, TX, November 1991.

Grinstein, G. G., et al. 天irtual Environment Architecture for Rapid
Application Development,・*Proceedings of The Contributed Sessions 1993
Conference on Intelligent Computer Aided Training and Virtual
Environment Technology* (ICAT-VET ・3), Houston, TX, May 1993.

Haymann蓬aber, G., et al. 鄭n Expert System to Advise Astronauts During
Experiments: The Protocol Manager Module,・*Proceedings of The Third
Annual Workshop on Space Operations Automation and Robotics (SOAR ・9)*,
Houston, TX, July 1989.

Hill, T., and Faltisco, R. 的ntelligent Fault Management for the Space
Station Active Thermal Control System,・*Proceedings of The Fifth Annual
Workshop on Space Operations Applications and Research (SOAR ・1)*,
Houston, TX, July 1991.

Hipwell, D. P. 泥eveloping Realistic Cooperative Behaviors for
Autonomous Agents in Air Combat Simuation,・M.S. Thesis, Air Force
Institute of Technology, Wright Patternson AFB, Ohio, 1993.

Hughes, P. M. 鼎LEAR: Automating Control Centers with Expert System
Technology,・*Proceedings of The Third Annual Workshop on Space
Operations Automation and Robotics (SOAR ・9)*, Houston, TX, July 1989.

Johnson, W. B., et al. 鄭n Intelligent Tutoring System for Space Shuttle
Diagnosis,・*Proceedings of The Second Annual Workshop on Space
Operations Automation and Robotics (SOAR ・8)*, Albuquerque, NM, July
1988.

Kahn, M. G., et al. 鄭n Expert System for Culture-Based Infection
Control Surveillance,・*Proceedings of The Seventeenth Annual Symposium
on Computer Applications in Medical Care* (SCAMC ・3), Washington, D.C.,
October 1993.

Kingston, J. 撤ragmatic KADS: A methodological approach to a small
knowledge based systems project,・*Expert Systems: The International
Journal of Knowledge Engineering*, 4, 4, November 1992.

Kosta, C. P., and Krolak, P. D. 迭apid Prototyping 3D Virtual World
Interfaces within a Virtual Factory Environment,・*Proceedings of The
Contributed Sessions 1993 Conference on Intelligent Computer Aided
Training and Virtual Environment Technology* (ICAT-VET ・3), Houston,
TX, May 1993.

Kosta, C. P., and Krolak, P. D. 鄭n Artificial Reality Environment for
Remote Factory Control and Monitoring,・*Vision 21: Interdisciplinary
Science and Engineering in the Era of Cyberspace*, NASA/Lewis Research
Center, December 1993.

Kovarik, V. J. 鄭utonomously Acquiring Declarative and Procedural Domain
Knowledge for ICAT Systems,・*Proceedings of The Contributed Sessions
1993 Conference on Intelligent Computer Aided Training and Virtual
Environment Technology* (ICAT-VET ・3), Houston, TX, May 1993.

Lauriente, M., et al. 泥iagnosing Anomalies of Spacecraft for Space
Maintenance and Servicing,・*Proceedings of The Seventh Annual Workshop
on Space Operations Applications and Research (SOAR ・3)*, Houston, TX,
August 1993.

Lee, L., and Hill, R. W., 撤rocess Control and Recovery in the Link
Monitor and Control Operator Assistant,・*Proceedings of The Sixth
Annual Workshop on Space Operations Applications and Research (SOAR
・2)*, Houston, TX, August 1992.

Leinweber, D. 擢inance,・*Expert Systems and Artificial Intelligence:
Applications and Management,* Howard W. Sams & Company, Bartee, T. C.
ed., 1988.

Loftin, K. C., et al. 典he Application of Integrated Knowledge烹ased
Systems for the Biomedical Risk Accessment Intelligent Network (BRAIN),・
*Proceedings of Technology 2002: The Third National Technology Transfer
Conference and Exposition*, Washington D.C., February 1993.

Loftin, R. B., and Savely, R.T. 的ntelligent Computer Aided Training and
Tutoring,・*Proceedings of the Technology 2000 Conference*, Washington,
DC, November 1990.

Loftin, R. B., et al. 鄭n Intelligent Training System for Space Shuttle
Flight Controllers,・*Innovative Applications of Artificial
Intelligence*, 1989, AAAI Press/The MIT Press, Menlo Press, Schoor,
Herbert, and Rappaport, Alain ed.

Loftin, R. B., et al. 鄭n Intelligent Training System for Payload泡ssist
Module Deploys,・*Proceedings of The First Annual Workshop on Space
Operations Automation and Robotics (SOAR ・7)*, Houston, TX, August
1987.

Lucas, T. S., and Lewis, G. 泥ISPLAVAL: An Expert System Approach for
the Training of Display Builders,・*Proceedings of Contributed Sessions
1991 Conference on Intelligent Computer Aided Training*, Houston, TX,
November 1991.

McCarthy, L., et al. 鉄patial Considerations for Instructional
Development in a Virtual Environment,・*Proceedings of The Contributed
Sessions 1993 Conference on Intelligent Computer Aided Training and
Virtual Environment Technology* (ICAT-VET ・3), Houston, TX, May 1993.

溺ission Accomplished,・*NASA Tech Briefs*, September 1993.

Mitchell, P. 鄭n Expert System for Shuttle and Satellite Radar Tracker
Scheduling,・*Proceedings of The Second Annual Workshop on Space
Operations Automation and Robotics (SOAR ・8)*, Albuquerque, NM, July
1988.

Mitchell, R. 摘xpert Systems and Air砲ombat Simulation,・*AI Expert*,
September 1989.

Mortenson, P. 撤redicting Wind Shear from the Dispatch Office,・*Airline
Executive*, April 1988.

Mueller, S. 泥evelopment of a Personal砲omputer烹ased Intelligent
Tutoring System,・*Proceedings of The Second Annual Workshop on Space
Operations Automation and Robotics (SOAR ・8)*, Albuquerque, NM, July
1988.

Muratore, J., et al. 鉄pace Shuttle Telemetry Monitoring,・*Innovative
Applications of Artificial Intelligence*, 1989, AAAI Press/The MIT
Press, Menlo Press, Schoor, Herbert, and Rappaport, Alain ed.

Nash, J. 摘xpert Systems: A New Partnership,・*AI Expert*, December
1992.

Norton, J. E., et al. 溺icrocomputer Intelligence for Technical Training
(MITT): The Evolution of an Intelligent Tutoring System,・*Proceedings
of Contributed Sessions 1991 Conference on Intelligent Computer Aided
Training,* Houston, TX, November 1991.

*Proceedings of the First CLIPS Conference*, Houston, Texas, August
1990.

*Proceedings of the Second CLIPS Conference*, Houston, Texas, September
1991.

*Proceedings of the Third CLIPS Conference*, Houston, Texas, September
1994.

Robey, B., et al. 典he DRAIR Advisor: A Knowledge烹ased System for
Materiel Deficiency Analysis,・*Proceedings of the Fifth Innovative
Applications of Artificial Intelligence Conference*, July 11・5, 1993,
Washington, D.C.

Robey, B., et al. 泥RAIR Advisor: A Knowledge-Based System for
Materiel-Deficiency Analysis,・*AI magazine*, Volume 15, No. 2, Summer,
1994.

Rolincik, M, et al. 鄭n On僕ine Expert System for Diagnosing
Environmentally Induced Spacecraft Anomalies using CLIPS,・*Proceedings
of The Sixth Annual Workshop on Space Operations Applications and
Research (SOAR ・2)*, Houston, TX, August 1992.

Rolincik, M., et al. 鄭n Expert System for Diagnosing Environmentally
Induced Spacecraft Anomolies,・*Proceedings of The Fifth Annual Workshop
on Space Operations Applications and Research (SOAR ・1)*, Houston, TX,
July 1991.

Saito, T., et al. 鄭cquiring Knowledge within an ICAT (Intelligent
Computer泡ided Training) Environment: Factors and Issues,・*Proceedings
of Contributed Sessions 1991 Conference on Intelligent Computer Aided
Training*, Houston, TX, November 1991.

Saito, T., et al. 徹n the Acquisition and Representation of Procedural
Knowledge,・*Proceedings of The Fifth Annual Workshop on Space
Operations Applications and Research (SOAR ・1)*, Houston, TX, July
1991.

Scholtz, T. 典he State Transition Diagram with Path Priority and it痴
Applications,・M.S. Thesis, Naval Postgraduate School, Monterey, CA,
September 1993.

Schultz, R. D, and Stobie, I. 典he AI Bus Architecture for Distributed
Knowledge烹ased Systems,・*Proceedings of The Fourth Annual Workshop on
Space Operations Applications and Research (SOAR ・0)*, Albuquerque, NM,
June 1990.

Spelt, P. F. 鏑earning by an Autonomous Robot at a Process Control
Panel,・*IEEE Expert*, Winter 1989.

*Spinoff 1993*, NASA, pp. 88, 102, 120・21, 1994.

*Spinoff 1992*, NASA, p. 121, 1993.

*Spinoff 1991*, NASA, pp. 110・11, 1992.

Swartz, M., et al. 的ntelligent Help for Radar Maintenance
Troubleshooters,・*Proceedings of Contributed Sessions 1991 Conference
on Intelligent Computer Aided Training*, Houston, TX, November 1991.

Szatkowski, G. P., and Levin, B. E. 摘xpert System Decision Support for
Low膨ost Launch Vehicle Operations,・*Proceedings of The Fourth Annual
Workshop on Space Operations Applications and Research (SOAR ・0)*,
Albuquerque, NM, June 1990.

Truszkowski, W. 鄭dvances in Knowledge烹ased Software Engineering,・
*Proceedings of the Technology 2001Conference*, San Jose, CA, December
1991.

Wallfesh, S. K. 的nfantry Load Planning with LES,・*Artificial
Intelligence Applications for Logistics, Aerospace Systems, Robotics &
Personnel,* American Defense Preparedness Association, WIlliamsburg, VA,
8・0 March 1993.

Wang, L., and Bochsler, D. 鉄pace Shuttle Onboard Navigation Console
Expert/Trainer System,・*Proceedings of The First Annual Workshop on
Space Operations Automation and Robotics (SOAR ・7)*, Houston, TX,
August 1987.

Warren, K. C., and Goodman, B. A. 摘ngineering Intelligent Tutoring
Systems,・Proceedings of The Contributed Sessions 1993 Conference on
Intelligent Computer Aided Training and Virtual Environment Technology
(ICAT-VET ・3), Houston, TX, May 1993.

Wiederholt, B. J. 溺ITT Writer: An Authoring System for Developing
Intelligent Tutors for Complex Technical Domains,・*Proceedings of
Contributed Sessions 1991 Conference on Intelligent Computer Aided
Training*, Houston, TX, November 1991.

Woods, D. 鉄pace Station Freedom: Embedding AI,・*AI Expert*, April
1992.

Enhancements/Implementation

Donnell, B., 徹bject/rule Integration in CLIPS,・*Expert Systems: The
International Journal of Knowledge Engineering and Neural Networks*,
Learned Information, New Jersey, February 1994, Vol. 11, No. 1, ISSN
0266-4720, pp. 29-45.

Franke, J. L., et al. 鄭 General Purpose Certainty Processing Extension
to CLIPS,・*Proceedings of The 7th Florida Artificial Intelligence
Research Symposium*, Florida AI Research Society, 1994.

Le, T., and Homeier, P. 撤ortable Inference Engine: An Extended CLIPS
for Real傍ime Production Systems,・*Proceedings of The Second Annual
Workshop on Space Operations Automation and Robotics (SOAR ・8)*,
Albuquerque, NM, July 1988.

Li, Y. P. 泥CLIPS輸 Distributed CLIPS Implementation,・*Proceedings of
the 9th AIAA Computing in Aerospace Conference*, American Institute of
Aeronautics and Astronautics, San Diego, CA, October 1993.

Odette, L. L., *Intelligent Embedded Systems*, Addison妨esley Publishing
Company, pp.63・10,1991.

Riley, G. 的mplementing CLIPS on a Parallel Computer,・*Proceedings of
The First Annual Workshop on Space Operations Automation and Robotics
(SOAR ・7)*, Houston, TX, August 1987.

Riley, G. 的mplementation of an Expert System Shell on a Parallel
Computer,・*Proceedings of The Third Annual Artificial Intelligence &
Advanced Computer Technology Conference*, Long Beach Convention Center,
Long Beach, CA, April 1987.

Salzgeber, M. J., et al. 溺anaging Uncertainty in CLIPS: A System Level
Approach,・*Proceedings of The 6th Florida Artificial Intelligence
Research Symposium*, Florida AI Research Society, 1993.

Eick, C. F., et al. 泥ALI輸 Knowledge Base Management System,・
*Proceedings of the 1st Conference on Industrial and Engineering
Applications of AI & Expert Systems*.

Rete Pattern Matching Algorithm

Forgy, C. 迭ete: A Fast Algorithm for the Many Pattern/Many Object
Pattern Match Problem,・Artificial Intelligence, 19, pp. 17・7, 1982.

Forgy, C. 徹n the Efficient Implementation of Production Systems,・Ph.
D. Thesis, Carnegie-Mellon University, 1979.

Schneier, B. 典he Rete Matching Algorithm,・*AI Expert*, pp. 24-29,
December 1992.

Index
=====

- **190**: **44**? 7?DERIVE 22?NONE 22?self 109, **110**\ ( 7) 7\*
**190**\ \*\* **197**/ **191**\ & 7, **41**\ + **189**\ < 7, **164**\ <=
**165**\ <> **163**\ = **46**, **162=>** **27**> **163**>= **164**\ \|
7, **41**\ ~ 7, **41**\ $? 7abs **192**\ abstraction **18**\ action 16,
27, **159**\ activated 28active-duplicate-instance 83, 115,
**130**\ active-initialize-instance 83, **124**\ active-make-instance
83, **121**, 123active-message-duplicate-instance 83, 115,
**132**\ active-message-modify-instance 83, 115,
**129**\ active-modify-instance 83, 114, **128**\ Ada iv, 8, 9, 15,
16Advanced Programming Guide v, 1, 3, 5, 8, 51, 161, 178, 194, 309agenda
28, **29**, 33, 62, 289, 290, 291allowed-classes
154allowed-instance-names 155allowed-instances 155ampersand 7and
**165**\ antecedent **15**\ any-factp **230**, 324any-instancep 83,
**139**\ apropos **275**\ arrow **27**\ ART iiiArtificial Intelligence
Section iiiassert 11, 22, 83, 211, **217**, 220, 266assert-string
**220**\ attributedefault 22auto-focus **63**\ backslash 7, 180, 185,
186, 220Basic Programming Guide iv, v, 1batch 3, 5, **272**\ batch\* 5,
**272**\ bind 39, 65, 83, 110, 126, **199**\ bload **270**, 271,
274bload-instances 307, **308**\ break 83, 139, 202, **204**,
230browse-classes **302**\ bsave 153, 270, **271**\ bsave-instances
**307**, 308build **175**, 326C iii, 8, 9, 12, 15, 16,
21call-next-handler 83, 117, 118, **255**\ call-next-method 83, 86,
**241**, 242call-specific-method 75, 83, 86, **242**\ carriage return
7case sensitive 7check-syntax 177class 8, **13**, 78, 258, 299,
302abstract 90, **95**, 253, 299concrete 91, **95**, 253existence
**245**\ immediate **95**, 107non-reactive **95**\ precedence 93reactive
91, **95**, 253specific **92**, 95, 100, 118system **89**\ ADDRESS
**89**\ EXTERNAL-ADDRESS **89**\ FACT-ADDRESS **89**\ FLOAT
**89**\ INITIAL-OBJECT **89**\ INSTANCE **89**\ INSTANCE-ADDRESS
**89**\ INSTANCE-NAME **89**\ INTEGER **89**\ LEXEME **89**\ MULTIFIELD
**89**\ NUMBER **89**\ OBJECT **89**, 92, 302PRIMITIVE **89**\ STRING
**89**\ SYMBOL **89**\ USER **89**, 92, 112, 124, 257, 306user-defined
8, 14, **306**\ class function 240, **258**\ class-abstractp
**247**\ class-existp **245**\ class-reactivep **247**\ class-slots
**248**\ class-subclasses **248**\ class-superclasses **247**\ clear 11,
25, 65, 123, 145, 147, 151, 270, **271**\ clear-focus-stack
**292**\ CLIPS iiiCLOS 75, 89close **180**\ command 3, **159**,
269command prompt **3**\ comment 7, 10Common Lisp Object System
ivcondition **16**\ conditional element **16**, 25, 27, 33, 62and 27,
33, **53**\ exists 33, **55**\ forall 33, **56**\ logical 33,
**58**\ not 33, **54**\ or 33, **52**\ pattern 27, 33, **34**\ literal
**35**\ test 30, 33, **51**\ conflict resolution strategy 16, **28**,
29, 271, 272, 291breadth **29**\ complexity **30**\ depth **29**\ lex
**30**\ mea **31**\ random **32**\ simplicity **29**\ consequent
**15**\ conservation 253conserve-mem 270, **310**\ constant 3,
**9**\ constraint 33, 34, **41**, 44connective 34, **41**\ field
**34**\ literal **35**\ predicate 34, **44**, 51return value 34,
**46**\ construct 3, **10**, 175constructs 316constructs-to-c 274, 324,
325convenience 253COOL iv, 8, 14, 17, 18, 19, 75, 78, **89**, 240, 244,
298create$ **166**\ crlf **181**\ daemon **111**, 121, 137deactivated
28declarative technique **86**, **107**, 118declare
**62**\ default-dynamic 22defclass 8, 10, **91**, 105,
**298**\ defclass-module **244**\ deffacts 10, **13**, **25**,
283deffacts-module **234**\ deffunction **9**, 10, 16, **71**, 75, 294,
295action **72**\ advantages over generic functions **341**\ execution
error **72**\ recursion **72**\ regular parameter **71**\ return value
**72**\ wildcard parameter **71**\ deffunction-module
**238**\ defgeneric 10, **75**, 76defgeneric-module **239**\ defglobal
10, **15**, **65**, 293defglobal-module **238**\ definstances 10,
**15**, 91, **122**, 305initial-object **91**\ definstances-module
**256**\ defmessage-handler 10, 105, **106**, **303**\ defmethod 10,
**75**, 76defmodule 10, **145**, 308, **309**\ defmodules 17defrule 10,
**27**, **283**\ defrule-module **235**\ deftemplate 10, **12**, **21**,
279deftemplate fact **12**, 21, 220deftemplate-module
**212**\ deftemplate-slot-allowed-values **212**,
322deftemplate-slot-cardinality **212**, 322deftemplate-slot-defaultp
**213**, 322deftemplate-slot-default-value **214**,
322deftemplate-slot-existp **214**, 322deftemplate-slot-multip **215**,
322deftemplate-slot-names **215**, 322deftemplate-slot-range **216**,
322deftemplate-slot-singlep **216**, 323deftemplate-slot-type
323deftemplate-slot-types **216**\ deg-grad **195**\ deg-rad
**195**\ delayed-do-for-all-facts 230, **233**,
324delayed-do-for-all-instances 83, 139, **142**, 204, 327delete$
**168**\ delete-instance 126, **257**\ delete-member$ **172**\ delimiter
**7**\ dependencies **289**\ dependents **289**\ describe-class 92,
**299**\ direct-insert$ **262**\ div **191**\ do-for-all-facts 230,
**232**, 233, 324do-for-all-instances 83, 139, **141**, 142, 204,
327do-for-fact 230, **232**, 324do-for-instance 83, 139, **141**, 204,
327double quote 7dribble-off **276**\ dribble-on **275**\ duplicate 11,
13, 23, 83, **220**, 266duplicate-instance 83, 115, **130**\ dynamic
binding **18**\ dynamic-get 125, **260**, 371dynamic-put 125, **261**,
371embedded application 5encapsulation **18**, 89, 109,
120EnvFalseSymbol 325EnvTrueSymbol 325EOF **182**, 183, **187**\ eq
**161**\ eval **175**, 326evenp **160**\ exit 4, 180, **271**\ exp
**197**\ expand$ 83, 108, 266, 325explode$ **168**\ exponential notation
6exporting constructs 147expression **10**\ external-address 6, **8**,
9, 181-f 4-f2 5facet 91, **96**, 299accessinitialize-only
**98**\ read-only **98**\ read-write **98**\ create-accessor **103**,
253, 326?NONE **103**\ read **103**\ read-write **103**\ write
**103**\ default **96**\ default-dynamic **96**\ multislot
**96**\ override-message **104**\ pattern-matchnon-reactive
**101**\ reactive **101**\ propagationinherit **99**\ no-inherit 95,
**99**\ shared 96single-slot **96**\ slot **96**\ sourcecomposite 95,
**100**\ exclusive **100**\ storagelocal **97**\ shared
**97**\ visibility **102**\ private **102**\ public **102**\ fact
**11**, 13, 25, 280fact identifier **11**\ fact-address 6, 8, 9, **11**,
50, 181, 227fact-existp **222**\ fact-index **11**, 23, 218, 219, 220,
**221**, 289fact-list **11**, 13, 25, 27fact-relation **222**\ facts
**280**\ fact-set **226**\ action 229distributed action **229**\ member
**226**\ member variable **226**, 229query **228**, 229query execution
error **230**\ query functions **230**\ template **226**\ template
restriction **226**\ fact-slot-names **222**\ fact-slot-value
**223**\ FALSE 44FalseSymbol 325fetch **312**, 324ff 181field **9**,
12find-all-facts **231**, 233, 324find-all-instances 83, **140**,
142find-fact **231**, 324find-instance 83, **140**\ fire 27first$
**171**\ float 6, 8, **193**\ floatp **159**\ focus 28, 63, 151,
**291**\ foreach 83, 204, **206**, 266, 322format **183**, 188,
323FORTRAN 9funcall **211**, 324, 325, 327function 3, **9**, 16, 75,
138, 159, 229call 3, **10**\ external 5, 39, 46, 51predicate **44**, 51,
159, 259reserved names **393**\ system defined **9**, 393user defined 8,
**9**, 51, 378generic dispatch **75**, 76, 79, **81**, 341generic
function 14, 16, 17, **75**, **295**\ disadvantages **341**\ header 76,
**77**\ order dependence **76**\ ordering of method parameter
restrictions **341**\ performance penalty **76**\ return value
**87**\ gensym **207**, 208gensym\* 121, 129, **207**,
208get-auto-float-dividend **274**\ get-char **186**,
323get-class-defaults-mode **253**, 327get-current-module
**263**\ get-defclass-list **244**\ get-deffacts-list
**234**\ get-deffunction-list **238**\ get-defgeneric-list
**239**\ get-defglobal-list **237**\ GetDefglobalValue
325get-definstances-list **256**\ get-defmessage-handler-list
**248**\ get-defmethod-list **239**\ get-defmodule-list
**263**\ get-defrule-list **235**\ get-deftemplate-list
**217**\ get-dynamic-constraint-checking **274**\ get-fact-duplication
**282**\ get-fact-list **224**\ get-focus **236**\ get-focus-stack
**236**\ get-function-restrictions 83, **210**\ get-incremental-reset
**288**\ get-method-restrictions 83, **243**\ GetNextFactInTemplate
324get-profile-percent-threshold 315get-region **314**,
323get-reset-globals **294**\ get-salience-evaluation
**292**\ get-sequence-operator-recognition
**267**\ get-static-constraint-checking **275**\ get-strategy
**291**\ grad-deg **195**\ halt **291**\ help 322help-path 322I/O router
178if 83, **200**, 266if portion **15**\ imperative technique **86**,
**107**, 118implode$ **169**\ importing constructs 147incremental reset
28, 272, 288Inference Corporation iiiinference engine **16**, 27,
28inheritance 14, **18**, 91, 95class precedence list **19**, 91,
**92**, 93, 95, 100, 118, 299class precedence list 106is-a 92multiple
14, **19**, 89, **92**, **93**, 302initialize-instance 83, 97, 104, 112,
**124**, 257init-slots 112, 121, 124, **257**\ insert$ **170**\ instance
8, **13**, **14**, 15, 95, 97, 299, **306**\ active **109**, 116, 118,
125, 126, 257, 260, 306creation 116, **120**\ deletion 113direct 90, 91,
**95**, 99initialization 112, 120, 124, 257manipulation
**120**\ printing 113instance-address 6, **8**, 9, 50, 181, 258, 259,
341instance-addressp **260**\ instance-existp 260instance-list 15,
27instance-name 6, **8**, 135, **258**, 259, **260**\ instance-namep
260instance-name-to-symbol **259**\ instancep **259**\ instances
**306**\ instance-set **135**\ action 138class restriction
**135**\ distributed action **137**\ member **135**\ member variable
**135**, 138query 19, **137**, 138, 341query execution error
**139**\ query functions **139**\ template **135**\ integer 6, 8,
**193**\ integerp **159**\ integration 5Interfaces Guide v, 3Jess 207-l
5left-hand side **15**\ length **209**\ length$ 96, 108, **172**,
209less than 7lexemep **160**\ LHS **27**\ line feed 7LISP iii,
15list-defclasses 298list-deffacts **283**\ list-deffunctions
**295**\ list-defgenerics **296**\ list-defglobals
**293**\ list-definstances **305**\ list-defmessage-handlers
**303**\ list-defmethods 77, 83, 84, **296**, 297list-defmodules
**309**\ list-defrules **284**\ list-deftemplates
**279**\ list-focus-stack **292**\ list-watch-items **278**\ load 5,
**269**, 270, 272load\* **269**\ load-facts **280**\ LoadFactsFromString
326load-instances **308**\ local 281log **197**\ log10 **198**\ logical
name **179**, 275nil 181, 183stdin 182, 183, 186, 187, 275stdout 181,
183, 275t 181, 182, 183, 186, 187wclips 275wdialog 275wdisplay 275werror
275wtrace 275wwarning 275logical support **58**, 218, 219,
289loop-for-count 83, **202**, 204, 266lowcase **176**\ make-instance 8,
48, 83, 95, 97, 99, 104, 112, **120**, 122, 257, 308matches
**285**\ math functions **189**, **194**\ max
**192**\ max-number-of-elements 156member$ **167**, 326mem-requests
**309**\ mem-used **309**\ message 14, 16, **17**, 18, 19, 75, 89, 97,
106, 108, 109, 116, 118, **120**, 121, 124dispatch **107**\ execution
error 108, 118, 254execution error **119**\ implementation **106**,
107return value **120**\ message dispatch
**116**\ message-duplicate-instance 83, 104, 115,
**131**\ message-handler 14, 16, **17**, 19, 76, 89, 91, 92, 99,
**106**, 109, 118, 120, 125, 199, 257, 299, 306, 341action
**109**\ applicability 107, 108, 116, **118**, **304**\ documentation
**105**\ existence **246**\ forward declaration **105**\ regular
parameter **108**\ return value **120**\ shadow **118**, 254specific
116, 118, 120systemcreate **116**, 120, 121, 326delete **113**, 121,
122, **126**, 129direct-duplicate **115**, 130direct-modify **114**,
127, 128init 99, **112**, 120, 121, 124, 256message-duplicate **115**,
131, 132message-modify **115**, 128, 129print **113**\ typeafter
**106**, 118, 120around **106**, 118, 120, 254before **106**, 118,
120primary **106**, 118, 120wildcard parameter
**108**\ message-handler-existp **246**\ message-modify-instance 83,
104, 115, **128**\ method 17, **75**, 76, 89action **76**\ applicability
**79**, 86, 297execution error **86**, 240explicit **75**, 79,
**82**\ implicit **75**, 76, 79index **77**, 296parameter query
restriction **78**\ parameter restriction 76, 77, **78**, 82,
84parameter type restriction **78**\ precedence 78, 79, **84**,
296regular parameter **78**, 79return value **87**\ shadow **86**, 240,
304wildcard parameter **79**\ wildcard parameter restriction 76min
**192**\ min-number-of-elements 156mod **198**\ modify 11, 13, 23, 83,
**219**, 266modify-instance 83, 114, **127**\ module specifier
147multifield value 8, **9**\ multifield wildcard 36multifieldp
**161**\ named fields **12**\ NASA iiineq **162**\ next-handlerp 83,
**254**\ next-methodp 83, **240**\ non-FALSE 44non-ordered fact **12**,
21not **166**\ nth$ 96, **166**\ numberp **159**\ object 8, **13**, 17,
18behavior **13**, 16, **17**, 18, 75, 91, **106**, 107primitive type
**14**\ properties **13**, 14, 17, **18**, **91**\ reference **8**, 13,
19object-pattern-match-delay 48, 83, **126**, 266oddp **161**\ off
316open **179**, 180operating-system **211**, 322OPS5 30options
**273**\ or **165**\ ordered fact **12**, 21overload 10, **17**, 71,
**75**, 76, 341override-next-handler 83, 117, 118,
**255**\ override-next-method 83, 86, **241**, 242parenthesis 7,
10Pascal 12, 16, 21pattern **16**, 27pattern entity
**27**\ pattern-address 50pattern-matching **16**, 65, 66performance
**339**\ pi **196**\ pointerp **161**\ polymorphism **18**\ pop-focus
**237**\ ppdefclass **298**\ ppdeffacts **283**\ ppdeffunction
**295**\ ppdefgeneric **295**\ ppdefglobal **293**\ ppdefinstances
**305**\ ppdefmessage-handler **303**\ ppdefmethod **296**\ ppdefmodule
**309**\ ppdefrule 147, 271, **284**\ ppdeftemplate **279**\ ppfact
**282**, 323ppinstance **306**\ pprule 284prefix notation
10preview-generic **297**\ preview-send **304**\ printout **181**, 183,
327print-region **313**\ profile 316profile-info 315, 316profile-reset
315progn 83, 126, 138, **203**, 204, 266, 316, 327progn$ 83, **203**,
204, 207, 266, 326quote 7rad-deg **196**\ random **208**, 327read 178,
**182**, 183, 272readline **182**, 272read-number **187**, 188,
323Reference Manual v, viirefresh **288**\ refresh-agenda
**292**\ release-mem **310**\ remove **186**\ remove-break
**287**\ rename **185**\ replace$ **170**\ replace-member$
**172**\ reset 11, 13, 15, 25, 65, 91, 120, 122, 151, 159, 271, 272,
283, 294rest$ **171**\ restore-instances **308**\ RETE algorithm
**339**\ retract 11, 50, **218**\ return 28, 83, 139, 151, 202, **204**,
230, 266RHS **27**\ right-hand side **15**\ round **198**\ roundoff
6RtnArgCount 326rule **15**, **27**\ run 151, **290**, 291salience 28,
29, **62**, 292dynamic 28, **63**, 292save **270**, 310, 326save-facts
**281**\ save-instances **307**, 308scientific math functions
**194**\ seed 33, **209**\ semicolon 7, 10send 17, 19, 106, 116, 119,
120, 304, 341sequence expansion 39sequencep 161set-auto-float-dividend
191, **274**\ set-break **287**\ set-class-defaults-mode **253**, 326,
327set-current-module 147, **263**, 290set-dynamic-constraint-checking
23, 105, 153, 271, **274**\ set-fact-duplication **281**\ setgen
**208**\ set-incremental-reset **288**\ set-locale 187, **188**,
323set-profile-percent-threshold 315set-reset-globals 65,
**294**\ set-salience-evaluation 63,
**292**\ set-sequence-operator-recognition
**267**\ set-static-constraint-checking 23, 105, 153,
**274**\ set-strategy 29, 159, **291**\ show-breaks
**288**\ show-defglobals **294**\ significant digits 6single-field value
**9**\ single-field wildcard 36slot **12**, 14, 19, 91, 92, **95**, 100,
120, 245, 299access 98, 103, 245, 246accessor **103**,
137put-<slot-name> 121default value 96, 97, 121, 124, 257, 307direct
access **110**, 125, 137, 199existence **245**\ facet 96,
**100**\ inheritance propagation 99multifield **261**\ overlay
**100**\ override 99visibility **246**\ slot daemons
341slot-allowed-classes **254**, 323slot-allowed-values
**251**\ slot-cardinality **251**\ slot-default-value
**252**\ slot-delete$ **262**\ slot-direct-accessp
**246**\ slot-direct-delete$ **262**\ slot-direct-replace$
**261**\ slot-existp **245**\ slot-facets **249**\ slot-initablep
**246**\ slot-insert$ 96, **262**\ slot-override 121, 124, 257,
307slot-publicp **246**\ slot-range **252**\ slot-replace$
**261**\ slot-sources **250**\ slot-types **250**\ slot-writablep
**246**, 325Smalltalk iv, 75, 89sort **210**, 327space 7specificity
**29**\ sqrt **196**\ standard math functions **189**\ str-cat
**173**\ str-compare **176**\ str-index **174**\ string 6, **7**,
8stringp **160**\ string-to-field **178**, 327str-length
**177**\ subclass **92**, 116, 245, 299, 302subclassp **245**\ subseq$
**169**\ subsetp **167**\ sub-string **174**\ superclass 91, **92**, 95,
106, 245, 299direct **92**\ superclassp **245**\ switch 83, **205**,
266, 327symbol 6, **7**, 8, 258, 259reserved 12and 12declare 12exists
12forall 12logical 12not 12object 12or 12test 12symbolp
**160**\ symbol-to-instance-name **259**\ sym-cat **173**\ system
**273**\ tab 7, 181template 219then portion **15**\ tilde 7time
**209**\ timer **211**\ timer 327top level **3**\ toss
**315**\ trigonometric math functions **194**\ TrueSymbol 325truth
maintenance **58**\ type function **240**, 258unconditional support
**58**\ undefclass **298**\ undeffacts **283**\ undeffunction
**295**\ undefgeneric **296**\ undefglobal 65, **293**\ undefinstances
**306**\ undefmessage-handler **304**\ undefmethod **297**\ undefrule
147, **284**\ undeftemplate **279**\ unmake-instance 50,
**257**\ unwatch **278**\ upcase **176**\ User痴 Guide v,
viiuser-functions 316value **9**\ variable 5, 7, **9**, 11, 15, 34, 35,
38, 54, 175, **199**\ global 3, 15, 62, **65**, 199, 271vertical bar
7visible 281vtab 181watch **276**, 278watch itemactivations 28,
**277**\ all **277**\ compilations 269, **276**\ deffunctions
**277**\ facts 217, 218, **276**\ focus **277**\ generic-functions
**277**\ globals 65, **277**\ instances **277**\ message-handlers
**277**\ messages **277**\ methods **277**\ rules **277**, 290slots
**277**\ statistics **277**, 290while 83, **201**, 266wildcard 34, 35,
**36**\ wordp 160

.. |image0| image:: media/image1.png
   :width: 3.02768in
   :height: 3.5in
.. |image1| image:: media/image2.emf
   :width: 5.31944in
   :height: 5.54167in
.. |image2| image:: media/image3.emf
   :width: 6.5in
   :height: 4.89802in
.. |image3| image:: media/image4.emf
   :width: 5.45in
   :height: 7.9125in
.. |image4| image:: media/image5.emf
   :width: 2.57431in
   :height: 2.83333in
.. |image5| image:: media/image6.emf
   :width: 6.06667in
   :height: 1.71111in
.. |FST| image:: media/image7.jpeg
   :width: 5.81944in
   :height: 1.86111in
.. |image7| image:: media/image8.emf
   :width: 0.38889in
   :height: 0.47222in
