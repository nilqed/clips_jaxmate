Section 13: Commands
====================

This section describes commands primarily intended for use from the
top-level command prompt. These commands may also be used from
constructs and other places where functions can be used.

13.1 Environment Commands
-------------------------

The following commands control the CLIPS environment.

13.1.1 Loading Constructs From A File
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.Loads the constructs stored in the file specified by <file-name> into
the environment. If the compilations item is being watched (see section
13.2), then an informational message (including the type and name of the
construct) will be displayed for each construct loaded. If the
compilations item is not being watched, then a character is printed for
each construct loaded (“*” for defrule, “$” for deffacts, “%” for
deftemplate, “:” for defglobal, “!” for deffunction, “^” for defgeneric,
“&” for defmethod, “#” for defclass, “~” for defmessage-handler, “@” for
definstances, and “+” for defmodule). This function returns TRUE if the
file was successfully loaded, otherwise FALSE is returned.

Syntax

(load <file-name>)

13.1.2 Loading Constructs From A File without Progress Information
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Loads the constructs stored in the file specified by <file-name> into
the environment, however, unlike the load command informational
messsages are not printed to show the progress of loading the file.
Error messages are still printed if errors are encountered while loading
the file. This function returns TRUE if the file was successfully
loaded, otherwise FALSE is returned.

Syntax

(load\* <file-name>)

13.1.3 Saving All Constructs To A File
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Saves all of the constructs (defrules, deffacts, deftemplates, etc.) in
the current envi­ronment into the file specified by <file-name>. Note
that deffunctions and generic functions are saved twice to the file.
Because it is possible to create circular references among deffunctions
and generic functions by redefining them, a forward declaration
(containing no actions) of each function is saved first to the file, and
then the actual declaration (containing the actions) is saved. This
function returns TRUE if the file was successfully saved, otherwise
FALSE is returned. This function uses the pretty-print forms of the
constructs. If pretty-printing has been disabled by the **conserve-mem**
command, then the **save** command will have no output.

Syntax

(save <file-name>)

13.1.4 Loading a Binary Image
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Loads the constructs stored in the binary file specified by <file-name>
into the environment. The specified file must have been created by
**bsave**. Loading a binary image is quicker than using the **load**
command to load a ASCII text file. A **bload** clears all constructs
from the current CLIPS environment (as well as all facts and instances).
**Bload** can be called at any time unless some constructs that
**bload** will affect are in use (e.g. a deffunction is currently
executing). The only constructive/destructive operation that can occur
after a **bload** is the **clear** command or the **bload** command
(which clears the current binary image). This means that constructs
cannot be loaded or deleted while a **bload** is in effect. In order to
add constructs to a binary image, the original ASCII text file must be
reloaded, the new constructs added, and then another **bsave** must be
performed. This function returns TRUE if the file was successfully
bloaded, otherwise FALSE is returned.

Binary images can be loaded into different compile-time configurations
of CLIPS, as long as the same version of CLIPS is used and all the
functions and constructs needed by the binary image are supported. In
addition, binary images should theoretically work across different
hardware platforms if internal data representations are equivalent (e.g.
same integer size, same byte order, same floating-point format, etc).
However, it is NOT recommended that this be attempted.

Syntax

(bload <file-name>)

13.1.5 Saving a Binary Image
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Saves all of the constructs in the current envi­ronment into the file
specified by <file-name>. The save file is written using a binary format
which results in faster load time. The save file must be loaded via the
**bload** command. A **bsave** may be performed at any time (except when
a **bload** is in effect). The pretty print representation of a
construct is not saved with a binary image (thus, commands like
**ppdefrule** will show no output for any of the rules associated with
the binary image). In addition, constraint information associated with
constructs is not saved to the binary image unless dynamic constraint
checking is enabled (using the **set-dynamic-constraint-checking**
command). This function returns TRUE if the file was successfully
bsaved, otherwise FALSE is returned.

Syntax

(bsave <file-name>)

13.1.6 Clearing CLIPS
~~~~~~~~~~~~~~~~~~~~~

Clears CLIPS. Removes all constructs and all associated data structures
(such as facts and instances) from the CLIPS environment. A clear may be
performed safely at any time, however, certain constructs will not allow
themselves to be deleted while they are in use. For example, while
deffacts are being reset (by the **reset** command), it is not possible
to remove them using the **clear** command. Note that the **clear**
command does not effect many environment characteristics (such as the
current conflict resolution strategy). This function has no return
value.

Syntax

(clear)

13.1.7 Exiting CLIPS
~~~~~~~~~~~~~~~~~~~~

Quits CLIPS. This function has no return value.

Syntax

(exit [<integer-expression>])

The optional <integer-expression> argument allows the exit status code
to be specified which is eventually passed to the C exit function.

13.1.8 Resetting CLIPS
~~~~~~~~~~~~~~~~~~~~~~

Resets CLIPS. Removes all activations from the agenda, all facts from
the fact-list and all instances of user-defined classes, then assigns
global variables their initial values, asserts all facts listed in
def­facts statements into the fact-list, creates all instances listed in
definstances statements, sets the current module to the MAIN module and
automatically focuses on the same module. Incremental reset is supported
for rules. This means that rules can be activated from facts that were
asserted before the rule was defined without performing a reset. A reset
can be performed while rules are executing. Note that the **reset**
command does not effect many environment characteristics (such as the
current conflict resolution strategy). This function has no return
value.

Syntax

(reset)

13.1.9 Executing Commands From a File
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Allows “batch” processing of CLIPS interactive com­mands by replacing
standard input with the contents of a file. Any command or function can
be used in a batch file, as well as construct definitions and re­sponses
to **read** or **readline** function calls. The **load** command should
be used in batch files rather than defining constructs directly. The
**load** command expects only constructs and hence moves to the next
construct when an error occurs. The **batch** command, however, moves on
until it finds the next construct *or* command (and in the case of a
construct this is likely to generate more errors as the remaining
commands and functions in the construct are parsed). This function
returns TRUE if the batch file was successfully executed, otherwise
FALSE is returned. Note that the **batch** command operates by replacing
standard input rather than by immediately executing the commands found
in the batch file. In effect, if you execute a batch command from the
RHS of a rule, the commands in that batch file will not be processed
until control is returned to the top-level prompt.

Syntax

(batch <file-name>)

13.1.10 Executing Commands From a File Without Replacing Standard Input
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Evaluates the series of commands stored in the file specified by
<file-name>. Unlike the **batch** command, **batch\*** evaluates all of
the commands in the specified file before returning. The **batch\***
command does not replace standard input and thus a **batch\*** file
cannot be used to provide input to functions such as **read** and
**readline**. In addition, commands stored in the **batch\*** file and
the return value of these commands are not echoed to standard output.

The **batch\*** command is not available for binary-load only or
run-time CLIPS configurations (see the *Advanced Programming Guide*).

Syntax

(batch\* <file-name>)

13.1.11 Determining CLIPS Compilation Options
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Generates a textual description of the settings of the CLIPS compiler
flags. This function has no return value.

Syntax

(options)

13.1.12 Calling the Operating System
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The **system** function allows a call to the operating system. It is
patterned after the **system** function provided to C on most UNIX
systems. This function has no return value.

Syntax

(system <lexeme-expression>*)

Example

| (defrule print-directory
| (print-directory ?directory)
| =>
| (system "dir " ?directory)); Note space => "dir<space>"

Note that any spaces needed for a proper parsing of the **system**
command must be added by the user in the call to system. Also note that
the system command is not guaranteed to execute (e.g., the operating
system may not have enough memory to spawn a new process).

? Portability Note

Not all operating systems/compilers provide this function. The code is
stored in the **sysdep.c** file, and the default coding for generic
CLIPS is a non­functional stub that will compile on any machine. On some
machines (such as an IBM PC running DOS), there may be insufficient
memory available to spawn a subprocess to execute the system command. In
such a case, the command will not be executed and the system command
will return with no action taken.

13.1.13 Setting The Auto-Float Dividend Behavior
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This function sets auto-float dividend behavior. When this behavior is
enabled (TRUE by default) the dividend of the division function is
automatically converted to a floating point number. The return value for
this function is the old value for the behavior.

Syntax

(set-auto-float-dividend <boolean-expression>)

13.1.14 Getting The Auto-Float Dividend Behavior
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This function returns the current value of the auto-float dividend
behavior (TRUE or FALSE).

Syntax

(get-auto-float-dividend)

13.1.15 Setting the Dynamic Constraint Checking Behavior
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This function sets dynamic constraint checking behavior. When this
behavior is disabled (FALSE by default), newly created data objects
(such as deftemplate facts and instances) do not have their slot values
checked for constraint violations. When this behavior is enabled (TRUE),
the slot values are checked for constraint violations. The return value
for this function is the old value for the behavior. Constraint
information is not saved when using the **bload** and
**constructs-to-c** command if dynamic constraint checking is disabled.

Syntax

(set-dynamic-constraint-checking <boolean-expression>)

13.1.16 Getting the Dynamic Constraint Checking Behavior
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This function returns the current value of the dynamic constraint
checking behavior (TRUE or FALSE).

Syntax

(get-dynamic-constraint-checking)

13.1.17 Setting the Static Constraint Checking Behavior
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This function sets static constraint checking behavior. When this
behavior is disabled (FALSE), constraint violations are not checked when
function calls and constructs are parsed. When this behavior is enabled
(TRUE by default), constraint violations are checked when function calls
and constructs are parsed. The return value for this function is the old
value for the behavior.

Syntax

(set-static-constraint-checking <boolean-expression>)

13.1.18 Getting the Static Constraint Checking Behavior
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This function returns the current value of the static constraint
checking behavior (TRUE or FALSE).

Syntax

(get-static-constraint-checking)

13.1.19 Finding Symbols
~~~~~~~~~~~~~~~~~~~~~~~

This command displays all symbols currently defined in CLIPS which
contain a specified substring. This command has no return value.

Syntax

(apropos <lexeme>)

Example

CLIPS> (apropos pen)

dependents

mv-append

open

dependencies

CLIPS>

13.2 Debugging Commands
-----------------------

The following commands control the CLIPS debugging features.

13.2.1 Generating Trace Files
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Sends all information normally sent to the logical names **wclips**,
**wdialog**, **wdisplay**, **werror**, **wwarning**, **wtrace**, and
**stdout** to <file-name> as well as to their normal destination.
Additionally, all information received from logical name **stdin** is
also sent to <file-name> as well as being returned by the requesting
function. This function returns TRUE if the dribble file was
successfully opened, otherwise FALSE is returned.

Syntax

(dribble-on <file-name>)

13.2.2 Closing Trace Files
~~~~~~~~~~~~~~~~~~~~~~~~~~

Stops sending trace information to the dribble file. This function
returns TRUE if the dribble file was successfully closed, otherwise
FALSE is returned.

Syntax

(dribble-off)

13.2.3 Enabling Watch Items
~~~~~~~~~~~~~~~~~~~~~~~~~~~

This function causes messages to be displayed when certain CLIPS
operations take place.

Syntax

(watch <watch-item>)

<watch-item> ::= all \|

compilations \|

statistics \|

focus \|

messages \|

deffunctions <deffunction-name>\* \|

globals <global-name>\* \|

rules <rule-name>\* \|

activations <rule-name>\* \|

facts <deftemplate-name>\* \|

instances <class-name>\* \|

slots <class-name>\* \|

message-handlers <handler-spec-1>\*

[<handler-spec-2>]) \|

generic-functions <generic-name>\* \|

methods <method-spec-1>\* [<method-spec-2>]

<handler-spec-1> ::= <class-name>

<handler-name> <handler-type>

<handler-spec-2> ::= <class-name>

[<handler-name> [<handler-type>]]

<method-spec-1> ::= <generic-name> <method-index>

<method-spec-2> ::= <generic-name> [<method-index>]

If **compilations** are watched, the progress of construct definitions
will be displayed.

If **facts** are watched, all fact assertions and retractions will be
displayed. Optionally, facts associated with individual deftemplates can
be watched by specifying one or more deftemplate names.

If **rules** are watched, all rule firings will be dis­played. If
**activations** are watched, all rule activations and deactivations will
be displayed. Optionally, rule firings and activations associated with
individual defrules can be watched by specifying one or more defrule
names. If **statistics** are watched, timing information along with
other information (average number of facts, average number of
activations, etc.) will be displayed after a run. Note that the number
of rules fired and timing information is not printed unless this item is
being watch. If **focus** is watched, then changes to the current focus
will be displayed.

If **globals** are watched, variable assignments to globals variables
will be displayed. Optionally, variable assignments associated with
individual defglobals can be watched by specifying one or more defglobal
names. If **deffunctions** are watched, the start and finish of
deffunctions will be displayed. Optionally, the start and end display
associated with individual deffunctions can be watched by specifying one
or more deffunction names.

If **generic-functions** are watched, the start and finish of generic
functions will be displayed. Optionally, the start and end display
associated with individual defgenerics can be watched by specifying one
or more defgeneric names. If **methods** are watched, the start and
finish of individual methods within a generic function will be
displayed. Optionally, individual methods can be watched by specifying
one or more methods using a defgeneric name and a method index. When the
method index is not specified, then all methods of the specified
defgeneric will be watched.

If **instances** are watched, creation and deletion of instances will be
displayed. If **slots** are watched, changes to any instance slot values
will be displayed. Optionally, instances and slots associated with
individual concrete defclasses can be watched by specifying one or more
concrete defclass names. If **message-handlers** are watched, the start
and finish of individual message-handlers within a message will be
displayed. Optionally, individual message-handlers can be watched by
specifying one or more message-handlers using a defclass name, a
message-handler name, and a message-handler type. When the
message-handler name and message-handler type are not specified, then
all message-handlers for the specified class will be watched. When the
message-handler type is not specified, then all message-handlers for the
specified class with the specified message-handler name will be watched.
If **messages** are watched, the start and finish of messages will be
displayed.

For the watch items that allow individual constructs to be watched, if
no constructs are specified, then all constructs of that type will be
watched. If all constructs associated with a watch item are being
watched, then newly defined constructs of the same type will also be
watched. A construct retains its old watch state if it is redefined. If
**all** is watched, then all other watch items will be watched. By
default, only compilations are watched. The watch function has no return
value.

Example

CLIPS> (watch rules)

CLIPS>

13.2.4 Disabling Watch Items
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This function disables the effect of the **watch** command.

Syntax

(unwatch <watch-item>)

This command is identical to the watch command with the exception that
it disables watch items rather than enabling them. This function has no
return value.

Example

CLIPS> (unwatch all)

CLIPS>

13.2.5 Viewing the Current State of Watch Items
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This command displays the current state of watch items.

Syntax

(list-watch-items [<watch-item>])

This command displays the current state of all watch items. If called
without the <watch-item> argument, the global watch state of all watch
items is displayed. If called with the <watch-item> argument, the global
watch state for that item is displayed followed by the individual watch
states for each item of the specified type which can be watched. This
function has no return value.

Example

CLIPS> (list-watch-items)

facts = off

instances = off

slots = off

rules = off

activations = off

messages = off

message-handlers = off

generic-functions = off

methods = off

deffunctions = off

compilations = on

statistics = off

globals = off

focus = off

CLIPS> (list-watch-items facts)

facts = off

MAIN:

initial-fact = off

CLIPS>

13.3 Deftemplate Commands
-------------------------

The following commands manipulate deftemplates.

13.3.1 Displaying the Text of a Deftemplate
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Displays the text of a given deftemplate. This function has no return
value.

Syntax

(ppdeftemplate <deftemplate-name>)

13.3.2 Displaying the List of Deftemplates
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Displays the names of all deftemplates. This function has no return
value.

Syntax

(list-deftemplates [<module-name>])

If <module-name> is unspecified, then the names of all deftemplates in
the current module are displayed. If <module-name> is specified, then
the names of all deftemplates in the specified module are displayed. If
<module-name> is the symbol \*, then the names of all deftemplates in
all modules are displayed.

13.3.3 Deleting a Deftemplate
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This function deletes a previously defined deftemplate.

Syntax

(undeftemplate <deftemplate-name>)

If the deftemplate is in use (for example by a fact or a rule), then the
deletion will fail. Otherwise, no further uses of the deleted
deftemplate are permitted (unless redefined). If the symbol \* is used
for <deftemplate-name>, then all deftemplates will be deleted (unless
there is a deftemplate named \*). This function has no return value.

13.4 Fact Commands
------------------

The following commands display information about facts.

13.4.1 Displaying the Fact-List
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Displays facts stored in the fact-list.

Syntax

(facts [<module-name>]

[<start-integer-expression>

[<end-integer-expression>

[<max-integer-expression>]]])

If <module-name> is not specified, then only facts visible to the
current module will be displayed. If <module-name> is specified, then
only facts visible to the specified module are displayed. If the symbol
\* is used for <module-name>, then facts from any module may be
displayed. If the start argument is speci­fied, only facts with
fact-indices greater than or equal to this argument are displayed. If
the end argument is speci­fied, only facts with fact-indices less than
or equal to this argument are displayed. If the max argument is
speci­fied, then no facts will be displayed beyond the specified maximum
number of facts to be displayed. This function has no return value.

13.4.2 Loading Facts From a File
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This function will assert a file of information as facts into the CLIPS
fact-list. It can read files created with save-facts or any ASCII text
file. Each fact should begin with a left parenthesis and end with a
right parenthesis. Facts may span across lines and can be written in
either ordered or deftemplate format. This function returns TRUE if the
fact file was successfully loaded, otherwise FALSE is returned.

Syntax

(load-facts <file-name>)

13.4.3 Saving The Fact-List To A File
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This function saves all of the facts in the current fact-list into the
file specified by <file-name>. External-address and fact-address fields
are saved as strings. Instance-address fields are converted to
instance-names. Optionally, the scope of facts to be saved can be
specified. If <save-scope> is the symbol **visible**, then all facts
visible to the current module are saved. If <save-scope> is the symbol
**local**, then only those facts with deftemplates defined in the
current module are saved. If <save-scope> is not specified, it defaults
to **local**. If <save-scope> is specified, then one or more deftemplate
names may also be specified. In this event, only those facts with
associated with a corresponding deftemplate in the specified list will
be saved. This function returns TRUE if the fact file was successfully
saved, otherwise FALSE is returned.

Syntax

(save-facts <file-name> [<save-scope> <deftemplate-names>*])

<save-scope> ::= visible \| local

13.4.4 Setting the Duplication Behavior of Facts
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This function sets fact duplication behavior. When this behavior is
disabled (FALSE by default), asserting a duplicate of a fact already in
the fact-list produces no effect. When enabled (TRUE), the duplicate
fact is asserted with a new fact-index. The return value for this
function is the old value for the behavior.

Syntax

(set-fact-duplication <boolean-expression>)

Example

CLIPS> (clear)

CLIPS> (get-fact-duplication)

FALSE

CLIPS> (watch facts)

CLIPS> (assert (a))

==> f-1 (a)

<Fact-1>

CLIPS> (assert (a))

FALSE

CLIPS> (set-fact-duplication TRUE)

FALSE

CLIPS> (assert (a))

==> f-2 (a)

<Fact-2>

CLIPS> (unwatch facts)

CLIPS> (set-fact-duplication FALSE)

TRUE

CLIPS>

13.4.5 Getting the Duplication Behavior of Facts
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This function returns the current value of the fact duplication behavior
(TRUE or FALSE).

Syntax

(get-fact-duplication)

13.4.6 Displaying a Single Fact
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Displays a single fact, placing each slot and its value on a separate
line. Optionally the logical name to which output is sent can be
specified and slots containing their default values can be excluded from
the output. If <logical-name> is **t** or unspecified, then output is
sent to the logical name **stdout**, otherwise it is sent to the
specified logical name. If <ignore-defaults-flag> is FALSE or
unspecified, then all of the fact’s slots are displayed, otherwise slots
with static defaults are only displayed if their current slot value
differs from their initial default value.

Syntax

(ppfact <fact-specifier> [<logical-name> [<ignore-defaults-flag>]])

Example

CLIPS> (clear)

CLIPS>

(deftemplate foo

(slot x (default 3))

(slot y)

(multislot z (default a b)))

CLIPS> (assert (foo))

<Fact-1>

CLIPS> (ppfact 1 t)

(foo

(x 3)

(y nil)

(z a b))

CLIPS> (ppfact 1 t TRUE)

(foo)

CLIPS> (modify 1 (y 2) (z c))

<Fact-2>

CLIPS> (ppfact 2 t TRUE)

(foo

(y 2)

(z c))

CLIPS>

13.5 Deffacts Commands
----------------------

The following commands manipulate deffacts.

13.5.1 Displaying the Text of a Deffacts
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Displays the text of a given deffacts. This function has no return
value.

Syntax

(ppdeffacts <deffacts-name>)

13.5.2 Displaying the List of Deffacts
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Displays the names of all deffacts stored in the CLIPS environment.

Syntax

(list-deffacts [<module-name>])

If <module-name> is unspecified, then the names of all deffacts in the
current module are displayed. If <module-name> is specified, then the
names of all deffacts in the specified module are displayed. If
<module-name> is the symbol \*, then the names of all deffacts in all
modules are displayed. This function has no return value.

13.5.3 Deleting a Deffacts
~~~~~~~~~~~~~~~~~~~~~~~~~~

This function deletes a previously defined deffacts.

Syntax

(undeffacts <deffacts-name>)

All facts listed in the deleted deffacts construct will no longer be
asserted as part of a reset. If the symbol \* is used for
<deffacts-name>, then all deffacts will be deleted (unless there exists
a deffacts named \*). The **undeffacts** command can be used to remove
deffacts at any time. Exceptions: When deffacts are being reset as part
of the **reset** command, they cannot be removed. This function has no
return value.

13.6 Defrule Commands
---------------------

The following commands manipulate defrules.

13.6.1 Displaying the Text of a Rule
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Displays the text of a given rule.

Syntax

(ppdefrule <rule-name>)

The **pprule** command can also be used for this purpose. This function
has no return value.

13.6.2 Displaying the List of Rules
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Displays the names of all rules stored in the CLIPS environment.

Syntax

(list-defrules [<module-name>])

If <module-name> is unspecified, then the names of all defrules in the
current module are displayed. If <module-name> is specified, then the
names of all defrules in the specified module are displayed. If
<module-name> is the symbol \*, then the names of all defrules in all
modules are displayed. This function has no return value.

13.6.3 Deleting a Defrule
~~~~~~~~~~~~~~~~~~~~~~~~~

This function deletes a previously defined defrule.

Syntax

(undefrule <defrule-name>)

If the defrule is in use (for example if it is firing), then the
deletion will fail. If the symbol \* is used for <defrule-name>, then
all defrule will be deleted (unless there is a defrule named \*). This
function has no return value.

13.6.4 Displaying Matches for a Rule
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For a specified rule, displays the list of the facts or instances which
match each pattern in the rule’s LHS, the partial matches for the rule,
and the activations for the rule. When listed as a partial match, the
*not*, *exists*, and *forall* CEs are shown as an asterisk. Other CEs
contained within these CEs are not displayed as part of the information
shown for a partial match. This function returns FALSE if the specified
rule does not exist or the function is passed invalid arguments,
otherwise a multifield value is returned containing three values: the
combined sum of the matches for each pattern, the combined sum of
partial matches, and the number of activations.

Syntax

(matches <rule-name> [<verbosity>])

where <verbosity> is either verbose, succinct, or terse. If <verbosity>
is not specified or <verbosity> is verbose, then output will include
details for each match, partial match, and activation. If <verbosity> is
succinct, then output will just include the total number of matches,
partial matches, and activations. If <verbosity> is terse, no output
will be displayed.

Example

The rule *matches-example-1* has three patterns and none are added by
CLIPS. Fact f-1 matches the first pattern, facts f-2 and f-3 match the
the second pattern, and fact f-4 matches the third pattern. Issuing the
run command will remove all of the rule’s activations from the agenda.

CLIPS> (clear)

CLIPS>

(defrule matches-example-1

(a ?)

(b ?)

(c ?)

=>)

CLIPS> (reset)

CLIPS> (assert (a 1) (b 1) (b 2) (c 1))

<Fact-4>

CLIPS> (facts)

f-0 (initial-fact)

f-1 (a 1)

f-2 (b 1)

f-3 (b 2)

f-4 (c 1)

For a total of 5 facts.

CLIPS> (run)

CLIPS>

The rule *matches-example-2* has three patterns. There are no matches
for the first pattern (since there are no *d* facts), facts f-2 and f-3
match the third pattern, and fact f-4 matches the forth pattern.

CLIPS>

(defrule matches-example-2

(not (d ?))

(exists (b ?x)

(c ?x))

=>)

CLIPS>

Listing the matches for the rule *matches-example-1* displays the
matches for the patterns indicated previously. There are two partial
matches which satisfy the first two patterns and two partial matches
which satisfy all three patterns. Since all of the rule’s activations
were allowed to fire there are none listed.

CLIPS> (matches matches-example-1)

Matches for Pattern 1

f-1

Matches for Pattern 2

f-2

f-3

Matches for Pattern 3

f-4

Partial matches for CEs 1 - 2

f-1,f-3

f-1,f-2

Partial matches for CEs 1 - 3

f-1,f-2,f-4

f-1,f-3,f-4

Activations

None

(4 4 0)

CLIPS>

Listing the matches for the rule *matches-example-2* displays the
matches for the patterns indicated previously. There is one partial
match which satisfies the first two CEs (the *not* pattern and the
*exists* CE). The \* indicates an existential match that is not
associated with specific facts/instances (e.g. the *not* CE is satisfied
because there are no *d* facts matching the pattern so \* is used to
indicate a match as there’s no specific fact matching that pattern).
Since none of the rule’s activations were allowed to fire they are
listed. The list of activations will always be a subset of the partial
matches for all of the rule’s CEs.

CLIPS> (matches matches-example-2)

Matches for Pattern 1

None

Matches for Pattern 2

f-2

f-3

Matches for Pattern 3

f-4

Partial matches for CEs 1 - 2

\*,f-2

\*,f-3

Partial matches for CEs 1 - 3

\*,f-2,f-4

Partial matches for CEs 1 (P1) , 2 (P2 - P3)

\*,\*

Activations

\*,\*

(3 4 1)

CLIPS>

If you just want a summary of the partial matches, specify succinct or
terse as the second argument to the function.

CLIPS> (matches matches-example-2 succinct)

Pattern 1: 0

Pattern 2: 2

Pattern 3: 1

CEs 1 - 2: 2

CEs 1 - 3: 1

CEs 1 (P1) , 2 (P2 - P3): 1

Activations: 1

(3 4 1)

CLIPS> (matches matches-example-2 terse)

(3 4 1)

CLIPS>

 13.6.5 Setting a Breakpoint for a Rule
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Sets a breakpoint for a given rule.

Syntax

(set-break <rule-name>)

If a breakpoint is set for a given rule, execution will stop prior to
executing that rule. At least one rule must fire before a break­point
will stop execution. This function has no return value.

13.6.6 Removing a Breakpoint for a Rule
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Removes a breakpoint for a given rule.

Syntax

(remove-break [<defrule-name>])

If no argument is given, then all breakpoints are removed. This function
has no return value.

13.6.7 Displaying Rule Breakpoints
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This command displays all the rules which have breakpoints set. This
function has no return value.

Syntax

(show-breaks [<module-name>])

If <module-name> is unspecified, then the names of all rules having
breakpoints in the current module are displayed. If <module-name> is
specified, then the names of all rules having breakpoints in the
specified module are displayed. If <module-name> is the symbol \*, then
the names of all rules having breakpoints in all modules are displayed.

13.6.8 Refreshing a Rule
~~~~~~~~~~~~~~~~~~~~~~~~

Places all current activations of a given rule on the agenda. This
function has no return value.

Syntax

(refresh <rule-name>)

13.6.9 Setting the Incremental Reset Behavior
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This function sets the incremental reset behavior. When this behavior is
enabled (TRUE by default), newly defined rules are updated based upon
the current state of the fact-list. When disabled (FALSE), newly defined
rules are only updated by facts added after the rule is defined. In
order to prevent rules from obtaining an inconsistent state, the
incremental reset behavior can only be changed when there are no rules
currently defined. The return value for this function is the old value
for the behavior.

Syntax

(set-incremental-reset <boolean-expression>)

13.6.10 Getting the Incremental Reset Behavior
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This function returns the current value of the incremental reset
behavior (TRUE or FALSE).

Syntax

(get-incremental-reset)

13.6.11 Determining the Logical Dependencies of a Pattern Entity
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The **dependencies** function lists the partial matches from which a
pattern entity receives logical support. This function has no return
value.

Syntax

(dependencies <fact-or-instance-specifier>)

The term <fact-or-instance-specifier> includes variables bound on the
LHS to fact-addresses or instance-addresses as described in section
5.4.1.8, the fact-index of the desired fact (e.g. 3 for the fact labeled
f-3), or the instance-name (e.g. [object]).

Example

| (defrule list-dependencies
| ?f <- (factoid $?)
| =>
| (dependencies ?f))

13.6.12 Determining the Logical Dependents of a Pattern Entity
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The **dependents** function lists all pattern entities which receive
logical support from a pattern entity. This function has no return
value.

Syntax

(dependents <fact-or-instance-specifier>)

The term <fact-or-instance-specifier> includes variables bound on the
LHS to fact-addresses or instance-addresses as described in section
5.4.1.8, the fact-index of the desired fact (e.g. 3 for the fact labeled
f-3), or the instance-name (e.g. [object]).

Example

| (defrule list-dependents
| ?f <- (factoid $?)
| =>
| (dependents ?f))

13.7 Agenda Commands
--------------------

The following commands manipulate agenda.

13.7.1 Displaying the Agenda
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Displays all activations on the agenda. This function has no return
value.

Syntax

(agenda [<module-name>])

If <module-name> is unspecified, then all activations in the current
module (not the current focus) are displayed. If <module-name> is
specified, then all activations on the agenda of the specified module
are displayed. If <module-name> is the symbol \*, then the activations
on all agendas in all modules are displayed.

13.7.2 Running CLIPS
~~~~~~~~~~~~~~~~~~~~

Starts execution of the rules. If the optional first argument is
positive, execu­tion will cease after the specified number of rule
firings or when the agenda con­tains no rule activations. If there are
no arguments or the first argument is a negative integer, execution will
cease when the agenda contains no rule activations. If the focus stack
is empty, then the MAIN module is automatically becomes the current
focus. The **run** command has no additional effect if evaluated while
rules are executing. Note that the number of rules fired and timing
information is no longer printed after the completion of the run command
unless the statistics item is being watched (see section 13.2). If the
rules item is being watched, then an informational message will be
printed each time a rule is fired. This function has no return value.

Syntax

(run [<integer-expression>])

13.7.3 Focusing on a Group of Rules
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Pushes one or more modules onto the focus stack. The specified modules
are pushed onto the focus stack in the reverse order they are listed.
The current module is set to the last module pushed onto the focus
stack. The current focus is the top module of the focus stack. Thus
(focus A B C) pushes C, then B, then A unto the focus stack so that A is
now the current focus. Note that the current focus is different from the
current module. Focusing on a module implies “remembering” the current
module so that it can be returned to later. Setting the current module
with the **set-current-module** function changes it without remembering
the old module. Before a rule executes, the current module is changed to
the module in which the executing rule is defined (the current focus).
This function returns a boolean value: FALSE if an error occurs,
otherwise TRUE.

Syntax

(focus <module-name>+)

13.7.4 Stopping Rule Execution
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The **halt** function may be used on the RHS of a rule to prevent
further rule firing. It is called without arguments. After **halt** is
called, control is returned from the **run** command. The agenda is left
intact, and execution may be continued with a **run** command. This
function has no return value.

Syntax

(halt)

13.7.5 Setting The Current Conflict Resolution Strategy
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This function sets the current conflict resolution strategy. The default
strategy is depth.

Syntax

(set-strategy <strategy>)

where <strategy> is either depth, breadth, simplicity, complexity, lex,
mea, or random. The old conflict resolution strategy is returned. The
agenda will be reordered to reflect the new conflict resolution
strategy.

13.7.6 Getting The Current Conflict Resolution Strategy
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This function returns the current conflict resolution strategy (depth,
breadth, simplicity, complexity, lex, mea, or random).

Syntax

(get-strategy)

13.7.7 Listing the Module Names on the Focus Stack
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The command **list-focus-stack** list all module names on the focus
stack. The first name listed is the current focus.

Syntax

(list-focus-stack)

13.7.8 Removing all Module Names from the Focus Stack
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The command **clear-focus-stack** removes all module names from the
focus stack.

Syntax

(clear-focus-stack)

13.7.9 Setting the Salience Evaluation Behavior
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This function sets the salience evaluation behavior. By default,
salience values are only evaluated when a rule is defined.

Syntax

(set-salience-evaluation <value>)

where <value> is either when-defined, when-activated, or every-cycle.
The return value for this function is the old value for salience
evaluation. The value when-defined forces salience evaluation at the
time of rule definition. The value when-activation forces salience
evaluation at the time of rule definition and upon being activated. The
value every-cycle forces evaluation at the time of rule definition, upon
being activated, and after every rule firing.

13.7.10 Getting the Salience Evaluation Behavior
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This function returns the current salience evaluation behavior
(when-defined, when-activated, or every-cycle).

Syntax

(get-salience-evaluation)

13.7.11 Refreshing the Salience Value of Rules on the Agenda
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This function forces reevaluation of saliences of rules on the agenda
regardless of the current salience evaluation setting. This function has
no return value.

Syntax

(refresh-agenda [<module-name>])

If <module-name> is unspecified, then the agenda in the current module
is refreshed. If <module-name> is specified, then the agenda in the
specified module is refreshed. If <module-name> is the symbol \*, then
the agenda in every module is refreshed.

13.8 Defglobal Commands
-----------------------

The following commands manipulate defglobals.

13.8.1 Displaying the Text of a Defglobal
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Displays the text required to define a given global variable. Note that
unlike other constructs such as deffacts and definstances, defglobal
definitions have no name associated with the entire construct. The
variable name passed to ppdefglobal should not include the question mark
or the asterisks (e.g. x is the variable name for the global variable
?*x*). This function has no return value.

Syntax

(ppdefglobal <global-variable-name>)

13.8.2 Displaying the List of Defglobals
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Displays the names of all defglobals. This function has no return value.

Syntax

(list-defglobals [<module-name>])

If <module-name> is unspecified, then the names of all defglobals in the
current module are displayed. If <module-name> is specified, then the
names of all defglobals in the specified module are displayed. If
<module-name> is the symbol \*, then the names of all defglobals in all
modules are displayed.

13.8.3 Deleting a Defglobal
~~~~~~~~~~~~~~~~~~~~~~~~~~~

This function deletes a previously defined defglobal.

Syntax

(undefglobal <defglobal-name>)

If the defglobal is in use (for example if it is referred to in a
deffunction), then the deletion will fail. Otherwise, no further uses of
the deleted defglobal are permitted (unless redefined). If the symbol \*
is used for <defglobal-name>, then all defglobals will be deleted
(unless there is a defglobal named \*). This function has no return
value.

13.8.4 Displaying the Values of Global Variables
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Displays the names and current values of all defglobals. This function
has no return value.

Syntax

(show-defglobals [<module-name>])

If <module-name> is unspecified, then the names and values of all
defglobals in the current module are displayed. If <module-name> is
specified, then the names and values of all defglobals in the specified
module are displayed. If <module-name> is the symbol \*, then the names
and values of all defglobals in all modules are displayed.

13.8.5 Setting the Reset Behavior of Global Variables
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This function sets the reset global variables behavior. When this
behavior is enabled (TRUE by default) global variables are reset to
their original values when the **reset** command is performed. The
return value for this function is the old value for the behavior.

Syntax

(set-reset-globals <boolean-expression>)

13.8.6 Getting the Reset Behavior of Global Variables
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This function returns the current value of the reset global variables
behavior (TRUE or FALSE).

Syntax

(get-reset-globals)

13.9 Deffunction Commands
-------------------------

The following commands manipulate deffunctions.

13.9.1 Displaying the Text of a Deffunction
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Displays the text of a given deffunction. This function has no return
value.

Syntax

(ppdeffunction <deffunction-name>)

13.9.2 Displaying the List of Deffunctions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Displays the names of all deffunctions stored in the CLIPS environment.
This function has no return value.

Syntax

(list-deffunctions)

13.9.3 Deleting a Deffunction
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This function deletes a previously defined deffunction.

Syntax

(undeffunction <deffunction-name>)

If the symbol \* is used for <deffunction-name>, then all deffunctions
will be deleted (unless there exists a deffunction called \*). The
undeffunction command can be used to remove deffunctions at any time.
Exceptions: A deffunction may not be deleted when it is executing or
when there is still a reference to it in another loaded construct, such
as a rule RHS. This function has no return value.

13.10 Generic Function Commands
-------------------------------

The following commands manipulate generic functions.

13.10.1 Displaying the Text of a Generic Function Header
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Displays the text of a given generic function header. This function has
no return value.

Syntax

(ppdefgeneric <generic-function-name>)

13.10.2 Displaying the Text of a Generic Function Method
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Displays the text of a given method.

Syntax

(ppdefmethod <generic-function-name> <index>)

where <index> is the method index (see section 8.4.2). This function has
no return value.

13.10.3 Displaying the List of Generic Functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Displays the names of all generic functions stored in the CLIPS
environment.

Syntax

(list-defgenerics [<module-name>])

If <module-name> is unspecified, then the names of all defgenerics in
the current module are displayed. If <module-name> is specified, then
the names of all defgenerics in the specified module are displayed. If
<module-name> is the symbol \*, then the names of all defgenerics in all
modules are displayed. This function has no return value.

13.10.4 Displaying the List of Methods for a Generic Function
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If no name is given, this function lists all generic function methods in
the CLIPS environment. If a name is given, then only the methods for the
named generic function are listed. The methods are listed in decreasing
order of precedence (see section 8.5.2) for each generic function.
Method indices can be seen using this function. This function has no
return value.

Syntax

(list-defmethods [<generic-function-name>])

13.10.5 Deleting a Generic Function
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This function deletes a previously defined generic function.

Syntax

(undefgeneric <generic-function-name>)

If the symbol \* is used for <generic-function-name>, then all generic
functions will be deleted (unless there exists a generic function called
\*). This function removes the header and all methods for a generic
function. The undefgeneric command can be used to remove generic
functions at any time. Exceptions: A generic function may not be deleted
when any of its methods are executing or when there is still a reference
to it in another loaded construct, such as a rule RHS. This function has
no return value.

13.10.6 Deleting a Generic Function Method
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This function deletes a previously defined generic function method.

Syntax

(undefmethod <generic-function-name> <index>)

where <index> is the index of the method to be deleted for the generic
function. If the symbol \* is used for <index>, then all the methods for
the generic function will be deleted. (This is different from the
undefgeneric command because the header is not removed.) If \* is used
for <generic-function-name>, then \* must also be specified for <index>,
and all the methods for all generic functions will be removed. This
function removes the specified method for a generic function, but even
if the method removed is the last one, the generic function header is
not removed. The undefmethod command can be used to remove methods at
any time. Exceptions: A method may not be deleted when it or any of the
other methods for the same generic function are executing. This function
has no return value.

13.10.7 Previewing a Generic Function Call
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This debugging function lists all *applicable* methods for a particular
generic function call in order of decreasing precedence (see section
8.5.2). The function **list-defmethods** is different in that it lists
*all* methods for a generic function.

Syntax

(preview-generic <generic-function-name> <expression>*)

This function does not actually execute any of the methods, but any
side-effects of evaluating the generic function arguments and any query
parameter restrictions (see section 8.4.3) in methods do occur. The
output for the first example in section 8.5.2 would be as follows:

Example

CLIPS> (preview-generic + 4 5)

+ #7 (INTEGER <qry>) (INTEGER <qry>)

+ #8 (INTEGER <qry>) (NUMBER)

+ #3 (INTEGER) (INTEGER)

+ #4 (INTEGER) (NUMBER)

+ #6 (NUMBER) (INTEGER <qry>)

+ #2 (NUMBER) (INTEGER)

+ #SYS1 (NUMBER) (NUMBER) ($? NUMBER)

+ #5 (NUMBER) (NUMBER) ($? PRIMITIVE)

CLIPS>

13.11 CLIPS Object-Oriented Language (COOL) Commands
----------------------------------------------------

The following commands manipulate elements of COOL.

13.11.1 Class Commands
~~~~~~~~~~~~~~~~~~~~~~

The following commands manipulate defclasses.

13.11.1.1 Displaying the Text of a Defclass
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Displays the text of a given defclass. This function has no return
value.

Syntax

(ppdefclass <class-name>)

13.11.1.2 Displaying the List of Defclasses
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Displays the names of all defclasses stored in the CLIPS environment. If
<module-name> is unspecified, then the names of all defclasses in the
current module are displayed. If <module-name> is specified, then the
names of all defclasses in the specified module are displayed. If
<module-name> is the symbol \*, then the names of all defclasses in all
modules are displayed. This function has no return value.

Syntax

(list-defclasses [<module-name>])

13.11.1.3 Deleting a Defclass
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This function deletes a previously defined defclass and all its
subclasses from the CLIPS environment.

Syntax

(undefclass <class-name>)

If the symbol \* is used for <class-name>, then all defclasses will be
deleted (unless there exists a defclass called \*). The undefclass
command can be used to remove defclasses at any time. Exceptions: A
defclass may not be deleted if it has any instances or if there is still
a reference to it in another loaded construct, such as a generic
function method. This function has no return value.

13.11.1.4 Examining a Class
^^^^^^^^^^^^^^^^^^^^^^^^^^^

This function provides a verbose description of a class including:
abstract role (whether direct instances can be created or not), direct
superclasses and subclasses, class precedence list, slots with all their
facets and sources, and all recognized message-handlers. This function
has no return value.

Syntax

(describe-class <class-name>)

Example

CLIPS>

(defclass CHILD (is-a USER)

(role abstract)

(multislot parents (cardinality 2 2))

(slot age (type INTEGER)

(range 0 18))

(slot sex (access read-only)

(type SYMBOL)

(allowed-symbols male female)

(storage shared)))

CLIPS>

(defclass BOY (is-a CHILD)

(slot sex (source composite)

(default male)))

CLIPS>

(defmessage-handler BOY play ()

(printout t "The boy is now playing..." crlf))

CLIPS> (describe-class CHILD)

================================================================================

\*******************************************************************************\*

Abstract: direct instances of this class cannot be created.

Direct Superclasses: USER

Inheritance Precedence: CHILD USER OBJECT

Direct Subclasses: BOY

--------------------------------------------------------------------------------

SLOTS : FLD DEF PRP ACC STO MCH SRC VIS CRT OVRD-MSG SOURCE(S)

parents : MLT STC INH RW LCL RCT EXC PRV RW put-parents CHILD

age : SGL STC INH RW LCL RCT EXC PRV RW put-age CHILD

sex : SGL STC INH R SHR RCT EXC PRV R NIL CHILD

Constraint information for slots:

SLOTS : SYM STR INN INA EXA FTA INT FLT

parents : + + + + + + + + RNG:[-oo..+oo] CRD:[2..2]

age : + RNG:[0..18]

sex : #

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

get-parents primary in class CHILD

put-parents primary in class CHILD

get-age primary in class CHILD

put-age primary in class CHILD

get-sex primary in class CHILD

\*******************************************************************************\*

================================================================================

CLIPS>

The following table explains the fields and their possible values in the
slot descriptions:

========= ================= =====================================================================
**Field** **Values**        **Explanation**
========= ================= =====================================================================
FLD          SGL/MLT           Field type (single-field or multifield)
DEF          STC/DYN/NIL       Default value (static, dynamic, or none)
PRP          INH/NIL           Propagation to subclasses (inheritable or not inheritable)
ACC          RW/R/INT          Access (read-write, read-only, or initialize-only)
STO          LCL/SHR           Storage (local or shared)
MCH          RCT/NIL           Pattern-match (reactive or non-reactive)
SRC          CMP/EXC           Source type (composite or exclusive)
VIS          PUB/PRV           Visibility (public or private)
CRT          R/W/RW/NIL        Automatically created accessors (read, write, read-write, or none)
OVRD-MSG     <message-name>    Name of message sent for slot-overrides in make-instance, etc.
SOURCE(S)    <class-name>+     Source of slot (more than one class for composite)
========= ================= =====================================================================

In the constraint information summary for the slots, each of the columns
shows one of the primitive data types . A *+* in the column means that
any value of that type is allowed in the slot. A *#* in the column means
that some values of that type are allowed in the slot. Range and
cardinality constraints are displayed to the far right of each slot's
row. The following table explains the abbreviations used in the
constraint information summary for the slots:

================ ================
**Abbreviation** **Explanation**
================ ================
SYM              Symbol
STR              String
INN              Instance Name
INA              Instance Address
EXA              External Address
FTA              Fact Address
INT              Integer
FLT              Float
RNG              Range
CRD              Cardinality
================ ================

13.11.1.5 Examining the Class Hierarchy
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This function provides a rudimentary display of the inheritance
relationships between a class and all its subclasses. Indentation
indicates a subclass. Because of multiple inheritance, some classes may
appear more than once. Asterisks mark classes which are direct
subclasses of more than one class. With no arguments, this function
starts with the root class OBJECT. This function has no return value.

Syntax

(browse-classes [<class-name>])

Example

CLIPS> (clear)

CLIPS> (defclass A (is-a USER))

CLIPS> (defclass B (is-a USER))

CLIPS> (defclass C (is-a A B))

CLIPS> (defclass D (is-a USER))

CLIPS> (defclass E (is-a C D))

CLIPS> (defclass F (is-a E))

CLIPS> (browse-classes)

OBJECT

PRIMITIVE

NUMBER

INTEGER

FLOAT

LEXEME

SYMBOL

STRING

MULTIFIELD

ADDRESS

EXTERNAL-ADDRESS

FACT-ADDRESS

INSTANCE-ADDRESS \*

INSTANCE

INSTANCE-ADDRESS \*

INSTANCE-NAME

USER

INITIAL-OBJECT

A

C \*

E \*

F

B

C \*

E \*

F

D

E \*

F

CLIPS>

13.11.2 Message-handler Commands
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following commands manipulate defmessage-handlers.

13.11.2.1 Displaying the Text of a Defmessage-handler
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Displays the text of a given defmessage-handler. This function has no
return value.

Syntax

Defaults are in **bold italics**.

(ppdefmessage-handler <class-name> <handler-name>

[<handler-type>])

<handler-type> ::= around \| before \| **primary** \| after

13.11.2.2 Displaying the List of Defmessage-handlers
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

With no arguments, this function lists all handlers in the system. With
one argument, this function lists all handlers for the specified class.
If the optional argument “inherit” is given, inherited message-handlers
are also included. This function has no return value.

Syntax

(list-defmessage-handlers [<class-name> [inherit]])

Example

List all primary handlers in the system.

CLIPS> (clear)

CLIPS> (defclass A (is-a USER))

CLIPS> (defmessage-handler A foo ())

CLIPS> (list-defmessage-handlers A)

foo primary in class A

For a total of 1 message-handler.

CLIPS> (list-defmessage-handlers A inherit)

init primary in class USER

delete primary in class USER

create primary in class USER

print primary in class USER

direct-modify primary in class USER

message-modify primary in class USER

direct-duplicate primary in class USER

message-duplicate primary in class USER

foo primary in class A

For a total of 9 message-handlers.

CLIPS>

13.11.2.3 Deleting a Defmessage-handler
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This function deletes a previously defined message-handler.

Syntax

Defaults are in **bold italics**.

(undefmessage-handler <class-name> <handler-name>

[<handler-type>])

<handler-type> ::= around \| before \| **primary** \| after

An asterisk can be used to specify a wildcard for any of the arguments.
(Wildcards will not work for the class name or handler name if there is
a class or handler called \*.) The undefmessage-handler command can be
used to remove handlers at any time. Exceptions: A handler may not be
deleted when it or any of the other handlers for the same class are
executing. This function has no return value.

Example

Delete all primary handlers in the system.

CLIPS> (undefmessage-handler \* \*)

CLIPS>

13.11.2.4 Previewing a Message
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Displays a list of all the applicable message-handlers for a message
sent to an instance of a particular class. The level of indentation
indicates the number of times a handler is shadowed, and lines connect
the beginning and ending portions of the execution of a handler if it
encloses shadowed handlers. The right double-angle brackets indicate the
beginning of handler execution, and the left double-angle brackets
indicate the end of handler execution. Message arguments are not
necessary for a preview since they do not dictate handler applicability.

Syntax

(preview-send <class-name> <message-name>)

Example

For the example in section 9.5.3, the output would be:

CLIPS> (preview-send USER my-message)

>> my-message around in class USER

\| >> my-message around in class OBJECT

\| \| >> my-message before in class USER

\| \| << my-message before in class USER

\| \| >> my-message before in class OBJECT

\| \| << my-message before in class OBJECT

\| \| >> my-message primary in class USER

\| \| \| >> my-message primary in class OBJECT

\| \| \| << my-message primary in class OBJECT

\| \| << my-message primary in class USER

\| \| >> my-message after in class OBJECT

\| \| << my-message after in class OBJECT

\| \| >> my-message after in class USER

\| \| << my-message after in class USER

\| << my-message around in class OBJECT

<< my-message around in class USER

CLIPS>

13.11.3 Definstances Commands
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following commands manipulate definstances.

13.11.3.1 Displaying the Text of a Definstances
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Displays the text of a given definstances. This function has no return
value.

Syntax

(ppdefinstances <definstances-name>)

13.11.3.2 Displaying the List of Definstances
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Displays the names of all definstances stored in the CLIPS environment.
This function has no return value.

Syntax

(list-definstances)

13.11.3.3 Deleting a Definstances
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This function deletes a previously defined definstances.

Syntax

(undefinstances <definstances-name>)

If the symbol \* is used for <definstances-name>, then all definstances
will be deleted (unless there exists a definstances called \*). The
undefinstances command can be used to remove definstances at any time.
Exceptions: A definstances may not be deleted when any of the instances
in it are being created. This function has no return value.

13.11.4 Instances Commands
~~~~~~~~~~~~~~~~~~~~~~~~~~

The following commands manipulate instances of user-defined classes.

13.11.4.1 Listing the Instances
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If no arguments are specified, all instances in scope of the current
module are listed. If a module name is given, all instances within the
scope of that module are given. If “*” is specified (and there is no
module named “*”), all instances in all modules are listed (only
instances which actually belong to classes of a module are listed for
each module to prevent duplicates). If a class name is specified, only
the instances for the named class are listed. If a class is specified,
then the optional keyword *inherit* causes this function to list
instances of subclasses of the class as well. This function has no
return value.

Syntax

(instances [<module-name> [<class-name> [inherit]]])

13.11.4.2 Printing an Instance’s Slots from a Handler
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This function operates implicitly on the active instance (see section
9.4.1.1) for a message, and thus can only be called from within the body
of a message-handler. This function directly prints the slots of the
active instance and is the one used to implement the print handler
attached to class USER (see section 9.4.4.3). This function has no
return value.

Syntax

(ppinstance)

13.11.4.3 Saving Instances to a Text File
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This function saves all instances in the CLIPS environment to the
specified file in the following format:

(<instance-name> of <class-name> <slot-override>*)

<slot-override> ::= (<slot-name> <single-field-value>*)

A slot-override is generated for every slot of every instance,
regardless of whether the slot currently holds a default value or not.
External-address and fact-address slot values are saved as strings.
Instance-address slot values are saved as instance-names. This function
returns the number of instances saved.

Syntax

(save-instances <file-name>

[local \| visible [[inherit] <class>+])

By default, save-instances saves only the instances of all defclasses in
the current module. Specifying **visible** saves instances for all
classes within scope of the current module. Also, particular classes may
be specified for saving, but they must be in scope according to the
local or visible option. The inherit keyword can be used to force the
saving of indirect instances of named classes as well (by default only
direct instances are saved for named classes). Subclasses must still be
in local or visible scope in order for their instances to be saved.
Unless the inherit option is specified, only concrete classes can be
specified. At least one class is required for the inherit option.

The file generated by this function can be loaded by either
**load-instances** or **restore-instances**. save-instances does not
preserve module information, so the instance file should be loaded into
the module which was current when it was saved.

13.11.4.4 Saving Instances to a Binary File
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The function **bsave-instances** works exactly like **save-instances**
except that the instances are saved in a binary format which can only be
loaded with the function **bload-instances**. The advantage to this
format is that loading binary instances can be much faster than loading
text instances for large numbers of instances. The disadvantage is that
the file is not usually portable to other platforms.

Syntax

(bsave-instances <file-name>

[local \| visible [[inherit] <class>+])

13.11.4.5 Loading Instances from a Text File
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This function loads instances from a file into the CLIPS environment. It
can read files created with save-instances or any ASCII text file. Each
instance should be in the format described in section 13.11.4.3
(although the instance name can be left unspecified). Calling
load-instances is exactly equivalent to a series of make-instance calls
(in CLIPS version 5.1, slot access restrictions, such as read-only, were
suspended during calls to load-instances). This function returns the
number of instances loaded or -1 if it could not access the instance
file.

Syntax

(load-instances <file-name>)

13.11.4.6 Loading Instances from a Text File without Message Passing
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The function **restore-instances** loads instances from a file into the
CLIPS environment. It can read files created with save-instances or any
ASCII text file. Each instance should be in the format described in
section 13.11.4.3 (although the instance name can be left unspecified).
It is similar in operation to load-instances, however, unlike
load-instances, restore-instances does not use message-passing for
deletions, initialization, or slot-overrides. Thus in order to preserve
object encapsulation, it is recommended that restore-instances only be
used with files generated by save-instances. This function returns the
number of instances loaded or -1 if it could not access the instance
file.

Syntax

(restore-instances <file-name>)

13.11.4.7 Loading Instances from a Binary File
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This function is similar to **restore-instances** except that it can
only work with files generated by **bsave-instances**. See section
13.11.4.4 for a discussion of the merits of using binary instance files.

Syntax

(bload-instances <file-name>)

13.12 Defmodule Commands
------------------------

The following commands manipulate defmodule constructs.

13.12.1 Displaying the Text of a Defmodule
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Displays the text of a given defmodule. This function has no return
value.

Syntax

(ppdefmodule <defmodule-name>)

13.12.2 Displaying the List of Defmodules
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Displays the names of all defmodule constructs stored in the CLIPS
environment. This function has no return value.

Syntax

(list-defmodules)

13.13 Memory Management Commands
--------------------------------

The following commands display CLIPS memory status information. CLIPS
memory management is described more fully in the *Advanced Pro­gramming
Guide*.

13.13.1 Determining the Amount of Memory Used by CLIPS
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Returns an integer representing the number of bytes CLIPS has currently
in-use or has held for later use. This number does not include operating
system overhead for allocating memory.

Syntax

(mem-used)

13.13.2 Determining the Number of Memory Requests Made by CLIPS
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Returns an integer representing the number of times CLIPS has requested
memory from the operating system. If the operating system overhead for
allocating memory is known, then the total memory used can be calculated
by

(+ (mem-used) (\* <overhead-in-bytes> (mem-requests)))

Syntax

(mem-requests)

13.13.3 Releasing Memory Used by CLIPS
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Releases all free memory held internally by CLIPS back to the operating
system. CLIPS will automatically call this function if it is running low
on memory to allow the operating system to coalesce smaller memory
blocks into larger ones. This function generally should not be called
unless the user knows *exactly* what he/she is doing (since calling this
function can prevent CLIPS from reusing memory efficiently and thus slow
down performance). This function returns an integer representing the
amount of memory freed to the operating system.

Syntax

(release-mem)

13.13.4 Conserving Memory
~~~~~~~~~~~~~~~~~~~~~~~~~

Turns on or off the storage of information used for **save** and pretty
print commands. This can save considerable memory in a large system. It
should be called *prior* to loading any constructs. This function has no
return value.

Syntax

(conserve-mem <value>)

where value is either **on** or **off**.

13.14 External Text Manipulation
--------------------------------

CLIPS provides a set of functions to build and access a hierarchical
lookup system for multiple external files. Each file contains a set of
text entries in a special format that CLIPS can later reference and
display. The basic concept is that CLIPS retains a “map” of the text
file in memory and can easily pull sections of text from the file
without having to store the whole file in memory and without having to
sequentially search the file for the appropriate text.

13.14.1 External Text File Format
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Each external text file to be loaded into CLIPS must be described in a
particular way. Each topic entry in each file must be in the format
shown following.

Syntax

<level-num> <entry-type> BEGIN-ENTRY- <topic-name>

•

•

Topic information in form to be displayed when referenced.

•

•

END-ENTRY

The delimiter strings (lines with BEGIN_ENTRY or END_ENTRY info) must be
the only things on their lines. Embedded white space between the fields
of the delimiters is allowed.

The first parameter, <level-num>, is the level of the hierarchical tree
to which the entry belongs. The lower the number, the closer to the root
level the topic is; i.e., the lowest level number indicates the root
level. Subtopics are indicated by making the level number of the current
topic larger than the previous entry (which is to be the parent). Thus,
the tree must be entered in the file sequentially; i.e., a topic with
all its subtopics must be described before going on to a topic at the
same level. Entering a number less than that of the previous topic will
cause the tree to be searched upwards until a level number is found
which is less than the current one. The current topic then will be
at­tached as a subtopic at that level. In this manner, multiple root
trees may be created. Level number and order of entry in a file can
indicate the order of precedence in which a list of subtopics that are
all children of the same topic will be searched. Topics with the same
level number will be searched in the order in which they appear in the
file. Topics with lower-level numbers will be searched first.

Example

| 0MBEGIN-ENTRY-ROOT
| -- Text --
| END-ENTRY
| 2IBEGIN-ENTRY-SUBTOPIC1
| -- Text --
| END-ENTRY
| 1IBEGIN-ENTRY-SUBTOPIC2
| -- Text --
| END-ENTRY

In the above example, SUBTOPIC1 and SUBTOPIC2 are children of ROOT.
However, in searching the children of ROOT, SUBTOPIC2 would be found
first.

The second parameter in the format defined above, the <entry-type>, must
be a single capital letter, either M (for MENU) or I (for INFORMATION).
Only MENU entries may have subtopics.

The third parameter defined above, the <topic-name>, can be any
alphanumeric string of up to 80 characters. No white space can be
embedded in the name.

Beginning a line with the delimiter “$$” forces the loader to treat the
line as pure text, even if one of the key delimiters is in it. When the
line is printed, the dollar signs are treated as blanks.

Example

| 0MBEGIN-ENTRY-ROOT1
| -- Root1 Text --
| END-ENTRY
| 1MBEGIN-ENTRY-SUBTOPIC1
| -- Subtopic1 Text --
| END-ENTRY
| 2IBEGIN-ENTRY-SUBTOPIC4
| -- Subtopic4 Text --
| END-ENTRY
| 1IBEGIN-ENTRY-SUBTOPIC2
| -- Subtopic2 Text --
| END-ENTRY
| 0IBEGIN-ENTRY-ROOT2
| -- Root2 Text --
| END-ENTRY
| -1MBEGIN-ENTRY-ROOT3
| -- Root3 Text --
| END-ENTRY
| 0IBEGIN-ENTRY-SUBTOPIC3
| -- Subtopic3 Text --
| END-ENTRY

Tree Diagram of Above Example :

| -> ROOT3 ---------> ROOT1 ---------> ROOT2
| \| / \\

| \| / \\
| V V V

| SUBTOPIC3 SUBTOPIC1 SUBTOPIC2
| \|
| \|
| V
| SUBTOPIC4

13.14.2 External Text Manipulation Functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following functions can be used by users to maintain their own
information system.

13.14.2.1 Fetch
^^^^^^^^^^^^^^^

The function **fetch** loads the named file (which must be in the format
defined in section 13.14.1) into the internal lookup table.

Syntax

(fetch <file-name>)

The function returns the number of entries loaded if the fetch
succeeded. If the file could not be loaded or was loaded already, the
function returns the symbol FALSE.

13.14.2.2 Print-region
^^^^^^^^^^^^^^^^^^^^^^

The function **print-region** looks up a specified entry in a particular
file which has been loaded previously into the lookup table and prints
the contents of that entry to the specified output.

Syntax

(print-region <logical-name> <file-name> <topic-field>*)

where <logical-name> is a name previously attached to an output device.
To send the output to stdout, specify t for the logical name.
<file-name> is the name of the previously loaded file in which the entry
is to be found, and the optional arguments, <topic-field>*, is the full
path of the topic entry to be found.

Each element or field in the path is delimited by white space, and the
function is not case sensitive. In addition, the entire name of a field
does not need to be specified. Only enough characters to distinguish the
field from other choices at the same level of the tree are necessary. If
there is a conflict, the function will pick the first one in the list. A
few special fields can be specified.

   ^ Branch up one level.

   ? When specified at the end of a path, this forces a display of the
   current menu, even on branch-ups.

   <nil> Giving no topic field will branch up one level.

The level of the tree for a file remains constant between calls to
**print-region**. All levels count from menu only. Information levels do
not count for branching up or down. To access an entry at the root level
after branching down several levels in a previous call or series of
calls, an equal number of branches up must be executed.

Examples

To display the entry for ROOT SUBTOPIC from the file foo.lis on the
screen, type

(print-region t "foo.lis" ROOT SUBTOPIC)

or, using less characters,

(print-region t "foo.lis" roo sub)

Only one entry can be accessed per **print-region** call. The function
returns the symbol TRUE if the print-region succeeded. If the entry was
not found, it returns FALSE.

CLIPS> (fetch "foo.lis")

7

CLIPS> (print-region t "foo.lis" roo sub)

-- Subtopic3 Text --

TRUE

CLIPS> (print-region t "foo.lis" "?")

-- Root3 Text --

TRUE

CLIPS> (print-region t "foo.lis" ^ root1 sub)

-- Subtopic1 Text --

TRUE

CLIPS> (print-region t "foo.lis" sub)

-- Subtopic4 Text --

TRUE

CLIPS> (print-region t "foo.lis" ^ subtopic2)

-- Subtopic2 Text --

TRUE

CLIPS> (print-region t "foo.lis" ^ root2)

-- Root2 Text --

TRUE

CLIPS> (toss "foo.lis")

TRUE

CLIPS>

13.14.2.3 Get-region
^^^^^^^^^^^^^^^^^^^^

The function **get-region** looks up a specified entry in a particular
file which has been loaded previously into the lookup table and returns
the contents of that entry as a string.

Syntax

(get-region <file-name> <topic-field>*)

where <file-name> is the name of the previously loaded file in which the
entry is to be found, and the optional arguments, <topic-field>*, is the
full path of the topic entry to be found. The **get-region** the
**print-region** functions share the same behavior for the special topic
fields and maintaining the level of the tree for a file between function
calls. If an error occurs, this function returns an empty string.

13.14.2.4 Toss
^^^^^^^^^^^^^^

The function **toss** unloads the named file from the internal lookup
table and releases the memory back to the system.

Syntax

(toss <file-name>)

The function returns the symbol TRUE if the toss succeeded. If the file
was not on the lookup table, it returns FALSE.

13.15 Profiling Commands
------------------------

The following commands provide the ability to profile CLIPS programs for
performance.

13.15.1 Setting the Profiling Report Threshold
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The **set-profile-percent-threshold** command sets the minimum
percentage of time that must be spent executing a construct or user
function for it to be displayed by the **profile-info** command. By
default, the percent threshold is zero, so all constructs or
user-functions that were profiled and executed at least once will be
displayed by the **profile-info** command. The return value of this
function is the old percent threshold.

Syntax

(set-profile-percent-threshold <number in the range 0 to 100>)

13.15.2 Getting the Profiling Report Threshold
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The **get-profile-percent-threshold** command returns the current value
of the profile percent threshold.

Syntax

(get-profile-percent-threshold)

13.15.3 Resetting Profiling Information
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The **profile-reset** command resets all profiling information currently
collected for constructs and user functions.

Syntax

(profile-reset)

13.15.4 Displaying Profiling Information
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The **profile-info** command displays profiling information currently
collected for constructs or user functions. Profiling information is
displayed in six columns. The first column contains the name of the
construct or user function profiled. The second column indicates the
number of times the construct or user function was executed. The third
column is the amount of time spent executing the construct or user
function. The fourth column is the percentage of time spent in the
construct or user function with respect to the total amount of time
profiling was enabled. The fifth column is the total amount of time
spent in the first execution of the construct or user function and all
subsequent calls to other constructs/user functions. The sixth column is
the percentage of this time with respect to the total amount of time
profiling was enabled.

Syntax

(profile-info)

13.15.5 Profiling Constructs and User Functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The **profile** command is used to enable/disable profiling of
constructs and user functions. If **constructs** are profiled, then the
amount of time spent executing deffunctions, generic functions, message
handlers, and the RHS of defrules is tracked. If **user-functions** are
profiled, then the time spent executing system and user defined
functions is tracked. System defined functions include predefined
functions available for your own use such as the **<** and **numberp**
functions in addition to low level internal functions which are not
available for your use (these will usually appear in **profile-info**
output in all capital letters or surrounded by parentheses). It is not
possible to profile constructs and user-functions at the same time.
Enabling one disables the other. The **off** keyword argument disables
profiling. Profiling can be repeatedly enable and disabled as long as
only one of **constructs** or **user-functions** is consistently
enabled. The total amount of time spent with profiling enabled will be
displayed by the **profile-info** command. If profiling is enabled from
the command prompt, it is a good idea to place the calls enabling and
disabling profiling within a single **progn** function call. This will
prevent the elapsed profiling time from including the amount of time
needed to type the commands being profiled.

Syntax

(profile constructs \| user-functions \| off)

Example

CLIPS> (clear)

CLIPS> (deffacts start (fact 1))

CLIPS>

(deffunction function-1 (?x)

(bind ?y 1)

(loop-for-count (\* ?x 10)

(bind ?y (+ ?y ?x))))

CLIPS>

(defrule rule-1

?f <- (fact ?x&:(< ?x 100))

=>

(function-1 ?x)

(retract ?f)

(assert (fact (+ ?x 1))))

CLIPS>

(reset)

CLIPS>

(progn (profile constructs)

(run)

(profile off))

CLIPS> (profile-info)

Profile elapsed time = 0.474921 seconds

Construct Name Entries Time % Time+Kids %+Kids

-------------- ------- ------ ----- --------- ------

\**\* Deffunctions \**\*

function-1 99 0.436704 91.92% 0.436704 91.92%

\**\* Defrules \**\*

rule-1 99 0.027561 5.80% 0.464265 97.72%

CLIPS> (profile-reset)

CLIPS> (reset)

CLIPS>

(progn (profile user-functions)

(run)

(profile off))

CLIPS> (profile-info)

Profile elapsed time = 12.0454 seconds

Function Name Entries Time % Time+Kids %+Kids

------------- ------- ------ ----- --------- ------

retract 99 0.007953 0.07% 0.010646 0.09%

assert 99 0.012160 0.10% 0.032766 0.27%

run 1 0.047421 0.39% 12.045301 100.00%

profile 1 0.000049 0.00% 0.000049 0.00%

\* 99 0.005579 0.05% 0.007610 0.06%

+ 49599 3.626217 30.10% 5.765490 47.86%

< 99 0.005234 0.04% 0.007749 0.06%

progn 49698 2.353003 19.53% 11.997880 99.61%

loop-for-count 99 1.481078 12.30% 11.910553 98.88%

PCALL 99 0.020747 0.17% 11.943234 99.15%

FACT_PN_VAR3 99 0.002515 0.02% 0.002515 0.02%

FACT_JN_VAR1 99 0.002693 0.02% 0.002693 0.02%

FACT_JN_VAR3 198 0.004718 0.04% 0.004718 0.04%

FACT_STORE_MULTIFIELD 99 0.005478 0.05% 0.012857 0.11%

PROC_PARAM 49599 1.036460 8.60% 1.036460 8.60%

PROC_GET_BIND 49500 1.102682 9.15% 1.102682 9.15%

PROC_BIND 49599 2.331363 19.35% 8.089474 67.16%

CLIPS> (set-profile-percent-threshold 1)

0.0

CLIPS> (profile-info)

Profile elapsed time = 12.0454 seconds

Function Name Entries Time % Time+Kids %+Kids

------------- ------- ------ ----- --------- ------

+ 49599 3.626217 30.10% 5.765490 47.86%

progn 49698 2.353003 19.53% 11.997880 99.61%

loop-for-count 99 1.481078 12.30% 11.910553 98.88%

PROC_PARAM 49599 1.036460 8.60% 1.036460 8.60%

PROC_GET_BIND 49500 1.102682 9.15% 1.102682 9.15%

PROC_BIND 49599 2.331363 19.35% 8.089474 67.16%

CLIPS> (profile-reset)

CLIPS> (profile-info)

CLIPS>
