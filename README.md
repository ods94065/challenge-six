# Challenge Six

The purpose of this exercise is to show whether implementing a
traditional business application in Common Lisp, using functional
programming techniques, offers significant reductions in source code
size from a more traditional imperative and procedural programming
approach. Performance, compiled code size, code difficulty, and other
considerations are not considered in this example.

The exercise was proposed in
[http://c2.com/cgi/wiki?ChallengeSixVersusFpDiscussion](the
ChallengeSixVersusFpDiscussion on Ward Cunningham's Wiki).

## Problem and Approach

The original source code implements a simple reporting tool in Visual
Basic and ASP. The user may log in and run reports with a
user-specified set of search variables. There are no fancy algorithms
here, no elaborate data structures: just simple code hooking up a
dynamic query mechanism to a web page.

The original code accepts a number of design and maintainability
limitations in the name of convenience and less code. It uses global
variables liberally, interleaves business logic and presentation, and
uses a totally unstructured approach to data that comes from accessing
SQL result sets directly &ndash; all aspects of web appliaction design
that I would not endorse if approaching a new project on my own
terms. Fixing these aspects, however, would increase the size of the
codebase substantially, and give limited insight into the advantages
or disadvantages of Lisp. Therefore, the Common Lisp implementation
also adheres to this structure, which gets us relatively close to an
apples-to-apples comparison. From this basis, I take whatever
functional and syntactic benefits Lisp allows.

Like I believe many Lisp programmers do, I have attempted to use
functional patterns and utilities judiciously, but I have by no means
attempted to make my code completely pure or functional. Comparisons
with other functional programming languages like Haskell, which
actually is quite a bit more streamlined than Lisp when it comes to
functional programming features, could yield additional interesting
fruit.

## Source Code Structure

* `src/vbasic` contains the original source code that came with the
  problem statement.
* `src/lisp` contains the Common Lisp implementation.

## Running the Example

The Common Lisp example was created for a multithreaded build of SBCL
running on a Mac OS X x86 platform; compatibility with other platforms
and Lisp implementations have not been tested. It requires the sqlite3
library for the database.

The easiest way to run the example is:

1. [Install Quicklisp](http://www.quicklisp.org/beta/) (by default
   this installs into `~/quicklisp`)

2. `cd ~/quicklisp/local-projects`

3. `ln -s ~/src/challenge-six` (or wherever you have cloned this repo)

4. Launch Lisp (I use SLIME in Emacs over SBCL)

5. From the Lisp interpreter, run: `(ql:quickload :challenge-six)` to
   load the program

6. Run: `(challenge-six:start-app "~/src/challenge-six" 8081)`
   (replacing the path with the path to your repo, and the port
   number with whatever port you desire). This creates the database
   and starts up the server.

7. Fire up a web browser and go to
   [http://localhost:8081/login](http://localhost:8081/login]). You
   should see a simple login page. Enter `test` as the user and
   `11111111` as the password, and you should see the report list
   page.
 
## Analysis

For the analysis, I stripped all single-line and multi-line comments, as
well as blank lines and docstrings. I didn't bother to clean up minor
discrepancies in code and names.

I'll start with the meat: the comparison of the implementation of
`/report/run`. Comparing `report.lisp` to `REPORT.ASP` yields:

<table>
  <tr><th>&nbsp;</th><th>VBasic</th><th>Lisp</th></tr>
  <tr><td>Lines</td><td>311</td><td>220</td></tr>
  <tr><td>Words</td><td>1405</td><td>1012</td></tr>
  <tr><td>Characters</td><td>10741</td><td>9101</td></tr>
</table>

This gives us a line savings of 29% and a word savings of 28% &ndash; Q.E.D!

However, this is too facile an analysis. Even considering the
similarities in approach, we face the following serious challenges in
comparing the two solutions:

* The Lisp platform I use does not have all the built-in libraries and
  capabilities of the Visual Basic platform. For example, the login
  page had to be coded up, and a handler function for the page had to
  be explicitly instantiated. That's because I chose `hunchentoot`
  for the Lisp web server, which is somewhat lower-level than ASP
  programmers typically operate at.
* I didn't completely take whitespace into account. Whitespace gets
  used very differently in a traditionally-written Lisp program than
  in Visual Basic, as you can see from the "left-side shape" of the
  code with respect to the left margin. I have tried to keep to
  reasonably standard Lisp coding style, but this is considerably more
  dense than non-Lisp programmers may be accustomed to &ndash;
  parentheses, unlike braces, are not generally given their own line
  breaks, and chaining forms together into a one-line expression is
  not considered as bad in terms of style as multiple-statement
  one-liner functions tend to be in procedural programming languages.
* I also didn't take into account that Lisp parentheses are used to
  close of code blocks instead of keywords. This decreases the
  Lisp word count, but tends to increase the number of characters
  since there are a lot of parentheses (see below). Depending on how
  you look at it, this could be considered a distraction from the
  desired comparison, which is the details of the code itself.
* A set of utility functions and macros were refactored to
  `controller.lisp`, `tags.lisp`, and `util.lisp`. The point of these
  utilities is to demonstrate some of the power of syntactic and
  functional abstractions. They will pay increasing dividends in code
  simplification the larger the codebase gets.

I think the exclusion of utilities provides a useful view into the
kinds of benefits that good Lisp design can bring to a
project. However, a full picture must take them into account. To
factor in the use of utilities, let's take a look at the projects
overall. For this, I exclude two Lisp files, `start-site.lisp` and
`login.lisp`, since they have no analog in the Visual Basic codebase,
and will not contribute useful information to the analysis at hand.

<table>
  <tr><th>&nbsp;</th><th>VBasic</th><th>Lisp</th></tr>
  <tr><td>Lines</td><td>383</td><td>351</td></tr>
  <tr><td>Words</td><td>1676</td><td>1519</td></tr>
  <tr><td>Characters</td><td>12848</td><td>14033</td></tr>
</table>

Even in this case, we still see a reduction of 8.4% on lines and 9.1%
on words. The primary additions on the Lisp side are from the
utilities `controller.lisp`, `tags.lisp`, and `util.lisp`, which
contribute 32% of the lines and 29% of the words.

The increase in characters has a simple explanation. If you subtract
opening and closing parentheses, the number of characters on the Lisp
side drops dramatically, to 12669. This is a strong indication that
Lisp programmers do not take into account the additional keystrokes
incurred by working in an S-expression grammar when they make claims
about the amount of coding a Lisp programmer has to do to solve a
problem. The syntax of Lisp appears to be one of the main reasons why
programmers choose _not_ to program in Lisp, so although it is not
especially germane to our analysis, it can't be overlooked when
weighing Lisp's pros and cons as a language.

Instead of trying to refine our counting technique any further, I
think it is more productive to focus on the coding constructs
themselves, and look at the Lisp features that offer ways of
streamlining and simplifying code expressions.

## Macros and Syntax Manipulation

Many of the benefits I get in terms of reducing code come from use of
Lisp's macro facility. Macros operate before the code is compiled, and
the values that they are passed are Lisp forms, represented as list
data. This distinguishes them from most macro facilities in
programming languages, where the values they are passed are simply
strings passed directly from the lexer. Macros are effectively used to
add new syntactic constructs to Lisp.

### HTML Generation

The _HTML generator macros_ from the `yaclml` library make it possible
to interleave HTML tags and Lisp code in a way that is quite natural
for a Lisp programmer. This is far safer, more readable, and more
hassle-free than the string-based concatenation approach exhibited in
the Visual Basic source (which is a bit silly, since it's embedded in
an ASP page!), and for the given example, it fits perfectly. 

The use of macros for HTML generation has several important
benefits:

1. It allows the attributes used by the elements to be checked at
   compile time.
2. It transforms at compile time into efficient printing code
   substituted for the HTML generator calls. Using the HTML generator
   macros incurs no runtime overhead.
3. It provides additional terseness over HTML templates when used
   in a markup-heavy situation (which is certainly the case in this
   example).
4. It embeds quite naturally into the existing tree structure of Lisp
   code and syntax.

The last point is important. It means that YACLML is effectively
extending Lisp's syntax with constructs that apply specifically to the
web programming domain.

Using HTML generators, however, is an approach that only makes sense
in certain projects. Larger web development projects are probably
going to want to use markup-based templates for presentation so that
web designers can work with them, and so that there is a clearer
demarcation between business logic and presentation.

The main disadvantage of using `yaclml` in this example is that it
does not lend itelf very easily to functional programming. Note the
prevalence of `dolist` iteration in the code instead of functional
constructs like `mapcar` or `reduce`. This is because we are not
building up a structure to transform or map over &ndash; we are simply
printing as we go.

I take advantage of the `deftag-macro` facility provided by `yaclml`
to simplify a number of the common HTML chunks used in our page. These
are contained in `tags.lisp`. `<hidden` and `<password` cover specific
types of input elements. Other macros wrap a body of stuff given to
them, like `<center` and `<simple-page`. These are different than
functions because they operate before the code is compiled, and the
values that they are passed are Lisp forms as list data.

### Hunchentoot Handlers

Hunchentoot, the library that provides the web server implementation
for this example, uses macros to define syntactic constructs of its
own. The `define-easy-handler` construct looks like a function
definition but adds HTTP request/response handling and URL routing to
the body of the function. Input parameters are covered with special
forms at the top of the function.

I extend this concept to a construct called `define-demo-handler`
which not only does all of the above, it covers establishing a DB
connection and handling login as well. Every request handler that is
defined with this macro picks up those capabilities for free.

The difference between this and simple function decomposition is its
ability to bind variables for the body of the construct to use, and
change the syntax of the construct. For normal functions and macros,
the function name would simply be given as the second argument to
`defun` or `defmacro`, but in this case the URL routing is so
important to the operation of the handler that it is given special
billing. Query parameters are handled in a separate form so that it's
clear that they're distinct from the name and URL. They are directly
bound to variables that get used in the following code.

Again, the purpose of these constructs is to provide a mechanism for
implementing handlers that is easy to use and designed specifically
for its problem domain.

### Simplifying Data Access and Other Shortcuts

Not all macros are designed as domain-specific extensions. Sometimes
macros simply provide cheap ways of streamlining code.

In the Lisp implementation, I use one of the simplest constructs Lisp
has for manipulating data: associative lists, or simply unordered
lists of key-value pairs. Lookup is slow, but for small collections
like this demo uses, it's acceptable. One of the problems with
associative lists, though, is that the syntax for manipulating them in
Common Lisp is clumsy: `(cdr (assoc :key alist))` accesses an item in
the list.

Using macros, I define a streamlined way of accessing items in an
associative list that arguably does even better than the `rs["key"]`
form which is used in the Visual Basic example. The macro `with-assoc`
takes an associative list, and a list of variables to be bound with
values from the associative list. The twist is: the _names of the
variables themselves_ form the keys we use when we look up the values!
This means the wrapped code can simply read elements from the
associative list as simple variables variables, with no further
hassle. This is the sort of shortcut that can only be written
effectively with the use of macros.

This is an example of a highly prevalent design pattern in Lisp:
`with-*` macros.  These are macros that take care of setup and
teardown of some objects that are used in the body of the macro, even
in exceptional cases. They make code considerably more concise, and
can often make it more robust as well. `with-slots`, for example, is a
standard Common Lisp macro very similar to `with-assoc` that binds
variables to slots (i.e. members) of an object for the wrapped code to
use. CLSQL's `with-transaction` makes sure each transaction ends with
a commit or a rollback.

For writing data, instead of repeatedly writing `(acons :key value
alist)` to add each key and value to the associative list, the
`assoc_insert` macro takes a set of variables that have already been
defined, and adds them to the associative list using the variable
names themselves as keys. It's fast but quite dirty, since you now
have to take care to name your variables the way you want them to
appear in the associative list, and the forms you pass to
`assoc-insert` can't be any more complicated than just variable
names. (I could make this more usable by allowing a form `(keyname
value)` to be passed to `assoc-insert` instead of just `variable`,
where you want or need to the variable name to be different, or you
want to plug in an arbitrary form to calculate the value.)

A few more shortcuts were taken to cut down on the noise: I use `<++`
as a shortcut to the clumsy `(concatenate 'string ...)` form, and
`query-zip` as a simple way to run a query and return the results in
rows of associative lists.

The main downside to macros, as for any form of functional
decomposition, is that, if the abstractions are unfamiliar or
overwrought, they will make the programmer's job harder, not easier,
since the prgrammer now has additional layers of code to
understand. Since macros are effectively manipulating syntax, creating
custom bindings and affecting what input gets evaluated when, they can
have particularly deep or surprising consequences for both the macro
creator and user. Also, macros can lead to cryptic and unintuitive
backtraces in runtime code when things go wrong.

## The Value of Expressions

Lisp code, like most functional programming code, tends to be
_expression-oriented_: most forms return a value. This generally lends
itself to tighter code. For example, things like the `if` special form
can be used on the right side of initializations, and the `format`
function (Lisp's corollary to C's `printf`) can return a string
directly. Instead of the common procedural technique of building up an
answer statement by statement and then returning the answer, good Lisp
style often has you doing both at the same time. For example, instead
of this C++ code:

    std::vector<string> x() {
        std::vector<string> r;
        r.push_back("foo");
        r.push_back("bar");
        r.push_back("baz");
        return r;
    }

the Lisp corollary looks like this:

    (defun x () (list "foo" "bar" "baz"))

When you have a bunch of value-oriented functions, it becomes easy to
chain them into more sophisticated expressions. The idea of
_functional composition_ is that you can visualize and write your
operation in terms of chaining a series of function calls together,
passing the output of one into the input of the other. This can lead
to some pretty powerful one-liners:

    (defun the-list-to-list (str)
      (cons "(any)" (mapcar #'trim (split-sequence #\, str))))

This takes a string of comma-separated items, splits them up into a
list, trims each item in the list (`mapcar` is covered in detail
below), adds `(any)` to the front of the list, and returns the whole
shebang.

Obviously, this technique, taken too far, can lead to madness, and
people will have different tolerance levels as to how many layers of
composition they can take in when reading code. Newcomers to
functional programming may well have trouble reading lines of code
like this, since they are after all doing a lot, and data flows from
right to left. But with the right building blocks, the resulting code
can be very compact and approachable.

## Higher-Level Programming

### Higher-Order Functions

The places in our example where functional programming techniques help
the most are actually in the utilities themselves, in
`util.lisp`. Lisp offers several higher-order facilities that make
transforming collections of items easier. In this example, I use just
one of them: `mapcar`.

`mapcar` provides an efficient way to transform one list of stuff into
another, where there is a 1-1 mapping of items in the input to
items in the output. For example, this:

    (defun zip-select-results (results columns)
      (let ((keys (mapcar #'key-symb columns)))
        (mapcar #'(lambda (vals) (mapcar #'cons keys vals))
	  results)))

This code does the following:

* Defines a function that coalesces all of its arguments into a
  string, then uppercases that string, then converts that string to a
  symbol in the keyword package, then returns the results of the
  symbol-making operation.
* Uses that function to turn a list of column names (as strings) into a
  list of keys.
* Creates a function that turns a list of values into a list of
  key-value pairs (i.e. an associative list), where the function picks
  up the set of keys automatically from the closure. (Here, I also use
  the fact that `mapcar` can take two lists and a function that takes
  two arguments. It iterates through both lists simultaneously and
  passes an item from each list to the transformation function.)
* Uses that function to convert a sequence of SQL query result rows
  into a sequence of associative lists.

Compare this to:

    (defun zip-select results (results columns)
      (let (zip-results)
        (dolist (row results)
          (let (zip-row)
	    (loop for i from 0 below (length row) do
              (push (cons (key-symb (nth columns)) (nth row)) zip-row))
            (push (nreverse zip-row) zip-results)))
        (nreverse zip-results)))

There are benefits in addition to code size:

* The use of `mapcar` abstracts away the details of iterating through
  multiple arrays, leading to safer code (what if you used `to`
  instead of `below`, or forgot to reverse the lists?).
* When a known pure function is passed as an argument, `mapcar` makes
  the purpose of the code clear: to transform one collection of items
  into another one. On the imperative side, loops can do anything, so
  one has to peer a bit deeper when reading and maintaining them.
* The use of `mapcar` also guides us into extracting the kernels of
  our loop into pure functions. Pure functions are easier to test, and
  tend to be easier to reuse, than loop bodies. They are also easy to
  parallelize, as the prevalence of map/reduce solutions in the big
  data space illustrates.
* Similar mapping techniques result in even leaner code when the
  details of iteration are more complicated (e.g. through trees).

The use of `mapcar`, however, also comes with downsides:

* The `mapcar` solution will tend to be a slower solution than
  iterative code because of the function calls &ndash; though in many
  situations this tradeoff is perfectly acceptable.
* Lisp makes it easy to pass a non-pure function to `mapcar`, which
  can make the code do something very different from the pure
  transformation that the `mapcar` function implies. Unless the
  function being passed is a lambda expression, the implementation
  details are often hidden at the point of the `mapcar` call (consider
  the call that uses `key-symb`), making its non-pure nature less
  obvious.

### Anonymous Functions

The use of `lambda` in the example above also deserves attention.
`lambda` allows us to define a small helper function on the spot,
without having to give it a name and place it in a different part of
the file. In functional programming little anonymous functions like
this are pretty common conveniences. Compare this to Java's current
closest analog to anonymous functions:

    Arrays.sort(arr, new Comparator<String>() {
        public int compare(String s1, String s2) {
            return s1.length - s2.length;
        }
    }

In Lisp, the corollary is something like:

    (sort arr #'(lambda (s1 s2) 
        (< (length s1) (length s2))))

though for this particular case there's an even more convenient option:

    (sort arr #'< :key #'length)

### Closures

Finally, note this detail from the first example:

    (lambda (vals) (mapcar #'cons keys vals))

Note that I didn't have to pass `keys` as an argument to the
function, even though it's clearly referenced by the code &ndash; its
value is determined by the surrounding context when the `lambda`
expression is evaluated! This is the power of _closures_ at work, and
can help avoid the kind of dumb errors you run into when threading
some set of variables from one function to another.

## TODOs

* The insertion of form data directly into the query string leaves us
  open to SQL injection. I've used an escaping function on input
  parameters to try to ameliorate this, but a cleaner and
  better-performing approach would use prepared statements and
  parameterized queries. Unfortunately, `clsql` does not seem to
  support this at present.

## Conclusion

I've shown that, at least notionally, you can write more compact
programs in Common Lisp than you can in Visual Basic, even for simple
examples. The abstractions and utilities developed in the Common Lisp
implementation can yield great benefits at scale as well.

Beyond this, I've shown some of the features of Lisp that contribute
to these benefits: syntactic manipulation and functional programming.
I've also covered some of the other benefits that functional
programming provides: correctness and safety of iteration, better
reusability of code, and clearer code intentions when reading code.

These are several ways, both big and little, that Lisp, and functional
programming, can improve programmer productivity. Lisp helps
programmers move up the abstraction ladder, away from the
straightforward but often-messy details of iterating through data,
towards higher-order function composition and data transformation, and
towards syntactic extensions that support the problem domains at hand.

Even in an example with only modest programming demands, such as the
example given, Lisp can still help you do more with less code.
