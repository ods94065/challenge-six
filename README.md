# Challenge Six

The purpose of this example is to show whether implementing a
real-world example in Common Lisp offers any improvement over a more
mainstream programming language like Visual Basic.

## Source Code Structure

   * `src/vbasic` contains the original source code that came with the problem statement.
   * `src/lisp` contains a Common Lisp implementation.

## Running the Example

The Common Lisp example was created for a multithreaded build of SBCL
running on a Mac OS X x86 platform; compatibility with other platforms
and Lisp implementations have not been tested.

The easiest way to run the example is:

   1. Install Quicklisp from quicklisp.org (by default this installs into `~/quicklisp`)
   2. `cd ~/quicklisp/local-projects`
   3. `ln -s ~/src/challenge-six` (or wherever you have cloned this repo)
   4. Launch Lisp (I use SLIME in Emacs over SBCL)
   5. From the Lisp interpreter, run: `(ql:quickload :challenge-six)` to load the program
   6. Run: `(challenge-six:start-site "~/src/challenge-six" 8081)`
      (replacing the path with the path to your repo, and the port
      number with whatever port you desire)
   7. Fire up a web browser and go to http://localhost:8081/login. You
      should see a simple login page. Enter `test` as the user and
      `11111111` as the password, and you should see the report list page.

## Analysis

`wc report.lisp` on my system currently yields 246 lines, 1087 words, and 10.2K characters.

`wc REPORT.ASP` on my system currently yields 370 lines, 1561 words, and 12.3K characters.

This gives us a line savings of 33.5% and a word savings of 30.4% &ndash; quod erat demonstrandum!

Obviously, this is way too facile an analysis. A precise analysis is impossible because of the following vagaries:

   * The Lisp platform does not have all the built-in capabilities of
     the Visual Basic platform. For example, the login page had to be
     coded up.
   * The web server and framework that Lisp uses is very different from
     Visual Basic's. In many ways it's lower level.
   * What constitutes a word in the Lisp world is drastically
     different than in the Visual Basic world, and neither is
     necessarily well-represented by `wc`.
   * We didn't take whitespace or comments into account. Whitespace gets
     used very differently in a traditionally-written Lisp program than
     in Visual Basic, as you can see from the "left-side shape" of the code
     with respect to the left margin. Comments were added to
     `util.lisp`.
   * A set of custom HTML tags were refactored to `tags.lisp`. That
     adds 30 lines, 132 words, and 1190 characters right there.
   * A set of utility functions were refactored to `util.lisp`. It's
     not clear to what extent these should be counted, as many of
     these are generic utilities that come from other Lisp projects.
     At any rate, these utilities show off some of the power of
     syntactic and functional abstractions.

If we chose to count `util.lisp`, `tags.lisp`, and `packages.lisp` in
addition to `report.lisp` and `report-list.lisp`. Stacking that up
against all the ASP yields a similar number of lines, 14% more words,
and 25% more characters &ndash; yikes! But this is lying as badly as
the analysis above.

Ultimately, trying to compare code size, i.e. the "amount of coding",
between the two platforms in any numerical way is a fool's errand
&ndash; the two platforms are simply apples-and-oranges different from
each other. It is more profitable to look at the benefits we were able
to take advantage of in the Lisp implementation, and get an idea of
how that might benefit larger projects or continuations of this
project.

The critique that spawned this project was aimed at functional
programming in general. However, Common Lisp is a multiparadign
language &ndash; in fact, is one of the most protean languages yet
invented! &ndash; so not all of the features we take advantage of in
this example are going to relate specifically to functional
programming. Comparisons with other functional programming languages
like Haskell might be worthwhile as well.
