## Test environments
* local Arch/Manjaro 21.0, R-4.0.4
* ubuntu 20.04 (via GitHub Actions), R-release
* ubuntu 20.04 (via GitHub Actions), R-devel
* windows latest (via GitHub Actions), R-release

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Reviewer comments

If there are references describing the methods in your package, please
add these in the description field of your DESCRIPTION file in the form
authors (year) <doi:...>
authors (year) <arXiv:...>
authors (year, ISBN:...)
or if those are not available: <https:...>
with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for
auto-linking.
(If you want to add a title as well please put it in quotes: "Title")

Please remove the quotes around function names. e.g.: `smooth_intens()`
--> smooth_intens()

> references and DOIs were added as suggested
> quotes were removed

Please add \value to .Rd files regarding exported methods and explain
the functions results in the documentation. Please write about the
structure of the output (class) and also what the output means. (If a
function does not return a value, please document that too, e.g.
\value{No return value, called for side effects} or similar)
Missing Rd-tags:
      run_app.Rd: \value

> @return/value was added as suggested

You write information messages to the console that cannot be easily
suppressed.
It is more R like to generate objects that can be used to extract the
information a user is interested in, and then print() that object.
Instead of print()/cat() rather use message()/warning()  or
if(verbose)cat(..) (or maybe stop()) if you really have to write text to
the console.
(except for print, summary, interactive functions)

> This refers to manage_lib.R; cat() was exchanged with message()
