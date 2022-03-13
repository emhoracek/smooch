# Contributing to Smooch

You want to work on Smooch? That's amazing! There are so many ways you can help,
and all of them would mean so much to this project.

## Contents

 * [Filing an issue](#filing-an-issue)
   * [KiSS set bugs](#kiss-set-bugs)
 * [Writing documentation](#writing-documentation)
 * [Contributing code](#contributing-code)
 * [Running the frontend](#frontend-javascripthtmlcss)
 * [Running Smooch on your computer](#running-the-backend)

## Filing an issue

You'll need to try and [get Smooch working on your own computer](https://github.com/emhoracek/smooch/blob/master/CONTRIBUTING.md#run-smooch-on-your-computer) first (for
now anyway), but one of the easiest ways you can contribute to Smooch is by
trying to load a set and see if it works!

Whether dolls look weird in your browser, or you can''t get the Smooch Haskell
app to work locally -- you can file an issue.

Even if you're pretty sure that the app *does* work and you just can't figure it out --
still file an issue! If installation directions aren't clear or don't work,
that's a bug in the documentation.

When filing a issues:
 * Please mention how you encountered the problem.
   * Were you looking at the demos on Github.io, at the JavaScript by itself,
     or at the Haskell app?
   * Are you using Windows, Linux, Mac, or some other OS?
   * If applicable, what browser were you using?
 * What are the steps I can take to make the problem happen again?
 * Describe what you were *expecting* to happen and what *actually* happened.

### KiSS set bugs

KiSS set bugs are a special category for Smooch.

One really helpful contribution you can make is to try out KiSS dolls from
[Okatuworld](http://www.otakuworld.com/kiss) and see which ones work
and which don't. I would look for dolls that are as simple as possible
-- a single palette, a small wardrobe, no animations or special
effects (no "FKiSS"). (Please be aware that many dolls on Otakuworld are
not appropriate for children or even worksafe.)

Try the doll you've chosen in a KiSS doll viewer like [GnomeKiSS for
Linux](http://devel.tlrmx.org/kiss/), [Direct KiSS or PlayKiSS for
Windows](http://otakuworld.com/index.html?/kiss/viewers.htm) or
[UltraKiSS for anything with
Java](http://www.wmiles.com/projects/ultrakiss). Then, fire up Smooch
and try uploading the doll. The doll may not look right or it
may cause an error in the Haskell or Javascript!

If you can add an issue describing the problems with the doll and
perhaps even start investigating what is causing the problem that
would be super helpful <3 <3 <3

If a supported set has a bug:
 * Please link to somewhere the set can be downloaded (such as the specific page
   on the Big KiSS Page)
 * Describe the problem and how it happened, and give your operating system
   (Windows, Linux, Mac, etc) and browser (Firefox, Chrome, IE, etc)
 * If the problem is visual, add a screenshot of the doll in a working viewer
   such as UltraKiSS or GnomeKiSS as well as a screenshot of the problem in
   Smooch.

If the doll is small and has lots of interesting problems, it
may be a good candidate for being included in the repo as a
sample doll.


## Writing documentation

You can write about how to use Smooch or how to contribute to Smooch. Or, you
could write about the Kisekae Set System in general, or its history or anything.

Q: But what if I don't know anything about Smooch or Kisekae?

A: You can still help! Ask any questions that you have! If you have a question,
somebody else probably has that question too. Many projects encourage people to
ask questions in another channel -- for very good reason -- but I would prefer
if you asked by creating an [issue](https://github.com/emhoracek/smooch/issues). If you don't feel comfortable asking in
public, you're also welcome to send an email to libby@daydrea.me!

## Contributing code

Pick an [issue](https://github.com/emhoracek/smooch/issues), or below are some more general things you can work on.

Please comment on the issue you want to work on to claim it *before* you start
work. That way you can take your time working instead of racing to get the first
pull request in.

Don't see an interesting issue, or all the issues are claimed? Below are some more ideas. If any looks interesting, please create an issue for it so we can discuss different approaches and so other people can know that you're working on it.

### Ideas

 * Refactor and document the code so it's easier to understand
 * Improve performance
 * Fix [issues](https://github.com/emhoracek/smooch/issues)
 * Add features from the [roadmap](https://github.com/emhoracek/smooch/blob/master/README.md#roadmap)

## Running the frontend (JavaScript)

You can work on the [JavaScript frontend](https://github.com/emhoracek/smooch/tree/master/javascript) of
Smooch separately from the backend. The README has instructions.

## Running the backend

You can work on the [Haskell backend](https://github.com/emhoracek/smooch/tree/master/app) of
Smooch separately from the fronted. The README has instructions.
