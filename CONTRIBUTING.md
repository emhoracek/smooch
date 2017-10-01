# Contributing to Smooch

You want to work on Smooch? That's amazing! There are so many ways you can help,
and all of them would mean so much to this project.

## Contents

 * [Filing an issue](#filing-an-issue)
   * [KiSS set bugs](#kiss-set-bugs)
 * [Writing documentation](#writing-documentation)
 * [Contributing code](#contributing-code)
   * [Frontend](#frontend-javascripthtmlcss)
   * [Backend](#backend-haskell)
 * [Running Smooch on your computer](#run-smooch-on-your-computer)

## Filing an issue

You'll need to try and [get Smooch working on your own computer](https://github.com/emhoracek/smooch/blob/master/CONTRIBUTING.md#run-smooch-on-your-computer) first (for
now anyway), but one of the easiest ways you can contribute to Smooch is by
trying to load a set and see if it works!

Whether you were unable to compile `cel2pnm`, or dolls look weird in your
browser, or you can''t get the Smooch Haskell app to work locally -- you can
file an issue.

Even if you're pretty sure that the app *does* work and you just can't figure it out --
still file an issue! If installation directions aren't clear or don't work,
that's a bug in the documentation.

When filing a issues:
 * Please mention how you encountered the problem.
   * Were you looking at the demos on Github.io, at cel2pnm, at the JavaScript by itself, or
     at the Haskell app?
   * Are you using Windows, Linux, Mac, or some other OS?
   * If applicable, what browser were you using?
 * What are the steps I can take to make the problem happen again?
 * Describe what you were *expecting* to happen and what *actually* happened.

### KiSS set bugs

KiSS set bugs are a special category for Smooch.

Right now, Smooch *only supports* the first KiSS specification from 1992. It
doesn't support F(rench)KiSS, FKiSS2, FKiSS3.. etc, or Enhanced Palettes or
Cherry KiSS. I want to add editing capabilities before moving on to scripting
support, or fancy paletting that few dolls ever used.

If a supported set has a bug:
 * Please link to somewhere the set can be downloaded (such as the specific page
   on the Big KiSS Page)
 * Describe the problem and how it happened, and give your operating system
   (Windows, Linux, Mac, etc) and browser (Firefox, Chrome, IE, etc)
 * If the problem is visual, add a screenshot of the doll in a working viewer
   such as UltraKiSS or GnomeKiSS as well as a screenshot of the problem in
   Smooch.

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

#### Frontend (JavaScript/HTML/CSS)

You can work on the [JavaScript frontend](https://github.com/emhoracek/smooch/tree/master/javascript) of
Smooch.

 * Refactor and document the code so it's easier to understand
 * Improve performance
 * Move cel and palette parsing into JavaScript from cel2pnm`*`
 * Add editing capabilities `*`
   * Artists can change the layering of cels (raising or lowering in relation to other items)
   * Artists can change the offset of a cel in relation its object
   * Artists can change which palette a cel uses
   * Artists can edit a palette
   * *MAYBE* Artists can edit a cel with pixel art tools!
 * Add set creation capabilities `*`
   * Artists can upload a _cel sheet_ (an image with a solid or transparent background containing many cels)
   * Artists can select cels from the cel sheet and name them
   * Artists can insert cels into a set
   * Artists can group cels into objects
   * Artists can save a set 
   * Artists can publish sets to share with others

The ones marked with a `*` will need some work on the backend as well.

#### Backend (Haskell)

You can work on the [Haskell backend](https://github.com/emhoracek/smooch/tree/master/app) of Smooch.

 * Add a database/storage layer
   * Add users and limit uploading to users`*``
   * Store set data somewhere other than local filesystem
 * Figure out how to make handling LZH archives safe and secure.

Items maked a `*` will need work on the frontend as well.

## Run Smooch on your computer

If any of this is confusing or doesn't work -- don't hesitate to let me know by opening an issue @emhoracek and
I'll help you out.

## General Packages

Please make sure you have the following general software installed

| Required | Library | Version Range | Notes |
| ------------- | ------------- | ---| --- |
| ✔ | [GCC compiler](https://gcc.gnu.org/)  | >= 7.2 | GCC is the compiler for [GNU operating system](http://www.gnu.org/gnu/thegnuproject.html), which includes C, C++, Objective-C, Fortran, Ada, and Go |
| ✔ | [stack ](https://docs.haskellstack.org/en/stable/README/#the-haskell-tool-stack)  | >= 1.5.1 | `stack` is a Haskell dependency management tool |
|  | [Homebrew](https://brew.sh/)  | >= 1.3.4 | Homebrew is the missing package manager for macOS |

### Install `Homebrew`

Following the [Homebrew install guide here](https://brew.sh/).
* Open your terminal, copy and paste this command into your Terminal:
`/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"`

### Install `GCC`

You'll need a C compiler to compile the `cel2pnm` program for smooch, let's install the GCC compiler.

* On Mac OSX you can use `Homebrew` to install `gcc`, copy and past this command into your Terminal: `brew install gcc`
* On Windows / Linux or other OS, follow the [GCC installation guide](https://gcc.gnu.org/install/).

![alt text](https://preview.ibb.co/fmhcrG/brew_install.png "Brew install screenshot")

## Project setup

First, clone this project from Github

```
git clone https://github.com/emhoracek/smooch.git
cd smooch
```
![alt text](https://preview.ibb.co/exRHQb/gitclone.png "Git clone repo")


### Install `cel2pnm`
Now change to the `cel2pnm` directory.

* Now that you are in the top-level folder of this project, change into the `cel2pnm` directory: `cd cel2pnm`
* Compile Smooch's `cel2pnm` program by running this command:

```
gcc cel2pnm.c -o cel2pnm
```
![alt text](https://preview.ibb.co/npWqdw/compile_smooch.png "Compile cel2pnm program")

* Put `cel2pnm` in your [$PATH](https://askubuntu.com/questions/551990/what-does-path-mean).

### Install `stack`

You'll need `stack` to build Smooch. [`stack`](https://github.com/commercialhaskell/stack) is a Haskell dependency management
tool (kinda like `npm` for JavaScript). 

* Following the `stack` [install guide here](https://docs.haskellstack.org/en/stable/README/#how-to-install)
* Copy and paste this command into your Terminal:

```
curl -sSL https://get.haskellstack.org/ | sh
```

![alt text](https://preview.ibb.co/cYhc8w/install_stack.png "Install stack")

* Once you have `stack` installed, change to the `app` directory and run `stack setup`. 
This will install the correct Haskell version (this will take a while if you don't already have it).

![alt text](https://preview.ibb.co/dNNoFb/stack_setup.png "stack setup")

* Next, run `stack build` which will install project dependencies
![alt text](https://preview.ibb.co/iC64ow/stack_build.png "stack build")

* Next, run `stack install rivet-autoimporter`

That will compile the Smooch app.

### Setting up your database

Smooch uses a PostgreSQL database. You can find detailed installation guides on
the [PostgreSQL wiki](https://wiki.postgresql.org/wiki/Detailed_installation_guides).

Once Postgres is installed, create a user:

```
createuser -s smooch_user -W
```

This will ask for a password -- enter `111`. (This is only for local development
so it's okay to have a bad password like this.)

Next create two databases, one for development and one for testing:

```
createdb -O smooch_user smooch_devel
createdb -O smooch_user smooch_test
```

Now that you have two empty databases, you can set them up by running:

```
stack exec migrate devel up
stack exec migrate test up
```

### Running Smooch

Now you can run `stack exec smooch` and open `localhost:8000` in your browser.
You should see Smooch running!

You can also run tests with `stack test` or open a REPL with `stack ghci smooch:smooch`.
