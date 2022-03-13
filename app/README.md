# The Haskell backend

Here's how the Haskell code is organized:

  * exe/Main runs the Warp webserver.
  * src/
    * Web does routing and request handling
    * Ctxt defines the request context and provides helper functions
    * ParseCNF parses the configuration file
    * ParseCel gets image data from cel files
    * ParseKCF gets color data from palette files
    * CelToPng coverts cels to pngs
    * Kiss contains the KiSS data types (and Aeson instances)
    * Shell converts the files (hmmm... maybe it should be
      called "Convert" instead?)
    * Upload processes uploaded sets
    * Users/
       * Model has the data type declaration and functions for querying users
       * View has functions for displaying user data
       * Controller has handlers and routes for users.
    * Dolls/
       * Model has the data type declaration and functions for querying dolls
       * View has functions for displaying doll data
       * Controller has handlers and routes for dolls.
  * tests/ contains spec tests
  * templates/ contains the templates for the pages
  * static/ contains the CSS and JavaScript resources. These
    are symlinks to the top level "javascript" directory, so
    that's where you should work on them.
    * static/sets/ is where the uploaded kiss sets are stored.
  * smooch.cabal is the Cabal file (lists all the libraries and
    modules that are used)
  * stack.yaml is the Stack file (helps manage dependencies)

To work on this codebase, follow the directions in the [CONTRIBUTING](https://github.com/emhoracek/smooch/blob/master/CONTRIBUTING.md#run-smooch-on-your-computer) file.

If you have *any issues at all* getting started with Smooch, it would be
extremely helpful to me if you filed a ticket in the "Issues" section on
Github and tagged me (@emhoracek)! I want to know about any
obstacles anyone is facing in using Smooch -- if you're having trouble,
someone else probably will, too!

## Current state of the app

Right now, a user is greeted with an invitation to create an account,
login, or view a doll. This has been moved to `old-index.tpl` bc
there are some bugs with sessions.

The user can view a doll by entering an OtakuWorld link or uploading
a file. The app checks if the link has already been converted and
if not it downloads it. The app hashes the file and checks if
the file has already been converted.

To a convert a doll, it's unzipped, the configuration is parsed into JSON, and
all the ".cel" images are converted to pngs. Then a response with all the
images, the JSON, and the KiSS javascript is served to the user.

## Sample dolls

A few sample dolls are now included in the repo in
"/haskell/tests/samples/". These dolls were mostly chosen for lack of
advanced FKiSS features, non-graphic nudity or no nudity at all (for
ease of working on this in public ;) ), and (with the exceptions noted
below) small file size.

  * arwen.lzh - [Arwen](http://otakuworld.com/kiss/dolls/pages/a/arwen.htm) by
    Anime Craze and Aragonite. (A larger doll.)
  * boredom.lzh --
    [Boredom](http://otakuworld.com/kiss/dolls/pages/b/boredom.htm) by
    Jade Gordon.
  * lina.lzh -- [Lina
    Inverse](http://otakuworld.com/kiss/dolls/pages/l/lina.htm) by
    Anna.
  * lucca.lzh -- [Lucca](http://otakuworld.com/kiss/dolls/pages/l/lucca1r.htm)
    by Bryan O'Malley. (Another larger doll.)
  * washua.lzh -- [Washu](http://otakuworld.com/kiss/dolls/pages/w/washua.htm)
    by Alison.
  * sk_kimux.lzh -- [Mini-Skuld](http://otakuworld.com/kiss/dolls/pages/s/sk_kimux.htm)
    by MUX. (A Japanese doll.)

It bothers me that I don't have permission from the artists to use
these dolls, but if I only used the dolls of artists I knew how to
reach, I would have no non-FKiSS dolls to use for testing. :/ If any
of these artists finds this repo and would like their work removed, I
am happy to do so. Email me at libby@daydrea.me.

## Running the app

### General Packages

Please make sure you have the following tools installed

| Required | Library | Version Range | Notes |
| ------------- | ------------- | ---| --- |
| ✔ | [GCC compiler](https://gcc.gnu.org/)  | >= 7.2 | GCC is the compiler for [GNU operating system](http://www.gnu.org/gnu/thegnuproject.html), which includes C, C++, Objective-C, Fortran, Ada, and Go |
| ✔ | [stack](https://docs.haskellstack.org/en/stable/README/#the-haskell-tool-stack)  | >= 1.5.1 | `stack` is a Haskell dependency management tool |
|  | [Homebrew](https://brew.sh/)  | >= 1.3.4 | Homebrew is the missing package manager for macOS |
|  | [lhasa](https://fragglet.github.io/lhasa/) |  | Lhasa decompresses .lzh (LHA / LHarc) and .lzs (LArc) archives |

#### Install `GCC`

You'll need a C compiler when running `stack setup`, let's install the GCC compiler.

* On Mac OSX you can use `Homebrew` to install `gcc`, copy and past this command into your Terminal: `brew install gcc`
* On Windows / Linux or other OS, follow the [GCC installation guide](https://gcc.gnu.org/install/).

![alt text](https://preview.ibb.co/fmhcrG/brew_install.png "Brew install screenshot")

#### Install `Homebrew`

Following the [Homebrew install guide here](https://brew.sh/).
* Open your terminal, copy and paste this command into your Terminal:
`/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"`

#### Install `lhasa`

* On Mac OSX you can use `Homebrew` to install `lhasa`, copy and past this command into your Terminal: `brew install lhasa`

#### Other dependencies

You'll also need libgmp3-dev, libpq-dev, libicu-dev, and zlib1g-dev.

### Project setup

1. [Fork](https://help.github.com/articles/fork-a-repo/) this project, clone your fork, and add the original repo as a remote:

   ```bash
   # Clone your fork of the repo
   git clone https://github.com/<your-username>/smooch
   # Navigate to the newly cloned directory
   cd smooch
   # Assign the original repo to a remote called "upstream"
   git remote add upstream https://github.com/emhoracek/smooch.git
   ```

    *NOTE*: You can type `git remote -v` to check which repositories your origin and upstream are pointing to.

2. Whenever you want to sync your fork with the original repo, do the following:

    ```bash
   git checkout master
   git pull upstream master
   git push
   ```
3. If you want to work on an issue, create a new topic branch (off of `master`) to contain your feature, change,
   or fix.

   **IMPORTANT**: Making changes in `master` is discouraged. You should always
   keep your local `master` in sync with upstream `master` and make your
   changes in feature branches.

   ```bash
   git checkout -b <topic-branch-name>
   ```
4. Commit your changes in logical chunks. Keep your commit messages organized, with a short description in the first line and more detailed information on the following lines.

5. Push your topic branch up to your fork:

   ```bash
   git push origin <topic-branch-name>
   ```

6. [Open a Pull Request](https://help.github.com/articles/about-pull-requests/) with a clear title and description.

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

* Run `stack install rivet-autoimporter`, this is a tool for migrating the database.

* Finally, run `stack build` which will install project dependencies and compile the Smooch app.
![alt text](https://preview.ibb.co/iC64ow/stack_build.png "stack build")

### Setting up your database

Smooch uses a PostgreSQL database. You can set up your databases automatically
with Docker Compose or manually.

#### Docker Compose

If you use Docker, you can use Docker Compose to set up your database. Just run
`docker-compose up` to start up a PostgreSQL container with all the needed users and
databases.

Then, to add the tables, run:

```
stack exec migrate devel up
stack exec migrate test up
```

#### Manual setup

You can find detailed installation guides on
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

Now you can run `ENV=devel stack exec smooch` and open `localhost:8000` in your browser.
You should see Smooch running!

You can also run tests with `stack test` or open a REPL with `stack ghci smooch:smooch`.
