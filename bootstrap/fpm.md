# NAME

**fpm**(1) - A Fortran package manager and build system

# SYNOPSIS

**fpm** *SUBCOMMAND* \[SUBCOMMAND\_OPTIONS\]

**fpm** **--help**|**--version**|**--list**

# DESCRIPTION

**fpm**(1) is a package manager that helps you create Fortran projects
from source -- it automatically determines dependencies\!

Most significantly **fpm**(1) lets you draw upon other **fpm**(1)
packages in distributed **git**(1) repositories as if the packages were
a basic part of your default programming environment, as well as letting
you share your projects with others in a similar manner.

All output goes into the directory "build/" which can generally be
removed and rebuilt if required. Note that if external packages are
being used you need network connectivity to rebuild from scratch.

# SUBCOMMANDS

Valid **fpm**(1) subcommands are:

  - build Compile the packages into the "build/" directory.

  - new Create a new Fortran package directory with sample files.

  - update Update the project dependencies.

  - run Run the local package binaries. Defaults to all binaries for
    that release.

  - test Run the tests.

  - help Alternate to the **--help** switch for displaying help text.

  - list Display brief descriptions of all subcommands.

  - install Install project.

  - clean Delete directories in the "build/" directory, except
    dependencies. Prompts for confirmation to delete.

Their syntax is

``` 
    build [--profile PROF] [--flag FFLAGS] [--list] [--compiler COMPILER_NAME]
          [--tests]
    new NAME [[--lib|--src] [--app] [--test] [--example]]|
             [--full|--bare][--backfill]
    update [NAME(s)] [--fetch-only] [--clean]
    run [[--target] NAME(s)] [--profile PROF] [--flag FFLAGS] [--list] [--all]
        [--example] [--runner "CMD"] [--compiler COMPILER_NAME] [-- ARGS]
    test [[--target] NAME(s)] [--profile PROF] [--flag FFLAGS] [--list]
         [--runner "CMD"] [--compiler COMPILER_NAME] [-- ARGS]
    help [NAME(s)]
    list [--list]
    install [--profile PROF] [--flag FFLAGS] [--no-rebuild] [--prefix PATH]
            [options]
    clean [--skip] [--all]
```

# SUBCOMMAND OPTIONS

**-C**, **--directory** PATH Change working directory to PATH before
running any command

  - ****--profile** PROF**  
    selects the compilation profile for the build. Currently available
    profiles are "release" for high optimization and "debug" for full
    debug options. If **--flag** is not specified the "debug" flags are
    the default.

  - ****--compiler** NAME**  
    Specify a compiler name. The default is "gfortran" unless set by the
    environment variable FPM\_FC.

  - ****--c-compiler** NAME**  
    Specify the C compiler name. Automatically determined by default
    unless set by the environment variable FPM\_CC.

  - ****--archiver** NAME**  
    Specify the archiver name. Automatically determined by default
    unless set by the environment variable FPM\_AR.

  - ****--flag****  
    FFLAGS selects compile arguments for the build, the default value is
    set by the FPM\_FFLAGS environment variable. These are added to the
    profile options if **--profile** is specified, else these these
    options override the defaults. Note object and .mod directory
    locations are always built in.

  - ****--c-flag** CFLAGS**  
    selects compile arguments specific for C source in the build. The
    default value is set by the FPM\_CFLAGS environment variable.

  - ****--link-flag** LDFLAGS**  
    select arguments passed to the linker for the build. The default
    value is set by the FPM\_LDFLAGS environment variable.
    
      - ****--list****  
        List candidates instead of building or running them. On the
        **fpm**(1) command this shows a brief list of subcommands.
    
      - ****--runner** CMD**  
        Provides a command to prefix program execution paths.
    
      - **-- ARGS**  
        Arguments to pass to executables.
    
      - ****--skip****  
        Delete directories in the build/ directory without prompting,
        but skip dependencies.
    
      - ****--all****  
        Delete directories in the build/ directory without prompting,
        including dependencies.

# VALID FOR ALL SUBCOMMANDS

  - ****--help****  
    Show help text and exit

  - ****--verbose****  
    Display additional information when available

  - ****--version****  
    Show version information and exit.

@file You may replace the default options for the **fpm**(1) command
from a file if your first options begin with @file. Initial options will
then be read from the "response file" "file.rsp" in the current
directory.

> If "file" does not exist or cannot be read, then an error occurs and
> the program stops. Each line of the file is prefixed with "options"
> and interpreted as a separate argument. The file itself may not
> contain @file arguments. That is, it is not processed recursively.
> 
> For more information on response files see
> 
> ``` 
>       https://urbanjost.github.io/M_CLI2/set_args.3m_cli2.html
> ```
> 
> The basic functionality described here will remain the same, but other
> features described at the above reference may change.
> 
> An example file:
> 
> ``` 
>      # my build options
>      options build
>      options --compiler gfortran
>      options --flag "-pg -static -pthread -Wunreachable-code -Wunused
>       -Wuninitialized -g -O -fbacktrace -fdump-core -fno-underscoring
>       -frecord-marker=4 -L/usr/X11R6/lib -L/usr/X11R6/lib64 -lX11"
> ```
> 
> Note **--flag** would have to be on one line as response files do not
> (currently) allow for continued lines or multiple specifications of
> the same option.

# ENVIRONMENT VARIABLES

  - **FPM\_FC**  
    sets the path to the Fortran compiler used for the build, will be
    overwritten by **--compiler** command line option

  - **FPM\_FFLAGS**  
    sets the arguments for the Fortran compiler will be overwritten by
    **--flag** command line option

  - **FPM\_CC**  
    sets the path to the C compiler used for the build, will be
    overwritten by **--c-compiler** command line option

  - **FPM\_CFLAGS**  
    sets the arguments for the C compiler will be overwritten by
    **--c-flag** command line option

  - **FPM\_AR**  
    sets the path to the archiver used for the build, will be
    overwritten by **--archiver** command line option

  - **FPM\_LDFLAGS**  
    sets additional link arguments for creating executables will be
    overwritten by **--link-flag** command line option

# EXAMPLES

sample commands:

``` 
    fpm new mypackage --app --test
    fpm build
    fpm test
    fpm run
    fpm run --example
    fpm new --help
    fpm run myprogram --profile release -- -x 10 -y 20 --title "my title"
    fpm install --prefix ~/.local
    fpm clean --all
```

# SEE ALSO

  - The **fpm**(1) home page is at
    https://github.com/fortran-lang/**fpm**

  - Registered **fpm**(1) packages are at
    https://fortran-lang.org/packages

  - The **fpm**(1) TOML file format is described at
    https://github.com/fortran-lang/**fpm**/blob/main/manifest-reference.md

# NAME

**new**(1) - the **fpm**(1) subcommand to initialize a new project

# SYNOPSIS

**fpm** *new* *NAME* \[\[-**-lib**|**--src**\] \[-**-app**\]
\[-**-test**\] \[-**-example**\]\]|
\[-**-full**|**--bare**\]\[-**-backfill**\] **fpm** *new*
**--help**|**--version**

# DESCRIPTION

"**fpm** *new*" creates and populates a *new* programming project
directory. It

>   - creates a directory with the specified name
> 
>   - runs the command "git init" in that directory
> 
>   - populates the directory with the default project directories
> 
>   - adds sample Fortran source files

The default file structure (that will be automatically scanned) is

``` 
     NAME/
       fpm.toml
       src/
           NAME.f90
       app/
           main.f90
       test/
           check.f90
       example/
           demo.f90
```

Using this file structure is highly encouraged, particularly for small
packages primarily intended to be used as dependencies.

If you find this restrictive and need to customize the package structure
you will find using the **--full** switch creates a heavily annotated
manifest file with references to documentation to aid in constructing
complex package structures.

Remember to update the information in the sample "fpm.toml" file with
your name and e-mail address.

# OPTIONS

  - ***NAME***  
    the name of the project directory to create. The name must be made
    of up to 63 ASCII letters, digits, underscores, or hyphens, and
    start with a letter.

The default is to create the src/, app/, and test/ directories. If any
of the following options are specified then only the selected
subdirectories are generated:

  - ****--lib**,**--src****  
    create directory src/ and a placeholder module named "NAME.f90" for
    use with subcommand "build".

  - ****--app****  
    create directory app/ and a placeholder main program for use with
    subcommand "run".

  - ****--test****  
    create directory test/ and a placeholder program for use with the
    subcommand "test". Note that sans "**--lib**" it really does not
    have anything to test.

  - ****--example****  
    create directory example/ and a placeholder program for use with the
    subcommand "run **--example**". It is only created by default if
    "**--full** is" specified.

So the default is equivalent to

``` 
    fpm NAME --lib --app --test
```

  - ****--backfill****  
    By default the directory must not exist. If this option is present
    the directory may pre-exist and only subdirectories and files that
    do not already exist will be created. For example, if you previously
    entered "**fpm** *new* myname **--lib**" entering "**fpm** *new*
    myname **-full** **--backfill**" will create any missing app/,
    example/, and test/ directories and programs.

  - ****--full****  
    By default a minimal manifest file ("fpm.toml") is created that
    depends on auto-discovery. With this option a much more extensive
    manifest sample is written and the example/ directory is created and
    populated. It is designed to facilitate creating projects that
    depend extensively on non-default build options.

  - ****--bare****  
    A minimal manifest file ("fpm.toml") is created and "README.md" file
    is created but no directories or sample Fortran are generated.

  - ****--help****  
    print this help and exit

  - ****--version****  
    print program version information and exit

# EXAMPLES

Sample use

``` 
   fpm new myproject  # create new project directory and seed it
   cd myproject       # Enter the new directory
   # and run commands such as
   fpm build
   fpm run            # run lone example application program
   fpm test           # run example test program(s)
   fpm run --example  # run lone example program

   fpm new A --full # create example/ and an annotated fpm.toml as well
   fpm new A --bare # create no directories
   create any missing files in current directory
   fpm new --full --backfill
```

# NAME

**build**(1) - the **fpm**(1) subcommand to build a project

# SYNOPSIS

**fpm** *build* \[-**-profile** *PROF*\] \[-**-flag** *FFLAGS*\]
\[-**-compiler** *COMPILER\_NAME*\] \[-**-list**\] \[-**-tests**\]

**fpm** *build* **--help**|**--version**

# DESCRIPTION

The "**fpm** *build*" command

>   - Fetches any dependencies
> 
>   - Scans your sources
> 
>   - Builds them in the proper order

The Fortran source files are assumed by default to be in

>   - src/ for modules and procedure source
> 
>   - app/ main **program**(s) for applications
> 
>   - test/ main **program**(s) and support files for project tests
> 
>   - example/ main **program**(s) for example programs Changed or *new*
>     files found are rebuilt. The results are placed in the *build*/
>     directory.

Non-default pathnames and remote dependencies are used if specified in
the "fpm.toml" file.

# OPTIONS

  - ****--profile** *PROF***  
    selects the compilation profile for the *build*. Currently available
    profiles are "release" for high optimization and "debug" for full
    debug options. If **--flag** is not specified the "debug" flags are
    the default.

  - ****--compiler** *NAME***  
    Specify a compiler name. The default is "gfortran" unless set by the
    environment variable FPM\_FC.

  - ****--c-compiler** *NAME***  
    Specify the C compiler name. Automatically determined by default
    unless set by the environment variable FPM\_CC.

  - ****--archiver** *NAME***  
    Specify the archiver name. Automatically determined by default
    unless set by the environment variable FPM\_AR.

  - ****--flag****  
    *FFLAGS* selects compile arguments for the *build*, the default
    value is set by the FPM\_FFLAGS environment variable. These are
    added to the profile options if **--profile** is specified, else
    these these options override the defaults. Note object and .mod
    directory locations are always built in.

  - ****--c-flag** CFLAGS**  
    selects compile arguments specific for C source in the *build*. The
    default value is set by the FPM\_CFLAGS environment variable.

  - ****--link-flag** LDFLAGS**  
    select arguments passed to the linker for the *build*. The default
    value is set by the FPM\_LDFLAGS environment variable.

  - ****--list****  
    list candidates instead of building or running them

  - ****--tests****  
    *build* all tests (otherwise only if needed)

  - ****--show-model****  
    show the model and exit (do not *build*)

  - ****--help****  
    print this help and exit

  - ****--version****  
    print program version information and exit

# ENVIRONMENT VARIABLES

  - **FPM\_FC**  
    sets the path to the Fortran compiler used for the *build*, will be
    overwritten by **--compiler** command line option

  - **FPM\_FFLAGS**  
    sets the arguments for the Fortran compiler will be overwritten by
    **--flag** command line option

  - **FPM\_CC**  
    sets the path to the C compiler used for the *build*, will be
    overwritten by **--c-compiler** command line option

  - **FPM\_CFLAGS**  
    sets the arguments for the C compiler will be overwritten by
    **--c-flag** command line option

  - **FPM\_AR**  
    sets the path to the archiver used for the *build*, will be
    overwritten by **--archiver** command line option

  - **FPM\_LDFLAGS**  
    sets additional link arguments for creating executables will be
    overwritten by **--link-flag** command line option

# EXAMPLES

Sample commands:

``` 
  fpm build                   # build with debug options
  fpm build --profile release # build with high optimization
```

# NAME

**run**(1) - the **fpm**(1) subcommand to run project applications

# SYNOPSIS

**fpm** run \[\[-**-target**\] *NAME*(*s*) \[-**-profile** *PROF*\]
\[-**-flag** *FFLAGS*\] \[-**-compiler** *COMPILER\_NAME*\]
\[-**-runner** "CMD"\] \[-**-example**\] \[-**-list**\] \[-**-all**\]
\[-- ARGS\]

**fpm** run **--help**|**--version**

# DESCRIPTION

Run the applications in your **fpm**(1) package. By default applications
in /app or specified as "executable" in your "fpm.toml" manifest are
used. Alternatively demonstration programs in example/ or specified in
the "example" section in "fpm.toml" can be executed. The applications
are automatically rebuilt before being run if they are out of date.

# OPTIONS

  - ****--target** *NAME*(*s*)**  
    list of application names to execute. No name is required if only
    one target exists. If no name is supplied and more than one
    candidate exists or a name has no match a list is produced and
    **fpm**(1) exits.
    
    Basic "globbing" is supported where "?" represents any single
    character and "\*" represents any string. Note The glob string
    normally needs quoted to the special characters from shell
    expansion.

  - ****--all****  
    Run all examples or applications. An alias for **--target** '\*'.

  - ****--example****  
    Run example programs instead of applications.

  - ****--profile** *PROF***  
    selects the compilation profile for the *build*. Currently available
    profiles are "release" for high optimization and "debug" for full
    debug options. If **--flag** is not specified the "debug" flags are
    the default.

  - ****--compiler** *NAME***  
    Specify a compiler name. The default is "gfortran" unless set by the
    environment variable FPM\_FC.

  - ****--c-compiler** *NAME***  
    Specify the C compiler name. Automatically determined by default
    unless set by the environment variable FPM\_CC.

  - ****--archiver** *NAME***  
    Specify the archiver name. Automatically determined by default
    unless set by the environment variable FPM\_AR.

  - ****--flag****  
    *FFLAGS* selects compile arguments for the *build*, the default
    value is set by the FPM\_FFLAGS environment variable. These are
    added to the profile options if **--profile** is specified, else
    these these options override the defaults. Note object and .mod
    directory locations are always built in.

  - ****--c-flag** CFLAGS**  
    selects compile arguments specific for C source in the *build*. The
    default value is set by the FPM\_CFLAGS environment variable.

  - ****--link-flag** LDFLAGS**  
    select arguments passed to the linker for the *build*. The default
    value is set by the FPM\_LDFLAGS environment variable.

  - ****--runner** CMD**  
    A command to prefix the program execution paths with. see "**fpm**
    help runner" for further details.

  - ****--list****  
    list basenames of candidates instead of running them. Note
    out-of-date candidates will still be rebuilt before being listed.

  - **-- ARGS**  
    optional arguments to pass to the **program**(*s*). The same
    arguments are passed to all program names specified.

# ENVIRONMENT VARIABLES

  - **FPM\_FC**  
    sets the path to the Fortran compiler used for the *build*, will be
    overwritten by **--compiler** command line option

  - **FPM\_FFLAGS**  
    sets the arguments for the Fortran compiler will be overwritten by
    **--flag** command line option

  - **FPM\_CC**  
    sets the path to the C compiler used for the *build*, will be
    overwritten by **--c-compiler** command line option

  - **FPM\_CFLAGS**  
    sets the arguments for the C compiler will be overwritten by
    **--c-flag** command line option

  - **FPM\_AR**  
    sets the path to the archiver used for the *build*, will be
    overwritten by **--archiver** command line option

  - **FPM\_LDFLAGS**  
    sets additional link arguments for creating executables will be
    overwritten by **--link-flag** command line option

# EXAMPLES

**fpm**(1) - run or display project applications:

``` 
  fpm run        # run a target when only one exists or list targets
  fpm run --list # list basename of all targets, running nothing.
  fpm run "demo*" --list # list target basenames starting with "demo*".
  fpm run "psi*" --runner # list target pathnames starting with "psi*".
  fpm run --all  # run all targets, no matter how many there are.

  # run default program built or to be built with the compiler command
  # "f90". If more than one app exists a list displays and target names
  # are required.
  fpm run --compiler f90

  # run example programs instead of the application programs.
  fpm run --example "*"

  # run a specific program and pass arguments to the command
  fpm run myprog -- -x 10 -y 20 --title "my title line"

  # run production version of two applications
  fpm run --target prg1,prg2 --profile release

  # install executables in directory (assuming install(1) exists)
  fpm run --runner 'install -b -m 0711 -p -t /usr/local/bin'
```

# NAME

**clean**(1) - delete the *build*

# SYNOPSIS

**fpm** *clean*

# DESCRIPTION

Prompts the user to confirm deletion of the *build*. If affirmative,
directories in the *build*/ directory are deleted, except dependencies.

# OPTIONS

  - ****--skip****  
    delete the *build* without prompting but skip dependencies.

  - ****--all****  
    delete the *build* without prompting including dependencies.

# NAME

**test**(1) - the **fpm**(1) subcommand to run project tests

# SYNOPSIS

**fpm** test \[\[-**-target**\] *NAME*(*s*)\] \[-**-profile** *PROF*\]
\[-**-flag** *FFLAGS*\] \[-**-compiler** *COMPILER\_NAME* \]
\[-**-runner** "CMD"\] \[-**-list**\]\[-- ARGS\]

**fpm** test **--help**|**--version**

# DESCRIPTION

Run applications you have built to test your project.

# OPTIONS

  - ****--target** *NAME*(*s*)**  
    optional list of specific test names to execute. The default is to
    run all the tests in test/ or the tests listed in the "fpm.toml"
    file.
    
    Basic "globbing" is supported where "?" represents any single
    character and "\*" represents any string. Note The glob string
    normally needs quoted to protect the special characters from shell
    expansion.

  - ****--profile** *PROF***  
    selects the compilation profile for the *build*. Currently available
    profiles are "release" for high optimization and "debug" for full
    debug options. If **--flag** is not specified the "debug" flags are
    the default.

  - ****--compiler** *NAME***  
    Specify a compiler name. The default is "gfortran" unless set by the
    environment variable FPM\_FC.

  - ****--c-compiler** *NAME***  
    Specify the C compiler name. Automatically determined by default
    unless set by the environment variable FPM\_CC.

  - ****--archiver** *NAME***  
    Specify the archiver name. Automatically determined by default
    unless set by the environment variable FPM\_AR.

  - ****--flag****  
    *FFLAGS* selects compile arguments for the *build*, the default
    value is set by the FPM\_FFLAGS environment variable. These are
    added to the profile options if **--profile** is specified, else
    these these options override the defaults. Note object and .mod
    directory locations are always built in.

  - ****--c-flag** CFLAGS**  
    selects compile arguments specific for C source in the *build*. The
    default value is set by the FPM\_CFLAGS environment variable.

  - ****--link-flag** LDFLAGS**  
    select arguments passed to the linker for the *build*. The default
    value is set by the FPM\_LDFLAGS environment variable.

  - ****--runner** CMD**  
    A command to prefix the program execution paths with. see "**fpm**
    help runner" for further details.

  - ****--list****  
    list candidate basenames instead of running them. Note they

  - ****--list****  
    will still be built if not currently up to date.

  - **-- ARGS**  
    optional arguments to pass to the test **program**(*s*). The same
    arguments are passed to all test names specified.

# ENVIRONMENT VARIABLES

  - **FPM\_FC**  
    sets the path to the Fortran compiler used for the *build*, will be
    overwritten by **--compiler** command line option

  - **FPM\_FFLAGS**  
    sets the arguments for the Fortran compiler will be overwritten by
    **--flag** command line option

  - **FPM\_CC**  
    sets the path to the C compiler used for the *build*, will be
    overwritten by **--c-compiler** command line option

  - **FPM\_CFLAGS**  
    sets the arguments for the C compiler will be overwritten by
    **--c-flag** command line option

  - **FPM\_AR**  
    sets the path to the archiver used for the *build*, will be
    overwritten by **--archiver** command line option

  - **FPM\_LDFLAGS**  
    sets additional link arguments for creating executables will be
    overwritten by **--link-flag** command line option

# EXAMPLES

run tests

``` 
 # run default tests in /test or as specified in "fpm.toml"
 fpm test

 # run using compiler command "f90"
 fpm test --compiler f90

 # run a specific test and pass arguments to the command
 fpm test mytest -- -x 10 -y 20 --title "my title line"

 fpm test tst1 tst2 --profile PROF  # run production version of two tests
```

# NAME

**--runner**(1) - a shared option for specifying an application to
launch executables.

# SYNOPSIS

**fpm** run|test **--runner** *CMD* \`\`\` -- *SUFFIX\_OPTIONS*

# DESCRIPTION

The **--runner** option allows specifying a program to launch
executables selected via the **fpm**(1) subcommands "run" and "test".
This gives easy recourse to utilities such as debuggers and other tools
that wrap other executables.

These external commands are not part of **fpm**(1) itself as they vary
from platform to platform or require independent installation.

# OPTION

  - ****--runner** '*CMD*'**  
    quoted command used to launch the **fpm**(1) executables. Available
    for both the "run" and "test" subcommands. If the keyword is
    specified without a value the default command is "echo".

  - **-- *SUFFIX\_OPTIONS***  
    additional options to suffix the command *CMD* and executable file
    names with.

# EXAMPLES

Use cases for '**fpm** run|test **--runner** "*CMD*"' include employing
the following common GNU/Linux and Unix commands:

## INTERROGATE

  - nm - list symbols from object files

  - size - list section sizes and total size.

  - ldd - print shared object dependencies

  - ls - list directory contents

  - stat - display file or file system status

  - file - determine file type

## PERFORMANCE AND DEBUGGING

  - gdb - The GNU Debugger

  - valgrind - a suite of tools for debugging and profiling

  - time - time a simple command or give resource usage

  - timeout - run a command with a time limit

## COPY

  - install - copy files and set attributes

  - tar - an archiving utility

## ALTER

  - rm - remove files or directories

  - chmod - change permissions of a file

  - strip - remove unnecessary information from strippable files

For example

``` 
  fpm test --runner gdb
  fpm run --runner "tar cvfz $HOME/bundle.tgz"
  fpm run --runner ldd
  fpm run --runner strip
  fpm run --runner 'cp -t /usr/local/bin'

  # options after executable name can be specified after the -- option
  fpm --runner cp run -- /usr/local/bin/
  # generates commands of the form "cp $FILENAME /usr/local/bin/"

  # bash(1) alias example:
  alias fpm-install=\
  "fpm run --profile release --runner 'install -vbp -m 0711 -t ~/.local/bin'"
  fpm-install
```

# NAME

**install**(1) - install **fpm** projects

# SYNOPSIS

**fpm** *install* \[-**-profile** *PROF*\] \[-**-flag** *FFLAGS*\]
\[-**-list**\] \[-**-no-rebuild**\] \[-**-prefix** *DIR*\]
\[-**-bindir** *DIR*\] \[-**-libdir** *DIR*\] \[-**-includedir** *DIR*\]
\[-**-verbose**\]

# DESCRIPTION

Subcommand to *install* **fpm** projects. Running *install* will export
the current project to the selected prefix, this will by default
*install* all executables (tests and examples are excluded) which are
part of the projects. Libraries and module files are only installed for
projects requiring the installation of those components in the package
manifest.

# OPTIONS

  - ****--list****  
    list all installable targets for this project, but do not *install*
    any of them

  - ****--profile** *PROF***  
    selects the compilation profile for the *build*. Currently available
    profiles are "release" for high optimization and "debug" for full
    debug options. If **--flag** is not specified the "debug" flags are
    the default.

  - ****--flag****  
    *FFLAGS* selects compile arguments for the *build*, the default
    value is set by the FPM\_FFLAGS environment variable. These are
    added to the profile options if **--profile** is specified, else
    these these options override the defaults. Note object and .mod
    directory locations are always built in.

  - ****--c-flag** CFLAGS**  
    selects compile arguments specific for C source in the *build*. The
    default value is set by the FPM\_CFLAGS environment variable.

  - ****--link-flag** LDFLAGS**  
    select arguments passed to the linker for the *build*. The default
    value is set by the FPM\_LDFLAGS environment variable.

  - ****--no-rebuild****  
    do not rebuild project before installation

  - ****--prefix** *DIR***  
    path to installation directory (requires write access), the default
    prefix on Unix systems is $HOME/.local and %APPDATA%\\local on
    Windows

  - ****--bindir** *DIR***  
    subdirectory to place executables in (default: bin)

  - ****--libdir** *DIR***  
    subdirectory to place libraries and archives in (default: lib)

  - ****--includedir** *DIR***  
    subdirectory to place headers and module files in (default: include)

  - ****--verbose****  
    print more information

# ENVIRONMENT VARIABLES

  - **FPM\_FC**  
    sets the path to the Fortran compiler used for the *build*, will be
    overwritten by **--compiler** command line option

  - **FPM\_FFLAGS**  
    sets the arguments for the Fortran compiler will be overwritten by
    **--flag** command line option

  - **FPM\_CC**  
    sets the path to the C compiler used for the *build*, will be
    overwritten by **--c-compiler** command line option

  - **FPM\_CFLAGS**  
    sets the arguments for the C compiler will be overwritten by
    **--c-flag** command line option

  - **FPM\_AR**  
    sets the path to the archiver used for the *build*, will be
    overwritten by **--archiver** command line option

  - **FPM\_LDFLAGS**  
    sets additional link arguments for creating executables will be
    overwritten by **--link-flag** command line option

# EXAMPLES

1.  Install release version of project:

<!-- end list -->

``` 
    fpm install --profile release
```

2.  Install the project without rebuilding the executables:

<!-- end list -->

``` 
    fpm install --no-rebuild
```

3.  Install executables to a custom prefix into the exe directory:

<!-- end list -->

``` 
    fpm install --prefix $PWD --bindir exe
```

# NAME

**update**(1) - manage project dependencies

# SYNOPSIS

**fpm** update \[-**-fetch-only**\] \[--*clean*\] \[-**-verbose**\]
**\[NAME**(*s*)\]

# DESCRIPTION

Manage and update project dependencies. If no dependency names are
provided all the dependencies are updated automatically.

# OPTIONS

  - ****--fetch-only****  
    Only fetch dependencies, do not update existing projects

  - **--*clean***  
    Do not use previous dependency cache

  - ****--verbose****  
    Show additional printout

# SEE ALSO

The **fpm**(1) home page at https://github.com/fortran-lang/**fpm**

# NAME

**list**(1) - list summary of **fpm**(1) subcommands

# SYNOPSIS

**fpm** *list* \[-*list*\]

**fpm** *list* **--help**|**--version**

# DESCRIPTION

Display a short description for each **fpm**(1) subcommand.

# OPTIONS

  - **--*list***  
    display a *list* of command options as well. This is the same output
    as generated by "**fpm** --*list*".

# EXAMPLES

display a short *list* of **fpm**(1) subcommands

``` 
  fpm list
  fpm --list
```

# NAME

**help**(1) - the **fpm**(1) subcommand to display help

# SYNOPSIS

*fpm* *help* \[fpm\] \[new\] \[build\] \[run\] \[test\] \[help\]
\[version\] \[manual\] \[runner\]

# DESCRIPTION

The "*fpm* *help*" command is an alternative to the --*help* parameter
on the **fpm**(1) command and its subcommands.

# OPTIONS

  - ****NAME**(*s*)**  
    A *list* of topic names to display. All the subcommands have their
    own page (*new*, *build*, *run*, *test*, \`\`\`).
    
    The special name "*manual*" displays all the **fpm**(1) built-in
    documentation.
    
    The default is to display *help* for the **fpm**(1) command itself.

# EXAMPLES

Sample usage:

``` 
     fpm help           # general fpm(1) command help
     fpm help version   # show program version
     fpm help new       # display help for "new" subcommand
     fpm help manual    # All fpm(1) built-in documentation

```

  - **Version:**  
    0.5.0, alpha
    
      - **Program:**  
        **fpm**(1) Description: A Fortran package manager and *build*
        system
    
      - **Home Page:**  
        https://github.com/fortran-lang/*fpm*
    
      - **License:**  
        MIT
    
      - **OS Type:**  
        Linux *fpm*: Leaving directory '/home'
