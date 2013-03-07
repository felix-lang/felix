NAME: pkgtool

DESCRIPTION: Packaging Management Tool

VERSION: .03

AUTHOR:  Mike Maul

PKG_URL: https://github.com/mmaul/pkgtool

CATEGORY: UTIL

LIBDIR: PKGTOOL

-----

PKGTOOL is what happens when a build system, a package manager and a test 
framework meet up at a bar and have a few too many drinks. Seriously though
pkgtool is a framework that combines the common aspects of all three. 
Also a flx compiler API interface happend by at the bar towards the end and 
got in on the fun.

PkgTool is used to implement ''scoop'' a distributed package management 
application. PkgTool is also used to implement SetupTool build/test/package management framework.

Features
=======

* Package build framework
* Package test framework
* Package installation framework
* Distributed remote package repository 
* Distributed package management application
* Centeralized package directory

Installation
============

Installation is simple because PkgTool uses PkgtTool for build and installation.
All that is needed is:

    flx setup install

This will build the PkgTool executables and install the executables and library
components in in the Felix INSTALL_ROOT directory. The scoop executable is also
installed in the /usr/local/bin directory by default.  You will need sufficient 
access priviledges to write to INSTALL_ROOT and /usr/local/bin. when runing this.

Package Management
==================
Packages exist as independant remote git repositories which conform to certain 
structure dependant on their function. There are three types of packages:

* Applications
* Libraries
* Web Applications

Each of the different package types exhibit certain common preferences that 
created the need for thir distinction.

* Applications have a need for one or more user executables
* Libraries hae a need to present one or more API interfaces
* Web Applications have a need to present one executable along with associated content

Independant package repositories are usless if you don't know where they are, and even if you do
it's inconvient. Which is why the ''litterbox'' central package index exists. Litterbox contains 
the package README.md in side a directory for each package. Litterbox is self is housed is a remote 
git repository on github.com. One particularly nice feature is the packages are easly browsable simple
by going to the the litterbox repositirey with a web browser and being able to access the package README.md's
with few clicks. The other nice thing is there is no seperate package meta-data. THe package meta-data is 
contained at the top of the package README.md. In fact you can see the package metadata for PkgTool simply by 
looking at the top of this file.

##Litterbox Package Index
The Litterbox Package Index is located at https://github.com/mmaul/litterbox

scoop - Package Manager
=====
Scoop is the user interface to to distributed package management aspect of PkgTool. Scoop allows you to view the
contents of the litterbox package index, get and install packages from remote git repositories.

### Scoop commands are presented below:

    scoop refresh            <options> Refreshes litterbox package directory cache
    scoop list    [installed] Lists all packages on litterbox or if 'installed'
                              is supplied lists package that hae been installed
    scoop search  <package>  <options> Searches for package on litterbox
    scoop info    <package>  <options> Displays package info
    scoop get     <package>  <options> [destination]Pull package from litterbox to current working directory
    scoop install <package>  <options> Pull, builds and install package
    scoop help    [command]  <options> Displays detailed help for command

### Scoop commands can be followed by options

    --litterbox-url=<remote repository>  Remote package directory

    --litterbox=<location>               Location to create local package directory 
                                         repo if HOME/.felix/litterbox is unacceptable

    --degitify                           Remove .git directory from downloaded packages
    
    --force                              Proceed with installation even if tests fail
    
    -L[C/C++ library paths]              Supply additional C/C++ include paths

    -I[C/C++ library paths]              Supply sdditional C/C++ library paths

    --dry-run                             Don't actuall install, but tell us where you would.


### Environmental Variables ###
    LITTERBOX= Location to create local package directory repo if HOME/.felix/litterbox is unacceptable
    LITTERBOX_URL=  Remote package directory


SetupTool - Build and Testing Framework
=======================================
So one can write programs with out build and test frameworks just fine. But if you want to share your software
with others it really helps to have a build and test framework. If there is something wrong you can fix it, if you
need to move an executable to some location or other you can do it. Once your code has gone out into the world
you can't fix it quite so easily. That is why a using a test framework can shake out problems that might surface 
after release. A build frame work makes it easier to automate your builds and makes it easier for other people to 
build your code. It also makes it easier easier for you package binary and source distributions.

SetupTool it PkgTool's solution to this. SetupTool is presented to the user as a Felix source file called 
''setup.flx'' located in the top level of the package directory. This is used to instantiate the build system and
allow for user customizations to the build system. If you are note deviating from the standard behaviors your
setup.flx file can be quite small. Below is a minimum setup.flx

    include "PKGTOOL/pkgtool";
    BUILD_LIKE = App;
    SetupTool::run();
    
That is all you need to build a Felix Application with Setup tool. (Well that and
a README.md package definition file and a conformant directory structure. As
far as the directory structure goes 'scoop get lib-template mylib' can help you
with that.)

What does that actually do for you?
Providing you are conforming to the App directory structure for your project it wil:
* Compile all felix source in the bin directory to a static binary executable
* Execute tests source located in the test directory report the results and stop the build on errors.
* Allow you to install your application to the file system 
* Allow you to create a binary distribution

There are three package types App, Lib and WebApp; each has a specific directory strucuture and buid behaiors.

###  App Package Directory Structure
    README.md - Package metadata and documentation
    app            
       Application support code (Directory name is usualy same as package name) 
    bin
       Application frontend
    config
       Package config files
    setup.flx - Build script
    setup.log - Build log
    test
       Test code
### App Package Default Behavior 
#### Build Phase
The flx files in the are compiled with flx compiler with the --static switch to generate an executable binary in
the bin directory
#### Test Phase
The flx files in the test directory except those starting with a capital 'C' or 'D' are executed in the test context.
Files starting with 'C' are reserved for configuration test scrips used in the Build Phase. Files starting with capital
'D' are reserved for test datafiles.
#### Install/Dist Phase
Install phase will place binaries generated in bin directory in Felis INSTALL_ROOT/bin or if the --prefix=DIR switch was
specified place the bin directory under the path specified in the --prefix option. The Dist variation of the Install
Phase will create a dist folder inside the package dir and copy the executable files from the bin director into 
a bin directory inside the dist folder.

###  Lib Package Directory Structure
    README.md - Package metadata and documentation
    <APP>        
       Library code <APP> should match value of LIB_DIR parameter in package README.md  
    bin
       Executables related to library
    config
       Package config files
    examples
       Example code
    setup.flx - Build script
    setup.log - Build log
    test
       Test code
       
### Lib Package Default Behavior 
#### Build Phase
The flx files in the are compiled with flx compiler with the --static switch to generate an executable binary in
the bin directory
#### Test Phase
The flx files in the test directory except those starting with a capital 'C' or 'D' are executed in the test context.
Files starting with 'C' are reserved for configuration test scrips used in the Build Phase. Files starting with capital
'D' are reserved for test datafiles.
#### Install/Dist Phase
Install phase will place binaries generated in bin directory in Felis INSTALL_ROOT/bin or if the --prefix=DIR switch was
specified place the bin directory under the path specified in the --prefix option. Library source code in the directory
named in the LIBDIR parameter will be placed under the Felix INSTALL_ROOT/lib directory. Felix package config files 
(fpc) in the config directory will be placed in the Felix INSTALL_ROOT/config directory. The Dist variation of the 
Install Phase preforms similar operations however the dist directory will be the root as opposed to the Felix 
INSTALL_ROOT directory.

###  WebApp Package Directory Structure
    README.md - Package metadata and documentation
    app            
       Web application code where the folder name will be the name set in the LIBDIR parameter 
    config
       Package config files
       application config files
    examples
       Example code
    html
      css                   
        stylesheets
      images                
        image content         
      js            
        javascript code
    setup.flx - Build script
    setup.log - Build log
    test
       Test code
### WebApp Package Default Behavior 
#### Build Phase
The file matching the NAME parameter in the app directory is compiled as a static executable app directory
#### Test Phase
The flx files in the test directory except those starting with a capital 'C' or 'D' are executed in the test context.
Files starting with 'C' are reserved for configuration test scrips used in the Build Phase. Files starting with capital
'D' are reserved for test datafiles.
#### Install/Dist Phase
Install phase will the executable generated in app directory in directory specified in the DEST_DIR variable which
should be set in the setup.flx file. The contents of the cfg and html directories are also copied to the directory
specified in DEST_DIR.  The Dist variation of the Install will preform the same actions except copying to the dist
directory.

### README.md - Package metadata and Introductory Documentation
The README.md file is a GitHub flavored markdown document that also contains
package definition. It als MUST be in the top level directory of your package.

#### What's in README.md?
The packag definition component is located at the top of
the README.md  and consistes of Key pair. The Package definition section ends
when a like starting with 5 or more consecutive dashes. Below is an example.

    NAME: mylib
    VERSION: 1.01
    PKG_URL: https://github.com/me/mylib.git
    AUTHOR: Me
    LIBDIR: MYLIB

While you can put any thing you want in the README.md file if you want it
to work with SetupTool you HAVE TO supply the fields marked REQUIRED.
Below are the recognized fields:

|Field       | Description                           |   Required   |
|------------|---------------------------------------|--------------|
|NAME        | Package Name                          |      YES     |
|VERSION     | Numeric float only                    |      YES     |
|PKG_URL     | Remote Git repo URL                   |      YES     |
|LIBDIR      | Reccomended for lib's defaults to NAME| YES for LIBs |
|DEPENDENCIES| Comma sep list of require packages    |      NO      |
|CATEGORY    | Comma sep list of pkg categories      |      NO      |
|PLATFORMS   | WIN32,OSX,UNIX                        |      NO      |
|DESCRIPTION |                                       |      NO      |
|AUTHOR      |                                       |      NO      |
|AUTHOR_URL  |                                       |      NO      |
|LICENSE     |                                       |      NO      |

#### README.md and the Litterbox Package Index
So you might like to share your code. When your ready all you need to do
is send your README.md file to the Felix group mailing list. It will then
be paced in the Litterbox package index on Git Hub in a directory named
with your package name. If you like you can visit the Litterbox in your
browser to see what it's like: https://github.com/mmaul/litterbox.git

### SetupTool commands are presented below:

    flx setup build    [options]      performs config and build tasks
    flx setup test     [options]      performs config, build and test tasks
    flx setup install  [options]      performs config, build, test and install tasks
    flx setup dist     [options]      performs config, build, test and install to the dist directory
    flx setup force    [options]      performs config, build and install tasks
    flx setup info     [options]      will display package information
    flx setup clean    [options]      will delete generated files
    flx setup degitify [options]      removes git info from package dir
    flx setup help     [command]      will display detailed help for command

### SetupTool commands can be followed by options
    --degitify                           Remove .git directory from downloaded packages
    
    --force                              Proceed with installation even if tests fail
    
    -L[C/C++ library paths]              Supply additional C/C++ include paths

    -I[C/C++ library paths]              Supply sdditional C/C++ library paths

    --dry-run                             Don't actuall install, but tell us where you would.

## Customising the Phases
Each phase can be customized to execute user supplied code by creatting instances of the procedures defined
for each phase. They are as follows

* Build - proc build: 1
* Test - proc test: 1
* Install - proc install: 1
* Clean - proc clean: 1

Below is an example of creating a build phase customization

    instance PkgTool {
      proc build () {
        task("Custom build task for " + NAME);
        default_build();
      }
    }

Note the ''default_build()'' that invokes the default build phase behaior.

## Writing Tests

Below is an example of a test from the webapp-template package which illistrates a simple test

    include "PKGTOOL/pkgtool";
    include "web/http_request";
    include "web/server_config";
    include "web/http_handler";
    include "app/hello";
    open PkgTool;
    open ServerConfig;
    open HTTPRequest;
    open HTTPHandler;
    open Hello;
    
    var mock_server_config = basic_server_config(Empty[http_handler]);
    var mock_request = http_request(GET ,"http://127.0.0.1:8080/hello","/hello",
                                    Empty[string^2],Empty[string^2],Empty[string^2]);
    assert_true(hello_route(mock_server_config,mock_request),"Does /hello route?");
    
Notice the inclusion of the PkgTool library which is needed for access to the test framework aspect of PkgTool.
The call to assert_true generates the test label and the value of the first parameter determine the test result.

### Test Framework Interface
The following signatures descript the interface to the test framework

    proc imply(name:string)
    
The procure imply is a reachability test if the imply function is reached then it implies that the test is successful.

    proc test_fail(s:string)
The procedure test_fail is essentially the inverse of imply. If test_fail is reached then test fails and halts
the Test phase and any follow on phases.

    proc assert_true(result:bool,name:string,fail_message:string)
    proc assert_true(result:bool,name:string)
    proc assert_true(result:bool)

The assert true test use the result parameter to determing success or failure of the test. Additionally fail_message
paramater can be used to display additional information regarding the failure.

    proc warning(s:string)
The procedure warning will display a warning message but allow the test to continue.

### Handling of Test exit status codes
By default if a test program exists with a non zero status code the entire test is a failure and execution of 
the Test phase halts as well as any follow on phases

## Logging

All output presented to the console is also written to the file named in the variable SETUP_LOG. By default 
the log file name is called 'setup_log'. Additionally logged output from the Test Phase can be seperated from the
by setting the variable TEST_LOG to a desired file name. By default TEST_LOG is also set to 'setup.log'
The contents of the log files are turnicated on each run of setup.flx.


PkgTool Caveats
===============
PkgTool has been coded to be cross platform. It has not been verified on 
Windows, and before it works there will be blood.

PkgTool TODO
============
- [ ] Package uninstall
- [ ] Test on Windows platform
