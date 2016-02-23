To be expanded!

# Testers
Take the (not so) nightly builds and execute the plugin on your code base.
Report any exceptions or plugin's misbehaviour in GitHub issue tracker. 
Also report any suggestions there.

Suggestions for new refactorings are welcome. Please make sure they contain two code samples:
One that shows the code before the refactoring and one that shows it after.

# Testers + Developers
Add your name to AUTHORS.txt.
Add a line to each file you modify (including samples) with such text:
    (C) name + email + the work you did.

# Developers

See [Hacking AutoRefactor](https://github.com/JnRouvignac/AutoRefactor/wiki/Hacking-AutoRefactor) to setup your Eclipse environment and download the source. Then you are ready to hack AutoRefactor!

TODO: Setup the eclipse/ folder for Eclipse settings.
Make sure your code respect the overall style of the project.

When contributing refactorings, it is mandatory to also contribute backing samples in and samples out for testing purposes.
For testing and coherency purposes, the name of the refactoring class and the samples must match:
* The refactoring class must be named XXXRefactoring
* The sample classes must be named XXXSample

You can run tests by:
* Running ```mvn clean install```
* Or running JUnit tests directly from Eclipse

### JDT Gotchas

Along with some surprising APIs in JDT, there are a few things developers should keep in mind when writing refactoring rules. See [JDT Gotchas](https://github.com/JnRouvignac/AutoRefactor/wiki/JDT-Gotchas)

# Website developers/designers

* [AutoRefactor website](http://autorefactor.org) could certainly do with some love!
