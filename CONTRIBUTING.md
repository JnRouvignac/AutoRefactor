To be expanded!

# Testers
Take the (not so) nightly builds and execute the plugin on your code base.
Report any exceptions or plugin's misbehaviour in GitHub issue tracker.
Also report any suggestions there.
Suggestions for new refactorings are welcome. Please make sure they contain 2 code samples:
One that shows the code before the refactoring and one that shows it after.

# Testers + Developers
Add your name to AUTHORS.txt.
Add a line to each file you modify (including samples) with such text:
    (C) name + email + the work you did.

# Developers
TODO: Setup the eclipse/ folder for Eclipse settings.
Make sure your code respect the overall style of the project.

When contributing refactorings, it is mandatory to also contribute backing samples in and samples out for testing purposes.
For testing and coherency purposes, the name of the refactoring class and the samples must match:
* The refactoring class must be named XXXRefactoring
* The sample classes must be named XXXSample

You can run tests by:
1. Go to AutoRefactorHandler.execute() method, and change "if (false) {" to "if (true) {"
2. Run "Eclipse application"
3. In package explorer view, select the "samples_in" package and hit Shift+Ctrl+Y
4. Once tests have run, revert the changes to the "sample_in" package with using git
