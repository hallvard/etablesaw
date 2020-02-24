# Xaw Scripting DSL

Although some data manipulation may be done in the editor and views, the power of the tablesaw library is unleashed by the **Xaw** scripting DSL. The Xaw language is basically syntactic sugar for Java provided out-of-box by Xbase, some extra table and column-oriented operators and literal syntax and extension methods for reading and writing files and linking it to the table data registry.

The scripts are translated to Java in the context of the classpath of the projects they're within, and can be executed within the workbench so they may consume table data from or provide table data to workbench parts.

An Xaw script typically load table data from one or more files, manipulates tables and columns, derive new tables and output the result. In addition, the integration with the table registry makes it possible to use intermediate and resulting table data as the source for views.
