# etablesaw - Eclipse integration for jtablesaw/tablesaw (data frame library for java)

The [Tablesaw](https://github.com/jtablesaw/tablesaw) data frame library provides capabilities similar to Python pandas. It has classes for tables and columns for various types and lots of ways of analysing and manipulating them, e.g. filter, slicing, aggregating and running statistics. It even integrates with [Smile](http://haifengl.github.io) to provide more advanced statistics and machine learning features.

The goal of etablesaw is to turn Eclipse into a workbench for working with data frames based on tablesaw. Currently it provides:

- a [data frame editor](etablesaw/etablesaw.docs/table-editor.md) based on [NatTable](https://www.eclipse.org/nattable/) supporting CSV and Excel file formats
- [views](etablesaw/etablesaw.docs/views.md) for viewing and deriving table data, including table and various charts
- a [table registry](etablesaw/etablesaw.docs/table-registry.md) that allows workbench parts (table data providers) to be the source of table data in other parts (table data consumers)
- [a scripting DSL](etablesaw/etablesaw.docs/xaw.md) based on [Xtext and Xbase](https://www.eclipse.org/Xtext/documentation/305_xbase.html) that gives a more interactive Tablesaw experience by integrating with the rest

Most features are extensible, e.g. it's easy to add new file formats to the editor or new chart views.

## Documentation

The documentation is available at [hallvard.github.io/etablesaw](https://hallvard.github.io/etablesaw). The documentation source is found in the [etablesaw.docs sub-folder](etablesaw/etablesaw.docs/README.md).

## Installation

The etablesaw plug-ins are available as one feature in the update site found at [hallvard.github.io/etablesaw](https://hallvard.github.io/etablesaw). Use **Help > Install New Software...** and enter this URL in the **Install** dialog.

## Contributing

To build the software, clone the repository and import it using **Import... > General > Existing Projects into Workspace** and select the root folder of the newly cloned repo. Make sure to check **Search for Nested Projects**, so you discover and import all sub-projects.

The project is configured to build with **Maven** and **Tycho**, so you should have **m2e** installed. You will need to install **Lifecycle Mappings** for eclipse-specific packaging to prevent errors in **pom.xml** files. It's easiest to do this by using quick fix on these markers, i.e. open a pom.xml file with an error marker, hover over a red squiggle and use the appropriate option.

To try etablesaw out from the dev. environment, launch a new eclipse and import the etablesaw.ui.examples project.
