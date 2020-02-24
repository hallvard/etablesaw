# Linking table providers and consumers

The table data registry allows table data providers and consumers to be linked, e.g. the data in the editor to be plotted in the bar chart view. A workbench part registers to *provide table data* that other parts may consume. The providers may notify the consumers that the table data has changed, to dependent workbench parts may be updated.

Both the table editor and view provide the currently filtered and selected table data through this mechanism. Hence, a bar chart linked to an editor will update when the row selection changes or a filter is applied. The views that derive new table data also provide the resulting table. This allows e.g. a crosstab to be plotted.
