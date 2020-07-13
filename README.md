# Boxplot Shiny operator for Tercen

##### Description

The `Boxplot Shiny operator` is an operator to represent data as boxplots in Tercen.

##### Usage

Input projection|.
---|---
`y-axis`        | numeric, measurement to represent 
`row`           | factor, groups corresponding to different boxes along x axis
`column`        | factor (optional), groups corresponding to different plot panels
`colors`        | factor (optional), groups for corresponding to box and points coloring 

Output relations|.
---|---
`Operator view`        | view of the Shiny application

##### Details

The operator takes all the values of a cell and represents a boxplot. Depending on the assignment of rows, columns and colors in the Tercen projection, the layout will be different.

#### References

https://en.wikipedia.org/wiki/Box_plot
