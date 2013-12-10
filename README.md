Mondrian Schema Generator
=====
Package mondrianr creates Mondrian schema for given table(s). It's goal is to enable easy analysis of data stored in PostgreSQL or R in Mondrian compatible tools. [Mondrian](http://mondrian.pentaho.com/documentation/olap.php) is an open source OLAP engine. It executes queries written in the [MDX](http://mondrian.pentaho.com/documentation/mdx.php) language, reading data from a relational database (RDBMS).

`mondrianr` works well with [Saiku](http://meteorite.bi/saiku) (open source OLAP analytical tool) for which valid data source definition file can be optionally generated.

Main features:
------------------

 - Generates Mondrian schema for PostgreSQL table or R data frame
 - Generates datasource definition for Saiku
 - Maps date columns to existing time dimension (PostgreSQL)
 - Maps date columns to automatically generated time dimension (R data frame)
 - Can choose aggregators for numeric columns
 - Can specify what data types are considered to be numeric
 - Can specify what columns should be used to form dimensions