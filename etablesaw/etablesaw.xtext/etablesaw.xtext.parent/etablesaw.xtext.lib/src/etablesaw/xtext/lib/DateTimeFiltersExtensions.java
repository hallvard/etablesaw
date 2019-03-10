package etablesaw.xtext.lib;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;

import tech.tablesaw.api.DateColumn;
import tech.tablesaw.api.DateTimeColumn;
import tech.tablesaw.api.TimeColumn;
import tech.tablesaw.columns.dates.DateFilters;
import tech.tablesaw.columns.datetimes.DateTimeFilters;
import tech.tablesaw.columns.times.TimeFilters;
import tech.tablesaw.selection.Selection;

public class DateTimeFiltersExtensions {
    
	// DateFilters

    // >
    public static Selection operator_greaterThan(DateFilters dateColumn, LocalDate date) {
        return dateColumn.isAfter(date);
    }
    public static Selection operator_greaterThan(DateFilters dateColumn, int date) {
        return dateColumn.isAfter(date);
    }
    public static Selection operator_greaterThan(DateFilters dateColumn, DateColumn otherDateColumn) {
        return dateColumn.isAfter(otherDateColumn);
    }
    // <
    public static Selection operator_lessThan(DateFilters dateColumn, LocalDate date) {
        return dateColumn.isBefore(date);
    }
    public static Selection operator_lessThan(DateFilters dateColumn, int date) {
        return dateColumn.isBefore(date);
    }
    public static Selection operator_lessThan(DateFilters dateColumn, DateColumn otherDateColumn) {
        return dateColumn.isBefore(otherDateColumn);
    }    

    // DateTimeFilters
    
    // >
    public static Selection operator_greaterThan(DateTimeFilters dateTimeColumn, LocalDateTime dateTime) {
        return dateTimeColumn.isAfter(dateTime);
    }
    public static Selection operator_greaterThan(DateTimeFilters dateTimeColumn, LocalDate date) {
        return dateTimeColumn.isAfter(date);
    }
    public static Selection operator_greaterThan(DateTimeFilters dateTimeColumn, DateTimeColumn otherDateTimeColumn) {
        return dateTimeColumn.isAfter(otherDateTimeColumn);
    }
    // <
    public static Selection operator_lessThan(DateTimeFilters dateTimeColumn, LocalDateTime dateTime) {
        return dateTimeColumn.isBefore(dateTime);
    }
    public static Selection operator_lessThan(DateTimeFilters dateTimeColumn, LocalDate date) {
        return dateTimeColumn.isBefore(date);
    }
    public static Selection operator_lessThan(DateTimeFilters dateTimeColumn, DateTimeColumn otherDateTimeColumn) {
        return dateTimeColumn.isBefore(otherDateTimeColumn);
    }    
    
    // TimeFilters
    
    // >
    public static Selection operator_greaterThan(TimeFilters timeColumn, LocalTime time) {
        return timeColumn.isAfter(time);
    }
    public static Selection operator_greaterThan(TimeFilters timeColumn, int time) {
        return timeColumn.isAfter(time);
    }
    public static Selection operator_greaterThan(TimeFilters timeColumn, TimeColumn otherTimeColumn) {
        return timeColumn.isAfter(otherTimeColumn);
    }
    // <
    public static Selection operator_lessThan(TimeFilters timeColumn, LocalTime time) {
        return timeColumn.isBefore(time);
    }
    public static Selection operator_lessThan(TimeFilters timeColumn, int time) {
        return timeColumn.isBefore(time);
    }
    public static Selection operator_lessThan(TimeFilters timeColumn, TimeColumn otherTimeColumn) {
        return timeColumn.isBefore(otherTimeColumn);
    }
}
