package etablesaw.xtext.lib;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.Iterator;
import java.util.function.Supplier;

import tech.tablesaw.columns.dates.DateFillers;
import tech.tablesaw.columns.datetimes.DateTimeFillers;
import tech.tablesaw.columns.times.TimeFillers;

public class DateTimeFillersExtensions {
    
    // DateFillers

    // =>
    public static <T> T operator_doubleArrow(Iterator<LocalDate> dates, DateFillers<T> dateColumn) {
        return dateColumn.fillWith(dates);
    }
    public static <T> T operator_doubleArrow(Supplier<LocalDate> dates, DateFillers<T> dateColumn) {
        return dateColumn.fillWith(dates);
    }
    public static <T> T operator_doubleArrow(Iterable<LocalDate> dates, DateFillers<T> dateColumn) {
        return dateColumn.fillWith(dates);
    }

    // DateTimeFillers
    
    // =>
    public static <T> T operator_doubleArrow(Iterator<LocalDateTime> dateTimes, DateTimeFillers<T> dateTimeColumn) {
        return dateTimeColumn.fillWith(dateTimes);
    }
    public static <T> T operator_doubleArrow(Supplier<LocalDateTime> dateTimes, DateTimeFillers<T> dateTimeColumn) {
        return dateTimeColumn.fillWith(dateTimes);
    }
    public static <T> T operator_doubleArrow(Iterable<LocalDateTime> dateTimes, DateTimeFillers<T> dateTimeColumn) {
        return dateTimeColumn.fillWith(dateTimes);
    }
    
    // TimeFillers
    
    // =>
    public static <T> T operator_doubleArrow(Iterator<LocalTime> dateTimes, TimeFillers<T> timeColumn) {
        return timeColumn.fillWith(dateTimes);
    }
    public static <T> T operator_doubleArrow(Supplier<LocalTime> dateTimes, TimeFillers<T> timeColumn) {
        return timeColumn.fillWith(dateTimes);
    }
    public static <T> T operator_doubleArrow(Iterable<LocalTime> dateTimes, TimeFillers<T> timeColumn) {
        return timeColumn.fillWith(dateTimes);
    }
}
