package etablesaw.xtext.lib;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;

import org.eclipse.xtext.xbase.lib.Pure;

public class LocalDateTimeExtensions {

	@Pure
	public static LocalDateTime operator_plus(final LocalDate date, final LocalTime time) {
		return LocalDateTime.of(date, time);
	}
	@Pure
	public static LocalDateTime operator_plus(final LocalTime time, final LocalDate date) {
		return LocalDateTime.of(date, time);
	}
	@Pure
	public static LocalDateTime operator_plus(final LocalDateTime dateTime, final LocalTime time) {
		LocalDateTime result = dateTime;
		if (time.getHour() > 0) {
			result = result.plusHours(time.getHour());
		}
		if (time.getMinute() > 0) {
			result = result.plusMinutes(time.getMinute());
		}
		if (time.getSecond() > 0) {
			result = result.plusSeconds(time.getSecond());
		}
		return result;
	}
	@Pure
	public static LocalDateTime operator_minus(final LocalDateTime dateTime, final LocalTime time) {
		LocalDateTime result = dateTime;
		if (time.getHour() > 0) {
			result = result.minusHours(time.getHour());
		}
		if (time.getMinute() > 0) {
			result = result.minusMinutes(time.getMinute());
		}
		if (time.getSecond() > 0) {
			result = result.minusSeconds(time.getSecond());
		}
		return result;
	}
	@Pure
	public static LocalDateTime operator_divide(final LocalDateTime dateTime, final LocalTime time) {
		LocalDateTime result = dateTime;
		if (dateTime.getHour() != time.getHour()) {
			result = result.withHour(time.getHour());
		}
		if (dateTime.getMinute() != time.getMinute()) {
			result = result.withMinute(time.getMinute());
		}
		if (dateTime.getSecond() != time.getSecond()) {
			result = result.withSecond(time.getSecond());
		}
		return result;
	}
	@Pure
	public static LocalDateTime operator_divide(final LocalDateTime dateTime, final LocalDate date) {
		LocalDateTime result = dateTime;
		if (dateTime.getYear() != date.getYear()) {
			result = result.withYear(date.getYear());
		}
		if (dateTime.getMonthValue() != date.getMonthValue()) {
			result = result.withMonth(date.getMonthValue());
		}
		if (dateTime.getDayOfMonth() != date.getDayOfMonth()) {
			result = result.withDayOfMonth(date.getDayOfMonth());
		}
		return result;
	}
}
