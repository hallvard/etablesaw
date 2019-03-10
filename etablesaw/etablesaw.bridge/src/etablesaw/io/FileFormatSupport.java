package etablesaw.io;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.function.Supplier;

import tech.tablesaw.api.Table;

public interface FileFormatSupport {
	/**
	 * Indicates support for a specific file format.
	 * @param format File extension of format
	 * @return true if both read and write is supported, null for read-only support and false of no support at all.
	 */
	public Boolean supportsFormat(String format);

	/**
	 * Reads a specific format from input
	 * @param name file name with format extension
	 * @param input
	 * @return A set of tables
	 * @throws IOException
	 */
	public Table[] read(String name, Supplier<InputStream> input) throws IOException;

	/**
	 * Writes table(s) that was previously read
	 * @param tables
	 * @param name file name with format extension
	 * @param output
	 * @throws IOException
	 */
	public void write(Table[] tables, String name, OutputStream output) throws IOException;
}
