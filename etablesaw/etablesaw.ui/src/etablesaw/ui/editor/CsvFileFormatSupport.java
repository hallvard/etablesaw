package etablesaw.ui.editor;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import tech.tablesaw.api.Table;

public class CsvFileFormatSupport implements FileFormatSupport {

	public CsvFileFormatSupport() {
	}

	@Override
	public Boolean supportsFormat(final String format) {
		if ("csv".equals(format)) {
			return null;
		}
		return false;
	}

	@Override
	public Table[] read(final String name, final InputStream input) throws IOException {
		final Table table = Table.read().csv(input, name);
		return new Table[]{table};
	}

	@Override
	public void write(final Table[] tables, final String name, final OutputStream output) throws IOException {
		throw new UnsupportedOperationException("Write of " + name + " not supported");
	}
}
