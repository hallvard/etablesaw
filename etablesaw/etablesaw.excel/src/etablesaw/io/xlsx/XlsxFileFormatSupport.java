package etablesaw.io.xlsx;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Collection;
import java.util.function.Supplier;

import etablesaw.io.FileFormatSupport;
import tech.tablesaw.api.Table;
import tech.tablesaw.io.xlsx.XlsxReadOptions;
import tech.tablesaw.io.xlsx.XlsxReader;

public class XlsxFileFormatSupport implements FileFormatSupport {

	public XlsxFileFormatSupport() {
	}

	@Override
	public Boolean supportsFormat(final String format) {
		if ("xlsx".equals(format)) {
			return null;
		}
		return false;
	}

	@Override
	public Table[] read(final String name, final Supplier<InputStream> input) throws IOException {
		final XlsxReadOptions options = new XlsxReadOptions.Builder(input.get())
		        .tableName(name)
		        .build();
		final Collection<Table> tables = new XlsxReader().readMultiple(options);
		return tables.toArray(new Table[tables.size()]);
	}

	@Override
	public void write(final Table[] tables, final String name, final OutputStream output) throws IOException {
		throw new UnsupportedOperationException("Write of " + name + " not supported");
	}
}
