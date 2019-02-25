package etablesaw.ui.editor;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import tech.tablesaw.api.Table;
import tech.tablesaw.io.xlsx.XlsxReadOptions;

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
	public Table[] read(final String name, final InputStream input) throws IOException {
		final XlsxReadOptions.Builder builder = new XlsxReadOptions.Builder(input);
		builder.tableName(name);
		final Table[] tables = Table.read().xlsx(builder.build());
		return tables;
	}

	@Override
	public void write(final Table[] tables, final String name, final OutputStream output) throws IOException {
		throw new UnsupportedOperationException("Write of " + name + " not supported");
	}
}
