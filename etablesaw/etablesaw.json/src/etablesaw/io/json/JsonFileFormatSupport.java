package etablesaw.io.json;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.function.Supplier;

import etablesaw.io.FileFormatSupport;
import tech.tablesaw.api.Table;
import tech.tablesaw.io.json.JsonReadOptions;
import tech.tablesaw.io.json.JsonReader;

public class JsonFileFormatSupport implements FileFormatSupport {

	public JsonFileFormatSupport() {
	}

	@Override
	public Boolean supportsFormat(final String format) {
		if ("json".equals(format)) {
			return null;
		}
		return false;
	}

	@Override
	public Table[] read(final String name, final Supplier<InputStream> input) throws IOException {
		final JsonReadOptions builder = JsonReadOptions.builder(input.get())
		        .tableName(name)
		        .build();
		Table table = new JsonReader().read(builder);
		return (table != null ? new Table[]{ table } : new Table[0]);
	}

	@Override
	public void write(final Table[] tables, final String name, final OutputStream output) throws IOException {
		throw new UnsupportedOperationException("Write of " + name + " not supported");
	}
}
