package etablesaw.io.csv;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Supplier;

import etablesaw.io.FileFormatSupport;
import tech.tablesaw.api.Table;
import tech.tablesaw.io.csv.CsvReadOptions;

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
    public Table[] read(final String name, final Supplier<InputStream> input) throws IOException {
        char separator = ',';
        try (InputStream inputStream = input.get()) {
            separator = guessSeparator(inputStream, ",;\t", 5);
        }
        try (InputStream inputStream = input.get()) {
            CsvReadOptions options = CsvReadOptions.builder(inputStream, name)
                    .separator(separator)
                    .build();
            final Table table = Table.read().csv(options);
            return new Table[] { table };
        }
    }

    private char guessSeparator(final InputStream input, final String candidates, final int lineCount) throws IOException {
        final List<String> lines = new ArrayList<>(lineCount);
        try (final BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(input))) {
            String line = null;
            while ((line = bufferedReader.readLine()) != null) {
                lines.add(line);
            }
        }
        final int[][] counts = new int[candidates.length()][lines.size()];
        for (int lineNum = 0; lineNum < lines.size(); lineNum++) {
            final String s = lines.get(lineNum);
            if (s != null) {
                for (int i = 0; i < s.length(); i++) {
                    final int pos = candidates.indexOf(s.charAt(i));
                    if (pos >= 0) {
                        counts[pos][lineNum]++;
                    }
                }
            }
        }
        int best = 0;
        outer: for (int cand = 0; cand < counts.length; cand++) {
            final int firstCount = counts[cand][0];
            if (firstCount < 1) {
                continue;
            }
            for (int lineNum = 1; lineNum < lines.size(); lineNum++) {
                if (counts[cand][lineNum] != firstCount) {
                    continue outer;
                }
            }
            best = cand;
        }
        return candidates.charAt(best);
    }

    @Override
    public void write(final Table[] tables, final String name, final OutputStream output) throws IOException {
        throw new UnsupportedOperationException("Write of " + name + " not supported");
    }
}
