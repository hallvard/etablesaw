package etablesaw.xtext.ui.commands;

import java.io.IOException;
import java.io.OutputStream;

public class SplittingOutputStream extends OutputStream {

	private final Thread ouputThread;
	private final OutputStream[] outputStreams;

	public SplittingOutputStream(final Thread outputThread, final OutputStream... outputStreams) {
		this.ouputThread = outputThread;
		this.outputStreams = outputStreams;
	}

	public SplittingOutputStream(final boolean onlyCurrentThread, final OutputStream... outputStreams) {
		this(onlyCurrentThread ? Thread.currentThread() : null, outputStreams);
	}

	protected boolean shouldCopy() {
		return (ouputThread == null || Thread.currentThread() == ouputThread);
	}

	public OutputStream[] getOutputStreams() {
		return outputStreams;
	}

	@Override
	public void write(final int b) throws IOException {
		if (shouldCopy()) {
			IOException ex = null;
			for (int i = 0; i < outputStreams.length; i++) {
				try {
					outputStreams[i].write(b);
				} catch (final IOException e) {
					if (ex == null) {
						ex = e;
					}
				}
			}
			if (ex != null) {
				throw ex;
			}
		}
	}

	@Override
	public void write(final byte b[]) throws IOException {
		if (shouldCopy()) {
			IOException ex = null;
			for (int i = 0; i < outputStreams.length; i++) {
				try {
					outputStreams[i].write(b);
				} catch (final IOException e) {
					if (ex == null) {
						ex = e;
					}
				}
			}
			if (ex != null) {
				throw ex;
			}
		}
	}

	@Override
	public void write(final byte b[], final int offset, final int length) throws IOException {
		if (shouldCopy()) {
			IOException ex = null;
			for (int i = 0; i < outputStreams.length; i++) {
				try {
					outputStreams[i].write(b, offset, length);
				} catch (final IOException e) {
					if (ex == null) {
						ex = e;
					}
				}
			}
			if (ex != null) {
				throw ex;
			}
		}
	}

	@Override
	public void close() throws IOException {
		super.close();
		if (shouldCopy()) {
			IOException ex = null;
			for (int i = 0; i < outputStreams.length; i++) {
				try {
					outputStreams[i].close();
				} catch (final IOException e) {
					if (ex == null) {
						ex = e;
					}
				}
			}
			if (ex != null) {
				throw ex;
			}
		}
	}

	@Override
	public void flush() throws IOException {
		super.flush();
		if (shouldCopy()) {
			IOException ex = null;
			for (int i = 0; i < outputStreams.length; i++) {
				try {
					outputStreams[i].flush();
				} catch (final IOException e) {
					if (ex == null) {
						ex = e;
					}
				}
			}
			if (ex != null) {
				throw ex;
			}
		}
	}
}
