package etablesaw.ui.nattable;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.part.EditorPart;

import etablesaw.ui.Activator;
import etablesaw.ui.TableProvider;
import tech.tablesaw.api.Table;
import tech.tablesaw.io.csv.CsvReadOptions;
import tech.tablesaw.io.csv.CsvReader;
import tech.tablesaw.io.csv.CsvWriteOptions;
import tech.tablesaw.io.csv.CsvWriter;

public class NatTablesawCvsEditor extends EditorPart implements TableProvider, ISelectionProvider {

	private Table modelTable;

	@Override
	public void init(final IEditorSite site, final IEditorInput input) throws PartInitException {
		setSite(site);
		setInput(input);
		if (input instanceof IFileEditorInput) {
			final IFile file = ((IFileEditorInput) input).getFile();
			setPartName(file.getName());
			loadCsv(file, null);
		}
	}

	private CsvReadOptions csvOptions = null;

	protected void loadCsv(final IFile file, final IProgressMonitor monitor) {
		try {
			final char separator = guessSeparator(file.getContents(), ",;\t", 5);
			csvOptions = new CsvReadOptions.
					Builder(new BufferedReader(new InputStreamReader(file.getContents())))
					.tableName(file.getName())
					.separator(separator)
					.build();
			modelTable = new CsvReader().read(csvOptions);
		} catch (final CoreException e) {
		} catch (final IOException e) {
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
	public void doSave(final IProgressMonitor monitor) {
		if (modelTable != null) {
			saveCsv(((IFileEditorInput) getEditorInput()).getFile(), monitor);
		}
	}

	@Override
	public void doSaveAs() {
	}

	protected void saveCsv(final IFile file, final IProgressMonitor monitor) {
		try {
			final ByteArrayOutputStream output = new ByteArrayOutputStream();
			final CsvWriteOptions.Builder csvOptionsBuilder = new CsvWriteOptions.Builder(output);
			if (csvOptions != null) {
				csvOptionsBuilder
				.separator(csvOptions.separator())
				.lineEnd(csvOptions.lineEnding());
			}
			new CsvWriter(modelTable, csvOptionsBuilder.build()).write();
			file.setContents(new ByteArrayInputStream(output.toByteArray()), 0, monitor);
			setDirty(false);
		} catch (final CoreException e) {
		}
	}

	private boolean dirty = false;

	protected void setDirty(final boolean dirty) {
		final boolean changed = this.dirty != dirty;
		this.dirty = dirty;
		if (changed) {
			firePropertyChange(IEditorPart.PROP_DIRTY);
		}
	}

	@Override
	public boolean isDirty() {
		return dirty;
	}

	@Override
	public boolean isSaveAsAllowed() {
		return false;
	}

	private NatTablesawViewer natTablesawViewer;

	@Override
	public void createPartControl(final Composite parent) {
		natTablesawViewer = new NatTablesawViewer();
		natTablesawViewer.setEditable(true);
		natTablesawViewer.createPartControl(parent);
		natTablesawViewer.setInput(modelTable);
		natTablesawViewer.addTableChangeListener(new TablesawDataProvider.Listener() {
			@Override
			public void rowsChanged(final int startRange, final int endRange) {
				// used for filters
			}
			@Override
			public void cellChanged(final int row, final int column, final Object oldValue, final Object newValue) {
				setDirty(true);
			}
		});
		if (getEditorInput() instanceof IFileEditorInput) {
			final IFile file = ((IFileEditorInput) getEditorInput()).getFile();
			Activator.getInstance().getTableProviderRegistry().registerTableProvider(file.getName(), this);
		}
		//		tablesawViewer.addSelectionChangedListener(selectionChangeListener);
	}

	@Override
	public void setFocus() {
	}

	// TableProvider

	@Override
	public Table getTable() {
		return natTablesawViewer.getTable();
	}

	@Override
	public void addTableDataProviderListener(final TableProvider.Listener listener) {
		natTablesawViewer.addTableDataProviderListener(listener);
	}

	@Override
	public void removeTableDataProviderListener(final TableProvider.Listener listener) {
		natTablesawViewer.removeTableDataProviderListener(listener);
	}

	// ISelectionProvider

	@Override
	public void setSelection(final ISelection selection) {
		// not sure this makes much sense
	}

	@Override
	public ISelection getSelection() {
		return StructuredSelection.EMPTY;
	}

	// forward selection changes
	private final ISelectionChangedListener selectionChangeListener = new ISelectionChangedListener() {
		@Override
		public void selectionChanged(final SelectionChangedEvent event) {
			if (selectionListeners != null) {
				for (final ISelectionChangedListener selectionChangedListener : selectionListeners) {
					selectionChangedListener.selectionChanged(event);
				}
			}
		}
	};

	private Collection<ISelectionChangedListener> selectionListeners;

	@Override
	public void addSelectionChangedListener(final ISelectionChangedListener listener) {
		if (selectionListeners == null) {
			selectionListeners = new ArrayList<ISelectionChangedListener>();
		}
		selectionListeners.add(listener);
	}

	@Override
	public void removeSelectionChangedListener(final ISelectionChangedListener listener) {
		if (selectionListeners != null) {
			selectionListeners.remove(listener);
		}
	}

	// IAdaptable

	@Override
	public <T> T getAdapter(final Class<T> adapter) {
		return super.getAdapter(adapter);
	}
}
