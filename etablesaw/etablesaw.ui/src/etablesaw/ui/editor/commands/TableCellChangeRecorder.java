package etablesaw.ui.editor.commands;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.core.commands.ExecutionException;

import etablesaw.ui.editor.TablesawDataProvider;

public class TableCellChangeRecorder implements TablesawDataProvider.Listener {

    private TablesawDataProvider dataProvider = null;
    private Collection<CellChange> allRecordings = null;
    private Collection<CellChange> recordings = null;
    
    public TableCellChangeRecorder() {
    }

    public TableCellChangeRecorder(TablesawDataProvider dataProvider) {
        startRecording(dataProvider);
    }
    
    public void startRecording(TablesawDataProvider dataProvider) {
        dataProvider.addTableChangeListener(this);
        this.dataProvider = dataProvider;
        recordings = new ArrayList<CellChange>();
    }

    public List<CellChange> stopRecording() {
        this.dataProvider.removeTableChangeListener(this);
        if (allRecordings == null) {
            allRecordings = new ArrayList<>();
        }
        allRecordings.addAll(recordings);
        recordings = null;
        return new ArrayList<>(allRecordings);
    }
    
    public boolean hasRecorded() {
        return allRecordings != null && allRecordings.size() > 0;
    }

    public boolean isRecording() {
        return recordings != null;
    }
    
    public void doTableCellChanges(boolean undo) throws ExecutionException {
        for (TableCellChangeRecorder.CellChange tableCellChange : allRecordings) {
            Object value = (undo ? tableCellChange.oldValue : tableCellChange.newValue);
            dataProvider.setDataValue(tableCellChange.column, tableCellChange.row, value);
        }
    }

    @Override
    public void providerRowsChanged(int startRow, int endRow) {
        // ignore
    }

    @Override
    public void tableCellChanged(int row, int column, Object oldValue, Object newValue) {
        if (isRecording()) {
            recordings.add(new CellChange(row, column, oldValue, newValue));
        }
    }
    
    public static class CellChange {
        public final int row, column;
        public final Object oldValue, newValue;

        public CellChange(int row, int column, Object oldValue, Object newValue) {
            super();
            this.row = row;
            this.column = column;
            this.oldValue = oldValue;
            this.newValue = newValue;
        }
    }
}
