package etablesaw.ui.smile;

import java.util.HashMap;
import java.util.Map;

import tech.tablesaw.api.CategoricalColumn;
import tech.tablesaw.api.IntColumn;
import tech.tablesaw.columns.Column;

public class CategoryMap<T> {

    private Map<T, Integer> category2ordinal = new HashMap<T, Integer>();
    private Map<Integer, T> ord2category = new HashMap<Integer, T>();
    private IntColumn classColumn;
    private int[] classArray = null;
    private CategoricalColumn<T> categoricalColumn;

    public CategoryMap(CategoricalColumn<T> categoricalColumn, String classColumnName) {
        this.categoricalColumn = categoricalColumn;
        classColumn = IntColumn.create(classColumnName, categoricalColumn.size());
        int i = 0;
        for (T t : categoricalColumn) {
            if (! category2ordinal.containsKey(t)) {
                int ordinal = category2ordinal.size();
                category2ordinal.put(t, ordinal);
                ord2category.put(ordinal, t);
            }
            classColumn.set(i, category2ordinal.get(t));
            i++;
        }
        classArray = new int[classColumn.size()];
        i = 0;
        for (Integer label : classColumn) {
            classArray[i] = label;
            i++;
        }
    }
    
    public IntColumn getClassColumn() {
        return classColumn;
    }

    public int[] getClassArray() {
        return classArray;
    }

    public int getCategoryCount() {
        return ord2category.size();
    }

    public T getCategory(int label) {
        return ord2category.get(label);
    }
    
    public CategoricalColumn<T> getCategoricalColumn() {
        return categoricalColumn;
    }
    
    public Column<T> createCategoryColumn(int[] labels, String columnName) {
        Column<T> copy = categoricalColumn.emptyCopy(categoricalColumn.size());
        copy.setName(columnName);
        for (int i = 0; i < labels.length; i++) {
            copy.set(i, getCategory(labels[i]));
        }
        return copy;
    }
}
