package etablesaw.ui.examples;

import etablesaw.xtext.lib.TableDef;
import etablesaw.xtext.lib.TypedTable;
import tech.tablesaw.api.IntColumn;
import tech.tablesaw.api.StringColumn;

@TableDef(columnNames = { "studieprogramkode", "studentnr", "karakter", "vurdresstatnavn" }, columnTypes = { String.class, int.class, String.class, String.class })
@SuppressWarnings("all")
public class combineAndSortTDT4100n_tdt4100n extends TypedTable<combineAndSortTDT4100n_tdt4100n.Row> {
  private final StringColumn studieprogramkodeColumn;
  
  private final IntColumn studentnrColumn;
  
  private final StringColumn karakterColumn;
  
  private final StringColumn vurdresstatnavnColumn;
  
  public combineAndSortTDT4100n_tdt4100n(final String tableName, final StringColumn studieprogramkodeColumn, final IntColumn studentnrColumn, final StringColumn karakterColumn, final StringColumn vurdresstatnavnColumn) {
    super(tableName, studieprogramkodeColumn, studentnrColumn, karakterColumn, vurdresstatnavnColumn);
    this.studieprogramkodeColumn = studieprogramkodeColumn;
    this.studentnrColumn = studentnrColumn;
    this.karakterColumn = karakterColumn;
    this.vurdresstatnavnColumn = vurdresstatnavnColumn;
    
  }
  
  public combineAndSortTDT4100n_tdt4100n(final String tableName) {
    this(tableName, StringColumn.create("studieprogramkode"), IntColumn.create("studentnr"), StringColumn.create("karakter"), StringColumn.create("vurdresstatnavn"));
  }
  
  public StringColumn getStudieprogramkodeColumn() {
    return studieprogramkodeColumn;
  }
  
  public IntColumn getStudentnrColumn() {
    return studentnrColumn;
  }
  
  public StringColumn getKarakterColumn() {
    return karakterColumn;
  }
  
  public StringColumn getVurdresstatnavnColumn() {
    return vurdresstatnavnColumn;
  }
  
  public combineAndSortTDT4100n_tdt4100n emptyCopy() {
    return new combineAndSortTDT4100n_tdt4100n(name(), getStudieprogramkodeColumn().emptyCopy(), getStudentnrColumn().emptyCopy(), getKarakterColumn().emptyCopy(), getVurdresstatnavnColumn().emptyCopy());
  }
  
  public interface RowData {
    public abstract String getStudieprogramkode();
    
    public abstract int getStudentnr();
    
    public abstract String getKarakter();
    
    public abstract String getVurdresstatnavn();
  }
  
  public static class Row extends tech.tablesaw.api.Row implements combineAndSortTDT4100n_tdt4100n.RowData {
    public String getStudieprogramkode() {
      return table.getStudieprogramkodeColumn().get(getRowNumber());
    }
    
    public int getStudentnr() {
      return table.getStudentnrColumn().get(getRowNumber());
    }
    
    public String getKarakter() {
      return table.getKarakterColumn().get(getRowNumber());
    }
    
    public String getVurdresstatnavn() {
      return table.getVurdresstatnavnColumn().get(getRowNumber());
    }
    
    private final combineAndSortTDT4100n_tdt4100n table;
    
    public Row(final combineAndSortTDT4100n_tdt4100n table) {
      super(table);
      this.table = table;
    }
    
    @Override
    public combineAndSortTDT4100n_tdt4100n.Row next() {
      super.next();
      return this;
    }
    
    public combineAndSortTDT4100n_tdt4100n.Row setStudieprogramkode(final String studieprogramkode) {
      table.getStudieprogramkodeColumn().set(getRowNumber(), studieprogramkode);
      return this;
    }
    
    public combineAndSortTDT4100n_tdt4100n.Row setStudentnr(final int studentnr) {
      table.getStudentnrColumn().set(getRowNumber(), studentnr);
      return this;
    }
    
    public combineAndSortTDT4100n_tdt4100n.Row setKarakter(final String karakter) {
      table.getKarakterColumn().set(getRowNumber(), karakter);
      return this;
    }
    
    public combineAndSortTDT4100n_tdt4100n.Row setVurdresstatnavn(final String vurdresstatnavn) {
      table.getVurdresstatnavnColumn().set(getRowNumber(), vurdresstatnavn);
      return this;
    }
  }
  
  protected combineAndSortTDT4100n_tdt4100n.Row row() {
    return new Row(this);
  }
  
  public combineAndSortTDT4100n_tdt4100n append(final combineAndSortTDT4100n_tdt4100n.Row row) {
    getStudieprogramkodeColumn().append(row.getStudieprogramkode());
    getStudentnrColumn().append(row.getStudentnr());
    getKarakterColumn().append(row.getKarakter());
    getVurdresstatnavnColumn().append(row.getVurdresstatnavn());
    return this;
  }
}
