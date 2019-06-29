package etablesaw.xtext.jvmmodel;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.xbase.XVariableDeclaration;
import org.eclipse.xtext.xbase.lib.IterableExtensions;

import com.google.inject.Inject;

import etablesaw.xtext.xaw.InlineTableRow;
import etablesaw.xtext.xaw.TableColumn;
import etablesaw.xtext.xaw.TableDef;
import etablesaw.xtext.xaw.TableLiteral;
import etablesaw.xtext.xaw.Xaw;

public class XawUtil {

    @Inject
    private ITableTypeNameProvider tableTypeNameProvider;
    
    public String getTableTypeName(TableDef tableDef) {
        return tableTypeNameProvider.getTableTypeName(getXaw(tableDef).getQName(), tableDef.getName());
    }

    public String getTableTypeName(TableLiteral tableLiteral) {
        String name = tableLiteral.getName();
        if (name == null && tableLiteral.eContainer() instanceof XVariableDeclaration) {
            name = ((XVariableDeclaration) tableLiteral.eContainer()).getName();
        }
        return (name != null ? tableTypeNameProvider.getTableTypeName(getXaw(tableLiteral).getQName(), name) : name);
    }
    
    public Iterable<TableColumn> getTableColumns(final TableLiteral literal) {
        return IterableExtensions.filter(literal.getExpressions(), TableColumn.class);
    }

    public Iterable<InlineTableRow> getTableRows(final TableLiteral literal) {
        return IterableExtensions.filter(literal.getExpressions(), InlineTableRow.class);
    }

    public boolean isColumnTable(TableLiteral tableLiteral) {
        if (tableLiteral.getName() != null) {
            return false;
        }
        int colCount = 0;
        for (final TableColumn col : getTableColumns(tableLiteral)) {
            if (colCount > 1 || col.getExpression() != null) {
                return false;
            }
            colCount++;
        }
        return colCount == 1;
    }
    
    public Xaw getXaw(EObject eObject) {
        while (eObject != null) {
            if (eObject instanceof Xaw) {
                return (Xaw) eObject;
            }
            eObject = eObject.eContainer();
        }
        return null;
    }
}
