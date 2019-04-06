package etablesaw.xtext.jvmmodel;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.xbase.XVariableDeclaration;

import com.google.inject.Inject;

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
