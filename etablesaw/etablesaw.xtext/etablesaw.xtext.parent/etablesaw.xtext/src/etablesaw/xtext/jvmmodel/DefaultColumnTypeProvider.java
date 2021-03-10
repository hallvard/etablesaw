package etablesaw.xtext.jvmmodel;

import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.xbase.jvmmodel.JvmTypesBuilder;

import com.google.inject.Inject;

import etablesaw.xtext.lib.ColumnTypeUtil;

public class DefaultColumnTypeProvider implements IColumnTypeProvider {
        
	@Inject
	private JvmTypesBuilder typeRefBuilder;

	@Override
	public JvmTypeReference getColumnTypeReference(final JvmTypeReference elementType) {
	    String qName = elementType.getQualifiedName();
        Class<?> columnType = ColumnTypeUtil.getColumnType(qName);
	    if (columnType == null) {
	        throw new RuntimeException("Unsupported column type: " + qName);	        
	    }
        return typeRefBuilder.newTypeRef(elementType, columnType);
	}

	protected boolean isSame(final Class<?> clazz, final JvmTypeReference type) {
		return clazz.getName().equals(type.getQualifiedName());
	}
}
