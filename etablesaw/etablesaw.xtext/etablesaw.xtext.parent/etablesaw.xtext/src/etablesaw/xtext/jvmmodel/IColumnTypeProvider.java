package etablesaw.xtext.jvmmodel;

import org.eclipse.xtext.common.types.JvmTypeReference;

public interface IColumnTypeProvider {

	public JvmTypeReference getColumnTypeReference(JvmTypeReference columnElementType);

}
