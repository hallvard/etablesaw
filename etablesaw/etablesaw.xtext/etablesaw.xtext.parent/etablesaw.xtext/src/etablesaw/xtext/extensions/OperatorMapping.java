package etablesaw.xtext.extensions;

import static org.eclipse.xtext.naming.QualifiedName.create;

import org.eclipse.xtext.naming.QualifiedName;

public class OperatorMapping extends org.eclipse.xtext.xbase.scoping.featurecalls.OperatorMapping {

	public static final QualifiedName EVAL = create("?");

	@Override
	protected void initializeMapping() {
		map.put(EVAL, create("eval"));
		super.initializeMapping();
	}
}
