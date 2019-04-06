package etablesaw.xtext.extensions;

import static org.eclipse.xtext.naming.QualifiedName.create;

import org.eclipse.xtext.naming.QualifiedName;

public class OperatorMapping extends org.eclipse.xtext.xbase.scoping.featurecalls.OperatorMapping {

	public static final QualifiedName EVAL                 = create("?");
	public static final QualifiedName SINGLE_AND           = create("&");
	public static final QualifiedName SINGLE_OR            = create("|");
	public static final QualifiedName SINGLE_AND_ASSIGN    = create("&=");
	public static final QualifiedName SINGLE_OR_ASSIGN     = create("|=");

	@Override
	protected void initializeMapping() {
		map.put(EVAL, create("eval"));
		map.put(SINGLE_AND,           create(OP_PREFIX + "singleAnd"));
		map.put(SINGLE_OR,            create(OP_PREFIX + "singleOr"));
		map.put(SINGLE_AND_ASSIGN,    create(OP_PREFIX + "singleAndAssign"));
		map.put(SINGLE_OR_ASSIGN,     create(OP_PREFIX + "singleOrAssign"));
		super.initializeMapping();
	}
}
