package etablesaw.xtext.extensions;

import java.util.Arrays;
import java.util.List;

import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.scoping.impl.ImportNormalizer;
import org.eclipse.xtext.xbase.scoping.XImportSectionNamespaceScopeProvider;

public class XawImportSectionNamespaceScopeProvider extends XImportSectionNamespaceScopeProvider {

	public static final QualifiedName JAVA_UTIL = QualifiedName.create("java","util");
	public static final QualifiedName JAVA_IO = QualifiedName.create("java","io");
	public static final QualifiedName TABLESAW_API = QualifiedName.create("tech","tablesaw","api");
	//	public static final QualifiedName TABLESAW_COLUMN = QualifiedName.create("tech","tablesaw","columns", "Column");
	//	public static final QualifiedName TABLESAW_COLUMNS_NUMBERS = QualifiedName.create("tech","tablesaw","columns", "numbers");
	//	public static final QualifiedName TABLESAW_COLUMNS_STRINGS = QualifiedName.create("tech","tablesaw","columns", "strings");
	//	public static final QualifiedName TABLESAW_CVS = QualifiedName.create("tech","tablesaw","io", "cvs");

	@Override
	protected List<ImportNormalizer> getImplicitImports(final boolean ignoreCase) {
		final List<ImportNormalizer> implicitImports = super.getImplicitImports(ignoreCase);
		implicitImports.addAll(Arrays.asList(
				doCreateImportNormalizer(JAVA_UTIL,						true, false),
				doCreateImportNormalizer(JAVA_IO, 						true, false),
				doCreateImportNormalizer(TABLESAW_API, 					true, false)
				//				doCreateImportNormalizer(TABLESAW_COLUMN, 				false, false),
				//				doCreateImportNormalizer(TABLESAW_COLUMNS_NUMBERS, 		true, false),
				//				doCreateImportNormalizer(TABLESAW_COLUMNS_STRINGS, 		true, false),
				//				doCreateImportNormalizer(TABLESAW_CVS, 					true, false)
				));
		return implicitImports;
	}
}
