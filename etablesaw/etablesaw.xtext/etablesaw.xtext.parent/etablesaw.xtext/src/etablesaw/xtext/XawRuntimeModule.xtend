/*
 * generated by Xtext 2.12.0
 */
package etablesaw.xtext

import com.google.inject.Binder
import com.google.inject.name.Names
import etablesaw.xtext.extensions.ImplicitlyImportedTypes
import etablesaw.xtext.extensions.OperatorMapping
import etablesaw.xtext.extensions.XawImportSectionNamespaceScopeProvider
import etablesaw.xtext.jvmmodel.DefaultColumnTypeProvider
import etablesaw.xtext.jvmmodel.DefaultTableTypeNameProvider
import etablesaw.xtext.jvmmodel.IColumnTypeProvider
import etablesaw.xtext.jvmmodel.ITableTypeNameProvider
import etablesaw.xtext.jvmmodel.XawCompiler
import etablesaw.xtext.jvmmodel.XawIdentifiableSimpleNameProvider
import etablesaw.xtext.jvmmodel.XawReflectAccess
import etablesaw.xtext.jvmmodel.XawTypeComputer
import org.eclipse.xtext.common.types.util.JavaReflectAccess
import org.eclipse.xtext.scoping.IScopeProvider
import org.eclipse.xtext.scoping.impl.AbstractDeclarativeScopeProvider
import org.eclipse.xtext.xbase.compiler.XbaseCompiler
import org.eclipse.xtext.xbase.featurecalls.IdentifiableSimpleNameProvider
import org.eclipse.xtext.xbase.scoping.batch.ImplicitlyImportedFeatures
import org.eclipse.xtext.xbase.typesystem.computation.ITypeComputer

/**
 * Use this class to register components to be used at runtime / without the Equinox extension registry.
 */
class XawRuntimeModule extends AbstractXawRuntimeModule {
	
	def Class<? extends ImplicitlyImportedFeatures> bindImplicitlyImportedFeatures() {
		ImplicitlyImportedTypes
	}

	override void configureIScopeProviderDelegate(Binder binder) {
		binder.bind(IScopeProvider).annotatedWith(Names.named(AbstractDeclarativeScopeProvider.NAMED_DELEGATE))
			.to(XawImportSectionNamespaceScopeProvider);
	}
	
	def Class<? extends org.eclipse.xtext.xbase.scoping.featurecalls.OperatorMapping> bindOperatorMapping() {
		OperatorMapping
	}
	
	override Class<? extends ITypeComputer> bindITypeComputer() {
		XawTypeComputer
	}

	def Class<? extends XbaseCompiler> bindXbaseCompiler() {
		XawCompiler
	}

    // https://blog.hargrave.io/2007/09/classforname-caches-defined-class-in.html

	def Class<? extends JavaReflectAccess> bindJavaReflectAccess() {
		XawReflectAccess
	}

    override Class<? extends IdentifiableSimpleNameProvider> bindIdentifiableSimpleNameProvider() {
        return XawIdentifiableSimpleNameProvider
    }
    
	def Class<? extends IColumnTypeProvider> bindIColumnTypeProvider() {
		DefaultColumnTypeProvider
	}
    
	def Class<? extends ITableTypeNameProvider> bindITableTypeNameProvider() {
		DefaultTableTypeNameProvider
	}
}