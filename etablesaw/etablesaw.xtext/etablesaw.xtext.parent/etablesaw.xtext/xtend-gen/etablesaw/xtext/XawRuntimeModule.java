/**
 * generated by Xtext 2.12.0
 */
package etablesaw.xtext;

import com.google.inject.Binder;
import com.google.inject.name.Names;
import etablesaw.xtext.AbstractXawRuntimeModule;
import etablesaw.xtext.extensions.ImplicitlyImportedTypes;
import etablesaw.xtext.extensions.XawImportSectionNamespaceScopeProvider;
import etablesaw.xtext.jvmmodel.DefaultColumnTypeProvider;
import etablesaw.xtext.jvmmodel.DefaultTableTypeNameProvider;
import etablesaw.xtext.jvmmodel.IColumnTypeProvider;
import etablesaw.xtext.jvmmodel.ITableTypeNameProvider;
import etablesaw.xtext.jvmmodel.XawCompiler;
import etablesaw.xtext.jvmmodel.XawIdentifiableSimpleNameProvider;
import etablesaw.xtext.jvmmodel.XawReflectAccess;
import etablesaw.xtext.jvmmodel.XawTypeComputer;
import org.eclipse.xtext.common.types.util.JavaReflectAccess;
import org.eclipse.xtext.scoping.IScopeProvider;
import org.eclipse.xtext.scoping.impl.AbstractDeclarativeScopeProvider;
import org.eclipse.xtext.xbase.compiler.XbaseCompiler;
import org.eclipse.xtext.xbase.featurecalls.IdentifiableSimpleNameProvider;
import org.eclipse.xtext.xbase.scoping.batch.ImplicitlyImportedFeatures;
import org.eclipse.xtext.xbase.scoping.featurecalls.OperatorMapping;
import org.eclipse.xtext.xbase.typesystem.computation.ITypeComputer;

/**
 * Use this class to register components to be used at runtime / without the Equinox extension registry.
 */
@SuppressWarnings("all")
public class XawRuntimeModule extends AbstractXawRuntimeModule {
  public Class<? extends ImplicitlyImportedFeatures> bindImplicitlyImportedFeatures() {
    return ImplicitlyImportedTypes.class;
  }
  
  @Override
  public void configureIScopeProviderDelegate(final Binder binder) {
    binder.<IScopeProvider>bind(IScopeProvider.class).annotatedWith(Names.named(AbstractDeclarativeScopeProvider.NAMED_DELEGATE)).to(XawImportSectionNamespaceScopeProvider.class);
  }
  
  public Class<? extends OperatorMapping> bindOperatorMapping() {
    return etablesaw.xtext.extensions.OperatorMapping.class;
  }
  
  @Override
  public Class<? extends ITypeComputer> bindITypeComputer() {
    return XawTypeComputer.class;
  }
  
  public Class<? extends XbaseCompiler> bindXbaseCompiler() {
    return XawCompiler.class;
  }
  
  public Class<? extends JavaReflectAccess> bindJavaReflectAccess() {
    return XawReflectAccess.class;
  }
  
  @Override
  public Class<? extends IdentifiableSimpleNameProvider> bindIdentifiableSimpleNameProvider() {
    return XawIdentifiableSimpleNameProvider.class;
  }
  
  public Class<? extends IColumnTypeProvider> bindIColumnTypeProvider() {
    return DefaultColumnTypeProvider.class;
  }
  
  public Class<? extends ITableTypeNameProvider> bindITableTypeNameProvider() {
    return DefaultTableTypeNameProvider.class;
  }
}
