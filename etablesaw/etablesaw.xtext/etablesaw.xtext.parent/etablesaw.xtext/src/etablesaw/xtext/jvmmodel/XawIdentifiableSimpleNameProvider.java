package etablesaw.xtext.jvmmodel;

import java.util.Collection;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.xbase.featurecalls.IdentifiableSimpleNameProvider;
import org.eclipse.xtext.xbase.jvmmodel.IJvmModelAssociations;

import com.google.inject.Inject;

import etablesaw.xtext.xaw.Xaw;

public class XawIdentifiableSimpleNameProvider extends IdentifiableSimpleNameProvider {
	
    @Inject
    private IJvmModelAssociations jvmModelAssociations;
    
    @Override
	public String getSimpleName(JvmIdentifiableElement element) {
        Collection<EObject> sourceElements = jvmModelAssociations.getSourceElements(element);
        if (sourceElements != null && sourceElements.size() > 0) {
            final Object sourceElement = sourceElements.iterator().next();
            if (sourceElement instanceof Xaw) {
                String name = ((Xaw) sourceElement).getQName();
                if (element.getIdentifier().equals(((Xaw) sourceElement).getQName())) {
                    return "this";
                }
            }
        }
		return super.getSimpleName(element);
	}
}
