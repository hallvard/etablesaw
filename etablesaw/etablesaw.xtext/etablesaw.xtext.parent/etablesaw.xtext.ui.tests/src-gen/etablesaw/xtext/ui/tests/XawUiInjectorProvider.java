/*
 * generated by Xtext 2.14.0
 */
package etablesaw.xtext.ui.tests;

import com.google.inject.Injector;
import etablesaw.xtext.ui.internal.XtextActivator;
import org.eclipse.xtext.testing.IInjectorProvider;

public class XawUiInjectorProvider implements IInjectorProvider {

	@Override
	public Injector getInjector() {
		return XtextActivator.getInstance().getInjector("etablesaw.xtext.Xaw");
	}

}
