package etablesaw.xtext.extensions;

import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import etablesaw.xtext.lib.TablesawExtensions;

public class ImplicitlyImportedTypes extends org.eclipse.xtext.xbase.scoping.batch.ImplicitlyImportedFeatures {

	@Override
	protected List<Class<?>> getStaticImportClasses() {
		return sorted(super.getStaticImportClasses()
				);
	}

	@Override
	protected List<Class<?>> getExtensionClasses() {
		return sorted(super.getExtensionClasses(),
				TablesawExtensions.class
				);
	}

	public final static Comparator<Class<?>> classComparator = new Comparator<Class<?>>() {
		@Override
		public int compare(final Class<?> c1, final Class<?> c2) {
			return c1.getSimpleName().compareTo(c2.getSimpleName());
		}
	};

	private List<Class<?>> sorted(final List<Class<?>> classList, final Class<?>... classesToAdd) {
		classList.addAll(Arrays.asList(classesToAdd));
		Collections.sort(classList, classComparator);
		return classList;
	}
}
