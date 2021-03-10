package etablesaw.xtext.extensions;

import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import etablesaw.xtext.lib.ColumnExtensions;
import etablesaw.xtext.lib.ColumnLiterals;
import etablesaw.xtext.lib.ColumnTypeUtil;
import etablesaw.xtext.lib.DateTimeFillersExtensions;
import etablesaw.xtext.lib.DateTimeFiltersExtensions;
import etablesaw.xtext.lib.DoubleColumnExtensions;
import etablesaw.xtext.lib.LocalDateTimeExtensions;
import etablesaw.xtext.lib.NumberFillersExtensions;
import etablesaw.xtext.lib.NumberFiltersExtensions;
import etablesaw.xtext.lib.SelectionExtensions;
import etablesaw.xtext.lib.TableExtensions;

public class ImplicitlyImportedTypes extends org.eclipse.xtext.xbase.scoping.batch.ImplicitlyImportedFeatures {

	@Override
	protected List<Class<?>> getStaticImportClasses() {
		return sorted(super.getStaticImportClasses(),
				ColumnLiterals.class
				);
	}

	@Override
	protected List<Class<?>> getExtensionClasses() {
		return sorted(super.getExtensionClasses(),
		        SelectionExtensions.class,
				TableExtensions.class,
				ColumnExtensions.class,
				NumberFillersExtensions.class,
				NumberFiltersExtensions.class,
				DoubleColumnExtensions.class,
				LocalDateTimeExtensions.class,
				DateTimeFillersExtensions.class,
				DateTimeFiltersExtensions.class
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
