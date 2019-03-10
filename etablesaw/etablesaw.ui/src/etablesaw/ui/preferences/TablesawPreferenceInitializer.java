package etablesaw.ui.preferences;

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.preference.IPreferenceStore;

import etablesaw.ui.Activator;

public class TablesawPreferenceInitializer extends AbstractPreferenceInitializer {

    public final static String TEMPLATES_LOCATION_PREFERENCE = "templatesLocation";
    
    @Override
    public void initializeDefaultPreferences() {
        IPreferenceStore preferences = Activator.getInstance().getPreferenceStore();
        preferences.setDefault(TEMPLATES_LOCATION_PREFERENCE, "");
    }
}
