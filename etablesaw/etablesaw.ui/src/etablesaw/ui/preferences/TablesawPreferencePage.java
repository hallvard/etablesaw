package etablesaw.ui.preferences;

import java.io.File;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.preference.DirectoryFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.dialogs.ContainerSelectionDialog;

import etablesaw.ui.Activator;

public class TablesawPreferencePage extends FieldEditorPreferencePage implements IWorkbenchPreferencePage {
    
    protected IPreferenceStore doGetPreferenceStore() {
        return Activator.getInstance().getPreferenceStore();
    }

    private StringFieldEditor templateLocationEditor;

    @Override
    protected void createFieldEditors() {
        templateLocationEditor = new DirectoryFieldEditor(TablesawPreferenceInitializer.TEMPLATES_LOCATION_PREFERENCE, "Templates folder", getFieldEditorParent()) {
            @Override
            protected String changePressed() {
                return selectWorkspaceFolder(getTextControl().getText());
            }
        };
        addField(templateLocationEditor);
    }

    private String selectWorkspaceFolder(String currentPath) {
        IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
        IContainer folderSelection = root;
        if (currentPath != null && new File(currentPath).exists()) {
            for (IProject project : root.getProjects()) {
                String projectPath = project.getLocation().toString();
                if (currentPath.startsWith(projectPath)) {
                    folderSelection = project.getFolder(new Path(currentPath.substring(projectPath.length() + 1)));
                    break;
                }                    
            }
        }
        ContainerSelectionDialog folderDialog = new ContainerSelectionDialog(getShell(), folderSelection, false, "Select template folder");
        if (folderDialog.open() == Dialog.OK) {
            Object[] selection = folderDialog.getResult();
            if (selection != null && selection.length > 0) {
                Path newPath = new Path(selection[0].toString());
                return root.getFolder(newPath).getLocation().toString();
            }
        }
        return null;
    }

    @Override
    public void init(IWorkbench workbench) {
    }
}
