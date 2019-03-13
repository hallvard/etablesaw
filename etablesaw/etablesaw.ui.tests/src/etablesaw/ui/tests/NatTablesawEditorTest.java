package etablesaw.ui.tests;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.Path;
import org.eclipse.ui.IEditorPart;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import etablesaw.ui.nattable.NatTablesawEditor;

public class NatTablesawEditorTest extends AbstractWorkbenchTest {
    
    private final static String pluginProject = "etablesaw.ui.tests";
    private final static String projectName = "editortest";

    private IProject project;

    @Before
    public void setUpTestProject() throws Exception {
        project = createProject(projectName);
    }

    private void testOpenEditor(String path, String expected) throws Exception {
        IFile file = createFile(new Path("/" + projectName + "/" + path), getPluginTestFileContents(pluginProject, "/editor-test-files/" + path));
        IEditorPart editor = openEditor(file, "etablesaw.ui.nattable.cvs");
        Assert.assertTrue(editor instanceof NatTablesawEditor);
        NatTablesawEditor tableEditor = (NatTablesawEditor) editor;
        Assert.assertNotNull(tableEditor.getTable());
    }

    @Test
    public void testOpenTestFiles() throws Exception {
        testOpenEditor("csv-example1.csv", "tja");
        testOpenEditor("json-example1.json", "tja");
        testOpenEditor("json-example1.json", "tja");
        testOpenEditor("xlsx-example1.xlsx", "tja");
    }
}
