package etablesaw.ui.tests;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.Path;
import org.eclipse.ui.IEditorPart;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import etablesaw.ui.editor.NatTablesawEditor;

public class NatTablesawEditorTest extends AbstractWorkbenchTest {
    
    private final static String pluginProject = "etablesaw.ui.tests";
    private final static String projectName = "editortest";

    private IProject project;

    @Before
    public void setUpTestProject() throws Exception {
        project = createProject(projectName);
    }

    private NatTablesawEditor testOpenEditor(String path) throws Exception {
        IFile file = createFile(new Path("/" + projectName + "/" + path), getPluginTestFileContents(pluginProject, "/editor-test-files/" + path));
        IEditorPart editor = openEditor(file, "etablesaw.ui.editor.table");
        Assert.assertTrue(editor instanceof NatTablesawEditor);
        NatTablesawEditor tableEditor = (NatTablesawEditor) editor;
        Assert.assertNotNull(tableEditor.getTable());
        return tableEditor;
    }

    @Test
    public void testOpenTestFiles() throws Exception {
        NatTablesawEditor csvEditor = testOpenEditor("csv-example1.csv");
        NatTablesawEditor jsonEditor1 = testOpenEditor("json-example1.json");
        NatTablesawEditor jsonEditor2 = testOpenEditor("json-example1.json");
        NatTablesawEditor xlsxEditor = testOpenEditor("xlsx-example1.xlsx");
    }
}
