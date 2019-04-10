package etablesaw.xtext.ui.commands;

import java.io.IOException;
import java.io.PrintStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Color;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.console.ConsolePlugin;
import org.eclipse.ui.console.IConsole;
import org.eclipse.ui.console.IConsoleConstants;
import org.eclipse.ui.console.IConsoleManager;
import org.eclipse.ui.console.IConsoleView;
import org.eclipse.ui.console.MessageConsole;
import org.eclipse.ui.console.MessageConsoleStream;
import org.eclipse.xtext.nodemodel.util.NodeModelUtils;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.ui.editor.XtextEditor;
import org.eclipse.xtext.ui.editor.model.IXtextDocument;
import org.eclipse.xtext.util.concurrent.IUnitOfWork;
import org.eclipse.xtext.xbase.interpreter.impl.XbaseInterpreter;

import com.google.inject.Injector;

import etablesaw.xtext.jvmmodel.JavaProjectClassLoader;
import etablesaw.xtext.lib.XawBase;
import etablesaw.xtext.ui.internal.XtextActivator;
import etablesaw.xtext.xaw.Xaw;
import tech.tablesaw.api.Table;

public class XawEditorInterpreter {

    protected Injector getInjector() {
        final XtextActivator activator = XtextActivator.getInstance();
        return activator != null ? activator.getInjector(XtextActivator.ETABLESAW_XTEXT_XAW) : null;
    }

    protected XbaseInterpreter getXawInterpreter() {
        final Injector injector = getInjector();
        return (injector != null ? injector.getInstance(XbaseInterpreter.class) : null);
    }

    private ImageDescriptor xawImageDescriptor = null;

    public ImageDescriptor getXawImageDescriptor() {
        if (xawImageDescriptor == null) {
            try {
                xawImageDescriptor = ImageDescriptor
                        .createFromURL(new URL("platform:/plugin/" + XtextActivator.PLUGIN_ID + "/icons/run_xaw.png"));
            } catch (final MalformedURLException e) {
            }
        }
        return xawImageDescriptor;
    }

    private MessageConsole findConsole(final String name) {
        final ConsolePlugin plugin = ConsolePlugin.getDefault();
        final IConsoleManager conMan = plugin.getConsoleManager();
        final IConsole[] existing = conMan.getConsoles();
        for (int i = 0; i < existing.length; i++) {
            if (name.equals(existing[i].getName())) {
                return (MessageConsole) existing[i];
            }
        }
        final MessageConsole xawConsole = new MessageConsole(name, getXawImageDescriptor());
        conMan.addConsoles(new IConsole[] { xawConsole });
        return xawConsole;
    }

    private MessageConsole interpreterConsole = null;
    private MessageConsoleStream consoleOut = null, consoleErr = null;

    private void setUpConsole() {
        interpreterConsole = findConsole("Xaw");
        consoleOut = interpreterConsole.newMessageStream();
        consoleErr = interpreterConsole.newMessageStream();
        if (outColor != null) {
            consoleOut.setColor(outColor);
        }
        if (errColor != null) {
            consoleErr.setColor(errColor);
        }
        final IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
        final String id = IConsoleConstants.ID_CONSOLE_VIEW;
        try {
            final IConsoleView view = (IConsoleView) page.showView(id);
            view.display(interpreterConsole);
        } catch (final PartInitException e) {
        }
    }

    private PrintStream oldOut = null, oldErr = null;

    private Color outColor = null, errColor = null;

    public void setOutColor(final Color outColor) {
        this.outColor = outColor;
    }

    public void setErrColor(final Color errColor) {
        this.errColor = errColor;
    }

    private void setUpStreams() {
        if (consoleOut != null) {
            System.setOut(new PrintStream(new SplittingOutputStream(true, oldOut = System.out, consoleOut)));
        }
        if (consoleErr != null) {
            System.setErr(new PrintStream(new SplittingOutputStream(true, oldErr = System.err, consoleErr)));
        }
    }

    private void tearDownConsoleAndStreams() {
        if (oldOut != null) {
            System.setOut(oldOut);
            oldOut = null;
        }
        if (oldErr != null) {
            System.setErr(oldErr);
            oldErr = null;
        }
        if (consoleOut != null) {
            try {
                consoleOut.close();
            } catch (final IOException e) {
            }
            consoleOut = null;
        }
        if (consoleErr != null) {
            try {
                consoleErr.close();
            } catch (final IOException e) {
            }
            consoleErr = null;
        }
        interpreterConsole = null;
    }

    private final boolean useConsole = true;

    protected ClassLoader getParentClassLoader() {
        return XawBase.class.getClassLoader();
    }
    
    private Map<String, JavaProjectClassLoader> javaProjectClassLoaders = new HashMap<String, JavaProjectClassLoader>();

    public JavaProjectClassLoader getJavaProjectClassLoader(String projectName) {
        JavaProjectClassLoader javaProjectClassLoader = javaProjectClassLoaders.get(projectName);
        if (javaProjectClassLoader == null) {
            javaProjectClassLoader = new JavaProjectClassLoader(getParentClassLoader(), projectName);
//            Listener listener = new Listener() {
//                @Override
//                public void classLoaderReplaced(JavaProjectClassLoader classLoader) {
//                    JavaProjectClassLoader newClassLoader = new JavaProjectClassLoader(classLoader);
//                    classLoader.removeListener(this);
//                    newClassLoader.addListener(this);
//                    javaProjectClassLoaders.put(projectName, newClassLoader);
//                }
//            };
//            javaProjectClassLoader.addListener(listener);
            javaProjectClassLoaders.put(projectName, javaProjectClassLoader);
        }
        return javaProjectClassLoader;
    }

    public void interpretActiveXaw(final XtextEditor xtextEditor) {
        JavaProjectClassLoader javaProjectClassLoader = null;
        if (xtextEditor.getEditorInput() instanceof IFileEditorInput) {
            IFile file = ((IFileEditorInput) xtextEditor.getEditorInput()).getFile();
            String projectName = file.getFullPath().segment(0);
            if (projectName != null) {
                try {
                    javaProjectClassLoader = getJavaProjectClassLoader(projectName);
                } catch (Exception e) {
                }
            }
        }
        final ClassLoader interpreterClassLoader = javaProjectClassLoader;
        if (useConsole) {
            // must be done outside job
            setUpConsole();
        }
        final Job xawRun = new Job("Xaw") {
            @Override
            protected IStatus run(final IProgressMonitor monitor) {
                // must be done on correct thread
                setUpStreams();
                try {
                    interpretActiveXaw(xtextEditor.getDocument(), interpreterClassLoader);
                    return Status.OK_STATUS;
                } catch (final Exception e) {
                    return Status.CANCEL_STATUS;
                } finally {
                    tearDownConsoleAndStreams();
                }
            }
        };
        xawRun.schedule();
    }

    public void interpretActiveXaw(final IXtextDocument xtextDocument, ClassLoader interpreterClassLoader) {
        xtextDocument.readOnly(new IUnitOfWork<Void, XtextResource>() {
            @Override
            public java.lang.Void exec(XtextResource state) throws Exception {
                if (state != null) {
                    for (final EObject eObject : state.getContents()) {
                        if (eObject instanceof Xaw) {
                            interpretXaw((Xaw) eObject, interpreterClassLoader);
                        }
                    }
                }
                return null;
            }
        });
    }

    private String asSourceText(final EObject eObject) {
        return NodeModelUtils.getTokenText(NodeModelUtils.getNode(eObject));
    }

    protected Map<String, Table> interpretXaw(final Xaw xaw, ClassLoader interpreterClassLoader) {
        XawBase thisXaw;
        try {
            thisXaw = (XawBase) interpreterClassLoader.loadClass(xaw.getQName()).newInstance();
            TableProviderRegistryPorter porter = new TableProviderRegistryPorter();
            thisXaw.setImporter(porter);
            thisXaw.setExporter(porter);
            thisXaw.run();
        } catch (Exception ex) {
          System.err.println("!!! " + ex);
          ex.printStackTrace(System.err);
        }
//        final XbaseInterpreter xawInterpreter = getXawInterpreter();
//        if (interpreterClassLoader != null) {
//            xawInterpreter.setClassLoader(interpreterClassLoader);
//        }
//        XawBase thisXaw = new XawBase();
//        xawInterpreter.setThisXaw(thisXaw);
//        Throwable ex = null;
//        IEvaluationResult result = null;
//        try {
//            final IEvaluationContext context = contextProvider.get();
//            context.newValue(QualifiedName.create("this"), thisXaw);
//            return super.evaluate(xaw, context, CancelIndicator.NullImpl);
//
//            result = xawInterpreter.evaluate(xaw);
//            ex = result.getException();
//        } catch (final Exception e) {
//            ex = e;
//        }
//        if (ex != null) {
//            System.err.println("!!! " + ex);
//            ex.printStackTrace(System.err);
//        } else {
//            System.out.println("=> " + result.getResult());
//        }
        return null;
    }
 }
