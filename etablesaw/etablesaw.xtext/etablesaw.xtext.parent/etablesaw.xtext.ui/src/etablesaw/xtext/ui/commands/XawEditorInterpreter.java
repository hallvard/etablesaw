package etablesaw.xtext.ui.commands;

import java.io.IOException;
import java.io.PrintStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Map;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Color;
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
import org.eclipse.xtext.xbase.interpreter.IEvaluationResult;

import com.google.inject.Injector;

import etablesaw.ui.Activator;
import etablesaw.ui.TableProviderHelper;
import etablesaw.ui.TableProviderRegistry;
import etablesaw.xtext.jvmmodel.XawInterpreter;
import etablesaw.xtext.ui.internal.XtextActivator;
import etablesaw.xtext.xaw.Xaw;
import tech.tablesaw.api.Table;

public class XawEditorInterpreter {

	private XawInterpreter interpreter;

	protected Injector getInjector() {
		final XtextActivator activator = XtextActivator.getInstance();
		return activator != null ? activator.getInjector(XtextActivator.ETABLESAW_XTEXT_XAW) : null;
	}

	protected XawInterpreter getXawInterpreter() {
		if (interpreter == null) {
			final Injector injector = getInjector();
			interpreter = (injector != null ? injector.getInstance(XawInterpreter.class) : null);
		}
		return interpreter;
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

	public void interpretActiveXaw(final XtextEditor xtextEditor) {
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
					final Map<String, Table> tables = interpretActiveXaw(xtextEditor.getDocument());
					if (tables != null) {
						xtextEditor.getShell().getDisplay().asyncExec(new Runnable() {
							@Override
							public void run() {
								// must be done on UI thread
								registerTables(tables);
							}
						});
					}
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

	public Map<String, Table> interpretActiveXaw(final IXtextDocument xtextDocument) {
		return xtextDocument.readOnly(new IUnitOfWork<Map<String, Table>, XtextResource>() {
			@Override
			public Map<String, Table> exec(final XtextResource state) throws Exception {
				if (state != null) {
					for (final EObject eObject : state.getContents()) {
						if (eObject instanceof Xaw) {
							return interpretXaw((Xaw) eObject, true);
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

	protected Map<String, Table> interpretXaw(final Xaw xaw, final boolean registerTables) {
		final XawInterpreter xawInterpreter = getXawInterpreter();
		Throwable ex = null;
		IEvaluationResult result = null;
		try {
			result = xawInterpreter.evaluate(xaw);
			ex = result.getException();
		} catch (final Exception e) {
			ex = e;
		}
		if (ex != null) {
			System.err.println("!!! " + ex);
			ex.printStackTrace(System.err);
		} else {
			System.out.println("=> " + result.getResult());
		}
		return (registerTables ? xawInterpreter.getTopLevelTables() : null);
	}

	protected void registerTables(final Map<String, Table> tables) {
		final TableProviderRegistry tableProviderRegistry = Activator.getInstance().getTableProviderRegistry();
		for (final String varName : tables.keySet()) {
			tableProviderRegistry.registerTableProvider(varName, new TableProviderHelper(tables.get(varName)));
		}
	}
}
