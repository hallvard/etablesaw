package etablesaw.xtext.ui.commands;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.handlers.HandlerUtil;
import org.eclipse.xtext.ui.editor.XtextEditor;

public class XawEditorRunXawHandler extends AbstractHandler {

	private final XawEditorInterpreter interpreter = new XawEditorInterpreter();

	@Override
	public Object execute(final ExecutionEvent event) throws ExecutionException {
		final IEditorPart activeEditor = HandlerUtil.getActiveEditor(event);
		if (activeEditor instanceof XtextEditor) {
			interpreter.setErrColor(Display.getCurrent().getSystemColor(SWT.COLOR_RED));
			interpreter.interpretActiveXaw(((XtextEditor) activeEditor));
		}
		return null;
	}
}
