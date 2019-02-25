package etablesaw.xtext.ui;

import org.eclipse.ui.editors.text.TextEditorActionContributor;

public class XawEditorActionContributor extends TextEditorActionContributor {

	//	private IAction interpretAction;
	//
	//	private final XawEditorInterpreter interpreter = new XawEditorInterpreter();
	//
	//	@Override
	//	public void init(final IActionBars bars) {
	//		super.init(bars);
	//		interpretAction = new Action("Run Xaw", interpreter.getXawImageDescriptor()) {
	//			{
	//				setAccelerator(SWT.COMMAND | 'R');
	//			}
	//			@Override
	//			public void run() {
	//				interpreter.interpretActiveXaw(((XtextEditor) getActiveEditorPart()));
	//			}
	//		};
	//		bars.getToolBarManager().add(interpretAction);
	//	}
}
