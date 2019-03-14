package etablesaw.ui.util;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Supplier;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;

// https://github.com/lawhcd/SWTMultiCheckSelectionCombo
public class MultiCheckSelectionShell {

	private List<Option> options = new ArrayList<Option>();
	private Button[] buttons;
	private final List<SelectionListener> selectionListeners = new ArrayList<SelectionListener>();

	class Option {
		String text;
		boolean selection = false;
		Option(final String text) {
			if (text == null) throw new IllegalArgumentException();
			this.text = text;
		}
		Option(final String text, final boolean selection) {
			if (text == null) throw new IllegalArgumentException();
			this.text = text;
			this.selection = selection;
		}
		void toggle() {
		    selection = ! selection;
		}		    
	}

	List<Option> getOptions() {
        return options;
    }
	
	private Control owner;
	private Listener listener;
	
	private Supplier<String[]> itemsProvider;

	public void setItemsProvider(final Supplier<String[]> itemsProvider) {
		this.itemsProvider = itemsProvider;
	}

	public MultiCheckSelectionShell(Control owner, Listener listener, int eventType) {
	    this.owner = owner;
	    this.listener = listener;
	    if (eventType >= 0) {
    		owner.addListener(eventType, event -> {
    			if (itemsProvider != null) {
    				final String[] newItems = itemsProvider.get();
    				if (newItems != null) {
    		            setItems(newItems, true);
    				}
    			}
    			openShell();
    		});
	    }
	}

	public MultiCheckSelectionShell(Control owner) {
	    this(owner, null, -1);
	}

	private String title = null;
	
	public void setTitle(String title) {
        this.title = title;
    }
	
	private String toggleButtonText = null;
	
	public void setToggleButtonText(String toggleButtonText) {
        this.toggleButtonText = toggleButtonText;
    }

	private boolean notifyOnSelection = false;
	
	public void setNotifyOnSelection(boolean notifyOnSelection) {
        this.notifyOnSelection = notifyOnSelection;
    }
	
	private boolean notifyOnClose = true;
	
	public void setNotifyOnClose(boolean notifyOnClose) {
	    this.notifyOnClose = notifyOnClose;
	}

	private int locationXOffset = 0;
	private int locationYOffset = 0;
	
	public void setLocationOffsets(int locationXOffset, int locationYOffset) {
        this.locationXOffset = locationXOffset;
        this.locationYOffset = locationYOffset;
    }

	private float locationWidthFactor = 0.0f;
	private float locationHeightFactor = 1.0f;

	public void setLocationFactors(float locationWidthFactor, float locationHeightFactor) {
	    this.locationWidthFactor = locationWidthFactor;
	    this.locationHeightFactor = locationHeightFactor;
	}

	public void openShell() {
		final Point p = owner.getParent().toDisplay(owner.getLocation());
		final Point size = owner.getSize();
		final int dx = locationXOffset + (int) (size.x * locationWidthFactor);
        final int dy = locationYOffset + (int) (size.y * locationHeightFactor);
		final Rectangle shellRect = new Rectangle(p.x + dx, p.y + dy, size.x, 0);
		final Shell shell = new Shell(owner.getShell(), SWT.BORDER | (title != null ? SWT.TITLE : SWT.NONE));
		if (title != null) {
		    shell.setText(title);
		}

//		shell.setLayout(new FillLayout());
//		ScrolledComposite scroll = new ScrolledComposite(shell, SWT.H_SCROLL | SWT.V_SCROLL);		
//		final Composite buttonsParent = new Composite(scroll, SWT.NONE);
//		scroll.setContent(buttonsParent);
		
		final Composite buttonsParent = shell;
		int columnCount = (int) Math.pow(getItemCount(), 0.3);
        buttonsParent.setLayout(new GridLayout(columnCount, false));
//        if (title != null) {
//            final Label titleText = new Label(buttonsParent, SWT.NONE);
//            titleText.setText(title);
//            titleText.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, columnCount, 1));
//        }
		if (toggleButtonText != null) {
		    final Button toggle = new Button(buttonsParent, SWT.BUTTON1);
		    toggle.setText(toggleButtonText);
//		    toggle.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, columnCount, 1));
		    toggle.addListener(SWT.MouseDown, e -> {
		        toggleAll();
		        for (final SelectionListener listener : selectionListeners) {
		            listener.widgetSelected(new SelectionEvent(e));
		        }
		    });
		}
		Listener keyListener = event -> {
            if (event.keyCode == SWT.CR) {
                closeShell(buttonsParent, event);
            }
        };
		buttons = new Button[options.size()];
		for (int i = 0; i < options.size(); i++) {
			final Button button = new Button(buttonsParent, SWT.CHECK);
			final Option option = options.get(i);
			button.setText(option.text);
			button.setSelection(option.selection);
			button.addListener(SWT.Selection, event -> {
				option.selection = button.getSelection();
				if (notifyOnSelection) {
    				for (final SelectionListener listener : selectionListeners) {
    				    listener.widgetSelected(new SelectionEvent(event));
    				}
				}
                forwardEvent(event);
			});
			button.pack();
			button.addListener(SWT.KeyDown, keyListener);
			buttons[i] = button;
		}

		shell.pack();
		shell.setLocation(shellRect.x, shellRect.y);

		shell.addListener(SWT.KeyDown, keyListener);
		shell.addListener(SWT.Deactivate, event -> {
			closeShell(buttonsParent, event);
		});
		shell.open();
	}

    private void closeShell(final Composite buttonsParent, Event event) {
        if (buttonsParent != null && (! buttonsParent.isDisposed())) {
        	buttonsParent.setVisible(false);
        	for (int i = 0; i < options.size(); i++) {
        	    if (hasButton(i)) {
        	        buttons[i].dispose();
        	    }
        	}
        	buttons = null;
        	buttonsParent.dispose();
        	forwardEvent(event);
        	if (notifyOnClose) {
        	    for (final SelectionListener listener : selectionListeners) {
        	        listener.widgetSelected(new SelectionEvent(event));
        	    }
        	}
        }
    }

    private void forwardEvent(Event event) {
        if (listener != null) {
            listener.handleEvent(event);
        }
    }

	/**
	 *
	 * Adds the argument to the end of the receiver's list.
	 *
	 * @param string the option to be added
	 * @throws IllegalArgumentException if the string is null
	 * @since version 1.0.0.0
	 */
	public void add(final String string) {
		options.add(new Option(string));
	}

	/**
	 *
	 * Adds the argument with selection to the end of the receiver's list.
	 *
	 * @param string the new item
	 * @param selection default selection of the new item
	 * @throws IllegalArgumentException if the string is null
	 * @since version 1.0.0.0
	 */
	public void add(final String string, final boolean selection) {
		options.add(new Option(string, selection));
	}

	/**
	 *
	 * Adds the argument to the receiver's list at the given zero-relative index.
	 *
	 * @param string the new item
	 * @param index the index for the item
	 * @throws IllegalArgumentException if the string is null
	 * @throws IllegalArgumentException if the index is not between 0 and the number of elements in the list (inclusive)
	 * @since version 1.0.0.0
	 */
	public void add(final String string, final int index) {
		if (index < 0 || index > options.size()) throw new IllegalArgumentException("ERROR_INVALID_RANGE");
		options.add(index, new Option(string));
	}

	/**
	 *
	 * Adds the argument with selection to the receiver's list at the given zero-relative index.
	 *
	 * @param string the new item
	 * @param selection default selection of the new item
	 * @param index the index for the item
	 * @throws IllegalArgumentException if the string is null
	 * @throws IllegalArgumentException if the index is not between 0 and the number of elements in the list (inclusive)
	 * @since version 1.0.0.0
	 */
	public void add(final String string, final boolean selection, final int index) {
		if (index < 0 || index > options.size()) throw new IllegalArgumentException("ERROR_INVALID_RANGE");
		options.add(index, new Option(string, selection));
	}

	/**
	 *
	 * Adds the listener to the collection of listeners who will be notified when the user changes the receiver's selection, by sending it one of the messages defined in the SelectionListener interface.
	 *
	 * widgetSelected is called when the user changes the multi-check-selection-combo's list selection. widgetDefaultSelected is called when the floating shell is deactivated.
	 *
	 * @param listener the listener which should be notified
	 * @throws IllegalArgumentException if the listener is null
	 * @since version 1.0.0.0
	 */
	public void addSelectionListener(final SelectionListener listener) {
		if (listener == null) throw new IllegalArgumentException();
		selectionListeners.add(listener);
	}

	/**
	 *
	 * Deselects the item at the given zero-relative index in the receiver's list. If the item at the index was already deselected, it remains deselected. Indices that are out of range are ignored.
	 *
	 * @param index the index of the item to deselect
	 * @since version 1.0.0.0
	 */
	public void deselect(final int index) {
		if (index >= 0 && index < options.size()) {
			options.get(index).selection = false;
			if (hasButton(index)) {
				buttons[index].setSelection(false);
			}
		}
	}

	/**
	 *
	 * Deselects all selected items in the receiver's list.
	 *
	 * @since version 1.0.0.0
	 */
	public void deselectAll() {
		for (final Option options : options) {
			options.selection = false;
		}
		for (int i = 0; i < options.size(); i++) {
		    if (hasButton(i)) {
		        buttons[i].setSelection(false);
		    }
		}
	}

	/**
	 *
	 * Returns the item at the given, zero-relative index in the receiver's list. Throws an exception if the index is out of range.
	 *
	 * @param index the index of the item to return
	 * @return the item at the given index
	 * @throws IllegalArgumentException if the index is not between 0 and the number of elements in the list minus 1 (inclusive)
	 * @since version 1.0.0.0
	 */
	public String getItem(final int index) {
		checkrange(index);
		return options.get(index).text;
	}



	/**
	 *
	 * Returns the number of items contained in the receiver's list.
	 *
	 * @return the number of items
	 * @since version 1.0.0.0
	 */
	public int getItemCount() {
		return options.size();
	}

	/**
	 *
	 * Returns the zero-relative indices of the items which are currently selected in the receiver's list, or empty array if no item is selected.
	 *
	 * @return the indices of the selected items
	 * @since version 1.0.0.0
	 */
	public int[] getSelectionIndices() {
		final ArrayDeque<Integer> selections = new ArrayDeque<Integer>();
		for (int i = 0; i<options.size(); i++) {
			if (options.get(i).selection) {
				selections.add(i);
			}
		}
		return selections.stream().mapToInt(i->i).toArray();
	}

	/**
	 *
	 * Returns the items which are currently selected in the receiver's list, or empty array if no item is selected.
	 *
	 * @return array of the selected items
	 * @since version 1.0.0.0
	 */
	public String[] getSelections() {
		final ArrayDeque<String> selections = new ArrayDeque<String>();
		for (int i = 0; i<options.size(); i++) {
			final Option o = options.get(i);
			if (o.selection) {
				selections.add(o.text);
			}
		}
		return selections.toArray(new String[selections.size()]);
	}

	/**
	 *
	 * Searches the receiver's list starting at the first item (index 0) until an item is found that is equal to the argument, and returns the index of that item. If no item is found, returns -1.
	 *
	 * @param string the search item
	 * @return index of the item
	 * @throws IllegalArgumentException if the string is null
	 * @since version 1.0.0.0
	 */
	public int indexOf(final String string) {
		if (string == null) throw new IllegalArgumentException();
		for (int i=0; i <options.size(); i++) {
			if (options.get(i).text.equals(string)) {
				return i;
			}
		}
		return -1;
	}

	/**
	 *
	 * Searches the receiver's list starting at the given, zero-relative index until an item is found that is equal to the argument, and returns the index of that item. If no item is found or the starting index is out of range, returns -1.
	 *
	 * @param string the search item
	 * @param start the zero-relative index at which to begin the search
	 * @return index of the item
	 * @throws IllegalArgumentException if the string is null
	 * @since version 1.0.0.0
	 */
	public int indexOf(final String string, final int start) {
		if (string == null) throw new IllegalArgumentException();
		if (start < 0 || start >= options.size()) return -1;
		for (int i=start; i <options.size(); i++) {
			if (options.get(i).text.equals(string)) {
				return i;
			}
		}
		return -1;
	}

	/**
	 *
	 * Removes the item from the receiver's list at the given zero-relative index.
	 *
	 * Disables but does not remove buttons from the floating shell if it is open. The floating shell will be updated when it is deactivated and re-populated
	 *
	 * @param index index for the item
	 * @throws IllegalArgumentException if the index is not between 0 and the number of elements in the list minus 1 (inclusive)
	 * @since version 1.0.0.0
	 */
	public void remove(final int index) {
		checkrange(index);
		options.remove(index);
		if (hasButton(index)) {
			buttons[index].setEnabled(false);
		}
	}

	/**
	 *
	 * Removes the items from the receiver's list which are between the given zero-relative start and end indices (inclusive).
	 *
	 * Disables but does not remove buttons from the floating shell if it is open. The floating shell will be updated when it is deactivated and re-populated
	 *
	 * @param start start of range
	 * @param end end of range
	 * @throws IllegalArgumentException  if either the start or end are not between 0 and the number of elements in the list minus 1 (inclusive)
	 * @since version 1.0.0.0
	 */
	public void remove(final int start, final int end) {
		checkrange(start);
		checkrange(end);
		assert start <= end;
		for (int i = start; i <= end; i++) {
			options.remove(i);
			if (hasButton(i)) {
				buttons[i].setEnabled(false);
			}
		}
	}

	/**
	 *
	 * Searches the receiver's list starting at the first item until an item is found that is equal to the argument, and removes that item from the list.
	 *
	 * Disables but does not remove buttons from the floating shell if it is open. The floating shell will be updated when it is deactivated and re-populated
	 *
	 * @param string the item to remove
	 * @throws IllegalArgumentException if the string is null
	 * @throws IllegalArgumentException if the string is not found in the list.
	 * @since version 1.0.0.0
	 */
	public void remove(final String string) {
		if (string != null) {
			for (int i=0; i < options.size(); i++) {
				if (options.get(i).text.equals(string)) {
					options.remove(i);
					if (hasButton(i)) {
						buttons[i].setEnabled(false);
					}
					return;
				}
			}
		}
		throw new IllegalArgumentException();
	}

	/**
	 *
	 * Removes all of the items from the receiver's list and restores receiver's text field to default.
	 *
	 * Disables but does not remove buttons from the floating shell if it is open. The floating shell will be updated when it is deactivated and re-populated
	 *
	 * @since version 1.0.0.0
	 */
	public void removeAll() {
        for (int i=0; i < options.size(); i++) {
            if (hasButton(i)) {
                buttons[i].setEnabled(false);
            }
        }
		options.clear();
	}

	/**
	 *
	 * Removes the listener from the collection of listeners who will be notified when the user changes the receiver's selection.
	 *
	 * @param listener the listener which should no longer be notified
	 * @throws IllegalArgumentException if the listener is null
	 * @since version 1.0.0.0
	 */
	public void removeSelectionListener(final SelectionListener listener) {
		if (listener == null) throw new IllegalArgumentException();
		selectionListeners.remove(listener);
	}

	private boolean hasButton(int index) {
	    return buttons != null && (index < 0 || (! buttons[index].isDisposed()));
	}

	/**
	 *
	 * Selects the item at the given zero-relative index in the receiver's list. If the item at the index was already selected, it remains selected. Indices that are out of range are ignored.
	 *
	 * @param index the index of item to select
	 * @since version 1.0.0.0
	 */
	public void select(final int index) {
		if (index >= 0 && index < options.size()) {
			options.get(index).selection = true;
			if (hasButton(index)) {
				buttons[index].setSelection(true);
			}
		}
	}

	/**
	 *
	 * Selects the items at the given zero-relative indices in the receiver's list. If the item at the index was already selected, it remains selected. Indices that are out of range are ignored.
	 *
	 * @param indices the indices of items to select
	 * @since version 1.0.0.0
	 */
	public void select(final int[] indices) {
		for (final int i : indices) {
			select(i);
		}
	}

	/**
	 *
	 * Sets the text of the item in the receiver's list at the given zero-relative index to the string argument.
	 *
	 * @param index the index for the item
	 * @param string new text for the item
	 * @throws IllegalArgumentException if the index is not between 0 and the number of elements in the list minus 1 (inclusive)
	 * @throws IllegalArgumentException if the string is null
	 * @since version 1.0.0.0
	 */
	public void setItem(final int index, final String string) {
		checkrange(index);
		if (string == null) throw new IllegalArgumentException();
		options.get(index).text = string;
		if (hasButton(index)) {
			buttons[index].setText(string);
			buttons[index].pack();
		}
	}

	/**
	 *
	 * Sets the receiver's list to be the given array of items.
	 *
	 * @param items the array of items
	 * @throws IllegalArgumentException if the items array is null
	 * @throws IllegalArgumentException if an item in the items array is null

	 * @since version 1.0.0.0
	 */
	public void setItems(final String[] items) {
		options = new ArrayList<Option>(items.length);
		for (final String s : items) {
			add(s);
		}
	}
	
	public void setItems(final String[] newItems, boolean keepSelections) {
	    setItemsAndSelections(newItems, keepSelections ? getSelections() : null);
	}

	public void setItemsAndSelections(final String[] newItems, final String[] selections) {
        setItems(newItems);
        setSelections(selections);
    }

    public void setSelections(final String... selections) {
        if (selections != null) {
            for (String selection : selections) {
                int pos = indexOf(selection);
                if (pos >= 0) {
                    select(pos);
                }
            }
        }
    }

	/**
	 *
	 * Toggles the selection of each item in the receiver's list.
	 *
	 * @since version 1.0.0.0
	 */
	public void toggleAll() {
	    for (int i = 0; i < options.size(); i++) {
			Option option = options.get(i);
            option.toggle();
	        if (hasButton(i)) {
	            buttons[i].setSelection(option.selection);
	        }
		}
	}

	private void checkrange(final int index) {
		if (index < 0 || index >= options.size()) throw new IllegalArgumentException("ERROR_INVALID_RANGE");
	}
}
