package tablesaw.ui.examples;

import etablesaw.xtext.lib.XawBase;
import tablesaw.ui.examples.Test3_tab1;

@SuppressWarnings("all")
public class Test3 extends XawBase implements Runnable {
  public void run() {
    final Test3_tab1 tab3 = new tablesaw.ui.examples.Test3_tab1("tab1");
    System.out.println(tab3);
  }
  
  public static void main(final String[] args) {
    new Test3().run();
  }
}
