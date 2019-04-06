package tablesaw.ui.examples;

import etablesaw.xtext.lib.XawBase;
import tablesaw.ui.examples.Test2_tab1;

@SuppressWarnings("all")
public class Test2 extends XawBase implements Runnable {
  public void run() {
    final Test2_tab1 tab3 = new tablesaw.ui.examples.Test2_tab1("tab1");
    System.out.println(tab3);
  }
  
  public static void main(final String[] args) {
    new Test2().run();
  }
}
