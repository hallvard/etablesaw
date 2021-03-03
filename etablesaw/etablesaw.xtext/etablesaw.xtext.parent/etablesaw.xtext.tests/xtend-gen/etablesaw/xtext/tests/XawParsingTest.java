/**
 * generated by Xtext 2.12.0
 */
package etablesaw.xtext.tests;

import com.google.inject.Inject;
import etablesaw.xtext.tests.XawInjectorProvider;
import etablesaw.xtext.xaw.Xaw;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.testing.InjectWith;
import org.eclipse.xtext.testing.XtextRunner;
import org.eclipse.xtext.testing.util.ParseHelper;
import org.eclipse.xtext.testing.validation.ValidationTestHelper;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Extension;
import org.junit.Test;
import org.junit.runner.RunWith;

@RunWith(XtextRunner.class)
@InjectWith(XawInjectorProvider.class)
@SuppressWarnings("all")
public class XawParsingTest {
  @Inject
  @Extension
  private ValidationTestHelper _validationTestHelper;
  
  @Inject
  @Extension
  private ParseHelper<Xaw> _parseHelper;
  
  @Test
  public void loadModel() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("xaw etablesaw.xtext.tests.LoadModelTest");
      _builder.newLine();
      _builder.append("val halAge = 52.0");
      _builder.newLine();
      _builder.append("val table1 =");
      _builder.newLine();
      _builder.append("# String name, double age #");
      _builder.newLine();
      _builder.append("| \"Hallvard\", halAge |");
      _builder.newLine();
      _builder.append("def String helper1(Object o) String.valueOf(o)");
      _builder.newLine();
      _builder.append("helper1(table1.column(\"age\").get(0))");
      _builder.newLine();
      this._validationTestHelper.assertNoErrors(this._parseHelper.parse(_builder));
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
}