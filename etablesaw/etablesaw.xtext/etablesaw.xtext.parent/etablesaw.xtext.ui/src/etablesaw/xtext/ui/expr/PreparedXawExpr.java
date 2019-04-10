package etablesaw.xtext.ui.expr;

import java.io.IOException;
import java.io.InputStream;
import java.io.StringBufferInputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.resource.XtextResourceSet;
import org.eclipse.xtext.util.CancelIndicator;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.interpreter.IEvaluationContext;
import org.eclipse.xtext.xbase.interpreter.IEvaluationResult;
import org.eclipse.xtext.xbase.interpreter.impl.XbaseInterpreter;

import com.google.inject.Guice;
import com.google.inject.Injector;

import etablesaw.ui.expr.PreparedExpr;
import etablesaw.xtext.XawRuntimeModule;
import etablesaw.xtext.jvmmodel.DefaultColumnTypeProvider;
import etablesaw.xtext.jvmmodel.IColumnTypeProvider;
import etablesaw.xtext.xaw.Xaw;
import tech.tablesaw.api.ColumnType;

public class PreparedXawExpr implements PreparedExpr {

    private Injector xawInjector;
    
    protected Injector getInjector() {
        if (xawInjector == null) {
            xawInjector = Guice.createInjector(new XawRuntimeModule());
        }
        return xawInjector;
    }

    protected <T> T getInstance(Class<T> clazz) {
        return getInjector().getInstance(clazz);
    }

    private IColumnTypeProvider columnTypeProvider = new DefaultColumnTypeProvider();
    
    public IColumnTypeProvider getColumnTypeProvider() {
        if (columnTypeProvider == null) {
            columnTypeProvider = getInstance(IColumnTypeProvider.class);
        }
        return columnTypeProvider;
    }
    
    private String expr;
    private Map<String, String> varNameMap;
    private Resource xbaseRes;
    private Xaw xaw;
    
	public PreparedXawExpr(final String expr, final Map<String, ColumnType> varTypes, String colVar) {
	    this.expr = expr;
	    varNameMap = new HashMap<String, String>();
	    StringBuilder builder = new StringBuilder("xaw Expr\n\n");
	    builder.append("def foo(");
	    for (Map.Entry<String, ColumnType> varType : varTypes.entrySet()) {
	        String elementTypeName = simplifyTypeName(DefaultColumnTypeProvider.getElementType(varType.getValue()).getName());
            String varName = varType.getKey();
            if (varName.equals(colVar)) {
                varNameMap.put(colVar, "it");
                varName = "it";
            } else {
                String altName = fixName(varName);
                if (altName != null && (! altName.equals(varName))) {
                    varNameMap.put(varName, altName);
                    varName = altName;
                }
            }
            if (builder.charAt(builder.length() - 1) != '(') {
                builder.append(", ");                
            }
            builder.append(String.format("%s %s", elementTypeName, varName));
        }
	    builder.append(")\n   ");
	    builder.append(expr);
	    builder.append("\n");
	    InputStream input = new StringBufferInputStream(builder.toString());
	    xbaseRes = createResource(URI.createURI("expr.xaw"));
	    try {
	        xbaseRes.load(input, null);
        } catch (IOException e) {
        }
        for (Resource.Diagnostic diagnostic : xbaseRes.getErrors()) {
            diagnostics.add(diagnostic.getMessage());
        }
	    for (Object o : xbaseRes.getContents()) {
	        if (o instanceof Xaw) {
	            xaw = (Xaw) o;
	            return;
	        }
	    }
	}

	protected Resource createResource(URI uri) {
        XtextResourceSet resSet = getInstance(XtextResourceSet.class);
        Resource resource = getInstance(XtextResource.class);
        resource.setURI(uri);
        resSet.getResources().add(resource);
        return resource;
	}

    protected String fixName(String varName) {
        return varName.replaceAll("[\\W]", "_");
    }

    protected String simplifyTypeName(String elementTypeName) {
        String autoImportedPackages[] = {"java.lang."};
        for (int i = 0; i < autoImportedPackages.length; i++) {
            if (elementTypeName.startsWith(autoImportedPackages[i])) {
                elementTypeName = elementTypeName.substring(autoImportedPackages[i].length());
                break;
            }
        }
        return elementTypeName;
    }

	public XExpression getXbaseExpr() {
	    return xaw.getMethods().get(0).getBody();
	}

	@Override
	public String getExpr() {
        return expr;
	}

	@Override
	public ColumnType getColumnType() {
		return null;
	}

	private Collection<String> diagnostics = new ArrayList<String>();

	@Override
	public Collection<String> getDiagnostics() {
		return diagnostics;
	}

	public Object eval(final Map<String, Object> varValues) {
	    XbaseInterpreter xbaseInterpreter = getInstance(XbaseInterpreter.class);
	    IEvaluationContext evaluationContext = getInjector().getProvider(IEvaluationContext.class).get();
		for (final Map.Entry<String, Object> variable : varValues.entrySet()) {
			String varName = variable.getKey();
			if (varNameMap.containsKey(varName)) {
			    varName = varNameMap.get(varName);
			}
			final Object value = variable.getValue();
			evaluationContext.newValue(QualifiedName.create(varName), value);
		}
		IEvaluationResult result = null;
        try {
            XExpression xbaseExpr = getXbaseExpr();
            result = xbaseInterpreter.evaluate(xbaseExpr, evaluationContext, CancelIndicator.NullImpl);
        } catch (Exception e) {
            diagnostics.add(e.getMessage());
        }
		if (result != null && result.getException() != null) {
		    diagnostics.add(result.getException().getMessage());
		    return null;
		}
		return (result != null ? result.getResult() : null);
	}
}
