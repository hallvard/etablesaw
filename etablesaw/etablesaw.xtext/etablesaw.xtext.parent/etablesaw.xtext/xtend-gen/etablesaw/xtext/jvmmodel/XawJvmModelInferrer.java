/**
 * generated by Xtext 2.12.0
 */
package etablesaw.xtext.jvmmodel;

import com.google.common.base.Objects;
import com.google.common.collect.Iterables;
import com.google.common.collect.Iterators;
import com.google.inject.Inject;
import etablesaw.xtext.jvmmodel.IColumnTypeProvider;
import etablesaw.xtext.jvmmodel.ITableTypeNameProvider;
import etablesaw.xtext.jvmmodel.XawUtil;
import etablesaw.xtext.lib.TypedTable;
import etablesaw.xtext.lib.XawBase;
import etablesaw.xtext.xaw.TableColumn;
import etablesaw.xtext.xaw.TableColumnDef;
import etablesaw.xtext.xaw.TableDef;
import etablesaw.xtext.xaw.TableLiteral;
import etablesaw.xtext.xaw.XMethod;
import etablesaw.xtext.xaw.Xaw;
import java.util.Arrays;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.common.types.JvmAnnotationReference;
import org.eclipse.xtext.common.types.JvmAnnotationValue;
import org.eclipse.xtext.common.types.JvmConstructor;
import org.eclipse.xtext.common.types.JvmField;
import org.eclipse.xtext.common.types.JvmFormalParameter;
import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.JvmMember;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmStringAnnotationValue;
import org.eclipse.xtext.common.types.JvmTypeAnnotationValue;
import org.eclipse.xtext.common.types.JvmTypeParameter;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.JvmVisibility;
import org.eclipse.xtext.common.types.TypesFactory;
import org.eclipse.xtext.common.types.util.TypeReferences;
import org.eclipse.xtext.xbase.compiler.output.ITreeAppendable;
import org.eclipse.xtext.xbase.jvmmodel.AbstractModelInferrer;
import org.eclipse.xtext.xbase.jvmmodel.IJvmDeclaredTypeAcceptor;
import org.eclipse.xtext.xbase.jvmmodel.JvmAnnotationReferenceBuilder;
import org.eclipse.xtext.xbase.jvmmodel.JvmTypesBuilder;
import org.eclipse.xtext.xbase.lib.Extension;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.IteratorExtensions;
import org.eclipse.xtext.xbase.lib.ObjectExtensions;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import tech.tablesaw.api.Row;

/**
 * <p>Infers a JVM model from the source model.</p>
 * 
 * <p>The JVM model should contain all elements that would appear in the Java code
 * which is generated from the source model. Other models link against the JVM model rather than the source model.</p>
 */
@SuppressWarnings("all")
public class XawJvmModelInferrer extends AbstractModelInferrer {
  /**
   * convenience API to build and initialize JVM types and their members.
   */
  @Inject
  @Extension
  private JvmTypesBuilder _jvmTypesBuilder;
  
  @Inject
  @Extension
  private TypeReferences _typeReferences;
  
  @Inject
  @Extension
  private XawUtil _xawUtil;
  
  protected void _infer(final Xaw xaw, final IJvmDeclaredTypeAcceptor acceptor, final boolean isPreIndexingPhase) {
    final Procedure1<JvmGenericType> _function = (JvmGenericType it) -> {
      EList<JvmTypeReference> _superTypes = it.getSuperTypes();
      JvmTypeReference _typeRef = this._typeReferenceBuilder.typeRef(XawBase.class);
      this._jvmTypesBuilder.<JvmTypeReference>operator_add(_superTypes, _typeRef);
    };
    final JvmGenericType clazz = this._jvmTypesBuilder.toClass(xaw, xaw.getQName(), _function);
    final Procedure1<JvmGenericType> _function_1 = (JvmGenericType it) -> {
      EList<JvmMember> _members = it.getMembers();
      final Procedure1<JvmOperation> _function_2 = (JvmOperation it_1) -> {
        it_1.setVisibility(JvmVisibility.PUBLIC);
        final JvmOperation method = it_1;
        this._jvmTypesBuilder.setBody(it_1, xaw);
      };
      JvmOperation _method = this._jvmTypesBuilder.toMethod(xaw, "run", this._typeReferenceBuilder.typeRef(Void.TYPE), _function_2);
      this._jvmTypesBuilder.<JvmOperation>operator_add(_members, _method);
      EList<JvmMember> _members_1 = it.getMembers();
      final Procedure1<JvmOperation> _function_3 = (JvmOperation it_1) -> {
        it_1.setVisibility(JvmVisibility.PUBLIC);
        it_1.setStatic(true);
        EList<JvmFormalParameter> _parameters = it_1.getParameters();
        JvmFormalParameter _parameter = this._jvmTypesBuilder.toParameter(xaw, "args", this._typeReferences.createArrayType(this._typeReferenceBuilder.typeRef(String.class)));
        this._jvmTypesBuilder.<JvmFormalParameter>operator_add(_parameters, _parameter);
        final Procedure1<ITreeAppendable> _function_4 = (ITreeAppendable it_2) -> {
          StringConcatenation _builder = new StringConcatenation();
          _builder.append("new ");
          String _simpleName = clazz.getSimpleName();
          _builder.append(_simpleName);
          _builder.append("().run();");
          it_2.append(_builder);
        };
        this._jvmTypesBuilder.setBody(it_1, _function_4);
      };
      JvmOperation _method_1 = this._jvmTypesBuilder.toMethod(xaw, "main", this._typeReferenceBuilder.typeRef(Void.TYPE), _function_3);
      this._jvmTypesBuilder.<JvmOperation>operator_add(_members_1, _method_1);
      EList<XMethod> _methods = xaw.getMethods();
      for (final XMethod method : _methods) {
        this.inferMethod(clazz, method);
      }
    };
    acceptor.<JvmGenericType>accept(clazz, _function_1);
    EList<TableDef> _tableDefs = xaw.getTableDefs();
    for (final TableDef tableDef : _tableDefs) {
      {
        final JvmGenericType tableClass = this.inferTableClass(xaw, this._xawUtil.getTableTypeName(tableDef), tableDef.getTableColumDefs(), acceptor);
        this.optionallyAddAsMember(clazz, tableClass);
      }
    }
    final Procedure1<TableLiteral> _function_2 = (TableLiteral it) -> {
      boolean _isColumnTable = this._xawUtil.isColumnTable(it);
      boolean _not = (!_isColumnTable);
      if (_not) {
        final Function1<TableColumn, TableColumnDef> _function_3 = (TableColumn it_1) -> {
          return it_1.getColumnDef();
        };
        final Iterable<TableColumnDef> columnDefs = IterableExtensions.<TableColumn, TableColumnDef>map(Iterables.<TableColumn>filter(it.getExpressions(), TableColumn.class), _function_3);
        boolean _hasNext = columnDefs.iterator().hasNext();
        if (_hasNext) {
          final JvmGenericType tableClass = this.inferTableClass(xaw, this._xawUtil.getTableTypeName(it), columnDefs, acceptor);
          this.optionallyAddAsMember(clazz, tableClass);
        }
      }
    };
    IteratorExtensions.<TableLiteral>forEach(Iterators.<TableLiteral>filter(xaw.eAllContents(), TableLiteral.class), _function_2);
  }
  
  public boolean optionallyAddAsMember(final JvmGenericType clazz, final JvmGenericType tableClass) {
    boolean _xifexpression = false;
    int _indexOf = tableClass.getQualifiedName().indexOf(".");
    boolean _lessThan = (_indexOf < 0);
    if (_lessThan) {
      boolean _xblockexpression = false;
      {
        tableClass.setStatic(true);
        EList<JvmMember> _members = clazz.getMembers();
        _xblockexpression = this._jvmTypesBuilder.<JvmGenericType>operator_add(_members, tableClass);
      }
      _xifexpression = _xblockexpression;
    }
    return _xifexpression;
  }
  
  public boolean inferMethod(final JvmGenericType jvmClass, final XMethod xMethod) {
    EList<JvmMember> _members = jvmClass.getMembers();
    final Procedure1<JvmOperation> _function = (JvmOperation it) -> {
      EList<JvmTypeParameter> _typeParameters = xMethod.getTypeParameters();
      for (final JvmTypeParameter typeParameter : _typeParameters) {
        EList<JvmTypeParameter> _typeParameters_1 = it.getTypeParameters();
        JvmTypeParameter _cloneWithProxies = this._jvmTypesBuilder.<JvmTypeParameter>cloneWithProxies(typeParameter);
        this._jvmTypesBuilder.<JvmTypeParameter>operator_add(_typeParameters_1, _cloneWithProxies);
      }
      it.setVisibility(JvmVisibility.PRIVATE);
      int paramNum = 1;
      EList<JvmFormalParameter> _parameters = xMethod.getParameters();
      for (final JvmFormalParameter parameter : _parameters) {
        {
          String _elvis = null;
          String _name = parameter.getName();
          if (_name != null) {
            _elvis = _name;
          } else {
            _elvis = ("arg" + Integer.valueOf(paramNum));
          }
          final JvmFormalParameter formalParameter = this._jvmTypesBuilder.toParameter(parameter, _elvis, parameter.getParameterType());
          EList<JvmFormalParameter> _parameters_1 = it.getParameters();
          this._jvmTypesBuilder.<JvmFormalParameter>operator_add(_parameters_1, formalParameter);
          paramNum++;
        }
      }
      this._jvmTypesBuilder.setBody(it, xMethod.getBody());
    };
    JvmOperation _method = this._jvmTypesBuilder.toMethod(xMethod, xMethod.getName(), xMethod.getReturnType(), _function);
    return this._jvmTypesBuilder.<JvmOperation>operator_add(_members, _method);
  }
  
  @Inject
  @Extension
  private TypesFactory typesFactory;
  
  @Inject
  @Extension
  private ITableTypeNameProvider tableTypeNameProvider;
  
  @Inject
  @Extension
  private IColumnTypeProvider columnTypeProvider;
  
  public JvmGenericType inferTableClass(final EObject owner, final String typeName, final Iterable<TableColumnDef> columns, final IJvmDeclaredTypeAcceptor acceptor) {
    JvmGenericType _xblockexpression = null;
    {
      final JvmGenericType clazz = this._jvmTypesBuilder.toClass(owner, typeName);
      final Procedure1<JvmGenericType> _function = (JvmGenericType it) -> {
        it.setInterface(true);
      };
      final JvmGenericType rowInterface = this._jvmTypesBuilder.toInterface(owner, "RowData", _function);
      final Procedure1<JvmGenericType> _function_1 = (JvmGenericType it) -> {
        EList<JvmTypeReference> _superTypes = it.getSuperTypes();
        JvmTypeReference _typeRef = this._typeReferenceBuilder.typeRef(rowInterface);
        this._jvmTypesBuilder.<JvmTypeReference>operator_add(_superTypes, _typeRef);
      };
      final JvmGenericType rowClass = this._jvmTypesBuilder.toClass(owner, "Row", _function_1);
      EList<JvmTypeReference> _superTypes = clazz.getSuperTypes();
      JvmTypeReference _typeRef = this._typeReferenceBuilder.typeRef(TypedTable.class, this._typeReferenceBuilder.typeRef(rowClass));
      this._jvmTypesBuilder.<JvmTypeReference>operator_add(_superTypes, _typeRef);
      final Procedure1<JvmGenericType> _function_2 = (JvmGenericType it) -> {
        this.initRowClass(owner, columns, clazz, rowInterface);
        this.initRowClass(owner, columns, clazz, rowClass);
        final JvmAnnotationReference annotation = this.inferTableClassAnnotations(columns, clazz);
        EList<JvmAnnotationReference> _annotations = clazz.getAnnotations();
        this._jvmTypesBuilder.<JvmAnnotationReference>operator_add(_annotations, annotation);
        final Function1<TableColumnDef, Boolean> _function_3 = (TableColumnDef it_1) -> {
          return Boolean.valueOf(((it_1.getType() != null) && (!Objects.equal(it_1.getType().getIdentifier(), "void"))));
        };
        final Iterable<TableColumnDef> tableColumns = IterableExtensions.<TableColumnDef>filter(columns, _function_3);
        for (final TableColumnDef column : tableColumns) {
          EList<JvmMember> _members = it.getMembers();
          final Procedure1<JvmField> _function_4 = (JvmField it_1) -> {
            it_1.setVisibility(JvmVisibility.PRIVATE);
            it_1.setFinal(true);
          };
          JvmField _field = this._jvmTypesBuilder.toField(owner, this.tableTypeNameProvider.getColumnName(column.getName()), this.columnTypeProvider.getColumnTypeReference(column.getType()), _function_4);
          this._jvmTypesBuilder.<JvmField>operator_add(_members, _field);
        }
        EList<JvmMember> _members_1 = it.getMembers();
        final Procedure1<JvmConstructor> _function_5 = (JvmConstructor it_1) -> {
          EList<JvmFormalParameter> _parameters = it_1.getParameters();
          JvmFormalParameter _parameter = this._jvmTypesBuilder.toParameter(owner, "tableName", this._typeReferenceBuilder.typeRef(String.class));
          this._jvmTypesBuilder.<JvmFormalParameter>operator_add(_parameters, _parameter);
          for (final TableColumnDef expr : tableColumns) {
            EList<JvmFormalParameter> _parameters_1 = it_1.getParameters();
            String _name = expr.getName();
            String _plus = (_name + "Column");
            JvmFormalParameter _parameter_1 = this._jvmTypesBuilder.toParameter(owner, _plus, this.columnTypeProvider.getColumnTypeReference(expr.getType()));
            this._jvmTypesBuilder.<JvmFormalParameter>operator_add(_parameters_1, _parameter_1);
          }
          final Procedure1<ITreeAppendable> _function_6 = (ITreeAppendable it_2) -> {
            StringConcatenation _builder = new StringConcatenation();
            _builder.append("super(tableName, ");
            final Function1<TableColumnDef, CharSequence> _function_7 = (TableColumnDef it_3) -> {
              return this.tableTypeNameProvider.getColumnName(it_3.getName());
            };
            String _join = IterableExtensions.<TableColumnDef>join(tableColumns, ", ", _function_7);
            _builder.append(_join);
            _builder.append(");");
            it_2.append(_builder);
            it_2.newLine();
            for (final TableColumnDef expr_1 : tableColumns) {
              {
                StringConcatenation _builder_1 = new StringConcatenation();
                _builder_1.append("this.");
                String _name_1 = expr_1.getName();
                _builder_1.append(_name_1);
                _builder_1.append("Column = ");
                String _columnName = this.tableTypeNameProvider.getColumnName(expr_1.getName());
                _builder_1.append(_columnName);
                _builder_1.append(";");
                it_2.append(_builder_1);
                it_2.newLine();
              }
            }
          };
          this._jvmTypesBuilder.setBody(it_1, _function_6);
        };
        JvmConstructor _constructor = this._jvmTypesBuilder.toConstructor(owner, _function_5);
        this._jvmTypesBuilder.<JvmConstructor>operator_add(_members_1, _constructor);
        boolean _isEmpty = IterableExtensions.isEmpty(tableColumns);
        boolean _not = (!_isEmpty);
        if (_not) {
          EList<JvmMember> _members_2 = it.getMembers();
          final Procedure1<JvmConstructor> _function_6 = (JvmConstructor it_1) -> {
            EList<JvmFormalParameter> _parameters = it_1.getParameters();
            JvmFormalParameter _parameter = this._jvmTypesBuilder.toParameter(owner, "tableName", this._typeReferenceBuilder.typeRef(String.class));
            this._jvmTypesBuilder.<JvmFormalParameter>operator_add(_parameters, _parameter);
            final Procedure1<ITreeAppendable> _function_7 = (ITreeAppendable it_2) -> {
              StringConcatenation _builder = new StringConcatenation();
              _builder.append("this(tableName");
              it_2.append(_builder);
              for (final TableColumnDef column_1 : tableColumns) {
                {
                  StringConcatenation _builder_1 = new StringConcatenation();
                  _builder_1.append(", ");
                  it_2.append(_builder_1);
                  it_2.append(this.columnTypeProvider.getColumnTypeReference(column_1.getType()).getType());
                  StringConcatenation _builder_2 = new StringConcatenation();
                  _builder_2.append(".create(\"");
                  String _name = column_1.getName();
                  _builder_2.append(_name);
                  _builder_2.append("\")");
                  it_2.append(_builder_2);
                }
              }
              StringConcatenation _builder_1 = new StringConcatenation();
              _builder_1.append(");");
              it_2.append(_builder_1);
            };
            this._jvmTypesBuilder.setBody(it_1, _function_7);
          };
          JvmConstructor _constructor_1 = this._jvmTypesBuilder.toConstructor(owner, _function_6);
          this._jvmTypesBuilder.<JvmConstructor>operator_add(_members_2, _constructor_1);
        }
        for (final TableColumnDef column_1 : tableColumns) {
          EList<JvmMember> _members_3 = it.getMembers();
          final Procedure1<JvmOperation> _function_7 = (JvmOperation it_1) -> {
            it_1.setVisibility(JvmVisibility.PUBLIC);
            final Procedure1<ITreeAppendable> _function_8 = (ITreeAppendable it_2) -> {
              StringConcatenation _builder = new StringConcatenation();
              _builder.append("return ");
              String _columnName = this.tableTypeNameProvider.getColumnName(column_1.getName());
              _builder.append(_columnName);
              _builder.append(";");
              it_2.append(_builder);
            };
            this._jvmTypesBuilder.setBody(it_1, _function_8);
          };
          JvmOperation _method = this._jvmTypesBuilder.toMethod(owner, this.tableTypeNameProvider.getColumnGetterName(column_1.getName()), this.columnTypeProvider.getColumnTypeReference(column_1.getType()), _function_7);
          this._jvmTypesBuilder.<JvmOperation>operator_add(_members_3, _method);
        }
        EList<JvmMember> _members_4 = it.getMembers();
        final Procedure1<JvmOperation> _function_8 = (JvmOperation it_1) -> {
          it_1.setVisibility(JvmVisibility.PUBLIC);
          final Procedure1<ITreeAppendable> _function_9 = (ITreeAppendable it_2) -> {
            StringConcatenation _builder = new StringConcatenation();
            _builder.append("return new ");
            String _simpleName = clazz.getSimpleName();
            _builder.append(_simpleName);
            _builder.append("(name()");
            it_2.append(_builder);
            for (final TableColumnDef expr : tableColumns) {
              StringConcatenation _builder_1 = new StringConcatenation();
              _builder_1.append(", ");
              String _columnGetterName = this.tableTypeNameProvider.getColumnGetterName(expr.getName());
              _builder_1.append(_columnGetterName);
              _builder_1.append("().emptyCopy()");
              it_2.append(_builder_1);
            }
            StringConcatenation _builder_2 = new StringConcatenation();
            _builder_2.append(");");
            it_2.append(_builder_2);
          };
          this._jvmTypesBuilder.setBody(it_1, _function_9);
        };
        JvmOperation _method_1 = this._jvmTypesBuilder.toMethod(owner, "emptyCopy", this._typeReferenceBuilder.typeRef(clazz), _function_8);
        this._jvmTypesBuilder.<JvmOperation>operator_add(_members_4, _method_1);
        EList<JvmMember> _members_5 = it.getMembers();
        final Procedure1<JvmOperation> _function_9 = (JvmOperation it_1) -> {
          EList<JvmFormalParameter> _parameters = it_1.getParameters();
          JvmFormalParameter _parameter = this._jvmTypesBuilder.toParameter(owner, "rowSize", this._typeReferenceBuilder.typeRef(int.class));
          this._jvmTypesBuilder.<JvmFormalParameter>operator_add(_parameters, _parameter);
          it_1.setVisibility(JvmVisibility.PUBLIC);
          final Procedure1<ITreeAppendable> _function_10 = (ITreeAppendable it_2) -> {
            StringConcatenation _builder = new StringConcatenation();
            _builder.append("return new ");
            String _simpleName = clazz.getSimpleName();
            _builder.append(_simpleName);
            _builder.append("(name()");
            it_2.append(_builder);
            for (final TableColumnDef expr : tableColumns) {
              StringConcatenation _builder_1 = new StringConcatenation();
              _builder_1.append(", ");
              String _columnGetterName = this.tableTypeNameProvider.getColumnGetterName(expr.getName());
              _builder_1.append(_columnGetterName);
              _builder_1.append("().emptyCopy(rowSize)");
              it_2.append(_builder_1);
            }
            StringConcatenation _builder_2 = new StringConcatenation();
            _builder_2.append(");");
            it_2.append(_builder_2);
          };
          this._jvmTypesBuilder.setBody(it_1, _function_10);
        };
        JvmOperation _method_2 = this._jvmTypesBuilder.toMethod(owner, "emptyCopy", this._typeReferenceBuilder.typeRef(clazz), _function_9);
        this._jvmTypesBuilder.<JvmOperation>operator_add(_members_5, _method_2);
        EList<JvmMember> _members_6 = it.getMembers();
        this._jvmTypesBuilder.<JvmGenericType>operator_add(_members_6, rowInterface);
        EList<JvmMember> _members_7 = it.getMembers();
        this._jvmTypesBuilder.<JvmGenericType>operator_add(_members_7, rowClass);
        EList<JvmMember> _members_8 = it.getMembers();
        final Procedure1<JvmOperation> _function_10 = (JvmOperation it_1) -> {
          it_1.setVisibility(JvmVisibility.PUBLIC);
          final Procedure1<ITreeAppendable> _function_11 = (ITreeAppendable it_2) -> {
            StringConcatenation _builder = new StringConcatenation();
            _builder.append("return new Row(this);");
            it_2.append(_builder);
          };
          this._jvmTypesBuilder.setBody(it_1, _function_11);
        };
        JvmOperation _method_3 = this._jvmTypesBuilder.toMethod(owner, "row", this._typeReferenceBuilder.typeRef(rowClass), _function_10);
        this._jvmTypesBuilder.<JvmOperation>operator_add(_members_8, _method_3);
        EList<JvmMember> _members_9 = it.getMembers();
        final Procedure1<JvmOperation> _function_11 = (JvmOperation it_1) -> {
          EList<JvmFormalParameter> _parameters = it_1.getParameters();
          JvmFormalParameter _parameter = this._jvmTypesBuilder.toParameter(owner, "row", this._typeReferenceBuilder.typeRef(rowInterface));
          this._jvmTypesBuilder.<JvmFormalParameter>operator_add(_parameters, _parameter);
          it_1.setVisibility(JvmVisibility.PUBLIC);
          final Procedure1<ITreeAppendable> _function_12 = (ITreeAppendable it_2) -> {
            for (final TableColumnDef expr : tableColumns) {
              {
                StringConcatenation _builder = new StringConcatenation();
                String _columnGetterName = this.tableTypeNameProvider.getColumnGetterName(expr.getName());
                _builder.append(_columnGetterName);
                _builder.append("().append(row.");
                String _columnValueGetterName = this.tableTypeNameProvider.getColumnValueGetterName(expr.getName());
                _builder.append(_columnValueGetterName);
                _builder.append("());");
                it_2.append(_builder);
                it_2.newLine();
              }
            }
            StringConcatenation _builder = new StringConcatenation();
            _builder.append("return this;");
            it_2.append(_builder);
          };
          this._jvmTypesBuilder.setBody(it_1, _function_12);
        };
        JvmOperation _method_4 = this._jvmTypesBuilder.toMethod(owner, "append", this._typeReferenceBuilder.typeRef(clazz), _function_11);
        this._jvmTypesBuilder.<JvmOperation>operator_add(_members_9, _method_4);
      };
      acceptor.<JvmGenericType>accept(clazz, _function_2);
      _xblockexpression = clazz;
    }
    return _xblockexpression;
  }
  
  public JvmGenericType initRowClass(final EObject owner, final Iterable<TableColumnDef> tableColumns, final JvmGenericType clazz, final JvmGenericType rowClass) {
    final Procedure1<JvmGenericType> _function = (JvmGenericType it) -> {
      it.setVisibility(JvmVisibility.PUBLIC);
      it.setStatic(true);
      for (final TableColumnDef column : tableColumns) {
        EList<JvmMember> _members = it.getMembers();
        final Procedure1<JvmOperation> _function_1 = (JvmOperation it_1) -> {
          it_1.setVisibility(JvmVisibility.PUBLIC);
          boolean _isInterface = rowClass.isInterface();
          if (_isInterface) {
            it_1.setAbstract(true);
          } else {
            final Procedure1<ITreeAppendable> _function_2 = (ITreeAppendable it_2) -> {
              StringConcatenation _builder = new StringConcatenation();
              _builder.append("return table.");
              String _columnGetterName = this.tableTypeNameProvider.getColumnGetterName(column.getName());
              _builder.append(_columnGetterName);
              _builder.append("().get(getRowNumber());");
              it_2.append(_builder);
            };
            this._jvmTypesBuilder.setBody(it_1, _function_2);
          }
        };
        JvmOperation _method = this._jvmTypesBuilder.toMethod(owner, this.tableTypeNameProvider.getColumnValueGetterName(column.getName()), this._jvmTypesBuilder.cloneWithProxies(column.getType()), _function_1);
        this._jvmTypesBuilder.<JvmOperation>operator_add(_members, _method);
      }
      boolean _isInterface = rowClass.isInterface();
      boolean _not = (!_isInterface);
      if (_not) {
        EList<JvmTypeReference> _superTypes = it.getSuperTypes();
        JvmTypeReference _typeRef = this._typeReferenceBuilder.typeRef(Row.class);
        this._jvmTypesBuilder.<JvmTypeReference>operator_add(_superTypes, _typeRef);
        EList<JvmMember> _members_1 = it.getMembers();
        final Procedure1<JvmField> _function_2 = (JvmField it_1) -> {
          it_1.setVisibility(JvmVisibility.PRIVATE);
          it_1.setFinal(true);
        };
        JvmField _field = this._jvmTypesBuilder.toField(owner, "table", this._typeReferenceBuilder.typeRef(clazz), _function_2);
        this._jvmTypesBuilder.<JvmField>operator_add(_members_1, _field);
        EList<JvmMember> _members_2 = it.getMembers();
        final Procedure1<JvmConstructor> _function_3 = (JvmConstructor it_1) -> {
          EList<JvmFormalParameter> _parameters = it_1.getParameters();
          JvmFormalParameter _parameter = this._jvmTypesBuilder.toParameter(owner, "table", this._typeReferenceBuilder.typeRef(clazz));
          this._jvmTypesBuilder.<JvmFormalParameter>operator_add(_parameters, _parameter);
          final Procedure1<ITreeAppendable> _function_4 = (ITreeAppendable it_2) -> {
            StringConcatenation _builder = new StringConcatenation();
            _builder.append("super(table);");
            it_2.append(_builder);
            it_2.newLine();
            it_2.append("this.table = table;");
          };
          this._jvmTypesBuilder.setBody(it_1, _function_4);
        };
        JvmConstructor _constructor = this._jvmTypesBuilder.toConstructor(owner, _function_3);
        this._jvmTypesBuilder.<JvmConstructor>operator_add(_members_2, _constructor);
        EList<JvmMember> _members_3 = it.getMembers();
        final Procedure1<JvmOperation> _function_4 = (JvmOperation it_1) -> {
          EList<JvmAnnotationReference> _annotations = it_1.getAnnotations();
          JvmAnnotationReference _overrideAnnotation = this.overrideAnnotation(clazz);
          this._jvmTypesBuilder.<JvmAnnotationReference>operator_add(_annotations, _overrideAnnotation);
          final Procedure1<ITreeAppendable> _function_5 = (ITreeAppendable it_2) -> {
            StringConcatenation _builder = new StringConcatenation();
            _builder.append("super.next();");
            it_2.append(_builder);
            it_2.newLine();
            StringConcatenation _builder_1 = new StringConcatenation();
            _builder_1.append("return this;");
            it_2.append(_builder_1);
          };
          this._jvmTypesBuilder.setBody(it_1, _function_5);
        };
        JvmOperation _method_1 = this._jvmTypesBuilder.toMethod(owner, "next", this._typeReferenceBuilder.typeRef(it), _function_4);
        this._jvmTypesBuilder.<JvmOperation>operator_add(_members_3, _method_1);
        for (final TableColumnDef column_1 : tableColumns) {
          EList<JvmMember> _members_4 = it.getMembers();
          final Procedure1<JvmOperation> _function_5 = (JvmOperation it_1) -> {
            it_1.setVisibility(JvmVisibility.PUBLIC);
            EList<JvmFormalParameter> _parameters = it_1.getParameters();
            JvmFormalParameter _parameter = this._jvmTypesBuilder.toParameter(owner, column_1.getName(), this._jvmTypesBuilder.cloneWithProxies(column_1.getType()));
            this._jvmTypesBuilder.<JvmFormalParameter>operator_add(_parameters, _parameter);
            final Procedure1<ITreeAppendable> _function_6 = (ITreeAppendable it_2) -> {
              StringConcatenation _builder = new StringConcatenation();
              _builder.append("table.");
              String _columnGetterName = this.tableTypeNameProvider.getColumnGetterName(column_1.getName());
              _builder.append(_columnGetterName);
              _builder.append("().set(getRowNumber(), ");
              String _name = column_1.getName();
              _builder.append(_name);
              _builder.append(");");
              it_2.append(_builder);
              it_2.newLine();
              StringConcatenation _builder_1 = new StringConcatenation();
              _builder_1.append("return this;");
              it_2.append(_builder_1);
            };
            this._jvmTypesBuilder.setBody(it_1, _function_6);
          };
          JvmOperation _method_2 = this._jvmTypesBuilder.toMethod(owner, this.tableTypeNameProvider.getColumnValueSetterName(column_1.getName()), this._typeReferenceBuilder.typeRef(rowClass), _function_5);
          this._jvmTypesBuilder.<JvmOperation>operator_add(_members_4, _method_2);
        }
      }
    };
    return ObjectExtensions.<JvmGenericType>operator_doubleArrow(rowClass, _function);
  }
  
  @Inject
  private JvmAnnotationReferenceBuilder.Factory annotationReferenceBuilderFactory;
  
  public JvmAnnotationReference overrideAnnotation(final JvmGenericType context) {
    return this.annotationReferenceBuilderFactory.create(context.eResource().getResourceSet()).annotationRef(Override.class);
  }
  
  public JvmAnnotationReference inferTableClassAnnotations(final Iterable<TableColumnDef> tableColumns, final JvmGenericType clazz) {
    JvmAnnotationReference _xblockexpression = null;
    {
      final JvmAnnotationReferenceBuilder builder = this.annotationReferenceBuilderFactory.create(clazz.eResource().getResourceSet());
      final JvmAnnotationReference tableAnnotation = builder.annotationRef(etablesaw.xtext.lib.TableDef.class);
      EList<JvmAnnotationValue> _explicitValues = tableAnnotation.getExplicitValues();
      JvmStringAnnotationValue _createJvmStringAnnotationValue = this.typesFactory.createJvmStringAnnotationValue();
      final Procedure1<JvmStringAnnotationValue> _function = (JvmStringAnnotationValue it) -> {
        final Function1<JvmOperation, Boolean> _function_1 = (JvmOperation it_1) -> {
          String _simpleName = it_1.getSimpleName();
          return Boolean.valueOf(Objects.equal(_simpleName, "columnNames"));
        };
        it.setOperation(IterableExtensions.<JvmOperation>findFirst(tableAnnotation.getAnnotation().getDeclaredOperations(), _function_1));
        EList<String> _values = it.getValues();
        final Function1<TableColumnDef, String> _function_2 = (TableColumnDef it_1) -> {
          return it_1.getName();
        };
        Iterable<String> _map = IterableExtensions.<TableColumnDef, String>map(tableColumns, _function_2);
        this._jvmTypesBuilder.<String>operator_add(_values, _map);
      };
      JvmStringAnnotationValue _doubleArrow = ObjectExtensions.<JvmStringAnnotationValue>operator_doubleArrow(_createJvmStringAnnotationValue, _function);
      this._jvmTypesBuilder.<JvmAnnotationValue>operator_add(_explicitValues, _doubleArrow);
      EList<JvmAnnotationValue> _explicitValues_1 = tableAnnotation.getExplicitValues();
      JvmTypeAnnotationValue _createJvmTypeAnnotationValue = this.typesFactory.createJvmTypeAnnotationValue();
      final Procedure1<JvmTypeAnnotationValue> _function_1 = (JvmTypeAnnotationValue it) -> {
        final Function1<JvmOperation, Boolean> _function_2 = (JvmOperation it_1) -> {
          String _simpleName = it_1.getSimpleName();
          return Boolean.valueOf(Objects.equal(_simpleName, "columnTypes"));
        };
        it.setOperation(IterableExtensions.<JvmOperation>findFirst(tableAnnotation.getAnnotation().getDeclaredOperations(), _function_2));
        EList<JvmTypeReference> _values = it.getValues();
        final Function1<TableColumnDef, JvmTypeReference> _function_3 = (TableColumnDef it_1) -> {
          JvmTypeReference _type = it_1.getType();
          JvmTypeReference _cloneWithProxies = null;
          if (_type!=null) {
            _cloneWithProxies=this._jvmTypesBuilder.cloneWithProxies(_type);
          }
          return _cloneWithProxies;
        };
        Iterable<JvmTypeReference> _map = IterableExtensions.<TableColumnDef, JvmTypeReference>map(tableColumns, _function_3);
        this._jvmTypesBuilder.<JvmTypeReference>operator_add(_values, _map);
      };
      JvmTypeAnnotationValue _doubleArrow_1 = ObjectExtensions.<JvmTypeAnnotationValue>operator_doubleArrow(_createJvmTypeAnnotationValue, _function_1);
      this._jvmTypesBuilder.<JvmAnnotationValue>operator_add(_explicitValues_1, _doubleArrow_1);
      _xblockexpression = tableAnnotation;
    }
    return _xblockexpression;
  }
  
  public void infer(final EObject xaw, final IJvmDeclaredTypeAcceptor acceptor, final boolean isPreIndexingPhase) {
    if (xaw instanceof Xaw) {
      _infer((Xaw)xaw, acceptor, isPreIndexingPhase);
      return;
    } else if (xaw != null) {
      _infer(xaw, acceptor, isPreIndexingPhase);
      return;
    } else {
      throw new IllegalArgumentException("Unhandled parameter types: " +
        Arrays.<Object>asList(xaw, acceptor, isPreIndexingPhase).toString());
    }
  }
}
