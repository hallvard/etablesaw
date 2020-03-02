/*
 * generated by Xtext 2.12.0
 */
package etablesaw.xtext.jvmmodel

import com.google.inject.Inject
import etablesaw.xtext.lib.TableDef
import etablesaw.xtext.lib.TypedTable
import etablesaw.xtext.lib.XawBase
import etablesaw.xtext.xaw.TableColumn
import etablesaw.xtext.xaw.TableColumnDef
import etablesaw.xtext.xaw.TableLiteral
import etablesaw.xtext.xaw.XMethod
import etablesaw.xtext.xaw.Xaw
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.common.types.JvmGenericType
import org.eclipse.xtext.common.types.JvmVisibility
import org.eclipse.xtext.common.types.TypesFactory
import org.eclipse.xtext.common.types.util.TypeReferences
import org.eclipse.xtext.xbase.jvmmodel.AbstractModelInferrer
import org.eclipse.xtext.xbase.jvmmodel.IJvmDeclaredTypeAcceptor
import org.eclipse.xtext.xbase.jvmmodel.JvmAnnotationReferenceBuilder
import org.eclipse.xtext.xbase.jvmmodel.JvmTypesBuilder
import tech.tablesaw.api.Row

/**
 * <p>Infers a JVM model from the source model.</p> 
 *
 * <p>The JVM model should contain all elements that would appear in the Java code 
 * which is generated from the source model. Other models link against the JVM model rather than the source model.</p>     
 */
class XawJvmModelInferrer extends AbstractModelInferrer {

	/**
	 * convenience API to build and initialize JVM types and their members.
	 */
	@Inject extension JvmTypesBuilder
	@Inject extension TypeReferences

    @Inject extension XawUtil

	def dispatch void infer(Xaw xaw, IJvmDeclaredTypeAcceptor acceptor, boolean isPreIndexingPhase) {
		val clazz = xaw.toClass(xaw.QName) [
			superTypes += typeRef(XawBase)
//			superTypes += typeRef(Runnable)
		]
		acceptor.accept(clazz) [
			members += xaw.toMethod("run", typeRef(Void.TYPE), [
				visibility = JvmVisibility.PUBLIC
				val method = it
				body = xaw
			])
			members += xaw.toMethod("main", typeRef(Void.TYPE)) [
				visibility = JvmVisibility.PUBLIC
				static = true
				parameters += xaw.toParameter("args", createArrayType(typeRef(String)))
				body = [
					append('''new «clazz.simpleName»().run();''')
				]
			]
			for (method : xaw.methods) {
    			inferMethod(clazz, method)
			}
		]
        for (tableDef : xaw.tableDefs) {
            val tableClass = inferTableClass(xaw, tableDef.getTableTypeName, tableDef.tableColumDefs, acceptor)
            optionallyAddAsMember(clazz, tableClass)
        }
		xaw.eAllContents.filter(TableLiteral).forEach[
		    if (! isColumnTable(it)) {
    		    val columnDefs = expressions.filter(TableColumn).map[columnDef]
    		    if (columnDefs.iterator.hasNext) {
                    val tableClass = inferTableClass(xaw, tableTypeName, columnDefs, acceptor)
                    optionallyAddAsMember(clazz, tableClass)
    		    }
		    }
		]
	}

    def optionallyAddAsMember(JvmGenericType clazz, JvmGenericType tableClass) {
        if (tableClass.qualifiedName.indexOf(".") < 0) {
            tableClass.static = true
            clazz.members += tableClass
        }
    }

	def inferMethod(JvmGenericType jvmClass, XMethod xMethod) {
		jvmClass.members += xMethod.toMethod(xMethod.name, xMethod.returnType) [
		    for (typeParameter : xMethod.typeParameters) {
    		    typeParameters += typeParameter.cloneWithProxies
		    }
			visibility = JvmVisibility.PRIVATE
			var paramNum = 1
			for (parameter : xMethod.parameters) {
				val formalParameter = parameter.toParameter(parameter.name ?: ("arg" + paramNum), parameter.parameterType)
				parameters += formalParameter
				paramNum++
			}
			body = xMethod.body
		]
	}

    @Inject extension TypesFactory typesFactory

    @Inject extension ITableTypeNameProvider tableTypeNameProvider;
    @Inject extension IColumnTypeProvider columnTypeProvider

	def inferTableClass(EObject owner, String typeName, Iterable<TableColumnDef> columns, IJvmDeclaredTypeAcceptor acceptor) {
        val clazz = owner.toClass(typeName)
        val rowInterface = owner.toInterface("RowData") [
            interface = true
        ]
        val rowClass = owner.toClass("Row") [
            superTypes += typeRef(rowInterface)
        ]
        clazz.superTypes += typeRef(TypedTable, typeRef(rowClass))
        acceptor.accept(clazz) [
            initRowClass(owner, columns, clazz, rowInterface)
            initRowClass(owner, columns, clazz, rowClass)
            val annotation = inferTableClassAnnotations(columns, clazz)
            clazz.annotations += annotation
            val tableColumns = columns.filter[type !== null && type.identifier != "void"]
            for (column : tableColumns) {
                members += owner.toField(column.name.columnName, getColumnTypeReference(column.type)) [
                    visibility = JvmVisibility.PRIVATE
                    final = true
                ]
            }
            members += owner.toConstructor[
                parameters += owner.toParameter("tableName", typeRef(String))
                for (expr : tableColumns) {
                    parameters += owner.toParameter(expr.name + "Column", getColumnTypeReference(expr.type))
                }
                body = [
                    append('''super(tableName, «tableColumns.join(", ", [ name.columnName ])»);''')
                    newLine
                    for (expr : tableColumns) {
                        append('''this.«expr.name»Column = «expr.name.columnName»;''')
                        newLine
                    }
                ]
            ]
            if (! tableColumns.isEmpty) {
                members += owner.toConstructor[
                    parameters += owner.toParameter("tableName", typeRef(String))
                    body = [
                        append('''this(tableName''')
                        for (column : tableColumns) {
                            append(''', ''')
                            append(column.type.columnTypeReference.type)
                            append('''.create("«column.name»")''')
                        }
                        append(''');''')
                    ]
                ]
            }
            for (column : tableColumns) {
                members += owner.toMethod(column.name.columnGetterName, column.type.columnTypeReference) [
                    visibility = JvmVisibility.PUBLIC
                    body = [
                        append('''return «column.name.columnName»;''')
                    ]
                ]
            }
            members += owner.toMethod("emptyCopy", typeRef(clazz)) [
                visibility = JvmVisibility.PUBLIC
                body = [
                    append('''return new «clazz.simpleName»(name()''')
                    for (expr : tableColumns) {
                        append(''', «expr.name.columnGetterName»().emptyCopy()''')
                    }
                    append(''');''')
                ]
            ]
            members += owner.toMethod("emptyCopy", typeRef(clazz)) [
                parameters += owner.toParameter("rowSize", typeRef(int))
                visibility = JvmVisibility.PUBLIC
                body = [
                    append('''return new «clazz.simpleName»(name()''')
                    for (expr : tableColumns) {
                        append(''', «expr.name.columnGetterName»().emptyCopy(rowSize)''')
                    }
                    append(''');''')
                ]
            ]
            members += rowInterface
            members += rowClass
            members += owner.toMethod("row", typeRef(rowClass)) [
                visibility = JvmVisibility.PUBLIC
                body = [
                    append('''return new Row(this);''')
                ]
            ]
            members += owner.toMethod("append", typeRef(clazz)) [
                parameters += owner.toParameter("row", typeRef(rowInterface))
                visibility = JvmVisibility.PUBLIC
                body = [
                    for (expr : tableColumns) {
                        append('''«expr.name.columnGetterName»().append(row.«expr.name.columnValueGetterName»());''')
                        newLine
                    }
                    append('''return this;''')
                ]
            ]
        ]
        clazz
	}

    def initRowClass(EObject owner, Iterable<TableColumnDef> tableColumns, JvmGenericType clazz, JvmGenericType rowClass) {
        rowClass => [
            visibility = JvmVisibility.PUBLIC
            static = true
            for (column : tableColumns) {
                members += owner.toMethod(column.name.columnValueGetterName, column.type.cloneWithProxies) [
                    visibility = JvmVisibility.PUBLIC
                    if (rowClass.isInterface) {
                        abstract = true
                    } else { 
                        body = [
                            append('''return table.«column.name.columnGetterName»().get(getRowNumber());''')
                        ]
                    }
                ]
            }
            if (! rowClass.isInterface) {
                superTypes += typeRef(Row)
                members += owner.toField("table", typeRef(clazz)) [
                    visibility = JvmVisibility.PRIVATE
                    final = true
                ]
                members += owner.toConstructor[
                    parameters += owner.toParameter("table", typeRef(clazz))
                    body = [
                        append('''super(table);''')
                        newLine
                        append("this.table = table;")
                    ]
                ]
                members += owner.toMethod("next", typeRef(it)) [
                    annotations += overrideAnnotation(clazz)
                    body = [
                        append('''super.next();''')
                        newLine
                        append('''return this;''')
                    ]
                ]
                for (column : tableColumns) {
                    members += owner.toMethod(column.name.columnValueSetterName, typeRef(rowClass)) [
                        visibility = JvmVisibility.PUBLIC
                        parameters += owner.toParameter(column.name, column.type.cloneWithProxies)
                        body = [
                            append('''table.«column.name.columnGetterName»().set(getRowNumber(), «column.name»);''')
                            newLine
                            append('''return this;''')
                        ]
                    ]
                }
            }
        ]
    }

    @Inject JvmAnnotationReferenceBuilder.Factory annotationReferenceBuilderFactory
    
    def overrideAnnotation(JvmGenericType context) {
        annotationReferenceBuilderFactory.create(context.eResource().getResourceSet()).annotationRef(Override)
    }
    
    def inferTableClassAnnotations(Iterable<TableColumnDef> tableColumns, JvmGenericType clazz) {
        val JvmAnnotationReferenceBuilder builder = annotationReferenceBuilderFactory.create(clazz.eResource().getResourceSet())
        val tableAnnotation = builder.annotationRef(TableDef)
        tableAnnotation.explicitValues += createJvmStringAnnotationValue => [
            operation = tableAnnotation.annotation.declaredOperations.findFirst[simpleName == "columnNames"]
            values += tableColumns.map[name]
        ]
        tableAnnotation.explicitValues +=  createJvmTypeAnnotationValue => [
            operation = tableAnnotation.annotation.declaredOperations.findFirst[simpleName == "columnTypes"]
            values += tableColumns.map[it | type?.cloneWithProxies]
        ]
        tableAnnotation
    }
}