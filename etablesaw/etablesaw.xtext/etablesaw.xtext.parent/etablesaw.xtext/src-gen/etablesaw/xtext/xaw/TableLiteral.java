/**
 * generated by Xtext 2.14.0
 */
package etablesaw.xtext.xaw;

import org.eclipse.xtext.xbase.XBlockExpression;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Table Literal</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link etablesaw.xtext.xaw.TableLiteral#getName <em>Name</em>}</li>
 * </ul>
 *
 * @see etablesaw.xtext.xaw.XawPackage#getTableLiteral()
 * @model
 * @generated
 */
public interface TableLiteral extends XBlockExpression
{
  /**
   * Returns the value of the '<em><b>Name</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Name</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Name</em>' attribute.
   * @see #setName(String)
   * @see etablesaw.xtext.xaw.XawPackage#getTableLiteral_Name()
   * @model
   * @generated
   */
  String getName();

  /**
   * Sets the value of the '{@link etablesaw.xtext.xaw.TableLiteral#getName <em>Name</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Name</em>' attribute.
   * @see #getName()
   * @generated
   */
  void setName(String value);

} // TableLiteral
