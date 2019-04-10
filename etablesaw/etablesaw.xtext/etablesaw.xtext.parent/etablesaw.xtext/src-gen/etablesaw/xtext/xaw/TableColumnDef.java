/**
 * generated by Xtext 2.14.0
 */
package etablesaw.xtext.xaw;

import org.eclipse.emf.ecore.EObject;

import org.eclipse.xtext.common.types.JvmTypeReference;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Table Column Def</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link etablesaw.xtext.xaw.TableColumnDef#getType <em>Type</em>}</li>
 *   <li>{@link etablesaw.xtext.xaw.TableColumnDef#getName <em>Name</em>}</li>
 * </ul>
 *
 * @see etablesaw.xtext.xaw.XawPackage#getTableColumnDef()
 * @model
 * @generated
 */
public interface TableColumnDef extends EObject
{
  /**
   * Returns the value of the '<em><b>Type</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Type</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Type</em>' containment reference.
   * @see #setType(JvmTypeReference)
   * @see etablesaw.xtext.xaw.XawPackage#getTableColumnDef_Type()
   * @model containment="true"
   * @generated
   */
  JvmTypeReference getType();

  /**
   * Sets the value of the '{@link etablesaw.xtext.xaw.TableColumnDef#getType <em>Type</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Type</em>' containment reference.
   * @see #getType()
   * @generated
   */
  void setType(JvmTypeReference value);

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
   * @see etablesaw.xtext.xaw.XawPackage#getTableColumnDef_Name()
   * @model
   * @generated
   */
  String getName();

  /**
   * Sets the value of the '{@link etablesaw.xtext.xaw.TableColumnDef#getName <em>Name</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Name</em>' attribute.
   * @see #getName()
   * @generated
   */
  void setName(String value);

} // TableColumnDef