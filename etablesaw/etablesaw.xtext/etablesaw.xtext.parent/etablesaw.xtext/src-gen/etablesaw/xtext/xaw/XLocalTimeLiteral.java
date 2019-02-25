/**
 * generated by Xtext 2.14.0
 */
package etablesaw.xtext.xaw;

import org.eclipse.xtext.xbase.XExpression;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>XLocal Time Literal</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link etablesaw.xtext.xaw.XLocalTimeLiteral#getHour <em>Hour</em>}</li>
 *   <li>{@link etablesaw.xtext.xaw.XLocalTimeLiteral#getMin <em>Min</em>}</li>
 *   <li>{@link etablesaw.xtext.xaw.XLocalTimeLiteral#getSecond <em>Second</em>}</li>
 * </ul>
 *
 * @see etablesaw.xtext.xaw.XawPackage#getXLocalTimeLiteral()
 * @model
 * @generated
 */
public interface XLocalTimeLiteral extends XExpression
{
  /**
   * Returns the value of the '<em><b>Hour</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Hour</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Hour</em>' attribute.
   * @see #setHour(int)
   * @see etablesaw.xtext.xaw.XawPackage#getXLocalTimeLiteral_Hour()
   * @model
   * @generated
   */
  int getHour();

  /**
   * Sets the value of the '{@link etablesaw.xtext.xaw.XLocalTimeLiteral#getHour <em>Hour</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Hour</em>' attribute.
   * @see #getHour()
   * @generated
   */
  void setHour(int value);

  /**
   * Returns the value of the '<em><b>Min</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Min</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Min</em>' attribute.
   * @see #setMin(int)
   * @see etablesaw.xtext.xaw.XawPackage#getXLocalTimeLiteral_Min()
   * @model
   * @generated
   */
  int getMin();

  /**
   * Sets the value of the '{@link etablesaw.xtext.xaw.XLocalTimeLiteral#getMin <em>Min</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Min</em>' attribute.
   * @see #getMin()
   * @generated
   */
  void setMin(int value);

  /**
   * Returns the value of the '<em><b>Second</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Second</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Second</em>' attribute.
   * @see #setSecond(int)
   * @see etablesaw.xtext.xaw.XawPackage#getXLocalTimeLiteral_Second()
   * @model
   * @generated
   */
  int getSecond();

  /**
   * Sets the value of the '{@link etablesaw.xtext.xaw.XLocalTimeLiteral#getSecond <em>Second</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Second</em>' attribute.
   * @see #getSecond()
   * @generated
   */
  void setSecond(int value);

} // XLocalTimeLiteral
