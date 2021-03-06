/**
 * generated by Xtext 2.14.0
 */
package etablesaw.xtext.xaw.impl;

import etablesaw.xtext.xaw.TableDef;
import etablesaw.xtext.xaw.XMethod;
import etablesaw.xtext.xaw.Xaw;
import etablesaw.xtext.xaw.XawPackage;

import java.util.Collection;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.InternalEList;

import org.eclipse.xtext.xbase.impl.XBlockExpressionImpl;

import org.eclipse.xtext.xtype.XImportSection;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Xaw</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link etablesaw.xtext.xaw.impl.XawImpl#getImportSection <em>Import Section</em>}</li>
 *   <li>{@link etablesaw.xtext.xaw.impl.XawImpl#getQName <em>QName</em>}</li>
 *   <li>{@link etablesaw.xtext.xaw.impl.XawImpl#getTableDefs <em>Table Defs</em>}</li>
 *   <li>{@link etablesaw.xtext.xaw.impl.XawImpl#getMethods <em>Methods</em>}</li>
 * </ul>
 *
 * @generated
 */
public class XawImpl extends XBlockExpressionImpl implements Xaw
{
  /**
   * The cached value of the '{@link #getImportSection() <em>Import Section</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getImportSection()
   * @generated
   * @ordered
   */
  protected XImportSection importSection;

  /**
   * The default value of the '{@link #getQName() <em>QName</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getQName()
   * @generated
   * @ordered
   */
  protected static final String QNAME_EDEFAULT = null;

  /**
   * The cached value of the '{@link #getQName() <em>QName</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getQName()
   * @generated
   * @ordered
   */
  protected String qName = QNAME_EDEFAULT;

  /**
   * The cached value of the '{@link #getTableDefs() <em>Table Defs</em>}' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getTableDefs()
   * @generated
   * @ordered
   */
  protected EList<TableDef> tableDefs;

  /**
   * The cached value of the '{@link #getMethods() <em>Methods</em>}' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getMethods()
   * @generated
   * @ordered
   */
  protected EList<XMethod> methods;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected XawImpl()
  {
    super();
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  protected EClass eStaticClass()
  {
    return XawPackage.Literals.XAW;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public XImportSection getImportSection()
  {
    return importSection;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetImportSection(XImportSection newImportSection, NotificationChain msgs)
  {
    XImportSection oldImportSection = importSection;
    importSection = newImportSection;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, XawPackage.XAW__IMPORT_SECTION, oldImportSection, newImportSection);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setImportSection(XImportSection newImportSection)
  {
    if (newImportSection != importSection)
    {
      NotificationChain msgs = null;
      if (importSection != null)
        msgs = ((InternalEObject)importSection).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - XawPackage.XAW__IMPORT_SECTION, null, msgs);
      if (newImportSection != null)
        msgs = ((InternalEObject)newImportSection).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - XawPackage.XAW__IMPORT_SECTION, null, msgs);
      msgs = basicSetImportSection(newImportSection, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, XawPackage.XAW__IMPORT_SECTION, newImportSection, newImportSection));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public String getQName()
  {
    return qName;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setQName(String newQName)
  {
    String oldQName = qName;
    qName = newQName;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, XawPackage.XAW__QNAME, oldQName, qName));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<TableDef> getTableDefs()
  {
    if (tableDefs == null)
    {
      tableDefs = new EObjectContainmentEList<TableDef>(TableDef.class, this, XawPackage.XAW__TABLE_DEFS);
    }
    return tableDefs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<XMethod> getMethods()
  {
    if (methods == null)
    {
      methods = new EObjectContainmentEList<XMethod>(XMethod.class, this, XawPackage.XAW__METHODS);
    }
    return methods;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs)
  {
    switch (featureID)
    {
      case XawPackage.XAW__IMPORT_SECTION:
        return basicSetImportSection(null, msgs);
      case XawPackage.XAW__TABLE_DEFS:
        return ((InternalEList<?>)getTableDefs()).basicRemove(otherEnd, msgs);
      case XawPackage.XAW__METHODS:
        return ((InternalEList<?>)getMethods()).basicRemove(otherEnd, msgs);
    }
    return super.eInverseRemove(otherEnd, featureID, msgs);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public Object eGet(int featureID, boolean resolve, boolean coreType)
  {
    switch (featureID)
    {
      case XawPackage.XAW__IMPORT_SECTION:
        return getImportSection();
      case XawPackage.XAW__QNAME:
        return getQName();
      case XawPackage.XAW__TABLE_DEFS:
        return getTableDefs();
      case XawPackage.XAW__METHODS:
        return getMethods();
    }
    return super.eGet(featureID, resolve, coreType);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @SuppressWarnings("unchecked")
  @Override
  public void eSet(int featureID, Object newValue)
  {
    switch (featureID)
    {
      case XawPackage.XAW__IMPORT_SECTION:
        setImportSection((XImportSection)newValue);
        return;
      case XawPackage.XAW__QNAME:
        setQName((String)newValue);
        return;
      case XawPackage.XAW__TABLE_DEFS:
        getTableDefs().clear();
        getTableDefs().addAll((Collection<? extends TableDef>)newValue);
        return;
      case XawPackage.XAW__METHODS:
        getMethods().clear();
        getMethods().addAll((Collection<? extends XMethod>)newValue);
        return;
    }
    super.eSet(featureID, newValue);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public void eUnset(int featureID)
  {
    switch (featureID)
    {
      case XawPackage.XAW__IMPORT_SECTION:
        setImportSection((XImportSection)null);
        return;
      case XawPackage.XAW__QNAME:
        setQName(QNAME_EDEFAULT);
        return;
      case XawPackage.XAW__TABLE_DEFS:
        getTableDefs().clear();
        return;
      case XawPackage.XAW__METHODS:
        getMethods().clear();
        return;
    }
    super.eUnset(featureID);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public boolean eIsSet(int featureID)
  {
    switch (featureID)
    {
      case XawPackage.XAW__IMPORT_SECTION:
        return importSection != null;
      case XawPackage.XAW__QNAME:
        return QNAME_EDEFAULT == null ? qName != null : !QNAME_EDEFAULT.equals(qName);
      case XawPackage.XAW__TABLE_DEFS:
        return tableDefs != null && !tableDefs.isEmpty();
      case XawPackage.XAW__METHODS:
        return methods != null && !methods.isEmpty();
    }
    return super.eIsSet(featureID);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public String toString()
  {
    if (eIsProxy()) return super.toString();

    StringBuilder result = new StringBuilder(super.toString());
    result.append(" (qName: ");
    result.append(qName);
    result.append(')');
    return result.toString();
  }

} //XawImpl
