/**
 * generated by Xtext 2.12.0
 */
package etablesaw.xtext;

import etablesaw.xtext.XawStandaloneSetupGenerated;

/**
 * Initialization support for running Xtext languages without Equinox extension registry.
 */
@SuppressWarnings("all")
public class XawStandaloneSetup extends XawStandaloneSetupGenerated {
  public static void doSetup() {
    new XawStandaloneSetup().createInjectorAndDoEMFRegistration();
  }
}