/*
 * generated by Xtext 2.12.0
 */
package etablesaw.xtext


/**
 * Initialization support for running Xtext languages without Equinox extension registry.
 */
class XawStandaloneSetup extends XawStandaloneSetupGenerated {

	def static void doSetup() {
		new XawStandaloneSetup().createInjectorAndDoEMFRegistration()
	}
}