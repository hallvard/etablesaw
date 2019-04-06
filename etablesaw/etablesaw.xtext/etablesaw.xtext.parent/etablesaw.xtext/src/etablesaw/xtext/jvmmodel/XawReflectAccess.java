package etablesaw.xtext.jvmmodel;

import org.eclipse.xtext.common.types.util.JavaReflectAccess;

import com.google.inject.Inject;

public class XawReflectAccess extends JavaReflectAccess {
    
    private ClassLoader classLoader = getClass().getClassLoader();

    private ClassFinder classFinder;

    @Inject(optional = true)
    public void setClassLoader(ClassLoader classLoader) {
        if (classLoader != this.classLoader) {
            this.classLoader = classLoader;
            classFinder = null;
        }
    }

    @Override
    public ClassFinder getClassFinder() {
        if (classFinder == null) {
            classFinder = new ClassFinder(classLoader);
        }
        return classFinder;
    }
    
    public static class ClassFinder extends org.eclipse.xtext.common.types.access.impl.ClassFinder {

        public ClassFinder(ClassLoader classLoader) {
            super(classLoader);
        }

        @Override
        protected Class<?> forName(String name, ClassLoader classLoader) throws ClassNotFoundException {
            if (! Character.isJavaIdentifierStart(name.charAt(0))) {
                return super.forName(name, classLoader);
            }
            if (! Character.isJavaIdentifierPart(name.charAt(name.length() - 1))) {
                return super.forName(name, classLoader);
            }
            return classLoader.loadClass(name);
        }
    }
}
