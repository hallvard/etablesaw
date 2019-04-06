package etablesaw.xtext.jvmmodel;

import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.core.ElementChangedEvent;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IElementChangedListener;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaElementDelta;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.launching.JavaRuntime;

// adapted from https://gitlab.stud.idi.ntnu.no/tobiaas/scene-builder-plugin/blob/master/sb4e/src/no/tobask/sb4e/EclipseProjectsClassLoader.java

public class JavaProjectClassLoader extends ClassLoader {

    private final static String JAVA_NATURE_ID = "org.eclipse.jdt.core.javanature";

    // use this one when replacing javaProjectClassLoader
    JavaProjectClassLoader(JavaProjectClassLoader javaProjectClassLoader) {
        super(javaProjectClassLoader.getParent());
        this.allLoaders = new ArrayList<>(javaProjectClassLoader.allLoaders);
        this.replaceableLoaders = new ArrayList<>(javaProjectClassLoader.replaceableLoaders);
        this.replaceableClasses = new HashMap<>(javaProjectClassLoader.replaceableClasses);
        javaProjectClassLoader.dispose();
        addClasspathChangeListener();
    }

	public JavaProjectClassLoader(ClassLoader parentClassLoader, IProject... projects) {
	    super(parentClassLoader);
		for (final IProject project : ResourcesPlugin.getWorkspace().getRoot().getProjects()) {
		    try {
                if (project.hasNature(JAVA_NATURE_ID)) {
                    addProjectClassLoaders(JavaCore.create(project));
                }
            } catch (CoreException e) {
            }
		}
		if (allLoaders.size() + replaceableLoaders.size() == 0) {
		    throw new IllegalArgumentException("No Java projects in " + Arrays.asList(projects));
		}
		addClasspathChangeListener();
	}
	
	public JavaProjectClassLoader(ClassLoader parentClassLoader) {
		this(parentClassLoader, ResourcesPlugin.getWorkspace().getRoot().getProjects());
	}
	public JavaProjectClassLoader(ClassLoader parentClassLoader, String projectName) {
	    this(parentClassLoader, ResourcesPlugin.getWorkspace().getRoot().getProject(projectName));
	}

	public JavaProjectClassLoader(ClassLoader parentClassLoader, IProject project) {
		this(parentClassLoader, new IProject[]{project});
	}

	private ClasspathChangeListener classpathChangeListener;
	
	private void addClasspathChangeListener() {
		classpathChangeListener = new ClasspathChangeListener();
		JavaCore.addElementChangedListener(classpathChangeListener, ElementChangedEvent.POST_CHANGE);
	}

	public void dispose() {
		JavaCore.removeElementChangedListener(classpathChangeListener);		
	}
	
    private List<URLClassLoader> allLoaders = new ArrayList<>();
    private List<URLClassLoader> replaceableLoaders = new ArrayList<>();

	private void addProjectClassLoaders(final IJavaProject javaProject) {
	    Collection<URL> others = new ArrayList<>();
	    Collection<URL> replaceable = new ArrayList<>();
		try {
			final URL[] urls = getClassPathAsUrls(javaProject);
			for (int i = 0; i < urls.length; i++) {
			    URL url = urls[i];
                if (isReplaceableUrl(url)) {
                    replaceable.add(url);
			    } else {
			        others.add(url);
			    }
            }
			allLoaders.add(new URLClassLoader(others.toArray(new URL[others.size()]), getParent()));
			for (URL url : replaceable) {
			    URLClassLoader loader = new URLClassLoader(new URL[] {url}, getParent());
                replaceableLoaders.add(loader);
                allLoaders.add(loader);
            }
		} catch (final CoreException e) {
		}
	}

    protected boolean isReplaceableUrl(URL url) {
        return ! url.getPath().endsWith(".jar");
    }

	private URL[] getClassPathAsUrls(final IJavaProject javaProject) throws CoreException {
		final String[] classPath = JavaRuntime.computeDefaultRuntimeClassPath(javaProject);
		final List<URL> urls = new ArrayList<>();
		for (final String entry : classPath) {
			final IPath path = new Path(entry);
			try {
				urls.add(path.toFile().toURI().toURL());
			} catch (final MalformedURLException e) {
			}
		}
		return urls.toArray(new URL[urls.size()]);
	}

	private Map<String, URLClassLoader> replaceableClasses = new HashMap<String, URLClassLoader>();
	
	@Override
	public Class<?> findClass(final String name) throws ClassNotFoundException {
		for (final URLClassLoader loader : allLoaders) {
			try {
				Class<?> loadedClass = loader.loadClass(name);
				if (loadedClass != null && replaceableLoaders.contains(loader)) {
				    replaceableClasses.put(name, loader);
				}
                return loadedClass;
			} catch (final ClassNotFoundException e) {
			}
		}
		throw new ClassNotFoundException(name + " not found");
	}

    protected void updateClassLoader(URLClassLoader classLoader) {
        int pos = replaceableLoaders.indexOf(classLoader);
        if (pos >= 0) {
            // remove class names from map
            for (String className : new ArrayList<String>(replaceableClasses.keySet())) {
                if (replaceableClasses.get(className) == classLoader) {
                    replaceableClasses.remove(className);
                }
            }
            // replace with new class loader
            URLClassLoader replacement = new URLClassLoader(classLoader.getURLs(), classLoader.getParent());
            replaceableLoaders.set(pos, replacement);
            allLoaders.set(allLoaders.indexOf(classLoader), replacement);
//            notifyListeners();
        }
    }

	// https://stackoverflow.com/questions/10173053/how-to-detect-changes-to-eclipse-classpath-container-contents
    
//    public static interface Listener {
//        public void classLoaderReplaced(JavaProjectClassLoader classLoader);
//    }
//    
//    private Collection<Listener> listeners = new ArrayList<>();
//    
//    public void addListener(Listener listener) {
//        listeners.add(listener);
//    }
//    public void removeListener(Listener listener) {
//        listeners.remove(listener);
//    }
//    protected void notifyListeners() {
//        // make a copy, since list may be changed by listener
//        for (Listener listener : new ArrayList<>(listeners)) {
//            listener.classLoaderReplaced(this);
//        }
//    }

    public void notifyClasspathChanged(IJavaProject el) {
	    // TODO
	}
	
	private class ClasspathChangeListener implements IElementChangedListener {

	    @Override
	    public void elementChanged(ElementChangedEvent event) {
	        visit(event.getDelta());
	    }

	    private void visit(IJavaElementDelta delta) {
	        IJavaElement javaElement = delta.getElement();
	        switch (javaElement.getElementType()) {
	        case IJavaElement.JAVA_PROJECT:
	            if (isClasspathChanged(delta.getFlags())) {
	                notifyClasspathChanged((IJavaProject) javaElement);
	            }
	        case IJavaElement.JAVA_MODEL:
	        case IJavaElement.PACKAGE_FRAGMENT_ROOT:
	        case IJavaElement.PACKAGE_FRAGMENT:
	            for (IJavaElementDelta childDelta : delta.getAffectedChildren()) {
	                visit(childDelta);
	            }
	            break;
	        case IJavaElement.COMPILATION_UNIT:
	            try {
                    for (IType type : ((ICompilationUnit) javaElement).getTypes()) {
                        String qName = type.getFullyQualifiedName();
                        URLClassLoader classLoader = replaceableClasses.get(qName);
                        if (classLoader != null) {
                            updateClassLoader(classLoader);
                        }
                    }
                } catch (JavaModelException e) {
                }
	            break;
	        default:
	            break;
	        }
	    }

        private boolean isClasspathChanged(int flags) {
	        return (flags & (IJavaElementDelta.F_CLASSPATH_CHANGED | IJavaElementDelta.F_RESOLVED_CLASSPATH_CHANGED)) != 0;
	    }
	}
}
