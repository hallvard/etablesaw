package etablesaw.xtext.jvmmodel;

import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.core.ElementChangedEvent;
import org.eclipse.jdt.core.IElementChangedListener;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaElementDelta;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.launching.JavaRuntime;

// adapted from https://gitlab.stud.idi.ntnu.no/tobiaas/scene-builder-plugin/blob/master/sb4e/src/no/tobask/sb4e/EclipseProjectsClassLoader.java

public class JavaProjectClassLoader extends ClassLoader {

	private Collection<URLClassLoader> loaders = new ArrayList<>();

	public JavaProjectClassLoader(IProject... projects) {
		for (final IProject project : ResourcesPlugin.getWorkspace().getRoot().getProjects()) {
			loaders.add(getProjectClassLoader(JavaCore.create(project)));
		}
		addClasspathChangeListener();
	}
	
	public JavaProjectClassLoader() {
		this(ResourcesPlugin.getWorkspace().getRoot().getProjects());
	}

	public JavaProjectClassLoader(IProject project) {
		this(new IProject[]{project});
	}

	private ClasspathChangeListener classpathChangeListener;
	
	private void addClasspathChangeListener() {
		classpathChangeListener = new ClasspathChangeListener();
		JavaCore.addElementChangedListener(classpathChangeListener, ElementChangedEvent.POST_CHANGE);
	}

	public void dispose() {
		JavaCore.removeElementChangedListener(classpathChangeListener);		
	}
	
	private URLClassLoader getProjectClassLoader(final IJavaProject javaProject) {
		final ClassLoader parentClassLoader = javaProject.getClass().getClassLoader();
		try {
			final URL[] urls = getClassPathAsUrls(javaProject);
			return new URLClassLoader(urls, parentClassLoader);
		} catch (final CoreException e) {
			e.printStackTrace();
			return null;
		}
	}

	private URL[] getClassPathAsUrls(final IJavaProject javaProject) throws CoreException {
		final String[] classPath = JavaRuntime.computeDefaultRuntimeClassPath(javaProject);
		final List<URL> urls = new ArrayList<>();
		for (final String entry : classPath) {
			final IPath path = new Path(entry);
			try {
				urls.add(path.toFile().toURI().toURL());
			} catch (final MalformedURLException e) {
				e.printStackTrace();
			}
		}
		return urls.toArray(new URL[urls.size()]);
	}

	@Override
	public Class<?> findClass(final String name) throws ClassNotFoundException {
		for (final URLClassLoader loader : loaders) {
			try {
				final Class<?> clazz = loader.loadClass(name);
				return clazz;
			} catch (final ClassNotFoundException e) {
			}
		}
		throw new ClassNotFoundException(name + " not found");
	}
	
	// https://stackoverflow.com/questions/10173053/how-to-detect-changes-to-eclipse-classpath-container-contents
	
	public void notifyClasspathChanged(IJavaProject el) {
		
	}
	
	private class ClasspathChangeListener implements IElementChangedListener {

	    @Override
	    public void elementChanged(ElementChangedEvent event) {
	        visit(event.getDelta());
	    }

	    private void visit(IJavaElementDelta delta) {
	        IJavaElement el = delta.getElement();
	        switch (el.getElementType()) {
	        case IJavaElement.JAVA_MODEL:
	        	for (IJavaElementDelta c : delta.getAffectedChildren()) {
	        		visit(c);
	        	}
	            break;
	        case IJavaElement.JAVA_PROJECT:
	            if (isClasspathChanged(delta.getFlags())) {
	                notifyClasspathChanged((IJavaProject)el);
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
