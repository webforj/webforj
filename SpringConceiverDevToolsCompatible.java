package com.webforj.spring;

import com.webforj.App;
import com.webforj.component.Component;
import com.webforj.component.event.ComponentEvent;
import com.webforj.conceiver.Conceiver;
import com.webforj.conceiver.ConceiverProvider;
import com.webforj.conceiver.DefaultConceiver;
import com.webforj.conceiver.exception.ConceiverException;
import com.webforj.environment.ObjectTable;
import com.webforj.exceptions.WebforjAppInitializeException;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.NoSuchBeanDefinitionException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.stereotype.Component as SpringComponent;

import java.lang.ref.WeakReference;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Spring Conceiver implementation that is compatible with Spring Boot DevTools.
 * 
 * This implementation handles the classloader boundary issues that occur when
 * Spring Boot DevTools is enabled by:
 * 
 * 1. Using weak references to avoid holding onto classes from reloaded classloaders
 * 2. Storing the ApplicationContext reference in a way that's accessible across classloaders
 * 3. Falling back gracefully when beans aren't available
 * 4. Clearing stale conceiver instances on context refresh
 */
@SpringComponent
public class SpringConceiverDevToolsCompatible implements Conceiver, ApplicationContextAware {
    
    private static final String CONTEXT_KEY = "com.webforj.spring.ApplicationContext";
    private static final String CONCEIVER_CLASSLOADER_KEY = "com.webforj.spring.Conceiver.classLoader";
    
    // Use weak references to avoid classloader leaks
    private static final Map<ClassLoader, WeakReference<ApplicationContext>> contextMap = 
        new ConcurrentHashMap<>();
    
    private final DefaultConceiver fallbackConceiver = new DefaultConceiver();
    private ApplicationContext applicationContext;
    
    @Override
    public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
        this.applicationContext = applicationContext;
        
        // Store context reference accessible across classloaders
        ClassLoader classLoader = applicationContext.getClassLoader();
        contextMap.put(classLoader, new WeakReference<>(applicationContext));
        
        // Also store in ObjectTable for cross-classloader access
        ObjectTable.put(CONTEXT_KEY, applicationContext);
        
        // Clear any existing conceiver to force reload
        clearExistingConceiver();
        
        // Register this conceiver
        registerThisConceiver();
    }
    
    @Override
    public <T> T get(Class<T> classOfT) {
        ApplicationContext context = findApplicationContext();
        
        if (context != null) {
            try {
                // First try to get as a Spring bean
                return context.getBean(classOfT);
            } catch (NoSuchBeanDefinitionException e) {
                // Not a Spring bean, try to create with Spring's autowiring
                try {
                    return context.getAutowireCapableBeanFactory()
                        .createBean(classOfT);
                } catch (Exception ex) {
                    // Fall back to default conceiver
                    return fallbackConceiver.get(classOfT);
                }
            }
        }
        
        // No Spring context available, use default conceiver
        return fallbackConceiver.get(classOfT);
    }
    
    @Override
    public App getApplication(Class<? extends App> appClass) throws WebforjAppInitializeException {
        ApplicationContext context = findApplicationContext();
        
        if (context != null) {
            try {
                // First try as Spring bean
                return context.getBean(appClass);
            } catch (NoSuchBeanDefinitionException e) {
                // Create with Spring autowiring
                try {
                    return context.getAutowireCapableBeanFactory()
                        .createBean(appClass);
                } catch (Exception ex) {
                    throw new WebforjAppInitializeException(
                        "Failed to create application instance: " + appClass.getName(), ex);
                }
            }
        }
        
        // Fallback to default
        return fallbackConceiver.getApplication(appClass);
    }
    
    @Override
    public <T extends Component> T getComponent(Class<T> classOfT) {
        ApplicationContext context = findApplicationContext();
        
        if (context != null) {
            try {
                return context.getBean(classOfT);
            } catch (NoSuchBeanDefinitionException e) {
                try {
                    return context.getAutowireCapableBeanFactory()
                        .createBean(classOfT);
                } catch (Exception ex) {
                    return fallbackConceiver.getComponent(classOfT);
                }
            }
        }
        
        return fallbackConceiver.getComponent(classOfT);
    }
    
    @Override
    public <T extends ComponentEvent<?>> T getComponentEvent(Component component, 
            Class<?> eventClass, Map<String, Object> data) {
        // Component events typically don't need Spring injection
        return fallbackConceiver.getComponentEvent(component, eventClass, data);
    }
    
    /**
     * Find the ApplicationContext, handling classloader boundaries.
     */
    private ApplicationContext findApplicationContext() {
        // First try the instance variable
        if (applicationContext != null) {
            return applicationContext;
        }
        
        // Try to get from ObjectTable (cross-classloader)
        try {
            Object stored = ObjectTable.get(CONTEXT_KEY);
            if (stored instanceof ApplicationContext) {
                return (ApplicationContext) stored;
            }
        } catch (Exception e) {
            // ObjectTable might not have the context
        }
        
        // Try to find from current classloader
        ClassLoader currentLoader = Thread.currentThread().getContextClassLoader();
        WeakReference<ApplicationContext> ref = contextMap.get(currentLoader);
        if (ref != null) {
            ApplicationContext ctx = ref.get();
            if (ctx != null) {
                return ctx;
            }
        }
        
        // Try parent classloaders
        ClassLoader loader = currentLoader;
        while (loader != null) {
            ref = contextMap.get(loader);
            if (ref != null) {
                ApplicationContext ctx = ref.get();
                if (ctx != null) {
                    return ctx;
                }
            }
            loader = loader.getParent();
        }
        
        return null;
    }
    
    /**
     * Clear existing conceiver if it's from a different classloader.
     */
    private void clearExistingConceiver() {
        try {
            // Check if there's an existing conceiver from a different classloader
            Object storedClassLoader = ObjectTable.get(CONCEIVER_CLASSLOADER_KEY);
            ClassLoader currentClassLoader = this.getClass().getClassLoader();
            
            if (storedClassLoader != null && !storedClassLoader.equals(currentClassLoader)) {
                // Clear the conceiver as it's from a different classloader
                ObjectTable.clear(ConceiverProvider.LOOKUP_KEY);
            }
            
            // Store current classloader
            ObjectTable.put(CONCEIVER_CLASSLOADER_KEY, currentClassLoader);
        } catch (Exception e) {
            // Ignore errors during cleanup
        }
    }
    
    /**
     * Register this conceiver instance.
     */
    private void registerThisConceiver() {
        // Force registration of this conceiver
        ObjectTable.put(ConceiverProvider.LOOKUP_KEY, this);
    }
    
    /**
     * Clean up resources when context is closed.
     */
    public void onApplicationContextClosed() {
        // Remove from context map
        if (applicationContext != null) {
            contextMap.remove(applicationContext.getClassLoader());
        }
        
        // Clear from ObjectTable
        ObjectTable.clear(CONTEXT_KEY);
        ObjectTable.clear(ConceiverProvider.LOOKUP_KEY);
        ObjectTable.clear(CONCEIVER_CLASSLOADER_KEY);
    }
}