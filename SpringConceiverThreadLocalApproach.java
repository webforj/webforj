package com.webforj.spring;

import com.webforj.App;
import com.webforj.component.Component;
import com.webforj.component.event.ComponentEvent;
import com.webforj.conceiver.Conceiver;
import com.webforj.conceiver.DefaultConceiver;
import com.webforj.exceptions.WebforjAppInitializeException;
import org.springframework.beans.factory.NoSuchBeanDefinitionException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.stereotype.Component as SpringComponent;

import java.util.Map;

/**
 * Alternative Spring Conceiver implementation using ThreadLocal to handle
 * classloader boundaries with Spring Boot DevTools.
 * 
 * This approach stores the ApplicationContext in a ThreadLocal, which naturally
 * handles the classloader boundary issues since each request thread will have
 * its own context reference.
 */
@SpringComponent
public class SpringConceiverThreadLocalApproach implements Conceiver, ApplicationContextAware {
    
    // ThreadLocal to store ApplicationContext per thread
    private static final ThreadLocal<ApplicationContext> contextHolder = new ThreadLocal<>();
    
    // Instance variable as backup
    private ApplicationContext applicationContext;
    private final DefaultConceiver fallbackConceiver = new DefaultConceiver();
    
    @Override
    public void setApplicationContext(ApplicationContext applicationContext) {
        this.applicationContext = applicationContext;
        // Also set in ThreadLocal for current thread
        contextHolder.set(applicationContext);
    }
    
    @Override
    public <T> T get(Class<T> classOfT) {
        ApplicationContext context = getActiveContext();
        
        if (context != null) {
            try {
                return context.getBean(classOfT);
            } catch (NoSuchBeanDefinitionException e) {
                try {
                    return context.getAutowireCapableBeanFactory().createBean(classOfT);
                } catch (Exception ex) {
                    return fallbackConceiver.get(classOfT);
                }
            }
        }
        
        return fallbackConceiver.get(classOfT);
    }
    
    @Override
    public App getApplication(Class<? extends App> appClass) throws WebforjAppInitializeException {
        ApplicationContext context = getActiveContext();
        
        if (context != null) {
            try {
                return context.getBean(appClass);
            } catch (NoSuchBeanDefinitionException e) {
                try {
                    return context.getAutowireCapableBeanFactory().createBean(appClass);
                } catch (Exception ex) {
                    throw new WebforjAppInitializeException(
                        "Failed to create application instance: " + appClass.getName(), ex);
                }
            }
        }
        
        return fallbackConceiver.getApplication(appClass);
    }
    
    @Override
    public <T extends Component> T getComponent(Class<T> classOfT) {
        ApplicationContext context = getActiveContext();
        
        if (context != null) {
            try {
                return context.getBean(classOfT);
            } catch (NoSuchBeanDefinitionException e) {
                try {
                    return context.getAutowireCapableBeanFactory().createBean(classOfT);
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
        return fallbackConceiver.getComponentEvent(component, eventClass, data);
    }
    
    /**
     * Get the active ApplicationContext, checking ThreadLocal first.
     */
    private ApplicationContext getActiveContext() {
        ApplicationContext context = contextHolder.get();
        if (context != null) {
            return context;
        }
        
        // Fall back to instance variable
        return applicationContext;
    }
    
    /**
     * Static method to set the context for the current thread.
     * This can be called from a Spring interceptor or filter.
     */
    public static void setContextForThread(ApplicationContext context) {
        contextHolder.set(context);
    }
    
    /**
     * Clear the context for the current thread.
     * Should be called at the end of request processing.
     */
    public static void clearContextForThread() {
        contextHolder.remove();
    }
}