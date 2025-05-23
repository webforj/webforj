package com.webforj.springboot.conceiver;

import com.webforj.App;
import com.webforj.component.Component;
import com.webforj.component.event.ComponentEvent;
import com.webforj.conceiver.Conceiver;
import com.webforj.conceiver.DefaultConceiver;
import com.webforj.conceiver.exception.ConceiverException;
import com.webforj.exceptions.WebforjAppInitializeException;
import java.util.Map;
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;

public class SpringConceiver implements Conceiver, ApplicationContextAware {

    private ApplicationContext applicationContext;
    private final Conceiver defaultConceiver = new DefaultConceiver();

    @Override
    public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
        this.applicationContext = applicationContext;
    }

    @Override
    public <T> T get(Class<T> classOfT) {
        if (applicationContext == null) {
            // Should not happen in a Spring-managed environment after initialization
            // but as a safeguard:
            return defaultConceiver.get(classOfT);
        }
        try {
            return applicationContext.getBean(classOfT);
        } catch (BeansException e) {
            // Bean not found in Spring context, try to create it with DefaultConceiver
            // This allows for non-Spring managed classes to still be instantiated.
            return defaultConceiver.get(classOfT);
        }
    }

    @Override
    public App getApplication(Class<? extends App> appClass) throws WebforjAppInitializeException {
        if (applicationContext == null) {
            return defaultConceiver.getApplication(appClass);
        }
        try {
            // Typically, the main App class itself might not be a Spring bean managed by getBean(Class)
            // If it is, this will work. Otherwise, DefaultConceiver logic is more appropriate.
            // Or, a specific method in SpringConceiver could be used if Apps have special Spring handling.
            // For now, let's see if it's a bean.
            return applicationContext.getBean(appClass);
        } catch (BeansException e) {
            // Fallback to default behavior if not a Spring bean
            return defaultConceiver.getApplication(appClass);
        }
    }

    @Override
    public <T extends Component> T getComponent(Class<T> classOfT) {
        if (applicationContext == null) {
            return defaultConceiver.getComponent(classOfT);
        }
        try {
            return applicationContext.getBean(classOfT);
        } catch (BeansException e) {
            // Fallback to default behavior if not a Spring bean
            return defaultConceiver.getComponent(classOfT);
        }
    }

    @Override
    public <E extends ComponentEvent<?>> E getComponentEvent(Component component, Class<?> eventClass, Map<String, Object> data) {
        // ComponentEvents are unlikely to be Spring beans themselves.
        // They are data objects, often with specific constructors.
        // DefaultConceiver is suitable here.
        if (applicationContext == null) {
             // This is to ensure that defaultConceiver is not null if applicationContext is null
             // given that defaultConceiver is initialized in the declaration.
             // However, to be absolutely safe and explicit:
            Conceiver fallback = this.defaultConceiver != null ? this.defaultConceiver : new DefaultConceiver();
            return fallback.getComponentEvent(component, eventClass, data);
        }
        return defaultConceiver.getComponentEvent(component, eventClass, data);
    }
}
