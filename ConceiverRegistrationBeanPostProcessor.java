package com.webforj.spring;

import com.webforj.conceiver.ConceiverProvider;
import com.webforj.environment.ObjectTable;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.core.Ordered;
import org.springframework.core.PriorityOrdered;

/**
 * BeanPostProcessor that ensures the Spring conceiver is registered early
 * in the Spring lifecycle, before any webforj components are created.
 */
public class ConceiverRegistrationBeanPostProcessor implements BeanPostProcessor, PriorityOrdered {
    
    private boolean conceiverRegistered = false;
    
    @Override
    public Object postProcessBeforeInitialization(Object bean, String beanName) throws BeansException {
        // Register the conceiver as soon as we see it
        if (bean instanceof SpringConceiverDevToolsCompatible && !conceiverRegistered) {
            // Clear any existing conceiver
            ObjectTable.clear(ConceiverProvider.LOOKUP_KEY);
            
            // Register the new conceiver
            ObjectTable.put(ConceiverProvider.LOOKUP_KEY, bean);
            conceiverRegistered = true;
        }
        
        return bean;
    }
    
    @Override
    public int getOrder() {
        // Run very early to ensure conceiver is available
        return Ordered.HIGHEST_PRECEDENCE;
    }
}