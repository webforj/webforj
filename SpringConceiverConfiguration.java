package com.webforj.spring;

import com.webforj.conceiver.ConceiverProvider;
import com.webforj.environment.ObjectTable;
import org.springframework.boot.devtools.restart.RestartScope;
import org.springframework.context.ApplicationListener;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.event.ContextClosedEvent;
import org.springframework.context.event.ContextRefreshedEvent;

/**
 * Spring configuration for the DevTools-compatible conceiver.
 * 
 * This configuration ensures proper initialization and cleanup of the conceiver
 * when Spring contexts are refreshed or closed (which happens during DevTools restarts).
 */
@Configuration
public class SpringConceiverConfiguration {
    
    /**
     * The conceiver bean that will be registered with webforj.
     * Using @RestartScope ensures a new instance is created on each DevTools restart.
     */
    @Bean
    @RestartScope
    public SpringConceiverDevToolsCompatible springConceiver() {
        return new SpringConceiverDevToolsCompatible();
    }
    
    /**
     * Listener that clears the conceiver on context refresh.
     * This ensures the new conceiver instance is used after a DevTools restart.
     */
    @Bean
    public ApplicationListener<ContextRefreshedEvent> conceiverRefreshListener() {
        return event -> {
            // Clear any existing conceiver to force re-registration
            ObjectTable.clear(ConceiverProvider.LOOKUP_KEY);
            
            // The conceiver will be re-registered when it's first accessed
            // due to the @RestartScope annotation
        };
    }
    
    /**
     * Listener that cleans up conceiver resources on context close.
     */
    @Bean
    public ApplicationListener<ContextClosedEvent> conceiverCleanupListener(
            SpringConceiverDevToolsCompatible conceiver) {
        return event -> {
            conceiver.onApplicationContextClosed();
        };
    }
    
    /**
     * Alternative approach: Use a BeanPostProcessor to ensure the conceiver
     * is registered early in the Spring lifecycle.
     */
    @Bean
    public ConceiverRegistrationBeanPostProcessor conceiverRegistrationProcessor() {
        return new ConceiverRegistrationBeanPostProcessor();
    }
}