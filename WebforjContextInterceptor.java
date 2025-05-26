package com.webforj.spring;

import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.HandlerInterceptor;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * Interceptor that ensures the Spring ApplicationContext is available
 * in the ThreadLocal for each request when using the ThreadLocal approach.
 */
@Component
public class WebforjContextInterceptor implements HandlerInterceptor {
    
    private final ApplicationContext applicationContext;
    
    public WebforjContextInterceptor(ApplicationContext applicationContext) {
        this.applicationContext = applicationContext;
    }
    
    @Override
    public boolean preHandle(HttpServletRequest request, HttpServletResponse response, 
            Object handler) throws Exception {
        // Set the context for this thread
        SpringConceiverThreadLocalApproach.setContextForThread(applicationContext);
        return true;
    }
    
    @Override
    public void afterCompletion(HttpServletRequest request, HttpServletResponse response, 
            Object handler, Exception ex) throws Exception {
        // Clean up ThreadLocal to prevent memory leaks
        SpringConceiverThreadLocalApproach.clearContextForThread();
    }
}