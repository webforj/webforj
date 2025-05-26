# Spring Boot DevTools Compatible Conceiver

This directory contains solutions for making the webforj Conceiver mechanism work correctly with Spring Boot DevTools, which uses different classloaders that can cause issues with the default implementation.

## The Problem

When Spring Boot DevTools is enabled:
- Application classes are loaded by `RestartClassLoader`
- Servlet and webforj bootstrap code use `AppClassLoader`
- The `ConceiverProvider` stores the conceiver in a static location that persists across classloader boundaries
- This causes the Spring conceiver to be unable to find Spring beans

## Solutions

### 1. SpringConceiverDevToolsCompatible (Recommended)

**File:** `SpringConceiverDevToolsCompatible.java`

This implementation handles classloader boundaries by:
- Using weak references to avoid classloader leaks
- Storing ApplicationContext in multiple locations for cross-classloader access
- Automatically clearing stale conceiver instances on context refresh
- Falling back gracefully when Spring beans aren't available

**Usage:**
```java
@Configuration
@Import(SpringConceiverConfiguration.class)
public class MySpringBootApp {
    // Your configuration
}
```

### 2. SpringConceiverThreadLocalApproach (Alternative)

**File:** `SpringConceiverThreadLocalApproach.java`

This implementation uses ThreadLocal storage:
- Each request thread maintains its own ApplicationContext reference
- Naturally handles classloader boundaries
- Requires interceptor configuration for web applications

**Usage:**
```java
@Configuration
public class WebConfig implements WebMvcConfigurer {
    @Autowired
    private WebforjContextInterceptor contextInterceptor;
    
    @Override
    public void addInterceptors(InterceptorRegistry registry) {
        registry.addInterceptor(contextInterceptor);
    }
}
```

## Implementation Details

### Service Registration

Create a file `META-INF/services/com.webforj.conceiver.Conceiver` in your resources:
```
com.webforj.spring.SpringConceiverDevToolsCompatible
```

### Key Features

1. **Classloader Detection**: Detects when a conceiver from a different classloader is present and clears it
2. **Multiple Lookup Strategies**: Tries multiple approaches to find the ApplicationContext
3. **Graceful Fallback**: Falls back to default conceiver for non-Spring managed classes
4. **Memory Leak Prevention**: Uses weak references and proper cleanup

### Configuration Options

The `SpringConceiverConfiguration` class provides:
- `@RestartScope` bean definition for DevTools compatibility
- Context refresh listener to clear stale conceivers
- Context close listener for cleanup
- Early registration via BeanPostProcessor

## Testing with DevTools

To test the implementation:

1. Enable Spring Boot DevTools in your project
2. Start your application
3. Make a code change that triggers a restart
4. Verify that Spring beans are still properly injected after the restart

## Troubleshooting

If you encounter issues:

1. **ClassCastException**: Ensure old conceiver instances are cleared on context refresh
2. **Beans not found**: Check that ApplicationContext is properly stored and retrieved
3. **Memory leaks**: Verify weak references are used and cleanup is performed

## Production Considerations

While these implementations work with DevTools, in production:
- DevTools is typically disabled
- The standard conceiver implementation without these workarounds is sufficient
- Consider using a profile-specific configuration to only enable these in development