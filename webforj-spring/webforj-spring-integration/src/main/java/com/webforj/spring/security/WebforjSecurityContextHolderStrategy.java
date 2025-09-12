package com.webforj.spring.security;

import com.webforj.Environment;
import jakarta.servlet.http.HttpSession;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicReference;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolderStrategy;
import org.springframework.security.core.context.SecurityContextImpl;
import org.springframework.security.web.context.HttpSessionSecurityContextRepository;

/**
 * A SecurityContextHolderStrategy that retrieves the security context from the HTTP session using
 * webforJ's Environment.
 *
 * <p>
 * This strategy ensures that webforJ components can access the Spring Security context even when
 * the standard ThreadLocal-based strategy doesn't work due to webforJ's application lifecycle where
 * new application instances are created after authentication.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.10
 */
public final class WebforjSecurityContextHolderStrategy implements SecurityContextHolderStrategy {

  private final ThreadLocal<SecurityContext> contextHolder = new ThreadLocal<>();

  @Override
  public void clearContext() {
    contextHolder.remove();
  }

  @Override
  public SecurityContext getContext() {
    // Try to get from HTTP session first via webforJ Environment
    SecurityContext context = getFromHttpSession().orElseGet(contextHolder::get);
    if (context == null) {
      context = createEmptyContext();
      contextHolder.set(context);
    }
    return context;
  }

  private Optional<SecurityContext> getFromHttpSession() {
    Environment env = Environment.getCurrent();
    if (env == null) {
      return Optional.empty();
    }

    Optional<HttpSession.Accessor> accessor = env.getSessionAccessor();
    if (accessor.isEmpty()) {
      return Optional.empty();
    }

    // Spring Security stores the context in session with this key
    AtomicReference<Object> contextRef = new AtomicReference<>();
    accessor.get().access(session -> contextRef.set(
        session.getAttribute(HttpSessionSecurityContextRepository.SPRING_SECURITY_CONTEXT_KEY)));

    Object securityContext = contextRef.get();
    if (securityContext instanceof SecurityContext) {
      return Optional.of((SecurityContext) securityContext);
    }

    return Optional.empty();
  }

  @Override
  public void setContext(SecurityContext securityContext) {
    contextHolder.set(securityContext);
  }

  @Override
  public SecurityContext createEmptyContext() {
    return new SecurityContextImpl();
  }
}
