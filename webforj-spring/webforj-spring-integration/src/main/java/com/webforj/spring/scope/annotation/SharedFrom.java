package com.webforj.spring.scope.annotation;

import com.webforj.component.Component;
import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Specifies that a route-scoped bean should be shared from a specific component in the route
 * hierarchy.
 *
 * <p>
 * By default, route-scoped beans are shared from the topmost component in the route hierarchy. This
 * annotation allows you to override that behavior and specify a different component as the sharing
 * point. The bean instance will be shared across the specified component and all its child routes.
 * </p>
 *
 * <p>
 * Example usage:
 *
 * <pre>
 * {@code @RouteScope}
 * {@code @SharedFrom(ProductsLayout.class)}
 * {@code @Component}
 * public class ProductService {
 *   // This service will be shared from ProductsLayout
 *   // throughout its entire child hierarchy
 * }
 * </pre>
 * </p>
 *
 * <p>
 * The specified component must be present in the current route hierarchy for the scope to be valid.
 * If the component is not found in the hierarchy when the bean is requested, the bean creation will
 * fail with an {@link IllegalStateException}.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.03
 * @see RouteScope
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface SharedFrom {

  /**
   * The component class from which the bean should be shared.
   *
   * <p>
   * The bean instance will be shared across this component and all its child routes in the
   * hierarchy. This component must be present in the current route hierarchy.
   * </p>
   *
   * @return the component class from which sharing starts
   */
  Class<? extends Component> value();
}
