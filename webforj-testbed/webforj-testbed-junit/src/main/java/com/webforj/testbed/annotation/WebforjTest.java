package com.webforj.testbed.annotation;

import com.webforj.App;
import com.webforj.component.Component;
import com.webforj.testbed.WebforjTestbed;
import com.webforj.testbed.junit.WebforjTestbedExtension;
import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Runs every test of the annotated class inside a {@link WebforjTestbed}.
 *
 * <p>
 * The context is set up before each test and torn down after it, so every test starts from a clean
 * state. Test methods receive the context by declaring a {@link WebforjTestbed} parameter or by
 * calling {@link WebforjTestbed#getCurrent()}.
 * </p>
 *
 * <pre>
 * &#64;WebforjTest
 * class MyComponentTest {
 *
 *   &#64;Test
 *   void shouldUpdateText(WebforjTestbed context) {
 *     Button button = new Button("Save");
 *     context.add(button);
 *
 *     button.setText("Saved");
 *
 *     assertEquals("Saved", button.getText());
 *   }
 * }
 * </pre>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@Documented
@Inherited
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@ExtendWith(WebforjTestbedExtension.class)
public @interface WebforjTest {

  /**
   * The application class to launch in the context.
   *
   * <p>
   * The class is launched through the same bootstrap a production deployment uses, so its
   * {@code run} method executes and its annotations, including
   * {@link com.webforj.annotation.Routify @Routify}, take effect. Set an application class when the
   * test exercises whole application behavior. The default launches no application and the context
   * starts with an empty frame, which is the right starting point for tests that add components
   * directly.
   * </p>
   *
   * @return the application class
   */
  Class<? extends App> app() default App.class;

  /**
   * The packages to scan for components annotated with
   * {@link com.webforj.router.annotation.Route @Route}.
   *
   * <p>
   * Every component annotated with {@code @Route} inside the given packages is registered, the same
   * way {@link com.webforj.annotation.Routify @Routify} registers routes in a running application.
   * Set packages when the test should navigate the real route table of the application. When the
   * test needs only specific views, use {@link #routeClasses()} instead.
   * </p>
   *
   * @return the packages to scan
   */
  String[] routePackages() default {};

  /**
   * The component classes annotated with {@link com.webforj.router.annotation.Route @Route} to
   * register in the context.
   *
   * <p>
   * Each class is registered directly and its path comes from its {@code @Route} annotation. No
   * package scanning takes place, so the route table contains exactly the views the test names. Use
   * classes to keep the test isolated and its dependencies explicit. To register everything a
   * package contains, use {@link #routePackages()} instead.
   * </p>
   *
   * @return the components to register
   */
  Class<? extends Component>[] routeClasses() default {};
}
