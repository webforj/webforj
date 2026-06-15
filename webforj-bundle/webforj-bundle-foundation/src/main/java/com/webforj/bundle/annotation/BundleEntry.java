package com.webforj.bundle.annotation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Binds a webforJ class to a bundle entry authored under {@code src/main/frontend}.
 *
 * <p>
 * The value is the path of the entry source file relative to {@code src/main/frontend}. When the
 * webforJ router creates an instance of the annotated class, the runtime resolves that entry to its
 * built output and loads the script, the stylesheet, or both. Outputs load once and stay for the
 * life of the page.
 * </p>
 *
 * <pre>
 * {@literal @}Route("/")
 * {@literal @}BundleEntry("counter/counter.ts")
 * public class CounterView extends Composite&lt;Div&gt; {
 *   private final Div self = getBoundComponent();
 *
 *   public CounterView() {
 *     self.add(new Element("hello-counter"));
 *   }
 * }
 * </pre>
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
@Repeatable(BundleEntry.Container.class)
@Inherited
@Documented
public @interface BundleEntry {

  /**
   * The path of the entry source file relative to {@code src/main/frontend}.
   *
   * <p>
   * For example {@code "counter/counter.ts"} points to
   * {@code src/main/frontend/counter/counter.ts}, and {@code "theme/theme.css"} points to a
   * stylesheet entry. The author owns the layout, any path is accepted as long as the file exists.
   * </p>
   *
   * @return the entry source path relative to the bundle source root
   */
  String value();

  /**
   * Whether the entry loads only while the application runs in debug mode.
   *
   * <p>
   * A debug entry is built like any other, but the runtime injects its output only when the webforJ
   * environment reports debug mode. In a production run the output stays on disk and is never added
   * to the page, so a diagnostics overlay or a development only tool ships without reaching the
   * browser outside debug.
   * </p>
   *
   * @return {@code true} to inject the entry only in debug mode, {@code false} to always inject it
   */
  boolean debug() default false;

  /**
   * A container for {@link BundleEntry} annotations.
   *
   * @author Hyyan Abo Fakher
   * @since 26.01
   */
  @Retention(RetentionPolicy.RUNTIME)
  @Target(ElementType.TYPE)
  @Inherited
  @Documented
  public @interface Container {

    /**
     * A set of {@link BundleEntry} annotations.
     *
     * @return the set of {@link BundleEntry} annotations
     */
    BundleEntry[] value();
  }
}
