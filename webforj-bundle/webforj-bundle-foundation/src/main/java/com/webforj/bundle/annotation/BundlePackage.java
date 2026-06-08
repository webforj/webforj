package com.webforj.bundle.annotation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Declares an npm package that the annotated class needs at the client side.
 *
 * <p>
 * webforJ scans the project classpath for {@code @BundlePackage} declarations, materializes them
 * into package.json, and installs and bundles the corresponding sources authored under
 * {@code src/main/frontend}.
 * </p>
 *
 * <p>
 * Example:
 * </p>
 *
 * <pre>
 * {@literal @}BundlePackage(value = "react", version = "^19.0.0")
 * {@literal @}BundlePackage(value = "lit", version = "^3.0.0")
 * public class RevenueChart extends Composite&lt;Div&gt; {
 *   // ...
 * }
 * </pre>
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
@Repeatable(BundlePackage.Container.class)
@Inherited
@Documented
public @interface BundlePackage {

  /**
   * The npm package name (for example {@code "react"} or {@code "@scope/name"}).
   *
   * @return the npm package name
   */
  String value();

  /**
   * The npm semver range (for example {@code "^19.0.0"}).
   *
   * <p>
   * Resolved by Bun at install time.
   * </p>
   *
   * @return the npm semver range
   */
  String version();

  /**
   * Whether the package is needed only at build time rather than shipped to the browser.
   *
   * <p>
   * A runtime package that the bundled sources import (for example {@code "react"} or
   * {@code "lit"}) lands in package.json {@code dependencies}. A build time package (for example a
   * compiler a source file relies on) lands in {@code devDependencies}.
   * </p>
   *
   * @return {@code true} to install as a dev dependency, {@code false} for a runtime dependency
   */
  boolean dev() default false;

  /**
   * A container for {@link BundlePackage} annotations.
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
     * A set of {@link BundlePackage} annotations.
     *
     * @return the set of {@link BundlePackage} annotations
     */
    BundlePackage[] value();
  }
}
